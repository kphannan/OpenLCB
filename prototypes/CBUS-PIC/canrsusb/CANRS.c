/*  OpenLCB RS232 or USB interface for CBUS modules

    2 Dec 2009

    Must define USB or RS232 before compiling.

    Uses 4 MHz resonator and PLL for 16 MHz clock

    Serial BAUD is 115.2KB, 87 usec per byte
    USB version uses parallel USB adapter and virtual com port at any speed.

    Copyright (C) 2008    Mike Johnson

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
//*********************************************************************************

#include "../canlib/frametypes.c"
#include "../canlib/general.c"
#include "../canlib/entry.c"
#include "../canlib/ecan.c"
#include "../canlib/serial.c"

//*********************************************************************************
//    define RAM storage
//*********************************************************************************

#pragma udata

BYTE i, j, k, t;
BYTE txerrcnt;
BYTE rxerrcnt;
BYTE dgcnt;
unsigned int DNID;
BYTE canTraffic;           // yellow led CAN traffic indicator

#ifdef RS232
// Serial buffer data
BYTE sertxin, sertxout;    // transmit buffer pointers
BYTE serrcin, serrcout;    // receive buffer pointers
BYTE byte;                 // character returned by GetSerial
#pragma udata grp1
#define SERIALTXMASK 0xFF
far BYTE sertxbuf[256];
#pragma udata grp2
#define SERIALRXMASK 0x7F
far BYTE serrcbuf[128];
#pragma udata
#endif

//*********************************************************************************
//        ROM module info
//*********************************************************************************

#ifdef USB
#define modulestring "OpenLCB PIC CANUSB "  __DATE__ " " __TIME__
#else
#define modulestring "OpenLCB PIC CANRS "  __DATE__ " " __TIME__
#endif

#pragma romdata
const rom BYTE xml[] = "<cdi><id><software>" modulestring "</software></id>"
    "<se na=\"Location\" or=\"#0080\" sp=\"#FE\" bu=\"#103\">"
      "<ch na=\"Location\" si=\"64\"/>"
    "</se>"
    "<se na=\"Node Id\" or=\"#0040\" sp=\"#FE\" bu=\"#143\">"
      "<in na=\"Serial\" si=\"1\"/>"
      "<in na=\"Member\" si=\"3\"/>"
      "<by na=\"Group\" si=\"2\"/>"
    "</se>"
  "</cdi>";

// 0x0027
#pragma romdata module = 0x001020
const rom BYTE valid = 0;     // tmp set to 0xFF by PC side of loader. 

#pragma romdata
#pragma udata ovrly

//*********************************************************************************
//        High priority interrupt. Not used
//*********************************************************************************

#pragma interrupt hpinterrupt
void hpinterrupt(void)
{
}

//*********************************************************************************
//        Low priority interrupt.
//*********************************************************************************

#pragma interruptlow lpinterrupt
void lpinterrupt(void)
{
#ifdef RS232
    if (PIR1bits.RCIF) {             // Receive buffer loaded
        serrcbuf[serrcin++] = RCREG; // also clears RCIF
        serrcin &= SERIALRXMASK;
        if (RCSTAbits.OERR)          // overflow error
            RCSTAbits.CREN = 0;
        if (((serrcout-serrcin)&SERIALRXMASK) < 10) // buffer getting full
            SERIALCTS = 1;           // block CTS
    }
    if (PIR1bits.TXIF) {             // Transmit buffer empty
        if (sertxin==sertxout)       // nothing to send
            PIE1bits.TXIE = 0;       // disable TX interrupt
        else { // send a character
            TXREG = sertxbuf[sertxout++]; // also clears TXIF
            sertxout &= SERIALTXMASK;
        }
    }
#endif
}

//*********************************************************************************
//        Basic I/O stuff.
//*********************************************************************************

BOOL GetSerial(void)
{
#ifdef USB
    if (USBRXF == 0) {
        TRISC = 0xFF;
        USBRD = 1;
        USBRD = 0;        // RD strobe, reads on low
        byte = PORTC;
        USBRD = 1;
        return TRUE;
    }
    return FALSE;
#else
    far overlay BOOL c;
    PIE1bits.RCIE = 0; // disable interrupts while we mess with the buffer
    if (serrcin==serrcout) { // nothing to get
        c = FALSE;
        goto ret;
    }
    else { // get a character
        byte = serrcbuf[serrcout++];
        serrcout &= SERIALRXMASK;
        c = TRUE;
        if (((serrcout-serrcin)&SERIALRXMASK) > 20) // un-block cts
            SERIALCTS = 0;
    }
ret:
    PIE1bits.RCIE = 1; // enable interrupt
    return c;
#endif
}

BOOL PutSerial(BYTE c)
{
#ifdef USB
    while (USBTXE == 1) ;
    TRISC = 0x00;
    PORTC = c;
    USBWR = 1;
    USBWR = 0;
    TRISC = 0xFF;
#else
    PIE1bits.TXIE = 0;        // disable interrupts while we mess with the buffer
    sertxbuf[sertxin++] = c;
    sertxin &= SERIALTXMASK;
    if (sertxin == sertxout)  // buffer full - throw away the character
        sertxin = (sertxin - 1) & SERIALTXMASK; // move pointer back
    //if (SERIALRTS==0)
        PIE1bits.TXIE = 1;    // enable TX interrupt
#endif
    return TRUE;
}

BOOL SendMessage(void)
{
    if (ECANSendMessage()==FALSE)
        return FALSE;
    PrintPacket();
    return TRUE;
}

BOOL ReceiveMessage()
{
    if (ECANReceiveMessage()) {
        PrintPacket();
        return TRUE;
    }
    if (ReadPacket()) {
        while (ECANSendMessage()==0) ;
        return TRUE;
    }
    return FALSE;;
}

//*********************************************************************************
//        packet handler
//*********************************************************************************

void packet(void)
{
    if (CB_SourceNID == ND.nodeIdAlias) { // conflict
        if ((CB_FrameType&0x8000)==0x0000) { // CIM
            CB_SourceNID = ND.nodeIdAlias;
            CB_FrameType = FT_RID;
            CB_datalen = 0;
            while (SendMessage()==0) ;
        }
        else
            CheckAlias(1);
    }
    else if (CB_FrameType == FT_VNSN) { // send full NID
        SendNSN(FT_NSN);
    }
    else if (CB_FrameType == FT_EVENT) {
        canTraffic = 1;
    }
    else if (CB_FrameType == FT_AMD && CB_SourceNID == DNID) { // node reset before end of datagram
        DNID = -1;
        dgcnt = 0;
    }
    else if (CB_FrameType==(FT_DGM|ND.nodeIdAlias) || CB_FrameType==(FT_DGL|ND.nodeIdAlias)
      || CB_FrameType==(FT_DGS|ND.nodeIdAlias)) {
        if (DNID == (-1) || (HI(CB_FrameType)&0xF0)==(FT_DGS>>8)) { // first packet
            dgcnt = 0;
            DNID = CB_SourceNID;
        }
        else if (DNID != CB_SourceNID) {
            sendnack(CB_SourceNID,0);
            return;
        }
        for (i=0; i<CB_datalen && dgcnt<72; i++)
            GP_block[dgcnt++] = CB_data[i];
        if ((HI(CB_FrameType)&0xF0)==(FT_DGL>>8) || (HI(CB_FrameType)&0xF0)==(FT_DGS>>8)) { // end of datagram
            DNID = -1;
            dgcnt = 0;
            if (GP_block[0] == DG_MEMORY) {
                UP(GP_address) = GP_block[3];
                HI(GP_address) = GP_block[4];
                LO(GP_address) = GP_block[5];
                if (GP_block[1] == DGM_WRITE) {
                    if (GP_block[6] == 0xFE) { // program memory
                        // write program data
                        ProgramMemoryWrite(GP_address, 64, (BYTE * far)&GP_block[7]);
                        sendack(CB_SourceNID);
                        return;
                    }
                }
                else if (GP_block[1] == DGM_READ) {
                    if (GP_block[6]==0xFF) { // XML file
                        // read XML file
                        GP_address += (unsigned short long)&xml[0];
                        i = GP_block[7];
                        ProgramMemoryRead(GP_address, i, (BYTE * far)&GP_block[7]);
                        StartSendBlock(i+7, CB_SourceNID);
                        return;
                    }
                    if (GP_block[6]==0xFE) { // program memory
                        // read program data
                        i = GP_block[7];
                        ProgramMemoryRead(GP_address, i, (BYTE * far)&GP_block[7]);
                        StartSendBlock(i+7, CB_SourceNID);
                        return;
                    }
                }
                else if (CB_data[1] == DGM_UPDCOMP) {
		          // change the valid program flag
		          ProgramMemoryRead(STARTADDRESS,64,(BYTE * far)GP_block);
		          if (GP_block[0x0027]!=0) {
		              GP_block[0x0027] = 0;
		              ProgramMemoryWrite(STARTADDRESS,64,(BYTE * far)GP_block);
		          }
		          // start the program
		          _asm
		              reset
		              goto 0x000000
		          _endasm
                }
                else if (CB_data[1] == DGM_REBOOT) {
                    // re-start the program
                    _asm
                        reset
                        goto 0x000000
                    _endasm
                }
                else if (CB_data[1] == DGM_LOADER) { // program upgrade
                    INTCONbits.GIEH = 0;    // disable all interrupts          
                    INTCONbits.GIEL = 0;
                    Loader();               // call loader, never returns here
                }
            } // memory op
            sendnack(CB_SourceNID,CB_data[1]);
        } // end of datagram
    } // datagram
}

//*********************************************************************************
//        main setup routine
//*********************************************************************************

void main(void)
{
    ADCON1 = 0x0F;        // turn off A/D, all digital I/O
    TRISA = 0x20;        // Port A outputs except reset pin

    InitSerial();
    ECANInitialize();

//  Interrupts
    RCONbits.IPEN = 1;    // enable interrupt priority levels
    IPR1 = 0;             // all interrupts are low priority
    IPR2 = 0;
    IPR3 = 0;
#ifdef RS232
    PIE1bits.RCIE = 1;    // enable interrupt on RS232 input    
    INTCONbits.GIEL = 1;  // enable low priority interrupts
    INTCONbits.GIEH = 1;  // enable high priority interrupts, even if not used ?
#endif

    GreenLEDOn();
    YellowLEDOff();
    CheckAlias(0);
    PutRomString((BYTE rom far *)"\r\nStart "  modulestring "\r\n");
    RXindex = 0;
    dgcnt = 0;
    DNID = -1;
    SendNSN(FT_INIT);

    while (1) {
        if (Timer3Test()) {  // 25 msec timer
            if (canTraffic) {
                YellowLEDOn();
            }
            else {
                YellowLEDOff();
            }
            canTraffic = 0;
        }

        EndSendBlock();

        if (ReceiveMessage()) {
            if (CB_msgFlags&ECAN_OVERFLOW)
                PutRomString((BYTE rom far *)"-ECAN Buffer overflow-\r\n");
            if (CB_msgFlags&ECAN_INVALID_MSG)
                PutRomString((BYTE rom far *)"-ECAN Invalid message-\r\n");
            packet();
        }

        if (TXERRCNT != txerrcnt) {
            txerrcnt = TXERRCNT;
            PutRomString((BYTE rom far *)"-ECAN TXERRCNT ");
            Put2Hex(TXERRCNT);
            PutRomString((BYTE rom far *)"-\r\n");
        }
        if (RXERRCNT != rxerrcnt) {
            rxerrcnt = RXERRCNT;
            PutRomString((BYTE rom far *)"-ECAN RXERRCNT ");
            Put2Hex(RXERRCNT);
            PutRomString((BYTE rom far *)"-\r\n");
        }
    }
}



