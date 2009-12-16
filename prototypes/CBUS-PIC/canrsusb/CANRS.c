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
BYTE timer;
unsigned int blocks;
unsigned int DNID;

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
#define modulestring "OpenLCB for MERG CANUSB "  __DATE__ " " __TIME__
#else
#define modulestring "OpenLCB for MERG CANRS "  __DATE__ " " __TIME__
#endif

#pragma romdata
const rom BYTE xml[] = 
    "<XML>\r\n"
    "<NodeString>" modulestring "</NodeString>\r\n"
    "<EventData>\r\n"
    "</EventData>\r\n"
    "<NodeVariable>\r\n"
    "</NodeVariable>\r\n"
    "</XML>\r\n";

#pragma romdata module = 0x001020
const rom BYTE version[7] = { 0,1,0,1,0,1,0 };
// 0x0027
const rom BYTE valid = 0;     // tmp set to 0xFF by PC side of loader. 
const rom unsigned long xmlstart = (unsigned long)xml;
const rom unsigned int xmlsize = sizeof xml;
// 0x002E
const rom BYTE spare[18] = {
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF };
// 0x0040
const rom BYTE mstr[64] = modulestring;

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

void PutSerial(BYTE c)
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

rom unsigned int bits[10] = {
    0x03FE, 0x03FD, 0x03FB, 0x03F7, 0x03EF, 0x03DF, 0x03BF, 0x037F, 0x02FF, 0x01FF
};

void packet(void)
{
    if (CB_SourceNID == ND.nodeIdAlias) { // conflict
        if ((CB_FrameType&0x8000)==0x0000) { // CIM
            CB_SourceNID = ND.nodeIdAlias;
            CB_FrameType = FT_RIM;
            CB_datalen = 0;
            while (SendMessage()==0) ;
        }
        else
            CheckAlias(1);
    }
    else if (CB_FrameType == FT_VNSN) { // send full NID
        CB_FrameType = FT_DAA | CB_SourceNID;
        CB_SourceNID = ND.nodeIdAlias;
        CB_datalen = 7;
        CB_data[0] = DAA_NSN;
        CB_data[1] = ND.nodeId[5];
        CB_data[2] = ND.nodeId[4];
        CB_data[3] = ND.nodeId[3];
        CB_data[4] = ND.nodeId[2];
        CB_data[5] = ND.nodeId[1];
        CB_data[6] = ND.nodeId[0];
        while (SendMessage()==0) ;
    }
    else if (CB_FrameType == (FT_DAA | ND.nodeIdAlias) ) {
        if (CB_data[0] == DAA_UPGSTART) { // program upgrade
            INTCONbits.GIEH = 0;    // disable all interrupts          
            INTCONbits.GIEL = 0;
            Loader();               // call loader, never returns here
        }
        else if (CB_data[0] == DAA_REBOOT) {
            // re-start the program
            _asm
                reset
                goto 0x000000
            _endasm
        }
        else if (CB_data[0] == DAA_UPGREAD) { // single block read
            sendblock(CB_SourceNID);
        }
        else if (CB_data[0] == DAA_UPGADDR) {   // single block write
            DNID = CB_SourceNID;
            UP(GP_address) = CB_data[1];
            HI(GP_address) = CB_data[2];
            LO(GP_address) = CB_data[3];
            blocks = 0x03FF;
            timer = 0;
        }
        else if ((CB_data[0]&0xF0) == DAA_DATA && blocks != 0) { // data block
            if (DNID!=CB_SourceNID) {
               sendack(ACK_ALIASERROR,CB_SourceNID);
               return;
            }
            else {
                t = CB_data[0];
                blocks &= bits[t];
                t = t * 7;
                for (i = 1; i<8 && t<64; i++)
                    GP_block[t++] = CB_data[i];
                if (blocks==0) {
                    ProgramMemoryWrite(GP_address, 64, (BYTE * far)GP_block);
                    sendack(ACK_OK,DNID);    // OK

                }
            }
        }
        else if (CB_data[0] == DAA_CEERASEH || CB_data[0] == DAA_DEFAULT)
            sendack(ACK_OK, CB_SourceNID);
        else if (CB_data[0] == DAA_NVREAD || CB_data[0] == DAA_CEREADH 
          || CB_data[0] == DAA_PEREAD)
            sendack(ACK_NODATA, CB_SourceNID);
        else if (CB_data[0] == DAA_NVWRITE || CB_data[0] == DAA_CEWRITEH 
          || CB_data[0] == DAA_PEWRITEH)
            sendack(ACK_NOSPACE, CB_SourceNID);
    }
}

//*********************************************************************************
//        main setup routine
//*********************************************************************************

void main(void)
{
    ADCON1 = 0x0F;        // turn off A/D, all digital I/O
    TRISA = 0x20;        // Port A outputs except reset pin

    InitSerial();
    Timer3Init();

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

    // send INIT packet
    CB_SourceNID = ND.nodeIdAlias;
    CB_FrameType = FT_INIT;
    CB_datalen = 0;
    while (SendMessage()==0) ;

    while (1) {
        if (Timer3Test()) {  // 100 msec timer
            timer++;
            if (blocks!=0 && timer>20) { // send timeout ack
                sendack(ACK_TIMEOUT, DNID); // timeout
                blocks = 0;
            }
        }

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



