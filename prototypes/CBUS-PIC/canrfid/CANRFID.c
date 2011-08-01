/*
    OpenLCB RFID Using MERG RS232 and MERG RFID reader kit

    2 Dec 2009

    Serial BAUD is 9.6KB
    RFID Format is <STX 02> <10 bytes of hex data> <2 bytes xor of data> <cr> <lf> <ETX 03>

    To use the CANRS hardware the RFID output needs to be on pin 3 instead of pin 2
    and the gender of the D-type needs changing.

    Copyright (C) 2009    Mike Johnson

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

//*********************************************************************************
//    define RAM storage
//*********************************************************************************

#pragma udata

unsigned int DNID;
BYTE dgcnt;
BYTE i, t;
BOOL errorflag;
BYTE canTraffic;               // yellow led CAN traffic indicator

// Serial Input Buffer for 1 packet
static BYTE RXindex;           // index into buffer
static BYTE RXbuf[30];         // receive packet buffer

// Serial buffer data
static BYTE serrcin, serrcout; // receive buffer pointers
static BYTE byte;              // character returned by GetSerial
#pragma udata grp2
#define SERIALRXMASK 0x7F
static far BYTE serrcbuf[128];
#pragma udata

//*********************************************************************************
//        ROM module info
//*********************************************************************************

#define modulestring "OpenLCB PIC RFID input "  __DATE__ " " __TIME__

#pragma romdata
const rom BYTE xml[] = 
    "<cdi><id><software>" modulestring "</software></id>"
    "<se na=\"Location\" or=\"#0080\" sp=\"#FE\" bu=\"#103\">"
      "<ch na=\"Location\" si=\"64\"/>"
    "</se>"
    "<se na=\"Node Id\" or=\"#0040\" sp=\"#FE\" bu=\"#143\">"
      "<in na=\"Serial\" si=\"1\"/>"
      "<in na=\"Member\" si=\"3\"/>"
      "<by na=\"Group\" si=\"2\"/>"
    "</se>"
    "</cdi>";

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
    if (PIR1bits.RCIF) {             // Receive buffer loaded
        serrcbuf[serrcin++] = RCREG; // also clears RCIF
        serrcin &= SERIALRXMASK;
        if (RCSTAbits.OERR)          // overflow error
            RCSTAbits.CREN = 0;
    }
}

BOOL GetSerial(void)
{
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
        // if (((serrcout-serrcin)&SERIALRXMASK) > 20) // un-block cts
        //     SERIALCTS = 0;
    }
ret:
    PIE1bits.RCIE = 1; // enable interrupt
    return c;
}

BYTE GetHex(BYTE a)
{
    far overlay BYTE c = a;
    if (c>='0' && c<='9')
        return c-'0';
    if (c>='A' && c<='F')
        return c-'A'+10;
    if (c>='a' && c<='f')
        return c-'a'+10;
    errorflag = TRUE;
    return 0;
}

BYTE Get2Hex(BYTE a, BYTE b)
{
    return (GetHex(a)<<4) | GetHex(b);
}

BOOL SendMessage(void)
{
    return ECANSendMessage();
}

BOOL ReceiveMessage(void)
{
    return ECANReceiveMessage();
}

//*********************************************************************************
//        packet handler
//*********************************************************************************

void Packet(void)
{
    far overlay BYTE i;

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
}

void DatagramPacket(void)
{
    far overlay BYTE i;
    if ((HI(CB_FrameType)&0xF0)==(FT_DGF>>8) || (HI(CB_FrameType)&0xF0)==(FT_DGS>>8)) {
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
                // write data
                if (GP_block[6] == 0xFE) {
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
                if (GP_block[6] == 0xFE) {
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
}

//*********************************************************************************
//        main setup routine
//*********************************************************************************

void main(void)
{
    ADCON1 = 0x0F;        // turn off A/D, all digital I/O
        
    TRISA = 0x20;        // Port A outputs except reset pin
    TRISB = 0x0A;        // RB0 is CTS, RB1 is RTS,  RB2 = CANTX, RB3 = CANRX, RB4,5 are logic 
                         // RB6,7 for debug and ICSP and diagnostic LEDs
    PORTB = 0x04;        // CAN recessive
    TRISC = 0x80;        // Port C 6,7 are serial.

//  Serial interface
    SPBRG = 0x9F;        // 16000000/9600/4-1 = 19F
    SPBRGH = 1;          // for Baud = 9600 use 1, for 115KB use 0
    BAUDCON = 0x08;      // set BAUDCON to 16 bit
    TXSTA = 0x24;        // set transmit 8 bit, enable, async, high speed(brgh=1)    
    RCSTA = 0x90;        // set serial enable, continuous receive enable, 8 bit
    serrcin = serrcout = 0;

    ECANInitialize();

//  Interrupts
    RCONbits.IPEN = 1;    // enable interrupt priority levels
    
    IPR1 = 0;             // all interrupts are low priority
    IPR2 = 0;
    IPR3 = 0;

    PIE1bits.RCIE = 1;    // enable interrupt on RS232 input    
    INTCONbits.GIEL = 1;  // enable low priority interrupts
    INTCONbits.GIEH = 1;  // enable high priority interrupts, even if not used ?

    GreenLEDOn();
    YellowLEDOff();
    DNID = -1;
    dgcnt = 0;
    i = 0x80;              // short delay needed before serial output, don't know why
    while (--i) ;

    CheckAlias(0);
    SendNSN(FT_INIT);

    RXindex = 0;
    while (1) {
        // 25 msec timer
        if (Timer3Test()) { 
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
            if (CB_FrameType==(FT_DGF|ND.nodeIdAlias) || CB_FrameType==(FT_DGM|ND.nodeIdAlias)
              || CB_FrameType==(FT_DGL|ND.nodeIdAlias) || CB_FrameType==(FT_DGS|ND.nodeIdAlias)) {
                canTraffic = 1;
                DatagramPacket();
            }
            else
                Packet();
        }

        if (GetSerial()) {
            if (byte==0x02) { // start of message
                RXbuf[0] = 0x02;
                RXindex = 1;
           }
           else if (byte==0x03) { // end of message
                if (RXbuf[0]!=0x02)
                    goto error;
                errorflag = FALSE;
                CB_SourceNID = ND.nodeIdAlias;
                CB_FrameType = FT_EVENT;
                CB_datalen = 8;
                CB_data[0] = 0x07;
                CB_data[1] = 0;
                CB_data[2] = 0;
                CB_data[3] = Get2Hex(RXbuf[1], RXbuf[2]);
                CB_data[4] = Get2Hex(RXbuf[3], RXbuf[4]);
                CB_data[5] = Get2Hex(RXbuf[5], RXbuf[6]);
                CB_data[6] = Get2Hex(RXbuf[7], RXbuf[8]);
                CB_data[7] = Get2Hex(RXbuf[9], RXbuf[10]);
                if (!errorflag)
                    while (SendMessage()==0) ;
error:          RXindex = 0;
            }
            else { // put a character in the buffer
                if (RXindex >= 29) {
                    RXindex = 0;
                }
                RXbuf[RXindex++] = byte;
            }
        }
    }
}

