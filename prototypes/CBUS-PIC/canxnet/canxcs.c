/*
    OpenLCB interface to XpressNet Command Station

    NV 0 is the XpressNet ID for the interface, default 29

I modified a CANACC8 to have an SN75176 RS-485 driver chip.

Serial Rx pic c7/Rx/pin18 to 75176 pin 1
Serial Tx pic c6/tx/pin17 to 75176 pin 4
Transmit enable pic c5/pin16 to 75176 pins 2 and 3
75176 pin 5 to 0v
75176 pin 8 to 5v

XpressNet L to 12v (15 ma per cab)
XpressNet M to 0v
XpressNet A to 75176 pin 6
XpressNet B to 75176 pin 7
120 ohm terminating resistor across pins 6 and 7

Modifications to PCB.

Delete ULN2803, cut track to pin 4 and 5 of ULN2803 socket.
Rx data - connect pin 4 to pin 8.
Enable - connect pin 5 to pin 6.
5v - connect 5v to pin 15
0v - connect 0v to pin 12
Insert SN75176 with pin 1 in socket pin 4


    CANXCS Uses a modified CANACC8 with RS485 driver.
    Uses 4 MHz resonator and PLL for 16 MHz clock
    This interface uses FLiM to get a Node Number.

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

#define SendMessage ECANSendMessage
#define ReceiveMessage ECANReceiveMessage

#include "../canlib/frametypes.c"
#include "../canlib/general.c"
#include "../canlib/entry.c"
#include "../canlib/ecan.c"
#include "../canlib/eeprom.c"

//*********************************************************************************
// define RAM storage
//*********************************************************************************

#define DEFAULTXNETID 29

#pragma udata

unsigned int DNID;        // block transfer source NID
BYTE blocks;              // loader block count
BYTE canTraffic;          // yellow led CAN traffic indicator
BYTE timer100;            // 100 msec count
#pragma udata rt
BYTE far ramtable[65];	  // for program memory reads and writes
#pragma udata
unsigned long address;    // block read or write address

BYTE BitMask[8];                // bit masks

// XpressNet input
#pragma udata
BYTE xnetid;
BYTE xnptr;                // pointer to input buffer
BYTE xin;                  // input character
BYTE xin9;                 // input character 9th bit
BYTE xln;                  // input message length
BOOL xnok;                 // input message complete
BOOL xnmsg;
#pragma udata xinb
far BYTE xinbuf[20];       // input buffer

// XpressNet output
#pragma udata
BOOL xoutrdy;              // msg ready to transmit
BYTE xoutptr;
BYTE xoutlen;
#pragma udata xoutb
far BYTE xoutbuf[20];      // output buffer

// RS bus to events
#pragma udata rs_state
far BYTE rs_state[128];

#pragma udata

//*********************************************************************************
//    EEPROM
//*********************************************************************************

#pragma romdata eedata_scn=0xf00000

rom BYTE XNETID = DEFAULTXNETID;

#pragma romdata

//*********************************************************************************
// ROM module info
//*********************************************************************************

#define modulestring "OpenLCB to Xbus Command Station "  __DATE__ " " __TIME__

#pragma romdata
const rom BYTE xml[] = 
    "<XML>\r\n"
    "<NodeString>" modulestring "</NodeString>\r\n"
    "<EventData>\r\n"
    "</EventData>\r\n"
    "<NodeVariable>\r\n"
      "<name>Xnetid</name><default>29</default>\r\n"
    "</NodeVariable>\r\n"
    "</XML>\r\n";

#pragma romdata module = 0x001020
const rom BYTE version[7] = { 
    0,1,0,1,0,1,0 
};
// 0x0027
const rom BYTE valid = 0;     // tmp set to 0xFF by PC side of loader. 
const rom unsigned long xmlstart = (unsigned long) xml;
const rom unsigned int xmlsize = sizeof xml;
// 0x002E
const rom BYTE spare[18] = {
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF 
};
// 0x0040
const rom BYTE mstr[64] = modulestring ;

rom BYTE pollparity[128] = {
    0x00,0x81,0x82,0x03,0x84,0x05,0x06,0x87,0x88,0x09,0x0A,0x8B,0x0C,0x8D,0x8E,0x0F,
    0x90,0x11,0x12,0x93,0x14,0x95,0x96,0x17,0x18,0x99,0x9A,0x1B,0x9C,0x1D,0x1E,0x9F,
    0xA0,0x21,0x22,0xA3,0x24,0xA5,0xA6,0x27,0x28,0xA9,0xAA,0x2B,0xAC,0x2D,0x2E,0xAF,
    0x30,0xB1,0xB2,0x33,0xB4,0x35,0x36,0xB7,0xB8,0x39,0x3A,0xBB,0x3C,0xBD,0xBE,0x3F,
    0xC0,0x41,0x42,0xC3,0x44,0xC5,0xC6,0x47,0x48,0xC9,0xCA,0x4B,0xCC,0x4D,0x4E,0xCF,
    0x50,0xD1,0xD2,0x53,0xD4,0x55,0x56,0xD7,0xD8,0x59,0x5A,0xDB,0x5C,0xDD,0xDE,0x5F,
    0x60,0xE1,0xE2,0x63,0xE4,0x65,0x66,0xE7,0xE8,0x69,0x6A,0xEB,0x6C,0xED,0xEE,0x6F,
    0xF0,0x71,0x72,0xF3,0x74,0xF5,0xF6,0x77,0x78,0xF9,0xFA,0x7B,0xFC,0x7D,0x7E,0xFF
};

#pragma romdata

#pragma udata overly    // needed for overlay

//*********************************************************************************
// High priority interrupt. Not used
//*********************************************************************************

#pragma interrupt hpinterrupt
void hpinterrupt(void)
{
}

//*********************************************************************************
// Low priority interrupt.
//*********************************************************************************

// XNET 62.5k x 9 bit data, 192usec per character

#pragma interruptlow lpinterrupt
void lpinterrupt(void)
{
    if (PIR1bits.RCIF) {             // Receive buffer loaded
        xin9 = RCSTA;
        xin = RCREG;            // also clears RCIF
        if (xin9&0x01) {        // polling or msg
            xin &= 0x7F;        // ignore parity
            if (xin == (0x40|xnetid)) { // poll
                if (xoutrdy) {
                    PORTCbits.RC5 = 1;      // Enable transmit on RS-485 chip
                    TXREG = xoutbuf[0];     // also clears TXIF
                    PIE1bits.TXIE = 1;      // enable TX interrupt
                    YellowLEDOn();
                }
            }
            else if (xin == 0x60 || xin == (0x60|xnetid) // message
              || xin == 0x20 || xin == (0x20|xnetid)) {  // RS bus feedback 
                xnok = FALSE;
                xnptr = 1;
                xinbuf[0] = xin;
                xnmsg = TRUE;
            }
            else if (xin == xnetid) { // error
                xoutbuf[0] = 0x20;     // send ack
                xoutbuf[1] = 0x20;
                xoutlen = 2;
                xoutptr = 1;
                xoutrdy = TRUE;
                PORTCbits.RC5 = 1;     // Enable transmit on RS-485 chip
                TXREG = xoutbuf[0];    // also clears TXIF
                PIE1bits.TXIE = 1;     // enable TX interrupt
                YellowLEDOn();
                xnmsg = FALSE;
                xinbuf[0] = xin;
                xinbuf[1] = 0x01;      // unknown error
                xinbuf[2] = 0x03;
                xinbuf[3] = 0x02;
                xnok = TRUE;
            }
        }
        else if (xnmsg) {
            xinbuf[xnptr++] = xin;
            if (xnptr == 2)
                xln = (xin & 0x0F) + 2;
            else if (xnptr > xln) {
                xinbuf[xnptr] = 0xFF;
                xnok = TRUE;
                xnmsg = FALSE;
            }
        }
        if (RCSTAbits.OERR || RCSTAbits.FERR) {
            RCSTAbits.CREN = 0;
            RCSTAbits.CREN = 1;
        }
    }

    if (PIR1bits.TXIF && PIE1bits.TXIE) { // Transmit buffer empty
        TXREG = xoutbuf[xoutptr++];       // also clears TXIF
        YellowLEDOn();
        if (xoutptr >= xoutlen) {         // nothing to send
            PIE1bits.TXIE = 0;            // disable TX interrupt
            xoutrdy = FALSE;
            while(TXSTAbits.TRMT == 0) ;  // wait until all sent, 35usec
            PORTCbits.RC5 = 0;            // Disable transmit on RS-485 chip
            RCSTAbits.CREN = 0;           // get rid of input junk
            RCSTAbits.CREN = 1;
            xin = RCREG;
       }
    }
}

//*********************************************************************************
// XpressNet In
//*********************************************************************************

BYTE XpressNetIn(void)
{
    far overlay BYTE i, j, addr, data;
    if (!xnok)
        return 0;
    if ((xinbuf[1]&0xF0) == 0x40) { // feedback broadcast
        i = 2;
        while (i <= (xinbuf[1]&0x0F)) {
            addr = xinbuf[i]&0x7F;
            data = rs_state[addr];
            if ((xinbuf[i+1]&0x10) == 0x10) // upper nibble
                data = (data & 0x0F) | (xinbuf[i+1]<<4);
            else // lower nibble
                data = (data & 0xF0) | (xinbuf[i+1]&0x0F);
            for (j=0; j<8; j++) {
                if ((BitMask[j]&data) != (BitMask[j]&rs_state[addr])) {
                    CB_FrameType = FT_EVENT;
                    CB_SourceNID = ND.nodeIdAlias;
                    CB_datalen = 8;
                    CB_data[0] = ND.nodeId[5];
                    CB_data[1] = ND.nodeId[4];
                    CB_data[2] = ND.nodeId[3];
                    CB_data[3] = ND.nodeId[2];
                    CB_data[4] = ND.nodeId[1];
                    CB_data[5] = ND.nodeId[0];
                    CB_data[6] = addr>>4;
                    CB_data[7] = (addr<<4) | (j<<1);
                    if ((BitMask[j]&data) != 0)
                        CB_data[7]++;
                    while (SendMessage()==0) ;
                }
            }
            rs_state[addr] = data;
        }
    }
    else {
        CB_FrameType = FT_XPRESSNET;
        CB_SourceNID = ND.nodeIdAlias;
        CB_datalen = (xinbuf[1]&0x0F)+2;
        CB_data[0] = xinbuf[1];
        CB_data[1] = xinbuf[2];
        CB_data[2] = xinbuf[3];
        CB_data[3] = xinbuf[4];
        CB_data[4] = xinbuf[5];
        CB_data[5] = xinbuf[6];
        CB_data[6] = xinbuf[7];
        CB_data[7] = xinbuf[8];
        while (SendMessage()==0) ;
    }
    xnok = FALSE;
    return 0;
}

//*********************************************************************************
//  Main packet handling is here
//*********************************************************************************

void DisableInterrupts(void)
{
    INTCONbits.GIEH = 0;     // disable all high priority interrupts          
    INTCONbits.GIEL = 0;     // disable all low priority interrupts
}

void EnableInterrupts(void)
{
    INTCONbits.GIEL = 1;    // enable all low priority interrupts
    INTCONbits.GIEH = 1;     // enable all high priority interrupts          
}

rom unsigned int bits[10] = {
    0x03FE, 0x03FD, 0x03FB, 0x03F7, 0x03EF, 0x03DF, 0x03BF, 0x037F, 0x02FF, 0x01FF
};

void Packet(void)
{
    far overlay BYTE i, t, tmp;

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
        SendNSN(FT_NSN);
    }
    else if (CB_FrameType == FT_EVENT) {
       canTraffic = 1;
    }
    else if (CB_FrameType == FT_XPRESSNET) {
        if (xoutrdy) {
            sendack(ACK_NOSPACE, CB_SourceNID);
            return;
        }
        t = CB_data[0]&0x0F;
        tmp = 0;
        for (i=0; i<=t; i++) {
            xoutbuf[i] = CB_data[i];
            tmp ^= CB_data[i];
        }
        xoutbuf[i] = tmp;
        xoutlen = t+2;
        xoutptr = 1;
        xoutrdy = TRUE;
    }
}

void DAA_Packet(void)
{
    far overlay BYTE i, t, tmp;

    if ((CB_data[0]&0xF0) == DAA_DATA && blocks != 0) { // data block
        if (DNID!=CB_SourceNID) {
           sendack(ACK_ALIASERROR, CB_SourceNID);
           sendack(ACK_ALIASERROR, DNID);
        }
        else {
            t = CB_data[0];
            blocks &= bits[t];
            t = t * 7;
            for (i = 1; i<8 && t<64; i++)
                GP_block[t++] = CB_data[i];
            if (blocks==0) {
                DisableInterrupts();
                ProgramMemoryWrite(GP_address, 64, (BYTE * far)GP_block);
                EnableInterrupts();
                sendack(ACK_OK,DNID);    // OK
            }
        }
        return;
    }

    switch(CB_data[0]) {
    case DAA_UPGSTART: // program upgrade
        DisableInterrupts();
        Loader();               // call loader, never returns here
        // break;

    case DAA_REBOOT:
        // re-start the program
        _asm
            reset
            goto 0x000000
        _endasm
        break;

    case DAA_UPGREAD: // single block read
        sendblock(CB_SourceNID);
        break;

    case DAA_UPGADDR: // single block write
        DNID = CB_SourceNID;
        UP(GP_address) = CB_data[1];
        HI(GP_address) = CB_data[2];
        LO(GP_address) = CB_data[3];
        blocks = 0x03FF;
        timer100 = 0;
        break;

    case DAA_NVREAD: // Node variable read
        if (CB_data[1] == 0) { // xnetid
            CB_FrameType = FT_DAA | CB_SourceNID;
            CB_SourceNID = ND.nodeIdAlias;
            CB_datalen = 3;
            CB_data[0] = DAA_NVREPLY;
            // CB_data[1] = CB_data[1];
            CB_data[2] = xnetid;
            while (ECANSendMessage()==0) ;
            return;
        }
        sendack(ACK_NODATA, CB_SourceNID); 
        break;

    case DAA_NVWRITE: // Node variable write byte
        if (CB_data[1] == 0) {
            xnetid = CB_data[2];
            EEPROMWrite((int)&XNETID, xnetid);
            sendack(ACK_OK, CB_SourceNID);
            return;
        }
        sendack(ACK_NOSPACE, CB_SourceNID);
        break;

    case DAA_DEFAULT:
        xnetid = DEFAULTXNETID;
        EEPROMWrite((int)&XNETID, xnetid);
    case DAA_PEERASE:
        sendack(ACK_OK, CB_SourceNID);
        break;

    case DAA_PEREAD:
    case DAA_CEREADH: // Event read
        sendack(ACK_NODATA, CB_SourceNID);
        break;

    case DAA_PEWRITEH:
    case DAA_CEERASEH: // Event erase
    case DAA_CEWRITEH: // Event write
        sendack(ACK_NOSPACE, CB_SourceNID);
        break;

    case DAA_XPRESSNET:
        if (xoutrdy) {
            sendack(ACK_NOSPACE, CB_SourceNID);
            return;
        }
        t = CB_data[1]&0x0F;
        tmp = 0;
        for (i=0; i<=t; i++) {
            xoutbuf[i] = CB_data[i+1];
            tmp ^= CB_data[i+1];
        }
        xoutbuf[i] = tmp;
        xoutlen = t+2;
        xoutptr = 1;
        xoutrdy = TRUE;
        break;
    }
}

//*********************************************************************************
// main setup routine
//*********************************************************************************

// wait 20 usec
void wait(void)
{
    far overlay BYTE i;
    i= (20*4/2);  // for 16MHz, 4 instructions per uS 
    while(--i) ;  // 2 instruction loop        
}

void main(void)
{
    far overlay BYTE i;

    // Initialize
    LATA = 0;            // Clear latches
    LATB = 0;
    LATC = 0;

    ADCON1 = 0x0F;       // turn off A/D, all digital I/O

    TRISA = 0x07;        // A0, 1 switches, 2 is S1 button
    TRISB = 0x0A;        // RB0, 1, 4, 5 switches, RB2 = CANTX, RB3 = CANRX 
    // RB6,7 for debug and ICSP and diagnostic LEDs
    PORTB = 0x04;        // CAN recessive
    TRISC = 0x80;        // PORTC 5 is TX enable, 6,7 are serial I/O.

    // Serial interface
    SPBRG = 15;          // Baud = 62.5k
    SPBRGH = 0;
    BAUDCON = 0x08;      // set BAUDCON to 16 bit
    TXSTA = 0x60;        // set transmit 9 bit, enable, async, 9th bit = 0  
    RCSTA = 0xD0;        // set serial enable, continuous receive enable, 9 bit

    BitMask[0] = 0x01;
    BitMask[1] = 0x02;
    BitMask[2] = 0x04;
    BitMask[3] = 0x08;
    BitMask[4] = 0x10;
    BitMask[5] = 0x20;
    BitMask[6] = 0x40;
    BitMask[7] = 0x80;

    // ECAN         
    ECANInitialize();

    // Interrupts
    RCONbits.IPEN = 1;    // enable interrupt priority levels
    IPR1 = 0;             // all interrupts are low priority
    IPR2 = 0;
    IPR3 = 0;
    PIE1bits.RCIE = 1;    // enable interrupt on RS232 input    
    EnableInterrupts();

    // Start
    GreenLEDOff();
    YellowLEDOff();

    xnetid = EEPROMRead((int)&XNETID);

    i = 0x80;             // short delay needed before serial output, don't know why
    while (--i) ;

    CheckAlias(0);
    GreenLEDOn();
    canTraffic = 0;

    // send INIT packet
    SendNSN(FT_INIT);

    while (1) {

        if (Timer3Test()) {
            if (canTraffic) {
                YellowLEDOn();
            }
            else {
                YellowLEDOff();
            }
            canTraffic = 0;

            if (blocks != 0 && timer100>20) { // send timeout ack
                sendack(ACK_TIMEOUT, DNID); // timeout
                blocks = 0;
            }
        }

        XpressNetIn();

        if (ECANReceiveMessage()) {
            if (CB_SourceNID == ND.nodeIdAlias) {    // conflict
                if ((CB_FrameType&0x8000)==0x0000) { // CIM or RIM message
                    CB_SourceNID = ND.nodeIdAlias;
                    CB_FrameType = FT_RIM;
                    CB_datalen = 0;
                    while (SendMessage()==0) ;
                }
                else
                    CheckAlias(1);                  // get new alias
            }
            else if (CB_FrameType == (FT_DAA | ND.nodeIdAlias) ) {
                canTraffic = 1;
                DAA_Packet();
            }
            else
                Packet();
        }

    }
}

//*********************************************************************************

