/*  OpenLCB for MERG CBUS CANACE3 Control Panel switch scanning

    16 Dec 2009

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

#define SendMessage ECANSendMessage
#define ReceiveMessage ECANReceiveMessage

#include "../canlib/frametypes.c"
#include "../canlib/general.c"
#include "../canlib/entry.c"
#include "../canlib/ecan.c"
#ifdef INTERLOCK
#include "interlock.c"
#endif

//*********************************************************************************
//    Ram
//*********************************************************************************

#ifdef INTERLOCK
#define ALARM PORTAbits.RA5
#endif

#pragma udata

BYTE timer;           // long period timer, 100 msec count

BYTE OldBuffer[16];   // debounced keystate scan buffer
BYTE NewBuffer[16];   // last keystate scan buffer

BYTE BitMask[8];      // bit masks

BYTE event[8];        // tmp event string space
BYTE eventcnt;        // 1 bit for each part of event msg pair
unsigned int eventindex; 
BYTE canTraffic;      // flash yellow led on can traffic
unsigned int blocks;  // 1 bit per block for data transfer
BYTE i, t;
unsigned int DNID;    // NIDa of device sending data
BYTE startofday;      // send switch states on power up
BYTE starttimeout;    // 2 sec delay before sending

#pragma udata ovrly

//*********************************************************************************
//        ROM module info
//*********************************************************************************

#ifdef INTERLOCK
#define modulestring "OpenLCB for CANACE3 Interlock "  __DATE__ " " __TIME__ 
#else
#define modulestring "OpenLCB for MERG CANACE3 "  __DATE__ " " __TIME__ 
#endif

#pragma romdata
const rom BYTE xml[] = 
    "<XML>\r\n"
    "<NodeString>" modulestring "</NodeString>\r\n"
    "<EventData>\r\n"
    "</EventData>\r\n"
    "<NodeVariable>\r\n"
    "  <Name>Refresh</Name>\r\n"
    "</NodeVariable>\r\n"
    "</XML>\r\n";

#pragma romdata module = 0x001020
const rom BYTE version[7] = { 0,1,0,1,0,1,0 };
// 0x0027
const rom BYTE valid = 0;     // tmp set to 0xFF by PC side of loader. 
const rom unsigned long xmlstart = (unsigned long) xml;
const rom unsigned int xmlsize = sizeof xml;
// 0x002E
const rom BYTE spare[18] = {
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF };
// 0x0040
const rom BYTE mstr[64] = modulestring ;

const rom BYTE EventTable[2048];

#pragma romdata

//*********************************************************************************
//    Program
//*********************************************************************************

#pragma interrupt hpinterrupt
void hpinterrupt(void)
{
}

#pragma interruptlow lpinterrupt
void lpinterrupt(void)
{
}

void SetDefault(void)
{
    far overlay BYTE i,j;
    far overlay unsigned int a;

    for (a=0; a<2048; a+=64) 
    {
        for (j=0; j<64; j+=8) {
            for (i=0; i<6; i++)
                GP_block[j+i] = ND.nodeId[5-i];
            GP_block[j+6] = 0;
            GP_block[j+7] = (a+j)>>3; 
        }
        ProgramMemoryWrite((unsigned short long)&EventTable[a],64,(BYTE * far)&GP_block[0]);
    }
}

void ReadEvent(unsigned int evno)
{
    far overlay unsigned int a;
    if (evno>=256) {
        sendack(ACK_NODATA, CB_SourceNID);
        return;
    }
    a = evno<<3;
    ProgramMemoryRead((unsigned short long)&EventTable[a], 8, (BYTE * far)&GP_block[0]);
    CB_FrameType = FT_DAA | CB_SourceNID;
    CB_SourceNID = ND.nodeIdAlias;
    CB_data[0] = DAA_PEWRITEH;
    CB_data[1] = GP_block[0];
    CB_data[2] = GP_block[1];
    CB_data[3] = GP_block[2];
    CB_data[4] = GP_block[3];
    CB_data[5] = GP_block[4];
    CB_data[6] = GP_block[5];
    CB_data[7] = GP_block[6];
    CB_datalen = 8;
    while (SendMessage()==0) ;
    CB_data[0] = DAA_PEWRITEL;
    CB_data[1] = GP_block[7];
    CB_data[2] = HI(evno);
    CB_data[3] = LO(evno);
    CB_datalen = 4;
    while (SendMessage()==0) ;
}

// event stored in 8 bytes of event global

void WriteEvent(unsigned int evno)
{
    far overlay unsigned int a;
    far overlay BYTE j;
    if (evno>=256) {
        sendack(ACK_NOSPACE, CB_SourceNID);
        return;
    }
    a = evno<<3;
    i = LO(a) & 0x3F;
    ProgramMemoryRead((unsigned short long)&EventTable[a&0x07C0],64,(BYTE * far)&GP_block[0]);
    for (j=0; j<8; j++)
        GP_block[i+j] = event[j];
    ProgramMemoryWrite((unsigned short long)&EventTable[a&0x07C0],64,(BYTE * far)&GP_block[0]);
    sendack(ACK_OK,DNID);
}

// overwrite with all 0xFF

void EraseEvent(unsigned int evno)
{
    event[0] = 0xFF;
    event[1] = 0xFF;
    event[2] = 0xFF;
    event[3] = 0xFF;
    event[4] = 0xFF;
    event[5] = 0xFF;
    event[6] = 0xFF;
    event[7] = 0xFF;
    WriteEvent(evno);
}

//*********************************************************************************
//    Scan
//*********************************************************************************

#ifdef INTERLOCK
BOOL locked(BYTE n)
{
    far overlay BOOL stack[10];
    far overlay BYTE sp;
    far overlay BYTE c;
    overlay rom BYTE * far p;
    sp = 0;
    p = locking[n+1];
    stack[0] = FALSE; // for null strings the lever is not locked
    while ((c = *p++) != 0)
    {
        if (c == 0xFF) {
            sp--;
            stack[sp-1] &= stack[sp];
        }
        else if (c == 0x7F) {
            sp--;
            stack[sp-1] |= stack[sp];
        }
        else if ((c&0x80)==0x80) {
            c--;
            stack[sp++] = (OldBuffer[(c&0x7F)>>3] & BitMask[c&0x07]) == 0;
        }
        else {
            c--;
            stack[sp++] = ((OldBuffer[c>>3] & BitMask[c&0x07])) != 0;
        }
    }
    return stack[0];
}
#endif

// scan and debounce switches or push buttons

void scan(void)
{
    far overlay BYTE Bitcng;   // changed bits in this col
    far overlay BYTE Bitcnt;   // bit counter in scan
    far overlay BYTE Row;      // row data for switch scan
    far overlay BYTE Ccount;   // column counter for switch scan
    far overlay BYTE i;
    far overlay BOOL t;
    far overlay unsigned int j;

    for (Ccount = 0; Ccount<16; Ccount++) {
        PORTA = (PORTA & 0xF0) | Ccount;
        // 0.1 ms to allow data to settle
        i = 400/3;     // for 16MHz, 400 instructions per 0.1mS 
        while(--i) ;   // 3 instruction loop        
        Row = PORTC;
        Bitcng = ~(Row ^ NewBuffer[Ccount]) & (Row ^ OldBuffer[Ccount]);
        NewBuffer[Ccount] = Row;
        for (Bitcnt=0; Bitcng!=0; Bitcnt++) {
            if ((BitMask[Bitcnt] & Bitcng) != 0) {
#ifdef INTERLOCK
                if (locked(Ccount<<3 | Bitcnt)) {
                    ALARM = 1;
                    return;
                }
#endif
                Bitcng &= ~BitMask[Bitcnt];
                i = (Ccount<<4) | (Bitcnt<<1);
                if ((Row & BitMask[Bitcnt]) == 0)
                    i |= 0x01;
                j = (unsigned int) i;
                j = j<<3;
                CB_SourceNID = ND.nodeIdAlias;
                CB_FrameType = FT_EVENT;
                CB_datalen = 8;
                t = FALSE;
                for (i=0; i<8; i++) {
                    CB_data[i] = EventTable[j+i];
                    if (CB_data[i]!=0xFF)
                        t = TRUE;
                }
                if (t) {
                    if (SendMessage()==0)   
                        return;
                }
                OldBuffer[Ccount] ^= BitMask[Bitcnt];
            }
        }
    }
}

//*********************************************************************************
//        packet handler
//*********************************************************************************

rom unsigned int bits[10] = {
    0x03FE, 0x03FD, 0x03FB, 0x03F7, 0x03EF, 0x03DF, 0x03BF, 0x037F, 0x02FF, 0x01FF
};

void Packet(void)
{
    if (CB_FrameType == FT_VNSN) { // send full NID
        SendNSN(FT_NSN);
    }
    else if (CB_FrameType == FT_EVENT) {
       canTraffic = 1;
    }
}

void DAA_Packet(void)
{
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
                ProgramMemoryWrite(GP_address, 64, (BYTE * far)GP_block);
                sendack(ACK_OK,DNID);    // OK
            }
        }
        return;
    }

    switch(CB_data[0]) {
    case DAA_UPGSTART: // program upgrade
        INTCONbits.GIEH = 0;    // disable all interrupts          
        INTCONbits.GIEL = 0;
        Loader();               // call loader, never returns here
        break;

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
        timer = 0;
        break;

    case DAA_DEFAULT:
        SetDefault();
        sendack(ACK_OK, CB_SourceNID);
        break;

    case DAA_PEERASE:
        event[0] = 0xFF;
        event[1] = 0xFF;
        event[2] = 0xFF;
        event[3] = 0xFF;
        event[4] = 0xFF;
        event[5] = 0xFF;
        event[6] = 0xFF;
        event[7] = 0xFF;
        WriteEvent(((unsigned int)CB_data[1]<<8) | CB_data[2]);
        break;

    case DAA_PEREAD:
        ReadEvent(((unsigned int)CB_data[1]<<8) | CB_data[2]);
        break;

    case DAA_PEWRITEH:
        event[0] = CB_data[1];
        event[1] = CB_data[2];
        event[2] = CB_data[3];
        event[3] = CB_data[4];
        event[4] = CB_data[5];
        event[5] = CB_data[6];
        event[6] = CB_data[7];
        goto PEW;

    case DAA_PEWRITEL:
        event[7] = CB_data[1];
        HI(eventindex) = CB_data[2];
        LO(eventindex) = CB_data[3];
PEW:
        if (eventcnt==0) {
            DNID = CB_SourceNID;
            eventcnt++;
            timer = 0;
            return;
        }
        eventcnt = 0;
        if (DNID != CB_SourceNID) {
            sendack(ACK_ALIASERROR, CB_SourceNID);
            sendack(ACK_ALIASERROR, DNID);
            return;
        }
        WriteEvent(eventindex);
        break;

    case DAA_CEERASEH:
        sendack(ACK_OK, CB_SourceNID);
        break;

    case DAA_NVREAD:
    case DAA_CEREADH:
        sendack(ACK_NODATA, CB_SourceNID);
        break;

    case DAA_NVWRITE:
    case DAA_CEWRITEH:
        sendack(ACK_NOSPACE, CB_SourceNID);
        break;
    }
}

//*********************************************************************************
//        MAIN
//*********************************************************************************

void main(void) {
    far overlay BYTE oldswitchid;  // canid/nodenumber switch value
    far overlay BYTE i, t;
    far overlay unsigned int j;

    INTCON = 0;
    ADCON0 = 0;
    ADCON1 = 0b00001111;
    TRISA =  0x10;             // Port A 0-3 Col ouput, 4 = S1, 5 = J5 ALARM
    TRISB = 0b00111000;        // RB2 = CANTX, RB3 = CANRX, 
                               // RB4,5 are sel input, RB6,7 for leds
    PORTBbits.RB2 = 1;         // CAN recessive
    TRISC = 0xFF;              // inputs

    RCONbits.IPEN = 1;         // enable interrupt priority levels
    EECON1 = 0;

    ECANInitialize();        
    Timer3Init();

    IPR3 = 0;                  // All IRQs low priority
    IPR1 = 0;
    IPR2 = 0;
    PIE1 = 0;
    PIE2 = 0;
    PIE3 = 0;
    INTCON2 = 0;
    INTCON3 = 0;
    PIR1 = 0;
    PIR2 = 0;
    PIR3 = 0;

//  timer 1 for 20 msec overflow
    T1CON = 0xA1;                      // enable timer1, 16 bit mode, 4 prescaler
    TMR1H = (0x10000 - 20000) >> 8;    // 16 * 20000 / 16,000,000 = 20 msec
    TMR1L = (0x10000 - 20000) & 0xFF;
    IPR1bits.TMR1IP = 1;
    PIE1bits.TMR1IE = 1;

    BitMask[0] = 0x01;
    BitMask[1] = 0x02;
    BitMask[2] = 0x04;
    BitMask[3] = 0x08;
    BitMask[4] = 0x10;
    BitMask[5] = 0x20;
    BitMask[6] = 0x40;
    BitMask[7] = 0x80;

    CheckAlias(0);
    startofday = 128;
    starttimeout = 20;
    GreenLEDOn();
    YellowLEDOff();

    // Initialize if EventTable is all zero
    ProgramMemoryRead((unsigned short long)&EventTable[0],64,(BYTE * far)&GP_block[0]);
    for (i=0; i<64; i++) {
        if (GP_block[i]!=0)
            break;
        if (i==63)
            SetDefault();
    }

    timer = 0;
    eventcnt = 0;

    // scan current switch states to buffer
    for (i = 0; i < 16; i++) {
        PORTA = i;
        // 0.1 ms to allow data to settle
        t = 400/3;     // for 16MHz, 400 instructions per 0.1mS 
        while(--t) ;   // 3 instruction loop        
        OldBuffer[i] = NewBuffer[i] = PORTC;
    }

    // send init
    SendNSN(FT_INIT);

    // Simple loop looking for a timer overflow or received CAN frame
    while (1) {
        // 100 msec timer
        if (Timer3Test()) {
#ifdef INTERLOCK
            ALARM = 0;
#endif
            if (canTraffic) {
                YellowLEDOn();
            }
            else {
                YellowLEDOff();
            }
            canTraffic = 0;

            timer++;
            if ((blocks!=0 || eventcnt!=0) && timer>20) { // send timeout ack
                sendack(ACK_TIMEOUT, DNID); // timeout
                blocks = 0;
                eventcnt = 0;
            }
            if (starttimeout)
                starttimeout--;
        }

        // 20 msec timer
        if (PIR1bits.TMR1IF) { 
            PIR1bits.TMR1IF = 0;            // reload the timer for 20 msec
            TMR1H = (0x10000 - 20000) >> 8; // 16 * 20000 / 16,000,000 = 20 msec
            TMR1L = (0x10000 - 20000) & 0xFF;

            // call scan every 20msec        
            scan(); // takes at least 16 x 0.1 ms

            // start of day, 1 every 20msec, about 2.5 secs
            if (starttimeout==0 && startofday!=0) {
                startofday--;
                CB_SourceNID = ND.nodeIdAlias;
                CB_FrameType = FT_EVENT;
                CB_datalen = 8;
                j = startofday<<1;
                if ((OldBuffer[startofday>>3] & BitMask[startofday&0x07]) == 0)
                    j |= 0x01;
                j = j<<3;
                t = FALSE;
                for (i=0; i<8; i++) {
                    CB_data[i] = EventTable[j+i];
                    if (CB_data[i]!=0xFF)
                        t = TRUE;
                }
                if (t) {
                    if (SendMessage()==0)   
                        return;
                }
            }
        }

        if (ReceiveMessage()) {
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

