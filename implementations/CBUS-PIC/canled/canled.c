/*    FLiM and SLiM compatible CANLED in C

    Short events have nodenumber 0.

    In learn mode the led corresponding to the led select switch is on. This
    is useful for testing  as well.

    Start-up led check on power up all the leds come on for a short time. 
    This does not use the normal display data area, so the real led state
    can update during the test.

    For teaching from a PC, subtract 1 from the led number, led numbers are 0
    to 63. Events are stored as 5 bytes, the 4 byte event number and one byte
    event. The event byte is the 6 bits led number, 1 bit polarity, and 1 bit
    toggle. Add 64 for Invert Polarity, and add 128 for Toggle mode to the
    led number.
    
    Copyright (C) 2008 Mike Johnson

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or any
    later version.

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
#define HASH2048
#include "../canlib/hash.c"

//*********************************************************************************
//        RAM data
//*********************************************************************************

#pragma udata

unsigned int AllLedOnTest; // start up test timer
BOOL LedsOn;

// hp int data
BYTE HpIndex;              // shift bit counter
BYTE HpColData;            // data shifted out
BYTE HpLedColumn[8];       // bit pattern for leds
BYTE HpLedRow;             // row number being output

BYTE BitMask[8];           // bit masks
unsigned int DNID;         // block transfer address
BYTE learnlink;            // debounce learn button
BYTE event[10];            // 10 bytes for an event
unsigned int timer;        // timeout timer for loader or event data
BOOL evdataexpected;       // event data expected flag
unsigned int blocks;     // loader - 1 bit for each packet to be transfered

//*********************************************************************************
//        ROM module info
//*********************************************************************************

#define modulestring "MERGCBUS CAN LED driver "  __DATE__ " " __TIME__ 

#pragma romdata
const rom BYTE xml[] = 
    "<XML>\r\n"
    "<NodeString>" modulestring "</NodeString>\r\n"
    "<EventData>\r\n"
    "  <name>Event Number</name><bits>32</bits>\r\n"
    "  <name>Led number</name><bits>6</bits>\r\n"
    "  <name>Polarity</name><bits>1</bits>\r\n"
    "  <name>Toggle</name><bits>1</bits>\r\n"
    "</EventData>\r\n"
    "<NodeVariable>\r\n"
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

#pragma romdata

#pragma udata overly    // needed for overlay

//*********************************************************************************
//        Low level routines
//*********************************************************************************

// Timer interrupt Timer0 in 16 bit mode
// 16/4MHz with prescale 8 gives 2 usec steps, counts up to overflow 
// so it needs to be reloaded with 0x10000-(t usec/2)

// Every 2.5 msec to drive LEDS, 100Hz refresh
#pragma interrupt hpinterrupt
void hpinterrupt(void)
{
    TMR0H = (0x10000-(25000/2))>>8;    // load the timer for the next bit
    TMR0L = (0x10000-(25000/2)) & 0xFF;
    INTCONbits.TMR0IF = 0;            // clear interrupt flag

    HpLedRow = (HpLedRow+1)& 0x03;    // 4 rows(0-3) 

    // preload next coloumn data upper byte first
    if (HpLedRow > 3)
        HpColData = 0;
    else if (AllLedOnTest)
        HpColData = 0xFF;
    else
        HpColData = HpLedColumn[(HpLedRow<<1)+1];
    for (HpIndex=0; HpIndex<8; HpIndex++) {
        if (HpColData & 0x80)
            PORTBbits.RB5 = 1;    // output a 1
        else
            PORTBbits.RB5 = 0;    // output a 0
        PORTBbits.RB4 = 1;        // clock column chip
        PORTBbits.RB4 = 0;
        HpColData <<= 1;
    }
    // preload lower byte of coloumn
    if (HpLedRow > 3)
        HpColData = 0;
    else if (AllLedOnTest)
        HpColData = 0xFF;
    else
        HpColData = HpLedColumn[HpLedRow<<1];
    for (HpIndex=0; HpIndex<8; HpIndex++) {
        if (HpColData & 0x80)
            PORTBbits.RB5 = 1;    // output a 1
        else
            PORTBbits.RB5 = 0;    // output a 0
        PORTBbits.RB4 = 1;        // clock column chip
        PORTBbits.RB4 = 0;
        HpColData <<= 1;
    }

    PORTCbits.RC2 = 1;            // disable row drive
    _asm nop _endasm;             // small delay
    PORTBbits.RB1 = 1;            // load column data from shift register
    PORTBbits.RB1 = 0;
    PORTC &= 0x3F;
    PORTC |= ((HpLedRow&0x03) << 6);
    if (LedsOn)
        PORTCbits.RC2 = 0;        // enable row drive
}

void DisableLeds(void)
{
    LedsOn = FALSE;
    PORTCbits.RC2 = 1;            // disable row drive
}

void EnableLeds(void)
{
    LedsOn = TRUE;
}

#pragma interruptlow lpinterrupt
void lpinterrupt(void)
{
}

BOOL SetupButtonDown(void)
{ 
    return PORTBbits.RB0==0;
}

// inputs
#define LEDSelect (PORTA & 0x3F)
#define Polarity !PORTCbits.RC4
#define Toggle !PORTCbits.RC3
#define Unlearn !PORTCbits.RC1
#define Learn !PORTCbits.RC0

BOOL SendMessage(void)
{
    return ECANSendMessage();
}

BOOL ReceiveMessage(void)
{
    return ECANReceiveMessage();
}

void DoEvent(BOOL on, unsigned int action)
{
    far overlay BYTE i, j;
    i = action & 0x07;
    j = (action >> 3 ) & 0x07;
    if (on) {
        HpLedColumn[j] |= BitMask[i];
    }
    else {
        HpLedColumn[j] &= ~BitMask[i];
    }
}

//*********************************************************************************
//    void wait(t)        wait t msec
//*********************************************************************************

void wait(BYTE t)
{
    far overlay unsigned int i;
    while(t--) {
        i= (4000/7);    // for 16MHz, 4000 instructions per mS 
        while(--i) ;    // 7 instruction loop        
    }
}

//*********************************************************************************
//        Packet and DAA_Packet
//*********************************************************************************

rom unsigned int bits[10] = {
    0x03FE, 0x03FD, 0x03FB, 0x03F7, 0x03EF, 0x03DF, 0x03BF, 0x037F, 0x02FF, 0x01FF
};

void Packet(void)
{
    far overlay BYTE i;

    switch(CB_FrameType) {
    case FT_VNSN: // send full NID
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
        return;

    case FT_ASON: // Short on event
        for (i=0; i<8; i++)
            CB_data[i] = 0;

    case FT_ACON: // On event
        if (learnlink) {            // in learn mode
            if (Unlearn) {          // unlean all events with this event number
                EraseEvent(&CB_data[0]);
            }
            else {                  // learn this event, using the switches and links
                LOWD(event[8]) = LEDSelect;
                if (Polarity)
                    event[8] |= 0x40;
                if (Toggle)
                    event[8] |= 0x80;
                SaveEvent((BYTE * far)&CB_data[0], LOWD(event[8]));
            }
        }
        else {
            Find((BYTE * far)&CB_data[0]);
        }
        return;

    case FT_ASOF: // Short off event
        for (i=0; i<8; i++)
            CB_data[i] = 0;

    case FT_ACOF:    // Off event
        if (learnlink) {            // in learn mode
            if (Unlearn) {          // unlean all events with this event number
                EraseEvent(&CB_data[0]);
            }
            else {                  // learn this event, using the switches and links
                LOWD(event[8]) = LEDSelect;
                if (Polarity)
                    event[8] |= 0x40;
                if (Toggle)
                    event[8] |= 0x80;
                SaveEvent((BYTE * far)&CB_data[0], LOWD(event[8]));
            }
        }
        else {
	        Find((BYTE * far)&CB_data[0]);
        }
        return;
    }
}

void DAA_Packet(void)
{
    far overlay BYTE i, t;

    if ((CB_data[0]&0xF0)==DAA_DATA) { // loader or event data
        if (DNID!=CB_SourceNID) {
           sendack(5,CB_SourceNID);
           return;
        }
        if (blocks != 0) { // loader data block
            t = CB_data[0];
            blocks &= bits[t];
            t = t * 7;
            for (i = 1; i<8 && t<64; i++)
                GP_block[t++] = CB_data[i];
            if (blocks == 0) {
                DisableLeds();
                ProgramMemoryWrite(GP_address, 64, (BYTE * far)GP_block);
                EnableLeds();
                sendack(0,DNID);    // OK
            }
        }
        else if (evdataexpected) { // event data
            SaveEvent(&event[0],CB_data[1]);
            evdataexpected = FALSE;
            sendack(0, CB_SourceNID);
        }
        return;
    }

    switch(CB_data[0]) {
    case DAA_UPGSTART:
        DisableLeds();
        INTCONbits.GIEH = 0;    // disable all interrupts          
        INTCONbits.GIEL = 0;
        Loader();               // call loader, never returns here

    case DAA_UPGREAD: // single block read
        sendblock(CB_SourceNID);
        break;

    case DAA_UPGADDR:  // single block write
        DNID = CB_SourceNID;
        UP(GP_address) = CB_data[1];
        HI(GP_address) = CB_data[2];
        LO(GP_address) = CB_data[3];
        blocks = 0x03FF;
        timer = 0;
        break;

    case DAA_EVREADH: // Event read
        DNID = CB_SourceNID;
        *(unsigned long *)&event[0] = *(unsigned long *)&CB_data[1];
        break;

    case DAA_EVREADL: // Event read
        DNID = CB_SourceNID;
        *(unsigned long *)&event[4] = *(unsigned long *)&CB_data[1];
        if (*(unsigned long *)&event[0] == 0 && *(unsigned long *)&event[4] == 0)
            ReadTable(CB_data[5]);
        else
            ReadEvent(&event[0],CB_data[5]);
        break;

    case DAA_EVERASEH: // Event erase
        DNID = CB_SourceNID;
        *(unsigned long *)&event[0] = *(unsigned long *)&CB_data[1];
        break;

    case DAA_EVERASEL: // Event erase
        DNID = CB_SourceNID;
        if (*(unsigned long *)&event[0] == 0 && *(unsigned long *)&event[4] == 0)
            EraseAllEvents();
        else
            EraseEvent(&event[0]);
        sendack(0, DNID);
        break;

    case DAA_EVWRITEH: // Event write
        DNID = CB_SourceNID;
        *(unsigned long *)&event[0] = *(unsigned long *)&CB_data[1];
        break;

    case DAA_EVWRITEL: // Event write
        DNID = CB_SourceNID;
        *(unsigned long *)&event[4] = *(unsigned long *)&CB_data[1];
        blocks = timer = 0;
        evdataexpected = TRUE;
        break;

    case DAA_NVRD: // Node variable read
        sendack(3, CB_SourceNID);
        break;

    case DAA_NVSET: // Node variable write byte
        sendack(4, CB_SourceNID);
        break;
    }
}


//*********************************************************************************
//        Main
//*********************************************************************************

void main(void)
{
    far overlay BYTE i,j;

//  Initialize
    ADCON1 = 0x0F;        // turn off A/D, all digital I/O
        
    TRISA = 0x3F;         // 6 bit LED select switch
    LATA = 0;
    TRISB = 0x09;         // RB0 spare, RB1 U6 /LE,  RB2 = CANTX, RB3 = CANRX, RB4 U6 CLK 
                          // RB5 U6 SDI, RB6,7 for debug and ICSP and diagnostic LEDs
    LATB = 0;
    PORTB = 0x04;         // CAN recessive
    TRISC = 0x1B;         // RC0 Learn, RC1 Unlearn, RC2 U5 enable, RC3 Toggle, RC4 Polarity
                          // RC6 U5 lsb row select, RC7 U5 msb row select
    LATC = 0;
    PORTC = 0x04;         // LED disable
    INTCON2 = 0;          // portb pullup enable

//  timer 0, 2.5 msec, in 2 usec steps, low priority interrupt
    T0CON = 0x82;         // set up timer0, 16 bit, prescale 1:8
    TMR0H = 0;            // clear timer
    TMR0L = 0;            // clear timer

    timer = 0;
    AllLedOnTest = 10;    // 1 sec at 100 msec
    ECANInitialize();

//  interrupts
    INTCON = 0x20;        // enable TMR0 interrupt
    INTCON2 = 0x04;       // Portb pullups enable, TMR0 high priority
    INTCON3 = 0;
    PIR1 = 0;
    PIR2 = 0;
    PIR3 = 0;
    PIE1 = 0x20;          // enable async receive interrupt
    PIE2 = 0;
    PIE3 = 0;
    IPR1 = 0;
    IPR2 = 0;
    IPR3 = 0;
    RCON = 0x80;          // enable priority levels

    BitMask[0] = 0x01;
    BitMask[1] = 0x02;
    BitMask[2] = 0x04;
    BitMask[3] = 0x08;
    BitMask[4] = 0x10;
    BitMask[5] = 0x20;
    BitMask[6] = 0x40;
    BitMask[7] = 0x80;
    
    learnlink = 0;
    LedsOn = TRUE;

    for (i=0; i<8; i++)
        HpLedColumn[i] = 0;

    INTCONbits.GIEH = 1;  // enable all high priority interrupts          
    INTCONbits.GIEL = 1;  // enable all low priority interrupts

    ProgramMemoryRead((unsigned long)&table[(unsigned int)i<<6], 64, (BYTE * far)GP_block);
    j = 0;
    for (i=0; i<64; i++)
        j |= GP_block[i];
    if (j == 0)
        EraseAllEvents();

    if (Unlearn) {        // power on erase all events
        wait(10);
        if (Unlearn) {    // debounce and erase all events
            EraseAllEvents();
            for (i=0; i<8; i++)
                HpLedColumn[i] = 0;
        }
    }

    Timer3Init();
    CheckAlias(0);

    // Main loop
    while(1) {

        // 100 msec timer
        if (Timer3Test()) { 
            timer++;
            if ((evdataexpected || blocks!=0) && timer>20) { // send timeout ack
                sendack(2, CB_SourceNID); // timeout
                blocks = 0;
                evdataexpected = FALSE;
            }
            if (Learn) { // debounce entry to learn mode
                if (learnlink++ > 2) { // in learn mode
                    learnlink = 3;
                    YellowLEDOn();
                    // light the led for the switch position
                    for (i=0; i<8; i++)
                        HpLedColumn[i] = 0;
                    i = LEDSelect;
                    HpLedColumn[(i>>3)&0x07] = BitMask[i&0x07];
                } 
            }
            else {    // exit learn mode
                learnlink = 0;
                YellowLEDOff();
                // ReadLedState();    // recover led state from eeprom
            }
            if (AllLedOnTest) {         // led startup test timer
                --AllLedOnTest;         // end of test when zero
            }
        }

        if (ECANReceiveMessage()) {
            if (CB_SourceNID == ND.nodeIdAlias) { // conflict
                if ((CB_FrameType&0x8000)==0x0000) { // CIM or RIM message
                    CB_SourceNID = ND.nodeIdAlias;
                    CB_FrameType = FT_RIM;
                    CB_datalen = 0;
                    while (SendMessage()==0) ;
                }
                else
                    CheckAlias(1);                  // get new alias
            }
            else if (CB_FrameType == (FT_DAA | ND.nodeIdAlias) )
                DAA_Packet();
            else
                Packet();
        }    
    }
}
