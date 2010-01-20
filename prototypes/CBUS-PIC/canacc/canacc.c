/*  OpenLCB for MERG CBUS CANACC4/CANACC5/CANACC8/CANACE8C

    15 Dec 2009

    ACC4 - CDU point outputs
    ACC8 (also ACC5) 8 outputs
    ACE8C - 8 inputs
    SERVO - 8 Servo's

    Must define conditional compilation for hardware/module type on the command line.

    For the servo or multifunction operation the ULN2803 driver chip is replaced by
    a DIL 1k resitor pack. The maximum output from 5v is 5ma, enough to drive an LED
    or servo but a transistor is needed to drive a relay or uncoupling magnet. Since
    the driver chip is not used the variable voltage regulator circuit is also not
    needed and can be omitted. For input operation there are no pull up resistors,
    and the 1k resistance limits the maximum input voltage to about 25 volts.

    For the SERVO code
    NodeVariable #0 to #7 the ON event servo position.
    NodeVariable #8 to #15 the OFF event servo position.

    For ACE8C
    NodeVariable #0 is the extra debounce time in 10 msec
    For all inputs, 2.5 seconds after power on, all the current input states are 
    sent as events.

    Event data is 8 bytes for the EventNumber, (6 bytes node id and 2 bytes event)
    and 2 bytes for the event action data. The first byte has 1 bit per output, 
    the bit is set if the event should change the output. The second byte also
    has 1 bit per output, and gives the polarity of the change. For normal
    polarity the on event sets the output on, and the off event sets the output
    off. For inverted polarity the opposite happens.

    Copyright (C) 2009 Mike Johnson

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
#ifndef ACE8C
#define EVENTSIZE 9
#define TABLESIZE 640 // 10
#include "../canlib/hash.c"
#endif

//*********************************************************************************
//    Forward refs
//*********************************************************************************

void hpinterrupt(void);
void lpinterrupt(void);
void Packet(void);
void DAA_Packet(void);
void EnableInterrupts(void);
void DisableInterrupts(void);
#ifdef SERVO
void servo_unlearn(void);
void servo_setup(void);
#endif
void InitRamFromEEPROM(void);

//*********************************************************************************
//    Definitions
//*********************************************************************************

#define SW_ON    0          // switch positions
#define SW_OFF   1

#ifdef ACC4
#define LEARN PORTBbits.RB4    // learn switch
#define UNLEARN PORTAbits.RA5  // unlearn / setup jumper
#define PUSHBTN PORTAbits.RA3  // Pushbutton
#define POLARITY PORTBbits.RB5 // pol switch
// Macro to unscramble output address from DIP switches
#define SelectionSwitches() (((PORTB & 0x03) << 1) |((PORTB&0x20)>>5))
#endif

#ifdef ACE8C
#define LEARN PORTAbits.RA4    // learn switch
#define UNLEARN PORTAbits.RA5  // unlearn / setup jumper
#define PUSHBTN PORTBbits.RB0  // Pushbutton
#define POLARITY PORTAbits.RA3 // pol switch
// Macro to unscramble output address from DIP switches
#define SelectionSwitches() (PORTA & 0x07)
#endif

#ifdef ACC8
#define LEARN PORTAbits.RA1    // learn switch
#define UNLEARN PORTAbits.RA0  // unlearn / setup jumper
#define PUSHBTN PORTAbits.RA2  // Pushbutton
#define POLARITY PORTBbits.RB5 // pol switch
// Macro to unscramble output address from DIP switches
#define SelectionSwitches() (((PORTB & 0x10)>>2)|(PORTB & 0x03))
#endif

#ifdef SERVO
#define LEARN PORTAbits.RA1    // learn switch
#define UNLEARN PORTAbits.RA0  // unlearn / setup jumper
#define PUSHBTN PORTAbits.RA2  // Pushbutton
#define POLARITY PORTBbits.RB5 // pol switch
// Macro to unscramble output address from DIP switches
#define SelectionSwitches() (((PORTB & 0x10)>>2)|(PORTB & 0x03))
#define NUM_SERVOS       8	     // max number of servos
#define ENDPOINT_1MS    -4000    // TMR1 delay for 1ms when current_output bit = 0
#define SERVO_MID       -6000    // TMR1 delay for midpoint
#define ENDPOINT_2MS    -8000    // TMR1 delay for 2ms when current_output bit = 1
#define SERVO_MIN       -(int)40
#endif

//*********************************************************************************
//    Ram
//*********************************************************************************

#pragma udata

#ifdef ACE8C
BYTE sendallbits;               // event action type 0
BYTE OldBuffer;                 // debounced scan data buffer
BYTE NewBuffer;                 // current scan buffer
BYTE scandelay;                 // debounce time
BYTE scancount;
volatile BYTE timer10ms;        // 10 msec timer for input scan
#endif

BYTE BitMask[8];                // bit masks

BYTE event[10];
BYTE eventcnt;
unsigned int eventindex;
unsigned int DNID;              // block transfer source NID
BYTE blocks;                    // loader block count
BYTE canTraffic;                // yellow led CAN traffic indicator

unsigned volatile int timer100;    // long period timer, inc every 100ms

#ifdef SERVO
unsigned volatile int timer_20ms; // 5sec time 250 x 20ms
BYTE next_portc;	              // used by hp interrupt to update portc 
BYTE new_output;                // next value of all the outputs
BYTE current_output;            // updated by hp int from new_output during idle period
BYTE servo_state;		        // interrupt state counter
BYTE servo_index;               // internal data for hp int
BYTE servo_mask;
int next_tmr;                   // next time for interrupt timer100
#pragma udata svo1
far BYTE servo_on[NUM_SERVOS];  // servo endpoint
far BYTE servo_off[NUM_SERVOS]; // servo opposite endpoint
#pragma udata
#endif

#ifdef ACC4
BYTE pulsetimer;                // delay after a pulse
BYTE pulseoutput;               // bits of pulse requests
BYTE pulseon;                   // set during a pulse
#endif

#pragma udata ovrly

//*********************************************************************************
//        ROM module info
//*********************************************************************************

#ifdef ACC4
#define modulestring "OpenLCB Output driver for CANACC4 " __DATE__ " " __TIME__
#endif

#ifdef ACC8
#define modulestring "OpenLCB Output driver for CANACC5/8 " __DATE__ " " __TIME__
#endif

#ifdef SERVO
#define modulestring "OpenLCB Servo Output Driver " __DATE__ " " __TIME__
#endif

#ifdef ACE8C
#define modulestring "OpenLCB Input Driver for CANACE8C " __DATE__ " " __TIME__
#endif

#ifdef MULTIFN
#define modulestring "OpenLCB Multifunction for CANACC5/8 " __DATE__ " " __TIME__
#endif

#pragma romdata

const rom BYTE xml[] = 
    "<XML>\r\n"
    "<NodeString>" modulestring "</NodeString>\r\n"
#ifdef ACC4
    "<EventData>\r\n"
      "<name>Event Number</name><bits>48</bits>\r\n"
      "<name>Output</name><bits>3</bits>\r\n"
    "</EventData>\r\n"
#endif
#ifdef SERV0
    "<EventData>\r\n"
      "<name>Event Number</name><bits>48</bits>\r\n"
      "<name>Output</name><bits>3</bits>\r\n"
      "<name>Polarity</name><bits>1</bits>\r\n"
    "</EventData>\r\n"
    "<NodeVariable>\r\n"
      "<name>Servo#0 off position</name><default>125</default>\r\n"
      "<name>Servo#1 off position</name><default>125</default>\r\n"
      "<name>Servo#2 off position</name><default>125</default>\r\n"
      "<name>Servo#3 off position</name><default>125</default>\r\n"
      "<name>Servo#4 off position</name><default>125</default>\r\n"
      "<name>Servo#5 off position</name><default>125</default>\r\n"
      "<name>Servo#6 off position</name><default>125</default>\r\n"
      "<name>Servo#7 off position</name><default>125</default>\r\n"
      "<name>Servo#0 on position</name><default>125</default>\r\n"
      "<name>Servo#1 on position</name><default>125</default>\r\n"
      "<name>Servo#2 on position</name><default>125</default>\r\n"
      "<name>Servo#3 on position</name><default>125</default>\r\n"
      "<name>Servo#4 on position</name><default>125</default>\r\n"
      "<name>Servo#5 on position</name><default>125</default>\r\n"
      "<name>Servo#6 on position</name><default>125</default>\r\n"
      "<name>Servo#7 on position</name><default>125</default>\r\n"
    "</NodeVariable>\r\n"
#endif
#ifdef ACE8C
    "<NodeVariable>\r\n"
      "<name>Debounce</name><default>0</default>\r\n"
    "</NodeVariable>\r\n"
#endif
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

#ifdef ACE8C
const rom BYTE PETable[128];		// 8 inputs x 2 states x 8 bytes
#endif

#pragma romdata

//*********************************************************************************
//    EEPROM
//*********************************************************************************

#pragma romdata eedata_scn=0xf00000

#ifdef SERVO
// Newly programmed part defaults to servo mid positions
// NV#0 to NV#7
rom BYTE SERVO_ON[NUM_SERVOS] = { 125, 125, 125, 125, 125, 125, 125, 125 };
// NV#8 to NV#15
rom BYTE SERVO_OFF[NUM_SERVOS] = { 125, 125, 125, 125, 125, 125, 125, 125 };
// NV#16 to NV#23
#endif

#ifdef ACE8C
rom BYTE DEBOUNCE = 0;
#endif

#pragma romdata

//*******************************************************************************
//        High priority interrupt
//*******************************************************************************

// At the start of the sequence, servo 0 output is set high and TMR1 loaded for
// a timeout between 1ms and 2ms acording to the desired servo position. 
// On the next interrupt, the servo output is cleared and TMR1 is loaded to
// complete the 2ms period.
// This sequence is repeated for all 8 servos (16ms) followed by 4ms idle time.
// If timer_20ms has expired then power off servos to prevent cheap servos
// from buzzing continuously and dissipating power in the regulator.
// When any servo state changes, all the servos are powered up.

#pragma interrupt hpinterrupt

#ifdef SERVO

void hpinterrupt(void) {

    PIR1bits.TMR1IF = 0;
    TMR1H = HI(next_tmr);
    TMR1L = LO(next_tmr);
    PORTC = next_portc;

    next_portc = 0;    // Turn off all servos next time

    if (servo_state < 16 && timer_20ms > 0) {
        if ((servo_state & 1) == 0) { // even states 0 - 14
            next_portc = servo_mask;  // turn on servo next time round
            // Pre-calculate next delay for servo position
            if (current_output & servo_mask) {
                LO(next_tmr) = servo_on[servo_index];
            } 
            else {
                LO(next_tmr) = servo_off[servo_index];
            }
            HI(next_tmr) = 0;
            next_tmr = (next_tmr<<4) - 8000;
        } 
        else { // odd states 1 - 15
            next_tmr = ENDPOINT_2MS - next_tmr; // Pre-calculate remaining delay
            if (next_tmr > SERVO_MIN) 
                next_tmr = SERVO_MIN;
            servo_mask = servo_mask<<1;
            servo_index++;
        }
    } 
    else { // not a servo, or timeslot 16 to 19
        next_tmr = ENDPOINT_1MS; // 1 msec
    }

    servo_state++;
    if (servo_state == 20) { // 20 ms timer
        servo_state = 0;
        servo_index = 0;
        servo_mask = 1;
        if (current_output != new_output) { // only update when idle
            current_output = new_output; 
            timer_20ms = 100; // 2 seconds
        }
        else if (timer_20ms > 0) { // 2 sec = 100 x 20 ms
            timer_20ms--;
        }
    } 
}

#else

void hpinterrupt(void) {
}

#endif

//*********************************************************************************
//        Low priority interrupt
//*********************************************************************************

#pragma interruptlow lpinterrupt
void lpinterrupt(void)
{
}

//*********************************************************************************
//    Consumer Events
//*********************************************************************************

void DoEvent(static BYTE * far ev)
{
    far overlay BYTE e = ev[0];
    far overlay BYTE output = BitMask[e&0x07];

#ifdef ACC4
    switch (e&0x07) {
    case 0:  // 1A
        pulseoutput |= 0x01;
        pulseoutput &= ~0x80;
        break;
    case 1: // 1B  
        pulseoutput |= 0x80;
        pulseoutput &= ~0x01;
        break; 
    case 2: // 2A
        pulseoutput |= 0x02;
        pulseoutput &= ~0x40;
        break; 
    case 3: // 2B  
        pulseoutput |= 0x40;
        pulseoutput &= ~0x02;
        break; 
    case 4: // 3A  
        pulseoutput |= 0x04;
        pulseoutput &= ~0x20;
        break; 
    case 5: // 3B  
        pulseoutput |= 0x20;
        pulseoutput &= ~0x04;
        break; 
    case 6: // 4A
        pulseoutput |= 0x08;
        pulseoutput &= ~0x10;
        break; 
    case 7: // 4B
        pulseoutput |= 0x10;
        pulseoutput &= ~0x08;
        break; 
    }
#endif

#ifdef ACC8
    if (e & 0x08) { // on
        PORTC |= output; // set output bit
    }
    else { // off
        PORTC &= ~output; // clear output bit
    }
    if (e & 0x10) { // toggle next opposite way
        if (e & 0x08) { // on
            PORTC &= ~(output<<1); // set output bit
        }
        else { // off
            PORTC |= (output<<1); // clear output bit
        }
    }
#endif

#ifdef SERVO
    if (e & 0x08) { // on
        new_output |= output; // set output bit
    }
    else { // off
        new_output  &= ~output; // clear output bit
    }
#endif

}

//*********************************************************************************
//    Scan and Producer Events
//*********************************************************************************

#ifdef ACE8C

void PESetDefault(void)
{
    far overlay BYTE a, i,j;
    
    for (a=0; a<128; a+=64)
    {
        for (j=0; j<64; j+=8) {
            for (i=0; i<6; i++)
                GP_block[j+i] = ND.nodeId[5-i];
            GP_block[j+6] = 0;
            GP_block[j+7] = (a+j)>>3; 
        }
        ProgramMemoryWrite((unsigned short long)&PETable[a],64,(BYTE * far)&GP_block[0]);
    }
}

void PEReadEvent(unsigned int evno)
{
    far overlay BYTE a, i;
    if (evno>=16) {
        sendack(ACK_NODATA, CB_SourceNID);
        return;
    }
    a = LO(evno)<<3;
    i = LO(a) & 0x3F;
    ProgramMemoryRead((unsigned short long)&PETable[a&0x40],64,(BYTE * far)&GP_block[0]);
    CB_FrameType = FT_DAA | CB_SourceNID;
    CB_SourceNID = ND.nodeIdAlias;
    CB_data[0] = DAA_PEWRITEH;
    CB_data[1] = GP_block[i];
    CB_data[2] = GP_block[i+1];
    CB_data[3] = GP_block[i+2];
    CB_data[4] = GP_block[i+3];
    CB_data[5] = GP_block[i+4];
    CB_data[6] = GP_block[i+5];
    CB_data[7] = GP_block[i+6];
    CB_datalen = 8;
    while (SendMessage()==0) ;
    CB_data[0] = DAA_PEWRITEL;
    CB_data[1] = GP_block[i+7];
    CB_data[2] = HI(evno);
    CB_data[3] = LO(evno);
    CB_datalen = 4;
    while (SendMessage()==0) ;
}

// event stored in 8 bytes of event global

void PEWriteEvent(unsigned int evno)
{
    far overlay BYTE a, i, j;
    if (evno>=16) {
        sendack(ACK_NOSPACE, CB_SourceNID);
        return;
    }
    a = LO(evno)<<3;
    i = LO(a) & 0x3F;
    ProgramMemoryRead((unsigned short long)&PETable[a&0x40],64,(BYTE * far)&GP_block[0]);
    for (j=0; j<8; j++)
        GP_block[i+j] = event[j];
    ProgramMemoryWrite((unsigned short long)&PETable[a&0x40],64,(BYTE * far)&GP_block[0]);
    sendack(ACK_OK,DNID);
}

// overwrite with all 0xFF

void PEEraseEvent(unsigned int evno)
{
    far overlay BYTE i;
    event[0] = 0xFF;
    event[1] = 0xFF;
    event[2] = 0xFF;
    event[3] = 0xFF;
    event[4] = 0xFF;
    event[5] = 0xFF;
    event[6] = 0xFF;
    event[7] = 0xFF;
    PEWriteEvent(evno);
}

// scan and debounce inputs
// return true if a CAN packet is sent, so it can also be acted upon in this module
BOOL scan(void)
{
    far overlay BYTE Row;          // row data for switch scan
    far overlay BYTE Bitcng;       // bit change in scan
    far overlay BYTE Bitcnt;       // bit counter in scan
    far overlay BYTE i, j;
    far overlay BOOL t;

    Row = PORTC;
    Bitcng = ~(Row ^ NewBuffer) & (Row ^ OldBuffer);
    NewBuffer = Row;
    for (Bitcnt=0; Bitcng!=0; Bitcnt++) {
        if ((BitMask[Bitcnt] & Bitcng) != 0) {
            Bitcng &= ~BitMask[Bitcnt];
            j = Bitcnt<<4;
            if ((Row & BitMask[Bitcnt]) == 0) 
                j += 8;
            CB_SourceNID = ND.nodeIdAlias;
            CB_FrameType = FT_EVENT;
            CB_datalen = 8;
            t = FALSE;
            for (i=0; i<8; i++) {
                CB_data[i] = PETable[j+i];
                if (CB_data[i]!=0xFF)
                    t = TRUE;
            }
            if (t) {
                if (ECANSendMessage()==0)   
                    return FALSE;
            }
            OldBuffer ^= BitMask[Bitcnt];
            return TRUE;
        }
    }
    return FALSE;
}

#endif

//*********************************************************************************
//  Main packet handling is here
//*********************************************************************************

rom unsigned int bits[10] = {
    0x03FE, 0x03FD, 0x03FB, 0x03F7, 0x03EF, 0x03DF, 0x03BF, 0x037F, 0x02FF, 0x01FF
};

void Packet(void)
{
    far overlay BYTE i;

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
    else if (CB_FrameType == FT_EVENT) {
       canTraffic = 1;
#ifndef ACE8C
       if (LEARN == SW_ON) {            // in learn mode
            if (UNLEARN==SW_ON) {        // unlean all events with this event number
                EraseEvent(&CB_data[0]);
            }
            else {                  // learn this event, using the switches and links
                event[0] = CB_data[0];
                event[1] = CB_data[1];
                event[2] = CB_data[2];
                event[3] = CB_data[3];
                event[4] = CB_data[4];
                event[5] = CB_data[5];
                event[6] = CB_data[6];
                event[7] = CB_data[7];
                LOWD(event[8]) = SelectionSwitches();
#ifdef ACC8
                if (POLARITY==SW_ON)
                    event[8] |= 0x08;
#endif
#ifdef SERVO
                if (POLARITY==SW_ON)
                    event[8] |= 0x08;
#endif
                SaveEvent((BYTE * far)&event[0]);
                DoEvent((BYTE * far)&event[8]);
            }
        }
        else {
            Find((BYTE * far)&CB_data[0]);
        }
#endif
/*
        // send debug packet
        CB_SourceNID = ND.nodeIdAlias;
        CB_FrameType = 0xF000;
        CB_datalen = 4;
        CB_data[0] = LEARN;
        CB_data[1] = UNLEARN;
        CB_data[2] = SelectionSwitches();
        CB_data[3] = PUSHBTN;
        while (SendMessage()==0) ;
*/
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

    case DAA_DEFAULT:
#ifdef ACE8C
        PESetDefault();
#endif
        sendack(ACK_OK, CB_SourceNID);
        break;

    case DAA_PEERASE:
#ifdef ACE8C
        event[0] = 0xFF;
        event[1] = 0xFF;
        event[2] = 0xFF;
        event[3] = 0xFF;
        event[4] = 0xFF;
        event[5] = 0xFF;
        event[6] = 0xFF;
        event[7] = 0xFF;
        PEWriteEvent(((unsigned int)CB_data[1]<<8) | CB_data[2]);
#else
        sendack(ACK_OK, CB_SourceNID);
#endif
        break;

    case DAA_PEREAD:
#ifdef ACE8C
        PEReadEvent(((unsigned int)CB_data[1]<<8) | CB_data[2]);
#else
        sendack(ACK_NODATA, CB_SourceNID);
#endif
        break;

    case DAA_PEWRITEH:
#ifdef ACE8C
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
            timer100 = 0;
            return;
        }
        eventcnt = 0;
        if (DNID != CB_SourceNID) {
            sendack(ACK_ALIASERROR, CB_SourceNID);
            sendack(ACK_ALIASERROR, DNID);
            return;
        }
        PEWriteEvent(eventindex);
#else
        sendack(ACK_NOSPACE, CB_SourceNID);
#endif
        break;

    case DAA_NVREAD: // Node variable read
#ifdef SERVO
        if (CB_data[1] <= 26) {
            tmp = EEPROMRead(CB_data[1]); 
            CB_FrameType = FT_DAA | CB_SourceNID;
            CB_SourceNID = ND.nodeIdAlias;
            CB_datalen = 3;
            CB_data[0] = DAA_NVREPLY;
            // CB_data[1] = CB_data[1];
            CB_data[2] = tmp;
            while (ECANSendMessage()==0) ;
            return;
        }
#endif

#ifdef ACE8C
        if (CB_data[1] == 0) {
            tmp = EEPROMRead(CB_data[1]); 
            CB_FrameType = FT_DAA | CB_SourceNID;
            CB_SourceNID = ND.nodeIdAlias;
            CB_datalen = 3;
            CB_data[0] = DAA_NVREPLY;
            // CB_data[1] = CB_data[1];
            CB_data[2] = tmp;
            while (ECANSendMessage()==0) ;
            return;
        }
#endif
        sendack(ACK_NODATA, CB_SourceNID); 
        break;

    case DAA_NVWRITE: // Node variable write byte
#ifdef SERVO
        if (CB_data[1] <= 26) {
            EEPROMWrite(CB_data[1], CB_data[2]);
            InitRamFromEEPROM();
            sendack(ACK_OK, CB_SourceNID);
            return;
        }
#endif
#ifdef ACE8C
        if (CB_data[1] == 0) {
            scandelay = CB_data[2];
            EEPROMWrite(CB_data[1], scandelay);
            sendack(ACK_OK, CB_SourceNID);
            return;
        }
#endif
        sendack(ACK_NOSPACE, CB_SourceNID);
        break;


    case DAA_CEREADH: // Event read
#ifdef ACE8C
        sendack(ACK_NODATA, CB_SourceNID); 
#else
        event[0] = CB_data[1];
        event[1] = CB_data[2];
        event[2] = CB_data[3];
        event[3] = CB_data[4];
        event[4] = CB_data[5];
        event[5] = CB_data[6];
        event[6] = CB_data[7];
        goto CER;

    case DAA_CEREADL: // Event read
        event[7] = CB_data[1];
        HI(eventindex) = CB_data[2];
        LO(eventindex) = CB_data[3];
CER:
        if (eventcnt==0) {
            DNID = CB_SourceNID;
            eventcnt++;
            timer100 = 0;
            return;
        }
        eventcnt = 0;
        if (DNID != CB_SourceNID) {
            sendack(ACK_ALIASERROR, CB_SourceNID);
            sendack(ACK_ALIASERROR, DNID);
            return;
        }
        ReadEvent(&event[0], eventindex);
#endif
        break;

    case DAA_CEERASEH: // Event erase
#if !defined(ACE8C)
        event[0] = CB_data[1];
        event[1] = CB_data[2];
        event[2] = CB_data[3];
        event[3] = CB_data[4];
        event[4] = CB_data[5];
        event[5] = CB_data[6];
        event[6] = CB_data[7];
        goto CEE;

    case DAA_CEERASEL: // Event erase
        event[7] = CB_data[1];
CEE:
        if (eventcnt==0) {
            DNID = CB_SourceNID;
            eventcnt++;
            timer100 = 0;
            return;
        }
        eventcnt = 0;
        if (DNID != CB_SourceNID) {
            sendack(ACK_ALIASERROR, CB_SourceNID);
            sendack(ACK_ALIASERROR, DNID);
            return;
        }
        EraseEvent(&event[0]);
        sendack(ACK_OK, DNID);
#else
        sendack(ACK_NOSPACE, CB_SourceNID); 
#endif
        break;

    case DAA_CEWRITEH: // Event write
#if !defined(ACE8C)
        event[0] = CB_data[1];
        event[1] = CB_data[2];
        event[2] = CB_data[3];
        event[3] = CB_data[4];
        event[4] = CB_data[5];
        event[5] = CB_data[6];
        event[6] = CB_data[7];
        goto CEW;

    case DAA_CEWRITEL: // Event write
        event[7] = CB_data[1];
        event[8] = CB_data[3]; // data
CEW:
        if (eventcnt==0) {
            DNID = CB_SourceNID;
            eventcnt++;
            timer100 = 0;
            return;
        }
        eventcnt = 0;
        if (DNID != CB_SourceNID) {
            sendack(ACK_ALIASERROR, CB_SourceNID);
            sendack(ACK_ALIASERROR, DNID);
            return;
        }
        SaveEvent(event);
        sendack(ACK_OK, CB_SourceNID);
#else
        sendack(ACK_NOSPACE, CB_SourceNID); 
#endif
        break;
    }
}

//*******************************************************************************
//    Start and stop routines
//*******************************************************************************

// These routines get called whenever a slow operation needs to be done
// such as program memory write. Or if the program is to be stopped with
// the power on such as when the loader is called.

// DisableInterrupts - This should stop any pulsed outputs and disable
// interrupts in a safe way. 
// For servos this has to wait until the end of an output pulse
// For LEDs and DCC this can just turn off the power

void DisableInterrupts(void)
{
#ifdef SERVO
    while (servo_state & 1) ; // wait until not pulsing a servo
    INTCONbits.GIEH = 0;     // disable all high priority interrupts          
    INTCONbits.GIEL = 0;     // disable all low priority interrupts
#endif
}

// EnableInterrupts - This should restart the interrupts in a safe way.
// For LEDs and servos this can just restart the interrupts,
// for DCC it has to restart from the beginning of a packet.

void EnableInterrupts(void)
{
#ifdef SERVO
    INTCONbits.GIEH = 1;    // enable all high priority interrupts          
    INTCONbits.GIEL = 1;    // enable all low priority interrupts
#endif
}

//*******************************************************************************
//    Servo routines
//*******************************************************************************

#ifdef SERVO

// Set default servo endpoints

void servo_unlearn(void) {
    far overlay BYTE i;

    for (i = 0; i<NUM_SERVOS; i++) {
        EEPROMWrite((int)&SERVO_ON[i], SERVO_MID);
        EEPROMWrite((int)&SERVO_OFF[i], SERVO_MID);
    }
}

/*
 * Set servo endpoints
 *
 * Arrive here if UNLEARN & LEARN both on during power up. Once here there's no
 * way out other than to change the learn and unlearn switches and cycle the power.
 *
 * Endpoints are "nearer 1ms" and "nearer 2ms" corresponding to OFF & ON events.
 * Polarity can be used during event learning to effectively reverse the 
 * endpoints.
 * 
 * Once here, the UNLEARN & LEARN switches select the setting mode:
 * UNLEARN     LEARN
 *    ON        ON        Select endpoint nearest 2ms
 *    ON        OFF       Select endpoint nearest 1ms
 *    OFF       ON        Save servo position to EEPROM
 *    OFF       OFF       Save servo position to EEPROM
 *
 *        Set 0 - 2 select the output to be adjusted
 *        POL selects the direction: OFF = towards 1ms,
 *            ON = towards 2ms
 *        S1 ON performs the selected action
 *
 * By convention, the values stored in SERVO_ON should be nearer 1ms and the values
 * in SERVO_OFF should be nearer 2ms but this convention is not enforced so servo
 * movement could be reversed. Servo movement can also be reversed by using the POL
 * switch when learning an event.
 */

void servo_setup(void) {
    far overlay BYTE servo = SelectionSwitches();
    far overlay BYTE * far servo_ptr;
    far overlay BYTE addr;
    far overlay BYTE saved = 8;
    far overlay unsigned int count;

    while (1) { // Determine mode
        if (UNLEARN==SW_ON) {
            // ISR updates position
            if (LEARN == SW_ON) { // Nearer 2ms endpoint
                servo_ptr = (BYTE far *)&servo_off[servo];
                new_output |= BitMask[servo];
            } 
            else { // Nearer 1ms endpoint
                servo_ptr = (BYTE far *)&servo_on[servo];
                new_output &= ~BitMask[servo];
            }
	        if (PUSHBTN == SW_ON) { // Adjust servo position
	            if (POLARITY == SW_ON) { // Towards 2ms is longer (more negative) delay value
	                *servo_ptr--;
	                if (*servo_ptr == 0) { 
	                    *servo_ptr = 1;
	                }
	            } 
	            else { // Towards 1ms is inter (more positive) delay value
	                *servo_ptr++;
	                if (*servo_ptr == 250) { 
	                    *servo_ptr = 249;
	                }
	            }
	            // flag not saved
	            saved = 8;
	            // Keep servo powered for 1s
	            timer_20ms = 50;
	            // delay if switch kept down so servo doesn't move too fast
	            for (count=250; (count>0) && (PUSHBTN == SW_ON); count--)  ; 
	        }
        }
        else { // UNLEARN == SW_OFF
            if (PUSHBTN == SW_ON) { // Check if we've already saved it
                if (saved != servo) { // Store current setting in EEPROM
                    if (LEARN == SW_ON) { // Nearer 2ms endpoint
                        addr = (BYTE)&SERVO_OFF[servo];
                    } 
                    else { // Nearer 1ms endpoint
                        addr = (BYTE)&SERVO_ON[servo];
                    }
                    EEPROMWrite(addr, *servo_ptr);
                    saved = servo;
                }
            }
        }
        // refresh which servo being adjusted
        servo = SelectionSwitches();
    }
}

#endif

//*********************************************************************************
//        MAIN
//*********************************************************************************

void InitRamFromEEPROM(void)
{
#ifdef SERVO
    far overlay BYTE i;
    for (i=0; i<8; i++) {
        servo_on[i] = EEPROMRead((int)&SERVO_ON[i]);
        servo_off[i] = EEPROMRead((int)&SERVO_OFF[i]);
    }
#endif
}

void main(void) {
    far overlay BYTE i, j, k;
    far overlay BOOL t;

    INTCON = 0;
    ADCON0 = 0;
    ADCON1 = 0b00001111;
#ifdef ACC4
    TRISA =  0b00101000;    // Port A 5 is unlearn, 3 = S1
#else
    TRISA =  0b00000111;    // Port A 0 is unlearn, 1 is polarity, 2 = S1
#endif
    // RB0,1 logic inputs,  RB2 = CANTX, RB3 = CANRX, RB4,5 are logic input 
    // RB6,7 for debug, ICSP and diagnostics LEDs
    TRISB = 0b00111011;
    PORTB = 0;
    PORTBbits.RB2 = 1;            // CAN recessive
#ifdef ACE8C
    TRISC = 0xFF;                 // initially all inputs
#else
    TRISC = 0x00;                 // initially all outputs
#endif
    PORTC = 0;                    // all outputs/servo's off
    RCONbits.IPEN = 1;            // enable interrupt priority levels
    EECON1 = 0;

    ECANInitialize();        

    IPR3 = 0;                    // All IRQs low priority
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

    BitMask[0] = 0x01;
    BitMask[1] = 0x02;
    BitMask[2] = 0x04;
    BitMask[3] = 0x08;
    BitMask[4] = 0x10;
    BitMask[5] = 0x20;
    BitMask[6] = 0x40;
    BitMask[7] = 0x80;

    InitRamFromEEPROM();

#ifdef SERVO
    new_output = current_output = 0;
    servo_state = 19;
    next_portc = 0;
    // Setup TMR1 for servo pulse timing, 16MHz Fosc = 4MHz Fcyc, 1:1 prescale
    TMR1H = ((int)-4000) >> 8;
    TMR1L = (int)-4000;
    T1CON = 0b10000000;

    // Enable TMR1 interrupts at high priority
    T1CONbits.TMR1ON = 1;
    IPR1bits.TMR1IP = 1;
    PIE1bits.TMR1IE = 1;
    EnableInterrupts();		// enable interrupts
    timer_20ms = 100;         // Power up servos for 2s
#endif

#ifdef ACE8C
//  timer 1 for 10 msec overflow
    T1CON = 0x81;         // enable timer1, 16 bit mode, no prescaler
    TMR1H = (0x10000 - 40000) >> 8;    // 4 * 40000 / 16,000,000 = 10 msec
    TMR1L = (0x10000 - 40000) & 0xFF;
    IPR1bits.TMR1IP = 1;
    PIE1bits.TMR1IE = 1;
    timer10ms = 0;
    scancount = scandelay = EEPROMRead((unsigned int)&DEBOUNCE);
    sendallbits = 0xFF;
    OldBuffer = PORTC;
#endif

    CheckAlias(0);
    GreenLEDOn();
    YellowLEDOff();
    canTraffic = 0;

#ifndef ACE8C
    // all zero after 1st programming chip
    ProgramMemoryRead((unsigned long)&table[0], 64, (BYTE * far)GP_block);
    j = 0;
    for (i=0; i<64; i++)
        j |= GP_block[i];
    if (j == 0) {
        EraseEvent(event);
    }

    // Test for power up mode, Unlearn switch on, learn switch off
    if ((LEARN == SW_OFF) && (UNLEARN == SW_ON)) {
        event[0] = 0;
        event[1] = 0;
        event[2] = 0;
        event[3] = 0;
        event[4] = 0;
        event[5] = 0;
        event[6] = 0;
        event[7] = 0;
        event[8] = 0;
        event[9] = 0;
        EraseEvent(event);
#ifdef SERVO
        if (PUSHBTN == SW_ON) { // Unlearn all servo settings
            servo_unlearn();
        }
#endif
    }
#endif

    // Unlearn switch on, learn switch on
    // Enter servo setting mode
/*
    if ((LEARN == SW_ON) && (UNLEARN == SW_ON)) {
        servo_setup();
    }
*/
    timer100 = 0;
    eventcnt = 0;

    // send INIT packet
    CB_SourceNID = ND.nodeIdAlias;
    CB_FrameType = FT_INIT;
    CB_datalen = 0;
    while (SendMessage()==0) ;

    // Simple loop looking for a received CAN frame
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

#ifdef ACC4
            if (pulseon) { // end sending a pulse 100 msec wide
                PORTC = 0;
                pulseon = 0;
                pulsetimer = 5; // 600 msec recharge delay
            }
            else if (pulsetimer) { // 600 msec delay after a pulse
                pulsetimer--;
            }
            else if (pulseoutput) { // select another to pulse
                for (i=0; i<8; i++) {
                    if (pulseoutput&BitMask[i]) {
                        pulseoutput &= ~BitMask[i]; // clear request bit 
                        PORTC |= BitMask[i];        // set output
                        pulseon = BitMask[i];
                        break;                        
                    }
                }
            }
#endif
        }

#ifdef ACE8C 
        // 10 msec timer
        if (PIR1bits.TMR1IF) { 
            PIR1bits.TMR1IF = 0;            // reload the timer for 10 msec
            TMR1H = (0x10000 - 40000) >> 8; // 4 * 40000 / 16,000,000 = 10 msec
            TMR1L = (0x10000 - 40000) & 0xFF;
            timer10ms++;

            // call scan every (scandelay+1) x 10msec 
            if (scancount==0) {       
                scan();
                scancount = scandelay;
            }
            else
                scancount--;

            // send every switch state event after 2.5 sec start up delay 
            if (timer10ms>0x7F && sendallbits!=0) { // (start of day)
                for (i=0; i<8; i++) {
                    if ((BitMask[i]&sendallbits)!=0) {
                        CB_SourceNID = ND.nodeIdAlias;
                        CB_FrameType = FT_EVENT;
                        CB_datalen = 8;
                        j = i << 4;
                        if ((OldBuffer & BitMask[i])==0)
                            j += 8;
                        t = FALSE;
                        for (k=0; k<8; k++) {
                            CB_data[k] = PETable[j+k];
                            if (CB_data[k]!=0xFF)
                                t = TRUE;
                        }
                        if (t) {
                            if (ECANSendMessage()==0)   
                                break;
                        }
                        sendallbits &= ~BitMask[i];
                        break;
                    }
                }
            }
        }
#endif

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
