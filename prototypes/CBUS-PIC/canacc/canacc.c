/*  OpenLCB for MERG CBUS CANACC4/CANACC5/CANACC8/CANACE8C

    30 March 2010

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
#include "../canlib/ecan.c"
#include "../canlib/entry.c"
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
void servo_setup(void);
void InitRamFromEEPROM(void);
#endif

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
#endif

//*********************************************************************************
//    Ram
//*********************************************************************************

#pragma udata

#ifdef ACE8C
BYTE sendallbits;               // event action type 0
BYTE OutputState;               // debounced scan data buffer
BYTE PrevInput1;                // previous scan buffer
BYTE PrevInput2;                // previous previous scan buffer
BYTE PrevInput3;                // previous previous previous scan buffer
BYTE scandelay;                 // debounce time
BYTE scancount;
volatile BYTE timer15ms;        // 15 msec timer for input scan
volatile int timer13us;         // 13 usec timer count for input scan
volatile BYTE timerflag;		// scan flag
BYTE event[8];
#endif

BYTE BitMask[8];                // bit masks

unsigned int DNID;              // block transfer source NID
BYTE dgcnt;
BYTE canTraffic;                // yellow led CAN traffic indicator

#ifdef SERVO
BYTE next_portc;	            // used by hp interrupt to update portc 
BYTE new_output;                // next value of all the outputs
BYTE servo_state;		        // interrupt state counter
BYTE servo_index;               // index of servo to pulse
int next_tmr;                   // next time for interrupt
BYTE pulsetimer;                // timer for pulsing
BYTE pulseoutput;               // bits of pulse requests
BYTE pulseon;                   // set while pulsing
#pragma udata svo1
far char servopos[16];          // servo endpoint
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

#pragma romdata

const rom BYTE xml[] = 
    "<cdi><id><software>" modulestring "</software></id>"
    "<se na=\"Location\" or=\"#0080\" sp=\"#FE\" bu=\"#303\">"
      "<ch na=\"Location\" si=\"64\"/>"
    "</se>"
    "<se na=\"Node Id\" or=\"#0040\" sp=\"#FE\" bu=\"#343\">"
      "<in na=\"Serial\" si=\"1\"/>"
      "<in na=\"Member\" si=\"3\"/>"
      "<by na=\"Group\" si=\"2\"/>"
    "</se>"
#ifdef ACC4
    "<se na=\"Events\" bu=\"#323\">"
      "<gr rep=\"100\">"
      "<by na=\"Event Number\" si=\"8\"/>"
      "<by na=\"Action\" si=\"1\"/>"
      "</gr>"
    "</se>"
#endif
#ifdef ACC8
    "<se na=\"Events\" bu=\"#323\">"
      "<gr rep=\"100\">"
      "<by na=\"Event Number\" si=\"8\"/>"
      "<by na=\"Action\" si=\"1\"/>"
      "</gr>"
    "</se>"
#endif
#ifdef SERVO
    "<se na=\"Events\" bu=\"#323\">"
      "<gr rep=\"100\">"
        "<by na=\"Event Number\" si=\"8\"/>"
        "<by na=\"Action\" si=\"1\"/>"
      "</gr>"
    "</se>"
    "<se na=\"Servo posn.\" sp=\"1\" bu=\"#323\">"
      "<gr rep=\"8\">"
        "<int na=\"Servo off\" si=\"1\"/>"
        "<int na=\"Servo on\" si=\"1\"/>"
      "</gr>"
    "</se>"
#endif
#ifdef ACE8C
    "<se na=\"Events\" bu=\"#323\">"
      "<gr rep=\"8\">"
        "<by na=\"Off Event Number\" si=\"8\"/>"
        "<by na=\"On Event Number\" si=\"8\"/>"
      "</gr>"
    "</se>"
    "<se na=\"Debounce\" sp=\"1\" bu=\"#323\">"
      "<by na=\"Debounce\" si=\"1\"/>"
    "</se>"
#endif
    "</cdi>";

#pragma romdata module = 0x001020
const rom BYTE valid = 0;         // tmp set to 0xFF by PC side of loader. 

#ifdef ACE8C
#pragma romdata events = 0x001040
const rom BYTE EventTable[128];   // 8 inputs x 2 states x 8 bytes
#endif

//*********************************************************************************
//    EEPROM
//*********************************************************************************

#pragma romdata eedata_scn=0xf00000

#ifdef SERVO
rom BYTE SERVOPOS[16];
#endif

#ifdef ACE8C
rom BYTE DEBOUNCE;
#endif

#pragma romdata

//*******************************************************************************
//        High priority interrupt
//*******************************************************************************

// Send a pulse to one servo every 50ms

#pragma interrupt hpinterrupt

#ifdef SERVO

// Timer1 is 2000 counts per msec
// only 1 servo at a time
void hpinterrupt(void) {

    PIR1bits.TMR1IF = 0;
    TMR1H = HI(next_tmr);
    TMR1L = LO(next_tmr);
    PORTC = next_portc;

    next_portc = 0;    // Turn off all servos next time

    if (pulseon) {
        if (servo_state == 0) {
            next_portc = pulseon;     // turn on servo next time round
            // Pre-calculate next delay for servo position
            HI(next_tmr) = 0;
            if (new_output & pulseon) {
                LO(next_tmr) = servopos[(servo_index<<1)+1];
            } 
            else {
                LO(next_tmr) = servopos[servo_index<<1];
            }
            next_tmr = (0-3000) - (next_tmr<<3);
            servo_state = 1;
        } 
        else { // odd state 1
            next_tmr = (-40000) - next_tmr; // Pre-calculate remaining part of 20 msec
            servo_state = 0;
            if (--pulsetimer==0)
                pulseon = 0;
        }
    } 
    else { // not a servo, or timeslot 16 to 19
        next_tmr = -40000; // 20 msec
    }
}

#else
#ifdef ACE8C

void hpinterrupt(void) {
    TMR1H = (0x10000 - 32) >> 8; // 4 * 52 / 16,000,000 = 13 usec for 38kHz
    TMR1L = (0x10000 - 32) & 0xFF;
    PIR1bits.TMR1IF = 0;
    timer13us--;
    if (timer13us==0) {
        timer13us = 15000/13;
        timerflag++;
    }
}

#else

void hpinterrupt(void) {
}

#endif
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
    pulseoutput |= output;
    if (e & 0x08) { // on
        new_output |= output; // set output bit
    }
    else { // off
        new_output  &= ~output; // clear output bit
    }
#endif

}

//*********************************************************************************
//    Setdefault
//*********************************************************************************

void SetDefault(void)
{
#ifdef ACE8C
    far overlay BYTE a, i,j;
    scandelay = 0;
    EEPROMWrite((int)&DEBOUNCE, 0);
    for (a=0; a<128; a+=64)
    {
        for (j=0; j<64; j+=8) {
            GP_block[j] = (a+j)>>3; 
            GP_block[j+1] = 0;
            for (i=0; i<6; i++)
                GP_block[j+i+2] = ND.nodeId[i];
        }
        ProgramMemoryWrite((unsigned short long)&EventTable[a],64,(BYTE * far)&GP_block[0]);
    }
#endif
#ifdef SERVO
    far overlay BYTE i;
    EraseAll();
    event[1] = 0;
    for (i=0; i<6; i++)
        event[i+2] = ND.nodeId[i];
    for (i=0; i<16; i++) {
        event[0] = i;
        event[8] = (i>>1) | ((i&1)<<3);
        SaveEvent((BYTE * far)event);
    }
    for (i=0; i<16; i++) {
        servopos[i] = 0;
        EEPROMWrite((int)&SERVOPOS[i], 0);
    }
#endif
#ifdef ACC8
    far overlay BYTE i;
    EraseAll();
    event[1] = 0;
    for (i=0; i<6; i++)
        event[i+2] = ND.nodeId[i];
    for (i=0; i<16; i++) {
        event[0] = i;
        event[8] = (i>>1) | ((i&1)<<3);
        SaveEvent((BYTE * far)event);
    }
#endif
}

//*********************************************************************************
//    INPUT module code
//*********************************************************************************

#ifdef ACE8C

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
    Bitcng = ~(Row ^ PrevInput1) & ~(Row ^ PrevInput2) & ~(Row ^ PrevInput3) & (Row ^ OutputState);
    PrevInput3 = PrevInput2;
    PrevInput2 = PrevInput1;
    PrevInput1 = Row;
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
                CB_data[i] = EventTable[j+7-i];
                if (CB_data[i]!=0xFF)
                    t = TRUE;
            }
            if (t) {
                if (ECANSendMessage()==0)   
                    return FALSE;
            }
            OutputState ^= BitMask[Bitcnt];
            return TRUE;
        }
    }
    return FALSE;
}

#endif

//*********************************************************************************
//  Main packet handling is here
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
    }
}

void DatagramPacket(void)
{
    far overlay BYTE i;
    if (DNID == -1)
        DNID = CB_SourceNID;
    else if (DNID != CB_SourceNID) {
        sendnack(CB_SourceNID,99);
        return;
    }
    for (i=0; i<CB_datalen && dgcnt<72; i++)
        GP_block[dgcnt++] = CB_data[i];
    if ((CB_FrameType&0xF000) == FT_DGL || (CB_FrameType&0xF000) == FT_DGS) { // end of datagram
        DNID = -1;
        dgcnt = 0;
        if (GP_block[0] == DG_MEMORY) {
            UP(GP_address) = GP_block[3];
            HI(GP_address) = GP_block[4];
            LO(GP_address) = GP_block[5];
            if (GP_block[1] == DGM_WRITE) {
                // write data
#ifndef ACE8C
                if (GP_block[6] == 0) { // event data
                    WriteAllEvents(GP_address);
                    sendack(CB_SourceNID);
                    return;
                }
#endif
#ifdef ACE8C
                if (GP_block[6] == 0) { // event data
                    GP_address += (unsigned short long)&EventTable[0];
                }
#endif
                if (GP_block[6] == 0xFE || GP_block[6] == 0 ) {
                    ProgramMemoryWrite(GP_address, 64, (BYTE * far)&GP_block[7]);
                    sendack(CB_SourceNID);
                    return;
                }
#ifdef ACE8C
                if (GP_block[6] == 1) { // debounce
                    scandelay = GP_block[7];
                    EEPROMWrite((int)&DEBOUNCE, scandelay);
                    sendack(CB_SourceNID);
                    return;
                }
#endif
#ifdef SERVO
                if (GP_block[6] == 1) { // servo position data
                    for (i=0; i<16; i++) {
                        servopos[i] = GP_block[i+7];
                        EEPROMWrite((int)&SERVOPOS[i], servopos[i]);
                    }
                    sendack(CB_SourceNID);
                    return;
                }
#endif
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
#ifndef ACE8C
                if (GP_block[6] == 0) { // event data
                    i = GP_block[7];
                    ReadAllEvents(GP_address);
                    StartSendBlock(i+7, CB_SourceNID);
                    return;
                }
#endif
#ifdef ACE8C
                if (GP_block[6] == 0) { // event data
                    GP_address += (unsigned short long)&EventTable[0];
                }
#endif
                if (GP_block[6] == 0xFE || GP_block[6] == 0 ) {
                    i = GP_block[7];
                    ProgramMemoryRead(GP_address, i, (BYTE * far)&GP_block[7]);
                    StartSendBlock(i+7, CB_SourceNID);
                    return;
                }
#ifdef ACE8C
                if (GP_block[6] == 1) { // debounce
                    GP_block[7] = scandelay;
                    StartSendBlock(1+7, CB_SourceNID);
                    return;
                }
#endif
#ifdef SERVO
                if (GP_block[6] == 1) { // servo position data
                    for (i=0; i<16; i++)
                        GP_block[i+7] = servopos[i];
                    StartSendBlock(16+7, CB_SourceNID);
                    return;
                }
#endif
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
            else if (CB_data[1] == DGM_FACTORY) { // defaults
		        SetDefault();
		        sendack(CB_SourceNID);
            }
        } // memory op
        sendnack(CB_SourceNID,CB_data[1]);
    } // end of datagram
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
    while (pulseon) ;       // wait until not pulsing a servo
#endif 
    INTCONbits.GIEH = 0;    // disable all high priority interrupts          
    INTCONbits.GIEL = 0;    // disable all low priority interrupts
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
#ifdef ACE8C
    INTCONbits.GIEH = 1;    // enable all high priority interrupts          
    INTCONbits.GIEL = 1;    // enable all low priority interrupts
#endif
}

//*******************************************************************************
//    Servo routines
//*******************************************************************************

#ifdef SERVO

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
                servo_ptr = (BYTE far *)&servopos[servo<<1];
                new_output |= BitMask[servo];
            } 
            else { // Nearer 1ms endpoint
                servo_ptr = (BYTE far *)&servopos[(servo<<1)+1];
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
	            // delay if switch kept down so servo doesn't move too fast
	            for (count=250; (count>0) && (PUSHBTN == SW_ON); count--)  ; 
	        }
        }
        else { // UNLEARN == SW_OFF
            if (PUSHBTN == SW_ON) { // Check if we've already saved it
                if (saved != servo) { // Store current setting in EEPROM
                    if (LEARN == SW_ON) { // Nearer 2ms endpoint
                        addr = (BYTE)&SERVOPOS[servo<<1];
                    } 
                    else { // Nearer 1ms endpoint
                        addr = (BYTE)&SERVOPOS[(servo<<1)+1];
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

#ifdef SERVO
void InitRamFromEEPROM(void)
{
    far overlay BYTE i;
    for (i=0; i<16; i++) {
        servopos[i] = EEPROMRead((int)&SERVOPOS[i]);
    }
}
#endif

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
#ifdef SERVO
    InitRamFromEEPROM();
    new_output = 0;
    servo_state = 19;
    next_portc = 0;
    pulseon = 0;
    pulseoutput = 0;
    pulsetimer = 0;
    // Setup TMR1 for servo pulse timing, 16MHz Fosc = 4MHz Fcyc, 1:2 prescale
    TMR1H = 0; // 65536*2 msec before 1st interrupt
    TMR1L = 0;
    T1CON = 0b10010000;

    // Enable TMR1 interrupts at high priority
    T1CONbits.TMR1ON = 1;
    IPR1bits.TMR1IP = 1;
    PIE1bits.TMR1IE = 1;
    EnableInterrupts();		// enable interrupts
#endif
#ifdef ACE8C
    timer13us = 10000/13;
    timerflag = 0;
    T1CON = 0x81;         // enable timer1, 16 bit mode, no prescaler
    TMR1H = (0x10000 - 52) >> 8; // 4 * 52 / 16,000,000 = 13 usec for 38kHz
    TMR1L = (0x10000 - 52) & 0xFF;

    // Enable TMR1 interrupts at high priority
    T1CONbits.TMR1ON = 1;
    IPR1bits.TMR1IP = 1;
    PIE1bits.TMR1IE = 1;
    EnableInterrupts();		// enable interrupts
    timer15ms = 0;
    scancount = scandelay = EEPROMRead((unsigned int)&DEBOUNCE);
    sendallbits = 0xFF;
    OutputState = PORTC & 0x7F;
    PrevInput1 = PrevInput2 = PrevInput3 = OutputState;
#endif

    CheckAlias(0);
    GreenLEDOn();
    YellowLEDOff();
    canTraffic = 0;

#ifndef ACE8C
    // Test for power up mode, Unlearn switch on, learn switch off
    if ((LEARN == SW_OFF) && (UNLEARN == SW_ON)) {
        EraseAll();
#ifdef SERVO
//        if (PUSHBTN == SW_ON) // Unlearn all servo settings
//            SetDefault();
#endif
    }
#endif

#ifdef SERVO
    // Unlearn switch on, learn switch on
    // Enter servo setting mode
//    if ((LEARN == SW_ON) && (UNLEARN == SW_ON))
//        servo_setup();
#endif
    dgcnt = 0;
    DNID = -1;
    SendNSN(FT_INIT);

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
#ifdef SERVO
            if (pulsetimer==0 && pulseoutput) { // select another to pulse
                for (i=0; i<8; i++) {
                    if (pulseoutput&BitMask[i]) {
                        pulseoutput &= ~BitMask[i]; // clear request bit 
                        pulseon = BitMask[i];
                        servo_index = i;
                        pulsetimer = 25;         // no of 20 msec of pulses
                        break;                        
                    }
                }
            }
#endif
        }

#ifdef ACE8C 
        // 10 msec timer
        if (timerflag) { 
            timerflag--;
            timer15ms++;

            // call scan every (scandelay+1) x 10msec 
            if (scancount==0) {       
                scan();
                scancount = scandelay;
            }
            else
                scancount--;

            // send every switch state event after 2 sec start up delay 
            if (timer15ms>0x7F && sendallbits!=0) { // (start of day)
                for (i=0; i<8; i++) {
                    if ((BitMask[i]&sendallbits)!=0) {
                        CB_SourceNID = ND.nodeIdAlias;
                        CB_FrameType = FT_EVENT;
                        CB_datalen = 8;
                        j = i << 4;
                        if ((OutputState & BitMask[i])==0)
                            j += 8;
                        t = FALSE;
                        for (k=0; k<8; k++) {
                            CB_data[k] = EventTable[j+k];
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
        EndSendBlock();

        if (ECANReceiveMessage()) {
            if ((CB_FrameType&0xCFFF) == (FT_DG | ND.nodeIdAlias) )  {
                canTraffic = 1;
                DatagramPacket();
            }
            else
                Packet();
        }
    }
}

//*********************************************************************************
