/*
    Command station - DCC with OpenLCB interface
    Based on MERG CANACC5 or CANACC8 with DCC booster

    Jan 2010

    All timers and delays configured for 16MHz clock

    Copyright (C) 2008, 2009    Mike Johnson

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

//*********************************************************************************
//        Constants
//*********************************************************************************

#define DCCPHASE1 PORTCbits.RC2
#define DCCPHASE2 PORTCbits.RC3

void CsSend(void);
BYTE SpeedConvert(BYTE speed, BYTE steps);

//*********************************************************************************
//        Define RAM storage
//*********************************************************************************

// Fast access (near) data, lower 96 bytes
// Rest is far data

#pragma udata

unsigned int timer;      // 1 msec counter

// high priority interrupt for sending DCC packets, 10 bytes
BYTE hp_overloadflag;    // porta including overload
BYTE hp_nextbit;         // time of the next half bit
BYTE hp_bitcnt;          // half bit count
BYTE hp_numberofbits;    // number of half bits in packet
BYTE hp_buf[6];          // current packet

BYTE dcchp;              // hp sending
BYTE dccslot;            // slot number
BYTE dcctype;            // slot type
BYTE dccrefreshslot;     // low priority refresh slot number
BYTE dccrefreshtype;
BYTE dccreset;           // sends a dcc reset sequence
BYTE dccpoweroff;        // stop power
BYTE dccinsertidle;      // same address - so insert idle
BYTE dccaddr0;           // last address
BYTE dccaddr1;
BYTE dccsave[6];         // save buffer for insert idle
BYTE dccsavelen;         // length of packet saved

// DCC slot data
#ifdef __18F2480
#define DCCSLOTMAX 16
#endif

#ifdef __18F2680
#define DCCSLOTMAX 128
#endif

// I tried using a structure for this but the generated code was very large
// using bitfields also increased the data size

#define INUSE      0x80  // loco or consist is inuse by a cab
#define CONSISTED  0x10  // in a consist
#define MASTER     0x20  // master of consist group 
#define TIMEOUT    0x0C  // 2 bits, 00 = don't refresh, C0 max count
#define TIMESTEP   0x04

#define SPEEDSTEPS 0x03  // 00 = 128, 01 = 14 steps, 10 = 28 steps 
#define STEPS128   0x00
#define STEPS14    0x01
#define STEPS28    0x10
#pragma udata sess0
far BYTE DCC_flags[DCCSLOTMAX];
#pragma udata sess1
far BYTE DCC_speed[DCCSLOTMAX]; // in DCC format
#pragma udata sess2
far unsigned int DCC_address[DCCSLOTMAX]; // 14 bit address
#pragma udata sess3
far BYTE DCC_fn0[DCCSLOTMAX];   // fn0-4
#pragma udata sess4
far BYTE DCC_fn5[DCCSLOTMAX];   // fn5-12
#pragma udata sess5
far BYTE DCC_fn13[DCCSLOTMAX];  // fn13-20
#pragma udata sess6
far BYTE DCC_fn21[DCCSLOTMAX];  // fn21-28
#pragma udata sess7
far BYTE DCC_lru[DCCSLOTMAX];   // least recently used chain
#pragma udata sess8
#define CSPEED 0x80             // 1 bit change flags
#define CFN0   0x40
#define CFN5   0x20
#define CFN9   0x10
#define CFN13  0x0C             // 2 bit send counts
#define CFN21  0x03
far BYTE DCC_changes[DCCSLOTMAX]; // change flags and counts
#pragma udata sess9
far BYTE DCC_consist[DCCSLOTMAX]; // consist address

#pragma udata
BYTE LruHead;                   // head of the lru chain

#pragma udata dcc_sp
// DCC special packets, accessory, operations mode programming, 56 bytes
// Again using a structure produced a very large increase in the generated code size
#define DCCHPMAX 4
#define DCCRETRY 3
#define DEACTIVATE 0x40              // flag to mark deactivate follows activate
#define OPMODEWRITE 0x80             // operation mode write
far BYTE specialdccfull[DCCHPMAX];   // 0 = empty, n = number of times to send
far BYTE specialdcclen[DCCHPMAX];    // number of bytes
far BYTE specialdcc0[DCCHPMAX];
far BYTE specialdcc1[DCCHPMAX];
far BYTE specialdcc2[DCCHPMAX];
far BYTE specialdcc3[DCCHPMAX];
far BYTE specialdcc4[DCCHPMAX];
far BYTE specialdcc5[DCCHPMAX];

#pragma udata

BYTE blocks;                // loader block count
BYTE canTraffic;
int DNID;

#pragma udata ovrly

//*********************************************************************************
//        ROM module info
//*********************************************************************************

#define modulestring "OpenLCB command station " __DATE__ " " __TIME__

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

#pragma romdata

//*********************************************************************************
//        EEDATA
//*********************************************************************************

#pragma romdata eedata = 0xF00000

//*********************************************************************************
//        High priority interrupt
//        Timer 0 used only for sending DCC packets, every 58 usec
//*********************************************************************************

// Timer interrupt Timer0 in 16 bit mode
// 16/4MHz with prescale 8 gives 2 usec steps, counts up to overflow 
// so 0x10000-29 or 0x10000-50 for 58 or 100 usec
// and 0 = 131 msec for overloadalarm timeout
#define ONEBITTIME 0x100-(53/2)
#define ZEROBITTIME 0x100-(100/2)
// DCC packets are: at least 14 bits preamble, 3, 4 or 5 bytes long
// with a zero before each byte and a one at the end (start of next preamble)
// at most 120 half bits long.

// Don't call anything from the interrrupt, it causes a big overhead
// use inline code instead

#pragma interrupt hpinterrupt
void hpinterrupt(void)
{
    TMR0H = 0xFF;           // load the timer for the next bit
    TMR0L = hp_nextbit;
    INTCONbits.TMR0IF = 0;  // clear interrupt flag
 //   hp_overloadflag = 0xFF; //PORTA;    // read overload alarm input before turning off the bridge
    DCCPHASE1 = 0;          // turn off the bridge
    DCCPHASE2 = 0;
    if (/*((hp_overloadflag&0x10)==0) ||*/ dccpoweroff) { // turn off the power
        TMR0H = 0;          // 131 msec
        TMR0L = 0;
        hp_numberofbits = 30 + 18 * 3;
        hp_bitcnt = 0;
        hp_nextbit = ONEBITTIME;
        hp_buf[0] = 0xFF;   // idle packets
        hp_buf[1] = 0x00;
        hp_buf[2] = 0xFF ^ 0x00;
        return;
    } 
    Nop();    // needs about 2 usec +-1 usec between bridge off and bridge on
    Nop();
    if (hp_bitcnt&0x01) // turn one side of the bridge on
        DCCPHASE2 = 1;    // 1st phase
    else 
        DCCPHASE1 = 1;    // 2nd phase

    hp_bitcnt++;
    if (hp_bitcnt&0x01) // 1st phase, next bit will be the same as the previous
        return;

    // 2nd phase, need to calc the next bit
    if (hp_bitcnt<30) // still preamble
        return;
    if (hp_bitcnt>=hp_numberofbits) { // end of packet
        hp_nextbit = ONEBITTIME;
        hp_bitcnt = 0;
        hp_numberofbits = 0;
        return;
    }
    if ((hp_bitcnt==30) || (hp_bitcnt==30+18) || (hp_bitcnt==30+18*2) 
      || (hp_bitcnt==30+18*3) || (hp_bitcnt==30+18*4) || (hp_bitcnt==30+18*5)) {
        hp_nextbit = ZEROBITTIME; // zero padding bit
        return;
    }

    // data sent most significant bit first
    if (hp_buf[0]&0x80) // test the next bit
        hp_nextbit = ONEBITTIME;  // one bit
    else 
        hp_nextbit = ZEROBITTIME; // zero bit

    // rotate all 6 bytes one bit to the left, inline asm is so much faster    
    // using the carry bit, the buffer must be in the access area
    _asm
    rlcf hp_buf+5,1,0    
    rlcf hp_buf+4,1,0    
    rlcf hp_buf+3,1,0    
    rlcf hp_buf+2,1,0    
    rlcf hp_buf+1,1,0    
    rlcf hp_buf+0,1,0
    _endasm    
}

BYTE SendSpeed(BYTE i)
{
    far overlay BYTE tmpa;
    tmpa = 0; // byte count, pointer into interrupt buffer
    // fill in address bytes
    if (DCC_address[i] > 99) // long (14 bit) address
         hp_buf[tmpa++] = 0xC0 | HI(DCC_address[i]);
    hp_buf[tmpa++] = LO(DCC_address[i]);
        
    // fill in speed
    if ((DCC_flags[i]&SPEEDSTEPS) == 0) // 128 steps
        hp_buf[tmpa++] = 0x3F; 
    hp_buf[tmpa++] = DCC_speed[i];  
    return tmpa;
}

void DCCUpdate(void)
{
    // hp_numberofbits is a signal between this code and the interrupt, if its zero
    // then the buffer is empty, non-zero means the interrupt can transmit the data
    // There is just over 1msec (30 x 58 usec) to update the buffer.
    far overlay BYTE i, tmpa, tmpb, tmpc, tmpd;

    if (hp_numberofbits || dccpoweroff) // busy
        return;

    if (dccreset) {
        if ((--dccreset)<20) {
            hp_buf[0] = 0xFF; // idle packets
            hp_buf[1] = 0x00;
            hp_buf[2] = 0xFF ^ 0x00;
            hp_numberofbits = 30 + 18 * 3;
            goto exittrace;
        }
        hp_buf[0] = 0x00;    // Reset packets
        hp_buf[1] = 0x00;
        hp_buf[2] = 0x00 ^ 0x00;
        hp_numberofbits = 30 + 18 * 3;
        goto exittrace;
    }

    if (dccinsertidle) {
        dccinsertidle = 0;
        dccaddr0 = hp_buf[0] = dccsave[0]; // restore saved data
        dccaddr1 = hp_buf[1] = dccsave[1];
        hp_buf[2] = dccsave[2];
        hp_buf[3] = dccsave[3];
        hp_buf[4] = dccsave[4];
        hp_buf[5] = dccsave[5];
        hp_numberofbits = dccsavelen;
        goto exittrace;
    }
 
    // jump out of the loop when we find something to send
    while (1) {
        while (dcchp==2) { // hp sending, every 128 msec scan all hp slots
            dccslot++;
            if (dccslot >= DCCHPMAX) { // all done
                dccslot = 0xFF;        // for changes start 
                dcctype = 0;
                dcchp = 1;               // not hp mode
                break;
            }
            if (specialdccfull[dccslot]) {
                tmpa = specialdcclen[dccslot]; // number of bytes
                hp_buf[0] = specialdcc0[dccslot];
                hp_buf[1] = specialdcc1[dccslot];
                hp_buf[2] = specialdcc2[dccslot];
                hp_buf[3] = specialdcc3[dccslot];
                hp_buf[4] = specialdcc4[dccslot];
                hp_buf[5] = specialdcc5[dccslot];

                // decrement count
                if (specialdccfull[dccslot]&OPMODEWRITE) { 
                    // special case of operation mode write
                    specialdccfull[dccslot] = (specialdccfull[dccslot]&0x0F)-1;
                    if (specialdccfull[dccslot]!=0)
                        specialdccfull[dccslot] |= OPMODEWRITE;
                    if ((specialdccfull[dccslot]&0x01)==0)
                        dccslot--;    // to force 2 consecutive write packets
                }
                else if (specialdccfull[dccslot]&DEACTIVATE) { 
                    // special case of deactivate following activate
                    specialdccfull[dccslot] = (specialdccfull[dccslot]&0x0F)-1;
                    if (specialdccfull[dccslot]==0) {
                        specialdccfull[dccslot] = DCCRETRY;
                        specialdcc1[dccslot] &= (~0x08); // change command to deactivate
                    }
                    else 
                        specialdccfull[dccslot] |= DEACTIVATE;
                }
                else 
                    specialdccfull[dccslot]--;
                goto ret;
            }
        }

        while (dcchp==1) { // change sending
            dccslot++;
            if (dccslot >= DCCHPMAX) { // all done
                dccslot = 0xFF;        // for next start 
                dcctype++;
                if (dcctype==6) {
                    dcchp = 0;         // low priority mode
                    break;
                } 
            }
            switch (dcctype) {
            case 0: // speed changes
	          if (DCC_changes[dccslot]&CSPEED) {
	              tmpa = SendSpeed(dccslot);
	              DCC_changes[dccslot] &= ~CSPEED;
                    goto ret;
	          }
                break;
            case 1: // fn0 changes
	          if (DCC_changes[dccslot]&CFN0) {
	              DCC_changes[dccslot] &= ~CFN0;
                    tmpa = 0x80 | DCC_fn0[dccslot]; 
                    goto refsend;
                }   
            case 2: // fn5 changes
	          if (DCC_changes[dccslot]&CFN5) {
	              DCC_changes[dccslot] &= ~CFN5;
                    tmpa = 0xB0 | (DCC_fn5[dccslot]&0x0F); 
                    goto refsend;
                }   
            case 3: // fn9 changes
	          if (DCC_changes[dccslot]&CFN9) {
	              DCC_changes[dccslot] &= ~CFN9;
                    tmpa = 0xA0 | (DCC_fn5[dccslot]>>4);
                    goto refsend;
                }   
            case 4: // fn13 changes
	          if (DCC_changes[dccslot]&CFN13) {
	              DCC_changes[dccslot] -= 0x04;
                    tmpa = 0xDE;
                    tmpb = DCC_fn13[dccslot]; 
                    goto fn13send;
                }   
            case 5: // fn21 changes
	          if (DCC_changes[dccslot]&CFN21) {
	              DCC_changes[dccslot] -= 0x01;
                    tmpa = 0xDF;
                    tmpb = DCC_fn21[dccslot]; 
fn13send:           if (DCC_address[dccslot] > 99) {
		            hp_buf[0] = HI(DCC_address[dccslot]) | 0xC0;    // 14 bit address
		            hp_buf[1] = LO(DCC_address[dccslot]);           // 14 bit address
		            hp_buf[2] = tmpa;
                        hp_buf[3] = tmpb;
		            tmpa = 4;
		        }
		        else {
		            hp_buf[0] = LO(DCC_address[dccslot]); // 7 bit address
		            hp_buf[1] = tmpa;
		            hp_buf[2] = tmpb;
		            tmpa = 3;
		        }
		        goto ret;
                }   
            }
        }

        // next lp slot
        dccrefreshslot++;
        if (dccrefreshslot>=DCCSLOTMAX) {
            dccrefreshslot = 0xFF;
            dccrefreshtype = (dccrefreshtype+1) & 0x03;
            hp_buf[0] = 0xFF;    // send idle
            hp_buf[1] = 0x00;
            hp_buf[2] = 0xFF ^ 0x00;
            dccaddr0 = dccaddr1 = 0xFF;
            hp_numberofbits = 30 + 18 * 3;
            goto exittrace;
        }

        // Low priority refresh of speed and F0 to F12
        switch (dccrefreshtype) {
        case 0: // Send a speed packet, unless emergency stopped or zero speed
            if ((DCC_flags[dccrefreshslot]&TIMEOUT) == 0) {
	            if ((DCC_flags[dccrefreshslot]&SPEEDSTEPS)==0) { // 128
	                if ((DCC_speed[dccrefreshslot]&0x7E) == 0)
	                    break;
	            }
	            else { // 14 or 28
	                if ((DCC_speed[dccrefreshslot]&0x0E) == 0)
	                    break;
	            }
            }
            tmpa = SendSpeed(dccrefreshslot);
            goto ret;
        case 1: // fn0-fn4
            if ((DCC_flags[dccrefreshslot]&TIMEOUT) == 0) {
                if (DCC_fn0[dccrefreshslot]==0)
                    break;
            }
            tmpa = 0x80 | DCC_fn0[dccrefreshslot]; 
            goto refsend;
        case 2: // fn5-fn8
            if ((DCC_flags[dccrefreshslot]&TIMEOUT) == 0) {
                if ((DCC_fn5[dccrefreshslot]&0x0F)==0)
                    break;
            }
            tmpa = 0xB0 | (DCC_fn5[dccrefreshslot]&0x0F); 
            goto refsend;
        case 3: // fn9-fn12
            if ((DCC_flags[dccrefreshslot]&TIMEOUT) == 0) {
                if ((DCC_fn5[dccrefreshslot]&0xF0)==0)
                    break;
            }
            tmpa = 0xA0 | (DCC_fn5[dccrefreshslot]>>4);
refsend:    if (DCC_address[dccrefreshslot] > 99) { // 14 bit address
                hp_buf[0] = HI(DCC_address[dccrefreshslot]) | 0xC0; 
                hp_buf[1] = LO(DCC_address[dccrefreshslot]);
                hp_buf[2] = tmpa;
                tmpa = 3;
            }
            else {
                hp_buf[0] = LO(DCC_address[dccrefreshslot]); // 7 bit address
                hp_buf[1] = tmpa;
                tmpa = 2;
            }
            goto ret;
        }
    } // end while

ret:
    // calc check byte and number of half bits
    tmpb = tmpa;
    tmpc = 0;
    tmpd = 30 + 18; // number of half bits
    while (tmpb) {
        tmpc ^= hp_buf[--tmpb];
        tmpd += 18;
    }
    hp_buf[tmpa] = tmpc;

    // check for the same address
    // packets with the same address need a 5ms gap, idle is 5.68 msec
    if ((dccaddr0==hp_buf[0]) && ((dccaddr0&0xC0)!= 0xC0 || dccaddr1==hp_buf[1])) {
        dccinsertidle = 1;
        dccsave[0] = hp_buf[0];
        dccsave[1] = hp_buf[1];
        dccsave[2] = hp_buf[2];
        dccsave[3] = hp_buf[3];
        dccsave[4] = hp_buf[4];
        dccsave[5] = hp_buf[5];
        dccsavelen = tmpd;
        hp_buf[0] = 0xFF; // idle packet
        hp_buf[1] = 0x00;
        hp_buf[2] = 0xFF ^ 0x00;
        hp_numberofbits = 30 + 18 * 3;
        dccaddr0 = dccaddr1 = 0xFF;
    }
    else { // save address bytes
        hp_numberofbits = tmpd;
        dccaddr0=hp_buf[0];
        dccaddr1=hp_buf[1];
    }

exittrace: ;
/*
    if (hp_buf[0]!=0xFF || hp_buf[1]!=0x00) { // not idle
        CB_data[0] = 0xFF;
        CB_data[1] = hp_numberofbits;
        if (dcchp)
            CB_data[1] |= 0x80;
        CB_data[2] = hp_buf[0];
        CB_data[3] = hp_buf[1];
        CB_data[4] = hp_buf[2];
        i = 5;
        if (hp_numberofbits>30 + 18 * 3) {
            CB_data[i++] = hp_buf[3];
            if (hp_numberofbits>30 + 18 * 4) {
                CB_data[i++] = hp_buf[4];
                if (hp_numberofbits>30 + 18 * 5) {
                    CB_data[i++] = hp_buf[5];
                }
            }
        }
        CB_datalen = i;
        CsSend();
    }
*/
}

//*********************************************************************************
//        Low priority interrupt
//*********************************************************************************

#pragma interruptlow lpinterrupt
void lpinterrupt(void)
{
}

//*********************************************************************************
//        Low level routines
//*********************************************************************************

void CsSend(void)
{
    CB_SourceNID = ND.nodeIdAlias;
    while (SendMessage()==0) ;
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
    INTCONbits.GIEH = 0;     // disable all high priority interrupts          
    INTCONbits.GIEL = 0;     // disable all low priority interrupts
    DCCPHASE1 = 0;           // turn off the bridge
    DCCPHASE2 = 0;
}

// EnableInterrupts - This should restart the interrupts in a safe way.
// For LEDs and servos this can just restart the interrupts,
// for DCC it has to restart from the beginning of a packet.

void EnableInterrupts(void)
{
    hp_numberofbits = 0;    // initialize DCC interrupt data
    hp_bitcnt = 0;
    hp_nextbit = ONEBITTIME;
    hp_overloadflag = 0;
    dccinsertidle = 0;
    dccaddr0 = dccaddr1 = 0xFF;

    INTCONbits.GIEH = 1;    // enable all high priority interrupts          
    INTCONbits.GIEL = 1;    // enable all low priority interrupts
}

//*********************************************************************************
//        Session allocation
//*********************************************************************************

// Least recently used

void LRUpromote(BYTE index)
{
    far overlay BYTE i;
    // move this entry to the head of the chain
    for (i=0; i<DCCSLOTMAX; i++) {
        if (DCC_lru[i] == index) {
            DCC_lru[i] = DCC_lru[index]; // unlink index
            DCC_lru[index] = LruHead;    // add index as the new head
            LruHead = index;
            return;
        }
    }
}

BYTE LRUtail(void)
{
    far overlay BYTE i; 
    // find the last entry, and move it to the head
    for (i=0; i<DCCSLOTMAX; i++) {
        if (DCC_lru[i] == 0xFF) {
            LRUpromote(i);
            return i;
        }
    }
}

void LRUinit(void)
{
    far overlay BYTE i;
    for (i=0; i<DCCSLOTMAX; i++) {
        DCC_lru[i] = i-1; // end of chain is 0xFF
    }
    LruHead = DCCSLOTMAX-1;
}

// Find session for loco address, if necessary allocate it
// Try to re-use entries so the loco state is preserved

BYTE FindAddLoco(unsigned int addr)
{
    far overlay BYTE i;

    addr &= 0x3FFF;
    if (addr==0)
        return 0xFF;
    if (addr>99)
        addr |= 0xC000;

    for (i=0; i<DCCSLOTMAX; i++) {
        if (DCC_address[i]==addr) {
            return i;
        }
    }

    i = LRUtail();
    DCC_flags[i] = 0;
    DCC_address[i] = addr;
    DCC_speed[i] = 0x80;
    DCC_fn0[i] = 0;
    DCC_fn5[i] = 0;
    DCC_fn13[i] = 0;
    DCC_fn21[i] = 0;
    DCC_consist[i] = 0;
    return i;
}

// Find a free high priority DCC buffer
// All changes are sent via these buffers

BYTE AllocDccHp(void)
{
    far overlay BYTE i;

    for (i=0; i<DCCHPMAX; i++) {
        if (specialdccfull[i]==0)
            return i;
    }
    return 0xFF;
}

//*********************************************************************************
//        Consist, MU, Double Header
//*********************************************************************************

// delete the first entry from a consist, if its empty delete the group

void DccConsist(BYTE session, BYTE address, BYTE reverse)
{
    far overlay BYTE i, tmpd;

    address &= 0x7F;
    DCC_consist[session] = address;
    if (address && reverse)
        DCC_consist[session] |= 0x80;
    i = AllocDccHp();
    if (i>DCCHPMAX)
        return;
    if (HI(DCC_address[session]) || (LO(DCC_address[session])&0x80)) {
        specialdcc0[i] = HI(DCC_address[session]) | 0xC0; // 14 bit address
        specialdcc1[i] = LO(DCC_address[session]);        // 14 bit address
        specialdcc2[i] = 0x12;  
        if (reverse)
            specialdcc2[i] = 0x13;  
        specialdcc3[i] = address;  
        specialdcclen[i] = 4;
    }
    else {
        specialdcc0[i] = LO(DCC_address[session]); // 7 bit address
        specialdcc2[i] = 0x12;  
        if (reverse)
            specialdcc2[i] = 0x13;  
        specialdcc3[i] = address;  
        specialdcclen[i] = 3;
    }
    specialdccfull[tmpd] = DCCRETRY;
}

//*********************************************************************************
//        CBUS Input Packet Handling
//*********************************************************************************

void Packet(void)
{
    far overlay BYTE i, j;

    switch (CB_FrameType) {
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

    case FT_EVENT:
       canTraffic = 1;
       return;

    case FT_RESTP: // request emergency stop all
        i=AllocDccHp(); // broadcast emergency stop
        if (i <= DCCHPMAX) {
            specialdccfull[i] = DCCRETRY;
            specialdcc0[i] = 0;    // broadcast address 
            specialdcc1[i] = 0x61; // 14/28 step emergency stop 
            specialdcclen[i] = 2;
        }
        for (i=0; i<DCCSLOTMAX; i++) { // individual emergency stop
            if (DCC_speed!=0 || (DCC_flags[i]&TIMEOUT)!=0) {
                if ((DCC_flags[i]&SPEEDSTEPS)==0)
                    DCC_speed[i] = (DCC_speed[i]&0x80) | 0x01;
                else
                    DCC_speed[i] = (DCC_speed[i]&0xE0) | 0x01;
                DCC_changes[i] |= CSPEED;
            }
        }
        CB_FrameType = FT_ESTOP; // emergency stopped
        CB_datalen = 0;
        CsSend();    
        return;

    case FT_RTOF:  // request track Off
        dccpoweroff = 1;
        CB_FrameType = FT_TOF;
        CB_datalen = 0;
        CsSend();    
        return;

    case FT_RTON: // Track On
        dccpoweroff = 0;
        dccreset = 40;
        CB_FrameType = FT_TON;
        CB_datalen = 0;
        CsSend();    
        return;

    case FT_RLOC: // request loco info
        i = FindAddLoco(((unsigned int)CB_data[0]<<8) | CB_data[1]);
        if (i<DCCSLOTMAX) {
            CB_FrameType = FT_PLOC; // PLOC
            CB_data[0] = i;
            CB_data[1] = HI(DCC_address[i]);
            CB_data[2] = LO(DCC_address[i]);
            CB_data[3] = DCC_flags[i];
            CB_data[4] = DCC_speed[i];
            CB_data[5] = DCC_fn0[i];
            CB_data[6] = DCC_fn5[i];
            CB_datalen = 7;
            CsSend();  
            CB_FrameType = FT_PLOCF; // PLOCF
            CB_data[0] = i;
            CB_data[1] = HI(DCC_address[i]);
            CB_data[2] = LO(DCC_address[i]);
            CB_data[3] = DCC_fn0[i];
            CB_data[4] = DCC_fn5[i];
            CB_data[5] = DCC_fn13[i];
            CB_data[6] = DCC_fn21[i];
            CB_datalen = 7;
            CsSend();
            DCC_flags[i] |= INUSE; // mark it as inuse
            return;
        }
        return;

    case FT_KLOC: // no longer in use
        DCC_flags[CB_data[0]] &= ~INUSE;
        return;

    case FT_PCON: // Consist
        DccConsist(CB_data[0], CB_data[1], 0);
        return;

    case FT_KCON: // Remove from consist
        DccConsist(CB_data[0], 0, 0);
        return;

    case FT_STMOD: // change speed steps
        if ((DCC_flags[CB_data[0]]&SPEEDSTEPS)!=CB_data[1]) {
            if (CB_data[1]==0x00)
                DCC_speed[CB_data[0]] = (DCC_speed[CB_data[0]]&0x20) << 2;
            else
                DCC_speed[CB_data[0]] = 0x40 | ((DCC_speed[CB_data[0]]&0x80) >> 2);
            DCC_flags[CB_data[0]] = (DCC_flags[CB_data[0]]&~SPEEDSTEPS) | CB_data[1];
        }
        return;

    case FT_DSPD: // Speed
        DCC_speed[CB_data[0]] = CB_data[1];
        LRUpromote(CB_data[0]);
        DCC_changes[CB_data[0]] |= CSPEED; // set changed
        DCC_flags[CB_data[0]] |= TIMEOUT;  // set timeout to max
        return;

    case FT_DFUN: // fn's
        // session in CB_data[0], Group in CB_data[1], Fn in CB_data[2] 
        // Group 1 Fn0-4, 2 Fn5-8, 3 Fn9-12, 4 Fn13-20, 5 Fn21-28
        LRUpromote(CB_data[0]);
        switch (CB_data[1]) {
        case 1: // FL, F1 - F4
            DCC_flags[CB_data[0]] |= TIMEOUT; // set timeout to max
            DCC_changes[CB_data[0]] |= CFN0;
            DCC_fn0[CB_data[0]] = CB_data[2];
            break;

        case 2: // F5 - F8
            DCC_flags[CB_data[0]] |= TIMEOUT; // set timeout to max
            DCC_changes[CB_data[0]] |= CFN5;
            DCC_fn5[CB_data[0]] = (DCC_fn5[CB_data[0]]&0xF0) | (CB_data[2]&0x0F);
            break;

        case 3: // F9 - F12
            DCC_flags[CB_data[0]] |= TIMEOUT; // set timeout to max
            DCC_changes[CB_data[0]] |= CFN9;
            DCC_fn5[CB_data[0]] = (DCC_fn5[CB_data[0]]&0x0F) | (CB_data[2]<<4);
            break;
	
        case 4: // F13 - F20
            // DCC_flags[CB_data[0]] |= TIMEOUT; // set timeout to max
            DCC_changes[CB_data[0]] |= CFN13;
            DCC_fn13[CB_data[0]] = CB_data[2]; 
            break;
	
        case 5: // F21 - F28
            // DCC_flags[CB_data[0]] |= TIMEOUT; // set timeout to max
            DCC_changes[CB_data[0]] |= CFN21;
            DCC_fn21[CB_data[0]] = CB_data[2]; 
        }

    case FT_RDCC3: // 3 byte DCC packet
        j = 3;
        goto rdcc;

    case FT_RDCC4: // 4 byte DCC packet
        j = 4;
        goto rdcc;

    case FT_RDCC5: // 5 byte DCC packet
        j = 5;
        goto rdcc;

    case FT_RDCC6: // 6 byte DCC packet
        j = 6;
rdcc:   i=AllocDccHp();
        if (i>DCCHPMAX)
            return;
        specialdccfull[i] = CB_data[0];
        specialdcc0[i] = CB_data[1];
        specialdcc1[i] = CB_data[2];
        specialdcc2[i] = CB_data[3];
        specialdcc3[i] = CB_data[4];
        specialdcc4[i] = CB_data[5];
        specialdcclen[i] = j;
        return;
    }

    return;
}

rom unsigned int bits[10] = {
    0x03FE, 0x03FD, 0x03FB, 0x03F7, 0x03EF, 0x03DF, 0x03BF, 0x037F, 0x02FF, 0x01FF
};

void DAA_Packet(void)
{
    far overlay BYTE i, t;

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
    case DAA_CEERASEH:
    case DAA_PEERASE:
        sendack(ACK_OK, CB_SourceNID);
        break;

    case DAA_NVREAD:
    case DAA_CEREADH:
    case DAA_PEREAD:
        sendack(ACK_NODATA, CB_SourceNID);
        break;

    case DAA_NVWRITE:
    case DAA_CEWRITEH:
    case DAA_PEWRITEH:
        sendack(ACK_NOSPACE, CB_SourceNID);
        break;
    }
}

//*********************************************************************************
//        Main
//*********************************************************************************

void main(void)
{
    far overlay BYTE i;
    INTCON = 0;
    ADCON0 = 0;
    ADCON1 = 0b00001111;
    TRISA =  0b00000111; // Port A 0 is unlearn, 1 is polarity, 2 = S1
    // RB0,1 logic inputs,  RB2 = CANTX, RB3 = CANRX, RB4,5 are logic input 
    // RB6,7 for debug, ICSP and diagnostics LEDs
    TRISB = 0b00111011;
    PORTB = 0;
    PORTBbits.RB2 = 1;   // CAN recessive
    TRISC = 0;           // Outputs
    PORTC = 0;
    RCONbits.IPEN = 1;   // enable interrupt priority levels
    EECON1 = 0;

    // timer 0 for 2 usec steps, high priority interrupt
    T0CON = 0x82;       // set up timer0, 16 bit, prescale 1:8
    TMR0H = 0;          // clear timer
    TMR0L = 0;          // clear timer

    timer = 0;

    //  ECAN         
    ECANInitialize();

    // interrupts
    INTCON = 0x20;       // enable TMR0 interrupt
    INTCON2 = 0x04;      // Portb pullups enable, TMR0 high priority
    RCON = 0x80;         // enable priority levels

    LRUinit();

    dccreset = 50;          // send 30 reset and 20 idle packets
    EnableInterrupts();	    // enable interrupts
    GreenLEDOn();
    YellowLEDOff();
    CheckAlias(0);

    // send init
    CB_FrameType = FT_INIT;
    CB_datalen = 0;
    CsSend();

    CB_data[0] = FT_RESET; // send reset to cabs and adapters
    CB_datalen = 0;
    CsSend();

    // Main loop
    while (1) {

        // 100 msec timer
        if (Timer3Test()) { 
            if (canTraffic) {
                YellowLEDOn();
            }
            else {
                YellowLEDOff();
            }
            canTraffic = 0;

            timer++;
            if (dcchp==0) {     // every 100 msec
                dcchp = 2;      // start dcc hp sending
            }
            if ((timer&0x007F)==0) {      // 12.8 secs
                // Dec change timeout
                for (i=0; i<DCCSLOTMAX; i++) {
                    if (DCC_flags[i]&TIMEOUT)
                        DCC_flags[i] -= TIMESTEP;
                } 
            }
            if (blocks!=0 && timer>20) { // send timeout ack
                sendack(2, DNID); // timeout
                blocks = 0;
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

        DCCUpdate();    // Send a DCC packet to the high priority interrupt routine
    }
}

//*********************************************************************************

