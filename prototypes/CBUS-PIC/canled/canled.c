/*  OpenLCB for MERG CBUS CANLED

    16 Dec 2009

    Led numbers are 0 to 63. Events are stored as 9 bytes, the 8 byte event
    number and one byte action. The event byte is the 6 bits led number, 2 bits
    LED state, 00 for off, 01 for on, 10 for flash, 11 spare code. Add 64 for
    on, and add 128 for flash to the led number.
    
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

#define SendMessage ECANSendMessage
#define ReceiveMessage ECANReceiveMessage

#include "../canlib/frametypes.c"
#include "../canlib/general.c"
#include "../canlib/entry.c"
#include "../canlib/ecan.c"
#define EVENTSIZE 9
#define TABLESIZE 2880    // 45 buckets
//#define TABLESIZE 3200      // 50 buckets
#include "../canlib/hash.c"

// inputs
#define SetupButton !PORTBbits.RB0
#define LEDSelect (PORTA & 0x3F)
#define Polarity !PORTCbits.RC4
#define Toggle !PORTCbits.RC3
#define Unlearn !PORTCbits.RC1
#define Learn !PORTCbits.RC0

//*********************************************************************************
//        RAM data
//*********************************************************************************

#pragma udata

unsigned int AllLedOnTest; // start up test timer
BOOL LedsOn;

// hp int data
BYTE HpIndex;              // shift bit counter
BYTE HpColData;            // data shifted out
BYTE HpLedColumn[16];      // bit pattern for leds
BYTE HpLedRow;             // row number being output
BYTE HpFlash;              // counter for flashing

BYTE BitMask[8];           // bit masks
unsigned int DNID;         // block transfer address
BYTE dgcnt;
BYTE learnlink;            // debounce learn button
BYTE canTraffic;           // yellow led CAN traffic indicator

//*********************************************************************************
//        ROM module info
//*********************************************************************************

#define modulestring "OpenLCB PIC CANLED "  __DATE__ " " __TIME__ 

#pragma romdata
const rom BYTE xml[] = "<cdi><id><software>" modulestring "</software></id>"
    "<se na=\"Location\" or=\"#0080\" sp=\"#FE\" bu=\"#303\">"
      "<ch na=\"Location\" si=\"64\"/>"
    "</se>"
    "<se na=\"Node Id\" or=\"#0040\" sp=\"#FE\" bu=\"#343\">"
      "<in na=\"Serial\" si=\"1\"/>"
      "<in na=\"Member\" si=\"3\"/>"
      "<by na=\"Group\" si=\"2\"/>"
    "</se>"
    "<se na=\"Events\" sp=\"0\" bu=\"#303\">"
      "<gr rep=\"150\"/>"
        "<by na=\"Event\" si=\"8\"/>"
        "<by na=\"Action\"/>"
      "</gr>"
    "</se>"
  "</cdi>";

#pragma romdata module = 0x001020
const rom BYTE valid = 0;     // tmp set to 0xFF by PC side of loader. 

#pragma romdata
#pragma udata overly    // needed for overlay

//*********************************************************************************
//        Low level routines
//*********************************************************************************

// Timer interrupt Timer0 in 16 bit mode
// 16/4MHz with prescale 8 gives 2 usec steps, counts up to overflow 
// so it needs to be reloaded with 0x10000-(t usec/2)

// Every 2.5 msec to drive LEDS, 100Hz refresh
// Flashing 400 msec on and 400 msec off

#pragma interrupt hpinterrupt
void hpinterrupt(void)
{
    TMR0H = (0x10000-(2500/2))>>8;     // load the timer for the next bit
    TMR0L = (0x10000-(2500/2)) & 0xFF;
    INTCONbits.TMR0IF = 0;              // clear interrupt flag

    HpLedRow = (HpLedRow+1)& 0x03;      // 4 rows(0-3) 

    // preload next coloumn data upper byte first
    if (AllLedOnTest)
        HpColData = 0xFF;
    else if (HpFlash&0x04)
        HpColData = HpLedColumn[(HpLedRow<<1)+9];
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
    if (AllLedOnTest)
        HpColData = 0xFF;
    else if (HpFlash&0x04)
        HpColData = HpLedColumn[(HpLedRow<<1)+8];
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
 
void DoEvent(static BYTE * far ev)
{
    far overlay BYTE i, j, m;
    m = ev[0];
    i = m & 0x07;
    j = (m >> 3 ) & 0x07;
    m &= 0xC0;
    if (m == 0x00) {
        HpLedColumn[j] &= ~BitMask[i];
        HpLedColumn[j+8] &= ~BitMask[i];
    }
    else if (m == 0x40) {
        HpLedColumn[j] &= ~BitMask[i];
        HpLedColumn[j+8] |= BitMask[i];
    }
    else if (m == 0x80) {
        HpLedColumn[j] |= BitMask[i];
        HpLedColumn[j+8] &= ~BitMask[i];
    }
    else {
        HpLedColumn[j] |= BitMask[i];
        HpLedColumn[j+8] |= BitMask[i];
    }
}

void SetDefault(void)
{
    EraseAll();
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
//        Packet and DatagramPacket
//*********************************************************************************

void Packet(void)
{
    far overlay BYTE i;

    switch(CB_FrameType) {
    case FT_VNSN: // send full NID
        SendNSN(FT_NSN);
        return;

    case FT_EVENT: // Event
        canTraffic = 1; 
        if (learnlink) {            // in learn mode
            if (Unlearn) {          // unlean all events with this event number
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
                LOWD(event[8]) = LEDSelect;
                if (Polarity)
                    event[8] |= 0x40;
                if (Toggle)
                    event[8] |= 0x80;
                SaveEvent((BYTE * far)&event[0]);
            }
        }
        else {
            Find((BYTE * far)&CB_data[0]);
        }
        return;
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
                if (GP_block[6] == 0xFE) { // program memory
                    // write program data
                    ProgramMemoryWrite(GP_address, 64, (BYTE * far)&GP_block[7]);
                    sendack(CB_SourceNID);
                    return;
                }
                if (GP_block[6] == 0) { // event data

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
                if (GP_block[6] == 0) { // event data

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
                PORTCbits.RC2 = 1;      // disable row drive
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
    for (i=0; i<16; i++)
        HpLedColumn[i] = 0;

    DNID = -1;
    dgcnt = 0;

    INTCONbits.GIEH = 1;  // enable all high priority interrupts          
    INTCONbits.GIEL = 1;  // enable all low priority interrupts

    // all zero after 1st programming chip
    ProgramMemoryRead((unsigned long)&table[0], 64, (BYTE * far)GP_block);
    j = 0;
    for (i=0; i<64; i++)
        j |= GP_block[i];
    if (j == 0) {
        EraseAll();
    }

    if (Unlearn) {            // power on erase all events
        wait(10);
        if (Unlearn) {        // debounce and erase all events
            EraseAll();
            for (i=0; i<8; i++)
                HpLedColumn[i] = 0;
        }
    }

    CheckAlias(0);
    GreenLEDOn();
    YellowLEDOff();
    SendNSN(FT_INIT);

    // Main loop
    while(1) {
        // 100 msec timer
        if (Timer3Test()) { 
            if (canTraffic) {
                YellowLEDOn();
            }
            else {
                YellowLEDOff();
            }
            canTraffic = 0;

            HpFlash++;
            if (Learn) { // debounce entry to learn mode
                if (learnlink++ > 2) { // in learn mode
                    learnlink = 3;
                    YellowLEDOn();
                    // light the led for the switch position
                    for (i=0; i<16; i++)
                        HpLedColumn[i] = 0;
                    i = LEDSelect;
                    HpLedColumn[(i>>3)&0x07] = BitMask[i&0x07];
                    HpLedColumn[((i>>3)&0x07) + 8] = BitMask[i&0x07];
                } 
            }
            else if (learnlink) {    // exit learn mode
                learnlink = 0;
                YellowLEDOff();
            }
            if (AllLedOnTest) {         // led startup test timer
                --AllLedOnTest;         // end of test when zero
            }
        }

        EndSendBlock();

        if (ReceiveMessage()) {
            if (CB_SourceNID == ND.nodeIdAlias) { // conflict
                if ((CB_FrameType&0x8000)==0x0000) { // CIM or RIM message
                    CB_SourceNID = ND.nodeIdAlias;
                    CB_FrameType = FT_RID;
                    CB_datalen = 0;
                    while (SendMessage()==0) ;
                }
                else
                    CheckAlias(1);                  // get new alias
            }
            else if (CB_FrameType==(FT_DGF|ND.nodeIdAlias) || CB_FrameType==(FT_DGM|ND.nodeIdAlias)
              || CB_FrameType==(FT_DGL|ND.nodeIdAlias) || CB_FrameType==(FT_DGS|ND.nodeIdAlias)) {
                canTraffic = 1; 
                DatagramPacket();
            }
            else
                Packet();
        }    
    }
}
