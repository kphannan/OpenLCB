/* 
    OpenLCB for an LCD display module

    Displays a message on a simple parallel LCB module connected to a MERG CANACC8. 
    Message is up to 64 bytes sent to address 0x000000

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

//*********************************************************************************
//    Forward refs
//*********************************************************************************

void hpinterrupt(void);
void lpinterrupt(void);
void packet(void);
void EnableInterrupts(void);
void DisableInterrupts(void);

//*********************************************************************************
//    Ram
//*********************************************************************************

#pragma udata

unsigned int blocks;
unsigned int DNID;
BYTE timer;
BYTE i, t;
BYTE line;
BYTE canTraffic;           // yellow led CAN traffic indicator

#pragma udata ovrly

//*********************************************************************************
//        ROM module info
//*********************************************************************************

#define modulestring "OpenLCB PIC LCD display driver " __DATE__ " " __TIME__

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
//        High priority interrupt. Not used
//*********************************************************************************

#pragma interrupt hpinterrupt
void hpinterrupt(void)
{
}

//*********************************************************************************
//        Low priority interrupt
//*********************************************************************************

#pragma interruptlow lpinterrupt
void lpinterrupt(void)
{
}

//*********************************************************************************
//        LCD Driver
//*********************************************************************************

#define LCDDATA PORTC
#define LCDBF PORTCbits.RC3
#define LCDRW PORTCbits.RC4
#define LCDRS PORTCbits.RC5
#define LCDE PORTCbits.RC6

void wait(BYTE t)
{
    far overlay unsigned int i;
    while(t--) {
        i= (4000/7);    // for 16MHz, 4000 instructions per mS 
        while(--i) ;    // 7 instruction loop        
    }
}

void lcdwrite(BYTE b)
{
    LCDDATA = b;
    _asm
        nop
    _endasm;
    LCDE = 1;
    _asm
        nop
        nop
    _endasm;
    LCDE = 0;
    _asm
        nop
    _endasm;
}

void lcdbusy(void)
{
    far overlay BYTE i;
    i = 250 ;        // 50 uS 
    while(--i) {
        _asm
            nop
        _endasm;
    } 
/*
    far overlay BYTE t;
    TRISC = 0xFF;        // Inputs
    LCDRW = 1;
    do {
        LCDE = 1;
        t = LCDBF;
        LCDE = 0;
        _asm
            nop
        _endasm;
        LCDE = 1;
        _asm
            nop
        _endasm;
        LCDE = 0;
    } while (t == 1);
    LCDRW = 0;
    TRISC = 0;           // Outputs
*/
}

// write a control byte
void lcdcontrol(BYTE b)
{
    lcdbusy();
    lcdwrite(b>>4);
    lcdwrite(b&0x0F);
}

// write a character byte
void lcdchar(BYTE b)
{
    lcdbusy();
    lcdwrite(0x20 | (b>>4));
    lcdwrite(0x20 | (b&0x0F));
}

void LcdRomString(BYTE rom far *p)
{
    line = 0;
    lcdcontrol(0x01); // clear display
    wait(2);
    lcdcontrol(0x80); // 1st line is character 0
    while (*p) {
        if (*p=='\n') {
            p++;
            line++;
            lcdcontrol(0x80+40*line); // 2nd line is character 40
        }
        else
            lcdchar(*p++);
    }
}

void LcdRamString(BYTE far *p)
{
    line = 0;
    lcdcontrol(0x01); // clear display
    wait(2);
    lcdcontrol(0x80); // 1st line is character 0
    while (*p) {
        if (*p=='\n') {
            p++;
            line++;
            lcdcontrol(0x80+40*line); // 2nd line is character 40
        }
        else
            lcdchar(*p++);
    }
}

void lcdinit(void)
{
    // non power up initialize sequence
    TRISC = 0;         // Outputs
    PORTC = 0x00;      // data clock low
    wait(15);
    lcdwrite(0x03);    // 8 bit mode
    wait(5);
    lcdwrite(0x03);    // 8 bit mode
    wait(2);
    lcdwrite(0x03);    // 8 bit mode
    lcdbusy();
    lcdwrite(0x02);    // change to 4 bit mode
    lcdcontrol(0x28);  // 4 bit, 2 line, 5x8 font
    lcdcontrol(0x01);  // clear display 
    wait(2);
    //lcdcontrol(0x06);  // inc entry adddress, no shift 
    lcdcontrol(0x0C);  // display on, no cursor
    LcdRomString((BYTE rom far *)"OpenLCB "__TIME__"\n"  __DATE__ );
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
    canTraffic = 1; 
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
                if (GP_address != 0)
                    ProgramMemoryWrite(GP_address, 64, (BYTE * far)GP_block);
                else
                    LcdRamString((BYTE * far)GP_block);
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
    case DAA_PEERASE:
    case DAA_CEERASEH:
        sendack(ACK_OK, CB_SourceNID);
        break;

    case DAA_PEREAD:
    case DAA_NVREAD:
    case DAA_CEREADH:
        sendack(ACK_NODATA, CB_SourceNID);
        break;

    case DAA_PEWRITEH:
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
    far overlay BYTE i;

    INTCON = 0;
    ADCON0 = 0;
    ADCON1 = 0b00001111;
    TRISA =  0b00000111;    // Port A 0 is unlearn, 1 is polarity, 2 = S1
    // RB0,1 logic inputs,  RB2 = CANTX, RB3 = CANRX, RB4,5 are logic input 
    // RB6,7 for debug, ICSP and diagnostics LEDs
    TRISB = 0b00111011;
    PORTB = 0;
    PORTBbits.RB2 = 1;            // CAN recessive
    TRISC = 0;                    // Outputs
    PORTC = 0; 
    RCONbits.IPEN = 1;            // enable interrupt priority levels
    EECON1 = 0;

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

    GreenLEDOn();
    YellowLEDOff();
    ECANInitialize();        
    Timer3Init();
    lcdinit();
    CheckAlias(0);
    SendNSN(FT_INIT);

    // Simple loop looking for a received CAN frame
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
            if (blocks!=0 && timer>20) { // send timeout ack
                sendack(ACK_TIMEOUT, DNID); // timeout
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
            else if (CB_FrameType == (FT_DAA | ND.nodeIdAlias) )
                DAA_Packet();
            else
                Packet();
        }
    }
}

//*********************************************************************************
