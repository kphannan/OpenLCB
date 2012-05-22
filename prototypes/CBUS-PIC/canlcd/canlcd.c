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

unsigned int DNID;
BYTE dgcnt;
BYTE i, t;
BYTE line;
BYTE canTraffic;           // yellow led CAN traffic indicator

#pragma udata txt
BYTE far textdata[160];

#pragma udata ovrly

//*********************************************************************************
//        ROM module info
//*********************************************************************************

#define modulestring "OpenLCB PIC LCD display driver " __DATE__ " " __TIME__

#pragma romdata
const rom BYTE xml[] = 
    "<cdi><id><software>" modulestring "</software></id>"
    "<se na=\"Location\" or=\"#0080\" sp=\"#FE\" bu=\"#323\">"
      "<ch na=\"Location\" si=\"64\"/>"
    "</se>"
    "<se na=\"Node Id\" or=\"#0040\" sp=\"#FE\" bu=\"#343\">"
      "<in na=\"Serial\" si=\"1\"/>"
      "<in na=\"Member\" si=\"3\"/>"
      "<by na=\"Group\" si=\"2\"/>"
    "</se>"
    "<se na=\"Text\" bu=\"#23\">"
      "<ch na=\"Text\" si=\"80\"/>"
    "</se>"
    "</cdi>";

#pragma romdata module = 0x001020
const rom BYTE valid = 0;     // tmp set to 0xFF by PC side of loader. 

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

void SetDefault(void)
{
    lcdinit();
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
    far overlay BYTE i, j;
    if ((HI(CB_FrameType)&0xF0)==(FT_DGF>>8) || (HI(CB_FrameType)&0xF0)==(FT_DGS>>8)) { // first packet
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
                if (GP_block[6] == 0) {
                    j = LO(GP_address);
                    lcdcontrol(0x80 | j); // set character position
                    for (i=7; i<64+7; i++) {
                        if (GP_block[i]==0)
                            break;
                        if (GP_block[i]=='\n') {
                            if (j<40) {
                                j = 40;
                                lcdcontrol(0x80+40); // 2nd line is character 40
                            }
                            else {
                                j = 0;
                                lcdcontrol(0x80); // back to 1st line
                            }
                        }
                        else {
                            lcdchar(GP_block[i]);
                            j++;
                        }
                    }
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
    lcdinit();
    CheckAlias(0);
    SendNSN(FT_INIT);
    DNID = -1;
    dgcnt = 0;

    // Simple loop looking for a received CAN frame
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
            if (CB_FrameType==(FT_DGM|ND.nodeIdAlias) || CB_FrameType==(FT_DGL|ND.nodeIdAlias)
              || CB_FrameType==(FT_DGF|ND.nodeIdAlias) || CB_FrameType==(FT_DGS|ND.nodeIdAlias)) {
                canTraffic = 1;
                DatagramPacket();
            }
            else
                Packet();
        }
    }
}

//*********************************************************************************
