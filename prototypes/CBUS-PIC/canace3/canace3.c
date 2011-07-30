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

//*********************************************************************************
//    Ram
//*********************************************************************************

#ifdef INTERLOCK
#define ALARM PORTAbits.RA5
#endif

#pragma udata

BYTE OldBuffer[16];   // debounced keystate scan buffer
BYTE NewBuffer[16];   // last keystate scan buffer
BYTE BitMask[8];      // bit masks
BYTE canTraffic;      // flash yellow led on can traffic
BYTE i, t;
BYTE dgcnt;
unsigned int DNID;    // NIDa of device sending data
BYTE startofday;      // send switch states on power up
BYTE starttimeout;    // 2 sec delay before sending

#pragma udata ovrly

//*********************************************************************************
//        ROM module info
//*********************************************************************************

#define QUOTED(x) #x

#ifdef INTERLOCK
#define modulestring "OpenLCB PIC CANACE3 Interlock "  __DATE__ " " __TIME__ 
#define DATASIZE 2048
#define DATASIZES QUOTED(DATASIZE) // "#800"
#else
#define modulestring "OpenLCB PIC CANACE3 "  __DATE__ " " __TIME__ 
#endif

#pragma romdata
const rom BYTE xml[] = "<cdi><id><software>" modulestring "</software></id>"
    "<se na=\"Location\" or=\"#0080\" sp=\"#FE\" bu=\"#323\">"
      "<ch na=\"Location\" si=\"64\"/>"
    "</se>"
    "<se na=\"Node Id\" or=\"#0040\" sp=\"#FE\" bu=\"#343\">"
      "<in na=\"Serial\" si=\"1\"/>"
      "<in na=\"Member\" si=\"3\"/>"
      "<by na=\"Group\" si=\"2\"/>"
    "</se>"
    "<se na=\"Event Data\" sp=\"0\" bu=\"#303\">"
      "<gr rep=\"128\">"
        "<by na=\"Event Off\" size=\"8\"/>"
        "<by na=\"Event On\" size=\"8\"/>"
      "</gr>"
    "</se>"
#ifdef INTERLOCK
    "<se na=\"Interlocks\" sp=\"1\" bu=\"#303\">"
      "<gr na=\"Lever-1\" rep=\"128\">"
        "<in na=\"Start\" si=\"2\"/>"
      "</gr>"
      "<gr rep=\"" DATASIZES "\">"
        "<by na=\"Data\"/>"
      "</gr>"
    "</se>"
#endif
  "</cdi>";

#pragma romdata module = 0x001020
const rom BYTE valid = 0;     // tmp set to 0xFF by PC side of loader. 

#pragma romdata events = 0x001040
const rom BYTE EventTable[2048];

#ifdef INTERLOCK
#pragma romdata interlk = 0x001840
const rom unsigned int LockStart[128];
const rom BYTE LockData[DATASIZE];
#endif

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
            GP_block[j] = (a+j)>>3; 
            GP_block[j+1] = 0;
            for (i=0; i<6; i++)
                GP_block[j+i+2] = ND.nodeId[i];
        }
        ProgramMemoryWrite((unsigned short long)&EventTable[a],64,(BYTE * far)&GP_block[0]);
    }
#ifdef INTERLOCK
    for (i=0; i<64; i++)
        GP_block[i] = 0;
    for (a=0; a<1024; a+=64)
        ProgramMemoryWrite((unsigned short long)&LockStart[a],64,(BYTE * far)&GP_block[0]);
#endif
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

    p = &LockData[LockStart[n]];

    stack[0] = FALSE; // for null strings the lever is not locked
    sp = 1;
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
                    CB_data[i] = EventTable[j+7-i];
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

void Packet(void)
{
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
                if (GP_block[6] == 0) { // event data
                    GP_address += (unsigned short long)&EventTable[0];
                }
#ifdef INTERLOCK
                else if (GP_block[6] == 1) { // interlock data
                    GP_address += (unsigned short long)&LockStart[0];
                }
#endif
                if (GP_block[6] == 0xFE || GP_block[6] == 0 
#ifdef INTERLOCK
|| GP_block[6] == 1
#endif
                  ) {
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
                if (GP_block[6] == 0) { // event data
                    GP_address += (unsigned short long)&EventTable[0];
                }
#ifdef INTERLOCK
                else if (GP_block[6] == 1) { // interlock data
                    GP_address += (unsigned short long)&LockStart[0];
                }
#endif
                if (GP_block[6] == 0xFE || GP_block[6] == 0 
#ifdef INTERLOCK
|| GP_block[6] == 1
#endif
                  ) {
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
    starttimeout = 80;
    GreenLEDOn();
    YellowLEDOff();
    DNID = -1;
    dgcnt = 0;

    // Initialize if EventTable is all zero
    ProgramMemoryRead((unsigned short long)&EventTable[0],64,(BYTE * far)&GP_block[0]);
    for (i=0; i<64; i++) {
        if (GP_block[i]!=0)
            break;
        if (i==63)
            SetDefault();
    }

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
        // 25 msec timer
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
            if (starttimeout)
                starttimeout--;

            // call scan every 25msec        
            scan(); // takes at least 16 x 0.1 ms

            // start of day, 1 every 25msec, about 2.5 secs
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
                if (t && SendMessage()==0) 
                    startofday++; // try again next time  
            }
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
    }
}

//*********************************************************************************

