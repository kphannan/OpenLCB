/*  OpenLCB general.c

    15 Dec 2009

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

#ifdef __18F2480
#include "p18F2480.h"
#endif

#ifdef __18F2580
#include "p18F2580.h"
#endif

#ifdef __18F2585
#include "p18F2585.h"
#endif

#ifdef __18F2680
#include "p18F2680.h"
#endif

#ifdef __18F4580
#include "p18F4580.h"
#endif

typedef enum _BOOL { FALSE = 0, TRUE } BOOL;
typedef unsigned char BYTE;
#define LO(n) *((unsigned char *)&n)
#define HI(n) *((unsigned char *)&n + 1)
#define UP(n) *((unsigned char *)&n + 2)
#define LOHI(n) *((unsigned char *)&n + 2)
#define HIHI(n) *((unsigned char *)&n + 3)
#define LOWD(n) *((unsigned int *)&n)

//        Startup code and interrupts

#define LOADERADDRESS  0x000030
#define NODEDATA       0x000040
#define STARTADDRESS   0x001000
#define HIGHINTADDRESS 0x001008
#define LOWINTADDRESS  0x001018

#define ECANBuffer0Full (TXB0CON & 0x08)
#define ECANBuffersFull ((TXB0CON & 0x08) && (TXB1CON & 0x08))

//*********************************************************************************
//    define RAM storage
//*********************************************************************************

#pragma udata
typedef enum _ECAN_MSG_FLAGS  {
    ECAN_OVERFLOW      = 0b00001000,
    ECAN_INVALID_MSG   = 0b00010000
} ECAN_MSG_FLAGS;

// Ecan message data
unsigned int CB_SourceNID;       // 12 bit source NID alias
unsigned int CB_FrameType;       // 16 bit frame type
BYTE CB_data[8];                 // up to 8 bytes of data
BYTE CB_datalen;                 // length of data
ECAN_MSG_FLAGS CB_msgFlags;      // message flags or'ed together

BYTE SendBlockCount;
BYTE SendBlockMax;
unsigned int SendBlockNID;

// Data stored in Program Memory has restrictions
// Program Memory erase is for aligned 64 byte blocks
// Write is 8 bytes at a time

struct {                         // ram copy of nodedata
    BYTE nodeId[6];
    BYTE seedNodeId[6];
    unsigned int nodeIdAlias;
} ND;

#pragma udata blockdata
BYTE far GP_block[72];	              // for program memory reads and writes
far unsigned short long GP_address;      // block read or write address

#pragma udata ovrly

//*********************************************************************************
//        Forward References
//*********************************************************************************

BOOL SendMessage(void);
BOOL ReceiveMessage(void);
void SendNSN(unsigned int ft);

//*********************************************************************************
//         Green and Yellow Leds
//*********************************************************************************

#define GreenLEDOn()   PORTBbits.RB7 = 1
#define GreenLEDOff()  PORTBbits.RB7 = 0
#define YellowLEDOn()  PORTBbits.RB6 = 1;
#define YellowLEDOff() PORTBbits.RB6 = 0;

//*********************************************************************************
//    Timer 3	1/10 th seconds
//*********************************************************************************

// 25 msec timer

void Timer3Init(void)
{
    T3CON = 0b00110001;             // Timer 3 16bit R/W, 16MHz/32
    TMR3H = (0x10000-12500)>>8;     // 50000 for (16,000,000/32/40)
    TMR3L = (0x10000-12500) & 0xFF;
}

BOOL Timer3Test(void)
{
    if (PIR2bits.TMR3IF) {
        TMR3H = (0x10000-12500)>>8; // 50000 for (16,000,000/32/40)
        TMR3L = (0x10000-12500) & 0xFF;
        PIR2bits.TMR3IF = 0;
        return TRUE;
    }
    return FALSE;
}

//*********************************************************************************
//        Program Memory read and write
//*********************************************************************************

// read a block of up to 255 bytes
void ProgramMemoryRead(unsigned short long adr, BYTE len, BYTE * far buffer)
{
    TBLPTRL = LO(adr);
    TBLPTRH = HI(adr);
    TBLPTRU = UP(adr);
    while(len--) {
        _asm
            tblrdpostinc
        _endasm
        *buffer++ = TABLAT;
    }
}

// Erase and Write an aligned block of up to 64 bytes
// Write must be a multiple of 8 bytes
// Erase converts 0's to 1's, write converts 1's to o's

void ProgramMemoryWrite(unsigned short long adr, BYTE len, BYTE * far buffer)
{
    far overlay BYTE intsave;
    TBLPTRL = LO(adr);
    TBLPTRH = HI(adr);
    TBLPTRU = UP(adr);

    EECON1 = 0x94;            // program memory, row erase, write enable
    intsave = INTCON;
    INTCONbits.GIEH = 0;      // disable all high priority interrupts          
    INTCONbits.GIEL = 0;      // disable all low priority interrupts
    EECON2 = 0x55;
    EECON2 = 0xAA;
    EECON1bits.WR = 1;        // Start the write, cpu stalls for 2 msec
    INTCON = intsave;         // restore interrupts

    // because the tblptr has to be in the block being written its best to
    // use pre increment writes, which mean decrementing tblptr before we start

    _asm
        tblrdpostdec          // dummy read to dec tblptr
    _endasm

    while(len--) {
        TABLAT = *buffer++;
        _asm
            tblwtpreinc
        _endasm
        if ((TBLPTRL&0x07)==0x07 || len==0) {
            EECON1 = 0x84;          // program memory, write enable
            intsave = INTCON;
            INTCONbits.GIEH = 0;    // disable all high priority interrupts          
            INTCONbits.GIEL = 0;    // disable all low priority interrupts
            EECON2 = 0x55;
            EECON2 = 0xAA;
            EECON1bits.WR = 1;      // Start the write, cpu stalls for 2 msec
            INTCON = intsave;       // restore interrupts
        }        
    }
    EECON1bits.WREN = 0;     // Disable writes
}

//*********************************************************************************
//        Setup nodeId alias
//*********************************************************************************

// s = 0	Power on, get Alias from Flash memeory
// s = 1	Late conflict detected.
// long is 32 bits, short long is 24 bits.

void CheckAlias(BYTE cs)
{
    far overlay BYTE s = cs;
    far overlay BYTE i;
    far overlay BYTE t[6];
    far overlay unsigned long sum;

    if (s==0) {
        // read unique nodeId and alias
        ProgramMemoryRead(NODEDATA, sizeof ND, (BYTE * far)&ND);
        ND.seedNodeId[0] = ND.nodeId[0];
        ND.seedNodeId[1] = ND.nodeId[1];
        ND.seedNodeId[2] = ND.nodeId[2];
        ND.seedNodeId[3] = ND.nodeId[3];
        ND.seedNodeId[4] = ND.nodeId[4];
        ND.seedNodeId[5] = ND.nodeId[5];
    }
    else {
tryagain: 
        s = 1;	// set flag to save on exit
        // NID+1 = (2**9+1) NID + 0x1B0CA37A4BA9
        // t = seedNodeId << 8;
        t[0] = 0;
        t[1] = ND.seedNodeId[0];
        t[2] = ND.seedNodeId[1];
        t[3] = ND.seedNodeId[2];
        t[4] = ND.seedNodeId[3];
        t[5] = ND.seedNodeId[4];

        // ninth shift
    	*((unsigned short long *)&t[3]) <<= 1;
        if (t[2]&0x80)
    		t[3] |= 1;
    	*((unsigned short long *)&t[0]) <<= 1;

        // add low 24 bits
        sum = *((unsigned short long *)&t[0]);
        sum += *((unsigned short long *)&ND.seedNodeId[0]);
        sum += 0x7A4BA9;
        *((unsigned short long *)&ND.seedNodeId[0]) = *((unsigned short long *)&sum);
        // add high 24 bits plus carry from low 24 bits
        *((unsigned char *)&sum) = *((unsigned char *)&sum + 2);
        sum += *((unsigned short long *)&t[3]);
        sum += *((unsigned short long *)&ND.seedNodeId[3]);
        sum += 0x1B0CA3;
        *((unsigned short long *)&ND.seedNodeId[3]) = *((unsigned short long *)&sum);
    }

    if (s==1 || ND.nodeIdAlias==0 || ND.nodeIdAlias==0xFFFF) {
        // Convert 48 bit number to 12 bits by xor 4 groups of 12 bits
        ND.nodeIdAlias =  *((unsigned int *)&ND.seedNodeId[0]);
        ND.nodeIdAlias ^= *((unsigned int *)&ND.seedNodeId[1]) >> 4;
        ND.nodeIdAlias ^= *((unsigned int *)&ND.seedNodeId[3]);
        ND.nodeIdAlias ^= *((unsigned int *)&ND.seedNodeId[4]) >> 4;
        s = 1;	// set flag to save on exit
    }

    // send out the Alias in 4 CIM packets
    ND.nodeIdAlias &= 0x0FFF; 
    i = 0;
    while(TRUE) {
        if (!ECANBuffer0Full) {
            CB_SourceNID = ND.nodeIdAlias;  // alias
            CB_datalen = 0;
            if (i==0) {
                CB_FrameType = FT_CIM0;
                CB_FrameType |= *((unsigned int *)&ND.nodeId[4]) >> 4;
                SendMessage();
                i=1;
            }
            else if (i==1) {
                CB_FrameType =  FT_CIM1;
                CB_FrameType |= *((unsigned int *)&ND.nodeId[3]) & 0x0FFF;
                SendMessage();
                i=2;
            }
            else if (i==2) {
                CB_FrameType =  FT_CIM2;
                CB_FrameType |= *((unsigned int *)&ND.nodeId[1]) >> 4;
                SendMessage();
                i=3;
            }
            else if (i==3) {
                CB_FrameType =  FT_CIM3;
                CB_FrameType |= *((unsigned int *)&ND.nodeId[0]) & 0x0FFF;
                SendMessage();
                i=4;
            }
            else if (i==4) { // start the timer
                Timer3Init();
                t[0] = 0;
                i=5;
           }
        }
        if (ReceiveMessage()) {
            if (CB_SourceNID==ND.nodeIdAlias) // someone objects
                goto tryagain;
        }
        if (Timer3Test()) {
            t[0]++;
            if (t[0]>6)
                break;
        }
    }
     
    // save Alias in Flash for next power on
    // a small chance of losing the unique nodeId when it erases
    if (s==1) {
        ProgramMemoryWrite(NODEDATA, sizeof ND, (BYTE * far)&ND);
    }
    CB_FrameType = FT_RID;
    CB_SourceNID = ND.nodeIdAlias;
    CB_datalen = 0;
    while (SendMessage()==0) ;
    SendNSN(FT_AMD);
}

//*********************************************************************************
//    SendNSN	Send node serial number
//*********************************************************************************

void SendNSN(unsigned int ft)
{
    CB_FrameType = ft;
    CB_SourceNID = ND.nodeIdAlias;
    CB_datalen = 6;
    CB_data[0] = ND.nodeId[5];
    CB_data[1] = ND.nodeId[4];
    CB_data[2] = ND.nodeId[3];
    CB_data[3] = ND.nodeId[2];
    CB_data[4] = ND.nodeId[1];
    CB_data[5] = ND.nodeId[0];
    while (SendMessage()==0) ;
}

//*********************************************************************************
//    SendAck	Send ACk or Error back to Caller
//*********************************************************************************

void sendack(unsigned int DNID)
{
    CB_SourceNID = ND.nodeIdAlias;
    CB_FrameType = FT_DGS | DNID;
    CB_datalen = 1;
    CB_data[0] = DG_OK;
    while (SendMessage()==0) ;    
}

void sendnack(unsigned int DNID, unsigned int err)
{
    CB_SourceNID = ND.nodeIdAlias;
    CB_FrameType = FT_DGS | DNID;
    CB_datalen = 3;
    CB_data[0] = DG_ERR;
    CB_data[1] = HI(err);
    CB_data[2] = LO(err);
    while (SendMessage()==0) ;    
}

//*********************************************************************************
//    sendblock	Send a block of 64 bytes back to Caller
//*********************************************************************************

// Send a block of 64 bytes from Program memory to the PC 
// Node has just received a send block request

void StartSendBlock(BYTE l, int DNID)
{
    GP_block[1] = 0x30; // read reply
    SendBlockCount = 0;
    SendBlockMax = l;
    SendBlockNID = DNID;
}

void EndSendBlock(void)
{
    if (SendBlockCount>=SendBlockMax || ECANBuffersFull)
        return;
    // send data bytes
    CB_SourceNID = ND.nodeIdAlias;
    CB_FrameType = FT_DG | SendBlockNID;
    CB_datalen = 8;
    if (SendBlockMax-SendBlockCount <= 8) {
        CB_datalen = SendBlockMax-SendBlockCount;
        if (SendBlockMax<=8)
            CB_FrameType = FT_DGS | SendBlockNID;
        else
            CB_FrameType = FT_DGL | SendBlockNID;
    }
    CB_data[0] = GP_block[SendBlockCount+0];
    CB_data[1] = GP_block[SendBlockCount+1];
    CB_data[2] = GP_block[SendBlockCount+2];
    CB_data[3] = GP_block[SendBlockCount+3];
    CB_data[4] = GP_block[SendBlockCount+4];
    CB_data[5] = GP_block[SendBlockCount+5];
    CB_data[6] = GP_block[SendBlockCount+6];
    CB_data[7] = GP_block[SendBlockCount+7];
    SendBlockCount += 8;
    SendMessage();    
}

//*********************************************************************************
//    void Loader(void)        jump to loader
//*********************************************************************************

// called after a DAA_UPGSTART with caller still in CB_SourceNID

void Loader(void)
{
    sendack(CB_SourceNID);
    _asm
        goto LOADERADDRESS
    _endasm
}

//*********************************************************************************


