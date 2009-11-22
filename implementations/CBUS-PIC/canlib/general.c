/*
    OpenLCB

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
//        These addresses will change when a boot loader is implemented

#define NODEDATA 0x0040
#define STARTADDRESS   0x001000
#define HIGHINTADDRESS 0x001008
#define LOWINTADDRESS  0x001018
#define LOADERADDRESS  0x000030

//*********************************************************************************
//    define RAM storage
//*********************************************************************************

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

// Data stored in Program Memory has restrictions
// Program Memory erase is for aligned 64 byte blocks
// Write is 8 bytes at a time

struct {                         // ram copy of nodedata
    BYTE nodeId[6];
    BYTE seedNodeId[6];
    unsigned int nodeIdAlias;
} ND;

#pragma udata blockdata
#pragma udata rt
BYTE far GP_block[64];	                 // for program memory reads and writes
far unsigned short long GP_address;      // block read or write address

#pragma udata
#pragma udata ovrly
#pragma romdata

//*********************************************************************************
//        Frame Types
//*********************************************************************************

#define FT_CIM0      0x0000    // Top 12 bits of NID
#define FT_CIM1      0x1000    // 2nd top 12 bits of NID
#define FT_CIM2      0x2000    // 3rd top 12 bits of NID
#define FT_CIM3      0x3000    // lowest 12 bits of NID
//                   0x4000    // spare 
//                   0x5000    // spare 
//                   0x6000    // spare 
#define FT_MR        0x7FFE    // Reset mapping, not sure this is actually useful ?
#define FT_RIM       0x7FFF    // RIM
//                   0x8000    // Broadcast message types
//                   0x9000    // spare 
//                   0xA000    // spare 
//                   0xB000    // spare 
//                   0xC000    // spare 
//                   0xD000    // spare 
#define FT_DAA       0xE000    // Destination Alias Addressed, message type is in the data
#define FT_STREAM    0xF000    // Stream data

// Destination Addressed, 1st byte of data is MTI

// Misc
#define DAA_DATA     0x00      // up to 0F, 7 bytes of data sequence number in low 4 bits
#define DAA_ACK      0x10      // ack with status
#define DAA_NSN      0x1B      // Node serial number

// Loader
#define DAA_UPGSTART 0x11      // enter loader
#define DAA_UPGRESET 0x12      // start program
#define DAA_UPGREAD  0x13      // read 64 bytes
#define DAA_UPGADDR  0x14      // write 64 bytes

// Event teaching
#define DAA_EVERASE  0x15      // erase events
#define DAA_EVREAD   0x16      // read events
#define DAA_EVWRITE  0x17      // write event

// Node variables
#define DAA_NVRD     0x18      // read
#define DAA_NVSET    0x19      // set
#define DAA_NVANS    0x1A      // reply to read

// Broadcast type

// Misc
#define FT_RESET     0x8000    // System reset
#define FT_VNSN      0x8001    // Verify Node Serial Number 
#define FT_INIT      0x8002    // Normal Initialization Complete
#define FT_BOOT      0x8003    // Boot Loader Initialization Complete

// Accessory
#define FT_ACOF      0x8010    // Off
#define FT_ACON      0x8011    // On
#define FT_ASOF      0x8012    // Short Off
#define FT_ASON      0x8013    // Short On
#define FT_RFID      0x8014    // RFID tag

// Track commands
#define FT_TOF       0x8020    // Track Off, broadcast from CS
#define FT_TON       0x8021    // Track On or Normal operation, broadcast from CS
#define FT_ESTOP     0x8022    // Track Stopped (em. stop)
#define FT_CSRESET   0x8023    // Command station Reset
#define FT_RTOF      0x8024    // Request Track Off, from CAB
#define FT_RTON      0x8025    // Request Track On or Normal operation, from CAB
#define FT_RESTP     0x8026    // Request Emergency Stop ALL

// CAB commands
#define FT_RLOC      0x8030    // Request loco info
#define FT_STMOD     0x8031    // Request speed step change
#define FT_DSPD      0x8032    // Set Engine Speed/Dir
#define FT_DFUN      0x8033    // Set engine functions
#define FT_PLOC      0x8034    // Engine report from CS
#define FT_PLOCF     0x8035    // Engine function report from CS
#define FT_KLOC      0x8036    // Release loco

// Consist commands
#define FT_PCON      0x8037    // Consist Engine
#define FT_KCON      0x8038    // Remove engine from consist

// DCC programming
#define FT_RDCC3     0x8040    // Request 3 byte DCC packet
#define FT_RDCC4     0x8041    // Request 4 byte DCC packet
#define FT_RDCC5     0x8042    // Request 5 byte DCC packet
#define FT_RDCC6     0x8043    // Request 6 byte DCC packet

//*********************************************************************************
//        Forward References
//*********************************************************************************

BOOL SendMessage(void);
BOOL ReceiveMessage(void);

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

// 100 msec timer

void Timer3Init(void)
{
    T3CON = 0b00110001;   // Timer 3 16bit R/W, 16MHz/32
    TMR3H = 0x3C;         // 0x10000-50000 for (16,000,000/32/10)
    TMR3L = 0xB0;
}

BOOL Timer3Test(void)
{
    if (PIR2bits.TMR3IF) {
        TMR3H = 0x3C;
        TMR3L = 0xB0;
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
    far overlay unsigned int i;
    far overlay BYTE t[6];
    far overlay unsigned long sum;

    if (s==0) {
        // read unique nodeId and alias
        ProgramMemoryRead(NODEDATA, sizeof ND, (BYTE * far)&ND);
        *((unsigned short long *)&ND.seedNodeId[0]) = *((unsigned short long *)&ND.nodeId[0]);
        *((unsigned short long *)&ND.seedNodeId[3]) = *((unsigned short long *)&ND.nodeId[3]);
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
        ND.nodeIdAlias &= 0x0FFF; 
        s = 1;	// set flag to save on exit
    }

    // send out the Alias in 4 CIM packets
    CB_SourceNID = ND.nodeIdAlias;  // alias
    CB_FrameType = FT_CIM0;
    CB_FrameType |= (*((unsigned int *)&ND.nodeId[4]) & 0xFFF0) >> 4;
    CB_datalen = 0;
    while(SendMessage()==0) ;
    CB_FrameType =  FT_CIM1;
    CB_FrameType |= *((unsigned int *)&ND.nodeId[3]) & 0x0FFF;
    while(SendMessage()==0) ;
    CB_FrameType =  FT_CIM2;
    CB_FrameType |= (*((unsigned int *)&ND.nodeId[1]) & 0xFFF0) >> 4;
    while(SendMessage()==0) ;
    CB_FrameType =  FT_CIM3;
    CB_FrameType |= *((unsigned int *)&ND.nodeId[0]) & 0x0FFF;
    while(SendMessage()==0) ;

    // wait 1 second to see if anyone objects
    Timer3Init();
    t[0] = 0;
    while (t[0] < 10) { // 1 second time out
        if (Timer3Test())
            t[0]++;
        if (ReceiveMessage()) {
            if (CB_SourceNID==ND.nodeIdAlias) { // someone objects
                goto tryagain;
            }
        }
    }

    // save Alias in Flash for next power on
    // a small chance of losing the unique nodeId when it erases
    if (s==1) {
        ProgramMemoryWrite(NODEDATA, sizeof ND, (BYTE * far)&ND);
    }
}

//*********************************************************************************
//    SendAck	Send ACk or Error back to Caller
//*********************************************************************************

// 0 for OK 
// 1 for CRC error (not used)
// 2 for timeout 
// 3 for no data 
// 4 for no space. 
// 5 wrong SourceAlias

void sendack(BYTE v, unsigned int DNID)
{
    CB_SourceNID = ND.nodeIdAlias;
    CB_FrameType = FT_DAA | DNID;
    CB_datalen = 2;
    CB_data[0] = DAA_ACK;
    CB_data[1] = v;
    while (SendMessage()==0) ;    
}

//*********************************************************************************
//    sendblock	Send a block of 64 bytes back to Caller
//*********************************************************************************

// Send a block of 64 bytes from Program memory to the PC 
// Node has just received a send block request

void sendblock(unsigned int DNID)
{
    far overlay BYTE i;
    far overlay BYTE j;

    UP(GP_address) = CB_data[1];
    HI(GP_address) = CB_data[2];
    LO(GP_address) = CB_data[3];

    // get the data
    ProgramMemoryRead(GP_address, 64, (BYTE *)&GP_block[0]);

    // send data packets
    for (i=0, j=0; i<64; i+=7, j++) {
        // send data bytes
        CB_SourceNID = ND.nodeIdAlias;
        CB_FrameType = FT_DAA | DNID;
        CB_datalen = 8;
        CB_data[0] = DAA_DATA | j;
        CB_data[1] = GP_block[i+0];
        CB_data[2] = GP_block[i+1];
        CB_data[3] = GP_block[i+2];
        CB_data[4] = GP_block[i+3];
        CB_data[5] = GP_block[i+4];
        CB_data[6] = GP_block[i+5];
        CB_data[7] = GP_block[i+6];
        while (SendMessage()==0) ;    
    }
}

//*********************************************************************************
//    void Loader(void)        jump to loader
//*********************************************************************************

// called after a DAA_UPGSTART with caller still in CB_SourceNID

void Loader(void)
{
    sendack(0,CB_SourceNID);
    _asm
        goto 0x000030
    _endasm
}

//*********************************************************************************


