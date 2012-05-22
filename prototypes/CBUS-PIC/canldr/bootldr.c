/*  CBUS Module Hardware, OpenLCB Boot loader

    22 Dec 2009

    This boot loader will occupy the bottom 4k of memory    
    The real code for the module will start at 0x001000.
    The interrupts routines will be mapped to 0x001008 and 0x001018.

    Uses 4 MHz resonator and PLL for 16 MHz clock

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

#include "../canlib/frametypes.c"
#include "../canlib/general.c"
//#include "../canlib/eeprom.c"
#ifdef CAN
#include "../canlib/ecan.c"
#endif
#ifdef RS232
#define SERIAL
#include "../canlib/serial.c"
#endif
#ifdef USB
#define SERIAL
#include "../canlib/serial.c"
#endif

//*********************************************************************************
//    Config register settings, only set in the boot loader
//*********************************************************************************

#ifdef __18F2480
#pragma config OSC=HSPLL, FCMEN=ON, IESO=OFF                           // 1
#pragma config BORV=0, BOREN=BOHW, PWRT=ON                             // 2
#pragma config WDT=OFF, WDTPS=8192, MCLRE=ON, LPT1OSC=OFF, PBADEN=OFF  // 3
#pragma config DEBUG=OFF, XINST=OFF, BBSIZ=1024, LVP=OFF, STVREN=ON    // 4
#pragma config CP0=OFF, CP1=OFF, CPB=OFF, CPD=OFF                      // 5
#pragma config WRT0=OFF, WRT1=OFF, WRTB=OFF, WRTC=OFF, WRTD=OFF        // 6
#pragma config EBTR0=OFF, EBTR1=OFF, EBTRB=OFF                         // 7
#endif

#ifdef __18F2580
#pragma config OSC=HSPLL, FCMEN=ON, IESO=OFF                           // 1
#pragma config BORV=1, BOREN=BOHW, PWRT=ON                             // 2
#pragma config WDT=OFF, WDTPS=8192, MCLRE=ON, LPT1OSC=OFF, PBADEN=OFF  // 3
#pragma config DEBUG=OFF, XINST=OFF, BBSIZ=1024, LVP=OFF, STVREN=ON    // 4
#pragma config CP0=OFF, CP1=OFF, CP2=OFF, CP3=OFF, CPB=OFF, CPD=OFF    // 5
#pragma config WRT0=OFF, WRT1=OFF, WRT2=OFF, WRT3=OFF, WRTB=OFF, WRTC=OFF, WRTD=OFF // 6
#pragma config EBTR0=OFF, EBTR1=OFF, EBTR2=OFF, EBTR3=OFF, EBTRB=OFF   // 7
#endif

//*********************************************************************************
//    define RAM storage
//*********************************************************************************

#pragma udata

BYTE dgcnt;
int DNID;

BYTE outptr, sendptr;
#pragma udata outbuf
far BYTE outbuf[256];
#pragma udata

//*********************************************************************************
//        Forward References
//*********************************************************************************

void main(void);
void __hpint(void);
void loader(void);

//*********************************************************************************
//        Fixed location rom data
//*********************************************************************************

#pragma code __entry = 0x000000
void __entry (void)
{
    _asm
        goto main
    _endasm
}

// Due to a bug involving 2 cycle instructions and the fast save registers
// with some versions of chip we have to implement this work around
#pragma code hpintvector = 0x000008
void hpintvector(void)
{
    _asm
        call __hpint,1
    _endasm
}

void __hpint(void)
{
    _asm
        pop
        goto STARTADDRESS+0x000008
    _endasm
}

#pragma code lowintvector = 0x000018
void lowintvector(void)
{
    _asm
        goto STARTADDRESS+0x000018
    _endasm
}

#pragma code jumptoloader = LOADERADDRESS
void jumptoloader(void)
{
    _asm
        goto loader
    _endasm
}

#pragma romdata romnodedata = NODEDATA
#ifdef SERIAL
rom BYTE ND_nodeid[6] = { 0x01, 0x00, 0x00, 0x00, 0x00, 0x00 };
#else
rom BYTE ND_nodeid[6] = { 0x02, 0x00, 0x00, 0x00, 0x00, 0x00 };
#endif
rom BYTE ND_seedid[6] = { 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF };
rom BYTE ND_alias[2] = { 0xFF, 0xFF };
rom BYTE ND_spare[64-14] = {
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
};

// 0x000080
const rom BYTE ustr[64] = "Location not yet defined";

const rom BYTE xml[] = "<cdi><id><loader>OpenLCB PIC "
#ifdef USB
    "USB Boot Loader "
#endif
#ifdef RS232
     "RS232 Boot Loader  "
#endif
#ifdef CAN
    "CAN Boot Loader " 
#endif
     __DATE__ " " __TIME__
    "</loader></id><se na=\"Loc\" or=\"#0040\" sp=\"#FE\" bu=\"#143\">"
    "<by na=\"Node Id\" si=\"6\"/>"
    "<by si=\"58\"/>"
    "<ch na=\"Loc\" si=\"64\"/></se></cdi>";

#pragma code
#pragma udata ovrly

//*********************************************************************************
//    Serial input
//*********************************************************************************

#ifdef USB

BOOL GetSerial(void)
{
    if (USBRXF == 0) {
        TRISC = 0xFF;
        USBRD = 1;
        USBRD = 0;        // RD strobe, reads on low
        byte = PORTC;
        USBRD = 1;
        return TRUE;
    }
    return FALSE;
}

BOOL PutSerial(BYTE c)
{
    while (USBTXE == 1) ;
    TRISC = 0x00;
    PORTC = c;
    USBWR = 1;
    USBWR = 0;
    TRISC = 0xFF;
    return TRUE;
}

#endif

#ifdef RS232

BOOL GetSerial(void)
{
    if (PIR1bits.RCIF) { // Receive buffer loaded
        byte = RCREG;    // also clears RCIF
        return TRUE;
    }
    return FALSE;
}

BOOL PutSerial(BYTE c)
{
    if ((outptr+1) == sendptr)
        return FALSE;
    outbuf[outptr++] = c;
    return TRUE;
}

void SerialIO(void)
{
    if (outptr==sendptr) // anything to send ?
        return;
    if (PIR1bits.TXIF==0) // Transmit buffer empty ?
        return;
    TXREG = outbuf[sendptr++];
    // sendptr &= SENDMASK;
}
#endif

//*********************************************************************************
//    Loader
//*********************************************************************************

BOOL SendMessage(void)
{
#ifdef CAN
    return ECANSendMessage();
#endif
#ifdef SERIAL
#ifdef RS232
    SerialIO();
    if (((sendptr-outptr-1)&0xE0)==0) // check space in buffer
        return FALSE;
#endif
    return PrintPacket();
#endif
}

BOOL ReceiveMessage()
{
#ifdef CAN
    return ECANReceiveMessage();
#endif
#ifdef SERIAL
    return ReadPacket();
#endif
}

void Packet(void)
{
    far overlay BYTE i;

    if (CB_SourceNID == ND.nodeIdAlias) { // conflict
        if ((CB_FrameType&0x8000)==0x0000) { // CIM or RID message
            CB_SourceNID = ND.nodeIdAlias;
            CB_FrameType = FT_RID;
            CB_datalen = 0;
            while (SendMessage()==0) ;
        }
        else
            CheckAlias(1);                  // get new alias
    }
    else if (CB_FrameType == FT_VNSN) { // send full NID
        SendNSN(FT_NSN);
    }
    else if (CB_FrameType==(FT_DGM|ND.nodeIdAlias) || CB_FrameType==(FT_DGL|ND.nodeIdAlias)
      || CB_FrameType==(FT_DGF|ND.nodeIdAlias) || CB_FrameType==(FT_DGS|ND.nodeIdAlias)) {
        if ((HI(CB_FrameType)&0xF0)==(FT_DGS>>8) || (HI(CB_FrameType)&0xF0)==(FT_DGF>>8)) { // first packet
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
                }
                else if (CB_data[1] == DGM_UPDCOMP) {
		          // change the valid program flag
		          ProgramMemoryRead(STARTADDRESS,64,(BYTE * far)GP_block);
		          if (GP_block[0x0020]!=0) {
		              GP_block[0x0020] = 0;
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
            } // memory op
            sendnack(CB_SourceNID,0);
        } // end of datagram
    } // datagram
}

void Loader2(void)
{
    SendNSN(FT_INIT);
    dgcnt = 0;
    DNID = -1;
    while(1) {
#ifdef RS232
        SerialIO();
#endif
        EndSendBlock();

        if (ReceiveMessage()) {
            Packet();
        }
    }
}

void initialize(void)
{
    ADCON1 = 0x0F;       // turn off A/D, all digital I/O
    TRISA = 0x3C;        // Port A outputs except S1 input
    TRISB = 0x3B;        // RB0 = input, RB1 = input, RB2 = CANTX, RB3 = CANRX,
                         // RB4,5 input, RB6,7 LED output
    PORTB = 0x04;        // CAN recessive
    TRISC = 0xFF;        // Port C all input.
    INTCON2 = 0;         // portb pullup enable

    outptr = 0;          // initialize buffer pointers
    sendptr = 0;

#ifdef SERIAL
    InitSerial();
#endif

    YellowLEDOn();
    GreenLEDOn();
}

void loader(void)
{
    initialize();
    Loader2();
}

//*********************************************************************************
// Main
//*********************************************************************************

void __zero_memory(void);

void main(void)
{
    _asm
        // Initialize the stack pointer
        lfsr 1, _stack
        lfsr 2, _stack
    _endasm

    __zero_memory();    // clear all ram

    // read valid pogram loaded flag from program memory
    TBLPTRL = 0x20;
    TBLPTRH = STARTADDRESS >> 8;
    TBLPTRU = 0;
    _asm
        tblrd
    _endasm
    if (TABLAT == 0) {
        _asm
            goto STARTADDRESS    // start loaded program
        _endasm
    }

    // no valid program loaded

    initialize();
#ifdef CAN
    ECANInitialize();
#endif
    CheckAlias(0);
    while(1) {
        Loader2();
    }
}