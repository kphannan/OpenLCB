/*  CBUS Module Hardware, OpenLCB Boot loader

    2 Dec 2009

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
#include "../canlib/eeprom.c"
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
#pragma config OSC=HSPLL, FCMEN=ON, IESO=OFF                    // 1
#pragma config BORV=1, BOREN=BOHW, PWRT=ON                    // 2
#pragma config WDT=OFF, WDTPS=8192, MCLRE=ON, LPT1OSC=OFF, PBADEN=OFF    // 3
#pragma config DEBUG=OFF, XINST=OFF, BBSIZ=1024, LVP=OFF, STVREN=ON    // 4
#pragma config CP0=OFF, CP1=OFF, CPB=OFF, CPD=OFF                // 5
#pragma config WRT0=OFF, WRT1=OFF, WRTB=OFF, WRTC=OFF, WRTD=OFF        // 6
#pragma config EBTR0=OFF, EBTR1=OFF, EBTRB=OFF                    // 7
#endif

#ifdef __18F2580
#pragma config OSC=HSPLL, FCMEN=ON, IESO=OFF                    // 1
#pragma config BORV=1, BOREN=BOHW, PWRT=ON                    // 2
#pragma config WDT=OFF, WDTPS=8192, MCLRE=ON, LPT1OSC=OFF, PBADEN=OFF    // 3
#pragma config DEBUG=OFF, XINST=OFF, BBSIZ=1024, LVP=OFF, STVREN=ON    // 4
#pragma config CP0=OFF, CP1=OFF, CP2=OFF, CP3=OFF, CPB=OFF, CPD=OFF    // 5
#pragma config WRT0=OFF, WRT1=OFF, WRT2=OFF, WRT3=OFF, WRTB=OFF, WRTC=OFF, WRTD=OFF // 6
#pragma config EBTR0=OFF, EBTR1=OFF, EBTR2=OFF, EBTR3=OFF, EBTRB=OFF    // 7
#endif

#ifdef __18F2585
#pragma config OSC=HSPLL, FCMEN=ON, IESO=OFF                    // 1
#pragma config BORV=1, BOREN=BOHW, PWRT=ON                    // 2
#pragma config WDT=OFF, WDTPS=8192, MCLRE=ON, LPT1OSC=OFF, PBADEN=OFF    // 3
#pragma config DEBUG=OFF, XINST=OFF, BBSIZ=1024, LVP=OFF, STVREN=ON    // 4
#pragma config CP0=OFF, CP1=OFF, CP2=OFF, CP3=OFF, CPB=OFF, CPD=OFF    // 5
#pragma config WRT0=OFF, WRT1=OFF, WRT2=OFF, WRT3=OFF, WRTB=OFF, WRTC=OFF, WRTD=OFF // 6
#pragma config EBTR0=OFF, EBTR1=OFF, EBTR2=OFF, EBTR3=OFF, EBTRB=OFF    // 7
#endif

#ifdef __18F4580    // for BC1A
#pragma config OSC=HS, FCMEN=ON, IESO=OFF                        // 1
#pragma config BORV=1, BOREN=BOHW, PWRT=ON                    // 2
#pragma config WDT=OFF, WDTPS=8192, MCLRE=ON, LPT1OSC=OFF, PBADEN=OFF    // 3
#pragma config DEBUG=OFF, XINST=OFF, BBSIZ=1024, LVP=OFF, STVREN=ON    // 4
#pragma config CP0=OFF, CP1=OFF, CP2=OFF, CP3=OFF, CPB=OFF, CPD=OFF    // 5
#pragma config WRT0=OFF, WRT1=OFF, WRT2=OFF, WRT3=OFF, WRTB=OFF, WRTC=OFF, WRTD=OFF // 6
#pragma config EBTR0=OFF, EBTR1=OFF, EBTR2=OFF, EBTR3=OFF, EBTRB=OFF    // 7
#endif

//*********************************************************************************
//    define RAM storage
//*********************************************************************************

#pragma udata

unsigned int timer;        // 1 msec ticks
unsigned int blocks;       // parts of a block received OK
BYTE t;
unsigned int DNID = 0;

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

#pragma code

#pragma romdata rominfo = 0x000020
rom unsigned int makercode = 1;
rom unsigned int productcode = 1;
rom unsigned int versioncode = 1;
#pragma romdata

#pragma code jumptoloader = LOADERADDRESS
void jumptoloader(void)
{
    _asm
        goto loader
    _endasm

}
#pragma code

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
#ifdef USB
rom BYTE idstring[64] = "OpenLCB PIC USB Boot Loader "  __DATE__ " " __TIME__;
#else
#ifdef RS232
rom BYTE idstring[64] = "OpenLCB PIC RS232 Boot Loader "  __DATE__ " " __TIME__;
#else
rom BYTE idstring[64] = "OpenLCB PIC CAN Boot Loader "  __DATE__ " " __TIME__;
#endif
#endif

// 0x0000C0
const rom BYTE ustr[64] = "User identification not yet defined";

#pragma romdata
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

void PutSerial(BYTE c)
{
    while (USBTXE == 1) ;
    TRISC = 0x00;
    PORTC = c;
    USBWR = 1;
    USBWR = 0;
    TRISC = 0xFF;
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

void PutSerial(BYTE c)
{
    while (PIR1bits.TXIF==0) ; // Transmit buffer empty ?
    TXREG = c;       // also clears TXIF
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
    PrintPacket();
    return TRUE;
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

rom unsigned int bits[10] = {
    0x03FE, 0x03FD, 0x03FB, 0x03F7, 0x03EF, 0x03DF, 0x03BF, 0x037F, 0x02FF, 0x01FF
};

void Packet(void)
{
    far overlay BYTE i;

    if (CB_SourceNID == ND.nodeIdAlias) { // conflict
        if ((CB_FrameType&0x8000)==0x0000) { // CIM or RIM message
            CB_SourceNID = ND.nodeIdAlias;
            CB_FrameType = FT_RIM;
            CB_datalen = 0;
            while (SendMessage()==0) ;
        }
        else
            CheckAlias(1);                  // get new alias
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
    else if (CB_FrameType == (FT_DAA | ND.nodeIdAlias) ) {
        if (CB_data[0] == DAA_UPGADDR) { // single GP_block write
            DNID = CB_SourceNID;
            UP(GP_address) = CB_data[1];
            HI(GP_address) = CB_data[2];
            LO(GP_address) = CB_data[3];
            blocks = 0x03FF; // 10 bits, 1 per block
            timer = 0;
        }
        else if ((CB_data[0]&0xF0) == DAA_DATA && blocks != 0) { // data GP_block
            if (DNID!=CB_SourceNID) {
               sendack(ACK_ALIASERROR, CB_SourceNID);
               return;
            }
            else {
                t = CB_data[0];
                blocks &= bits[t];
                t = t * 7;
                for (i = 1; i<8 && t<64; i++)
                    GP_block[t++] = CB_data[i];
                if (blocks == 0) {
                    if (UP(GP_address) <= 1) // Up to 128k Program memory
                        ProgramMemoryWrite(GP_address, 64, (BYTE * far)GP_block);
                    else if (UP(GP_address) == 0xF0) { // EEPROM 64k bytes
                        for (t=0; t<64; t++)
                            EEPROMWrite(LOWD(GP_address)+t, GP_block[t]);
                    }
                    sendack(ACK_OK, DNID);    // OK
                }
            }
        }
        else if (CB_data[0] == DAA_UPGREAD) { // single GP_block read
            sendblock(CB_SourceNID);
        }
        else if (CB_data[0] == DAA_UPGSTART) { // program upgrade
            sendack(ACK_OK, CB_SourceNID);    // Entered loader ack
        }
        else if (CB_data[0] == DAA_REBOOT) {
            // re-start the program
            _asm
                reset
                goto 0x000000
            _endasm
        }
        else if (CB_data[0] == DAA_UPGRESET) {
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
        else if (CB_data[0] == DAA_CEERASEH || CB_data[0] == DAA_DEFAULT)
            sendack(ACK_OK, CB_SourceNID);
        else if (CB_data[0] == DAA_NVRD || CB_data[0] == DAA_CEREADH 
          || CB_data[0] == DAA_PEREAD)
            sendack(ACK_NODATA, CB_SourceNID);
        else if (CB_data[0] == DAA_NVSET || CB_data[0] == DAA_CEWRITEH 
          || CB_data[0] == DAA_PEWRITEH)
            sendack(ACK_NOSPACE, CB_SourceNID);
    }
}

void Loader2(void)
{
    CB_SourceNID = ND.nodeIdAlias;
    CB_FrameType = FT_BOOT;
    CB_datalen = 0;
    while (SendMessage()==0) ;
    while(1) {
        if (Timer3Test()) {                // 100 msec timer
            timer++;
            if (blocks!=0 && timer>20) {   // send timeout ack
                sendack(ACK_TIMEOUT, DNID);          // timeout
                blocks = 0;
            }
        }
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

#ifdef SERIAL
    InitSerial();
#endif

    // timer 1 for 1 msec overflow
    Timer3Init();
    timer = 0;
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
    TBLPTRL = 0x27;
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