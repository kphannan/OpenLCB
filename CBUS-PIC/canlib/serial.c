/*
    OpenLCB Serial routines

    22 Dec 2009

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
//    define RAM storage
//*********************************************************************************

#ifdef USB
#define USBRXF PORTBbits.RB0
#define USBTXE PORTBbits.RB1
#define USBWR PORTBbits.RB4
#define USBRD PORTBbits.RB5
#endif

#ifdef RS232
#define SERIALCTS PORTBbits.RB0
#define SERIALRTS PORTBbits.RB1
#endif

#pragma udata

// Serial Input Buffer for 1 packet
BYTE RXindex;           // index into buffer
BYTE RXbuf[30];         // receive packet buffer
BYTE byte;              // GetSerial input character 
BOOL errorflag;         // Used by Gethex to signal invalid character

#pragma udata ovrly
#pragma romdata

//*********************************************************************************
//    Forward reference
//*********************************************************************************

BOOL PutSerial(BYTE c);
BOOL GetSerial(void);

//*********************************************************************************
// Serial routines
//*********************************************************************************

void InitSerial(void)
{
    far overlay BYTE i;
#ifdef RS232
    TRISB = 0x3A;        // RB0 is CTS, RB1 is RTS,  RB2 = CANTX, RB3 = CANRX
                         // RB4,5 are logic, RB6,7 diagnostic LEDs
    PORTB = 0x04;        // CAN recessive
    TRISC = 0xBF;        // Port C 6,7 are serial.

//  Serial interface
    SPBRG = 34;          // 16 for 230400, 34 for Baud = 115200 
    SPBRGH = 0;          // for Baud = 9600 use 1, for 115KB use 0
    BAUDCON = 0x08;      // set BAUDCON to 16 bit
    TXSTA = 0x24;        // set transmit 8 bit, enable, async, high speed(brgh=1)    
    RCSTA = 0x90;        // set serial enable, continuous receive enable, 8 bit
    SERIALCTS = 0;
#endif
#ifdef USB
    TRISB = 0x0B;        // RB0 = RXF input, RB1 = TXE input, RB2 = CANTX, RB3 = CANRX,
                         // RB4 = WR, RB5 = RD, RB6,7 diagnostic LEDs
    TRISC = 0xFF;        // Port C USB data, all input.
    PORTB = 0x04;        // CAN recessive
    USBRD = 1;
    USBWR = 0;
#endif

    i = 0x80;            // short delay needed before serial output, don't know why
    while (--i) ;
    RXindex = 0;
}

// output 2 hex ascii characters from 1 data byte

void Put1Hex(BYTE a)
{
    far overlay BYTE c;
    c = (a & 0x0F) + 0x30;
    if (c > 0x39)
        c += 7;
    PutSerial(c);
}

void Put2Hex(BYTE a)
{
    Put1Hex(a >> 4);
    Put1Hex(a);
}

void PutRomString(BYTE rom far *p)
{
    while (*p) 
        PutSerial(*p++);
}

BYTE GetHex(BYTE a)
{
    far overlay BYTE c = a;
    if (c>='0' && c<='9')
        return c-'0';
    if (c>='A' && c<='F')
        return c-'A'+10;
    if (c>='a' && c<='f')
        return c-'a'+10;
    errorflag = TRUE;
    return 0;
}

BYTE Get2Hex(BYTE a, BYTE b)
{
    return (GetHex(a)<<4) | GetHex(b);
}

BOOL PrintPacket(void)
{
    far overlay BYTE i;
    PutSerial(':');
    PutSerial('X');
    Put1Hex(1);                  // Priority
    Put2Hex(HI(CB_FrameType));
    Put2Hex(LO(CB_FrameType));
    Put1Hex(HI(CB_SourceNID));
    Put2Hex(LO(CB_SourceNID));
    PutSerial('N');
    for (i=0; i<CB_datalen; i++)
        Put2Hex(CB_data[i]);
    PutSerial(';');
    PutSerial('\r');
    PutSerial('\n');
    return TRUE;
}

BOOL ReadPacket(void)
{
    far overlay BYTE i, j;

    if (GetSerial()==FALSE)
        return FALSE;

    if (byte==':') { // start of message
        RXindex = 0;
        return FALSE;
    }

    if (byte!=';') { // part of message
        if (RXindex >= 29) {
           RXindex = 0;
        }
        RXbuf[RXindex++] = byte;
        return FALSE;
    }

    // end of message = complete msg in buffer
    RXbuf[RXindex++] = byte;
    errorflag = FALSE;
    
    if (RXbuf[0]!='X' && RXbuf[0]!='x')
        goto error;

    // header
    HI(CB_FrameType) = GetHex(RXbuf[1]);
    HI(CB_FrameType) = Get2Hex(RXbuf[2], RXbuf[3]);
    LO(CB_FrameType) = Get2Hex(RXbuf[4], RXbuf[5]);
    HI(CB_SourceNID) = GetHex(RXbuf[6]);
    LO(CB_SourceNID) = Get2Hex(RXbuf[7], RXbuf[8]);

    if (errorflag || (RXbuf[9]!='N' && RXbuf[9]!='n'))
        goto error;

    // data
    j = 10;
    for (i=0; i<8; i++, j+=2) {
        if (RXbuf[j]==';')
            break;
        CB_data[i] = Get2Hex(RXbuf[j], RXbuf[j+1]);
    }
    CB_datalen = i;

    RXindex = 0;
    if (errorflag) {
error:
        PutRomString((BYTE rom far *)" Error\r\n");
        return FALSE;
    }
    return TRUE;
}

//*********************************************************************************
