/*
    OpenLCB - Only extended frames

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

#pragma udata ovrly
#pragma romdata

//*********************************************************************************
// void ECANInitialize(void)
//*********************************************************************************

void ECANInitialize(void)
{
    // Put module into Configuration mode.
    CANCON &= 0x1F;                  // clear previous mode
    CANCON |= 0x80;                  // set config mode
    while( (CANCON & 0xE0) != 0x80); // Wait till desired mode is set.

    // Bit rate 125000
#ifdef MHZ32 // 32 MHz processor
    BRGCON1 = 0x07;            // SJW=0, 1 x Tq, prescaler BRP=8
    BRGCON2 = 0xDE;            // DE or 9E Sample=thrice or once?, phseg1=4, propseg=7
    BRGCON3 = 0x03;            // WAKEUP enable, filter disable, phseg2=4
#else // 16 MHz processor
    BRGCON1 = 0x03;            // SJW=0, 1 x Tq, prescaler BRP=4
    BRGCON2 = 0xDE;            // DE or 9E Sample=thrice or once?, phseg1=4, propseg=7
    BRGCON3 = 0x03;            // WAKEUP enable, filter disable, phseg2=4
#endif
    CIOCON  = 0x20;            // drive high(not tristate), disable capture mode
    ECANCON = 0x80;            // mode 2
    ECANCONbits.FIFOWM = 1;    // 1 buffer left warning

    // Make B0-B5 Receive buffers.
    BSEL0 = 0;

    // Set receive buffer modes.
    RXB0CON = 0;    // 0 = only valid msgs, 0x40 everything
    RXB1CON = 0;
    B0CON = 0;
    B1CON = 0;
    B2CON = 0;
    B3CON = 0;
    B4CON = 0;
    B5CON = 0;

    // Set all masks to ignore and all filters to accept
    RXM0SIDH = 0;           // masks 0
    RXM1SIDH = 0;                          
    RXM0SIDL = 0x08;        // use bit in RXFnSIDL
    RXM1SIDL = 0x08;         
    RXM0EIDH = 0;
    RXM1EIDH = 0;
    RXM0EIDL = 0;
    RXM1EIDL = 0;
    RXFCON0 = 0xFF;         // 0 = disable, 0xFF= enable
    RXFCON1 = 0xFF;         
    SDFLC = 0;              // I don't understand this ?
    MSEL0 = 0;              // use mask 0
    MSEL1 = 0;
    MSEL2 = 0;
    MSEL3 = 0;                
    RXF0SIDL = 0x08;        // accept only extended msgs
    RXF1SIDL = 0x08;
    RXF2SIDL = 0x08;
    RXF3SIDL = 0x08;
    RXF4SIDL = 0x08;
    RXF5SIDL = 0x08;
    RXF6SIDL = 0x08;
    RXF7SIDL = 0x08;
    RXF8SIDL = 0x08;
    RXF9SIDL = 0x08;
    RXF10SIDL = 0x08;
    RXF11SIDL = 0x08;
    RXF12SIDL = 0x08;
    RXF13SIDL = 0x08;
    RXF14SIDL = 0x08;
    RXF15SIDL = 0x08;

    // Switch back to Normal mode
    CANCON &= 0x1F;            // clear previous mode & set Normal mode
    while(CANCON & 0xE0) ;     // Wait till normal mode is set.
}

//*********************************************************************************
// Function:        ECANSendMessage
// Input:           CB_SourceNID - 12 bits header
//                  CB_FrameType - 16 bits header
//                  CB_data      - Data bytes of up to 8 bytes in length
//                  CB_dataLen   - Data length from 0 thru 8.
// Result:          TRUE, if an empty buffer was found and loaded
//*********************************************************************************

// only 1 buffers used to maintain transmit packet order

#define ECANBufferFull (TXB0CON & 0x08)

BOOL ECANSendMessage(void)
{
    if (TXB0CON & 0x08)
        return FALSE;

    TXB0CON &= 0xFC;                     // clear send priority bits
    TXB0DLC = CB_datalen;

    // Rearrange header
    TXB0SIDH = 0x80 | (HI(CB_FrameType) >> 1);
    TXB0SIDL = ((HI(CB_FrameType)&0x01) << 7 ) | ((LO(CB_FrameType)&0xC0)>>1) 
               | 0x08 | ((LO(CB_FrameType)&0x30)>>4);
    TXB0EIDH = (HI(CB_SourceNID)&0x0F) | ((LO(CB_FrameType)&0x0F)<<4);
    TXB0EIDL = LO(CB_SourceNID);

    // Copy data bytes.
    TXB0D0 = CB_data[0];
    TXB0D1 = CB_data[1];
    TXB0D2 = CB_data[2];
    TXB0D3 = CB_data[3];
    TXB0D4 = CB_data[4];
    TXB0D5 = CB_data[5];
    TXB0D6 = CB_data[6];
    TXB0D7 = CB_data[7];

    TXB0CON |= 0x08;                    // set TXREQ bit.
    return TRUE;
}

//*********************************************************************************
// Function:    BOOL ECANReceiveMessage
// Output:        CB_SourceNID - 12 bits header
//                CB_FrameType - 16 bits header
//                CB_data        - 8 byte data buffer
//                CB_dataLen     - buffer for count of bytes copied
//                CB_msgFlags    - buffer for or'ed error flags
// Result:        TRUE, if a full receive buffer was found 
//*********************************************************************************

BOOL ECANReceiveMessage(void)
{
    far overlay BYTE i;
    far overlay BYTE * far ptr;        // ECAN buffer pointers

    if (!COMSTATbits.FIFOEMPTY) //if FIFO empty 
        return FALSE;

    // Set access ram pointers to the buffer
    switch(CANCON&0x07) {
    case 0:
        ptr = (BYTE far *)&RXB0CON;
        break;
    case 1:
        ptr = (BYTE far *)&RXB1CON;
        break;
    case 2:
        ptr = (BYTE far *)&B0CON;
        break;
    case 3:
        ptr = (BYTE far *)&B1CON;
        break;
    case 4:
        ptr = (BYTE far *)&B2CON;
        break;
    case 5:
        ptr = (BYTE far *)&B3CON;
        break;
    case 6:
        ptr = (BYTE far *)&B4CON;
        break;
    default:
        ptr = (BYTE far *)&B5CON;
    }

    CB_msgFlags = 0;
    if ( COMSTATbits.RXB1OVFL ) { // test for overflow
        CB_msgFlags |= ECAN_OVERFLOW;
        COMSTATbits.RXB1OVFL = 0;
    }

    // Get FrameType and SourceNID
    HI(CB_FrameType) = *(ptr+1) << 1;
    HI(CB_FrameType) |= (*(ptr+2) & 0x80) >> 7;
    LO(CB_FrameType) = (*(ptr+2) & 0x60) << 1;
    LO(CB_FrameType) |= (*(ptr+2) & 0x03) << 4;
    LO(CB_FrameType) |= *(ptr+3) >> 4;
    HI(CB_SourceNID) = *(ptr+3) & 0x0F;
    LO(CB_SourceNID) = *(ptr+4);

    // Retrieve DLC, and check RTR bit
    CB_datalen = *(ptr+5) & 0x0F;

    // Get message data itself
    for ( i = 0; i < CB_datalen; i++ )
        CB_data[i] = ptr[i+6];

    // Record and Clear any previous invalid message bit flag.
    if ( PIR3bits.IRXIF ) {
        CB_msgFlags |= ECAN_INVALID_MSG;
        PIR3bits.IRXIF = 0;
    }

    // Mark that this buffer as empty.
    *ptr &= 0x7f;                // clear RXFUL bit
    PIR3bits.RXB1IF = 0;         // clear msg received interrupt flag
    COMSTATbits.FIFOEMPTY = 0;   // Workaround for Rev A1 silicon
    return TRUE;
}

//*********************************************************************************
