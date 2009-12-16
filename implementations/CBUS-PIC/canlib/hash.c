/*  OpenLCB hash.c

    15 Dec 2009

    DoEvent has to be defined in the calling code.    
    Define EVENTSIZE as the number of bytes of the data + 8.
    Define TABLESIZE, must be a multiple of the 64 byte bucket size.

    30 buckets = 1920
    35         = 2240
    40         = 2560
    45         = 2880
    50         = 3200
    55         = 3520
    60         = 3840

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

void DoEvent(static BYTE * far ev);

/* *******************************************************************************

For 2560 bytes of Program memory, divided into 40 buckets of 
64 bytes each 64 byte bucket holds 7 x 9 byte events.
So 7 x 40 = 280 is the maximum number of events
In practice 80% is a good maximum, so 224 is a better limit.

Short events have nodenumber 0
FF,FF,FF,FF,FF,FF,FF,FF is end of chain
FF,FF,FF,FF,FF,FF,FF,00 is a deleted entry to be stepped over

******************************************************************************** */

//*********************************************************************************
//        ROM module info
//*********************************************************************************

// These tables need to be aligned on 64 byte boundary

#pragma romdata table = 0x1100

BYTE rom table[TABLESIZE];          // initializes to zero, 64 byte aligned

rom BYTE hashdatatable[256] = {
      0,  94, 188, 226,  97,  63, 221, 131, 194, 156, 126,  32, 163, 253,  31,  65,
    157, 195,  33, 127, 252, 162,  64,  30,  95,   1, 227, 189,  62,  96, 130, 220,
     35, 125, 159, 193,  66,  28, 254, 160, 225, 191,  93,   3, 128, 222,  60,  98,
    190, 224,   2,  92, 223, 129,  99,  61, 124,  34, 192, 158,  29,  67, 161, 255,
     70,  24, 250, 164,  39, 121, 155, 197, 132, 218,  56, 102, 229, 187,  89,   7,
    219, 133, 103,  57, 186, 228,   6,  88,  25,  71, 165, 251, 120,  38, 196, 154,
    101,  59, 217, 135,   4,  90, 184, 230, 167, 249,  27,  69, 198, 152, 122,  36,
    248, 166,  68,  26, 153, 199,  37, 123,  58, 100, 134, 216,  91,   5, 231, 185,
    140, 210,  48, 110, 237, 179,  81,  15,  78,  16, 242, 172,  47, 113, 147, 205,
     17,  79, 173, 243, 112,  46, 204, 146, 211, 141, 111,  49, 178, 236,  14,  80,
    175, 241,  19,  77, 206, 144, 114,  44, 109,  51, 209, 143,  12,  82, 176, 238,
     50, 108, 142, 208,  83,  13, 239, 177, 240, 174,  76,  18, 145, 207,  45, 115,
    202, 148, 118,  40, 171, 245,  23,  73,   8,  86, 180, 234, 105,  55, 213, 139,
     87,   9, 235, 181,  54, 104, 138, 212, 149, 203,  41, 119, 244, 170,  72,  22,
    233, 183,  85,  11, 136, 214,  52, 106,  43, 117, 151, 201,  74,  20, 246, 168,
    116,  42, 200, 150,  21,  75, 169, 247, 182, 232,  10,  84, 215, 137, 107,  53
};

#pragma romdata

//*********************************************************************************
//        Event table
//*********************************************************************************

// convert the event number to a hash key using random numbers

unsigned int hash(static BYTE * far ev) {
    far overlay BYTE i, h = 0;
    far overlay unsigned int r;
    for (i=0; i<8; i++)
        h ^= hashdatatable[ev[i]];
    r = (unsigned int)h << 6;
    while (r >= TABLESIZE)
        r -= TABLESIZE;
    return r;
}

BOOL equal(static BYTE * far s1, static BYTE * far s2, BYTE l)
{
    far overlay BYTE i;
    for (i=0; i<l; i++) {
        if (s1[i] != s2[i])
            return FALSE;
    }
    return TRUE;
}

BOOL equal0x00(static BYTE * far s1, BYTE l)
{
    far overlay BYTE i;
    for (i=0; i<l; i++) {
        if (s1[i] != 0x00)
            return FALSE;
    }
    return TRUE;
}

BOOL equal0xFF(static BYTE * far s1, BYTE l)
{
    far overlay BYTE i;
    for (i=0; i<l; i++) {
        if (s1[i] != 0xFF)
            return FALSE;
    }
    return TRUE;
}

//*********************************************************************************
//        Find
//*********************************************************************************

// find all matching events

void Find(static BYTE * far ev) 
{
    far overlay unsigned int i;
    far overlay BYTE j;

    i = hash(ev); // calc hash index
    while (1) {
        ProgramMemoryRead((unsigned short long)&table[i], 64, (BYTE * far)GP_block);
        for (j=0; j<(64-EVENTSIZE); j+=EVENTSIZE) { // scan bucket
            if (equal((BYTE * far)&GP_block[j], &ev[0], 8))
                DoEvent((BYTE * far)&GP_block[j+8]);
            else if (equal0xFF((BYTE * far)&GP_block[j],8)) {
                return;
            }
        }
        i += 64;
        if (i>=TABLESIZE)
            i -= TABLESIZE; // next bucket
    }
}

//*********************************************************************************
//        SendEvent EVENTSIZE bytes in ev
//*********************************************************************************

void SendEvent(BYTE * far ev)
{
    far overlay BYTE i;
    CB_FrameType = FT_DAA | CB_SourceNID;
    CB_SourceNID = ND.nodeIdAlias;
    CB_data[0] = DAA_CEWRITEH;
    for (i=0; i<7; i++)
        CB_data[i+1] = ev[i];
    CB_datalen = 8;
    while (ECANSendMessage()==0) ;
    CB_data[0] = DAA_CEWRITEL;
    CB_data[1] = ev[7];
#if EVENTSIZE == 8
    CB_data[2] = 0;
    CB_datalen = 3;
#endif
#if EVENTSIZE == 9
    CB_data[2] = 1;
    CB_data[3] = ev[8];
    CB_datalen = 4;
#endif
#if EVENTSIZE == 10
    CB_data[2] = 2;
    CB_data[3] = ev[8];
    CB_data[4] = ev[9];
    CB_datalen = 5;
#endif
#if EVENTSIZE == 11
    CB_data[2] = 3;
    CB_data[3] = ev[8];
    CB_data[4] = ev[9];
    CB_data[5] = ev[10];
    CB_datalen = 6;
#endif
#if EVENTSIZE == 12
    CB_data[2] = 4;
    CB_data[3] = ev[8];
    CB_data[4] = ev[9];
    CB_data[5] = ev[10];
    CB_data[6] = ev[11];
    CB_datalen = 7;
#endif
#if EVENTSIZE == 13
    CB_data[2] = 5;
    CB_data[3] = ev[8];
    CB_data[4] = ev[9];
    CB_data[5] = ev[10];
    CB_data[6] = ev[11];
    CB_data[7] = ev[12];
    CB_datalen = 8;
#endif
    while (ECANSendMessage()==0) ;
}

//*********************************************************************************
//        ReadEvent
//*********************************************************************************

void ReadEvent(static BYTE * far ev, BYTE index) 
{
    far overlay unsigned int i;
    far overlay BYTE j;
    if (equal0x00((BYTE * far)&ev[0], 8)) {
        // read all events
        for (i=0; i<TABLESIZE; i+=64) {
            ProgramMemoryRead((unsigned short long)&table[i], 64, (BYTE * far)GP_block);
            for (j=0; j<(64-EVENTSIZE); j+=EVENTSIZE) { // scan bucket
                if (!equal0xFF((BYTE * far)&GP_block[j], 7)) {
                    if (index==0) {
                        SendEvent((BYTE * far)&GP_block[j]);
                        return;
                    }
                    index--;
                }
            }
       }
       sendack(ACK_NODATA, CB_SourceNID);
    }
    else {
        // read only matching events
        i = hash(ev); // calc hash index
        while (1) {
            ProgramMemoryRead((unsigned short long)&table[i], 64, (BYTE * far)GP_block);
            for (j=0; j<(64-EVENTSIZE); j+=EVENTSIZE) { // scan bucket
                if (equal((BYTE * far)&GP_block[j], &ev[0], 8)) {
                    if (index==0) {
                        SendEvent((BYTE * far)&GP_block[j]);
                        return;
                    }
                    index--;
                }
                else if (equal0xFF((BYTE * far)&GP_block[j], 8)) {
                    sendack(ACK_NODATA, CB_SourceNID);
                    return;
                }
            }
            i += 64;
            if (i>=TABLESIZE)
                i -= TABLESIZE; // next bucket
        }
    }
}

//*********************************************************************************
//        SaveEvent
//*********************************************************************************

// overwrite a deleted entry or the end of a chain, entries can overflow
// ino the next bucket(64 byte page) but the search will start to slow down

void SaveEvent(static BYTE * far ev)
{
    far overlay unsigned int i;
    far overlay BYTE j, k;

    // check for an exact duplicate
    i = hash(ev); // calc hash index
    while (1) {
        ProgramMemoryRead((unsigned short long)&table[i], 64, (BYTE * far)GP_block);
        for (j=0; j<(64-EVENTSIZE); j+=EVENTSIZE) { // scan bucket
            if (equal((BYTE * far)&GP_block[j], ev, EVENTSIZE)) {
                // don't write an exact duplicate event
                return;
            }
            else if (equal0xFF((BYTE * far)&GP_block[j], 8)) { // end of chain
                goto write;
            }
        }
        i += 64;
        if (i>=TABLESIZE)
            i -= TABLESIZE; // next bucket
    }

write:
    i = hash(ev); // calc hash index
    while (1) {
        ProgramMemoryRead((unsigned short long)&table[i], 64, (BYTE * far)GP_block);
        for (j=0; j<(64-EVENTSIZE); j+=EVENTSIZE) { // scan bucket
            if (equal0xFF((BYTE * far)&GP_block[j], 7)) {
                for (k=0; k<EVENTSIZE; k++)
                    GP_block[j+k] = ev[k];
                ProgramMemoryWrite((unsigned short long)&table[i], 64, (BYTE * far)GP_block);
                return;
            }
        }
        i += 64;
        if (i>=TABLESIZE)
            i -= TABLESIZE; // next bucket
    }
}

//*********************************************************************************
//        EraseEvent
//*********************************************************************************

void EraseEvent(static BYTE * far ev)
{
    far overlay BOOL changed; // if TRUE the page has been updated and needs writing back    
    far overlay unsigned int i;
    far overlay BYTE j, k;

    if (equal0x00((BYTE * far)&ev[0], 8)) {
        // erase all events
        for (j=0; j<64; j++) // clear 64 bytes
            GP_block[j] = 0xFF;
        for (i=0; i<TABLESIZE; i+=64) { // 64 or 32  x 64 byte pages
            ProgramMemoryWrite((unsigned short long)&table[i], 64, (BYTE * far)GP_block);
        }
    }
    else {
        // only erase matching events
        i = hash(ev); // calc hash index
        while (1) {
            ProgramMemoryRead((unsigned short long)&table[i], 64, (BYTE * far)GP_block);
            changed = FALSE;
            for (j=0; j<(64-EVENTSIZE); j+=EVENTSIZE) { // scan bucket
                if (equal((BYTE * far)&GP_block[j], (BYTE * far)&ev[0], 8)) {
                    for (k=0; k<EVENTSIZE; k++)
                        GP_block[j+k] = 0xFF;
                    GP_block[j+EVENTSIZE-1] = 0;
                    changed = TRUE;
                }
                else if (equal0xFF((BYTE * far)&GP_block[j], 8)) { // end of chain
                    if (changed) {
                        ProgramMemoryWrite((unsigned short long)&table[i], 64, (BYTE * far)GP_block);
                    }
                    return;
                }
            }
            if (changed) {
                ProgramMemoryWrite((unsigned short long)&table[i], 64, (BYTE * far)GP_block);
            }
            i += 64;
            if (i>=TABLESIZE)
                i -= TABLESIZE; // next bucket
        }
    }
}

//*********************************************************************************
