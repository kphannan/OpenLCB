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

#pragma udata ovrly
#pragma romdata

void DoEvent(BOOL on, unsigned int i);

/* *******************************************************************************

For 4096 or 2048 bytes of Program memory, divided into 64 or 32 buckets of 
64 bytes each 64 byte bucket holds 6 x 10 byte events (8 bytes for the event
and 2 bytes for the action required.

Short events have nodenumber 0
FF,FF,FF,FF,xx,xx,xx,xx is end of chain
FF,FF,00,00,xx,xx,xx,xx is a deleted entry to be stepped over

So 6 x 64 or 6 x 32 = 384 or 192 is the maximum number of events
In practice 80% is a good maximum, so 300 or 150 is a better limit.

******************************************************************************** */

//*********************************************************************************
//        ROM module info
//*********************************************************************************

// These tables need to be aligned on 64 byte boundary

#define EVENTSIZE 10
#ifdef HASH2048
#define TABLEMASK 0x1F			// 0x1F for 2048
BYTE rom table[2048]; 	            // initializes to zero, 64 byte aligned
#else // HASH4096
#define TABLEMASK 0x3F			// 0x3F for 4096
BYTE rom table[4096]; 	            // initializes to zero, 64 byte aligned
#endif

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

// convert the event number to 0-63 hash key using random numbers
// uses the bottom bits of the nodenumber and eventnumber to index the table
// of random numbers and them xor's them together.

BYTE hash(static BYTE * far ev) {
    far overlay BYTE i, h = 0;
    for (i=0; i<8; i++)
        h ^= hashdatatable[ev[i]];
    return h & TABLEMASK;
}

//*********************************************************************************
//        Find
//*********************************************************************************

// find all matching events

void Find(static BYTE * far ev) 
{
    far overlay BYTE i, j;
    far overlay BOOL on;
    on = CB_FrameType == FT_ACON || CB_FrameType == FT_ASON;

    i = hash(ev); // calc hash index
    while (1) {
        ProgramMemoryRead((unsigned long)&table[(unsigned int)i<<6], 64, 
          (BYTE * far)GP_block);
        for (j=0; j<64; j+=EVENTSIZE) { // scan bucket
            if (*(unsigned long*)&GP_block[j] == *(unsigned long*)&ev[0]
              && *(unsigned long*)&GP_block[j+4] == *(unsigned long*)&ev[4]) {
                DoEvent(on, LOWD(GP_block[j+8]));
            }
            else if (*(unsigned long*)&GP_block[j]==0xFFFFFFFF) {
                return;
            }
        }
        i = (i+1) & TABLEMASK; // next bucket
    }
}

//*********************************************************************************
//        SendEvent 10 bytes in ev
//*********************************************************************************

void SendEvent(BYTE * far ev)
{
    CB_FrameType = FT_DAA | CB_SourceNID;
    CB_SourceNID = ND.nodeIdAlias;
    CB_data[0] = DAA_EVWRITEH;
    CB_data[1] = ev[0];
    CB_data[2] = ev[1];
    CB_data[3] = ev[2];
    CB_data[4] = ev[3];
    CB_datalen = 5;
    while (ECANSendMessage()==0) ;
    CB_data[0] = DAA_EVWRITEL;
    CB_data[1] = ev[4];
    CB_data[2] = ev[5];
    CB_data[3] = ev[6];
    CB_data[4] = ev[7];
    CB_data[5] = 2;
    CB_datalen = 6;
    while (ECANSendMessage()==0) ;
    CB_data[0] = DAA_DATA;
    CB_data[1] = ev[8];
    CB_data[2] = ev[9];
    CB_datalen = 3;
    while (ECANSendMessage()==0) ;
}

//*********************************************************************************
//        ReadEvent
//*********************************************************************************

// find all matching events

void ReadEvent(static BYTE * far ev, BYTE index) 
{
    far overlay BYTE i, j;
    i = hash(ev); // calc hash index
    while (1) {
        ProgramMemoryRead((unsigned long)&table[(unsigned int)i<<6], 64, (BYTE * far)GP_block);
        for (j=0; j<64; j+=EVENTSIZE) { // scan bucket
            if (*(unsigned long*)&GP_block[j] == *(unsigned long*)&ev[0]
              && *(unsigned long*)&GP_block[j+4] == *(unsigned long*)&ev[4]) {
                if (index==0) {
                    SendEvent((BYTE * far)&GP_block[j]);
                    return;
                }
                index--;
            }
            else if (*(unsigned long*)GP_block[j]==0xFFFFFFFF) {
                sendack(3, CB_SourceNID);
                return;
            }
        }
        i = (i+1) & TABLEMASK; // next bucket
    }
}

//*********************************************************************************
//        ReadTable
//*********************************************************************************

// read all of the event table

void ReadTable(BYTE index) 
{
    far overlay BYTE i, j;
    for (i=0; i<(TABLEMASK+1); i++) {
        ProgramMemoryRead((unsigned long)&table[(unsigned int)i<<6], 64, (BYTE * far)GP_block);
        for (j=0; j<64; j+=EVENTSIZE) { // scan bucket
            if (LOWD(GP_block[j])!=0xFFFF) {
                if (index==0) {
                    SendEvent((BYTE * far)&GP_block[j]);
                    return;
                }
                index--;
            }
        }
    }
    sendack(3, CB_SourceNID);
}

//*********************************************************************************
//        SaveEvent
//*********************************************************************************

// overwrite a deleted entry or the end of a chain, entries can overflow
// ino the next bucket(64 byte page) but the search will start to slow down

void SaveEvent(static BYTE * far ev, unsigned int event)
{
    far overlay BYTE i, j;

    // check for an exact duplicate
    i = hash(ev); // calc hash index
    while (1) {
        ProgramMemoryRead((unsigned long)&table[(unsigned int)i<<6], 64, (BYTE * far)GP_block);
        for (j=0; j<64; j+=EVENTSIZE) { // scan bucket
            if (*(unsigned long*)&GP_block[j] == *(unsigned long*)&ev[0]
              && *(unsigned long*)&GP_block[j+4] == *(unsigned long*)&ev[4]
              && LOWD(GP_block[j+8])==event) {
                // don't write an exact duplicate event
                return;
            }
            else if (LOWD(GP_block[j])==0xFFFF) { // end of chain or deleted entry
                goto write;
            }
        }
        i = (i+1) & TABLEMASK; // next bucket
    }

write:
    i = hash(ev); // calc hash index
    while (1) {
        ProgramMemoryRead((unsigned long)&table[(unsigned int)i<<6], 64, (BYTE * far)GP_block);
        for (j=0; j<64; j+=EVENTSIZE) { // scan bucket
            if (LOWD(GP_block[j])==0xFFFF) {
                GP_block[j] = ev[0];
                GP_block[j+1] = ev[1];
                GP_block[j+2] = ev[2];
                GP_block[j+3] = ev[3];
                GP_block[j+4] = ev[4];
                GP_block[j+5] = ev[5];
                GP_block[j+6] = ev[6];
                GP_block[j+7] = ev[7];
                LOWD(GP_block[j+8]) = event;
                ProgramMemoryWrite((unsigned long)&table[(unsigned int)i<<6], 64, (BYTE * far)GP_block);
                return;
            }
        }
        i = (i+1) & TABLEMASK; // next bucket
    }
}

//*********************************************************************************
//        EraseEvent
//*********************************************************************************

void EraseEvent(static BYTE * far ev)
{
    far overlay BOOL changed;    // if TRUE the page has been updated and needs writing back    
    far overlay BYTE i, j;

    i = hash(ev); // calc hash index
    while (1) {
        ProgramMemoryRead((unsigned long)&table[(unsigned int)i<<6], 64, (BYTE * far)GP_block);
        changed = FALSE;
        for (j=0; j<64; j+=EVENTSIZE) { // scan bucket
            if (*(unsigned long*)&GP_block[j] == *(unsigned long*)&ev[0]
              && *(unsigned long*)&GP_block[j+4] == *(unsigned long*)&ev[4]) {
                *(unsigned long *)&GP_block[j] = 0xFFFF0000;
                changed = TRUE;
            }
            else if (*(unsigned long*)&GP_block[j] == 0xFFFFFFFF) { // end of chain
                if (changed) {
                    ProgramMemoryWrite((unsigned long)&table[(unsigned int)i<<8], 64, (BYTE * far)GP_block);
                }
                return;
            }
        }
        if (changed) {
            ProgramMemoryWrite((unsigned long)&table[(unsigned int)i<<8], 64, (BYTE * far)GP_block);
        }
        i = (i+1) & TABLEMASK; // next bucket
    }
}

//*********************************************************************************
//        EraseAllEvents	
//*********************************************************************************

void EraseAllEvents(void)
{
    far overlay BYTE i;
    for (i=0; i<64; i++) // clear 64 bytes
        GP_block[i] = 0xFF;
    for (i=0; i<(TABLEMASK+1); i++) { // 64 x 64 byte pages
        ProgramMemoryWrite((unsigned long)&table[(unsigned int)i<<6], 64, (BYTE * far)GP_block);
    }
}

//*********************************************************************************
