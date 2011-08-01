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

#pragma udata hashbuf
BYTE far hashbuffer[64];
BYTE far eventoffset;
BYTE far event[EVENTSIZE];
unsigned int far bucketaddress;

#pragma udata ovrly
#pragma romdata

void DoEvent(static BYTE * far ev);

/* *******************************************************************************

For 2560 bytes of Program memory, divided into 40 buckets of 
64 bytes each 64 byte bucket holds 7 x 9 byte events.
So 7 x 40 = 280 is the maximum number of events
In practice 80% is a good maximum, so 224 is a better limit.

00,00,00,00,00,00,00,00 is end of chain
FF,00,00,00,00,00,00,00 is a deleted entry to be stepped over

******************************************************************************** */

//*********************************************************************************
//        ROM module info
//*********************************************************************************

// These tables need to be aligned on 64 byte boundary

#pragma romdata table = 0x1100

BYTE rom table[TABLESIZE];          // initializes to zero, 64 byte aligned

#pragma romdata

//*********************************************************************************
//        Event table
//*********************************************************************************

// convert the event number to a bucket start address

unsigned int hash(static BYTE * far ev) {
    far overlay BYTE h = 0;
    far overlay BYTE i;
    far overlay unsigned int r;
    for (i=0; i<8; i++)
        h ^= ev[i];
    r = (unsigned int)h << 6;
    while (r >= TABLESIZE)
        r -= TABLESIZE;
    return r;
}

BOOL EVequal(static BYTE * far s1, static BYTE * far s2)
{
    far overlay BYTE i;
    for (i=0; i<8; i++) {
        if (s1[i] != s2[7-i])
            return FALSE;
    }
    return TRUE;
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

//*********************************************************************************
//        Erase All
//*********************************************************************************

void EraseAll(void)
{
    unsigned int a;
    far overlay BYTE i;
    for (i=0; i<64; i++)
        hashbuffer[i] = 0;    
    for (a=0; a<TABLESIZE; a+=64)
        ProgramMemoryWrite((unsigned short long)&table[a], 64, (BYTE * far)hashbuffer);
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
        ProgramMemoryRead((unsigned short long)&table[i], 64, (BYTE * far)hashbuffer);
        for (j=0; j<(64-EVENTSIZE); j+=EVENTSIZE) { // scan bucket
            if (EVequal((BYTE * far)&hashbuffer[j], &ev[0]))
                DoEvent((BYTE * far)&hashbuffer[j+8]);
            else if (equal0x00((BYTE * far)&hashbuffer[j],8)) {
                return;
            }
        }
        i += 64;
        if (i>=TABLESIZE)
            i -= TABLESIZE; // next bucket
    }
}

//*********************************************************************************
//        Read all events into buffer
//*********************************************************************************

void ReadAllEvents(int start) 
{
    far overlay char j;           // byte within bucket
    far overlay char k;           // byte within event
    far overlay char eventoffset; // output buffer pointer
    
    eventoffset = 7;
    for (j=7; j<64+7; j++)
        GP_block[j] = 0;

    if (start==0) 
        bucketaddress=0;          // bucket address

    j = k = bucketaddress&0x3F;
    while (k>=0)
        k -= EVENTSIZE;
    k += EVENTSIZE;
    if (k==0 && j+EVENTSIZE >= 64) // skip junk at end of bucket
        bucketaddress += 64-j;

    while (bucketaddress<TABLESIZE) {
        if ((bucketaddress&0x3F)==0) {
            ProgramMemoryRead((unsigned short long)&table[bucketaddress], 64, (BYTE * far)hashbuffer);
            k = 0;
            j = 0;
        }
        if (k == 0 && equal0x00((BYTE * far)&hashbuffer[j+1], 7)) {
            j += EVENTSIZE;
            bucketaddress += EVENTSIZE;
        }
        else {
            GP_block[eventoffset++] = hashbuffer[j++];
            bucketaddress++;
            k++;
            if (eventoffset>=64+7) // output buffer full
                return;
        }
        if (k>=EVENTSIZE)
            k = 0;
        if (k==0 && j+EVENTSIZE >= 64) // skip junk at end of bucket
            bucketaddress += 64-j;
    }
}

//*********************************************************************************
//        Read all events one by one
//*********************************************************************************

BOOL ReadOneEvent(int start) 
{
    far overlay unsigned int ba;  // bucket address
    far overlay char j;           // byte within bucket
    far overlay char k;           // byte within event
    far overlay char eventoffset; // output buffer pointer
    
    for (ba=0; ba<TABLESIZE; ba+=64) {
        ProgramMemoryRead((unsigned short long)&table[ba], 64, (BYTE * far)hashbuffer);
        for (j=0; j<64; j+=EVENTSIZE) {
            if (!equal0x00((BYTE * far)&hashbuffer[j+1], 7)) {
                if (start==0) {
                    for (k=0; k<8; k++)
                        CB_data[k] = hashbuffer[j+7-k];
                    return TRUE;
                }
                start--; 
            }
        }
    }
    return FALSE;
}

//*********************************************************************************
//        Save Event
//*********************************************************************************

void SaveEvent(static BYTE * far ev)
{
    far overlay unsigned int i;
    far overlay BYTE j, k;

    // check for an exact duplicate
    i = hash(ev); // calc hash index
    while (1) {
        ProgramMemoryRead((unsigned short long)&table[i], 64, (BYTE * far)hashbuffer);
        for (j=0; j<(64-EVENTSIZE); j+=EVENTSIZE) { // scan bucket
            if (equal((BYTE * far)&hashbuffer[j], ev, EVENTSIZE)) {
                // don't write an exact duplicate event
                return;
            }
            else if (equal0x00((BYTE * far)&hashbuffer[j], 8)) { // end of chain
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
        ProgramMemoryRead((unsigned short long)&table[i], 64, (BYTE * far)hashbuffer);
        for (j=0; j<(64-EVENTSIZE); j+=EVENTSIZE) { // scan bucket
            if (equal0x00((BYTE * far)&hashbuffer[j+1], 7)) {
                for (k=0; k<EVENTSIZE; k++)
                    hashbuffer[j+k] = ev[k];
                ProgramMemoryWrite((unsigned short long)&table[i], 64, (BYTE * far)hashbuffer);
                return;
            }
        }
        i += 64;
        if (i>=TABLESIZE)
            i -= TABLESIZE; // next bucket
    }
}

//*********************************************************************************
//        Write Events from buffer
//*********************************************************************************

void WriteAllEvents(unsigned int start)
{
    far overlay BYTE p;
    if (start==0) {
        EraseAll();
        eventoffset=0;
    }
    p = 7;
    do {
        while(eventoffset<EVENTSIZE && p<64+7) {
            event[eventoffset++] = GP_block[p++];
        }
        if (eventoffset>=EVENTSIZE) {
            eventoffset=0;
            if (!equal0x00((BYTE * far)event, 8)) {
                SaveEvent((BYTE * far)event);
            }
        }
    } while(p<64+7);
}

//*********************************************************************************
//        EraseEvent
//*********************************************************************************

void EraseEvent(static BYTE * far ev)
{
    far overlay BOOL changed; // if TRUE the page has been updated and needs writing back    
    far overlay unsigned int i;
    far overlay BYTE j, k;

    // only erase matching events
    i = hash(ev); // calc hash index
    while (1) {
        ProgramMemoryRead((unsigned short long)&table[i], 64, (BYTE * far)hashbuffer);
        changed = FALSE;
        for (j=0; j<(64-EVENTSIZE); j+=EVENTSIZE) { // scan bucket
            if (equal((BYTE * far)&hashbuffer[j], (BYTE * far)&ev[0], 8)) {
                for (k=0; k<EVENTSIZE; k++)
                    hashbuffer[j+k] = 0;
                hashbuffer[j] = 0xFF;
                changed = TRUE;
            }
            else if (equal0x00((BYTE * far)&hashbuffer[j], 8)) { // end of chain
                if (changed) {
                    ProgramMemoryWrite((unsigned short long)&table[i], 64, (BYTE * far)hashbuffer);
                }
                return;
            }
        }
        if (changed) {
            ProgramMemoryWrite((unsigned short long)&table[i], 64, (BYTE * far)hashbuffer);
        }
        i += 64;
        if (i>=TABLESIZE)
            i -= TABLESIZE; // next bucket
    }

}

