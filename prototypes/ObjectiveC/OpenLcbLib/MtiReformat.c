//
//  MtiReformat.c
//  OpenLcbLib
//
//  Created by Bob Jacobsen on 5/31/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//

#include "MtiReformat.h"

u_int32_t CanHeaderFromMti(u_int16_t mti, u_int16_t source_alias, u_int16_t dest_alias, u_int8_t flags ) {
    
    u_int32_t result = 0x18000000;
    result |= source_alias&0xFFF;
    
    u_int did_present   = mti & 0x0004;
    u_int eid_present   = mti & 0x0002;
    u_int flags_present   = mti & 0x0001;
    
    // now create subfields of result
    
    // format (MSNibble)
    int format = 0;  // start 'simple' by default
    
    // determine if "simple MTI", handle
    if ( (mti&0x2000) != 0 ) format = 1; // complex
    
    if (did_present != 0) format = 6;  // carries dest ID
    
    // two special cases
    if (mti == 0x3404) format = 5; // last datagram, might be 4 later when split
    if (mti == 0x3694) format = 7; // stream data send
    
    u_int typebyte_offset = (mti & 0x0FF0 );  // left in higher nibbles to reduce shift later
    
    // finally, assemble the 15-bit section of the header
    u_int section = format<<12;
    if (did_present !=0) {
        section |= dest_alias;
    } else {
        // no destination alias, so fill with type_byte
        section |= typebyte_offset;  // already shifted 4 above
        // handle low 4 bits in order
        // EID?
        if (eid_present != 0) {
            section |= 0x08;
        }
        if (flags_present != 0) section |= (flags&0x3);
        else section |= 0x07;
    }
    
    // formulate final header and return
    result |= (section << 12);
    return result;
}

u_int16_t MtiFromCanHeader(u_int32_t header, u_int8_t byte0 ) {
    u_int16_t result = 0;
    u_int32_t format = (header & 0x7000000) >> 24; // 0 through 7
    switch (format) {
        case 1: // complex
            result = 0x2000; // mark complex, then fall through
        case 0: // simple
            // simple or complex MTI
            result |= 0x1000 | ((header & 0x00FF0000) >> 12);
            if (header & 0x8000) result |= 0x02;         // carries event ID
            if ((header & 0x4000) == 0) result |= 0x01;  // carries flags
            return result;
        case 2:
        case 3:
            // reserved
            return 0;
        case 4:
        case 5:
            // datagram
            return 0x3404;
        case 6:
            // DestID non stream
            return 0x3004 | (byte0 << 4);
        case 7:
            // stream data send
            return 0x3694;
        default:
            // error
            return 0;
    }
}