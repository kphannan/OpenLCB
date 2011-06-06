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
    
    u_int did   = mti & 0x0004;
    //u_int eid   = mti & 0x0002;
    u_int flg   = mti & 0x0001;
    u_int def   = mti & 0x0007;
    
    // now create subfields of result
    
    // format (MSNibble)
    u_int format = 1;

    // determine if "simple MTI", handle
    switch (mti) {
        case 0x30A4: // Verify node ID number
        case 0x30A0: // Verify node ID number
        case 0x32E4: // Protocol support enquiry
        case 0x32F4: // Protocol support reply
        case 0x3242: // Identify consumers
        case 0x3282: // Identify consumers
        case 0x32B4: // Identify events
        case 0x32B0: // Identify events
        case 0x30C2: // Learn event
        case 0x32D2: // P/C event report
        case 0x3404: // Datagram (general)
        case 0x34C4: // Datagram (general)
        case 0x34D4: // Datagram rejected
            format = 0;
    }
    
    if (def == 4) format = 6;
    if (mti == 0x3404) format = 5; // last datagram, might be 4 later when split
    if (mti == 0x3694) format = 7; // stream data send
    
    u_int typebyte_offset = (mti & 0x0FF0 );  // left in higher nibbles to reduce shift later
    
    // finally, assemble the 15-bit section of the header
    u_int section = format<<12;
    if (did) section |= dest_alias;
    else {
        // no destination alias, so fill with type_byte
        section |= typebyte_offset;  // already shifted 4 above
        // handle flags
        if (flg) section |= (flags&0xF);
        else section |= 0xF;
    }
    
    // formulate final header and return
    result |= (section << 12);
    return result;
}

u_int16_t MtiFromCanHeader(u_int32_t header, u_int8_t byte0 ) {
    u_int16_t result;
    u_int32_t format = (header & 0x7000000) >> 24; // 0 through 7
    switch (format) {
        case 0:
        case 1:
            // simple or complex MTI
            result = 0x3000 | ((header & 0x00FF0000) >> 12); // no flags
            // special cases for flags
            // ..
            return result;
        case 2:
        case 3:
            // reserved
            return 0;
        case 4:
        case 5:
            // datagram
        case 6:
            // DestID non stream
            return 0x3004 | (byte0 << 4);
        case 7:
            // stream data send
            return 0;
        default:
            // error
            return 0;
    }
}