//
//  OlcbCanFrame.m
//  OpenLcbLib
//
//  Created by Bob Jacobsen on 5/22/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//

#import "OlcbCanFrame.h"
#import "OlcbMessage.h"
#import "MtiReformat.h"

@implementation OlcbCanFrame

- (OlcbCanFrame*)initFromString: (NSString*) line {
    u_int16_t mti;
    
    if (line.length >= 10) {
        // expecting 
        // load header from initial part
        // From http://forums.macrumors.com/showthread.php?t=977076
        sscanf([[[line substringToIndex: 10] substringFromIndex: 2] UTF8String], "%x", &header);
        
        // load content
        length = 0;
        while (length < 8 && line.length > 11+2*length) {
            int val;
            sscanf([[[line substringFromIndex: 11+2*length] substringToIndex: 2 ] UTF8String], "%x", &val);
            //NSLog(@"found: %@ %d", [[line substringFromIndex: 11+2*length] substringToIndex: 2 ], val);
            bytes[length] = val;
            length++;
        }
        
        // if this an OpenLCB message frame?
        if ( (header & 0x8000000) != 0) {
            // OpenLCB message frame
            mti = (u_int16_t) MtiFromCanHeader(header, bytes[0]);
            message = [[OlcbMessage alloc] initFromFields: mti data: bytes length: length ];
        } else {
            // CAN control frame
            message = nil;
        }
    } else {
        // not in a frame format (could be standard header, or junk)
        header = 0;
        length = 0;
    }
    return self;
}

- (OlcbCanFrame*)initFromMessage: (OlcbMessage*) msg {
    // convert MTI to header
    
    return self;
}

- (u_int32_t)header { return header; }
- (u_int8_t*)bytes { return bytes; }
- (u_int)length { return length; }
- (OlcbMessage*)message { return message; }


@end
