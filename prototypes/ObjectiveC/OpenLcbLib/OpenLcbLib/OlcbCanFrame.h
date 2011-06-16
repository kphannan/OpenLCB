//
//  OlcbCanFrame.h
//  OpenLcbLib
//
//  Created by Bob Jacobsen on 5/22/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//

#import <Foundation/Foundation.h>
@class OlcbMessage;

@interface OlcbCanFrame : NSObject {
    u_int32_t   header;
    u_int8_t    bytes[8];
    u_int       length;
    OlcbMessage* message;
}

/**
 * From a characte string like :X182DF285N0203040506070809;
 * Trailing whitespace OK, leading whitespace is not.
 */
- (OlcbCanFrame*)initFromString: (NSString*) line;

- (OlcbCanFrame*)initFromMessage: (OlcbMessage*) msg;

- (u_int32_t)header;
- (u_int8_t*)bytes;
- (u_int)length;
- (OlcbMessage*)message;

@end
