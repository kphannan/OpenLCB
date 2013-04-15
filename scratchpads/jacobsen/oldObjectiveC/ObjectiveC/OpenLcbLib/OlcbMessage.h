//
//  OlcbMessage.h
//  OpenLcbLib
//
//  Created by Bob Jacobsen on 6/1/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <sys/types.h>
@protocol OlcbMessageProcessor;

@interface OlcbMessage : NSObject {
@protected
    u_int16_t mti;
    u_int8_t* content;
    u_int length;
}

- (OlcbMessage*)initFromFields: (u_int16_t) mti data: (u_int8_t[]) content length: (u_int) length;

- (u_int16_t) mti;
- (u_int8_t) byte: (int) index;

- (void) dispatch: (id <OlcbMessageProcessor>) processor;

@end