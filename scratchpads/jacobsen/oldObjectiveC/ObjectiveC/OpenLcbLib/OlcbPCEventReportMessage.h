//
//  OlcbPCEventReceivedMessage.h
//  OpenLcbLib
//
//  Created by Bob Jacobsen on 6/15/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "OlcbMessage.h"
#import "OlcbMessageProcessor.h"

@interface OlcbPCEventReportMessage : OlcbMessage {
    
}

- (void) dispatch: (id <OlcbMessageProcessor>) processor;

- (OlcbMessage*)initFromFields: (u_int16_t) mti data: (u_int8_t[]) content length: (u_int) length;

@end
