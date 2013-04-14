//
//  OlcbPCEventReceivedMessage.m
//  OpenLcbLib
//
//  Created by Bob Jacobsen on 6/15/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//

#import "OlcbPCEventReportMessage.h"
#import "OlcbMessage.h"
#import "OlcbMessageProcessor.h"

@implementation OlcbPCEventReportMessage

- (void) dispatch: (id <OlcbMessageProcessor>) processor {
    if ([processor respondsToSelector:@selector(processPCEventReport:)]) {
        [processor processPCEventReport: self];
    } else {
        [processor processDefaultMessage: self];        
    }
}

- (OlcbMessage*)initFromFields: (u_int16_t) mti_a data: (u_int8_t[]) content_a length: (u_int) length_a {
    
    // build from specific values
    mti = mti_a;
    content = content_a;
    length = length_a;
    
    return self;
}

@end
