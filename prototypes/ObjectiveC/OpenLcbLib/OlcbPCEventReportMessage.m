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
    if ([processor respondsToSelector:@selector(processPCEventReceived:)]) {
        [processor processPCEventReceived: self];
    } else {
        [processor processDefaultMessage: self];        
    }
}

@end
