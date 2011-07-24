//
//  OlcbMessage.m
//  OpenLcbLib
//
//  Created by Bob Jacobsen on 6/1/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//

#import "OlcbMessage.h"
#import "OlcbMessageProcessor.h"

#import "OlcbMtiDefinitions.h"

#import "OlcbInitializationCompleteMessage.h"
#import "OlcbPCEventReportMessage.h"

@implementation OlcbMessage

- (id)init
{
    self = [super init];
    if (self) {
        // Initialization code here.
    }
    
    return self;
}

- (void)dealloc
{
    [super dealloc];
}

- (OlcbMessage*)initFromFields: (u_int16_t) mti_a data: (u_int8_t[]) content_a length: (u_int) length_a {
    
    // handle type selection
    switch (mti_a) {
        case MTI_INITIALIZATION_COMPLETE:
            self = [[OlcbInitializationCompleteMessage alloc] initFromFields:mti_a data:content_a length:length_a];
            return self;
        case MTI_PC_EVENT_REPORT:
            self = [[OlcbPCEventReportMessage alloc] initFromFields:mti_a data:content_a length:length_a];
            return self;
    }
    // no specific value found
    // build generic from specific values
    content = content_a;
    length = length_a;
    
    return self;
}

- (u_int16_t) mti { return mti; }

- (u_int8_t) byte: (int) index {
    return content[index];
}

- (void) dispatch: (id <OlcbMessageProcessor>) processor {
    [processor processDefaultMessage: self];
}

@end

