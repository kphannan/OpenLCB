//
//  OlcbMessage.m
//  OpenLcbLib
//
//  Created by Bob Jacobsen on 6/1/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//

#import "OlcbMessage.h"
#import "OlcbMessageProcessor.h"


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

- (OlcbMessage*)initFromFields: (u_int32_t) mti_a data: (u_int8_t[]) content_a length: (u_int) length_a {
    // build from specific values
    mti = mti_a;
    content = content_a;
    length = length_a;
    return self;
}

- (u_int16_t) mti { return mti; }

- (void) dispatch: (id <OlcbMessageProcessor>) processor {
    [processor processDefaultMessage: self];
}

@end

