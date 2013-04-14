//
//  OlcbInitializationCompleteMessage.m
//  OpenLcbLib
//
//  Created by Bob Jacobsen on 6/15/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//

#import "OlcbInitializationCompleteMessage.h"
#import "OlcbMessage.h"
#import "OlcbMessageProcessor.h"

@implementation OlcbInitializationCompleteMessage

- (void) dispatch: (id <OlcbMessageProcessor>) processor {
    if ([processor respondsToSelector:@selector(processInitializationComplete:)]) {
        [processor processInitializationComplete: self];
    } else {
        [processor processDefaultMessage: self];        
    }
}

@end
