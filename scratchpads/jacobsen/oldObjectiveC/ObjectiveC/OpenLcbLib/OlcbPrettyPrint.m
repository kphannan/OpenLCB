//
//  OlcbPrettyPrint.m
//  OpenLcbLib
//
//  Created by Bob Jacobsen on 6/15/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//
//  Pretty-print code for OlcbMessages
//  Create an object of this type, ask it to process messages
//  and then retrieve the result.
//

#import "OlcbPrettyPrint.h"

@implementation OlcbPrettyPrint

@synthesize result;

- (void)processDefaultMessage: (OlcbMessage*) msg {
    self.result = @"Unknown message type";
}

- (void)processInitializationComplete: (OlcbInitializationCompleteMessage*) msg {
    self.result = nil;
}

@end
