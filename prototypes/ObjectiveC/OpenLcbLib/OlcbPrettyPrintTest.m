//
//  OlcbPrettyPrintTest.m
//  OpenLcbLib
//
//  Created by Bob Jacobsen on 6/15/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//

#import "OlcbPrettyPrintTest.h"
#import "OlcbPCEventReportMessage.h"
#import "OlcbPrettyPrint.h"

@implementation OlcbPrettyPrintTest

- (void)testProcessPCEventReport {
    // create an initialization complete processor, and a message, and have them process each other
    OlcbPrettyPrint* a = [[OlcbPrettyPrint alloc] init];
    
    [[OlcbPCEventReportMessage alloc] dispatch: a];
    
    STAssertEqualObjects( a.result, @"Unknown message type", @"processPCEventReceived saw wrong string: %@", a.result);
}

@end
