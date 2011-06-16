//
//  OlcbMessageProcessorTest.m
//  OpenLcbLib
//
//  Created by Bob Jacobsen on 6/12/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//

#import "OlcbMessageProcessorTest.h"
#import "OlcbMessageProcessor.h"
#import "OlcbMessage.h"
#import "OlcbInitializationCompleteMessage.h"
#import "OlcbPCEventReportMessage.h"

static int result;

@interface TestPartialProtocol : NSObject < OlcbMessageProcessor >
- (void)processDefaultMessage: (OlcbMessage*) msg;
- (void)processInitializationComplete: (OlcbMessage*) msg;
@end
@implementation TestPartialProtocol
- (void)processDefaultMessage: (OlcbMessage*) msg {
    result = 1;
}
- (void)processInitializationComplete: (OlcbMessage*) msg {
    result = 2;
}
@end

@interface TestAllMessages : NSObject < OlcbMessageProcessor >
- (void)processDefaultMessage: (OlcbMessage*) msg;
- (void)processInitializationComplete: (OlcbMessage*) msg;
@end
@implementation TestAllMessages
- (void)processDefaultMessage: (OlcbMessage*) msg {
    result = 1;
}
- (void)processInitializationComplete: (OlcbMessage*) msg {
    result = 2;
}
- (void)processPCEventReport: (OlcbMessage*) msg {
    result = 3;
}
@end

@implementation OlcbMessageProcessorTest
- (void)setUp
{
    [super setUp];
    
    // Set-up code here.
}

- (void)tearDown
{
    // Tear-down code here.
    
    [super tearDown];
}

- (void)testProcessPartial {
    // create an initialization complete processor, and a message, and have them process each other
    TestPartialProtocol* a = [TestPartialProtocol alloc];
    
    result = 0;
    [[OlcbInitializationCompleteMessage alloc] dispatch: a];
    STAssertEquals( result, 2, @"testProcessPartial saw %d instead of 2 on InitializationComplete", result);
    
    result = 0;
    [[OlcbPCEventReportMessage alloc] dispatch: a];
    STAssertEquals( result, 1, @"testProcessPartial saw %d instead of 1 on PCEventReport", result);
}

- (void)testProcessFull {
    // create an initialization complete processor, and a message, and have them process each other
    TestAllMessages* a = [TestAllMessages alloc];
    
    result = 0;
    [[OlcbInitializationCompleteMessage alloc] dispatch: a];
    STAssertEquals( result, 2, @"processPCEventReceived saw %d instead of 2 on InitializationComplete", result);

    result = 0;
    [[OlcbPCEventReportMessage alloc] dispatch: a];
    STAssertEquals( result, 1, @"processPCEventReceived saw %d instead of 1 on PCEventReport", result);
}

@end
