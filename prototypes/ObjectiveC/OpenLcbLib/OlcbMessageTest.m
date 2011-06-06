//
//  OlcbMessageTest.m
//  OpenLcbLib
//
//  Created by Bob Jacobsen on 6/1/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//

#import "OlcbMessageTest.h"
#import "OlcbMessage.h"


@implementation OlcbMessageTest

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

- (void)testAlloc {
    [OlcbMessage alloc];
}
@end
