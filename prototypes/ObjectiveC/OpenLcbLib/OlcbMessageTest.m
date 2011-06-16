//
//  OlcbMessageTest.m
//  OpenLcbLib
//
//  Created by Bob Jacobsen on 6/1/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//

#import "OlcbMessageTest.h"
#import "OlcbMessage.h"

#import "OlcbTestDefinitions.h"

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

- (void)testInitFromFields {
    [[OlcbMessage alloc] initFromFields: MTI_CAN_INIT_FROM_FIELDS data: nil length: 0];
}
@end
