//
//  OlcbNodeIDTest.m
//  OpenLcbLib
//
//  Created by Bob Jacobsen on 6/1/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//

#import "OlcbNodeIDTest.h"
#import "OlcbNodeID.h"


@implementation OlcbNodeIDTest

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

- (void) testFromBytes {
    u_int8_t array1[] = {0,1,2,3,4,5};
    u_int8_t array2[] = {5,4,3,2,1,0};
     
    OlcbNodeID* n1a = [[OlcbNodeID alloc] initFromArray: array1];
    OlcbNodeID* n1b = [[OlcbNodeID alloc] initFromArray: array1];
    OlcbNodeID* n2 = [[OlcbNodeID alloc] initFromArray: array2];

    STAssertTrue( [n1a equals: n1a], @"test n1a == n1a");    
    STAssertTrue( [n1a equals: n1b], @"test n1a == n1b");    
    STAssertTrue( ![n1a equals: n2], @"test n1a != n2");    

}
@end
