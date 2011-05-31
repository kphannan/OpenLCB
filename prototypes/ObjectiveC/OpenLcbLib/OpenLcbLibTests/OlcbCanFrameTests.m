//
//  OlcbCanFrameTests.m
//  OpenLcbLib
//
//  Created by Bob Jacobsen on 5/31/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//

#import "OlcbCanFrameTests.h"
#import "OlcbCanFrame.h"


@implementation OlcbCanFrameTests

- (void)testAlloc {
    
    NSLog(@"in testAlloc");
    [OlcbCanFrame alloc];
    
}
- (void)testInitFromString {
    
    NSLog(@"in testInitFromString");
    NSString* line = @":X182DF285N0203040506080082;";
    u_int32_t ok_header = 0x182DF285;

    OlcbCanFrame* result = [[OlcbCanFrame alloc] initFromString: line];
    
    STAssertEquals(ok_header, [result header], nil);
        
}

@end
