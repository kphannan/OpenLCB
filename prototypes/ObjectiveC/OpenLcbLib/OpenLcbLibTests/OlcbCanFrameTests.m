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
    u_int8_t* bytes;
    
    NSString* line = @":X182DF285N02030405060708F9;";
    u_int32_t ok_header = 0x182DF285;
    
    OlcbCanFrame* result = [[OlcbCanFrame alloc] initFromString: line];
    bytes = [result bytes];

    STAssertEquals(ok_header, [result header], nil);
    STAssertEquals((u_int)8, [result length], nil);

    STAssertEquals((u_int8_t) 0x02, bytes[0], @"expected 0x02, found %x", bytes[0]);
    STAssertEquals((u_int8_t) 0x03, bytes[1], @"expected 0x03, found %x", bytes[1]);
    STAssertEquals((u_int8_t) 0x04, bytes[2], @"expected 0x04, found %x", bytes[2]);
    STAssertEquals((u_int8_t) 0x05, bytes[3], @"expected 0x05, found %x", bytes[3]);
    STAssertEquals((u_int8_t) 0x06, bytes[4], @"expected 0x06, found %x", bytes[4]);
    STAssertEquals((u_int8_t) 0x07, bytes[5], @"expected 0x07, found %x", bytes[5]);
    STAssertEquals((u_int8_t) 0x08, bytes[6], @"expected 0x08, found %x", bytes[6]);
    STAssertEquals((u_int8_t) 0xF9, bytes[7], @"expected 0xF9, found %x", bytes[7]);
    
    STAssertNotNil([result message],nil);
        
}

@end
