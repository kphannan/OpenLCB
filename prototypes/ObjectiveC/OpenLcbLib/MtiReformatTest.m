//
//  MtiReformatTest.m
//  OpenLcbLib
//
//  Created by Bob Jacobsen on 5/31/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//

#import "MtiReformatTest.h"
#import "MtiReformat.h"

@implementation MtiReformatTest

void doOneHeader(id self, NSString* msg, u_int32_t expect, u_int16_t mti, u_int16_t source, u_int16_t dest, u_int8_t flags) {
    u_int32_t result = CanHeaderFromMti(mti, source, dest, 0xF);
    STAssertEquals(result, expect, @"%@ expected 0x%x, got 0x%x", msg, expect, result);    
}

- (void)testToCAN {
    
    doOneHeader(self, @"Initialization complete", 0x1908F123, 
                0x3080, 0x123, 0x456, 0x0);
    doOneHeader(self, @"Verify node ID number", 0x1E456123, 
                0x30A4, 0x123, 0x456, 0x0);
    doOneHeader(self, @"Verify node ID number", 0x180AF123, 
                0x30A0, 0x123, 0x456, 0x0);
    doOneHeader(self, @"Verified node ID number", 0x190BF123, 
                0x30B0, 0x123, 0x456, 0x0);

    doOneHeader(self, @"P/C Event Report", 0x182DF123, 
                0x32D2, 0x123, 0x456, 0x0);
    
    
}

void doOneMti(id self, NSString* msg, u_int32_t header, u_int16_t expect, u_int8_t byte0) {
    u_int16_t result = (u_int16_t) MtiFromCanHeader(header, byte0);
    STAssertEquals(result, expect, @"%@ expected 0x%x, got 0x%x", msg, expect, result);    
}

- (void)testFromCAN {
    
    doOneMti(self, @"Initialization complete", 0x1908F123, 0x3080, 0x00);
    doOneMti(self, @"Verify node ID number",   0x1E456123, 0x30A4, 0x0A);
    doOneMti(self, @"Verify node ID number",   0x180AF123, 0x30A0, 0x00);
    doOneMti(self, @"Verified node ID number", 0x190BF123, 0x30B0, 0x00);
    
    doOneMti(self, @"P/C Event Report",        0x182DF123, 0x32D2, 0x00);
    
}
@end
