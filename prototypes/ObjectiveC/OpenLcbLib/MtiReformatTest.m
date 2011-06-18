//
//  MtiReformatTest.m
//  OpenLcbLib
//
//  Created by Bob Jacobsen on 5/31/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//

#import "MtiReformatTest.h"
#import "MtiReformat.h"

#import "OlcbTestDefinitions.h"

@implementation MtiReformatTest

void doOneHeader(id self, NSString* msg, u_int16_t mti, u_int32_t expect, u_int16_t source, u_int16_t dest, u_int8_t flags) {
    u_int32_t result = CanHeaderFromMti(mti, source, dest, 0xF);
    STAssertEquals(result, expect, @"%@ expected 0x%x, got 0x%x", msg, expect, result);    
}

- (void)testToCAN {
    
    doOneHeader(self, @"Initialization complete", MTI_INITIALIZATION_COMPLETE, 
                HEADER_CAN_INITIALIZATION_COMPLETE, 
                0x123, 0x456, BYTE00_CAN_INITIALIZATION_COMPLETE);

    doOneHeader(self, @"Verify node ID number", MTI_VERIFY_NODE_ID_GLOBAL, 
                HEADER_CAN_VERIFY_NODE_ID_GLOBAL, 
                0x123, 0x456, BYTE00_CAN_VERIFY_NODE_ID_GLOBAL);
    
    doOneHeader(self, @"Verify node ID number", MTI_VERIFY_NODE_ID_ADDR, 
                HEADER_CAN_VERIFY_NODE_ID_ADDR, 
                0x123, 0x456, BYTE00_CAN_VERIFY_NODE_ID_ADDR);

    doOneHeader(self, @"Verified node ID number", MTI_VERIFIED_NODE_ID, 
                HEADER_CAN_VERIFIED_NODE_ID, 
                0x123, 0x456, BYTE00_CAN_VERIFIED_NODE_ID);

    doOneHeader(self, @"P/C Event Report", MTI_PC_EVENT_REPORT, 
                HEADER_CAN_PC_EVENT_REPORT, 
                0x123, 0x456, BYTE00_CAN_PC_EVENT_REPORT);
    
    
}

void doOneMti(id self, NSString* msg, u_int16_t expect, u_int32_t header, u_int8_t byte0) {
    u_int16_t result = (u_int16_t) MtiFromCanHeader(header, byte0);
    STAssertEquals(result, expect, @"%@ expected 0x%x, got 0x%x", msg, expect, result);    
}

- (void)testFromCAN {
    
    doOneMti(self, @"Initialization complete", 
                MTI_INITIALIZATION_COMPLETE, 
                HEADER_CAN_INITIALIZATION_COMPLETE, BYTE00_CAN_INITIALIZATION_COMPLETE);

    doOneMti(self, @"Verify node ID number",   
                MTI_VERIFY_NODE_ID_GLOBAL, 
                HEADER_CAN_VERIFY_NODE_ID_GLOBAL, BYTE00_CAN_VERIFY_NODE_ID_GLOBAL);

    doOneMti(self, @"Verify node ID number",   
                MTI_VERIFY_NODE_ID_ADDR, 
                HEADER_CAN_VERIFY_NODE_ID_ADDR, BYTE00_CAN_VERIFY_NODE_ID_ADDR);

    doOneMti(self, @"Verified node ID number", 
                MTI_VERIFIED_NODE_ID, 
                HEADER_CAN_VERIFIED_NODE_ID, BYTE00_CAN_VERIFIED_NODE_ID);
    
    doOneMti(self, @"P/C Event Report",        
                MTI_PC_EVENT_REPORT, 
                HEADER_CAN_PC_EVENT_REPORT, BYTE00_CAN_PC_EVENT_REPORT);
    
}
@end
