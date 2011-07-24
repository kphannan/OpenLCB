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

void doOneHeader(id self, NSString* msg, u_int16_t mti, u_int32_t expect, u_int8_t flags) {
    u_int16_t source = TEST_ASSUMED_SRC; // assumed for tests, at least for now
    u_int16_t dest = TEST_ASSUMED_DEST;  // assumed for tests, at least for now
    u_int32_t result = CanHeaderFromMti(mti, source, dest, 0xF);
    STAssertEquals(result, expect, @"%@ expected 0x%x, got 0x%x", msg, expect, result);    
}

- (void)testToCAN {
    
    doOneHeader(self, @"Initialization complete", MTI_INITIALIZATION_COMPLETE, 
                HEADER_CAN_INITIALIZATION_COMPLETE, 
                BYTE00_CAN_INITIALIZATION_COMPLETE);

    doOneHeader(self, @"Verify node ID addressed", MTI_VERIFY_NODE_ID_ADDR, 
                HEADER_CAN_VERIFY_NODE_ID_ADDR, 
                BYTE00_CAN_VERIFY_NODE_ID_ADDR);

    doOneHeader(self, @"Verify node ID global", MTI_VERIFY_NODE_ID_GLOBAL, 
                HEADER_CAN_VERIFY_NODE_ID_GLOBAL, 
                BYTE00_CAN_VERIFY_NODE_ID_GLOBAL);
    
    doOneHeader(self, @"Verified node ID number", MTI_VERIFIED_NODE_ID, 
                HEADER_CAN_VERIFIED_NODE_ID, 
                BYTE00_CAN_VERIFIED_NODE_ID);
    
    doOneHeader(self, @"Optional Interaction Rejected", MTI_OPTIONAL_INT_REJECTED, 
                HEADER_CAN_OPTIONAL_INT_REJECTED, 
                BYTE00_CAN_OPTIONAL_INT_REJECTED);
    
    doOneHeader(self, @"Terminate Due to Error", MTI_TERM_DUE_TO_ERROR, 
                HEADER_CAN_TERM_DUE_TO_ERROR, 
                BYTE00_CAN_TERM_DUE_TO_ERROR);
    
    
    doOneHeader(self, @"Protocol Support Inquiry", MTI_PROTOCOL_SUPPORT_INQ, 
                HEADER_CAN_PROTOCOL_SUPPORT_INQ, 
                BYTE00_CAN_PROTOCOL_SUPPORT_INQ);
    
    doOneHeader(self, @"Protocol Support Reply", MTI_PROTOCOL_SUPPORT_REPLY, 
                HEADER_CAN_PROTOCOL_SUPPORT_REPLY, 
                BYTE00_CAN_PROTOCOL_SUPPORT_REPLY);

    

    doOneHeader(self, @"Ident Consumers", MTI_IDENT_CONSUMERS, 
                HEADER_CAN_IDENT_CONSUMERS, 
                BYTE00_CAN_IDENT_CONSUMERS);
    
    doOneHeader(self, @"Consumer identify range", MTI_CONSUMER_IDENT_RANGE, 
                HEADER_CAN_CONSUMER_IDENT_RANGE, 
                BYTE00_CAN_CONSUMER_IDENT_RANGE);
    
    doOneHeader(self, @"Consumer Identified", MTI_CONSUMER_IDENTIFIED, 
                HEADER_CAN_CONSUMER_IDENTIFIED, 
                BYTE00_CAN_CONSUMER_IDENTIFIED);
    
    doOneHeader(self, @"Identify Producers", MTI_IDENT_PRODUCERS, 
                HEADER_CAN_IDENT_PRODUCERS, 
                BYTE00_CAN_IDENT_PRODUCERS);
    
    doOneHeader(self, @"Producer Identified Range", MTI_PRODUCER_IDENT_RANGE, 
                HEADER_CAN_PRODUCER_IDENT_RANGE, 
                BYTE00_CAN_PRODUCER_IDENT_RANGE);
    
    doOneHeader(self, @"Producer Identified", MTI_PRODUCER_IDENTIFIED, 
                HEADER_CAN_PRODUCER_IDENTIFIED, 
                BYTE00_CAN_PRODUCER_IDENTIFIED);
    
    doOneHeader(self, @"Identify Events Addr", MTI_IDENT_EVENTS_ADDR, 
                HEADER_CAN_IDENT_EVENTS_ADDR, 
                BYTE00_CAN_IDENT_EVENTS_ADDR);
    
    doOneHeader(self, @"Identify Events", MTI_IDENT_EVENTS, 
                HEADER_CAN_IDENT_EVENTS, 
                BYTE00_CAN_IDENT_EVENTS);
    
    doOneHeader(self, @"Learn Event", MTI_LEARN_EVENT, 
                HEADER_CAN_LEARN_EVENT, 
                BYTE00_CAN_LEARN_EVENT);
    
    doOneHeader(self, @"", MTI_PC_EVENT_REPORT, 
                HEADER_CAN_PC_EVENT_REPORT, 
                BYTE00_CAN_PC_EVENT_REPORT);
    
    
    
    doOneHeader(self, @"P/C Event Report", MTI_PC_EVENT_REPORT, 
                HEADER_CAN_PC_EVENT_REPORT, 
                BYTE00_CAN_PC_EVENT_REPORT);
    

    
    doOneHeader(self, @"Datagram", MTI_DATAGRAM, 
                HEADER_CAN_DATAGRAM, 
                BYTE00_CAN_DATAGRAM);
    
    doOneHeader(self, @"Datagram OK", MTI_DATAGRAM_OK, 
                HEADER_CAN_DATAGRAM_OK, 
                BYTE00_CAN_DATAGRAM_OK);
    
    doOneHeader(self, @"Datagram reject", MTI_DATAGRAM_REJ, 
                HEADER_CAN_DATAGRAM_REJ, 
                BYTE00_CAN_DATAGRAM_REJ);
        
    
    doOneHeader(self, @"Stream Init Req", MTI_STREAM_INIT_REQ, 
                HEADER_CAN_STREAM_INIT_REQ, 
                BYTE00_CAN_STREAM_INIT_REQ);
    
    doOneHeader(self, @"Stream Init Reply", MTI_STREAM_INIT_REPLY, 
                HEADER_CAN_STREAM_INIT_REPLY, 
                BYTE00_CAN_STREAM_INIT_REPLY);
    
    doOneHeader(self, @"Stream Data", MTI_STREAM_DATA, 
                HEADER_CAN_STREAM_DATA, 
                BYTE00_CAN_STREAM_DATA);
    
    doOneHeader(self, @"Stream Data Proceed", MTI_STREAM_DATA_PROC, 
                HEADER_CAN_STREAM_DATA_PROC, 
                BYTE00_CAN_STREAM_DATA_PROC);
    
    doOneHeader(self, @"Stream Data Complete", MTI_STREAM_DATA_COMP, 
                HEADER_CAN_STREAM_DATA_COMP, 
                BYTE00_CAN_STREAM_DATA_COMP);
    

}

void doOneMti(id self, NSString* msg, u_int16_t expect, u_int32_t header, u_int8_t byte0) {
    u_int16_t result = (u_int16_t) MtiFromCanHeader(header, byte0);
    STAssertEquals(result, expect, @"%@ expected 0x%x, got 0x%x", msg, expect, result);    
}

- (void)testFromCAN {
    
    doOneMti(self, @"Initialization complete", 
                MTI_INITIALIZATION_COMPLETE, 
                HEADER_CAN_INITIALIZATION_COMPLETE, BYTE00_CAN_INITIALIZATION_COMPLETE);

    doOneMti(self, @"Verify node ID global",   
                MTI_VERIFY_NODE_ID_GLOBAL, 
                HEADER_CAN_VERIFY_NODE_ID_GLOBAL, BYTE00_CAN_VERIFY_NODE_ID_GLOBAL);

    doOneMti(self, @"Verify node ID addressed",   
                MTI_VERIFY_NODE_ID_ADDR, 
                HEADER_CAN_VERIFY_NODE_ID_ADDR, BYTE00_CAN_VERIFY_NODE_ID_ADDR);

    doOneMti(self, @"Verified node ID number", 
             MTI_VERIFIED_NODE_ID, 
             HEADER_CAN_VERIFIED_NODE_ID, BYTE00_CAN_VERIFIED_NODE_ID);
    
    doOneMti(self, @"Optional Interaction Rejected", 
             MTI_OPTIONAL_INT_REJECTED, 
             HEADER_CAN_OPTIONAL_INT_REJECTED, BYTE00_CAN_OPTIONAL_INT_REJECTED);
    
    doOneMti(self, @"Terminate Due to Error", 
             MTI_TERM_DUE_TO_ERROR, 
             HEADER_CAN_TERM_DUE_TO_ERROR, BYTE00_CAN_TERM_DUE_TO_ERROR);
    

    doOneMti(self, @"Protocol Support Inquiry", 
             MTI_PROTOCOL_SUPPORT_INQ, 
             HEADER_CAN_PROTOCOL_SUPPORT_INQ, BYTE00_CAN_PROTOCOL_SUPPORT_INQ);
    
    doOneMti(self, @"Protocol Support Reply", 
             MTI_PROTOCOL_SUPPORT_REPLY, 
             HEADER_CAN_PROTOCOL_SUPPORT_REPLY, BYTE00_CAN_PROTOCOL_SUPPORT_REPLY);
    

    
    doOneMti(self, @"Ident Consumers", MTI_IDENT_CONSUMERS, 
                HEADER_CAN_IDENT_CONSUMERS, 
                BYTE00_CAN_IDENT_CONSUMERS);
    
    doOneMti(self, @"Consumer identify range", MTI_CONSUMER_IDENT_RANGE, 
                HEADER_CAN_CONSUMER_IDENT_RANGE, 
                BYTE00_CAN_CONSUMER_IDENT_RANGE);
    
    doOneMti(self, @"Consumer Identified", MTI_CONSUMER_IDENTIFIED, 
                HEADER_CAN_CONSUMER_IDENTIFIED, 
                BYTE00_CAN_CONSUMER_IDENTIFIED);
    
    doOneMti(self, @"Identify Producers", MTI_IDENT_PRODUCERS, 
                HEADER_CAN_IDENT_PRODUCERS, 
                BYTE00_CAN_IDENT_PRODUCERS);
    
    doOneMti(self, @"Producer Identified Range", MTI_PRODUCER_IDENT_RANGE, 
                HEADER_CAN_PRODUCER_IDENT_RANGE, 
                BYTE00_CAN_PRODUCER_IDENT_RANGE);
    
    doOneMti(self, @"Producer Identified", MTI_PRODUCER_IDENTIFIED, 
                HEADER_CAN_PRODUCER_IDENTIFIED, 
                BYTE00_CAN_PRODUCER_IDENTIFIED);
    
    doOneMti(self, @"Identify Events Addr", MTI_IDENT_EVENTS_ADDR, 
                HEADER_CAN_IDENT_EVENTS_ADDR, 
                BYTE00_CAN_IDENT_EVENTS_ADDR);
    
    doOneMti(self, @"Identify Events", MTI_IDENT_EVENTS, 
                HEADER_CAN_IDENT_EVENTS, 
                BYTE00_CAN_IDENT_EVENTS);
    
    doOneMti(self, @"Learn Event", MTI_LEARN_EVENT, 
                HEADER_CAN_LEARN_EVENT, 
                BYTE00_CAN_LEARN_EVENT);
    
    doOneMti(self, @"", MTI_PC_EVENT_REPORT, 
                HEADER_CAN_PC_EVENT_REPORT, 
                BYTE00_CAN_PC_EVENT_REPORT);

    
    doOneMti(self, @"Datagram", 
             MTI_DATAGRAM, 
             HEADER_CAN_DATAGRAM, BYTE00_CAN_DATAGRAM);
    
    doOneMti(self, @"Datagram OK", 
             MTI_DATAGRAM_OK, 
             HEADER_CAN_DATAGRAM_OK, BYTE00_CAN_DATAGRAM_OK);
    
    doOneMti(self, @"Datagram Rej", 
             MTI_DATAGRAM_REJ, 
             HEADER_CAN_DATAGRAM_REJ, BYTE00_CAN_DATAGRAM_REJ);
    
    

    doOneMti(self, @"Stream Init Req", 
                MTI_STREAM_INIT_REQ, 
                HEADER_CAN_STREAM_INIT_REQ, 
                BYTE00_CAN_STREAM_INIT_REQ);
    
    doOneMti(self, @"Stream Init Reply", 
                MTI_STREAM_INIT_REPLY, 
                HEADER_CAN_STREAM_INIT_REPLY, 
                BYTE00_CAN_STREAM_INIT_REPLY);
    
    doOneMti(self, @"Stream Data", 
                MTI_STREAM_DATA, 
                HEADER_CAN_STREAM_DATA, 
                BYTE00_CAN_STREAM_DATA);
    
    doOneMti(self, @"Stream Data Proceed", 
                MTI_STREAM_DATA_PROC, 
                HEADER_CAN_STREAM_DATA_PROC, 
                BYTE00_CAN_STREAM_DATA_PROC);
    
    doOneMti(self, @"Stream Data Complete", 
                MTI_STREAM_DATA_COMP, 
                HEADER_CAN_STREAM_DATA_COMP, 
                BYTE00_CAN_STREAM_DATA_COMP);
    
}
@end
