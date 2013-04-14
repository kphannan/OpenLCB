//
//  OlcbMtiDefinitions.h
//  OpenLcbLib
//
//  Created by Bob Jacobsen on 7/23/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//

#define MTI_INITIALIZATION_COMPLETE         0x3080
#define MTI_VERIFY_NODE_ID_ADDR             0x30A4
#define MTI_VERIFY_NODE_ID_GLOBAL           0x10A0
#define MTI_VERIFIED_NODE_ID                0x30B0
#define MTI_OPTIONAL_INT_REJECTED           0x30C4
#define MTI_TERM_DUE_TO_ERROR               0x30D4

// Protocol support messages

#define MTI_PROTOCOL_SUPPORT_INQ            0x32E4
#define MTI_PROTOCOL_SUPPORT_REPLY          0x32F4

// Event exchange messages

#define MTI_IDENT_CONSUMERS                 0x1242
#define MTI_CONSUMER_IDENT_RANGE            0x3252
#define MTI_CONSUMER_IDENTIFIED             0x3263
#define MTI_IDENT_PRODUCERS                 0x1282
#define MTI_PRODUCER_IDENT_RANGE            0x3292
#define MTI_PRODUCER_IDENTIFIED             0x32A3
#define MTI_IDENT_EVENTS_ADDR               0x32B4
#define MTI_IDENT_EVENTS                    0x12B0
#define MTI_LEARN_EVENT                     0x12C2
#define MTI_PC_EVENT_REPORT                 0x12D2

// Datagram messages

#define MTI_DATAGRAM                        0x3404
#define MTI_DATAGRAM_OK                     0x34C4
#define MTI_DATAGRAM_REJ                    0x34D4

// Stream messages

#define MTI_STREAM_INIT_REQ                 0x34E4
#define MTI_STREAM_INIT_REPLY               0x34F4
#define MTI_STREAM_DATA                     0x3694
#define MTI_STREAM_DATA_PROC                0x36A4
#define MTI_STREAM_DATA_COMP                0x36B4
