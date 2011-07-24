//
//  OlcbTestDefinitions.h
//  OpenLcbLib
//
//  Created by Bob Jacobsen on 6/15/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//
//  Common definitions used in test code.
//
//  We _redefine_ things like MTIs, etc, here so that their values in the
//  main code are actually tested.


// For CAN, assume source address alias is 0x123, dest address (if present) is 0x456
#define TEST_ASSUMED_SRC  0x123
#define TEST_ASSUMED_DEST 0x456

#define MTI_CAN_INIT_FROM_FIELDS 0x1800000

// Base messages

#define MTI_INITIALIZATION_COMPLETE         0x3080
#define HEADER_CAN_INITIALIZATION_COMPLETE  0x19087123
#define BYTE00_CAN_INITIALIZATION_COMPLETE  0x00

#define MTI_VERIFY_NODE_ID_ADDR             0x30A4
#define HEADER_CAN_VERIFY_NODE_ID_ADDR      0x1E456123
#define BYTE00_CAN_VERIFY_NODE_ID_ADDR      0x0A

#define MTI_VERIFY_NODE_ID_GLOBAL           0x10A0
#define HEADER_CAN_VERIFY_NODE_ID_GLOBAL    0x180A7123
#define BYTE00_CAN_VERIFY_NODE_ID_GLOBAL    0x00

#define MTI_VERIFIED_NODE_ID                0x30B0
#define HEADER_CAN_VERIFIED_NODE_ID         0x190B7123
#define BYTE00_CAN_VERIFIED_NODE_ID         0x00

#define MTI_OPTIONAL_INT_REJECTED           0x30C4
#define HEADER_CAN_OPTIONAL_INT_REJECTED    0x1E456123
#define BYTE00_CAN_OPTIONAL_INT_REJECTED    0x0C

#define MTI_TERM_DUE_TO_ERROR               0x30D4
#define HEADER_CAN_TERM_DUE_TO_ERROR        0x1E456123
#define BYTE00_CAN_TERM_DUE_TO_ERROR        0x0D

// Protocol support messages

#define MTI_PROTOCOL_SUPPORT_INQ            0x32E4
#define HEADER_CAN_PROTOCOL_SUPPORT_INQ     0x1E456123
#define BYTE00_CAN_PROTOCOL_SUPPORT_INQ     0x2E

#define MTI_PROTOCOL_SUPPORT_REPLY          0x32F4
#define HEADER_CAN_PROTOCOL_SUPPORT_REPLY   0x1E456123
#define BYTE00_CAN_PROTOCOL_SUPPORT_REPLY   0x2F

// Event exchange messages

#define MTI_IDENT_CONSUMERS                 0x1242
#define HEADER_CAN_IDENT_CONSUMERS          0x1824F123
#define BYTE00_CAN_IDENT_CONSUMERS          0x00

#define MTI_CONSUMER_IDENT_RANGE            0x3252
#define HEADER_CAN_CONSUMER_IDENT_RANGE     0x1925F123
#define BYTE00_CAN_CONSUMER_IDENT_RANGE     0x00

#define MTI_CONSUMER_IDENTIFIED             0x3263
#define HEADER_CAN_CONSUMER_IDENTIFIED      0x1926B123
#define BYTE00_CAN_CONSUMER_IDENTIFIED      0x00

#define MTI_IDENT_PRODUCERS                 0x1282
#define HEADER_CAN_IDENT_PRODUCERS          0x1828F123
#define BYTE00_CAN_IDENT_PRODUCERS          0x00

#define MTI_PRODUCER_IDENT_RANGE            0x3292
#define HEADER_CAN_PRODUCER_IDENT_RANGE     0x1929F123
#define BYTE00_CAN_PRODUCER_IDENT_RANGE     0x00

#define MTI_PRODUCER_IDENTIFIED             0x32A3
#define HEADER_CAN_PRODUCER_IDENTIFIED      0x192AB123
#define BYTE00_CAN_PRODUCER_IDENTIFIED      0x00

#define MTI_IDENT_EVENTS_ADDR               0x32B4
#define HEADER_CAN_IDENT_EVENTS_ADDR        0x1E456123
#define BYTE00_CAN_IDENT_EVENTS_ADDR        0x2B

#define MTI_IDENT_EVENTS                    0x12B0
#define HEADER_CAN_IDENT_EVENTS             0x182B7123
#define BYTE00_CAN_IDENT_EVENTS             0x00

#define MTI_LEARN_EVENT                     0x12C2
#define HEADER_CAN_LEARN_EVENT              0x182CF123
#define BYTE00_CAN_LEARN_EVENT              0x00

#define MTI_PC_EVENT_REPORT                 0x12D2
#define HEADER_CAN_PC_EVENT_REPORT          0x182DF123
#define BYTE00_CAN_PC_EVENT_REPORT          0x00

// Datagram messages

#define MTI_DATAGRAM                        0x3404
#define HEADER_CAN_DATAGRAM                 0x1D456123
#define BYTE00_CAN_DATAGRAM                 0x00

#define MTI_DATAGRAM_OK                     0x34C4
#define HEADER_CAN_DATAGRAM_OK              0x1E456123
#define BYTE00_CAN_DATAGRAM_OK              0x4C

#define MTI_DATAGRAM_REJ                    0x34D4
#define HEADER_CAN_DATAGRAM_REJ             0x1E456123
#define BYTE00_CAN_DATAGRAM_REJ             0x4D


// Stream messages

#define MTI_STREAM_INIT_REQ                 0x34E4
#define HEADER_CAN_STREAM_INIT_REQ          0x1E456123
#define BYTE00_CAN_STREAM_INIT_REQ          0x4E

#define MTI_STREAM_INIT_REPLY               0x34F4
#define HEADER_CAN_STREAM_INIT_REPLY        0x1E456123
#define BYTE00_CAN_STREAM_INIT_REPLY        0x4F

#define MTI_STREAM_DATA                     0x3694
#define HEADER_CAN_STREAM_DATA              0x1F456123
#define BYTE00_CAN_STREAM_DATA              0x00

#define MTI_STREAM_DATA_PROC                0x36A4
#define HEADER_CAN_STREAM_DATA_PROC         0x1E456123
#define BYTE00_CAN_STREAM_DATA_PROC         0x6A

#define MTI_STREAM_DATA_COMP                0x36B4
#define HEADER_CAN_STREAM_DATA_COMP         0x1E456123
#define BYTE00_CAN_STREAM_DATA_COMP         0x6B



//#define MTI_                0x
//#define HEADER_CAN_         0x1E456123
//#define BYTE00_CAN_         0x00


