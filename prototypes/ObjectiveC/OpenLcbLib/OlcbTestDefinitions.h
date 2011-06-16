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


#define MTI_CAN_INIT_FROM_FIELDS 0x1800000

#define HEADER_CAN_INITIALIZATION_COMPLETE  0x1908F123
#define BYTE00_CAN_INITIALIZATION_COMPLETE  0x00
#define MTI_INITIALIZATION_COMPLETE         0x3080

#define HEADER_CAN_VERIFY_NODE_ID_GLOBAL    0x180AF123
#define BYTE00_CAN_VERIFY_NODE_ID_GLOBAL    0x00
#define MTI_VERIFY_NODE_ID_GLOBAL           0x30A0

#define HEADER_CAN_VERIFY_NODE_ID_ADDR      0x1E456123
#define BYTE00_CAN_VERIFY_NODE_ID_ADDR      0x0A
#define MTI_VERIFY_NODE_ID_ADDR             0x30A4

#define HEADER_CAN_VERIFIED_NODE_ID         0x190BF123
#define BYTE00_CAN_VERIFIED_NODE_ID         0x00
#define MTI_VERIFIED_NODE_ID                0x30B0

#define HEADER_CAN_PC_EVENT_REPORT          0x182DF123
#define BYTE00_CAN_PC_EVENT_REPORT          0x00
#define MTI_PC_EVENT_REPORT                 0x32D2

