unit olcb_defines;

// ******************************************************************************
//
// * Copyright:
//     (c) Mustangpeak Software 2012.
//
//     The contents of this file are subject to the GNU GPL v3 licence/ you maynot use
//     this file except in compliance with the License. You may obtain a copy of the
//     License at http://www.gnu.org/licenses/gpl.html
//
// * Revision History:
//     2012-08-05:   Created
//
// * Description:

//
// *****************************************************************************

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  MTI_ADDRESS_PRESENT                = $00008000;                                // Address in the CAN Data present if set

  MTI_CAN                            = $00000000;                                // Frame Type CAN Control Message
  MTI_CID0                           = $07000000;                                // First 12 Bits of 48 bit Node ID
  MTI_CID1                           = $06000000;                                // 2rd 12 Bits of 48 bit Node ID
  MTI_CID2                           = $05000000;                                // 3nd 12 Bits of 48 bit Node ID
  MTI_CID3                           = $04000000;                                // Last 12 Bits of 48 bit Node ID
  MTI_CID4                           = $03000000;                                // non-OpenLCB Protocol
  MTI_CID5                           = $02000000;                                // non-OpenLCB Protocol
  MTI_CID6                           = $01000000;                                // non-OpenLCB Protocol
  MTI_CID_MASK                       = $07000000;

  MTI_RID                            = $00700000;                                // Reserve ID
  MTI_AMD                            = $00701000;                                // Alias Map Definition
  MTI_AME                            = $00702000;                                // Alias Mapping Enquiry
  MTI_AMR                            = $00703000;                                // Alias Map Reset Frame

  MTI_MASK                              = $0FFFF000;
  MTI_FRAME_TYPE_MASK                   = $0F000000;
  MTI_FRAME_TYPE_GENERAL                = $09000000;
  MTI_FRAME_TYPE_DATAGRAM_ONLY_FRAME    = $0A000000;
  MTI_FRAME_TYPE_DATAGRAM_FRAME_START   = $0B000000;
  MTI_FRAME_TYPE_DATAGRAM_FRAME         = $0C000000;
  MTI_FRAME_TYPE_DATAGRAM_FRAME_END     = $0D000000;

  MTI_ADDRESSED_MASK                 = $00008000;
  MTI_SIMPLE_PROTOCOL_MASK           = $00010000;
  MTI_EVENT_PRESENT_MASK             = $00002000;

  MTI_INITIALIZATION_COMPLETE        = $09100000;                                // Databytes = Full Node ID
  MTI_VERIFY_NODE_ID_NUMBER_DEST     = $09488000;                                // Databytes = Destination Alias
  MTI_VERIFY_NODE_ID_NUMBER          = $09490000;                                //
  MTI_VERIFIED_NODE_ID_NUMBER        = $09170000;                                // {Optional Full Node ID}
  MTI_OPTIONAL_INTERACTION_REJECTED  = $09068000;                                // Databytes = Destination Alias, Error, {Optional Info}
  MTI_TERMINATE_DUE_TO_ERROR         = $090A8000;                                // Databytes = Destination Alias, Error, {Optional Info}

  MTI_PROTOCOL_SUPPORT_INQUIRY       = $09828000;                                // Databytes = Destination Alias
  MTI_PROTOCOL_SUPPORT_REPLY         = $09668000;                                // Databytes = Destination Alias, Protocol Flags

  MTI_CONSUMER_IDENTIFY              = $098F4000;                                // Databytes = EventID
  MTI_CONSUMER_IDENTIFY_RANGE        = $094A4000;                                // Databytes = EventID with Mask
  MTI_CONSUMER_IDENTIFIED_UNKNOWN    = $094C7000;                                // Databytes = EventID
  MTI_CONSUMER_IDENTIFIED_SET        = $094C4000;                                // Databytes = EventID
  MTI_CONSUMER_IDENTIFIED_CLEAR      = $094C5000;                                // Databytes = EventID
  MTI_CONSUMER_IDENTIFIED_RESERVED   = $094C6000;                                // Databytes = EventID
  MTI_PRODUCER_IDENDIFY              = $09914000;                                // Databytes = EventID
  MTI_PRODUCER_IDENTIFY_RANGE        = $09524000;                                // Databytes = EventID with Mask
  MTI_PRODUCER_IDENTIFIED_UNKNOWN    = $09547000;                                // Databytes = EventID
  MTI_PRODUCER_IDENTIFIED_SET        = $09544000;                                // Databytes = EventID
  MTI_PRODUCER_IDENTIFIED_CLEAR      = $09545000;                                // Databytes = EventID
  MTI_PRODUCER_IDENTIFIED_RESERVED   = $09546000;                                // Databytes = EventID
  MTI_EVENTS_IDENTIFY_DEST           = $09968000;                                // Databytes = Destination Alias
  MTI_EVENTS_IDENTIFY                = $09970000;                                //
  MTI_EVENT_LEARN                    = $09594000;                                // Databytes = EventID
  MTI_PC_EVENT_REPORT                = $095B4000;                                // Databytes = EventID  (Infamouse PCER)

  MTI_SIMPLE_NODE_INFO_REQUEST       = $09DE8000;                                // Databytes = Destination Alias
  MTI_SIMPLE_NODE_INFO_REPLY         = $09A08000;                                // Databytes = Destination Alias, ACDI Data

  MTI_SIMPLE_TRAIN_INFO_REQUEST      = $09DA8000;                                // Databytes = Destination Alias
  MTI_SIMPLE_TRAIN_INFO_REPLY        = $099C8000;                                // Databytes = Destination Alias, ACDI Data

  MTI_TRACTION_PROTOCOL              = $095EA000;                                // Databyte = depends
  MTI_TRACTION_PROXY_PROTOCOL        = $091EA000;
  MTI_TRACTION_REPLY                 = $095E8000;                                // Databyte = depends
  MTI_TRACTION_PROXY_REPLY           = $091E8000;

  MTI_STREAM_INIT_REQUEST            = $09CC8000;
  MTI_STREAM_INIT_REPLY              = $09868000;
  MTI_FRAME_TYPE_CAN_STREAM_SEND     = $0F000000;
  MTI_STREAM_PROCEED                 = $09888000;
  MTI_STREAM_COMPLETE                = $098A8000;

  MTI_DATAGRAM_OK_REPLY              = $09A28000;                                // Databytes = Destination Alias
  MTI_DATAGRAM_REJECTED_REPLY        = $09A48000;                                // Databytes = Destination Alias, Error Code

  DATAGRAM_OK_ACK_REPLY_PENDING      = $80;

  STREAM_REPLY_CONTENT_TYPE                               = $01;                // LSB = 1 = first 6 bytes in data are UID of data type in stream
  STREAM_REPLY_UNEXPECTED_ERROR                           = $02;                // Bit 2 = 1 the Stream was Rejected, Out of order, or other "should not happen" error
  STREAM_REPLY_PERMANENT_ERROR                            = $40;                // Bit 6 = 1 = if STREAM_REPLY_ACCEPT = 1 then this is the error type where 1 = permanent
  STREAM_REPLY_ACCEPT                                     = $80;                // MSB = 1 = Accept

  STREAM_REPLY_ERROR_LOGGED                               = $01;                // Error was logged
  STREAM_REPLY_INVALID_REQUEST                            = $20;                // if Error is permanent then these are the possible reasons
  STREAM_REPLY_SOURCE_NOT_PERMITTED                       = $40;                // if Error is permanent then these are the possible reasons
  STREAM_REPLY_STREAM_NOT_ACCEPTED                        = $80;                // if Error is permanent then these are the possible reasons

  MASK_SOURCE_ALIAS                  = $00000FFF;                                // Masks out just the Source Alias Address

  PIP_PIP                            = $800000000000;
  PIP_DATAGRAM                       = $400000000000;
  PIP_STREAM                         = $200000000000;
  PIP_MEMORY_CONFIG                  = $100000000000;
  PIP_RESERVATION                    = $080000000000;
  PIP_EVENT_EXCHANGE                 = $040000000000;
  PIP_IDENTIFCIATION                 = $020000000000;
  PIP_TEACH_LEARN                    = $010000000000;
  PIP_REMOTE_BUTTON                  = $008000000000;
  PIP_ABBREVIATED_CDI                = $004000000000;
  PIP_DISPLAY                        = $002000000000;
  PIP_SIMPLE_NODE_ID                 = $001000000000;
  PIP_CDI                            = $000800000000;
  PIP_TRACTION                       = $000400000000;
  PIP_FDI                            = $000200000000;
  PIP_TRACTION_PROXY                 = $000100000000;

  PIP_UNASSIGNED                     = $0000FFFFFFF0;
  PIP_RESERVED                       = $00000000000F;

  STR_PIP_PIP                        = 'Protocol Identification Protocol';
  STR_PIP_DATAGRAM                   = 'Datagram Protocol';
  STR_PIP_STREAM                     = 'Stream Protocol';
  STR_PIP_MEMORY_CONFIG              = 'Memory Configuration Protocol';
  STR_PIP_RESERVATION                = 'Reservation Protocol';
  STR_PIP_EVENT_EXCHANGE             = 'Event Exchange Protocol';
  STR_PIP_IDENTIFCIATION             = 'Identification Protocol';
  STR_PIP_TEACH_LEARN                = 'Teach/Learn Protocol';
  STR_PIP_REMOTE_BUTTON              = 'Remote Button Protocol';
  STR_PIP_ABBREVIATED_CDI            = 'Abbreviated CDI Protocol';
  STR_PIP_DISPLAY                    = 'Display Protocol';
  STR_PIP_SIMPLE_NODE_ID             = 'Simple Node ID (SNII/SNIP) Protocol';
  STR_PIP_CDI                        = 'Configuration Description Information (CDI) Protocol';
  STR_PIP_TRACTION                   = 'Traction Protocol';
  STR_PIP_FDI                        = 'Function Description Information (FDI) Protocol';
  STR_PIP_TRACTION_PROTOCOL                        = 'Traction Proxy Protocol';


  OIR_TEMPORARY_ERROR                 = $1000;
  OIR_PERMANENT_ERROR                 = $2000;

const
  MCP_WRITE                           = $00;                                    // MemoryConfigurationProtocol - Write Memory Mask
  MCP_WRITE_STREAM                    = $20;
  MCP_READ                            = $40;                                    // MemoryConfigurationProtocol - Read Memory Mask
  MCP_READ_STREAM                     = $60;
  MCP_OPERATION                       = $80;                                    // MemoryConfigurationProtocol - Operation Mask
  MCP_READ_DATAGRAM_REPLY             = $50;                                    // MemoryConfigurationProtocol - Read Reply Mask [Does not include the Address Space Mask "or" it with the the Address space masks below]
  MCP_WRITE_DATAGRAM_REPLY            = $10;
  MCP_READ_STREAM_REPLY               = $60;

  MCP_READ_OK                         = $50;
  MCP_READ_ERROR                      = $58;
  MCP_WRITE_OK                        = $10;
  MCP_WRITE_ERROR                     = $18;

  MCP_CDI                             = $03;                                    // Address space = CDI ($FF) access Mask
  MCP_ALL                             = $02;                                    // Address space = All ($FE) access Mask
  MCP_CONFIGURATION                   = $01;                                    // Address space = Basic Configuration ($FD) access Mask
  MCP_NONE                            = $00;                                    // Use the optional {Space} byte in the datagram to defin the address space

  MCP_OP_GET_CONFIG                  = $80;                                     // MemoryConfigurationProtocol Operation - Get Configuration
  MCP_OP_GET_CONFIG_REPLY            = $82;                                     // MemoryConfigurationProtocol Operation - Get Configuration Reply
  MCP_OP_GET_ADD_SPACE_INFO          = $84;                                     // MemoryConfigurationProtocol Operation - Get Add Space Info
  MCP_OP_GET_ADD_SPACE_INFO_REPLY    = $86;                                     // MemoryConfigurationProtocol Operation - Get Add Space Info Reply
  MCP_OP_LOCK                        = $88;                                     // MemoryConfigurationProtocol Operation - Lock Node
  MCP_OP_LOCK_REPLY                  = $8A;                                     // MemoryConfigurationProtocol Operation - Lock Node Reply
  MCP_OP_GET_UNIQUEID                = $8C;                                     // MemoryConfigurationProtocol Operation - Get Unique ID Key
  MCP_OP_GET_UNIQUEID_REPLY          = $8E;                                     // MemoryConfigurationProtocol Operation - Get Unique ID Key Reply

  MCP_OP_GET_ADD_SPACE_INFO_REPLY_PRESENT = $01;

  MCP_OP_FREEZE                      = $A0;                                     // MemoryConfigurationProtocol Operation - Freeze Node
  MCP_OP_INDICATE                    = $A4;                                     // MemoryConfigurationProtocol Operation - Indicate
  MCP_OP_RESETS                      = $A8;                                     // MemoryConfigurationProtocol Operation - Resets


  MSI_CDI                            = $FF;                                     // MemorySpaceIdentifier - Access the Configuration Definition Infomation (CDI)
  MSI_ALL                            = $FE;                                     // MemorySpaceIdentifier - Access All memory (define all in the application)
  MSI_CONFIG                         = $FD;                                     // MemorySpaceIdentifier - Access basic configuration memory that feeds into the CDI
  MSI_ACDI_MFG                       = $FC;                                     // MemorySpaceIdentifier - Access the ACDI Manfacturers Info
  MSI_ACDI_USER                      = $FB;                                     // MemorySpaceIdentifier - Access the ACDI User definable Info
  MSI_FDI                            = $FA;                                     // MemorySpaceIdentifier - Access the Traction Functions definable Info
  MSI_FSI                            = $F9;                                     // MemorySpaceIdentifier = Access the Traction Function State Information

  MCO_WRITE_UNDER_MASK               = $8000;                                   // MemoryConfigurationOptions - Write under mask supported
  MCO_UNALIGNED_READS                = $4000;                                   // MemoryConfigurationOptions - Unaligned memory Reads supported
  MCO_UNALIGNED_WRITES               = $2000;                                   // MemoryConfigurationOptions - Unaligned memory Writes supported
  MCO_ACDI_MFG_READS                 = $0800;                                   // MemoryConfigurationOptions - Address Space 0xFC supported (ACDI Manufacturer Area) for reads
  MCO_ACDI_USER_READS                = $0400;                                   // MemoryConfigurationOptions - Address Space 0xFB supported (ACDI User Defined Area) for reads
  MCO_ACDI_USER_WRITES               = $0200;                                   // MemoryConfigurationOptions - Address Space 0xFB supported (ACDI User Defined Area) for writes
  MCO_RESERVED                       = $1FFF;

  MCWL_ONE_BYTE                      = $80;                                     // MemoryConfigurationWriteLength - 1 Byte Write Supported
  MCWL_TWO_BYTE                      = $40;                                     // MemoryConfigurationWriteLength - 2 Byte Write Supported
  MCWL_FOUR_BYTE                     = $20;                                     // MemoryConfigurationWriteLength - 4 Byte Write Supported
  MCWL_64_BYTE                       = $10;                                     // MemoryConfigurationWriteLength - 64 Byte (exactly) Write Supported
  MCWL_ARBITRARY_BYTE                = $02;                                     // MemoryConfigurationWriteLength - Any Number of Byte Write Supported
  MCWL_STREAM_WRITE_SUPPORTED        = $01;                                     // MemoryConfigurationWriteLength - Stream Write Supported
  MCWL_RESERVED                      = $0C;

  TRACTION_PROXY_ALLOCATE            = $01;
  TRACTION_PROXY_ATTACH              = $02;
  TRACTION_PROXY_DETACH              = $03;
  TRACTION_PROXY_MANAGE              = $80;
  TRACTION_PROXY_MANAGE_RESERVE      = $01;
  TRACTION_PROXY_MANAGE_RELEASE      = $02;

  TRACTION_PROXY_TECH_ID_DCC              = $01;
  TRACTION_PROXY_TECH_ID_DC               = $02;
  TRACTION_PROXY_TECH_ID_MARKLIN_DIG      = $03;
  TRACTION_PROXY_TECH_ID_MARKLIN_DELTA    = $04;
  TRACTION_PROXY_TECH_ID_MARKLIN_DIG_ESU  = $05;
  TRACTION_PROXY_TECH_ID_SELECTRIX        = $06;
  TRACTION_PROXY_TECH_ID_MTH_DCS          = $07;
  TRACTION_PROXY_TECH_ID_LIONEL_TMCC      = $08;

  TRACTION_PROXY_MANAGE_RESERVE_REPLY_OK   = $00;    // Failed is not 0
  TRACTION_PROXY_MANAGE_RESERVE_REPLY_FAIL = $FF;    // Failed

  //

  TRACTION_FLAGS_ALIAS_INCLUDED       = $01;

  TRACTION_SPEED_DIR                  = $00;
  TRACTION_FUNCTION                   = $01;
  TRACTION_E_STOP                     = $02;

  TRACTION_QUERY_SPEED                = $10;
  TRACTION_QUERY_FUNCTION             = $11;

  TRACTION_CONTROLLER_CONFIG          = $20;
  TRACTION_CONTROLLER_CONFIG_ASSIGN   = $01;
  TRACTION_CONTROLLER_CONFIG_RELEASE  = $02;
  TRACTION_CONTROLLER_CONFIG_QUERY    = $03;
  TRACTION_CONTROLLER_CONFIG_NOTIFY   = $04;
  TRACTION_CONTROLLER_CONFIG_REPLY_OK = $00;
  TRACTION_CONTROLLER_CONFIG_REPLY_FAIL = $FF;

  TRACTION_CONSIST                    = $30;
  TRACTION_CONSIST_ATTACH             = $01;
  TRACTION_CONSIST_DETACH             = $02;
  TRACTION_CONSIST_QUERY              = $03;

  TRACTION_MANAGE                     = $40;
  TRACTION_MANAGE_RESERVE             = $01;
  TRACTION_MANAGE_RELEASE             = $02;

  TRACTION_MANAGE_RESERVE_REPLY_OK   = $00;    // Failed is not 0
  TRACTION_MANAGE_RESERVE_REPLY_FAIL = $FF;    // Failed

  MAX_DATAGRAM_LENGTH = 72;
  MAX_CONFIG_MEM_READWRITE_SIZE = 64;

  MAX_MULTIFRAME_LEN = 12;

  DATAGRAM_REJECTED                        = $0000;
  DATAGRAM_REJECTED_PERMANENT_ERROR        = $1000;
  DATAGRAM_REJECTED_INFORMATION_LOGGED     = $1010;
  DATAGRAM_REJECTED_SOURCE_NOT_PERMITTED   = $1020;
  DATAGRAM_REJECTED_DATAGRAMS_NOT_ACCEPTED = $1040;
  DATAGRAM_REJECTED_BUFFER_FULL            = $2000;
  DATAGRAM_REJECTED_OUT_OF_ORDER           = $6000;
  DATAGRAM_REJECTED_NO_RESEND_MASK         = $1000;
  DATAGRAM_REJECTED_RESEND_MASK            = $2000;
  DATAGRAM_REJECTED_TRANSPORT_ERROR_MASK   = $4000;

  DATAGRAM_PROTOCOL_LOGREQUEST             = $01;
  DATAGRAM_PROTOCOL_LOGREPLY               = $02;
  DATAGRAM_PROTOCOL_CONFIGURATION          = $20;
  DATAGRAM_PROTOCOL_REMOTEBUTTON           = $21;
  DATAGRAM_PROTOCOL_DISPLAY                = $28;
  DATAGRAM_PROTOCOL_TRAINCONTROL           = $30;
  DATAGRAM_PROTOCOL_TWOBYTE                = $E0;
  DATAGRAM_PROTOCOL_SIXBYTE                = $F0;

  MAX_EVENT_LEN                            = 8;

type
  TDatagramArray = array[0..MAX_DATAGRAM_LENGTH-1] of Byte;
  PDatagramArray = ^TDatagramArray;

  TStreamArray = array of Byte;

  TEventID = array[0..MAX_EVENT_LEN-1] of Byte;
  PEventID = ^TEventID;

  THexArray = TEventID;

  TMultiFrameArray = array[0..MAX_MULTIFRAME_LEN-1] of Byte;
  PMultiFrameArray = ^TMultiFrameArray;

const
  NULL_EVENT_ID : TEventID = (0, 0, 0, 0, 0, 0, 0, 0);
  EVENT_EMERGENCY_STOP       : TEventID = ($01, $00, $00, $00, $00, $00, $FF, $FF);
  EVENT_NEW_LOG_ENTRY        : TEventID = ($01, $00, $00, $00, $00, $00, $FF, $F8);
  EVENT_IDENT_BUTTON_PRESSED : TEventID = ($01, $00, $00, $00, $00, $00, $FE, $00);
  EVENT_DUPLICATE_ID_DETECTED: TEventID = ($01, $10, $00, $00, $00, $00, $02, $01);
  EVENT_IS_TRAIN             : TEventID = ($01, $01, $00, $00, $00, $00, $03, $03);
  EVENT_IS_PROXY             : TEventID = ($01, $01, $00, $00, $00, $00, $03, $04);
  EVENT_DELIVERS_CLOCK       : TEventID = ($01, $01, $00, $00, $00, $00, $05, $01);

implementation

end.

