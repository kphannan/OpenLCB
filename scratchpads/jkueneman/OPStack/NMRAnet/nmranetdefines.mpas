unit nmranetdefines;

{$IFDEF FPC}
interface
{$ENDIF}

uses
  opstacktypes;

const
  MTI_OLCB_MSG                              = $08000000;                        //

  MTI_CAN_CID0                              = $7000;                            // First 12 Bits of 48 bit Node ID
  MTI_CAN_CID1                              = $6000;                            // 2rd 12 Bits of 48 bit Node ID
  MTI_CAN_CID2                              = $5000;                            // 3nd 12 Bits of 48 bit Node ID
  MTI_CAN_CID3                              = $4000;                            // Last 12 Bits of 48 bit Node ID
  MTI_CAN_CID4                              = $3000;                            // non-OpenLCB Protocol
  MTI_CAN_CID5                              = $2000;                            // non-OpenLCB Protocol
  MTI_CAN_CID6                              = $1000;                            // non-OpenLCB Protocol
  MTI_CAN_CID_MASK                          = $7000;

  MTI_CAN_RID                               = $0700;                            // Reserve ID
  MTI_CAN_AMD                               = $0701;                            // Alias Map Definition
  MTI_CAN_AME                               = $0702;                            // Alias Mapping Enquiry
  MTI_CAN_AMR                               = $0703;                            // Alias Map Reset Frame

  MTI_MASK                              = $FFFF;
  MTI_FRAME_TYPE_MASK                   = $F000;
  MTI_FRAME_TYPE_CAN_GENERAL            = $9000;
  MASK_SOURCE_ALIAS                     = $00000FFF;                            // Masks out just the Source Alias Address


  MTI_ADDRESSED_MASK                 = $0008;
  MTI_SIMPLE_PROTOCOL_MASK           = $0010;
  MTI_EVENT_PRESENT_MASK             = $0002;

  MTI_INITIALIZATION_COMPLETE        = $0100;                                // Databytes = Full Node ID
  MTI_VERIFY_NODE_ID_NUMBER_DEST     = $0488;                                // Databytes = Destination Alias
  MTI_VERIFY_NODE_ID_NUMBER          = $0490;                                //
  MTI_VERIFIED_NODE_ID_NUMBER        = $0170;                                // {Optional Full Node ID}
  MTI_OPTIONAL_INTERACTION_REJECTED  = $0068;                                // Databytes = Destination Alias, Error, {Optional Info}
  MTI_TERMINATE_DUE_TO_ERROR         = $00A8;                                // Databytes = Destination Alias, Error, {Optional Info}
  MTI_PROTOCOL_SUPPORT_INQUIRY       = $0828;                                // Databytes = Destination Alias
  MTI_PROTOCOL_SUPPORT_REPLY         = $0668;                                // Databytes = Destination Alias, Protocol Flags
  MTI_CONSUMER_IDENTIFY              = $08F4;                                // Databytes = EventID
  MTI_CONSUMER_IDENTIFIED_RANGE      = $04A4;                                // Databytes = EventID with Mask
  MTI_CONSUMER_IDENTIFIED_UNKNOWN    = $04C7;                                // Databytes = EventID
  MTI_CONSUMER_IDENTIFIED_SET        = $04C4;                                // Databytes = EventID
  MTI_CONSUMER_IDENTIFIED_CLEAR      = $04C5;                                // Databytes = EventID
  MTI_CONSUMER_IDENTIFIED_RESERVED   = $04C6;                                // Databytes = EventID
  MTI_PRODUCER_IDENTIFY              = $0914;                                // Databytes = EventID
  MTI_PRODUCER_IDENTIFIED_RANGE      = $0524;                                // Databytes = EventID with Mask
  MTI_PRODUCER_IDENTIFIED_UNKNOWN    = $0547;                                // Databytes = EventID
  MTI_PRODUCER_IDENTIFIED_SET        = $0544;                                // Databytes = EventID
  MTI_PRODUCER_IDENTIFIED_CLEAR      = $0545;                                // Databytes = EventID
  MTI_PRODUCER_IDENTIFIED_RESERVED   = $0546;                                // Databytes = EventID
  MTI_EVENTS_IDENTIFY_DEST           = $0968;                                // Databytes = Destination Alias
  MTI_EVENTS_IDENTIFY                = $0970;                                //
  MTI_EVENT_LEARN                    = $0594;                                // Databytes = EventID
  MTI_PC_EVENT_REPORT                = $05B4;                                // Databytes = EventID  (Infamouse PCER)
  MTI_TRACTION_PROTOCOL              = $05EA;                                // Databytes = Protocol (Train Protocol, DCC, etc), Operation, control
  MTI_TRACTION_REPLY                 = $05E8;                                // Databytes = Reply data/information
  MTI_TRACTION_PROXY_PROTOCOL        = $01EA;                                // Databytes = Protocol (Train Protocol, DCC, etc), Operation, control
  MTI_TRACTION_PROXY_REPLY           = $01E8;                                // Databytes = Reply data/information
  MTI_XPRESSNET                      = $0820;
  MTI_REMOTE_BUTTON_REQUEST          = $0948;
  MTI_REMOTE_BUTTON_REPLY            = $0949;
  MTI_SIMPLE_TRAIN_NODE_INFO_REQUEST = $0DA8;
  MTI_SIMPLE_TRAIN_NODE_INFO_REPLY   = $09C8;
  MTI_SIMPLE_NODE_INFO_REQUEST       = $0DE8;                                // Databytes = Destination Alias
  MTI_SIMPLE_NODE_INFO_REPLY         = $0A08;                                // Databytes = Destination Alias, ACDI Data
  MTI_DATAGRAM                       = $1C48;
  MTI_FRAME_TYPE_CAN_DATAGRAM_ONLY_FRAME = $A000;
  MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME_START= $B000;
  MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME      = $C000;
  MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME_END  = $D000;
  MTI_DATAGRAM_OK_REPLY              = $0A28;                                // Databytes = Destination Alias
  MTI_DATAGRAM_REJECTED_REPLY        = $0A48;
  MTI_STREAM_INIT_REQUEST            = $0CC8;
  MTI_STREAM_INIT_REPLY              = $0868;
  MTI_FRAME_TYPE_CAN_STREAM_SEND     = $F000;
  MTI_STEAM_SEND                     = $1F88;
  MTI_STREAM_PROCEED                 = $0888;
  MTI_STREAM_COMPLETE                = $08A8;

const
  LEN_PIV_PROTOCOL = 6;                                                         // 6 Bytes long

type
  TPIVProtocolValueArray = array[0..LEN_PIV_PROTOCOL-1] of Byte;

const
  PIV_PROTOCOL_ID_PROTOCOL: TPIVProtocolValueArray           = ($80, $00, $00, $00, $00, $00);
  PIV_PROTOCOL_ID_DATAGRAM: TPIVProtocolValueArray           = ($40, $00, $00, $00, $00, $00);
  PIV_PROTOCOL_ID_STREAM: TPIVProtocolValueArray             = ($20, $00, $00, $00, $00, $00);
  PIV_PROTOCOL_ID_CONFIGURATION: TPIVProtocolValueArray      = ($10, $00, $00, $00, $00, $00);
  PIV_PROTOCOL_ID_RESERVATION: TPIVProtocolValueArray        = ($08, $00, $00, $00, $00, $00);
  PIV_PROTOCOL_ID_PRODUCER_CONSUMER: TPIVProtocolValueArray  = ($04, $00, $00, $00, $00, $00);
  PIV_PROTOCOL_ID_IDENTIFICATION: TPIVProtocolValueArray     = ($02, $00, $00, $00, $00, $00);
  PIV_PROTOCOL_ID_TEACH_LEARN: TPIVProtocolValueArray        = ($01, $00, $00, $00, $00, $00);
  PIV_PROTOCOL_ID_REMOTE_BUTTON: TPIVProtocolValueArray      = ($00, $80, $00, $00, $00, $00);
  PIV_PROTOCOL_ID_CDI: TPIVProtocolValueArray                = ($00, $40, $00, $00, $00, $00);
  PIV_PROTOCOL_ID_DISPLAY: TPIVProtocolValueArray            = ($00, $20, $00, $00, $00, $00);
  PIV_PROTOCOL_ID_TRACTION: TPIVProtocolValueArray           = ($00, $04, $00, $00, $00, $00);
  PIV_PROTOCOL_ID_FDI: TPIVProtocolValueArray                = ($00, $02, $00, $00, $00, $00);
  PIV_PROTOCOL_ID_TRACTION_PROXY: TPIVProtocolValueArray     = ($00, $01, $00, $00, $00, $00);

const
  DATAGRAM_TYPE_MEMORY_CONFIGURATION        = $20;                              // Memory Configuration Protocol
//  DATAGRAM_TYPE_BOOTLOADER                  = $40;                              // MAKING A GUESS ON A NUMBER>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  DATAGRAM_TYPE_TWO_BYTE_CMD_MASK           = $E0;                              // Next two bytes are command bytes and not data
  DATAGRAM_TYPE_SIX_BYTE_CMD_MASK           = $F0;                              // Next six bytes are command bytes and not data


type
  TDatagramErrorCode = array[0..1] of Byte;
  PDatagramErrorCode = ^TDatagramErrorCode;

const
  DATAGRAM_RESULT_OKAY                                    : TDatagramErrorCode = ($00, $00);
  // Errors that will cause sender to not retry
  DATAGRAM_RESULT_REJECTED_PERMANENT_ERROR                : TDatagramErrorCode = ($10, $00);
  DATAGRAM_RESULT_REJECTED_INFORMATION_LOGGED             : TDatagramErrorCode = ($10, $10);
  DATAGRAM_RESULT_REJECTED_SOURCE_NOT_PERMITTED           : TDatagramErrorCode = ($10, $20);
  DATAGRAM_RESULT_REJECTED_SOURCE_DATAGRAMS_NOT_ACCEPTED  : TDatagramErrorCode = ($10, $40);
  // Error that should cause sender to retry
  DATAGRAM_RESULT_REJECTED_BUFFER_FULL                    : TDatagramErrorCode = ($20, $00);
  // Error that was a transport problem
  DATAGRAM_RESULT_REJECTED_OUT_OF_ORDER                   : TDatagramErrorCode = ($60, $00);

  DATAGRAM_RESULT_REJECTED_NO_RESEND_MASK                 = $1000;
  DATAGRAM_RESULT_REJECTED_RESEND_MASK                    = $2000;
  DATAGRAM_RESULT_REJECTED_TRANSPORT_ERROR_MASK           = $4000;

  DATAGRAM_OK_ACK_REPLY_PENDING                           = $80;


  STREAM_REPLY_CONTENT_TYPE                               = $01;                // LSB = 1 = first 6 bytes in data are UID of data type in stream
  STREAM_REPLY_UNEXPECTED_ERROR                           = $02;                // Bit 2 = 1 the Stream was Rejected, Out of order, or other "should not happen" error
  STREAM_REPLY_PERMANENT_ERROR                            = $40;                // Bit 6 = 1 = if STREAM_REPLY_ACCEPT = 1 then this is the error type where 1 = permanent
  STREAM_REPLY_ACCEPT                                     = $80;                // MSB = 1 = Accept

  STREAM_REPLY_ERROR_LOGGED                               = $01;                // Error was logged
  STREAM_REPLY_INVALID_REQUEST                            = $20;                // if Error is permanent then these are the possible reasons
  STREAM_REPLY_SOURCE_NOT_PERMITTED                       = $40;                // if Error is permanent then these are the possible reasons
  STREAM_REPLY_STREAM_NOT_ACCEPTED                        = $80;                // if Error is permanent then these are the possible reasons

  STREAM_REPLY_BUFFER_FULL                                = $20;                // if Error is permanent then these are the possible reasons
  STREAM_REPLY_INTERNAL_ERROR                             = $40;                // if Error is not permanent then these are the possible reasons

const
  MSI_CDI                            = $FF;                                     // MemorySpaceIdentifier - Access the Configuration Definition Infomation (CDI)
  MSI_ALL                            = $FE;                                     // MemorySpaceIdentifier - Access All memory (define all in the application)
  MSI_CONFIG                         = $FD;                                     // MemorySpaceIdentifier - Access basic configuration memory that feeds into the CDI
  MSI_ACDI_MFG                       = $FC;                                     // MemorySpaceIdentifier - Access the ACDI Manfacturers Info
  MSI_ACDI_USER                      = $FB;                                     // MemorySpaceIdentifier - Access the ACDI User definable Info
  MSI_FDI                            = $FA;                                     // MemorySpaceIdentifier - Access the Function Definition Information (FDI)

  MCP_CDI                             = $03;                                    // Address space = CDI ($FF) access Mask
  MCP_ALL                             = $02;                                    // Address space = All ($FE) access Mask
  MCP_CONFIGURATION                   = $01;                                    // Address space = Basic Configuration ($FD) access Mask
  MCP_NONE                            = $00;                                    // Use the optional {Space} byte in the datagram to defin the address space

  MCP_COMMAND_MASK                    = $C0;                                    // Upper 2 bits are the command type

  // Upper 2 bits 0 = Write Command
  MCP_COMMAND_WRITE                   = $00;                                    // MemoryConfigurationProtocol - Write Memory Mask
  MCP_COMMAND_WRITE_STREAM            = $20;                                    // MemoryConfigurationProtocol - Write Memory Mask with Streams
  MCP_COMMAND_WRITE_UNDER_MASK        = $08;                                    // MemoryConfigurationProtocol - Write Memory under mask Mask
  MCP_COMMAND_WRITE_REPLY_OK          = $10;
  MCP_COMMAND_WRITE_REPLY_FAIL        = $18;

  // Upper 2 bits 1 = Read Command
  MCP_COMMAND_READ                    = $40;                                    // MemoryConfigurationProtocol - Read Memory Mask
  MCP_COMMAND_READ_STREAM             = $60;                                    // MemoryConfigurationProtocol - Read Memory with Streams
  MCP_COMMAND_READ_REPLY_OK           = $50;                                    // MemoryConfigurationProtocol - Read Reply Mask [Does not include the Address Space Mask "or" it with the the Address space masks below]
  MCP_COMMAND_READ_REPLY_FAIL         = $58;
  MCP_COMMAND_READ_STREAM_REPLY       = $30;

  // Upper 2 bits 2 = Operation Command
  MCP_OPERATION                       = $80;                                    // MemoryConfigurationProtocol - Operation Mask

  // Bit 0..2 are to define address space
  MCP_COMMAND_REPLY_FAIL              = 3;                                      // Bit 3 set mean the result is a failed result if it is a Command Reply
  MCP_COMMAND_WRITE_UNDER_MASK_BIT    = 3;                                      // Bit 3 set means write under mask for an in coming command
  MCP_REPLY_COMMAND_BIT               = 4;                                      // Bit 4 set means it is a reply to a Command
  MCP_STREAM_COMMAND_BIT              = 5;                                      // Bit 5 set means it is a Stream Command
  // Bit 6..7 define the Command

  MCP_OP_GET_CONFIG                  = $80;                                     // MemoryConfigurationProtocol Operation - Get Configuration
  MCP_OP_GET_CONFIG_REPLY            = $82;                                     // MemoryConfigurationProtocol Operation - Get Configuration Reply
  MCP_OP_GET_ADD_SPACE_INFO          = $84;                                     // MemoryConfigurationProtocol Operation - Get Add Space Info
  MCP_OP_GET_ADD_SPACE_INFO_REPLY    = $86;                                     // MemoryConfigurationProtocol Operation - Get Add Space Info Reply
  MCP_OP_LOCK                        = $88;                                     // MemoryConfigurationProtocol Operation - Lock Node
  MCP_OP_LOCK_REPLY                  = $8A;                                     // MemoryConfigurationProtocol Operation - Lock Node Reply
  MCP_OP_GET_UNIQUEID                = $8C;                                     // MemoryConfiguratio                    nProtocol Operation - Get Unique ID Key
  MCP_OP_GET_UNIQUEID_REPLY          = $8E;                                     // MemoryConfigurationProtocol Operation - Get Unique ID Key Reply

  MCP_OP_GET_ADD_SPACE_INFO_REPLY_PRESENT = $01;

  MCP_OP_FREEZE                      = $A0;                                     // MemoryConfigurationProtocol Operation - Freeze Node
  MCP_OP_INDICATE                    = $A4;                                     // MemoryConfigurationProtocol Operation - Indicate
  MCP_OP_UPDATE_COMPLETE             = $A8;                                     // MemoryConfigurationProtocol Operation - Update Complete
  MCP_OP_RESETS                      = $A9;                                     // MemoryConfigurationProtocol Operation - Resets

const
  NULL_EVENT_ID              : TEventID = (0, 0, 0, 0, 0, 0, 0, 0);
  EVENT_EMERGENCY_STOP       : TEventID = ($01, $00, $00, $00, $00, $00, $FF, $FF);
  EVENT_NEW_LOG_ENTRY        : TEventID = ($01, $00, $00, $00, $00, $00, $FF, $F8);
  EVENT_IDENT_BUTTON_PRESSED : TEventID = ($01, $00, $00, $00, $00, $00, $FE, $00);
  EVENT_DUPLICATE_ID_DETECTED: TEventID = ($01, $10, $00, $00, $00, $00, $02, $01);
  EVENT_IS_TRAIN             : TEventID = ($01, $01, $00, $00, $00, $00, $03, $03);
  EVENT_IS_PROXY             : TEventID = ($01, $01, $00, $00, $00, $00, $03, $04);
  EVENT_DELIVERS_CLOCK       : TEventID = ($01, $01, $00, $00, $00, $00, $05, $01);

const
  TS_WAITING_FOR_CONTROLLER_NOTIFY   = $01;
  TS_SEND_PROXY_ALLOCATE_REPLY       = $04;


  TRACTION_PROXY_ALLOCATE            = $01;
  TRACTION_PROXY_ATTACH              = $02;
  TRACTION_PROXY_ATTACH_REPLY        = $02;
  TRACTION_PROXY_DETACH              = $03;
  TRACTION_PROXY_MANAGE              = $80;
  TRACTION_PROXY_MANAGE_REPLY        = $80;
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

  TRACTION_CONSIST                    = $30;
  TRACTION_CONSIST_ATTACH             = $01;
  TRACTION_CONSIST_DETACH             = $02;
  TRACTION_CONSIST_QUERY              = $03;

  TRACTION_MANAGE                     = $40;
  TRACTION_MANAGE_RESERVE             = $01;
  TRACTION_MANAGE_RELEASE             = $02;

  TRACTION_MANAGE_RESERVE_REPLY_OK   = $00;    // Failed is not 0
  TRACTION_MANAGE_RESERVE_REPLY_FAIL = $FF;    // Failed

  TRACTION_CONTROLLER_ASSIGN_REPLY_OK   = $00;    // Failed is not 0
  TRACTION_CONTROLLER_ASSIGN_REPLY_FAIL = $FF;    // Failed is not 0

  TRACTION_CONTROLLER_NOTIFY_REPLY_OK   = $00;    // Failed is not 0
  TRACTION_CONTROLLER_NOTIFY_REPLY_FAIL = $FF;    // Failed is not 0

  TRACTION_CONTROLLER_ASSIGN_REPLY_FAIL_CONTROLLER_DENIED = $01;
  TRACTION_CONTROLLER_ASSIGN_REPLY_FAIL_TRAIN_DENIED      = $02;

  TRACTION_MANAGE_QUERY_REPLY        = $03;


implementation

end.
