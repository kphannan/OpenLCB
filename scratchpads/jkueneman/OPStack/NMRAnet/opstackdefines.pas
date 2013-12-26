unit opstackdefines;



{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}


uses
  {$IFDEF HARDWARE_TEMPLATE}
  template_node,
  template_vnode,
  {$ENDIF}
  opstacktypes,
  template_buffers;

const
  MAX_BUS_LOGIN_TIMEOUT = 5;                                                    // Number of 100ms time tick to wait for a node to send a RID to signal a duplicate Alais

// Various Statemachine defines
const
  STATE_NODE_START                       = 0;
  STATE_NODE_GENERATE_NODE_ALIAS         = 1;
  STATE_RANDOM_NUMBER_GENERATOR          = 2;
  STATE_NODE_TRANSMIT_CID                = 3;
  STATE_NODE_NEXT_CDI                    = 4;
  STATE_NODE_WAITSTATE                   = 5;
  STATE_NODE_SEND_LOGIN_RID              = 6;

  STATE_NODE_SEND_LOGIN_AMD              = 8;
  STATE_NODE_INITIALIZED                 = 9;
  STATE_NODE_LOGIN_IDENTIFY_EVENTS       = 10;
  STATE_NODE_PERMITTED                   = 11;
  STATE_NODE_INHIBITED                   = 12;
  STATE_NODE_DUPLICATE_FULL_ID           = 13;
  STATE_NODE_TAKE_OFFLINE                = 14;
  STATE_NODE_OFFLINE                     = 15;


// Nodes
const
  MAX_SIMPLE_BYTES = 8;
  MAX_SNIP_BYTES = 64;
  MAX_DATAGRAM_BYTES = 72;             // 64 Data bytes + 8 Bytes for DG Header

type
  TNodeID = array[0..1] of DWORD;                                               // WARNING READ THIS:::::   The Bottom 3 Bytes = [0] and the Top 3 Bytes = [1] The ID is not continious across the both DWords the upper nibble of the bottom DWord is not used

  // = array[0..0] of Byte;
 // PDataArray = ^TDataArray;
  TSimpleDataArray = array[0..MAX_SIMPLE_BYTES-1] of Byte;
  PSimpleDataArray = ^TSimpleDataArray;
  TDatagramDataArray = array[0..MAX_DATAGRAM_BYTES-1] of Byte;
  PDatagramDataArray = ^TDatagramDataArray;
  TStreamDataArray = array[0..USER_MAX_STREAM_BYTES-1] of Byte;
  PStreamDataArray = ^TStreamDataArray;
  TAcdiSnipDataArray = array[0..USER_MAX_ACDI_SNIP_BYTES] of Byte;
  PAcdiSnipDataArray = ^TAcdiSnipDataArray;

  TSimpleData = record
    Count: Word;
    Bytes: TSimpleDataArray;
  end;

  TDatagramData = record
    Count: Word;
    Bytes: TDatagramDataArray;
  end;

  TStreamData = record
    Count: Word;
    Bytes: TStreamDataArray;
  end;

// Events
const
  NULL_NODE_ID: TNodeID = (0, 0);

const
  NULL_EVENT_ID : TEventID = (0, 0, 0, 0, 0, 0, 0, 0);
  EVENT_EMERGENCY_STOP_ALL: TEventID = ($01, $01, $00, $00, $00, $00, $FF, $FF);
  EVENT_LOG_ENTRY_RECORDED: TEventID = ($01 ,$10, $00, $00, $00, $00, $FF, $F8);
  EVENT_IDENT_BUTTON_PRESSED: TEventID = ($01, $10, $00, $00, $00, $00, $FE, $00);
  EVENT_DUPLICATE_ID_DETECTED: TEventID = ($01, $10, $00, $00, $00, $00, $02, $01);

const
  EVENT_STATE_CLEAR                 = $00;
  EVENT_STATE_VALID                 = $01;
  EVENT_STATE_INVALID               = $02;
  EVENT_STATE_UNKNOWN               = $03;

// Message types

const
  MT_MASK               = $0F;                                                     // Strips off the CAN and Allocated flags
  MT_UNALLOCATED        = $00;
  MT_SIMPLE             = $01;                                                     // Message Type Identifiers
  MT_DATAGRAM           = $02;
  MT_STREAM             = $04;
  MT_ACDISNIP           = $08;

  MT_HIGH_PRIORITY_SEND = $10;
  MT_SEND               = $20;                                                     // Set if the message should just be sent as is
  MT_CAN_TYPE           = $40;                                                     // It is a CAN MTI
  MT_ALLOCATED          = $80;                                                     // Buffer was allocated from the Pool, do not set this manually !!!!!


const
  // :X19170640N0501010107015555;#0  Example.....
  // ^         ^                  ^
  // 0         10                28
  MAX_GRID_CONNECT_LEN = 29;
  GRID_CONNECT_HEADER_OFFSET_HI = 2;
  GRID_CONNECT_HEADER_OFFSET_LO = 4;
  GRID_CONNECT_DATA_OFFSET = 11;

type
  TGridConnectString = array[0..MAX_GRID_CONNECT_LEN-1] of char;
  PGridConnectString = ^TGridConnectString;


// *****************************************************************************
// Node
// *****************************************************************************

const                                                                           // NodeState the node empty and ready to allocate
  NS_EMPTY                = $00;                                                // NodeState the node is not allocated
  NS_ALLOCATED            = $01;                                                // NodeState the node is allocated
  NS_PERMITTED            = $02;                                                // NodeState CAN Frame Layer is permitted (Node ID's resolved with bus)
  NS_INITIALIZED          = $04;                                                // NodeState Message Layer has sent its first Initialize Complete Message
  NS_VIRTUAL              = $08;                                                // NodeState If is a virtual node
  NS_RELEASING            = $10;                                                // Node is tagged to send and AMD and be removed from the bus (while this is set what happens??)

  // MsgFlags in order of precidence (= 0 highest precidence)
  MF_DUPLICATE_NODE_ID        = $0001;                                          // MsgFlag, a Duplicate Node ID was detected, critical fault
  MF_DUPLICATE_ALIAS          = $0002;                                          // MsgFlag, a Duplicate Alias was Detected, critical fault
  MF_DUPLICATE_ALIAS_RID      = $0004;                                          // MsgFlag, a Duplicate Alias was Detected during a CID message, not a fault just need to respond to claim the Alias
  MF_ALIAS_MAP_ENQUIRY        = $0008;                                          // MsgFlag, an AMD message need to be responded to
  MF_VERIFY_NODE_ID           = $0010;                                          // MsgFlag, a Verify Node ID message needs to be responded to

type
  // Each Byte contains the state of up to 4 Events, as each event can have 3 state (2 bits)
  // The Index of the 2 bits block is mapped to the index into the defined array of Event ID values
  TNodeEventArray = array[0..USER_MAX_EVENTS_BYTES] of Byte;                    // Holds the current state (set, clear, unknown) of each Event.  Used when Consumer/Producer Indentifed messages need to be sent
  // Each Byte contains the state of up to 8 Events, as each event can have 1 state (1 bits)
  // The Index of the 1 bits block is mapped to the index into the defined array of Event ID values
  TNodePCERArray  = array[0..USER_MAX_PCER_BYTES] of Byte;                      // Holds a flag for if an Event requires a PCER message to be sent becuase that event has changed state

  TNodeInfo = record
    ID: TNodeID;                                                                // Unique 48 Bit ID for Node
    AliasID: Word;                                                              // 12 Bit Alias ID
  end;

  TNMRAnetNodeLoginInfo = record
    TimeCounter: Byte;                                                          // Number of timer ticks into the time waiting for a RID response from another node for our RID broadcasts
    iCID: Byte;                                                                 // Which of the 4 CIDs we are broadcasting
    Seed: TNodeID;                                                              // Seed for Random Number Generator in case we have to reseed because of a duplicate ID
  end;

  TNodeEvents = record
    Produced,
    Consumed  : TNodeEventArray;
    PCER      : TNodePCERArray;
  end;

const
  ABS_ALLOCATED            = $01;                                               // Array Buffer State Flag = Allocated Buffer
  ABS_HASBEENACKED         = $02;                                               // Array Buffer State Flag = The received Datagram buffer has be ACK'ed
  ABS_STREAM_OUTGOING      = $04;                                               // Flag the direction of the stream (if the buffer is a stream)
  ABS_STREAM_TYPE_ID       = $08;                                               // Flag if the Stream Buffer contains a valid Stream Type ID UID

type
  TSimpleBuffer = record
    State: Byte;                                                                // See ABS_xxxx flags
    DataBufferSize: Word;                                                       // Number of bytes in the DataBuffer
    DataArray: TSimpleDataArray;
  end;
  PSimpleBuffer = ^TSimpleBuffer;

  TDatagramBuffer = record
    State: Byte;                                                                // See ABS_xxxx flags
    DataBufferSize: Word;                                                       // Number of bytes in the DataArray
    DataArray: TDatagramDataArray;
    // *******
    CurrentCount: Word;                                                         // Current index of the number of bytes sent/received
    ResendCount: Byte;                                                          // Number of tries to resend the datagram if sending is rejected
    NextWaitingForAck: PByte;                                                   // Pointer to the Next _Message_ (not Buffer) that is waiting for an Ack
  end;
  PDatagramBuffer = ^TDatagramBuffer;

  {$IFDEF SUPPORT_STREAMS}
const
  MAX_STREAM_TYPE_ID = 6;

type
  TStreamTypeID = array[0..MAX_STREAM_TYPE_ID-1] of Byte;
  PStreamTypeID = ^TStreamTypeID;

  TStreamBuffer = record
    State: Byte;                                                                // See ABS_xxxx flags
    DataBufferSize: Word;                                                       // Number of bytes in the DataArray that are valid, for streams this is the negotiated buffer size
    DataArray: TStreamDataArray;
    // *******
    SourceStreamID,
    DestStreamID: Byte;
    StreamTypeID: TStreamTypeID;
    CurrentCount: DWord;                                                         // Current index of the number of bytes sent/received
    TotalMessageSize: DWord;                                                    // The total number of bytes to send in the interaction
    NegotiatedBufferSize: Word;
    NextActiveStream: PByte;
  end;
  PStreamBuffer = ^TStreamBuffer;
  {$ENDIF}

  TAcdiSnipBuffer = record
    State: Byte;                                                                // See ABS_xxxx flags
    DataBufferSize: Word;                                                       // Number of bytes in the DataArray
    DataArray: TAcdiSnipDataArray;
    // *******
    CurrentCount: Word;                                                         // Current index of the number of bytes sent/received
  end;
  PAcdiSnipBuffer = ^TAcdiSnipBuffer;

type
  {$IFDEF FPC}
  POPStackMessage = ^TOPStackMessage;
  {$ENDIF}
  TOPStackMessage = record                                                       // Used as the "base class" for all the message records, allows this class to be overlayed the other to fake inheritance
    MessageType: Byte;                                                          // MT_xxx Constant the identifies the type of message, bottom 4 bits are the type of message u
    Source: TNodeInfo;
    Dest: TNodeInfo;
    DestFlags: Byte;                                                            // The upper 4 bits sent in the Destination (when used for the message)
    {$IFDEF FPC}
    NextIncoming: POPStackMessage;
    {$ELSE}
    NextIncoming: ^TOPStackMessage;
    {$ENDIF}
    MTI: Word;
    Buffer: PSimpleBuffer;                                                      // This can be nil, CANBuffer, Datagram Buffer, or StreamBuffer based on the lower 4 bits of MessageType
    WatchDog: Word;                                                             // Watches for a abandon message, incremented every 100ms
  end;
  {$IFNDEF FPC}
  POPStackMessage = ^TOPStackMessage;
  {$ENDIF}

type
  TNMRAnetNode = record
    iIndex: Byte;                                                               // Index in the main array
    State: Byte;                                                                // See the NS_xxxx flags; State of the Node
    Events: TNodeEvents;
    Info: TNodeInfo;                                                            // Information about a Node
    Login: TNMRAnetNodeLoginInfo;                                               // Login Information
    Flags: Word;                                                                // Message Flags for messages passed to the Node through a simple set bit (no complex reply data needed like destination Alias), see the MF_xxxx flags
    iStateMachine: Byte;                                                        // Statemachine index for the main bus login
    IncomingMessages: POPStackMessage;                                          // Linked List of Messages incoming to process for the node
    StateMachineMessages: POPStackMessage;                                      // Linked List of Messages that need to run statemachine to operate allocated for this node
  end;
  PNMRAnetNode = ^TNMRAnetNode;

const
  DATAGRAM_PROCESS_ERROR_OK                  = $00;
  DATAGRAM_PROCESS_ERROR_BUFFER_FULL         = $02;
  DATAGRAM_PROCESS_ERROR_OUT_OF_ORDER        = $03;
  DATAGRAM_PROCESS_ERROR_SOURCE_NOT_ACCEPTED = $04;
  DATAGRAM_PROCESS_ERROR_QUIET_FAIL          = $05;

  STATE_CONFIG_MEM_STREAM_START                    = 0;
  STATE_CONFIG_MEM_STREAM_INIT                     = 1;
  STATE_CONFIG_MEM_STREAM_WAIT_FOR_INIT_REPLY      = 2;
  STATE_CONFIG_MEM_STREAM_SEND                     = 3;
  STATE_CONFIG_MEM_STREAM_WAIT_FOR_PROCEED         = 4;
  STATE_CONFIG_MEM_STREAM_SEND_COMPLETE            = 5;
  STATE_CONFIG_MEM_STREAM_COMPLETE                 = 6;

type
  TNMRAnetCanBuffer = record
    MTI: DWord;
    Payload: array[0..7] of byte;
    PayloadCount: Byte;
  end;
  PNMRAnetCanBuffer = ^TNMRAnetCanBuffer;


implementation

end.
