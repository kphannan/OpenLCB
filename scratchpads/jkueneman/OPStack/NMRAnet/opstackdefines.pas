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
  MAX_DATAGRAM_BYTES = 64;

type
  TNodeID = array[0..1] of DWORD;                                               // WARNING READ THIS:::::   The Bottom 3 Bytes = [0] and the Top 3 Bytes = [1] The ID is not continious across the both DWords the upper nibble of the bottom DWord is not used

  TDataArray = array[0..0] of Byte;
  PDataArray = ^TDataArray;
  TSimpleDataArray = array[0..MAX_SIMPLE_BYTES-1] of Byte;
  PSimpleDataArray = ^TSimpleDataArray;
  TDatagramDataArray = array[0..MAX_DATAGRAM_BYTES-1] of Byte;
  PDatagramDataArray = ^TDatagramDataArray;
  TStreamDataArray = array[0..USER_MAX_STREAM_BYTES-1] of Byte;
  PStreamDataArray = ^TStreamDataArray;

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
  EVENT_STATE_UNKOWN                = $03;

// Message types

const
  MT_UNALLOCATED     = $00;
  MT_SIMPLE          = $01;                                                       // Message Type Identifiers
  MT_DATAGRAM        = $02;
  MT_STREAM          = $04;
  MT_CAN_TYPE        = $40;                                                     // It is a CAN MTI
  MT_ALLOCATED       = $80;


const
  // :X19170640N0501010107015555;#0  Example.....
  // ^         ^                ^
  // 0         10               27
  MAX_GRID_CONNECT_LEN = 28;
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
  ABS_ALLOCATED   = $01;                                                        // Array Buffer State Flag = Allocated Buffer
  ABS_PROCESSING  = $02;                                                        // Array Buffer State Flag = The buffer is being used to transmit/receive and is not complete so don't use the data yet

type
  TBuffer = record
    State: Byte;                                                                // See ABS_xxxx flags
    iStateMachine: Byte;                                                        // Local Statemachine
    DataBufferSize: Word;                                                       // Number of bytes in the DataBuffer
    DataArray: TDataArray;
  end;
  PBUffer = ^TBuffer;

  TSimpleBuffer = record
    State: Byte;                                                                // See ABS_xxxx flags
    iStateMachine: Byte;                                                        // Local Statemachine
    DataBufferSize: Word;                                                       // Number of bytes in the DataBuffer
    DataArray: TSimpleDataArray;
  end;
  PSimpleBUffer = ^TSimpleBuffer;

  TDatagramBuffer = record
    State: Byte;                                                                // See ABS_xxxx flags
    iStateMachine: Byte;                                                        // Local Statemachine
    CurrentCount: Word;                                                         // Current index of the number of bytes sent/received
    DataBufferSize: Word;                                                       // Number of bytes in the DataArray
    DataArray: TDatagramDataArray;
  end;
  PDatagramBuffer = ^TDatagramBuffer;

  TStreamBuffer = record
    State: Byte;                                                                // See ABS_xxxx flags
    iStateMachine: Byte;                                                        // Local Statemachine
    CurrentCount: Word;                                                         // Current index of the number of bytes sent/received
    DataBufferSize: Word;                                                       // Number of bytes in the DataArray
    DataArray: TStreamDataArray;
  end;
  PStreamBuffer = ^TStreamBuffer;

type
  {$IFDEF FPC}
  PSimpleMessage = ^TSimpleMessage;
  {$ENDIF}
  TSimpleMessage = record                                                       // Used as the "base class" for all the message records, allows this class to be overlayed the other to fake inheritance
    MessageType: Byte;                                                          // MT_xxx Constant the identifies the type of message
    Source: TNodeInfo;
    Dest: TNodeInfo;
    {$IFDEF FPC}
    Next: PSimpleMessage;
    {$ELSE}
    Next: ^TSimpleMessage;
    {$ENDIF}
    MTI: Word;
    Buffer: PBuffer;
  end;

type
  TNMRAnetNode = record
    iIndex: Byte;                                                               // Index in the main array
    State: Byte;                                                                // See the NS_xxxx flags; State of the Node
    Events: TNodeEvents;
    Info: TNodeInfo;                                                            // Information about a Node
    Login: TNMRAnetNodeLoginInfo;                                               // Login Information
    Flags: Word;                                                                // Message Flags for messages passed to the Node through a simple set bit (no complex reply data needed like destination Alias), see the MF_xxxx flags
    iStateMachine: Byte;                                                        // Statemachine index for the main bus login
    Messages: PSimpleMessage;                                                   // Linked List of Message to process for the node

  //   BaseBuffers: PBaseBuffer;                                                   // Head of a possible linked list of dataless Messages Replies to service
 //   DatagramBuffers: PDatagramBuffer;                                           // Head of a possible linked list of Datagrams to service
 //   ConfigMemBuffers: PConfigMemBuffer;                                         // Head of a possible linked list of Configuration Memory Accesses to service
 //   DataBuffers: PDataBuffer;                                                   // Head of a possible linked list of Message Replies that need sent Data Bytes to be serviced
 //   StreamBuffers: PStreamBuffer;                                               // Head of a possible linked list of Streams to service
 //   ConfigurationAddress: Generic32BitPointer;                                  // Pointer into the EEProm Memory, user assigned in the NMRAnetAppCallbacks file
 //   ParentAlias,                                                                // Definition depends on what kind of node.  If a Throttle then Parent should never be set, If a Train then the will be the Owner Throttle
 //   ChildAlias,                                                                 // Definition depends on what kind of node.  If a Throttle then Child should the Train it is controlling, if a Train then should not be set
 //   LeftSibling,                                                                // Definition depends on what kind of node.  If Train then may be the next in a Consist Chain
 //   RightSibling: ^TNMRAnetNode;                                                // Definition depends on what kind of node.  If Train then may be the previous in a Consist Chain
 //   RAMAddress: Generic32BitPointer;                                            // Pointer to a DataStructure that is in Volatile RAM defined in the user defined NMRAnetAppCallbacks file, user assigned in the NMRAnetAppCallbacks file
  end;
  PNMRAnetNode = ^TNMRAnetNode;


implementation

end.
