unit opstackdefines;

{$I Options.inc}

interface

const
  MAX_BUS_LOGIN_TIMEOUT = 5;                                                    // Number of 100ms time tick to wait for a node to send a RID to signal a duplicate Alais

// Various Statemachine defines
const
  STATE_NODE_START                       = 0;
  STATE_NODE_GENERATE_NODE_ALIAS         = 1;
  STATE_RANDOM_NUMBER_GENERATOR             = 2;
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
  MAX_CAN_BYTES = 8;
  MAX_SNIP_BYTES = 64;
  MAX_DATAGRAM_BYTES = 64;
  MAX_STREAM_BYTES = 1024;

type
  TNodeID = array[0..1] of DWORD;                                               // WARNING READ THIS:::::   The Bottom 3 Bytes = [0] and the Top 3 Bytes = [1] The ID is not continious across the both DWords the upper nibble of the bottom DWord is not used
  TCANDataArray = array[0..MAX_CAN_BYTES-1] of Byte;
  TSNIPDataArray = array[0..MAX_SNIP_BYTES-1] of Byte;
  TDatagramDataArray = array[0..MAX_DATAGRAM_BYTES-1] of Byte;
  TStreamDataArray = array[0..MAX_STREAM_BYTES-1] of Byte;
  TArray = array[0..0] of Byte;

  TArrayData = record
    Count: Word;
    AnArray: TArray;
  end;
  PArrayData = ^TArrayData;

  TCANData = record
    Count: Word;
    Bytes: TCANDataArray;
  end;

  TSNIPData = record
    Count: Word;
    Bytes: TSNIPDataArray;
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

type
  TEventID = array[0..7] of Byte;                            // Is the 48 Bit node ID + 16 Bits of unique Event ID = 64 Bits

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
  MT_BASIC           = 1;                                                       // Message Type Identifiers
  MT_PROTCOLSUPPORT  = 2;
  MT_EVENT           = 3;
  MT_TRACTION        = 4;
  MT_REMOTEBUTTON    = 5;
  MT_SNIP            = 6;
  MT_DATATGRAM       = 7;
  MT_STREAM          = 8;
  MT_CAN             = 10;

type
  PMessage = ^Byte;

type
  TBaseMessage = record                                                         // Used as the "base class" for all the message records, allows this class to be overlayed the other to fake inheritance
    MessageType: Byte;                                                          // MT_xxx Constant the identifies the type of message
    Next: PMessage;
    MTI: DWord;
    SourceNodeID: TNodeID;                                                      // NodeID if the MTI carries it
    DestNodeID: TNodeID;
  end;
  PBaseMessage = ^TBaseMessage;

  // These messages types are the interface between the Physical Layer and the OPStack Library
  TNMRAnetCANLayerMessage = record                                              // Only called if the interface is CAN
    MessageType: Byte;                                                          // MT_xxx Constant the identifies the type of message
    Next: PMessage;
    MTI: DWord;
    SourceNodeID: TNodeID;                                                      // NodeID if the MTI carries it
    DestNodeID: TNodeID;                                                        // Not needed but allows common code for initializing and loading buffers
  end;
  PNMRAnetCANLayerMessage = ^TNMRAnetCANLayerMessage;

  TNMRAnetBasicMessage = record
    MessageType: Byte;                                                          // MT_xxx Constant the identifies the type of message
    Next: PMessage;
    MTI: DWord;
    SourceNodeID,
    DestNodeID: TNodeID;
    CANData: TCANData;
  end;
  PNMRAnetBasicMessage = ^TNMRAnetBasicMessage;

  TNMRAnetProtocolSupportMessage = record
    MessageType: Byte;                                                          // MT_xxx Constant the identifies the type of message
    Next: PMessage;
    MTI: DWord;
    SourceNodeID,
    DestNodeID: TNodeID;
    Protocols: DWord;
  end;
  PNMRAnetProtocolSupportMessage = ^TNMRAnetProtocolSupportMessage;

  TNMRAnetEventMessage = record
    MTI: DWord;
    SourceNodeID,
    DestNodeID: TNodeID;
    EventID: TEventID;
  end;
  PNMRAnetEventMessage = ^TNMRAnetEventMessage;

  TNMRAnetTractionMessage = record
    MessageType: Byte;                                                          // MT_xxx Constant the identifies the type of message
    Next: PMessage;
    MTI: DWord;
    SourceNodeID,
    DestNodeID: TNodeID;
    CANData: TCANData;
  end;
  PNMRAnetTractionMessage = ^TNMRAnetTractionMessage;

  TNMRAnetRemoteButtonMessage = record
    MessageType: Byte;                                                          // MT_xxx Constant the identifies the type of message
    Next: PMessage;
    MTI: DWord;
    SourceNodeID,
    DestNodeID: TNodeID;
    CANData: TCANData;
  end;
  PNMRAnetRemoteButtonMessage = ^TNMRAnetRemoteButtonMessage;

  TNMRAnetSNIPMessage = record
    MessageType: Byte;                                                          // MT_xxx Constant the identifies the type of message
    Next: PMessage;
    MTI: DWord;
    SourceNodeID,
    DestNodeID: TNodeID;
    SNIPData: TSNIPData;
  end;
  PNMRAnetSNIPMessage = ^TNMRAnetSNIPMessage;

  TNMRAnetDatagramMessage = record
    MessageType: Byte;                                                          // MT_xxx Constant the identifies the type of message
    Next: PMessage;
    MTI: DWord;
    SourceNodeID,
    DestNodeID: TNodeID;
    DatagramData: TDatagramData;
  end;
  PNMRAnetDatagramMessage = ^TNMRAnetDatagramMessage;

  TNMRAnetStreamMessage = record
    MessageType: Byte;                                                          // MT_xxx Constant the identifies the type of message
    Next: PMessage;
    MTI: DWord;
    SourceNodeID,
    DestNodeID: TNodeID;
    StreamData: TStreamData;
  end;
  PNMRAnetStreamMessage = ^TNMRAnetStreamMessage;

implementation

end.

