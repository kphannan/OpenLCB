unit opstackdefines;

{$I Options.inc}

interface

uses
  template_buffers;

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

type
  TNodeID = array[0..1] of DWORD;                                               // WARNING READ THIS:::::   The Bottom 3 Bytes = [0] and the Top 3 Bytes = [1] The ID is not continious across the both DWords the upper nibble of the bottom DWord is not used
  TCANDataArray = array[0..MAX_CAN_BYTES-1] of Byte;
  PCANDataArray = ^TCANDataArray;
  TDatagramDataArray = array[0..MAX_DATAGRAM_BYTES-1] of Byte;
  PDatagramDataArray = ^TDatagramDataArray;
  TStreamDataArray = array[0..USER_MAX_STREAM_BYTES-1] of Byte;
  PStreamDataArray = ^TStreamDataArray;

  TCANData = record
    Count: Word;
    Bytes: TCANDataArray;
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


implementation

end.

