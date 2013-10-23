unit opstackbuffers;

interface

uses
  opstackdefines,
  template_buffers;

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


procedure OPStack_Initialize;
function OPStack_AllocateCANArray: PCANDataArray;
function OPStack_AllocateDatagramArray: PDatagramDataArray;
function OPStack_AllocateSteamArray: PStreamDataArray;

implementation

type
  TCANArrayPool = record
    Pool: array[0..USER_MAX_CAN_ARRAY_BUFFERS] of TCANDataArray;
    Count: Word;
  end;

  TDatagramArrayPool = record
    Pool: array[0..USER_MAX_DATAGRAM_ARRAY_BUFFERS] of TDatagramDataArray;
    Count: Word;
  end;

  TStreamArrayPool = record
    Pool: array[0..USER_MAX_STREAM_ARRAY_BUFFERS] of TStreamDataArray;
    Count: Word;
  end;

var
  CANArrayPool: TCANArrayPool;
  DatagramArrayPool: TDatagramArrayPool;
  StreamArrayPool: TStreamArrayPool;

procedure OPStack_Initialize;
begin

end;

function OPStack_AllocateCANArray: PCANDataArray;
begin
  Result := nil;
end;

function OPStack_AllocateDatagramArray: PDatagramDataArray;
begin
  Result := nil;
end;

function OPStack_AllocateSteamArray: PStreamDataArray;
begin
  Result := nil;
end;

end.

