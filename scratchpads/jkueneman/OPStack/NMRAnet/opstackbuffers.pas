unit opstackbuffers;

interface

uses
  opstackdefines,
  template_buffers;

type
  TSimpleMessage = record                                                       // Used as the "base class" for all the message records, allows this class to be overlayed the other to fake inheritance
    MessageType: Byte;                                                          // MT_xxx Constant the identifies the type of message
    Source: TNodeInfo;
    Dest: TNodeInfo;
    Next: PMessage;
    MTI: DWord;
    Data: PByte;
    DataLen: Word;
    MessageSpecificInfo: PMessage;
  end;
  PSimpleMessage = ^TSimpleMessage;


procedure OPStack_Initialize;

function OPStack_AllocateMessage(var AMessage: PSimpleMessage; MTI: DWord; Next: PMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID): Boolean;
function OPStack_AllocateCANMessage(var AMessage: PSimpleMessage; MTI: DWord; Next: PMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID): Boolean;
function OPStack_AllocateDatagramMessage(var AMessage: PSimpleMessage; MTI: DWord; Next: PMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID): Boolean;
function OPStack_AllcoateStreamMessage(var AMessage: PSimpleMessage; MTI: DWord; Next: PMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID): Boolean;


procedure OPStack_LoadBaseMessageBuffer(Message: PSimpleMessage; MessageType: Byte; MTI: DWord; Next: PMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID);

implementation


type
  TCANArrayPool = record
    Pool: array[0..USER_MAX_CAN_ARRAY_BUFFERS-1] of TCANDataArray;
    Count: Word;
  end;

  TDatagramArrayPool = record
    Pool: array[0..USER_MAX_DATAGRAM_ARRAY_BUFFERS-1] of TDatagramDataArray;
    Count: Word;
  end;

  TStreamArrayPool = record
    Pool: array[0..USER_MAX_STREAM_ARRAY_BUFFERS-1] of TStreamDataArray;
    Count: Word;
  end;

  TSimpleMessagePool = record
    Pool: array[0..USER_MAX_SIMPLE_MESSAGE_BUFFERS-1] of TSimpleMessage;
    Count: Word;
  end;

var
  CANArrayPool: TCANArrayPool;
  DatagramArrayPool: TDatagramArrayPool;
  StreamArrayPool: TStreamArrayPool;
  SimpleMessagePool: TSimpleMessagePool;

procedure OPStack_Initialize;
var
  i, j: Integer;
begin
  for j := 0 to USER_MAX_CAN_ARRAY_BUFFERS  do                                  // Extra Byte at end for state flags
    for i := 0 to MAX_CAN_BYTES - 1 do
       CANArrayPool.Pool[j][i] := 0;
  CANArrayPool.Count := 0;

  for j := 0 to USER_MAX_DATAGRAM_ARRAY_BUFFERS  do                             // Extra Byte at end for state flags
    for i := 0 to MAX_DATAGRAM_BYTES - 1 do
       DatagramArrayPool.Pool[j][i] := 0;
  DatagramArrayPool.Count := 0;

  for j := 0 to USER_MAX_STREAM_ARRAY_BUFFERS  do                               // Extra Byte at end for state flags
    for i := 0 to USER_MAX_STREAM_BYTES - 1 do
       StreamArrayPool.Pool[j][i] := 0;
  StreamArrayPool.Count := 0;

  for j := 0 to USER_MAX_SIMPLE_MESSAGE_BUFFERS  do                               // Extra Byte at end for state flags
    OPStack_LoadBaseMessageBuffer(@SimpleMessagePool.Pool[j], MT_UNALLOCATED, 0, nil, 0, NULL_NODE_ID, 0, NULL_NODE_ID);
  SimpleMessagePool.Count := 0;
end;

function AllocateCANArray(AnArray: PCANDataArray): Boolean;
var
  i: Integer;
begin
  Result := False;
  if CANArrayPool.Count < USER_MAX_CAN_ARRAY_BUFFERS then
  begin
    for i := 0 to USER_MAX_CAN_ARRAY_BUFFERS - 1 do
    begin
      if CANArrayPool.Pool[i][MAX_CAN_BYTES] and $80 = 0 then
      begin
        AnArray := @CANArrayPool.Pool[i];
        CANArrayPool.Pool[i][MAX_CAN_BYTES] := CANArrayPool.Pool[i][MAX_CAN_BYTES] or $80;
        Inc(CANArrayPool.Count);
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function AllocateDatagramArray(AnArray: PDatagramDataArray): Boolean;
var
  i: Integer;
begin
  Result := False;
  if DatagramArrayPool.Count < USER_MAX_DATAGRAM_ARRAY_BUFFERS then
  begin
    for i := 0 to USER_MAX_DATAGRAM_ARRAY_BUFFERS - 1 do
    begin
      if DatagramArrayPool.Pool[i][MAX_DATAGRAM_BYTES] and $80 = 0 then
      begin
        AnArray := @DatagramArrayPool.Pool[i];
        DatagramArrayPool.Pool[i][MAX_DATAGRAM_BYTES] := DatagramArrayPool.Pool[i][MAX_DATAGRAM_BYTES] or $80;
        Inc(DatagramArrayPool.Count);
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function AllocateSteamArray(AnArray: PStreamDataArray): Boolean;
var
  i: Integer;
begin
  Result := False;
  if StreamArrayPool.Count < USER_MAX_STREAM_ARRAY_BUFFERS then
  begin
    for i := 0 to USER_MAX_STREAM_ARRAY_BUFFERS - 1 do
    begin
      if StreamArrayPool.Pool[i][USER_MAX_STREAM_BYTES] and $80 = 0 then
      begin
        AnArray := @StreamArrayPool.Pool[i];
        StreamArrayPool.Pool[i][USER_MAX_STREAM_BYTES] := StreamArrayPool.Pool[i][USER_MAX_STREAM_BYTES] or $80;
        Inc(StreamArrayPool.Count);
        Result := True;
        Exit;
      end;
    end;
  end;
end;

procedure DeAllocateCANArray(DataArray: PCANDataArray);
begin
  DataArray^[MAX_CAN_BYTES] := DataArray^[MAX_CAN_BYTES] and $7F;
  Dec(CANArrayPool.Count);
end;

procedure DeAllocateDatagramArray(DataArray: PDatagramDataArray);
begin
  DataArray^[MAX_DATAGRAM_BYTES] := DataArray^[MAX_DATAGRAM_BYTES] and $7F;
  Dec(DatagramArrayPool.Count);
end;

procedure DeAllocateSteamArray(DataArray: PStreamDataArray);
begin
  DataArray^[USER_MAX_STREAM_BYTES] := DataArray^[USER_MAX_STREAM_BYTES] and $7F;
  Dec(StreamArrayPool.Count);
end;

function NextFreeSimpleMessage(var SimpleMessage: PSimpleMessage): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to USER_MAX_SIMPLE_MESSAGE_BUFFERS - 1 do
  begin
    if SimpleMessagePool.Pool[i].MessageType and MT_ALLOCATED = 0 then
    begin
      SimpleMessage := @SimpleMessagePool.Pool[i];
      SimpleMessage^.MessageType := SimpleMessage^.MessageType or MT_ALLOCATED;
      Result := True;
    end;
  end;
end;

function OPStack_AllocateMessage(var AMessage: PSimpleMessage; MTI: DWord;
  Next: PMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID;
  DestAlias: Word; var DestNodeID: TNodeID): Boolean;
begin
  Result := False;
  if  NextFreeSimpleMessage(AMessage) then
  begin
    OPStack_LoadBaseMessageBuffer(AMessage, MT_SIMPLE, MTI, Next, SourceNodeAlias, SourceNodeID, DestAlias, DestNodeID);
    Result := True
  end;
end;

function OPStack_AllocateCANMessage(var AMessage: PSimpleMessage; MTI: DWord;
  Next: PMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID;
  DestAlias: Word; var DestNodeID: TNodeID): Boolean;
var
  DataArrayPtr: PCANDataArray;
begin
  Result := False;
  if  NextFreeSimpleMessage(AMessage) then
  begin
    if AllocateCANArray(DataArrayPtr) then
    begin
      OPStack_LoadBaseMessageBuffer(AMessage, MT_SIMPLE, MTI, Next, SourceNodeAlias, SourceNodeID, DestAlias, DestNodeID);
      AMessage^.Data := PByte( DataArrayPtr);
      Result := True
    end;
  end;
end;

function OPStack_AllocateDatagramMessage(var AMessage: PSimpleMessage;
  MTI: DWord; Next: PMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID;
  DestAlias: Word; var DestNodeID: TNodeID): Boolean;
begin
    Result := False;
end;

function OPStack_AllcoateStreamMessage(var AMessage: PSimpleMessage;
  MTI: DWord; Next: PMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID;
  DestAlias: Word; var DestNodeID: TNodeID): Boolean;
begin
    Result := False;
end;


procedure OPStack_LoadBaseMessageBuffer(Message: PSimpleMessage; MessageType: Byte; MTI: DWord; Next: PMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID);
begin
  Message^.MessageType := MessageType;
  Message^.MTI := MTI;
  Message^.Next := Next;
  Message^.Dest.AliasID := DestAlias;
  Message^.Dest.ID := DestNodeID;
  Message^.Source.AliasID := SourceNodeAlias;
  Message^.Source.ID := SourceNodeID;
  Message^.MessageSpecificInfo := nil;
  Message^.DataLen := 0;
  Message^.Data := nil;
end;

end.

