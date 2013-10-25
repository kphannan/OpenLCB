unit opstackbuffers;

{$IFDEF FPC}
interface
{$ENDIF}

uses
  opstacktypes,
  opstackdefines,
  template_buffers;

const
  ABS_ALLOCATED = $01;                                                          // Array Buffer State Flag = Allocated Buffer

type
  TBuffer = record
    State: Byte;                                                                // See ABS_xxxx flags
    iStateMachine: Byte;                                                        // Local Statemachine
    DataBufferSize: Word;                                                       // Number of bytes in the DataBuffer
    DataArray: TDataArray;
  end;
  PBUffer = ^TBuffer;

  TCANBuffer = record
    State: Byte;                                                                // See ABS_xxxx flags
    iStateMachine: Byte;                                                        // Local Statemachine
    DataBufferSize: Word;                                                       // Number of bytes in the DataBuffer
    DataArray: TCANDataArray;
  end;
  PCANBUffer = ^TCANBuffer;

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
  TSimpleMessage = record                                                       // Used as the "base class" for all the message records, allows this class to be overlayed the other to fake inheritance
    MessageType: Byte;                                                          // MT_xxx Constant the identifies the type of message
    Source: TNodeInfo;
    Dest: TNodeInfo;
    Next: PMessage;
    MTI: DWord;
    Buffer: PBuffer;
  end;
  PSimpleMessage = ^TSimpleMessage;


procedure OPStack_Initialize;

function OPStack_AllocateMessage(var AMessage: PSimpleMessage; MTI: DWord; Next: PMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID): Boolean;
function OPStack_AllocateCANMessage(var AMessage: PSimpleMessage; MTI: DWord; Next: PMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID): Boolean;
function OPStack_AllocateDatagramMessage(var AMessage: PSimpleMessage; MTI: DWord; Next: PMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID): Boolean;
function OPStack_AllcoateStreamMessage(var AMessage: PSimpleMessage; MTI: DWord; Next: PMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID): Boolean;
procedure OPStack_DeAllocateMessage(var AMessage: PSimpleMessage);



procedure OPStack_LoadBaseMessageBuffer(AMessage: PSimpleMessage; MessageType: Byte; MTI: DWord; Next: PMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID);

implementation

type
  TCANBufferPool = record
    Pool: array[0..USER_MAX_CAN_ARRAY_BUFFERS-1] of TCANBuffer;
    Count: Word;
  end;
  PCANBufferPool = ^TCANBufferPool;

  TDatagramBufferPool = record
    Pool: array[0..USER_MAX_DATAGRAM_ARRAY_BUFFERS-1] of TDatagramBuffer;
    Count: Word;
  end;
  PDatagramBufferPool = ^TDatagramBufferPool;

  TStreamBufferPool = record
    Pool: array[0..USER_MAX_STREAM_ARRAY_BUFFERS-1] of TStreamBuffer;
    Count: Word;
  end;
  PStreamBufferPool = ^TStreamBufferPool;

  TSimpleMessagePool = record
    Pool: array[0..USER_MAX_SIMPLE_MESSAGE_BUFFERS-1] of TSimpleMessage;
    Count: Word;
  end;

var
  CANBufferPool: TCANBufferPool;
  DatagramBufferPool: TDatagramBufferPool;
  StreamBufferPool: TStreamBufferPool;
  SimpleMessagePool: TSimpleMessagePool;

procedure OPStack_Initialize;
var
  i, j: Integer;
begin
  for j := 0 to USER_MAX_CAN_ARRAY_BUFFERS-1  do
  begin
    CANBufferPool.Pool[j].State := 0;
    CANBufferPool.Pool[j].iStateMachine := 0;
    CANBufferPool.Pool[j].DataBufferSize := 0;
    for i := 0 to MAX_CAN_BYTES - 1 do
      CANBufferPool.Pool[j].DataArray[i] := 0;
  end;
  CANBufferPool.Count := 0;

  for j := 0 to USER_MAX_DATAGRAM_ARRAY_BUFFERS-1  do
  begin
    DatagramBufferPool.Pool[j].State := 0;
    DatagramBufferPool.Pool[j].iStateMachine := 0;
    DatagramBufferPool.Pool[j].DataBufferSize := 0;
    for i := 0 to MAX_DATAGRAM_BYTES - 1 do
      DatagramBufferPool.Pool[j].DataArray[i] := 0;
  end;
  DatagramBufferPool.Count := 0;

  for j := 0 to USER_MAX_STREAM_ARRAY_BUFFERS-1  do
  begin
    StreamBufferPool.Pool[j].State := 0;
    StreamBufferPool.Pool[j].iStateMachine := 0;
    StreamBufferPool.Pool[j].DataBufferSize := 0;
    for i := 0 to USER_MAX_STREAM_BYTES - 1 do
      StreamBufferPool.Pool[j].DataArray[i] := 0;
  end;
  StreamBufferPool.Count := 0;

  for j := 0 to USER_MAX_SIMPLE_MESSAGE_BUFFERS  do                               // Extra Byte at end for state flags
    OPStack_LoadBaseMessageBuffer(@SimpleMessagePool.Pool[j], MT_UNALLOCATED, 0, nil, 0, NULL_NODE_ID, 0, NULL_NODE_ID);
  SimpleMessagePool.Count := 0;
end;

function AllocateCANBuffer(var Buffer: PCANBuffer): Boolean;
var
  i: Integer;
begin
  Result := False;
  if CANBufferPool.Count < USER_MAX_CAN_ARRAY_BUFFERS then
  begin
    for i := 0 to USER_MAX_CAN_ARRAY_BUFFERS - 1 do
    begin
      if CANBufferPool.Pool[i].State and ABS_ALLOCATED = 0 then
      begin
        Buffer := @CANBufferPool.Pool[i];
        CANBufferPool.Pool[i].State := CANBufferPool.Pool[i].State or ABS_ALLOCATED;
        Inc(CANBufferPool.Count);
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function AllocateDatagramBuffer(var Buffer: PDatagramBuffer): Boolean;
var
  i: Integer;
begin
  Result := False;
  if DatagramBufferPool.Count < USER_MAX_DATAGRAM_ARRAY_BUFFERS then
  begin
    for i := 0 to USER_MAX_DATAGRAM_ARRAY_BUFFERS - 1 do
    begin
      if DatagramBufferPool.Pool[i].State and ABS_ALLOCATED = 0 then
      begin
        Buffer := @DatagramBufferPool.Pool[i];
        DatagramBufferPool.Pool[i].State := DatagramBufferPool.Pool[i].State or ABS_ALLOCATED;
        Inc(DatagramBufferPool.Count);
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function AllocateStreamBuffer(var Buffer: PStreamBuffer): Boolean;
var
  i: Integer;
begin
  Result := False;
  if StreamBufferPool.Count < USER_MAX_STREAM_ARRAY_BUFFERS then
  begin
    for i := 0 to USER_MAX_STREAM_ARRAY_BUFFERS - 1 do
    begin
      if StreamBufferPool.Pool[i].State and ABS_ALLOCATED = 0 then
      begin
        Buffer := @StreamBufferPool.Pool[i];
        StreamBufferPool.Pool[i].State := StreamBufferPool.Pool[i].State or ABS_ALLOCATED;
        Inc(StreamBufferPool.Count);
        Result := True;
        Exit;
      end;
    end;
  end;
end;

procedure DeAllocateCANBuffer(Buffer: PCANBuffer);
begin
  Buffer^.State := Buffer^.State and not ABS_ALLOCATED;
  Dec(CANBufferPool.Count);
end;

procedure DeAllocateDatagramBuffer(Buffer: PDatagramBuffer);
begin
  Buffer^.State := Buffer^.State and not ABS_ALLOCATED;
  Dec(DatagramBufferPool.Count);
end;

procedure DeAllocateSteamBuffer(Buffer: PStreamBuffer);
begin
  Buffer^.State := Buffer^.State and not ABS_ALLOCATED;
  Dec(StreamBufferPool.Count);
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
      Break
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
  CANBuffer: PCANBuffer;
begin
  Result := False;
  if NextFreeSimpleMessage(AMessage) then
  begin
    if AllocateCANBuffer(CANBuffer) then
    begin
      OPStack_LoadBaseMessageBuffer(AMessage, MT_CAN, MTI, Next, SourceNodeAlias, SourceNodeID, DestAlias, DestNodeID);
      AMessage^.Buffer := PBuffer( CANBuffer);
      Result := True
    end else
    begin
      OPStack_DeAllocateMessage(AMessage);
      AMessage := nil;
    end;
  end;
end;

function OPStack_AllocateDatagramMessage(var AMessage: PSimpleMessage;
  MTI: DWord; Next: PMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID;
  DestAlias: Word; var DestNodeID: TNodeID): Boolean;
var
  DatagramBuffer: PDatagramBuffer;
begin
  Result := False;
  if NextFreeSimpleMessage(AMessage) then
  begin
    if AllocateDatagramBuffer(DatagramBuffer) then
    begin
      OPStack_LoadBaseMessageBuffer(AMessage, MT_DATAGRAM, MTI, Next, SourceNodeAlias, SourceNodeID, DestAlias, DestNodeID);
      AMessage^.Buffer := PBuffer( DatagramBuffer);
      Result := True
    end else
    begin
      OPStack_DeAllocateMessage(AMessage);
      AMessage := nil;
    end;
  end;
end;

function OPStack_AllcoateStreamMessage(var AMessage: PSimpleMessage;
  MTI: DWord; Next: PMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID;
  DestAlias: Word; var DestNodeID: TNodeID): Boolean;
var
  StreamBuffer: PStreamBuffer;
begin
  Result := False;
  if NextFreeSimpleMessage(AMessage) then
  begin
    if AllocateStreamBuffer(StreamBuffer) then
    begin
      OPStack_LoadBaseMessageBuffer(AMessage, MT_STREAM, MTI, Next, SourceNodeAlias, SourceNodeID, DestAlias, DestNodeID);
      AMessage^.Buffer := PBuffer( StreamBuffer);
      Result := True
    end else
    begin
      OPStack_DeAllocateMessage(AMessage);
      AMessage := nil;
    end;
  end;
end;

procedure OPStack_DeAllocateMessage(var AMessage: PSimpleMessage);
begin
  case (AMessage^.MessageType and $7F) of
    MT_CAN       : DeAllocateCANBuffer(PCANBuffer( AMessage^.Buffer));
    MT_DATAGRAM  : DeAllocateDatagramBuffer(PDatagramBuffer( AMessage^.Buffer));
    MT_STREAM    : DeAllocateSteamBuffer(PStreamBuffer( AMessage^.Buffer));
  end;
  AMessage^.MessageType := MT_UNALLOCATED;
end;


procedure OPStack_LoadBaseMessageBuffer(AMessage: PSimpleMessage; MessageType: Byte; MTI: DWord; Next: PMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID);
begin
  AMessage^.MessageType := AMessage^.MessageType or MessageType;
  AMessage^.MTI := MTI;
  AMessage^.Next := Next;
  AMessage^.Dest.AliasID := DestAlias;
  AMessage^.Dest.ID := DestNodeID;
  AMessage^.Source.AliasID := SourceNodeAlias;
  AMessage^.Source.ID := SourceNodeID;
  AMessage^.Buffer := nil;
end;

end.
