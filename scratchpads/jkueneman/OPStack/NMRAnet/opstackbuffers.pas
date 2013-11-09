unit opstackbuffers;

{$IFDEF FPC}
interface
{$ENDIF}

uses
  {$IFDEF FPC}
  Classes, SysUtils,
  {$ENDIF}
  opstacktypes,
  opstackdefines,
  nmranetdefines,
  template_buffers;

type
  TSimpleBufferPool = record
    Pool: array[0..USER_MAX_SIMPLE_ARRAY_BUFFERS-1] of TSimpleBuffer;
    Count: Word;
  end;
  PSimpleBufferPool = ^TSimpleBufferPool;

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

  TOPStackMessagePool = record
    Pool: array[0..USER_MAX_SIMPLE_MESSAGE_BUFFERS-1] of TOPStackMessage;
    Count: Word;
  end;


procedure OPStack_Initialize;

function OPStack_AllocateOPStackMessage(var AMessage: POPStackMessage; MTI: Word; Next: POPStackMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID): Boolean;
function OPStack_AllocateSimpleCANMessage(var AMessage: POPStackMessage; MTI: Word; Next: POPStackMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID): Boolean;
function OPStack_AllocateDatagramMessage(var AMessage: POPStackMessage; MTI: Word; Next: POPStackMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID): Boolean;
function OPStack_AllcoateStreamMessage(var AMessage: POPStackMessage; MTI: Word; Next: POPStackMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID): Boolean;
procedure OPStack_DeAllocateMessage(AMessage: POPStackMessage);

procedure OPStack_LoadBaseMessageBuffer(AMessage: POPStackMessage; MessageType: Byte; MTI: Word; Next: POPStackMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID);
procedure OPStack_LoadDatagramRejectedBuffer(SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID; ErrorCode: PSimpleDataArray);
procedure OPStack_LoadDatagramOkBuffer(var AMessage: TOPStackMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID; Flags: Byte);

procedure OPStack_CopyData(DestData, SourceData: PSimpleBuffer);
procedure OPStack_CopyDataArray(DestData: PSImpleBuffer; SourceDataArray: PSimpleDataArray; Count: Word);
procedure OPStack_CopyDataArrayWithDestOffset(DestData: PSimpleBuffer; SourceDataArray: PSimpleDataArray; DestOffset, Count: Word);

procedure OPstack_SwapDestAndSourceIDs(var AMessage: TOPStackMessage);

var
  SimpleBufferPool: TSimpleBufferPool;
  DatagramBufferPool: TDatagramBufferPool;
  StreamBufferPool: TStreamBufferPool;
  OPStackMessagePool: TOPStackMessagePool;
  DatagramRejected: TOPStackMessage;


implementation

var
  DatagramRejectedBuffer: TSimpleBuffer;

procedure OPStack_Initialize;
var
  i, j: Integer;
begin
  DatagramRejected.Buffer := @DatagramRejectedBuffer;
  for j := 0 to USER_MAX_SIMPLE_ARRAY_BUFFERS-1  do
  begin
    SimpleBufferPool.Pool[j].State := 0;
    SimpleBufferPool.Pool[j].iStateMachine := 0;
    SimpleBufferPool.Pool[j].DataBufferSize := 0;
    for i := 0 to MAX_SIMPLE_BYTES - 1 do
      SimpleBufferPool.Pool[j].DataArray[i] := 0;
  end;
  SimpleBufferPool.Count := 0;

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
    OPStack_LoadBaseMessageBuffer(@OPStackMessagePool.Pool[j], MT_UNALLOCATED, 0, nil, 0, NULL_NODE_ID, 0, NULL_NODE_ID);
  OPStackMessagePool.Count := 0;
end;

function AllocateSimpleBuffer(var Buffer: PSimpleBuffer): Boolean;
var
  i: Integer;
begin
  Result := False;
  if SimpleBufferPool.Count < USER_MAX_SIMPLE_ARRAY_BUFFERS then
  begin
    for i := 0 to USER_MAX_SIMPLE_ARRAY_BUFFERS - 1 do
    begin
      if SimpleBufferPool.Pool[i].State and ABS_ALLOCATED = 0 then
      begin
        Buffer := @SimpleBufferPool.Pool[i];
        SimpleBufferPool.Pool[i].State := SimpleBufferPool.Pool[i].State or ABS_ALLOCATED;
        Buffer^.DataBufferSize := 0;
        Buffer^.iStateMachine := 0;
        Inc(SimpleBufferPool.Count);
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

procedure DeAllocateSimpleBuffer(Buffer: PSimpleBuffer);
begin
  Buffer^.State := Buffer^.State and not ABS_ALLOCATED;
  Dec(SimpleBufferPool.Count);
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

function NextFreeOPStackMessage(var OPStackMessage: POPStackMessage): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to USER_MAX_SIMPLE_MESSAGE_BUFFERS - 1 do
  begin
    if OPStackMessagePool.Pool[i].MessageType and MT_ALLOCATED = 0 then
    begin
      OPStackMessage := @OPStackMessagePool.Pool[i];
      OPStackMessage^.MessageType := OPStackMessage^.MessageType or MT_ALLOCATED;
      Inc(OPStackMessagePool.Count);
      Result := True;
      Break
    end;
  end;
end;

function OPStack_AllocateOPStackMessage(var AMessage: POPStackMessage; MTI: Word;
  Next: POPStackMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID;
  DestAlias: Word; var DestNodeID: TNodeID): Boolean;
var
  SimpleBuffer: PSimpleBuffer;
begin
  Result := False;
  if NextFreeOPStackMessage(AMessage) then
  begin
    if AllocateSimpleBuffer(SimpleBuffer) then
    begin
      OPStack_LoadBaseMessageBuffer(AMessage, MT_SIMPLE, MTI, Next, SourceNodeAlias, SourceNodeID, DestAlias, DestNodeID);
      AMessage^.Buffer := PSimpleBuffer( SimpleBuffer);
      Result := True
    end else
    begin
      OPStack_DeAllocateMessage(AMessage);
      AMessage := nil;
    end;
  end;
end;

function OPStack_AllocateSimpleCANMessage(var AMessage: POPStackMessage;
  MTI: Word; Next: POPStackMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID;
  DestAlias: Word; var DestNodeID: TNodeID): Boolean;
begin
  Result := OPStack_AllocateOPStackMessage(AMessage, MTI, Next, SourceNodeAlias, SourceNodeID, DestAlias, DestNodeID);
  if Result then
    AMessage^.MessageType := AMessage^.MessageType or MT_CAN_TYPE;
end;

function OPStack_AllocateDatagramMessage(var AMessage: POPStackMessage;
  MTI: Word; Next: POPStackMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID;
  DestAlias: Word; var DestNodeID: TNodeID): Boolean;
var
  DatagramBuffer: PDatagramBuffer;
begin
  Result := False;
  if NextFreeOPStackMessage(AMessage) then
  begin
    if AllocateDatagramBuffer(DatagramBuffer) then
    begin
      OPStack_LoadBaseMessageBuffer(AMessage, MT_DATAGRAM, MTI, Next, SourceNodeAlias, SourceNodeID, DestAlias, DestNodeID);
      AMessage^.Buffer := PSimpleBuffer( DatagramBuffer);
      Result := True
    end else
    begin
      OPStack_DeAllocateMessage(AMessage);
      AMessage := nil;
    end;
  end;
end;

function OPStack_AllcoateStreamMessage(var AMessage: POPStackMessage;
  MTI: Word; Next: POPStackMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID;
  DestAlias: Word; var DestNodeID: TNodeID): Boolean;
var
  StreamBuffer: PStreamBuffer;
begin
  Result := False;
  if NextFreeOPStackMessage(AMessage) then
  begin
    if AllocateStreamBuffer(StreamBuffer) then
    begin
      OPStack_LoadBaseMessageBuffer(AMessage, MT_STREAM, MTI, Next, SourceNodeAlias, SourceNodeID, DestAlias, DestNodeID);
      AMessage^.Buffer := PSimpleBuffer( StreamBuffer);
      Result := True
    end else
    begin
      OPStack_DeAllocateMessage(AMessage);
      AMessage := nil;
    end;
  end;
end;

procedure OPStack_DeAllocateMessage(AMessage: POPStackMessage);
begin
  case (AMessage^.MessageType and MT_MASK) of
    MT_SIMPLE    : DeAllocateSimpleBuffer(PSimpleBuffer( AMessage^.Buffer));
    MT_DATAGRAM  : DeAllocateDatagramBuffer(PDatagramBuffer( AMessage^.Buffer));
    MT_STREAM    : DeAllocateSteamBuffer(PStreamBuffer( AMessage^.Buffer));
  end;
  AMessage^.MessageType := MT_UNALLOCATED;
  Dec(OPStackMessagePool.Count);

  if OPStackMessagePool.Count > 10 then
    beep;
end;

procedure OPStack_LoadDatagramRejectedBuffer(SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID; ErrorCode: PSimpleDataArray);
begin
  OPStack_LoadBaseMessageBuffer(@DatagramRejected, MT_SIMPLE, MTI_DATAGRAM_REJECTED_REPLY, nil, SourceNodeAlias, SourceNodeID, DestAlias, DestNodeID);
  OPStack_CopyDataArray(DatagramRejected.Buffer, ErrorCode, 2)
end;

procedure OPStack_LoadDatagramOkBuffer(var AMessage: TOPStackMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID; Flags: Byte);
begin
  OPStack_LoadBaseMessageBuffer(@AMessage, MT_SIMPLE, MTI_DATAGRAM_OK_REPLY, nil, SourceNodeAlias, SourceNodeID, DestAlias, DestNodeID);
  if Flags <> 0 then
  begin
    AMessage.Buffer^.DataBufferSize := 1;
    AMessage.Buffer^.DataArray[0] := Flags;
  end else
   AMessage.Buffer^.DataBufferSize := 0;
end;

procedure OPStack_LoadBaseMessageBuffer(AMessage: POPStackMessage; MessageType: Byte; MTI: Word; Next: POPStackMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID);
begin
  AMessage^.MessageType := AMessage^.MessageType or MessageType;
  AMessage^.MTI := MTI;
  AMessage^.Next := Next;
  AMessage^.Dest.AliasID := DestAlias;
  AMessage^.Dest.ID := DestNodeID;
  AMessage^.Source.AliasID := SourceNodeAlias;
  AMessage^.Source.ID := SourceNodeID;
  AMessage^.Buffer := nil;
  AMessage^.DestFlags := 0;
end;

procedure OPStack_CopyData(DestData, SourceData: PSimpleBuffer);
var
  i: Integer;
begin
  i := 0;
  while i < SourceData^.DataBufferSize do
  begin
    DestData^.DataArray[i] := SourceData^.DataArray[i];
    Inc(i)
  end
end;

procedure OPStack_CopyDataArray(DestData: PSImpleBuffer; SourceDataArray: PSimpleDataArray; Count: Word);
begin
  OPStack_CopyDataArrayWithDestOffset(DestData, SourceDataArray, 0, Count);
end;

procedure OPStack_CopyDataArrayWithDestOffset(DestData: PSImpleBuffer; SourceDataArray: PSimpleDataArray; DestOffset, Count: Word);
var
  i: Integer;
begin
  i := 0;
  while i < Count do
  begin
    DestData^.DataArray[i+DestOffset] := SourceDataArray^[i];
    Inc(i)
  end
end;

procedure OPstack_SwapDestAndSourceIDs(var AMessage: TOPStackMessage);
var
  Temp: TNodeInfo;
begin
  Temp.AliasID := AMessage.Source.AliasID;
  Temp.ID := AMessage.Source.ID;
  AMessage.Dest.AliasID := AMessage.Source.AliasID;
  AMessage.Dest.ID := AMessage.Source.ID;
  AMessage.Source.AliasID := Temp.AliasID;
  AMessage.Source.ID := Temp.ID;
end;

end.
