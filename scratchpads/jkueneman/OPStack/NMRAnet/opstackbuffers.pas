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


procedure OPStackBuffers_Initialize;

// Allocate Message helpers
function OPStackBuffers_AllocateOPStackMessage(var AMessage: POPStackMessage; MTI: Word; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID): Boolean;
function OPStackBuffers_AllocateSimpleCANMessage(var AMessage: POPStackMessage; MTI: Word; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID): Boolean;
function OPStackBuffers_AllocateDatagramMessage(var AMessage: POPStackMessage; MTI: Word; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID; DestFlags: Byte): Boolean;
function OPStackBuffers_AllcoateStreamMessage(var AMessage: POPStackMessage; MTI: Word; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID): Boolean;
procedure OPStackBuffers_DeAllocateMessage(AMessage: POPStackMessage);

// Load Message helpers
procedure OPStackBuffers_LoadMessage(AMessage: POPStackMessage; MTI: Word; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID; DestFlags: Byte);
procedure OPStackBuffers_LoadDatagramRejectedBuffer(SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID; ErrorCode: PSimpleDataArray);
procedure OPStackBuffers_LoadDatagramOkMessage(AMessage: POPStackMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID; Flags: Byte);
procedure OPStackBuffers_LoadOptionalInteractionRejected(AMessage: POPStackMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID; RejectedMTI: Word);

// Load Buffer helpers
procedure OPStackBuffers_LoadSimpleBuffer(ABuffer: PSimpleBuffer; iStateMachine: Byte; DataBufferSize: Word; DataArray: PSimpleDataArray; ArrayOffset: Integer);
procedure OPStackBuffers_LoadDatagramBuffer(ABuffer: PDatagramBuffer; iStateMachine: Byte; DataBufferSize: Word; DataArray: PSimpleDataArray; ArrayOffset: Integer);
procedure OPStackBuffers_LoadStreamBuffer(ABuffer: PStreamBuffer; iStateMachine: Byte; DataBufferSize: Word; DataArray: PSimpleDataArray; ArrayOffset: Integer);

// Zero buffer helpers
procedure OPStackBuffers_ZeroMessage(AMessage: POPStackMessage);
procedure OPStackBuffers_ZeroSimpleBuffer(ABuffer: PSimpleBuffer; ZeroArray: Boolean);
procedure OPStackBuffers_ZeroDatagramBuffer(ABuffer: PDatagramBuffer; ZeroArray: Boolean);
procedure OPStackBuffers_ZeroStreamBuffer(ABuffer: PStreamBuffer; ZeroArray: Boolean);

// Copy buffer helpers
procedure OPStackBuffers_CopyData(DestData, SourceData: PSimpleBuffer);
procedure OPStackBuffers_CopyDataArray(DestData: PSImpleBuffer; SourceDataArray: PSimpleDataArray; Count: Word);
procedure OPStackBuffers_CopyDataArrayWithDestOffset(DestData: PSimpleBuffer; SourceDataArray: PSimpleDataArray; DestOffset, Count: Word);

// Message Node ID helpers
procedure OPStackBuffers_SwapDestAndSourceIDs(var AMessage: TOPStackMessage);

var
  SimpleBufferPool: TSimpleBufferPool;
  DatagramBufferPool: TDatagramBufferPool;
  StreamBufferPool: TStreamBufferPool;
  OPStackMessagePool: TOPStackMessagePool;

  DatagramRejected: TOPStackMessage;
  DatagramRejectedBuffer: TSimpleBuffer;
  OptionalInteractionRejected: TOPStackMessage;
  OptionalInteractionRejectedBuffer: TSimpleBuffer;

implementation


procedure OPStackBuffers_Initialize;
var
  i, j: Integer;
begin
  DatagramRejected.Buffer := @DatagramRejectedBuffer;
  OptionalInteractionRejected.Buffer := @OptionalInteractionRejectedBuffer;

  for j := 0 to USER_MAX_SIMPLE_ARRAY_BUFFERS-1  do
    OPStackBuffers_ZeroSimpleBuffer(@SimpleBufferPool.Pool[j], True);
  SimpleBufferPool.Count := 0;

  for j := 0 to USER_MAX_DATAGRAM_ARRAY_BUFFERS-1  do
    OPStackBuffers_ZeroDatagramBuffer(@DatagramBufferPool.Pool[j], True);
  DatagramBufferPool.Count := 0;

  for j := 0 to USER_MAX_STREAM_ARRAY_BUFFERS-1  do
    OPStackBuffers_ZeroStreamBuffer(@StreamBufferPool.Pool[j], True);
  StreamBufferPool.Count := 0;

  for j := 0 to USER_MAX_SIMPLE_MESSAGE_BUFFERS  do                               // Extra Byte at end for state flags
    OPStackBuffers_ZeroMessage(@OPStackMessagePool.Pool[j]);
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
        OPStackBuffers_ZeroSimpleBuffer(Buffer, False);
        SimpleBufferPool.Pool[i].State := ABS_ALLOCATED;
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
        OPStackBuffers_ZeroDatagramBuffer(Buffer, False);
        DatagramBufferPool.Pool[i].State := ABS_ALLOCATED;
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
        OPStackBuffers_ZeroStreamBuffer(Buffer, False);
        StreamBufferPool.Pool[i].State := ABS_ALLOCATED;
        Inc(StreamBufferPool.Count);
        Result := True;
        Exit;
      end;
    end;
  end;
end;

procedure DeAllocateSimpleBuffer(Buffer: PSimpleBuffer);
begin
  if Buffer^.State and ABS_ALLOCATED  <> 0 then                                 // Only effect the pool if the buffer was allocated from the pool
  begin
    Dec(SimpleBufferPool.Count);
    Buffer^.State := 0
  end
end;

procedure DeAllocateDatagramBuffer(Buffer: PDatagramBuffer);
begin
  if Buffer^.State and ABS_ALLOCATED <> 0 then                                  // Only effect the pool if the buffer was allocated from the pool
  begin
    Dec(DatagramBufferPool.Count);
    Buffer^.State := 0
  end
end;

procedure DeAllocateSteamBuffer(Buffer: PStreamBuffer);
begin
  if Buffer^.State and ABS_ALLOCATED <> 0 then                                  // Only effect the pool if the buffer was allocated from the pool
  begin
    Dec(StreamBufferPool.Count);
    Buffer^.State := 0
  end
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
      OPStackMessage^.MessageType := MT_ALLOCATED;
      Inc(OPStackMessagePool.Count);
      Result := True;
      Break
    end;
  end;
end;

function OPStackBuffers_AllocateOPStackMessage(var AMessage: POPStackMessage; MTI: Word; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID): Boolean;
var
  SimpleBuffer: PSimpleBuffer;
begin
  Result := False;
  if NextFreeOPStackMessage(AMessage) then
  begin
    if AllocateSimpleBuffer(SimpleBuffer) then
    begin
      OPStackBuffers_LoadMessage(AMessage, MTI, SourceNodeAlias, SourceNodeID, DestAlias, DestNodeID, 0);
      AMessage^.MessageType := MT_SIMPLE or MT_ALLOCATED;
      AMessage^.Buffer := PSimpleBuffer( SimpleBuffer);
      Result := True
    end else
    begin
      OPStackBuffers_DeAllocateMessage(AMessage);
      AMessage := nil;
    end;
  end;
end;

function OPStackBuffers_AllocateSimpleCANMessage(var AMessage: POPStackMessage; MTI: Word; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID): Boolean;
begin
  Result := OPStackBuffers_AllocateOPStackMessage(AMessage, MTI, SourceNodeAlias, SourceNodeID, DestAlias, DestNodeID);
  if Result then
    AMessage^.MessageType := AMessage^.MessageType or MT_CAN_TYPE;
end;

function OPStackBuffers_AllocateDatagramMessage(var AMessage: POPStackMessage; MTI: Word; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID; DestFlags: Byte): Boolean;
var
  DatagramBuffer: PDatagramBuffer;
begin
  Result := False;
  if NextFreeOPStackMessage(AMessage) then
  begin
    if AllocateDatagramBuffer(DatagramBuffer) then
    begin
      OPStackBuffers_LoadMessage(AMessage, MTI, SourceNodeAlias, SourceNodeID, DestAlias, DestNodeID, DestFlags);
      AMessage^.MessageType := MT_DATAGRAM or MT_ALLOCATED;
      AMessage^.Buffer := PSimpleBuffer( PByte( DatagramBuffer));
      Result := True
    end else
    begin
      OPStackBuffers_DeAllocateMessage(AMessage);
      AMessage := nil;
    end;
  end;
end;

function OPStackBuffers_AllcoateStreamMessage(var AMessage: POPStackMessage; MTI: Word; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID): Boolean;
var
  StreamBuffer: PStreamBuffer;
begin
  Result := False;
  if NextFreeOPStackMessage(AMessage) then
  begin
    if AllocateStreamBuffer(StreamBuffer) then
    begin
      OPStackBuffers_LoadMessage(AMessage, MTI, SourceNodeAlias, SourceNodeID, DestAlias, DestNodeID, 0);
      AMessage^.MessageType := MT_STREAM or MT_ALLOCATED;
      AMessage^.Buffer := PSimpleBuffer( PByte( StreamBuffer));
      Result := True
    end else
    begin
      OPStackBuffers_DeAllocateMessage(AMessage);
      AMessage := nil;
    end;
  end;
end;

procedure OPStackBuffers_DeAllocateMessage(AMessage: POPStackMessage);
begin
  if AMessage^.MessageType and MT_ALLOCATED <> 0 then                           // Only deallocate if we allocated it
  begin
    case (AMessage^.MessageType and MT_MASK) of
      MT_SIMPLE    : DeAllocateSimpleBuffer(PSimpleBuffer( AMessage^.Buffer));
      MT_DATAGRAM  : DeAllocateDatagramBuffer(PDatagramBuffer( PByte(AMessage^.Buffer)));
      MT_STREAM    : DeAllocateSteamBuffer(PStreamBuffer( PByte( AMessage^.Buffer)));
    end;
    AMessage^.MessageType := MT_UNALLOCATED;
    Dec(OPStackMessagePool.Count);
  end;
end;

procedure OPStackBuffers_LoadDatagramRejectedBuffer(SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID; ErrorCode: PSimpleDataArray);
begin
  OPStackBuffers_LoadMessage(@DatagramRejected, MTI_DATAGRAM_REJECTED_REPLY, SourceNodeAlias, SourceNodeID, DestAlias, DestNodeID, 0);
  DatagramRejected.MessageType := MT_SIMPLE;                                    // This is not Allocated because the DatagramRejected Message is a local copy
  OPStackBuffers_CopyDataArray(DatagramRejected.Buffer, ErrorCode, 2)
end;

procedure OPStackBuffers_LoadDatagramOkMessage(AMessage: POPStackMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID; Flags: Byte);
begin
  OPStackBuffers_LoadMessage(AMessage, MTI_DATAGRAM_OK_REPLY, SourceNodeAlias, SourceNodeID, DestAlias, DestNodeID, 0);
  if Flags <> 0 then
  begin
    AMessage^.Buffer^.DataBufferSize := 1;
    AMessage^.Buffer^.DataArray[0] := Flags;
  end else
   AMessage^.Buffer^.DataBufferSize := 0;
end;

procedure OPStackBuffers_LoadOptionalInteractionRejected(AMessage: POPStackMessage; SourceNodeAlias: Word; var SourceNodeID: TNodeID; RejectedMTI: Word);
begin
  OPStackBuffers_LoadMessage(AMessage, MTI_OPTIONAL_INTERACTION_REJECTED, SourceNodeAlias, SourceNodeID, 0, NULL_NODE_ID, 0);
  AMessage^.Buffer^.DataBufferSize := 4;
  AMessage^.Buffer^.DataArray[0] := $20;
  AMessage^.Buffer^.DataArray[1] := $00;
  AMessage^.Buffer^.DataArray[2] := Hi( RejectedMTI);
  AMessage^.Buffer^.DataArray[3] := Lo( RejectedMTI);
end;

procedure LoadBuffer(ABuffer: PSimpleBuffer; iStateMachine: Byte; DataBufferSize: Word; DataArray: PSimpleDataArray; BufferSize: Word; ArrayOffset: Integer);
var
  i: Integer;
begin
  ABuffer^.iStateMachine := iStateMachine;
  ABuffer^.DataBufferSize := DataBufferSize;
  if DataArray <> nil then
  begin
    for i := 0 to DataBufferSize - 1 do
      ABuffer^.DataArray[i] := DataArray^[i + ArrayOffset];
  end
end;

procedure OPStackBuffers_LoadSimpleBuffer(ABuffer: PSimpleBuffer; iStateMachine: Byte; DataBufferSize: Word; DataArray: PSimpleDataArray; ArrayOffset: Integer);
begin
  LoadBuffer(ABuffer, iStateMachine, DataBufferSize, DataArray, MAX_SIMPLE_BYTES, ArrayOffset);
end;

procedure OPStackBuffers_LoadDatagramBuffer(ABuffer: PDatagramBuffer; iStateMachine: Byte; DataBufferSize: Word; DataArray: PSimpleDataArray; ArrayOffset: Integer);
begin
  LoadBuffer(PSimpleBuffer( PByte( ABuffer)), iStateMachine, DataBufferSize, DataArray, MAX_DATAGRAM_BYTES, ArrayOffset);
end;

procedure OPStackBuffers_LoadStreamBuffer(ABuffer: PStreamBuffer; iStateMachine: Byte; DataBufferSize: Word; DataArray: PSimpleDataArray; ArrayOffset: Integer);
begin
  LoadBuffer(PSimpleBuffer( PByte( ABuffer)), iStateMachine, DataBufferSize, DataArray, USER_MAX_STREAM_BYTES, ArrayOffset);
end;

procedure ZeroBuffer(ABuffer: PSimpleBuffer; DataBufferSize: Word);
var
  i: Integer;
begin
  ABuffer^.iStateMachine := 0;
  ABuffer^.State := 0;
  ABuffer^.DataBufferSize := 0;
  for i := 0 to DataBufferSize - 1 do
    ABuffer^.DataArray[i] := 0;
end;

procedure OPStackBuffers_ZeroSimpleBuffer(ABuffer: PSimpleBuffer; ZeroArray: Boolean);
begin
  if ZeroArray then
    ZeroBuffer(ABuffer, MAX_SIMPLE_BYTES)
  else
    ZeroBuffer(ABuffer, 0);
end;

procedure OPStackBuffers_ZeroDatagramBuffer(ABuffer: PDatagramBuffer; ZeroArray: Boolean);
begin
  if ZeroArray then
    ZeroBuffer(PSimpleBuffer( PByte( ABuffer)), MAX_DATAGRAM_BYTES)
  else
    ZeroBuffer(PSimpleBuffer( PByte( ABuffer)), 0)
end;

procedure OPStackBuffers_ZeroStreamBuffer(ABuffer: PStreamBuffer; ZeroArray: Boolean);
begin
  if ZeroArray then
    ZeroBuffer(PSimpleBuffer( PByte( ABuffer)), USER_MAX_STREAM_BYTES)
  else
    ZeroBuffer(PSimpleBuffer( PByte( ABuffer)), 0)
end;

procedure OPStackBuffers_LoadMessage(AMessage: POPStackMessage; MTI: Word; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID; DestFlags: Byte);
begin
  AMessage^.MTI := MTI;
  AMessage^.Dest.AliasID := DestAlias;
  AMessage^.Dest.ID := DestNodeID;
  AMessage^.Source.AliasID := SourceNodeAlias;
  AMessage^.Source.ID := SourceNodeID;
  AMessage^.DestFlags := DestFlags;
end;

procedure OPStackBuffers_ZeroMessage(AMessage: POPStackMessage);
begin
  AMessage^.MessageType := 0;
  AMessage^.MTI := 0;
  AMessage^.Next := nil;
  AMessage^.Dest.AliasID := 0;
  AMessage^.Dest.ID := NULL_NODE_ID;
  AMessage^.Source.AliasID := 0;
  AMessage^.Source.ID := NULL_NODE_ID;
  AMessage^.Buffer := nil;
  AMessage^.DestFlags := 0;
end;

procedure OPStackBuffers_CopyData(DestData, SourceData: PSimpleBuffer);
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

procedure OPStackBuffers_CopyDataArray(DestData: PSImpleBuffer; SourceDataArray: PSimpleDataArray; Count: Word);
begin
  OPStackBuffers_CopyDataArrayWithDestOffset(DestData, SourceDataArray, 0, Count);
end;

procedure OPStackBuffers_CopyDataArrayWithDestOffset(DestData: PSImpleBuffer; SourceDataArray: PSimpleDataArray; DestOffset, Count: Word);
begin
  DestData^.DataBufferSize := 0;
  while DestData^.DataBufferSize < Count do
  begin
    DestData^.DataArray[DestData^.DataBufferSize+DestOffset] := SourceDataArray^[DestData^.DataBufferSize];
    Inc(DestData^.DataBufferSize)
  end
end;

procedure OPStackBuffers_SwapDestAndSourceIDs(var AMessage: TOPStackMessage);
var
  Temp: TNodeInfo;
begin
  Temp.AliasID := AMessage.Dest.AliasID;
  Temp.ID := AMessage.Dest.ID;
  AMessage.Dest.AliasID := AMessage.Source.AliasID;
  AMessage.Dest.ID := AMessage.Source.ID;
  AMessage.Source.AliasID := Temp.AliasID;
  AMessage.Source.ID := Temp.ID;
end;

end.
