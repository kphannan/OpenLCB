unit opstackbuffers;

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

{.$DEFINE TRACK_BUFFERS}

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
    MaxCount: Word;
  end;
  PSimpleBufferPool = ^TSimpleBufferPool;

  TDatagramBufferPool = record
    Pool: array[0..USER_MAX_DATAGRAM_ARRAY_BUFFERS-1] of TDatagramBuffer;
    Count: Word;
    MaxCount: Word;
  end;
  PDatagramBufferPool = ^TDatagramBufferPool;

  {$IFDEF SUPPORT_STREAMS}
  TStreamBufferPool = record
    Pool: array[0..USER_MAX_STREAM_ARRAY_BUFFERS-1] of TStreamBuffer;
    Count: Word;
    MaxCount: Word;
  end;
  PStreamBufferPool = ^TStreamBufferPool;
  {$ENDIF}

  TAcdiSnipBufferPool = record
    Pool: array[0..USER_MAX_ACDI_SNIP_ARRAY_BUFFERS-1] of TAcdiSnipBuffer;
    Count: Word;
    MaxCount: Word;
  end;
  PAcdiSnipBufferPool = ^TAcdiSnipBufferPool;

  TMultiFramePool = record
    Pool: array[0..USER_MAX_MULTIFRAME_ARRAY_BUFFERS-1] of TMultiFrameBuffer;
    Count: Word;
    MaxCount: Word;
  end;
  PMultiFramePool = ^TMultiFramePool;

  TOPStackMessagePool = record
    Pool: array[0..USER_MAX_MESSAGE_ARRAY_BUFFERS-1] of TOPStackMessage;
    Count: Word;
    MaxCount: Word;
  end;


procedure OPStackBuffers_Initialize;
procedure OPStackBuffers_Timer;

// Allocate Message helpers
function OPStackBuffers_AllocateOPStackMessage(var AMessage: POPStackMessage; MTI: Word;  var Source: TNodeInfo; var Dest: TNodeInfo; IsCAN: Boolean): Boolean;
function OPStackBuffers_AllocateDatagramMessage(var AMessage: POPStackMessage; var Source: TNodeInfo; var Dest: TNodeInfo; DestFlags: Byte): Boolean;
{$IFDEF SUPPORT_STREAMS}function OPStackBuffers_AllcoateStreamMessage(var AMessage: POPStackMessage; MTI: Word;  var Source: TNodeInfo; var Dest: TNodeInfo; IsOutgoing: Boolean): Boolean;{$ENDIF}
function OPStackBuffers_Allcoate_ACDI_SNIP_Message(var AMessage: POPStackMessage; MTI: Word;  var Source: TNodeInfo; var Dest: TNodeInfo): Boolean;
function OPStackBuffers_AllocateMultiFrameMessage(var AMessage: POPStackMessage; MTI: Word;  var Source: TNodeInfo; var Dest: TNodeInfo): Boolean;
procedure OPStackBuffers_DeAllocateMessage(AMessage: POPStackMessage);

// Load Message helpers
procedure OPStackBuffers_LoadMessage(AMessage: POPStackMessage; MTI: Word; var Source: TNodeInfo; var Dest: TNodeInfo; FramingBits: Byte);
procedure OPStackBuffers_LoadOptionalInteractionRejected(AMessage: POPStackMessage; var Source: TNodeInfo; var Dest: TNodeInfo; RejectedMTI: Word; IsPermenent: Boolean);

// Zero buffer helpers
procedure OPStackBuffers_ZeroMessage(AMessage: POPStackMessage);
procedure OPStackBuffers_ZeroSimpleBuffer(ABuffer: PSimpleBuffer; ZeroArray: Boolean);
procedure OPStackBuffers_ZeroDatagramBuffer(ABuffer: PDatagramBuffer; ZeroArray: Boolean);
{$IFDEF SUPPORT_STREAMS}procedure OPStackBuffers_ZeroStreamBuffer(ABuffer: PStreamBuffer; ZeroArray: Boolean);{$ENDIF}
procedure OPStackBuffers_ZeroAcdiSnipBuffer(ABuffer: PAcdiSnipBuffer; ZeroArray: Boolean);
procedure OPStackBuffers_ZeroMultiFrameBuffer(ABuffer: PMultiFrameBuffer; ZeroArray: Boolean);

{$IFDEF SUPPORT_STREAMS}
function AllocateStreamSourceID: Byte;
function AllocateStreamDestID: Byte;
{$ENDIF}

// Copy buffer helpers
procedure OPStackBuffers_CopyData(DestData, SourceData: PSimpleBuffer);
procedure OPStackBuffers_CopyDataArray(DestData: PSImpleBuffer; SourceDataArray: PSimpleDataArray; Count: Word; ClearDestSize: Boolean);
procedure OPStackBuffers_CopyDataArrayWithSourceOffset(DestData: PSimpleBuffer; SourceDataArray: PSimpleDataArray; Count: Word; SourceOffset: Word);

{$IFNDEF FPC}
procedure OPStackBuffers_PrintBuffers;
{$ENDIF}

var
  SimpleBufferPool: TSimpleBufferPool;
  DatagramBufferPool: TDatagramBufferPool;
  {$IFDEF SUPPORT_STREAMS}
  StreamBufferPool: TStreamBufferPool;
  StreamSourceID: Byte;
  StreamDestID: Byte;
  {$ENDIF}
  AcdiSnipBufferPool: TAcdiSnipBufferPool;
  MultiFramePool: TMultiFramePool;
  OPStackMessagePool: TOPStackMessagePool;

implementation

{$IFNDEF FPC}
procedure OPStackBuffers_PrintBuffers;
begin
  UART1_Write_Text('Buffer State...'+LF);  
  
  UART1_Write_Text('Simple Buffers: '+LF);
  WordToStr(SimpleBufferPool.Count, s1);
  UART1_Write_Text('  Count: '+s1+LF);
  WordToStr(SimpleBufferPool.MaxCount, s1);
  UART1_Write_Text('  Max  : '+s1+LF);
  
  UART1_Write_Text('Datagram Buffers: '+LF);
  WordToStr(DatagramBufferPool.Count, s1);
  UART1_Write_Text('  Count: '+s1+LF);
  WordToStr(DatagramBufferPool.MaxCount, s1);
  UART1_Write_Text('  Max  : '+s1+LF);
  
  {$IFDEF SUPPORT_STREAMS}
  UART1_Write_Text('Stream Buffers: '+LF);
  WordToStr(StreamBufferPool.Count, s1);
  UART1_Write_Text('  Count: '+s1+LF);
  WordToStr(StreamBufferPool.MaxCount, s1);
  UART1_Write_Text('  Max  : '+s1+LF);
  {$ENDIF}
  
  UART1_Write_Text('ADCI/SNIP Buffers: '+LF);
  WordToStr(AcdiSnipBufferPool.Count, s1);
  UART1_Write_Text('  Count: '+s1+LF);
  WordToStr(AcdiSnipBufferPool.MaxCount, s1);
  UART1_Write_Text('  Max  : '+s1+LF);
  
  UART1_Write_Text('MultiFrame Buffers: '+LF);
  WordToStr(MultiFramePool.Count, s1);
  UART1_Write_Text('  Count: '+s1+LF);
  WordToStr(MultiFramePool.MaxCount, s1);
  UART1_Write_Text('  Max  : '+s1+LF);
  
  UART1_Write_Text('Message Pool Buffers: '+LF);
  WordToStr(OPStackMessagePool.Count, s1);
  UART1_Write_Text('  Count: '+s1+LF);
  WordToStr(OPStackMessagePool.MaxCount, s1);
  UART1_Write_Text('  Max  : '+s1+LF);
end;
{$ENDIF}


procedure OPStackBuffers_Initialize;
var
  j: Integer;
begin
  for j := 0 to USER_MAX_SIMPLE_ARRAY_BUFFERS-1  do
    OPStackBuffers_ZeroSimpleBuffer(@SimpleBufferPool.Pool[j], True);
  SimpleBufferPool.Count := 0;
  SimpleBufferPool.MaxCount := 0;

  for j := 0 to USER_MAX_DATAGRAM_ARRAY_BUFFERS-1  do
    OPStackBuffers_ZeroDatagramBuffer(@DatagramBufferPool.Pool[j], True);
  DatagramBufferPool.Count := 0;
  DatagramBufferPool.MaxCount := 0;

  for j := 0 to USER_MAX_ACDI_SNIP_ARRAY_BUFFERS-1  do
    OPStackBuffers_ZeroAcdiSnipBuffer(@AcdiSnipBufferPool.Pool[j], True);
  AcdiSnipBufferPool.Count := 0;
  AcdiSnipBufferPool.MaxCount := 0;

  {$IFDEF SUPPORT_STREAMS}
  for j := 0 to USER_MAX_STREAM_ARRAY_BUFFERS-1  do
    OPStackBuffers_ZeroStreamBuffer(@StreamBufferPool.Pool[j], True);
  StreamBufferPool.Count := 0;
  StreamBufferPool.MaxCount := 0;
  StreamSourceID := 0;
  StreamDestID := 0;
  {$ENDIF}
  
  for j := 0 to USER_MAX_MULTIFRAME_ARRAY_BUFFERS-1 do
    OPStackBuffers_ZeroMultiFrameBuffer(@MultiFramePool.Pool[j], True);
  MultiFramePool.Count := 0;
  MultiFramePool.MaxCount := 0;
  
  for j := 0 to USER_MAX_MESSAGE_ARRAY_BUFFERS-1  do
    OPStackBuffers_ZeroMessage(@OPStackMessagePool.Pool[j]);
  OPStackMessagePool.Count := 0;
  OPStackMessagePool.MaxCount := 0;
end;

procedure OPStackBuffers_Timer;
var
  i: Integer;
begin
  i := 0;
  while i < USER_MAX_MESSAGE_ARRAY_BUFFERS do
  begin
    Inc(OPStackMessagePool.Pool[i].WatchDog);
    Inc(i)
  end;
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
        if SimpleBufferPool.Count > SimpleBufferPool.MaxCount then
          SimpleBufferPool.MaxCount := SimpleBufferPool.Count;
        Result := True;
        {$IFDEF TRACK_BUFFERS}UART1_Write_Text('Alloc Simple'+LF);{$ENDIF}
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
        if DatagramBufferPool.Count > DatagramBufferPool.MaxCount then
          DatagramBufferPool.MaxCount := DatagramBufferPool.Count;
        Result := True;
        {$IFDEF TRACK_BUFFERS}UART1_Write_Text('Alloc Datagram'+LF);{$ENDIF}
        Exit;
      end;
    end;
  end;
end;

{$IFDEF SUPPORT_STREAMS}
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
        if StreamBufferPool.Count > StreamBufferPool.MaxCount then
          StreamBufferPool.MaxCount := StreamBufferPool.Count;
        Result := True;
        {$IFDEF TRACK_BUFFERS}UART1_Write_Text('Alloc Stream'+LF);{$ENDIF}
        Exit;
      end;
    end;
  end;
end;
{$ENDIF}

function AllocateAcdiSnipBuffer(var Buffer: PAcdiSnipBuffer): Boolean;
var
  i: Integer;
begin
  Result := False;
  if AcdiSnipBufferPool.Count < USER_MAX_ACDI_SNIP_ARRAY_BUFFERS then
  begin
    for i := 0 to USER_MAX_ACDI_SNIP_ARRAY_BUFFERS - 1 do
    begin
      if AcdiSnipBufferPool.Pool[i].State and ABS_ALLOCATED = 0 then
      begin
        Buffer := @AcdiSnipBufferPool.Pool[i];
        OPStackBuffers_ZeroAcdiSnipBuffer(Buffer, False);
        AcdiSnipBufferPool.Pool[i].State := ABS_ALLOCATED;
        Inc(AcdiSnipBufferPool.Count);
        if AcdiSnipBufferPool.Count > AcdiSnipBufferPool.MaxCount then
          AcdiSnipBufferPool.MaxCount := AcdiSnipBufferPool.Count;
        Result := True;
        {$IFDEF TRACK_BUFFERS}UART1_Write_Text('Alloc Snip'+LF);{$ENDIF}
        Exit;
      end;
    end;
  end;
end;

function AllocateMultiFrameBuffer(var Buffer: PMultiFrameBuffer): Boolean;
var
  i: Integer;
begin
  Result := False;
  if MultiFramePool.Count < USER_MAX_MULTIFRAME_ARRAY_BUFFERS then
  begin
    for i := 0 to USER_MAX_MULTIFRAME_ARRAY_BUFFERS - 1 do
    begin
      if MultiFramePool.Pool[i].State and ABS_ALLOCATED = 0 then
      begin
        Buffer := @MultiFramePool.Pool[i];
        OPStackBuffers_ZeroMultiFrameBuffer(Buffer, False);
        MultiFramePool.Pool[i].State := ABS_ALLOCATED;
        Inc(MultiFramePool.Count);
        if MultiFramePool.Count > MultiFramePool.MaxCount then
          MultiFramePool.MaxCount := MultiFramePool.Count;
        Result := True;
        {$IFDEF TRACK_BUFFERS}UART1_Write_Text('Alloc Multiframe'+LF);{$ENDIF}
        Exit;
      end;
    end;
  end;
end;

procedure DeAllocateSimpleBuffer(Buffer: PSimpleBuffer);
begin
  if Buffer^.State and ABS_ALLOCATED  <> 0 then                                 // Only effect the pool if the buffer was allocated from the pool
  begin
    {$IFDEF TRACK_BUFFERS}UART1_Write_Text('Dealloc Simple'+LF);{$ENDIF}
    Dec(SimpleBufferPool.Count);
    Buffer^.State := 0
  end
end;

procedure DeAllocateDatagramBuffer(Buffer: PDatagramBuffer);
begin
  if Buffer^.State and ABS_ALLOCATED <> 0 then                                  // Only effect the pool if the buffer was allocated from the pool
  begin
    {$IFDEF TRACK_BUFFERS}UART1_Write_Text('Dealloc Datagram'+LF);{$ENDIF}
    Dec(DatagramBufferPool.Count);
    Buffer^.State := 0
  end
end;

{$IFDEF SUPPORT_STREAMS}
procedure DeAllocateSteamBuffer(Buffer: PStreamBuffer);
begin
  if Buffer^.State and ABS_ALLOCATED <> 0 then                                  // Only effect the pool if the buffer was allocated from the pool
  begin
    {$IFDEF TRACK_BUFFERS}UART1_Write_Text('Dealloc Stream'+LF);{$ENDIF}
    Dec(StreamBufferPool.Count);
    Buffer^.State := 0
  end
end;
{$ENDIF}

procedure DeAllocateAcdiSnipBuffer(Buffer: PAcdiSnipBuffer);
begin
  if Buffer^.State and ABS_ALLOCATED <> 0 then                                  // Only effect the pool if the buffer was allocated from the pool
  begin
    {$IFDEF TRACK_BUFFERS}UART1_Write_Text('Dealloc Snip'+LF);{$ENDIF}
    Dec(AcdiSnipBufferPool.Count);
    Buffer^.State := 0
  end
end;

procedure DeAllocateMultiFrameBuffer(Buffer: PMultiFrameBuffer);
begin
  if Buffer^.State and ABS_ALLOCATED <> 0 then                                  // Only effect the pool if the buffer was allocated from the pool
  begin
    {$IFDEF TRACK_BUFFERS}UART1_Write_Text('Dealloc MultiFrame'+LF);{$ENDIF}
    Dec(MultiFramePool.Count);
    Buffer^.State := 0
  end
end;

function NextFreeOPStackMessage(var OPStackMessage: POPStackMessage): Boolean;
var
  i: Integer;
begin
  Result := False;
  OPStackMessage := nil;
  for i := 0 to USER_MAX_MESSAGE_ARRAY_BUFFERS - 1 do
  begin
    if OPStackMessagePool.Pool[i].MessageType and MT_ALLOCATED = 0 then
    begin
      OPStackMessage := @OPStackMessagePool.Pool[i];
      OPStackMessage^.MessageType := MT_ALLOCATED;
      OPStackBuffers_ZeroMessage(OPStackMessage);
      Inc(OPStackMessagePool.Count);
      if OPStackMessagePool.Count > OPStackMessagePool.MaxCount then
        OPStackMessagePool.MaxCount := OPStackMessagePool.Count;
      Result := True;
      Break
    end;
  end;
end;

function OPStackBuffers_AllocateOPStackMessage(var AMessage: POPStackMessage;
  MTI: Word; var Source: TNodeInfo; var Dest: TNodeInfo; IsCAN: Boolean
  ): Boolean;
var
  SimpleBuffer: PSimpleBuffer;
begin
  Result := False;
  if NextFreeOPStackMessage(AMessage) then
  begin
    AMessage^.MessageType := MT_SIMPLE or MT_ALLOCATED;
    if IsCAN then
    begin
      {$IFDEF TRACK_BUFFERS}UART1_Write_Text('CAN: ');{$ENDIF}
      AMessage^.MessageType := AMessage^.MessageType or MT_CAN_TYPE;
    end;
    if AllocateSimpleBuffer(SimpleBuffer) then
    begin
      OPStackBuffers_LoadMessage(AMessage, MTI, Source, Dest, 0);
      AMessage^.Buffer := PSimpleBuffer( SimpleBuffer);
      Result := True
    end else
    begin
      OPStackBuffers_DeAllocateMessage(AMessage);
      AMessage := nil;
    end;
  end
end;

function OPStackBuffers_AllocateDatagramMessage(var AMessage: POPStackMessage;
  var Source: TNodeInfo; var Dest: TNodeInfo; DestFlags: Byte): Boolean;
var
  DatagramBuffer: PDatagramBuffer;
begin
  Result := False;
  if NextFreeOPStackMessage(AMessage) then
  begin
    AMessage^.MessageType := MT_DATAGRAM or MT_ALLOCATED;
    if AllocateDatagramBuffer(DatagramBuffer) then
    begin
      OPStackBuffers_LoadMessage(AMessage, MTI_DATAGRAM, Source, Dest, DestFlags);
      AMessage^.Buffer := PSimpleBuffer( PByte( DatagramBuffer));
      Result := True
    end else
    begin
      OPStackBuffers_DeAllocateMessage(AMessage);
      AMessage := nil;
    end;
  end;
end;

{$IFDEF SUPPORT_STREAMS}
function OPStackBuffers_AllcoateStreamMessage(var AMessage: POPStackMessage; MTI: Word; SourceNodeAlias: Word; var SourceNodeID: TNodeID; DestAlias: Word; var DestNodeID: TNodeID; IsOutgoing: Boolean): Boolean;
var
  StreamBuffer: PStreamBuffer;
begin
  Result := False;
  if NextFreeOPStackMessage(AMessage) then
  begin
    AMessage^.MessageType := MT_STREAM or MT_ALLOCATED;
    if AllocateStreamBuffer(StreamBuffer) then
    begin
      OPStackBuffers_LoadMessage(AMessage, MTI, SourceNodeAlias, SourceNodeID, DestAlias, DestNodeID, 0);
      if IsOutgoing then
        StreamBuffer^.State := StreamBuffer^.State or ABS_STREAM_OUTGOING;
      AMessage^.Buffer := PSimpleBuffer( PByte( StreamBuffer));
      Result := True
    end else
    begin
      OPStackBuffers_DeAllocateMessage(AMessage);
      AMessage := nil;
    end;
  end;
end;
{$ENDIF}

function OPStackBuffers_Allcoate_ACDI_SNIP_Message(
  var AMessage: POPStackMessage; MTI: Word; var Source: TNodeInfo;
  var Dest: TNodeInfo): Boolean;
var
  AcdiSnipBuffer: PAcdiSnipBuffer;
begin
  Result := False;
  if NextFreeOPStackMessage(AMessage) then
  begin
    AMessage^.MessageType := MT_ACDISNIP or MT_ALLOCATED;
    if AllocateAcdiSnipBuffer(AcdiSnipBuffer) then
    begin
      OPStackBuffers_LoadMessage(AMessage, MTI, Source, Dest, 0);
      AMessage^.Buffer := PSimpleBuffer( PByte( AcdiSnipBuffer));
      Result := True
    end else
    begin
      OPStackBuffers_DeAllocateMessage(AMessage);
      AMessage := nil;
    end;
  end;
end;

function OPStackBuffers_AllocateMultiFrameMessage(
  var AMessage: POPStackMessage; MTI: Word; var Source: TNodeInfo;
  var Dest: TNodeInfo): Boolean;
var
  MultiFrameBuffer: PMultiFrameBuffer;
begin
  Result := False;
  if NextFreeOPStackMessage(AMessage) then
  begin
    AMessage^.MessageType := MT_MULTIFRAME or MT_ALLOCATED;
    if AllocateMultiFrameBuffer(MultiFrameBuffer) then
    begin
      OPStackBuffers_LoadMessage(AMessage, MTI, Source, Dest, 0);
      AMessage^.Buffer := PSimpleBuffer( PByte( MultiFrameBuffer));
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
  if AMessage = nil then Exit;
  if AMessage^.MessageType and MT_ALLOCATED <> 0 then                           // Only deallocate if we allocated it
  begin
    if AMessage^.Buffer <> nil then
    begin
      case (AMessage^.MessageType and MT_MASK) of
        MT_SIMPLE    : DeAllocateSimpleBuffer(PSimpleBuffer( AMessage^.Buffer));
        MT_DATAGRAM  : DeAllocateDatagramBuffer(PDatagramBuffer( PByte(AMessage^.Buffer)));
        {$IFDEF SUPPORT_STREAMS}
        MT_STREAM    : DeAllocateSteamBuffer(PStreamBuffer( PByte( AMessage^.Buffer)));
        {$ENDIF}
        MT_ACDISNIP  : DeAllocateAcdiSnipBuffer(PAcdiSnipBuffer( PByte( AMessage^.Buffer)));
        MT_MULTIFRAME : DeAllocateMultiFrameBuffer(PMultiFrameBuffer( PByte( AMessage^.Buffer)));
      end;
    end;
    AMessage^.MessageType := MT_UNALLOCATED;
    Dec(OPStackMessagePool.Count);
  end;
end;

procedure OPStackBuffers_LoadOptionalInteractionRejected(
  AMessage: POPStackMessage; var Source: TNodeInfo; var Dest: TNodeInfo;
  RejectedMTI: Word; IsPermenent: Boolean);
begin
  OPStackBuffers_LoadMessage(AMessage, MTI_OPTIONAL_INTERACTION_REJECTED, Source, Dest, 0);
  AMessage^.MessageType := MT_SIMPLE;
  AMessage^.Buffer^.DataBufferSize := 4;
  if IsPermenent then
  begin
    AMessage^.Buffer^.DataArray[0] := $20;
    AMessage^.Buffer^.DataArray[1] := $00;
  end else
  begin
    AMessage^.Buffer^.DataArray[0] := $10;
    AMessage^.Buffer^.DataArray[1] := $00;
  end;
  AMessage^.Buffer^.DataArray[2] := Hi( RejectedMTI);
  AMessage^.Buffer^.DataArray[3] := Lo( RejectedMTI);
end;

procedure ZeroBuffer(ABuffer: PSimpleBuffer; DataBufferSize: Word);
var
  i: Integer;
  BufferPtr: PByte;
begin
  ABuffer^.State := 0;
  ABuffer^.DataBufferSize := 0;
  BufferPtr := @ABuffer^.DataArray[0];
  for i := 0 to DataBufferSize - 1 do
  begin
    BufferPtr^ := 0;
    Inc(BufferPtr);
  end;
end;

procedure OPStackBuffers_ZeroSimpleBuffer(ABuffer: PSimpleBuffer; ZeroArray: Boolean);
var
  i: Integer;
begin
  ABuffer^.State := 0;
  ABuffer^.DataBufferSize := 0;
  if ZeroArray then
  begin
    for i := 0 to MAX_SIMPLE_BYTES - 1 do
      ABuffer^.DataArray[i] := 0;
  end;
end;

procedure OPStackBuffers_ZeroDatagramBuffer(ABuffer: PDatagramBuffer; ZeroArray: Boolean);
var
  i: Integer;
begin
  if ZeroArray then
  begin
    for i := 0 to MAX_DATAGRAM_BYTES - 1 do
      ABuffer^.DataArray[i] := 0;
  end;
  ABuffer^.State := 0;
  ABuffer^.DataBufferSize := 0;
  ABuffer^.CurrentCount := 0;
  ABuffer^.ResendCount := 0;
  ABuffer^.iStateMachine := 0;
end;

{$IFDEF SUPPORT_STREAMS}
procedure OPStackBuffers_ZeroStreamBuffer(ABuffer: PStreamBuffer; ZeroArray: Boolean);
var
  i: Integer;
begin
  if ZeroArray then
  begin
    for i := 0 to USER_MAX_STREAM_BYTES - 1 do
      ABuffer^.DataArray[i] := 0;
  end;
  ABuffer^.State := 0;
  ABuffer^.DataBufferSize := 0;
  ABuffer^.CurrentCount := 0;
  ABuffer^.SourceStreamID := 0;
  ABuffer^.DestStreamID := 0;
  for i := 0 to MAX_STREAM_TYPE_ID - 1 do
    ABuffer^.StreamTypeID[i] := 0;
  ABuffer^.TotalMessageSize := 0;
  ABuffer^.NextActiveStream := nil;
  ABuffer^.NegotiatedBufferSize := USER_MAX_STREAM_BYTES;
end;
{$ENDIF}

procedure OPStackBuffers_ZeroAcdiSnipBuffer(ABuffer: PAcdiSnipBuffer; ZeroArray: Boolean);
var
  i: Integer;
begin
  if ZeroArray then
  begin
    for i := 0 to USER_MAX_ACDI_SNIP_BYTES - 1 do
      ABuffer^.DataArray[i] := 0;
  end;
  ABuffer^.State := 0;
  ABuffer^.DataBufferSize := 0;
  ABuffer^.CurrentCount := 0;
end;

procedure OPStackBuffers_ZeroMultiFrameBuffer(ABuffer: PMultiFrameBuffer; ZeroArray: Boolean);
var
  i: Integer;
begin
  if ZeroArray then
  begin
    for i := 0 to USER_MAX_MULTI_FRAME_BYTES - 1 do
      ABuffer^.DataArray[i] := 0;
  end;
  ABuffer^.State := 0;
  ABuffer^.DataBufferSize := 0;
  ABuffer^.CurrentCount := 0;
end;

procedure OPStackBuffers_LoadMessage(AMessage: POPStackMessage; MTI: Word;
  var Source: TNodeInfo; var Dest: TNodeInfo; FramingBits: Byte);
begin
  AMessage^.MTI := MTI;
  AMessage^.Dest := Dest;
  AMessage^.Source := Source;
  AMessage^.FramingBits := FramingBits;
end;

procedure OPStackBuffers_ZeroMessage(AMessage: POPStackMessage);
begin
  AMessage^.MessageType := 0;
  AMessage^.MTI := 0;
  AMessage^.NextIncoming := nil;
  AMessage^.NextOutgoing := nil;
  AMessage^.Dest.AliasID := 0;
  AMessage^.Dest.ID := NULL_NODE_ID;
  AMessage^.Source.AliasID := 0;
  AMessage^.Source.ID := NULL_NODE_ID;
  AMessage^.Buffer := nil;
  AMessage^.FramingBits := 0;
  AMessage^.WatchDog := 0;
end;

{$IFDEF SUPPORT_STREAMS}
function AllocateStreamSourceID: Byte;
begin
  if StreamSourceID = 0 then
    Inc(StreamSourceID);
  Result := StreamSourceID;
  Inc(StreamSourceID);
end;

function AllocateStreamDestID: Byte;
begin
  if StreamDestID = 0 then
    Inc(StreamDestID);
  Result := StreamDestID;
  Inc(StreamDestID);
end;
{$ENDIF}

procedure OPStackBuffers_CopyData(DestData, SourceData: PSimpleBuffer);
var
  i: Integer;
begin
  i := 0;
  DestData^.DataBufferSize := SourceData^.DataBufferSize;
  while i < SourceData^.DataBufferSize do
  begin
    DestData^.DataArray[i] := SourceData^.DataArray[i];
    Inc(i)
  end
end;

procedure OPStackBuffers_CopyDataArray(DestData: PSImpleBuffer; SourceDataArray: PSimpleDataArray; Count: Word; ClearDestSize: Boolean);
var
  i: Integer;
begin
  if ClearDestSize then
    DestData^.DataBufferSize := 0;
  i := 0;
  while i < Count do
  begin
    DestData^.DataArray[DestData^.DataBufferSize] := SourceDataArray^[i];
    Inc(DestData^.DataBufferSize);
    Inc(i)
  end
end;

procedure OPStackBuffers_CopyDataArrayWithSourceOffset(DestData: PSimpleBuffer; SourceDataArray: PSimpleDataArray; Count: Word; SourceOffset: Word);
begin
  DestData^.DataBufferSize := 0;
  while DestData^.DataBufferSize < (Count - SourceOffset) do
  begin
    DestData^.DataArray[DestData^.DataBufferSize] := SourceDataArray^[DestData^.DataBufferSize+SourceOffset];
    Inc(DestData^.DataBufferSize);
  end
end;

end.
