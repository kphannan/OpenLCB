unit opstack_api;

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  {$IFDEF FPC}
  Classes,
  SysUtils,
  FileUtil,
  {$ENDIF}
  nmranetdefines,
  opstackbuffers,
  template_hardware,
  opstacktypes,
  opstackdefines,
  template_node;

function GetPhysicalNode: PNMRAnetNode;
function GetVirtualNode(Index: Integer): PNMRAnetNode;

function TrySendVerifyNodeIDAddressed(var Source: TNodeInfo; var Dest: TNodeInfo): Boolean;
function TrySendVerifyNodeIDGlobal(var Source: TNodeInfo): Boolean;
function TrySendSupportInquiry(var Source: TNodeInfo; var Dest: TNodeInfo): Boolean;
function TrySendIdentifyConsumer(var Source: TNodeInfo; EventID: PEventID): Boolean;
function TrySendIdentifyProducer(var Source: TNodeInfo; EventID: PEventID): Boolean;
function TrySendIdentifyEventsAddressed(var Source: TNodeInfo; var Dest: TNodeInfo): Boolean;
function TrySendIdentifyEventsGlobal(var Source: TNodeInfo): Boolean;
function TrySendLearnEvent(var Source: TNodeInfo; EventID: PEventID): Boolean;
function TrySendPCEventReport(var Source: TNodeInfo; EventID: PEventID): Boolean;
function TrySendRemoteButtonRequest(var Source: TNodeInfo; var Dest: TNodeInfo; DataBuffer: PSimpleBuffer): Boolean;
function TrySendSnipRequest(var Source: TNodeInfo; var Dest: TNodeInfo; AcdiSnipBuffer: PAcdiSnipBuffer): Boolean;

{$IFDEF SUPPORT_TRACTION}
function TrySendStnipRequest(var Source: TNodeInfo; var Dest: TNodeInfo; AcdiSnipBuffer: PAcdiSnipBuffer): Boolean;
function TrySendTractionFunctionSet(var SourceNode: PNMRAnetNode; var DestNode: PNMRAnetNode; FunctionAddress: DWord; Value: Word): Boolean;
function TrySendTractionSpeedSet(var SourceNode: PNMRAnetNode; var DestNode: PNMRAnetNode; Speed: Byte): Boolean;
function TrySendTractionDirectionSet(var SourceNode: PNMRAnetNode; var DestNode: PNMRAnetNode; IsForward: Boolean): Boolean;
{$ENDIF}

{$IFNDEF FPC}
var
  NodePool: TNodePool;  external;
{$ENDIF}

implementation

{$IFDEF FPC}
uses
  opstacknode;
{$ENDIF}

function GetPhysicalNode: PNMRAnetNode;
begin
  Result := @NodePool.Pool[0];
end;

function GetVirtualNode(Index: Integer): PNMRAnetNode;
begin
  Result := nil;
  if Index < 0 then
    Result := @NodePool.Pool[Index+1];
end;

function TrySendVerifyNodeIDAddressed(var Source: TNodeInfo; var Dest: TNodeInfo): Boolean;
var
  NewMessage: POPStackMessage;
begin
  Result := False;
  NewMessage := nil;
  if IsOutgoingBufferAvailable then
    if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI_VERIFY_NODE_ID_NUMBER_DEST, Source.AliasID, Source.ID, Dest.AliasID, Dest.ID) then
    begin
      OutgoingMessage(NewMessage);
      Result := True;
    end
end;

function TrySendVerifyNodeIDGlobal(var Source: TNodeInfo): Boolean;
var
  NewMessage: POPStackMessage;
begin
  Result := False;
  NewMessage := nil;
  // Does not matter we loaded Source into the destination, the loader will see the MTI does not have a dest and ignore it
  if IsOutgoingBufferAvailable then
    if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI_VERIFY_NODE_ID_NUMBER, Source.AliasID, Source.ID, Source.AliasID, Source.ID) then
    begin
      OutgoingMessage(NewMessage);
      Result := True;
    end
end;

function TrySendSupportInquiry(var Source: TNodeInfo; var Dest: TNodeInfo): Boolean;
var
  NewMessage: POPStackMessage;
begin
  Result := False;
  NewMessage := nil;
  // Does not matter we loaded Source into the destination, the loader will see the MTI does not have a dest and ignore it
  if IsOutgoingBufferAvailable then
    if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI_PROTOCOL_SUPPORT_INQUIRY, Source.AliasID, Source.ID, Dest.AliasID, Dest.ID) then
    begin
      OutgoingMessage(NewMessage);
      Result := True;
    end
end;

function TrySendIdentifyConsumer(var Source: TNodeInfo; EventID: PEventID): Boolean;
var
  NewMessage: POPStackMessage;
begin
  Result := False;
  NewMessage := nil;
  // Does not matter we loaded Source into the destination, the loader will see the MTI does not have a dest and ignore it
  if IsOutgoingBufferAvailable then
    if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI_CONSUMER_IDENTIFY, Source.AliasID, Source.ID, Source.AliasID, Source.ID) then
    begin
      NewMessage^.Buffer^.DataArray := EventID^;
      NewMessage^.Buffer^.DataBufferSize := 8;
      OutgoingMessage(NewMessage);
      Result := True;
    end
end;

function TrySendIdentifyProducer(var Source: TNodeInfo; EventID: PEventID): Boolean;
var
  NewMessage: POPStackMessage;
begin
  Result := False;
  NewMessage := nil;
  // Does not matter we loaded Source into the destination, the loader will see the MTI does not have a dest and ignore it
  if IsOutgoingBufferAvailable then
    if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI_PRODUCER_IDENTIFY, Source.AliasID, Source.ID, Source.AliasID, Source.ID) then
    begin
      NewMessage^.Buffer^.DataArray := EventID^;
      NewMessage^.Buffer^.DataBufferSize := 8;
      OutgoingMessage(NewMessage);
      Result := True;
    end
end;

function TrySendIdentifyEventsAddressed(var Source: TNodeInfo; var Dest: TNodeInfo): Boolean;
var
  NewMessage: POPStackMessage;
begin
  Result := False;
  NewMessage := nil;
  // Does not matter we loaded Source into the destination, the loader will see the MTI does not have a dest and ignore it
  if IsOutgoingBufferAvailable then
    if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI_EVENTS_IDENTIFY_DEST, Source.AliasID, Source.ID, Dest.AliasID, Dest.ID) then
    begin
      OutgoingMessage(NewMessage);
      Result := True;
    end
end;

function TrySendIdentifyEventsGlobal(var Source: TNodeInfo): Boolean;
var
  NewMessage: POPStackMessage;
begin
  Result := False;
  NewMessage := nil;
  // Does not matter we loaded Source into the destination, the loader will see the MTI does not have a dest and ignore it
  if IsOutgoingBufferAvailable then
    if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI_EVENTS_IDENTIFY, Source.AliasID, Source.ID, Source.AliasID, Source.ID) then
    begin
      OutgoingMessage(NewMessage);
      Result := True;
    end
end;

function TrySendLearnEvent(var Source: TNodeInfo; EventID: PEventID): Boolean;
var
  NewMessage: POPStackMessage;
begin
  Result := False;
  NewMessage := nil;
  // Does not matter we loaded Source into the destination, the loader will see the MTI does not have a dest and ignore it
  if IsOutgoingBufferAvailable then
    if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI_EVENT_LEARN, Source.AliasID, Source.ID, Source.AliasID, Source.ID) then
    begin
      NewMessage^.Buffer^.DataArray := EventID^;
      NewMessage^.Buffer^.DataBufferSize := 8;
      OutgoingMessage(NewMessage);
      Result := True;
    end
end;

function TrySendPCEventReport(var Source: TNodeInfo; EventID: PEventID): Boolean;
var
  NewMessage: POPStackMessage;
begin
  Result := False;
  NewMessage := nil;
  // Does not matter we loaded Source into the destination, the loader will see the MTI does not have a dest and ignore it
  if IsOutgoingBufferAvailable then
    if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI_PC_EVENT_REPORT, Source.AliasID, Source.ID, Source.AliasID, Source.ID) then
    begin
      NewMessage^.Buffer^.DataArray := EventID^;
      NewMessage^.Buffer^.DataBufferSize := 8;
      OutgoingMessage(NewMessage);
      Result := True;
    end
end;

function TrySendRemoteButtonRequest(var Source: TNodeInfo; var Dest: TNodeInfo; DataBuffer: PSimpleBuffer): Boolean;
var
  NewMessage: POPStackMessage;
begin
  Result := False;
  NewMessage := nil;
  // Does not matter we loaded Source into the destination, the loader will see the MTI does not have a dest and ignore it
  if IsOutgoingBufferAvailable then
    if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI_REMOTE_BUTTON_REQUEST, Source.AliasID, Source.ID, Dest.AliasID, Dest.ID) then
    begin
      NewMessage^.Buffer^.DataArray := DataBuffer^.DataArray;
      NewMessage^.Buffer^.DataBufferSize := DataBuffer^.DataBufferSize;
      OutgoingMessage(NewMessage);
      Result := True;
    end
end;

function TrySendSnipRequest(var Source: TNodeInfo; var Dest: TNodeInfo; AcdiSnipBuffer: PAcdiSnipBuffer): Boolean;
var
  NewMessage: POPStackMessage;
begin
  Result := False;
  NewMessage := nil;
  // Does not matter we loaded Source into the destination, the loader will see the MTI does not have a dest and ignore it
  if IsOutgoingBufferAvailable then
    if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI_SIMPLE_NODE_INFO_REQUEST, Source.AliasID, Source.ID, Dest.AliasID, Dest.ID) then
    begin
      OutgoingMessage(NewMessage);
      Result := True;
    end
end;

{$IFDEF SUPPORT_TRACTION}

function TrySendStnipRequest(var Source: TNodeInfo; var Dest: TNodeInfo; AcdiSnipBuffer: PAcdiSnipBuffer): Boolean;
var
  NewMessage: POPStackMessage;
begin
  Result := False;
  NewMessage := nil;
  // Does not matter we loaded Source into the destination, the loader will see the MTI does not have a dest and ignore it
  if IsOutgoingBufferAvailable then
    if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI_SIMPLE_TRAIN_NODE_INFO_REQUEST, Source.AliasID, Source.ID, Dest.AliasID, Dest.ID) then
    begin
      OutgoingMessage(NewMessage);
      Result := True;
    end
end;

function TrySendTractionFunctionSet(var SourceNode: PNMRAnetNode; var DestNode: PNMRAnetNode; FunctionAddress: DWord; Value: Word): Boolean;
var
  NewMessage: POPStackMessage;
begin
  Result := False;
  NewMessage := nil;
  if IsOutgoingBufferAvailable then
    if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI_TRACTION_PROTOCOL, SourceNode^.Info.AliasID, SourceNode^.Info.ID, DestNode^.Info.AliasID, DestNode^.Info.ID) then
    begin
      OutgoingMessage(NewMessage);
      Result := True;
    end
end;

function TrySendTractionSpeedSet(var SourceNode: PNMRAnetNode; var DestNode: PNMRAnetNode; Speed: Byte): Boolean;
var
  NewMessage: POPStackMessage;
begin
  Result := False;
  NewMessage := nil;
  if IsOutgoingBufferAvailable then
    if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI_TRACTION_PROTOCOL, SourceNode^.Info.AliasID, SourceNode^.Info.ID, DestNode^.Info.AliasID, DestNode^.Info.ID) then
    begin
      OutgoingMessage(NewMessage);
      Result := True;
    end
end;

function TrySendTractionDirectionSet(var SourceNode: PNMRAnetNode; var DestNode: PNMRAnetNode; IsForward: Boolean): Boolean;
var
  NewMessage: POPStackMessage;
begin
  Result := False;
  NewMessage := nil;
  if IsOutgoingBufferAvailable then
    if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI_TRACTION_PROTOCOL, SourceNode^.Info.AliasID, SourceNode^.Info.ID, DestNode^.Info.AliasID, DestNode^.Info.ID) then
    begin
      OutgoingMessage(NewMessage);
      Result := True;
    end
end;
{$ENDIF}

end.

