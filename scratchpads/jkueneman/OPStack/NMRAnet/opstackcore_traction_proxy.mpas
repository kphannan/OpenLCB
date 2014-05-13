unit opstackcore_traction_proxy;

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  {$IFNDEF FPC}
  NMRAnetDCC,
  {$ENDIF}
  opstack_api,
  template_hardware,
  opstacknode,
  opstackcore_basic,
  nmranetdefines,
  opstackdefines,
  template_userstatemachine,
  nmranetutilities,
  opstackbuffers,
  opstacktypes;

procedure TractionProxyProtocolMessage(DestNode: PNMRAnetNode; AMessage: POPStackMessage; IsReply: Boolean);
function TractionProxyProtocolReplyHandler(DestNode: PNMRAnetNode; var MessageToSend: POPStackMessage; NextMessage: POPStackMessage): Boolean;
procedure TractionProxyProtocolReply(DestNode: PNMRAnetNode; AMessage: POPStackMessage);

implementation

function TractionProxyProtocolManage(DestNode: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage): Boolean;
begin
  Result := False;
  MessageToSend := nil;
  if NextMessage^.Buffer^.DataArray[1] = TRACTION_MANAGE_RESERVE then
  begin
    if OPStackBuffers_AllocateOPStackMessage(MessageToSend, MTI_TRACTION_PROXY_REPLY, NextMessage^.Dest.AliasID, NextMessage^.Dest.ID, NextMessage^.Source.AliasID, NextMessage^.Source.ID) then
    begin
      MessageToSend^.Buffer^.DataBufferSize := 3;
      MessageToSend^.Buffer^.DataArray[0] := TRACTION_PROXY_MANAGE;
      MessageToSend^.Buffer^.DataArray[1] := TRACTION_PROXY_MANAGE_RESERVE;
      if NMRAnetUtilities_NullNodeIDInfo(DestNode^.TrainProxyData.Lock) or NMRAnetUtilities_EqualNodeIDInfo(DestNode^.TrainProxyData.Lock, NextMessage^.Source) then
      begin
        MessageToSend^.Buffer^.DataArray[2] := TRACTION_PROXY_MANAGE_RESERVE_REPLY_OK;
        DestNode^.TrainData.Lock.AliasID := NextMessage^.Source.AliasID;
        DestNode^.TrainData.Lock.ID[0] := NextMessage^.Source.ID[0];
        DestNode^.TrainData.Lock.ID[1] := NextMessage^.Source.ID[1];
      end else
        MessageToSend^.Buffer^.DataArray[2] := TRACTION_PROXY_MANAGE_RESERVE_REPLY_FAIL;
      Result := UnLinkDeAllocateAndTestForMessageToSend(DestNode, MessageToSend, NextMessage);
    end
  end else
  begin
    if NMRAnetUtilities_EqualNodeIDInfo(DestNode^.TrainProxyData.Lock, NextMessage^.Source) then
    begin
      DestNode^.TrainData.Lock.AliasID := 0;
      DestNode^.TrainData.Lock.ID[0] := 0;
      DestNode^.TrainData.Lock.ID[1] := 0;
    end;
    Result := UnLinkDeAllocateAndTestForMessageToSend(DestNode, MessageToSend, NextMessage);
  end;
end;

//******************************************************************************
// procedure TractionProxyProtocol
// Parameters:
//    AMessage: The incoming OPStack Message
//    DestNode: The node the message is meant for
// Description:
//    Takes incoming Traction Protocol and posts it to be disected and handled
//    later in the Reply
//******************************************************************************
procedure TractionProxyProtocolMessage(DestNode: PNMRAnetNode; AMessage: POPStackMessage; IsReply: Boolean);
var
  NewMessage: POPStackMessage;
  MTI: Word;
begin
  NewMessage := nil;
  if IsReply then
  begin
    MTI := MTI_TRACTION_PROXY_REPLY;
    if OPStackBuffers_AllocateMultiFrameMessage(NewMessage, MTI, AMessage^.Source.AliasID, AMessage^.Source.ID, AMessage^.Dest.AliasID, AMessage^.Dest.ID) then
    begin
      OPStackBuffers_CopyData(NewMessage^.Buffer, AMessage^.Buffer);
      OPStackNode_IncomingMessageLink(DestNode, NewMessage)
    end;
  end else
  begin
    MTI := MTI_TRACTION_PROXY_PROTOCOL;
    if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI, AMessage^.Source.AliasID, AMessage^.Source.ID, AMessage^.Dest.AliasID, AMessage^.Dest.ID) then
    begin
      OPStackBuffers_CopyData(NewMessage^.Buffer, AMessage^.Buffer);
      OPStackNode_IncomingMessageLink(DestNode, NewMessage)
    end else
      OptionalInteractionRejected(AMessage, False);                            // Try again if you wish
  end;
end;

function TractionProxyProtocolReplyHandler(DestNode: PNMRAnetNode; var MessageToSend: POPStackMessage; NextMessage: POPStackMessage): Boolean;
begin
  Result := False;
  MessageToSend := nil;
  {$IFDEF SUPPORT_TRACTION_PROXY}
  case NextMessage^.Buffer^.DataArray[0] of
    TRACTION_PROXY_MANAGE : begin Result := TractionProxyProtocolManage(DestNode, MessageToSend, NextMessage); Exit; end
  else begin
    AppCallback_TractionProxyProtocol(DestNode, NextMessage, NMRAnetUtilities_EqualNodeIDInfo(DestNode^.Info, NextMessage^.Source));
    Result := UnLinkDeAllocateAndTestForMessageToSend(DestNode, MessageToSend, NextMessage);
    end;
  end;
  {$ENDIF}
end;

procedure TractionProxyProtocolReply(DestNode: PNMRAnetNode; AMessage: POPStackMessage);
begin
  AppCallback_TractionProxyProtocolReply(DestNode, AMessage);
  UnLinkDeAllocateAndTestForMessageToSend(DestNode, nil, AMessage);
end;

end.