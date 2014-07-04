unit opstackcore_traction_proxy;

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  {$IFNDEF FPC}
  {$IFDEF SUPPORTS_DCC}
  NMRAnetDCC,
  {$ENDIF}
  {$ENDIF}
  opstacknode,
  opstackcore_basic,
  nmranetdefines,
  opstackdefines,
  template_userstatemachine,
  nmranetutilities,
  opstackbuffers;

procedure TractionProxyProtocolMessage(DestNode: PNMRAnetNode; AMessage: POPStackMessage);
function TractionProxyProtocolHandler(DestNode: PNMRAnetNode; var MessageToSend: POPStackMessage; NextMessage: POPStackMessage): Boolean;
procedure TractionProxyProtocolReply(DestNode: PNMRAnetNode; AMessage: POPStackMessage);

implementation

function TractionProxyProtocolManage(DestNode: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage): Boolean;
begin
  Result := False;
  MessageToSend := nil;
  if NextMessage^.Buffer^.DataArray[1] = TRACTION_MANAGE_RESERVE then
  begin
    if OPStackBuffers_AllocateOPStackMessage(MessageToSend, MTI_TRACTION_PROXY_REPLY, NextMessage^.Dest, NextMessage^.Source, False) then
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
procedure TractionProxyProtocolMessage(DestNode: PNMRAnetNode; AMessage: POPStackMessage);
begin
  OPStackNode_IncomingMessageLink(DestNode, AMessage)
end;

function TractionProxyProtocolHandler(DestNode: PNMRAnetNode; var MessageToSend: POPStackMessage; NextMessage: POPStackMessage): Boolean;
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