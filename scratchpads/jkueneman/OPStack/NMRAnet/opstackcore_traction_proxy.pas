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
  opstackbuffers,
  opstacktypes;

procedure TractionProxyProtocolMessage(DestNode: PNMRAnetNode; AMessage: POPStackMessage; IsReply: Boolean);
function TractionProxyProtocolReplyHandler(DestNode: PNMRAnetNode; var MessageToSend: POPStackMessage; NextMessage: POPStackMessage): Boolean;
procedure TractionProxyProtocolReply(DestNode: PNMRAnetNode; AMessage: POPStackMessage);

implementation

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
{$IFDEF SUPPORT_TRACTION_PROXY}
var
  Flag: Byte;
{$ENDIF}
begin
  Result := False;
  MessageToSend := nil;
  {$IFDEF SUPPORT_TRACTION_PROXY}
  if NextMessage^.Buffer^.DataArray[0] = TRACTION_PROXY_MANAGE then
  begin
    if NextMessage^.Buffer^.DataArray[1] = TRACTION_PROXY_MANAGE_RESERVE then
    begin
      Flag := $FF;   // Fail
      if (DestNode^.ProxyData.Lock.AliasID = 0) then
            if (DestNode^.ProxyData.Lock.AliasID = 0) then
              if (DestNode^.ProxyData.Lock.AliasID = 0) then
                Flag := 0;   // OK
      if TrySendTractionProxyManageReply(NextMessage^.Dest, NextMessage^.Source, Flag) then
      begin
        DestNode^.ProxyData.Lock.AliasID := NextMessage^.Source.AliasID;
        DestNode^.ProxyData.Lock.ID[0] := NextMessage^.Source.ID[0];
        DestNode^.ProxyData.Lock.ID[1] := NextMessage^.Source.ID[1];
      end
    end else
    begin
      if (DestNode^.ProxyData.Lock.AliasID = NextMessage^.Source.AliasID) then
        if (DestNode^.ProxyData.Lock.ID[0] = NextMessage^.Source.ID[0]) then
          if (DestNode^.ProxyData.Lock.ID[1] = NextMessage^.Source.ID[1]) then
          begin
            DestNode^.ProxyData.Lock.AliasID := 0;
            DestNode^.ProxyData.Lock.ID[0] := 0;
            DestNode^.ProxyData.Lock.ID[1] := 0;
          end;
    end
  end else
  begin
    // This a multi-frame messages so it has an allocated buffer
  end;
   Result := UnLinkDeAllocateAndTestForMessageToSend(DestNode, MessageToSend, NextMessage);
  {$ENDIF}
end;

procedure TractionProxyProtocolReply(DestNode: PNMRAnetNode; AMessage: POPStackMessage);
begin
  AppCallback_TractionProxyProtocol(DestNode, AMessage);
  UnLinkDeAllocateAndTestForMessageToSend(DestNode, nil, AMessage);
end;

end.