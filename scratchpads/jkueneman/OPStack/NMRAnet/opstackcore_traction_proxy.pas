unit opstackcore_traction_proxy;

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  {$IFNDEF FPC}
  NMRAnetDCC,
  {$ENDIF}
  template_hardware,
  opstacknode,
  opstackcore_basic,
  nmranetdefines,
  opstackdefines,
  opstackbuffers,
  opstacktypes;

procedure TractionProxyProtocol(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
procedure TractionProxyProtocolReply(AMessage: POPStackMessage; DestNode: PNMRAnetNode);

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
procedure TractionProxyProtocol(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
var
  NewMessage: POPStackMessage;
begin
  NewMessage := nil;
  if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI_TRACTION_PROXY_PROTOCOL, AMessage^.Source.AliasID, AMessage^.Source.ID, AMessage^.Dest.AliasID, AMessage^.Dest.ID) then
  begin
    OPStackBuffers_CopyData(NewMessage^.Buffer, AMessage^.Buffer);
    OPStackNode_IncomingMessageLink(DestNode, NewMessage)
  end else
    OptionalInteractionRejected(AMessage, False);                            // Try again if you wish
end;

procedure TractionProxyProtocolReply(Node: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage);
begin
  {$IFDEF SUPPORT_TRACTION_PROXY}
  if NextMessage^.Buffer^.DataArray[0] = TRACTION_PROXY_MANAGE then
  begin
    if NextMessage^.Buffer^.DataArray[1] = TRACTION_PROXY_MANAGE_RESERVE then
    begin
      if OPStackBuffers_AllocateOPStackMessage(MessageToSend, MTI_TRACTION_PROXY_REPLY, NextMessage^.Dest.AliasID, NextMessage^.Dest.ID, NextMessage^.Source.AliasID, NextMessage^.Source.ID) then
      begin
        // Only reserve if the node is not locked
        MessageToSend^.Buffer^.DataBufferSize := 3;
        MessageToSend^.Buffer^.DataArray[0] := TRACTION_PROXY_MANAGE;
        MessageToSend^.Buffer^.DataArray[1] := TRACTION_PROXY_MANAGE_RESERVE;
        MessageToSend^.Buffer^.DataArray[2] := $FF;   // Fail
        if (Node^.ProxyData.Lock.AliasID = 0) then
          if (Node^.ProxyData.Lock.AliasID = 0) then
            if (Node^.ProxyData.Lock.AliasID = 0) then
              MessageToSend^.Buffer^.DataArray[2] := 0;   // OK
      end
    end else
    begin
      if (Node^.ProxyData.Lock.AliasID = NextMessage^.Source.AliasID) then
        if (Node^.ProxyData.Lock.ID[0] = NextMessage^.Source.ID[0]) then
          if (Node^.ProxyData.Lock.ID[1] = NextMessage^.Source.ID[1]) then
          begin
            Node^.ProxyData.Lock.AliasID := 0;
            Node^.ProxyData.Lock.ID[0] := 0;
            Node^.ProxyData.Lock.ID[1] := 0;
          end;
    end
  end else
  begin
    // This a multi-frame messages so it has an allocated buffer
  end
  {$ENDIF}
end;

end.