unit opstackcore_traction_proxy;

{$IFDEF FPC}
interface
{$ENDIF}

uses
  {$IFNDEF FPC}
  NMRAnetDCC,
  {$ENDIF}
  template_hardware,
  Float16,
  opstacknode,
  opstackcore_basic,
  nmranetdefines,
  opstackdefines,
  opstackbuffers,
  opstacktypes;

procedure TractionProxyProtocol(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
procedure TractionProxyProtocolReply(Node: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage);

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
    OptionalInteractionRejected(AMessage, DestNode, False);                            // Try again if you wish
end;

procedure TractionProxyProtocolReply(Node: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage);
begin
  MessageToSend := nil;
end;

end.