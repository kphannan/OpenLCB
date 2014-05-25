unit opstackcore_button;


{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  {$IFNDEF FPC}
  NMRAnetDCC,
  {$ENDIF}
  opstacknode,
  opstackcore_basic,
  opstackdefines;

procedure RemoteButtonMessage(DestNode: PNMRAnetNode; AMessage: POPStackMessage);
function RemoteButtonReplyHandler(Node: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage): Boolean;
procedure RemoteButtonReply(Node: PNMRAnetNode; NextMessage: POPStackMessage);


implementation

//******************************************************************************
// procedure RemoteButtonMessage
// Parameters:
//    AMessage: The incoming OPStack Message
//    DestNode: The node the message is meant for
// Description:
//    Takes incoming Traction Protocol and posts it to be disected and handled
//    later in the Reply
//******************************************************************************
procedure RemoteButtonMessage(DestNode: PNMRAnetNode; AMessage: POPStackMessage);
begin
  OPStackNode_IncomingMessageLink(DestNode, AMessage)
end;

function RemoteButtonReplyHandler(Node: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage): Boolean;
begin
  Result := False;
  MessageToSend := nil;
  UnLinkDeAllocateAndTestForMessageToSend(Node, MessageToSend, NextMessage);
end;

procedure RemoteButtonReply(Node: PNMRAnetNode; NextMessage: POPStackMessage);
begin
  UnLinkDeAllocateAndTestForMessageToSend(Node, nil, NextMessage);
end;

end.
