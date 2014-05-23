unit opstackcore_button;


{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

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
var
  IsForward: Boolean;
  AbsoluteSpeed: Real;
  SpeedStep: Word;
  AddressHi, AddressLo: Byte;
  {$IFNDEF FPC}
  DCCPacket: TDCCPacket;
  {$ENDIF}
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
