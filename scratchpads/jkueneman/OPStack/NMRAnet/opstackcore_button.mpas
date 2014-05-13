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

procedure RemoteButtonMessage(DestNode: PNMRAnetNode; AMessage: POPStackMessage; IsReply: Boolean);
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
procedure RemoteButtonMessage(DestNode: PNMRAnetNode; AMessage: POPStackMessage; IsReply: Boolean);
var
  NewMessage: POPStackMessage;
  MTI: Word;
begin
  NewMessage := nil;
  if IsReply then
    MTI := MTI_REMOTE_BUTTON_REPLY
  else
    MTI := MTI_REMOTE_BUTTON_REQUEST;

  if OPStackBuffers_AllocateMultiFrameMessage(NewMessage, MTI, AMessage^.Dest.AliasID, AMessage^.Dest.ID, AMessage^.Source.AliasID, AMessage^.Source.ID) then
  begin
    OPStackBuffers_CopyData(NewMessage^.Buffer, AMessage^.Buffer);
    OPStackNode_IncomingMessageLink(DestNode, NewMessage)
  end else
  begin
    if not IsReply then
      OptionalInteractionRejected(AMessage, False);                            // Try again if you wish
  end;
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
