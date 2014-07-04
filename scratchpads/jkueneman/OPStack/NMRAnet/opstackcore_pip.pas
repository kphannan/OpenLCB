unit opstackcore_pip;

// TODO:  Extended Bits cause python scripts to complain

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  template_node,
  template_vnode,
  opstackcore_basic,
  opstacknode,
  opstackbuffers,
  nmranetdefines,
  template_userstatemachine,
  opstackdefines;

procedure ProtocolSupportMessage(DestNode: PNMRAnetNode; AMessage: POPStackMessage);
function ProtocolSupportInquiryHandler(DestNode: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage): Boolean;
procedure ProtocolSupportReply(DestNode: PNMRAnetNode; AMessage: POPStackMessage);

implementation

//
// When a message is received this function queues it up for later processing by the main statemachine
//
procedure ProtocolSupportMessage(DestNode: PNMRAnetNode; AMessage: POPStackMessage);
begin
  OPStackNode_IncomingMessageLink(DestNode, AMessage)                       // If it was sent as a multi Frame then just link it
end;

//
//  Called to automatically reply to a Simple Node Info Request
//
function ProtocolSupportInquiryHandler(DestNode: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage): Boolean;
var
  i, j: Integer;
begin
  Result := False;
  MessageToSend := nil;
  if OPStackBuffers_AllocateOPStackMessage(MessageToSend, MTI_PROTOCOL_SUPPORT_REPLY, NextMessage^.Dest, NextMessage^.Source, False) then
  begin
    for i := 0 to 8 - 1 do                                            // Since we are OR'ing we need to start in a known state
      MessageToSend^.Buffer^.DataArray[i] := 0;
    {$IFDEF SUPPORT_VIRTUAL_NODES}
    if DestNode^.State and NS_VIRTUAL <> 0 then
    begin
      for i := 0 to LEN_PIV_PROTOCOL-1 do
        for j := 0 to USER_PIV_VNODE_SUPPORTED_PROTOCOL_COUNT - 1 do
          MessageToSend^.Buffer^.DataArray[i] := MessageToSend^.Buffer^.DataArray[i] or USER_PIV_VNODE_SUPPORTED_PROTOCOLS[j][i];
      MessageToSend^.Buffer^.DataBufferSize := LEN_PIV_PROTOCOL;
    end else
    {$ENDIF}
    begin
      for i := 0 to LEN_PIV_PROTOCOL-1 do
        for j := 0 to USER_PIV_SUPPORTED_PROTOCOL_COUNT - 1 do
          MessageToSend^.Buffer^.DataArray[i] := MessageToSend^.Buffer^.DataArray[i] or USER_PIV_SUPPORTED_PROTOCOLS[j][i];
      MessageToSend^.Buffer^.DataBufferSize := LEN_PIV_PROTOCOL;
    end;
    Result := UnLinkDeAllocateAndTestForMessageToSend(DestNode, MessageToSend, NextMessage);
  end;
end;

//
// Called when the reply to a request by us is received and needs to be handled
//
procedure ProtocolSupportReply(DestNode: PNMRAnetNode; AMessage: POPStackMessage);
begin
  AppCallBack_ProtocolSupportReply(DestNode, AMessage);
  UnLinkDeAllocateAndTestForMessageToSend(DestNode, nil, AMessage);
end;

end.
