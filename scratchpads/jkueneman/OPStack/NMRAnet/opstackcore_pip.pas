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
  opstackdefines;

procedure ProtocolSupportInquiry(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
procedure ProtocolSupportReply(Node: PNMRAnetNode; var MessageToSend: POPStackMessage; var SourceID: TNodeInfo; var DestID: TNodeInfo);

implementation

procedure ProtocolSupportInquiry(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
var
  NewMessage: POPStackMessage;
begin
  // Since we don't implement extended protocols yet just reply when we see the start bit set (active 0)
  if AMessage^.DestFlags and PIP_EXTENSION_START_BIT_MASK = 0 then
  begin
    NewMessage := nil;
    if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI_PROTOCOL_SUPPORT_INQUIRY, AMessage^.Source.AliasID, AMessage^.Source.ID, AMessage^.Dest.AliasID, AMessage^.Dest.ID) then
      OPStackNode_IncomingMessageLink(DestNode, NewMessage)
    else
      OptionalInteractionRejected(AMessage, DestNode);                            // Try again if you wish
  end
end;

procedure ProtocolSupportReply(Node: PNMRAnetNode; var MessageToSend: POPStackMessage; var SourceID: TNodeInfo; var DestID: TNodeInfo);
var
  i, j: Integer;
begin
  MessageToSend := nil;
  if OPStackBuffers_AllocateOPStackMessage(MessageToSend, MTI_PROTOCOL_SUPPORT_REPLY, SourceID.AliasID, SourceID.ID, DestID.AliasID, DestID.ID) then
  begin
    for i := 0 to 8 - 1 do                                            // Since we are OR'ing we need to start in a known state
      MessageToSend^.Buffer^.DataArray[i] := 0;
    {$IFDEF SUPPORT_VIRTUAL_NODES}
    if Node^.State and NS_VIRTUAL <> 0 then
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
  end;
end;

end.

