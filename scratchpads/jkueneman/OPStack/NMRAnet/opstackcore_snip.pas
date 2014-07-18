unit opstackcore_snip;

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  opstackcore_basic,
  template_node,
  template_vnode,
  opstacknode,
  opstackbuffers,
  nmranetdefines,
  opstackdefines,
  template_userstatemachine,
  opstacktypes;

procedure SimpleNodeInfoMessage(DestNode: PNMRAnetNode; AMessage: POPStackMessage);
function SimpleNodeInfoRequestHandler(DestNode: PNMRAnetNode; var MessageToSend: POPStackMessage; AMessage: POPStackMessage): Boolean;
procedure SimpleNodeInfoRequestReply(DestNode: PNMRAnetNode; AMessage: POPStackMessage);

implementation

//
// When a message is received this function queues it up for later processing by the main statemachine
//
procedure SimpleNodeInfoMessage(DestNode: PNMRAnetNode; AMessage: POPStackMessage);
begin
  // Link it in
  OPStackNode_IncomingMessageLink(DestNode, AMessage)
end;

//
//  Called to automatically reply to a Simple Node Info Request
//
function SimpleNodeInfoRequestHandler(DestNode: PNMRAnetNode; var MessageToSend: POPStackMessage; AMessage: POPStackMessage): Boolean;
var
  MultiFrameStringBufferPtr: PMultiFrameStringBuffer;
  j: Integer;
begin
  Result := False;
  MessageToSend := nil;
  if OPStackBuffers_Allcoate_ACDI_SNIP_Message(MessageToSend, MTI_SIMPLE_NODE_INFO_REPLY, AMessage^.Dest, AMessage^.Source) then
  begin
    MultiFrameStringBufferPtr := PMultiFrameStringBuffer( PByte( MessageToSend^.Buffer));
    MultiFrameStringBufferPtr^.DataBufferSize := 0;
    {$IFDEF SUPPORT_VIRTUAL_NODES}
    if DestNode^.State and NS_VIRTUAL <> 0 then
    begin
      while MultiFrameStringBufferPtr^.DataBufferSize < USER_VNODE_MAX_ACDI_MFG_ARRAY do
      begin
        MultiFrameStringBufferPtr^.DataArray[MultiFrameStringBufferPtr^.DataBufferSize] := USER_VNODE_ACDI_MFG_STRINGS[MultiFrameStringBufferPtr^.DataBufferSize];
        Inc(MultiFrameStringBufferPtr^.DataBufferSize);
      end;
    end else
    {$ENDIF}
    begin
      while MultiFrameStringBufferPtr^.DataBufferSize < USER_MAX_ACDI_MFG_ARRAY do
      begin
        MultiFrameStringBufferPtr^.DataArray[MultiFrameStringBufferPtr^.DataBufferSize] := USER_ACDI_MFG_STRINGS[MultiFrameStringBufferPtr^.DataBufferSize];
        Inc(MultiFrameStringBufferPtr^.DataBufferSize);
      end;
    end;
    
    
    // Need to read configuration memory here in a callback
    MultiFrameStringBufferPtr^.DataArray[MultiFrameStringBufferPtr^.DataBufferSize] := 1;
    Inc(MultiFrameStringBufferPtr^.DataBufferSize);
    j := 0;
    while j < 2 do
    begin
      MultiFrameStringBufferPtr^.DataArray[MultiFrameStringBufferPtr^.DataBufferSize] := 0;
      Inc(MultiFrameStringBufferPtr^.DataBufferSize);
      Inc(j)
    end;

    Result := UnLinkDeAllocateAndTestForMessageToSend(DestNode, MessageToSend, AMessage);
  end;
end;

//
// Called when the reply to a request by us is received and needs to be handled
//
procedure SimpleNodeInfoRequestReply(DestNode: PNMRAnetNode; AMessage: POPStackMessage);
begin
  AppCallback_SimpleNodeInfoReply(DestNode, AMessage);
  UnLinkDeAllocateAndTestForMessageToSend(DestNode, nil, AMessage);
end;

end.
