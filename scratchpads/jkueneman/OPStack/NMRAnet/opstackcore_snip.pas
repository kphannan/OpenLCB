unit opstackcore_snip;

// TODO:  OPTIONAL INTERACTION REJECTED if we can't allocate buffers

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  template_node,
  template_vnode,
  opstacknode,
  opstackbuffers,
  template_event_callbacks,
  nmranetutilities,
  nmranetdefines,
  opstackdefines,
  opstacktypes;

procedure SimpleNodeInfoRequest(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
procedure SimpleNodeInfoReply(Node: PNMRAnetNode; var MessageToSend: POPStackMessage; var SourceID: TNodeInfo; var DestID: TNodeInfo);

implementation

procedure SimpleNodeInfoRequest(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
var
  NewMessage: POPStackMessage;
begin
  NewMessage := nil;
  if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI_SIMPLE_NODE_INFO_REQUEST, AMessage^.Source.AliasID, AMessage^.Source.ID, AMessage^.Dest.AliasID, AMessage^.Dest.ID) then
    OPStackNode_MessageLink(DestNode, NewMessage)
  else begin

  end;
end;

procedure SimpleNodeInfoReply(Node: PNMRAnetNode; var MessageToSend: POPStackMessage; var SourceID: TNodeInfo; var DestID: TNodeInfo);
var
  AcdiSnipBufferPtr: PAcdiSnipBuffer;
  j: Integer;
begin
  MessageToSend := nil;
  if OPStackBuffers_Allcoate_ACDI_SNIP_Message(MessageToSend, MTI_SIMPLE_NODE_INFO_REPLY, SourceID.AliasID, SourceID.ID, DestID.AliasID, DestID.ID) then
  begin
    AcdiSnipBufferPtr := PAcdiSnipBuffer( PByte( MessageToSend^.Buffer));
    AcdiSnipBufferPtr^.DataBufferSize := 0;
    AcdiSnipBufferPtr^.DataArray[AcdiSnipBufferPtr^.DataBufferSize] := ACDI_MFG_VERSION;
    Inc(AcdiSnipBufferPtr^.DataBufferSize);
    j := 0;
    if Node^.State and NS_VIRTUAL <> 0 then
    begin
      while j < USER_VNODE_MAX_ACDI_MFG_ARRAY do
      begin
        AcdiSnipBufferPtr^.DataArray[AcdiSnipBufferPtr^.DataBufferSize] := USER_VNODE_ACDI_MFG_STRINGS[j];
        Inc(AcdiSnipBufferPtr^.DataBufferSize);
        Inc(j)
      end;
    end else
    begin
      while j < USER_MAX_ACDI_MFG_ARRAY do
      begin
        AcdiSnipBufferPtr^.DataArray[AcdiSnipBufferPtr^.DataBufferSize] := USER_ACDI_MFG_STRINGS[j];
        Inc(AcdiSnipBufferPtr^.DataBufferSize);
        Inc(j)
      end;
    end;
    AcdiSnipBufferPtr^.DataArray[AcdiSnipBufferPtr^.DataBufferSize] := ACDI_USER_VERSION;
    Inc(AcdiSnipBufferPtr^.DataBufferSize);

    // Need to read configuration memory here in a callback
    j := 0;
    while j < 2 do
    begin
      AcdiSnipBufferPtr^.DataArray[AcdiSnipBufferPtr^.DataBufferSize] := 0;
      Inc(AcdiSnipBufferPtr^.DataBufferSize);
      Inc(j)
    end;
  end;
end;

end.

