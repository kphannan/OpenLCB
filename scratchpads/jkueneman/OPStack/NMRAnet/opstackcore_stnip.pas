unit opstackcore_stnip;

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
  opstacktypes;

procedure SimpleTrainNodeInfoRequest(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
procedure SimpleTrainNodeInfoRequestReply(Node: PNMRAnetNode; var MessageToSend: POPStackMessage; var SourceID: TNodeInfo; var DestID: TNodeInfo);

implementation

procedure SimpleTrainNodeInfoRequest(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
var
  NewMessage: POPStackMessage;
begin
  NewMessage := nil;
  if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI_SIMPLE_TRAIN_NODE_INFO_REQUEST, AMessage^.Source.AliasID, AMessage^.Source.ID, AMessage^.Dest.AliasID, AMessage^.Dest.ID) then
    OPStackNode_IncomingMessageLink(DestNode, NewMessage)
  else
    OptionalInteractionRejected(AMessage, DestNode, False);                            // Try again if you wish
end;

procedure SimpleTrainNodeInfoRequestReply(Node: PNMRAnetNode; var MessageToSend: POPStackMessage; var SourceID: TNodeInfo; var DestID: TNodeInfo);
var
  AcdiSnipBufferPtr: PAcdiSnipBuffer;
  j: Integer;
begin
  MessageToSend := nil;   {
  if OPStackBuffers_Allcoate_ACDI_SNIP_Message(MessageToSend, MTI_SIMPLE_TRAIN_NODE_INFO_REPLY, SourceID.AliasID, SourceID.ID, DestID.AliasID, DestID.ID) then
  begin
    AcdiSnipBufferPtr := PAcdiSnipBuffer( PByte( MessageToSend^.Buffer));
    AcdiSnipBufferPtr^.DataBufferSize := 0;
    {$IFDEF SUPPORT_VIRTUAL_NODES}
    if Node^.State and NS_VIRTUAL <> 0 then
    begin
      while AcdiSnipBufferPtr^.DataBufferSize < USER_VNODE_MAX_ACDI_MFG_ARRAY do
      begin
        AcdiSnipBufferPtr^.DataArray[AcdiSnipBufferPtr^.DataBufferSize] := USER_VNODE_ACDI_MFG_STRINGS[AcdiSnipBufferPtr^.DataBufferSize];
        Inc(AcdiSnipBufferPtr^.DataBufferSize);
      end;
    end else
    {$ENDIF}
    begin
      while AcdiSnipBufferPtr^.DataBufferSize < USER_MAX_ACDI_MFG_ARRAY do
      begin
        AcdiSnipBufferPtr^.DataArray[AcdiSnipBufferPtr^.DataBufferSize] := USER_ACDI_MFG_STRINGS[AcdiSnipBufferPtr^.DataBufferSize];
        Inc(AcdiSnipBufferPtr^.DataBufferSize);
      end;
    end;
    
    
    // Need to read configuration memory here in a callback
    AcdiSnipBufferPtr^.DataArray[AcdiSnipBufferPtr^.DataBufferSize] := 1;
    Inc(AcdiSnipBufferPtr^.DataBufferSize);
    j := 0;
    while j < 2 do
    begin
      AcdiSnipBufferPtr^.DataArray[AcdiSnipBufferPtr^.DataBufferSize] := 0;
      Inc(AcdiSnipBufferPtr^.DataBufferSize);
      Inc(j)
    end;
  end;        }
end;

end.
