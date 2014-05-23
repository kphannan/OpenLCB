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
  template_userstatemachine,
  opstacktypes;

procedure SimpleTrainNodeInfoMessage(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
function SimpleTrainNodeInfoRequestReplyHandler(Node: PNMRAnetNode; var MessageToSend: POPStackMessage; NextMessage: POPStackMessage): Boolean;
procedure SimpleTrainNodeInfoReply(Node: PNMRAnetNode; NextMessage: POPStackMessage);

implementation

procedure SimpleTrainNodeInfoMessage(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
begin
  OPStackNode_IncomingMessageLink(DestNode, AMessage)
end;

function SimpleTrainNodeInfoRequestReplyHandler(Node: PNMRAnetNode; var MessageToSend: POPStackMessage; NextMessage: POPStackMessage): Boolean;
var
  AcdiSnipBufferPtr: PAcdiSnipBuffer;
  j: Integer;
begin
  Result := False;
  MessageToSend := nil;
  if OPStackBuffers_Allcoate_ACDI_SNIP_Message(MessageToSend, MTI_SIMPLE_TRAIN_NODE_INFO_REPLY, NextMessage^.Dest.AliasID, NextMessage^.Dest.ID, NextMessage^.Source.AliasID, NextMessage^.Source.ID) then
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
  end;
  UnLinkDeAllocateAndTestForMessageToSend(Node, MessageToSend, NextMessage);
end;

procedure SimpleTrainNodeInfoReply(Node: PNMRAnetNode; NextMessage: POPStackMessage);
begin
  AppCallback_SimpleTrainNodeInfoReply(Node, NextMessage);
  UnLinkDeAllocateAndTestForMessageToSend(Node, nil, NextMessage);
end;

end.
