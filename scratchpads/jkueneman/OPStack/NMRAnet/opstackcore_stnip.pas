unit opstackcore_stnip;

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  nmranetutilities,
  opstackcore_basic,
  template_node,
  template_vnode,
  opstacknode,
  opstackbuffers,
  nmranetdefines,
  opstackdefines,
  template_configuration,
  template_userstatemachine,
  opstacktypes;

procedure SimpleTrainNodeInfoMessage(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
function SimpleTrainNodeInfoRequestReplyHandler(Node: PNMRAnetNode; var MessageToSend: POPStackMessage; NextMessage: POPStackMessage): Boolean;
procedure SimpleTrainNodeInfoReply(Node: PNMRAnetNode; NextMessage: POPStackMessage);

implementation

procedure ReadConfigStringOffset(ConfigOffset: DWord; AcdiSnipBufferPtr: PAcdiSnipBuffer);
var
  i, BufferLen: Integer;
  Buffer: TStnipBuffer;
begin
  if AppCallback_ReadConfiguration(ConfigOffset, 32, @Buffer) = 32 then
  begin
    BufferLen := -1;
    for i := 0 to STNIP_MAX_STR_LEN - 1 do
    begin
      if Buffer[i] = #0 then
      begin
        BufferLen := i;
        Break
      end;
    end;

    if BufferLen > -1 then
    begin
      for i := 0 to BufferLen do       // Need to add the Null so run all the way to BufferLen
      begin
        AcdiSnipBufferPtr^.DataArray[AcdiSnipBufferPtr^.DataBufferSize] := Ord( Buffer[i]);
        Inc(AcdiSnipBufferPtr^.DataBufferSize);
      end
    end else
    begin
      AcdiSnipBufferPtr^.DataArray[AcdiSnipBufferPtr^.DataBufferSize] := Ord(#0);
      Inc(AcdiSnipBufferPtr^.DataBufferSize);
    end
  end
end;

procedure SimpleTrainNodeInfoMessage(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
begin
  if AMessage^.MTI = MTI_SIMPLE_TRAIN_NODE_INFO_REPLY then                      // Take a crack at all incoming reply
    OPStackNode_IncomingMessageLink(DestNode, AMessage)
  else                                                                          // Incoming reqests need the node to support it first.
  if NMRAnetUtilities_NodeSupportsProtcol(DestNode, STNIP_PROTOCOL) then        // If we don't support this protocol drop it
    OPStackNode_IncomingMessageLink(DestNode, AMessage)
end;

function SimpleTrainNodeInfoRequestReplyHandler(Node: PNMRAnetNode; var MessageToSend: POPStackMessage; NextMessage: POPStackMessage): Boolean;
var
  AcdiSnipBufferPtr: PAcdiSnipBuffer;
  ConfigOffset: DWord;
begin
  Result := False;
  MessageToSend := nil;

  if OPStackBuffers_Allcoate_ACDI_SNIP_Message(MessageToSend, MTI_SIMPLE_TRAIN_NODE_INFO_REPLY, NextMessage^.Dest.AliasID, NextMessage^.Dest.ID, NextMessage^.Source.AliasID, NextMessage^.Source.ID) then
  begin
    AcdiSnipBufferPtr := PAcdiSnipBuffer( PByte( MessageToSend^.Buffer));
    AcdiSnipBufferPtr^.DataBufferSize := 0;

    ConfigOffset := 0;
    if Node^.iIndex > 0 then
      ConfigOffset := USER_CONFIGURATION_MEMORY_SIZE + ((Node^.iIndex - 1) * USER_VNODE_CONFIGURATION_MEMORY_SIZE);

    AcdiSnipBufferPtr^.DataArray[0] := 1;   // Version ID
    AcdiSnipBufferPtr^.DataBufferSize := 1;
    ReadConfigStringOffset(ConfigOffset + STNIP_OFFSET_ROADNAME, AcdiSnipBufferPtr);
    ReadConfigStringOffset(ConfigOffset + STNIP_OFFSET_CLASS, AcdiSnipBufferPtr);
    ReadConfigStringOffset(ConfigOffset + STNIP_OFFSET_ROADNUMBER, AcdiSnipBufferPtr);
    ReadConfigStringOffset(ConfigOffset + STNIP_OFFSET_TRAINNAME, AcdiSnipBufferPtr);
    ReadConfigStringOffset(ConfigOffset + STNIP_OFFSET_MANUFACTURER, AcdiSnipBufferPtr);
    ReadConfigStringOffset(ConfigOffset + STNIP_OFFSET_OWNER, AcdiSnipBufferPtr);
    Result := UnLinkDeAllocateAndTestForMessageToSend(Node, MessageToSend, NextMessage);    // Keep trying until we release the buffer to send the next one
  end;
end;

procedure SimpleTrainNodeInfoReply(Node: PNMRAnetNode; NextMessage: POPStackMessage);
begin
  AppCallback_SimpleTrainNodeInfoReply(Node, NextMessage);
  UnLinkDeAllocateAndTestForMessageToSend(Node, nil, NextMessage);
end;

end.
