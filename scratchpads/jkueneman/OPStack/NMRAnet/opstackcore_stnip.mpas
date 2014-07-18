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
function SimpleTrainNodeInfoRequestHandler(Node: PNMRAnetNode; var MessageToSend: POPStackMessage; NextMessage: POPStackMessage): Boolean;
procedure SimpleTrainNodeInfoReply(Node: PNMRAnetNode; NextMessage: POPStackMessage);

implementation

procedure ReadConfigStringOffset(ConfigOffset: DWord; MultiFrameStringBufferPtr: PMultiFrameStringBuffer);
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
        MultiFrameStringBufferPtr^.DataArray[MultiFrameStringBufferPtr^.DataBufferSize] := Ord( Buffer[i]);
        Inc(MultiFrameStringBufferPtr^.DataBufferSize);
      end
    end else
    begin
      MultiFrameStringBufferPtr^.DataArray[MultiFrameStringBufferPtr^.DataBufferSize] := Ord(#0);
      Inc(MultiFrameStringBufferPtr^.DataBufferSize);
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

function SimpleTrainNodeInfoRequestHandler(Node: PNMRAnetNode; var MessageToSend: POPStackMessage; NextMessage: POPStackMessage): Boolean;
var
  MultiFrameStringBufferPtr: PMultiFrameStringBuffer;
  ConfigOffset: DWord;
begin
  Result := False;
  MessageToSend := nil;

  if OPStackBuffers_Allcoate_ACDI_SNIP_Message(MessageToSend, MTI_SIMPLE_TRAIN_NODE_INFO_REPLY, NextMessage^.Dest, NextMessage^.Source) then
  begin
    MultiFrameStringBufferPtr := PMultiFrameStringBuffer( PByte( MessageToSend^.Buffer));
    MultiFrameStringBufferPtr^.DataBufferSize := 0;

    ConfigOffset := 0;
    if Node^.iIndex > 0 then
      ConfigOffset := USER_CONFIGURATION_MEMORY_SIZE + ((Node^.iIndex - 1) * USER_VNODE_CONFIGURATION_MEMORY_SIZE);

    MultiFrameStringBufferPtr^.DataArray[0] := 1;   // Version ID
    MultiFrameStringBufferPtr^.DataBufferSize := 1;
    ReadConfigStringOffset(ConfigOffset + STNIP_OFFSET_ROADNAME, MultiFrameStringBufferPtr);
    ReadConfigStringOffset(ConfigOffset + STNIP_OFFSET_CLASS, MultiFrameStringBufferPtr);
    ReadConfigStringOffset(ConfigOffset + STNIP_OFFSET_ROADNUMBER, MultiFrameStringBufferPtr);
    ReadConfigStringOffset(ConfigOffset + STNIP_OFFSET_TRAINNAME, MultiFrameStringBufferPtr);
    ReadConfigStringOffset(ConfigOffset + STNIP_OFFSET_MANUFACTURER, MultiFrameStringBufferPtr);
    ReadConfigStringOffset(ConfigOffset + STNIP_OFFSET_OWNER, MultiFrameStringBufferPtr);
    Result := UnLinkDeAllocateAndTestForMessageToSend(Node, MessageToSend, NextMessage);    // Keep trying until we release the buffer to send the next one
  end;
end;

procedure SimpleTrainNodeInfoReply(Node: PNMRAnetNode; NextMessage: POPStackMessage);
begin
  AppCallback_SimpleTrainNodeInfoReply(Node, NextMessage);
  UnLinkDeAllocateAndTestForMessageToSend(Node, nil, NextMessage);
end;

end.
