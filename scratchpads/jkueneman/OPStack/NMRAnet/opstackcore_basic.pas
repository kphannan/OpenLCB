unit opstackcore_basic;

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  template_hardware,
  opstackbuffers,
  opstacknode,
  nmranetdefines,
  nmranetutilities,
  opstackdefines;

procedure VerifyNodeIdByDestination(DestNode: PNMRAnetNode; AMessage: POPStackMessage);
procedure VerifyNodeId(DestNode: PNMRAnetNode; AMessage: POPStackMessage);
procedure OptionalInteractionRejected(AMessage: POPStackMessage; IsPermenent: Boolean);
procedure DatagramRejected(Dest, Source: PNodeInfo; ErrorCode: Word);
function UnLinkDeAllocateAndTestForMessageToSend(Node: PNMRAnetNode; MessageToSend, AMessage: POPStackMessage): Boolean;

implementation

procedure VerifyNodeIdByDestination(DestNode: PNMRAnetNode; AMessage: POPStackMessage);
begin
  if DestNode <> nil then
    OPStackNode_SetFlag(DestNode, MF_VERIFY_NODE_ID);      // All messages addressed to node get replies even if the payload is wrong!
end;

procedure VerifyNodeId(DestNode: PNMRAnetNode; AMessage: POPStackMessage);
begin
   if AMessage^.Buffer^.DataBufferSize = 0 then
    OPStackNode_SetFlags(MF_VERIFY_NODE_ID)
  else begin
    NMRAnetUtilities_SimpleDataToNodeID(@AMessage^.Buffer^.DataArray, AMessage^.Dest.ID);
    DestNode := OPStackNode_Find(AMessage, FIND_BY_DEST);
    if DestNode <> nil then  // The full Source ID was filled above so it will be use to search
      OPStackNode_SetFlag(DestNode, MF_VERIFY_NODE_ID);
  end;
end;

procedure OptionalInteractionRejected(AMessage: POPStackMessage; IsPermenent: Boolean);
var
  OptionalInteractionMessage: TOPStackMessage;
  OptionalnteractionBuffer: TSimpleBuffer;
begin
  OPStackBuffers_ZeroSimpleBuffer(@OptionalnteractionBuffer, False);
  OPStackBuffers_ZeroMessage(@OptionalInteractionMessage);
  OptionalInteractionMessage.Buffer := @OptionalnteractionBuffer;
  OPStackBuffers_LoadOptionalInteractionRejected(@OptionalInteractionMessage, AMessage^.Dest.AliasID, AMessage^.Dest.ID, AMessage^.Source.AliasID, AMessage^.Source.ID, AMessage^.MTI, IsPermenent);    // Unknown MTI sent to addressed node
  OutgoingCriticalMessage(@OptionalInteractionMessage);
end;

procedure DatagramRejected(Dest, Source: PNodeInfo; ErrorCode: Word);
var
  OptionalInteractionMessage: TOPStackMessage;
  OptionalnteractionBuffer: TSimpleBuffer;
  DatagramError: PSimpleDataArray;
begin
  OPStackBuffers_ZeroSimpleBuffer(@OptionalnteractionBuffer, False);
  OPStackBuffers_ZeroMessage(@OptionalInteractionMessage);
  OptionalInteractionMessage.Buffer := @OptionalnteractionBuffer;

  case ErrorCode of
    DATAGRAM_PROCESS_ERROR_BUFFER_FULL         : DatagramError := PSimpleDataArray( @DATAGRAM_RESULT_REJECTED_BUFFER_FULL);
    DATAGRAM_PROCESS_ERROR_OUT_OF_ORDER        : DatagramError := PSimpleDataArray(@DATAGRAM_RESULT_REJECTED_OUT_OF_ORDER);
    DATAGRAM_PROCESS_ERROR_SOURCE_NOT_ACCEPTED : DatagramError := PSimpleDataArray(@DATAGRAM_RESULT_REJECTED_SOURCE_DATAGRAMS_NOT_ACCEPTED);
  end;
  OPStackBuffers_LoadMessage(@OptionalInteractionMessage, MTI_DATAGRAM_REJECTED_REPLY, Dest^.AliasID, Dest^.ID, Source^.AliasID, Source^.ID, 0);
  OptionalInteractionMessage.MessageType := MT_SIMPLE;
  OPStackBuffers_CopyDataArray(OptionalInteractionMessage.Buffer, DatagramError, 2, True);
  OutgoingCriticalMessage(@OptionalInteractionMessage);
end;

// *****************************************************************************
//  procedure UnLinkDeAllocateAndTestForMessageToSend
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
function UnLinkDeAllocateAndTestForMessageToSend(Node: PNMRAnetNode; MessageToSend, AMessage: POPStackMessage): Boolean;
begin
  OPStackNode_IncomingMessageUnLink(Node, AMessage);
  OPStackBuffers_DeAllocateMessage(AMessage);
  if MessageToSend <> nil then
    Result := True
   else
    Result := False;
end;

end.

