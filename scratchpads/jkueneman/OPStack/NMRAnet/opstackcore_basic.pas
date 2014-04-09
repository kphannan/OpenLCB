unit opstackcore_basic;

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  template_hardware,
  opstackbuffers,
  opstacknode,
  nmranetutilities,
  opstackdefines;

procedure VerifyNodeIdByDestination(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
procedure VerifyNodeId(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
procedure OptionalInteractionRejected(AMessage: POPStackMessage; DestNode: PNMRAnetNode; IsPermenent: Boolean);

implementation

procedure VerifyNodeIdByDestination(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
begin
  if DestNode <> nil then
    OPStackNode_SetFlag(DestNode, MF_VERIFY_NODE_ID)      // All messages addressed to node get replies even if the payload is wrong!
end;

procedure VerifyNodeId(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
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

procedure OptionalInteractionRejected(AMessage: POPStackMessage; DestNode: PNMRAnetNode; IsPermenent: Boolean);
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

end.

