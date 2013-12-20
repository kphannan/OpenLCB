unit opstackcore_basic;

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

procedure VerifyNodeIdByDestination(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
procedure VerifyNodeId(AMessage: POPStackMessage; DestNode: PNMRAnetNode);

implementation

procedure VerifyNodeIdByDestination(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
begin
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

end.

