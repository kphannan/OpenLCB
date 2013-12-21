unit opstackcore_can;

// TODO:  Need to test FlushDestinationMessages

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  template_node,
  template_vnode,
  opstacknode,
  template_event_callbacks,
  nmranetutilities,
  nmranetdefines,
  opstackdefines,
  opstackcore_datagram,
  opstacktypes;

procedure DuplicateSourceIdDetected(AMessage: POPStackMessage; SourceNode: PNMRAnetNode);
procedure AliasMappingEnquiry(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
procedure AliasMappingDefinition(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
procedure AliasMappingReset(AMessage: POPStackMessage; DestNode: PNMRAnetNode);

implementation

procedure DuplicateSourceIdDetected(AMessage: POPStackMessage; SourceNode: PNMRAnetNode);
begin
  if (AMessage^.MTI = MTI_CAN_CID0) or (AMessage^.MTI = MTI_CAN_CID1) or (AMessage^.MTI = MTI_CAN_CID2) or (AMessage^.MTI = MTI_CAN_CID3) then
    OPStackNode_SetFlag(SourceNode, MF_DUPLICATE_ALIAS_RID)                 // Another node is trying to register our Alias, tell it NO!
  else
    OPStackNode_SetFlag(SourceNode, MF_DUPLICATE_ALIAS);                    // Another node is using our Alias, we have to disconnect from the network
end;

procedure AliasMappingEnquiry(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
begin
  if AMessage^.Buffer^.DataBufferSize = 0 then
    OPStackNode_SetFlags(MF_ALIAS_MAP_ENQUIRY)
  else begin
    NMRAnetUtilities_SimpleDataToNodeID(@AMessage^.Buffer^.DataArray, AMessage^.Dest.ID);
    DestNode := OPStackNode_FindByID(AMessage^.Dest.ID);
    if DestNode <> nil then       // The full Source ID was filled above so it will be use to search
    begin
      if OPStackNode_TestState(DestNode, NS_PERMITTED) then                     // Only reply if node is in Permitted state
        OPStackNode_SetFlag(DestNode, MF_ALIAS_MAP_ENQUIRY);
    end;
  end;
end;

procedure AliasMappingDefinition(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
begin
  if DestNode = nil then
    if AMessage^.Buffer^.DataBufferSize > 0 then
    begin
      NMRAnetUtilities_SimpleDataToNodeID(@AMessage^.Buffer^.DataArray, AMessage^.Dest.ID);
      DestNode := OPStackNode_FindByID(AMessage^.Dest.ID);
    end;
  if DestNode <> nil then                                                       // The full Source ID was filled above so it will be use to search
  begin
    if OPStackNode_TestState(DestNode, NS_PERMITTED) then                       // Only reply if node is in Permitted state
      OPStackNode_SetFlag(DestNode, MF_DUPLICATE_NODE_ID);                      // The other node has the same Node ID as we do!  Warning Will Robinson, Warning
    DatagramFlushDestinationMessages(AMessage^.Source);
  end;
end;

procedure AliasMappingReset(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
begin
  if DestNode = nil then
    if AMessage^.Buffer^.DataBufferSize > 0 then
    begin
      NMRAnetUtilities_SimpleDataToNodeID(@AMessage^.Buffer^.DataArray, AMessage^.Dest.ID);
      DestNode := OPStackNode_FindByID(AMessage^.Dest.ID);
    end;
  if DestNode <> nil then
  begin
    DatagramFlushDestinationMessages(AMessage^.Source);
  end;
end;

end.

