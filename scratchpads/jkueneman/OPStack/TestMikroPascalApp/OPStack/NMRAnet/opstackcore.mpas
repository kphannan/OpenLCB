unit opstackcore;

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  // Compile in different hardware layer interfaces here
  {$IFDEF HARDWARE_TEMPLATE}hardware_template, template_node, template_vnode, template_configmem,{$ENDIF}
  {$IFDEF HARDWARE_DSPIC_CAN}hardware_dspic_CAN,{$ENDIF}
  {$IFDEF HARDWARE_ENC28J60}hardware_ENC28j60,{$ENDIF}
  template_event_callbacks,
  nmranetutilities,
  nmranetdefines,
  opstackdefines,
  opstackbuffers,
  opstacktypes,
  opstacknode;



procedure OPStackCore_Initialize;                                               // Call once on program startup
procedure OPStackCore_Process;                                                  // Call as often as possible
procedure OPStackCore_Timer;                                                    // Call every 100ms

// Callback from the Hardware when a message is received
procedure IncomingMessageDispatch(AMessage: POPStackMessage; DestNode: PNMRAnetNode);

var
  OPStack: TOPStack;

implementation

// *****************************************************************************
//  procedure OPStackCore_Initialize
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure OPStackCore_Initialize;
begin
  OPStack.State := 0;
  OPStackNode_Initialize;
  OPStackBuffers_Initialize;
  OPStackNode_Allocate;                                                         // Allocate the hardware Node
end;

// *****************************************************************************
//  procedure SupportsVNodeEventAsConsumer
//     Parameters:
//     Returns:
//
//     Description:
//
// *****************************************************************************
function SupportsVNodeEventAsConsumer(Node: PNMRAnetNode; DataBytes: PEventID; var EventIndex: Integer): Boolean;
var
  Event: TEventID;
begin
  Result := False;
  {$IFDEF SUPPORT_AT_LEAST_ONE_VNODE_CONSUMED_EVENT}
  EventIndex := 0;
  while (EventIndex < USER_MAX_VNODE_SUPPORTED_EVENTS_CONSUMED) do
  begin
    if NMRAnetUtilities_EqualEventID(@USER_VNODE_SUPPORTED_EVENTS_CONSUMED[EventIndex], DataBytes) then
    begin
      Result := True;
      Exit
    end;
    Inc(EventIndex)
  end;
  EventIndex := 0;
  while (EventIndex < USER_MAX_VNODE_SUPPORTED_DYNAMIC_EVENTS_CONSUMED) do
  begin
    if AppCallback_DynamicVNodeConsumedEvent(Node, EventIndex, Event) then
      if NMRAnetUtilities_EqualEventID(@Event, DataBytes) then
      begin
        EventIndex := USER_MAX_VNODE_SUPPORTED_EVENTS_CONSUMED + EventIndex;
        Result := True;
        Exit;
      end;
    Inc(EventIndex);
  end
  {$ENDIF}
end;

// *****************************************************************************
//  procedure SupportsVNodeEventAsProducer
//     Parameters:
//     Returns:
//
//     Description:
//
// *****************************************************************************
function SupportsVNodeEventAsProducer(Node: PNMRAnetNode; DataBytes: PEventID; var EventIndex: Integer): Boolean;
var
  Event: TEventID;
begin
  Result := False;
  {$IFDEF SUPPORT_AT_LEAST_ONE_VNODE_PRODUCED_EVENT}
  EventIndex := 0;
  while (EventIndex < USER_MAX_VNODE_SUPPORTED_EVENTS_PRODUCED) do
  begin
    if NMRAnetUtilities_EqualEventID(@USER_VNODE_SUPPORTED_EVENTS_PRODUCED[EventIndex], DataBytes) then
    begin
      Result := True;
      Exit
    end;
    Inc(EventIndex)
  end;
  EventIndex := 0;
  while (EventIndex < USER_MAX_VNODE_SUPPORTED_DYNAMIC_EVENTS_PRODUCED) do
  begin
    if AppCallback_DynamicVNodeProducedEvent(Node, EventIndex, Event) then
      if NMRAnetUtilities_EqualEventID(@Event, DataBytes) then
      begin
        EventIndex := USER_MAX_VNODE_SUPPORTED_EVENTS_PRODUCED + EventIndex;
        Result := True;
        Exit;
      end;
    Inc(EventIndex);
  end
  {$ENDIF}
end;

// *****************************************************************************
//  procedure SupportsEventAsConsumer
//     Parameters:
//     Returns:
//
//     Description:
//
// *****************************************************************************
function SupportsEventAsConsumer(Node: PNMRAnetNode; DataBytes: PEventID; var EventIndex: Integer): Boolean;
var
  Event: TEventID;
begin
  Result := False;
  {$IFDEF SUPPORT_AT_LEAST_ONE_CONSUMED_EVENT}
  EventIndex := 0;
  while (EventIndex < USER_MAX_SUPPORTED_EVENTS_CONSUMED) do
  begin
    if NMRAnetUtilities_EqualEventID(@USER_SUPPORTED_EVENTS_CONSUMED[EventIndex], DataBytes) then
    begin
      Result := True;
      Exit
    end;
    Inc(EventIndex)
  end;
  EventIndex := 0;
  while (EventIndex < USER_MAX_SUPPORTED_DYNAMIC_EVENTS_CONSUMED) do
  begin
    if AppCallback_DynamicConsumedEvent(Node, EventIndex, Event) then
      if NMRAnetUtilities_EqualEventID(@Event, DataBytes) then
      begin
        EventIndex := USER_MAX_SUPPORTED_EVENTS_CONSUMED + EventIndex;
        Result := True;
        Exit;
      end;
    Inc(EventIndex);
  end
  {$ENDIF}
end;

// *****************************************************************************
//  procedure SupportsEventAsProducer
//     Parameters:
//     Returns:
//
//     Description:
//
// *****************************************************************************
function SupportsEventAsProducer(Node: PNMRAnetNode; DataBytes: PEventID; var EventIndex: Integer): Boolean;
var
  Event: TEventID;
begin
  Result := False;
  {$IFDEF SUPPORT_AT_LEAST_ONE_PRODUCED_EVENT}
  EventIndex := 0;
  while (EventIndex < USER_MAX_SUPPORTED_EVENTS_PRODUCED) do
  begin
    if NMRAnetUtilities_EqualEventID(@USER_SUPPORTED_EVENTS_PRODUCED[EventIndex], DataBytes) then
    begin
      Result := True;
      Exit
    end;
    Inc(EventIndex)
  end;
  EventIndex := 0;
  while (EventIndex < USER_MAX_SUPPORTED_DYNAMIC_EVENTS_PRODUCED) do
  begin
    if AppCallback_DynamicProducedEvent(Node, EventIndex, Event) then
      if NMRAnetUtilities_EqualEventID(@Event, DataBytes) then
      begin
        EventIndex := USER_MAX_SUPPORTED_EVENTS_PRODUCED + EventIndex;
        Result := True;
        Exit;
      end;
    Inc(EventIndex);
  end
  {$ENDIF}
end;

// *****************************************************************************
//  procedure IncomingMessageDispatch
//     Parameters:  AMessage: Pointer to a OPStackMessage object, may not be allocated
//                            from the pool and may be gone after the function returns
//                            Look at the AMessage^.MessageType for MT_ALLOCATED, if it
//                            exists then then message was allocated from the pool and
//                            can be used as such
//                  DestNode: Pointer to a NMRAnet Node if the message contained a
//                            destination ID (Alias or NodeID). If message is unaddressed
//                            contains nil
//     Returns:
//     Description:
// *****************************************************************************
procedure IncomingMessageDispatch(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
var
  VNodeEventIndex, NodeEventIndex, i: Integer;
  NewMessage: POPStackMessage;
  BufferAllocFailed: Boolean;
  SourceNode: PNMRAnetNode;
  OptionalInteractionMessage: TOPStackMessage;
begin
  BufferAllocFailed := False;
  // First thing is extract the Source Alias and make sure it is not a duplicate of one of our Node or vNode Aliases
  SourceNode := OPStackNode_Find(AMessage, FIND_BY_SOURCE);
  if SourceNode <> nil then
  begin
    if AMessage^.MessageType and MT_CAN_TYPE <> 0 then
    begin
      if (AMessage^.MTI = MTI_CAN_CID0) or (AMessage^.MTI = MTI_CAN_CID1) or (AMessage^.MTI = MTI_CAN_CID2) or (AMessage^.MTI = MTI_CAN_CID3) then
        OPStackNode_SetFlag(SourceNode, MF_DUPLICATE_ALIAS_RID)                 // Another node is trying to register our Alias, tell the NO!
      else
        OPStackNode_SetFlag(SourceNode, MF_DUPLICATE_ALIAS);                    // Another node is using our Alias, we have to disconnect from the network
    end else
      OPStackNode_SetFlag(SourceNode, MF_DUPLICATE_ALIAS);                      // Another node is using our Alias, we have to disconnect from the network
    Exit;
  end else
  begin
    if AMessage^.MessageType and MT_CAN_TYPE <> 0 then                          // Is it a CAN message?
    begin
      case AMessage^.MTI of
          MTI_CAN_AME :
              begin                                                             // Alias Map Enquiry.....
                if AMessage^.Buffer^.DataBufferSize = 0 then
                  OPStackNode_SetFlags(MF_ALIAS_MAP_ENQUIRY)
                else begin
                  NMRAnetUtilities_SimpleDataToNodeID(@AMessage^.Buffer^.DataArray, AMessage^.Dest.ID);
                  DestNode := OPStackNode_FindByID(AMessage^.Dest.ID);
                  if DestNode <> nil then       // The full Source ID was filled above so it will be use to search
                  begin
                    if OPStackNode_TestState(DestNode, NS_PERMITTED) then       // Only reply if node is in Permitted state
                      OPStackNode_SetFlag(DestNode, MF_ALIAS_MAP_ENQUIRY);
                  end;
                end;
                Exit;
              end;
          MTI_CAN_AMD :
              begin                                                             // Another node has sent an Alias Map Definition....
                NMRAnetUtilities_SimpleDataToNodeID(@AMessage^.Buffer^.DataArray, AMessage^.Dest.ID);
                DestNode := OPStackNode_FindByID(AMessage^.Dest.ID);
                if DestNode <> nil then                                        // The full Source ID was filled above so it will be use to search
                begin
                  if OPStackNode_TestState(DestNode, NS_PERMITTED) then         // Only reply if node is in Permitted state
                    OPStackNode_SetFlag(DestNode, MF_DUPLICATE_NODE_ID);      // The other node has the same Node ID as we do!  Warning Will Robinson, Warning
                end;
                Exit;
              end;
        end {case}
    end else
    begin
      case (AMessage^.MessageType) and MT_MASK of
          MT_SIMPLE :
              begin
                if AMessage^.MTI and MTI_ADDRESSED_MASK = MTI_ADDRESSED_MASK then
                begin
                  if DestNode <> nil then                                           // Destination messages come through so we can check for duplicate Aliases, if it is nil then done
                  begin
                    case AMessage^.MTI of
                      MTI_SIMPLE_NODE_INFO_REQUEST :
                          begin
                            NewMessage := nil;
                            if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI_SIMPLE_NODE_INFO_REPLY, AMessage^.Dest.AliasID, AMessage^.Dest.ID, AMessage^.Source.AliasID, AMessage^.Source.ID) then
                              OPStackNode_MessageLink(DestNode, NewMessage)
                            else
                              BufferAllocFailed := True
                          end;
                      MTI_VERIFY_NODE_ID_NUMBER_DEST  :
                          begin
                            OPStackNode_SetFlag(DestNode, MF_VERIFY_NODE_ID)      // All messages addressed to node get replies even if the payload is wrong!
                          end;
                      MTI_EVENTS_IDENTIFY_DEST          :
                          begin
                            OPStackNode_SetEventConsumedFlags(DestNode, EVENT_STATE_UNKNOWN);
                            OPStackNode_SetEventProducedFlags(DestNode, EVENT_STATE_UNKNOWN);
                          end;
                      MTI_PROTOCOL_SUPPORT_INQUIRY      :
                          begin
                            // BITS ARE NEGATIVE LOGIC
                            // Since we don't implement extended protocols yet just reply when we see the start bit set (active 0)
                            if AMessage^.DestFlags and PIP_EXTENSION_START_BIT_MASK = 0 then
                            begin
                              NewMessage := nil;
                              if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI_PROTOCOL_SUPPORT_REPLY, AMessage^.Dest.AliasID, AMessage^.Dest.ID, AMessage^.Source.AliasID, AMessage^.Source.ID) then
                                OPStackNode_MessageLink(DestNode, NewMessage)
                              else
                                BufferAllocFailed := True
                            end
                          end;
                      MTI_OPTIONAL_INTERACTION_REJECTED :
                          begin
                          end;
                      MTI_DATAGRAM_OK_REPLY :
                          begin
                            NewMessage := FindDatagramWaitingForAck(AMessage^.Dest, AMessage^.Source);
                            if NewMessage <> nil then
                            begin
                              RemoveDatagramWaitingForAck(NewMessage);
                              OPStackBuffers_DeAllocateMessage(NewMessage);
                            end
                          end;
                      MTI_DATAGRAM_REJECTED_REPLY :
                          begin
                            NewMessage := FindDatagramWaitingForAck(AMessage^.Dest, AMessage^.Source);
                            if NewMessage <> nil then
                            begin
                              // Need to send it again and again until we give up
                              RemoveDatagramWaitingForAck(NewMessage);
                              AddOutgoingDatagramMessage(NewMessage);
                            end
                          end
                    else begin
                        OPStackBuffers_LoadOptionalInteractionRejected(@OptionalInteractionMessage, AMessage^.Dest.AliasID, AMessage^.Dest.ID, AMessage^.Source.AliasID, AMessage^.Source.ID, AMessage^.MTI);    // Unknown MTI sent to addressed node
                        OutgoingCriticalMessage(@OptionalInteractionMessage);
                      end;
                    end; {case}
                  end else {Node <> nil}
                    Exit;
                end else  {addressed message}
                begin
                  case AMessage^.MTI of
                      MTI_VERIFY_NODE_ID_NUMBER   :
                          begin
                            if AMessage^.Buffer^.DataBufferSize = 0 then
                              OPStackNode_SetFlags(MF_VERIFY_NODE_ID)
                            else begin
                              NMRAnetUtilities_SimpleDataToNodeID(@AMessage^.Buffer^.DataArray, AMessage^.Dest.ID);
                              DestNode := OPStackNode_Find(AMessage, FIND_BY_DEST);
                              if DestNode <> nil then  // The full Source ID was filled above so it will be use to search
                                OPStackNode_SetFlag(DestNode, MF_VERIFY_NODE_ID);
                            end;
                            Exit;
                          end;
                      MTI_CONSUMER_IDENTIFY       :
                          begin
                            VNodeEventIndex := -1;
                            NodeEventIndex := -1;
                            for i := 0 to NodePool.AllocatedCount - 1 do
                            begin
                              DestNode := NodePool.AllocatedList[i];
                              if OPStackNode_TestState(DestNode, NS_VIRTUAL) then
                              begin
                                if SupportsVNodeEventAsConsumer(DestNode, @AMessage^.Buffer^.DataArray, VNodeEventIndex) then
                                  OPStackNode_SetEventFlag(DestNode^.Events.Consumed, VNodeEventIndex, EVENT_STATE_UNKNOWN);
                              end else
                              begin
                                if SupportsEventAsConsumer(DestNode, @AMessage^.Buffer^.DataArray, NodeEventIndex) then
                                  OPStackNode_SetEventFlag(DestNode^.Events.Consumed, NodeEventIndex, EVENT_STATE_UNKNOWN);
                              end
                            end;
                            Exit;
                          end;
                      MTI_CONSUMER_IDENTIFY_RANGE :
                          begin
                            // TODO
                            Exit;
                          end;
                      MTI_PRODUCER_IDENDIFY       :
                          begin
                            VNodeEventIndex := -1;
                            NodeEventIndex := -1;
                            for i := 0 to NodePool.AllocatedCount - 1 do
                            begin
                              DestNode := NodePool.AllocatedList[i];
                              if OPStackNode_TestState(DestNode, NS_VIRTUAL) then
                              begin
                                if SupportsVNodeEventAsProducer(DestNode, @AMessage^.Buffer^.DataArray, VNodeEventIndex) then
                                  OPStackNode_SetEventFlag(DestNode^.Events.Produced, VNodeEventIndex, EVENT_STATE_UNKNOWN);
                              end else
                              begin
                                if SupportsEventAsProducer(DestNode, @AMessage^.Buffer^.DataArray, NodeEventIndex) then
                                  OPStackNode_SetEventFlag(DestNode^.Events.Produced, NodeEventIndex, EVENT_STATE_UNKNOWN);
                              end
                            end;
                            Exit;
                          end;
                      MTI_PRODUCER_IDENTIFY_RANGE :
                          begin
                            // TODO
                            Exit;
                          end;
                      MTI_EVENT_LEARN             :
                          begin
                            Exit;
                          end;
                      MTI_EVENTS_IDENTIFY         :
                          begin
                            for i := 0 to NodePool.AllocatedCount - 1 do
                            begin
                              DestNode := NodePool.AllocatedList[i];
                              OPStackNode_SetEventProducedFlags(DestNode, EVENT_STATE_UNKNOWN);
                              OPStackNode_SetEventConsumedFlags(DestNode, EVENT_STATE_UNKNOWN);
                            end;
                            Exit;
                          end;
                  // Handling unknown MTI for all nodes (virtual and physical) is difficult and not sure it is needed
                  end; {case}
                end; {not an addressed message}
              end;
          MT_DATAGRAM :
              begin
                if DestNode^.State and NS_RELEASING = 0 then                      // if Releasing don't add more messages
                begin
                  if AMessage^.MessageType and MT_ALLOCATED <> 0 then
                    OPStackNode_MessageLink(DestNode, AMessage)                   // A Datagram Message was allocated, Attach it to the node to be processed in the main loop
                  else begin                                                      // If the Message is not from the Pools then allocate one and copy the contents
                    if OPStackBuffers_AllocateDatagramMessage(NewMessage, AMessage^.MTI, AMessage^.Source.AliasID, AMessage^.Source.ID, AMessage^.Dest.AliasID, AMessage^.Dest.ID, AMessage^.DestFlags) then
                    begin
                      OPStackBuffers_CopyDataArray(NewMessage^.Buffer, @AMessage^.Buffer^.DataArray, AMessage^.Buffer^.DataBufferSize, True);
                      OPStackNode_MessageLink(DestNode, NewMessage)
                    end
                  end
                end else
                begin
                  if AMessage^.MessageType and MT_ALLOCATED <> 0 then
                    OPStackBuffers_DeAllocateMessage(AMessage);
                end
              end;
          MT_STREAM :
              begin
                if DestNode^.State and NS_RELEASING = 0 then                      // if Releasing don't add more messages
                begin
                  if AMessage^.MessageType and MT_ALLOCATED <> 0 then
                    OPStackNode_MessageLink(DestNode, AMessage)                   // A Datagram Message and buffer was allocated Attach it to the node to be processed in the main loop
                  else begin                                                      // If the Message is not from the Pools then allocate one and copy the contents
                    if OPStackBuffers_AllcoateStreamMessage(NewMessage, AMessage^.MTI, AMessage^.Source.AliasID, AMessage^.Source.ID, AMessage^.Dest.AliasID, AMessage^.Dest.ID) then
                    begin
                      OPStackBuffers_CopyDataArray(NewMessage^.Buffer, @AMessage^.Buffer^.DataArray, AMessage^.Buffer^.DataBufferSize, True);
                      OPStackNode_MessageLink(DestNode, NewMessage)
                    end
                  end
                end else
                begin
                  if AMessage^.MessageType and MT_ALLOCATED <> 0 then
                    OPStackBuffers_DeAllocateMessage(AMessage);
                end
              end;
        end
      end;
  end;
end;

// *****************************************************************************
//  procedure MaxAddressByAddressSpace
//     Parameters:
//     Returns:
//
//     Description:
//
// *****************************************************************************
function MaxAddressByAddressSpace(Node: PNMRAnetNode; AddressSpace: Byte): DWord;
 begin
   case AddressSpace of
      MSI_CDI       : begin
                        {$IFDEF SUPPORT_VIRTUAL_NODES}
                        if Node^.State and NS_VIRTUAL <> 0 then
                          Result := USER_MAX_VNODE_CDI_ARRAY
                        else {$ENDIF}
                          Result := USER_MAX_CDI_ARRAY;
                      end;
      MSI_ALL       : Result := $FFFFFFFF;
      MSI_ACDI_MFG  : begin
                        {$IFDEF SUPPORT_VIRTUAL_NODES}
                        if Node^.State and NS_VIRTUAL <> 0 then
                          Result := USER_VNODE_MAX_ACDI_MFG_ARRAY + 1           // for the Version ID Byte
                        else {$ENDIF}
                          Result := USER_MAX_ACDI_MFG_ARRAY + 1                 // for the Version ID Byte
                      end;
      MSI_ACDI_USER : begin
                        {$IFDEF SUPPORT_VIRTUAL_NODES}
                        if Node^.State and NS_VIRTUAL <> 0 then
                          Result := USER_MAX_VNODE_USER_CONFIG_DATA + 1         // for the Version ID Byte
                        else {$ENDIF}
                          Result := USER_MAX_USER_CONFIG_DATA + 1               // for the Version ID Byte
                      end;
      MSI_CONFIG,
      MSI_FDI       : begin
                        Result := AppCallback_AddressSpaceSize(Node, AddressSpace);
                      end
    else
      Result := 0;
    end;
 end;

// *****************************************************************************
//  procedure ProcessAbandonMessages
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure ProcessAbandonMessages(Node: PNMRAnetNode);
begin
  if Node^.IncomingMessages <> nil then
  begin

  end;
end;

// *****************************************************************************
//  procedure ProcessMarkedForDelete
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure ProcessMarkedForDelete(Node: PNMRAnetNode);
var
  DoDeallocate: Boolean;
  i, j: Integer;
  OPStackMessage: POPStackMessage;
begin
  if OPStackNode_TestState(Node, NS_RELEASING) then
  begin
    OPStackMessage := nil;
    DoDeallocate := False;
    if IsOutgoingBufferAvailable then                                         // Make sure the Node is competely finished sending updates/datagrams/streams/etc
      if OPStackNode_TestState(Node, NS_PERMITTED) then
      begin
         if not OPStackNode_IsAnyEventSet(Node^.Events.Consumed) then
           if not OPStackNode_IsAnyEventSet(Node^.Events.Produced) then
             if not OPStackNode_IsAnyPCER_Set(Node) then
               if Node^.Flags = 0 then
                 if Node^.IncomingMessages = nil then
                   if Node^.OutgoingMessages = nil then
                     if OPStackBuffers_AllocateSimpleCANMessage(OPStackMessage, MTI_CAN_AMR, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
                     begin
                       NMRAnetUtilities_LoadSimpleDataWith48BitNodeID(Node^.Info.ID, PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray)^);
                       OPStackMessage^.Buffer^.DataBufferSize := 6;
                       OutgoingMessage(OPStackMessage); // Tell the network we are leaving
                       DoDeallocate := True;
                     end;
      end else
        DoDeallocate := True;                                                   // If it is not in the Permitted state then we are not allowed to send a AMR so just free it

    if DoDeallocate then
    begin
      i := 0;
      while i < NodePool.AllocatedCount do                                         // Search the Allocated List to remove it if it has made it into the list
      begin
        if NodePool.AllocatedList[i] = Node then                                   // Found the node
        begin
          NodePool.AllocatedList[i] := PNMRAnetNode( nil);                         // Nil it in the Allocated List
          j := i;
          while j < NodePool.AllocatedCount - 1 do                                 // Now Pack the list, moving higher indexed Nodes down one
          begin
            NodePool.AllocatedList[j] := NodePool.AllocatedList[j + 1];
            NodePool.AllocatedList[j + 1] := PNMRAnetNode( nil);
            Inc(j);
          end;
          Dec(NodePool.AllocatedCount);
          Node^.State := NS_EMPTY;                                               // Do this last so item is not allocated in an interrupt half way through this
          Break
        end;
        Inc(i);
      end;
    end;
  end;
end;

// *****************************************************************************
//  procedure NodeRunFlagsReply
//     Parameters:
//     Returns:    True if a message was loaded
//     Description: Picks up Flags in the node and tries to send the reply
// *****************************************************************************
function NodeRunFlagsReply(Node: PNMRAnetNode): Boolean;
const
  MF_CRITICAL = MF_DUPLICATE_NODE_ID or MF_DUPLICATE_ALIAS or MF_DUPLICATE_ALIAS_RID;
var
  OPStackMessage: POPStackMessage;
begin
  Result := False;
  OPStackMessage := nil;
  if Node^.Flags <> 0 then                                                      // See if any flags need to be replied to
    if IsOutgoingBufferAvailable then
    begin
       if Node^.Flags and MF_CRITICAL <> 0 then                                 // Critical issue detected (duplicate Node ID or Alias)
       begin
         if Node^.Flags and MF_DUPLICATE_NODE_ID <> 0 then
         begin
           Node^.Flags := Node^.Flags and not MF_DUPLICATE_NODE_ID;             // Clear the Flag
           Node^.iStateMachine := STATE_NODE_DUPLICATE_FULL_ID;
           Result := True;
         end else
         if Node^.Flags and MF_DUPLICATE_ALIAS <> 0 then
         begin
           Node^.Flags := Node^.Flags and not MF_DUPLICATE_ALIAS;               // Clear the Flag
           Node^.iStateMachine := STATE_NODE_INHIBITED;
           Result := True;
         end else
         if Node^.Flags and MF_DUPLICATE_ALIAS_RID <> 0 then                    // MsgFlag, a Duplicate Alias was Detected during a CID message, not a fault just need to respond to claim the Alias
         begin
           if OPStackBuffers_AllocateSimpleCANMessage(OPStackMessage, MTI_CAN_RID, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
           begin
             Node^.Flags := Node^.Flags and not MF_DUPLICATE_ALIAS_RID;         // Clear the Flag
             OutgoingMessage(OPStackMessage);
             Result := True;
           end
         end
       end else
       if Node^.Flags and MF_ALIAS_MAP_ENQUIRY <> 0 then                        // MsgFlag, an AME message needs to be responded to with an AMD
       begin
         if OPStackBuffers_AllocateSimpleCANMessage(OPStackMessage, MTI_CAN_AMD, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
         begin
           NMRAnetUtilities_LoadSimpleDataWith48BitNodeID(Node^.Info.ID, PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray)^);
           Node^.Flags := Node^.Flags and not MF_ALIAS_MAP_ENQUIRY;             // Clear the Flag
           OPStackMessage^.Buffer^.DataBufferSize := 6;
           OutgoingMessage(OPStackMessage);
           Result := True;
         end
       end else
       if Node^.Flags and MF_VERIFY_NODE_ID <> 0 then                           // MsgFlag, a Verify Node ID message needs to be responded to
       begin
         if OPStackBuffers_AllocateOPStackMessage(OPStackMessage, MTI_VERIFIED_NODE_ID_NUMBER, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
         begin
           NMRAnetUtilities_LoadSimpleDataWith48BitNodeID(Node^.Info.ID, PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray)^);
           Node^.Flags := Node^.Flags and not MF_VERIFY_NODE_ID;                // Clear the Flag
           OPStackMessage^.Buffer^.DataBufferSize := 6;
           OutgoingMessage(OPStackMessage);
           Result := True;
         end
       end
    end
end;

// *****************************************************************************
//  procedure NodeRunEventFlagsReply
//     Parameters:
//     Returns:     True if a message was loaded
//     Description: Picks up Event Flags in the node and tries to send the reply
// *****************************************************************************
function NodeRunEventFlagsReply(Node: PNMRAnetNode): Boolean;
var
  EventIndex: Integer;
  State: Byte;
  MTI: Word;
  OPStackMessage: POPStackMessage;
  DynamicEvent: TEventID;
begin
  Result := False;
  if OPStackNode_IsAnyEventSet(Node^.Events.Consumed) then
  begin
    State := 0;
    EventIndex := OPStackNode_NextEventFlag(Node^.Events.Consumed, State);      // what index and what state is it in, and clear it
    if EventIndex > -1  then
    begin
      OPStackMessage := nil;
      case State of
        EVENT_STATE_UNKNOWN : MTI := MTI_CONSUMER_IDENTIFIED_UNKNOWN;
        EVENT_STATE_VALID   : MTI := MTI_CONSUMER_IDENTIFIED_SET;
        EVENT_STATE_INVALID : MTI := MTI_CONSUMER_IDENTIFIED_CLEAR;
      end;

      {$IFDEF SUPPORT_AT_LEAST_ONE_VNODE_CONSUMED_EVENT}
      if Node^.State and NS_VIRTUAL <> 0 then
      begin
        if EventIndex >= USER_MAX_VNODE_SUPPORTED_EVENTS_CONSUMED then
        begin
          if EventIndex < USER_MAX_VNODE_SUPPORTED_EVENTS_CONSUMED + USER_MAX_VNODE_SUPPORTED_DYNAMIC_EVENTS_CONSUMED then
            if AppCallback_DynamicVNodeConsumedEvent(Node, EventIndex - USER_MAX_VNODE_SUPPORTED_EVENTS_CONSUMED, DynamicEvent) then
              if OPStackBuffers_AllocateOPStackMessage(OPStackMessage, MTI, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
              begin
                NMRAnetUtilities_LoadSimpleDataWithEventID(@DynamicEvent, PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray));
                OPStackMessage^.Buffer^.DataBufferSize := 8;
                OutgoingMessage(OPStackMessage);
                Result := True;
              end
        end else
        begin
          if OPStackBuffers_AllocateOPStackMessage(OPStackMessage, MTI, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
          begin
            NMRAnetUtilities_LoadSimpleDataWithEventID(@USER_VNODE_SUPPORTED_EVENTS_CONSUMED[EventIndex], PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray));
            OPStackMessage^.Buffer^.DataBufferSize := 8;
            OutgoingMessage(OPStackMessage);
            Result := True;
          end
        end
      end else
      {$ENDIF}
      begin
        {$IFDEF SUPPORT_AT_LEAST_ONE_CONSUMED_EVENT}
        if EventIndex >= USER_MAX_SUPPORTED_EVENTS_CONSUMED then
        begin
          if EventIndex < USER_MAX_SUPPORTED_EVENTS_CONSUMED + USER_MAX_SUPPORTED_DYNAMIC_EVENTS_CONSUMED then
            if AppCallback_DynamicConsumedEvent(Node, EventIndex - USER_MAX_SUPPORTED_EVENTS_CONSUMED, DynamicEvent) then
              if OPStackBuffers_AllocateOPStackMessage(OPStackMessage, MTI, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
              begin
                NMRAnetUtilities_LoadSimpleDataWithEventID(@DynamicEvent, PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray));
                OPStackMessage^.Buffer^.DataBufferSize := 8;
                OutgoingMessage(OPStackMessage);
                Result := True;
              end
        end else
        begin
          if OPStackBuffers_AllocateOPStackMessage(OPStackMessage, MTI, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
          begin
            NMRAnetUtilities_LoadSimpleDataWithEventID(@USER_SUPPORTED_EVENTS_CONSUMED[EventIndex], PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray));
            OPStackMessage^.Buffer^.DataBufferSize := 8;
            OutgoingMessage(OPStackMessage);
            Result := True;
          end
        end  {$ENDIF}
      end;
    end
  end else
  if OPStackNode_IsAnyEventSet(Node^.Events.Produced) then
  begin
    EventIndex := OPStackNode_NextEventFlag(Node^.Events.Produced, State);      // what index and what state is it in, and clear it
    if EventIndex > -1 then
    begin
      case State of
        EVENT_STATE_UNKNOWN : MTI := MTI_PRODUCER_IDENTIFIED_UNKNOWN;
        EVENT_STATE_VALID   : MTI := MTI_PRODUCER_IDENTIFIED_SET;
        EVENT_STATE_INVALID : MTI := MTI_PRODUCER_IDENTIFIED_CLEAR;
      end;

      {$IFDEF SUPPORT_AT_LEAST_ONE_VNODE_PRODUCED_EVENT}
      if Node^.State and NS_VIRTUAL <> 0 then
      begin
        if EventIndex >= USER_MAX_VNODE_SUPPORTED_EVENTS_PRODUCED then
        begin
          if EventIndex < USER_MAX_VNODE_SUPPORTED_EVENTS_PRODUCED + USER_MAX_VNODE_SUPPORTED_DYNAMIC_EVENTS_PRODUCED then
            if AppCallback_DynamicVNodeProducedEvent(Node, EventIndex - USER_MAX_VNODE_SUPPORTED_EVENTS_PRODUCED, DynamicEvent) then
              if OPStackBuffers_AllocateOPStackMessage(OPStackMessage, MTI, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
              begin
                NMRAnetUtilities_LoadSimpleDataWithEventID(@DynamicEvent, PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray));
                OPStackMessage^.Buffer^.DataBufferSize := 8;
                OutgoingMessage(OPStackMessage);
                Result := True;
              end
        end else
        begin
          if OPStackBuffers_AllocateOPStackMessage(OPStackMessage, MTI, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
          begin
            NMRAnetUtilities_LoadSimpleDataWithEventID(@USER_VNODE_SUPPORTED_EVENTS_PRODUCED[EventIndex], PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray));
            OPStackMessage^.Buffer^.DataBufferSize := 8;
            OutgoingMessage(OPStackMessage);
            Result := True;
          end
        end
      end else
      {$ENDIF}
      begin
        {$IFDEF SUPPORT_AT_LEAST_ONE_PRODUCED_EVENT}
        if EventIndex >= USER_MAX_SUPPORTED_EVENTS_PRODUCED then
        begin
          if EventIndex < USER_MAX_SUPPORTED_EVENTS_PRODUCED + USER_MAX_SUPPORTED_DYNAMIC_EVENTS_PRODUCED then
            if AppCallback_DynamicProducedEvent(Node, EventIndex - USER_MAX_SUPPORTED_EVENTS_PRODUCED, DynamicEvent) then
              if OPStackBuffers_AllocateOPStackMessage(OPStackMessage, MTI, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
              begin
                NMRAnetUtilities_LoadSimpleDataWithEventID(@DynamicEvent, PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray)) ;
                OPStackMessage^.Buffer^.DataBufferSize := 8;
                OutgoingMessage(OPStackMessage);
                Result := True;
              end
        end else
        begin
          if OPStackBuffers_AllocateOPStackMessage(OPStackMessage, MTI, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
          begin
            NMRAnetUtilities_LoadSimpleDataWithEventID(@USER_SUPPORTED_EVENTS_PRODUCED[EventIndex], PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray));
            OPStackMessage^.Buffer^.DataBufferSize := 8;
            OutgoingMessage(OPStackMessage);
            Result := True;
          end
        end   {$ENDIF}
      end
    end
  end
end;

// *****************************************************************************
//  procedure DecodeConfigMemReadWriteHeader
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure DecodeConfigMemReadWriteHeader(Node: PNMRAnetNode; Buffer: PDatagramDataArray; var AddressSpace: Byte; var ConfigAddress: DWord; var ReadCount: DWord; var DataOffset: Byte);
var
  MaxSpaceSize: DWord;
begin
  // Decode the Memory Space and where the Data starts
  DataOffset := 6;
  case Buffer^[1] and $03 of      // Strip off bottom two bits
    MCP_CDI            : AddressSpace := MSI_CDI;
    MCP_ALL            : AddressSpace := MSI_ALL;
    MCP_CONFIGURATION  : AddressSpace := MSI_CONFIG;
    MCP_NONE           :
      begin
        Inc(DataOffset);
        AddressSpace := Buffer^[6]
       end;
  end;
  ConfigAddress := DWord( Buffer^[2] shl 24) or DWord( Buffer^[3] shl 16) or DWord( Buffer^[4] shl 8) or DWord( Buffer^[5]);

  case Buffer^[1] and $F0 of
    MCP_COMMAND_READ_STREAM  : ReadCount := DWord( Buffer^[DataOffset] shl 24) or DWord( Buffer^[DataOffset+1] shl 16) or DWord( Buffer^[DataOffset+2] shl 8) or DWord( Buffer^[DataOffset+3]);
    MCP_COMMAND_READ         : ReadCount := Buffer^[DataOffset] and $7F         // Ignore the upper bit per the spec
  else
     ReadCount := 0;
  end;

     // Test the size against the size of the Address Space and adjust to the Max size if necessary
   MaxSpaceSize := MaxAddressByAddressSpace(Node, AddressSpace);
   if ConfigAddress >= MaxSpaceSize then                               // If the caller overruns the address we are done
     ReadCount := 0
   else begin
     if ConfigAddress + ReadCount > MaxSpaceSize then
       ReadCount := MaxSpaceSize - ConfigAddress;
   end
end;

// *****************************************************************************
//  procedure EncodeConfigMemReadWriteHeader
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure EncodeConfigMemReadWriteHeader(Buffer: PDatagramDataArray; IsRead: Boolean; IsStream: Boolean; AddressSpace: Byte; ConfigAddress: DWord; ReadCount: DWord; UseAddressSpaceByte: Boolean; var DataOffset: Byte);
begin
  Buffer^[0] := DATAGRAM_TYPE_MEMORY_CONFIGURATION;

  // Setup the Command
  if IsRead then
  begin
    if IsStream then
      Buffer^[1] := MCP_COMMAND_READ_STREAM
    else
      Buffer^[1] := MCP_COMMAND_READ;
  end else
  begin
    if IsStream then
      Buffer^[1] := MCP_COMMAND_WRITE_STREAM
    else
      Buffer^[1] := MCP_COMMAND_WRITE;
  end;

  DataOffset := 6;
  if UseAddressSpaceByte or (AddressSpace < MSI_CONFIG) then
  begin
    Inc(DataOffset);
    Buffer^[6] := AddressSpace
  end else
  begin
    case AddressSpace of
      MSI_CDI            : Buffer^[1] := Buffer^[1] or MCP_CDI;
      MSI_ALL            : Buffer^[1] := Buffer^[1] or MCP_ALL;
      MSI_CONFIG         : Buffer^[1] := Buffer^[1] or MCP_CONFIGURATION;
    end
  end;

  Buffer^[2] := ConfigAddress shr 24;
  Buffer^[3] := ConfigAddress shr 16;
  Buffer^[4] := ConfigAddress shr 8;
  Buffer^[5] := ConfigAddress;

  if IsRead then
  begin
    if IsStream then
    begin
      Buffer^[DataOffset] := ReadCount shr 24;
      Buffer^[DataOffset+1] := ReadCount shr 16;
      Buffer^[DataOffset+2] := ReadCount shr 8;
      Buffer^[DataOffset+3] := ReadCount;
    end else
      Buffer^[DataOffset] := ReadCount;
  end;
end;

procedure EncodeConfigMemReadWriteHeaderReply(Buffer: PDatagramDataArray; IsReplyOK, IsRead: Boolean);
begin
  if IsRead then
  begin
    if IsReplyOK then
      Buffer^[1] := Buffer^[1] or MCP_COMMAND_READ_REPLY_OK
    else
      Buffer^[1] := Buffer^[1] or MCP_COMMAND_READ_REPLY_FAIL;
  end else
  begin
    if IsReplyOK then
      Buffer^[1] := Buffer^[1] or MCP_COMMAND_WRITE_REPLY_OK
    else
      Buffer^[1] := Buffer^[1] or MCP_COMMAND_WRITE_REPLY_FAIL;
  end;
end;

// *****************************************************************************
//  procedure NodeRunPCERFlagsReply
//     Parameters:
//     Returns:     True if a message was loaded
//     Description: Picks up PCER Flags in the node and tries to send the reply
// *****************************************************************************
function NodeRunPCERFlagsReply(Node: PNMRAnetNode): Boolean;
var
  EventIndex: Integer;
  OPStackMessage: POPStackMessage;
begin
  Result := False;
  if OPStackNode_IsAnyPCER_Set(Node) then
  begin
    EventIndex := OPStackNode_NextPCER_Flag(Node);
    if EventIndex > -1 then
    begin
      OPStackMessage := nil;
      if OPStackBuffers_AllocateOPStackMessage(OPStackMessage, MTI_PC_EVENT_REPORT, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
      begin
        {$IFDEF SUPPORT_AT_LEAST_ONE_VNODE_PRODUCED_EVENT}
        if Node^.State and NS_VIRTUAL <> 0 then
          NMRAnetUtilities_LoadSimpleDataWithEventID(@USER_VNODE_SUPPORTED_EVENTS_PRODUCED[EventIndex], PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray))
        else
        {$ENDIF}
        begin
          {$IFDEF SUPPORT_AT_LEAST_ONE_NODE_PRODUCED_EVENT}
          NMRAnetUtilities_LoadSimpleDataWithEventID(@USER_SUPPORTED_EVENTS_PRODUCED[EventIndex], PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray));
          {$ENDIF}
        end;
        OPStackMessage^.Buffer^.DataBufferSize := 8;
        OutgoingMessage(OPStackMessage);
        Result := True;
      end
    end
  end
end;

// *****************************************************************************
//  procedure NodeRunMessageReply
//     Parameters:
//     Returns:    True if a message was loaded
//     Description: Picks up Buffers pending in the node and tries to send the reply
// *****************************************************************************
function NodeRunMessageBufferReply(Node: PNMRAnetNode): Boolean;
var
  NextMessage, NewMessage: POPStackMessage;
  LocalMessage: TOPStackMessage;
  LocalBuffer: TSimpleBuffer;
  DatagramBufferPtr: PDatagramBuffer;
  AcdiSnipBufferPtr: PAcdiSnipBuffer;
  i, j: Integer;
  AckFlags: Byte;
  MemorySpaceMaxAddress: DWord;
  AddressSpace, DataOffset: Byte;
  ConfigAddress, ReadCount: DWord;
begin
  Result := False;
  NextMessage := OPStackNode_NextMessage(Node);
  if NextMessage <> nil then
  begin
    case NextMessage^.MTI of
      MTI_SIMPLE_NODE_INFO_REPLY :
          begin
            if IsOutgoingBufferAvailable then
            begin
              if OPStackBuffers_Allcoate_ACDI_SNIP_Message(NewMessage, NextMessage^.MTI, NextMessage^.Source.AliasID, NextMessage^.Source.ID, NextMessage^.Dest.AliasID, NextMessage^.Dest.ID) then
              begin
                AcdiSnipBufferPtr := PAcdiSnipBuffer( PByte( NewMessage^.Buffer));
                AcdiSnipBufferPtr^.DataBufferSize := 0;
                AcdiSnipBufferPtr^.DataArray[AcdiSnipBufferPtr^.DataBufferSize] := ACDI_MFG_VERSION;
                Inc(AcdiSnipBufferPtr^.DataBufferSize);
                j := 0;
                if OPStackNode_TestState(Node, NS_VIRTUAL) then
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
                OutgoingMessage(NewMessage);
                OPStackNode_MessageUnLink(Node, NextMessage);
                OPStackBuffers_DeAllocateMessage(NextMessage);
              end;
            end;
            Exit;
          end;
      MTI_PROTOCOL_SUPPORT_REPLY :
          begin
            if IsOutgoingBufferAvailable then
            begin
              {$IFDEF SUPPORT_VIRTUAL_NODES}
              if OPStackNode_TestState(Node, NS_VIRTUAL) then
              begin
                for i := 0 to LEN_PIV_PROTOCOL-1 do
                  for j := 0 to USER_PIV_VNODE_SUPPORTED_PROTOCOL_COUNT - 1 do
                    NextMessage^.Buffer^.DataArray[i] := NextMessage^.Buffer^.DataArray[i] or USER_PIV_VNODE_SUPPORTED_PROTOCOLS[j][i];
                NextMessage^.Buffer^.DataBufferSize := LEN_PIV_PROTOCOL;
              end else
              {$ENDIF}
              begin
                for i := 0 to LEN_PIV_PROTOCOL-1 do
                  for j := 0 to USER_PIV_SUPPORTED_PROTOCOL_COUNT - 1 do
                    NextMessage^.Buffer^.DataArray[i] := NextMessage^.Buffer^.DataArray[i] or USER_PIV_SUPPORTED_PROTOCOLS[j][i];
                NextMessage^.Buffer^.DataBufferSize := LEN_PIV_PROTOCOL;
              end;
              OPStackNode_MessageUnLink(Node, NextMessage);
              OutgoingMessage(NextMessage);
              Result := True;
            end;
          end;
      MTI_DATAGRAM :
          begin
            DatagramBufferPtr := PDatagramBuffer( PByte( NextMessage^.Buffer));
            AckFlags := $00;                                                    // Default
            case DatagramBufferPtr^.DataArray[0] of
      {        DATAGRAM_TYPE_BOOTLOADER :
                  begin
                    if DatagramBufferPtr^.State and ABS_HASBEENACKED <> 0 then   // After ACKed we can work the reply
                    begin
                      OPStackNode_MessageUnLink(Node, NextMessage);
                      OPStackBuffers_DeAllocateMessage(NextMessage);
                      Exit;                                                     // Don't call Datagram OK again!
                    end;
                  end;         }
              DATAGRAM_TYPE_MEMORY_CONFIGURATION :
                  begin
                    AckFlags := $00;                                            // May want to change this for slow configuration reads/writes
                    if DatagramBufferPtr^.State and ABS_HASBEENACKED <> 0 then   // After ACKed we can work the reply
                    begin
                      DecodeConfigMemReadWriteHeader(Node, @DatagramBufferPtr^.DataArray, AddressSpace, ConfigAddress, ReadCount, DataOffset);
                      case DatagramBufferPtr^.DataArray[1] and $F0 of
                         MCP_COMMAND_READ :
                             begin
                               OPStackNode_MessageUnLink(Node, NextMessage);
                               OPStackBuffers_SwapDestAndSourceIDs(NextMessage);
                               if AddressSpace < MSI_CONFIG then
                                 EncodeConfigMemReadWriteHeader(@DatagramBufferPtr^.DataArray, True, False, AddressSpace, ConfigAddress, ReadCount, True, DataOffset)
                               else
                                 EncodeConfigMemReadWriteHeader(@DatagramBufferPtr^.DataArray, True, False, AddressSpace, ConfigAddress, ReadCount, False, DataOffset);
                               EncodeConfigMemReadWriteHeaderReply(@DatagramBufferPtr^.DataArray, True, True);

                               DatagramBufferPtr^.DataBufferSize := ReadCount+DataOffset;
                               DatagramBufferPtr^.CurrentCount := 0;
                               DatagramBufferPtr^.iStateMachine := 0;
                               case AddressSpace of
                                   MSI_CDI :
                                       begin
                                         {$IFDEF SUPPORT_VIRTUAL_NODES}
                                         if Node^.State and NS_VIRTUAL <> 0 then
                                         begin
                                           for i := 0 to ReadCount - 1 do
                                             DatagramBufferPtr^.DataArray[DataOffset+i] := USER_CDI_VNODE_ARRAY[i+ConfigAddress]
                                         end else {$ENDIF}
                                         begin
                                           for i := 0 to ReadCount - 1 do
                                              DatagramBufferPtr^.DataArray[DataOffset+i] := USER_CDI_ARRAY[i+ConfigAddress];
                                         end;
                                       end;
                                   MSI_ALL :
                                       begin
                                         for i := 0 to ReadCount - 1 do
                                           DatagramBufferPtr^.DataArray[DataOffset+i] := PByte(i)^
                                       end;
                                   MSI_CONFIG :
                                       begin
                                         for i := 0 to ReadCount - 1 do
                                           DatagramBufferPtr^.DataArray[DataOffset+i] := $33;      // TEMPORARY
                                       end;
                                   MSI_ACDI_MFG :
                                       begin
                                         {$IFDEF SUPPORT_VIRTUAL_NODES}
                                         if Node^.State and NS_VIRTUAL <> 0 then
                                         begin
                                           for i := 0 to ReadCount - 1 do
                                             DatagramBufferPtr^.DataArray[DataOffset+i] := USER_VNODE_ACDI_MFG_STRINGS[ConfigAddress + i];
                                         end else {$ENDIF}
                                         begin
                                           for i := 0 to ReadCount - 1 do
                                             DatagramBufferPtr^.DataArray[DataOffset+i] := USER_ACDI_MFG_STRINGS[ConfigAddress + i];
                                         end;
                                       end;
                                   MSI_ACDI_USER :
                                       begin
                                         for i := 0 to ReadCount - 1 do
                                           DatagramBufferPtr^.DataArray[DataOffset+i] := $AA;      // TEMPORARY
                                       end;
                                   MSI_FDI :
                                       begin
                                         for i := 0 to ReadCount - 1 do
                                           DatagramBufferPtr^.DataArray[DataOffset+i] := $FF;      // TEMPORARY
                                       end;
                               end;
                               OutgoingMessage(NextMessage);
                               Exit;                                            // Don't call Datagram OK again!
                             end;
                         MCP_COMMAND_READ_STREAM :
                             begin
                               OPStackNode_MessageUnLink(Node, NextMessage);
                               OPStackBuffers_DeAllocateMessage(NextMessage);
                               Exit;                                            // Don't call Datagram OK again!
                             end;
                         MCP_COMMAND_WRITE :
                             begin

                               OPStackNode_MessageUnLink(Node, NextMessage);
                               OPStackBuffers_DeAllocateMessage(NextMessage);
                               Exit;                                            // Don't call Datagram OK again!
                             end;
                         MCP_COMMAND_WRITE_STREAM :
                             begin
                               OPStackNode_MessageUnLink(Node, NextMessage);
                               OPStackBuffers_DeAllocateMessage(NextMessage);
                               Exit;                                            // Don't call Datagram OK again!
                             end;
                         MCP_OPERATION :
                             begin                                              // We will reuse the Datagram Buffer for this one
                               case DatagramBufferPtr^.DataArray[1] of          // Mask off the upper 2 bits
                                 MCP_OP_GET_CONFIG :
                                     begin
                                       OPstackBuffers_SwapDestAndSourceIDs(NextMessage);      // Reuse the Datagram Buffer
                                       NextMessage^.MTI := MTI_DATAGRAM;
                                       NextMessage^.DestFlags := $00;
                                       DatagramBufferPtr^.DataArray[0] := DATAGRAM_TYPE_MEMORY_CONFIGURATION;
                                       DatagramBufferPtr^.DataArray[1] := MCP_OP_GET_CONFIG_REPLY;
                                       {$IFDEF SUPPORT_VIRTUAL_NODES}
                                       if OPStackNode_TestState(Node, NS_VIRTUAL) then
                                       begin
                                         DatagramBufferPtr^.DataArray[2] := Hi( USER_VNODE_CONFIGMEM_OPTIONS);
                                         DatagramBufferPtr^.DataArray[3] := Lo( USER_VNODE_CONFIGMEM_OPTIONS);
                                         DatagramBufferPtr^.DataArray[4] := USER_VNODE_CONFIGMEM_WRITE_LENGTH;
                                         DatagramBufferPtr^.DataArray[5] := USER_VNODE_CONFIGMEM_HIGHEST_SPACE;
                                         DatagramBufferPtr^.DataArray[6] := USER_VNODE_CONFIGMEM_LOWEST_SPACE;
                                       end else
                                       {$ENDIF}
                                       begin
                                         DatagramBufferPtr^.DataArray[2] := Hi( USER_CONFIGMEM_OPTIONS);
                                         DatagramBufferPtr^.DataArray[3] := Lo( USER_CONFIGMEM_OPTIONS);
                                         DatagramBufferPtr^.DataArray[4] := USER_CONFIGMEM_WRITE_LENGTH;
                                         DatagramBufferPtr^.DataArray[5] := USER_CONFIGMEM_HIGHEST_SPACE;
                                         DatagramBufferPtr^.DataArray[6] := USER_CONFIGMEM_LOWEST_SPACE;
                                       end;
                                       DatagramBufferPtr^.DataBufferSize := 7;
                                       DatagramBufferPtr^.iStateMachine := 0;
                                       DatagramBufferPtr^.CurrentCount := 0;
                                       OPStackNode_MessageUnLink(Node, NextMessage);
                                       OutgoingMessage(NextMessage);
                                       Exit;                                    // Don't call Datagram OK again!
                                     end;
                                 MCP_OP_GET_ADD_SPACE_INFO :
                                     begin
                                       OPstackBuffers_SwapDestAndSourceIDs(NextMessage);      // Reuse the Datagram Buffer
                                       NextMessage^.MTI := MTI_DATAGRAM;
                                       NextMessage^.DestFlags := $00;
                                       DatagramBufferPtr^.DataArray[0] := DATAGRAM_TYPE_MEMORY_CONFIGURATION;
                                       DatagramBufferPtr^.DataArray[1] := MCP_OP_GET_ADD_SPACE_INFO_REPLY;
                                       if AppCallback_AddressSpacePresent(Node, DatagramBufferPtr^.DataArray[2]) then
                                         DatagramBufferPtr^.DataArray[1] := DatagramBufferPtr^.DataArray[1] or MCP_OP_GET_ADD_SPACE_INFO_REPLY_PRESENT;
                                       DatagramBufferPtr^.DataArray[2] := DatagramBufferPtr^.DataArray[2];
                                         // I am not supporting the ability to return anything but a $0 for the lower address so we only deal with offsets from zero in these calls
                                       MemorySpaceMaxAddress := MaxAddressByAddressSpace(Node, DatagramBufferPtr^.DataArray[2]);
                                       DatagramBufferPtr^.DataArray[3] := (DWord(MemorySpaceMaxAddress) shr 24) and $000000FF;
                                       DatagramBufferPtr^.DataArray[4] := (DWord(MemorySpaceMaxAddress) shr 16) and $000000FF;
                                       DatagramBufferPtr^.DataArray[5] := (DWord(MemorySpaceMaxAddress) shr 8) and $000000FF;
                                       DatagramBufferPtr^.DataArray[6] := DWord(MemorySpaceMaxAddress) and $000000FF;
                                       if AppCallback_AddressSpaceReadOnly(Node, DatagramBufferPtr^.DataArray[2]) then
                                         DatagramBufferPtr^.DataArray[7] := $01
                                       else
                                         DatagramBufferPtr^.DataArray[7] := $00;
                                       DatagramBufferPtr^.DataBufferSize := 8;
                                       DatagramBufferPtr^.CurrentCount := 0;
                                       DatagramBufferPtr^.iStateMachine := 0;
                                       OPStackNode_MessageUnLink(Node, NextMessage);
                                       OutgoingMessage(NextMessage);
                                       Exit;                                                     // Don't call Datagram OK again!
                                     end;
                                 MCP_OP_LOCK :
                                     begin
                                       OPStackNode_MessageUnLink(Node, NextMessage);
                                       OPStackBuffers_DeAllocateMessage(NextMessage);
                                       Exit;                                                     // Don't call Datagram OK again!
                                     end;
                                 MCP_OP_GET_UNIQUEID :
                                     begin
                                       OPStackNode_MessageUnLink(Node, NextMessage);
                                       OPStackBuffers_DeAllocateMessage(NextMessage);
                                       Exit;                                                     // Don't call Datagram OK again!
                                     end;
                                 MCP_OP_FREEZE :
                                     begin
                                       OPStackNode_MessageUnLink(Node, NextMessage);
                                       OPStackBuffers_DeAllocateMessage(NextMessage);
                                       Exit;                                                     // Don't call Datagram OK again!
                                     end;
                                 MCP_OP_INDICATE :
                                     begin
                                       OPStackNode_MessageUnLink(Node, NextMessage);
                                       OPStackBuffers_DeAllocateMessage(NextMessage);
                                       Exit;                                                     // Don't call Datagram OK again!
                                     end;
                                 MCP_OP_UPDATE_COMPLETE :
                                     begin
                                       OPStackNode_MessageUnLink(Node, NextMessage);
                                       OPStackBuffers_DeAllocateMessage(NextMessage);
                                       Exit;                                                     // Don't call Datagram OK again!
                                     end;
                                 MCP_OP_RESETS :
                                      begin
                                        {$IFNDEF FPC}
                                        asm
                                          reset;
                                        end;
                                        {$ENDIF}
                                     end
                                 else begin
                                   OPStackNode_MessageUnLink(Node, NextMessage);  // Don't know what that was but we got it, throw it away
                                   OPStackBuffers_DeAllocateMessage(NextMessage);
                                   Exit;                                                     // Don't call Datagram OK again!
                                 end;
                               end {case Operation}
                             end; {MPC_OPERATION}
                      end
                    end;
                  end;
              DATAGRAM_TYPE_TRAIN_CONTROL :
                  begin
                    if DatagramBufferPtr^.State and ABS_HASBEENACKED <> 0 then   // After ACKed we can work the reply
                    begin
                      OPStackNode_MessageUnLink(Node, NextMessage);
                      OPStackBuffers_DeAllocateMessage(NextMessage);
                    end;
                  end;
            end;

            if DatagramBufferPtr^.State and ABS_HASBEENACKED = 0 then
            begin
              if IsOutgoingBufferAvailable then
              begin
                LocalMessage.Buffer := @LocalBuffer;
                LocalMessage.MessageType := MT_SIMPLE;
                OPStackBuffers_LoadDatagramOkMessage(@LocalMessage, NextMessage^.Dest.AliasID, NextMessage^.Dest.ID, NextMessage^.Source.AliasID, NextMessage^.Source.ID, AckFlags);
                OutgoingMessage(@LocalMessage);
                DatagramBufferPtr^.State := DatagramBufferPtr^.State or ABS_HASBEENACKED;
              end
            end

          end;
    end;
  end;
end;

// *****************************************************************************
//  procedure OPStackCore_Timer
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure OPStackCore_Timer;
var
  i: Integer;
begin
  for i := 0 to NodePool.AllocatedCount - 1 do
    Inc(NodePool.AllocatedList[i]^.Login.TimeCounter);
 // NMRAnetBufferPools_100ms_TimeTick;
 // ServiceMode_100ms_TimeTick

end;

// *****************************************************************************
//  procedure NodeRunStateMachine
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure NodeRunStateMachine(Node: PNMRAnetNode);
var
  OPStackMessage: POPStackMessage;
  CAN_MTI: DWord;
begin
  OPStackMessage := nil;
  case Node^.iStateMachine of
    STATE_NODE_START :
      begin
        Node^.Login.iCID := 0;
        Node^.iStateMachine := STATE_NODE_TRANSMIT_CID;
      end;
    STATE_NODE_GENERATE_NODE_ALIAS :
      begin
        Node^.Info.AliasID := NMRAnetUtilities_CreateAliasID(Node^.Login.Seed, False);
        Node^.Login.iCID := 0;
        Node^.iStateMachine := STATE_NODE_TRANSMIT_CID;
      end;
    STATE_RANDOM_NUMBER_GENERATOR :
      begin
        NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed(Node^.Login.Seed);
        Node^.iStateMachine := STATE_NODE_GENERATE_NODE_ALIAS;
      end;
    STATE_NODE_TRANSMIT_CID :
      begin
        if IsOutgoingBufferAvailable then
        begin
          case Node^.Login.iCID of
            0 : CAN_MTI := MTI_CAN_CID0;
            1 : CAN_MTI := MTI_CAN_CID1;
            2 : CAN_MTI := MTI_CAN_CID2;
            3 : CAN_MTI := MTI_CAN_CID3;
          end;
          if OPStackBuffers_AllocateSimpleCANMessage(OPStackMessage, CAN_MTI, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
          begin
            OutgoingMessage(OPStackMessage);
            Node^.iStateMachine := STATE_NODE_NEXT_CDI;
          end
        end;
      end;
    STATE_NODE_NEXT_CDI :
      begin
        if Node^.Login.iCID < 3 then
        begin
          Inc(Node^.Login.iCID);
          Node^.iStateMachine := STATE_NODE_TRANSMIT_CID
        end else
        begin
          Node^.iStateMachine := STATE_NODE_WAITSTATE;
          Node^.Login.TimeCounter := 0;
        end;
      end;
    STATE_NODE_WAITSTATE :
      begin
        if Node^.Login.TimeCounter > MAX_BUS_LOGIN_TIMEOUT then
          Node^.iStateMachine := STATE_NODE_SEND_LOGIN_RID;
      end;
    STATE_NODE_SEND_LOGIN_RID :
      begin
        if OPStackNode_TestFlags(Node, MF_DUPLICATE_ALIAS, True) then
        begin
          Node^.iStateMachine := STATE_RANDOM_NUMBER_GENERATOR;
        end else
        begin
          if IsOutgoingBufferAvailable then
            if OPStackBuffers_AllocateSimpleCANMessage(OPStackMessage, MTI_CAN_RID, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
            begin
              OutgoingMessage(OPStackMessage);
              Node^.iStateMachine := STATE_NODE_SEND_LOGIN_AMD;
            end
        end
      end;
    STATE_NODE_SEND_LOGIN_AMD :
      begin
        if OPStackNode_TestFlags(Node, MF_DUPLICATE_ALIAS, True) then
        begin
          Node^.iStateMachine := STATE_RANDOM_NUMBER_GENERATOR;
        end else
        begin
          if IsOutgoingBufferAvailable then
            if OPStackBuffers_AllocateSimpleCANMessage(OPStackMessage, MTI_CAN_AMD, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
            begin
              NMRAnetUtilities_LoadSimpleDataWith48BitNodeID(Node^.Info.ID, PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray)^);
              OPStackMessage^.Buffer^.DataBufferSize := 6;
              OutgoingMessage(OPStackMessage);
              OPStackNode_SetState(Node, NS_PERMITTED);
              Node^.iStateMachine := STATE_NODE_INITIALIZED;
            end
        end
      end;
    STATE_NODE_INITIALIZED :
      begin
        if OPStackNode_TestFlags(Node, MF_DUPLICATE_ALIAS, True) then
        begin
          Node^.iStateMachine := STATE_RANDOM_NUMBER_GENERATOR;
        end else
        begin
          if IsOutgoingBufferAvailable then
            if OPStackBuffers_AllocateOPStackMessage(OPStackMessage, MTI_INITIALIZATION_COMPLETE, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
            begin
              NMRAnetUtilities_LoadSimpleDataWith48BitNodeID(Node^.Info.ID, PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray)^);
              OPStackMessage^.Buffer^.DataBufferSize := 6;
              OutgoingMessage(OPStackMessage);
              OPStackNode_SetState(Node, NS_INITIALIZED);
              Node^.iStateMachine := STATE_NODE_LOGIN_IDENTIFY_EVENTS;
            end
        end
      end;
    STATE_NODE_LOGIN_IDENTIFY_EVENTS :
      begin
        // Fake an Identify Events to allow the AppCallbacks to be called
        if OPStackBuffers_AllocateOPStackMessage(OPStackMessage, MTI_EVENTS_IDENTIFY, 0, NULL_NODE_ID, Node^.Info.AliasID, Node^.Info.ID) then  // Fake Source Node
        begin
          Node^.iStateMachine := STATE_NODE_PERMITTED;
          Hardware_DisableInterrupts;                                             // don't get stomped on by an incoming message within an interrupt
          IncomingMessageDispatch(OPStackMessage, Node);
          OPStackBuffers_DeAllocateMessage(OPStackMessage);
          Hardware_EnableInterrupts;
        end
      end;
    STATE_NODE_PERMITTED :
      begin
        Hardware_DisableInterrupts;
        if not NodeRunFlagsReply(Node) then
          if not NodeRunEventFlagsReply(Node) then
            if not NodeRunPCERFlagsReply(Node) then
              NodeRunMessageBufferReply(Node);

        ProcessMarkedForDelete(Node);                                           // Handle vNodes marked to be deleted
        ProcessAbandonMessages(Node);
        Hardware_EnableInterrupts;
      end;
    STATE_NODE_INHIBITED :
      begin
        // Any buffers will time out and self release
        if IsOutgoingBufferAvailable then
          if OPStackBuffers_AllocateSimpleCANMessage(OPStackMessage, MTI_CAN_AMR, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then  // Fake Source Node
          begin
            NMRAnetUtilities_LoadSimpleDataWith48BitNodeID(Node^.Info.ID, PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray)^);
            OPStackMessage^.Buffer^.DataBufferSize := 6;
            OutgoingMessage(OPStackMessage);
            OPStackNode_ClearState(Node, NS_PERMITTED);
            OPStackNode_ClearFlags(Node);
            Node^.iStateMachine := STATE_RANDOM_NUMBER_GENERATOR;
          end
      end;
    STATE_NODE_DUPLICATE_FULL_ID :
      begin
        // Any buffers will time out and self release
        if IsOutgoingBufferAvailable then
          if OPStackBuffers_AllocateSimpleCANMessage(OPStackMessage, MTI_CAN_AMR, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then  // Fake Source Node
          begin
            NMRAnetUtilities_LoadSimpleDataWith48BitNodeID(Node^.Info.ID, PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray)^);
            OPStackMessage^.Buffer^.DataBufferSize := 6;
            OutgoingMessage(OPStackMessage);
            OPStackNode_ClearState(Node, NS_PERMITTED);
            OPStackNode_ClearFlags(Node);
            Node^.iStateMachine := STATE_NODE_TAKE_OFFLINE;
          end
      end;
    STATE_NODE_TAKE_OFFLINE :
      begin
        if IsOutgoingBufferAvailable then
          if OPStackBuffers_AllocateOPStackMessage(OPStackMessage, MTI_PC_EVENT_REPORT, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then  // Fake Source Node
          begin
            OPStackMessage^.Buffer^.DataBufferSize := 8;
            PEventID( @OPStackMessage^.Buffer^.DataArray)^ := EVENT_DUPLICATE_ID_DETECTED;
            OutgoingMessage(OPStackMessage);
            Node^.iStateMachine := STATE_NODE_OFFLINE;
          end
      end;
    STATE_NODE_OFFLINE :
      begin
        // Done until reboot
      end
  else
     Node^.iStateMachine := STATE_NODE_INHIBITED;                                // We are confused, start over
  end;
end;

// *****************************************************************************
//  procedure OPStackCore_Process
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure OPStackCore_Process;
var
  Node: PNMRAnetnode;
begin
  if OPStack.State and OPS_PROCESSING <> 0 then
  begin
    Node := OPStackNode_NextNode;
    if Node <> nil then
      NodeRunStateMachine(Node);
    ProcessOutgoingAcdiSnips;
    ProcessOutgoingDatagrams;
    ProcessOutgoingStreams;
  end;
end;

end.
