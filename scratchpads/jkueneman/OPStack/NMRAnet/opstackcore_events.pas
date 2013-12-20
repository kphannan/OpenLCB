unit opstackcore_events;

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
  opstackbuffers,
  opstacktypes;

procedure IdentifyEvents(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
procedure IdentifyConsumers(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
procedure IdentifyProducers(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
procedure IdentifyRangeConsumers(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
procedure IdentifyRangeProducers(AMessage: POPStackMessage; DestNode: PNMRAnetNode);

function NodeRunPCERFlagsReply(Node: PNMRAnetNode; var OPStackMessage: POPStackMessage): Boolean;
function NodeRunEventFlagsReply(Node: PNMRAnetNode; var OPStackMessage: POPStackMessage): Boolean;
function NodeRunFlagsReply(Node: PNMRAnetNode; var OPStackMessage: POPStackMessage): Boolean;

implementation

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

procedure IdentifyEvents(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
var
  i: Integer;
begin
  if DestNode <> nil then
  begin
    OPStackNode_SetEventConsumedFlags(DestNode, EVENT_STATE_UNKNOWN);
    OPStackNode_SetEventProducedFlags(DestNode, EVENT_STATE_UNKNOWN);
    Exit;
  end else
  begin
    for i := 0 to NodePool.AllocatedCount - 1 do
    begin
      DestNode := NodePool.AllocatedList[i];
      OPStackNode_SetEventProducedFlags(DestNode, EVENT_STATE_UNKNOWN);
      OPStackNode_SetEventConsumedFlags(DestNode, EVENT_STATE_UNKNOWN);
    end;
    Exit;
  end;
end;

procedure IdentifyConsumers(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
var
  VNodeEventIndex, NodeEventIndex, i: Integer;
  LocalDestNode: PNMRAnetNode;
begin
  VNodeEventIndex := -1;
  NodeEventIndex := -1;
  for i := 0 to NodePool.AllocatedCount - 1 do
  begin
    LocalDestNode := NodePool.AllocatedList[i];
    {$IFDEF SUPPORT_VIRTUAL_NODES}
    if LocalDestNode^.State and NS_VIRTUAL <> 0 then
    begin
      if SupportsVNodeEventAsConsumer(LocalDestNode, @AMessage^.Buffer^.DataArray, VNodeEventIndex) then
        OPStackNode_SetEventFlag(LocalDestNode^.Events.Consumed, VNodeEventIndex, EVENT_STATE_UNKNOWN);
    end else
    {$ENDIF}
    begin
      if SupportsEventAsConsumer(LocalDestNode, @AMessage^.Buffer^.DataArray, NodeEventIndex) then
        OPStackNode_SetEventFlag(LocalDestNode^.Events.Consumed, NodeEventIndex, EVENT_STATE_UNKNOWN);
    end
  end;
end;

procedure IdentifyProducers(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
var
  VNodeEventIndex, NodeEventIndex, i: Integer;
  LocalDestNode: PNMRAnetNode;
begin
  VNodeEventIndex := -1;
  NodeEventIndex := -1;
  for i := 0 to NodePool.AllocatedCount - 1 do
  begin
    LocalDestNode := NodePool.AllocatedList[i];
    {$IFDEF SUPPORT_VIRTUAL_NODES}
    if LocalDestNode^.State and NS_VIRTUAL <> 0 then
    begin
      if SupportsVNodeEventAsProducer(LocalDestNode, @AMessage^.Buffer^.DataArray, VNodeEventIndex) then
        OPStackNode_SetEventFlag(LocalDestNode^.Events.Produced, VNodeEventIndex, EVENT_STATE_UNKNOWN);
    end else
    {$ENDIF}
    begin
      if SupportsEventAsProducer(LocalDestNode, @AMessage^.Buffer^.DataArray, NodeEventIndex) then
        OPStackNode_SetEventFlag(LocalDestNode^.Events.Produced, NodeEventIndex, EVENT_STATE_UNKNOWN);
    end
  end;
end;

procedure IdentifyRangeConsumers(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
begin

end;

procedure IdentifyRangeProducers(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
begin

end;

// *****************************************************************************
//  procedure NodeRunPCERFlagsReply
//     Parameters:
//     Returns:     True if a message was loaded
//     Description: Picks up PCER Flags in the node and tries to send the reply
// *****************************************************************************
function NodeRunPCERFlagsReply(Node: PNMRAnetNode; var OPStackMessage: POPStackMessage): Boolean;
var
  EventIndex: Integer;
begin
  Result := False;
  OPStackMessage := nil;
  if OPStackNode_IsAnyPCER_Set(Node) then
  begin
    EventIndex := OPStackNode_NextPCER_Flag(Node);
    if EventIndex > -1 then
    begin
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
        Result := True;
      end
    end
  end
end;

// *****************************************************************************
//  procedure NodeRunFlagsReply
//     Parameters:
//     Returns:    True if a message was loaded
//     Description: Picks up Flags in the node and tries to send the reply
///                : YOU MUST CHECK IsOutgoingBufferAvailable BEFORE CALLING THIS FUNCTION
// *****************************************************************************
function NodeRunFlagsReply(Node: PNMRAnetNode; var OPStackMessage: POPStackMessage): Boolean;
const
  MF_CRITICAL = MF_DUPLICATE_NODE_ID or MF_DUPLICATE_ALIAS or MF_DUPLICATE_ALIAS_RID;
begin
  Result := False;
  OPStackMessage := nil;
  if Node^.Flags <> 0 then                                                      // See if any flags need to be replied to
  begin
     if Node^.Flags and MF_CRITICAL <> 0 then                                 // Critical issue detected (duplicate Node ID or Alias)
     begin
       if Node^.Flags and MF_DUPLICATE_NODE_ID <> 0 then
       begin
         Node^.Flags := Node^.Flags and not MF_DUPLICATE_NODE_ID;             // Clear the Flag
         Node^.iStateMachine := STATE_NODE_DUPLICATE_FULL_ID;
       end else
       if Node^.Flags and MF_DUPLICATE_ALIAS <> 0 then
       begin
         Node^.Flags := Node^.Flags and not MF_DUPLICATE_ALIAS;               // Clear the Flag
         Node^.iStateMachine := STATE_NODE_INHIBITED;
       end else
       if Node^.Flags and MF_DUPLICATE_ALIAS_RID <> 0 then                    // MsgFlag, a Duplicate Alias was Detected during a CID message, not a fault just need to respond to claim the Alias
       begin
         if OPStackBuffers_AllocateSimpleCANMessage(OPStackMessage, MTI_CAN_RID, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
         begin
           Node^.Flags := Node^.Flags and not MF_DUPLICATE_ALIAS_RID;         // Clear the Flag
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
function NodeRunEventFlagsReply(Node: PNMRAnetNode; var OPStackMessage: POPStackMessage): Boolean;
var
  EventIndex: Integer;
  State: Byte;
  MTI: Word;
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
                Result := True;
              end
        end else
        begin
          if OPStackBuffers_AllocateOPStackMessage(OPStackMessage, MTI, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
          begin
            NMRAnetUtilities_LoadSimpleDataWithEventID(@USER_VNODE_SUPPORTED_EVENTS_CONSUMED[EventIndex], PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray));
            OPStackMessage^.Buffer^.DataBufferSize := 8;
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
                Result := True;
              end
        end else
        begin
          if OPStackBuffers_AllocateOPStackMessage(OPStackMessage, MTI, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
          begin
            NMRAnetUtilities_LoadSimpleDataWithEventID(@USER_SUPPORTED_EVENTS_CONSUMED[EventIndex], PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray));
            OPStackMessage^.Buffer^.DataBufferSize := 8;
            Result := True;
          end
        end
        {$ENDIF}
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
                Result := True;
              end
        end else
        begin
          if OPStackBuffers_AllocateOPStackMessage(OPStackMessage, MTI, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
          begin
            NMRAnetUtilities_LoadSimpleDataWithEventID(@USER_VNODE_SUPPORTED_EVENTS_PRODUCED[EventIndex], PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray));
            OPStackMessage^.Buffer^.DataBufferSize := 8;
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
                Result := True;
              end
        end else
        begin
          if OPStackBuffers_AllocateOPStackMessage(OPStackMessage, MTI, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
          begin
            NMRAnetUtilities_LoadSimpleDataWithEventID(@USER_SUPPORTED_EVENTS_PRODUCED[EventIndex], PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray));
            OPStackMessage^.Buffer^.DataBufferSize := 8;
            Result := True;
          end
        end
        {$ENDIF}
      end
    end
  end
end;

end.

