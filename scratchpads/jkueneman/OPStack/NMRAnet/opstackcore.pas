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
  opstackcanstatemachines,
  template_callbacks,
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
procedure IncomingMessageCallback(AMessage: POPStackMessage);

var
  OPStack: TOPStack;

implementation

// *****************************************************************************
//  procedure OPStack_Initialize
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure OPStack_Initialize;
begin
  OPStack.State := 0;                                                           // User must set OPS_Running to enable Library
end;

// *****************************************************************************
//  procedure OPStackCore_Initialize
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure OPStackCore_Initialize;
begin
  OPStackNode_Initialize;
  OPStack_Initialize;
  OPStackNode_Allocate;                                                         // Allocate the hardware Node
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
                          Result := USER_MAX_VNODE_ACDI_MFG_ARRAY + 1           // for the Version ID Byte
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
                   //     Result := AppCallback_AddressSpaceSize(Node, AddressSpace);
                      end
    else
      Result := 0;
    end;
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
//  procedure ProcessAbandonMessages
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure ProcessAbandonMessages(Node: PNMRAnetNode);
begin
  if Node^.Messages <> nil then
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
                 if OPStack_AllocateSimpleCANMessage(OPStackMessage, MTI_CAN_AMR, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
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
           if OPStack_AllocateSimpleCANMessage(OPStackMessage, MTI_CAN_RID, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
           begin
             Node^.Flags := Node^.Flags and not MF_DUPLICATE_ALIAS_RID;         // Clear the Flag
             OutgoingMessage(OPStackMessage);
             Result := True;
           end
         end
       end else
       if Node^.Flags and MF_ALIAS_MAP_ENQUIRY <> 0 then                        // MsgFlag, an AME message needs to be responded to with an AMD
       begin
         if OPStack_AllocateSimpleCANMessage(OPStackMessage, MTI_CAN_AMD, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
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
         if OPStack_AllocateOPStackMessage(OPStackMessage, MTI_VERIFIED_NODE_ID_NUMBER, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
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
        EVENT_STATE_INVALID : MTI := MTI_CONSUMER_IDENTIFIED_CLEAR
      end;

      {$IFDEF SUPPORT_AT_LEAST_ONE_VNODE_CONSUMED_EVENT}
      if Node^.State and NS_VIRTUAL <> 0 then
      begin
        if EventIndex >= USER_MAX_VNODE_SUPPORTED_EVENTS_CONSUMED then
        begin
          if EventIndex < USER_MAX_VNODE_SUPPORTED_EVENTS_CONSUMED + USER_MAX_VNODE_SUPPORTED_DYNAMIC_EVENTS_CONSUMED then
            if AppCallback_DynamicVNodeConsumedEvent(Node, EventIndex - USER_MAX_VNODE_SUPPORTED_EVENTS_CONSUMED, DynamicEvent) then
              if OPStack_AllocateOPStackMessage(OPStackMessage, MTI, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
              begin
                NMRAnetUtilities_LoadSimpleDataWithEventID(DynamicEvent, PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray)^);
                OPStackMessage^.Buffer^.DataBufferSize := 8;
                OutgoingMessage(OPStackMessage);
                Result := True;
              end
        end else
        begin
          if OPStack_AllocateOPStackMessage(OPStackMessage, MTI, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
          begin
            NMRAnetUtilities_LoadSimpleDataWithEventID(USER_VNODE_SUPPORTED_EVENTS_CONSUMED[EventIndex], PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray)^);
            OPStackMessage^.Buffer^.DataBufferSize := 8;
            OutgoingMessage(OPStackMessage);
            Result := True;
          end
        end
      end else
      {$ENDIF}
      begin
        if EventIndex >= USER_MAX_SUPPORTED_EVENTS_CONSUMED then
        begin
          if EventIndex < USER_MAX_SUPPORTED_EVENTS_CONSUMED + USER_MAX_SUPPORTED_DYNAMIC_EVENTS_CONSUMED then
            if AppCallback_DynamicConsumedEvent(Node, EventIndex - USER_MAX_SUPPORTED_EVENTS_CONSUMED, DynamicEvent) then
              if OPStack_AllocateOPStackMessage(OPStackMessage, MTI, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
              begin
                NMRAnetUtilities_LoadSimpleDataWithEventID(DynamicEvent, PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray)^);
                OPStackMessage^.Buffer^.DataBufferSize := 8;
                OutgoingMessage(OPStackMessage);
                Result := True;
              end
        end else
        begin
          if OPStack_AllocateOPStackMessage(OPStackMessage, MTI, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
          begin
            NMRAnetUtilities_LoadSimpleDataWithEventID(USER_SUPPORTED_EVENTS_CONSUMED[EventIndex], PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray)^);
            OPStackMessage^.Buffer^.DataBufferSize := 8;
            OutgoingMessage(OPStackMessage);
            Result := True;
          end
        end
      end
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
        EVENT_STATE_INVALID : MTI := MTI_PRODUCER_IDENTIFIED_CLEAR
      end;

      {$IFDEF SUPPORT_AT_LEAST_ONE_VNODE_PRODUCED_EVENT}
      if Node^.State and NS_VIRTUAL <> 0 then
      begin
        if EventIndex >= USER_MAX_VNODE_SUPPORTED_EVENTS_PRODUCED then
        begin
          if EventIndex < USER_MAX_VNODE_SUPPORTED_EVENTS_PRODUCED + USER_MAX_VNODE_SUPPORTED_DYNAMIC_EVENTS_PRODUCED then
            if AppCallback_DynamicVNodeProducedEvent(Node, EventIndex - USER_MAX_VNODE_SUPPORTED_EVENTS_PRODUCED, DynamicEvent) then
              if OPStack_AllocateOPStackMessage(OPStackMessage, MTI, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
              begin
                NMRAnetUtilities_LoadSimpleDataWithEventID(DynamicEvent, PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray)^);
                OPStackMessage^.Buffer^.DataBufferSize := 8;
                OutgoingMessage(OPStackMessage);
                Result := True;
              end
        end else
        begin
          if OPStack_AllocateOPStackMessage(OPStackMessage, MTI, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
          begin
            NMRAnetUtilities_LoadSimpleDataWithEventID(USER_VNODE_SUPPORTED_EVENTS_PRODUCED[EventIndex], PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray)^);
            OPStackMessage^.Buffer^.DataBufferSize := 8;
            OutgoingMessage(OPStackMessage);
            Result := True;
          end
        end
      end else
      {$ENDIF}
      begin
        if EventIndex >= USER_MAX_SUPPORTED_EVENTS_PRODUCED then
        begin
          if EventIndex < USER_MAX_SUPPORTED_EVENTS_PRODUCED + USER_MAX_SUPPORTED_DYNAMIC_EVENTS_PRODUCED then
            if AppCallback_DynamicProducedEvent(Node, EventIndex - USER_MAX_SUPPORTED_EVENTS_PRODUCED, DynamicEvent) then
              if OPStack_AllocateOPStackMessage(OPStackMessage, MTI, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
              begin
                NMRAnetUtilities_LoadSimpleDataWithEventID(DynamicEvent, PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray)^) ;
                OPStackMessage^.Buffer^.DataBufferSize := 8;
                OutgoingMessage(OPStackMessage);
                Result := True;
              end
        end else
        begin
          if OPStack_AllocateOPStackMessage(OPStackMessage, MTI, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
          begin
            NMRAnetUtilities_LoadSimpleDataWithEventID(USER_SUPPORTED_EVENTS_PRODUCED[EventIndex], PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray)^);
            OPStackMessage^.Buffer^.DataBufferSize := 8;
            OutgoingMessage(OPStackMessage);
            Result := True;
          end
        end
      end
    end
  end
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
      if OPStack_AllocateOPStackMessage(OPStackMessage, MTI_PC_EVENT_REPORT, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
      begin
        {$IFDEF SUPPORT_AT_LEAST_ONE_VNODE_PRODUCED_EVENT}
        if Node^.State and NS_VIRTUAL <> 0 then
          NMRAnetUtilities_LoadSimpleDataWithEventID(USER_VNODE_SUPPORTED_EVENTS_PRODUCED[EventIndex], PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray)^)
        else
        {$ENDIF}
          NMRAnetUtilities_LoadSimpleDataWithEventID(USER_SUPPORTED_EVENTS_PRODUCED[EventIndex], PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray)^);
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
  AMessagePtr: POPStackMessage;
  AMessage: TOPStackMessage;
  DatagramBufferPtr: PDatagramBuffer;
  i, j: Integer;
  AckFlags: Byte;
  MemorySpaceMaxAddress: DWord;
begin
  Result := False;
  AMessagePtr := OPStackNode_MessageBuffer(Node);
  if AMessagePtr <> nil then
  begin
    case AMessagePtr^.MTI of
      MTI_PROTOCOL_SUPPORT_REPLY :
          begin
            if IsOutgoingBufferAvailable then
            begin
              {$IFDEF SUPPORT_VIRTUAL_NODES}
              if OPStackNode_TestState(Node, NS_VIRTUAL) then
              begin
                for i := 0 to LEN_PIV_PROTOCOL-1 do
                  for j := 0 to USER_PIV_VNODE_SUPPORTED_PROTOCOL_COUNT - 1 do
                    AMessagePtr^.Buffer^.DataArray[i] := AMessagePtr^.Buffer^.DataArray[i] or USER_PIV_VNODE_SUPPORTED_PROTOCOLS[j][i];
                AMessagePtr^.Buffer^.DataBufferSize := LEN_PIV_PROTOCOL;
              end else
              {$ENDIF}
              begin
                for i := 0 to LEN_PIV_PROTOCOL-1 do
                  for j := 0 to USER_PIV_SUPPORTED_PROTOCOL_COUNT - 1 do
                    AMessagePtr^.Buffer^.DataArray[i] := AMessagePtr^.Buffer^.DataArray[i] or USER_PIV_SUPPORTED_PROTOCOLS[j][i];
                AMessagePtr^.Buffer^.DataBufferSize := LEN_PIV_PROTOCOL;
              end;
              OPStackNode_MessageUnLink(Node, AMessagePtr);
              OutgoingMessage(AMessagePtr);
              Result := True;
            end;
          end;
      MTI_DATAGRAM :
          begin
            DatagramBufferPtr := PDatagramBuffer( AMessagePtr^.Buffer);
            AckFlags := $00;                                                    // Default
            case DatagramBufferPtr^.DataArray[0] of
              DATAGRAM_TYPE_BOOTLOADER :
                  begin
                    if DatagramBufferPtr^.State and ABS_HASBEENACKED = 1 then   // After ACKed we can work the reply
                    begin
                      OPStackNode_MessageUnLink(Node, AMessagePtr);
                      OPStack_DeAllocateMessage(AMessagePtr);
                    end;
                  end;
              DATAGRAM_TYPE_MEMORY_CONFIGURATION :
                  begin
                    AckFlags := $00;                                            // May want to change this for slow configuration reads/writes
                    if DatagramBufferPtr^.State and ABS_HASBEENACKED = 1 then   // After ACKed we can work the reply
                    begin
                      case DatagramBufferPtr^.DataArray[1] and $F8 of           // Strip off bottom 3 bits
                         MCP_COMMAND_READ :
                             begin
                               OPStackNode_MessageUnLink(Node, AMessagePtr);
                               OPStack_DeAllocateMessage(AMessagePtr);
                             end;
                         MCP_COMMAND_READ_STREAM :
                             begin
                               OPStackNode_MessageUnLink(Node, AMessagePtr);
                               OPStack_DeAllocateMessage(AMessagePtr);
                             end;
                         MCP_COMMAND_WRITE :
                             begin
                               OPStackNode_MessageUnLink(Node, AMessagePtr);
                               OPStack_DeAllocateMessage(AMessagePtr);
                             end;
                         MCP_COMMAND_WRITE_STREAM :
                             begin
                               OPStackNode_MessageUnLink(Node, AMessagePtr);
                               OPStack_DeAllocateMessage(AMessagePtr);
                             end;
                         MCP_OPERATION :
                             begin
                               // We will reuse the Datagram Buffer for this one
                               case DatagramBufferPtr^.DataArray[1] of      // Mask off the upper 2 bits
                                 MCP_OP_GET_CONFIG :
                                     begin
                                       OPstack_SwapDestAndSourceIDs(AMessagePtr^);      // Reuse the Datagram Buffer
                                       AMessagePtr^.DestFlags := $00;
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
                                       OPStackNode_MessageUnLink(Node, AMessagePtr);
                                       OPStackNode_MessageLink(Node, AMessagePtr);
                                     end;
                                 MCP_OP_GET_ADD_SPACE_INFO :
                                     begin
                                       OPstack_SwapDestAndSourceIDs(AMessagePtr^);      // Reuse the Datagram Buffer
                                       AMessagePtr^.DestFlags := $00;
                                       DatagramBufferPtr^.DataArray[0] := DATAGRAM_TYPE_MEMORY_CONFIGURATION;
                                       DatagramBufferPtr^.DataArray[1] := MCP_OP_GET_ADD_SPACE_INFO_REPLY;
                                       if AppCallback_AddressSpacePresent(Node^, DatagramBufferPtr^.DataArray[2]) then
                                         DatagramBufferPtr^.DataArray[1] := DatagramBufferPtr^.DataArray[1] or MCP_OP_GET_ADD_SPACE_INFO_REPLY_PRESENT;
                                       DatagramBufferPtr^.DataArray[2] := DatagramBufferPtr^.DataArray[2];
                                         // I am not supporting the ability to return anything but a $0 for the lower address so we only deal with offsets from zero in these calls
                                       MemorySpaceMaxAddress := MaxAddressByAddressSpace(Node, DatagramBufferPtr^.DataArray[2]);
                                       DatagramBufferPtr^.DataArray[3] := (DWord(MemorySpaceMaxAddress) shr 24) and $000000FF;
                                       DatagramBufferPtr^.DataArray[4] := (DWord(MemorySpaceMaxAddress) shr 16) and $000000FF;
                                       DatagramBufferPtr^.DataArray[5] := (DWord(MemorySpaceMaxAddress) shr 8) and $000000FF;
                                       DatagramBufferPtr^.DataArray[6] := DWord(MemorySpaceMaxAddress) and $000000FF;
                                       if AppCallback_AddressSpaceReadOnly(Node, DatagramBuffer^.DataArray[2]) then
                                        DatagramBufferPtr^.DataArray[7] := $01
                                       else
                                         DatagramBufferPtr^.DataArray[7] := $00;
                                       DatagramBufferPtr^.DataBufferSize := 8;
                                       DatagramBufferPtr^.CurrentCount := 0;
                                       DatagramBufferPtr^.Statemachine := 0;
                                       OPStackNode_MessageUnLink(Node, AMessagePtr);
                                       OPStackNode_MessageLink(Node, AMessagePtr);
                                     end;
                                 MCP_OP_LOCK :
                                     begin
                                       OPStackNode_MessageUnLink(Node, AMessagePtr);
                                       OPStack_DeAllocateMessage(AMessagePtr);
                                     end;
                                 MCP_OP_GET_UNIQUEID :
                                     begin
                                       OPStackNode_MessageUnLink(Node, AMessagePtr);
                                       OPStack_DeAllocateMessage(AMessagePtr);
                                     end;
                                 MCP_OP_FREEZE :
                                     begin
                                       OPStackNode_MessageUnLink(Node, AMessagePtr);
                                       OPStack_DeAllocateMessage(AMessagePtr);
                                     end;
                                 MCP_OP_INDICATE :
                                     begin
                                       OPStackNode_MessageUnLink(Node, AMessagePtr);
                                       OPStack_DeAllocateMessage(AMessagePtr);
                                     end;
                                 MCP_OP_UPDATE_COMPLETE :
                                     begin
                                       OPStackNode_MessageUnLink(Node, AMessagePtr);
                                       OPStack_DeAllocateMessage(AMessagePtr);
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
                                   // Don't know what that was but we got it, throw it away
                                   OPStackNode_MessageUnLink(Node, AMessagePtr);
                                   OPStack_DeAllocateMessage(AMessagePtr);
                                 end;
                               end {case Operation}
                             end {MPC_OPERATION}
                      end
                    end;
                  end;
              DATAGRAM_TYPE_TRAIN_CONTROL :
                  begin
                    if DatagramBufferPtr^.State and ABS_HASBEENACKED = 1 then   // After ACKed we can work the reply
                    begin
                      OPStackNode_MessageUnLink(Node, AMessagePtr);
                      OPStack_DeAllocateMessage(AMessagePtr);
                    end;
                  end;
            end;
            if DatagramBufferPtr^.State and ABS_HASBEENACKED = 0 then
            begin
              if IsOutgoingBufferAvailable then
              begin
                OPStack_LoadDatagramOkBuffer(AMessage, AMessagePtr^.Dest.AliasID, AMessagePtr^.Dest.ID, AMessagePtr^.Source.AliasID, AMessagePtr^.Source.ID, AckFlags);
                OutgoingMessage(@AMessage);
                DatagramBufferPtr^.State := DatagramBufferPtr^.State and not ABS_HASBEENACKED;
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
//  procedure IncomingMessageCallback
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure IncomingMessageCallback(AMessage: POPStackMessage);
var
  Node: PNMRAnetNode;
  VNodeEventSupported, NodeEventSupported: Boolean;
  VNodeEventIndex, NodeEventIndex, i: Integer;
  NewMessage: POPStackMessage;
  BufferAllocFailed: Boolean;
begin
  BufferAllocFailed := False;
  // First thing is extract the Source Alias and make sure it is not a duplicate of one of our Node or vNode Aliases
  Node := OPStackNode_Find(AMessage, FIND_BY_SOURCE);
  if Node <> nil then
  begin
    if (AMessage^.MTI = MTI_CAN_CID0) or (AMessage^.MTI = MTI_CAN_CID1) or (AMessage^.MTI = MTI_CAN_CID2) or (AMessage^.MTI = MTI_CAN_CID3) then
      OPStackNode_SetFlag(Node, MF_DUPLICATE_ALIAS_RID)                         // A "good" duplicate Alias
    else
      OPStackNode_SetFlag(Node, MF_DUPLICATE_ALIAS);                             // Bad
    Exit;
  end else
  begin
    case (AMessage^.MessageType) and MT_MASK of
        MT_SIMPLE :
            begin
              if AMessage^.MTI and MTI_ADDRESSED_MASK = MTI_ADDRESSED_MASK then
              begin
                Node := OPStackNode_Find(AMessage, FIND_BY_DEST);
                if Node <> nil then
                begin
                  case AMessage^.MTI of
                    MTI_VERIFY_NODE_ID_NUMBER_DEST  :
                        begin
                          OPStackNode_SetFlag(Node, MF_VERIFY_NODE_ID)          // All messages addressed to node get replies even if the payload is wrong!
                        end;
                    MTI_EVENTS_IDENTIFY_DEST          :
                        begin
                          OPStackNode_SetEventConsumedFlags(Node, EVENT_STATE_UNKNOWN);
                          OPStackNode_SetEventProducedFlags(Node, EVENT_STATE_UNKNOWN);
                        end;
                    MTI_PROTOCOL_SUPPORT_INQUIRY      :
                        begin
                          // BITS ARE NEGATIVE LOGIC
                          // Since we don't implement extended protocols yet just reply when we see the start bit set (active 0)
                          if AMessage^.DestFlags and PIP_EXTENSION_START_BIT_MASK = 0 then
                          begin
                            if OPStack_AllocateOPStackMessage(NewMessage, MTI_PROTOCOL_SUPPORT_REPLY, nil, AMessage^.Dest.AliasID, AMessage^.Dest.ID, AMessage^.Source.AliasID, AMessage^.Source.ID) then
                            begin
                              OPStackNode_MessageLink(Node, NewMessage);
                            end else
                              BufferAllocFailed := True
                          end
                        end;
                    MTI_OPTIONAL_INTERACTION_REJECTED :
                        begin
                        end;
                    MTI_DATAGRAM_OK_REPLY :
                        begin
                          NewMessage := OPStackCANStatemachine_FindDatagramWaitingforACKStack(AMessage^.Source, AMessage^.Dest);
                          if NewMessage <> nil then
                            OPStackCANStatemachine_RemoveDatagramWaitingforACKStack(NewMessage);
                        end;
                    MTI_DATAGRAM_REJECTED_REPLY :
                        begin
                          NewMessage := OPStackCANStatemachine_FindDatagramWaitingforACKStack(AMessage^.Source, AMessage^.Dest);
                          if NewMessage <> nil then
                          begin
                            // Need to send it again and again until we give up
                          end;
                        end;
                  end
                end
              end else
              begin
                case AMessage^.MTI of
                  MTI_CAN_AME :
                      begin                                                 // Alias Map Enquiry.....
                        if AMessage^.Buffer^.DataBufferSize = 0 then
                          OPStackNode_SetFlags(MF_ALIAS_MAP_ENQUIRY)
                        else begin;
                          NMRAnetUtilities_SimpleDataToNodeID(@AMessage^.Buffer^.DataArray, AMessage^.Dest.ID);
                          Node := OPStackNode_Find(AMessage, FIND_BY_DEST);  // The full Source ID was filled above so it will be use to search
                          if Node <> nil then
                          begin
                            if OPStackNode_TestState(Node, NS_PERMITTED) then   // Only reply if node is in Permitted state
                              OPStackNode_SetFlag(Node, MF_ALIAS_MAP_ENQUIRY);
                          end;
                        end;
                        Exit;
                      end;
                  MTI_CAN_AMD :
                      begin                                                        // Another node has sent an Alias Map Definition....
                        NMRAnetUtilities_SimpleDataToNodeID(@AMessage^.Buffer^.DataArray, AMessage^.Dest.ID);
                        Node := OPStackNode_Find(AMessage, FIND_BY_DEST);  // The full Source ID was filled above so it will be use to search
                        if Node <> nil then
                          OPStackNode_SetFlags(MF_DUPLICATE_NODE_ID);          // The other node has the same Node ID as we do!  Warning Will Robinson, Warning
                        Exit;
                      end;
                  MTI_VERIFY_NODE_ID_NUMBER   :
                      begin
                        if AMessage^.Buffer^.DataBufferSize = 0 then
                          OPStackNode_SetFlags(MF_VERIFY_NODE_ID)
                        else begin
                          NMRAnetUtilities_SimpleDataToNodeID(@AMessage^.Buffer^.DataArray, AMessage^.Dest.ID);
                          Node := OPStackNode_Find(AMessage, FIND_BY_DEST);       // The full Source ID was filled above so it will be use to search
                          if Node <> nil then
                            OPStackNode_SetFlag(Node, MF_VERIFY_NODE_ID);
                        end;
                        Exit;
                      end;
                  MTI_CONSUMER_IDENTIFY       :
                      begin
                        VNodeEventIndex := -1;
                        NodeEventIndex := -1;
                        VNodeEventSupported := SupportsVNodeEventAsConsumer(Node, @AMessage^.Buffer^.DataArray, VNodeEventIndex);
                        NodeEventSupported := SupportsEventAsConsumer(Node, @AMessage^.Buffer^.DataArray, NodeEventIndex);
                        for i := 0 to NodePool.AllocatedCount - 1 do
                        begin
                          Node := NodePool.AllocatedList[i];
                          if OPStackNode_TestState(Node, NS_VIRTUAL) then
                          begin
                            if VNodeEventSupported then
                              OPStackNode_SetEventFlag(Node^.Events.Consumed, VNodeEventIndex, EVENT_STATE_UNKNOWN);
                          end else
                          begin
                            if NodeEventSupported then
                              OPStackNode_SetEventFlag(Node^.Events.Consumed, NodeEventIndex, EVENT_STATE_UNKNOWN);
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
                        VNodeEventSupported := SupportsVNodeEventAsProducer(Node, @AMessage^.Buffer^.DataArray, VNodeEventIndex);
                        NodeEventSupported := SupportsEventAsProducer(Node, @AMessage^.Buffer^.DataArray, NodeEventIndex);
                        for i := 0 to NodePool.AllocatedCount - 1 do
                        begin
                          Node := NodePool.AllocatedList[i];
                          if OPStackNode_TestState(Node, NS_VIRTUAL) then
                          begin
                            if VNodeEventSupported then
                              OPStackNode_SetEventFlag(Node^.Events.Produced, VNodeEventIndex, EVENT_STATE_UNKNOWN);
                          end else
                          begin
                            if NodeEventSupported then
                              OPStackNode_SetEventFlag(Node^.Events.Produced, NodeEventIndex, EVENT_STATE_UNKNOWN);
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
                          Node := NodePool.AllocatedList[i];
                          OPStackNode_SetEventProducedFlags(Node, EVENT_STATE_UNKNOWN);
                          OPStackNode_SetEventConsumedFlags(Node, EVENT_STATE_UNKNOWN);
                        end;
                        Exit;
                      end;

                end;
              end;
            end;
        MT_DATAGRAM,
        MT_STREAM :
            begin
              Node := OPStackNode_Find(AMessage, FIND_BY_DEST);
              if Node <> nil then
                OPStackNode_MessageLink(Node, AMessage);                          // Attach it to the node to be processed in the main loop
            end;
      end;
  end;
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
          if OPStack_AllocateSimpleCANMessage(OPStackMessage, CAN_MTI, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
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
            if OPStack_AllocateSimpleCANMessage(OPStackMessage, MTI_CAN_RID, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
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
            if OPStack_AllocateSimpleCANMessage(OPStackMessage, MTI_CAN_AMD, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
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
            if OPStack_AllocateOPStackMessage(OPStackMessage, MTI_INITIALIZATION_COMPLETE, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
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
        if OPStack_AllocateOPStackMessage(OPStackMessage, MTI_EVENTS_IDENTIFY, nil, 0, NULL_NODE_ID, Node^.Info.AliasID, Node^.Info.ID) then  // Fake Source Node
        begin
          Node^.iStateMachine := STATE_NODE_PERMITTED;
          Hardware_DisableInterrupts;                                             // don't get stomped on by an incoming message within an interrupt
          IncomingMessageCallback(OPStackMessage);
          OPStack_DeAllocateMessage(OPStackMessage);
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
          if OPStack_AllocateSimpleCANMessage(OPStackMessage, MTI_CAN_AMR, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then  // Fake Source Node
          begin
            OutgoingMessage(OPStackMessage);
            OPStackNode_ClearState(Node, NS_PERMITTED);
            OPStackNode_ClearFlags(Node);
            Node^.iStateMachine := STATE_NODE_TAKE_OFFLINE;
          end
      end;
    STATE_NODE_DUPLICATE_FULL_ID :
      begin
        // Any buffers will time out and self release
        if IsOutgoingBufferAvailable then
          if OPStack_AllocateSimpleCANMessage(OPStackMessage, MTI_CAN_AMR, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then  // Fake Source Node
          begin
            OutgoingMessage(OPStackMessage);
            OPStackNode_ClearState(Node, NS_PERMITTED);
            OPStackNode_ClearFlags(Node);
            Node^.iStateMachine := STATE_NODE_TAKE_OFFLINE;
          end
      end;
    STATE_NODE_TAKE_OFFLINE :
      begin
        if IsOutgoingBufferAvailable then
          if OPStack_AllocateOPStackMessage(OPStackMessage, MTI_PC_EVENT_REPORT, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then  // Fake Source Node
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
    begin
      NodeRunStateMachine(Node);
      OPStackCANStatemachine_ProcessOutgoingDatagramMessage;
      OPStackCANStatemachine_ProcessOutgoingStreamMessage;
    end;
  end;
end;

end.
