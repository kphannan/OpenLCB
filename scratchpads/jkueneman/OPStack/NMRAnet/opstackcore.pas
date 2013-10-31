unit opstackcore;

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  // Compile in different hardware layer interfaces here
  {$IFDEF HARDWARE_TEMPLATE}hardware_template, template_node, template_vnode,{$ENDIF}
  {$IFDEF HARDWARE_DSPIC_CAN}hardware_dspic_CAN,{$ENDIF}
  {$IFDEF HARDWARE_ENC28J60}hardware_ENC28j60,{$ENDIF}
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
procedure IncomingMessageCallback(AMessage: PSimpleMessage);

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
//  procedure ProcessMarkedForDeleteNodes
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure ProcessMarkedForDeleteNodes(Node: PNMRAnetNode);
var
  DoDeallocate: Boolean;
  i, j: Integer;
  SimpleMessage: PSimpleMessage;
begin
  if OPStackNode_TestState(Node, NS_RELEASING) then
  begin
    SimpleMessage := nil;
    DoDeallocate := False;
    if IsOutgoingBufferAvailable then                                         // Make sure the Node is competely finished sending updates/datagrams/streams/etc
      if OPStackNode_TestState(Node, NS_PERMITTED) then
      begin
         if not OPStackNode_IsAnyEventSet(Node^.Events.Consumed) then
           if not OPStackNode_IsAnyEventSet(Node^.Events.Produced) then
             if not OPStackNode_IsAnyPCER_Set(Node) then
               if Node^.Flags = 0 then
                 if OPStack_AllocateCANMessage(SimpleMessage, MTI_AMR, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
                 begin
                   OPStack_SetAsCAN_MTI(SimpleMessage);
                   NMRAnetUtilities_LoadCANDataWith48BitNodeID(Node^.Info.ID, PCANDataArray(@SimpleMessage^.Buffer^.DataArray)^);
                   SimpleMessage^.Buffer^.DataBufferSize := 6;
                   OutgoingMessage(SimpleMessage); // Tell the network we are leaving
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
  SimpleMessage: PSimpleMessage;
begin
  Result := False;
  SimpleMessage := nil;
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
           if OPStack_AllocateMessage(SimpleMessage, MTI_RID, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
           begin
             Node^.Flags := Node^.Flags and not MF_DUPLICATE_ALIAS_RID;         // Clear the Flag
             OPStack_SetAsCAN_MTI(SimpleMessage);
             OutgoingMessage(SimpleMessage);
             Result := True;
           end
         end
       end else
       if Node^.Flags and MF_ALIAS_MAP_ENQUIRY <> 0 then                        // MsgFlag, an AMD message needs to be responded to
       begin
         if OPStack_AllocateCANMessage(SimpleMessage, MTI_AMD, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
         begin
           NMRAnetUtilities_LoadCANDataWith48BitNodeID(Node^.Info.ID, PCANDataArray(@SimpleMessage^.Buffer^.DataArray)^);
           Node^.Flags := Node^.Flags and not MF_ALIAS_MAP_ENQUIRY;             // Clear the Flag
           SimpleMessage^.Buffer^.DataBufferSize := 6;
           OPStack_SetAsCAN_MTI(SimpleMessage);
           OutgoingMessage(SimpleMessage);
           Result := True;
         end
       end else
       if Node^.Flags and MF_VERIFY_NODE_ID <> 0 then                           // MsgFlag, a Verify Node ID message needs to be responded to
       begin
         if OPStack_AllocateCANMessage(SimpleMessage, MTI_VERIFIED_NODE_ID_NUMBER, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
         begin
           NMRAnetUtilities_LoadCANDataWith48BitNodeID(Node^.Info.ID, PCANDataArray(@SimpleMessage^.Buffer^.DataArray)^);
           Node^.Flags := Node^.Flags and not MF_VERIFY_NODE_ID;                // Clear the Flag
           SimpleMessage^.Buffer^.DataBufferSize := 6;
           OutgoingMessage(SimpleMessage);
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
  MTI: DWord;
  SimpleMessage: PSimpleMessage;
  DynamicEvent: TEventID;
begin
  Result := False;
  if OPStackNode_IsAnyEventSet(Node^.Events.Consumed) then
  begin
    State := 0;
    EventIndex := OPStackNode_NextEventFlag(Node^.Events.Consumed, State);      // what index and what state is it in, and clear it
    if EventIndex > -1  then
    begin
      SimpleMessage := nil;
      case State of
        EVENT_STATE_UNKOWN  : MTI := MTI_CONSUMER_IDENTIFIED_UNKNOWN;
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
              if OPStack_AllocateCANMessage(SimpleMessage, MTI, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
              begin
                NMRAnetUtilities_LoadCANDataWithEventID(DynamicEvent, PCANDataArray(@SimpleMessage^.Buffer^.DataArray)^);
                SimpleMessage^.Buffer^.DataBufferSize := 8;
                OutgoingMessage(SimpleMessage);
                Result := True;
              end
        end else
        begin
          if OPStack_AllocateCANMessage(SimpleMessage, MTI, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
          begin
            NMRAnetUtilities_LoadCANDataWithEventID(USER_SUPPORTED_VNODE_EVENTS_CONSUMED[EventIndex], PCANDataArray(@SimpleMessage^.Buffer^.DataArray)^);
            SimpleMessage^.Buffer^.DataBufferSize := 8;
            OutgoingMessage(SimpleMessage);
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
              if OPStack_AllocateCANMessage(SimpleMessage, MTI, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
              begin
                NMRAnetUtilities_LoadCANDataWithEventID(DynamicEvent, PCANDataArray(@SimpleMessage^.Buffer^.DataArray)^);
                SimpleMessage^.Buffer^.DataBufferSize := 8;
                OutgoingMessage(SimpleMessage);
                Result := True;
              end
        end else
        begin
          if OPStack_AllocateCANMessage(SimpleMessage, MTI, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
          begin
            NMRAnetUtilities_LoadCANDataWithEventID(USER_SUPPORTED_EVENTS_CONSUMED[EventIndex], PCANDataArray(@SimpleMessage^.Buffer^.DataArray)^);
            SimpleMessage^.Buffer^.DataBufferSize := 8;
            OutgoingMessage(SimpleMessage);
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
        EVENT_STATE_UNKOWN  : MTI := MTI_PRODUCER_IDENTIFIED_UNKNOWN;
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
              if OPStack_AllocateCANMessage(SimpleMessage, MTI, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
              begin
                NMRAnetUtilities_LoadCANDataWithEventID(DynamicEvent, PCANDataArray(@SimpleMessage^.Buffer^.DataArray)^);
                SimpleMessage^.Buffer^.DataBufferSize := 8;
                OutgoingMessage(SimpleMessage);
                Result := True;
              end
        end else
        begin
          if OPStack_AllocateCANMessage(SimpleMessage, MTI, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
          begin
            NMRAnetUtilities_LoadCANDataWithEventID(USER_SUPPORTED_VNODE_EVENTS_PRODUCED[EventIndex], PCANDataArray(@SimpleMessage^.Buffer^.DataArray)^);
            SimpleMessage^.Buffer^.DataBufferSize := 8;
            OutgoingMessage(SimpleMessage);
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
              if OPStack_AllocateCANMessage(SimpleMessage, MTI, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
              begin
                NMRAnetUtilities_LoadCANDataWithEventID(DynamicEvent, PCANDataArray(@SimpleMessage^.Buffer^.DataArray)^) ;
                SimpleMessage^.Buffer^.DataBufferSize := 8;
                OutgoingMessage(SimpleMessage);
                Result := True;
              end
        end else
        begin
          if OPStack_AllocateCANMessage(SimpleMessage, MTI, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
          begin
            NMRAnetUtilities_LoadCANDataWithEventID(USER_SUPPORTED_EVENTS_PRODUCED[EventIndex], PCANDataArray(@SimpleMessage^.Buffer^.DataArray)^);
            SimpleMessage^.Buffer^.DataBufferSize := 8;
            OutgoingMessage(SimpleMessage);
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
  SimpleMessage: PSimpleMessage;
begin
  Result := False;
  if OPStackNode_IsAnyPCER_Set(Node) then
  begin
    EventIndex := OPStackNode_NextPCER_Flag(Node);
    if EventIndex > -1 then
    begin
      SimpleMessage := nil;
      if OPStack_AllocateCANMessage(SimpleMessage, MTI_PC_EVENT_REPORT, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
      begin
        {$IFDEF SUPPORT_AT_LEAST_ONE_VNODE_PRODUCED_EVENT}
        if Node^.State and NS_VIRTUAL <> 0 then
          NMRAnetUtilities_LoadCANDataWithEventID(USER_SUPPORTED_VNODE_EVENTS_PRODUCED[EventIndex], PCANDataArray(@SimpleMessage^.Buffer^.DataArray)^)
        else
        {$ENDIF}
          NMRAnetUtilities_LoadCANDataWithEventID(USER_SUPPORTED_EVENTS_PRODUCED[EventIndex], PCANDataArray(@SimpleMessage^.Buffer^.DataArray)^);
        SimpleMessage^.Buffer^.DataBufferSize := 8;
        OutgoingMessage(SimpleMessage);
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
begin
  Result := False
end;


// *****************************************************************************
//  procedure NodeRunStateMachine
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure NodeRunStateMachine(Node: PNMRAnetNode);
var
  SimpleMessage: PSimpleMessage;
  MTI: DWord;
begin
  SimpleMessage := nil;
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
            0 : MTI := MTI_CID0 or DWord((Node^.Info.ID[1] shr 12) and $00000FFF);
            1 : MTI := MTI_CID1 or DWord( Node^.Info.ID[1] and $00000FFF);
            2 : MTI := MTI_CID2 or DWord((Node^.Info.ID[0] shr 12) and $00000FFF);
            3 : MTI := MTI_CID3 or DWord( Node^.Info.ID[0] and $00000FFF);
          end;
          if OPStack_AllocateMessage(SimpleMessage, MTI, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
          begin
            OPStack_SetAsCAN_MTI(SimpleMessage);
            OutgoingMessage(SimpleMessage);
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
            if OPStack_AllocateMessage(SimpleMessage, MTI_RID, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
            begin
              OPStack_SetAsCAN_MTI(SimpleMessage);
              OutgoingMessage(SimpleMessage);
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
            if OPStack_AllocateCANMessage(SimpleMessage, MTI_AMD, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
            begin
              NMRAnetUtilities_LoadCANDataWith48BitNodeID(Node^.Info.ID, PCANDataArray(@SimpleMessage^.Buffer^.DataArray)^);
              SimpleMessage^.Buffer^.DataBufferSize := 6;
              OPStack_SetAsCAN_MTI(SimpleMessage);
              OutgoingMessage(SimpleMessage);
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
            if OPStack_AllocateCANMessage(SimpleMessage, MTI_INITIALIZATION_COMPLETE, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then
            begin
              NMRAnetUtilities_LoadCANDataWith48BitNodeID(Node^.Info.ID, PCANDataArray(@SimpleMessage^.Buffer^.DataArray)^);
              SimpleMessage^.Buffer^.DataBufferSize := 6;
              OutgoingMessage(SimpleMessage);
              OPStackNode_SetState(Node, NS_INITIALIZED);
              Node^.iStateMachine := STATE_NODE_LOGIN_IDENTIFY_EVENTS;
            end
        end
      end;
    STATE_NODE_LOGIN_IDENTIFY_EVENTS :
      begin
        // Fake an Identify Events to allow the AppCallbacks to be called
        if OPStack_AllocateMessage(SimpleMessage, MTI_EVENTS_IDENTIFY, nil, 0, NULL_NODE_ID, Node^.Info.AliasID, Node^.Info.ID) then  // Fake Source Node
        begin
          Node^.iStateMachine := STATE_NODE_PERMITTED;
          Hardware_DisableInterrupts;                                             // don't get stomped on by an incoming message within an interrupt
          IncomingMessageCallback(SimpleMessage);
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

        ProcessMarkedForDeleteNodes(Node);                                      // Handle vNodes marked to be deleted
        Hardware_EnableInterrupts;

    //    ProcessNode(Node, @CANBuffer);                                        // Handle auto Actions to CAN/NMRAnet messages coming in
    //    ProcessOutgoingMessages(Node, @CANBuffer);                            // Handle outgoing messages like Datagrams
    //    ProcessAbandonBuffers(Node);                                          // Handle (free) buffers (datagrams mainly) that appear to be abandon in midstream

      end;
    STATE_NODE_INHIBITED :
      begin
        // Any buffers will time out and self release
        if IsOutgoingBufferAvailable then
          if OPStack_AllocateMessage(SimpleMessage, MTI_AMR, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then  // Fake Source Node
          begin
            OPStack_SetAsCAN_MTI(SimpleMessage);
            OutgoingMessage(SimpleMessage);
            OPStackNode_ClearState(Node, NS_PERMITTED);
            OPStackNode_ClearFlags(Node);
            Node^.iStateMachine := STATE_NODE_TAKE_OFFLINE;
          end
      end;
    STATE_NODE_DUPLICATE_FULL_ID :
      begin
        // Any buffers will time out and self release
        if IsOutgoingBufferAvailable then
          if OPStack_AllocateMessage(SimpleMessage, MTI_AMR, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then  // Fake Source Node
          begin
            OPStack_SetAsCAN_MTI(SimpleMessage);
            OutgoingMessage(SimpleMessage);
            OPStackNode_ClearState(Node, NS_PERMITTED);
            OPStackNode_ClearFlags(Node);
            Node^.iStateMachine := STATE_NODE_TAKE_OFFLINE;
          end
      end;
    STATE_NODE_TAKE_OFFLINE :
      begin
        if IsOutgoingBufferAvailable then
          if OPStack_AllocateCANMessage(SimpleMessage, MTI_PC_EVENT_REPORT, nil, Node^.Info.AliasID, Node^.Info.ID, 0, NULL_NODE_ID) then  // Fake Source Node
          begin
            SimpleMessage^.Buffer^.DataBufferSize := 8;
            PEventID( @SimpleMessage^.Buffer^.DataArray)^ := EVENT_DUPLICATE_ID_DETECTED;
            OPStack_SetAsCAN_MTI(SimpleMessage);
            OutgoingMessage(SimpleMessage);
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
procedure IncomingMessageCallback(AMessage: PSimpleMessage);
var
  Node: PNMRAnetNode;
  ID: DWord;
  ReleaseMessage: Boolean;
begin
  ReleaseMessage := True;
  // First thing is extract the Source Alias and make sure it is not a duplicate of one of our Node or vNode Aliases
  Node := OPStackNode_Find(AMessage, FIND_BY_SOURCE);
  if Node <> nil then
  begin
    ID := AMessage^.MTI and MTI_CID_MASK;
    if (ID = MTI_CID0) or (ID = MTI_CID1) or (ID = MTI_CID2) or (ID = MTI_CID3) then
      OPStackNode_SetFlag(Node, MF_DUPLICATE_ALIAS_RID)                         // A "good" duplicate Alias
    else
      OPStackNode_SetFlag(Node, MF_DUPLICATE_ALIAS);                             // Bad
    Exit;
  end else
  begin
    ID := AMessage^.MTI and MTI_MASK;
    case (AMessage^.MessageType) and $7F of
        MT_SIMPLE :
            begin
              case ID of
                MTI_AME  :   // Alias Map Enquiry.....
                    begin
                      Node := OPStackNode_Find(AMessage, FIND_BY_DEST);
                      if Node <> nil then
                        OPStackNode_SetFlag(Node, MF_ALIAS_MAP_ENQUIRY);
                      Exit
                    end;
                MTI_VERIFY_NODE_ID_NUMBER   :
                    begin
                      OPStackNode_SetFlags(MF_VERIFY_NODE_ID);                  // THIS IS NOT CLEAR IN THE SPEC
                      Exit
                    end;
              end
            end;
        MT_CAN :
            begin
              case ID of
                MTI_AME :
                    begin                                                 // Alias Map Enquiry.....
                      NMRAnetUtilities_CANDataToNodeID(@AMessage^.Buffer^.DataArray, AMessage^.Dest.ID, 0);
                      Node := OPStackNode_Find(AMessage, FIND_BY_DEST);  // The full Source ID was filled above so it will be use to search
                      if Node <> nil then
                      begin
                        if OPStackNode_TestState(Node, NS_PERMITTED) then   // Only reply if node is in Permitted state
                          OPStackNode_SetFlag(Node, MF_ALIAS_MAP_ENQUIRY);
                      end;
                      Exit;
                    end;
                MTI_AMD :
                    begin                                                        // Another node has sent an Alias Map Definition....
                      NMRAnetUtilities_CANDataToNodeID(@AMessage^.Buffer^.DataArray, AMessage^.Dest.ID, 0);
                      Node := OPStackNode_Find(AMessage, FIND_BY_DEST);  // The full Source ID was filled above so it will be use to search
                      if Node <> nil then
                        OPStackNode_SetFlags(MF_DUPLICATE_NODE_ID);          // The other node has the same Node ID as we do!  Warning Will Robinson, Warning
                      Exit;
                    end;
                MTI_VERIFY_NODE_ID_NUMBER   :
                    begin
                      NMRAnetUtilities_CANDataToNodeID(@AMessage^.Buffer^.DataArray, AMessage^.Dest.ID, 0);
                      Node := OPStackNode_Find(AMessage, FIND_BY_DEST);       // The full Source ID was filled above so it will be use to search
                      if Node <> nil then
                        OPStackNode_SetFlag(Node, MF_VERIFY_NODE_ID);
                      Exit;
                    end;
              end;
            end;
        MT_DATAGRAM :
            begin

            end;
        MT_STREAM :
            begin

            end
      end;
  end;
  if ReleaseMessage then
    OPStack_DeAllocateMessage(AMessage); ;
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
    end;
  end;
end;

end.
