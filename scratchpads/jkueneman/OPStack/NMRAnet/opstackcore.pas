unit opstackcore;

// TODOs
//   2) Check for Abandon Datagrams/Streams then Free
//   3) ACDI Read and Write not implemented correctly
//   4) FDI not implemented correctly
//   5) Do something with MTI_STREAM_INIT_REPLY error code results
//   6) Handle Optional Interaction Rejected messages.... Buffer them??????


{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  template_hardware,
  template_configuration,
  nmranetutilities,
  nmranetdefines,
  opstackdefines,
  opstackbuffers,
  opstacktypes,
  opstacknode,
  template_event_callbacks,
  template_node,
  template_vnode,
  opstackcore_events,
  opstackcore_can,
  opstackcore_pip,
  opstackcore_basic,
  {$IFDEF SUPPORT_STREAMS}opstackcore_stream,{$ENDIF}
  {$IFDEF SUPPORT_TRACTION}opstackcore_traction,{$ENDIF}
  {$IFDEF SUPPORT_TRACTION_PROXY}opstackcore_traction_proxy,{$ENDIF}
  opstackcore_snip,
  opstackcore_learn,
  opstackcore_datagram;

// User callable functions
procedure OPStackCore_Initialize;                                               // Call once on program startup
function OPStackCore_Process: PNMRAnetnode;                                     // Call as often as possible
procedure OPStackCore_Timer;                                                    // Call every 100ms
procedure OPStackCore_Enable(DoEnable: Boolean);                                // Enable Process Statemachine
function OPStackCore_IsRunning: Boolean;

// Callback from the Hardware when a message is received do not call directly
procedure IncomingMessageDispatch(AMessage: POPStackMessage; DestNode, SourceNode: PNMRAnetNode);

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
  TemplateConfiguration_Initialize;
  Hardware_Initialize;
  OPStackNode_Initialize;
  OPStackBuffers_Initialize;
  OPStackCoreDatagram_Initialize;
  {$IFDEF SUPPORT_STREAMS}
  OPStackCoreStream_Initialize;
  {$ENDIF}
  OPStackNode_Allocate;        // Allocate the hardware Node
end;

// *****************************************************************************
//  procedure CheckAndDeallocateMessage
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure CheckAndDeallocateMessage(AMessage: POPStackMessage);
begin
  if AMessage^.MessageType and  MT_ALLOCATED <> 0 then
    OPStackBuffers_DeAllocateMessage(AMessage);
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
//                  SourceNode : Pointer to a NMRAnet Node if the source alias from
//                               the message mactches.  This may be a problem so it
//                               need to be looked at here.
//     Returns:
//     Description:
// *****************************************************************************
procedure IncomingMessageDispatch(AMessage: POPStackMessage; DestNode, SourceNode: PNMRAnetNode);
begin

  // If the Node is being Release then don't do anything except deallocate the message if needed
  if DestNode <> nil then
    if DestNode^.State and NS_RELEASING <> 0 then
    begin
      CheckAndDeallocateMessage(AMessage);
      Exit;
    end;

  // Next is to extract the Source Alias and make sure it is not a duplicate of one of our Node or vNode Aliases
  if SourceNode <> nil then
  begin
    DuplicateSourceDetected(AMessage, SourceNode);
    CheckAndDeallocateMessage(AMessage);
    Exit;
  end;

  // If it is a CAN message then handle it
  if AMessage^.MessageType and MT_CAN_TYPE <> 0 then
  begin
    case AMessage^.MTI of
        MTI_CAN_AME : AliasMappingEnquiry(AMessage, DestNode);
        MTI_CAN_AMD : AliasMappingDefinition(AMessage, DestNode);
        MTI_CAN_AMR : AliasMappingReset(AMessage, DestNode);
    end; {case}
    CheckAndDeallocateMessage(AMessage);
    Exit;
  end;

  // It is a OLCB message
  case (AMessage^.MessageType) and MT_MASK of
      MT_SIMPLE :
          begin
            if AMessage^.MTI and MTI_ADDRESSED_MASK = MTI_ADDRESSED_MASK then   // Handle Simple Messages that may be addressed to one of our nodes
            begin
              if DestNode <> nil then                                           // If it is addressed and the DestNode = nil then it is not for us
              begin                                                             // We send all messages in to test for Releasing so this test is necessary
                case AMessage^.MTI of
                  MTI_SIMPLE_NODE_INFO_REQUEST      : begin SimpleNodeInfoRequest(AMessage, DestNode); Exit; end;         // Allocates Buffer to be processed in main loop
                  MTI_VERIFY_NODE_ID_NUMBER_DEST    : begin VerifyNodeIdByDestination(AMessage, DestNode); Exit; end;     // Sets Flag(s) to be processed in main loop
                  MTI_EVENTS_IDENTIFY_DEST          : begin IdentifyEvents(AMessage, DestNode); Exit; end;                // Sets Flag(s) to be processed in main loop
                  MTI_PROTOCOL_SUPPORT_INQUIRY      : begin ProtocolSupportInquiry(AMessage, DestNode); Exit; end;        // Allocates Buffer to be processed in main loop
                  MTI_DATAGRAM_OK_REPLY             : begin DatagramOkReply(AMessage, DestNode); Exit; end;               // Updates internal states
                  MTI_DATAGRAM_REJECTED_REPLY       : begin DatagramRejectedReply(AMessage, DestNode); Exit; end;         // Updates internal states
                  {$IFDEF SUPPORT_TRACTION}
                  MTI_TRACTION_PROTOCOL             : begin TractionProtocol(AMessage, DestNode); Exit; end;              // Allocates Buffer to be processed in main loop
                  {$ENDIF}
                  {$IFDEF SUPPORT_TRACTION_PROXY}
                  MTI_TRACTION_PROXY_PROTOCOL       : begin TractionProxyProtocol(AMessage, DestNode); Exit; end;         // Allocates Buffer to be processed in main loop
                  {$ENDIF}
                  {$IFDEF SUPPORT_STREAMS}
                  MTI_STREAM_INIT_REQUEST           : begin StreamInitRequest(AMessage, DestNode); Exit; end;            // Allocates Buffer to be processed in main loop
                  MTI_STREAM_INIT_REPLY             : begin StreamInitReply(AMessage, DestNode); Exit; end;              // Allocates Buffer to be processed in main loop
                  MTI_STREAM_PROCEED                : begin StreamProceed(AMessage, DestNode); Exit; end;                // Allocates Buffer to be processed in main loop
                  MTI_STREAM_COMPLETE               : begin StreamComplete(AMessage, DestNode); Exit; end;               // Allocates Buffer to be processed in main loop
                  {$ENDIF}
                  MTI_OPTIONAL_INTERACTION_REJECTED : begin end
                else
                  OptionalInteractionRejected(AMessage, DestNode, True);        // Unknown message, permenent error
                end; {case}
              end else
                Exit;                                                           // It is not for of or our nodes
            end else
            begin                                                               // Is not an Addressed message so handle it, the handler must decide what to do with DestNode = nil
              case AMessage^.MTI of
                  MTI_VERIFY_NODE_ID_NUMBER   : begin VerifyNodeId(AMessage, DestNode); Exit; end;              // Sets Flag(s) to be processed in main loop
                  MTI_CONSUMER_IDENTIFY       : begin IdentifyConsumers(AMessage, DestNode); Exit; end;         // Sets Flag(s) to be processed in main loop
                  MTI_CONSUMER_IDENTIFY_RANGE : begin IdentifyRangeConsumers(AMessage, DestNode); Exit; end;    // Sets Flag(s) to be processed in main loop
                  MTI_PRODUCER_IDENDIFY       : begin IdentifyProducers(AMessage, DestNode); Exit; end;         // Sets Flag(s) to be processed in main loop
                  MTI_PRODUCER_IDENTIFY_RANGE : begin IdentifyRangeProducers(AMessage, DestNode); end;          // Sets Flag(s) to be processed in main loop
                  MTI_EVENT_LEARN             : begin Learn(AMessage, DestNode); end;                           // Sets Flag(s) to be processed in main loop
                  MTI_EVENTS_IDENTIFY         : begin IdentifyEvents(AMessage, nil); Exit; end;                 // Sets Flag(s) to be processed in main loop
              end; {case}
            end;
            CheckAndDeallocateMessage(AMessage);
          end;
      {$IFDEF SUPPORT_STREAMS}
      MT_STREAM,
      {$ENDIF}
      MT_DATAGRAM :
          begin
            if DestNode <> nil then
              OPStackNode_IncomingMessageLink(DestNode, AMessage)
            else
              CheckAndDeallocateMessage(AMessage);
          end;
    end

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
//     Description: YOU MUST CHECK IsOutgoingBufferAvailable BEFORE CALLING THIS FUNCTION
// *****************************************************************************
procedure ProcessMarkedForDelete(Node: PNMRAnetNode);
var
  DoDeallocate: Boolean;
  i, j: Integer;
  OPStackMessage: POPStackMessage;
begin
  if IsOutgoingBufferAvailable then
  begin
    if OPStackNode_TestState(Node, NS_RELEASING) then
    begin
      OPStackMessage := nil;
      DoDeallocate := False;

      if OPStackNode_TestState(Node, NS_PERMITTED) then
      begin
         if not OPStackNode_IsAnyEventSet(Node^.Events.Consumed) then
           if not OPStackNode_IsAnyEventSet(Node^.Events.Produced) then
             if not OPStackNode_IsAnyPCER_Set(Node) then
               if Node^.Flags = 0 then
                 if Node^.IncomingMessages = nil then
                   if Node^.StateMachineMessages = nil then
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
end;

// *****************************************************************************
//  procedure UnLinkAndDeAllocateMessage
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
function UnLinkAndDeAllocateMessage(Node: PNMRAnetNode; MessageToSend, AMessage: POPStackMessage): Boolean;
begin
  OPStackNode_IncomingMessageUnLink(Node, AMessage);
  OPStackBuffers_DeAllocateMessage(AMessage);
  if MessageToSend <> nil then
    Result := True
   else
    Result := False;
end;

// *****************************************************************************
//  procedure NodeRunMessageReply
//     Parameters: Node: Node that has a message to send
//                 MessageToSend: The buffer to load if the function is successful in loading it
//                                If loaded and needs sending return True to this function
//     Returns:    True if a message was loaded and ready to send, this function does NOT actually send the message
//     Description: Picks up Buffers pending in the node and loads a message to send
// *****************************************************************************
function NodeRunMessageBufferReply(Node: PNMRAnetNode; var MessageToSend: POPStackMessage): Boolean;
var
  NextMessage: POPStackMessage;
begin
  Result := False;
  MessageToSend := nil;
  NextMessage := OPStackNode_NextIncomingMessage(Node);
  if NextMessage <> nil then
  begin
    if NextMessage^.MessageType and MT_SEND = 0 then
    begin
      case NextMessage^.MTI of
        MTI_SIMPLE_NODE_INFO_REQUEST :
            begin
              SimpleNodeInfoRequestReply(Node, MessageToSend, NextMessage^.Dest, NextMessage^.Source);
              Result := UnLinkAndDeAllocateMessage(Node, MessageToSend, NextMessage);
            end;
        MTI_PROTOCOL_SUPPORT_INQUIRY :
            begin
              ProtocolSupportInquiryReply(Node, MessageToSend, NextMessage);
              Result := UnLinkAndDeAllocateMessage(Node, MessageToSend, NextMessage);
            end;
        {$IFDEF SUPPORT_TRACTION}
        MTI_TRACTION_PROTOCOL :
            begin
              TractionProtocolReply(Node, MessageToSend, NextMessage);
              Result := UnLinkAndDeAllocateMessage(Node, MessageToSend, NextMessage);
            end;
        {$ENDIF}
        {$IFDEF SUPPORT_TRACTION_PROTOCOL}
        MTI_TRACTION_PROXY_PROTOCOL :
            begin
              TractionProxyProtocolReply(Node, MessageToSend, NextMessage);
              Result := UnLinkAndDeAllocateMessage(Node, MessageToSend, NextMessage);
            end;
        {$ENDIF}
        {$IFDEF SUPPORT_STREAMS}
        MTI_STREAM_INIT_REQUEST :
            begin
              StreamInitRequestReply(Node, MessageToSend, NextMessage);
              Result := UnLinkAndDeAllocateMessage(Node, MessageToSend, NextMessage);
            end;
        MTI_STREAM_INIT_REPLY :
            begin
              StreamInitReplyReply(Node, MessageToSend, NextMessage);
              Result := UnLinkAndDeAllocateMessage(Node, MessageToSend, NextMessage);
            end;
        MTI_STEAM_SEND :
            begin
              StreamSendReply(Node, MessageToSend, NextMessage);
              Result := UnLinkAndDeAllocateMessage(Node, MessageToSend, NextMessage);
            end;
        MTI_STREAM_PROCEED :
            begin
              StreamProceedReply(Node, MessageToSend, NextMessage);
              Result := UnLinkAndDeAllocateMessage(Node, MessageToSend, NextMessage);
            end;
        MTI_STREAM_COMPLETE :
            begin
              StreamCompleteReply(Node, MessageToSend, NextMessage);
              Result := UnLinkAndDeAllocateMessage(Node, MessageToSend, NextMessage);
            end;
        {$ENDIF}
        MTI_DATAGRAM :
            begin
              if DatagramSendAckReply(Node, MessageToSend, NextMessage^.Dest, NextMessage^.Source, PDatagramBuffer( PByte( NextMessage^.Buffer))) then
                DatagramReply(Node, MessageToSend, NextMessage);
              Result :=  MessageToSend <> nil
            end
      else begin
          OPStackNode_IncomingMessageUnLink(Node, NextMessage);                           // We don't handle these messages
          OPStackBuffers_DeAllocateMessage(NextMessage);
        end;
      end;
    end else
    begin
      OPStackNode_IncomingMessageUnLink(Node, NextMessage);
      MessageToSend := NextMessage;
      Result := True
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
  if OPStack.State and OPS_PROCESSING <> 0 then
  begin
    for i := 0 to NodePool.AllocatedCount - 1 do
      Inc(NodePool.AllocatedList[i]^.Login.TimeCounter);
    OPStackBuffers_Timer;
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
  i: Integer;
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
        if OPStackBuffers_AllocateOPStackMessage(OPStackMessage, MTI_EVENTS_IDENTIFY_DEST, 0, NULL_NODE_ID, Node^.Info.AliasID, Node^.Info.ID) then  // Fake Source Node
        begin
          {$IFDEF SUPPORT_VIRTUAL_NODES}
          if Node^.State and NS_VIRTUAL <> 0 then
          begin
            for i := 0 to USER_MAX_VNODE_SUPPORTED_EVENTS_CONSUMED - 1 do
              AppCallback_InitializeEvents(Node, i, EVENT_TYPE_CONSUMED);
            for i := 0 to USER_MAX_VNODE_SUPPORTED_EVENTS_PRODUCED - 1 do
              AppCallback_InitializeEvents(Node, i, EVENT_TYPE_PRODUCED);

            for i := 0 to USER_MAX_VNODE_SUPPORTED_DYNAMIC_EVENTS_CONSUMED - 1 do
              AppCallback_InitializeDynamicEvents(Node, i, EVENT_TYPE_CONSUMED);
            for i := 0 to USER_MAX_VNODE_SUPPORTED_DYNAMIC_EVENTS_PRODUCED - 1 do
              AppCallback_InitializeDynamicEvents(Node, i, EVENT_TYPE_PRODUCED);

          end else
          {$ENDIF}
          begin
            for i := 0 to USER_MAX_SUPPORTED_EVENTS_CONSUMED - 1 do
              AppCallback_InitializeEvents(Node, i, EVENT_TYPE_CONSUMED);
            for i := 0 to USER_MAX_VNODE_SUPPORTED_EVENTS_PRODUCED - 1 do
              AppCallback_InitializeEvents(Node, i, EVENT_TYPE_PRODUCED);

            for i := 0 to USER_MAX_SUPPORTED_DYNAMIC_EVENTS_CONSUMED - 1 do
              AppCallback_InitializeDynamicEvents(Node, i, EVENT_TYPE_CONSUMED);
            for i := 0 to USER_MAX_SUPPORTED_DYNAMIC_EVENTS_PRODUCED - 1 do
              AppCallback_InitializeDynamicEvents(Node, i, EVENT_TYPE_PRODUCED);
          end;

          Node^.iStateMachine := STATE_NODE_PERMITTED;
          IncomingMessageDispatch(OPStackMessage, Node, nil);
          OPStackBuffers_DeAllocateMessage(OPStackMessage);
        end
      end;
    STATE_NODE_PERMITTED :
      begin
        if IsOutgoingBufferAvailable then
        begin
          if NodeRunPCERFlagsReply(Node, OPStackMessage) then
            OutgoingMessage(OPStackMessage)
          else
          if NodeRunFlagsReply(Node, OPStackMessage) then
            OutgoingMessage(OPStackMessage)
          else
          if NodeRunEventFlagsReply(Node, OPStackMessage) then
            OutgoingMessage(OPStackMessage)
          else
          if NodeRunMessageBufferReply(Node, OPStackMessage) then
            OutgoingMessage(OPStackMessage);
          ProcessMarkedForDelete(Node);                                           // Handle vNodes marked to be deleted
          ProcessAbandonMessages(Node);
        end;
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
function OPStackCore_Process: PNMRAnetnode;
begin
  if OPStack.State and OPS_PROCESSING <> 0 then
  begin
    Hardware_DisableInterrupts;
    Result := OPStackNode_NextNode;
    if Result <> nil then
      NodeRunStateMachine(Result);
    ProcessHardwareMessages;
    Hardware_EnableInterrupts;
  end;
end;

procedure OPStackCore_Enable(DoEnable: Boolean);
begin
  if DoEnable then
    OPStack.State := OPStack.State or OPS_PROCESSING
  else
    OPStack.State := OPStack.State and not OPS_PROCESSING;
end;

function OPStackCore_IsRunning: Boolean;
begin
  Result := OPStack.State and OPS_PROCESSING <> 0
end;

end.
