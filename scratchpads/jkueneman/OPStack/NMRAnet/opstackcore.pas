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
  opstackcore_button,
  {$IFDEF SUPPORT_STREAMS}opstackcore_stream,{$ENDIF}
  {$IFDEF SUPPORT_TRACTION}opstackcore_traction, opstackcore_stnip,{$ENDIF}
  {$IFDEF SUPPORT_TRACTION_PROXY}opstackcore_traction_proxy,{$ENDIF}
  opstackcore_snip,
  opstackcore_datagram,
  template_userstatemachine;

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
  UserStateMachine_Initialize;
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
//     Parameters:  AMessage: Pointer to a OPStackMessage object that has been allocated
//                            from the pool
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
var
  NodeID: TNodeID;
begin
  // If the Node is being Released then don't do anything except deallocate the message if needed
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
    MT_SIMPLE,                                        //  HOW CAN I PUT DATAGRAMS AND STREAMS INTO THIS SAME FORMAT, THEY ARE ODD BALLS NOW
    MT_MULTIFRAME,
    MT_ACDISNIP :
        begin
          if AMessage^.MTI and MTI_ADDRESSED_MASK = MTI_ADDRESSED_MASK then   // Handle Simple Messages that may be addressed to one of our nodes
          begin
            if DestNode <> nil then                                           // If it is addressed and the DestNode = nil then it is not for us
            begin                                                             // We send all messages in to test for Releasing so this test is necessary
              case AMessage^.MTI of
                MTI_SIMPLE_NODE_INFO_REQUEST,
                MTI_SIMPLE_NODE_INFO_REPLY         : begin SimpleNodeInfoMessage(DestNode, AMessage); Exit; end;   // Exit and Don't free the Message
                MTI_VERIFY_NODE_ID_NUMBER_DEST     : begin VerifyNodeIdByDestination(DestNode, AMessage); end;     // Sets Flag(s) to be processed in main loop
                MTI_EVENTS_IDENTIFY_DEST           : begin IdentifyEvents(DestNode, AMessage); end;                // Sets Flag(s) to be processed in main loop
                MTI_PROTOCOL_SUPPORT_INQUIRY,
                MTI_PROTOCOL_SUPPORT_REPLY         : begin ProtocolSupportMessage(DestNode, AMessage); Exit; end;   // Don't free the Message
                MTI_DATAGRAM_OK_REPLY              : begin DatagramOkReply(DestNode, AMessage); end;               // Updates internal states
                MTI_DATAGRAM_REJECTED_REPLY        : begin DatagramRejectedReply(DestNode, AMessage); end;         // Updates internal states
                {$IFDEF SUPPORT_TRACTION}
                MTI_SIMPLE_TRAIN_NODE_INFO_REQUEST,
                MTI_SIMPLE_TRAIN_NODE_INFO_REPLY   : begin SimpleTrainNodeInfoMessage(AMessage, DestNode);  Exit; end;   // Don't free the Message
                MTI_TRACTION_PROTOCOL,
                MTI_TRACTION_REPLY                 : begin TractionProtocolMessage(AMessage, DestNode); Exit; end;   // Don't free the Message
                {$ENDIF}
                {$IFDEF SUPPORT_TRACTION_PROXY}
                MTI_TRACTION_PROXY_PROTOCOL,
                MTI_TRACTION_PROXY_REPLY           : begin TractionProxyProtocolMessage(DestNode ,AMessage); Exit; end;   // Don't free the Message
                {$ENDIF}
                MTI_REMOTE_BUTTON_REQUEST,
                MTI_REMOTE_BUTTON_REPLY            : begin RemoteButtonMessage(DestNode, AMessage); Exit; end;   // Don't free the Message
                {$IFDEF SUPPORT_STREAMS}
                MTI_STREAM_INIT_REQUEST            : begin StreamInitRequest(DestNode, AMessage); end;            // Allocates Buffer to be processed in main loop
                MTI_STREAM_INIT_REPLY              : begin StreamInitReply(DestNode, AMessage); end;              // Allocates Buffer to be processed in main loop
                MTI_STREAM_PROCEED                 : begin StreamProceed(DestNode, AMessage); end;                // Allocates Buffer to be processed in main loop
                MTI_STREAM_COMPLETE                : begin StreamComplete(DestNode, AMessage); end;               // Allocates Buffer to be processed in main loop
                {$ENDIF}
                MTI_OPTIONAL_INTERACTION_REJECTED  : begin {TODO: What to do if one of the messages is Rejected!} end
              else
                  OptionalInteractionRejected(AMessage, True);        // Unknown message, permenent error
              end; {case}
            end                                                      // It is not for of or our nodes
          end else
          begin                                                               // Is not an Addressed message so handle it, the handler must decide what to do with DestNode = nil
            case AMessage^.MTI of
                MTI_VERIFY_NODE_ID_NUMBER       : begin VerifyNodeId(DestNode, AMessage); end;              // Sets Flag(s) to be processed in main loop
                MTI_CONSUMER_IDENTIFY           : begin IdentifyConsumers(AMessage); end;         // Sets Flag(s) to be processed in main loop
                MTI_PRODUCER_IDENTIFY           : begin IdentifyProducers(AMessage); end;         // Sets Flag(s) to be processed in main loop
                MTI_EVENT_LEARN                 : begin LearnEvent(AMessage); end;                           // Sets Flag(s) to be processed in main loop
                MTI_EVENTS_IDENTIFY             : begin IdentifyEvents(nil, AMessage); end;                 // Sets Flag(s) to be processed in main loop

                MTI_CONSUMER_IDENTIFIED_RANGE,
                MTI_CONSUMER_IDENTIFIED_CLEAR,
                MTI_CONSUMER_IDENTIFIED_RESERVED,
                MTI_CONSUMER_IDENTIFIED_SET,
                MTI_CONSUMER_IDENTIFIED_UNKNOWN : begin AppCallback_ConsumerIdentified(AMessage^.Source, AMessage^.MTI, PEventID( PByte(@AMessage^.Buffer^.DataArray[0]))); end;
                MTI_PRODUCER_IDENTIFIED_RANGE,
                MTI_PRODUCER_IDENTIFIED_CLEAR,
                MTI_PRODUCER_IDENTIFIED_RESERVED,
                MTI_PRODUCER_IDENTIFIED_SET,
                MTI_PRODUCER_IDENTIFIED_UNKNOWN : begin AppCallback_ProducerIdentified( AMessage^.Source, AMessage^.MTI, PEventID( PByte(@AMessage^.Buffer^.DataArray[0]))); end;
                MTI_VERIFIED_NODE_ID_NUMBER     : begin AppCallback_VerifiedNodeID(AMessage^.Source, NMRAnetUtilities_Load48BitNodeIDWithSimpleData(NodeID, AMessage^.Buffer^.DataArray)); end;
                MTI_INITIALIZATION_COMPLETE     : begin AppCallback_InitializationComplete(AMessage^.Source,  NMRAnetUtilities_Load48BitNodeIDWithSimpleData(NodeID, AMessage^.Buffer^.DataArray)); end;
                MTI_PC_EVENT_REPORT             : begin AppCallBack_PCEventReport(AMessage^.Source, PEventID( PByte(@AMessage^.Buffer^.DataArray[0]))); end
             else {case}
               OptionalInteractionRejected(AMessage, True);        // Unknown message, permenent error
             end
          end
        end;
    {$IFDEF SUPPORT_STREAMS}
    MT_STREAM,
    {$ENDIF}
    MT_DATAGRAM :
        begin
          if DestNode <> nil then
          begin
            OPStackNode_IncomingMessageLink(DestNode, AMessage);
            Exit;   // Jump out, Don't Deallocate
          end else
            CheckAndDeallocateMessage(AMessage);
        end;
    end;
  CheckAndDeallocateMessage(AMessage);
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
                       NMRAnetUtilities_LoadSimpleDataWith48BitNodeID(@Node^.Info.ID, PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray));
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
//  procedure NodeRunMessageBufferReply
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
        MTI_SIMPLE_NODE_INFO_REQUEST         : begin Result := SimpleNodeInfoRequestReplyHandler(Node, MessageToSend, NextMessage); Exit; end;
        MTI_SIMPLE_NODE_INFO_REPLY           : begin SimpleNodeInfoRequestReply(Node, NextMessage); Exit; end;
        MTI_PROTOCOL_SUPPORT_INQUIRY         : begin Result := ProtocolSupportInquiryReplyHandler(Node, MessageToSend, NextMessage); Exit; end;
        MTI_PROTOCOL_SUPPORT_REPLY           : begin ProtocolSupportReply(Node, NextMessage); Exit; end;
        {$IFDEF SUPPORT_TRACTION}
        MTI_TRACTION_PROTOCOL                : begin Result := TractionProtocolReplyHandler(Node, MessageToSend, NextMessage); Exit; end;
        MTI_TRACTION_REPLY                   : begin TractionProtocolReply(Node, NextMessage); Exit; end;
        MTI_SIMPLE_TRAIN_NODE_INFO_REQUEST   : begin Result := SimpleTrainNodeInfoRequestReplyHandler(Node, MessageToSend, NextMessage); Exit; end;
        MTI_SIMPLE_TRAIN_NODE_INFO_REPLY     : begin SimpleTrainNodeInfoReply(Node, NextMessage); Exit; end;
        {$ENDIF}
        {$IFDEF SUPPORT_TRACTION_PROXY}
        MTI_TRACTION_PROXY_PROTOCOL          : begin Result := TractionProxyProtocolReplyHandler(Node, MessageToSend, NextMessage); Exit; end;
        MTI_TRACTION_PROXY_REPLY             : begin TractionProxyProtocolReply(Node, NextMessage); Exit; end;
        {$ENDIF}
        MTI_REMOTE_BUTTON_REQUEST            : begin Result := RemoteButtonReplyHandler(Node, MessageToSend, NextMessage); Exit; end;
        MTI_REMOTE_BUTTON_REPLY              : begin RemoteButtonReply(Node, NextMessage); Exit; end;
        {$IFDEF SUPPORT_STREAMS}
        MTI_STREAM_INIT_REQUEST :
            begin
              StreamInitRequestReply(Node, MessageToSend, NextMessage);
              Result := UnLinkDeAllocateAndTestForMessageToSend(Node, MessageToSend, NextMessage);
            end;
        MTI_STREAM_INIT_REPLY :
            begin
              StreamInitReplyReply(Node, MessageToSend, NextMessage);
              Result := UnLinkDeAllocateAndTestForMessageToSend(Node, MessageToSend, NextMessage);
            end;
        MTI_STEAM_SEND :
            begin
              StreamSendReply(Node, MessageToSend, NextMessage);
              Result := UnLinkDeAllocateAndTestForMessageToSend(Node, MessageToSend, NextMessage);
            end;
        MTI_STREAM_PROCEED :
            begin
              StreamProceedReply(Node, MessageToSend, NextMessage);
              Result := UnLinkDeAllocateAndTestForMessageToSend(Node, MessageToSend, NextMessage);
            end;
        MTI_STREAM_COMPLETE :
            begin
              StreamCompleteReply(Node, MessageToSend, NextMessage);
              Result := UnLinkDeAllocateAndTestForMessageToSend(Node, MessageToSend, NextMessage);
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
  Node: PNMRAnetNode;
begin
  if OPStack.State and OPS_PROCESSING <> 0 then
  begin
    for i := 0 to NodePool.AllocatedCount - 1 do
    begin
      Node := NodePool.AllocatedList[i];
      Inc(Node^.Login.TimeCounter);
      {$IFDEF SUPPORT_TRACTION}
      TractionProtocolTimerTick(Node);
      {$ENDIF}
    end;
    OPStackBuffers_Timer;
  end;
  AppCallback_Timer_100ms;
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
              NMRAnetUtilities_LoadSimpleDataWith48BitNodeID(@Node^.Info.ID, PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray));
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
              NMRAnetUtilities_LoadSimpleDataWith48BitNodeID(@Node^.Info.ID, PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray));
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
            NMRAnetUtilities_LoadSimpleDataWith48BitNodeID(@Node^.Info.ID, PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray));
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
            NMRAnetUtilities_LoadSimpleDataWith48BitNodeID(@Node^.Info.ID, PSimpleDataArray(@OPStackMessage^.Buffer^.DataArray));
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
    begin
      NodeRunStateMachine(Result);
      AppCallback_UserStateMachine_Process(Result);    // Do I want to let this run with nil?  Why should there be a nil?  There shouldn't
    end;
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
