unit opstackcore;

// TODOs
//   1) Datagram Rejected: Try N times then give up, re-tries forever now
//   2) Check for Abandon Datagrams/Streams then Free
//   3) ACDI Read and Write not implemented correctly
//   4) FDI not implemented correctly
//   5) Do something with MTI_STREAM_INIT_REPLY error code results


{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  // Compile in different hardware layer interfaces here
  {$IFDEF HARDWARE_TEMPLATE}hardware_template, template_configmem,{$ENDIF}
  {$IFDEF HARDWARE_DSPIC_CAN}hardware_dspic_CAN,{$ENDIF}
  {$IFDEF HARDWARE_ENC28J60}hardware_ENC28j60,{$ENDIF}
  nmranetutilities,
  nmranetdefines,
  opstackdefines,
  template_buffers,
  opstackbuffers,
  opstacktypes,
  opstacknode,
  opstackcore_events,
  opstackcore_can,
  opstackcore_pip,
  opstackcore_basic,
  {$IFDEF SUPPORT_STREAMS}
  opstackcore_stream,
  {$ENDIF}
  opstackcore_snip,
  opstackcore_learn,
  opstackcore_datagram;

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
  OPStackCoreDatagram_Initialize;
  OPStackNode_Allocate;                                                         // Allocate the hardware Node
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
  i: Integer;
  NewMessage: POPStackMessage;
  BufferAllocFailed: Boolean;
  SourceNode: PNMRAnetNode;
  OptionalInteractionMessage: TOPStackMessage;
begin
  BufferAllocFailed := False;
  if DestNode <> nil then
  begin
    if DestNode^.State and NS_RELEASING <> 0 then               // if Releasing don't add more messages
    begin
      if DestNode^.State and NS_ALLOCATED <> 0 then             // If is an allocated message then deallocate it
        OPStackBuffers_DeAllocateMessage(AMessage);;
      Exit;
    end
  end;

  // First thing is extract the Source Alias and make sure it is not a duplicate of one of our Node or vNode Aliases
  SourceNode := OPStackNode_Find(AMessage, FIND_BY_SOURCE);
  if SourceNode <> nil then
  begin
    if AMessage^.MessageType and MT_CAN_TYPE <> 0 then
      DuplicateSourceIdDetected(AMessage, SourceNode)
    else
      OPStackNode_SetFlag(SourceNode, MF_DUPLICATE_ALIAS);                      // Another node is using our Alias, we have to disconnect from the network
    Exit;
  end else
  begin
    if AMessage^.MessageType and MT_CAN_TYPE <> 0 then                          // Is it a CAN message?
    begin
      case AMessage^.MTI of
          MTI_CAN_AME : begin  AliasMappingEnquiry(AMessage, DestNode); Exit; end;
          MTI_CAN_AMD : begin AliasMappingDefinition(AMessage, DestNode); Exit; end;
          MTI_CAN_AMR : begin AliasMappingReset(AMessage, DestNode); Exit; end;
        end {case}
    end else
    begin
      case (AMessage^.MessageType) and MT_MASK of
          MT_SIMPLE :
              begin
                if AMessage^.MTI and MTI_ADDRESSED_MASK = MTI_ADDRESSED_MASK then
                begin
                  if DestNode <> nil then                                       // Destination messages come through so we can check for duplicate Aliases, if it is nil then done
                  begin
                    case AMessage^.MTI of
                      MTI_SIMPLE_NODE_INFO_REQUEST      : begin SimpleNodeInfoRequest(AMessage, DestNode); Exit; end;
                      MTI_VERIFY_NODE_ID_NUMBER_DEST    : begin VerifyNodeIdByDestination(AMessage, DestNode); Exit; end;
                      MTI_EVENTS_IDENTIFY_DEST          : begin IdentifyEvents(AMessage, DestNode); Exit; end;
                      MTI_PROTOCOL_SUPPORT_INQUIRY      : begin ProtocolSupportInquiry(AMessage, DestNode); Exit; end;
                      MTI_OPTIONAL_INTERACTION_REJECTED : begin end;
                      MTI_DATAGRAM_OK_REPLY             : begin DatagramOkReply(AMessage, DestNode); Exit; end;
                      MTI_DATAGRAM_REJECTED_REPLY       : begin DatagramRejectedReply(AMessage, DestNode); Exit; end;
                      {$IFDEF SUPPORT_STREAMS}
                      MTI_STREAM_INIT_REQUEST           : begin StreamInitRequest(AMessage, DestNode); Exit; end;
                      MTI_STREAM_INIT_REPLY             : begin StreamInitReply(AMessage, DestNode); Exit; end;
                      MTI_STREAM_PROCEED                : begin StreamProceed(AMessage, DestNode); Exit; end;
                      {$ENDIF}
                    else begin
                        OPStackBuffers_LoadOptionalInteractionRejected(@OptionalInteractionMessage, AMessage^.Dest.AliasID, AMessage^.Dest.ID, AMessage^.Source.AliasID, AMessage^.Source.ID, AMessage^.MTI);    // Unknown MTI sent to addressed node
                        OutgoingCriticalMessage(@OptionalInteractionMessage);
                      end;
                    end; {case}
                  end else
                    Exit;
                end else
                begin
                  case AMessage^.MTI of
                      MTI_VERIFY_NODE_ID_NUMBER   : begin VerifyNodeId(AMessage, DestNode); Exit; end;
                      MTI_CONSUMER_IDENTIFY       : begin IdentifyConsumers(AMessage, DestNode); Exit; end;
                      MTI_CONSUMER_IDENTIFY_RANGE : begin IdentifyRangeConsumers(AMessage, DestNode); Exit; end;
                      MTI_PRODUCER_IDENDIFY       : begin IdentifyProducers(AMessage, DestNode); Exit; end;
                      MTI_PRODUCER_IDENTIFY_RANGE : begin IdentifyRangeProducers(AMessage, DestNode); end;
                      MTI_EVENT_LEARN             : begin Learn(AMessage, DestNode); end;
                      MTI_EVENTS_IDENTIFY         : begin IdentifyEvents(AMessage, nil); Exit; end;
                      // Handling unknown MTI for all nodes (virtual and physical) is difficult and not sure it is needed based on python scripts
                  end; {case}
                end;
              end;
          {$IFDEF SUPPORT_STREAMS}
          MT_DATAGRAM,
          MT_STREAM :
          {$ELSE}
          MT_DATAGRAM :
          {$ENDIF}
              begin
                if DestNode <> nil then
                  OPStackNode_MessageLink(DestNode, AMessage)
                else
                  OPStackBuffers_DeAllocateMessage(AMessage);
              end;
        end
      end;
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
//     Description: YOU MUST CHECK IsOutgoingBufferAvailable BEFORE CALLING THIS FUNCTION
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

// *****************************************************************************
//  procedure NodeRunMessageReply
//     Parameters: Node: Node that has a message to send
//                 MessageToSend: The buffer to load if the function is successfull in loading it
//                                If loaded and needs sending return True to this funcions
//     Returns:    True if a message was loaded and ready to send, this function does NOT actually send the message
//     Description: Picks up Buffers pending in the node and loads a message to send
// *****************************************************************************
function NodeRunMessageBufferReply(Node: PNMRAnetNode; var MessageToSend: POPStackMessage): Boolean;
var
  NextMessage: POPStackMessage;
begin
  Result := False;
  MessageToSend := nil;
  NextMessage := OPStackNode_NextMessage(Node);
  if NextMessage <> nil then
  begin
    if NextMessage^.MessageType and MT_RESEND = 0 then
    begin
      case NextMessage^.MTI of
        MTI_SIMPLE_NODE_INFO_REQUEST :
            begin                                       //        REUSE THIS MESSAGE IN THE CALL
              SimpleNodeInfoReply(Node, MessageToSend, NextMessage^.Dest, NextMessage^.Source);
              if MessageToSend <> nil then
              begin
                OPStackNode_MessageUnLink(Node, NextMessage);
                OPStackBuffers_DeAllocateMessage(NextMessage);
                Result := True;
              end
            end;
        MTI_PROTOCOL_SUPPORT_INQUIRY :
            begin                                              //   REUSE THIS MESSAGE IN THE CALL
              ProtocolSupportReply(Node, MessageToSend, NextMessage^.Dest, NextMessage^.Source);
              if MessageToSend <> nil then
              begin
                OPStackNode_MessageUnLink(Node, NextMessage);
                OPStackBuffers_DeAllocateMessage(NextMessage);
                Result := True;
              end
            end;
        MTI_DATAGRAM :
            begin
              if DatagramSendAckReply(Node, MessageToSend, NextMessage^.Dest, NextMessage^.Source, PDatagramBuffer( PByte( NextMessage^.Buffer))) then
                DatagramReply(Node, MessageToSend, NextMessage);
              Result :=  MessageToSend <> nil
            end
      else begin
          OPStackNode_MessageUnLink(Node, NextMessage);                           // We don't handle these messages
          OPStackBuffers_DeAllocateMessage(NextMessage);
        end;
      end;
    end else
    begin
      OPStackNode_MessageUnLink(Node, NextMessage);
      MessageToSend := NextMessage;
      Result := True
    end
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
        if IsOutgoingBufferAvailable then                                         // Make sure the Node is competely finished sending updates/datagrams/streams/etc
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

      {        if not (Node) then
                {$IFDEF SUPPORT_STREAMS}
                if not NodeRunOutgoingStreamStateMachine(Node) then
                  if not NodeRunIncomingStreamStateMachine(Node) then
                {$ENDIF}

            }
          ProcessMarkedForDelete(Node);                                           // Handle vNodes marked to be deleted
          ProcessAbandonMessages(Node);
        end;
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
