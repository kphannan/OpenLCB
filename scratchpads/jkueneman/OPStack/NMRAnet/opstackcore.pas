unit opstackcore;

interface

{$I Options.inc}

uses
  // Compile in different hardware layer interfaces here
  {$IFDEF HARDWARE_TEMPLATE}hardware_template,{$ENDIF}
  {$IFDEF HARDWARE_DSPIC_CAN}hardware_dspic_CAN,{$ENDIF}
  {$IFDEF HARDWARE_ENC28J60}hardware_ENC28j60,{$ENDIF}
  nmranetutilities,
  nmranetdefines,
  opstackdefines,
  opstackbuffers,
  opstacknode;



procedure OPStackCore_Initialize;                                               // Call once on program startup
procedure OPStackCore_Process;                                                  // Call as often as possible
procedure OPStackCore_Timer;                                                    // Call every 100ms

// Callback from the Hardware when a message is received
procedure IncomingMessageCallback(Message: PMessage);


implementation

// *****************************************************************************
//  procedure OPStackCore_Initialize
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure OPStackCore_Initialize;
begin
  OPStackNode_Initialize;
  OPStackNode_Allocate;                                                         // Allocate the hardware Node
end;

// *****************************************************************************
//  procedure ProcessMarkedForDeleteNodes
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure ProcessMarkedForDeleteNodes(Node: PNMRAnetNode; var CANMessage: TNMRAnetCANLayerMessage);
var
  DoDeallocate: Boolean;
  i, j: Integer;
begin
  if OPStackNode_TestState(Node, NS_RELEASING) then
  begin
    DoDeallocate := False;
    if IsOutgoingBufferAvailable then                                         // Make sure the Node is competely finished sending updates/datagrams/streams/etc
      if OPStackNode_TestState(Node, NS_PERMITTED) then
      begin
         if not OPStackNode_IsAnyEventSet(Node^.Events.Consumed) then
           if not OPStackNode_IsAnyEventSet(Node^.Events.Produced) then
             if not OPStackNode_IsAnyPCER_Set(Node) then
               if Node^.Flags = 0 then
                 if Node^.MsgFlagsUserDefined = 0 then
                 begin
                   NMRAnetUtilities_LoadBaseMessageBuffer(@CANMessage, MT_CAN, MTI_AMR, nil, Node^.Info.ID, NULL_NODE_ID); // Tell the network we are leaving
                   OutgoingMessage(@CANMessage);
                   DoDeallocate := True;
                 end

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
//  procedure NodeRunStateMachine
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure NodeRunStateMachine(Node: PNMRAnetNode);
var
  CANMessage: TNMRAnetCANLayerMessage;
  BasicMessage: TNMRAnetBasicMessage;
  EventMessage: TNMRAnetEventMessage;
begin
  case Node^.iStateMachine of
    STATE_NODE_START :
      begin
        Node^.Login.iCID := 0;
        Node^.iStateMachine := STATE_NODE_TRANSMIT_CID;
      end;
    STATE_NODE_GENERATE_NODE_ALIAS :
      begin
        Node^.Info.AliasID := NMRAnetUtilities_CreateAliasID(Node^.Info.Seed, False);
        Node^.Login.iCID := 0;
        Node^.iStateMachine := STATE_NODE_TRANSMIT_CID;
      end;
    STATE_RANDOM_NUMBER_GENERATOR :
      begin
        NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed(Node^.Info.Seed);
        Node^.iStateMachine := STATE_NODE_GENERATE_NODE_ALIAS;
      end;
    STATE_NODE_TRANSMIT_CID :
      begin
        if IsOutgoingBufferAvailable then
        begin
          case Node^.Login.iCID of
            0 : NMRAnetUtilities_LoadBaseMessageBuffer(@CANMessage, MT_CAN, MTI_CID0 or DWord((Node^.Info.ID[1] shr 12) and $00000FFF), nil, Node^.Info.ID, NULL_NODE_ID);
            1 : NMRAnetUtilities_LoadBaseMessageBuffer(@CANMessage, MT_CAN, MTI_CID1 or DWord(Node^.Info.ID[1] and $00000FFF), nil, Node^.Info.ID, NULL_NODE_ID);
            2 : NMRAnetUtilities_LoadBaseMessageBuffer(@CANMessage, MT_CAN, MTI_CID2 or DWord((Node^.Info.ID[0] shr 12) and $00000FFF), nil, Node^.Info.ID, NULL_NODE_ID);
            3 : NMRAnetUtilities_LoadBaseMessageBuffer(@CANMessage, MT_CAN, DWord(Node^.Info.ID[0] and $00000FFF), nil, Node^.Info.ID, NULL_NODE_ID);
          end;
          OutgoingMessage(@CANMessage);
          Node^.iStateMachine := STATE_NODE_NEXT_CDI;
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
          begin
            NMRAnetUtilities_LoadBaseMessageBuffer(@CANMessage, MT_CAN, MTI_RID, nil, Node^.Info.ID, NULL_NODE_ID);
            OutgoingMessage(@CANMessage);
            Node^.iStateMachine := STATE_NODE_SEND_LOGIN_AMD;
          end;
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
          begin
            NMRAnetUtilities_LoadBaseMessageBuffer(@CANMessage, MT_CAN, MTI_AMD, nil, Node^.Info.ID, NULL_NODE_ID);
            OutgoingMessage(@CANMessage);
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
          begin
            NMRAnetUtilities_LoadBaseMessageBuffer(@BasicMessage, MT_BASIC, MTI_INITIALIZATION_COMPLETE, nil, Node^.Info.ID, NULL_NODE_ID);
            NMRAnetUtilities_LoadCANDataWith48BitNodeID(Node^.Info.ID, BasicMessage.CANData);
            BasicMessage.CANData.Count := 6;
            OutgoingMessage(@BasicMessage);
            OPStackNode_SetState(Node, NS_INITIALIZED);
            Node^.iStateMachine := STATE_NODE_LOGIN_IDENTIFY_EVENTS;
          end
        end
      end;
    STATE_NODE_LOGIN_IDENTIFY_EVENTS :
      begin
        // Fake an Identify Events to allow the AppCallbacks to be called
        NMRAnetUtilities_LoadBaseMessageBuffer(@EventMessage, MT_EVENT, MTI_EVENTS_IDENTIFY, nil, NULL_NODE_ID, NULL_NODE_ID);  // Fake Source Node
        Hardware_DisableInterrupts;                                             // don' get stomped on by and incoming message within an interrupt
        IncomingMessageCallback(@EventMessage);
        Hardware_EnableInterrupts;
        Node^.iStateMachine := STATE_NODE_PERMITTED;
      end;
    STATE_NODE_PERMITTED :
      begin
        Hardware_DisableInterrupts;
    //    ProcessNode(Node, @CANBuffer);                                        // Handle auto Actions to CAN/NMRAnet messages coming in
    //    ProcessOutgoingMessages(Node, @CANBuffer);                            // Handle outgoing messages like Datagrams
        ProcessMarkedForDeleteNodes(Node, CANMessage);                          // Handle vNodes marked to be deleted
    //    ProcessAbandonBuffers(Node);                                          // Handle (free) buffers (datagrams mainly) that appear to be abandon in midstream
        Hardware_EnableInterrupts
      end;
    STATE_NODE_INHIBITED :
      begin
        // Any buffers will time out and self release
        if IsOutgoingBufferAvailable then
        begin
          NMRAnetUtilities_LoadBaseMessageBuffer(@CANMessage, MT_CAN, MTI_AMR, nil, Node^.Info.ID, NULL_NODE_ID);
          OutgoingMessage(@CANMessage);
          OPStackNode_ClearState(Node, NS_PERMITTED);
          OPStackNode_ClearFlags(Node);
   //       ReleaseBuffers(Node);
          Node^.iStateMachine := STATE_NODE_TAKE_OFFLINE;
        end
      end;
    STATE_NODE_DUPLICATE_FULL_ID :
      begin
        // Any buffers will time out and self release
        if IsOutgoingBufferAvailable then
        begin
          NMRAnetUtilities_LoadBaseMessageBuffer(@BasicMessage, MT_CAN, MTI_AMR, nil, Node^.Info.ID, NULL_NODE_ID);
          OutgoingMessage(@CANMessage);
          OPStackNode_ClearState(Node, NS_PERMITTED);
          OPStackNode_ClearFlags(Node);
           //       ReleaseBuffers(Node);
          Node^.iStateMachine := STATE_NODE_TAKE_OFFLINE;
        end;
      end;
    STATE_NODE_TAKE_OFFLINE :
      begin
        if IsOutgoingBufferAvailable then
        begin
          EventMessage.SourceNodeID := Node^.Info.ID;
          EventMessage.DestNodeID := NULL_NODE_ID;
          EventMessage.EventID := EVENT_DUPLICATE_ID_DETECTED;
          Node^.iStateMachine := STATE_NODE_OFFLINE;
        end
      end;
    STATE_NODE_OFFLINE :
      begin
        // Done until reboot
      end
  else
     Node^.iStateMachine := STATE_NODE_INHIBITED                                // We are confused, start over
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

procedure IncomingMessageCallback(Message: PMessage);
begin
  case PBaseMessage( Message)^.MessageType of
      MT_BASIC :
          begin

          end;
      MT_PROTCOLSUPPORT :
          begin

          end;
      MT_EVENT :
          begin

          end;
      MT_TRACTION :
          begin

          end;
      MT_REMOTEBUTTON :
          begin

          end;
      MT_SNIP :
          begin

          end;
      MT_DATATGRAM :
          begin

          end;
      MT_STREAM :
          begin

          end;
      MT_CAN :
          begin

          end;
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
  Node := OPStackNode_NextNode;
  if Node <> nil then
  begin
    NodeRunStateMachine(Node);

  end;
end;

end.

