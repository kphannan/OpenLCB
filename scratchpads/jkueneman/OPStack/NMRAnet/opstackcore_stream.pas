unit opstackcore_stream;

// TODO : StreamProceed is broken, need to understand overall architecture to finish
// TODO : Does not handle non-CAN IDs yet
// TODO : Does not handle 6 byte Stream UIDs yet

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  opstacknode,
  opstackbuffers,
  nmranetdefines,
  opstackdefines,
  template_buffers,
  nmranetutilities,
  opstackcore_basic,
  opstacktypes;

procedure OPStackCoreStream_Initialize;

// Calls from received messages
procedure StreamInitRequest(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
procedure StreamInitReply(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
procedure StreamProceed(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
procedure StreamComplete(AMessage: POPStackMessage; DestNode: PNMRAnetNode);

// Calls from message replies
procedure StreamInitRequestReply(Node: PNMRAnetNode; var MessageToSend: POPStackMessage; StreamMessage: POPStackMessage);
procedure StreamInitReplyReply(Node: PNMRAnetNode; var MessageToSend: POPStackMessage; StreamMessage: POPStackMessage);
procedure StreamSendReply(Node: PNMRAnetNode; var MessageToSend: POPStackMessage; StreamMessage: POPStackMessage);
procedure StreamProceedReply(Node: PNMRAnetNode; var MessageToSend: POPStackMessage; StreamMessage: POPStackMessage);
procedure StreamCompleteReply(Node: PNMRAnetNode; var MessageToSend: POPStackMessage; StreamMessage: POPStackMessage);

implementation

var
  ActiveStreamList: POPStackMessage;
  StreamIDPool: Byte;

// *****************************************************************************
//  procedure OPStackCoreStream_Initialize;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCoreStream_Initialize;
begin
  ActiveStreamList := nil;
  StreamIDPool := 1;
end;

function GenerateStreamID: Byte;
begin
  Result := StreamIDPool;
  Inc(StreamIDPool);
end;

// *****************************************************************************
//  procedure AddWaitingForAckResponseMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure AddActiveStreamMessage(OPStackMessage: POPStackMessage);
var
  LocalMessage: POPStackMessage;
begin
  if ActiveStreamList = nil then
    ActiveStreamList := OPStackMessage
  else begin
    LocalMessage := ActiveStreamList;
    while PStreamBuffer( PByte( LocalMessage^.Buffer))^.NextActiveStream <> nil do     // Walk the stack to the end and add our new message
      LocalMessage := POPStackMessage( PStreamBuffer( PByte( LocalMessage^.Buffer))^.NextActiveStream);  // WARNING MUST REMOVE THE MESSAGE FROM THE STACK IF THE DATAGRAM IS ABANDON!!!!!!!
    PStreamBuffer(PByte( LocalMessage^.Buffer))^.NextActiveStream := PByte( OPStackMessage);
  end;
end;

// *****************************************************************************
//  procedure FindWaitingForAckResponseMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function FindActiveStreamMessage(var SourceID: TNodeInfo; var DestID: TNodeInfo): POPStackMessage;
var
  LocalMessage: POPStackMessage;
begin
  LocalMessage := ActiveStreamList;
  while LocalMessage <> nil do
  begin
    if NMRAnetUtilities_EqualNodeIDInfo(SourceID, LocalMessage^.Source) then
      if NMRAnetUtilities_EqualNodeIDInfo(DestID, LocalMessage^.Dest) then
      begin
        Result := LocalMessage;
        Exit;
      end;
    LocalMessage := POPStackMessage( PStreamBuffer( PByte( LocalMessage^.Buffer))^.NextActiveStream);
  end;
end;

// *****************************************************************************
//  procedure RemoveWaitingForAckResponseMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure RemoveActiveStreamMessage(OPStackMessage: POPStackMessage);
var
  LocalMessage, LocalMessageParent: POPStackMessage;
begin
  if ActiveStreamList <> nil then
  begin
    if ActiveStreamList = OPStackMessage then
    begin
      ActiveStreamList := POPStackMessage( PStreamBuffer( PByte( ActiveStreamList^.Buffer))^.NextActiveStream);
      PStreamBuffer( PByte( OPStackMessage^.Buffer))^.NextActiveStream := nil;
      Exit;
    end else
    begin
      LocalMessage := POPStackMessage( PStreamBuffer( PByte( ActiveStreamList^.Buffer))^.NextActiveStream);
      LocalMessageParent := ActiveStreamList;
      while LocalMessage <> nil do
      begin
        if LocalMessage = OPStackMessage then
        begin
          PStreamBuffer( PByte( LocalMessageParent^.Buffer))^.NextActiveStream := PStreamBuffer( PByte( LocalMessage^.Buffer))^.NextActiveStream;   // Unlink
          PStreamBuffer( PByte( LocalMessage^.Buffer))^.NextActiveStream := nil;
          Exit
        end;
        LocalMessageParent := LocalMessage;
        LocalMessage := POPStackMessage( PStreamBuffer( PByte( LocalMessage^.Buffer))^.NextActiveStream);
      end;
    end;
  end;
end;

// *****************************************************************************
//  procedure StreamFlushDestinationMessages;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure FlushActiveStreamMessagesByDestinationAlias(var DestID: TNodeInfo);
var
  LocalMessage, MatchingMessage: POPStackMessage;
begin
  LocalMessage := ActiveStreamList;
  while LocalMessage <> nil do
  begin
    if NMRAnetUtilities_EqualNodeIDInfo(DestID, LocalMessage^.Dest) then
    begin
      MatchingMessage := LocalMessage;
      LocalMessage := POPStackMessage( PStreamBuffer( PByte( LocalMessage^.Buffer))^.NextActiveStream); // Get the next one so we can remove the current one
      RemoveActiveStreamMessage(MatchingMessage);                               // Remove that message
      OPStackBuffers_DeAllocateMessage(MatchingMessage);                        // Free that message
    end else
      LocalMessage := POPStackMessage( PStreamBuffer( PByte( LocalMessage^.Buffer))^.NextActiveStream);
  end;
end;

// *****************************************************************************
//  procedure StreamInitRequest;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure StreamInitRequest(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
var
  NewMessage: POPStackMessage;
begin
  if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI_STREAM_INIT_REQUEST, AMessage^.Source.AliasID, AMessage^.Source.ID, AMessage^.Dest.AliasID, AMessage^.Dest.ID) then
  begin
    OPStackBuffers_CopyData(NewMessage^.Buffer, AMessage^.Buffer);
    OPStackNode_IncomingMessageLink(DestNode, NewMessage);
  end else
    OptionalInteractionRejected(AMessage, DestNode, False);                            // Try again if you wish
end;

// *****************************************************************************
//  procedure StreamInitReply;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure StreamInitReply(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
var
  NewMessage: POPStackMessage;
begin
  if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI_STREAM_INIT_REPLY, AMessage^.Source.AliasID, AMessage^.Source.ID, AMessage^.Dest.AliasID, AMessage^.Dest.ID) then
  begin
    OPStackBuffers_CopyData(NewMessage^.Buffer, AMessage^.Buffer);
    OPStackNode_IncomingMessageLink(DestNode, NewMessage);
  end else
    OptionalInteractionRejected(AMessage, DestNode, False);                            // Try again if you wish

  (*
  StreamMessage := nil;
//  StreamMessage := OPStackNode_FindStream(DestNode, AMessage^.Buffer^.DataArray[4], 0, AMessage^.Source);
  if StreamMessage <> nil then
  begin
    StreamBuffer := PStreamBuffer( PByte( StreamMessage^.Buffer));
    StreamBuffer^.TotalMessageSize := (AMessage^.Buffer^.DataArray[0] shl 8) or AMessage^.Buffer^.DataArray[1];
    if (AMessage^.Buffer^.DataArray[2] and STREAM_REPLY_ACCEPT <> 0) and (StreamBuffer^.DataBufferSize > 0) then
    begin                                             // Initialization was Accepted
      StreamBuffer^.iStateMachine := STATE_CONFIG_MEM_STREAM_SEND;
    end else
    begin                                             // Initialization was Rejected
      if AMessage^.Buffer^.DataArray[2] and STREAM_REPLY_UNEXPECTED_ERROR <> 0 then
      begin
        // Ugly error, not sure what to do
      end else
      if AMessage^.Buffer^.DataArray[2] and STREAM_REPLY_PERMANENT_ERROR <> 0 then
      begin
        case AMessage^.Buffer^.DataArray[3] and $E0 of
          STREAM_REPLY_INVALID_REQUEST       : begin end;
          STREAM_REPLY_SOURCE_NOT_PERMITTED  : begin end;
          STREAM_REPLY_STREAM_NOT_ACCEPTED   : begin end;
        end;
      end else
      begin
        // Not a critical error we can try again
        case StreamBuffer^.DataArray[3] and $E0 of
          STREAM_REPLY_BUFFER_FULL       : begin end;
          STREAM_REPLY_INTERNAL_ERROR  : begin end;
        end;
      end;
    end;
  end;        *)
end;

// *****************************************************************************
//  procedure StreamProceed;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure StreamProceed(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
var
  NewMessage: POPStackMessage;
begin
  if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI_STREAM_PROCEED, AMessage^.Source.AliasID, AMessage^.Source.ID, AMessage^.Dest.AliasID, AMessage^.Dest.ID) then
  begin
    OPStackBuffers_CopyData(NewMessage^.Buffer, AMessage^.Buffer);
    OPStackNode_IncomingMessageLink(DestNode, NewMessage);
  end else
    OptionalInteractionRejected(AMessage, DestNode, False);                            // Try again if you wish
end;

procedure StreamComplete(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
var
  NewMessage: POPStackMessage;
begin
  if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI_STREAM_COMPLETE, AMessage^.Source.AliasID, AMessage^.Source.ID, AMessage^.Dest.AliasID, AMessage^.Dest.ID) then
  begin
    OPStackBuffers_CopyData(NewMessage^.Buffer, AMessage^.Buffer);
    OPStackNode_IncomingMessageLink(DestNode, NewMessage);
  end else
    OptionalInteractionRejected(AMessage, DestNode, False);                            // Try again if you wish
end;

// *****************************************************************************
//  procedure StreamInitRequestReply
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure StreamInitRequestReply(Node: PNMRAnetNode; var MessageToSend: POPStackMessage; StreamMessage: POPStackMessage);
var
  BasicBuffer: PSimpleBuffer;
  StreamBuffer: PStreamBuffer;
  NewStreamMessage: POPStackMessage;
begin
  MessageToSend := nil;
  if OPStackBuffers_AllocateOPStackMessage(MessageToSend, MTI_STREAM_INIT_REPLY, StreamMessage^.Dest.AliasID, StreamMessage^.Dest.ID, StreamMessage^.Source.AliasID, StreamMessage^.Source.ID) then
    if OPStackBuffers_AllcoateStreamMessage(NewStreamMessage, MTI_STEAM_SEND, StreamMessage^.Dest.AliasID, StreamMessage^.Dest.ID, StreamMessage^.Source.AliasID, StreamMessage^.Source.ID, False) then
    begin
      BasicBuffer := MessageToSend^.Buffer;
      AddActiveStreamMessage(NewStreamMessage);
      StreamBuffer := PStreamBuffer( PByte( NewStreamMessage^.Buffer));
      StreamBuffer^.SourceStreamID := StreamMessage^.Buffer^.DataArray[4];
      StreamBuffer^.DestStreamID := GenerateStreamID;
      StreamBuffer^.NegotiatedBufferSize := (BasicBuffer^.DataArray[0] shl 8) or BasicBuffer^.DataArray[1];
      if StreamBuffer^.NegotiatedBufferSize > USER_MAX_STREAM_BYTES then
        StreamBuffer^.NegotiatedBufferSize := USER_MAX_STREAM_BYTES;


      BasicBuffer^.DataArray[0] := Hi( StreamBuffer^.NegotiatedBufferSize);
      BasicBuffer^.DataArray[1] := Lo( StreamBuffer^.NegotiatedBufferSize);

      // NEED TO SET FLAGS ACCORDINGLY HERE>>>>>>>>>
      BasicBuffer^.DataArray[2] := 0;                                           // No UID in stream
      BasicBuffer^.DataArray[3] := 0;                                           // No Additional


      BasicBuffer^.DataArray[4] := StreamBuffer^.SourceStreamID;                // Source Stream ID
      BasicBuffer^.DataArray[5] := StreamBuffer^.DestStreamID;                  // Dest Stream ID
      BasicBuffer^.DataBufferSize := 6;
    end else
    begin
      OPStackBuffers_DeAllocateMessage(MessageToSend);
      MessageToSend := nil;
    end;
end;

// *****************************************************************************
//  procedure StreamInitReplyReply
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure StreamInitReplyReply(Node: PNMRAnetNode; var MessageToSend: POPStackMessage; StreamMessage: POPStackMessage);
begin

end;

// *****************************************************************************
//  procedure StreamSendReply
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure StreamSendReply(Node: PNMRAnetNode; var MessageToSend: POPStackMessage; StreamMessage: POPStackMessage);
begin

end;

// *****************************************************************************
//  procedure StreamProceedReply
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure StreamProceedReply(Node: PNMRAnetNode; var MessageToSend: POPStackMessage; StreamMessage: POPStackMessage);
begin

end;

// *****************************************************************************
//  procedure StreamCompleteReply
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure StreamCompleteReply(Node: PNMRAnetNode; var MessageToSend: POPStackMessage; StreamMessage: POPStackMessage);
begin

end;

// *****************************************************************************
//  procedure NodeRunOutgoingStreamStateMachine
//     Parameters:
//     Returns:    True if a message was loaded
//     Description: Picks up Buffers pending in the node and tries to send the reply
//     Description: YOU MUST CHECK IsOutgoingBufferAvailable BEFORE CALLING THIS FUNCTION
// *****************************************************************************
function NodeRunOutgoingStreamStateMachine(Node: PNMRAnetNode): Boolean;
var
  NextStream: POPStackMessage;
  StreamBuffer: PStreamBuffer;
  LocalOutgoingMessage: TOPStackMessage;
  LocalOutgoingBuffer: TSimpleBuffer;
  LocalCount: Word;
begin
  Result := False;
  NextStream := nil;
 // NextStream := OPStackNode_NextStream(Node);
  if NextStream <> nil then
    if NextStream^.Buffer^.State and ABS_STREAM_OUTGOING <> 0 then
    begin
      StreamBuffer := PStreamBuffer( PByte( NextStream^.Buffer));
      case StreamBuffer^.iStateMachine of
        STATE_CONFIG_MEM_STREAM_START :
            begin
              // If this stream was kicked off by a Config Memory interaction it moves it off this state when it receives an ACK from the datagram reply
            end;
        STATE_CONFIG_MEM_STREAM_INIT :
            begin
              OPStackBuffers_ZeroMessage(@LocalOutgoingMessage);
              OPStackBuffers_ZeroSimpleBuffer(@LocalOutgoingBuffer, False);
              OPStackBuffers_LoadMessage(@LocalOutgoingMessage, MTI_STREAM_INIT_REQUEST, NextStream^.Source.AliasID, NextStream^.Source.ID, NextStream^.Dest.AliasID, NextStream^.Dest.ID, $00);
              LocalOutgoingMessage.MessageType := MT_SIMPLE;                  // Not a Stream MTI, just a normal 8 byte CAN message
              LocalOutgoingBuffer.DataBufferSize := 6;
              LocalOutgoingBuffer.DataArray[0] := Hi(USER_MAX_STREAM_BYTES);
              LocalOutgoingBuffer.DataArray[1] := Lo(USER_MAX_STREAM_BYTES);
              LocalOutgoingBuffer.DataArray[2] := 0;                                 // No unique stream content UIDs
              LocalOutgoingBuffer.DataArray[3] := 0;                                 // No flags for now
              LocalOutgoingBuffer.DataArray[4] := AllocateStreamSourceID;            // Unique ID
              LocalOutgoingBuffer.DataArray[5] := 0;                                 // Reserved
              StreamBuffer^.iStateMachine := STATE_CONFIG_MEM_STREAM_WAIT_FOR_INIT_REPLY;
              StreamBuffer^.SourceStreamID := LocalOutgoingBuffer.DataArray[4];
              StreamBuffer^.CurrentCount := 0;                                       // Who ever kicked this off should have filled in StreamBuffer^.TotalMessageSize
              LocalOutgoingMessage.Buffer := @LocalOutgoingBuffer;
    //          OutgoingMessage(@LocalOutgoingMessage);
              Result := True
            end;
        STATE_CONFIG_MEM_STREAM_WAIT_FOR_INIT_REPLY :
            begin
              // Stream Message updated from within the Receive decode
            end;
        STATE_CONFIG_MEM_STREAM_SEND :
            begin
              if StreamBuffer^.CurrentCount >= StreamBuffer^.TotalMessageSize then
              begin
                StreamBuffer^.iStateMachine := STATE_CONFIG_MEM_STREAM_SEND_COMPLETE;
                Exit;
              end else
              begin
                LocalCount := 0;

                if StreamBuffer^.State and ABS_STREAM_TYPE_ID <> 0 then
                begin
                  while LocalCount < MAX_STREAM_TYPE_ID do
                  begin
                    StreamBuffer^.DataArray[LocalCount] := StreamBuffer^.StreamTypeID[LocalCount];
                    Inc(LocalCount);
                  end;
                  StreamBuffer^.CurrentCount := StreamBuffer^.CurrentCount + 6;
                end;

                StreamBuffer^.DataArray[LocalCount] := StreamBuffer^.SourceStreamID;
                StreamBuffer^.DataArray[LocalCount+1] := StreamBuffer^.DestStreamID;
                StreamBuffer^.CurrentCount := StreamBuffer^.CurrentCount + 2;
                while StreamBuffer^.CurrentCount < StreamBuffer^.TotalMessageSize do
                begin
                  // LOAD UP THE BUFFER HERE
                  StreamBuffer^.DataArray[LocalCount] := $88;
                  Inc(StreamBuffer^.CurrentCount);
                  Inc(LocalCount);
                  if LocalCount >= StreamBuffer^.DataBufferSize then            // Only fill to the negotiated buffer size
                    Break;                                                      // This leaves CurrentCount as the total counter heading towards TotalMessagseSize
                end;

        //        OutgoingMessage(NextStream);                                    // MAKE SURE WE DON'T FREE THIS MESSAGE
                StreamBuffer^.iStateMachine := STATE_CONFIG_MEM_STREAM_WAIT_FOR_PROCEED;
                Result := True;
                Exit;
              end;
            end;
        STATE_CONFIG_MEM_STREAM_WAIT_FOR_PROCEED :                              // Also is a "wait for stream chunks to be sent" then "wait for proceed" if under a CAN bus
            begin
              Exit;
            end;
        STATE_CONFIG_MEM_STREAM_SEND_COMPLETE :
            begin
              OPStackBuffers_ZeroMessage(@LocalOutgoingMessage);
              OPStackBuffers_ZeroSimpleBuffer(@LocalOutgoingBuffer, False);
              OPStackBuffers_LoadMessage(@LocalOutgoingMessage, MTI_STREAM_COMPLETE, NextStream^.Source.AliasID, NextStream^.Source.ID, NextStream^.Dest.AliasID, NextStream^.Dest.ID, $00);
              LocalOutgoingBuffer.DataBufferSize := 4;
              LocalOutgoingBuffer.DataArray[0] := StreamBuffer^.SourceStreamID;
              LocalOutgoingBuffer.DataArray[1] := StreamBuffer^.DestStreamID;
              LocalOutgoingBuffer.DataArray[2] := 0;                                 // Flags
              LocalOutgoingBuffer.DataArray[3] := 0;                                 // Flags
              StreamBuffer^.iStateMachine := STATE_CONFIG_MEM_STREAM_COMPLETE;
              LocalOutgoingMessage.Buffer := @LocalOutgoingBuffer;
     //         OutgoingMessage(@LocalOutgoingMessage);
              Result := True;
              Exit;
            end;
        STATE_CONFIG_MEM_STREAM_COMPLETE :
            begin
              Exit;
            end;
      end;
    end;
end;

{ STREAM }
{
          MTI_FRAME_TYPE_STREAM_INIT_REQUEST  : begin  // Remote is asking to send data to us (we are the receiver)
          Node := NMRAnetNode_FindByAlias(NMRAnetUtilities_ExtractDestinationAlias(CANBuffer));
          if Node <> nil then
          begin
            if NMRAnetBufferPools_AllocateStreamBuffer(StreamBuffer, False) then
            begin
              StreamBuffer^.Alias := ((CANBuffer^.DataBytes[0] shl 8) or CANBuffer^.DataBytes[1]) and $0FFF;
              StreamBuffer^.NegotiatedTransferSize := (CANBuffer^.DataBytes[2] shl 8) or CANBuffer^.DataBytes[3];
              StreamBuffer^.Content.TypeIncluded := (CANBuffer^.DataBytes[4] and $01) <> 0;
              StreamBuffer^.RemoteStreamID := CANBuffer^.DataBytes[6];
              StreamBuffer^.State := StreamBuffer^.State or CBS_PROCESSING or CBS_OUTGOING; // Need to get the buffer into a Transmit mode
              StreamBuffer^.StateMachine := STATE_STREAM_INITIATE_REQEUST_WAIT_FOR_REPLY;  // Now let the ProcessHardwareMessages decide how to reply to the other node
              NMRAnetUtilities_StreamBufferLink(Node, StreamBuffer);
            end else
            begin
              // High priority fail reply
            end;
          end;
          end;
          MTI_FRAME_TYPE_STREAM_INIT_REPLY    : begin  // We asked to send data to the Remote and has replied
          Node := NMRAnetNode_FindByAlias(NMRAnetUtilities_ExtractDestinationAlias(CANBuffer));
          if Node <> nil then
          begin
             // Find and inprocess Stream that matches the SID that we sent first, we don't know what the remote SID is yet
             if NMRAnetUtilities_FindInProcessStreamInNode(Node, StreamBuffer, CANBuffer^.DataBytes[6], -1) then
             begin
               StreamBuffer^.iWatchdog := 0;
               StreamBuffer^.NegotiatedTransferSize := (CANBuffer^.DataBytes[2] shl 8) or CANBuffer^.DataBytes[3];
               StreamBuffer^.RemoteStreamID := CANBuffer^.DataBytes[7];
               StreamBuffer^.StateMachine := STATE_STREAM_SEND;  // Start Sending to the Remote node
               StreamBuffer^.State := StreamBuffer^.State or CBS_OUTGOING;
               // NEED TO FIND THE "ACCEPT" BIT SOMEWHERE
               if StreamBuffer^.NegotiatedTransferSize > 0 then
               begin
               end else
               begin
                 // The remote node did not want to play with us
                 NMRAnetUtilities_StreamBufferUnLink(Node, StreamBuffer);
                 NMRAnetBufferPools_ReleaseStreamBuffer(StreamBuffer);
               end
             end
          end;
          end;
          MTI_FRAME_TYPE_STREAM_SEND          : begin  // Remote is asked (and is) to send data to us (we are the receiver)
          Node := NMRAnetNode_FindByAlias( NMRAnetUtilities_ExtractDestinationCodedInMTIAlias(CANBuffer));
          if Node <> nil then
          begin
            // HOW DO WE KNOW WHAT STREAM IS WHAT HERE?????
            if NMRAnetUtilities_FindInProcessStreamInNode(Node, StreamBuffer, -1, -1) then
            begin
              StreamBuffer^.iWatchdog := 0;
              for i := 0 to CANBuffer^.DataCount - 1 do
              begin
                StreamBuffer^.DataBytes[StreamBuffer^.iByteCount] := CANBuffer^.DataBytes[i];
                Inc(StreamBuffer^.iByteCount)
              end;
              if StreamBuffer^.iByteCount >= StreamBuffer^.NegotiatedTransferSize then
              begin
          //      StreamBuffer^.StateMachine :=
              end
            end
          end
          end;
          MTI_FRAME_TYPE_STREAM_PROCEED       : begin
          Node := NMRAnetNode_FindByAlias(NMRAnetUtilities_ExtractDestinationAlias(CANBuffer));
          if Node <> nil then
          begin
          end;
          end;
          MTI_FRAME_TYPE_STREAM_COMPLETE      : begin
          Node := NMRAnetNode_FindByAlias(NMRAnetUtilities_ExtractDestinationAlias(CANBuffer));
          if Node <> nil then
          begin
          end;
          end;
      }

end.

