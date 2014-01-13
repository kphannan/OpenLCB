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

{$IFDEF SUPPORT_STREAMS}
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

{$ENDIF}

implementation

var
  ActiveStreamList: POPStackMessage;
  StreamIDPool: Byte;
  
{$IFDEF SUPPORT_STREAMS}

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
{$ENDIF}

end.