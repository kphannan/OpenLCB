unit opstackcore_stream;

// TODO : StreamProceed is broken, need to understand overall architecture to finish

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  opstackbuffers,
  nmranetdefines,
  opstackdefines,
  template_buffers,
  opstacktypes;

procedure StreamInitRequest(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
procedure StreamInitReply(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
procedure StreamProceed(AMessage: POPStackMessage; DestNode: PNMRAnetNode);

implementation

procedure StreamInitRequest(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
begin

end;

procedure StreamInitReply(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
var
  StreamMessage: POPStackMessage;
  StreamBuffer: PStreamBuffer;
begin
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
  end;
end;

procedure StreamProceed(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
var
  StreamMessage: POPStackMessage;
  StreamBuffer: PStreamBuffer;
begin
  StreamMessage := nil;
//  StreamMessage := FindStream(AMessage^.Source, AMessage^.Dest, AMessage^.Buffer^.DataArray[0], AMessage^.Buffer^.DataArray[1], STATE_CONFIG_MEM_STREAM_WAIT_FOR_PROCEED);
  if StreamMessage <> nil then
  begin
    StreamBuffer := PStreamBuffer( PByte( StreamMessage^.Buffer));
    StreamBuffer^.iStateMachine := STATE_CONFIG_MEM_STREAM_SEND;
    // there are flags but not defined yet.....
  end
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

end.

