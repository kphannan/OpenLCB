unit opstackcore_datagram;

// TODO : FindWaitingForAckResponseMessage - ONLY WORKS WITH ALIAS IDs,  Needs to work with Alias and/or Full IDs

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  opstack_api,
  opstacknode,
  opstackbuffers,
  opstackcore_configmem,
  template_hardware,
  nmranetdefines,
  nmranetutilities,
  opstackdefines,
  opstacktypes;

const
  MAX_DATAGRAM_RESEND_ATTEMPTS = 5;

procedure OPStackCoreDatagram_Initialize;

procedure DatagramOkReplyHandler(Node: PNMRAnetNode; OPStackMessage: POPStackMessage);
procedure DatagramRejectedReplyHandler(Node: PNMRAnetNode; OPStackMessage: POPStackMessage);
function DatagramReplyHandler(Node: PNMRAnetNode; var MessageToSend: POPStackMessage; DatagramMessage: POPStackMessage): Boolean;

implementation

// *****************************************************************************
//  procedure OPStackCoreDatagram_Initialize;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCoreDatagram_Initialize;
begin

end;

// *****************************************************************************
//  procedure DatagramReplyHandler;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function DatagramReplyHandler(Node: PNMRAnetNode; var MessageToSend: POPStackMessage; DatagramMessage: POPStackMessage): Boolean;
var
  DatagramBufferPtr: PDatagramBuffer;
begin
  Result := False;
  MessageToSend := nil;
  DatagramBufferPtr := PDatagramBuffer( PByte( DatagramMessage^.Buffer));

  case DatagramBufferPtr^.iStateMachine of
    STATE_DATAGRAM_SEND_ACK :     // The message pump can have this message and free it, we don't care
        begin
          if OPStackBuffers_AllocateOPStackMessage(MessageToSend, MTI_DATAGRAM_OK_REPLY, DatagramMessage^.Dest.AliasID, DatagramMessage^.Dest.ID, DatagramMessage^.Source.AliasID, DatagramMessage^.Source.ID, False) then
          begin
            MessageToSend^.Buffer^.DataBufferSize := 1;
            MessageToSend^.Buffer^.DataArray[0] := $00;                         // New Flags for the ACK (slow reply, etc) mmmm... need to have the type decoded to decide what to reply here.....
            Result := True;
            DatagramBufferPtr^.iStateMachine := STATE_DATAGRAM_PROCESS
          end
        end;
    STATE_DATAGRAM_PROCESS :
        begin                // reusing the Datagram buffer but we will send it ourselfs as we don't want it freed yet.
          case DatagramBufferPtr^.DataArray[0] of
              DATAGRAM_TYPE_MEMORY_CONFIGURATION :
                  begin
                    case DatagramBufferPtr^.DataArray[1] and $F0 of
                       MCP_COMMAND_READ             : begin CommandReadReplyHandler(Node, DatagramMessage); Exit; end;
                       MCP_COMMAND_READ_REPLY_OK    : begin Exit; end;
                       MCP_COMMAND_READ_REPLY_FAIL  : begin Exit; end;
                       MCP_COMMAND_READ_STREAM      : begin CommandReadStreamReplyHandler(Node, DatagramMessage); Exit; end;
                       MCP_COMMAND_READ_SREAM_REPLY : begin Exit; end;
                       MCP_COMMAND_WRITE            : begin CommandWriteReplyHandler(Node, DatagramMessage); Exit; end;
                       MCP_COMMAND_WRITE_REPLY_OK   : begin Exit; end;
                       MCP_COMMAND_WRITE_REPLY_FAIL : begin Exit; end;
                       MCP_COMMAND_WRITE_STREAM     : begin CommandWriteStreamReplyHandler(Node, DatagramMessage); Exit; end;
                       MCP_OPERATION :
                           begin
                             case DatagramBufferPtr^.DataArray[1] of
                               MCP_OP_GET_CONFIG         : begin OperationGetConfigurationReplyHandler(Node, DatagramMessage); Exit; end;
                               MCP_OP_GET_ADD_SPACE_INFO : begin OperationGetSpaceInfoReplyHandler(Node, DatagramMessage); Exit; end;
                               MCP_OP_LOCK               : begin OperationLockReplyHandler(Node, DatagramMessage); Exit; end;
                               MCP_OP_GET_UNIQUEID       : begin OperationGetUniqueIDReplyHandler(Node, DatagramMessage); Exit; end;
                               MCP_OP_FREEZE             : begin OperationFreezeReplyHandler(Node, DatagramMessage); Exit; end;
                               MCP_OP_INDICATE           : begin OperationIndicateReplyHandler(Node, DatagramMessage); Exit; end;
                               MCP_OP_UPDATE_COMPLETE    : begin OperationUpdateCompleteReplyHandler(Node, DatagramMessage); Exit; end;
                               MCP_OP_RESETS             : begin OperationResetReplyHandler(Node, DatagramMessage); Exit; end
                             else
                               begin DatagramBufferPtr^.iStateMachine := STATE_DATAGRAM_DONE; Exit; end;
                             end
                           end
                    else // case
                      begin DatagramBufferPtr^.iStateMachine := STATE_DATAGRAM_DONE; Exit; end;
                    end  // case
                  end
            else   // case
              begin DatagramBufferPtr^.iStateMachine := STATE_DATAGRAM_DONE; Exit; end;
            end;   // case
          Exit;
        end;
    STATE_DATAGRAM_SEND :
        begin
          if IsOutgoingBufferAvailable then
          begin
            OutgoingMessage(DatagramMessage, False);
            DatagramBufferPtr^.iStateMachine := STATE_DATAGRAM_WAITFOR_PROCESS_ACK;
          end;
          Exit;
        end;
    STATE_DATAGRAM_WAITFOR_PROCESS_REPLY :                                      // polled if we requested datagram information
        begin
          Exit;
        end;
    STATE_DATAGRAM_WAITFOR_PROCESS_ACK :                                        // polled if we sent datagram data and are waiting for the receiving node to reply
        begin
          Exit;
        end;
    STATE_DATAGRAM_DONE :
        begin
          OPStackNode_IncomingMessageUnLink(Node, DatagramMessage);
          OPStackBuffers_DeAllocateMessage(DatagramMessage);
        end;
  end;
end;

// *****************************************************************************
//  procedure DatagramOkReplyHandler;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure DatagramOkReplyHandler(Node: PNMRAnetNode; OPStackMessage: POPStackMessage);
var
  DatagramBufferPtr: PDatagramBuffer;
  Msg: POPStackMessage;
begin
  // These CAN NOT be linked to the node and processed in the main loop as the main loop
  // links are blocked by the datagram itself.  May need new link system once we get to Streams.
  // This should be ok due to the way we block the interupts
  // This OPStackMessage is for the Datagram OK message, NOT the core linked message....
  DatagramBufferPtr := PDatagramBuffer( PByte( Node^.IncomingMessages^.Buffer));
  if DatagramBufferPtr^.iStateMachine = STATE_DATAGRAM_WAITFOR_PROCESS_ACK then
  begin   // Gotta do this fast before the node sends us another datagram and we still have the buffer plugged up
    Msg := Node^.IncomingMessages;
    OPStackNode_IncomingMessageUnLink(Node, Msg);
    OPStackBuffers_DeAllocateMessage(Msg);
  end;
end;

// *****************************************************************************
//  procedure DatagramRejectedReplyHandler;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure DatagramRejectedReplyHandler(Node: PNMRAnetNode; OPStackMessage: POPStackMessage);
var
  DatagramBufferPtr: PDatagramBuffer;
  Msg: POPStackMessage;
  ErrorCode: Word;
begin
  // These CAN NOT be linked to the node and processed in the main loop as the main loop
  // links are blocked by the datagram itself.  May need new link system once we get to Streams.
  // This should be ok due to the way we block the interupts
  // This OPStackMessage is for the Datagram OK message, NOT the core linked message....
  DatagramBufferPtr := PDatagramBuffer( PByte( Node^.IncomingMessages^.Buffer));
  if DatagramBufferPtr^.iStateMachine = STATE_DATAGRAM_WAITFOR_PROCESS_ACK then
  begin
    ErrorCode := (OPStackMessage^.Buffer^.DataArray[0] shl 8) or OPStackMessage^.Buffer^.DataArray[1];
    if (ErrorCode and DATAGRAM_RESULT_REJECTED_RESEND_MASK <> 0) and (DatagramBufferPtr^.ResendCount < MAX_DATAGRAM_RESEND_ATTEMPTS) then
    begin
      Inc(DatagramBufferPtr^.ResendCount);
      DatagramBufferPtr^.CurrentCount := 0;
      DatagramBufferPtr^.iStateMachine := STATE_DATAGRAM_SEND;
    end else
    begin // Gotta do this fast before the node sends us another datagram and we still have the buffer plugged up
      Msg := Node^.IncomingMessages;
      OPStackNode_IncomingMessageUnLink(Node, Msg);
      OPStackBuffers_DeAllocateMessage(Msg);
    end;
  end;
end;

end.