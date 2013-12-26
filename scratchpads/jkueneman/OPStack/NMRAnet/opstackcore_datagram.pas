unit opstackcore_datagram;

// TODO : FindWaitingForAckResponseMessage - ONLY WORKS WITH ALIAS IDs,  Needs to work with Alias and/or Full IDs

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  opstacknode,
  opstackbuffers,
  opstackcore_configmem,
  nmranetdefines,
  nmranetutilities,
  opstackdefines,
  opstacktypes;

const
  MAX_DATAGRAM_RESEND_ATTEMPTS = 5;

procedure OPStackCoreDatagram_Initialize;

// Do not currently have a function that initiates a datagram from scratch, currently we mostly repond from a sent datagram
procedure DatagramOkReply(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
procedure DatagramRejectedReply(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
function DatagramSendAckReply(Node: PNMRAnetNode; var MessageToSend: POPStackMessage; var SourceID: TNodeInfo; var DestID: TNodeInfo; DatagramBufferPtr: PDatagramBuffer): Boolean;
function DatagramReply(Node: PNMRAnetNode; var MessageToSend: POPStackMessage; DatagramMessage: POPStackMessage): Boolean;

procedure FlushWaitingForAckResponseMessagesByDestinationAlias(var DestID: TNodeInfo);

implementation

var
  WaitingForAckList: POPStackMessage;

// *****************************************************************************
//  procedure OPStackCoreDatagram_Initialize;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCoreDatagram_Initialize;
begin
  WaitingForAckList := nil;
end;

// *****************************************************************************
//  procedure AddWaitingForAckResponseMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure AddWaitingForAckResponseMessage(OPStackMessage: POPStackMessage);
var
  LocalMessage: POPStackMessage;
begin
  if WaitingForAckList = nil then
    WaitingForAckList := OPStackMessage
  else begin
    LocalMessage := WaitingForAckList;
    while PDatagramBuffer( PByte( LocalMessage^.Buffer))^.NextWaitingForAck <> nil do     // Walk the stack to the end and add our new message
      LocalMessage := POPStackMessage( PDatagramBuffer( PByte( LocalMessage^.Buffer))^.NextWaitingForAck);  // WARNING MUST REMOVE THE MESSAGE FROM THE STACK IF THE DATAGRAM IS ABANDON!!!!!!!
    PDatagramBuffer(PByte( LocalMessage^.Buffer))^.NextWaitingForAck := PByte( OPStackMessage);
  end;
end;

// *****************************************************************************
//  procedure FindWaitingForAckResponseMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function FindWaitingForAckResponseMessage(var SourceID: TNodeInfo; var DestID: TNodeInfo): POPStackMessage;
var
  LocalMessage: POPStackMessage;
begin
  LocalMessage := WaitingForAckList;
  while LocalMessage <> nil do
  begin
    if NMRAnetUtilities_EqualNodeIDInfo(SourceID, LocalMessage^.Source) then
      if NMRAnetUtilities_EqualNodeIDInfo(DestID, LocalMessage^.Dest) then
      begin
        Result := LocalMessage;
        Exit;
      end;
    LocalMessage := POPStackMessage( PDatagramBuffer( PByte( LocalMessage^.Buffer))^.NextWaitingForAck);
  end;
end;

// *****************************************************************************
//  procedure RemoveWaitingForAckResponseMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure RemoveWaitingForAckResponseMessage(OPStackMessage: POPStackMessage);
var
  LocalMessage, LocalMessageParent: POPStackMessage;
begin
  if WaitingForAckList <> nil then
  begin
    if WaitingForAckList = OPStackMessage then
    begin
      WaitingForAckList := POPStackMessage( PDatagramBuffer( PByte( WaitingForAckList^.Buffer))^.NextWaitingForAck);
      PDatagramBuffer( PByte( OPStackMessage^.Buffer))^.NextWaitingForAck := nil;
      Exit;
    end else
    begin
      LocalMessage := POPStackMessage( PDatagramBuffer( PByte( WaitingForAckList^.Buffer))^.NextWaitingForAck);
      LocalMessageParent := WaitingForAckList;
      while LocalMessage <> nil do
      begin
        if LocalMessage = OPStackMessage then
        begin
          PDatagramBuffer( PByte( LocalMessageParent^.Buffer))^.NextWaitingForAck := PDatagramBuffer( PByte( LocalMessage^.Buffer))^.NextWaitingForAck;   // Unlink
          PDatagramBuffer( PByte( LocalMessage^.Buffer))^.NextWaitingForAck := nil;
          Exit
        end;
        LocalMessageParent := LocalMessage;
        LocalMessage := POPStackMessage( PDatagramBuffer( PByte( LocalMessage^.Buffer))^.NextWaitingForAck);
      end;
    end;
  end;
end;

// *****************************************************************************
//  procedure FlushWaitingForAckResponseMessagesByDestinationAlias;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure FlushWaitingForAckResponseMessagesByDestinationAlias(var DestID: TNodeInfo);
var
  LocalMessage, MatchingMessage: POPStackMessage;
begin
  LocalMessage := WaitingForAckList;
  while LocalMessage <> nil do
  begin
    if NMRAnetUtilities_EqualNodeIDInfo(DestID, LocalMessage^.Dest) then
    begin
      MatchingMessage := LocalMessage;
      LocalMessage := POPStackMessage( PDatagramBuffer( PByte( LocalMessage^.Buffer))^.NextWaitingForAck); // Get the next one so we can remove the current one
      RemoveWaitingForAckResponseMessage(MatchingMessage);                      // Remove that message
      OPStackBuffers_DeAllocateMessage(MatchingMessage);                        // Free that message
    end else
      LocalMessage := POPStackMessage( PDatagramBuffer( PByte( LocalMessage^.Buffer))^.NextWaitingForAck);
  end;
end;

// *****************************************************************************
//  procedure DatagramSendAckReply;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function DatagramSendAckReply(Node: PNMRAnetNode; var MessageToSend: POPStackMessage; var SourceID: TNodeInfo; var DestID: TNodeInfo; DatagramBufferPtr: PDatagramBuffer): Boolean;
var
  AckFlags: Byte;
begin
  Result := True;
  if DatagramBufferPtr^.State and ABS_HASBEENACKED = 0 then
  begin
    if OPStackBuffers_AllocateOPStackMessage(MessageToSend, MTI_DATAGRAM_OK_REPLY, SourceID.AliasID, SourceID.ID, DestID.AliasID, DestID.ID) then
    begin
      AckFlags := $00;        // May want to change this for slow configuration reads/writes
      OPStackBuffers_LoadDatagramOkMessage(MessageToSend, SourceID.AliasID, SourceID.ID, DestID.AliasID, DestID.ID, AckFlags);
      DatagramBufferPtr^.State := DatagramBufferPtr^.State or ABS_HASBEENACKED;
      Result := False;
    end
  end;
end;

// *****************************************************************************
//  procedure DatagramReply;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function DatagramReply(Node: PNMRAnetNode; var MessageToSend: POPStackMessage; DatagramMessage: POPStackMessage): Boolean;
var
  DatagramBufferPtr: PDatagramBuffer;
begin
  Result := False;
  DatagramMessage^.WatchDog := 0;
  DatagramBufferPtr := PDatagramBuffer( PByte( DatagramMessage^.Buffer));
  OPStackNode_IncomingMessageUnLink(Node, DatagramMessage);                             // Recycle the Datagram Message
  OPStackBuffers_SwapDestAndSourceIDs(DatagramMessage);
  DatagramMessage^.DestFlags := 0;
  DatagramBufferPtr^.CurrentCount := 0;
  DatagramBufferPtr^.ResendCount := 0;
  DatagramBufferPtr^.iStateMachine := 0;
  DatagramBufferPtr^.DataBufferSize := 0;
  DatagramBufferPtr^.NextWaitingForAck := nil;
  case DatagramBufferPtr^.DataArray[0] of
      DATAGRAM_TYPE_MEMORY_CONFIGURATION :
          begin
            MessageToSend := DatagramMessage;
            case DatagramBufferPtr^.DataArray[1] and $F0 of
               MCP_COMMAND_READ         : begin Result := CommandReadReply(Node, MessageToSend); end;
               MCP_COMMAND_READ_STREAM  : begin Result := CommandReadStreamReply(Node, MessageToSend); end;
               MCP_COMMAND_WRITE        : begin Result := CommandWriteReply(Node, MessageToSend); end;
               MCP_COMMAND_WRITE_STREAM : begin Result := CommandWriteStreamReply(Node, MessageToSend); end;
               MCP_OPERATION :
                   begin
                     case DatagramBufferPtr^.DataArray[1] of
                       MCP_OP_GET_CONFIG         : begin Result := OperationGetConfigurationReply(Node, MessageToSend); end;
                       MCP_OP_GET_ADD_SPACE_INFO : begin Result := OperationGetSpaceInfoReply(Node, MessageToSend); end;
                       MCP_OP_LOCK               : begin Result := OperationLockReply(Node, MessageToSend); end;
                       MCP_OP_GET_UNIQUEID       : begin Result := OperationGetUniqueIDReply(Node, MessageToSend); end;
                       MCP_OP_FREEZE             : begin Result := OperationFreezeReply(Node, MessageToSend); end;
                       MCP_OP_INDICATE           : begin Result := OperationIndicateReply(Node, MessageToSend); end;
                       MCP_OP_UPDATE_COMPLETE    : begin Result := OperationUpdateCompleteReply(Node, MessageToSend); end;
                       MCP_OP_RESETS             : begin Result := OperationResetReply(Node, MessageToSend); end
                     else begin OPStackBuffers_DeAllocateMessage(DatagramMessage); end;
                     end
                   end
            else begin OPStackBuffers_DeAllocateMessage(DatagramMessage); end;
            end
          end;
      DATAGRAM_TYPE_TRAIN_CONTROL : begin OPStackBuffers_DeAllocateMessage(DatagramMessage); end
    else begin OPStackBuffers_DeAllocateMessage(DatagramMessage); end;
    end;
  if Result then                                                                // If we sent a Datagram then we need to add it to our list to wait for the dest node to Ack it
    AddWaitingForAckResponseMessage(DatagramMessage);
end;

// *****************************************************************************
//  procedure DatagramOkReply;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure DatagramOkReply(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
var
  WaitingMessage: POPStackMessage;
begin
  WaitingMessage := FindWaitingForAckResponseMessage(AMessage^.Dest, AMessage^.Source);
  if WaitingMessage <> nil then
  begin
    RemoveWaitingForAckResponseMessage(WaitingMessage);
    OPStackBuffers_DeAllocateMessage(WaitingMessage);
  end;
end;

// *****************************************************************************
//  procedure DatagramRejectedReply;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure DatagramRejectedReply(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
var
  WaitingMessage: POPStackMessage;
  DatagramBuffer: PDatagramBuffer;
begin
  WaitingMessage := FindWaitingForAckResponseMessage(AMessage^.Dest, AMessage^.Source);
  if WaitingMessage <> nil then
  begin
    DatagramBuffer := PDatagramBuffer( PByte( WaitingMessage^.Buffer));
    RemoveWaitingForAckResponseMessage(WaitingMessage);
    if DatagramBuffer^.ResendCount < MAX_DATAGRAM_RESEND_ATTEMPTS then
    begin
      WaitingMessage^.MessageType := WaitingMessage^.MessageType or MT_SEND;
      Inc(DatagramBuffer^.ResendCount);
      DatagramBuffer^.CurrentCount := 0;
      OPStackNode_IncomingMessageLink(DestNode, WaitingMessage);
      AddWaitingForAckResponseMessage(WaitingMessage);
    end else
      OPStackBuffers_DeAllocateMessage(WaitingMessage);   // Giving Up
  end;
end;

end.
