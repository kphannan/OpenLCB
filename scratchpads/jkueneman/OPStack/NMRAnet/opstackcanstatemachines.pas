unit opstackcanstatemachines;


{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  {$IFDEF FPC}
  Classes, SysUtils,
  {$ENDIF}
  hardware_template,
  nmranetutilities,
  nmranetdefines,
  opstackdefines,
  opstackbuffers,
  opstacknode,
  opstacktypes;

procedure OPStackCANStatemachines_Initialize;
function OPStackCANStatemachine_ProcessIncomingDatagramMessage(OPStackMessage: POPStackMessage): POPStackMessage;
function OPStackCANStatemachine_ProcessIncomingStreamMessage(OPStackMessage: POPStackMessage): POPStackMessage;

procedure OPStackCANStatemachine_AddOutgoingDatagramMessage(OPStackDatagramMessage: POPStackMessage);
procedure OPStackCANStatemachine_RemoveOutgoingDatagramMessage(OPStackDatagramMessage: POPStackMessage);
procedure OPStackCANStatemachine_ProcessOutgoingDatagramMessage;

procedure OPStackCANStatemachine_AddOutgoingStreamMessage(OPStackStreamMessage: POPStackMessage);
procedure OPStackCANStatemachine_RemoveOutgoingStreamMessage(OPStackStreamMessage: POPStackMessage);
procedure OPStackCANStatemachine_ProcessOutgoingStreamMessage;

procedure OPStackCANStatemachine_RemoveDatagramWaitingforACKStack(OPStackStreamMessage: POPStackMessage);
function OPStackCANStatemachine_FindDatagramWaitingforACKStack(var SourceNodeID: TNodeInfo; var DestNodeID: TNodeInfo): POPStackMessage;

implementation

var
  DatagramInProcessStack: POPStackMessage;                                      // Linked List of incoming Datagrams
  StreamInProcessStack: POPStackMessage;                                        // Linked List of incoming Streams
  DatagramOutgoingProcessStack: POPStackMessage;                                // Linked List of outgoing Datagrams
  StreamOutgoingProcessStack: POPStackMessage;                                  // Linked List of outgoing Streams
  DatagramWaitingforACKStack: POPStackMessage;

// *****************************************************************************
//  procedure OPStackCANStatemachines_Initialize;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCANStatemachines_Initialize;
begin
  DatagramInProcessStack := nil;
  StreamInProcessStack := nil;
  DatagramOutgoingProcessStack := nil;
  StreamOutgoingProcessStack := nil;
end;

// *****************************************************************************
//  procedure FindInprocessMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function FindInprocessMessage(OPStackMessage, MessageStackRoot: POPStackMessage): POPStackMessage;
var
  LocalMessage: POPStackMessage;
begin
  Result := nil;
  LocalMessage := MessageStackRoot;
  while LocalMessage <> nil do
  begin
    if OPStackNode_Equal(LocalMessage, OPStackMessage) then
    begin
      Result := LocalMessage;
      Exit;
    end else
      LocalMessage := LocalMessage^.Next;
  end;
end;

// *****************************************************************************
//  procedure AddInprocessMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure AddInprocessMessage(OPStackMessage, MessageStackRoot: POPStackMessage);
var
  LocalMessage: POPStackMessage;
begin
  if MessageStackRoot = nil then
    MessageStackRoot := OPStackMessage
  else begin
    LocalMessage := MessageStackRoot;
    while LocalMessage^.Next <> nil do                                          // Walk the stck to the end and add our new message
      LocalMessage := LocalMessage^.Next;                                       // WARNING MUST REMOVE THE MESSAGE FROM THE STACK IF THE DATAGRAM IS ABANDON!!!!!!!
    LocalMessage^.Next := OPStackMessage;
  end;
end;

// *****************************************************************************
//  procedure RemoveInprocessMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure RemoveInprocessMessage(OPStackMessage, MessageStackRoot: POPStackMessage);
var
  LocalMessage, LocalMessageParent: POPStackMessage;
begin
  if MessageStackRoot = OPStackMessage then
  begin
    MessageStackRoot := OPStackMessage^.Next;
    Exit;
  end else
  begin
    LocalMessage := MessageStackRoot^.Next;
    LocalMessageParent := MessageStackRoot;
    while LocalMessage <> nil do
    begin
      if LocalMessage = OPStackMessage then
      begin
        LocalMessageParent^.Next := LocalMessage^.Next;                         // Unlink
        Exit
      end;
      LocalMessageParent := LocalMessage;
      LocalMessage := LocalMessage^.Next;
    end;
  end;
end;

// *****************************************************************************
//  procedure OPStackCANStatemachine_ProcessIncomingDatagramMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackCANStatemachine_ProcessIncomingDatagramMessage(OPStackMessage: POPStackMessage): POPStackMessage;
var
  InprocessDatagram: POPStackMessage;
begin
  Result := nil;
  InprocessDatagram := FindInprocessMessage(OPStackMessage, DatagramInProcessStack);
  case OPStackMessage^.MTI of
    MTI_FRAME_TYPE_CAN_DATAGRAM_ONLY_FRAME :
        begin
          if InprocessDatagram = nil then
          begin
            case OPStackMessage^.Buffer^.DataArray[0] of
              DATAGRAM_TYPE_BOOTLOADER,
              DATAGRAM_TYPE_MEMORY_CONFIGURATION,
              DATAGRAM_TYPE_TRAIN_CONTROL :
                  begin                                                         // Allocate a message for a full MTI_DATRGRAM and return the pointer to the message
                    if OPStack_AllocateDatagramMessage(InprocessDatagram, MTI_DATAGRAM, nil, OPStackMessage^.Source.AliasID, OPStackMessage^.Source.ID, OPStackMessage^.Dest.AliasID, OPStackMessage^.Dest.ID) then
                    begin
                      InprocessDatagram^.Buffer^.DataBufferSize := OPStackMessage^.Buffer^.DataBufferSize;
                      OPStack_CopyData(InprocessDatagram^.Buffer, OPStackMessage^.Buffer);
                      PDatagramBuffer( InprocessDatagram^.Buffer)^.CurrentCount := 0;
                      Result := InprocessDatagram;
                      Exit;
                    end else                                                    // No Buffer available, try again
                      OPStack_LoadDatagramRejectedBuffer(OPStackMessage^.Dest.AliasID, OPStackMessage^.Dest.ID, OPStackMessage^.Source.AliasID, OPStackMessage^.Source.ID, @DATAGRAM_RESULT_REJECTED_BUFFER_FULL)
                  end
              else begin                                                        // Unknown Datagram Type
                OPStack_LoadDatagramRejectedBuffer(OPStackMessage^.Dest.AliasID, OPStackMessage^.Dest.ID, OPStackMessage^.Source.AliasID, OPStackMessage^.Source.ID, @DATAGRAM_RESULT_REJECTED_SOURCE_DATAGRAMS_NOT_ACCEPTED)
              end;
            end;
          end else
          begin
            // The node has a DG connection already, can't have two just drop it
          end;
        end;
    MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME_START :
        begin
          if InprocessDatagram = nil then
          begin
            if OPStack_AllocateDatagramMessage(InprocessDatagram, MTI_DATAGRAM, nil, OPStackMessage^.Source.AliasID, OPStackMessage^.Source.ID, OPStackMessage^.Dest.AliasID, OPStackMessage^.Dest.ID) then
            begin
              InprocessDatagram^.Buffer^.DataBufferSize := OPStackMessage^.Buffer^.DataBufferSize;
              OPStack_CopyData(InprocessDatagram^.Buffer, OPStackMessage^.Buffer);
              PDatagramBuffer( InprocessDatagram^.Buffer)^.CurrentCount := OPStackMessage^.Buffer^.DataBufferSize;
              OPStackCANStatemachine_AddOutgoingDatagramMessage(InprocessDatagram);
              Exit
            end else                                                            // No Buffer available, try again
              OPStack_LoadDatagramRejectedBuffer(OPStackMessage^.Dest.AliasID, OPStackMessage^.Dest.ID, OPStackMessage^.Source.AliasID, OPStackMessage^.Source.ID, @DATAGRAM_RESULT_REJECTED_BUFFER_FULL)
          end else
          begin
            // The node has a DG connection already, can't have two just drop it
          end
        end;
    MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME       :
        begin
          if InprocessDatagram <> nil then
          begin
            OPStack_CopyDataArrayWithDestOffset(InprocessDatagram^.Buffer, @OPStackMessage^.Buffer^.DataArray, PDatagramBuffer( InprocessDatagram^.Buffer)^.CurrentCount, OPStackMessage^.Buffer^.DataBufferSize);
            PDatagramBuffer( InprocessDatagram^.Buffer)^.CurrentCount := PDatagramBuffer( InprocessDatagram^.Buffer)^.CurrentCount + OPStackMessage^.Buffer^.DataBufferSize;
          end else
          begin                                                                 // Problem out of order
            OPStack_LoadDatagramRejectedBuffer(OPStackMessage^.Dest.AliasID, OPStackMessage^.Dest.ID, OPStackMessage^.Source.AliasID, OPStackMessage^.Source.ID, @DATAGRAM_RESULT_REJECTED_OUT_OF_ORDER)
          end;
        end;
    MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME_END   :
        begin
          if InprocessDatagram <> nil then
          begin
            OPStack_CopyDataArrayWithDestOffset(InprocessDatagram^.Buffer, @OPStackMessage^.Buffer^.DataArray, PDatagramBuffer( InprocessDatagram^.Buffer)^.CurrentCount, OPStackMessage^.Buffer^.DataBufferSize);
            PDatagramBuffer( InprocessDatagram^.Buffer)^.CurrentCount := 0;      // Wooh Hoo, we are done
            case InprocessDatagram^.Buffer^.DataArray[0] of
              DATAGRAM_TYPE_BOOTLOADER,
              DATAGRAM_TYPE_MEMORY_CONFIGURATION,
              DATAGRAM_TYPE_TRAIN_CONTROL :
                  begin                                                         // Allocate a message for a full MTI_DATRGRAM and return the pointer to the message
                    OPStackCANStatemachine_RemoveOutgoingDatagramMessage(InprocessDatagram);                 // Pull it out of the working stack
                    Result := InprocessDatagram;                                // Send it back to be dispatched
                  end
            else begin                                                        // Unknown Datagram Type
                OPStack_LoadDatagramRejectedBuffer(OPStackMessage^.Dest.AliasID, OPStackMessage^.Dest.ID, OPStackMessage^.Source.AliasID, OPStackMessage^.Source.ID, @DATAGRAM_RESULT_REJECTED_SOURCE_DATAGRAMS_NOT_ACCEPTED)
              end;
            end
          end else
          begin                                                                 // Problem out of order
            OPStack_LoadDatagramRejectedBuffer(OPStackMessage^.Dest.AliasID, OPStackMessage^.Dest.ID, OPStackMessage^.Source.AliasID, OPStackMessage^.Source.ID, @DATAGRAM_RESULT_REJECTED_OUT_OF_ORDER)
          end;
        end;
    end; {case}
end;

// *****************************************************************************
//  procedure OPStackCANStatemachine_ProcessIncomingStreamMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackCANStatemachine_ProcessIncomingStreamMessage(OPStackMessage: POPStackMessage): POPStackMessage;
begin

end;

procedure OPStackCANStatemachine_AddOutgoingDatagramMessage(OPStackDatagramMessage: POPStackMessage);
begin
  AddInprocessMessage(OPStackDatagramMessage, DatagramOutgoingProcessStack);
end;

procedure OPStackCANStatemachine_RemoveOutgoingDatagramMessage(OPStackDatagramMessage: POPStackMessage);
begin
  RemoveInprocessMessage(OPStackDatagramMessage, DatagramOutgoingProcessStack);
end;

procedure OPStackCANStatemachine_ProcessOutgoingDatagramMessage;
var
  AMessage: TOPStackMessage;
  OutgoingMessagePtr: POPStackMessage;
  ABuffer: TSimpleBuffer;
  DatagramBuffer: PDatagramBuffer;
  MTI: Word;
  i: Integer;
begin
  OutgoingMessagePtr := DatagramOutgoingProcessStack;
  if IsOutgoingBufferAvailable then
    if OutgoingMessagePtr <> nil then                                 // We just work this stack from the top down, for now
    begin
      DatagramBuffer := PDatagramBuffer( OutgoingMessagePtr^.Buffer);
      if OutgoingMessagePtr^.Buffer^.DataBufferSize <= 8 then
      begin
        OPStack_LoadBaseMessageBuffer(@AMessage, MT_CAN_TYPE, MTI_FRAME_TYPE_CAN_DATAGRAM_ONLY_FRAME, nil, OutgoingMessagePtr^.Source.AliasID, OutgoingMessagePtr^.Source.ID, OutgoingMessagePtr^.Dest.AliasID, OutgoingMessagePtr^.Dest.ID);
        AMessage.Buffer := @ABuffer;
        OPStack_CopyDataArray(@ABuffer, @DatagramBuffer^.DataArray, OutgoingMessagePtr^.Buffer^.DataBufferSize);
        OPStackCANStatemachine_RemoveOutgoingDatagramMessage(OutgoingMessagePtr);
        OutgoingMessage(@AMessage);
      end else
      if PDatagramBuffer( OutgoingMessagePtr^.Buffer)^.CurrentCount = 0 then
        MTI := MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME_START
      else
      if OutgoingMessagePtr^.Buffer^.DataBufferSize - PDatagramBuffer( OutgoingMessagePtr^.Buffer)^.CurrentCount > 8 then
        MTI := MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME
      else begin
        MTI := MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME_END;
        RemoveInprocessMessage(DatagramOutgoingProcessStack, OutgoingMessagePtr);      // Done sending
        AddInprocessMessage(DatagramOutgoingProcessStack, DatagramWaitingforACKStack);           // Waiting for a reply
      end;

      i := 0;
      while DatagramBuffer^.CurrentCount < DatagramBuffer^.DataBufferSize do
      begin
        DatagramBuffer^.DataArray[i] := DatagramBuffer^.DataArray[DatagramBuffer^.CurrentCount];
        Inc(DatagramBuffer^.CurrentCount);
        OutgoingMessage(@AMessage);
        if i = 8 then
          Exit;
      end;
  end;
end;

procedure OPStackCANStatemachine_AddOutgoingStreamMessage(OPStackStreamMessage: POPStackMessage);
begin
  AddInprocessMessage(OPStackStreamMessage, StreamOutgoingProcessStack);
end;

procedure OPStackCANStatemachine_RemoveOutgoingStreamMessage(OPStackStreamMessage: POPStackMessage);
begin
  RemoveInprocessMessage(OPStackStreamMessage, StreamOutgoingProcessStack);
end;

procedure OPStackCANStatemachine_ProcessOutgoingStreamMessage;
begin
  if DatagramOutgoingProcessStack <> nil then
  begin

  end;
end;

procedure OPStackCANStatemachine_RemoveDatagramWaitingforACKStack(OPStackStreamMessage: POPStackMessage);
begin
  RemoveInprocessMessage(OPStackStreamMessage, DatagramWaitingforACKStack);
end;

function OPStackCANStatemachine_FindDatagramWaitingforACKStack(var SourceNodeID: TNodeInfo; var DestNodeID: TNodeInfo): POPStackMessage;
var
  Next: POPStackMessage;
begin
  Next := DatagramWaitingforACKStack;
  while Next <> nil do
  begin
    if NMRAnetUtilities_EqualNodeIDInfo(SourceNodeID, Next^.Source) then
      if NMRAnetUtilities_EqualNodeIDInfo(DestNodeID, Next^.Dest) then
      begin
        Result := Next;
        Exit;
      end;
    Next := DatagramWaitingforACKStack^.Next;
  end;
end;


{   SNIP STATEMACHINE  }


 { DATAGRAM }

{


}

{
   ConfigMemBuffer := nil;
  if NMRABusTxBufferAvailable then
      if NMRAnetBufferPools_AllocateConfigMemBuffer(ConfigMemBuffer) then
      begin
        i := 0;
        while (BaseBuffer^.StateMachine <> STATE_ACDI_DONE) and (i < 6) do   // All messages have the Destination Alias as the first 2 bytes so only 6 left to use
        begin
          case BaseBuffer^.StateMachine of
            STATE_ACDI_MFG_VERSION  :
                begin      {$IFDEF TRACE_SNIP}UART1_Write_Text('STATE_ACDI_MFG_VERSION'+LF); {$ENDIF}
                  DataBytes[i] := ACDI_MFG_VERSION;
                  Inc(i);
                  BaseBuffer^.Tag := 0;
                  BaseBuffer^.StateMachine := STATE_ACDI_MFG_INFO;
                end;
            STATE_ACDI_MFG_INFO :
                begin  {$IFDEF TRACE_SNIP}UART1_Write_Text('STATE_ACDI_MFG_INFO'+LF); {$ENDIF}
                  {$IFDEF SUPPORT_VIRTUAL_NODES}
                  if Node^.State and NS_VIRTUAL <> 0 then
                  begin
                    if BaseBuffer^.Tag < MAX_ACDI_MFG_ARRAY_VNODE then
                    begin
                      DataBytes[i] := ACDI_MFG_STRINGS_VNODE[BaseBuffer^.Tag];
                      Inc(BaseBuffer^.Tag);
                      Inc(i);
                    end else
                      BaseBuffer^.StateMachine := STATE_ACDI_USER_VERSION;
                  end else {$ENDIF}
                  begin
                    if BaseBuffer^.Tag < MAX_ACDI_MFG_ARRAY then
                    begin
                      DataBytes[i] := ACDI_MFG_STRINGS[BaseBuffer^.Tag];
                      Inc(BaseBuffer^.Tag);
                      Inc(i);
                    end else
                      BaseBuffer^.StateMachine := STATE_ACDI_USER_VERSION;
                  end;
                end;
            STATE_ACDI_USER_VERSION :
                begin    {$IFDEF TRACE_SNIP}UART1_Write_Text('STATE_ACDI_USER_VERSION'+LF); {$ENDIF}
                  DataBytes[i] := ACDI_USER_VERSION;
                  Inc(i);
                  BaseBuffer^.StateMachine := STATE_ACDI_USER_NAME;
                  BaseBuffer^.Tag := 1;  // EEPROM layout start at offset 1
                end;
            STATE_ACDI_USER_NAME :
                begin      {$IFDEF TRACE_SNIP}UART1_Write_Text('STATE_ACDI_USER_NAME'+LF); {$ENDIF}
                  if BaseBuffer^.Tag < MAX_USER_NAME then
                  begin
                    // Very wasteful and slow 1 at a time but it is easy
                    ConfigMemBuffer^.Address := BaseBuffer^.Tag;
                    ConfigMemBuffer^.DataCount := 1;
                    ConfigMemBuffer^.AddressSpace := MSI_CONFIG;
                    AppCallback_Configuration_Read(Node, ConfigMemBuffer);
                    DataBytes[i] := ConfigMemBuffer^.DataBytes[0];

                    if DataBytes[i] = #0 then
                      BaseBuffer^.StateMachine := STATE_ACDI_START_DESC
                    else
                    if BaseBuffer^.Tag = MAX_USER_NAME - 1 then
                      DataBytes[i] := #0;
                    Inc(i);
                    Inc(BaseBuffer^.Tag);
                  end else
                     BaseBuffer^.StateMachine := STATE_ACDI_START_DESC;
                end;
            STATE_ACDI_START_DESC :
                begin      {$IFDEF TRACE_SNIP}UART1_Write_Text('STATE_ACDI_START_DESC'+LF); {$ENDIF}
                  BaseBuffer^.Tag := MAX_USER_NAME + 1;  // EEPROM layout start at offset 1
                  BaseBuffer^.StateMachine := STATE_ACDI_USER_DESC;
                end;
            STATE_ACDI_USER_DESC :
                begin     {$IFDEF TRACE_SNIP}UART1_Write_Text('STATE_ACDI_USER_DESC'+LF); {$ENDIF}
                  if BaseBuffer^.Tag < MAX_USER_CONFIG_DATA then
                  begin
                    // Very wasteful and slow 1 at a time but it is easy
                    ConfigMemBuffer^.Address := BaseBuffer^.Tag;
                    ConfigMemBuffer^.DataCount := 1;
                    AppCallback_Configuration_Read(Node, ConfigMemBuffer);
                    DataBytes[i] := ConfigMemBuffer^.DataBytes[0];

                    if DataBytes[i] = #0 then
                      BaseBuffer^.StateMachine := STATE_ACDI_DONE
                    else
                    if BaseBuffer^.Tag = MAX_USER_CONFIG_DATA - 1 then
                      DataBytes[i] := #0;
                    Inc(i);
                    Inc(BaseBuffer^.Tag);
                  end else
                     BaseBuffer^.StateMachine := STATE_ACDI_DONE;
                end;
            STATE_ACDI_DONE :
                begin      {$IFDEF TRACE_SNIP}UART1_Write_Text('STATE_ACDI_DONE'+LF); {$ENDIF}
                  {$IFDEF ETHERNET_BUS}
                  CAN_Engine.TransmitImmediately := True;
                  {$ENDIF}
                end;
          end;
        end;

      if i > 0 then
        TransmitNMRABusLayerMsg(Node, CANBuffer, MTI_SIMPLE_NODE_INFO_REPLY, BaseBuffer^.Alias, i, @DataBytes, False, $00);

      if BaseBuffer^.StateMachine >= STATE_ACDI_DONE then
      begin
        NMRAnetUtilities_BaseBufferUnLink(Node, BaseBuffer);
        NMRAnetBufferPools_ReleaseBaseBuffer(BaseBuffer);
      end;

      NMRAnetBufferPools_ReleaseConfigMemBuffer(ConfigMemBuffer);
    end   }

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
              StreamBuffer^.StateMachine := STATE_STREAM_INITIATE_REQEUST_WAIT_FOR_REPLY;  // Now let the ProcessMessages decide how to reply to the other node
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

