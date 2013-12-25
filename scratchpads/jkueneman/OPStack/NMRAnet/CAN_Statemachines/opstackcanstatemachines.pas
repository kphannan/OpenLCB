unit opstackcanstatemachines;

// TODO: FlushCanSpecificBuffers not tested or know if it work right


{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  {$IFDEF FPC}
  Classes, SysUtils,
  {$ENDIF}
  hardware_template,
  nmranetdefines,
  opstackdefines,
  opstackbuffers,
  opstacktypes,
  opstackcanstatemachinesstream,
  opstackcanstatemachinessnip,
  opstackcanstatemachinesdatagram,
  opstackcanstatemachinesbuffers;

function OPStackCANStatemachine_NMRAnetCanBufferToOPStackBuffer(var NMRAnetCanBuffer: TNMRAnetCanBuffer; var OPStackMessage: POPStackMessage; var DestNode: PNMRAnetNode; var SourceNode: PNMRAnetNode): Boolean;
procedure OPStackCANStatemachine_OPStackMessageToNMRAnetCanBuffer(AMessage: POPStackMessage; NMRAnetCanBuffer: PNMRAnetCanBuffer);
procedure OPStackCANStatemachine_ProcessMessages;

implementation

{$IFDEF FPC}
uses
  opstacknode;
{$ENDIF}

// *****************************************************************************
//  procedure FlushCanSpecificBuffers;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure FlushCanSpecificBuffers(SourceAliasID: Word);
var
  AMessage: POPStackMessage;
begin
  AMessage := OPStackCANStatemachineBuffers_FindAnyDatagramOnOutgoingStack(SourceAliasID);
  while AMessage <> nil do
  begin
    OPStackCANStatemachineBuffers_RemoveOutgoingDatagramMessage(AMessage);
    OPStackBuffers_DeAllocateMessage(AMessage);
    AMessage := OPStackCANStatemachineBuffers_FindAnyDatagramOnOutgoingStack(SourceAliasID);
  end;

  AMessage := OPStackCANStatemachineBuffers_FindAnyDatagramOnIncomingStack(SourceAliasID);
  while AMessage <> nil do
  begin
    OPStackCANStatemachineBuffers_RemoveIncomingDatagramMessage(AMessage);
    OPStackBuffers_DeAllocateMessage(AMessage);
    AMessage := OPStackCANStatemachineBuffers_FindAnyDatagramOnIncomingStack(SourceAliasID);
  end;

  AMessage := OPStackCANStatemachineBuffers_FindAnyAcdiSnipOnOutgoingStack(SourceAliasID);
  while AMessage <> nil do
  begin
    OPStackCANStatemachineBuffers_RemoveAcdiSnipMessage(AMessage);
    OPStackBuffers_DeAllocateMessage(AMessage);
    AMessage := OPStackCANStatemachineBuffers_FindAnyAcdiSnipOnOutgoingStack(SourceAliasID);
  end;
end;

// *****************************************************************************
//  procedure OPStackCANStatemachine_NMRAnetCanBufferToOPStackBuffer;
//    Parameters:
//    Result:
//    Description: OPStackMessage MUST be zeroized before calling this function
// *****************************************************************************
function OPStackCANStatemachine_NMRAnetCanBufferToOPStackBuffer(var NMRAnetCanBuffer: TNMRAnetCanBuffer; var OPStackMessage: POPStackMessage; var DestNode: PNMRAnetNode; var SourceNode: PNMRAnetNode): Boolean;
var
  DatagramMessage: POPStackMessage;
  DatagramError: PSimpleDataArray;
  DatagramProcessErrorCode: Word;
begin
  Result := False;
  OPStackMessage^.Source.AliasID := NMRAnetCanBuffer.MTI and $00000FFF;          // All Source Aliases are the same
  SourceNode := OPStackNode_FindByAlias(OPStackMessage^.Source.AliasID);
  DestNode := nil;
  if NMRAnetCanBuffer.MTI and MTI_OLCB_MSG = 0 then
  begin                                                                         // It is a CAN message
    OPStackMessage^.MessageType := MT_SIMPLE or MT_CAN_TYPE;                     // It is not Allocated (on the stack) and is a CAN type message
    OPStackMessage^.MTI := NMRAnetCanBuffer.MTI shr 12;                          // Create the generic MTI
    if OPStackMessage^.MTI and $F000 > 0 then                                    // There are some special ones that are only the upper nibble
       OPStackMessage^.MTI := OPStackMessage^.MTI and $F000;
    OPStackBuffers_LoadSimpleBuffer(OPStackMessage^.Buffer, 0, NMRAnetCanBuffer.PayloadCount, @NMRAnetCanBuffer.Payload, 0);  // Copy over the payload
    case OPStackMessage^.MTI of
        MTI_CAN_AMD,                                                            // If another node has reset then we need to clear out any inprocess states with that node
        MTI_CAN_AMR :  FlushCanSpecificBuffers(OPStackMessage^.Source.AliasID); // This is only an issue with CAN so encapsulate it here in CAN only code
    end;
    Result := True;
  end else
  begin                                                                         // It is not a CAN message
    OPStackMessage^.MessageType := MT_SIMPLE;                                    // It is not Allocated (on the stack)
    OPStackMessage^.MTI := (NMRAnetCanBuffer.MTI shr 12) and $0FFF;              // Create the generic MTI
   case (NMRAnetCanBuffer.MTI shr 12) and $F000 of                              // If we just look at the upper nibble we can classify them as general, datagram, or stream
      MTI_FRAME_TYPE_CAN_GENERAL :
          begin
            if OPStackMessage^.MTI and MTI_ADDRESSED_MASK = MTI_ADDRESSED_MASK then
            begin                                                               // It is an addressed message so pull out the Destination
              OPStackMessage^.Dest.AliasID := ((NMRAnetCanBuffer.Payload[0] shl 8) or NMRAnetCanBuffer.Payload[1]) and $0FFF; // General CAN messages have the Alias in the payload first two bytes  ;
              DestNode := OPStackNode_FindByAlias(OPStackMessage^.Dest.AliasID);
              OPStackMessage^.DestFlags := NMRAnetCanBuffer.Payload[0] and $F0; // The upper nibble of the destination may have special meaning, pull it out
              OPStackBuffers_LoadSimpleBuffer(OPStackMessage^.Buffer, 0, NMRAnetCanBuffer.PayloadCount-2, @NMRAnetCanBuffer.Payload, 2);  // Copy over the payload, skipping the destination alias
              Result := True;
            end else
            begin
              OPStackBuffers_LoadSimpleBuffer(OPStackMessage^.Buffer, 0, NMRAnetCanBuffer.PayloadCount, @NMRAnetCanBuffer.Payload, 0); // Copy over the payload
              Result := True;
            end;
          end;
      MTI_FRAME_TYPE_CAN_DATAGRAM_ONLY_FRAME,
      MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME_START,
      MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME,
      MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME_END :
          begin
            OPStackMessage^.Dest.AliasID := (NMRAnetCanBuffer.MTI shr 12) and $0FFF;
            DestNode := OPStackNode_FindByAlias(OPStackMessage^.Dest.AliasID);
            OPStackMessage^.MTI := (NMRAnetCanBuffer.MTI shr 12) and $F000;
            OPStackBuffers_LoadSimpleBuffer(OPStackMessage^.Buffer, 0, NMRAnetCanBuffer.PayloadCount, @NMRAnetCanBuffer.Payload, 0);
            if DestNode <> nil then
            begin
              DatagramProcessErrorCode := StackCANStatemachineDatagram_ProcessIncomingDatagramMessage(@OPStackMessage^, DatagramMessage);
              case DatagramProcessErrorCode of
                  DATAGRAM_PROCESS_ERROR_OK                      :
                      begin
                        OPStackMessage := DatagramMessage;                        // Replace the passed message with the datagram message
                        Result := True;
                      end;
                  DATAGRAM_PROCESS_ERROR_QUIET_FAIL              :
                      begin
                        Result := False
                      end
                else begin
                    if DatagramMessage <> nil then
                      OPStackBuffers_DeAllocateMessage(DatagramMessage);          // If there is an error need to flush the buffer
                    case DatagramProcessErrorCode of
                      DATAGRAM_PROCESS_ERROR_BUFFER_FULL         : DatagramError := @DATAGRAM_RESULT_REJECTED_BUFFER_FULL;
                      DATAGRAM_PROCESS_ERROR_OUT_OF_ORDER        : DatagramError := @DATAGRAM_RESULT_REJECTED_OUT_OF_ORDER;
                      DATAGRAM_PROCESS_ERROR_SOURCE_NOT_ACCEPTED : DatagramError := @DATAGRAM_RESULT_REJECTED_SOURCE_DATAGRAMS_NOT_ACCEPTED;
                    end;
                    OPStackMessage^.MTI := MTI_DATAGRAM_REJECTED_REPLY;
                    OPStackMessage^.MessageType := OPStackMessage^.MessageType or MT_HIGH_PRIORITY_SEND;
                    OPStackBuffers_CopyDataArray(OPStackMessage^.Buffer, DatagramError, 2, True);
                    Result := True;
                  end;
              end
            end else
              Result := True
          end;
      MTI_FRAME_TYPE_CAN_STREAM_SEND :
          begin

          end;
    end;
  end;
end;

// *****************************************************************************
//  procedure OPStackCANStatemachine_OPStackMessageToNMRAnetCanBuffer;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCANStatemachine_OPStackMessageToNMRAnetCanBuffer(AMessage: POPStackMessage; NMRAnetCanBuffer: PNMRAnetCanBuffer);
var
  i: Integer;
begin
  NMRAnetCanBuffer^.PayloadCount := 0;
  if AMessage^.MessageType and MT_CAN_TYPE <> 0 then
  begin
    // This is a special case CAN message
    case AMessage^.MTI of
      MTI_CAN_CID0 : NMRAnetCanBuffer^.MTI := DWord(AMessage^.MTI shl 12) or (AMessage^.Source.ID[1] and $00FFF000) or AMEssage^.Source.AliasID or $10000000;
      MTI_CAN_CID1 : NMRAnetCanBuffer^.MTI := DWord(AMessage^.MTI shl 12) or ((AMessage^.Source.ID[1] shl 12) and $00FFF000) or AMEssage^.Source.AliasID or  $10000000;
      MTI_CAN_CID2 : NMRAnetCanBuffer^.MTI := DWord(AMessage^.MTI shl 12) or (AMessage^.Source.ID[0] and $00FFF000) or AMEssage^.Source.AliasID or $10000000;
      MTI_CAN_CID3 : NMRAnetCanBuffer^.MTI := DWord(AMessage^.MTI shl 12) or ((AMessage^.Source.ID[0] shl 12) and $00FFF000) or AMEssage^.Source.AliasID or  $10000000
    else
      NMRAnetCanBuffer^.MTI := (AMessage^.MTI shl 12) or AMessage^.Source.AliasID or $10000000;
    end;
  end else
  if AMessage^.MTI and MTI_FRAME_TYPE_MASK <= MTI_FRAME_TYPE_CAN_GENERAL then
  begin
    // This is a general case MTI ($9xxx)
    NMRAnetCanBuffer^.MTI := (AMessage^.MTI shl 12) or AMessage^.Source.AliasID or $19000000;
    if AMessage^.MTI and MTI_ADDRESSED_MASK = MTI_ADDRESSED_MASK then
    begin
      NMRAnetCanBuffer^.Payload[0] := Hi( AMessage^.Dest.AliasID);
      NMRAnetCanBuffer^.Payload[1] := Lo( AMessage^.Dest.AliasID);
      NMRAnetCanBuffer^.PayloadCount := 2;
    end;
  end else
  if AMessage^.MTI and MTI_FRAME_TYPE_MASK <= MTI_FRAME_TYPE_CAN_STREAM_SEND then
  begin
    // This is a datagram MTI ($Axxx...$Dxxxx) or stream MTI ($Fxxx)
    NMRAnetCanBuffer^.MTI := (AMessage^.MTI shl 12) or (AMessage^.Dest.AliasID shl 12) or AMessage^.Source.AliasID or $10000000;
  end;

  for i := 0 to AMessage^.Buffer^.DataBufferSize - 1 do
  begin
    NMRAnetCanBuffer^.Payload[NMRAnetCanBuffer^.PayloadCount] := AMessage^.Buffer^.DataArray[i];
    Inc(NMRAnetCanBuffer^.PayloadCount);
  end;
end;

procedure OPStackCANStatemachine_ProcessMessages;
begin
  StackCANStatemachineDatagram_ProcessOutgoingDatagramMessage;
  OPStackCANStatemachineSnip_ProcessOutgoingAcdiSnipMessage;
  PStackCANStatemachineStream_ProcessOutgoingStreamMessage;
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
