unit opstackcanstatemachines;

//TODO:  The FirstInProcess functions will always find the first one in the list regardless of the order it was put in.  Need to make this a indexed list


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
//  procedure ProcessIncomingDatagramMessage;
//    Parameters: OPStackMessage - Message incoming
//    Result:     ErrorCode -
//                Result - DATAGRAM_PROCESS_ERROR_xxxx constant
//    Description:
// *****************************************************************************
function ProcessIncomingDatagramMessage(OPStackMessage: POPStackMessage; var DatagramMessage: POPStackMessage): Byte;
var
  InProcessMessage: POPStackmessage;
begin
  Result := DATAGRAM_PROCESS_ERROR_QUIET_FAIL;
  DatagramMessage := nil;
  InProcessMessage := OPStackCANStatemachineBuffers_FindMessageOnIncomingDatagramStack(OPStackMessage);
  case OPStackMessage^.MTI of
    MTI_FRAME_TYPE_CAN_DATAGRAM_ONLY_FRAME :
        begin
          if InProcessMessage = nil then
          begin
            case OPStackMessage^.Buffer^.DataArray[0] of
      //        DATAGRAM_TYPE_BOOTLOADER,
              DATAGRAM_TYPE_MEMORY_CONFIGURATION,
              DATAGRAM_TYPE_TRAIN_CONTROL :
                  begin                                                         // Allocate a message for a full MTI_DATRGRAM and return the pointer to the message
                    if OPStackBuffers_AllocateDatagramMessage(InProcessMessage, OPStackMessage^.Source.AliasID, OPStackMessage^.Source.ID, OPStackMessage^.Dest.AliasID, OPStackMessage^.Dest.ID, 0) then
                    begin
                      InProcessMessage^.Buffer^.DataBufferSize := OPStackMessage^.Buffer^.DataBufferSize;
                      OPStackBuffers_CopyData(InProcessMessage^.Buffer, OPStackMessage^.Buffer);
                      PDatagramBuffer( PByte( InProcessMessage^.Buffer))^.CurrentCount := 0;
                      DatagramMessage := InProcessMessage;
                      Result := DATAGRAM_PROCESS_ERROR_OK;
                      Exit;
                    end else                                                    // No Buffer available, try again
                      Result := DATAGRAM_PROCESS_ERROR_BUFFER_FULL;
                  end
            else
              Result := DATAGRAM_PROCESS_ERROR_SOURCE_NOT_ACCEPTED;             // Unknowns Datagram type
            end;
          end                                                                   // The node has a DG connection already, can't have two just drop it
        end;
    MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME_START :
        begin
          if InProcessMessage = nil then
          begin
            if OPStackBuffers_AllocateDatagramMessage(InProcessMessage, OPStackMessage^.Source.AliasID, OPStackMessage^.Source.ID, OPStackMessage^.Dest.AliasID, OPStackMessage^.Dest.ID, 0) then
            begin
              InProcessMessage^.Buffer^.DataBufferSize := OPStackMessage^.Buffer^.DataBufferSize;
              OPStackBuffers_CopyData(InProcessMessage^.Buffer, OPStackMessage^.Buffer);
              PDatagramBuffer( PByte( InProcessMessage^.Buffer))^.CurrentCount := OPStackMessage^.Buffer^.DataBufferSize;
              OPStackCANStatemachineBuffers_AddIncomingDatagramMessage(InProcessMessage);
              Exit
            end                                                                 // No Buffer available, try again  Don't agree with this but Python test fails for overlapped datagram if I return th
          end                                                                   // The node has a DG connection already, can't have two just drop it
        end;
    MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME       :
        begin
          if InProcessMessage <> nil then
          begin
            OPStackBuffers_CopyDataArrayWithDestOffset(InProcessMessage^.Buffer, @OPStackMessage^.Buffer^.DataArray, OPStackMessage^.Buffer^.DataBufferSize, False);
            PDatagramBuffer( PByte( InProcessMessage^.Buffer))^.CurrentCount := PDatagramBuffer( PByte( InProcessMessage^.Buffer))^.CurrentCount + OPStackMessage^.Buffer^.DataBufferSize;
            Exit;
          end
        end;
    MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME_END   :
        begin
          if InProcessMessage <> nil then
          begin
            OPStackBuffers_CopyDataArrayWithDestOffset(InProcessMessage^.Buffer, @OPStackMessage^.Buffer^.DataArray, OPStackMessage^.Buffer^.DataBufferSize, False);
            PDatagramBuffer( PByte( InProcessMessage^.Buffer))^.CurrentCount := 0;      // Wooh Hoo, we are done
            OPStackCANStatemachineBuffers_RemoveIncomingDatagramMessage(InProcessMessage);   // Pull it out of the working stack
            DatagramMessage := InProcessMessage;                                        // Return the completed message
            case InProcessMessage^.Buffer^.DataArray[0] of
     //         DATAGRAM_TYPE_BOOTLOADER,
              DATAGRAM_TYPE_MEMORY_CONFIGURATION,
              DATAGRAM_TYPE_TRAIN_CONTROL :  Result := DATAGRAM_PROCESS_ERROR_OK // Send it back to be dispatched
            else
              Result := DATAGRAM_PROCESS_ERROR_SOURCE_NOT_ACCEPTED;              // Unknown Datagram type
            end
          end else
            Result := DATAGRAM_PROCESS_ERROR_OUT_OF_ORDER;
        end;
    end; {case}
end;

// *****************************************************************************
//  procedure ProcessOutgoingDatagramMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure ProcessOutgoingDatagramMessage;
var
  NewMessage: TOPStackMessage;
  NewBuffer: TSimpleBuffer;
  LocalOutgoingMessage: POPStackMessage;
  DatagramBuffer: PDatagramBuffer;
  MTI: Word;
begin
  if IsOutgoingBufferAvailable then
  begin
    LocalOutgoingMessage := OPStackCANStatemachineBuffers_FirstMessageOnOutgoingDatagramStack(0);
    if LocalOutgoingMessage <> nil then                                 // We just work this stack from the top down, for now
    begin
      DatagramBuffer := PDatagramBuffer( PByte( LocalOutgoingMessage^.Buffer));
      OPStackBuffers_ZeroMessage(@NewMessage);
      OPStackBuffers_ZeroSimpleBuffer(@NewBuffer, False);
      if LocalOutgoingMessage^.Buffer^.DataBufferSize <= 8 then
      begin
        OPStackBuffers_LoadMessage(@NewMessage, MTI_FRAME_TYPE_CAN_DATAGRAM_ONLY_FRAME, LocalOutgoingMessage^.Source.AliasID, LocalOutgoingMessage^.Source.ID, LocalOutgoingMessage^.Dest.AliasID, LocalOutgoingMessage^.Dest.ID, 0);
        NewMessage.MessageType := MT_SIMPLE;
        NewMessage.Buffer := @NewBuffer;
        OPStackBuffers_CopyDataArray(@NewBuffer, @DatagramBuffer^.DataArray, LocalOutgoingMessage^.Buffer^.DataBufferSize, True);
        NewMessage.Buffer^.DataBufferSize := LocalOutgoingMessage^.Buffer^.DataBufferSize;
        OPStackCANStatemachineBuffers_RemoveOutgoingDatagramMessage(LocalOutgoingMessage);
        OutgoingMessage(@NewMessage);
        Exit;
      end else
      if PDatagramBuffer( PByte( LocalOutgoingMessage^.Buffer))^.CurrentCount = 0 then
        MTI := MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME_START
      else
      if LocalOutgoingMessage^.Buffer^.DataBufferSize - PDatagramBuffer( PByte( LocalOutgoingMessage^.Buffer))^.CurrentCount > 8 then
        MTI := MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME
      else begin
        MTI := MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME_END;
        OPStackCANStatemachineBuffers_RemoveOutgoingDatagramMessage(LocalOutgoingMessage);
      end;

      OPStackBuffers_LoadMessage(@NewMessage, MTI, LocalOutgoingMessage^.Source.AliasID, LocalOutgoingMessage^.Source.ID, LocalOutgoingMessage^.Dest.AliasID, LocalOutgoingMessage^.Dest.ID, 0);
      OPStackBuffers_ZeroSimpleBuffer(@NewBuffer, False);
      NewMessage.MessageType := MT_SIMPLE;
      NewMessage.Buffer := @NewBuffer;
      while DatagramBuffer^.CurrentCount < DatagramBuffer^.DataBufferSize do
      begin
        NewMessage.Buffer^.DataArray[NewBuffer.DataBufferSize] := DatagramBuffer^.DataArray[DatagramBuffer^.CurrentCount];
        Inc(DatagramBuffer^.CurrentCount);
        Inc(NewBuffer.DataBufferSize);
        if NewBuffer.DataBufferSize = 8 then
          Break;
      end;
      OutgoingMessage(@NewMessage);
    end;
  end;
end;

// *****************************************************************************
//  procedure ProcessOutgoingAcdiSnipMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure ProcessOutgoingAcdiSnipMessage;
var
  LocalMessage: TOPStackMessage;
  LocalOutgoingMessagePtr: POPStackMessage;
  LocalBuffer: TSimpleBuffer;
  AcdiSnipBufferPtr: PAcdiSnipBuffer;
begin
  LocalOutgoingMessagePtr := OPStackCANStatemachineBuffers_FirstMessageOnOutgoingAcdiSnipStack(0);
  if IsOutgoingBufferAvailable then
    if LocalOutgoingMessagePtr <> nil then
    begin
      AcdiSnipBufferPtr := PAcdiSnipBuffer( PByte( LocalOutgoingMessagePtr^.Buffer));
      OPStackBuffers_LoadMessage(@LocalMessage, MTI_SIMPLE_NODE_INFO_REPLY, LocalOutgoingMessagePtr^.Source.AliasID, LocalOutgoingMessagePtr^.Source.ID, LocalOutgoingMessagePtr^.Dest.AliasID, LocalOutgoingMessagePtr^.Dest.ID, 0);
      OPStackBuffers_ZeroSimpleBuffer(@LocalBuffer, False);
      LocalMessage.MessageType := MT_SIMPLE;
      LocalMessage.Buffer := @LocalBuffer;
      LocalBuffer.DataBufferSize := 0;
      while AcdiSnipBufferPtr^.CurrentCount < AcdiSnipBufferPtr^.DataBufferSize do
      begin
        LocalBuffer.DataArray[LocalBuffer.DataBufferSize] := AcdiSnipBufferPtr^.DataArray[AcdiSnipBufferPtr^.CurrentCount];
        Inc(LocalBuffer.DataBufferSize );
        Inc(AcdiSnipBufferPtr^.CurrentCount);
        if LocalBuffer.DataBufferSize = 6 then
          Break;
      end;
      OutgoingMessage(@LocalMessage);

      if AcdiSnipBufferPtr^.CurrentCount >= AcdiSnipBufferPtr^.DataBufferSize then
      begin
        OPStackCANStatemachineBuffers_RemoveAcdiSnipMessage(LocalOutgoingMessagePtr);
        OPStackBuffers_DeAllocateMessage(LocalOutgoingMessagePtr);
      end;
    end;
end;

// *****************************************************************************
//  procedure FlushCanSpecificBuffers;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure FlushCanSpecificBuffers(SourceAliasID: Word);
begin
end;

{$IFDEF SUPPORT_STREAMS}
// *****************************************************************************
//  procedure ProcessOutgoingStreamMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure ProcessOutgoingStreamMessage;
var
  LocalOutgoingMessage: POPStackMessage;
begin
  LocalOutgoingMessage := OPStackCANStatemachineBuffers_FirstMessageOnOutgoingStreamStack(0);
  if LocalOutgoingMessage <> nil then                                           // We just work this stack from the top down, for now
    if IsOutgoingBufferAvailable then
    begin

    end;
end;

{$ENDIF}

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
        MTI_CAN_AMR :  FlushCanSpecificBuffers(OPStackMessage^.Source.AliasID);
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
              DatagramProcessErrorCode := ProcessIncomingDatagramMessage(@OPStackMessage^, DatagramMessage);
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

     (*

  CanMessageType := (NMRAnetCanBuffer.MTI shr 12) and $F000;                    // always true regardless
  SourceAlias := NMRAnetCanBuffer.MTI and $00000FFF;                            // always true regardless
  SourceNode := OPStackNode_FindByAlias(SourceAlias);                           // If this is set then we have a potential duplicate alias
  if NMRAnetCanBuffer.MTI and MTI_OLCB_MSG = 0 then                             // Is it a CAN message?
  begin
    OPStackMessage.MessageType := MT_SIMPLE or MT_CAN_TYPE;                     // Created on the heap not the pool
    RawMTI := (NMRAnetCanBuffer.MTI shr 12);
    if RawMTI and $F000 > 0 then
      RawMTI := RawMTI and $F000;
    OPStackBuffers_LoadMessage(@OPStackMessage, RawMTI, SourceAlias, NULL_NODE_ID, 0, NULL_NODE_ID, 0);
    OPStackBuffers_LoadSimpleBuffer(OPStackMessage.Buffer, 0, NMRAnetCanBuffer.PayloadCount, @NMRAnetCanBuffer.Payload, 0);
    case RawMTI of
        MTI_CAN_AMD,       // If another node has reset then we need to clear out any inprocess states with that node
        MTI_CAN_AMR :
            begin
              DatagramMessage := OPStackCANStatemachineBuffers_FindAnyDatagramOnOutgoingStack(OPStackMessage.Source.AliasID);
              while DatagramMessage <> nil do
              begin
                OPStackCANStatemachineBuffers_RemoveOutgoingDatagramMessage(DatagramMessage);
                OPStackBuffers_DeAllocateMessage(DatagramMessage);
                DatagramMessage := OPStackCANStatemachineBuffers_FindAnyDatagramOnOutgoingStack(OPStackMessage.Source.AliasID);
              end;

              DatagramMessage := OPStackCANStatemachineBuffers_FindAnyDatagramOnIncomingStack(OPStackMessage.Source.AliasID);
              while DatagramMessage <> nil do
              begin
                OPStackCANStatemachineBuffers_RemoveIncomingDatagramMessage(DatagramMessage);
                OPStackBuffers_DeAllocateMessage(DatagramMessage);
                DatagramMessage := OPStackCANStatemachineBuffers_FindAnyDatagramOnIncomingStack(OPStackMessage.Source.AliasID);
              end;

              AcdiSnipMessage := OPStackCANStatemachineBuffers_FindAnyAcdiSnipOnOutgoingStack(OPStackMessage.Source.AliasID);
              while AcdiSnipMessage <> nil do
              begin
                OPStackCANStatemachineBuffers_RemoveAcdiSnipMessage(AcdiSnipMessage);
                OPStackBuffers_DeAllocateMessage(AcdiSnipMessage);
                AcdiSnipMessage := OPStackCANStatemachineBuffers_FindAnyAcdiSnipOnOutgoingStack(OPStackMessage.Source.AliasID);
              end;
            end;
    end;
    IncomingMessageDispatch(@OPStackMessage, nil, SourceNode);
  end else
  begin
    OPStackMessage.MessageType := MT_SIMPLE;
    case CanMessageType of
      MTI_FRAME_TYPE_CAN_GENERAL :
          begin
            RawMTI := (NMRAnetCanBuffer.MTI shr 12) and $0FFF;
            if RawMTI and MTI_ADDRESSED_MASK = MTI_ADDRESSED_MASK then
            begin                                                               // It is an addressed message, need to call IncomingMessage always to test for duplicate Alias in the Source
              DestAlias := ((NMRAnetCanBuffer.Payload[0] shl 8) or NMRAnetCanBuffer.Payload[1]) and $0FFF;
              DestinationNode := OPStackNode_FindByAlias(DestAlias);
              OPStackBuffers_LoadMessage(@OPStackMessage, RawMTI, SourceAlias, NULL_NODE_ID, DestAlias, NULL_NODE_ID, NMRAnetCanBuffer.Payload[0] and $F0);
              OPStackBuffers_LoadSimpleBuffer(OPStackMessage.Buffer, 0, NMRAnetCanBuffer.PayloadCount-2, @NMRAnetCanBuffer.Payload, 2);
              IncomingMessageDispatch(@OPStackMessage, DestinationNode, SourceNode);
            end else
            begin                                                                 // It was not an addressed message
              OPStackBuffers_LoadMessage(@OPStackMessage, RawMTI, SourceAlias, NULL_NODE_ID, 0, NULL_NODE_ID, 0);
              OPStackBuffers_LoadSimpleBuffer(OPStackMessage.Buffer, 0, NMRAnetCanBuffer.PayloadCount, @NMRAnetCanBuffer.Payload, 0);
              IncomingMessageDispatch(@OPStackMessage, nil, SourceNode);
            end;
          end;
      MTI_FRAME_TYPE_CAN_DATAGRAM_ONLY_FRAME,
      MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME_START,
      MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME,
      MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME_END :
          begin
            DestAlias := (NMRAnetCanBuffer.MTI shr 12) and $0FFF;
            DestinationNode := OPStackNode_FindByAlias(DestAlias);
            if DestinationNode <> nil then
            begin
              OPStackBuffers_LoadMessage(@OPStackMessage, CanMessageType, SourceAlias, NULL_NODE_ID, DestAlias, NULL_NODE_ID, 0);   // These messages do not have real "MTIs", they are based on the identifier
              OPStackBuffers_LoadSimpleBuffer(OPStackMessage.Buffer, 0, NMRAnetCanBuffer.PayloadCount, @NMRAnetCanBuffer.Payload, 0);
              DatagramProcessErrorCode := ProcessIncomingDatagramMessage(@OPStackMessage, DatagramMessage);
              case DatagramProcessErrorCode of
                DATAGRAM_PROCESS_ERROR_OK                  :
                    begin
                      if DatagramMessage <> nil then
                        IncomingMessageDispatch(DatagramMessage, DestinationNode, SourceNode);
                      Exit;
                    end
              else begin
                  if DatagramMessage <> nil then
                    OPStackBuffers_DeAllocateMessage(DatagramMessage);
                  case DatagramProcessErrorCode of
                    DATAGRAM_PROCESS_ERROR_BUFFER_FULL         : DatagramError := @DATAGRAM_RESULT_REJECTED_BUFFER_FULL;
                    DATAGRAM_PROCESS_ERROR_OUT_OF_ORDER        : DatagramError := @DATAGRAM_RESULT_REJECTED_OUT_OF_ORDER;
                    DATAGRAM_PROCESS_ERROR_SOURCE_NOT_ACCEPTED : DatagramError := @DATAGRAM_RESULT_REJECTED_SOURCE_DATAGRAMS_NOT_ACCEPTED;
                  end;
                  DatagramRejectedMessage.Buffer := PSimpleBuffer( PByte( @DatagramRejectedBuffer));
                  OPStackBuffers_LoadDatagramRejectedBuffer(@DatagramRejectedMessage, OPStackMessage.Dest.AliasID, OPStackMessage.Dest.ID, OPStackMessage.Source.AliasID, OPStackMessage.Source.ID, DatagramError);
                  OutgoingCriticalMessage(@DatagramRejectedMessage);
                  Exit;
                end;
              end;
            end else
            begin                                                               // It is an addressed message, need to call IncomingMessage always to test for duplicate Alias in the Source
              OPStackBuffers_LoadMessage(@OPStackMessage, CanMessageType, SourceAlias, NULL_NODE_ID, 0, NULL_NODE_ID, 0);
              OPStackBuffers_LoadSimpleBuffer(OPStackMessage.Buffer, 0, NMRAnetCanBuffer.PayloadCount, @NMRAnetCanBuffer.Payload, 0);
              IncomingMessageDispatch(@OPStackMessage, nil, SourceNode);
            end;
          end;
      MTI_FRAME_TYPE_CAN_STREAM_SEND :
          begin

          end;
    end;
  end;     *)
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
  ProcessOutgoingDatagramMessage;
  ProcessOutgoingAcdiSnipMessage;
  ProcessOutgoingStreamMessage;
end;


{   SNIP STATEMACHINE  }


 { DATAGRAM }

{


}

(*
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
    end   *)

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
