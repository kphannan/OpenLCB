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
  nmranetdefines,
  opstackdefines,
  opstacktypes,
  opstackbuffers,
  opstackcore_basic,
  opstackcanstatemachinesmultiframe,
  {$IFDEF SUPPORT_STREAMS}
  opstackcanstatemachinesstream,
  {$ENDIF}
  opstackcanstatemachinessnip,
  opstackcanstatemachinesdatagram,
  opstackcanstatemachinesbuffers;

function OPStackCANStatemachine_NMRAnetCanBufferToOPStackBuffer(var OPStackMessage: POPStackMessage; var DestNode: PNMRAnetNode; var SourceNode: PNMRAnetNode; NMRAnetCanBuffer: PNMRAnetCanBuffer): Boolean;
procedure OPStackCANStatemachine_OPStackMessageToNMRAnetCanBuffer(AMessage: POPStackMessage; NMRAnetCanBuffer: PNMRAnetCanBuffer);
procedure OPStackCANStatemachine_ProcessMessages;

{$IFNDEF FPC}
function OPStackNode_FindByAlias(AliasID: Word): PNMRAnetNode; external;
{$ENDIF}

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

  {$IFDEF SUPPORT_STREAMS}
  AMessage := OPStackCANStatemachineBuffers_FindAnyStreamOnOutgoingStack(SourceAliasID, -1, -1);
  while AMessage <> nil do
  begin
    OPStackCANStatemachineBuffers_RemoveStreamMessage(AMessage);
    OPStackBuffers_DeAllocateMessage(AMessage);
    AMessage := OPStackCANStatemachineBuffers_FindAnyStreamOnOutgoingStack(SourceAliasID, -1, -1);
  end;
  {$ENDIF}

  AMessage := OPStackCANStatemachineBuffers_FindAnyMultiFrameOnOutgoingStack(SourceAliasID);
  while AMessage <> nil do
  begin
    OPStackCANStatemachineBuffers_RemoveMultiFrameMessage(AMessage);
    OPStackBuffers_DeAllocateMessage(AMessage);
    AMessage := OPStackCANStatemachineBuffers_FindAnyMultiFrameOnOutgoingStack(SourceAliasID);
  end;
end;


procedure NMRAnetCanBufferToOPStackBuffer(NMRAnetCanBuffer: PNMRAnetCanBuffer; OPStackMessage: POPStackMessage; FramingBits: Byte; MTI, DestAlias, SourceAlias: Word);
begin
  OPStackMessage^.FramingBits := FramingBits;
  OPStackMessage^.MessageType := MT_SIMPLE;
  OPStackMessage^.MTI := MTI;
  OPStackMessage^.Dest.ID := NULL_NODE_ID;
  OPStackMessage^.Dest.AliasID := DestAlias;
  OPStackMessage^.Source.ID := NULL_NODE_ID;
  OPStackMessage^.Source.AliasID := SourceAlias;
  OPStackBuffers_CopyDataArray(OPStackMessage^.Buffer, PSimpleDataArray( PByte( @NMRAnetCanBuffer^.Payload)), NMRAnetCanBuffer^.PayloadCount, True);
end;

// *****************************************************************************
//  procedure OPStackCANStatemachine_NMRAnetCanBufferToOPStackBuffer;
//    Parameters: NMRAnetCanBuffer - Buffer to convert to a message buffer
//                OPStackMessage   - Empty message buffer to convert the Buffer into
//                DestNode         - The internal PNMRAnetNode the message is destined for (nil, if the message was not inteded for any of our nodes)
//                SourceNode       - The internal PNMRAnetNode that matches the source node ID (this SHOULD be nil if not then there is a duplicate Alias out there)
//    Result:
//    Description: OPStackMessage MUST be zeroized before calling this function
// *****************************************************************************
function OPStackCANStatemachine_NMRAnetCanBufferToOPStackBuffer(var OPStackMessage: POPStackMessage; var DestNode: PNMRAnetNode; var SourceNode: PNMRAnetNode; NMRAnetCanBuffer: PNMRAnetCanBuffer): Boolean;
var
  ScratchMessage: TOPStackMessage;
  ScratchBuffer: TSimpleBuffer;
  DatagramMessagePtr, InProcessMessagePtr: POPStackMessage;
  DatagramProcessErrorCode: Word;
  MTI: DWord;
  SourceAlias, DestAlias: Word;
  FramingBits: Byte;
  Source: TNodeInfo;
begin
  Result := False;
  InProcessMessagePtr := nil;
  ScratchMessage.Buffer := @ScratchBuffer;
  OPStackMessage := nil;
  SourceAlias := NMRAnetCanBuffer^.MTI and $00000FFF;
  SourceNode := OPStackNode_FindByAlias(SourceAlias);                           // Where the message came from
  DestNode := nil;                                                              // Unknown if the message contains a destination yet
  if NMRAnetCanBuffer^.MTI and MTI_OLCB_MSG = 0 then
  begin
    MTI := (NMRAnetCanBuffer^.MTI shr 12) and $FFFF;
    if MTI and $F000 > 0 then
      MTI := MTI and $F000;                                                     // CID uses the upper nibble as the MTI, the rest use the lower 3 nibbles
    if OPStackBuffers_AllocateOPStackMessage(OPStackMessage, MTI, SourceAlias, NULL_NODE_ID, 0, NULL_NODE_ID, True) then
    begin
      OPStackBuffers_CopyDataArray(OPStackMessage^.Buffer, PSimpleDataArray( PByte( @NMRAnetCanBuffer^.Payload)), NMRAnetCanBuffer^.PayloadCount, True);  // Never has a dest
      case MTI of
        MTI_CAN_AMD,                                                            // If another node has reset then we need to clear out any inprocess states with that node
        MTI_CAN_AMR :  FlushCanSpecificBuffers(SourceAlias);                    // This is only an issue with CAN so encapsulate it here in CAN only code
      end;
      Result := True;
    end
  end else
  begin
    MTI := (NMRAnetCanBuffer^.MTI shr 12) and $0FFF;
    case (NMRAnetCanBuffer^.MTI shr 12) and $F000 of                            // If we just look at the upper nibble we can classify them as general, datagram, or stream
      MTI_FRAME_TYPE_CAN_GENERAL :
          begin
            if MTI and MTI_ADDRESSED_MASK = MTI_ADDRESSED_MASK then
            begin                                                               // It is an addressed message so pull out the Destination
              DestAlias := (NMRAnetCanBuffer^.Payload[0] shl 8) or NMRAnetCanBuffer^.Payload[1] and $0FFF;  // General CAN messages have the Alias in the payload first two bytes  ;
              DestNode := OPStackNode_FindByAlias(DestAlias);
              if DestNode <> nil then                                           // If it is not addressed to us then throw it away
              begin
                FramingBits := NMRAnetCanBuffer^.Payload[0] and $30;              // The upper nibble lower 2 bits (suppose to ignore the upper to for framing) of the destination may have special meaning, pull it out
                if FramingBits <> 0 then
                begin
                  NMRAnetCanBufferToOPStackBuffer(NMRAnetCanBuffer, @ScratchMessage, FramingBits, MTI, DestAlias, SourceAlias); // Need a OpStackMessage Buffer to call the next function
                  if StackCANStatemachineDatagram_ProcessIncomingMultiFrameMessage(@ScratchMessage, InProcessMessagePtr) then   // Don't dispatch it until it is fully received
                  begin
                    OPStackMessage := InProcessMessagePtr;                        // replace the last incoming frame with the full MultiFrame message
                    Result := True;
                  end;
                end else
                if MTI = MTI_SIMPLE_NODE_INFO_REPLY then                          // Does not use framing bits
                begin
                  NMRAnetCanBufferToOPStackBuffer(NMRAnetCanBuffer, @ScratchMessage, FramingBits, MTI, DestAlias, SourceAlias); // Need a OpStackMessage Buffer to call the next function
                  if OPStackCANStatemachineSnip_ProcessIncomingAcdiSnipMessage(@ScratchMessage, InProcessMessagePtr) then
                  begin
                    OPStackMessage := InProcessMessagePtr;                        // replace the last incoming frame with the full MultiFrame message
                    Result := True;
                  end;
                end else
                begin
                  // Addressed message but a single Frame
                  if OPStackBuffers_AllocateOPStackMessage(OPStackMessage, MTI, SourceAlias, NULL_NODE_ID, DestAlias, NULL_NODE_ID, False) then
                  begin
                    OPStackBuffers_CopyDataArrayWithSourceOffset(OPStackMessage^.Buffer, PSimpleDataArray( PByte( @NMRAnetCanBuffer^.Payload)), NMRAnetCanBuffer^.PayloadCount, 2);
                    Result := True;
                  end else
                    OptionalInteractionRejected(OPStackMessage, False);         // Out of buffers, try again later
                end;
              end;
            end else
            begin   // It is not an addressed message
              if OPStackBuffers_AllocateOPStackMessage(OPStackMessage, MTI, SourceAlias, NULL_NODE_ID, 0, NULL_NODE_ID, False) then
              begin
                OPStackBuffers_CopyDataArray(OPStackMessage^.Buffer, PSimpleDataArray( PByte( @NMRAnetCanBuffer^.Payload)), NMRAnetCanBuffer^.PayloadCount, True);
                Result := True;
              end else
                OptionalInteractionRejected(OPStackMessage, False);             // Out of buffers, please try again
            end;
          end;
      MTI_FRAME_TYPE_CAN_DATAGRAM_ONLY_FRAME,
      MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME_START,
      MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME,
      MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME_END :
        begin
          DestAlias := (NMRAnetCanBuffer^.MTI shr 12) and $0FFF;
          DestNode := OPStackNode_FindByAlias(DestAlias);
          MTI := (NMRAnetCanBuffer^.MTI shr 12) and $F000;
          NMRAnetCanBufferToOPStackBuffer(NMRAnetCanBuffer, @ScratchMessage, 0, MTI, DestAlias, SourceAlias); // Need a OpStackMessage Buffer to call the next function
          if DestNode <> nil then
          begin
            DatagramMessagePtr := nil;
            DatagramProcessErrorCode := StackCANStatemachineDatagram_ProcessIncomingDatagramMessage(@ScratchMessage, DatagramMessagePtr);
            case DatagramProcessErrorCode of
                DATAGRAM_PROCESS_ERROR_OK                      :
                    begin
                      OPStackMessage := DatagramMessagePtr;                        // Replace the passed message with the datagram message
                      Result := True;
                    end;
                DATAGRAM_PROCESS_ERROR_QUIET_FAIL              :
                    begin
                      Result := False
                    end
              else begin
                  if DatagramMessagePtr <> nil then
                    OPStackBuffers_DeAllocateMessage(DatagramMessagePtr);          // If there is an error, need to flush the buffer
                  Source.ID := NULL_NODE_ID;
                  Source.AliasID := SourceAlias;
                  DatagramRejected(@DestNode^.Info, @Source, DatagramProcessErrorCode);
                end;
            end
          end else
            Result := False
        end;
      MTI_FRAME_TYPE_CAN_STREAM_SEND :
            begin

            end;
    end; // case
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
      NMRAnetCanBuffer^.Payload[0] := Hi( AMessage^.Dest.AliasID) or AMessage^.FramingBits;
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
  {$IFDEF SUPPORT_STREAMS}
  OPStackCANStatemachineStream_ProcessOutgoingStreamMessage;
  {$ENDIF}
  OPStackCANStatemachineMultiFrame_ProcessOutgoingMultiFrameMessage;
end;

end.
