unit opstackcanstatemachinesmultiframe;

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  {$IFDEF FPC}
  Classes, SysUtils,
  {$ENDIF}
  template_hardware,
  nmranetdefines,
  opstackdefines,
  opstackbuffers,
  opstacktypes,
  opstackcore_basic,
  opstackcanstatemachinesbuffers;

procedure OPStackCANStatemachineMultiFrame_ProcessOutgoingMultiFrameMessage;
function StackCANStatemachineDatagram_ProcessIncomingMultiFrameMessage(OPStackMessage: POPStackMessage; var MultiFrameMessage: POPStackMessage): Boolean;

implementation

// *****************************************************************************
//  procedure OPStackCANStatemachineMultiFrame_ProcessOutgoingMultiFrameMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCANStatemachineMultiFrame_ProcessOutgoingMultiFrameMessage;
var
  LocalMessage: TOPStackMessage;
  LocalOutgoingMessagePtr: POPStackMessage;
  LocalBuffer: TSimpleBuffer;
  MultiFrameBufferPtr: PMultiFrameBuffer;
begin            
  if IsOutgoingBufferAvailable then
  begin
    LocalOutgoingMessagePtr := OPStackCANStatemachineBuffers_FirstMessageOnOutgoingMultiFrameStack(0);
    if LocalOutgoingMessagePtr <> nil then
    begin
      MultiFrameBufferPtr := PMultiFrameBuffer( PByte( LocalOutgoingMessagePtr^.Buffer));
      OPStackBuffers_LoadMessage(@LocalMessage, LocalOutgoingMessagePtr^.MTI, LocalOutgoingMessagePtr^.Source.AliasID, LocalOutgoingMessagePtr^.Source.ID, LocalOutgoingMessagePtr^.Dest.AliasID, LocalOutgoingMessagePtr^.Dest.ID, 0);
      OPStackBuffers_ZeroSimpleBuffer(@LocalBuffer, False);
      LocalMessage.MessageType := MT_SIMPLE;
      LocalMessage.Buffer := @LocalBuffer;
      LocalBuffer.DataBufferSize := 0;
      while MultiFrameBufferPtr^.CurrentCount < MultiFrameBufferPtr^.DataBufferSize do
      begin
        LocalBuffer.DataArray[LocalBuffer.DataBufferSize] := MultiFrameBufferPtr^.DataArray[MultiFrameBufferPtr^.CurrentCount];
        Inc(LocalBuffer.DataBufferSize );
        Inc(MultiFrameBufferPtr^.CurrentCount);
        if LocalBuffer.DataBufferSize = 6 then
          Break;
      end;

      // Set Framing Bits
      if MultiFrameBufferPtr^.CurrentCount = 6 then
        LocalMessage.FramingBits := $10                                         // First Frame
      else
      if MultiFrameBufferPtr^.CurrentCount >= MultiFrameBufferPtr^.DataBufferSize then
        LocalMessage.FramingBits := $20                                         // Upper nibble = $20 means last frame
      else
        LocalMessage.FramingBits := $30;                                        // Upper nibble = $30 means middle frame

      OutgoingMessage(@LocalMessage);

      if MultiFrameBufferPtr^.CurrentCount >= MultiFrameBufferPtr^.DataBufferSize then
      begin
        OPStackCANStatemachineBuffers_RemoveMultiFrameMessage(LocalOutgoingMessagePtr);
        OPStackBuffers_DeAllocateMessage(LocalOutgoingMessagePtr);
      end;
    end;
  end;
end;

function StackCANStatemachineDatagram_ProcessIncomingMultiFrameMessage(OPStackMessage: POPStackMessage; var MultiFrameMessage: POPStackMessage): Boolean;
var
  InProcessMessage: POPStackMessage;
  i: Integer;
  MultiFrameBuffer: PMultiFrameBuffer;
begin
  Result := False;
  MultiFrameMessage := nil;
  InProcessMessage := OPStackCANStatemachineBuffers_FindMessageOnIncomingMultiFrameStack(OPStackMessage);
  if InProcessMessage = nil then
  begin
    if OPStackMessage^.FramingBits = $10 then
    begin
      if OPStackBuffers_AllocateMultiFrameMessage(InProcessMessage, OPStackMessage^.MTI, OPStackMessage^.Source.AliasID, OPStackMessage^.Source.ID, OPStackMessage^.Dest.AliasID, OPStackMessage^.Dest.ID) then
        OPStackCANStatemachineBuffers_AddIncomingMultiFrameMessage(InProcessMessage)
      else
        Exit;     // Out of buffers, exit and wait until the last frame is sent to send OIR
    end else
    begin
      if OPStackMessage^.FramingBits = $20 then                                 // If the last frame and there is no inprocess message we are dropping the message
        OptionalInteractionRejected(OPStackMessage, False);
      Exit;                                                                     // If a middle frame then just drop it if not in the stack
    end
  end;

  MultiFrameBuffer := PMultiFrameBuffer( PByte( InProcessMessage^.Buffer));
  // middle Frame, or last Frame
  for i := 0 to OPStackMessage^.Buffer^.DataBufferSize - 1 do
  begin
    MultiFrameBuffer^.DataArray[MultiFrameBuffer^.DataBufferSize] := OPStackMessage^.Buffer^.DataArray[i];
    Inc(MultiFrameBuffer^.DataBufferSize);
  end;

  if OPStackMessage^.FramingBits = $20 then
  begin
    // Done
    OPStackCANStatemachineBuffers_RemoveIncomingMultiFrameMessage(InProcessMessage);
    MultiFrameMessage := InProcessMessage;
    Result := True
  end;

end;

end.
