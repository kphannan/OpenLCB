unit opstackcanstatemachinesmultiframe;

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

procedure OPStackCANStatemachineMultiFrame_ProcessOutgoingMultiFrameMessage;

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
      if MultiFrameBufferPtr^.CurrentCount >= MultiFrameBufferPtr^.DataBufferSize then
        LocalBuffer.DataArray[0] := LocalBuffer.DataArray[0] or $20                        // Upper nibble = $20 means last frame
      else
        LocalBuffer.DataArray[0] := LocalBuffer.DataArray[0] or $10;                       // Upper nibble = $10 mean more frames coming

      OutgoingMessage(@LocalMessage);

      if MultiFrameBufferPtr^.CurrentCount >= MultiFrameBufferPtr^.DataBufferSize then
      begin
        OPStackCANStatemachineBuffers_RemoveMultiFrameMessage(LocalOutgoingMessagePtr);
        OPStackBuffers_DeAllocateMessage(LocalOutgoingMessagePtr);
      end;
    end;
  end;
end;

end.
