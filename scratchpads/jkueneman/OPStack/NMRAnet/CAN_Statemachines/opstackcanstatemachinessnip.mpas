unit opstackcanstatemachinessnip;

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

procedure OPStackCANStatemachineSnip_ProcessOutgoingAcdiSnipMessage;

implementation

// *****************************************************************************
//  procedure OPStackCANStatemachineSnip_ProcessOutgoingAcdiSnipMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCANStatemachineSnip_ProcessOutgoingAcdiSnipMessage;
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

end.

