unit opstackcanstatemachinessnip;

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
  opstackcanstatemachinesbuffers;

procedure OPStackCANStatemachineSnip_ProcessOutgoingAcdiSnipMessage;

implementation

// *****************************************************************************
//  procedure OPStackCANStatemachineSnip_ProcessOutgoingAcdiSnipMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************



MOVE THIS INTO THE MULTI-FRAME CODE AND PUT THE FRAMING BITS IN THE SNIP, THERE IS NO REASON THAT IS NOT BACKWARDS COMPATIBLE.
THAT ALSO MEANS TOSS THE TRAIN SIMPLE INFO FILE AS THAT WILL USE FRAMING BITS.
opstackcanstatemachines.PStackCANStatemachine_NMRAnetCanBufferToOPStackBuffer NEEDS TO CAHNGE AS IT USES THE FRAMING BITS TO DECIDE TO DROP INTO
FRAMING MESSAGE INCOMING.  NEED TO INCLUDE SNIP MESSAGES AS WELL FOR NODES THAT DON''T USE FRAMING BITS ON SNIP

procedure OPStackCANStatemachineSnip_ProcessOutgoingAcdiSnipMessage;
var
  LocalMessage: TOPStackMessage;
  LocalOutgoingMessagePtr: POPStackMessage;
  LocalBuffer: TSimpleBuffer;
  AcdiSnipBufferPtr: PAcdiSnipBuffer;
begin            
  if IsOutgoingBufferAvailable then
  begin
    LocalOutgoingMessagePtr := OPStackCANStatemachineBuffers_FirstMessageOnOutgoingAcdiSnipStack(0);
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
end;

end.
