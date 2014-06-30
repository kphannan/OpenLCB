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
  opstackcore_basic,
  opstackcanstatemachinesbuffers;

procedure OPStackCANStatemachineSnip_ProcessOutgoingAcdiSnipMessage;
function OPStackCANStatemachineSnip_ProcessIncomingAcdiSnipMessage(OPStackMessage: POPStackMessage; var AcdiSnipMessage: POPStackMessage): Boolean;

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
  if IsOutgoingBufferAvailable then
  begin
    LocalOutgoingMessagePtr := OPStackCANStatemachineBuffers_FirstMessageOnOutgoingAcdiSnipStack(0);
    if LocalOutgoingMessagePtr <> nil then
    begin
      AcdiSnipBufferPtr := PAcdiSnipBuffer( PByte( LocalOutgoingMessagePtr^.Buffer));
      OPStackBuffers_LoadMessage(@LocalMessage, LocalOutgoingMessagePtr^.MTI, LocalOutgoingMessagePtr^.Source.AliasID, LocalOutgoingMessagePtr^.Source.ID, LocalOutgoingMessagePtr^.Dest.AliasID, LocalOutgoingMessagePtr^.Dest.ID, 0);
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

function OPStackCANStatemachineSnip_ProcessIncomingAcdiSnipMessage(OPStackMessage: POPStackMessage; var AcdiSnipMessage: POPStackMessage): Boolean;
var
  InProcessMessage: POPStackMessage;
  i: Integer;
  AcdiSnipBuffer: PAcdiSnipBuffer;
begin
  Result := False;
  AcdiSnipBuffer := nil;

  InProcessMessage := OPStackCANStatemachineBuffers_FindMessageOnIncomingAcdiSnipFrameStack(OPStackMessage);
  if InProcessMessage = nil then
  begin
    if OPStackBuffers_Allcoate_ACDI_SNIP_Message(InProcessMessage, OPStackMessage^.MTI, OPStackMessage^.Source.AliasID, OPStackMessage^.Source.ID, OPStackMessage^.Dest.AliasID, OPStackMessage^.Dest.ID) then
      OPStackCANStatemachineBuffers_AddIncomingAcdiSnipMessage(InProcessMessage)
    else begin
      OptionalInteractionRejected(OPStackMessage^.Source.AliasID, OPStackMessage^.Dest.AliasID, OPStackMessage^.Source.ID, OPStackMessage^.Dest.ID, OPStackMessage^.MTI, False);         // HOW DO I WAIT AND FIND THE LAST BIT WITHOUT THE FRAMING BITS????
      Exit;
    end;
  end;

  AcdiSnipBuffer := PAcdiSnipBuffer( PByte( InProcessMessage^.Buffer));

  for i := 2 to OPStackMessage^.Buffer^.DataBufferSize - 1 do
  begin
    AcdiSnipBuffer^.DataArray[AcdiSnipBuffer^.DataBufferSize] := OPStackMessage^.Buffer^.DataArray[i];
    Inc(AcdiSnipBuffer^.DataBufferSize);
    if OPStackMessage^.Buffer^.DataArray[i] = Ord( #0) then
      Inc(AcdiSnipBuffer^.CurrentCount);
  end;

  if AcdiSnipBuffer^.CurrentCount >= 5 then            // Found the 5 nulls?
  begin
    // Done
    OPStackCANStatemachineBuffers_RemoveIncomingAcdiSnipMessage(InProcessMessage);
    AcdiSnipMessage := InProcessMessage;
    Result := True
  end;
end;

end.
