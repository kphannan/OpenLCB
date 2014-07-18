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

procedure OPStackCANStatemachineSnip_ProcessOutgoingMultiFrameStringMessage;
function OPStackCANStatemachineSnip_ProcessIncomingMultiFrameStringMessage(OPStackMessage: POPStackMessage; var AcdiSnipMessage: POPStackMessage): Boolean;

implementation

// *****************************************************************************
//  procedure OPStackCANStatemachineSnip_ProcessOutgoingMultiFrameStringMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************

procedure OPStackCANStatemachineSnip_ProcessOutgoingMultiFrameStringMessage;
var
  LocalMessage: TOPStackMessage;
  LocalOutgoingMessagePtr: POPStackMessage;
  LocalBuffer: TSimpleBuffer;
  MultiFrameStringBufferPtr: PMultiFrameStringBuffer;
begin            
  if IsOutgoingBufferAvailable then
  begin
    LocalOutgoingMessagePtr := OPStackCANStatemachineBuffers_FirstMessageOnOutgoingMultiFrameStringStack(0);
    if LocalOutgoingMessagePtr <> nil then
    begin
      LocalOutgoingMessagePtr^.WatchDog_1s := 0;
      MultiFrameStringBufferPtr := PMultiFrameStringBuffer( PByte( LocalOutgoingMessagePtr^.Buffer));
      OPStackBuffers_LoadMessage(@LocalMessage, LocalOutgoingMessagePtr^.MTI, LocalOutgoingMessagePtr^.Source, LocalOutgoingMessagePtr^.Dest, 0);
      OPStackBuffers_ZeroSimpleBuffer(@LocalBuffer, False);
      LocalMessage.MessageType := MT_SIMPLE;
      LocalMessage.Buffer := @LocalBuffer;
      LocalBuffer.DataBufferSize := 0;
      while MultiFrameStringBufferPtr^.CurrentCount < MultiFrameStringBufferPtr^.DataBufferSize do
      begin
        LocalBuffer.DataArray[LocalBuffer.DataBufferSize] := MultiFrameStringBufferPtr^.DataArray[MultiFrameStringBufferPtr^.CurrentCount];
        Inc(LocalBuffer.DataBufferSize );
        Inc(MultiFrameStringBufferPtr^.CurrentCount);
        if LocalBuffer.DataBufferSize = 6 then
          Break;
      end;
      OutgoingMessage(@LocalMessage, True);

      if MultiFrameStringBufferPtr^.CurrentCount >= MultiFrameStringBufferPtr^.DataBufferSize then
      begin
        OPStackCANStatemachineBuffers_RemoveMultiFrameStringMessage(LocalOutgoingMessagePtr);
        OPStackBuffers_DeAllocateMessage(LocalOutgoingMessagePtr);
      end;
    end;
  end;
end;

function OPStackCANStatemachineSnip_ProcessIncomingMultiFrameStringMessage(OPStackMessage: POPStackMessage; var AcdiSnipMessage: POPStackMessage): Boolean;
var
  InProcessMessage: POPStackMessage;
  i: Integer;
  MultiFrameStringBuffer: PMultiFrameStringBuffer;
begin
  Result := False;
  MultiFrameStringBuffer := nil;

  InProcessMessage := OPStackCANStatemachineBuffers_FindMessageOnIncomingMultiFrameStringFrameStack(OPStackMessage);
  if InProcessMessage = nil then
  begin
    if OPStackBuffers_Allcoate_ACDI_SNIP_Message(InProcessMessage, OPStackMessage^.MTI, OPStackMessage^.Source, OPStackMessage^.Dest) then
      OPStackCANStatemachineBuffers_AddIncomingMultiFrameStringMessage(InProcessMessage)
    else begin
      OptionalInteractionRejected(OPStackMessage^.Dest, OPStackMessage^.Source, OPStackMessage^.MTI, False);         // HOW DO I WAIT AND FIND THE LAST BIT WITHOUT THE FRAMING BITS????
      Exit;
    end;
  end;

  InProcessMessage^.WatchDog_1s := 0;

  MultiFrameStringBuffer := PMultiFrameStringBuffer( PByte( InProcessMessage^.Buffer));

  for i := 2 to OPStackMessage^.Buffer^.DataBufferSize - 1 do
  begin
    MultiFrameStringBuffer^.DataArray[MultiFrameStringBuffer^.DataBufferSize] := OPStackMessage^.Buffer^.DataArray[i];
    Inc(MultiFrameStringBuffer^.DataBufferSize);
    if OPStackMessage^.Buffer^.DataArray[i] = Ord( #0) then
      Inc(MultiFrameStringBuffer^.CurrentCount);
  end;

  if MultiFrameStringBuffer^.CurrentCount >= 6 then            // Found the 6 nulls?
  begin
    // Done
    OPStackCANStatemachineBuffers_RemoveIncomingMultiFrameStringMessage(InProcessMessage);
    AcdiSnipMessage := InProcessMessage;
    Result := True
  end;
end;

end.
