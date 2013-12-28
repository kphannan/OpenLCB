unit opstackcanstatemachinesstream;

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  {$IFDEF FPC}
  Classes, SysUtils,
  {$ENDIF}
  opstackdefines,
  hardware_template,
  nmranetdefines,
  opstackcanstatemachinesbuffers;

procedure PStackCANStatemachineStream_ProcessOutgoingStreamMessage;

implementation

{$IFDEF SUPPORT_STREAMS}
// *****************************************************************************
//  procedure PStackCANStatemachineStream_ProcessOutgoingStreamMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure PStackCANStatemachineStream_ProcessOutgoingStreamMessage;
var
  LocalOutgoingMessage: POPStackMessage;
begin
  LocalOutgoingMessage := OPStackCANStatemachineBuffers_FirstMessageOnOutgoingStreamStack(0);
  if LocalOutgoingMessage <> nil then                                           // We just work this stack from the top down, for now
    if IsOutgoingBufferAvailable then
    begin
       LocalOutgoingMessage := LocalOutgoingMessage;
    end;
end;

{$ENDIF}

end.

