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

{$IFDEF SUPPORT_STREAMS}
procedure OPStackCANStatemachineStream_ProcessOutgoingStreamMessage;
{$ENDIF}

implementation

{$IFDEF SUPPORT_STREAMS}
// *****************************************************************************
//  procedure OPStackCANStatemachineStream_ProcessOutgoingStreamMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCANStatemachineStream_ProcessOutgoingStreamMessage;
var
  LocalOutgoingMessage: POPStackMessage;
begin   
  if IsOutgoingBufferAvailable then
  begin
    LocalOutgoingMessage := OPStackCANStatemachineBuffers_FirstMessageOnOutgoingStreamStack(0);
    if LocalOutgoingMessage <> nil then                                           // We just work this stack from the top down, for now
    begin
       LocalOutgoingMessage := LocalOutgoingMessage;
    end;
  end;
end;

{$ENDIF}

end.