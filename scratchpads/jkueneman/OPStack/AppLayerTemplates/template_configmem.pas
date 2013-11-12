unit template_configmem;

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  {$IFDEF FPC}
  Classes, SysUtils,
  {$ENDIF}
  opstackdefines;


function AppCallback_AddressSpacePresent(var Node: TNMRAnetNode; AddressSpace: Byte): Boolean;

implementation

function AppCallback_AddressSpacePresent(var Node: TNMRAnetNode; AddressSpace: Byte): Boolean;
begin
  Result := True
end;

end.
