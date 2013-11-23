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


function AppCallback_AddressSpacePresent(Node: PNMRAnetNode; AddressSpace: Byte): Boolean;
function AppCallback_AddressSpaceReadOnly(Node: PNMRAnetNode; AddressSpace: Byte): Boolean;
function AppCallback_AddressSpaceSize(Node: PNMRAnetNode; AddressSpace: Byte): DWord;

implementation

function AppCallback_AddressSpacePresent(Node: PNMRAnetNode; AddressSpace: Byte): Boolean;
begin
  Result := True
end;

function AppCallback_AddressSpaceReadOnly(Node: PNMRAnetNode; AddressSpace: Byte): Boolean;
begin
  Result := False
end;

function AppCallback_AddressSpaceSize(Node: PNMRAnetNode; AddressSpace: Byte): DWord;
begin
  Result := 44;
end;

end.
