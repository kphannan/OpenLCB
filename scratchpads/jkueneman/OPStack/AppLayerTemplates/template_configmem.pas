unit template_configmem;

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  {$IFDEF FPC}
  Classes, SysUtils,
  {$ENDIF}
  nmranetdefines,
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
  Result := False;
  case AddressSpace of
    MSI_CDI,
    MSI_ACDI_MFG,
    MSI_ALL        : Result := True
  end;
end;

function AppCallback_AddressSpaceSize(Node: PNMRAnetNode; AddressSpace: Byte): DWord;
begin
  Result := $FFFFFFFF;
end;

end.
