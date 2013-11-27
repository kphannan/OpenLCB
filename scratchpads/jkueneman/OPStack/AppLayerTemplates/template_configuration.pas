unit template_configuration;

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  {$IFDEF FPC}
  Classes,
  SysUtils,
  {$ENDIF}
  opstackdefines;

function AppCallback_ReadConfiguration(ConfigAddress: DWord; ReadCount: Word; DatagramData: PByte): Word;
function AppCallback_WriteConfiguration(ConfigAddress: DWord; ReadCount: Word; DatagramData: PByte): Word;

implementation

const
  MAX_MAP = $FFFF;
var
  ConfigurationMap: array[0..MAX_MAP - 1] of Byte;
  i: Integer;

function AppCallback_ReadConfiguration(ConfigAddress: DWord; ReadCount: Word; DatagramData: PByte): Word;
var
  i: Integer;
begin
  for i := 0 to ReadCount - 1 do
  begin
    if ConfigAddress < MAX_MAP then
      DatagramData^ := ConfigurationMap[ConfigAddress+i];
    Inc(DatagramData);
  end;
  Result := ReadCount;
end;

function AppCallback_WriteConfiguration(ConfigAddress: DWord; ReadCount: Word; DatagramData: PByte): Word;
var
  i: Integer;
  Temp: Byte;
begin
  for i := 0 to ReadCount - 1 do
  begin
    if ConfigAddress < MAX_MAP then
      ConfigurationMap[ConfigAddress+i] := DatagramData^;
    Inc(DatagramData);
  end;
  Result := ReadCount;
end;

initialization
  for i := 0 to MAX_MAP - 1 do
    ConfigurationMap[i] := 0;

end.

