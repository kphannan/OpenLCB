unit common_utilities;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TFormHideCallback = procedure of object;

  function ValidateHex(TestHexVal: string): string;
  function IsPrintableChar(C: Char): Boolean;

implementation

function ValidateHex(TestHexVal: string): string;
var
  CStylePos, PascalStylePos: SizeInt;
begin
  Result := Trim( TestHexVal);
  CStylePos := Pos('0x', Result);
  PascalStylePos := Pos('$', Result);
  if (PascalStylePos = 0) and (CStylePos = 0) then
    Result := '0x' + Result
end;

function IsPrintableChar(C: Char): Boolean;
begin
  Result := ((Ord( C) >= 32) and (Ord( C) <= 126))  or ((Ord( C) >= 128) and (Ord( C) <= 255))
end;

end.
