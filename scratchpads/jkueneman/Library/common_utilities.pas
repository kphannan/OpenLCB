unit common_utilities;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TFormHideCallback = procedure of object;

  function ValidateHex(TestHexVal: string): string;
  function IsPrintableChar(C: Char): Boolean;
  function StreamAsString(Stream: TStream): string;

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

function StreamAsString(Stream: TStream): string;
var
  i: Integer;
begin
  Stream.Position := 0;
 { SetLength(Result, Stream.Size);
  for i := 0 to Stream.Size - 1 do
    Result[i] := Chr( Stream.ReadByte); }
  Result := '';
  for i := 0 to Stream.Size - 1 do
    Result := Result + Chr( Stream.ReadByte);
end;

end.

