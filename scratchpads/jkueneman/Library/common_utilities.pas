unit common_utilities;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ELSE}
  LclIntf,
  {$ENDIF}
  SysUtils;

type
  TFormHideCallback = procedure of object;

  function ValidateHex(TestHexVal: string): string;
  function IsPrintableChar(C: Char): Boolean;
  function StreamAsString(Stream: TStream): string;
  function GetTickCount : DWORD;

implementation

function GetTickCount : DWORD;
 {On Windows, this is number of milliseconds since Windows was
   started. On non-Windows platforms, LCL returns number of
   milliseconds since Dec. 30, 1899, wrapped by size of DWORD.
   This value can overflow LongInt variable when checks turned on,
   so "wrap" value here so it fits within LongInt.
  Also, since same thing could happen with Windows that has been
   running for at least approx. 25 days, override it too.}
begin
{$IFDEF MSWINDOWS}
  Result := Windows.GetTickCount mod High(LongInt);
{$ELSE}
  Result := LclIntf.GetTickCount mod High(LongInt);
{$ENDIF}
end;

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

