unit nmra_dcc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  DCC_SPEED_STEP_14 = 14;
  DCC_SPEED_STEP_28 = 28;
  DCC_SPEED_STEP_128 = 128;

  DCC_SPEED_STEP_TABLE: array[0..31] of byte = (
    %00000,        // 0x00
    %10000,        // 0x10
    %00001,        // 0x01
    %10001,        // 0x11
    %00010,        // 0x02
    %10010,        // 0x12
    %00011,        // 0x03
    %10011,        // 0x13
    %00100,
    %10100,
    %00101,
    %10101,
    %00110,
    %10110,
    %00111,
    %10111,
    %01000,
    %11000,
    %01001,
    %11001,
    %01010,
    %11010,
    %01011,
    %11011,
    %01100,
    %11100,
    %01101,
    %11101,
    %01110,
    %11110,
    %01111,
    %11111
  );

  DCC_FUNCTION_TABLE: array[0..28] of byte = (
    %10010000,        // F0
    %10000001,        // F1
    %10000010,        // F2
    %10000100,        // F3
    %10001000,        // F4
    %10110001,        // F5
    %10110010,        // F6
    %10110100,        // F7
    %10111000,        // F8
    %10100001,        // F9
    %10100010,        // F10
    %10100100,        // F11
    %10101000,        // F12
    %00000001,        // F13
    %00000010,        // F14
    %00000100,        // F15
    %00001000,        // F16
    %00010000,        // F17
    %00100000,        // F18
    %01000000,        // F19
    %10000000,        // F20
    %00000001,        // F21
    %00000010,        // F22
    %00000100,        // F23
    %00001000,        // F24
    %00010000,        // F25
    %00100000,        // F26
    %01000000,        // F27
    %10000000         // F28
  );

function EncodeDCCSpeed(SpeedStep: Byte; IsForward: Boolean; Step: Byte; EStop: Boolean): Byte;
function EncodeFunction(FunctionNumber: Byte): Byte;

implementation

function EncodeDCCSpeed(SpeedStep: Byte; IsForward: Boolean; Step: Byte; EStop: Boolean): Byte;
begin
  case Step of
    DCC_SPEED_STEP_14, DCC_SPEED_STEP_28 :
      begin
        if SpeedStep = 0 then
          Result := %01000000 or DCC_SPEED_STEP_TABLE[0]
        else
        if EStop then
          Result := %01000000 or DCC_SPEED_STEP_TABLE[2]
        else
          Result := %01000000 or DCC_SPEED_STEP_TABLE[SpeedStep+3];

        if IsForward then
          Result := Result or %00100000;
      end;
    DCC_SPEED_STEP_128 :
      begin
        if EStop then
          Result := %00000001
        else begin
          Result := SpeedStep;
          if IsForward then
            Result := Result or %00100000;
        end;
      end;
  end;
end;

function EncodeFunction(FunctionNumber: Byte): Byte;
begin
  if FunctionNumber < 29 then
    Result := DCC_FUNCTION_TABLE[FunctionNumber]
  else
    Result := 0;
end;

end.

