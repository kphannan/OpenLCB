unit nmranetutilities;

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  opstacktypes,
  opstackdefines,
  opstackbuffers;


function NMRAnetUtilities_CreateAliasID(var Seed: TNodeID; Regenerate: Boolean): Word;
function NMRAnetUtilities_GenerateID_Alias_From_Seed(var Seed: TNodeID): Word;
procedure NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed(var Seed: TNodeID);
procedure NMRAnetUtilities_LoadSimpleDataWith48BitNodeID(var NodeID: TNodeID; var DataArray: TSimpleDataArray);
procedure NMRAnetUtilities_LoadSimpleDataWithEventID(var EventID: TEventID; var DataArray: TSimpleDataArray);
procedure NMRAnetUtilities_SimpleDataToNodeID(DataArray: PDataArray; var NodeID: TNodeID; iStartByte: Byte);


implementation

// *****************************************************************************
//  procedure NMRAnetUtilities_CreateAliasID
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
function NMRAnetUtilities_CreateAliasID(var Seed: TNodeID; Regenerate: Boolean): Word;
begin
  if Regenerate then
    NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed(Seed);
  Result := NMRAnetUtilities_GenerateID_Alias_From_Seed(Seed);
  if Result = 0 then
  begin
    NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed(Seed);
    Result := NMRAnetUtilities_GenerateID_Alias_From_Seed(Seed);
  end
end;

// *****************************************************************************
//  function NMRAnetUtilities_GenerateID_Alias_From_Global_Seed
//     Parameters:
//     Returns:
//
//     Description:
//
// *****************************************************************************
function NMRAnetUtilities_GenerateID_Alias_From_Seed(var Seed: TNodeID): Word;
begin
  Result := (Seed[0] xor Seed[1] xor (Seed[0] shr 12) xor (Seed[1] shr 12)) and $00000FFF;
end;

// *****************************************************************************
//  procedure PsudoRandomNumberGenerator
//     Parameters:
//     Returns:
//
//     Description:
//
// *****************************************************************************
procedure NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed(var Seed: TNodeID);
var
  temp1,              // Upper 24 Bits of temp 48 bit number
  temp2: DWORD;       // Lower 24 Bits of temp 48 Bit number
begin
  temp1 := ((Seed[1] shl 9) or ((Seed[0] shr 15) and $000001FF)) and $00FFFFFF;   // x(i+1)(2^9 + 1)*x(i) + C  = 2^9 * x(i) + x(i) + C
  temp2 := (Seed[0] shl 9) and $00FFFFFF;                                                                  // Calculate 2^9 * x

  Seed[0] := Seed[0] + temp2 + $7A4BA9;   // Now y = 2^9 * x so all we have left is x(i+1) = y + x + c
  Seed[1] := Seed[1] + temp1 + $1B0CA3;

  Seed[1] := (Seed[1] and $00FFFFFF) or (Seed[0] and $FF000000) shr 24;   // Handle the carries of the lower 24 bits into the upper
  Seed[0] := Seed[0] and $00FFFFFF;
end;

// *****************************************************************************
//  procedure NMRAnetUtilities_LoadSimpleDataWith48BitNodeID
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure NMRAnetUtilities_LoadSimpleDataWith48BitNodeID(var NodeID: TNodeID; var DataArray: TSimpleDataArray);
begin
  DataArray[0] := NodeID[1] shr 16;  // But these all need the 48 Bit Full ID in the Byte Fields
  DataArray[1] := NodeID[1] shr 8;
  DataArray[2] := NodeID[1];
  DataArray[3] := NodeID[0] shr 16;
  DataArray[4] := NodeID[0] shr 8;
  DataArray[5] := NodeID[0];
end;

// *****************************************************************************
//  procedure NMRAnetUtilities_LoadSimpleDataWithEventID
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure NMRAnetUtilities_LoadSimpleDataWithEventID(var EventID: TEventID; var DataArray: TSimpleDataArray);
begin
  DataArray[0] := EventID[0];
  DataArray[1] := EventID[1];
  DataArray[2] := EventID[2];
  DataArray[3] := EventID[3];
  DataArray[4] := EventID[4];
  DataArray[5] := EventID[5];
  DataArray[6] := EventID[6];
  DataArray[7] := EventID[7];
end;

// *****************************************************************************
//  procedure NMRAnetUtilities_SimpleBufferBytesToNodeID
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure NMRAnetUtilities_SimpleDataToNodeID(DataArray: PDataArray; var NodeID: TNodeID; iStartByte: Byte);
begin
  NodeID[1] := DataArray^[iStartByte+2];
  NodeID[1] := NodeID[1] or DataArray^[iStartByte+1] shl 8;
  NodeID[1] := NodeID[1] or DataArray^[iStartByte] shl 16;
  NodeID[0] := DataArray^[iStartByte+5];
  NodeID[0] := NodeID[0] or DataArray^[iStartByte+4] shl 8;
  NodeID[0] := NodeID[0] or DataArray^[iStartByte+3] shl 16;
end;

end.
