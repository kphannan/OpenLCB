unit nmranetutilities;

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  template_node,
  template_vnode,
  nmranetdefines,
  opstackdefines,
  opstacktypes;


function NMRAnetUtilities_CreateAliasID(var Seed: TNodeID; Regenerate: Boolean): Word;
function NMRAnetUtilities_GenerateID_Alias_From_Seed(var Seed: TNodeID): Word;
procedure NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed(var Seed: TNodeID);
procedure NMRAnetUtilities_LoadSimpleDataWith48BitNodeID(NodeID: PNodeID; DataArray: PSimpleDataArray);
function NMRAnetUtilities_Load48BitNodeIDWithSimpleData(var NodeID: TNodeID; var DataArray: TSimpleDataArray): PNodeID;
procedure NMRAnetUtilities_LoadSimpleDataWithEventID(EventID: PEventID; DataArray: PSimpleDataArray);
procedure NMRAnetUtilities_SimpleDataToNodeID(DataArray: PSimpleDataArray; var NodeID: TNodeID);
function NMRAnetUtilities_EqualEventID(Event1, Event2: PEventID): Boolean;
function NMRAnetUtilities_EqualNodeIDInfo(var Info1: TNodeInfo; var Info2: TNodeInfo): Boolean;
function NMRAnetUtilities_NullNodeIDInfo(var Info1: TNodeInfo): Boolean;
function NMRAnetUtilities_EqualNodeID(var NodeID1: TNodeID; var NodeID2: TNodeID): Boolean;
function NMRAnetUtilities_NodeSupportsProtcol(Node: PNMRAnetNode; var Protocol: TPIVProtocolValueArray): Boolean;


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
procedure NMRAnetUtilities_LoadSimpleDataWith48BitNodeID(NodeID: PNodeID; DataArray: PSimpleDataArray);
begin
  {$IFNDEF FPC}
  DataArray^[0] := Higher( NodeID^[1]); // But these all need the 48 Bit Full ID in the Byte Fields
  DataArray^[1] := Hi( NodeID^[1]);
  DataArray^[2] := Lo( NodeID^[1]);
  DataArray^[3] := Higher( NodeID^[0]);
  DataArray^[4] := Hi( NodeID^[0]);
  DataArray^[5] := Lo( NodeID^[0]);
  {$ELSE}
  DataArray^[0] := Byte( DWord( NodeID^[1] shr 16));  // But these all need the 48 Bit Full ID in the Byte Fields
  DataArray^[1] := Byte( DWord( NodeID^[1] shr 8));
  DataArray^[2] := Byte( NodeID^[1]);
  DataArray^[3] := Byte( DWord( NodeID^[0] shr 16));
  DataArray^[4] := Byte( DWord( NodeID^[0] shr 8));
  DataArray^[5] := Byte( NodeID^[0]);
  {$ENDIF}
end;


// *****************************************************************************
//  procedure NMRAnetUtilities_Load48BitNodeIDWithSimpleData
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
function NMRAnetUtilities_Load48BitNodeIDWithSimpleData(var NodeID: TNodeID; var DataArray: TSimpleDataArray): PNodeID;
begin
  Result := @NodeID;
  NodeID[1] := (DataArray[0] shl 16) or (DataArray[1] shl 8) or DataArray[2];
  NodeID[0] := (DataArray[3] shl 16) or (DataArray[4] shl 8) or DataArray[5];
end;

// *****************************************************************************
//  procedure NMRAnetUtilities_LoadSimpleDataWithEventID
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure NMRAnetUtilities_LoadSimpleDataWithEventID(EventID: PEventID; DataArray: PSimpleDataArray);
begin
  DataArray^[0] := EventID^[0];
  DataArray^[1] := EventID^[1];
  DataArray^[2] := EventID^[2];
  DataArray^[3] := EventID^[3];
  DataArray^[4] := EventID^[4];
  DataArray^[5] := EventID^[5];
  DataArray^[6] := EventID^[6];
  DataArray^[7] := EventID^[7];
end;

// *****************************************************************************
//  procedure NMRAnetUtilities_SimpleBufferBytesToNodeID
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure NMRAnetUtilities_SimpleDataToNodeID(DataArray: PSimpleDataArray; var NodeID: TNodeID);
begin
  NodeID[1] := DataArray^[2];
  NodeID[1] := NodeID[1] or DataArray^[1] shl 8;
  NodeID[1] := NodeID[1] or DataArray^[0] shl 16;
  NodeID[0] := DataArray^[5];
  NodeID[0] := NodeID[0] or DataArray^[4] shl 8;
  NodeID[0] := NodeID[0] or DataArray^[3] shl 16;
end;

// *****************************************************************************
//  procedure NMRAnetUtilities_EqualEventID
//     Parameters:
//     Returns:
//
//     Description:
//
// *****************************************************************************
function NMRAnetUtilities_EqualEventID(Event1, Event2: PEventID): Boolean;
var
  i: Integer;
begin
  Result := True;
  i := 0;
  while (i < 8) and Result do
  begin
    if Event1^[i] <> Event2^[i] then
    begin
      Result := False;
      Break
    end;
    Inc(i);
  end;
end;

// *****************************************************************************
//  procedure NMRAnetUtilities_EqualNodeIDInfo
//     Parameters:
//     Returns:
//
//     Description:
//
// *****************************************************************************
function NMRAnetUtilities_EqualNodeIDInfo(var Info1: TNodeInfo; var Info2: TNodeInfo): Boolean;
begin
  Result := False;
  if Info1.AliasID = Info2.AliasID then
    if Info1.ID[0] = Info2.ID[0] then
      if Info1.ID[1] = Info2.ID[1] then
        Result := True;
end;

function NMRAnetUtilities_NullNodeIDInfo(var Info1: TNodeInfo): Boolean;
begin
  Result := False;
  if Info1.AliasID = 0 then
    if Info1.ID[0] = 0 then
      if Info1.ID[1] = 0 then
        Result := True;
end;

// *****************************************************************************
//  procedure NMRAnetUtilities_EqualNodeID
//     Parameters:
//     Returns:
//
//     Description:
//
// *****************************************************************************
function NMRAnetUtilities_EqualNodeID(var NodeID1: TNodeID; var NodeID2: TNodeID): Boolean;
begin
  Result := False;
  if NodeID1[0] = NodeID2[0] then
    if NodeID1[1] = NodeID2[1] then
      Result := True;
end;

function NMRAnetUtilities_NodeSupportsProtcol(Node: PNMRAnetNode; var Protocol: TPIVProtocolValueArray): Boolean;
var
  i, j: Integer;
begin
  Result := False;
  if Node^.iIndex = 0 then
  begin
    for i := 0 to USER_PIV_SUPPORTED_PROTOCOL_COUNT - 1 do
    begin
      Result := True;
      for j := 0 to LEN_PIV_PROTOCOL - 1 do
      begin
        if USER_PIV_SUPPORTED_PROTOCOLS[i][j] <> Protocol[j] then
        begin
          Result := False;
          Break
        end;
      end;
      if Result then
        Break
    end;
  end else
  begin
    for i := 0 to USER_PIV_VNODE_SUPPORTED_PROTOCOL_COUNT - 1 do
    begin
      Result := True;
      for j := 0 to LEN_PIV_PROTOCOL - 1 do
      begin
        if USER_PIV_VNODE_SUPPORTED_PROTOCOLS[i][j] <> Protocol[j] then
        begin
          Result := False;
          Break
        end;
      end;
      if Result then
        Break
    end;
  end;
end;


end.
