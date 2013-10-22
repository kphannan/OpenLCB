unit nmranetutilities;

interface

{$I Options.inc}

uses
  opstackdefines;



function NMRAnetUtilities_CreateAliasID(var Seed: TNodeID; Regenerate: Boolean): Word;
function NMRAnetUtilities_GenerateID_Alias_From_Seed(var Seed: TNodeID): Word;
procedure NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed(var Seed: TNodeID);
procedure NMRAnetUtilities_LoadCANDataWith48BitNodeID(var NodeID: TNodeID; var Data: TCANData);
procedure NMRAnetUtilities_LoadBaseMessageBuffer(Message: PMessage; MessageType: Byte; MTI: DWord; Next: PMessage; var SourceNodeID: TNodeID; var DestNodeID: TNodeID);
procedure NMRAnetUtilities_FillDataArray(DataArray: PArrayData; Value, ArraySize, Count: Byte);

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
//  procedure NMRAnetUtilities_LoadCANDataWith48BitNodeID
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure NMRAnetUtilities_LoadCANDataWith48BitNodeID(var NodeID: TNodeID; var Data: TCANData);
begin
  Data.Count := 6;
  Data.Bytes[0] := NodeID[1] shr 16;  // But these all need the 48 Bit Full ID in the Byte Fields
  Data.Bytes[1] := NodeID[1] shr 8;
  Data.Bytes[2] := NodeID[1];
  Data.Bytes[3] := NodeID[0] shr 16;
  Data.Bytes[4] := NodeID[0] shr 8;
  Data.Bytes[5] := NodeID[0];
end;

procedure NMRAnetUtilities_LoadBaseMessageBuffer(Message: PMessage; MessageType: Byte; MTI: DWord; Next: PMessage; var SourceNodeID: TNodeID; var DestNodeID: TNodeID);
begin
  PBaseMessage( Message)^.MessageType := MessageType;
  PBaseMessage( Message)^.MTI := MTI;
  PBaseMessage( Message)^.Next := Next;
  PBaseMessage( Message)^.SourceNodeID := SourceNodeID;
  PBaseMessage( Message)^.DestNodeID := DestNodeID;
  case MessageType of
    MT_BASIC          :
        begin
          NMRAnetUtilities_FillDataArray(@PNMRAnetBasicMessage( Message)^.CANData, 0, MAX_CAN_BYTES, 0);
          Exit;
        end;
    MT_PROTCOLSUPPORT :
        begin
          PNMRAnetProtocolSupportMessage( Message)^.Protocols := 0;
          Exit;
        end;
    MT_EVENT          :
        begin
          PNMRAnetEventMessage( Message)^.EventID := NULL_EVENT_ID;
          Exit;
        end;
    MT_TRACTION       :
        begin
          Exit
        end;
    MT_REMOTEBUTTON   :
        begin
          NMRAnetUtilities_FillDataArray(@PNMRAnetRemoteButtonMessage( Message)^.CANData, 0, MAX_CAN_BYTES, 0);
          Exit;
        end;
    MT_SNIP           :
        begin
          NMRAnetUtilities_FillDataArray(@PNMRAnetSNIPMessage( Message)^.SNIPData, 0, MAX_SNIP_BYTES, 0);
          Exit;
        end;
    MT_DATATGRAM      :
        begin
          NMRAnetUtilities_FillDataArray(@PNMRAnetDatagramMessage( Message)^.DatagramData, 0, MAX_DATAGRAM_BYTES, 0);
          Exit;
        end;
    MT_STREAM         :
        begin
          NMRAnetUtilities_FillDataArray(@PNMRAnetStreamMessage( Message)^.StreamData, 0, MAX_STREAM_BYTES, 0);
          Exit;
        end;
    MT_CAN            :
        begin

        end;
  end;
end;

procedure NMRAnetUtilities_FillDataArray(DataArray: PArrayData; Value, ArraySize, Count: Byte);
var
  i: Integer;
begin
  for i := 0 to ArraySize - 1 do
    DataArray^.AnArray[i] := Value;
  DataArray^.Count := Count;
end;


end.

