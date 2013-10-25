unit gridconnect;

{$IFDEF FPC}
interface
{$ENDIF}

uses
  {$IFDEF FPC}
  Classes, SysUtils,
  {$ENDIF}
  opstackdefines,
  opstackbuffers;

const
  // :X19170640N0501010107015555;#0  Example.....
  // ^         ^                ^
  // 0         10               27
  MAX_GRID_CONNECT_LEN = 29;
  GRID_CONNECT_HEADER_OFFSET_HI = 2;
  GRID_CONNECT_HEADER_OFFSET_LO = 4;
  GRID_CONNECT_DATA_OFFSET = 11;

type
  TGridConnectString = string[MAX_GRID_CONNECT_LEN];


function MessageToGridConnect(Message: PSimpleMessage; var GridConnectBuffer: TGridConnectString): Integer;
procedure GridConnectToCANBuffer(var GridConnectBuffer: TGridConnectString; Message: PSimpleMessage);

implementation

function MessageToGridConnect(Message: PSimpleMessage; var GridConnectBuffer: TGridConnectString): Integer;
var
  ConvertString: string[8];
  i: Integer;
begin
  GridConnectBuffer[0] := ':';
  GridConnectBuffer[1] := 'X';
  {$IFDEF FPC}
  ConvertString := IntToHex(Message^.MTI, 8);
  {$ELSE}
  LongWordToHex(Message^.MTI, ConvertString);
  {$ENDIF}
  GridConnectBuffer[2] := ConvertString[0];
  GridConnectBuffer[3] := ConvertString[1];
  GridConnectBuffer[4] := ConvertString[2];
  GridConnectBuffer[5] := ConvertString[3];
  GridConnectBuffer[6] := ConvertString[4];
  GridConnectBuffer[7] := ConvertString[5];
  GridConnectBuffer[8] := ConvertString[6];
  GridConnectBuffer[9] := ConvertString[7];
  GridConnectBuffer[10] := 'N';
  Result := 11;
  if Message^.Buffer <> nil then
  begin
    if Message^.Buffer^.DataBufferSize > 0 then
    begin
      for i := 0 to Message^.Buffer^.DataBufferSize - 1 do
      begin
        {$IFDEF FPC}
        ConvertString := IntToHex(Message^.Buffer^.DataArray[i], 8);
        {$ELSE}
        WordToHex(Message^.Buffer^.DataArray[i], ConvertString);
        {$ENDIF}
        GridConnectBuffer[Result]     := ConvertString[2];
        GridConnectBuffer[Result + 1] := ConvertString[3];
        Result := Result + 2;
      end;
    end;
  end;
  GridConnectBuffer[Result] := ';';
  Inc(Result);
  GridConnectBuffer[Result] := #0;
end;

procedure GridConnectToCANBuffer(var GridConnectBuffer: TGridConnectString; Message: PSimpleMessage);
const
  MT_CAN_TRANSPORT = MT_SIMPLE or MT_CAN;
var
  ConvertString: string[8];
  i: Integer;
begin
  if Message^.MessageType and MT_CAN_TRANSPORT <> 0 then
  begin
    Message^.Buffer^.DataBufferSize := 0;
    for i := 0 to 7 do
      ConvertString[i] := GridConnectBuffer[GRID_CONNECT_HEADER_OFFSET_HI + i];
    ConvertString[8] := #0;
    {$IFDEF FPC}
    Message^.MTI := StrToInt('0x'+ConvertString);
    {$ELSE}
    Message^.MTI := HexToLongWord(ConvertString);
    {$ENDIF}
    if GridConnectBuffer[GRID_CONNECT_DATA_OFFSET] <> ';' then
    begin
      i := 0;
      while GridConnectBuffer[GRID_CONNECT_DATA_OFFSET + i] <> ';' do
      begin
        ConvertString[0] := GridConnectBuffer[GRID_CONNECT_DATA_OFFSET + i];
        ConvertString[1] := GridConnectBuffer[GRID_CONNECT_DATA_OFFSET + i + 1];
        ConvertString[2] := #0;
        {$IFDEF FPC}
        Message^.Buffer^.DataArray[Message^.Buffer^.DataBufferSize] := StrToInt('0x'+ConvertString);
        {$ELSE}
        Message^.Buffer^.DataBytes[Message^.Buffer^.DataBufferSize] := HexToWord(ConvertString);
        {$ENDIF}
        Inc(Message^.Buffer^.DataBufferSize);
        i := i + 2;
      end;
    end;
  end;
end;

end.

