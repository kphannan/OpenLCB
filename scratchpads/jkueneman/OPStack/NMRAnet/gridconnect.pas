unit gridconnect;

{$IFDEF FPC}
interface
{$ENDIF}

uses
  {$IFDEF FPC}
  Classes, SysUtils,
  {$ENDIF}
  HelperFunctions,
  nmranetdefines,
  opstackdefines,
  opstackbuffers;

type
  TGridConnectBuffer = record
    MTI: DWord;
    Payload: array[0..7] of byte;
    PayloadCount: Byte;
  end;
  PGridConnectBuffer = ^TGridConnectBuffer;

procedure GridConnect_Initialize;

function GridConnectDecodeMachine(NextChar: Char; var GridConnectStrPtr: PGridConnectString): Boolean;
procedure GridConnectToGridConnectBuffer(GridConnectStr: PGridConnectString; var GridConnectBuffer: TGridConnectBuffer);
function GridConnectBufferToGridConnect(var GridConnectBuffer: TGridConnectBuffer; var GridConnectStr: TGridConnectString): Integer;


implementation

const
  GRIDCONNECT_STATE_SYNC_START = 0;
  GRIDCONNECT_STATE_SYNC_FIND_X = 1;
  GRIDCONNECT_STATE_SYNC_FIND_HEADER = 2;
  GRIDCONNECT_STATE_SYNC_FIND_N = 3;
  GRIDCONNECT_STATE_SYNC_FIND_DATA = 4;

var
  ReceiveGridConnectBuffer: TGridConnectString;                                // Needs to persist between receptions
  ReceiveGridConnectBufferIndex: Integer;
  GridConnectReceiveState: Word;


// *****************************************************************************
//  procedure GridConnect_Initialize
//     Parameters:
//     Returns:
//     Description: Initializes the unit
// *****************************************************************************
procedure GridConnect_Initialize;
begin
  ReceiveGridConnectBufferIndex := 0;
  GridConnectReceiveState := 0;
end;


// *****************************************************************************
//  procedure GridConnectToMessage
//     Parameters: Message that contains the information in the GridConnect String
//                 GridConnectBuffer: array of characters to convert to a Message
//     Returns: Pointer to a GridConnectString if a complete one is available, else
//              nil.  The caller must use or copy the string before calling the
//              function again as the contents will be corrupted after that
//     Description: Takes a single character at a time and tries to create a
//                  GridConnect string from it in a statemachine
// *****************************************************************************
function GridConnectDecodeMachine(NextChar: Char; var GridConnectStrPtr: PGridConnectString): Boolean;
begin
 Result := False;
 case GridConnectReceiveState of
      GRIDCONNECT_STATE_SYNC_START :                                            // Find a starting ':'
        begin
          if NextChar = ':' then
          begin
            ReceiveGridConnectBufferIndex := 0;
            ReceiveGridConnectBuffer[ReceiveGridConnectBufferIndex] := ':';
            Inc(ReceiveGridConnectBufferIndex);
            GridConnectReceiveState := GRIDCONNECT_STATE_SYNC_FIND_X
          end
        end;
      GRIDCONNECT_STATE_SYNC_FIND_X :
        begin
          if NextChar <> ':' then                                               // Handle double ":"'s by doing nothing if the next byte is a ":", just wait for the next byte to see if it is a "X"
          begin
            if (NextChar = 'X') or (NextChar = 'x') then
            begin
              ReceiveGridConnectBuffer[ReceiveGridConnectBufferIndex] := 'X';
              Inc(ReceiveGridConnectBufferIndex);
              GridConnectReceiveState := GRIDCONNECT_STATE_SYNC_FIND_HEADER
            end else
               GridConnectReceiveState := GRIDCONNECT_STATE_SYNC_START          // Error, start over
          end
        end;
      GRIDCONNECT_STATE_SYNC_FIND_HEADER :
        begin
          if IsValidHexChar(NextChar) then
          begin
            ReceiveGridConnectBuffer[ReceiveGridConnectBufferIndex] := NextChar;
            if ReceiveGridConnectBufferIndex = 9 then
              GridConnectReceiveState := GRIDCONNECT_STATE_SYNC_FIND_N;
            Inc(ReceiveGridConnectBufferIndex);
          end else
            GridConnectReceiveState := GRIDCONNECT_STATE_SYNC_START             // Error start over
        end;
      GRIDCONNECT_STATE_SYNC_FIND_N :
        begin
          if (NextChar >= 'N') or (NextChar <= 'n') then
          begin
            ReceiveGridConnectBuffer[ReceiveGridConnectBufferIndex] := 'N';
            Inc(ReceiveGridConnectBufferIndex);
            GridConnectReceiveState := GRIDCONNECT_STATE_SYNC_FIND_DATA;
          end else
            GridConnectReceiveState := GRIDCONNECT_STATE_SYNC_START             // Error start over
        end;
      GRIDCONNECT_STATE_SYNC_FIND_DATA :
        begin
           if NextChar = ';'then
           begin
             if (ReceiveGridConnectBufferIndex + 1) mod 2 = 0 then              // 0 index, add 1 for the actual character count, if not true the result is broken
             begin
               ReceiveGridConnectBuffer[ReceiveGridConnectBufferIndex] := ';';
               ReceiveGridConnectBuffer[ReceiveGridConnectBufferIndex + 1] := #0;
               GridConnectStrPtr := @ReceiveGridConnectBuffer;
               Result := True;
             end;
             GridConnectReceiveState := GRIDCONNECT_STATE_SYNC_START            // Done
           end else
           begin
             if IsValidHexChar(NextChar) then
             begin
               ReceiveGridConnectBuffer[ReceiveGridConnectBufferIndex] := NextChar;
               Inc(ReceiveGridConnectBufferIndex);
             end else
               GridConnectReceiveState := GRIDCONNECT_STATE_SYNC_START;         // Error start over
           end
        end else
          GridConnectReceiveState := GRIDCONNECT_STATE_SYNC_START;              // Invalidate State Index
    end;  // Case

end;

// *****************************************************************************
//  procedure GridConnectToMessageBuffer
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure GridConnectToGridConnectBuffer(GridConnectStr: PGridConnectString; var GridConnectBuffer: TGridConnectBuffer);
var
  ConvertStr: array[0..8] of char;
  {$IFDEF FPC}
  ConvertPasStr: ansistring;
  {$ENDIF}
  i: Integer;
begin
  // First convert part of the message to see what we have
  for i := 0 to 7 do
    ConvertStr[i] := GridConnectStr^[GRID_CONNECT_HEADER_OFFSET_HI+i];
  ConvertStr[8] := #0;
  {$IFDEF FPC}
  ConvertPasStr := ConvertStr;
  GridConnectBuffer.MTI := StrToInt('0x' + ConvertPasStr);
  {$ELSE}
  GridConnectBuffer.MTI := HexToLongWord(ConvertStr);
  {$ENDIF}
  GridConnectBuffer.PayloadCount := 0;
  if GridConnectStr^[GRID_CONNECT_DATA_OFFSET] <> ';' then
  begin
    i := 0;
    while GridConnectStr^[GRID_CONNECT_DATA_OFFSET + i] <> ';' do
    begin
      ConvertStr[0] := GridConnectStr^[GRID_CONNECT_DATA_OFFSET + i];
      ConvertStr[1] := GridConnectStr^[GRID_CONNECT_DATA_OFFSET + i + 1];
      ConvertStr[2] := #0;
      {$IFDEF FPC}
      ConvertPasStr := ConvertStr;
      GridConnectBuffer.Payload[GridConnectBuffer.PayloadCount] := StrToInt('0x' + ConvertPasStr);
      {$ELSE}
      GridConnectBuffer.Payload[GridConnectBuffer.PayloadCount] := HexToWord(ConvertStr);
      {$ENDIF}
      Inc(GridConnectBuffer.PayloadCount);
      i := i + 2;
    end;
  end
end;

// *****************************************************************************
//  procedure MessageBufferToGridConnect
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
function GridConnectBufferToGridConnect(var GridConnectBuffer: TGridConnectBuffer; var GridConnectStr: TGridConnectString): Integer;
var
  ConvertString: array[0..8] of char;
  i: Integer;
begin
  GridConnectStr[0] := ':';
  GridConnectStr[1] := 'X';
  {$IFDEF FPC}
  ConvertString := IntToHex(GridConnectBuffer.MTI, 8);
  {$ELSE}
  LongWordToHex(GridConnectBuffer.MTI, ConvertString);
  {$ENDIF}
  GridConnectStr[2] := ConvertString[0];
  GridConnectStr[3] := ConvertString[1];
  GridConnectStr[4] := ConvertString[2];
  GridConnectStr[5] := ConvertString[3];
  GridConnectStr[6] := ConvertString[4];
  GridConnectStr[7] := ConvertString[5];
  GridConnectStr[8] := ConvertString[6];
  GridConnectStr[9] := ConvertString[7];
  GridConnectStr[10] := 'N';
  Result := 11;

    if GridConnectBuffer.PayloadCount > 0 then
    begin
      for i := 0 to GridConnectBuffer.PayloadCount - 1 do
      begin
        {$IFDEF FPC}
        ConvertString := IntToHex(GridConnectBuffer.Payload[i], 2);
        {$ELSE}
        WordToHex(GridConnectBuffer.Payload[i], ConvertString);
        {$ENDIF}
        GridConnectStr[Result]     := ConvertString[0];
        GridConnectStr[Result + 1] := ConvertString[1];
        Result := Result + 2;
      end;
    end;
  GridConnectStr[Result] := ';';
  Inc(Result);
  GridConnectStr[Result] := #0;
end;

end.
