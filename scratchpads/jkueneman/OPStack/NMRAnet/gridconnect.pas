unit gridconnect;

{$IFDEF FPC}
interface
{$ENDIF}

uses
  {$IFDEF FPC}
  Classes, SysUtils,
  {$ENDIF}
  nmranetdefines,
  opstackdefines,
  opstackbuffers;

procedure GridConnect_Initialize;

function MessageToGridConnect(Message: PSimpleMessage; var GridConnectBuffer: TGridConnectString): Integer;
procedure GridConnectToMessage(var GridConnectBuffer: TGridConnectString; Message: PSimpleMessage);
function GridConnectDecodeMachine(NextChar: Char): PGridConnectString;
function GridConnectToMessageBuffer(GridConnectStr: PGridConnectString): PSimpleMessage;
function MessageBufferToGridConnect(SimpleMessage: PSimpleMessage; var GridConnectStr: TGridConnectString): Integer;


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
//  procedure IsValidHexChar
//     Parameters: AChar: Charater to test
//     Returns:     True if the character is used in a Hex number
//     Description:
// *****************************************************************************
function IsValidHexChar(AChar: Char): Boolean;
begin
  Result := ((AChar >= '0') and (AChar <= '9')) or ((AChar >= 'A') and (AChar <= 'F')) or ((AChar >= 'a') and (AChar <= 'f'))
end;

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
//  procedure MessageToGridConnect
//     Parameters: Message to convert to a GridConnect String
//                 GridConnectBuffer: array of characters that receives the GridConnect string
//     Returns:     The number of bytes in the GridConnect string
//     Description: Converts a message into a Grid
// *****************************************************************************
function MessageToGridConnect(Message: PSimpleMessage; var GridConnectBuffer: TGridConnectString): Integer;
var
  ConvertString: array[0..7] of char;
  i: Integer;
begin
  {$IFDEF FPC}
    i := 1;
    GridConnectBuffer[0] := ':';
    GridConnectBuffer[1] := 'X';
    ConvertString := IntToHex(Message^.MTI, 8);
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
          ConvertString := IntToHex(Message^.Buffer^.DataArray[i], 4);
          GridConnectBuffer[Result]     := ConvertString[2];
          GridConnectBuffer[Result + 1] := ConvertString[3];
          Result := Result + 2;
        end;
      end;
    end;
    GridConnectBuffer[Result] := ';';
    Inc(Result);
    GridConnectBuffer[Result] := #0;
  {$ELSE}
    GridConnectBuffer[0] := ':';
    GridConnectBuffer[1] := 'X';
    LongWordToHex(Message^.MTI, ConvertString);
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
          WordToHex(Message^.Buffer^.DataArray[i], ConvertString);
          GridConnectBuffer[Result]     := ConvertString[2];
          GridConnectBuffer[Result + 1] := ConvertString[3];
          Result := Result + 2;
        end;
      end;
    end;
    GridConnectBuffer[Result] := ';';
    Inc(Result);
    GridConnectBuffer[Result] := #0;

  {$ENDIF}
end;

// *****************************************************************************
//  procedure GridConnectToMessage
//     Parameters: Message that contains the information in the GridConnect String
//                 GridConnectBuffer: array of characters to convert to a Message
//     Returns:
//     Description: Converts a GridConnect string to a Message
// *****************************************************************************
procedure GridConnectToMessage(var GridConnectBuffer: TGridConnectString; Message: PSimpleMessage);
const
  MT_CAN_TRANSPORT = MT_SIMPLE or MT_CAN;
var
  ConvertString: string[8];
  i: Integer;
begin
  {$IFDEF FPC}
  if Message^.MessageType and MT_CAN_TRANSPORT <> 0 then
  begin
    Message^.Buffer^.DataBufferSize := 0;
    for i := 0 to 7 do
      ConvertString[i] := GridConnectBuffer[GRID_CONNECT_HEADER_OFFSET_HI + i];
    ConvertString[8] := #0;
    Message^.MTI := StrToInt('0x'+ConvertString);
    if GridConnectBuffer[GRID_CONNECT_DATA_OFFSET] <> ';' then
    begin
      i := 0;
      while GridConnectBuffer[GRID_CONNECT_DATA_OFFSET + i] <> ';' do
      begin
        ConvertString[0] := GridConnectBuffer[GRID_CONNECT_DATA_OFFSET + i];
        ConvertString[1] := GridConnectBuffer[GRID_CONNECT_DATA_OFFSET + i + 1];
        ConvertString[2] := #0;
        Message^.Buffer^.DataArray[Message^.Buffer^.DataBufferSize] := StrToInt('0x'+ConvertString);
        Inc(Message^.Buffer^.DataBufferSize);
        i := i + 2;
      end;
    end;
  end;
 {$ELSE}
  if Message^.MessageType and MT_CAN_TRANSPORT <> 0 then
  begin
    Message^.Buffer^.DataBufferSize := 0;
    for i := 0 to 7 do
      ConvertString[i] := GridConnectBuffer[GRID_CONNECT_HEADER_OFFSET_HI + i];
    ConvertString[8] := #0;
    Message^.MTI := StrToInt('0x'+ConvertString);
    if GridConnectBuffer[GRID_CONNECT_DATA_OFFSET] <> ';' then
    begin
      i := 0;
      while GridConnectBuffer[GRID_CONNECT_DATA_OFFSET + i] <> ';' do
      begin
        ConvertString[0] := GridConnectBuffer[GRID_CONNECT_DATA_OFFSET + i];
        ConvertString[1] := GridConnectBuffer[GRID_CONNECT_DATA_OFFSET + i + 1];
        ConvertString[2] := #0;
        Message^.Buffer^.DataBytes[Message^.Buffer^.DataBufferSize] := HexToWord(ConvertString);
        Inc(Message^.Buffer^.DataBufferSize);
        i := i + 2;
      end;
    end;
  end;
 {$ENDIF}
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
function GridConnectDecodeMachine(NextChar: Char): PGridConnectString;
begin
 Result := nil;
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
               Result := @ReceiveGridConnectBuffer;
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
function GridConnectToMessageBuffer(GridConnectStr: PGridConnectString): PSimpleMessage;
var
  ConvertStr: array[0..8] of char;
  i: Integer;
  MTI: DWord;
  SourceAlias, DestAlias: Word;
begin
  Result := nil;
  // First convert part of the message to see what we have
  for i := 0 to 7 do
    ConvertStr[i] := GridConnectStr^[GRID_CONNECT_HEADER_OFFSET_HI+i];
  ConvertStr[8] := #0;
  {$IFDEF FPC}
  MTI := StrToInt('0x'+ConvertStr);
  {$ELSE}
  MTI := HexToLongWord(ConvertStr);
  {$ENDIF}
  if MTI and MTI_ADDRESSED_MASK <> 0 then
    SourceAlias := MTI and MASK_SOURCE_ALIAS;

  if GridConnectStr^[GRID_CONNECT_DATA_OFFSET] <> ';' then
  begin
    if OPStack_AllocateCANMessage(Result, MTI, nil, SourceAlias, NULL_NODE_ID, 0, NULL_NODE_ID) then
    begin
      Result^.Buffer^.DataBufferSize := 0;
      Result^.MTI := MTI and MTI_MASK;
      i := 0;
      while GridConnectStr^[GRID_CONNECT_DATA_OFFSET + i] <> ';' do
      begin
        ConvertStr[0] := GridConnectStr^[GRID_CONNECT_DATA_OFFSET + i];
        ConvertStr[1] := GridConnectStr^[GRID_CONNECT_DATA_OFFSET + i + 1];
        ConvertStr[2] := #0;
        {$IFDEF FPC}
        Result^.Buffer^.DataArray[Result^.Buffer^.DataBufferSize] := StrToInt('0x'+ConvertStr);
        {$ELSE}
        Result^.Buffer^.DataArray[Result^.Buffer^.DataBufferSize] := HexToWord(ConvertStr);
        {$ENDIF}
        Inc(Result^.Buffer^.DataBufferSize);
        i := i + 2;
      end;
    end
  end else
    OPStack_AllocateMessage(Result, MTI, nil, SourceAlias, NULL_NODE_ID, 0, NULL_NODE_ID);
end;

// *****************************************************************************
//  procedure MessageBufferToGridConnect
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
function MessageBufferToGridConnect(SimpleMessage: PSimpleMessage; var GridConnectStr: TGridConnectString): Integer;
var
  ConvertString: array[0..8] of char;
  i: Integer;
begin
  GridConnectStr[0] := ':';
  GridConnectStr[1] := 'X';
  {$IFDEF FPC}
  ConvertString := IntToHex(SimpleMessage^.MTI, 8);
  {$ELSE}
  LongWordToHex(Buffer^.ID, ConvertString);
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
  if SimpleMessage^.Buffer <> nil then
    if SimpleMessage^.Buffer^.DataBufferSize > 0 then
    begin
      for i := 0 to SimpleMessage^.Buffer^.DataBufferSize - 1 do
      begin
        {$IFDEF FPC}
        ConvertString := IntToHex(SimpleMessage^.Buffer^.DataArray[i], 2);
        {$ELSE}
        WordToHex(SimpleMessage^.Buffer^.DataArray[i], ConvertString);
        {$ENDIF}
        GridConnectStr[Result]     := ConvertString[2];
        GridConnectStr[Result + 1] := ConvertString[3];
        Result := Result + 2;
      end;
    end;
  GridConnectStr[Result] := ';';
  Inc(Result);
  GridConnectStr[Result] := #0;
end;

end.

