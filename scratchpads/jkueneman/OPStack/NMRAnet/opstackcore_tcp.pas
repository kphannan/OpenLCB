unit opstackcore_tcp;

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  opstackbuffers,
  nmranetdefines,
  opstackdefines;

// TCP Header:
//  |   Flags 16 Bits   |       Size 24 Bits          |             Originating/Gateway Node 48 Bits              |             Message Capture Time 48 Bits                  |
//  | Byte 00 | Byte 01 | Byte 02 | Byte 03 | Byte 04 | Byte 05 | Byte 06 | Byte 07 | Byte 08 | Byte 09 | Byte 10 | Byte 11 | Byte 12 | Byte 13 | Byte 14 | Byte 15 | Byte 16 |

const
  TCP_HEADER_SIZE = 17;  // 17 Bytes

const
  OPSTACK_TCP_FLAG_OLCB_MESSAGE = $8000;        //  Link Message = 0
  OPSTACK_TCP_FLAG_CHAINING     = $4000;
  // $2000, $1000 reserved
  OPSTACK_TCP_FLAG_MULTI_PART   = $0030;        // $0000 = Single part, $0010 = First part, $0030 = center part, $0020 = last part
  // Rest are reserved

type
  TTcpMessage = array[0..1499] of byte;
  PTcpMessage = ^TTcpMessage;

procedure OPStackcoreTcp_TcpMessageToOpStackMessage( TcpMessage: PTcpMessage; AMessage: POPStackMessage);

implementation

type
  PWord = ^Word;
  PDWord = ^DWord;

procedure WalkHeaderToMessaage(TcpMessage: PTcpMessage; var MessageSize: DWord; var MessageIndex: PByte);
var
  FlagPtr: PWord;
  Size: DWord;
  SizeOffset: PDWord;
begin
  MessageSize := 0;
  MessageIndex :=  @MessageIndex[0];
  FlagPtr := PWord( MessageIndex);
  SizeOffset := PDWord( PByte( @TcpMessage^[2]));
  Size := SizeOffset^ - 12;    // Remove the remaining header bytes in the size
  // find the end of the chain
  while FlagPtr^ and OPSTACK_TCP_FLAG_CHAINING <> 0 do
  begin
    MessageIndex := MessageIndex + TCP_HEADER_SIZE;
    FlagPtr := PWord( MessageIndex);
    Size := Size - 17
  end;
  MessageIndex := MessageIndex + TCP_HEADER_SIZE;  // Move to the data
  MessageSize := Size;
end;

procedure OPStackcoreTcp_TcpMessageToOpStackMessage(TcpMessage: PTcpMessage; AMessage: POPStackMessage);
var
  DataOffset: PByte;
  DataSize: DWord;
  MTI: PWord;
  i: Integer;
begin
  WalkHeaderToMessaage(TcpMessage, DataSize, DataOffset);
  MTI := PWord( DataOffset);
  case MTI^ of

    MTI_DATAGRAM :
      begin
        if OPStackBuffers_AllocateDatagramMessage(AMessage, PNodeInfo( PByte( @DataOffset[2]))^, PNodeInfo( PByte( @DataOffset[8]))^, 0) then
        begin
          DataOffset := DataOffset + 14;
          AMessage^.Buffer^.DataBufferSize := DataSize - 14;;
          for i := 0 to AMessage^.Buffer^.DataBufferSize - 1 do
          begin
            AMessage^.Buffer^.DataArray[i] := DataOffset^;
            DataOffset := DataOffset + 1
          end
        end;
      end;
  end;
end;

end.

