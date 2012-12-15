unit olcb_mem_protocol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, olcb_utilities, olcb_defines, olcb_threaded_stack;

type

  { TOlcbMemOptions }

  TOlcbMemOptions = class
  private
    FAddressSpaceHi: Byte;
    FAddressSpaceLo: Byte;
    FDescription: string;
    FOperationMask: Word;
    FWriteLengthMask: Byte;
    function GetReadFromMfgACDI: Boolean;
    function GetReadFromUserACDI: Boolean;
    function GetUnAlignedReads: Boolean;
    function GetUnAlignedWrites: Boolean;
    function GetWrite64Bytes: Boolean;
    function GetWriteArbitraryBytes: Boolean;
    function GetWriteFourBytes: Boolean;
    function GetWriteOneByte: Boolean;
    function GetWriteStreamBytes: Boolean;
    function GetWriteToUserACDI: Boolean;
    function GetWriteTwoBytes: Boolean;
    function GetWriteUnderMask: Boolean;
  protected
    property OperationMask: Word read FOperationMask write FOperationMask;
    property WriteLengthMask: Byte read FWriteLengthMask write FWriteLengthMask;
  public
    constructor Create;
    procedure LoadFromDatagram( Datagram: TDatagramReceive);
    property WriteUnderMask: Boolean read GetWriteUnderMask;
    property UnAlignedReads: Boolean read GetUnAlignedReads;
    property UnalignedWrites: Boolean read GetUnAlignedWrites;
    property ReadFromMfgACDI: Boolean read GetReadFromMfgACDI;
    property ReadFromUserACDI: Boolean read GetReadFromUserACDI;
    property WriteToUserACDI: Boolean read GetWriteToUserACDI;
    property WriteOneByte: Boolean read GetWriteOneByte;
    property WriteTwoBytes: Boolean read GetWriteTwoBytes;
    property WriteFourBytes: Boolean read GetWriteFourBytes;
    property Write64Bytes: Boolean read GetWrite64Bytes;
    property WriteArbitraryBytes: Boolean read GetWriteArbitraryBytes;
    property WriteStreamBytes: Boolean read GetWriteStreamBytes;
    property AddressSpaceHi: Byte read FAddressSpaceHi;
    property AddressSpaceLo: Byte read FAddressSpaceLo;
    property Description: string read FDescription;
  end;

  { TOlcbMemAddressSpace }

  TOlcbMemAddressSpace = class
  private
    FAddressHi: DWord;
    FAddressLo: DWord;
    FAddressLoImpliedZero: Boolean;
    FDescription: string;
    FIsPresent: Boolean;
    FIsReadOnly: Boolean;
    FSpace: Byte;
    function GetAddressSize: DWord;
    function GetSpaceAsHex: string;
  public
    constructor Create;
    procedure LoadByDatagram(ADatagram: TDatagramReceive);
    property AddressLo: DWord read FAddressLo write FAddressLo;
    property AddressLoImpliedZero: Boolean read FAddressLoImpliedZero write FAddressLoImpliedZero;
    property AddressHi: DWord read FAddressHi write FAddressHi;
    property AddressSize: DWord read GetAddressSize;
    property Description: string read FDescription write FDescription;
    property IsReadOnly: Boolean read FIsReadOnly write FIsReadOnly;
    property IsPresent: Boolean read FIsPresent write FIsPresent;
    property Space: Byte read FSpace write FSpace;
    property SpaceAsHex: string read GetSpaceAsHex;
  end;

  { TOlcbMemConfig }

  TOlcbMemConfig = class
  private
    FAddressSpaceList: TList;
    FOptions: TOlcbMemOptions;
    function GetAddressSpace(Index: Integer): TOlcbMemAddressSpace;
    function GetAddressCount: Integer;
    procedure SetAddressSpace(Index: Integer; AValue: TOlcbMemAddressSpace);
  protected
    procedure Clear;
    property AddressSpaceList: TList read FAddressSpaceList write FAddressSpaceList;
  public
    constructor Create;
    destructor Destroy; override;
    function AddAddressSpace(Datagram: TDatagramReceive): TOlcbMemAddressSpace;
    function FindAddressSpaceBySpaceID(AnAddress: Byte): TOlcbMemAddressSpace;

    property AddressSpace[Index: Integer]: TOlcbMemAddressSpace read GetAddressSpace write SetAddressSpace;
    property AddressSpaceCount: Integer read GetAddressCount;
    property Options: TOlcbMemOptions read FOptions;
  end;


implementation

{ TOlcbMemOptions }

function TOlcbMemOptions.GetReadFromMfgACDI: Boolean;
begin
  Result := OperationMask and MCO_ACDI_USER_READS = MCO_ACDI_USER_READS
end;

function TOlcbMemOptions.GetReadFromUserACDI: Boolean;
begin
  Result := OperationMask and MCO_ACDI_MFG_READS = MCO_ACDI_MFG_READS
end;

function TOlcbMemOptions.GetUnAlignedReads: Boolean;
begin
  Result := OperationMask and MCO_UNALIGNED_READS = MCO_UNALIGNED_READS
end;

function TOlcbMemOptions.GetUnAlignedWrites: Boolean;
begin
  Result := OperationMask and MCO_UNALIGNED_WRITES = MCO_UNALIGNED_WRITES
end;

function TOlcbMemOptions.GetWrite64Bytes: Boolean;
begin
  Result := WriteLengthMask and MCWL_64_BYTE = MCWL_64_BYTE
end;

function TOlcbMemOptions.GetWriteArbitraryBytes: Boolean;
begin
  Result := WriteLengthMask and MCWL_ARBITRARY_BYTE = MCWL_ARBITRARY_BYTE
end;

function TOlcbMemOptions.GetWriteFourBytes: Boolean;
begin
  Result := WriteLengthMask and MCWL_FOUR_BYTE = MCWL_FOUR_BYTE
end;

function TOlcbMemOptions.GetWriteOneByte: Boolean;
begin
  Result := WriteLengthMask and MCWL_ONE_BYTE = MCWL_ONE_BYTE
end;

function TOlcbMemOptions.GetWriteStreamBytes: Boolean;
begin
  Result := WriteLengthMask and MCWL_STREAM_WRITE_SUPPORTED = MCWL_STREAM_WRITE_SUPPORTED;
end;

function TOlcbMemOptions.GetWriteToUserACDI: Boolean;
begin
   Result := OperationMask and MCO_ACDI_USER_WRITES  = MCO_ACDI_USER_WRITES;
end;

function TOlcbMemOptions.GetWriteTwoBytes: Boolean;
begin
  Result := WriteLengthMask and MCWL_TWO_BYTE = MCWL_TWO_BYTE
end;

function TOlcbMemOptions.GetWriteUnderMask: Boolean;
begin
  Result := OperationMask and MCO_WRITE_UNDER_MASK = MCO_WRITE_UNDER_MASK
end;

constructor TOlcbMemOptions.Create;
begin
  FAddressSpaceHi := 0;
  FAddressSpaceLo := 0;
  FDescription := '';
  FOperationMask := 0;
  FWriteLengthMask := 0;
end;

procedure TOlcbMemOptions.LoadFromDatagram(Datagram: TDatagramReceive);
var
  i: Integer;
begin
  if (Datagram.RawDatagram[0] = DATAGRAM_PROTOCOL_CONFIGURATION) and (Datagram.RawDatagram[1] and MCP_OP_GET_CONFIG_REPLY = MCP_OP_GET_CONFIG_REPLY) then
  begin
    FOperationMask := Datagram.ExtractDataBytesAsInt(2, 3);
    FWriteLengthMask := Datagram.RawDatagram[4];
    FAddressSpaceHi := Datagram.RawDatagram[5];
    FAddressSpaceLo := Datagram.RawDatagram[6];
    i := 7;
    while (Chr( Datagram.RawDatagram[i]) <> #0) and (i < MAX_DATAGRAM_LENGTH) do
    begin
      FDescription[i - 7] := Chr( Datagram.RawDatagram[i]);
      Inc(i)
    end;
  end;
end;

{ TOlcbMemConfig }

function TOlcbMemConfig.GetAddressSpace(Index: Integer): TOlcbMemAddressSpace;
begin
  Result := nil;
  if (Index > -1) and (Index < AddressSpaceList.Count) then
    Result := TOlcbMemAddressSpace( AddressSpaceList[Index])
end;

function TOlcbMemConfig.GetAddressCount: Integer;
begin
  Result := AddressSpaceList.Count
end;

procedure TOlcbMemConfig.SetAddressSpace(Index: Integer; AValue: TOlcbMemAddressSpace);
begin
  if (Index > -1) and (Index < AddressSpaceList.Count) then
    AddressSpaceList[Index] := AValue                      // We don't free it
end;

procedure TOlcbMemConfig.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to AddressSpaceList.Count - 1 do
      TObject( FAddressSpaceList[i]).Free;
  finally
    AddressSpaceList.Clear;
  end;
end;

constructor TOlcbMemConfig.Create;
begin
  inherited Create;
  FAddressSpaceList := TList.Create;
  FOptions := TOlcbMemOptions.Create;
end;

destructor TOlcbMemConfig.Destroy;
begin
  Clear;
  FreeAndNil(FAddressSpaceList);
  FreeAndNil(FOptions);
  inherited Destroy;
end;

function TOlcbMemConfig.AddAddressSpace(Datagram: TDatagramReceive): TOlcbMemAddressSpace;
begin
  Result := FindAddressSpaceBySpaceID(Datagram.RawDatagram[2]);
  if not Assigned(Result) then
    Result := TOlcbMemAddressSpace.Create;
  Result.LoadByDatagram(Datagram);
end;

function TOlcbMemConfig.FindAddressSpaceBySpaceID(AnAddress: Byte): TOlcbMemAddressSpace;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  while (i < AddressSpaceList.Count) and not Assigned(Result) do
  begin
    if AddressSpace[i].Space = AnAddress then
      Result := AddressSpace[i];
    Inc(i);
  end;
end;


{ TOlcbMemAddressSpace }

function TOlcbMemAddressSpace.GetAddressSize: DWord;
begin
  Result := AddressHi-AddressLo
end;

function TOlcbMemAddressSpace.GetSpaceAsHex: string;
begin
  Result := IntToHex(FSpace, 2);
end;

constructor TOlcbMemAddressSpace.Create;
begin
  FAddressHi := 0;
  FAddressLo := 0;
  FAddressLoImpliedZero := True;
  FDescription := '';
  FIsReadOnly := False;
  FSpace := 0;
  FIsPresent := False;
end;

procedure TOlcbMemAddressSpace.LoadByDatagram(ADatagram: TDatagramReceive);
var
  DescriptionStart: Integer;
  Done: Boolean;
begin
  FAddressHi := ADatagram.ExtractDataBytesAsInt(3, 6);
  FAddressLo := 0;
  FAddressLoImpliedZero := True;
  FIsPresent := ADatagram.RawDatagram[1] and MCP_OP_GET_ADD_SPACE_INFO_REPLY_PRESENT = MCP_OP_GET_ADD_SPACE_INFO_REPLY_PRESENT;
  FIsReadOnly := ADatagram.RawDatagram[7] and $01 = $01;
  FSpace := ADatagram.RawDatagram[2];
  if ADatagram.CurrentPos > 8 then
  begin
    if ADatagram.RawDatagram[7] and $02 = $02 then
    begin
      FAddressLo := ADatagram.ExtractDataBytesAsInt(8, 11);
      DescriptionStart := 12;
    end else
      DescriptionStart := 8;
    Done := False;
    while not Done and (DescriptionStart < MAX_DATAGRAM_LENGTH) do
    begin
      FDescription := FDescription + Chr( ADatagram.RawDatagram[DescriptionStart]);
      Done := Chr( ADatagram.RawDatagram[DescriptionStart]) = #0;
      Inc(DescriptionStart);
    end;
  end;

end;

end.

