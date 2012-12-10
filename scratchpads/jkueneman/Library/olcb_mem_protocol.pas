unit olcb_mem_protocol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, olcb_utilities, olcb_defines, serialport_thread, datagram;

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

  { TOlcbMemAddress }

  TOlcbMemAddress = class
  private
    FAddressHi: DWord;
    FAddressLo: DWord;
    FAddressLoImpliedZero: Boolean;
    FDescription: string;
    FIsReadOnly: Boolean;
    FSpace: Byte;
    function GetAddressSize: DWord;
  public
    constructor Create;
    property AddressLo: DWord read FAddressLo write FAddressLo;
    property AddressLoImpliedZero: Boolean read FAddressLoImpliedZero write FAddressLoImpliedZero;
    property AddressHi: DWord read FAddressHi write FAddressHi;
    property AddressSize: DWord read GetAddressSize;
    property Description: string read FDescription write FDescription;
    property IsReadOnly: Boolean read FIsReadOnly write FIsReadOnly;
    property Space: Byte read FSpace write FSpace;
  end;

  { TOlcbMemConfig }

  TOlcbMemConfig = class
  private
    FAddressList: TList;
    FOptions: TOlcbMemOptions;
    function GetAddress(Index: Integer): TOlcbMemAddress;
    function GetAddressCount: Integer;
    procedure SetAddress(Index: Integer; AValue: TOlcbMemAddress);
  protected
    procedure Clear;
    property AddressList: TList read FAddressList write FAddressList;
  public
    constructor Create;
    destructor Destroy; override;

    property Address[Index: Integer]: TOlcbMemAddress read GetAddress write SetAddress;
    property AddressCount: Integer read GetAddressCount;
    property Options: TOlcbMemOptions read FOptions;
  end;


implementation

const
  STATE_OPTIONS_REPLY         = 0;
  STATE_DONE                  = 10;

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
    while (Chr( Datagram.RawDatagram[i]) <> #0) and (i < DATAGRAM_LENGTH) do
    begin
      FDescription[i - 7] := Chr( Datagram.RawDatagram[i]);
      Inc(i)
    end;
  end;
end;

{ TOlcbMemConfig }

function TOlcbMemConfig.GetAddress(Index: Integer): TOlcbMemAddress;
begin
  Result := nil;
  if (Index > -1) and (Index < AddressList.Count) then
    Result := TOlcbMemAddress( AddressList[Index])
end;

function TOlcbMemConfig.GetAddressCount: Integer;
begin
  Result := AddressList.Count
end;

procedure TOlcbMemConfig.SetAddress(Index: Integer; AValue: TOlcbMemAddress);
begin
  if (Index > -1) and (Index < AddressList.Count) then
    AddressList[Index] := AValue                      // We don't free it
end;

procedure TOlcbMemConfig.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to AddressList.Count - 1 do
      TObject( FAddressList[i]).Free;
  finally
    AddressList.Clear;
  end;
end;

constructor TOlcbMemConfig.Create;
begin
  inherited Create;
  FAddressList := TList.Create;
  FOptions := TOlcbMemOptions.Create;
end;

destructor TOlcbMemConfig.Destroy;
begin
  Clear;
  FreeAndNil(FAddressList);
  FreeAndNil(FOptions);
  inherited Destroy;
end;


{ TOlcbMemAddress }

function TOlcbMemAddress.GetAddressSize: DWord;
begin
  Result := AddressHi-AddressLo
end;

constructor TOlcbMemAddress.Create;
begin
  FAddressHi := 0;
  FAddressLo := 0;
  FAddressLoImpliedZero := True;
  FDescription := '';
  FIsReadOnly := False;
  FSpace := 0;
end;

end.

