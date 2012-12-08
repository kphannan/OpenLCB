unit olcb_mem_protocol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, olcb_utilities, olcb_defines, serialport_thread;

type

  { TOlcbMemOptions }

  TOlcbMemOptions = class
  private
    FOperationsMask: Word;
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
    function GetWriteToUserACDI: Booolean;
    function GetWriteTwoBytes: Boolean;
    function GetWriteUnderMask: Boolean;
  public
    property OperationMask: Word read FOperationsMask write FOperationsMask;
    property WriteLengthMask: Byte read FWriteLengthMask write FWriteLengthMask;
    property WriteUnderMask: Boolean read GetWriteUnderMask;
    property UnAlignedReads: Boolean read GetUnAlignedReads;
    property UnalignedWrites: Boolean read GetUnAlignedWrites;
    property ReadFromMfgACDI: Boolean read GetReadFromMfgACDI;
    property ReadFromUserACDI: Boolean read GetReadFromUserACDI;
    property WriteToUserACDI: Booolean read GetWriteToUserACDI;
    property WriteOneByte: Boolean read GetWriteOneByte;
    property WriteTwoBytes: Boolean read GetWriteTwoBytes;
    property WriteFourBytes: Boolean read GetWriteFourBytes;
    property Write64Bytes: Boolean read GetWrite64Bytes;
    property WriteArbitraryBytes: Boolean read GetWriteArbitraryBytes;
    property WriteStreamBytes: Boolean read GetWriteStreamBytes;

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

  { TOlcbMemAddressList }

  TOlcbMemAddressList = class
  private
    FList: TList;
    function GetAddress(Index: Integer): TOlcbMemAddress;
    function GetCount: Integer;
    procedure SetAddress(Index: Integer; AValue: TOlcbMemAddress);
  protected
    procedure Clear;
    property List: TList read FList write FList;
  public
    constructor Create;
    destructor Destroy; override;
    function Process(AHelper: TOpenLCBMessageHelper): TOlcbMemAddressList;

    property Address[Index: Integer]: TOlcbMemAddress read GetAddress write SetAddress;
    property Count: Integer read GetCount;
  end;


  { TOlcbMemAddressListReceiveManager }

  TOlcbMemAddressListReceiveManager = class
  private
    FMemAddressLists: TList;
    FSourceAlias: Word;
    function GetMemAddressList(Index: Integer): TOlcbMemAddressList;
  protected
    function FindInProcessMemAddressListByAlias(DestinationAlias: Word): TOlcbMemAddressList;
    property MemAddressLists: TList read FMemAddressLists write FMemAddressLists;
    property SourceAlias: Word read FSourceAlias write FSourceAlias;            // Alias of the receiver for the datagrams
  public
    constructor Create(ASourceAlias: Word);
    destructor Destroy; override;
    procedure Clear;
    function Process(AHelper: TOpenLCBMessageHelper): TOlcbMemAddressList;
    property MemAddressList[Index: Integer]: TOlcbMemAddressList read GetMemeAddressList;       // Inprocess and completed Datagrams, the order they are received is preserved
  end;

implementation

{ TOlcbMemOptions }

function TOlcbMemOptions.GetReadFromMfgACDI: Boolean;
begin
  Result := OperationsMask and MCO_ACDI_USER_READS <> 0
end;

function TOlcbMemOptions.GetReadFromUserACDI: Boolean;
begin
  Result := OperationsMask and MCO_ACDI_MFG_READS <> 0
end;

function TOlcbMemOptions.GetUnAlignedReads: Boolean;
begin
  Result := OperationsMask and MCO_UNALIGNED_READS <> 0
end;

function TOlcbMemOptions.GetUnAlignedWrites: Boolean;
begin
  Result := OperationsMask and MCO_UNALIGNED_WRITES <> 0
end;

function TOlcbMemOptions.GetWrite64Bytes: Boolean;
begin
  Result := WriteLengthMask and MCWL_64_BYTE <> 0
end;

function TOlcbMemOptions.GetWriteArbitraryBytes: Boolean;
begin
  Result := WriteLengthMask and MCWL_ARBITRARY_BYTE <> 0
end;

function TOlcbMemOptions.GetWriteFourBytes: Boolean;
begin
  Result := WriteLengthMask and MCWL_FOUR_BYTE <> 0
end;

function TOlcbMemOptions.GetWriteOneByte: Boolean;
begin
  Result := WriteLengthMask and MCWL_ONE_BYTE <> 0
end;

function TOlcbMemOptions.GetWriteStreamBytes: Boolean;
begin
  Result := WriteLengthMask and MCWL_STREAM_WRITE_SUPPORTED <> 0
end;

function TOlcbMemOptions.GetWriteToUserACDI: Booolean;
begin
   Result := OperationsMask and MCO_ACDI_USER_WRITES <> 0
end;

function TOlcbMemOptions.GetWriteTwoBytes: Boolean;
begin
  Result := OperationsMask and MCWL_TWO_BYTE <> 0
end;

function TOlcbMemOptions.GetWriteUnderMask: Boolean;
begin
  Result := OperationsMask and MCO_WRITE_UNDER_MASK <> 0
end;

{ TOlcbMemAddressListReceiveManager }

function TOlcbMemAddressListReceiveManager.GetMemAddressList(Index: Integer ): TOlcbMemAddressList;
begin
  if (Index > -1) and (Index < MemAddressLists.Count) then
    Result := TOlcbMemAddressList( MemAddressLists[Index])
  else
    Result := nil
end;

function TOlcbMemAddressListReceiveManager.FindInProcessMemAddressListByAlias(DestinationAlias: Word): TOlcbMemAddressList;
var
  i: Integer;
begin
  i := 0;
  Result := nil;
  while not Assigned(Result) and (i < MemAddressLists.Count) do
  begin
    if (MemAddressList[i].DestinationAlias = DestinationAlias) and not MemAddressList[i].Full then
      Result := MemAddressList[i];
    Inc(i)
  end;
end;

constructor TOlcbMemAddressListReceiveManager.Create(ASourceAlias: Word);
begin
  inherited Create;
  FSourceAlias := ASourceAlias;
  FMemAddressLists := TList.Create;
end;

destructor TOlcbMemAddressListReceiveManager.Destroy;
begin
  Clear;
  FreeAndNil(FMemAddressLists);
  inherited Destroy;
end;

procedure TOlcbMemAddressListReceiveManager.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to FMemAddressLists.Count - 1 do
      TObject( MemAddressLists[i]).Free;
  finally
    FMemAddressLists.Clear;
  end;
end;

function TOlcbMemAddressListReceiveManager.Process(AHelper: TOpenLCBMessageHelper): TOlcbMemAddressList;
var
  TestMemList: TOlcbMemAddressList;
begin
  if AHelper.MTI =  then
  begin
    TestMemList := FindInProcessMemAddressListByAlias(AHelper.SourceAliasID);
    if not Assigned(TestMemList) then
    begin
      TestMemList := TOlcbMemAddressList.Create(SourceAlias, AHelper.SourceAliasID);  // Create a new receiving Snii object for source alias of the message to us
      MemAddressLists.Add(TestMemList);
    end;
    Result := TestMemList.Process(AHelper);
    if Assigned(Result) then                                        // If it is complete then it is returned AND removed from the list, it is now owned by the caller
      MemAddressLists.Remove(Result);
  end;

end;

{ TOlcbMemAddressList }

function TOlcbMemAddressList.GetAddress(Index: Integer): TOlcbMemAddress;
begin
  Result := nil;
  if (Index > -1) and (Index < List.Count) then
    Result := TOlcbMemAddress( List[Index])
end;

function TOlcbMemAddressList.GetCount: Integer;
begin
  Result := List.Count
end;

procedure TOlcbMemAddressList.SetAddress(Index: Integer; AValue: TOlcbMemAddress);
begin
  if (Index > -1) and (Index < List.Count) then
    List[Index] := AValue                      // We don't free it
end;

procedure TOlcbMemAddressList.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to List.Count - 1 do
      TObject( FList[i]).Free;
  finally
    List.Clear;
  end;
end;

constructor TOlcbMemAddressList.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TOlcbMemAddressList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

function TOlcbMemAddressList.Process(AHelper: TOpenLCBMessageHelper): TOlcbMemAddressList;
begin
  Result := nil;
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

