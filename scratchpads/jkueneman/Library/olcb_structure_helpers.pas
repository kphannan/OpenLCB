unit olcb_structure_helpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, olcb_utilities, olcb_defines, olcb_threaded_stack;

type

  TOlcbStructureHelperBase = class
  public
    procedure CopyTo(Target: TOlcbStructureHelperBase); virtual; abstract;
  end;

  { TOlcbMemOptions }

  TOlcbMemOptions = class(TOlcbStructureHelperBase)
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
    procedure CopyTo(Target: TOlcbStructureHelperBase); override;
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

  TOlcbMemAddressSpace = class(TOlcbStructureHelperBase)
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
    procedure CopyTo(Target: TOlcbStructureHelperBase); override;
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

  TOlcbMemConfig = class(TOlcbStructureHelperBase)
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
    function AddAddressSpace: TOlcbMemAddressSpace;
    function AddAddressSpaceByDatagram(Datagram: TDatagramReceive): TOlcbMemAddressSpace;
    procedure CopyTo(Target: TOlcbStructureHelperBase); override;
    function FindAddressSpaceBySpaceID(AnAddress: Byte): TOlcbMemAddressSpace;

    property AddressSpace[Index: Integer]: TOlcbMemAddressSpace read GetAddressSpace write SetAddressSpace;
    property AddressSpaceCount: Integer read GetAddressCount;
    property Options: TOlcbMemOptions read FOptions;
  end;

  { TOlcbProtocolIdentification }

  TOlcbProtocolIdentification = class(TOlcbStructureHelperBase)
  private
    FMask: QWord;
    function GetAbbreviatedCDIProtocol: Boolean;
    function GetConfigDescriptionInfoProtocol: Boolean;
    function GetDatagramProtocol: Boolean;
    function GetFunctionStateInformationProtocol: Boolean;
    function GetDisplayProtocol: Boolean;
    function GetEventExchangeProtocol: Boolean;
    function GetFunctionDescriptionInfoProtocol: Boolean;
    function GetIdentificationProtocol: Boolean;
    function GetMemoryConfigProtocol: Boolean;
    function GetRemoteButtonProtocol: Boolean;
    function GetReservationProtocol: Boolean;
    function GetSimpleNodeInfoProtocol: Boolean;
    function GetSimpleProtocol: Boolean;
    function GetStreamProtocol: Boolean;
    function GetTeachingLearningConfigProtocol: Boolean;
    function GetTractionControlProtocol: Boolean;
  protected
    property Mask: QWord read FMask write FMask;
  public
    procedure CopyTo(Target: TOlcbStructureHelperBase); override;
    procedure LoadByMessage(AHelper: TOpenLCBMessageHelper);
    property SimpleProtocol: Boolean read GetSimpleProtocol;
    property DatagramProtocol: Boolean read GetDatagramProtocol;
    property StreamProtocol: Boolean read GetStreamProtocol;
    property MemoryConfigProtocol: Boolean read GetMemoryConfigProtocol;
    property ReservationProtocol: Boolean read GetReservationProtocol;
    property EventExchangeProtocol: Boolean read GetEventExchangeProtocol;
    property IdentificiationProtcol: Boolean read GetIdentificationProtocol;
    property TeachingLearningConfigProtocol: Boolean read GetTeachingLearningConfigProtocol;
    property RemoteButtonProtocol: Boolean read GetRemoteButtonProtocol;
    property AbbreviatedCDIProtocol: Boolean read GetAbbreviatedCDIProtocol;
    property DisplayProtocol: Boolean read GetDisplayProtocol;
    property SimpleNodeInfoProtocol: Boolean read GetSimpleNodeInfoProtocol;
    property ConfigDescriptionInfoProtocol: Boolean read GetConfigDescriptionInfoProtocol;
    property TractionControlProtocol: Boolean read GetTractionControlProtocol;
    property FunctionDescriptionInfoProtocol: Boolean read GetFunctionDescriptionInfoProtocol;
    property FunctionStateInformationProtocol: Boolean read GetFunctionStateInformationProtocol;
  end;

  { TOlcbSNIP }

  TOlcbSNIP = class(TOlcbStructureHelperBase)
  private
    FSniiHardwareVersion: string;
    FSniiMfgModel: string;
    FSniiMfgName: string;
    FSniiMfgVersion: Byte;
    FSniiSoftwareVersion: string;
    FSniiUserDescription: string;
    FSniiUserName: string;
    FSniiUserVersion: Byte;
  public
    constructor Create;
    procedure CopyTo(Target: TOlcbStructureHelperBase); override;
    property SniiMfgName: string read FSniiMfgName write FSniiMfgName;
    property SniiMfgModel: string read FSniiMfgModel write FSniiMfgModel;
    property SniiSoftwareVersion: string read FSniiSoftwareVersion write FSniiSoftwareVersion;
    property SniiHardwareVersion: string read FSniiHardwareVersion write FSniiHardwareVersion;
    property SniiUserName: string read FSniiUserName write FSniiUserName;
    property SniiUserDescription: string read FSniiUserDescription write FSniiUserDescription;
    property SniiUserVersion: Byte read FSniiUserVersion write FSniiUserVersion;
    property SniiMfgVersion: Byte read FSniiMfgVersion write FSniiMfgVersion;
  end;


implementation

{ TOlcbSNIP }

constructor TOlcbSNIP.Create;
begin
  FSniiHardwareVersion := '';
  FSniiMfgModel := '';
  FSniiMfgName := '';
  FSniiMfgVersion := 0;
  FSniiSoftwareVersion := '';
  FSniiUserDescription := '';
  FSniiUserName := '';
  FSniiUserVersion := 0;
end;

procedure TOlcbSNIP.CopyTo(Target: TOlcbStructureHelperBase);
var
  X: TOlcbSNIP;
begin
  if Target is TOlcbSNIP then
  begin
    X := TOlcbSNIP( Target);
    X.FSniiHardwareVersion := SniiHardwareVersion;
    X.FSniiMfgModel := SniiMfgModel;
    X.FSniiMfgName := SniiMfgName;
    X.FSniiMfgVersion := SniiMfgVersion;
    X.FSniiSoftwareVersion := SniiSoftwareVersion;
    X.FSniiUserDescription := SniiUserDescription;
    X.FSniiUserName := SniiUserName;
    X.FSniiUserVersion := SniiUserVersion;
  end;
end;

{ TOlcbProtocolIdentification }

function TOlcbProtocolIdentification.GetAbbreviatedCDIProtocol: Boolean;
begin
  Result := Mask and PIP_ABBREVIATED_CDI = PIP_ABBREVIATED_CDI;
end;

function TOlcbProtocolIdentification.GetConfigDescriptionInfoProtocol: Boolean;
begin
  Result := Mask and PIP_CDI = PIP_CDI;
end;

function TOlcbProtocolIdentification.GetDatagramProtocol: Boolean;
begin
  Result := Mask and PIP_DATAGRAM = PIP_DATAGRAM;
end;

function TOlcbProtocolIdentification.GetFunctionStateInformationProtocol: Boolean;
begin
  Result := Mask and PIP_FSI = PIP_FSI
end;

function TOlcbProtocolIdentification.GetDisplayProtocol: Boolean;
begin
  Result := Mask and PIP_DISPLAY = PIP_DISPLAY;
end;

function TOlcbProtocolIdentification.GetEventExchangeProtocol: Boolean;
begin
  Result := Mask and PIP_EVENT_EXCHANGE = PIP_EVENT_EXCHANGE;
end;

function TOlcbProtocolIdentification.GetFunctionDescriptionInfoProtocol: Boolean;
begin
  Result := Mask and PIP_FDI = PIP_FDI;
end;

function TOlcbProtocolIdentification.GetIdentificationProtocol: Boolean;
begin
  Result := Mask and PIP_IDENTIFCIATION = PIP_IDENTIFCIATION;
end;

function TOlcbProtocolIdentification.GetMemoryConfigProtocol: Boolean;
begin
  Result := Mask and PIP_MEMORY_CONFIG = PIP_MEMORY_CONFIG;
end;

function TOlcbProtocolIdentification.GetRemoteButtonProtocol: Boolean;
begin
  Result := Mask and PIP_REMOTE_BUTTON = PIP_REMOTE_BUTTON;
end;

function TOlcbProtocolIdentification.GetReservationProtocol: Boolean;
begin
  Result := Mask and PIP_RESERVATION = PIP_RESERVATION;
end;

function TOlcbProtocolIdentification.GetSimpleNodeInfoProtocol: Boolean;
begin
  Result := Mask and PIP_SIMPLE_NODE_ID = PIP_SIMPLE_NODE_ID;
end;

function TOlcbProtocolIdentification.GetSimpleProtocol: Boolean;
begin
  Result := Mask and PIP_PIP = PIP_PIP;
end;

function TOlcbProtocolIdentification.GetStreamProtocol: Boolean;
begin
  Result := Mask and PIP_STREAM = PIP_STREAM;
end;

function TOlcbProtocolIdentification.GetTeachingLearningConfigProtocol: Boolean;
begin
  Result := Mask and PIP_TEACH_LEARN = PIP_TEACH_LEARN;
end;

function TOlcbProtocolIdentification.GetTractionControlProtocol: Boolean;
begin
  Result := Mask and PIP_TRACTION = PIP_TRACTION;
end;

procedure TOlcbProtocolIdentification.CopyTo(Target: TOlcbStructureHelperBase);
var
  X: TOlcbProtocolIdentification;
begin
  if Target is TOlcbProtocolIdentification then
  begin
    X := TOlcbProtocolIdentification( Target);
    X.Mask := Mask;
  end;
end;

procedure TOlcbProtocolIdentification.LoadByMessage(AHelper: TOpenLCBMessageHelper);
begin
  FMask := AHelper.ExtractDataBytesAsInt(2, 7);
end;

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

procedure TOlcbMemOptions.CopyTo(Target: TOlcbStructureHelperBase);
var
  X: TOlcbMemOptions;
begin
  if Target is TOlcbMemOptions then
  begin
    X := TOlcbMemOptions( Target);
    X.FAddressSpaceHi := AddressSpaceHi;
    X.FAddressSpaceLo := AddressSpaceLo;
    X.FDescription := Description;
    X.FOperationMask := OperationMask;
    X.FWriteLengthMask := WriteLengthMask;
  end;
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

function TOlcbMemConfig.AddAddressSpace: TOlcbMemAddressSpace;
begin
  Result := TOlcbMemAddressSpace.Create;
  AddressSpaceList.Add(Result);
end;

function TOlcbMemConfig.AddAddressSpaceByDatagram(Datagram: TDatagramReceive): TOlcbMemAddressSpace;
begin
  Result := FindAddressSpaceBySpaceID(Datagram.RawDatagram[2]);
  if not Assigned(Result) then
  begin
    Result := TOlcbMemAddressSpace.Create;
    AddressSpaceList.Add(Result);
  end;
  Result.LoadByDatagram(Datagram);
end;

procedure TOlcbMemConfig.CopyTo(Target: TOlcbStructureHelperBase);
var
  X: TOlcbMemConfig;
  i: Integer;
  NewSpace: TOlcbMemAddressSpace;
begin
  if Target is TOlcbMemConfig then
  begin
    X := TOlcbMemConfig( Target);
    X.Clear;
    for i := 0 to AddressSpaceCount - 1 do
    begin
      NewSpace := TOlcbMemAddressSpace.Create;
      AddressSpace[i].CopyTo(NewSpace);
      X.AddressSpaceList.Add(NewSpace);
    end;
  end;
  Options.CopyTo(X.Options);
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

procedure TOlcbMemAddressSpace.CopyTo(Target: TOlcbStructureHelperBase);
var
  X: TOlcbMemAddressSpace;
begin
  if Target is TOlcbMemAddressSpace then
  begin
    X := TOlcbMemAddressSpace( Target);
    X.FAddressHi := AddressHi;
    X.FAddressLo := AddressLo;
    X.FAddressLoImpliedZero := AddressLoImpliedZero;
    X.FDescription := Description;
    X.FIsPresent := IsPresent;
    X.FIsReadOnly := IsReadOnly;
    X.FSpace := Space;
  end;
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
