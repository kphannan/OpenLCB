unit olcb_app_common_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IniFiles;

const
  STR_INI_COMPORT_SECTION = 'ComPort';
  STR_INI_COMPORT_PORT_KEY = 'Port';
  STR_INI_COMPORT_BAUD_KEY = 'Baud';
  STR_INI_COMPORT_BITS_KEY = 'DataBits';
  STR_INI_COMPORT_PARITY_KEY = 'Parity';
  STR_INI_COMPORT_STOPBITS_KEY = 'StopBits';
  STR_INI_COMPORT_FLOWCONTROL_KEY = 'FlowControl';

  STR_INT_GENERAL_SECTION = 'General';
  STR_INI_ALIASID = 'AliasID';
  STR_INI_NODEID = 'NodeID';
  STR_INI_SENDPACKETDELAY = 'SendDelay';

type
  TComPortParity = (
    cpp_None,
    cpp_Even,
    cpp_Odd,
    cpp_Mark,
    cpp_Space
  );
const
  HI_PARTITY_TYPE = 4;

type
  TComPortFlowControl = (
    cpf_None,
    cpf_CTS_RTS,            // Hardware with CTS/RTS
    cpf_DTR_DSR,            // Hardware with DTR/DSR
    cpf_XON_XOFF            // Software
  );

const
  HI_FLOWCONTROL_TYPE = 3;

type

  { TComPortSettings }

  TComPortSettings = class
  private
    FBaudRate: DWord;
    FDataBits: Byte;
    FFlowControl: TComPortFlowControl;
    FParity: TComPortParity;
    FPort: string;
    FStopBits: Byte;
  public
    constructor Create;
    procedure LoadFromFile(IniFile: TIniFile);
    procedure SaveToFile(IniFile: TIniFile);
    property BaudRate: DWord read FBaudRate write FBaudRate;
    property DataBits: Byte read FDataBits write FDataBits;
    property Parity: TComPortParity read FParity write FParity;
    property Port: string read FPort write FPort;
    property StopBits: Byte read FStopBits write FStopBits;
    property FlowControl: TComPortFlowControl read FFlowControl write FFlowControl;
  end;

  { TGeneralSettings }

  TGeneralSettings = class
  private
    FAliasID: string;
    FNodeID: string;
    FSendPacketDelay: Word;
  public
    constructor Create;
    procedure LoadFromFile(IniFile: TIniFile);
    procedure SaveToFile(IniFile: TIniFile);
    function AliasIDAsVal: Word;
    function NodeIDAsVal: DWord;
    property AliasID: string read FAliasID write FAliasID;
    property NodeID: string read FNodeID write FNodeID;
    property SendPacketDelay: Word read FSendPacketDelay write FSendPacketDelay;
  end;

  // Settings that all OLCB Applications will have in common

  { TOlcbCommonSettings }

  TOlcbCommonSettings = class
  private
    FAliasID: Word;
    FComPort: TComPortSettings;
    FGeneral: TGeneralSettings;
    FNodeID: DWord;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);
    property ComPort: TComPortSettings read FComPort write FComPort;
    property General: TGeneralSettings read FGeneral write FGeneral;

  end;

var
  GlobalSettings: TOlcbCommonSettings;

implementation

{ TGeneralSettings }

constructor TGeneralSettings.Create;
begin
  AliasID := '0x0AAA';
  NodeID := '0x010203040506';
  SendPacketDelay := 0;
end;

procedure TGeneralSettings.LoadFromFile(IniFile: TIniFile);
begin
  AliasID := IniFile.ReadString(STR_INT_GENERAL_SECTION, STR_INI_ALIASID, '0x0AAA');
  NodeID := IniFile.ReadString(STR_INT_GENERAL_SECTION, STR_INI_NODEID, '0x102030405006');
  SendPacketDelay := IniFile.ReadInteger(STR_INT_GENERAL_SECTION, STR_INI_SENDPACKETDELAY, 0);
end;

procedure TGeneralSettings.SaveToFile(IniFile: TIniFile);
begin
  IniFile.WriteString(STR_INT_GENERAL_SECTION, STR_INI_ALIASID, FAliasID);
  IniFile.WriteString(STR_INT_GENERAL_SECTION, STR_INI_NODEID, FNodeID);
  IniFile.WriteInteger(STR_INT_GENERAL_SECTION, STR_INI_SENDPACKETDELAY, FSendPacketDelay);
end;

function TGeneralSettings.AliasIDAsVal: Word;
begin
  Result := StrToInt(AliasID);
end;

function TGeneralSettings.NodeIDAsVal: DWord;
begin
  Result := StrToInt(NodeID)
end;

{ TComPortSettings }

constructor TComPortSettings.Create;
begin
  BaudRate := 333333;
  DataBits := 8;
  Parity := cpp_None;
  Port := 'COM2';
  StopBits := 0;
  FlowControl := cpf_None;
end;

procedure TComPortSettings.LoadFromFile(IniFile: TIniFile);
var
  Value: QWord;
begin
  Port := IniFile.ReadString(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_PORT_KEY, 'COM2');
  BaudRate := IniFile.ReadInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_BAUD_KEY, 333333);
  DataBits := IniFile.ReadInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_BITS_KEY, 8);
  StopBits := IniFile.ReadInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_STOPBITS_KEY, 0);
  Value := IniFile.ReadInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_PARITY_KEY, 0);
  if Value > HI_PARTITY_TYPE then
    Value := 0;
  Parity := TComPortParity( Value);

  Value := IniFile.ReadInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_FLOWCONTROL_KEY, 0);
  if Value > HI_FLOWCONTROL_TYPE then
    Value := 0;
  FlowControl := TComPortFlowControl( Value);
end;

procedure TComPortSettings.SaveToFile(IniFile: TIniFile);
begin
  IniFile.WriteString(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_PORT_KEY, Port);
  IniFile.WriteInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_BAUD_KEY, BaudRate);
  IniFile.WriteInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_BITS_KEY, DataBits);
  IniFile.WriteInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_STOPBITS_KEY, StopBits);
  IniFile.WriteInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_PARITY_KEY, Integer( Parity));
  IniFile.WriteInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_FLOWCONTROL_KEY, Integer( FlowControl));
end;


{ TOlcbCommonSettings }

constructor TOlcbCommonSettings.Create;
begin
  inherited;
  ComPort := TComPortSettings.Create;
  General := TGeneralSettings.Create;
end;

destructor TOlcbCommonSettings.Destroy;
begin
  FreeAndNil(FComPort);
  FreeAndNil(FGeneral);
  inherited Destroy;
end;

procedure TOlcbCommonSettings.LoadFromFile(FileName: string);
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(FileName);
  try
   ComPort.LoadFromFile(IniFile);
   General.LoadFromFile(IniFile);
  finally
    IniFile.Free;
  end;
end;

procedure TOlcbCommonSettings.SaveToFile(FileName: string);
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(FileName);
  try
    ComPort.SaveToFile(IniFile);
    General.SaveToFile(IniFile);
  finally
    IniFile.Free;
  end;
end;

initialization
  GlobalSettings := TOlcbCommonSettings.Create;

finalization
  FreeAndNil(GlobalSettings)

end.

