unit olcb_app_common_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Menus, ActnList, ExtCtrls, Buttons, IniFiles;

 // Classes, SysUtils, FileUtil, IniFiles;

const
  STR_INI_COMPORT_SECTION = 'ComPort';
  STR_INI_COMPORT_PORT_KEY = 'Port';
  STR_INI_COMPORT_BAUD_KEY = 'Baud';
  STR_INI_COMPORT_BITS_KEY = 'DataBits';
  STR_INI_COMPORT_PARITY_KEY = 'Parity';
  STR_INI_COMPORT_STOPBITS_KEY = 'StopBits';
  STR_INI_COMPORT_FLOWCONTROL_KEY = 'FlowControl';
  STR_INI_COMPORT_AUTOCONNECT = 'AutoConnect';

  STR_INT_GENERAL_SECTION = 'General';
  STR_INI_ALIASID = 'AliasID';
  STR_INI_NODEID = 'NodeID';
  STR_INI_SENDPACKETDELAY = 'SendDelay';
  STR_INI_AUTOSCAN = 'AutoScan';

  STR_INI_THROTTLE_SECTION = 'Throttle';
  STR_INI_THROTTLE_AUTOLOADFDI = 'AutoLoadFDI';

  STR_INI_ETHERNET_SECTION = 'Ethernet';
  STR_INI_ETHERNET_LOCAL_IP = 'LocalIP';
  STR_INI_ETHERNET_LOCAL_PORT = 'LocalPort';



  MAX_MESSAGE_WAIT_TIME_DEFAULT = 5000;    // 5 seconds
  MAX_DATAGRAM_WAIT_TIME_DEFAULT = 20000;  // 20 seconds

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
    FAutoConnectAtBoot: Boolean;
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

    property AutoConnectAtBoot: Boolean read FAutoConnectAtBoot write FAutoConnectAtBoot;
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
    FAutoScanNetworkAtBoot: Boolean;
    FDatagramWaitTime: DWord;
    FMessageWaitTime: DWord;
    FNodeID: string;
    FSendPacketDelay: Word;
    procedure SetAliasID(AValue: string);
  public
    constructor Create;
    procedure LoadFromFile(IniFile: TIniFile);
    procedure SaveToFile(IniFile: TIniFile);
    function AliasIDAsVal: Word;
    function NodeIDAsVal: DWord;
    property AutoScanNetworkAtBoot: Boolean read FAutoScanNetworkAtBoot write FAutoScanNetworkAtBoot;
    property AliasID: string read FAliasID write SetAliasID;
    property DatagramWaitTime: DWord read FDatagramWaitTime write FDatagramWaitTime;
    property NodeID: string read FNodeID write FNodeID;
    property SendPacketDelay: Word read FSendPacketDelay write FSendPacketDelay;
    property MessageWaitTime: DWord read FMessageWaitTime write FMessageWaitTime;
  end;

  { TThrottleSettings }

  TThrottleSettings = class
  private
    FAutoLoadFDI: Boolean;
  public
    constructor Create;
    procedure LoadFromFile(IniFile: TIniFile);
    procedure SaveToFile(IniFile: TIniFile);
    property AutoLoadFDI: Boolean read FAutoLoadFDI write FAutoLoadFDI;
  end;

  { TEthernetSettings }

  TEthernetSettings = class
  private
    FLocalIP: string;
    FLocalPort: Integer;
  public
    constructor Create;
    procedure LoadFromFile(IniFile: TIniFile);
    procedure SaveToFile(IniFile: TIniFile);
    property LocalIP: string read FLocalIP write FLocalIP;
    property LocalPort: Integer read FLocalPort write FLocalPort;
  end;

  // Settings that all OLCB Applications will have in common

  { TOlcbCommonSettings }

  TOlcbCommonSettings = class
  private
    FComPort: TComPortSettings;
    FEthernet: TEthernetSettings;
    FGeneral: TGeneralSettings;
    FThrottle: TThrottleSettings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);
    property ComPort: TComPortSettings read FComPort write FComPort;
    property Ethernet: TEthernetSettings read FEthernet write FEthernet;
    property General: TGeneralSettings read FGeneral write FGeneral;
    property Throttle: TThrottleSettings read FThrottle write FThrottle;
  end;

var
  GlobalSettings: TOlcbCommonSettings;
  GlobalSettingLock: TRTLCriticalSection;

implementation

{ TEthernetSettings }

constructor TEthernetSettings.Create;
begin
  LocalIP := '0.0.0.0';
  LocalPort := 12021;
end;

procedure TEthernetSettings.LoadFromFile(IniFile: TIniFile);
begin
  LocalIP := IniFile.ReadString(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_LOCAL_IP, '0.0.0.0');
  LocalPort := IniFile.ReadInteger(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_LOCAL_PORT, 12021);
end;

procedure TEthernetSettings.SaveToFile(IniFile: TIniFile);
begin
  IniFile.WriteString(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_LOCAL_IP, FLocalIP);
  IniFile.WriteInteger(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_LOCAL_PORT, FLocalPort);
end;

{ TThrottleSettings }

constructor TThrottleSettings.Create;
begin
  FAutoLoadFDI := True;
end;

procedure TThrottleSettings.LoadFromFile(IniFile: TIniFile);
begin
  AutoLoadFDI := IniFile.ReadBool(STR_INI_THROTTLE_SECTION, STR_INI_THROTTLE_AUTOLOADFDI, True);
end;

procedure TThrottleSettings.SaveToFile(IniFile: TIniFile);
begin
  IniFile.WriteBool(STR_INI_THROTTLE_SECTION, STR_INI_THROTTLE_AUTOLOADFDI, FAutoLoadFDI);
end;

{ TGeneralSettings }

procedure TGeneralSettings.SetAliasID(AValue: string);
begin
  if FAliasID=AValue then Exit;
  EnterCriticalsection(GlobalSettingLock);
  FAliasID:=AValue;
  LeaveCriticalsection(GlobalSettingLock);
end;

constructor TGeneralSettings.Create;
begin
  FAliasID := '0x0AAA';
  FNodeID := '0x010203040506';
  FSendPacketDelay := 0;
  FMessageWaitTime := MAX_MESSAGE_WAIT_TIME_DEFAULT;
  FDatagramWaitTime := MAX_DATAGRAM_WAIT_TIME_DEFAULT;
end;

procedure TGeneralSettings.LoadFromFile(IniFile: TIniFile);
begin
  AliasID := IniFile.ReadString(STR_INT_GENERAL_SECTION, STR_INI_ALIASID, '0x0AAA');
  NodeID := IniFile.ReadString(STR_INT_GENERAL_SECTION, STR_INI_NODEID, '0x102030405006');
  SendPacketDelay := IniFile.ReadInteger(STR_INT_GENERAL_SECTION, STR_INI_SENDPACKETDELAY, 0);
  AutoScanNetworkAtBoot := IniFile.ReadBool(STR_INT_GENERAL_SECTION, STR_INI_AUTOSCAN, True);
end;

procedure TGeneralSettings.SaveToFile(IniFile: TIniFile);
begin
  IniFile.WriteString(STR_INT_GENERAL_SECTION, STR_INI_ALIASID, FAliasID);
  IniFile.WriteString(STR_INT_GENERAL_SECTION, STR_INI_NODEID, FNodeID);
  IniFile.WriteInteger(STR_INT_GENERAL_SECTION, STR_INI_SENDPACKETDELAY, FSendPacketDelay);
  IniFile.WriteBool(STR_INT_GENERAL_SECTION, STR_INI_AUTOSCAN, FAutoScanNetworkAtBoot);
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
  FBaudRate := 333333;
  FDataBits := 8;
  FParity := cpp_None;
  FPort := 'COM2';
  FStopBits := 0;
  FFlowControl := cpf_None;
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
  AutoConnectAtBoot := IniFile.ReadBool(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_AUTOCONNECT, False);
end;

procedure TComPortSettings.SaveToFile(IniFile: TIniFile);
begin
  IniFile.WriteString(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_PORT_KEY, Port);
  IniFile.WriteInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_BAUD_KEY, BaudRate);
  IniFile.WriteInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_BITS_KEY, DataBits);
  IniFile.WriteInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_STOPBITS_KEY, StopBits);
  IniFile.WriteInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_PARITY_KEY, Integer( Parity));
  IniFile.WriteInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_FLOWCONTROL_KEY, Integer( FlowControl));
  IniFile.WriteBool(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_AUTOCONNECT, FAutoConnectAtBoot);
end;


{ TOlcbCommonSettings }

constructor TOlcbCommonSettings.Create;
begin
  inherited;
  FComPort := TComPortSettings.Create;
  FGeneral := TGeneralSettings.Create;
  FThrottle := TThrottleSettings.Create;
  FEthernet := TEthernetSettings.Create;
end;

destructor TOlcbCommonSettings.Destroy;
begin
  FreeAndNil(FComPort);
  FreeAndNil(FGeneral);
  FreeAndNil(FThrottle);
  FreeAndNil(FEthernet);
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
   Throttle.LoadFromFile(IniFile);
   Ethernet.LoadFromFile(IniFile);
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
    Throttle.SaveToFile(IniFile);
    Ethernet.SaveToFile(IniFile);
  finally
    IniFile.Free;
  end;
end;

initialization
  GlobalSettings := TOlcbCommonSettings.Create;
  InitCriticalSection(GlobalSettingLock);

finalization
  FreeAndNil(GlobalSettings);
  DoneCriticalsection(GlobalSettingLock);

end.
