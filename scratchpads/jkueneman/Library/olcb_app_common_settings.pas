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
  STR_INI_JMRI_FORMAT = 'JMRI_Format';
  STR_INI_LOGGING     = 'Logging';
  STR_INI_DETAILED_LOGGING = 'DetailedLogging';

  STR_INI_THROTTLE_SECTION = 'Throttle';
  STR_INI_THROTTLE_AUTOLOADFDI = 'AutoLoadFDI';

  STR_INI_ETHERNET_SECTION = 'Ethernet';
  STR_INI_ETHERNET_LOCAL_IP = 'LocalIP';
  STR_INI_ETHERNET_REMOTE_IP = 'RemoteIP';
  STR_INI_ETHERNET_CLIENT_PORT = 'ClientPort';
  STR_INI_ETHERNET_LISTEN_PORT = 'ListenPort';



  MAX_MESSAGE_WAIT_TIME_DEFAULT = 5000;    // 5 seconds
  MAX_DATAGRAM_WAIT_TIME_DEFAULT = 20000;  // 20 seconds
  MAX_STREAM_WAIT_TIME_DEFAULT = 20000;

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
    FDetailedLogging: Boolean;
    FJMRILogFormat: Boolean;
    FLogging: Boolean;
    FMessageWaitTime: DWord;
    FNodeID: string;
    FSendPacketDelay: Word;
    FStreamWaitTime: DWord;
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
    property StreamWaitTime: DWord read FStreamWaitTime write FStreamWaitTime;
    property MessageWaitTime: DWord read FMessageWaitTime write FMessageWaitTime;
    property JMRILogFormat: Boolean read FJMRILogFormat write FJMRILogFormat;
    property Logging: Boolean read FLogging write FLogging;
    property DetailedLogging: Boolean read FDetailedLogging write FDetailedLogging;
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
    FClientPort: Integer;
    FListenPort: Integer;
    FLocalIP: string;
    FRemoteIP: string;
  public
    constructor Create;
    procedure LoadFromFile(IniFile: TIniFile);
    procedure SaveToFile(IniFile: TIniFile);
    property LocalIP: string read FLocalIP write FLocalIP;
    property RemoteIP: string read FRemoteIP write FRemoteIP;
    property ListenPort: Integer read FListenPort write FListenPort;            // Storage if the connection is a listener
    property ClientPort: Integer read FClientPort write FClientPort;             // Storage if the connection is a client
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
  LocalIP := '127.0.0.1';
  RemoteIP := '127.0.0.1';
  ListenPort := 12021;
  ClientPort := 12022;
end;

procedure TEthernetSettings.LoadFromFile(IniFile: TIniFile);
begin
  LocalIP := IniFile.ReadString(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_LOCAL_IP, '127.0.0.1');
  RemoteIP := IniFile.ReadString(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_REMOTE_IP, '127.0.0.1');
  ListenPort := IniFile.ReadInteger(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_LISTEN_PORT, 12021);
  ClientPort := IniFile.ReadInteger(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_CLIENT_PORT, 12022);
end;

procedure TEthernetSettings.SaveToFile(IniFile: TIniFile);
begin
  IniFile.WriteString(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_LOCAL_IP, FLocalIP);
  IniFile.WriteString(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_REMOTE_IP, FRemoteIP);
  IniFile.WriteInteger(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_CLIENT_PORT, FClientPort);
  IniFile.WriteInteger(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_LISTEN_PORT, FListenPort);
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
  FStreamWaitTime := MAX_STREAM_WAIT_TIME_DEFAULT;
  FJMRILogFormat := False;
  FLogging := False;
  FDetailedLogging := False;
end;

procedure TGeneralSettings.LoadFromFile(IniFile: TIniFile);
begin
  AliasID := IniFile.ReadString(STR_INT_GENERAL_SECTION, STR_INI_ALIASID, '0x0AAA');
  NodeID := IniFile.ReadString(STR_INT_GENERAL_SECTION, STR_INI_NODEID, '0x102030405006');
  SendPacketDelay := IniFile.ReadInteger(STR_INT_GENERAL_SECTION, STR_INI_SENDPACKETDELAY, 0);
  AutoScanNetworkAtBoot := IniFile.ReadBool(STR_INT_GENERAL_SECTION, STR_INI_AUTOSCAN, True);
  FJMRILogFormat := IniFile.ReadBool(STR_INT_GENERAL_SECTION, STR_INI_JMRI_FORMAT, False);
  FLogging := IniFile.ReadBool(STR_INT_GENERAL_SECTION, STR_INI_LOGGING, False);
  FDetailedLogging := IniFile.ReadBool(STR_INT_GENERAL_SECTION, STR_INI_DETAILED_LOGGING, False);
end;

procedure TGeneralSettings.SaveToFile(IniFile: TIniFile);
begin
  IniFile.WriteString(STR_INT_GENERAL_SECTION, STR_INI_ALIASID, FAliasID);
  IniFile.WriteString(STR_INT_GENERAL_SECTION, STR_INI_NODEID, FNodeID);
  IniFile.WriteInteger(STR_INT_GENERAL_SECTION, STR_INI_SENDPACKETDELAY, FSendPacketDelay);
  IniFile.WriteBool(STR_INT_GENERAL_SECTION, STR_INI_AUTOSCAN, FAutoScanNetworkAtBoot);
  IniFile.WriteBool(STR_INT_GENERAL_SECTION, STR_INI_JMRI_FORMAT, FJMRILogFormat);
  IniFile.WriteBool(STR_INT_GENERAL_SECTION, STR_INI_LOGGING, FLogging);
  IniFile.WriteBool(STR_INT_GENERAL_SECTION, STR_INI_DETAILED_LOGGING, FDetailedLogging);
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
