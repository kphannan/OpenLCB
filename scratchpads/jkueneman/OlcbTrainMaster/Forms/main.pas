unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynMemo, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, ActnList, Menus, Buttons, Spin, form_throttle,
  ethernet_hub, olcb_app_common_settings, file_utilities,
  olcb_utilities, synaser, common_utilities, lcltype, olcb_transport_layer,
  types, olcb_defines, LMessages, Messages, LCLIntf, SynEditKeyCmds,
  SynEditMarkupHighAll,
  template_hardware, opstackcore, template_configuration;

const
  BUNDLENAME             = 'OpenLCB TrainMaster';
  PATH_LINUX_APP_FOLDER  = 'olcbtrainmaster/';
  PATH_SETTINGS_FILE     = 'settings.ini';
  PATH_CONFIGURATION_FILE = 'configuration.dat';

  WM_CLOSE_CONNECTIONS = WM_USER + 234;

type

  TLoadSettingType = (
    lstEthernet,
    lstCom,
    lstGeneral
  );

  { TOlcbThrottleTreeNode }

  TOlcbThrottleTreeNode = class(TTreeNode)
  private
    FThrottle: TFormThrottle;
  public
    property Throttle: TFormThrottle read FThrottle write FThrottle;
  end;
  TOlcbThrottleTreeNodeClass = class of TOlcbThrottleTreeNode;

  { TFormOlcbTrainMaster }

  TFormOlcbTrainMaster = class(TForm)
    ActionZeroizeConfigMemory: TAction;
    ActionStartNode: TAction;
    ActionLogCopy: TAction;
    ActionLogPaste: TAction;
    ActionLogSelectAll: TAction;
    ActionLogCut: TAction;
    ActionLogPause: TAction;
    ActionLogClear: TAction;
    ActionDetailedLogging: TAction;
    ActionEthernetClientConnection: TAction;
    ActionLogging: TAction;
    ActionRediscoverProxies: TAction;
    ActionToolsSettingsShowWin: TAction;
    ActionToolsPreferenceShowMac: TAction;
    ActionHelpAboutShow: TAction;
    ActionShowAllThrottles: TAction;
    ActionHideAllThrottles: TAction;
    ActionCloseAllThrottles: TAction;
    ActionCloseSelectedThrottles: TAction;
    ActionAddNewThrottle: TAction;
    ActionEthernetListenerConnection: TAction;
    ActionCOMConnection: TAction;
    ActionList: TActionList;
    BitBtnRescanPorts: TBitBtn;
    ButtonLocalHost: TButton;
    ButtonRemoteLocalHost: TButton;
    ButtonAddThrottles: TButton;
    ButtonCloseAllThrottles: TButton;
    ButtonDeleteSelectedThrottles: TButton;
    ButtonHideAllThrottles: TButton;
    ButtonRediscoverProxies: TButton;
    ButtonShowAllThrottles: TButton;
    ComboBoxBaud: TComboBox;
    ComboBoxComPort: TComboBox;
    ComboBoxDataBits: TComboBox;
    ComboBoxFlowControl: TComboBox;
    ComboBoxParity: TComboBox;
    ComboBoxStopBits: TComboBox;
    EditAliasID: TEdit;
    EditEthernetLocalIP: TEdit;
    EditEthernetRemoteIP: TEdit;
    EditNodeID: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBoxThrottles: TGroupBox;
    Image1: TImage;
    ImageList16x16: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    LabelBaud: TLabel;
    LabelBuild: TLabel;
    LabelBuildDate: TLabel;
    LabelComPort: TLabel;
    LabelCPU: TLabel;
    LabelDataBits: TLabel;
    LabelFlowControl: TLabel;
    LabelIcon: TLabel;
    LabelMyName: TLabel;
    LabelNodeExplorer: TLabel;
    LabelParity: TLabel;
    LabelStopBits: TLabel;
    LabelTargetCPU: TLabel;
    LabelTargetOperatingSystem: TLabel;
    LabelTargetOS: TLabel;
    LabelURLFreePascal: TLabel;
    LabelURLIcons: TLabel;
    LabelURLLazarus: TLabel;
    LabelWrittenIn: TLabel;
    MainMenu: TMainMenu;
    MenuItemCommonToolsSep4: TMenuItem;
    MenuItemCommonToolsLogging: TMenuItem;
    MenuItemCommonToolsZeroizeConfigMem: TMenuItem;
    MenuItemCommonToolsClearLog: TMenuItem;
    MenuItemCommonToolsDetailedLogging: TMenuItem;
    MenuItemCommonToolsComPort: TMenuItem;
    MenuItemCommonToolsSep2: TMenuItem;
    MenuItemCommonToolsStartNode: TMenuItem;
    MenuItemCommonToolsSep1: TMenuItem;
    MenuItemCommonToolsEthernetClient: TMenuItem;
    MenuItemCommonToolsEthernetListen: TMenuItem;
    MenuItemSynMemoLogPause: TMenuItem;
    MenuItemSynMemoLogSep3: TMenuItem;
    MenuItemSynMemoLogClear: TMenuItem;
    MenuItemSynMemoSelectAll: TMenuItem;
    MenuItemSynMemoLogSep2: TMenuItem;
    MenuItemSynMemoLogCut: TMenuItem;
    MenuItemSynMemoLogCopy: TMenuItem;
    MenuItemSynMemoLogSep1: TMenuItem;
    MenuItemToolsRediscoverProxies: TMenuItem;
    MenuItemTools: TMenuItem;
    MenuItemActionsHideAllThrottles: TMenuItem;
    MenuItemActionsCloseAllThrottles: TMenuItem;
    MenuItemActionsShowAllThrottles: TMenuItem;
    MenuItemActionsSpace2: TMenuItem;
    MenuItemActionsSpace1: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemActionsNewThrottle: TMenuItem;
    MenuItemActionsDeleteThrottles: TMenuItem;
    MenuItemActions: TMenuItem;
    MenuItemFile: TMenuItem;
    {$IFDEF DARWIN}
    FOSXMenu: TMenuItem;
    FOSXSep1Cmd: TMenuItem;
    FOSXPrefCmd: TMenuItem;
    {$ENDIF}
    PageControlMain: TPageControl;
    PanelBkGnd: TPanel;
    PopupMenuCommonTools: TPopupMenu;
    PopupMenuSynEditLog: TPopupMenu;
    SpinEditEthernetLocalPort: TSpinEdit;
    SpinEditEtherneRemotePort: TSpinEdit;
    SpinEditSendPacketDelay: TSpinEdit;
    Splitter1: TSplitter;
    StatusBar: TStatusBar;
    SynMemoLog: TSynMemo;
    TabSheetAbout: TTabSheet;
    TabSheetGeneral: TTabSheet;
    TabSheetMain: TTabSheet;
    TabSheetEthernet: TTabSheet;
    TabSheetCom: TTabSheet;
    TimerOpStackProcess: TTimer;
    TimerOpStackTimer: TTimer;
    ToolBarMain: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButtonLog: TToolButton;
    ToolButton3: TToolButton;
    ToolButtonCOM: TToolButton;
    ToolButtonEthernet: TToolButton;
    ToolButtonDeleteThrottle: TToolButton;
    ToolButtonNewThrottle: TToolButton;
    ToolButtonSeparator: TToolButton;
    TreeViewProxies: TTreeView;
    TreeViewThrottles: TTreeView;
    procedure ActionAddNewThrottleExecute(Sender: TObject);
    procedure ActionCloseAllThrottlesExecute(Sender: TObject);
    procedure ActionCOMConnectionExecute(Sender: TObject);
    procedure ActionCloseSelectedThrottlesExecute(Sender: TObject);
    procedure ActionDetailedLoggingExecute(Sender: TObject);
    procedure ActionEthernetClientConnectionExecute(Sender: TObject);
    procedure ActionEthernetListenerConnectionExecute(Sender: TObject);
    procedure ActionHelpAboutShowExecute(Sender: TObject);
    procedure ActionHideAllThrottlesExecute(Sender: TObject);
    procedure ActionLogClearExecute(Sender: TObject);
    procedure ActionLogCopyExecute(Sender: TObject);
    procedure ActionLogCutExecute(Sender: TObject);
    procedure ActionLoggingExecute(Sender: TObject);
    procedure ActionLogPasteExecute(Sender: TObject);
    procedure ActionLogPauseExecute(Sender: TObject);
    procedure ActionLogSelectAllExecute(Sender: TObject);
    procedure ActionRediscoverProxiesExecute(Sender: TObject);
    procedure ActionShowAllThrottlesExecute(Sender: TObject);
    procedure ActionStartNodeExecute(Sender: TObject);
    procedure ActionZeroizeConfigMemoryExecute(Sender: TObject);
    procedure BitBtnRescanPortsClick(Sender: TObject);
    procedure ButtonLocalHostClick(Sender: TObject);
    procedure ButtonRemoteLocalHostClick(Sender: TObject);
    procedure ComboBoxBaudChange(Sender: TObject);
    procedure ComboBoxComPortChange(Sender: TObject);
    procedure ComboBoxDataBitsChange(Sender: TObject);
    procedure ComboBoxFlowControlChange(Sender: TObject);
    procedure ComboBoxParityChange(Sender: TObject);
    procedure ComboBoxStopBitsChange(Sender: TObject);
    procedure EditAliasIDChange(Sender: TObject);
    procedure EditEthernetLocalIPChange(Sender: TObject);
    procedure EditNodeIDChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure LabelURLFreePascalClick(Sender: TObject);
    procedure LabelURLFreePascalMouseEnter(Sender: TObject);
    procedure LabelURLFreePascalMouseLeave(Sender: TObject);
    procedure LabelURLIconsClick(Sender: TObject);
    procedure LabelURLIconsMouseEnter(Sender: TObject);
    procedure LabelURLIconsMouseLeave(Sender: TObject);
    procedure LabelURLLazarusClick(Sender: TObject);
    procedure LabelURLLazarusMouseEnter(Sender: TObject);
    procedure LabelURLLazarusMouseLeave(Sender: TObject);
    procedure SpinEditEthernetLocalPortChange(Sender: TObject);
    procedure SpinEditSendPacketDelayChange(Sender: TObject);
    procedure SynMemoLogKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TabSheetComHide(Sender: TObject);
    procedure TabSheetComShow(Sender: TObject);
    procedure TabSheetEthernetHide(Sender: TObject);
    procedure TabSheetEthernetShow(Sender: TObject);
    procedure TabSheetGeneralHide(Sender: TObject);
    procedure TabSheetGeneralShow(Sender: TObject);
    procedure TimerOpStackProcessTimer(Sender: TObject);
    procedure TimerOpStackTimerTimer(Sender: TObject);
    procedure TreeViewThrottlesCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
  private
    FAppAboutCmd: TMenuItem;
    FComConnectionState: TConnectionState;
    FConfigurationFile: WideString;
    FEthernetConnectionState: TConnectionState;
    FPaused: Boolean;
    FSettingsFilePath: WideString;
    FSettingsLocked: Boolean;
    FShownOnce: Boolean;
    FThrottles: TThrottleList;
    { private declarations }
  protected
    procedure EthernetReceiveLogging(Sender: TObject; MessageStr: String);
    procedure EthernetSendLogging(Sender: TObject; MessageStr: String);
    procedure EthernetConnectState(Sender: TObject; ConnectionState: TConnectionState);
    procedure EthernetError(Sender: TObject; MessageStr: string);
    procedure ComPortError(Sender: TObject; MessageStr: String);
    procedure ComPortConnectionState(Sender: TObject; NewConnectionState: TConnectionState);
    procedure ComPortReceiveLogging(Sender: TObject; MessageStr: String);
    procedure ComPortSendLogging(Sender: TObject; MessageStr: String);
    procedure LoadSettings(SettingType: TLoadSettingType);
    procedure ScanComPorts;
    procedure StoreSettings(SettingType: TLoadSettingType);
    procedure ThrottleClosing(Throttle: TFormThrottle);
    procedure ThrottleHiding(Throttle: TFormThrottle);
    function DispatchTask(Task: TTaskOlcbBase): Boolean;
    procedure TaskDestroy(Sender: TTaskOlcbBase);
    procedure WMCloseConnections(var Message: TMessage); message WM_CLOSE_CONNECTIONS;

    property AppAboutCmd: TMenuItem read FAppAboutCmd write FAppAboutCmd;
    property ComConnectionState: TConnectionState read FComConnectionState write FComConnectionState;
    property ConfigurationFile: WideString read FConfigurationFile write FConfigurationFile;
    property EthernetConnectionState: TConnectionState read FEthernetConnectionState write FEthernetConnectionState;
    {$IFDEF DARWIN}
    property OSXMenu: TMenuItem read FOSXMenu write FOSXMenu;
    property OSXSep1Cmd: TMenuItem read FOSXSep1Cmd write FOSXSep1Cmd;
    property OSXPrefCmd: TMenuItem read FOSXPrefCmd write FOSXPrefCmd;
    {$ENDIF}
    property Paused: Boolean read FPaused write FPaused;
    property ShownOnce: Boolean read FShownOnce write FShownOnce;
    property SettingsLocked: Boolean read FSettingsLocked write FSettingsLocked;
  public
    { public declarations }
    property SettingsFilePath: WideString read FSettingsFilePath write FSettingsFilePath;
    property Throttles: TThrottleList read FThrottles write FThrottles;
    procedure UpdateUI;
  end;

var
  FormOlcbTrainMaster: TFormOlcbTrainMaster;

implementation

uses
  template_userstatemachine;

{$R *.lfm}

{ TFormOlcbTrainMaster }

procedure TFormOlcbTrainMaster.ActionAddNewThrottleExecute(Sender: TObject);
var
  Throttle: TFormThrottle;
  Node: TOlcbThrottleTreeNode;
begin
  Throttle := Throttles.CreateThrottle(EthernetHub, ComPortHub, @DispatchTask, ImageList16x16);
  if Assigned(Throttle) then
  begin
    Node := TreeViewThrottles.Items.Add(nil, Throttle.Caption) as TOlcbThrottleTreeNode;
    Node.Throttle := Throttle;
    UpdateUI;
  end;
end;

procedure TFormOlcbTrainMaster.ActionCloseAllThrottlesExecute(Sender: TObject);
begin
  Throttles.CloseAll;
end;

procedure TFormOlcbTrainMaster.ActionCOMConnectionExecute(Sender: TObject);
begin
  if ActionCOMConnection.Checked then
  begin
    {$IFDEF MSWINDOWS}
    ComPortHub.AddComPort(GlobalSettings.ComPort.BaudRate, GlobalSettings.ComPort.Port);
    {$ELSE}
      {$IFDEF DARWIN}
      ComPortHub.AddComPort(GlobalSettings.ComPort.BaudRate, PATH_OSX_DEV + GlobalSettings.ComPort.Port);
      {$ELSE}
      ComPortHub.AddComPort(GlobalSettings.ComPort.BaudRate, PATH_LINUX_DEV + GlobalSettings.ComPort.Port);
      {$ENDIF}
    {$ENDIF}
    ComPortHub.EnableReceiveMessages := ActionLogging.Checked;
    ComPortHub.EnableSendMessages := ActionLogging.Checked;
  end
  else begin
    ComPortHub.RemoveComPort(nil);
  end;
  UpdateUI
end;

procedure TFormOlcbTrainMaster.ActionDetailedLoggingExecute(Sender: TObject);
begin
  ;
end;

procedure TFormOlcbTrainMaster.ActionEthernetClientConnectionExecute(Sender: TObject);
begin
  EthernetHub.Listener := False;
  EthernetHub.EnableSendMessages := ActionLogging.Checked;
  EthernetHub.EnableReceiveMessages := ActionLogging.Checked;
  EthernetHub.Enabled := ActionEthernetClientConnection.Checked;
end;

procedure TFormOlcbTrainMaster.ActionCloseSelectedThrottlesExecute(Sender: TObject);
begin
  ;
end;

procedure TFormOlcbTrainMaster.ActionEthernetListenerConnectionExecute(Sender: TObject);
begin
  EthernetHub.Listener := True;
  EthernetHub.EnableSendMessages := ActionLogging.Checked;
  EthernetHub.EnableReceiveMessages := ActionLogging.Checked;
  EthernetHub.Enabled := ActionEthernetListenerConnection.Checked;
end;

procedure TFormOlcbTrainMaster.ActionHelpAboutShowExecute(Sender: TObject);
begin
  PageControlMain.ActivePage := TabSheetAbout;
end;

procedure TFormOlcbTrainMaster.ActionHideAllThrottlesExecute(Sender: TObject);
begin
  Throttles.HideAll;
end;

procedure TFormOlcbTrainMaster.ActionLogClearExecute(Sender: TObject);
begin
  SynMemoLog.ClearAll;
end;

procedure TFormOlcbTrainMaster.ActionLogCopyExecute(Sender: TObject);
begin
  SynMemoLog.CommandProcessor(TSynEditorCommand(ecCopy), ' ', nil)
end;

procedure TFormOlcbTrainMaster.ActionLogCutExecute(Sender: TObject);
begin
  SynMemoLog.CommandProcessor(TSynEditorCommand(ecCut), ' ', nil);
end;

procedure TFormOlcbTrainMaster.ActionLoggingExecute(Sender: TObject);
begin
  ComPortHub.EnableReceiveMessages := ActionLogging.Checked;
  ComPortHub.EnableSendMessages := ActionLogging.Checked;
  EthernetHub.EnableReceiveMessages := ActionLogging.Checked;
  EthernetHub.EnableSendMessages := ActionLogging.Checked;
end;

procedure TFormOlcbTrainMaster.ActionLogPasteExecute(Sender: TObject);
begin
  SynMemoLog.CommandProcessor(TSynEditorCommand(ecPaste), ' ', nil);
end;

procedure TFormOlcbTrainMaster.ActionLogPauseExecute(Sender: TObject);
begin
  Paused := not Paused;
end;

procedure TFormOlcbTrainMaster.ActionLogSelectAllExecute(Sender: TObject);
begin
  SynMemoLog.SelectAll;
end;

procedure TFormOlcbTrainMaster.ActionRediscoverProxiesExecute(Sender: TObject);
var
  Task: TTaskIdentifyProducer;
begin
  // Send a IsProxy Message and collect the results
  Task := TTaskIdentifyProducer.Create(GlobalSettings.General.AliasIDAsVal, 0, True, EVENT_IS_PROXY);
  try
    ComPortHub.AddTask(Task);
    EthernetHub.AddTask(Task);
  finally
    Task.Free;
  end;
end;

procedure TFormOlcbTrainMaster.ActionShowAllThrottlesExecute(Sender: TObject);
begin
  Throttles.ShowAll;
end;

procedure TFormOlcbTrainMaster.ActionStartNodeExecute(Sender: TObject);
begin
  if OPStackCore_IsRunning then
  begin
    OPStackCore_Enable(False);
    ActionStartNode.ImageIndex := 239;
    ActionStartNode.Caption := 'Node Stopped';
  end else
  begin
    ActionStartNode.ImageIndex := 240;
    OPStackCore_Initialize;
    OPStackCore_Enable(True);
    ActionStartNode.Caption := 'Node Running';
  end;
end;

procedure TFormOlcbTrainMaster.ActionZeroizeConfigMemoryExecute(Sender: TObject);
begin
  ZeroConfiguration
end;

procedure TFormOlcbTrainMaster.BitBtnRescanPortsClick(Sender: TObject);
begin
  ScanComPorts;
end;

procedure TFormOlcbTrainMaster.ButtonLocalHostClick(Sender: TObject);
begin
  EditEthernetLocalIP.Text := '127.0.0.1';
end;

procedure TFormOlcbTrainMaster.ButtonRemoteLocalHostClick(Sender: TObject);
begin
  EditEthernetRemoteIP.Text := '127.0.0.1';
end;

procedure TFormOlcbTrainMaster.ComboBoxBaudChange(Sender: TObject);
begin
  StoreSettings(lstCom);
end;

procedure TFormOlcbTrainMaster.ComboBoxComPortChange(Sender: TObject);
begin
  StoreSettings(lstCom);
end;

procedure TFormOlcbTrainMaster.ComboBoxDataBitsChange(Sender: TObject);
begin
  StoreSettings(lstCom);
end;

procedure TFormOlcbTrainMaster.ComboBoxFlowControlChange(Sender: TObject);
begin
  StoreSettings(lstCom);
end;

procedure TFormOlcbTrainMaster.ComboBoxParityChange(Sender: TObject);
begin
  StoreSettings(lstCom);
end;

procedure TFormOlcbTrainMaster.ComboBoxStopBitsChange(Sender: TObject);
begin
  StoreSettings(lstCom);
end;

procedure TFormOlcbTrainMaster.ComPortConnectionState(Sender: TObject; NewConnectionState: TConnectionState);
begin
  ComConnectionState := NewConnectionState;
  UpdateUI;
end;

procedure TFormOlcbTrainMaster.ComPortReceiveLogging(Sender: TObject; MessageStr: String);
begin
  SynMemoLog.BeginUpdate;
  try
    SynMemoLog.Lines.Add(MessageStr);
  finally
    SynMemoLog.EndUpdate;
  end;
end;

procedure TFormOlcbTrainMaster.ComPortSendLogging(Sender: TObject; MessageStr: String);
begin
  SynMemoLog.BeginUpdate;
  try
    SynMemoLog.Lines.Add(MessageStr);
  finally
    SynMemoLog.EndUpdate;
  end;
end;

procedure TFormOlcbTrainMaster.LoadSettings(SettingType: TLoadSettingType);
begin
  if not SettingsLocked then
  begin
    SettingsLocked := True;
    try
      case SettingType of
        lstCom:
          begin
            ScanComPorts;
            ComboBoxComPort.ItemIndex := ComboBoxComPort.Items.IndexOf(GlobalSettings.ComPort.Port);
            if (ComboBoxComPort.ItemIndex < 0) and (ComboBoxComPort.Items.Count > 0) then
              ComboBoxComPort.ItemIndex := 0;
            ComboBoxBaud.ItemIndex := ComboBoxBaud.Items.IndexOf(IntToStr(GlobalSettings.ComPort.BaudRate));
            ComboBoxDataBits.ItemIndex := ComboBoxDataBits.Items.IndexOf(IntToStr( GlobalSettings.ComPort.DataBits));
            ComboBoxStopBits.ItemIndex := ComboBoxStopBits.Items.IndexOf(IntToStr( GlobalSettings.ComPort.StopBits));
            ComboBoxParity.ItemIndex := Integer( GlobalSettings.ComPort.Parity);
            ComboBoxFlowControl.ItemIndex := Integer( GlobalSettings.ComPort.FlowControl);
          end;
        lstEthernet:
          begin
            EditEthernetLocalIP.Text := GlobalSettings.Ethernet.LocalIP;
            EditEthernetRemoteIP.Text := GlobalSettings.Ethernet.RemoteIP;
            SpinEditEthernetLocalPort.Value := GlobalSettings.Ethernet.ListenPort;
            SpinEditEtherneRemotePort.Value := GlobalSettings.Ethernet.ClientPort;
          end;
        lstGeneral:
          begin
            SpinEditSendPacketDelay.Value := GlobalSettings.General.SendPacketDelay;
            EditAliasID.Caption := ValidateHex( GlobalSettings.General.AliasID);
            EditNodeID.Caption := ValidateHex(GlobalSettings.General.NodeID);
          end;
      end;
    finally
      SettingsLocked := False;
    end;
  end;
end;

procedure TFormOlcbTrainMaster.ScanComPorts;
begin
  ComboBoxComPort.Items.Delimiter := ';';
  ComboBoxComPort.Items.DelimitedText := StringReplace(GetSerialPortNames, ',', ';', [rfReplaceAll, rfIgnoreCase]);
  if ComboBoxComPort.Items.Count > 0 then
    ComboBoxComPort.ItemIndex := 0;
end;

procedure TFormOlcbTrainMaster.SpinEditEthernetLocalPortChange(Sender: TObject);
begin
  StoreSettings(lstEthernet)
end;

procedure TFormOlcbTrainMaster.SpinEditSendPacketDelayChange(Sender: TObject);
begin
  StoreSettings(lstGeneral)
end;

procedure TFormOlcbTrainMaster.StoreSettings(SettingType: TLoadSettingType);
begin
  if not SettingsLocked then
  begin
    case SettingType of
      lstCom:
        begin
          GlobalSettings.ComPort.Port := ComboBoxComPort.Caption;
          GlobalSettings.ComPort.BaudRate := StrToInt( ComboBoxBaud.Caption);
          GlobalSettings.ComPort.DataBits := StrToInt( ComboBoxDataBits.Caption);
          GlobalSettings.ComPort.StopBits := StrToInt( ComboBoxStopBits.Caption);
          GlobalSettings.ComPort.Parity := TComPortParity( ComboBoxParity.ItemIndex);
          GlobalSettings.ComPort.FlowControl := TComPortFlowControl( ComboBoxFlowControl.ItemIndex);
        end;
      lstEthernet:
        begin
          GlobalSettings.Ethernet.LocalIP := EditEthernetLocalIP.Text;      // Should validate this
          GlobalSettings.Ethernet.RemoteIP := EditEthernetRemoteIP.Text;      // Should validate this
          GlobalSettings.Ethernet.ListenPort := SpinEditEthernetLocalPort.Value;
          GlobalSettings.Ethernet.ClientPort := SpinEditEtherneRemotePort.Value;
        end;
      lstGeneral:
        begin
          GlobalSettings.General.SendPacketDelay := SpinEditSendPacketDelay.Value;
          GlobalSettings.General.AliasID := ValidateHex(EditAliasID.Caption);
          GlobalSettings.General.NodeID := ValidateHex(EditNodeID.Caption);
        end;
    end;
    GlobalSettings.SaveToFile(UTF8ToSys( SettingsFilePath));
  end;
end;

procedure TFormOlcbTrainMaster.SynMemoLogKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
      // Windows/Linux/OSX already handled by SynEdit using the Windows Shortcuts
  {$IFDEF darwin}
  if (Shift = [ssMeta]) then
  begin
    case Key of
    VK_C: SynMemoLog.CommandProcessor(TSynEditorCommand(ecCopy), ' ', nil);
    VK_V: SynMemoLog.CommandProcessor(TSynEditorCommand(ecPaste), ' ', nil);
    VK_X: SynMemoLog.CommandProcessor(TSynEditorCommand(ecCut), ' ', nil);
    end;
  end;
  {$ENDIF}
end;

procedure TFormOlcbTrainMaster.ComPortError(Sender: TObject; MessageStr: String);
begin
 ShowMessage(MessageStr);
 PostMessage(Handle, WM_CLOSE_CONNECTIONS, 0, 0);
end;

procedure TFormOlcbTrainMaster.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  Throttles.CloseAll;
end;

procedure TFormOlcbTrainMaster.FormCreate(Sender: TObject);
var
  Markup: TSynEditMarkupHighlightAllCaret;
begin
  Throttles := TThrottleList.Create;
  Throttles.OnThrottleClose := @ThrottleClosing;
  Throttles.OnThrottleHide := @ThrottleHiding;
  ComPortHub.OnReceiveMessage := @ComPortReceiveLogging;
  ComPortHub.OnSendMessage := @ComPortSendLogging;
  ComPortHub.OnErrorMessage := @ComPortError;
  ComPortHub.OnConnectionStateChange := @ComPortConnectionState;

  EthernetHub.OnReceiveMessage := @EthernetReceiveLogging;
  EthernetHub.OnSendMessage := @EthernetSendLogging;
  EthernetHub.OnErrorMessage := @EthernetError;
  EthernetHub.OnConnectionStateChange := @EthernetConnectState;

  FComConnectionState := csDisconnected;
  FEthernetConnectionState := csDisconnected;
  FShownOnce := False;
  FSettingsLocked := False;
  FPaused := False;

  Markup := SynMemoLog.MarkupByClass[TSynEditMarkupHighlightAllCaret] as TSynEditMarkupHighlightAllCaret;
  Markup.MarkupInfo.FrameColor := clSkyBlue;
  Markup.MarkupInfo.Background := clSkyBlue;
  Markup.WaitTime := 500;
  Markup.Trim := True;
  Markup.FullWord := False;
  Markup.IgnoreKeywords := False;

  FConfigurationFile := '';

  Template_UserStateMachine_OnTaskDestroy := @TaskDestroy;

//  EthernetHub.OnBeforeDestroyTask := @HubDestroyTask;

end;

procedure TFormOlcbTrainMaster.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FThrottles);
end;

procedure TFormOlcbTrainMaster.FormShow(Sender: TObject);
var
  i: Integer;
  Child: TMenuItem;
begin
  if not ShownOnce then
  begin
    {$IFDEF DARWIN}
    OSXMenu := TMenuItem.Create(Self);  {Application menu}
    OSXMenu.Caption := #$EF#$A3#$BF;  {Unicode Apple logo char}
    MainMenu.Items.Insert(0, OSXMenu);

    AppAboutCmd := TMenuItem.Create(Self);
    AppAboutCmd.Action := ActionHelpAboutShow;
    AppAboutCmd.Caption := 'About ' + BUNDLENAME;
    OSXMenu.Add(AppAboutCmd);  {Add About as item in application menu}

    OSXSep1Cmd := TMenuItem.Create(Self);
    OSXSep1Cmd.Caption := '-';
    OSXMenu.Add(OSXSep1Cmd);

    ActionToolsPreferenceShowMac.ShortCut := ShortCut(VK_OEM_COMMA, [ssMeta]);
    OSXPrefCmd := TMenuItem.Create(Self);
    OSXPrefCmd.Action := ActionToolsPreferenceShowMac;
    OSXMenu.Add(OSXPrefCmd);
    ActionToolsSettingsShowWin.Visible := False;
    {$ELSE}
    AppAboutCmd := TMenuItem.Create(Self);
    AppAboutCmd.Action := ActionHelpAboutShow;
    MenuItemHelp.Add(AppAboutCmd);
    {$ENDIF}
    {$IFDEF Linux}
    FormSettings.SettingsFilePath:= GetSettingsPath + {PATH_LINUX_APP_FOLDER +} PATH_SETTINGS_FILE;
    GlobalSettings.LoadFromFile(UTF8ToSys( GetSettingsPath + {PATH_LINUX_APP_FOLDER +} PATH_SETTINGS_FILE));
    {$ELSE}
   SettingsFilePath := GetSettingsPath + PATH_SETTINGS_FILE;
    ConfigurationFile := GetSettingsPath + PATH_CONFIGURATION_FILE;
    GlobalSettings.LoadFromFile(UTF8ToSys( GetSettingsPath + PATH_SETTINGS_FILE));
    SetConfigurationFile(ConfigurationFile);
    {$ENDIF}

    MenuItemTools.AddSeparator;
    for i := 0 to PopupMenuCommonTools.Items.Count - 1 do
    begin
      Child := PopupMenuCommonTools.Items.Items[0];
      PopupMenuCommonTools.Items.Remove(Child);
      MenuItemTools.Add(Child);
    end;

    LabelBuildDate.Caption := {$I %DATE%} + ': ' + {$I %TIME%};
    LabelTargetOS.Caption := {$I %FPCTARGETOS%};
    LabelTargetCPU.Caption := {$I %FPCTARGETCPU%};

    ActionLogging.Execute;       // Set Logging by default
    ShownOnce := True;
  end;
  UpdateUI
end;

procedure TFormOlcbTrainMaster.Image1Click(Sender: TObject);
begin
  OpenURL('http://www.openlcb.org');
end;

procedure TFormOlcbTrainMaster.LabelURLFreePascalClick(Sender: TObject);
begin
  OpenURL('http://' + LabelURLFreePascal.Caption);
end;

procedure TFormOlcbTrainMaster.LabelURLFreePascalMouseEnter(Sender: TObject);
begin
  LabelURLFreePascal.Font.Style := [fsUnderline];
end;

procedure TFormOlcbTrainMaster.LabelURLFreePascalMouseLeave(Sender: TObject);
begin
  LabelURLFreePascal.Font.Style := [];
end;

procedure TFormOlcbTrainMaster.LabelURLIconsClick(Sender: TObject);
begin
  OpenURL('http://' + LabelURLIcons.Caption);
end;

procedure TFormOlcbTrainMaster.LabelURLIconsMouseEnter(Sender: TObject);
begin
  LabelURLIcons.Font.Style := [fsUnderline];
end;

procedure TFormOlcbTrainMaster.LabelURLIconsMouseLeave(Sender: TObject);
begin
  LabelURLIcons.Font.Style := [];
end;

procedure TFormOlcbTrainMaster.LabelURLLazarusClick(Sender: TObject);
begin
  OpenURL('http://' + LabelURLLazarus.Caption);
end;

procedure TFormOlcbTrainMaster.LabelURLLazarusMouseEnter(Sender: TObject);
begin
  LabelURLLazarus.Font.Style := [fsUnderline];
end;

procedure TFormOlcbTrainMaster.LabelURLLazarusMouseLeave(Sender: TObject);
begin
  LabelURLLazarus.Font.Style := [];
end;

procedure TFormOlcbTrainMaster.TabSheetComHide(Sender: TObject);
begin
  StoreSettings(lstCom)
end;

procedure TFormOlcbTrainMaster.TabSheetComShow(Sender: TObject);
begin
  LoadSettings(lstCom)
end;

procedure TFormOlcbTrainMaster.TabSheetEthernetHide(Sender: TObject);
begin
  StoreSettings(lstEthernet)
end;

procedure TFormOlcbTrainMaster.TabSheetEthernetShow(Sender: TObject);
begin
  LoadSettings(lstEthernet)
end;

procedure TFormOlcbTrainMaster.TabSheetGeneralHide(Sender: TObject);
begin
  StoreSettings(lstGeneral)
end;

procedure TFormOlcbTrainMaster.TabSheetGeneralShow(Sender: TObject);
begin
  LoadSettings(lstGeneral)
end;

procedure TFormOlcbTrainMaster.TaskDestroy(Sender: TTaskOlcbBase);
var
  Node: TTreeNode;
begin
  if Sender is TTaskSimpleNodeInformation then
  begin
    TreeViewProxies.Items.AddChild(nil, (Sender as TTaskSimpleNodeInformation).Snip.SniiMfgName);
  end;
end;

procedure TFormOlcbTrainMaster.ThrottleClosing(Throttle: TFormThrottle);
var
  i: Integer;
begin
  for i := 0 to TreeViewThrottles.Items.Count - 1 do
  begin
    if (TreeViewThrottles.Items[i] as TOlcbThrottleTreeNode).Throttle = Throttle then
    begin
      TreeViewThrottles.Items.Delete(TreeViewThrottles.Items[i]);
      Break;
    end;
  end;
  UpdateUI
end;

procedure TFormOlcbTrainMaster.ThrottleHiding(Throttle: TFormThrottle);
begin
  //
end;

procedure TFormOlcbTrainMaster.TimerOpStackProcessTimer(Sender: TObject);
begin
  OPStackCore_Process;
end;

procedure TFormOlcbTrainMaster.TimerOpStackTimerTimer(Sender: TObject);
begin
  OPStackCore_Timer;
end;

function TFormOlcbTrainMaster.DispatchTask(Task: TTaskOlcbBase): Boolean;
begin
  Result := False;
  if Task.DestinationAlias = 0 then
  begin
    Result := EthernetHub.AddTask(Task);
    if not Result then
      Result := ComPortHub.AddTask(Task)
    else ComPortHub.AddTask(Task)
  end else
  begin
    if EthernetHub.AddTask(Task) then
      Result := True
    else
    if ComPortHub.AddTask(Task) then
      Result := True
  end;
  Task.Free;  // Task is Cloned
end;

procedure TFormOlcbTrainMaster.EditAliasIDChange(Sender: TObject);
begin
  StoreSettings(lstGeneral)
end;

procedure TFormOlcbTrainMaster.EditEthernetLocalIPChange(Sender: TObject);
begin
  StoreSettings(lstEthernet)
end;

procedure TFormOlcbTrainMaster.EditNodeIDChange(Sender: TObject);
begin
  StoreSettings(lstGeneral)
end;

procedure TFormOlcbTrainMaster.EthernetConnectState(Sender: TObject; ConnectionState: TConnectionState);
begin
  if (Sender is TEthernetListenDameonThread) or (not EthernetHub.Listener) then
    EthernetConnectionState := ConnectionState;
  UpdateUI
end;

procedure TFormOlcbTrainMaster.EthernetError(Sender: TObject; MessageStr: string);
begin
 ShowMessage(MessageStr);
 PostMessage(Handle, WM_CLOSE_CONNECTIONS, 0, 0);
end;

procedure TFormOlcbTrainMaster.TreeViewThrottlesCreateNodeClass(
  Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
  NodeClass := TOlcbThrottleTreeNode
end;

procedure TFormOlcbTrainMaster.EthernetReceiveLogging(Sender: TObject; MessageStr: String);
begin
  if not Paused then
  begin
    SynMemoLog.BeginUpdate();
     try
       if ActionDetailedLogging.Checked then
         SynMemoLog.Lines.Add( MessageToDetailedMessage(MessageStr, False))
       else
         SynMemoLog.Lines.Add(MessageStr);
     finally
       SynMemoLog.CaretY := SynMemoLog.LineHeight * SynMemoLog.Lines.Count;
       SynMemoLog.EndUpdate;
     end;
  end;
end;

procedure TFormOlcbTrainMaster.EthernetSendLogging(Sender: TObject; MessageStr: String);
begin
  if not Paused then
  begin
     SynMemoLog.BeginUpdate();
     try
       if ActionDetailedLogging.Checked then
         SynMemoLog.Lines.Add( MessageToDetailedMessage(MessageStr, True))
       else
         SynMemoLog.Lines.Add(MessageStr);
     finally
       SynMemoLog.CaretY := SynMemoLog.LineHeight * SynMemoLog.Lines.Count;
       SynMemoLog.EndUpdate;
     end;
  end;
end;

procedure TFormOlcbTrainMaster.UpdateUI;
begin
  if ComponentState * [csDestroying] = [] then
  begin
    ActionCloseSelectedThrottles.Enabled := TreeViewThrottles.Selected <> nil;
    ActionShowAllThrottles.Enabled := TreeViewThrottles.Items.Count > 0;
    ActionCloseAllThrottles.Enabled := TreeViewThrottles.Items.Count > 0;
    ActionHideAllThrottles.Enabled := TreeViewThrottles.Items.Count > 0;

    case ComConnectionState of
      csDisconnected :
        begin
          ActionCOMConnection.ImageIndex := 892;
          Statusbar.Panels[0].Text := 'COM: Disconnected';
        end;
      csConnecting :
        begin
          ActionCOMConnection.ImageIndex := 892;
          Statusbar.Panels[0].Text := 'COM: Connecting';
        end;
      csConnected :
        begin
          ActionCOMConnection.ImageIndex := 891;
          Statusbar.Panels[0].Text := 'COM: Connected';
        end;
    end;

    case EthernetConnectionState of
      csDisconnected :
        begin
          ActionEthernetListenerConnection.ImageIndex := 989;
          Statusbar.Panels[1].Text := 'Ethernet: Disconnected';
        end;
      csConnecting :
        begin
          ActionEthernetListenerConnection.ImageIndex := 989;
         Statusbar.Panels[1].Text := 'Ethernet: Connecting';
        end;
      csDisconnecting :
        begin
          ActionEthernetListenerConnection.ImageIndex := 989;
          Statusbar.Panels[1].Text := 'Ethernet: Disconnecting';
        end;
      csConnected :
        begin
          ActionEthernetListenerConnection.ImageIndex := 988;
          if EthernetHub.Listener then
            Statusbar.Panels[1].Text := 'Connected - Listening on: ' + GlobalSettings.Ethernet.LocalIP + ':' + IntToStr(GlobalSettings.Ethernet.ListenPort)
          else
            Statusbar.Panels[1].Text := 'Connected - Client: ' + GlobalSettings.Ethernet.LocalIP + ':' + IntToStr(GlobalSettings.Ethernet.ClientPort)
        end;
    end;

    if Assigned(EthernetHub) then
      Statusbar.Panels[2].Text := 'Clients: ' + IntToStr(EthernetHub.ClientThreadList.Count);
  end;
end;

procedure TFormOlcbTrainMaster.WMCloseConnections(var Message: TMessage);
begin
  if ActionEthernetListenerConnection.Checked then
    ActionEthernetListenerConnection.Execute;
  if ActionEthernetClientConnection.Checked then
    ActionEthernetClientConnection.Execute;
  if ActionCOMConnection.Checked then
    ActionCOMConnection.Execute;
  UpdateUI
end;

end.

