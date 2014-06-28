unit unitmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynMemo, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, ActnList, Menus, Buttons, Spin,
  com_port_hub, ethernet_hub, olcb_app_common_settings, file_utilities,
  olcb_utilities, synaser, common_utilities, lcltype, olcb_transport_layer,
  types, olcb_defines, LMessages, Messages, LCLIntf, SynEditKeyCmds, SynEditMarkupHighAll,
  formtrainnode, opstackbuffers,
  template_hardware, opstackcore, template_configuration, template_userstatemachine;

const
  BUNDLENAME             = 'OpenLCB CommandStationEmulator';
  PATH_LINUX_APP_FOLDER  = 'olcbcommandstationemulator/';
  PATH_SETTINGS_FILE     = 'settings.ini';
  PATH_CONFIGURATION_FILE = 'configuration.dat';

  WM_CLOSE_CONNECTIONS = WM_USER + 234;

type

  TLoadSettingType = (
    lstEthernet,
    lstCom,
    lstGeneral
  );

  TOlcbTrainTreeNode = class(TTreeNode)
  private
    FTrain: TFormIsTrainNode;
  public
    property Train: TFormIsTrainNode read FTrain write FTrain;
  end;
  TOlcbTrainTreeNodeClass = class of TOlcbTrainTreeNode;

  { TForm1 }

  TForm1 = class(TForm)
    ActionLogInJMRI: TAction;
    ActionZeroizeConfigMemory: TAction;
    ActionAddNewTrain: TAction;
    ActionCloseAllTrains: TAction;
    ActionCloseSelectedTrains: TAction;
    ActionDetailedLogging: TAction;
    ActionHideAllTrains: TAction;
    ActionLogClear: TAction;
    ActionLogCopy: TAction;
    ActionLogCut: TAction;
    ActionLogPaste: TAction;
    ActionLogPause: TAction;
    ActionLogSelectAll: TAction;
    ActionRediscoverProxies: TAction;
    ActionShowAllTrains: TAction;
    ActionStartNode: TAction;
    ActionCOMConnection: TAction;
    ActionEthernetClientConnection: TAction;
    ActionEthernetListenerConnection: TAction;
    ActionHelpAboutShow: TAction;
    ActionList: TActionList;
    ActionLogging: TAction;
    ActionToolsPreferenceShowMac: TAction;
    ActionToolsSettingsShowWin: TAction;
    BitBtnRescanPorts: TBitBtn;
    ButtonHideAllThrottles: TButton;
    ButtonLocalHost: TButton;
    ButtonRefreshBufferCount: TButton;
    ButtonRemoteLocalHost: TButton;
    ButtonShowAllThrottles: TButton;
    CheckBoxJMRILogging: TCheckBox;
    ComboBoxBaud: TComboBox;
    ComboBoxComPort: TComboBox;
    ComboBoxDataBits: TComboBox;
    ComboBoxFlowControl: TComboBox;
    ComboBoxParity: TComboBox;
    ComboBoxStopBits: TComboBox;
    EditEthernetLocalIP: TEdit;
    EditEthernetRemoteIP: TEdit;
    GroupBoxLogging: TGroupBox;
    GroupBoxThrottles: TGroupBox;
    Image1: TImage;
    ImageList16x16: TImageList;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LabelBaud: TLabel;
    LabelBuild1: TLabel;
    LabelBuildDate: TLabel;
    LabelComPort: TLabel;
    LabelCPU: TLabel;
    LabelDataBits: TLabel;
    LabelDatagramCount: TLabel;
    LabelDatagramCountMax: TLabel;
    LabelFlowControl: TLabel;
    LabelIcon: TLabel;
    LabelMessageCount: TLabel;
    LabelMessageCountMax: TLabel;
    LabelMultiFrameCount: TLabel;
    LabelMultiFrameCountMax: TLabel;
    LabelMyName: TLabel;
    LabelNodeExplorer: TLabel;
    LabelParity: TLabel;
    LabelSimpleCount: TLabel;
    LabelSimpleCountMax: TLabel;
    LabelSnipCount: TLabel;
    LabelSnipCountMax: TLabel;
    LabelStopBits: TLabel;
    LabelTargetCPU: TLabel;
    LabelTargetOperatingSystem: TLabel;
    LabelTargetOS: TLabel;
    LabelURLFreePascal: TLabel;
    LabelURLIcons: TLabel;
    LabelURLLazarus: TLabel;
    LabelWrittenIn: TLabel;
    MainMenu: TMainMenu;
    MenuItemCommonToolsClearLog: TMenuItem;
    MenuItemCommonToolsComPort: TMenuItem;
    MenuItemCommonToolsDetailedLogging: TMenuItem;
    MenuItemCommonToolsEthernetClient: TMenuItem;
    MenuItemCommonToolsEthernetListen: TMenuItem;
    MenuItemCommonToolsLogging: TMenuItem;
    MenuItemCommonToolsSep1: TMenuItem;
    MenuItemCommonToolsSep2: TMenuItem;
    MenuItemCommonToolsSep4: TMenuItem;
    MenuItemCommonToolsStartNode: TMenuItem;
    MenuItemCommonToolsZeroizeConfigMem: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemTools: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemSynMemoLogClear: TMenuItem;
    MenuItemSynMemoLogCopy: TMenuItem;
    MenuItemSynMemoLogCut: TMenuItem;
    MenuItemSynMemoLogPause: TMenuItem;
    MenuItemSynMemoLogSep1: TMenuItem;
    MenuItemSynMemoLogSep2: TMenuItem;
    MenuItemSynMemoLogSep3: TMenuItem;
    MenuItemSynMemoSelectAll: TMenuItem;
    PageControlMain: TPageControl;
    PopupMenuCommonTools: TPopupMenu;
    PopupMenuSynEditLog: TPopupMenu;
    SpinEditEtherneRemotePort: TSpinEdit;
    SpinEditEthernetLocalPort: TSpinEdit;
    SpinEditSendPacketDelay: TSpinEdit;
    StatusBar: TStatusBar;
    SynMemoLog: TSynMemo;
    TabSheetAbout: TTabSheet;
    TabSheetComPort: TTabSheet;
    TabSheetCommandStation: TTabSheet;
    TabSheetEthernet: TTabSheet;
    TabSheetGeneral: TTabSheet;
    ToolBarMain: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButtonCOM: TToolButton;
    ToolButtonDeleteThrottle: TToolButton;
    ToolButtonEthernet: TToolButton;
    ToolButtonNewThrottle: TToolButton;
    ToolButtonSeparator: TToolButton;
    TreeViewTrains: TTreeView;
    procedure ActionCOMConnectionExecute(Sender: TObject);
    procedure ActionDetailedLoggingExecute(Sender: TObject);
    procedure ActionEthernetClientConnectionExecute(Sender: TObject);
    procedure ActionEthernetListenerConnectionExecute(Sender: TObject);
    procedure ActionHelpAboutShowExecute(Sender: TObject);
    procedure ActionHideAllTrainsExecute(Sender: TObject);
    procedure ActionLogClearExecute(Sender: TObject);
    procedure ActionLogCopyExecute(Sender: TObject);
    procedure ActionLogCutExecute(Sender: TObject);
    procedure ActionLoggingExecute(Sender: TObject);
    procedure ActionLogInJMRIExecute(Sender: TObject);
    procedure ActionLogPasteExecute(Sender: TObject);
    procedure ActionLogPauseExecute(Sender: TObject);
    procedure ActionLogSelectAllExecute(Sender: TObject);
    procedure ActionShowAllTrainsExecute(Sender: TObject);
    procedure ActionStartNodeExecute(Sender: TObject);
    procedure ActionZeroizeConfigMemoryExecute(Sender: TObject);
    procedure ButtonLocalHostClick(Sender: TObject);
    procedure ButtonLoggingClearClick(Sender: TObject);
    procedure ButtonRefreshBufferCountClick(Sender: TObject);
    procedure ButtonRemoteLocalHostClick(Sender: TObject);
    procedure EditAliasIDChange(Sender: TObject);
    procedure EditEthernetLocalIPChange(Sender: TObject);
    procedure EditEthernetRemoteIPChange(Sender: TObject);
    procedure EditNodeIDChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
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
    procedure SpinEditEtherneRemotePortChange(Sender: TObject);
    procedure SpinEditEthernetLocalPortChange(Sender: TObject);
    procedure SpinEditSendPacketDelayChange(Sender: TObject);
    procedure TabSheetComPortHide(Sender: TObject);
    procedure TabSheetComPortShow(Sender: TObject);
    procedure TabSheetEthernetHide(Sender: TObject);
    procedure TabSheetEthernetShow(Sender: TObject);
    procedure TabSheetGeneralHide(Sender: TObject);
    procedure TabSheetGeneralShow(Sender: TObject);
    procedure TreeViewTrainsCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
  private
    FAppAboutCmd: TMenuItem;
    FComConnectionState: TConnectionState;
    FConfigurationFile: WideString;
    FEthernetConnectionCount: Integer;
    FEthernetConnectionState: TConnectionState;
    {$IFDEF DARWIN}
    FOSXMenu: TMenuItem;
    FOSXPrefCmd: TMenuItem;
    FOSXSep1Cmd: TMenuItem;
    {$ENDIF}
    FSettingsFilePath: WideString;
    FPaused: Boolean;
    FSettingsLocked: Boolean;
    FShownOnce: Boolean;
    FTrainNodeList: TTrainNodeList;
    { private declarations }
  protected
    procedure MessageLogging(Sender: TObject; MessageStr: String);
    procedure NodeEvent(Sender: TObject; EventList: TList);
    procedure EthernetError(Sender: TObject; MessageStr: string);
    procedure EthernetConnectState(Sender: TObject; ConnectionState: TConnectionState);
    procedure ComPortError(Sender: TObject; MessageStr: String);
    procedure ComPortConnectionState(Sender: TObject; NewConnectionState: TConnectionState);
    procedure LoadSettings(SettingType: TLoadSettingType);
    procedure ScanComPorts;
    procedure StoreSettings(SettingType: TLoadSettingType);
    procedure WMCloseConnections(var Message: TMessage); message WM_CLOSE_CONNECTIONS;

    property AppAboutCmd: TMenuItem read FAppAboutCmd write FAppAboutCmd;
    property ComConnectionState: TConnectionState read FComConnectionState write FComConnectionState;
    property ConfigurationFile: WideString read FConfigurationFile write FConfigurationFile;
    property EthernetConnectionState: TConnectionState read FEthernetConnectionState write FEthernetConnectionState;
    property EthernetConnectionCount: Integer read FEthernetConnectionCount write FEthernetConnectionCount;
    {$IFDEF DARWIN}
    property OSXMenu: TMenuItem read FOSXMenu write FOSXMenu;
    property OSXSep1Cmd: TMenuItem read FOSXSep1Cmd write FOSXSep1Cmd;
    property OSXPrefCmd: TMenuItem read FOSXPrefCmd write FOSXPrefCmd;
    {$ENDIF}
    property Paused: Boolean read FPaused write FPaused;
    property SettingsFilePath: WideString read FSettingsFilePath write FSettingsFilePath;
    property SettingsLocked: Boolean read FSettingsLocked write FSettingsLocked;
    property ShownOnce: Boolean read FShownOnce write FShownOnce;
  public
    { public declarations }

    procedure UpdateUI;
    procedure UpdateMessageCountUI;
    property TrainNodeList: TTrainNodeList read FTrainNodeList write FTrainNodeList;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ActionCOMConnectionExecute(Sender: TObject);
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
  end
  else begin
    ComPortHub.RemoveComPort(nil);
  end;
  UpdateUI
end;

procedure TForm1.ActionDetailedLoggingExecute(Sender: TObject);
begin
  StoreSettings(lstGeneral); ;
end;

procedure TForm1.ActionEthernetClientConnectionExecute(Sender: TObject);
begin
  EthernetHub.Listener := False;
  EthernetHub.Enabled := ActionEthernetClientConnection.Checked;
end;

procedure TForm1.ActionEthernetListenerConnectionExecute(Sender: TObject);
begin
  EthernetHub.Listener := True;
  EthernetHub.Enabled := ActionEthernetListenerConnection.Checked;
end;

procedure TForm1.ActionHelpAboutShowExecute(Sender: TObject);
begin
  PageControlMain.ActivePage := TabSheetAbout;
end;

procedure TForm1.ActionHideAllTrainsExecute(Sender: TObject);
begin
  TrainNodeList.HideAll;
end;

procedure TForm1.ActionLogClearExecute(Sender: TObject);
begin
  SynMemoLog.ClearAll;
end;

procedure TForm1.ActionLogCopyExecute(Sender: TObject);
begin
  SynMemoLog.CommandProcessor(TSynEditorCommand(ecCopy), ' ', nil);
end;

procedure TForm1.ActionLogCutExecute(Sender: TObject);
begin
  SynMemoLog.CommandProcessor(TSynEditorCommand(ecCut), ' ', nil);
end;

procedure TForm1.ActionLoggingExecute(Sender: TObject);
begin
  StoreSettings(lstGeneral);
  if ActionLogging.Checked then
    NodeThread.OnLogMessages := @MessageLogging
  else
    NodeThread.OnLogMessages := nil;
end;

procedure TForm1.ActionLogInJMRIExecute(Sender: TObject);
begin
  StoreSettings(lstGeneral);
end;

procedure TForm1.ActionLogPasteExecute(Sender: TObject);
begin
  SynMemoLog.CommandProcessor(TSynEditorCommand(ecPaste), ' ', nil);
end;

procedure TForm1.ActionLogPauseExecute(Sender: TObject);
begin
  Paused := not Paused
end;

procedure TForm1.ActionLogSelectAllExecute(Sender: TObject);
begin
    SynMemoLog.SelectAll;
end;

procedure TForm1.ActionShowAllTrainsExecute(Sender: TObject);
begin
  TrainNodeList.ShowAll;
end;

procedure TForm1.ActionStartNodeExecute(Sender: TObject);
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

procedure TForm1.ActionZeroizeConfigMemoryExecute(Sender: TObject);
begin
  ZeroConfiguration
end;

procedure TForm1.ButtonLocalHostClick(Sender: TObject);
begin
  EditEthernetLocalIP.Text := '127.0.0.1';
end;

procedure TForm1.ButtonLoggingClearClick(Sender: TObject);
begin
  SynMemoLog.BeginUpdate();
  try
    SynMemoLog.Lines.Clear;
  finally
    SynMemoLog.EndUpdate;
  end;
end;

procedure TForm1.ButtonRefreshBufferCountClick(Sender: TObject);
begin
  UpdateMessageCountUI
end;

procedure TForm1.ButtonRemoteLocalHostClick(Sender: TObject);
begin
  EditEthernetRemoteIP.Text := '127.0.0.1';
end;

procedure TForm1.EditAliasIDChange(Sender: TObject);
begin
  StoreSettings(lstGeneral)
end;

procedure TForm1.EditEthernetLocalIPChange(Sender: TObject);
begin
  StoreSettings(lstEthernet)
end;

procedure TForm1.EditEthernetRemoteIPChange(Sender: TObject);
begin
  StoreSettings(lstEthernet)
end;

procedure TForm1.EditNodeIDChange(Sender: TObject);
begin
  StoreSettings(lstGeneral)
end;


procedure TForm1.FormCreate(Sender: TObject);
var
  Markup: TSynEditMarkupHighlightAllCaret;
begin
  CreateHubs;
  NodeThread.OnLogMessages := @MessageLogging;

  EthernetHub.OnErrorMessage := @EthernetError;
  EthernetHub.OnConnectionStateChange := @EthernetConnectState;
  FEthernetConnectionCount := 0;

  ComPortHub.OnErrorMessage := @ComPortError;
  ComPortHub.OnConnectionStateChange := @ComPortConnectionState;

  NodeThread.OnNodeEvent := @NodeEvent;

  FShownOnce := False;
  OPStackCore_Initialize;
  EthernetConnectionState := csDisconnected;
  ComConnectionState := csDisconnected;
  FSettingsLocked := False;
  FPaused := False;

  Markup := SynMemoLog.MarkupByClass[TSynEditMarkupHighlightAllCaret] as TSynEditMarkupHighlightAllCaret;
  Markup.MarkupInfo.FrameColor := clSkyBlue;
  Markup.MarkupInfo.Background := clSkyBlue;
  Markup.WaitTime := 500;
  Markup.Trim := True;
  Markup.FullWord := False;
  Markup.IgnoreKeywords := False;

  TrainNodeList := TTrainNodeList.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTrainNodeList);
end;

procedure TForm1.FormShow(Sender: TObject);
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
 //   FormMessageLog.SynMemo.Font.Height := 0;
 //   FormEthernetMessageLog.SynMemo.Font.Height := 0;
    {$ELSE}
    AppAboutCmd := TMenuItem.Create(Self);
    AppAboutCmd.Action := ActionHelpAboutShow;
    MenuItemHelp.Add(AppAboutCmd);
    {$ENDIF}
    {$IFDEF Linux}
    SettingsFilePath:= GetSettingsPath + {PATH_LINUX_APP_FOLDER +} PATH_SETTINGS_FILE;
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

    LoadSettings(lstCom);
    LoadSettings(lstEthernet);
    LoadSettings(lstGeneral);

    if ActionLogging.Checked then
      NodeThread.OnLogMessages := @MessageLogging
    else
      NodeThread.OnLogMessages := nil;

    ShownOnce := True;
  end;
  UpdateUI
end;

procedure TForm1.Image1Click(Sender: TObject);
begin
  OpenURL('http://www.openlcb.org');
end;

procedure TForm1.LabelURLFreePascalClick(Sender: TObject);
begin
  OpenURL('http://' + LabelURLFreePascal.Caption);
end;

procedure TForm1.LabelURLFreePascalMouseEnter(Sender: TObject);
begin
  LabelURLFreePascal.Font.Style := [fsUnderline];
end;

procedure TForm1.LabelURLFreePascalMouseLeave(Sender: TObject);
begin
  LabelURLFreePascal.Font.Style := [];
end;

procedure TForm1.LabelURLIconsClick(Sender: TObject);
begin
  OpenURL('http://' + LabelURLIcons.Caption);
end;

procedure TForm1.LabelURLIconsMouseEnter(Sender: TObject);
begin
  LabelURLIcons.Font.Style := [fsUnderline];
end;

procedure TForm1.LabelURLIconsMouseLeave(Sender: TObject);
begin
  LabelURLIcons.Font.Style := [];
end;

procedure TForm1.LabelURLLazarusClick(Sender: TObject);
begin
  OpenURL('http://' + LabelURLLazarus.Caption);
end;

procedure TForm1.LabelURLLazarusMouseEnter(Sender: TObject);
begin
   LabelURLLazarus.Font.Style := [fsUnderline];
end;

procedure TForm1.LabelURLLazarusMouseLeave(Sender: TObject);
begin
   LabelURLLazarus.Font.Style := [];
end;

procedure TForm1.EthernetConnectState(Sender: TObject; ConnectionState: TConnectionState);
begin
  if (Sender is TEthernetListenDameonThread) or (not EthernetHub.Listener) then
      EthernetConnectionState := ConnectionState
    else
    if (Sender is TSocketThread) then
    begin
      case ConnectionState of
        csConnected    : Inc(FEthernetConnectionCount);
        csDisconnected : Dec(FEthernetConnectionCount);
      end;
    end;
    UpdateUI
end;

procedure TForm1.ComPortError(Sender: TObject; MessageStr: String);
begin
  ShowMessage(MessageStr);
  PostMessage(Handle, WM_CLOSE_CONNECTIONS, 0, 0);
end;

procedure TForm1.ComPortConnectionState(Sender: TObject; NewConnectionState: TConnectionState);
begin
  ComConnectionState := NewConnectionState;
  UpdateUI;
end;

procedure TForm1.EthernetError(Sender: TObject; MessageStr: string);
begin
  ShowMessage(MessageStr);
 PostMessage(Handle, WM_CLOSE_CONNECTIONS, 0, 0);
end;

procedure TForm1.MessageLogging(Sender: TObject; MessageStr: String);
begin
  PrintToSynMemo(MessageStr, SynMemoLog, Paused, ActionDetailedLogging.Checked, ActionLogInJMRI.Checked);
end;

procedure TForm1.NodeEvent(Sender: TObject; EventList: TList);
var
  i: Integer;
  Event: TNodeEvent;
  EventTrainInfo: TNodeEventTrainInfo;
  TrainForm: TFormIsTrainNode;
begin
  try
    for i := 0 to EventList.Count - 1 do
    begin
      if TObject( EventList[i]) is TNodeEventTrainInfo then
      begin
        EventTrainInfo := TNodeEventTrainInfo( EventList[i]);
        TrainForm := TrainNodeList.Find(EventTrainInfo.Address, EventTrainInfo.SpeedSteps);
        if Assigned(TrainForm) then
          TrainForm.EventTrainInfo(EventTrainInfo)
        else begin
          TrainForm := TrainNodeList.CreateTrain(ImageList16x16);
          TrainForm.EventTrainInfo(EventTrainInfo);
        end;
      end;
      Event := TNodeEvent( EventList[i]);
      FreeAndNil(Event);
    end;
  finally
    FreeAndNil(EventList)
  end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  NodeThread.OnLogMessages := nil;
  EthernetHub.Enabled := False;
  ComPortHub.RemoveComPort(nil);
  DestroyHubs;
end;

procedure TForm1.LoadSettings(SettingType: TLoadSettingType);
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
            ActionLogInJMRI.Checked := GlobalSettings.General.JMRILogFormat;
            ActionLogging.Checked := GlobalSettings.General.Logging;
            ActionDetailedLogging.Checked := GlobalSettings.General.DetailedLogging;
          end;
      end;
    finally
      SettingsLocked := False;
    end;
  end;
end;

procedure TForm1.ScanComPorts;
begin
  ComboBoxComPort.Items.Delimiter := ';';
  ComboBoxComPort.Items.DelimitedText := StringReplace(GetSerialPortNames, ',', ';', [rfReplaceAll, rfIgnoreCase]);
  if ComboBoxComPort.Items.Count > 0 then
    ComboBoxComPort.ItemIndex := 0;
end;

procedure TForm1.SpinEditEtherneRemotePortChange(Sender: TObject);
begin
  StoreSettings(lstEthernet)
end;

procedure TForm1.SpinEditEthernetLocalPortChange(Sender: TObject);
begin
  StoreSettings(lstEthernet)
end;

procedure TForm1.SpinEditSendPacketDelayChange(Sender: TObject);
begin
  StoreSettings(lstGeneral)
end;

procedure TForm1.StoreSettings(SettingType: TLoadSettingType);
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
          GlobalSettings.General.JMRILogFormat := ActionLogInJMRI.Checked;
          GlobalSettings.General.Logging := ActionLogging.Checked;
          GlobalSettings.General.DetailedLogging := ActionDetailedLogging.Checked;
        end;
    end;
    GlobalSettings.SaveToFile(UTF8ToSys( SettingsFilePath));
  end;
end;

procedure TForm1.WMCloseConnections(var Message: TMessage);
begin
  if ActionEthernetListenerConnection.Checked then
    ActionEthernetListenerConnection.Execute;
  if ActionEthernetClientConnection.Checked then
    ActionEthernetClientConnection.Execute;
  if ActionCOMConnection.Checked then
    ActionCOMConnection.Execute;
  UpdateUI
end;

procedure TForm1.TabSheetComPortHide(Sender: TObject);
begin
  StoreSettings(lstCom)
end;

procedure TForm1.TabSheetComPortShow(Sender: TObject);
begin
  LoadSettings(lstCom)
end;

procedure TForm1.TabSheetEthernetHide(Sender: TObject);
begin
  StoreSettings(lstEthernet)
end;

procedure TForm1.TabSheetEthernetShow(Sender: TObject);
begin
  LoadSettings(lstEthernet)
end;

procedure TForm1.TabSheetGeneralHide(Sender: TObject);
begin
  StoreSettings(lstGeneral)
end;

procedure TForm1.TabSheetGeneralShow(Sender: TObject);
begin
  LoadSettings(lstGeneral)
end;

procedure TForm1.TreeViewTrainsCreateNodeClass(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
begin
  NodeClass := TOlcbTrainTreeNode
end;

procedure TForm1.UpdateUI;
begin
  if ComponentState * [csDestroying] = [] then
  begin

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
          Statusbar.Panels[1].Text := 'Ethernet: Disconnected';
        end;
      csConnecting :
        begin
         Statusbar.Panels[1].Text := 'Ethernet: Connecting';
        end;
      csDisconnecting :
        begin
         Statusbar.Panels[1].Text := 'Ethernet: Disconnecting';
        end;
      csConnected :
        begin
          Statusbar.Panels[1].Text := 'Ethernet: Connected: ' + GlobalSettings.Ethernet.LocalIP + ':' + IntToStr(GlobalSettings.Ethernet.ListenPort);
        end;
    end;
    Statusbar.Panels[2].Text := 'Clients: ' + IntToStr(EthernetConnectionCount);
  end;
  UpdateMessageCountUI;
end;

procedure TForm1.UpdateMessageCountUI;
begin
  LabelSimpleCount.Caption := IntToSTr(SimpleBufferPool.Count);
  LabelSnipCount.Caption := IntToStr(AcdiSnipBufferPool.Count);
  LabelDatagramCount.Caption := IntToStr(DatagramBufferPool.Count);
  LabelMultiFrameCount.Caption := IntToStr(MultiFramePool.Count);
  LabelMessageCount.Caption := IntToStr(OPStackMessagePool.Count);

  LabelSimpleCountMax.Caption := IntToSTr(SimpleBufferPool.MaxCount);
  LabelSnipCountMax.Caption := IntToStr(AcdiSnipBufferPool.MaxCount);
  LabelDatagramCountMax.Caption := IntToStr(DatagramBufferPool.MaxCount);
  LabelMultiFrameCountMax.Caption := IntToStr(MultiFramePool.MaxCount);
  LabelMessageCountMax.Caption := IntToStr(OPStackMessagePool.MaxCount);
end;

end.

