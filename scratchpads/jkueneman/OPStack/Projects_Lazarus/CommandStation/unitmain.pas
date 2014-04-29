unit unitmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynMemo, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, ActnList, Menus, Buttons, Spin,
  com_port_hub, ethernet_hub, olcb_app_common_settings, file_utilities,
  olcb_utilities, synaser, common_utilities, lcltype, olcb_transport_layer,
  types, olcb_defines, LMessages, Messages, LCLIntf, SynEditKeyCmds, SynEditMarkupHighAll,
  formtrainnode,
  template_hardware, opstackcore, template_configuration, template_userstatemachine;

const
  BUNDLENAME             = 'OpenLCB CommandStationEmulator';
  PATH_LINUX_APP_FOLDER  = 'olcbcommandstationemulator/';
  PATH_SETTINGS_FILE     = 'settings.ini';
  PATH_CONFIGURATION_FILE = 'configuration.dat';

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
    ButtonRemoteLocalHost: TButton;
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
    GroupBoxLogging: TGroupBox;
    GroupBoxThrottles: TGroupBox;
    Image1: TImage;
    ImageList16x16: TImageList;
    Label1: TLabel;
    Label10: TLabel;
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
    TimerGeneral: TTimer;
    TimerOpStackProcess: TTimer;
    TimerOpStackTimer: TTimer;
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
    procedure ActionLogPasteExecute(Sender: TObject);
    procedure ActionLogPauseExecute(Sender: TObject);
    procedure ActionLogSelectAllExecute(Sender: TObject);
    procedure ActionShowAllTrainsExecute(Sender: TObject);
    procedure ActionStartNodeExecute(Sender: TObject);
    procedure ActionZeroizeConfigMemoryExecute(Sender: TObject);
    procedure ButtonLocalHostClick(Sender: TObject);
    procedure ButtonLoggingClearClick(Sender: TObject);
    procedure ButtonRemoteLocalHostClick(Sender: TObject);
    procedure EditAliasIDChange(Sender: TObject);
    procedure EditEthernetLocalIPChange(Sender: TObject);
    procedure EditEthernetRemoteIPChange(Sender: TObject);
    procedure EditNodeIDChange(Sender: TObject);
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
    procedure TimerGeneralTimer(Sender: TObject);
    procedure TimerOpStackProcessTimer(Sender: TObject);
    procedure TimerOpStackTimerTimer(Sender: TObject);
    procedure TreeViewTrainsCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
  private
    FAppAboutCmd: TMenuItem;
    FConfigurationFile: WideString;
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
    procedure EthernetReceiveLogging(Sender: TObject; MessageStr: String);
    procedure EthernetSendLogging(Sender: TObject; MessageStr: String);
    procedure EthernetError(Sender: TObject; ErrorMessage: string);
    procedure EthernetConnectState(Sender: TObject; ConnectionState: TConnectionState);
    procedure DestroyTask(Sender: TTaskOlcbBase);
    procedure LoadSettings(SettingType: TLoadSettingType);
    procedure ScanComPorts;
    procedure StoreSettings(SettingType: TLoadSettingType);

    property AppAboutCmd: TMenuItem read FAppAboutCmd write FAppAboutCmd;
    property ConfigurationFile: WideString read FConfigurationFile write FConfigurationFile;
    property EthernetConnectionState: TConnectionState read FEthernetConnectionState write FEthernetConnectionState;
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
    ComPortHub.EnableReceiveMessages := ActionLogging.Checked;
    ComPortHub.EnableSendMessages := ActionLogging.Checked;
  end
  else begin
    ComPortHub.RemoveComPort(nil);
  end;
  UpdateUI
end;

procedure TForm1.ActionDetailedLoggingExecute(Sender: TObject);
begin
  ;
end;

procedure TForm1.ActionEthernetClientConnectionExecute(Sender: TObject);
begin
  EthernetHub.Listener := False;
  EthernetHub.EnableSendMessages := ActionLogging.Checked;
  EthernetHub.EnableReceiveMessages := ActionLogging.Checked;
  EthernetHub.Enabled := ActionEthernetClientConnection.Checked;
end;

procedure TForm1.ActionEthernetListenerConnectionExecute(Sender: TObject);
begin
  EthernetHub.Listener := True;
  EthernetHub.EnableSendMessages := ActionLogging.Checked;
  EthernetHub.EnableReceiveMessages := ActionLogging.Checked;
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
  EthernetHub.EnableReceiveMessages := ActionLogging.Checked;
  EthernetHub.EnableSendMessages := ActionLogging.Checked;
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
  EthernetHub.OnReceiveMessage := @EthernetReceiveLogging;
  EthernetHub.OnSendMessage:=@EthernetSendLogging;
  EthernetHub.OnBeforeDestroyTask := @DestroyTask;
  EthernetHub.OnErrorMessage := @EthernetError;
  EthernetHub.OnConnectionStateChange := @EthernetConnectState;
  EthernetHub.EnableReceiveMessages := ActionLogging.Checked;
  EthernetHub.EnableSendMessages := ActionLogging.Checked;
  FShownOnce := False;
  OPStackCore_Initialize;
  EthernetConnectionState := csDisconnected;
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
  if (ActionEthernetClientConnection.Checked) or (Sender is TEthernetListenDameonThread) then
    EthernetConnectionState := ConnectionState;
  UpdateUI
end;


procedure TForm1.DestroyTask(Sender: TTaskOlcbBase);
begin
  Exit;
end;

procedure TForm1.EthernetError(Sender: TObject; ErrorMessage: string);
begin

end;

procedure TForm1.EthernetReceiveLogging(Sender: TObject; MessageStr: String);
begin
  if not Paused then
  begin
    SynMemoLog.BeginUpdate();
     try
       if ActionDetailedLogging.Checked then
         SynMemoLog.Lines.Add( MessageToDetailedMessage( MessageStr, False))
       else
         SynMemoLog.Lines.Add(MessageStr);
     finally
       SynMemoLog.CaretY := SynMemoLog.LineHeight * SynMemoLog.Lines.Count;
       SynMemoLog.EndUpdate;
     end;
  end;
end;

procedure TForm1.EthernetSendLogging(Sender: TObject; MessageStr: String);
begin
  if not Paused then
  begin
    SynMemoLog.BeginUpdate();
    try
      if ActionDetailedLogging.Checked then
        SynMemoLog.Lines.Add( MessageToDetailedMessage( MessageStr, True))
      else
        SynMemoLog.Lines.Add(MessageStr);
    finally
      SynMemoLog.CaretY := SynMemoLog.LineHeight * SynMemoLog.Lines.Count;
      SynMemoLog.EndUpdate;
    end;
  end;
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
            EditAliasID.Caption := ValidateHex( GlobalSettings.General.AliasID);
            EditNodeID.Caption := ValidateHex(GlobalSettings.General.NodeID);
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
          GlobalSettings.General.AliasID := ValidateHex(EditAliasID.Caption);
          GlobalSettings.General.NodeID := ValidateHex(EditNodeID.Caption);
        end;
    end;
    GlobalSettings.SaveToFile(UTF8ToSys( SettingsFilePath));
  end;
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

procedure TForm1.TimerGeneralTimer(Sender: TObject);
var
  i: Integer;
  Link: PLinkRec;
  Node: TOlcbTrainTreeNode;
  TrainForm: TFormIsTrainNode;
begin
  System.EnterCriticalsection(OPStackCriticalSection);
  try
    for i := 0 to Sync.NextLink - 1 do
    begin
      Link := @Sync.Link[i];
      if Link^.SyncState and SYNC_NODE_INFO <> 0 then
      begin
        Link^.SyncState := Link^.SyncState and not SYNC_NODE_INFO;
        TrainForm := TrainNodeList.CreateTrain(ImageList16x16);
        if Assigned(TrainForm) then
        begin
          TrainForm.UpdateStatus('Creating and logging OpenLCB node into network.... Please Wait');
          TrainForm.LoadTrainState(Link);
          Node := TreeViewTrains.Items.Add(nil, TrainForm.Caption) as TOlcbTrainTreeNode;
          Node.Train := TrainForm;
          UpdateUI;
          TrainForm.Caption := 'Train Node [' + TrainForm.LabelAddress.Caption + ']';
          TrainForm.UpdateStatus('Train Node Created');
          Link^.Train.ObjPtr := TrainForm;
        end;
      end else
      if Link^.SyncState and SYNC_CONTROLLER <> 0 then
      begin
        Link^.SyncState := Link^.SyncState and not SYNC_CONTROLLER;
        TrainForm.LoadTrainState(Link);
        TrainForm.UpdateStatus('Throttle assignment changed');
      end;
    end;
  finally
    System.LeaveCriticalsection(OPStackCriticalSection);
  end;
end;

procedure TForm1.TimerOpStackProcessTimer(Sender: TObject);
begin
  OPStackCore_Process;
end;

procedure TForm1.TimerOpStackTimerTimer(Sender: TObject);
begin
  OPStackCore_Timer;
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
    case EthernetConnectionState of
      csDisconnected :
        begin
          Statusbar.Panels[0].Text := 'Ethernet: Disconnected';
        end;
      csConnecting :
        begin
         Statusbar.Panels[0].Text := 'Ethernet: Connecting';
        end;
      csDisconnecting :
        begin
         Statusbar.Panels[0].Text := 'Ethernet: Disconnecting';
        end;
      csConnected :
        begin
          Statusbar.Panels[0].Text := 'Ethernet: Connected: ' + GlobalSettings.Ethernet.LocalIP + ':' + IntToStr(GlobalSettings.Ethernet.ListenPort);
        end;
    end;
    if Assigned(EthernetHub) then
      Statusbar.Panels[1].Text := 'Clients: ' + IntToStr(EthernetHub.ClientThreadList.Count);
  end;
end;

end.

