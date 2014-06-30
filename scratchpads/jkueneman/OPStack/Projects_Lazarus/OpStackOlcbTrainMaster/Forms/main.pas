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
  template_hardware, opstackcore, template_configuration, template_node,
  opstackbuffers, opstackdefines, threadedstringlist, nmranetutilities,
  Float16;

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

type
  TThrottleServerState = record
    Proxy: TNodeInfo;
  end;

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
    ActionLogInJMRI: TAction;
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
    ButtonRefreshBufferCount: TButton;
    ButtonLocalHost: TButton;
    ButtonRemoteLocalHost: TButton;
    ButtonAddThrottles: TButton;
    ButtonCloseAllThrottles: TButton;
    ButtonDeleteSelectedThrottles: TButton;
    ButtonHideAllThrottles: TButton;
    ButtonRediscoverProxies: TButton;
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
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
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
    Label4: TLabel;
    Label9: TLabel;
    LabelDatagramCount: TLabel;
    LabelDatagramCountMax: TLabel;
    LabelMessageCount: TLabel;
    LabelMessageCountMax: TLabel;
    LabelMultiFrameCount: TLabel;
    LabelMultiFrameCountMax: TLabel;
    LabelSimpleCount: TLabel;
    Label3: TLabel;
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
    ToolBarMain: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButtonCOM: TToolButton;
    ToolButtonDeleteThrottle: TToolButton;
    ToolButtonEthernet: TToolButton;
    ToolButtonLog: TToolButton;
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
    procedure ActionLogInJMRIExecute(Sender: TObject);
    procedure ActionLogPasteExecute(Sender: TObject);
    procedure ActionLogPauseExecute(Sender: TObject);
    procedure ActionLogSelectAllExecute(Sender: TObject);
    procedure ActionShowAllThrottlesExecute(Sender: TObject);
    procedure ActionStartNodeExecute(Sender: TObject);
    procedure ActionZeroizeConfigMemoryExecute(Sender: TObject);
    procedure BitBtnRescanPortsClick(Sender: TObject);
    procedure ButtonLocalHostClick(Sender: TObject);
    procedure ButtonRefreshBufferCountClick(Sender: TObject);
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
    procedure SpinEditEthernetLocalPortChange(Sender: TObject);
    procedure SpinEditSendPacketDelayChange(Sender: TObject);
    procedure SynMemoLogKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TabSheetComHide(Sender: TObject);
    procedure TabSheetComShow(Sender: TObject);
    procedure TabSheetEthernetHide(Sender: TObject);
    procedure TabSheetEthernetShow(Sender: TObject);
    procedure TabSheetGeneralHide(Sender: TObject);
    procedure TabSheetGeneralShow(Sender: TObject);
    procedure TreeViewThrottlesCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
  private
    FAppAboutCmd: TMenuItem;
    FComConnectionState: TConnectionState;
    FConfigurationFile: WideString;
    FEthernetConnectionCount: Integer;
    FEthernetConnectionState: TConnectionState;
    FPaused: Boolean;
    FSettingsFilePath: WideString;
    FSettingsLocked: Boolean;
    FShownOnce: Boolean;
    FThrottles: TThrottleList;
    FThrottleServerState: TThrottleServerState;
    { private declarations }
  protected
    function AnyConnectionActive: Boolean;
    procedure EthernetConnectState(Sender: TObject; ConnectionState: TConnectionState);
    procedure EthernetError(Sender: TObject; MessageStr: string);
    procedure ComPortError(Sender: TObject; MessageStr: String);
    procedure ComPortConnectionState(Sender: TObject; NewConnectionState: TConnectionState);
    procedure LoadSettings(SettingType: TLoadSettingType);
    procedure MessageLogging(Sender: TObject; MessageStr: String);
    procedure NodeEvent(Sender: TObject; EventList: TList);
    procedure ScanComPorts;
    procedure StoreSettings(SettingType: TLoadSettingType);
    procedure ThrottleClosing(Throttle: TFormThrottle);
    procedure ThrottleHiding(Throttle: TFormThrottle);
    procedure UpdateMessageCountUI;
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
    property ShownOnce: Boolean read FShownOnce write FShownOnce;
    property SettingsLocked: Boolean read FSettingsLocked write FSettingsLocked;
  public
    { public declarations }
    property SettingsFilePath: WideString read FSettingsFilePath write FSettingsFilePath;
    property Throttles: TThrottleList read FThrottles write FThrottles;
    property ThrottleServerState: TThrottleServerState read FThrottleServerState write FThrottleServerState;
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
  TreeNode: TOlcbThrottleTreeNode;
begin
  begin
   Throttle := nil;
    begin      // In the Ping callback it will see a Cab without a link and fire off the Traction Proxy, it uses the CabID to match TreeNode with Throttle
      Throttle := Throttles.CreateThrottle(EthernetHub, ComPortHub, ImageList16x16);
      if Assigned(Throttle) then
      begin
        Throttle.PanelMain.Enabled := False;                                        // Not until the TreeNode is created
        Throttle.UpdateStatus(0, 'Creating and logging OpenLCB TreeNode into network.... Please Wait');
        TreeNode := TreeViewThrottles.Items.Add(nil, Throttle.Caption) as TOlcbThrottleTreeNode;
        TreeNode.Throttle := Throttle;
        NodeThread.AddTask(TNodeTaskAllocateNewNode.Create(NodePool.Pool[0].Info, NullNodeInfo, STATE_THROTTLE_ROOT_ALLOCATE_NEW, Throttle));  // Ask the Throttle Manager Node to create a new virtual Throttle node
        UpdateUI;
      end;
    end;
  end
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
  end
  else begin
    ComPortHub.RemoveComPort(nil);
  end;
  UpdateUI
end;

procedure TFormOlcbTrainMaster.ActionDetailedLoggingExecute(Sender: TObject);
begin
  StoreSettings(lstGeneral);
end;

procedure TFormOlcbTrainMaster.ActionEthernetClientConnectionExecute(Sender: TObject);
begin
  EthernetHub.Listener := False;
  EthernetHub.Enabled := ActionEthernetClientConnection.Checked;
end;

procedure TFormOlcbTrainMaster.ActionCloseSelectedThrottlesExecute(Sender: TObject);
begin
  ;
end;

procedure TFormOlcbTrainMaster.ActionEthernetListenerConnectionExecute(Sender: TObject);
begin
  EthernetHub.Listener := True;
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
  StoreSettings(lstGeneral);
  if ActionLogging.Checked then
    NodeThread.OnLogMessages := @MessageLogging
  else
    NodeThread.OnLogMessages := nil;
end;

procedure TFormOlcbTrainMaster.ActionLogInJMRIExecute(Sender: TObject);
begin
  StoreSettings(lstGeneral);
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

procedure TFormOlcbTrainMaster.ActionShowAllThrottlesExecute(Sender: TObject);
begin
  Throttles.ShowAll;
end;

procedure TFormOlcbTrainMaster.ActionStartNodeExecute(Sender: TObject);
begin
  if NodeThread.StackRunning then
  begin
    ActionStartNode.ImageIndex := 239;
    ActionStartNode.Caption := 'Node Stopped';
    NodeThread.EnableNode(False);
  end else
  begin
    ActionStartNode.ImageIndex := 240;
    ActionStartNode.Caption := 'Node Running';
    NodeThread.InitializeNode;
    NodeThread.EnableNode(True);
  end;
end;

procedure TFormOlcbTrainMaster.ActionZeroizeConfigMemoryExecute(Sender: TObject);
begin
  ZeroConfiguration
end;

function TFormOlcbTrainMaster.AnyConnectionActive: Boolean;
begin
  Result := (ActionEthernetClientConnection.Enabled or ActionEthernetListenerConnection.Enabled or ActionCOMConnection.Enabled) and (EthernetHub.Enabled or ComPortHub.Connected);
end;

procedure TFormOlcbTrainMaster.BitBtnRescanPortsClick(Sender: TObject);
begin
  ScanComPorts;
end;

procedure TFormOlcbTrainMaster.ButtonLocalHostClick(Sender: TObject);
begin
  EditEthernetLocalIP.Text := '127.0.0.1';
end;

procedure TFormOlcbTrainMaster.ButtonRefreshBufferCountClick(Sender: TObject);
begin
  UpdateMessageCountUI
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
          GlobalSettings.General.JMRILogFormat := ActionLogInJMRI.Checked;
          GlobalSettings.General.Logging := ActionLogging.Checked;
          GlobalSettings.General.DetailedLogging := ActionDetailedLogging.Checked;
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

procedure TFormOlcbTrainMaster.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  NodeThread.OnLogMessages := nil;
  EthernetHub.Enabled := False;
  ComPortHub.RemoveComPort(nil);
  Throttles.CloseAll;
  DestroyHubs;
end;

procedure TFormOlcbTrainMaster.FormCreate(Sender: TObject);
var
  Markup: TSynEditMarkupHighlightAllCaret;
begin
  CreateHubs;
  Throttles := TThrottleList.Create;
  Throttles.OnThrottleClose := @ThrottleClosing;
  Throttles.OnThrottleHide := @ThrottleHiding;

  ComPortHub.OnErrorMessage := @ComPortError;
  ComPortHub.OnConnectionStateChange := @ComPortConnectionState;

  EthernetHub.OnErrorMessage := @EthernetError;
  EthernetHub.OnConnectionStateChange := @EthernetConnectState;

  FComConnectionState := csDisconnected;
  FEthernetConnectionState := csDisconnected;
  EthernetConnectionCount := 0;


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

  // Initialize the State of the Node
  FThrottleServerState.Proxy.ID := NULL_NODE_ID;
  FThrottleServerState.Proxy.AliasID := 0;

  FConfigurationFile := '';
  UserStateMachine_Initialize;
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

    NodeThread.OnNodeEvent := @NodeEvent;
    if ActionLogging.Checked then
      NodeThread.OnLogMessages := @MessageLogging;

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

procedure TFormOlcbTrainMaster.UpdateMessageCountUI;
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

procedure TFormOlcbTrainMaster.MessageLogging(Sender: TObject; MessageStr: String);
begin
  PrintToSynMemo(MessageStr, SynMemoLog, Paused, ActionDetailedLogging.Checked, ActionLogInJMRI.Checked);
end;

procedure TFormOlcbTrainMaster.NodeEvent(Sender: TObject; EventList: TList);
var
  i, j: Integer;
  ProxyEvent: TNodeEventProxyAssigned;
  Event: TNodeEvent;
begin
  try
    for i := 0 to EventList.Count - 1 do
    begin
      if TObject( EventList[i]) is TNodeEventProxyAssigned then
      begin
        ProxyEvent := TObject( EventList[i]) as TNodeEventProxyAssigned;
        FThrottleServerState.Proxy.AliasID := ProxyEvent.NodeInfo.AliasID;
        FThrottleServerState.Proxy.ID := ProxyEvent.NodeInfo.ID;
        UpdateUI;
      end else
      if TObject( EventList[i]) is TNodeEventNodeCreated then
      begin
        Event := TNodeEvent( EventList[i]);
        if Throttles.IndexOf( Event.LinkedObj as TFormThrottle) > -1 then
          (Event.LinkedObj as TFormThrottle).EventNodeAllocated(Event as TNodeEventNodeCreated);
      end else
      if TObject( EventList[i]) is TNodeEventThrottleAssignedToTrain then
      begin
        Event := TNodeEvent( EventList[i]);
        if Throttles.IndexOf( Event.LinkedObj as TFormThrottle) > -1 then
          (Event.LinkedObj as TFormThrottle).EventTrainAllocated(Event as TNodeEventThrottleAssignedToTrain);
      end else
      if TObject( EventList[i]) is TNodeEventFunctionQuery then
      begin
         Event := TNodeEvent( EventList[i]);
        if Throttles.IndexOf( Event.LinkedObj as TFormThrottle) > -1 then
          (Event.LinkedObj as TFormThrottle).EventFunctionQuery(Event as TNodeEventFunctionQuery);
      end else
      if TObject( EventList[i]) is TNodeEventSpeedDirQuery then
      begin
        Event := TNodeEvent( EventList[i]);
        if Throttles.IndexOf( Event.LinkedObj as TFormThrottle) > -1 then
          (Event.LinkedObj as TFormThrottle).EventSpeedDirQuery(Event as TNodeEventSpeedDirQuery);
      end else
      if TObject( EventList[i]) is TNodeEventIsTrain then
      begin
        Event := TNodeEvent( EventList[i]);
        if Event.LinkedObj = nil then
        begin
          for j := 0 to Throttles.Count - 1 do
            Throttles[j].EventIsTrain(Event as TNodeEventIsTrain);
        end else
        begin
          if Throttles.IndexOf( Event.LinkedObj as TFormThrottle) > -1 then
          (Event.LinkedObj as TFormThrottle).EventIsTrain(Event as TNodeEventIsTrain);
        end;
      end else
      if TObject( EventList[i]) is TNodeEventSimpleTrainNodeInfo then
      begin
        Event := TNodeEvent( EventList[i]);
        if Throttles.IndexOf( Event.LinkedObj as TFormThrottle) > -1 then
          (Event.LinkedObj as TFormThrottle).EventSimpleTrainNodeInfo(Event as TNodeEventSimpleTrainNodeInfo);
      end;
      TObject( EventList[i]).Destroy
    end;
  finally
    FreeAndNil(EventList);
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

    ActionAddNewThrottle.Enabled := not NMRAnetUtilities_NullNodeIDInfo(FThrottleServerState.Proxy) and AnyConnectionActive;

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

    if EthernetHub.Listener and EthernetHub.Enabled then
      Statusbar.Panels[2].Text := 'Clients: ' + IntToStr(EthernetConnectionCount)
    else
      Statusbar.Panels[2].Text := '';

    UpdateMessageCountUI;
  end;
end;

procedure TFormOlcbTrainMaster.WMCloseConnections(var Message: TMessage);
begin
  ActionEthernetListenerConnection.Checked := False;
  ActionEthernetClientConnection.Checked := False;
  ActionCOMConnection.Checked := False;
  UpdateUI
end;

end.

