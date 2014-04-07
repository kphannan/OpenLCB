unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynMemo, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, ActnList, Menus, Buttons, Spin, form_throttle,
  com_port_hub, ethernet_hub, olcb_app_common_settings, file_utilities,
  olcb_utilities, synaser, common_utilities, lcltype, olcb_transport_layer,
  types, olcb_defines;

const
  BUNDLENAME             = 'OpenLCB TrainMaster';
  PATH_LINUX_APP_FOLDER  = 'olcbtrainmaster/';
  PATH_SETTINGS_FILE     = 'settings.ini';

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
    ActionEthernetConnection: TAction;
    ActionCOMConnection: TAction;
    ActionList: TActionList;
    BitBtnRescanPorts: TBitBtn;
    Button1: TButton;
    ButtonAddThrottles: TButton;
    ButtonCloseAllThrottles: TButton;
    ButtonDeleteSelectedThrottles: TButton;
    ButtonHideAllThrottles: TButton;
    ButtonRediscoverProxies: TButton;
    ButtonShowAllThrottles: TButton;
    CheckBoxLoggingEnabled: TCheckBox;
    ComboBoxBaud: TComboBox;
    ComboBoxComPort: TComboBox;
    ComboBoxDataBits: TComboBox;
    ComboBoxFlowControl: TComboBox;
    ComboBoxParity: TComboBox;
    ComboBoxStopBits: TComboBox;
    EditAliasID: TEdit;
    EditEthernetLocalIP: TEdit;
    EditNodeID: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBoxThrottles: TGroupBox;
    ImageList16x16: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    LabelBaud: TLabel;
    LabelComPort: TLabel;
    LabelDataBits: TLabel;
    LabelFlowControl: TLabel;
    LabelParity: TLabel;
    LabelStopBits: TLabel;
    MainMenu: TMainMenu;
    MenuItemToolsLogging: TMenuItem;
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
    SpinEditEthernetLocalPort: TSpinEdit;
    SpinEditSendPacketDelay: TSpinEdit;
    Splitter1: TSplitter;
    StatusBar: TStatusBar;
    SynMemoLogging: TSynMemo;
    TabSheetGeneral: TTabSheet;
    TabSheetMain: TTabSheet;
    TabSheetEthernet: TTabSheet;
    TabSheetCom: TTabSheet;
    ToolBarMain: TToolBar;
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
    procedure ActionEthernetConnectionExecute(Sender: TObject);
    procedure ActionHideAllThrottlesExecute(Sender: TObject);
    procedure ActionLoggingExecute(Sender: TObject);
    procedure ActionRediscoverProxiesExecute(Sender: TObject);
    procedure ActionShowAllThrottlesExecute(Sender: TObject);
    procedure BitBtnRescanPortsClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
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
    procedure SpinEditEthernetLocalPortChange(Sender: TObject);
    procedure SpinEditSendPacketDelayChange(Sender: TObject);
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
    FComPortHub: TComPortHub;
    FEthernetConnectionState: TConnectionState;
    FEthernetHub: TEthernetHub;
    FSettingsFilePath: WideString;
    FSettingsLocked: Boolean;
    FShownOnce: Boolean;
    FThrottles: TThrottleList;
    { private declarations }
  protected
    procedure EthernetReceiveLogging(Sender: TObject; MessageStr: String);
    procedure EthernetSendLogging(Sender: TObject; MessageStr: String);
    procedure ComPortError(Sender: TObject; MessageStr: String);
    procedure ComPortConnectionState(Sender: TObject; NewConnectionState: TConnectionState);
    procedure ComPortReceiveLogging(Sender: TObject; MessageStr: String);
    procedure ComPortSendLogging(Sender: TObject; MessageStr: String);
    procedure HubConnect(HostIP: string; HostPort: Integer) ;
    procedure HubDisconnect(HostIP: string; HostPort: Integer) ;
    procedure LoadSettings(SettingType: TLoadSettingType);
    procedure ScanComPorts;
    procedure StoreSettings(SettingType: TLoadSettingType);
    procedure ThrottleClosing(Throttle: TFormThrottle);
    procedure ThrottleHiding(Throttle: TFormThrottle);
    function DispatchTask(Task: TTaskOlcbBase): Boolean;

    property AppAboutCmd: TMenuItem read FAppAboutCmd write FAppAboutCmd;
    property ComConnectionState: TConnectionState read FComConnectionState write FComConnectionState;
    property EthernetConnectionState: TConnectionState read FEthernetConnectionState write FEthernetConnectionState;
    {$IFDEF DARWIN}
    property OSXMenu: TMenuItem read FOSXMenu write FOSXMenu;
    property OSXSep1Cmd: TMenuItem read FOSXSep1Cmd write FOSXSep1Cmd;
    property OSXPrefCmd: TMenuItem read FOSXPrefCmd write FOSXPrefCmd;
    {$ENDIF}
    property ShownOnce: Boolean read FShownOnce write FShownOnce;
    property SettingsLocked: Boolean read FSettingsLocked write FSettingsLocked;
  public
    { public declarations }
    property ComPortHub: TComPortHub read FComPortHub write FComPortHub;
    property EthernetHub: TEthernetHub read FEthernetHub write FEthernetHub;
    property SettingsFilePath: WideString read FSettingsFilePath write FSettingsFilePath;
    property Throttles: TThrottleList read FThrottles write FThrottles;
    procedure UpdateUI;
  end;

var
  FormOlcbTrainMaster: TFormOlcbTrainMaster;

implementation

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
 //   ComPortHub.EnableReceiveMessages := ActionToolsCOMPortMessageLogShow.Checked;
 //   ComPortHub.EnableSendMessages := ActionToolsCOMPortMessageLogShow.Checked;

 //   if GlobalSettings.General.AutoScanNetworkAtBoot then
 //     ActionOpenLCBCommandIdentifyIDGlobal.Execute;
  end
  else begin
    ComPortHub.RemoveComPort(nil);
  end;
  UpdateUI
end;

procedure TFormOlcbTrainMaster.ActionCloseSelectedThrottlesExecute(Sender: TObject);
begin
  ;
end;

procedure TFormOlcbTrainMaster.ActionEthernetConnectionExecute(Sender: TObject);
begin
  if ActionEthernetConnection.Checked then
  begin
    EthernetHub.Enabled := True;
  end
  else begin
    EthernetHub.Enabled := False;
  end;
end;

procedure TFormOlcbTrainMaster.ActionHideAllThrottlesExecute(Sender: TObject);
begin
  Throttles.HideAll;
end;

procedure TFormOlcbTrainMaster.ActionLoggingExecute(Sender: TObject);
begin
  ComPortHub.EnableReceiveMessages := ActionLogging.Checked;
  ComPortHub.EnableSendMessages := ActionLogging.Checked;
  EthernetHub.EnableReceiveMessages := ActionLogging.Checked;
  EthernetHub.EnableSendMessages := ActionLogging.Checked;
end;

procedure TFormOlcbTrainMaster.ActionRediscoverProxiesExecute(Sender: TObject);
var
  Task: TTaskIdentifyProducer;
begin
  // Send a IsProxy Message and collect the results
  Task := TTaskIdentifyProducer.Create(GlobalSettings.General.AliasIDAsVal, 0, True, EVENT_PROXY);
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

procedure TFormOlcbTrainMaster.BitBtnRescanPortsClick(Sender: TObject);
begin
  ScanComPorts;
end;

procedure TFormOlcbTrainMaster.Button1Click(Sender: TObject);
begin
  EditEthernetLocalIP.Text := '127.0.0.1';
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
  SynMemoLogging.BeginUpdate;
  try
    SynMemoLogging.Lines.Add(MessageStr);
  finally
    SynMemoLogging.EndUpdate;
  end;
end;

procedure TFormOlcbTrainMaster.ComPortSendLogging(Sender: TObject; MessageStr: String);
begin
  SynMemoLogging.BeginUpdate;
  try
    SynMemoLogging.Lines.Add(MessageStr);
  finally
    SynMemoLogging.EndUpdate;
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
            SpinEditEthernetLocalPort.Value := GlobalSettings.Ethernet.LocalPort;
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
          GlobalSettings.Ethernet.LocalPort := SpinEditEthernetLocalPort.Value;
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

procedure TFormOlcbTrainMaster.ComPortError(Sender: TObject; MessageStr: String);
begin
 ShowMessage(MessageStr);
 ActionCOMConnection.Checked := False;
end;

procedure TFormOlcbTrainMaster.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  Throttles.CloseAll;
  FreeAndNil(FEthernetHub);
  FreeAndNil(FComPortHub);
end;

procedure TFormOlcbTrainMaster.FormCreate(Sender: TObject);
begin
  Throttles := TThrottleList.Create;
  Throttles.OnThrottleClose := @ThrottleClosing;
  Throttles.OnThrottleHide := @ThrottleHiding;
  ComPortHub := TComPortHub.Create;
  ComPortHub.SyncErrorMessageFunc := @ComPortError;
  ComPortHub.SyncConnectionStateFunc := @ComPortConnectionState;
  EthernetHub := TEthernetHub.Create;
  FComConnectionState := csDisconnected;
  FEthernetConnectionState := csDisconnected;
  FShownOnce := False;
  FSettingsLocked := False;
  ComPortHub.SyncReceiveMessageFunc := @ComPortReceiveLogging;
  ComPortHub.SyncSendMessageFunc := @ComPortSendLogging;
  EthernetHub.SyncReceiveMessageFunc := @EthernetReceiveLogging;
  EthernetHub.SyncSendMessageFunc := @EthernetSendLogging;
end;

procedure TFormOlcbTrainMaster.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FThrottles);
end;

procedure TFormOlcbTrainMaster.FormShow(Sender: TObject);
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
 //   MenuItemToolsSep2.Visible := False;
 //   FormMessageLog.SynMemo.Font.Height := 0;
 //   FormEthernetMessageLog.SynMemo.Font.Height := 0;
    {$ELSE}
    AppAboutCmd := TMenuItem.Create(Self);
    AppAboutCmd.Action := ActionHelpAboutShow;
    MenuItemHelp.Add(AppAboutCmd);
    {$ENDIF}
 //   FormMessageLog.HideCallback := @SyncMessageLogHide;
 //   FormEthernetMessageLog.HideCallback := @SyncEthernetMessageLogHide;
    {$IFDEF Linux}
    FormSettings.SettingsFilePath:= GetSettingsPath + {PATH_LINUX_APP_FOLDER +} PATH_SETTINGS_FILE;
    GlobalSettings.LoadFromFile(UTF8ToSys( GetSettingsPath + {PATH_LINUX_APP_FOLDER +} PATH_SETTINGS_FILE));
    {$ELSE}
    SettingsFilePath := GetSettingsPath + PATH_SETTINGS_FILE;
    GlobalSettings.LoadFromFile(UTF8ToSys( GetSettingsPath + PATH_SETTINGS_FILE));
    {$ENDIF}
    ShownOnce := True;
  end;
  UpdateUI
end;

procedure TFormOlcbTrainMaster.HubConnect(HostIP: string; HostPort: Integer);
begin

end;

procedure TFormOlcbTrainMaster.HubDisconnect(HostIP: string; HostPort: Integer);
begin

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

procedure TFormOlcbTrainMaster.TreeViewThrottlesCreateNodeClass(
  Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
  NodeClass := TOlcbThrottleTreeNode
end;

procedure TFormOlcbTrainMaster.EthernetReceiveLogging(Sender: TObject; MessageStr: String);
begin

end;

procedure TFormOlcbTrainMaster.EthernetSendLogging(Sender: TObject; MessageStr: String);
begin

end;

procedure TFormOlcbTrainMaster.UpdateUI;
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
        ActionEthernetConnection.ImageIndex := 989;
        Statusbar.Panels[1].Text := 'Ethernet: Disconnected';
      end;
    csConnecting :
      begin
        ActionEthernetConnection.ImageIndex := 989;
       Statusbar.Panels[1].Text := 'Ethernet: Connecting';
      end;
    csConnected :
      begin
        ActionEthernetConnection.ImageIndex := 988;
        Statusbar.Panels[1].Text := 'Ethernet: Connected';
      end;
  end;
end;

end.

