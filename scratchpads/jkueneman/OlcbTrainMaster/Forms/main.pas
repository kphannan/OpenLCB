unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, ActnList, Menus, Buttons, Spin, form_throttle,
  com_port_hub, ethernet_hub, olcb_app_common_settings, file_utilities,
  olcb_utilities, synaser, common_utilities, lcltype, olcb_transport_layer;

const
  BUNDLENAME             = 'OpenLCB TrainMaster';
  PATH_LINUX_APP_FOLDER  = 'olcbtrainmaster/';
  PATH_SETTINGS_FILE     = 'settings.ini';

type
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
    ButtonAddThrottles: TButton;
    ButtonDeleteSelectedThrottles: TButton;
    ButtonCloseAllThrottles: TButton;
    ButtonShowAllThrottles: TButton;
    ButtonHideAllThrottles: TButton;
    ComboBoxBaud: TComboBox;
    ComboBoxComPort: TComboBox;
    ComboBoxDataBits: TComboBox;
    ComboBoxFlowControl: TComboBox;
    ComboBoxParity: TComboBox;
    ComboBoxStopBits: TComboBox;
    EditAliasID: TEdit;
    EditEthernetLocalIP: TEdit;
    EditNodeID: TEdit;
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
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
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
    TreeViewThrottles: TTreeView;
    procedure ActionAddNewThrottleExecute(Sender: TObject);
    procedure ActionCloseAllThrottlesExecute(Sender: TObject);
    procedure ActionCOMConnectionExecute(Sender: TObject);
    procedure ActionCloseSelectedThrottlesExecute(Sender: TObject);
    procedure ActionEthernetConnectionExecute(Sender: TObject);
    procedure ActionHideAllThrottlesExecute(Sender: TObject);
    procedure ActionShowAllThrottlesExecute(Sender: TObject);
    procedure BitBtnRescanPortsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
    FEthernetHub: TEthernetHub;
    FSettingsFilePath: WideString;
    FShownOnce: Boolean;
    FThrottles: TThrottleList;
    { private declarations }
  protected
    procedure ComPortError(Sender: TObject; MessageStr: String);
    procedure ComPortConnectionState(Sender: TObject; NewConnectionState: TConnectionState);
    procedure LoadSettings;
    procedure ScanComPorts;
    procedure StoreSettings;
    procedure ThrottleClosing(Throttle: TFormThrottle);
    procedure ThrottleHiding(Throttle: TFormThrottle);
    function DispatchTask(Task: TTaskOlcbBase): Boolean;

    property AppAboutCmd: TMenuItem read FAppAboutCmd write FAppAboutCmd;
    property ComConnectionState: TConnectionState read FComConnectionState write FComConnectionState;
    {$IFDEF DARWIN}
    property OSXMenu: TMenuItem read FOSXMenu write FOSXMenu;
    property OSXSep1Cmd: TMenuItem read FOSXSep1Cmd write FOSXSep1Cmd;
    property OSXPrefCmd: TMenuItem read FOSXPrefCmd write FOSXPrefCmd;
    {$ENDIF}
    property ShownOnce: Boolean read FShownOnce write FShownOnce;
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
  ShowMessage('Ethernet not supported yet');
  if ActionEthernetConnection.Checked then
  begin

  end
  else begin

  end;
end;

procedure TFormOlcbTrainMaster.ActionHideAllThrottlesExecute(Sender: TObject);
begin
  Throttles.HideAll;
end;

procedure TFormOlcbTrainMaster.ActionShowAllThrottlesExecute(Sender: TObject);
begin
  Throttles.ShowAll;
end;

procedure TFormOlcbTrainMaster.BitBtnRescanPortsClick(Sender: TObject);
begin
  ScanComPorts;
end;

procedure TFormOlcbTrainMaster.ComPortConnectionState(Sender: TObject; NewConnectionState: TConnectionState);
begin
  ComConnectionState := NewConnectionState;
  UpdateUI;
end;

procedure TFormOlcbTrainMaster.LoadSettings;
begin
  //Ethernet
  EditEthernetLocalIP.Text := GlobalSettings.Ethernet.LocalIP;
  SpinEditEthernetLocalPort.Value := GlobalSettings.Ethernet.LocalPort;

  // ComPort
  ScanComPorts;
  ComboBoxComPort.ItemIndex := ComboBoxComPort.Items.IndexOf(GlobalSettings.ComPort.Port);
  if (ComboBoxComPort.ItemIndex < 0) and (ComboBoxComPort.Items.Count > 0) then
    ComboBoxComPort.ItemIndex := 0;
  ComboBoxBaud.ItemIndex := ComboBoxBaud.Items.IndexOf(IntToStr(GlobalSettings.ComPort.BaudRate));
  ComboBoxDataBits.ItemIndex := ComboBoxDataBits.Items.IndexOf(IntToStr( GlobalSettings.ComPort.DataBits));
  ComboBoxStopBits.ItemIndex := ComboBoxStopBits.Items.IndexOf(IntToStr( GlobalSettings.ComPort.StopBits));
  ComboBoxParity.ItemIndex := Integer( GlobalSettings.ComPort.Parity);
  ComboBoxFlowControl.ItemIndex := Integer( GlobalSettings.ComPort.FlowControl);

  // General
  SpinEditSendPacketDelay.Value := GlobalSettings.General.SendPacketDelay;
  EditAliasID.Caption := ValidateHex( GlobalSettings.General.AliasID);
  EditNodeID.Caption := ValidateHex(GlobalSettings.General.NodeID);
end;

procedure TFormOlcbTrainMaster.ScanComPorts;
begin
  ComboBoxComPort.Items.Delimiter := ';';
  ComboBoxComPort.Items.DelimitedText := StringReplace(GetSerialPortNames, ',', ';', [rfReplaceAll, rfIgnoreCase]);
  if ComboBoxComPort.Items.Count > 0 then
    ComboBoxComPort.ItemIndex := 0;
end;

procedure TFormOlcbTrainMaster.StoreSettings;
begin
  // ComPort
  GlobalSettings.ComPort.Port := ComboBoxComPort.Caption;
  GlobalSettings.ComPort.BaudRate := StrToInt( ComboBoxBaud.Caption);
  GlobalSettings.ComPort.DataBits := StrToInt( ComboBoxDataBits.Caption);
  GlobalSettings.ComPort.StopBits := StrToInt( ComboBoxStopBits.Caption);
  GlobalSettings.ComPort.Parity := TComPortParity( ComboBoxParity.ItemIndex);
  GlobalSettings.ComPort.FlowControl := TComPortFlowControl( ComboBoxFlowControl.ItemIndex);

  // General
  GlobalSettings.General.SendPacketDelay := SpinEditSendPacketDelay.Value;
  GlobalSettings.General.AliasID := ValidateHex(EditAliasID.Caption);
  GlobalSettings.General.NodeID := ValidateHex(EditNodeID.Caption);

  // Ethernet
  GlobalSettings.Ethernet.LocalIP := EditEthernetLocalIP.Text;      // Should validate this
  GlobalSettings.Ethernet.LocalPort := SpinEditEthernetLocalPort.Value;

  GlobalSettings.SaveToFile(UTF8ToSys( SettingsFilePath));
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
  FShownOnce := False;
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

procedure TFormOlcbTrainMaster.TabSheetComHide(Sender: TObject);
begin
  StoreSettings
end;

procedure TFormOlcbTrainMaster.TabSheetComShow(Sender: TObject);
begin
  LoadSettings
end;

procedure TFormOlcbTrainMaster.TabSheetEthernetHide(Sender: TObject);
begin
  StoreSettings
end;

procedure TFormOlcbTrainMaster.TabSheetEthernetShow(Sender: TObject);
begin
  LoadSettings
end;

procedure TFormOlcbTrainMaster.TabSheetGeneralHide(Sender: TObject);
begin
  StoreSettings
end;

procedure TFormOlcbTrainMaster.TabSheetGeneralShow(Sender: TObject);
begin
  LoadSettings
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

procedure TFormOlcbTrainMaster.TreeViewThrottlesCreateNodeClass(
  Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
  NodeClass := TOlcbThrottleTreeNode
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


  if ActionEthernetConnection.Checked then
  begin
    ActionEthernetConnection.ImageIndex := 988;
    Statusbar.Panels[1].Text := 'Ethernet: Connected';
  end
  else begin
    ActionEthernetConnection.ImageIndex := 989;
    Statusbar.Panels[1].Text := 'Ethernet: Disconnected';
  end;

end;

end.

