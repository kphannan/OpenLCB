unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, ActnList, Menus, Buttons, Spin, form_throttle,
  com_port_hub, ethernet_hub, olcb_app_common_settings, file_utilities,
  olcb_utilities;

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
    CheckBoxAutoScanAtStart: TCheckBox;
    ComboBoxBaud: TComboBox;
    ComboBoxComPort: TComboBox;
    ComboBoxDataBits: TComboBox;
    ComboBoxFlowControl: TComboBox;
    ComboBoxParity: TComboBox;
    ComboBoxStopBits: TComboBox;
    EditAliasID: TEdit;
    EditEthernetLocalIP: TEdit;
    EditNodeID: TEdit;
    ImageList: TImageList;
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
    MenuItemActionsNewThrottle: TMenuItem;
    MenuItemActionsDeleteThrottles: TMenuItem;
    MenuItemActions: TMenuItem;
    MenuItemFile: TMenuItem;
    PageControlMain: TPageControl;
    PanelBkGnd: TPanel;
    SpinEditEthernetLocalPort: TSpinEdit;
    SpinEditSendPacketDelay: TSpinEdit;
    Splitter1: TSplitter;
    StatusBar: TStatusBar;
    TabSheetGeneral: TTabSheet;
    TabSheetMain: TTabSheet;
    TabSheetEthernet: TTabSheet;
    TabSheetCAN: TTabSheet;
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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TreeViewThrottlesCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
  private
    FComConnectionState: TConnectionState;
    FComPortHub: TComPortHub;
    FThrottles: TThrottleList;
    { private declarations }
  protected
    procedure ComPortError(Sender: TObject; MessageStr: String);
    procedure ComPortConnectionState(Sender: TObject; ConnectionState: TConnectionState);
    procedure ThrottleClosing(Throttle: TFormThrottle);
    procedure ThrottleHiding(Throttle: TFormThrottle);
    property ComConnectionState: TConnectionState read FComConnectionState write FComConnectionState;
  public
    { public declarations }

    property ComPortHub: TComPortHub read FComPortHub write FComPortHub;
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
  Throttle := Throttles.CreateThrottle;
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

procedure TFormOlcbTrainMaster.ComPortConnectionState(Sender: TObject; ConnectionState: TConnectionState);
begin
  UpdateUI;
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
  ComPortHub.Destroy;
end;

procedure TFormOlcbTrainMaster.FormCreate(Sender: TObject);
begin
  Throttles := TThrottleList.Create;
  Throttles.OnThrottleClose := @ThrottleClosing;
  Throttles.OnThrottleHide := @ThrottleHiding;
  ComPortHub := TComPortHub.Create;
  ComPortHub.SyncErrorMessageFunc := @ComPortError;
  ComPortHub.SyncConnectionStateFunc := @ComPortConnectionState;
  FComConnectionState := csDisconnected;
end;

procedure TFormOlcbTrainMaster.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FThrottles);
end;

procedure TFormOlcbTrainMaster.FormShow(Sender: TObject);
begin
  UpdateUI
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

