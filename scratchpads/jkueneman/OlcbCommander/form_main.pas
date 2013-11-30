{

TODO:  Make Node Captions More User Friendly - Bob Jacobson Mar 31, 2013

 - Network Tree
  |-0x0567
  |  |- NodeID: 01.02.03.04.05.06.07.00
  |  |- Configuration Memory Protocol
  |  |  |- Configuration Memory Options
  |  |  |- Address Spaces
  |  |    |- 0xFF - CDI
  |  |    |- 0xFE - All
  |  |    |- 0xFD - Configuration
  |  |    |- ect...
  |  |- Events
  |  |  |- Consumers
  |  |  |- Producers
  |  |- Protocol Identification Protocol
  |  |- Simple Node Information Protocol
  |-0x0456
     |- NodeID: 01.02.03.04.05.06.07.08
     |- Configuration Memory Protocol
     |  |- Configuration Memory Options
     |  |- Address Spaces
     |    |- 0xFF - CDI
     |    |- 0xFE - All
     |    |- 0xFD - Configuration
     |    |- ect...
     |- Events
     |  |- Consumers
     |  |- Producers
     |- Protocol Identification Protocol
     |- Simple Node Information Protocol

 - Train Tree
   |-0x0567
   |  |- NodeID: 01.02.03.04.05.06.07.08
   |  |- IsTrain: True
   |  |- IsIdleProxy: True
   |  |- IsInUserProxy: False
   |  |- DCC Address - Unassigned
   |-0x0789
   |  |- NodeID: 01.02.03.04.05.06.07.00
   |  |- IsTrain: True
   |  |- IsIdleProxy: False
   |  |- IsInUserProxy: True
   |  |- DCC Address - Unassigned
   |-0x0A4C
      |- NodeID: 01.02.03.04.05.06.07.01
      |- IsTrain: True
      |- IsIdleProxy: False
      |- IsInUserProxy: True
      |- DCC Address - 20 [Short]

}
unit form_main;

{$mode objfpc}{$H+}

{.$DEFINE DEBUG_THREAD}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, ActnList, ComCtrls, ExtCtrls, Buttons, com_port_hub,
  olcb_app_common_settings, file_utilities, form_settings, form_about, lcltype,
  types, olcb_utilities, olcb_defines, form_messagelog, olcb_node,
  form_config_mem_viewer, laz2_DOM, laz2_XMLRead,
  laz2_XMLWrite, common_utilities, form_awesome_throttle, blcksock,
  form_train_config_editor, ethernet_hub, form_ethernet_messagelog,

  {$IFDEF DEBUG_THREAD}
  form_thread_debug,
  {$ENDIF}
  olcb_transport_layer;

const
  BUNDLENAME             = 'OpenLCB Commander';
  PATH_LINUX_APP_FOLDER  = 'olcbcommander/';
  PATH_SETTINGS_FILE     = 'settings.ini';

  STR_NETWORKTREE_ROOT  = 'Network Tree';
  STR_TRAINTREE_ROOT    = 'Train Tree';
  STR_COMMANDSTATIONTREE_ROOT = 'Command Station Tree';
  STR_PROTOCOLSUPPORT   = 'Protocol Identification Protocol';
  STR_SNIP              = 'Simple Node Identification Protocol (SNIP/SNII)';
  STR_MEM_PROTOCOL      = 'Configuration Memory Protocol';
  STR_CONFIGMEM_OPTIONS = 'Options';
  STR_CONFIGMEM_ADDRESS_SPACES = 'Address Spaces';
  STR_EVENTS            = 'Events';
  STR_CONSUMERS         = 'Consumers';
  STR_PRODUCERS         = 'Producers';
  STR_PROTOCOLS         = 'Protocols';
  STR_NODEID            = 'Node ID';
  STR_ALIASID           = 'Alias ID';
  STR_ISTRAIN           = 'IsTrain';
  STR_ISIDLEPROXY       = 'IsIdleProxy';
  STR_ISINUSEPROXY      = 'IsInUseProxy';
  STR_DCC_ADDRESS       = 'DCC Address';
  STR_SHORT_ADDRESS     = '[Short]';
  STR_LONG_ADDRESS      = '[Long]';
  STR_UNKNOWN           = '[Unknown]';
  STR_UNASSIGNED        = '[Unassigned]';

type

  { TOlcbTreeNode }

  TOlcbTreeNode = class( TTreeNode)
  private
    FExpandedOnce: Boolean;
    FOlcbData: TOpenLcbNode;
  public
    constructor Create(AnOwner: TTreeNodes);
    destructor Destroy; override;
    property OlcbData: TOpenLcbNode read FOlcbData write FOlcbData;
    property ExpandedOnce: Boolean read FExpandedOnce write FExpandedOnce;
  end;

  { TFormOLCB_Commander }

  TFormOLCB_Commander = class(TForm)
    ActionOpenLCBCommandReadFDIWithStream: TAction;
    ActionOpenLCBCommandReadConfigurationWithStream: TAction;
    ActionOpenLCBCommandReadAllWithStream: TAction;
    ActionOpenLCBCommandReadCDIWithStream: TAction;
    ActionShowLoopTime: TAction;
    ActionToolsEthernetHubDisconnect: TAction;
    ActionToolsEthernetHubMessageLogShow: TAction;
    ActionToolsEthernetHubConnect: TAction;
    ActionConfigEditorsHideAll: TAction;
    ActionConfigEditorsShowAll: TAction;
    ActionConfigEditorsCloseAll: TAction;
    ActionConfigEditorsCreate: TAction;
    ActionOpenLCBCommandReadFDI: TAction;
    ActionTreeviewNetworkCollapseSelected: TAction;
    ActionTreeviewNetworkExpandSelected: TAction;
    ActionOpenLCBCommandIdentifyEvents: TAction;
    ActionThrottlesHideAll: TAction;
    ActionThrottlesShowAll: TAction;
    ActionThrottlesCloseAll: TAction;
    ActionThrottlesCreate: TAction;
    ActionOpenLCBCommandReadUserACDI: TAction;
    ActionOpenLCBCommandReadMfgACDI: TAction;
    ActionOpenLCBCommandReadConfiguration: TAction;
    ActionOpenLCBCommandReadAll: TAction;
    ActionOpenLCBCommandReadCDI: TAction;
    ActionTreeviewNetworkExpandAll: TAction;
    ActionTreeviewNetworkCollapseAll: TAction;
    ActionOpenLCBCommandMemConfigAllSpacesInfo: TAction;
    ActionOpenLCBCommandMemConfigOptions: TAction;
    ActionOpenLCBCommandAll: TAction;
    ActionOpenLCBCommandSNII: TAction;
    ActionOpenLCBCommandProtocolSupport: TAction;
    ActionToolsCOMPortMessageLogShow: TAction;
    ActionOpenLCBCommandIdentifyIDGlobal: TAction;
    ActionToolsComDisconnect: TAction;
    ActionToolsComConnect: TAction;
    ActionHelpAboutShow: TAction;
    ActionToolsPreferenceShowMac: TAction;
    ActionToolsSettingsShowWin: TAction;
    ActionListMain: TActionList;
    ApplicationProperties: TApplicationProperties;
    ImageList24x24: TImageList;
    ImageList16x16: TImageList;
    ImageListMainSmall: TImageList;
    ListBoxLog: TListBox;
    MainMenu: TMainMenu;
    MainMenu1: TMainMenu;
    MenuItemReadFDIWithStream: TMenuItem;
    MenuItemReadCDIWithStream: TMenuItem;
    MenuItemReadAllMemoryWithStream: TMenuItem;
    MenuItemReadConfigMemWithStream: TMenuItem;
    MenuItemConfigurationWithStreamSubMenu: TMenuItem;
    MenuItemHelpLoopTime: TMenuItem;
    MenuItemToolsEthernetHubConnect: TMenuItem;
    MenuItemToolsEthernetHubDisconnect: TMenuItem;
    MenuItemToolsEthernetMessageLog: TMenuItem;
    MenuItemToolsSep3: TMenuItem;
    MenuItemConfigEditorsSep1: TMenuItem;
    MenuItemConfigEditorsShowAll: TMenuItem;
    MenuItemConfigEditorsHide: TMenuItem;
    MenuItemConfigEditorsClose: TMenuItem;
    MenuItemConfigEditorsSep0: TMenuItem;
    MenuItemConfigEditorCreate: TMenuItem;
    MenuItemConfigEditors: TMenuItem;
    MenuItemConfigurationSubMenu: TMenuItem;
    MenuItemConfigureNode: TMenuItem;
    MenuItemTVPopupSep4: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItemThrottlesSep0: TMenuItem;
    MenuItemThrottlesCreate: TMenuItem;
    MenuItemThrottlesSep1: TMenuItem;
    MenuItemThrottlesCloseAll: TMenuItem;
    MenuItemThrottlesShowAll: TMenuItem;
    MenuItemThrottlesHideAll: TMenuItem;
    MenuItemThrottles: TMenuItem;
    MenuItemReadCDI: TMenuItem;
    MenuItemReadConfigMem: TMenuItem;
    MenuItemReadAllMemory: TMenuItem;
    MenuItemReadMfgACDI: TMenuItem;
    MenuItemReadUserACDI: TMenuItem;
    MenuItemTVPopupIdentifyGlobal: TMenuItem;
    MenuItemTVPopupExpandAll: TMenuItem;
    MenuItemTVPopupCollapseAll: TMenuItem;
    MenuItemTVPopupSep3: TMenuItem;
    MenuItemTVPopupSep2: TMenuItem;
    MenuItemReadFDI: TMenuItem;
    MenuItemTVPopupSep1: TMenuItem;
    MenuItemToolsSep2: TMenuItem;
    MenuItemToolsMessageLog: TMenuItem;
    MenuItemToolsComConnect: TMenuItem;
    MenuItemToolsComDisconnect: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemToolsSettingsShow: TMenuItem;
    MenuItemTools: TMenuItem;
    PageControlMain: TPageControl;
    Panel1: TPanel;
    PanelNetwork: TPanel;
    PopupMenuTreeNode: TPopupMenu;
    Splitter1: TSplitter;
    StatusBar: TStatusBar;
    TabSheetNetwork: TTabSheet;
    ToolBar1: TToolBar;
    ToolButtonLogEthernet: TToolButton;
    TreeViewNetwork: TTreeView;
    procedure ActionConfigEditorsCloseAllExecute(Sender: TObject);
    procedure ActionConfigEditorsCreateExecute(Sender: TObject);
    procedure ActionConfigEditorsHideAllExecute(Sender: TObject);
    procedure ActionConfigEditorsShowAllExecute(Sender: TObject);
    procedure ActionOpenLCBCommandReadAllWithStreamExecute(Sender: TObject);
    procedure ActionOpenLCBCommandReadCDIWithStreamExecute(Sender: TObject);
    procedure ActionOpenLCBCommandReadConfigurationWithStreamExecute(Sender: TObject);
    procedure ActionOpenLCBCommandReadFDIWithStreamExecute(Sender: TObject);
    procedure ActionShowLoopTimeExecute(Sender: TObject);
    procedure ActionToolsEthernetHubDisconnectExecute(Sender: TObject);
    procedure ActionToolsEthernetHubMessageLogShowExecute(Sender: TObject);
    procedure ActionHelpAboutShowExecute(Sender: TObject);
    procedure ActionOpenLCBCommandAllExecute(Sender: TObject);
    procedure ActionOpenLCBCommandIdentifyEventsExecute(Sender: TObject);
    procedure ActionOpenLCBCommandIdentifyIDGlobalExecute(Sender: TObject);
    procedure ActionOpenLCBCommandMemConfigOptionsExecute(Sender: TObject);
    procedure ActionOpenLCBCommandMemConfigAllSpacesInfoExecute(Sender: TObject);
    procedure ActionOpenLCBCommandProtocolSupportExecute(Sender: TObject);
    procedure ActionOpenLCBCommandReadAllExecute(Sender: TObject);
    procedure ActionOpenLCBCommandReadCDIExecute(Sender: TObject);
    procedure ActionOpenLCBCommandReadConfigurationExecute(Sender: TObject);
    procedure ActionOpenLCBCommandReadFDIExecute(Sender: TObject);
    procedure ActionOpenLCBCommandReadFSIExecute(Sender: TObject);
    procedure ActionOpenLCBCommandReadMfgACDIExecute(Sender: TObject);
    procedure ActionOpenLCBCommandReadUserACDIExecute(Sender: TObject);
    procedure ActionOpenLCBCommandSNIIExecute(Sender: TObject);
    procedure ActionThrottlesCloseAllExecute(Sender: TObject);
    procedure ActionThrottlesCreateExecute(Sender: TObject);
    procedure ActionThrottlesHideAllExecute(Sender: TObject);
    procedure ActionThrottlesShowAllExecute(Sender: TObject);
    procedure ActionToolsComConnectExecute(Sender: TObject);
    procedure ActionToolsComDisconnectExecute(Sender: TObject);
    procedure ActionToolsConfigureNodeExecute(Sender: TObject);
    procedure ActionToolsEthernetHubConnectExecute(Sender: TObject);
    procedure ActionToolsCOMPortMessageLogShowExecute(Sender: TObject);
    procedure ActionToolsPreferenceShowMacExecute(Sender: TObject);
    procedure ActionToolsSettingsShowWinExecute(Sender: TObject);
    procedure ActionTreeviewNetworkCollapseAllExecute(Sender: TObject);
    procedure ActionTreeviewNetworkCollapseSelectedExecute(Sender: TObject);
    procedure ActionTreeviewNetworkExpandAllExecute(Sender: TObject);
    procedure ActionTreeviewNetworkExpandSelectedExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItemToolsSep3Click(Sender: TObject);
    procedure MenuItemConfigEditorsClick(Sender: TObject);
    procedure MenuItemThrottlesClick(Sender: TObject);
    procedure PanelNetworkClick(Sender: TObject);
    procedure ToolBar1Click(Sender: TObject);
    procedure TreeViewNetworkContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure TreeViewNetworkCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
    procedure TreeViewNetworkCustomCreateItem(Sender: TCustomTreeView; var ATreeNode: TTreenode);
    procedure TreeViewNetworkExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    procedure TreeViewNetworkSelectionChanged(Sender: TObject);
  private
    FCommandStationNode: TTreeNode;
    FComPortHub: TComPortHub;
    FConfigEditorList: TFormConfigEditorList;
    FEthernetHub: TEthernetHub;
    FLazyLoadTaskList: TList;
    FMessageHelper: TOpenLCBMessageHelper;
    {$IFDEF DARWIN}
    FOSXMenu: TMenuItem;
    FOSXSep1Cmd: TMenuItem;
    FOSXPrefCmd: TMenuItem;
    {$ENDIF}
    FAppAboutCmd: TMenuItem;
    FRootNetworkNode: TTreeNode;
    FRootTrainNode: TTreeNode;
    FShownOnce: Boolean;
    FThrottleList: TFormThrottleList;
    { private declarations }
    procedure RunAllOnNode(Node: TOlcbTreeNode);
    procedure RunConfigMemoryOptionsTaskOnNode(Node: TOlcbTreeNode);
    procedure RunIdentifyEventsGlobal;
    procedure RunIdentifyEventsOnNode(Node: TOlcbTreeNode);
    procedure RunMemConfigSpacesAllInfoOnNode(Node: TOlcbTreeNode);
    procedure RunProtocolSupportOnNode(Node: TOlcbTreeNode);
    procedure RunReadMemorySpaceOnNode(Node: TOlcbTreeNode; AddressSpace: Byte; UseStream: Boolean);
    procedure RunSNIIOnNode(Node: TOlcbTreeNode);
    procedure RunVerifyNodeIdGlobal;
  protected
    function AddNetworkTrainAlias(NodeAlias: Word; NodeID: QWord): TOlcbTreeNode;
    function AddNetworkTreeAlias(NodeAlias: Word; NodeID: QWord; QueryEvents: Boolean): TOlcbTreeNode;
    function AddNetworkCommandStationAlias(NodeAlias: Word; NodeID: QWord): TOlcbTreeNode;
    procedure AddConfigEditorSubMenu(ConfigEditor: TFormTrainConfigEditor);
    procedure AddThrottleSubMenu(Throttle: TFormAwesomeThrottle);
    procedure ComConnect;
    procedure ComDisconnect;
    procedure CreateAndLoadNodeBoolean(ParentNode: TTreeNode; Description: string; IsTrue: Boolean);
    procedure CreateAndLoadNodeSimple(ParentNode: TTreeNode; Description: string);
    procedure DeleteNetworkTreeAlias(NodeAlias: Word);
    procedure DeleteConfigEditorSubMenu(ConfigEditor: TFormTrainConfigEditor);
    procedure DeleteThrottleSubMenu(Throttle: TFormAwesomeThrottle);
    function DispatchTask(Task: TOlcbTaskBase): Boolean;
    procedure EthernetConnect;
    procedure EthernetDisconnect;
    function FindChildThatContainsText(ParentNode: TTreeNode; TestString: string): TTreeNode;
    function FindTreeNodeByAlias(Root: TTreeNode; AnAliasID: Word): TOlcbTreeNode;
    function FindSNIPNode(AliasNode: TTreeNode): TTreeNode;
    function FindPIPNode(AliasNode: TTreeNode): TTreeNode;
    function FindNodeIDNode(AliasNode: TTreeNode): TTreeNode;
    function FindConsumerNode(AliasNode: TTreeNode): TTreeNode;
    function FindProducerNode(AliasNode: TTreeNode): TTreeNode;
    function FindMemConfigOptionsNode(AliasNode: TTreeNode): TTreeNode;
    function FindMemConfigAddressSpaceNode(AliasNode: TTreeNode): TTreeNode;
    function IsParentRootNode(Node: TOlcbTreeNode): Boolean;
    procedure RefreshNetworkTreeAliasConfigMemOptions(NodeAlias: Word; Options: TOlcbMemOptions);
    procedure RefreshNetworkTreeAliasConfigMemAddressSpaceInfo(NodeAlias: Word; AddressSpace: TOlcbMemAddressSpace);
    procedure RefreshNetworkTreeAliasEvents(NodeAlias: Word; LocalHelper: TOpenLCBMessageHelper);
    procedure RefreshTrainTreeAliasEvents(NodeAlias: Word; NodeID: QWord; LocalHelper: TOpenLCBMessageHelper);
    procedure RefreshCommandStationTreeAliasEvents(NodeAlias: Word; NodeID: QWord; LocalHelper: TOpenLCBMessageHelper);
    procedure RefreshNetworkTreeAliasSnip(NodeAlias: Word; Snip: TOlcbSNIP);
    procedure RefreshNetworkTreeAliasProtocolSupport(NodeAlias: Word; Protocols: QWord);
    procedure SyncErrorCOMPortMessage(MessageStr: String);
    procedure SyncReceiveCOMPortMessage(MessageStr: String);
    procedure SyncSendCOMPortMessage(MessageStr: String);
    procedure SyncErrorEthernetMessage(MessageStr: String);
    procedure SyncReceiveEthernetMessage(MessageStr: String);
    procedure SyncSendEthernetMessage(MessageStr: String);
    procedure SyncHubConnect(HostIP: string; HostPort: Integer);
    procedure SyncHubDisconnect(HostIP: string; HostPort: Integer);
    procedure SyncHubNewClient(SocketCount: Integer);
    procedure SyncHubDroppedClient(SocketCount: Integer);
    procedure SyncHubOnStatus(Client: TClientSocketThread; Reason: THookSocketReason; Value: String);
    {$IFDEF DEBUG_THREAD} procedure SyncDebugMessage(Info: TComPortThreadDebugRec); {$ENDIF}
    procedure SyncMessageLogHide;
    procedure SyncEthernetMessageLogHide;
    procedure SyncThrottleHide(Throttle: TFormAwesomeThrottle);
    procedure SyncThrottleClose(Throttle: TFormAwesomeThrottle);
    procedure SyncConfigEditorHide(ConfigEditor: TFormTrainConfigEditor);
    procedure SyncConfigEditorClose(ConfigEditor: TFormTrainConfigEditor);
    procedure OnBeforeDestroyTask(Sender: TOlcbTaskBase);
    procedure OnConfigEditorMenuItemClick(Sender: TObject);
    procedure OnThrottleMenuItemClick(Sender: TObject);

    procedure UpdateUI;
    {$IFDEF DARWIN}
    property OSXMenu: TMenuItem read FOSXMenu write FOSXMenu;
    property OSXSep1Cmd: TMenuItem read FOSXSep1Cmd write FOSXSep1Cmd;
    property OSXPrefCmd: TMenuItem read FOSXPrefCmd write FOSXPrefCmd;
    {$ENDIF}
    property AppAboutCmd: TMenuItem read FAppAboutCmd write FAppAboutCmd;
    property ComPortHub: TComPortHub read FComPortHub write FComPortHub;
    property ConfigEditorList: TFormConfigEditorList read FConfigEditorList write FConfigEditorList;
    property EthernetHub: TEthernetHub read FEthernetHub write FEthernetHub;
    property LazyLoadTaskList: TList read FLazyLoadTaskList write FLazyLoadTaskList;
    property MessageHelper: TOpenLCBMessageHelper read FMessageHelper write FMessageHelper;
    property RootNetworkNode: TTreeNode read FRootNetworkNode write FRootNetworkNode;
    property RootTrainNode: TTreeNode read FRootTrainNode write FRootTrainNode;
    property RootCommandStationNode: TTreeNode read FCommandStationNode write FCommandStationNode;
    property ShownOnce: Boolean read FShownOnce write FShownOnce;
    property ThrottleList: TFormThrottleList read FThrottleList write FThrottleList;
  public
    { public declarations }
  end;

var
  FormOLCB_Commander: TFormOLCB_Commander;

implementation

 { TOlcbTreeNode }

constructor TOlcbTreeNode.Create(AnOwner: TTreeNodes);
begin
  inherited Create(AnOwner);
  FOlcbData := TOpenLcbNode.Create;
  FExpandedOnce := False;
end;

destructor TOlcbTreeNode.Destroy;
begin
  FreeAndNil(FOlcbData);
  inherited Destroy;
end;

{$R *.lfm}

{ TFormOLCB_Commander }

procedure TFormOLCB_Commander.ActionToolsSettingsShowWinExecute(Sender: TObject);
begin
  FormSettings.ShowModal;
end;

procedure TFormOLCB_Commander.ActionThrottlesCreateExecute(Sender: TObject);
var
  FormAwesomeThrottle: TFormAwesomeThrottle;
begin
  FormAwesomeThrottle := TFormAwesomeThrottle.Create(Application);
  FormAwesomeThrottle.OnThrottleClose := @SyncThrottleClose;
  FormAwesomeThrottle.OnThrottleHide := @SyncThrottleHide;
  FormAwesomeThrottle.InitTransportLayers(EthernetHub, ComPortHub, @DispatchTask);
  FormAwesomeThrottle.ImageList16x16 := ImageList16x16;
  ThrottleList.Add(FormAwesomeThrottle);
  FormAwesomeThrottle.Show;
  UpdateUI // Update the Separator before the user tries to open the Throttle Menu
end;

procedure TFormOLCB_Commander.ActionTreeviewNetworkCollapseAllExecute(Sender: TObject);
begin
  TreeViewNetwork.BeginUpdate;
  try
    TreeViewNetwork.FullCollapse;
    RootNetworkNode.Expanded := True;
    RootTrainNode.Expanded := True;
    RootCommandStationNode.Expanded := True;
  finally
    TreeViewNetwork.EndUpdate;
  end;
end;

procedure TFormOLCB_Commander.ActionTreeviewNetworkCollapseSelectedExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to TreeViewNetwork.SelectionCount - 1 do
    TreeViewNetwork.Selections[i].Collapse(True);
end;

procedure TFormOLCB_Commander.ActionTreeviewNetworkExpandAllExecute(Sender: TObject);
begin
  TreeViewNetwork.FullExpand;
end;

procedure TFormOLCB_Commander.ActionTreeviewNetworkExpandSelectedExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to TreeViewNetwork.SelectionCount - 1 do
    TreeViewNetwork.Selections[i].Expand(True);
end;

procedure TFormOLCB_Commander.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  ComDisconnect;
  ThrottleList.Clear;
  ConfigEditorList.Clear;

 // CanClose := TaskObjects = 0;
 // ShowMessage('TaskCount = ' + IntToStr(TaskObjects));
end;

procedure TFormOLCB_Commander.ActionHelpAboutShowExecute(Sender: TObject);
begin
  FormAbout.ShowModal
end;

procedure TFormOLCB_Commander.ActionConfigEditorsCreateExecute(Sender: TObject);
var
  ConfigEditor: TFormTrainConfigEditor;
  i: Integer;
  Node: TOlcbTreeNode;
begin
  for i := 0 to TreeViewNetwork.SelectionCount - 1 do
  begin
    Node := TreeViewNetwork.Selections[i] as TOlcbTreeNode;
    if IsParentRootNode(Node) then
    begin
      ConfigEditor := ConfigEditorList.FindEditorByAlias(Node.OlcbData.NodeIDAlias);
      if not Assigned(ConfigEditor) then
      begin
        ConfigEditor := TFormTrainConfigEditor.Create(Application);
        ConfigEditor.ImageList16x16 := ImageList16x16;
        ConfigEditor.InitTransportLayers(EthernetHub, ComPortHub, @DispatchTask);
        ConfigEditor.Caption := ConfigEditor.Caption + ' - 0x' + IntToHex(Node.OlcbData.NodeIDAlias, 4) + ' [' + IntToStr(Node.OlcbData.NodeIDAlias) + ']';
        ConfigEditor.AliasID := Node.OlcbData.NodeIDAlias;
        ConfigEditor.OnConfigEditorClose := @SyncConfigEditorClose;
        ConfigEditor.OnConfigEditorHide := @SyncConfigEditorHide;
        ConfigEditorList.Add(ConfigEditor);
      end;
      ConfigEditor.Show;
      UpdateUI // Update the Separator before the user tries to open the Config Menu
    end;
  end;
end;

procedure TFormOLCB_Commander.ActionConfigEditorsCloseAllExecute(Sender: TObject);
begin
  ConfigEditorList.CloseAll;
end;

procedure TFormOLCB_Commander.ActionConfigEditorsHideAllExecute(Sender: TObject);
begin
  ConfigEditorList.HideAll;
end;

procedure TFormOLCB_Commander.ActionConfigEditorsShowAllExecute(Sender: TObject);
begin
  ConfigEditorList.ShowAll;
end;

procedure TFormOLCB_Commander.ActionOpenLCBCommandReadAllWithStreamExecute(Sender: TObject);
var
  Node: TOlcbTreeNode;
  i: Integer;
 begin
  for i := 0 to TreeViewNetwork.SelectionCount - 1 do
  begin
    Node := TreeViewNetwork.Selections[i] as TOlcbTreeNode;
    if IsParentRootNode(Node) then
      RunReadMemorySpaceOnNode(Node, MSI_ALL, True);
  end;
end;

procedure TFormOLCB_Commander.ActionOpenLCBCommandReadCDIWithStreamExecute(Sender: TObject);
var
  Node: TOlcbTreeNode;
  i: Integer;
 begin
  for i := 0 to TreeViewNetwork.SelectionCount - 1 do
  begin
    Node := TreeViewNetwork.Selections[i] as TOlcbTreeNode;
    if IsParentRootNode(Node) then
      RunReadMemorySpaceOnNode(Node, MSI_CDI, True);
  end;

end;

procedure TFormOLCB_Commander.ActionOpenLCBCommandReadConfigurationWithStreamExecute(Sender: TObject);
 var
  Node: TOlcbTreeNode;
  i: Integer;
 begin
  for i := 0 to TreeViewNetwork.SelectionCount - 1 do
  begin
    Node := TreeViewNetwork.Selections[i] as TOlcbTreeNode;
    if IsParentRootNode(Node) then
      RunReadMemorySpaceOnNode(Node, MSI_CONFIG, True);
  end;
end;

procedure TFormOLCB_Commander.ActionOpenLCBCommandReadFDIWithStreamExecute(Sender: TObject);
 var
   Node: TOlcbTreeNode;
   i: Integer;
  begin
   for i := 0 to TreeViewNetwork.SelectionCount - 1 do
   begin
     Node := TreeViewNetwork.Selections[i] as TOlcbTreeNode;
     if IsParentRootNode(Node) then
       RunReadMemorySpaceOnNode(Node, MSI_FDI, True);
   end;
end;

procedure TFormOLCB_Commander.ActionShowLoopTimeExecute(Sender: TObject);
begin
  ShowMessage('LoopTime = ' + IntToStr(LoopTime));
end;

procedure TFormOLCB_Commander.ActionToolsEthernetHubDisconnectExecute(Sender: TObject);
begin
  EthernetDisconnect;
end;

procedure TFormOLCB_Commander.ActionToolsEthernetHubMessageLogShowExecute( Sender: TObject);
begin
  FormEthernetMessageLog.Show;
  if EthernetHub.Enabled then
  begin
    EthernetHub.EnableReceiveMessages := True;
    EthernetHub.EnableSendMessages := True;
  end;
  ActionToolsEthernetHubMessageLogShow.Checked := True;
end;

procedure TFormOLCB_Commander.ActionOpenLCBCommandAllExecute(Sender: TObject);
var
  Node: TOlcbTreeNode;
begin
  Node := RootNetworkNode.GetFirstChild as TOlcbTreeNode;
  while Assigned(Node) do
  begin
    RunAllOnNode(Node);
    Node := RootNetworkNode.GetNextChild(Node) as TOlcbTreeNode;
  end;
end;

procedure TFormOLCB_Commander.ActionOpenLCBCommandIdentifyEventsExecute(Sender: TObject);
var
  Node: TOlcbTreeNode;
begin
  Node := RootNetworkNode.GetFirstChild as TOlcbTreeNode;
  while Assigned(Node) do
  begin
    RunIdentifyEventsOnNode(Node);
    Node := RootNetworkNode.GetNextChild(Node) as TOlcbTreeNode;
  end;
end;

procedure TFormOLCB_Commander.ActionOpenLCBCommandIdentifyIDGlobalExecute(Sender: TObject);
begin
 TreeViewNetwork.BeginUpdate;
  try
    RootNetworkNode.DeleteChildren;
    RootNetworkNode.HasChildren := True;
    RootNetworkNode.Expanded := True;
    RootTrainNode.DeleteChildren;
    RootTrainNode.HasChildren := True;
    RootTrainNode.Expanded := True;
    RootCommandStationNode.DeleteChildren;
    RootCommandStationNode.HasChildren := True;
    RootCommandStationNode.Expanded := True;
  finally
    TreeViewNetwork.EndUpdate;
    RunVerifyNodeIdGlobal;
  end;
end;

procedure TFormOLCB_Commander.ActionOpenLCBCommandMemConfigOptionsExecute(Sender: TObject);
var
  Node: TOlcbTreeNode;
  i: Integer;
begin
  for i := 0 to TreeViewNetwork.SelectionCount - 1 do
  begin
    Node := TreeViewNetwork.Selections[i] as TOlcbTreeNode;
    if IsParentRootNode(Node) then
      RunConfigMemoryOptionsTaskOnNode(Node);
  end;
end;

procedure TFormOLCB_Commander.ActionOpenLCBCommandMemConfigAllSpacesInfoExecute(Sender: TObject);
var
  Node: TOlcbTreeNode;
  i: Integer;
begin
  for i := 0 to TreeViewNetwork.SelectionCount - 1 do
  begin
    Node := TreeViewNetwork.Selections[i] as TOlcbTreeNode;
    if IsParentRootNode(Node) then
      RunMemConfigSpacesAllInfoOnNode(Node);
  end;
end;

procedure TFormOLCB_Commander.ActionOpenLCBCommandProtocolSupportExecute(Sender: TObject);
var
  Node: TOlcbTreeNode;
  i: Integer;
 begin
  for i := 0 to TreeViewNetwork.SelectionCount - 1 do
  begin
    Node := TreeViewNetwork.Selections[i] as TOlcbTreeNode;
    if IsParentRootNode(Node) then
      RunProtocolSupportOnNode(Node);
  end;
end;

procedure TFormOLCB_Commander.ActionOpenLCBCommandReadAllExecute(Sender: TObject);
var
  Node: TOlcbTreeNode;
  i: Integer;
 begin
  for i := 0 to TreeViewNetwork.SelectionCount - 1 do
  begin
    Node := TreeViewNetwork.Selections[i] as TOlcbTreeNode;
    if IsParentRootNode(Node) then
      RunReadMemorySpaceOnNode(Node, MSI_ALL, False);
  end;
end;

procedure TFormOLCB_Commander.ActionOpenLCBCommandReadCDIExecute(Sender: TObject);
var
  Node: TOlcbTreeNode;
  i: Integer;
 begin
  for i := 0 to TreeViewNetwork.SelectionCount - 1 do
  begin
    Node := TreeViewNetwork.Selections[i] as TOlcbTreeNode;
    if IsParentRootNode(Node) then
      RunReadMemorySpaceOnNode(Node, MSI_CDI, False);
  end;
end;

procedure TFormOLCB_Commander.ActionOpenLCBCommandReadConfigurationExecute(Sender: TObject);
var
  Node: TOlcbTreeNode;
  i: Integer;
 begin
  for i := 0 to TreeViewNetwork.SelectionCount - 1 do
  begin
    Node := TreeViewNetwork.Selections[i] as TOlcbTreeNode;
    if IsParentRootNode(Node) then
      RunReadMemorySpaceOnNode(Node, MSI_CONFIG, False);
  end;
end;

procedure TFormOLCB_Commander.ActionOpenLCBCommandReadFDIExecute(Sender: TObject);
var
  Node: TOlcbTreeNode;
  i: Integer;
 begin
  for i := 0 to TreeViewNetwork.SelectionCount - 1 do
  begin
    Node := TreeViewNetwork.Selections[i] as TOlcbTreeNode;
    if IsParentRootNode(Node) then
      RunReadMemorySpaceOnNode(Node, MSI_FDI, False);
  end;
end;

procedure TFormOLCB_Commander.ActionOpenLCBCommandReadFSIExecute(Sender: TObject);
var
  Node: TOlcbTreeNode;
  i: Integer;
 begin
  for i := 0 to TreeViewNetwork.SelectionCount - 1 do
  begin
    Node := TreeViewNetwork.Selections[i] as TOlcbTreeNode;
    if IsParentRootNode(Node) then
      RunReadMemorySpaceOnNode(Node, MSI_FSI, False);
  end;

end;

procedure TFormOLCB_Commander.ActionOpenLCBCommandReadMfgACDIExecute(Sender: TObject);
var
  Node: TOlcbTreeNode;
  i: Integer;
 begin
  for i := 0 to TreeViewNetwork.SelectionCount - 1 do
  begin
    Node := TreeViewNetwork.Selections[i] as TOlcbTreeNode;
    if IsParentRootNode(Node) then
      RunReadMemorySpaceOnNode(Node, MSI_ACDI_MFG, False);
  end;
end;

procedure TFormOLCB_Commander.ActionOpenLCBCommandReadUserACDIExecute(Sender: TObject);
var
  Node: TOlcbTreeNode;
  i: Integer;
 begin
  for i := 0 to TreeViewNetwork.SelectionCount - 1 do
  begin
    Node := TreeViewNetwork.Selections[i] as TOlcbTreeNode;
    if IsParentRootNode(Node) then
      RunReadMemorySpaceOnNode(Node, MSI_ACDI_USER, False)
  end;
end;

procedure TFormOLCB_Commander.ActionOpenLCBCommandSNIIExecute(Sender: TObject);
var
  Node: TOlcbTreeNode;
  i: Integer;
begin
  for i := 0 to TreeViewNetwork.SelectionCount - 1 do
  begin
    Node := TreeViewNetwork.Selections[i] as TOlcbTreeNode;
    if IsParentRootNode(Node) then
      RunSNIIOnNode(Node);
  end;
end;

procedure TFormOLCB_Commander.ActionThrottlesCloseAllExecute(Sender: TObject);
begin
  ThrottleList.CloseAll;
end;

procedure TFormOLCB_Commander.ActionThrottlesHideAllExecute(Sender: TObject);
begin
  ThrottleList.HideAll;
end;

procedure TFormOLCB_Commander.ActionThrottlesShowAllExecute(Sender: TObject);
begin
  ThrottleList.ShowAll;
end;

procedure TFormOLCB_Commander.ActionToolsComConnectExecute(Sender: TObject);
begin
  ComConnect
end;

procedure TFormOLCB_Commander.ActionToolsComDisconnectExecute(Sender: TObject);
begin
  ComDisconnect;
end;

procedure TFormOLCB_Commander.ActionToolsConfigureNodeExecute(Sender: TObject);
begin

end;

procedure TFormOLCB_Commander.ActionToolsEthernetHubConnectExecute(Sender: TObject);
begin
  EthernetConnect;
end;

procedure TFormOLCB_Commander.ActionToolsCOMPortMessageLogShowExecute(Sender: TObject);
begin
  FormMessageLog.Show;
  if Assigned(ComPortHub) then
  begin
    ComPortHub.EnableReceiveMessages := True;
    ComPortHub.EnableSendMessages := True;
  end;
  ActionToolsCOMPortMessageLogShow.Checked := True;
end;

procedure TFormOLCB_Commander.ActionToolsPreferenceShowMacExecute(Sender: TObject);
begin
  FormSettings.Show
end;

procedure TFormOLCB_Commander.FormCreate(Sender: TObject);
begin
  FShownOnce := False;
  FMessageHelper := TOpenLCBMessageHelper.Create;
  FThrottleList := TFormThrottleList.Create;
  FConfigEditorList := TFormConfigEditorList.Create;
  FLazyLoadTaskList := TList.Create;
  FEthernetHub := TEthernetHub.Create;

  FEthernetHub.SyncErrorMessageFunc := @SyncErrorEthernetMessage;
  FEthernetHub.SyncReceiveMessageFunc := @SyncReceiveEthernetMessage;
  FEthernetHub.SyncSendMessageFunc := @SyncSendEthernetMessage;
  FEthernetHub.OnBeforeDestroyTask := @OnBeforeDestroyTask;
  FEthernetHub.OnHubConnect := @SyncHubConnect;
  FEthernetHub.OnHubDisconnect := @SyncHubDisconnect;
  FEthernetHub.OnClientClientConnect := @SyncHubNewClient;
  FEthernetHub.OnClientDisconnect := @SyncHubDroppedClient;
  FEthernetHub.OnSyncStatus := @SyncHubOnStatus;

  FComPortHub := TComPortHub.Create;
  FComPortHub.SyncReceiveMessageFunc := @SyncReceiveCOMPortMessage;
  FComPortHub.SyncSendMessageFunc := @SyncSendCOMPortMessage;
  FComPortHub.SyncErrorMessageFunc := @SyncErrorCOMPortMessage;
  FComPortHub.OnBeforeDestroyTask := @OnBeforeDestroyTask;
end;

procedure TFormOLCB_Commander.FormDestroy(Sender: TObject);
begin
  FreeAndNil( FMessageHelper);
  FreeAndNil( FThrottleList);
  FreeAndNil( FLazyLoadTaskList);
  FreeAndNil( FConfigEditorList);
  FreeAndNil(FEthernetHub);
  FreeAndNil(FComPortHub);
end;

procedure TFormOLCB_Commander.FormShow(Sender: TObject);
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
    MenuItemToolsSep2.Visible := False;
    FormMessageLog.SynMemo.Font.Height := 0;
    FormEthernetMessageLog.SynMemo.Font.Height := 0;
    {$ELSE}
    AppAboutCmd := TMenuItem.Create(Self);
    AppAboutCmd.Action := ActionHelpAboutShow;
    MenuItemHelp.Add(AppAboutCmd);
    {$ENDIF}
    FormMessageLog.HideCallback := @SyncMessageLogHide;
    FormEthernetMessageLog.HideCallback := @SyncEthernetMessageLogHide;
    {$IFDEF Linux}
    FormSettings.SettingsFilePath:= GetSettingsPath + {PATH_LINUX_APP_FOLDER +} PATH_SETTINGS_FILE;
    GlobalSettings.LoadFromFile(UTF8ToSys( GetSettingsPath + {PATH_LINUX_APP_FOLDER +} PATH_SETTINGS_FILE));
    {$ELSE}
    FormSettings.SettingsFilePath := GetSettingsPath + PATH_SETTINGS_FILE;
    GlobalSettings.LoadFromFile(UTF8ToSys( GetSettingsPath + PATH_SETTINGS_FILE));
    {$ENDIF}

    RootNetworkNode := TreeViewNetwork.Items.AddChild(nil, STR_NETWORKTREE_ROOT);
    RootNetworkNode.ImageIndex := 35;
    RootNetworkNode.SelectedIndex := 35;

    RootTrainNode := TreeViewNetwork.Items.AddChild(nil, STR_TRAINTREE_ROOT);
    RootTrainNode.ImageIndex := 35;
    RootTrainNode.SelectedIndex := 35;

    RootCommandStationNode := TreeViewNetwork.Items.AddChild(nil, STR_COMMANDSTATIONTREE_ROOT);
    RootCommandStationNode.ImageIndex := 35;
    RootCommandStationNode.SelectedIndex := 35;

    if GlobalSettings.ComPort.AutoConnectAtBoot then
     ActionToolsComConnect.Execute;
    ShownOnce := True
  end;
  UpdateUI;

  {$IFDEF DEBUG_THREAD}
  FormThreadDebug := TFormThreadDebug.Create(Application);
  FormThreadDebug.Show;
  {$ENDIF}
end;

procedure TFormOLCB_Commander.MenuItemToolsSep3Click(Sender: TObject);
begin

end;

procedure TFormOLCB_Commander.MenuItemConfigEditorsClick(Sender: TObject);
var
  Start, i: Integer;
begin
  Start := MenuItemConfigEditors.IndexOf(MenuItemConfigEditorsSep1);
  i := MenuItemConfigEditors.Count;
  if Start > -1 then
  begin
    while MenuItemConfigEditors.Count - 1 > Start do
      MenuItemConfigEditors.Delete(MenuItemConfigEditors.Count - 1);
  end;
  for i := 0 to ConfigEditorList.Count - 1 do
     AddConfigEditorSubMenu(ConfigEditorList.ConfigEditors[i]);
end;

procedure TFormOLCB_Commander.MenuItemThrottlesClick(Sender: TObject);
var
  Start, i: Integer;
begin
  Start := MenuItemThrottles.IndexOf(MenuItemThrottlesSep1);
  i := MenuItemThrottles.Count;
  if Start > -1 then
  begin
    while MenuItemThrottles.Count - 1 > Start do
      MenuItemThrottles.Delete(MenuItemThrottles.Count - 1);
  end;
  for i := 0 to ThrottleList.Count - 1 do
     AddThrottleSubMenu(ThrottleList.Throttles[i]);
end;

procedure TFormOLCB_Commander.PanelNetworkClick(Sender: TObject);
begin

end;

procedure TFormOLCB_Commander.ToolBar1Click(Sender: TObject);
begin

end;

procedure TFormOLCB_Commander.TreeViewNetworkContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  ScreenPos: TPoint;
  Node: TTreeNode;
  Tree: TTreeview;
begin
  Tree := (Sender as TTreeview);
  ScreenPos := Tree.ClientToScreen(MousePos);
  Node := Tree.GetNodeAt(MousePos.X, MousePos.Y);

  PopupMenuTreeNode.PopUp(ScreenPos.X, ScreenPos.Y);
  Handled := True;
end;

procedure TFormOLCB_Commander.TreeViewNetworkCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
  NodeClass := TOlcbTreeNode;
end;

procedure TFormOLCB_Commander.TreeViewNetworkCustomCreateItem(Sender: TCustomTreeView; var ATreeNode: TTreenode);
begin
  ATreeNode := TOlcbTreeNode.Create(Sender.Items)
end;

procedure TFormOLCB_Commander.TreeViewNetworkExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
begin
  if IsParentRootNode((Node as TOlcbTreeNode)) then
  begin
    if not (Node as TOlcbTreeNode).ExpandedOnce then
    begin
      RunAllOnNode(Node as TOlcbTreeNode);
      (Node as TOlcbTreeNode).ExpandedOnce := True
    end;
  end;
end;

procedure TFormOLCB_Commander.TreeViewNetworkSelectionChanged(Sender: TObject);
begin
  UpdateUI
end;

procedure TFormOLCB_Commander.RunAllOnNode(Node: TOlcbTreeNode);
begin
  RunConfigMemoryOptionsTaskOnNode(Node);
  RunMemConfigSpacesAllInfoOnNode(Node);
  RunProtocolSupportOnNode(Node);
  RunSNIIOnNode(Node);
  RunIdentifyEventsOnNode(Node);
end;

procedure TFormOLCB_Commander.RunConfigMemoryOptionsTaskOnNode(Node: TOlcbTreeNode);
var
  Task: TConfigMemoryOptionsTask;
begin
  Task := TConfigMemoryOptionsTask.Create(GlobalSettings.General.AliasIDAsVal, Node.OlcbData.NodeIDAlias, True);
  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
  DispatchTask(Task);
end;

procedure TFormOLCB_Commander.RunIdentifyEventsGlobal;
var
  Task: TIdentifyEventsTask;
begin
  Task := TIdentifyEventsTask.Create(GlobalSettings.General.AliasIDAsVal, 0, True);
  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
  DispatchTask(Task);
end;

procedure TFormOLCB_Commander.RunIdentifyEventsOnNode(Node: TOlcbTreeNode);
var
  Task: TIdentifyEventsAddressedTask;
begin
  Task := TIdentifyEventsAddressedTask.Create(GlobalSettings.General.AliasIDAsVal, Node.OlcbData.NodeIDAlias, True);
  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
  DispatchTask(Task);
end;

procedure TFormOLCB_Commander.RunMemConfigSpacesAllInfoOnNode(Node: TOlcbTreeNode);
var
  Task: TEnumAllConfigMemoryAddressSpaceInfoTask;
begin
  Task := TEnumAllConfigMemoryAddressSpaceInfoTask.Create(GlobalSettings.General.AliasIDAsVal, Node.OlcbData.NodeIDAlias, True);
  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
  DispatchTask(Task);
end;

procedure TFormOLCB_Commander.RunProtocolSupportOnNode(Node: TOlcbTreeNode);
var
  Task: TProtocolSupportTask;
begin
  Task := TProtocolSupportTask.Create(GlobalSettings.General.AliasIDAsVal, Node.OlcbData.NodeIDAlias, True);
  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
  DispatchTask(Task);
end;

procedure TFormOLCB_Commander.RunReadMemorySpaceOnNode(Node: TOlcbTreeNode; AddressSpace: Byte; UseStream: Boolean);
var
  Task: TReadAddressSpaceMemoryTask;
  StreamTask: TReadAddressSpaceMemoryWithStreamTask;
begin
  if UseStream then
  begin
    case AddressSpace of
      MSI_CDI: begin
                 StreamTask := TReadAddressSpaceMemoryWithStreamTask.Create(GlobalSettings.General.AliasIDAsVal, Node.OlcbData.NodeIDAlias, True, AddressSpace, True);
                 StreamTask.Terminator := #0;
               end
      else
        StreamTask := TReadAddressSpaceMemoryWithStreamTask.Create(GlobalSettings.General.AliasIDAsVal, Node.OlcbData.NodeIDAlias, True, AddressSpace, False);
    end;
    StreamTask.ForceOptionalSpaceByte := False;
    StreamTask.OnBeforeDestroy := @OnBeforeDestroyTask;
    DispatchTask(StreamTask);
  end else
  begin
    case AddressSpace of
      MSI_CDI: begin
                 Task := TReadAddressSpaceMemoryTask.Create(GlobalSettings.General.AliasIDAsVal, Node.OlcbData.NodeIDAlias, True, AddressSpace, True);
                 Task.Terminator := #0;
               end
      else
        Task := TReadAddressSpaceMemoryTask.Create(GlobalSettings.General.AliasIDAsVal, Node.OlcbData.NodeIDAlias, True, AddressSpace, False);
    end;
    Task.ForceOptionalSpaceByte := False;
    Task.OnBeforeDestroy := @OnBeforeDestroyTask;
    DispatchTask(Task);
  end
end;

procedure TFormOLCB_Commander.RunSNIIOnNode(Node: TOlcbTreeNode);
var
  Task: TSimpleNodeInformationTask;
begin
  Task := TSimpleNodeInformationTask.Create(GlobalSettings.General.AliasIDAsVal, Node.OlcbData.NodeIDAlias, True);
  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
  DispatchTask(Task);
end;

procedure TFormOLCB_Commander.RunVerifyNodeIdGlobal;
var
  Task: TVerifyNodeIDGlobalTask;
begin
  Task := TVerifyNodeIDGlobalTask.Create(GlobalSettings.General.AliasIDAsVal, 0, True);
  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
  DispatchTask(Task);
end;

function TFormOLCB_Commander.AddNetworkCommandStationAlias(NodeAlias: Word; NodeID: QWord): TOlcbTreeNode;
begin
  Result := FindTreeNodeByAlias(RootCommandStationNode, NodeAlias);
  if not Assigned(Result) then
  begin
    TreeViewNetwork.BeginUpdate;
    try
      Result := TreeViewNetwork.Items.AddChild(RootCommandStationNode, STR_ALIASID + ': 0x' + IntToHex(NodeAlias, 4)) as TOlcbTreeNode;
      Result.ImageIndex := 41;
      Result.SelectedIndex := 41;
      Result.OlcbData.NodeIDAlias := NodeAlias;
      if NodeID <> 0 then
        Result.OlcbData.NodeID := NodeID;

      if NodeID <> 0 then
         CreateAndLoadNodeSimple(Result, STR_NODEID + ': ' + NodeIDToDotHex(NodeID))
      else
         CreateAndLoadNodeSimple(Result, STR_NODEID + ': ' + STR_UNKNOWN);
    finally
      RootCommandStationNode.CustomSort(@TreeViewNetwork.DefaultTreeViewSort);
      if RootCommandStationNode.Count = 1 then
        RootCommandStationNode.Expand(False);
      TreeViewNetwork.EndUpdate;
    end;
  end;
  UpdateUI
end;

procedure TFormOLCB_Commander.AddConfigEditorSubMenu(ConfigEditor: TFormTrainConfigEditor);
var
  MenuItem: TMenuItem;
begin
  MenuItem := TMenuItem.Create(Self);
  MenuItemConfigEditors.Add(MenuItem);
  MenuItem.Caption := ConfigEditor.Caption;
  MenuItem.Tag := PtrInt( ConfigEditor);
  MenuItem.OnClick := @OnConfigEditorMenuItemClick;
end;

function TFormOLCB_Commander.AddNetworkTrainAlias(NodeAlias: Word; NodeID: QWord): TOlcbTreeNode;
begin
  Result := FindTreeNodeByAlias(RootTrainNode, NodeAlias);
  if not Assigned(Result) then
  begin
    TreeViewNetwork.BeginUpdate;
    try
      Result := TreeViewNetwork.Items.AddChild(RootTrainNode, STR_ALIASID + ': 0x' + IntToHex(NodeAlias, 4)) as TOlcbTreeNode;
      Result.ImageIndex := 41;
      Result.SelectedIndex := 41;
      Result.OlcbData.NodeIDAlias := NodeAlias;
      if NodeID <> 0 then
        Result.OlcbData.NodeID := NodeID;

      if NodeID <> 0 then
         CreateAndLoadNodeSimple(Result, STR_NODEID + ': ' + NodeIDToDotHex(NodeID))
      else
         CreateAndLoadNodeSimple(Result, STR_NODEID + ': ' + STR_UNKNOWN);

      CreateAndLoadNodeSimple(Result, STR_ISIDLEPROXY + ': ' + STR_UNKNOWN);
      CreateAndLoadNodeSimple(Result, STR_ISINUSEPROXY + ': ' + STR_UNKNOWN);
      CreateAndLoadNodeSimple(Result, STR_DCC_ADDRESS + ': ' + STR_UNKNOWN);
    finally
      RootTrainNode.CustomSort(@TreeViewNetwork.DefaultTreeViewSort);
      if RootTrainNode.Count = 1 then
        RootTrainNode.Expand(False);
      TreeViewNetwork.EndUpdate;
    end;
  end;
  UpdateUI
end;

function TFormOLCB_Commander.AddNetworkTreeAlias(NodeAlias: Word; NodeID: QWord; QueryEvents: Boolean): TOlcbTreeNode;
var
  ProtocolsNode, ConfigMemProtocolNode, EventsNode, ChildNode : TTreeNode;
begin
  Result := FindTreeNodeByAlias(RootNetworkNode, NodeAlias);
  if not Assigned(Result) then
  begin
    TreeViewNetwork.BeginUpdate;
    try
      Result := TreeViewNetwork.Items.AddChild(RootNetworkNode, STR_ALIASID + ': 0x' + IntToHex(NodeAlias, 4)) as TOlcbTreeNode;
      Result.ImageIndex := 41;
      Result.SelectedIndex := 41;
      Result.OlcbData.NodeIDAlias := NodeAlias;
      if NodeID <> 0 then
        Result.OlcbData.NodeID := NodeID;

      if NodeID <> 0 then
         CreateAndLoadNodeSimple(Result, STR_NODEID + ': ' + NodeIDToDotHex(NodeID))
      else
         CreateAndLoadNodeSimple(Result, STR_NODEID + ': ' + STR_UNKNOWN);

      EventsNode := TreeviewNetwork.Items.AddChild(Result, STR_EVENTS);
      EventsNode.ImageIndex := 41;
      EventsNode.SelectedIndex := 41;
      ChildNode := TreeviewNetwork.Items.AddChild(EventsNode, STR_CONSUMERS);
      ChildNode.ImageIndex := 41;
      ChildNode.SelectedIndex := 41;
      ChildNode := TreeviewNetwork.Items.AddChild(EventsNode, STR_PRODUCERS);
      ChildNode.ImageIndex := 41;
      ChildNode.SelectedIndex := 41;

      ProtocolsNode := TreeviewNetwork.Items.AddChild(Result, STR_PROTOCOLS);
      ProtocolsNode.ImageIndex := 41;
      ProtocolsNode.SelectedIndex := 41;

      ConfigMemProtocolNode := TreeviewNetwork.Items.AddChild(ProtocolsNode, STR_MEM_PROTOCOL);
      ConfigMemProtocolNode.ImageIndex := 41;
      ConfigMemProtocolNode.SelectedIndex := 41;
      ChildNode := TreeviewNetwork.Items.AddChild(ConfigMemProtocolNode, STR_CONFIGMEM_OPTIONS);
      ChildNode.ImageIndex := 41;
      ChildNode.SelectedIndex := 41;
      ChildNode := TreeviewNetwork.Items.AddChild(ConfigMemProtocolNode, STR_CONFIGMEM_ADDRESS_SPACES);
      ChildNode.ImageIndex := 41;
      ChildNode.SelectedIndex := 41;

      ChildNode := TreeviewNetwork.Items.AddChild(ProtocolsNode, STR_PROTOCOLSUPPORT);
      ChildNode.ImageIndex := 41;
      ChildNode.SelectedIndex := 41;

      ChildNode := TreeviewNetwork.Items.AddChild(ProtocolsNode, STR_SNIP);
      ChildNode.ImageIndex := 41;
      ChildNode.SelectedIndex := 41;
    finally
      RootNetworkNode.CustomSort(@TreeViewNetwork.DefaultTreeViewSort);
      TreeViewNetwork.EndUpdate;
      if QueryEvents then
        RunIdentifyEventsOnNode(Result);
      RunSNIIOnNode(Result)
    end;
  end;
  UpdateUI
end;

procedure TFormOLCB_Commander.AddThrottleSubMenu(Throttle: TFormAwesomeThrottle);
var
  MenuItem: TMenuItem;
begin
  MenuItem := TMenuItem.Create(Self);
  MenuItemThrottles.Add(MenuItem);
  MenuItem.Caption := Throttle.Caption;
  MenuItem.Tag := PtrInt( Throttle);
  MenuItem.OnClick := @OnThrottleMenuItemClick;
end;

procedure TFormOLCB_Commander.ComConnect;
var
  i: Integer;
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
  ComPortHub.EnableReceiveMessages := ActionToolsCOMPortMessageLogShow.Checked;
  ComPortHub.EnableSendMessages := ActionToolsCOMPortMessageLogShow.Checked;

  if GlobalSettings.General.AutoScanNetworkAtBoot then
    ActionOpenLCBCommandIdentifyIDGlobal.Execute;

  RootNetworkNode.HasChildren := True;
  RootNetworkNode.Expanded := True;
  UpdateUI;
end;

procedure TFormOLCB_Commander.ComDisconnect;
begin
  ComPortHub.RemoveComPort(nil);
  UpdateUI;
end;

procedure TFormOLCB_Commander.CreateAndLoadNodeBoolean(ParentNode: TTreeNode; Description: string; IsTrue: Boolean);
var
   DataChild: TTreeNode;
begin
  if IsTrue then
    DataChild := TreeViewNetwork.Items.AddChild(ParentNode, Description + 'True')
  else
    DataChild := TreeViewNetwork.Items.AddChild(ParentNode, Description + 'False');
  DataChild.ImageIndex := 40;
  DataChild.SelectedIndex := 40;
end;

procedure TFormOLCB_Commander.CreateAndLoadNodeSimple(ParentNode: TTreeNode; Description: string);
var
   DataChild: TTreeNode;
begin
  DataChild := TreeViewNetwork.Items.AddChild(ParentNode, Description);
  DataChild.ImageIndex := 40;
  DataChild.SelectedIndex := 40;
end;

procedure TFormOLCB_Commander.DeleteNetworkTreeAlias(NodeAlias: Word);
var
  AliasNode, TrainNode, CommandStationNode: TTreeNode;
begin
  AliasNode := FindTreeNodeByAlias(RootNetworkNode, NodeAlias);
  TrainNode := FindTreeNodeByAlias(RootTrainNode, NodeAlias);
  CommandStationNode := FindTreeNodeByAlias(RootCommandStationNode, NodeAlias);
  if Assigned(AliasNode) or Assigned(TrainNode) or Assigned(CommandStationNode) then
  begin
    TreeViewNetwork.BeginUpdate;
    try
      if Assigned(AliasNode) then
        TreeViewNetwork.Items.Delete(AliasNode);
      if Assigned(TrainNode) then
        TreeViewNetwork.Items.Delete(TrainNode);
      if Assigned(CommandStationNode) then
        TreeViewNetwork.Items.Delete(CommandStationNode);
    finally
      TreeViewNetwork.EndUpdate;
      UpdateUI;
    end;
  end;
end;

procedure TFormOLCB_Commander.DeleteConfigEditorSubMenu(ConfigEditor: TFormTrainConfigEditor);
var
  i: Integer;
begin
  for i := 0 to MenuItemConfigEditors.Count - 1 do
  begin
    if MenuItemConfigEditors.Items[i].Tag = PtrInt( ConfigEditor) then
    begin
      MenuItemConfigEditors.Delete(i);
      Break
    end;
  end;
end;

procedure TFormOLCB_Commander.DeleteThrottleSubMenu(Throttle: TFormAwesomeThrottle);
var
  i: Integer;
begin
  for i := 0 to MenuItemThrottles.Count - 1 do
  begin
    if MenuItemThrottles.Items[i].Tag = PtrInt( Throttle) then
    begin
      MenuItemThrottles.Delete(i);
      Break
    end;
  end;
end;

function TFormOLCB_Commander.DispatchTask(Task: TOlcbTaskBase): Boolean;
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

procedure TFormOLCB_Commander.EthernetConnect;
begin
  EthernetHub.Enabled := True;
  EthernetHub.EnableReceiveMessages := ActionToolsEthernetHubMessageLogShow.Checked;
  EthernetHub.EnableSendMessages := ActionToolsEthernetHubMessageLogShow.Checked;
  UpdateUI
end;

procedure TFormOLCB_Commander.EthernetDisconnect;
begin
  EthernetHub.Enabled := False;
  UpdateUI
end;

function TFormOLCB_Commander.FindChildThatContainsText(ParentNode: TTreeNode; TestString: string): TTreeNode;
begin
  Result := ParentNode.GetFirstChild;
  while (Result <> nil) and (Pos(TestString, Result.Text) = 0) do
    Result := Result.GetNextSibling;
end;

function TFormOLCB_Commander.FindTreeNodeByAlias(Root: TTreeNode; AnAliasID: Word): TOlcbTreeNode;
var
  TreeNode: TTreeNode;
begin
  Result := nil;
  TreeNode := Root.GetFirstChild;
  while Assigned(TreeNode) and not Assigned(Result) do
  begin
    if TOlcbTreeNode( TreeNode).OlcbData.NodeIDAlias = AnAliasID then
      Result := TOlcbTreeNode( TreeNode);
    TreeNode := Root.GetNextChild(TreeNode);
  end;
end;

function TFormOLCB_Commander.FindSNIPNode(AliasNode: TTreeNode): TTreeNode;
var
  ProtocolsNode: TTreeNode;
begin
  Result := nil;
  ProtocolsNode := AliasNode.FindNode(STR_PROTOCOLS);
  if Assigned(ProtocolsNode) then
    Result := ProtocolsNode.FindNode(STR_SNIP);
end;

function TFormOLCB_Commander.FindPIPNode(AliasNode: TTreeNode): TTreeNode;
var
  ProtocolsNode: TTreeNode;
begin
  Result := nil;
  ProtocolsNode := AliasNode.FindNode(STR_PROTOCOLS);
  if Assigned(ProtocolsNode) then
    Result := ProtocolsNode.FindNode(STR_PROTOCOLSUPPORT);
end;

function TFormOLCB_Commander.FindNodeIDNode(AliasNode: TTreeNode): TTreeNode;
begin
  Result := FindChildThatContainsText(AliasNode, STR_NODEID);
end;

function TFormOLCB_Commander.FindConsumerNode(AliasNode: TTreeNode): TTreeNode;
var
  EventsNode: TTreeNode;
begin
  Result := nil;
  EventsNode := AliasNode.FindNode(STR_EVENTS);
  if Assigned(EventsNode) then
    Result := EventsNode.FindNode(STR_CONSUMERS);
end;

function TFormOLCB_Commander.FindProducerNode(AliasNode: TTreeNode): TTreeNode;
var
  EventsNode: TTreeNode;
begin
  Result := nil;
  EventsNode := AliasNode.FindNode(STR_EVENTS);
  if Assigned(EventsNode) then
    Result := EventsNode.FindNode(STR_PRODUCERS);
end;

function TFormOLCB_Commander.FindMemConfigOptionsNode(AliasNode: TTreeNode): TTreeNode;
var
  ProtocolsNode, MemConfigNode: TTreeNode;
begin
  Result := nil;
  ProtocolsNode := AliasNode.FindNode(STR_PROTOCOLS);
  if Assigned(ProtocolsNode) then
  begin
    MemConfigNode := ProtocolsNode.FindNode(STR_MEM_PROTOCOL);
    if Assigned(MemConfigNode) then
      Result := MemConfigNode.FindNode(STR_CONFIGMEM_OPTIONS);
  end;
end;

function TFormOLCB_Commander.FindMemConfigAddressSpaceNode(AliasNode: TTreeNode): TTreeNode;
var
  ProtocolsNode, MemConfigNode: TTreeNode;
begin
  Result := nil;
  ProtocolsNode := AliasNode.FindNode(STR_PROTOCOLS);
  if Assigned(ProtocolsNode) then
  begin
    MemConfigNode := ProtocolsNode.FindNode(STR_MEM_PROTOCOL);
    if Assigned(MemConfigNode) then
      Result := MemConfigNode.FindNode(STR_CONFIGMEM_ADDRESS_SPACES);
  end;
end;

function TFormOLCB_Commander.IsParentRootNode(Node: TOlcbTreeNode): Boolean;
begin
  Result := (Node.Parent = RootNetworkNode) or (Node.Parent = RootCommandStationNode) or (Node.Parent = RootTrainNode)
end;

procedure TFormOLCB_Commander.RefreshNetworkTreeAliasConfigMemOptions(NodeAlias: Word; Options: TOlcbMemOptions);
var
  Node: TOlcbTreeNode;
  ConfigChildRoot: TTreeNode;
begin
  Node := FindTreeNodeByAlias(RootNetworkNode, NodeAlias);
  if Assigned(Node) then
  begin
    // Update Data
    if Node.OlcbData.ConfigMem = nil then
      Node.OlcbData.ConfigMem := TOlcbMemConfig.Create;
    Options.CopyTo(Node.OlcbData.ConfigMem.Options);

    // Update UI
    ConfigChildRoot := FindMemConfigOptionsNode(Node);
    if Assigned(ConfigChildRoot) then
    begin
      TreeViewNetwork.BeginUpdate;
      try
        ConfigChildRoot.DeleteChildren;
        CreateAndLoadNodeBoolean(ConfigChildRoot, 'Write Under Mask = ', Node.OlcbData.ConfigMem.Options.WriteUnderMask);
        CreateAndLoadNodeBoolean(ConfigChildRoot, 'UnAligned Reads = ', Node.OlcbData.ConfigMem.Options.UnAlignedReads);
        CreateAndLoadNodeBoolean(ConfigChildRoot, 'UnAligned Writes = ', Node.OlcbData.ConfigMem.Options.UnalignedWrites);
        CreateAndLoadNodeBoolean(ConfigChildRoot, 'Read from Mfg ACDI = ', Node.OlcbData.ConfigMem.Options.ReadFromMfgACDI);
        CreateAndLoadNodeBoolean(ConfigChildRoot, 'Read from User ACDI = ', Node.OlcbData.ConfigMem.Options.ReadFromUserACDI);
        CreateAndLoadNodeBoolean(ConfigChildRoot, 'Write to User ACDI = ', Node.OlcbData.ConfigMem.Options.WriteToUserACDI);
        CreateAndLoadNodeBoolean(ConfigChildRoot, 'Write 1 Byte = ', Node.OlcbData.ConfigMem.Options.WriteOneByte);
        CreateAndLoadNodeBoolean(ConfigChildRoot, 'Write 2 Bytes = ', Node.OlcbData.ConfigMem.Options.WriteTwoBytes);
        CreateAndLoadNodeBoolean(ConfigChildRoot, 'Write 4 Bytes = ', Node.OlcbData.ConfigMem.Options.WriteFourBytes);
        CreateAndLoadNodeBoolean(ConfigChildRoot, 'Write 64 Bytes = ', Node.OlcbData.ConfigMem.Options.Write64Bytes);
        CreateAndLoadNodeBoolean(ConfigChildRoot, 'Write Arbitrary Bytes = ', Node.OlcbData.ConfigMem.Options.WriteArbitraryBytes);
        CreateAndLoadNodeBoolean(ConfigChildRoot, 'Write by Stream = ', Node.OlcbData.ConfigMem.Options.WriteStreamBytes);
        CreateAndLoadNodeSimple(ConfigChildRoot, 'Address Space Hi: ' + IntToStr(Node.OlcbData.ConfigMem.Options.AddressSpaceHi));
        CreateAndLoadNodeSimple(ConfigChildRoot, 'Address Space Lo: ' + IntToStr(Node.OlcbData.ConfigMem.Options.AddressSpaceLo));
        CreateAndLoadNodeSimple(ConfigChildRoot, 'Description: ' + Node.OlcbData.ConfigMem.Options.Description);
      finally
        TreeViewNetwork.EndUpdate;
      end;
    end;
  end;
end;

procedure TFormOLCB_Commander.RefreshNetworkTreeAliasConfigMemAddressSpaceInfo(NodeAlias: Word; AddressSpace: TOlcbMemAddressSpace);
var
  Node: TOlcbTreeNode;
  ConfigAddressSpacesChild, AddressSpaceChild: TTreeNode;
  LocalAddressSpace: TOlcbMemAddressSpace;
begin
  Node := FindTreeNodeByAlias(RootNetworkNode, NodeAlias);
  if Assigned(Node) then
  begin
    // Update the Data
    if not Assigned(Node.OlcbData.ConfigMem) then
      Node.OlcbData.ConfigMem := TOlcbMemConfig.Create;
    LocalAddressSpace := Node.OlcbData.ConfigMem.FindAddressSpaceBySpaceID(AddressSpace.Space);
    if not Assigned(LocalAddressSpace) then
      LocalAddressSpace := Node.OlcbData.ConfigMem.AddAddressSpace;
    AddressSpace.CopyTo(LocalAddressSpace);

    // Update UI
    ConfigAddressSpacesChild := FindMemConfigAddressSpaceNode(Node);
    if Assigned(ConfigAddressSpacesChild) then
    begin
      TreeViewNetwork.BeginUpdate;
      try
        AddressSpaceChild := FindChildThatContainsText(ConfigAddressSpacesChild, '0x' + LocalAddressSpace.SpaceAsHex);
        if not Assigned(AddressSpaceChild) then
        begin
          AddressSpaceChild := TreeViewNetwork.Items.AddChild(ConfigAddressSpacesChild, '0x' + LocalAddressSpace.SpaceAsHex + ' [' + IntToStr(LocalAddressSpace.Space) + '] : ' + AddressSpaceToString(LocalAddressSpace.Space));
          AddressSpaceChild.ImageIndex := 41;
          AddressSpaceChild.SelectedIndex := 41;
        end;
        AddressSpaceChild.DeleteChildren;
        CreateAndLoadNodeSimple(AddressSpaceChild, 'Hi Address: 0x' + IntToHex(LocalAddressSpace.AddressHi, 4));
        CreateAndLoadNodeSimple(AddressSpaceChild, 'Lo Address: 0x' + IntToHex(LocalAddressSpace.AddressLo, 4));
        CreateAndLoadNodeBoolean(AddressSpaceChild, 'Implied Lo Address: ', LocalAddressSpace.AddressLoImpliedZero);
        CreateAndLoadNodeBoolean(AddressSpaceChild, 'Is ReadOnly: ', LocalAddressSpace.IsReadOnly);
        CreateAndLoadNodeBoolean(AddressSpaceChild, 'Is Present: ', LocalAddressSpace.IsPresent);
        CreateAndLoadNodeSimple(AddressSpaceChild, 'Description: ' + LocalAddressSpace.Description);
      finally
        TreeViewNetwork.EndUpdate;
      end;
    end;
  end;
end;

procedure TFormOLCB_Commander.RefreshNetworkTreeAliasEvents(NodeAlias: Word; LocalHelper: TOpenLCBMessageHelper);
var
  EventID: TEventID;
  Node, ConsumerNode, ProducerNode, ChildNode: TTreeNode;
begin
  Node := FindTreeNodeByAlias(RootNetworkNode, NodeAlias);
  if Assigned(Node) then
  begin
    TreeViewNetwork.BeginUpdate;
    try
      EventID := LocalHelper.Data;
      case LocalHelper.MTI of
        MTI_CONSUMER_IDENTIFIED_CLEAR,
        MTI_CONSUMER_IDENTIFIED_SET,
        MTI_CONSUMER_IDENTIFIED_UNKNOWN,
        MTI_CONSUMER_IDENTIFIED_RESERVED :
          begin
            ConsumerNode := FindConsumerNode(Node);
            if Assigned(ConsumerNode) then
            begin
              ChildNode := FindChildThatContainsText(ConsumerNode, EventToDoxHex(EventID));
              if Assigned(ChildNode) then
              begin
                case LocalHelper.MTI of
                  MTI_CONSUMER_IDENTIFIED_CLEAR    : ChildNode.Text := EventToDoxHex(EventID) + ' [State = Clear] : ' + EventIDToString(@EventID);
                  MTI_CONSUMER_IDENTIFIED_SET      : ChildNode.Text := EventToDoxHex(EventID) + ' [State = Set] : ' + EventIDToString(@EventID);
                  MTI_CONSUMER_IDENTIFIED_UNKNOWN  : ChildNode.Text := EventToDoxHex(EventID) + ' [State = Unknown] : ' + EventIDToString(@EventID);
                  MTI_CONSUMER_IDENTIFIED_RESERVED : ChildNode.Text := EventToDoxHex(EventID) + ' [State = Reserved] : ' + EventIDToString(@EventID);
                end
              end else
              begin
                case LocalHelper.MTI of
                  MTI_CONSUMER_IDENTIFIED_CLEAR    : CreateAndLoadNodeSimple(ConsumerNode, EventToDoxHex(EventID) + ' [State = Clear] : ' + EventIDToString(@EventID));
                  MTI_CONSUMER_IDENTIFIED_SET      : CreateAndLoadNodeSimple(ConsumerNode, EventToDoxHex(EventID) + ' [State = Set] : ' + EventIDToString(@EventID));
                  MTI_CONSUMER_IDENTIFIED_UNKNOWN  : CreateAndLoadNodeSimple(ConsumerNode, EventToDoxHex(EventID) + ' [State = Unknown] : ' + EventIDToString(@EventID));
                  MTI_CONSUMER_IDENTIFIED_RESERVED : CreateAndLoadNodeSimple(ConsumerNode, EventToDoxHex(EventID) + ' [State = Reserved] : ' + EventIDToString(@EventID));
                end
              end
            end
          end;
        MTI_PRODUCER_IDENTIFIED_CLEAR,
        MTI_PRODUCER_IDENTIFIED_SET,
        MTI_PRODUCER_IDENTIFIED_UNKNOWN,
        MTI_PRODUCER_IDENTIFIED_RESERVED :
          begin
            ProducerNode := FindProducerNode(Node);
            if Assigned(ProducerNode) then
            begin
              ChildNode := FindChildThatContainsText(ProducerNode, EventToDoxHex(EventID));
              if Assigned(ChildNode) then
              begin
                case LocalHelper.MTI of
                  MTI_PRODUCER_IDENTIFIED_CLEAR    : ChildNode.Text := EventToDoxHex(EventID) + ' [State = Clear] : ' + EventIDToString(@EventID);
                  MTI_PRODUCER_IDENTIFIED_SET      : ChildNode.Text := EventToDoxHex(EventID) + ' [State = Set] : ' + EventIDToString(@EventID);
                  MTI_PRODUCER_IDENTIFIED_UNKNOWN  : ChildNode.Text := EventToDoxHex(EventID) + ' [State = Unknown] : ' + EventIDToString(@EventID);
                  MTI_PRODUCER_IDENTIFIED_RESERVED : ChildNode.Text := EventToDoxHex(EventID) + ' [State = Reserved] : ' + EventIDToString(@EventID);
                end
              end else
              begin
                case LocalHelper.MTI of
                  MTI_PRODUCER_IDENTIFIED_CLEAR    : CreateAndLoadNodeSimple(ProducerNode, EventToDoxHex(EventID) + ' [State = Clear] : ' + EventIDToString(@EventID));
                  MTI_PRODUCER_IDENTIFIED_SET      : CreateAndLoadNodeSimple(ProducerNode, EventToDoxHex(EventID) + ' [State = Set] : ' + EventIDToString(@EventID));
                  MTI_PRODUCER_IDENTIFIED_UNKNOWN  : CreateAndLoadNodeSimple(ProducerNode, EventToDoxHex(EventID) + ' [State = Unknown] : ' + EventIDToString(@EventID));
                  MTI_PRODUCER_IDENTIFIED_RESERVED : CreateAndLoadNodeSimple(ProducerNode, EventToDoxHex(EventID) + ' [State = Reserved] : ' + EventIDToString(@EventID));
                end
              end
            end
          end;
    end;
    RefreshTrainTreeAliasEvents(NodeAlias, (Node as TOlcbTreeNode).OlcbData.NodeID, LocalHelper);
    RefreshCommandStationTreeAliasEvents(NodeAlias, (Node as TOlcbTreeNode).OlcbData.NodeID, LocalHelper);
    finally
      TreeViewNetwork.EndUpdate;
    end
  end;
end;

procedure TFormOLCB_Commander.RefreshTrainTreeAliasEvents(NodeAlias: Word; NodeID: QWord; LocalHelper: TOpenLCBMessageHelper);

      procedure UpdateDccAddressEventCaption(EventID: TEventID; Address: Integer);
      var
        Node, ChildNode: TTreeNode;
      begin
        Node := AddNetworkTrainAlias(NodeAlias, NodeID);
        if Assigned(Node) then
        begin
          ChildNode := FindChildThatContainsText(Node, STR_DCC_ADDRESS);
          if Assigned(ChildNode) then
          begin
            if Address > -1 then
            begin
              Address := ((EventID[4] shl 8) or EventID[5]) and $3FFF;  // Strip off the Extended bits if there are there
              if EventID[4] and $C0 = $C0 then
                ChildNode.Text := STR_DCC_ADDRESS + ': ' + IntToStr(Address) + ' ' + STR_LONG_ADDRESS
              else
                ChildNode.Text := STR_DCC_ADDRESS + ': ' + IntToStr(Address) + ' ' + STR_SHORT_ADDRESS
            end else
              ChildNode.Text := STR_DCC_ADDRESS + ': ' + STR_UNKNOWN;
          end
        end;
      end;

var
  EventID: TEventID;
  Node, ChildNode: TTreeNode;
begin
  EventID := LocalHelper.Data;

  if EqualEvents(@EventID, @EVENT_TRAIN) then
    Node := AddNetworkTrainAlias(NodeAlias, NodeID)
  else
  if EqualEvents(@EventID, @EVENT_TRAIN_PROXY_IDLE) then
  begin
    Node := AddNetworkTrainAlias(NodeAlias, NodeID);
    if Assigned(Node) then
    begin
      ChildNode :=  FindChildThatContainsText(Node, STR_ISIDLEPROXY);
      if Assigned(ChildNode) then
      begin
        case LocalHelper.MTI of
          MTI_PRODUCER_IDENTIFIED_CLEAR: ChildNode.Text := SetBooleanCaption(STR_ISIDLEPROXY + ': ', False);
          MTI_PRODUCER_IDENTIFIED_SET:
            begin
              ChildNode.Text := SetBooleanCaption(STR_ISIDLEPROXY + ': ', True);
              UpdateDccAddressEventCaption(EventID, -1);
            end;
          MTI_PRODUCER_IDENTIFIED_UNKNOWN: ChildNode.Text := STR_ISIDLEPROXY + ': ' + STR_UNKNOWN;
        end
      end
    end
  end else
  if EqualEvents(@EventID, @EVENT_TRAIN_PROXY_INUSE) then
  begin
    Node := AddNetworkTrainAlias(NodeAlias, NodeID);
    if Assigned(Node) then
    begin
      ChildNode :=  FindChildThatContainsText(Node, STR_ISINUSEPROXY);
      if Assigned(ChildNode) then
      begin
        case LocalHelper.MTI of
          MTI_PRODUCER_IDENTIFIED_CLEAR: ChildNode.Text := SetBooleanCaption(STR_ISINUSEPROXY + ': ', False);
          MTI_PRODUCER_IDENTIFIED_SET: ChildNode.Text := SetBooleanCaption(STR_ISINUSEPROXY + ': ', True);
          MTI_PRODUCER_IDENTIFIED_UNKNOWN: ChildNode.Text := STR_ISINUSEPROXY + ': ' + STR_UNKNOWN;
        end
      end
    end
  end else
  if (EventID[0] = $06) and (EventID[1] = $01) then
    UpdateDccAddressEventCaption(EventID, 1)
end;

procedure TFormOLCB_Commander.RefreshCommandStationTreeAliasEvents(NodeAlias: Word; NodeID: QWord; LocalHelper: TOpenLCBMessageHelper);
var
  EventID: TEventID;
begin
  EventID := LocalHelper.Data;
  if EqualEvents(@EventID, @EVENT_COMMAND_STATION) then
    AddNetworkCommandStationAlias(NodeAlias, NodeID);
end;

procedure TFormOLCB_Commander.RefreshNetworkTreeAliasProtocolSupport(NodeAlias: Word; Protocols: QWord);
var
  ProtocolChild: TTreeNode;
  Node: TOlcbTreeNode;
  Mask: QWord;
  i: Integer;
begin
  Node := FindTreeNodeByAlias(RootNetworkNode, NodeAlias);
  if Assigned(Node) then
  begin
    // Update the Data
    Node.OlcbData.ProtocolSupport := Protocols;

    // Update the UI
    ProtocolChild := FindPIPNode(Node);
    if Assigned(ProtocolChild) then
    begin
      TreeViewNetwork.BeginUpdate;
      try
        ProtocolChild.DeleteChildren;

        Mask := $800000000000;
        for i := 0 to 47 do
        begin
          if Protocols and Mask <> 0 then
            CreateAndLoadNodeSimple(ProtocolChild, ProtocolSupportReplyToString( Mask));
          Mask := Mask shr 1;
        end
      finally
        TreeViewNetwork.EndUpdate;
      end;
    end
  end
end;

procedure TFormOLCB_Commander.RefreshNetworkTreeAliasSnip(NodeAlias: Word; Snip: TOlcbSNIP);
var
  ProtocolChild: TTreeNode;
  Node: TOlcbTreeNode;
begin
  Node := FindTreeNodeByAlias(RootNetworkNode, NodeAlias);
  if Assigned(Node) then
  begin
    // Update the Data
    if not Assigned(Node.OlcbData.Snii) then
      Node.OlcbData.Snii := TOlcbSNIP.Create;
    Snip.CopyTo(Node.OlcbData.Snii);

    // Update the UI
    ProtocolChild := FindSNIPNode(Node);
    if Assigned(ProtocolChild) then
    begin
      TreeViewNetwork.BeginUpdate;
      try
        ProtocolChild.DeleteChildren;
        CreateAndLoadNodeSimple(ProtocolChild, 'Content Version: ' + IntToStr(Node.OlcbData.Snii.SniiMfgVersion));
        CreateAndLoadNodeSimple(ProtocolChild, 'Mfg Name : ' + Node.OlcbData.Snii.SniiMfgName);
        CreateAndLoadNodeSimple(ProtocolChild, 'Mfg Model : ' + Node.OlcbData.Snii.SniiMfgModel);
        CreateAndLoadNodeSimple(ProtocolChild, 'Hardware Ver: ' + Node.OlcbData.Snii.SniiHardwareVersion);
        CreateAndLoadNodeSimple(ProtocolChild, 'Software Ver : ' + Node.OlcbData.Snii.SniiSoftwareVersion);
        CreateAndLoadNodeSimple(ProtocolChild, 'Content Version: ' + IntToStr(Node.OlcbData.Snii.SniiMfgVersion));
        CreateAndLoadNodeSimple(ProtocolChild, 'User Name : ' + Node.OlcbData.Snii.SniiUserName);
        CreateAndLoadNodeSimple(ProtocolChild, 'User Desc : ' + Node.OlcbData.Snii.SniiUserDescription);

        Node.Text := Node.OlcbData.Snii.SniiMfgName + '  Model: ' + Node.OlcbData.Snii.SniiMfgModel + '  User Name: ' + Node.OlcbData.Snii.SniiUserName + '  AliasID: [0x' + IntToHex(Node.OlcbData.NodeIDAlias, 4) + ']';
        RootNetworkNode.CustomSort(@TreeViewNetwork.DefaultTreeViewSort);
      finally
        TreeViewNetwork.EndUpdate;
      end;
    end
  end
end;

procedure TFormOLCB_Commander.SyncReceiveCOMPortMessage(MessageStr: String);
begin
  MessageHelper.Decompose(MessageStr);
  if FormMessageLog.Visible and not FormMessageLog.Paused then
  begin
    FormMessageLog.SynMemo.Lines.BeginUpdate;
    FormMessageLog.SynMemo.Text := FormMessageLog.SynMemo.Text + MessageToDetailedMessage( MessageStr, False);
    FormMessageLog.SynMemo.CaretY := FormMessageLog.SynMemo.LineHeight * FormMessageLog.SynMemo.Lines.Count;
    FormMessageLog.SynMemo.Lines.EndUpdate;
  end;
end;

procedure TFormOLCB_Commander.SyncSendCOMPortMessage(MessageStr: String);
begin
  if MessageHelper.Decompose(MessageStr) then
  begin
    if FormMessageLog.Visible and not FormMessageLog.Paused then
    begin
      FormMessageLog.SynMemo.Lines.BeginUpdate;
      FormMessageLog.SynMemo.Text := FormMessageLog.SynMemo.Text + MessageToDetailedMessage( MessageStr, True);
      FormMessageLog.SynMemo.CaretY := FormMessageLog.SynMemo.LineHeight * FormMessageLog.SynMemo.Lines.Count;
      FormMessageLog.SynMemo.Lines.EndUpdate;
    end;
  end;
end;

procedure TFormOLCB_Commander.SyncErrorEthernetMessage(MessageStr: String);
begin
 // UpdateUI;
 // ShowMessage(MessageStr);
end;

procedure TFormOLCB_Commander.SyncReceiveEthernetMessage(MessageStr: String);
begin
  MessageHelper.Decompose(MessageStr);
  if FormEthernetMessageLog.Visible and not FormEthernetMessageLog.Paused then
  begin
    FormEthernetMessageLog.SynMemo.Lines.BeginUpdate;
    FormEthernetMessageLog.SynMemo.Text := FormEthernetMessageLog.SynMemo.Text + MessageToDetailedMessage( MessageStr, False);
    FormEthernetMessageLog.SynMemo.CaretY := FormEthernetMessageLog.SynMemo.LineHeight * FormEthernetMessageLog.SynMemo.Lines.Count;
    FormEthernetMessageLog.SynMemo.Lines.EndUpdate;
  end;
end;

procedure TFormOLCB_Commander.SyncSendEthernetMessage(MessageStr: String);
begin
  if MessageHelper.Decompose(MessageStr) then
  begin
    if FormEthernetMessageLog.Visible and not FormEthernetMessageLog.Paused then
    begin
      FormEthernetMessageLog.SynMemo.Lines.BeginUpdate;
      FormEthernetMessageLog.SynMemo.Text := FormEthernetMessageLog.SynMemo.Text + MessageToDetailedMessage( MessageStr, True);
      FormEthernetMessageLog.SynMemo.CaretY := FormEthernetMessageLog.SynMemo.LineHeight * FormEthernetMessageLog.SynMemo.Lines.Count;
      FormEthernetMessageLog.SynMemo.Lines.EndUpdate;
    end;
  end;
end;

procedure TFormOLCB_Commander.SyncHubConnect(HostIP: string; HostPort: Integer);
begin
  Statusbar.Panels[2].Text := 'Hub Connected to IP: ' + HostIP + ' Port: ' + IntToStr(HostPort);
  Statusbar.Panels[3].Text := 'Clients = 0';
end;

procedure TFormOLCB_Commander.SyncHubDisconnect(HostIP: string; HostPort: Integer);
begin
  Statusbar.Panels[2].Text := 'Hub Disconnected';
  Statusbar.Panels[3].Text := '';
end;

procedure TFormOLCB_Commander.SyncHubNewClient(SocketCount: Integer);
begin
  Statusbar.Panels[3].Text := 'Client Count: ' + IntToStr(SocketCount);
end;

procedure TFormOLCB_Commander.SyncHubDroppedClient(SocketCount: Integer);
begin
  Statusbar.Panels[3].Text := 'Client Count: ' + IntToStr(SocketCount);
end;

procedure TFormOLCB_Commander.SyncHubOnStatus(Client: TClientSocketThread; Reason: THookSocketReason; Value: String);
begin
  if ToolButtonLogEthernet.Down then
  begin
    ListBoxLog.Items.BeginUpdate;
    case Reason of
      HR_ResolvingBegin :  ListBoxLog.AddItem('Resolving Begin: ' + Value + ' [Thread: 0x' + IntToHex(IntPtr( Client), 8) + ']', nil);
      HR_ResolvingEnd :    ListBoxLog.AddItem('Resolving End: ' + Value + ' [Thread: 0x' + IntToHex(IntPtr( Client), 8) + ']', nil);
      HR_SocketCreate :    ListBoxLog.AddItem('Socket Create: ' + Value + ' [Thread: 0x' + IntToHex(IntPtr( Client), 8) + ']', nil);
      HR_SocketClose :     ListBoxLog.AddItem('Socket Close: ' + Value + ' [Thread: 0x' + IntToHex(IntPtr( Client), 8) + ']', nil);
      HR_Bind :            ListBoxLog.AddItem('Bind: ' + Value + ' [Thread: 0x' + IntToHex(IntPtr( Client), 8) + ']', nil);
      HR_Connect :         ListBoxLog.AddItem('Connect: ' + Value + ' [Thread: 0x' + IntToHex(IntPtr( Client), 8) + ']', nil);
      HR_CanRead :         ListBoxLog.AddItem('Can Read: ' + Value + ' [Thread: 0x' + IntToHex(IntPtr( Client), 8) + ']', nil);
      HR_CanWrite :        ListBoxLog.AddItem('Can Write: ' + Value + ' [Thread: 0x' + IntToHex(IntPtr( Client), 8) + ']', nil);
      HR_Listen :          ListBoxLog.AddItem('Listen: ' + Value + ' [Thread: 0x' + IntToHex(IntPtr( Client), 8) + ']', nil);
      HR_Accept :          ListBoxLog.AddItem('Accept: ' + Value + ' [Thread: 0x' + IntToHex(IntPtr( Client), 8) + ']', nil);
      HR_ReadCount :       ListBoxLog.AddItem('Read Count: ' + Value + ' [Thread: 0x' + IntToHex(IntPtr( Client), 8) + ']', nil);
      HR_WriteCount :      ListBoxLog.AddItem('Write Count: ' + Value + ' [Thread: 0x' + IntToHex(IntPtr( Client), 8) + ']', nil);
      HR_Wait :            ListBoxLog.AddItem('Wait: ' + Value + ' [Thread: 0x' + IntToHex(IntPtr( Client), 8) + ']', nil);
   //   HR_Error :           ListBoxLog.AddItem('Error: ' + Value + ' [Thread: 0x' + IntToHex(IntPtr( Client), 8) + ']', nil);
    end;
    ListBoxLog.Items.EndUpdate;
  end;
end;

 {$IFDEF DEBUG_THREAD}
procedure TFormOLCB_Commander.SyncDebugMessage(Info: TComPortThreadDebugRec);
begin
  FormThreadDebug.LabelDatagramReceiveObjects.Caption := IntToStr( Info.ReceiveDatagramCount);
  FormThreadDebug.LabelDatagramReceiveObjectsMax.Caption := IntToStr(Info.MaxReceiveDatagramCount);
  FormThreadDebug.LabelDatagramSendObjects.Caption := IntToStr( Info.SendDatagramCount);
  FormThreadDebug.LabelDatagramSendObjectsMax.Caption := IntToStr( Info.MaxSendDatagramCount);
  FormThreadDebug.LabelTaskObjects.Caption := IntToStr( Info.TaskCount);
  FormThreadDebug.LabelTaskObjectsMax.Caption := IntToStr( Info.MaxTaskCount);
  FormThreadDebug.LabelMaxTime.Caption := IntToStr(Info.MaxThreadTime);
  FormThreadDebug.LabelLoopTime.Caption := IntToStr(Info.ThreadTime);
end;
{$ENDIF}

procedure TFormOLCB_Commander.SyncErrorCOMPortMessage(MessageStr: String);
begin
  UpdateUI;
  ShowMessage(MessageStr);
end;

procedure TFormOLCB_Commander.SyncMessageLogHide;
begin
  ActionToolsCOMPortMessageLogShow.Checked := False;
  if Assigned(ComPortHub) then
  begin
    ComPortHub.EnableReceiveMessages := False;
    ComPortHub.EnableSendMessages := False;
  end;
end;

procedure TFormOLCB_Commander.SyncEthernetMessageLogHide;
begin
  ActionToolsEthernetHubMessageLogShow.Checked := False;
  if EthernetHub.Enabled then
  begin
    EthernetHub.EnableReceiveMessages := False;
    EthernetHub.EnableSendMessages := False;
  end;
end;

procedure TFormOLCB_Commander.SyncThrottleHide(Throttle: TFormAwesomeThrottle);
begin

end;

procedure TFormOLCB_Commander.SyncThrottleClose(Throttle: TFormAwesomeThrottle);
var
  Index: Integer;
begin
  Index := ThrottleList.IndexOf( Throttle);
  if Index > -1 then
  begin
    ThrottleList.Delete( Index);
    DeleteThrottleSubMenu(Throttle);
    UpdateUI
  end
end;

procedure TFormOLCB_Commander.SyncConfigEditorHide(ConfigEditor: TFormTrainConfigEditor);
begin

end;

procedure TFormOLCB_Commander.SyncConfigEditorClose(ConfigEditor: TFormTrainConfigEditor);
var
  Index: Integer;
begin
  Index := ConfigEditorList.IndexOf( ConfigEditor);
  if Index > -1 then
  begin
    ConfigEditorList.Delete( Index);
    ComPortHub.RemoveAndFreeTasks( PtrInt( ConfigEditor));
    DeleteConfigEditorSubMenu(ConfigEditor);
    UpdateUI
  end
end;

procedure TFormOLCB_Commander.OnBeforeDestroyTask(Sender: TOlcbTaskBase);
var
  MemTask: TReadAddressSpaceMemoryTask;
  MemConfigViewer: TFormMemConfigViewer;
  EnumAllSpaces: TEnumAllConfigMemoryAddressSpaceInfoTask;
  ADoc: TXMLDocument;
  x: string;
  i: Integer;
  Done: Boolean;
begin
  if not Sender.ForceTermination then
  begin
    if Sender is TReadAddressSpaceMemoryTask then
    begin
      MemTask := TReadAddressSpaceMemoryTask( Sender);
      MemConfigViewer := TFormMemConfigViewer.Create(Application);
      MemConfigViewer.Caption := MemConfigViewer.Caption + ' - Alias:  0x' + IntToHex(MemTask.DestinationAlias, 4) + '  Address Space: ' + IntToStr(MemTask.AddressSpace);
      case MemTask.AddressSpace of
        MSI_CDI{, MSI_FDI}:
          begin
            try
              MemConfigViewer.KHexEditor.Visible := False;
              MemConfigViewer.SynEditCDI.Visible := True;
              MemConfigViewer.SynEditCDI.BeginUpdate;
              MemConfigViewer.SynEditCDI.ClearAll;
              MemConfigViewer.SynEditCDI.EndUpdate;
              MemTask.DataStream.Position := MemTask.DataStream.Size - 1;

              Done := False;
              MemTask.DataStream.Position := 0;
              while not Done and (MemTask.DataStream.Position < MemTask.DataStream.Size) do
              begin
                if Char( MemTask.DataStream.ReadByte) = #0 then
                begin
                  // Strip the null and any trailing characters.
                  MemTask.DataStream.Size := MemTask.DataStream.Position - 1;
                  Done := True;
                end
              end;
              MemTask.DataStream.Position := 0;
              ReadXMLFile(ADoc, MemTask.DataStream);                 // This corrupts the stream from its original contents
              WriteXMLFile(ADoc, MemTask.DataStream);
              MemConfigViewer.XmlDoc := ADoc;
              x := StreamAsString(MemTask.DataStream);
              MemConfigViewer.SynEditCDI.Text := x ;
            except
              MemConfigViewer.SynEditCDI.Lines.Add('************************');
              MemConfigViewer.SynEditCDI.Lines.Add('ERROR');
              MemConfigViewer.SynEditCDI.Lines.Add('************************');
              MemConfigViewer.SynEditCDI.Lines.Add('FAILED TO PARSE XML FILE');
              MemConfigViewer.SynEditCDI.Lines.Add('************************');
              FreeAndNil(ADoc);
            end;
          end
       else
          begin
            MemConfigViewer.SynEditCDI.Visible := False;
            MemConfigViewer.KHexEditor.Visible := True;
            MemTask.DataStream.Position := 0;
            MemConfigViewer.KHexEditor.LoadFromStream(MemTask.DataStream);
          end;
      end;
      MemConfigViewer.Show;
    end else
    if Sender is TConfigMemoryAddressSpaceInfoTask then
      RefreshNetworkTreeAliasConfigMemAddressSpaceInfo(Sender.DestinationAlias, TConfigMemoryAddressSpaceInfoTask( Sender).ConfigMemoryAddressSpace)
    else
    if Sender is TConfigMemoryOptionsTask then
       RefreshNetworkTreeAliasConfigMemOptions(Sender.DestinationAlias, TConfigMemoryOptionsTask( Sender).ConfigMemoryOptions)
    else
    if Sender is TConfigMemoryAddressSpaceInfoTask then
      RefreshNetworkTreeAliasConfigMemAddressSpaceInfo(Sender.DestinationAlias, TConfigMemoryAddressSpaceInfoTask( Sender).ConfigMemoryAddressSpace)
    else
    if Sender is TProtocolSupportTask then
      RefreshNetworkTreeAliasProtocolSupport(Sender.DestinationAlias, TProtocolSupportTask( Sender).Protocols)
    else
    if Sender is TSimpleNodeInformationTask then
      RefreshNetworkTreeAliasSnip(Sender.DestinationAlias, TSimpleNodeInformationTask( Sender).Snip)
    else
    if Sender is TEnumAllConfigMemoryAddressSpaceInfoTask then
    begin
      EnumAllSpaces := TEnumAllConfigMemoryAddressSpaceInfoTask( Sender);
      for i := 0 to EnumAllSpaces.ConfigMemAddressInfo.AddressSpaceCount - 1 do
        RefreshNetworkTreeAliasConfigMemAddressSpaceInfo(Sender.DestinationAlias, EnumAllSpaces.ConfigMemAddressInfo.AddressSpace[i])
    end else
    if Sender is TEventTask then
    begin
      RefreshNetworkTreeAliasEvents(Sender.DestinationAlias, Sender.MessageHelper);
      for i := 0 to ThrottleList.Count - 1 do
        ThrottleList.Throttles[i].EventTaskReceived(Sender as TEventTask);
    end else
    if Sender is TCANLayerTask then
    begin
      case Sender.MessageHelper.MTI of
        MTI_AMD : AddNetworkTreeAlias(Sender.DestinationAlias, Sender.MessageHelper.ExtractDataBytesAsInt(0, 5), True);
        MTI_AMR : DeleteNetworkTreeAlias(Sender.DestinationAlias);
      end
    end else
    if Sender is TVerifiedNodeIDTask then
    begin
      if Sender.MessageHelper.DataCount = 6 then
        AddNetworkTreeAlias(Sender.DestinationAlias, Sender.MessageHelper.ExtractDataBytesAsInt(0, 5), True)
      else
        AddNetworkTreeAlias(Sender.DestinationAlias, 0, True);
    end
   else
    if Sender is TTractionProtocolTask then
    begin

    end else
    if Sender is TInitializationCompleteTask then
    begin

    end;
  end;

  StatusBar.Panels[4].Text := 'Task Count: ' + IntToStr(TaskObjects-1)  // This task will be freed
end;

procedure TFormOLCB_Commander.OnConfigEditorMenuItemClick(Sender: TObject);
begin
  TFormTrainConfigEditor( TMenuItem(Sender).Tag).ShowOnTop
end;

procedure TFormOLCB_Commander.OnThrottleMenuItemClick(Sender: TObject);
begin
  TFormAwesomeThrottle( TMenuItem(Sender).Tag).ShowOnTop
end;

procedure TFormOLCB_Commander.UpdateUI;
var
  ConfigEditorCreateEnabled: Boolean;
  i: Integer;
begin
  ActionToolsComConnect.Enabled := not ComPortHub.Connected;
  ActionToolsComDisconnect.Enabled := ComPortHub.Connected;

  ActionToolsEthernetHubConnect.Enabled := not EthernetHub.Enabled;
  ActionToolsEthernetHubDisconnect.Enabled := EthernetHub.Enabled;

  ActionOpenLCBCommandIdentifyIDGlobal.Enabled := Assigned(FComPortHub) or EthernetHub.Enabled;
  ActionOpenLCBCommandProtocolSupport.Enabled := (TreeViewNetwork.SelectionCount > 0);
  ActionOpenLCBCommandMemConfigOptions.Enabled := (TreeViewNetwork.SelectionCount > 0);
  ActionOpenLCBCommandMemConfigAllSpacesInfo.Enabled:= (TreeViewNetwork.SelectionCount > 0);
  ActionOpenLCBCommandSNII.Enabled := (TreeViewNetwork.SelectionCount > 0);
  ActionOpenLCBCommandAll.Enabled := (RootNetworkNode.Count > 0);

  StatusBar.Panels[0].Text := 'Node Count = ' + IntToStr(RootNetworkNode.Count);
  StatusBar.Panels[1].Text := 'Train Count = ' + IntToStr(RootTrainNode.Count);

  MenuItemThrottlesSep1.Visible := ThrottleList.Count > 0;
  ActionThrottlesCloseAll.Enabled := ThrottleList.Count > 0;
  ActionThrottlesHideAll.Enabled := ThrottleList.Count > 0;
  ActionThrottlesShowAll.Enabled := ThrottleList.Count > 0;

  MenuItemConfigEditorsSep1.Visible := ConfigEditorList.Count > 0;
  ActionConfigEditorsCloseAll.Enabled := ConfigEditorList.Count > 0;
  ActionConfigEditorsHideAll.Enabled := ConfigEditorList.Count > 0;
  ActionConfigEditorsShowAll.Enabled := ConfigEditorList.Count > 0;
  ConfigEditorCreateEnabled := False;
  if TreeViewNetwork.SelectionCount > 0 then
  begin
    ConfigEditorCreateEnabled := True; // Think Positve
    for i := 0 to TreeViewNetwork.SelectionCount - 1 do
    begin
      if not IsParentRootNode(TreeViewNetwork.Selections[i] as TOlcbTreeNode) then
        ConfigEditorCreateEnabled := False;
    end;
  end;
  ActionConfigEditorsCreate.Enabled := ConfigEditorCreateEnabled;
  ActionOpenLCBCommandReadCDI.Enabled := ConfigEditorCreateEnabled;
  ActionOpenLCBCommandReadAll.Enabled := ConfigEditorCreateEnabled;
  ActionOpenLCBCommandReadConfiguration.Enabled := ConfigEditorCreateEnabled;
  ActionOpenLCBCommandReadMfgACDI.Enabled := ConfigEditorCreateEnabled;
  ActionOpenLCBCommandReadUserACDI.Enabled := ConfigEditorCreateEnabled;
  ActionOpenLCBCommandReadFDI.Enabled := ConfigEditorCreateEnabled;
end;


end.
