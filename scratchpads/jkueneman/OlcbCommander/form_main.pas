unit form_main;

{$mode objfpc}{$H+}

{.$DEFINE DEBUG_THREAD}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, ActnList, ComCtrls, ExtCtrls, Buttons, olcb_threaded_stack,
  olcb_app_common_settings, file_utilities, form_settings, form_about, lcltype,
  types, olcb_utilities, olcb_defines, form_messagelog, olcb_node, olcb_mem_protocol,
  {$IFDEF DEBUG_THREAD}
  form_thread_debug
  {$ENDIF}
  olcb_common_tasks;

const
  BUNDLENAME             = 'OpenLCB Commander';
  PATH_LINUX_APP_FOLDER  = 'olcbcommander/';
  PATH_SETTINGS_FILE     = 'settings.ini';

  STR_NETWORKTREE_ROOT  = 'Network Tree';
  STR_PROTOCOLSUPPORT   = 'Supported Protocols';
  STR_SNII              = 'Simple Node Identification Protocol (SNIP/SNII)';
  STR_MEM_PROTOCOL      = 'Memory Protocol';
  STR_CONFIGMEM_OPTIONS = 'Options';
  STR_CONFIGMEM_ADDRESS_SPACES = 'Address Spaces';
  STR_CONFIGMEM_ADDRESS_SPACE = 'Address Space: ';


type
  TUpdateTreeReason = (
    utr_Add,
    utr_Remove
  );

type

  { TOlcbTreeNode }

  TOlcbTreeNode = class( TTreeNode)
  private
    FOlcbData: TOpenLcbNode;
  public
    constructor Create(AnOwner: TTreeNodes);
    destructor Destroy; override;
    property OlcbData: TOpenLcbNode read FOlcbData write FOlcbData;
  end;

  { TFormOLCB_Commander }

  TFormOLCB_Commander = class(TForm)
    ActionTreeviewNetworkExpandAll: TAction;
    ActionTreeviewNetworkCollapseAll: TAction;
    ActionOpenLCBCommandMemConfigSpaces: TAction;
    ActionOpenLCBCommandMemConfigOptions: TAction;
    ActionOpenLCBCommandAll: TAction;
    ActionOpenLCBCommandSNII: TAction;
    ActionOpenLCBCommandProtocolSupport: TAction;
    ActionToolsMessageLogShow: TAction;
    ActionOpenLCBCommandIdentifyIDGlobal: TAction;
    ActionToolsComDisconnect: TAction;
    ActionToolsComConnect: TAction;
    ActionHelpAboutShow: TAction;
    ActionToolsPreferenceShowMac: TAction;
    ActionToolsSettingsShowWin: TAction;
    ActionListMain: TActionList;
    ApplicationProperties1: TApplicationProperties;
    BitBtnNetworkRefresh: TBitBtn;
    BitBtnProtocolSupport: TBitBtn;
    BitBtnSNIP: TBitBtn;
    BitBtnNetworkRefresh3: TBitBtn;
    BitBtnMemConfgOptions: TBitBtn;
    BitBtnMemConfigAddressSpaces: TBitBtn;
    ImageListMainSmall: TImageList;
    LabelNetworkNodeCount: TLabel;
    LabelNetworkNodeCountValue: TLabel;
    MainMenu: TMainMenu;
    MenuItemTVPopupIdentifyGlobal: TMenuItem;
    MenuItemTVPopupExpandAll: TMenuItem;
    MenuItemTVPopupCollapseAll: TMenuItem;
    MenuItemTVPopupSep3: TMenuItem;
    MenuItemTVPopupSNIP: TMenuItem;
    MenuItemTVPopupSep2: TMenuItem;
    MenuItemTVPopupProtocol: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItemTVPopupSep1: TMenuItem;
    MenuItemTVPopupMemOptions: TMenuItem;
    MenuItemTVPopupMemAddressSpaces: TMenuItem;
    MenuItemToolsSep2: TMenuItem;
    MenuItemToolsMessageLog: TMenuItem;
    MenuItemToolsComConnect: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItemToolsComDisconnect: TMenuItem;
    MenuItemToolsSep1: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemToolsSettingsShow: TMenuItem;
    MenuItemTools: TMenuItem;
    PageControlMain: TPageControl;
    PanelOLCBNetwork: TPanel;
    PanelNetwork: TPanel;
    PopupMenuTreeNode: TPopupMenu;
    SplitterMain: TSplitter;
    TabSheetNetwork: TTabSheet;
    TabSheetTrain: TTabSheet;
    TimerComStartup: TTimer;
    TreeViewNetwork: TTreeView;
    procedure ActionHelpAboutShowExecute(Sender: TObject);
    procedure ActionOpenLCBCommandAllExecute(Sender: TObject);
    procedure ActionOpenLCBCommandIdentifyIDGlobalExecute(Sender: TObject);
    procedure ActionOpenLCBCommandMemConfigOptionsExecute(Sender: TObject);
    procedure ActionOpenLCBCommandMemConfigSpacesExecute(Sender: TObject);
    procedure ActionOpenLCBCommandProtocolSupportExecute(Sender: TObject);
    procedure ActionOpenLCBCommandSNIIExecute(Sender: TObject);
    procedure ActionToolsComConnectExecute(Sender: TObject);
    procedure ActionToolsComDisconnectExecute(Sender: TObject);
    procedure ActionToolsMessageLogShowExecute(Sender: TObject);
    procedure ActionToolsPreferenceShowMacExecute(Sender: TObject);
    procedure ActionToolsSettingsShowWinExecute(Sender: TObject);
    procedure ActionTreeviewNetworkCollapseAllExecute(Sender: TObject);
    procedure ActionTreeviewNetworkExpandAllExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerComStartupTimer(Sender: TObject);
    procedure TreeViewNetworkAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
    procedure TreeViewNetworkCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
    procedure TreeViewNetworkCustomCreateItem(Sender: TCustomTreeView; var ATreeNode: TTreenode);
    procedure TreeViewNetworkSelectionChanged(Sender: TObject);
  private
    FComPortThread: TComPortThread;
    FMessageHelper: TOpenLCBMessageHelper;
    {$IFDEF DARWIN}
    FOSXMenu: TMenuItem;
    FOSXSep1Cmd: TMenuItem;
    FOSXPrefCmd: TMenuItem;
    {$ENDIF}
    FAppAboutCmd: TMenuItem;
    FRootNetworkNode: TTreeNode;
    FShownOnce: Boolean;
    { private declarations }
  protected
    procedure ComDisconnect;
    function FindTreeNodeByAlias(AnAliasID: Word): TOlcbTreeNode;
    procedure SyncDatagramMemConfigAddressSpaceInfoReply(Datagram: TDatagramReceive);
    procedure SyncDatagramMemConfigOperationReply(Datagram: TDatagramReceive);
    procedure SyncErrorMessage(MessageStr: String);
    procedure SyncReceiveMessage(MessageStr: String);
    procedure SyncSniiReply(Snii: TSnii);
    procedure SyncSendMessage(MessageStr: String);
    procedure SyncMessageLogHide;
    procedure ProcessNodeTree(LocalHelper: TOpenLCBMessageHelper; Reason: TUpdateTreeReason);
    procedure ProcessProtocolSupport(LocalHelper: TOpenLCBMessageHelper);
    procedure UpdateUI;
    {$IFDEF DARWIN}
    property OSXMenu: TMenuItem read FOSXMenu write FOSXMenu;
    property OSXSep1Cmd: TMenuItem read FOSXSep1Cmd write FOSXSep1Cmd;
    property OSXPrefCmd: TMenuItem read FOSXPrefCmd write FOSXPrefCmd;
    {$ENDIF}
    property AppAboutCmd: TMenuItem read FAppAboutCmd write FAppAboutCmd;
    property ComPortThread: TComPortThread read FComPortThread write FComPortThread;
    property MessageHelper: TOpenLCBMessageHelper read FMessageHelper write FMessageHelper;
    property RootNetworkNode: TTreeNode read FRootNetworkNode write FRootNetworkNode;
    property ShownOnce: Boolean read FShownOnce write FShownOnce;
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

procedure TFormOLCB_Commander.ActionTreeviewNetworkCollapseAllExecute(Sender: TObject);
begin
  TreeViewNetwork.BeginUpdate;
  try
    TreeViewNetwork.FullCollapse;
    RootNetworkNode.Expanded := True;
  finally
    TreeViewNetwork.EndUpdate;
  end;
end;

procedure TFormOLCB_Commander.ActionTreeviewNetworkExpandAllExecute(Sender: TObject);
begin
  TreeViewNetwork.FullExpand;
end;

procedure TFormOLCB_Commander.ActionHelpAboutShowExecute(Sender: TObject);
begin
  FormAbout.ShowModal
end;

procedure TFormOLCB_Commander.ActionOpenLCBCommandAllExecute(Sender: TObject);
var
  Node: TTreeNode;
  Task: TQueryAllAddressSpacesInfoTask;
begin
  Node := RootNetworkNode.GetFirstChild;
  while Assigned(Node) do
  begin
    MessageHelper.Load(ol_OpenLCB, MTI_PROTOCOL_SUPPORT_INQUIRY, GlobalSettings.General.AliasIDAsVal, StrToInt( Node.Text), 2, 0, 0, 0, 0 ,0 ,0 ,0 ,0);
    ComPortThread.Add(MessageHelper.Encode);

    MessageHelper.Load(ol_OpenLCB, MTI_SIMPLE_NODE_INFO_REQUEST, GlobalSettings.General.AliasIDAsVal, StrToInt( Node.Text), 2, 0, 0, 0, 0 ,0 ,0 ,0 ,0);
    ComPortThread.Add(MessageHelper.Encode);

    Task := TQueryAllAddressSpacesInfoTask.Create(GlobalSettings.General.AliasIDAsVal, StrToInt( Node.Text));
    ComPortThread.AddTask(Task);

    Node := RootNetworkNode.GetNextChild(Node);
  end;
end;

procedure TFormOLCB_Commander.ActionOpenLCBCommandIdentifyIDGlobalExecute(Sender: TObject);
begin
  TreeViewNetwork.BeginUpdate;
  try
    RootNetworkNode.DeleteChildren;
  finally
    TreeViewNetwork.EndUpdate;
  end;
  MessageHelper.Load(ol_OpenLCB, MTI_VERIFY_NODE_ID_NUMBER, GlobalSettings.General.AliasIDAsVal, 0, 0, 0, 0, 0, 0 ,0 ,0 ,0 ,0);
  ComPortThread.Add(MessageHelper.Encode);
end;

procedure TFormOLCB_Commander.ActionOpenLCBCommandMemConfigOptionsExecute(Sender: TObject);
var
  Node: TOlcbTreeNode;
  i: Integer;
  Datagram: TDatagramSend;
begin
  for i := 0 to TreeViewNetwork.SelectionCount - 1 do
  begin
    Node := TreeViewNetwork.Selections[i] as TOlcbTreeNode;
    if Node.Parent = RootNetworkNode then
    begin;
      Datagram := TDatagramSend.Create;
      Datagram.Initialize(nil, HEADER_MEMCONFIG_OPTIONS_REQUEST, 2, GlobalSettings.General.AliasIDAsVal, Node.OlcbData.NodeIDAlias);
      ComPortThread.AddDatagramToSend(Datagram);
    end;
  end;
end;

procedure TFormOLCB_Commander.ActionOpenLCBCommandMemConfigSpacesExecute(Sender: TObject);
var
  Node: TOlcbTreeNode;
  i: Integer;
  Task: TOlcbTaskBase;
begin
  for i := 0 to TreeViewNetwork.SelectionCount - 1 do
  begin
    Node := TreeViewNetwork.Selections[i] as TOlcbTreeNode;
    if Node.Parent = RootNetworkNode then
    begin;
      Task := TQueryAllAddressSpacesInfoTask.Create(GlobalSettings.General.AliasIDAsVal, StrToInt( Node.Text));
      ComPortThread.AddTask(Task);
    end;
  end;
end;

procedure TFormOLCB_Commander.ActionOpenLCBCommandProtocolSupportExecute(Sender: TObject);
var
  Node: TTreeNode;
  i: Integer;
begin
  for i := 0 to TreeViewNetwork.SelectionCount - 1 do
  begin
    Node := TreeViewNetwork.Selections[i];
    if Node.Parent = RootNetworkNode then
    begin;
      MessageHelper.Load(ol_OpenLCB, MTI_PROTOCOL_SUPPORT_INQUIRY, GlobalSettings.General.AliasIDAsVal, StrToInt( Node.Text), 2, 0, 0, 0, 0 ,0 ,0 ,0 ,0);
      ComPortThread.Add(MessageHelper.Encode);
    end;
  end;
end;

procedure TFormOLCB_Commander.ActionOpenLCBCommandSNIIExecute(Sender: TObject);
var
  Node: TTreeNode;
  i: Integer;
begin
  for i := 0 to TreeViewNetwork.SelectionCount - 1 do
  begin
    Node := TreeViewNetwork.Selections[i];
    if Node.Parent = RootNetworkNode then
    begin;
      MessageHelper.Load(ol_OpenLCB, MTI_SIMPLE_NODE_INFO_REQUEST, GlobalSettings.General.AliasIDAsVal, StrToInt( Node.Text), 2, 0, 0, 0, 0 ,0 ,0 ,0 ,0);
      ComPortThread.Add(MessageHelper.Encode);
    end;
  end;
end;

procedure TFormOLCB_Commander.ActionToolsComConnectExecute(Sender: TObject);
begin
 FComPortThread := TComPortThread.Create(True);
 try
   ComPortThread.FreeOnTerminate := True;
   {$IFDEF MSWINDOWS}
   ComPortThread.Port := GlobalSettings.ComPort.Port;
   {$ELSE}
     {$IFDEF DARWIN}
     ComPortThread.Port := PATH_OSX_DEV + GlobalSettings.ComPort.Port;
     {$ELSE}
     ComPortThread.Port := PATH_LINUX_DEV + GlobalSettings.ComPort.Port;
     {$ENDIF}
   {$ENDIF}
   ComPortThread.BaudRate := GlobalSettings.ComPort.BaudRate;

   ComPortThread.SyncReceiveMessageFunc := @SyncReceiveMessage;
   ComPortThread.SyncSendMessageFunc := @SyncSendMessage;
   ComPortThread.SyncErrorMessageFunc := @SyncErrorMessage;
   ComPortThread.SyncDatagramMemConfigOperationReplyFunc := @SyncDatagramMemConfigOperationReply;
   ComPortThread.SyncDatagramMemConfigAddressSpaceInfoReplyFunc := @SyncDatagramMemConfigAddressSpaceInfoReply;
   ComPortThread.SyncSniiReplyFunc := @SyncSniiReply;
   ComPortThread.Suspended := False;
   TimerComStartup.Enabled := True;
   UpdateUI;
 except
   if Assigned(ComPortThread) then
   begin
     ComPortThread.Terminate;
     ComPortThread := nil;
   end;
   UpdateUI
 end;
end;

procedure TFormOLCB_Commander.ActionToolsComDisconnectExecute(Sender: TObject);
begin
  ComDisconnect;
end;

procedure TFormOLCB_Commander.ActionToolsMessageLogShowExecute(Sender: TObject);
begin
  FormMessageLog.Show;
  ActionToolsMessageLogShow.Checked := True;
end;

procedure TFormOLCB_Commander.ActionToolsPreferenceShowMacExecute(Sender: TObject);
begin
  FormSettings.Show
end;

procedure TFormOLCB_Commander.FormCreate(Sender: TObject);
begin
  FShownOnce := False;
  FMessageHelper := TOpenLCBMessageHelper.Create;
end;

procedure TFormOLCB_Commander.FormDestroy(Sender: TObject);
begin
  ComDisconnect;
  FreeAndNil( FMessageHelper);
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
   // MenuItemToolsSeparatorWin.Visible := False;
    {$ELSE}
    AppAboutCmd := TMenuItem.Create(Self);
    AppAboutCmd.Action := ActionHelpAboutShow;
    MenuItemHelp.Add(AppAboutCmd);
    {$ENDIF}
    FormMessageLog.HideCallback := @SyncMessageLogHide;
    {$IFDEF Linux}
    FormSettings.SettingsFilePath:= GetApplicationPath + PATH_LINUX_APP_FOLDER + PATH_SETTINGS_FILE;
    GlobalSettings.LoadFromFile(UTF8ToSys( GetApplicationPath + PATH_LINUX_APP_FOLDER + PATH_SETTINGS_FILE));
    {$ELSE}
    FormSettings.SettingsFilePath := GetApplicationPath + PATH_SETTINGS_FILE;
    GlobalSettings.LoadFromFile(UTF8ToSys( GetApplicationPath + PATH_SETTINGS_FILE));
    {$ENDIF}

    RootNetworkNode := TreeViewNetwork.Items.AddChild(nil, STR_NETWORKTREE_ROOT);
    RootNetworkNode.ImageIndex := 35;
    RootNetworkNode.SelectedIndex := 35;
    UpdateUI;
    if GlobalSettings.ComPort.AutoConnectAtBoot then
     ActionToolsComConnect.Execute;
    ShownOnce := True
  end;

  {$IFDEF DEBUG_THREAD}
  FormThreadDebug.Show;
  {$ENDIF}
end;

procedure TFormOLCB_Commander.TimerComStartupTimer(Sender: TObject);
begin
  if GlobalSettings.General.AutoScanNetworkAtBoot then
     ActionOpenLCBCommandIdentifyIDGlobal.Execute;
  TimerComStartup.Enabled := False;
end;

procedure TFormOLCB_Commander.TreeViewNetworkAdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
//var
//  R: TRect;
begin
  PaintImages := True;
  DefaultDraw := True;
 // R := Node.DisplayRect(True);
//  Sender.Canvas.Brush.Color := clBlue;
//  Sender.Canvas.FillRect(R);
end;

procedure TFormOLCB_Commander.TreeViewNetworkCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
  NodeClass := TOlcbTreeNode;
end;

procedure TFormOLCB_Commander.TreeViewNetworkCustomCreateItem(Sender: TCustomTreeView; var ATreeNode: TTreenode);
begin
  ATreeNode := TOlcbTreeNode.Create(Sender.Items)
end;

procedure TFormOLCB_Commander.TreeViewNetworkSelectionChanged(Sender: TObject);
begin
  UpdateUI
end;

procedure TFormOLCB_Commander.ComDisconnect;
begin
  if Assigned(FComPortThread) then
  begin
    ComPortThread.Terminate;
    FComPortThread := nil;
  end;
  UpdateUI;
end;

function TFormOLCB_Commander.FindTreeNodeByAlias(AnAliasID: Word): TOlcbTreeNode;
var
  TreeNode: TTreeNode;
begin
 Result := nil;
 TreeNode := RootNetworkNode.GetFirstChild;
 while Assigned(TreeNode) and not Assigned(Result) do
 begin
   if TOlcbTreeNode( TreeNode).OlcbData.NodeIDAlias = AnAliasID then
     Result := TOlcbTreeNode( TreeNode);
   TreeNode := RootNetworkNode.GetNextChild(TreeNode);
 end;

end;

procedure TFormOLCB_Commander.SyncDatagramMemConfigAddressSpaceInfoReply(Datagram: TDatagramReceive);

  procedure LoadNode(ParentNode: TTreeNode; Description: string; IsTrue: Boolean);
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

  procedure LoadNodeSimple(ParentNode: TTreeNode; Description: string);
  var
     DataChild: TTreeNode;
  begin
    DataChild := TreeViewNetwork.Items.AddChild(ParentNode, Description);
    DataChild.ImageIndex := 40;
    DataChild.SelectedIndex := 40;
  end;

var
  Node: TOlcbTreeNode;
  MemProtocolChild, ConfigAddressSpacesChild, AddressSpaceChild: TTreeNode;
  AddressSpace: TOlcbMemAddressSpace;
  SpaceName: string;
begin
  TreeViewNetwork.BeginUpdate;
  try
    Node := FindTreeNodeByAlias(Datagram.DestinationAlias);
    if Assigned(Node) then
    begin
      MemProtocolChild := Node.FindNode(STR_MEM_PROTOCOL);
      if not Assigned(MemProtocolChild) then
        MemProtocolChild := TreeViewNetwork.Items.AddChild(Node, STR_MEM_PROTOCOL);
      MemProtocolChild.ImageIndex := 41;
      MemProtocolChild.SelectedIndex := 41;

      ConfigAddressSpacesChild := MemProtocolChild.FindNode(STR_CONFIGMEM_ADDRESS_SPACES);
      if not Assigned(ConfigAddressSpacesChild) then
        ConfigAddressSpacesChild := TreeViewNetwork.Items.AddChild(MemProtocolChild, STR_CONFIGMEM_ADDRESS_SPACES);
      ConfigAddressSpacesChild.ImageIndex := 41;
      ConfigAddressSpacesChild.SelectedIndex := 41;

      AddressSpace := Node.OlcbData.ConfigMem.AddAddressSpace(Datagram);
      SpaceName := ' 0x' + AddressSpace.SpaceAsHex + ' [' + IntToStr(AddressSpace.Space) + '] : ' + AddressSpaceToString(AddressSpace.Space);
      AddressSpaceChild := ConfigAddressSpacesChild.FindNode(SpaceName);
      if not Assigned(AddressSpaceChild) then
        AddressSpaceChild := TreeViewNetwork.Items.AddChild(ConfigAddressSpacesChild, SpaceName);
      AddressSpaceChild.ImageIndex := 41;
      AddressSpaceChild.SelectedIndex := 41;
      AddressSpaceChild.DeleteChildren;

      LoadNodeSimple(AddressSpaceChild, 'Hi Address: 0x' + IntToHex(AddressSpace.AddressHi, 4));
      LoadNodeSimple(AddressSpaceChild, 'Lo Address: 0x' + IntToHex(AddressSpace.AddressLo, 4));
      LoadNode(AddressSpaceChild, 'Implied Lo Address: ', AddressSpace.AddressLoImpliedZero);
      LoadNode(AddressSpaceChild, 'Is ReadOnly: ', AddressSpace.IsReadOnly);
      LoadNode(AddressSpaceChild, 'Is Present: ', AddressSpace.IsPresent);
      LoadNodeSimple(AddressSpaceChild, 'Description: ' + AddressSpace.Description);
    end;
  finally
    TreeViewNetwork.EndUpdate;
  end;
end;

procedure TFormOLCB_Commander.SyncDatagramMemConfigOperationReply(Datagram: TDatagramReceive);

  procedure LoadNode(ParentNode: TTreeNode; Description: string; IsTrue: Boolean);
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

  procedure LoadNodeSimple(ParentNode: TTreeNode; Description: string);
  var
     DataChild: TTreeNode;
  begin
    DataChild := TreeViewNetwork.Items.AddChild(ParentNode, Description);
    DataChild.ImageIndex := 40;
    DataChild.SelectedIndex := 40;
  end;

var
  Node: TOlcbTreeNode;
  ConfigChildRoot, ConfigOptionChild: TTreeNode;
begin
  TreeViewNetwork.BeginUpdate;
  try
    Node := FindTreeNodeByAlias(Datagram.DestinationAlias);
    if Assigned(Node) then
    begin
      ConfigChildRoot := Node.FindNode(STR_MEM_PROTOCOL);
      if Assigned(ConfigChildRoot) then
      begin
        ConfigOptionChild := ConfigChildRoot.FindNode(STR_CONFIGMEM_OPTIONS);
        if Assigned(ConfigOptionChild) then
          ConfigOptionChild.DeleteChildren
        else begin
          ConfigOptionChild := TreeViewNetwork.Items.AddChild(ConfigChildRoot, STR_CONFIGMEM_OPTIONS);
          ConfigOptionChild.ImageIndex := 41;
          ConfigOptionChild.SelectedIndex := 41;
        end
      end else
      begin
        ConfigChildRoot := TreeViewNetwork.Items.AddChild(Node, STR_MEM_PROTOCOL);
        ConfigChildRoot.ImageIndex := 41;
        ConfigChildRoot.SelectedIndex := 41;
        ConfigOptionChild := TreeViewNetwork.Items.AddChild(ConfigChildRoot, STR_CONFIGMEM_OPTIONS);
        ConfigOptionChild.ImageIndex := 41;
        ConfigOptionChild.SelectedIndex := 41;
      end;
      if Node.OlcbData.ConfigMem = nil then
        Node.OlcbData.ConfigMem := TOlcbMemConfig.Create;

      Node.OlcbData.ConfigMem.Options.LoadFromDatagram(Datagram);

      LoadNode(ConfigOptionChild, 'Write Under Mask = ', Node.OlcbData.ConfigMem.Options.WriteUnderMask);

      LoadNode(ConfigOptionChild, 'UnAligned Reads = ', Node.OlcbData.ConfigMem.Options.UnAlignedReads);
      LoadNode(ConfigOptionChild, 'UnAligned Writes = ', Node.OlcbData.ConfigMem.Options.UnalignedWrites);
      LoadNode(ConfigOptionChild, 'Read from Mfg ACDI = ', Node.OlcbData.ConfigMem.Options.ReadFromMfgACDI);
      LoadNode(ConfigOptionChild, 'Read from User ACDI = ', Node.OlcbData.ConfigMem.Options.ReadFromUserACDI);
      LoadNode(ConfigOptionChild, 'Write to User ACDI = ', Node.OlcbData.ConfigMem.Options.WriteToUserACDI);
      LoadNode(ConfigOptionChild, 'Write 1 Byte = ', Node.OlcbData.ConfigMem.Options.WriteOneByte);
      LoadNode(ConfigOptionChild, 'Write 2 Bytes = ', Node.OlcbData.ConfigMem.Options.WriteTwoBytes);
      LoadNode(ConfigOptionChild, 'Write 4 Bytes = ', Node.OlcbData.ConfigMem.Options.WriteFourBytes);
      LoadNode(ConfigOptionChild, 'Write 64 Bytes = ', Node.OlcbData.ConfigMem.Options.Write64Bytes);
      LoadNode(ConfigOptionChild, 'Write Arbitrary Bytes = ', Node.OlcbData.ConfigMem.Options.WriteArbitraryBytes);
      LoadNode(ConfigOptionChild, 'Write by Stream = ', Node.OlcbData.ConfigMem.Options.WriteStreamBytes);
      LoadNodeSimple(ConfigOptionChild, 'Address Space Hi: ' + IntToStr(Node.OlcbData.ConfigMem.Options.AddressSpaceHi));
      LoadNodeSimple(ConfigOptionChild, 'Address Space Lo: ' + IntToStr(Node.OlcbData.ConfigMem.Options.AddressSpaceLo));
      LoadNodeSimple(ConfigOptionChild, 'Description: ' + Node.OlcbData.ConfigMem.Options.Description);
    end;
  finally
    TreeViewNetwork.EndUpdate;
  end;
end;

procedure TFormOLCB_Commander.SyncReceiveMessage(MessageStr: String);
begin
  MessageHelper.Decompose(MessageStr);
  if FormMessageLog.Visible then
  begin
    FormMessageLog.SynMemo.Lines.BeginUpdate;
    FormMessageLog.SynMemo.Text := FormMessageLog.SynMemo.Text + MessageToDetailedMessage( MessageStr, False);
    FormMessageLog.SynMemo.CaretY := FormMessageLog.SynMemo.LineHeight * FormMessageLog.SynMemo.Lines.Count;
    FormMessageLog.SynMemo.Lines.EndUpdate;
  end;
  case MessageHelper.MTI of
    MTI_VERIFIED_NODE_ID_NUMBER   : ProcessNodeTree(MessageHelper, utr_Add);
    MTI_AMD                       : ProcessNodeTree(MessageHelper, utr_Add);
    MTI_AMR                       : ProcessNodeTree(MessageHelper, utr_Remove);
    MTI_PROTOCOL_SUPPORT_REPLY    : ProcessProtocolSupport(MessageHelper);
  end;
end;

procedure TFormOLCB_Commander.SyncSniiReply(Snii: TSnii);

  procedure AddSniiChild(ProtocolChild: TTreeNode; LabelStr: string);
  var
    Node: TTreeNode;
  begin
    Node := TreeViewNetwork.Items.AddChild(ProtocolChild, LabelStr);
    Node.ImageIndex := 40;
    Node.SelectedIndex := 40;
  end;

var
  ProtocolChild: TTreeNode;
  Node: TOlcbTreeNode;
begin
  if Assigned(Snii) then
  begin
    Node := FindTreeNodeByAlias(Snii.DestinationAlias);
    if Assigned(Node) then
    begin
      Node.OlcbData.Snii := Snii.Duplicate;                                     // Make a Copy for the Node
      ProtocolChild := Node.FindNode(STR_SNII);
      if Assigned(ProtocolChild) then
        ProtocolChild.DeleteChildren
      else
        ProtocolChild := TreeViewNetwork.Items.AddChild(Node, STR_SNII);
      ProtocolChild.ImageIndex := 41;
      ProtocolChild.SelectedIndex := 41;
      AddSniiChild(ProtocolChild, 'Content Version: ' + IntToStr(Snii.SniiMfgVersion));
      AddSniiChild(ProtocolChild, 'Mfg Name : ' + Snii.SniiMfgName);
      AddSniiChild(ProtocolChild, 'Mfg Model : ' + Snii.SniiMfgModel);
      AddSniiChild(ProtocolChild, 'Hardware Ver: ' + Snii.SniiHardwareVersion);
      AddSniiChild(ProtocolChild, 'Software Ver : ' + Snii.SniiSoftwareVersion);
      AddSniiChild(ProtocolChild, 'Content Version: ' + IntToStr(Snii.SniiMfgVersion));
      AddSniiChild(ProtocolChild, 'User Name : ' + Snii.SniiUserName);
      AddSniiChild(ProtocolChild, 'User Desc : ' + Snii.SniiUserDescription);
    end                                                               // No one to use it, throw it away
  end
end;

procedure TFormOLCB_Commander.SyncSendMessage(MessageStr: String);
begin
  if MessageHelper.Decompose(MessageStr) then
  begin
    if FormMessageLog.Visible then
    begin
      FormMessageLog.SynMemo.Lines.BeginUpdate;
      FormMessageLog.SynMemo.Text := FormMessageLog.SynMemo.Text + MessageToDetailedMessage( MessageStr, True);
      FormMessageLog.SynMemo.CaretY := FormMessageLog.SynMemo.LineHeight * FormMessageLog.SynMemo.Lines.Count;
      FormMessageLog.SynMemo.Lines.EndUpdate;
    end;
  end;
end;

procedure TFormOLCB_Commander.SyncErrorMessage(MessageStr: String);
begin
  if Assigned(ComPortThread) then
  begin
    ComPortThread.Terminate;
    ComPortThread := nil
  end;
  UpdateUI;
  ShowMessage(MessageStr);
end;

procedure TFormOLCB_Commander.SyncMessageLogHide;
begin
  ActionToolsMessageLogShow.Checked := False;
end;

procedure TFormOLCB_Commander.ProcessNodeTree(LocalHelper: TOpenLCBMessageHelper; Reason: TUpdateTreeReason);
var
  Node: TOlcbTreeNode;
begin
  TreeViewNetwork.BeginUpdate;
  try
    case Reason of
      utr_Add :
        begin
          Node := FindTreeNodeByAlias(LocalHelper.SourceAliasID);
          if not Assigned(Node) then
          begin
            Node := TreeViewNetwork.Items.AddChild(RootNetworkNode, '0x' + IntToHex(LocalHelper.SourceAliasID, 4)) as TOlcbTreeNode;
            Node.OlcbData.NodeIDAlias := LocalHelper.SourceAliasID;
            if LocalHelper.DataCount = 8 then
              Node.OlcbData.NodeID := LocalHelper.ExtractDataBytesAsInt(2, 8);    // Assumption this is the full ID in the databytes
          //  Node.;
            Node.ImageIndex := 34;
            Node.SelectedIndex := 34;;
          end
        end;
      utr_Remove :
        begin
          Node := FindTreeNodeByAlias(LocalHelper.SourceAliasID);
          if Assigned(Node) then
            TreeViewNetwork.Items.Delete(Node);
        end;
    end
  finally
    TreeViewNetwork.EndUpdate;
    UpdateUI
  end
end;

procedure TFormOLCB_Commander.ProcessProtocolSupport(LocalHelper: TOpenLCBMessageHelper);
var
  ProtocolChild: TTreeNode;
  Node: TOlcbTreeNode;
  Mask, Protocols: QWord;
  i: Integer;
begin
  Node := FindTreeNodeByAlias(LocalHelper.SourceAliasID);
  if Assigned(Node) then
  begin
    TreeViewNetwork.BeginUpdate;
    try
      ProtocolChild := Node.FindNode(STR_PROTOCOLSUPPORT);
      if Assigned(ProtocolChild) then
        ProtocolChild.DeleteChildren
      else
        ProtocolChild := TreeViewNetwork.Items.AddChild(Node, STR_PROTOCOLSUPPORT);
      ProtocolChild.ImageIndex := 41;
      ProtocolChild.SelectedIndex := 41;
      Protocols := LocalHelper.ExtractDataBytesAsInt(2, 7);    // First 2 are the destination alias
      Node.OlcbData.ProtocolSupport := Protocols;
      Mask := $800000000000;
      for i := 0 to 47 do
      begin
        if Protocols and Mask <> 0 then
        begin
          Node := TreeViewNetwork.Items.AddChild(ProtocolChild, ProtocolSupportReplyToString( Mask)) as TOlcbTreeNode;
          Node.ImageIndex := 40;
          Node.SelectedIndex := 40;
        end;
        Mask := Mask shr 1;
      end
    finally
      TreeViewNetwork.EndUpdate;
    end;
  end
end;

procedure TFormOLCB_Commander.UpdateUI;
begin
  ActionToolsComConnect.Enabled := not Assigned(FComPortThread);
  ActionToolsComDisconnect.Enabled := Assigned(FComPortThread);

  ActionOpenLCBCommandIdentifyIDGlobal.Enabled := Assigned(FComPortThread);
  ActionOpenLCBCommandProtocolSupport.Enabled := (TreeViewNetwork.SelectionCount > 0);
  ActionOpenLCBCommandMemConfigOptions.Enabled := (TreeViewNetwork.SelectionCount > 0);
  ActionOpenLCBCommandMemConfigSpaces.Enabled:= (TreeViewNetwork.SelectionCount > 0);
  ActionOpenLCBCommandSNII.Enabled := (TreeViewNetwork.SelectionCount > 0);
  ActionOpenLCBCommandAll.Enabled := (RootNetworkNode.Count > 0);

  LabelNetworkNodeCountValue.Caption := IntToStr(RootNetworkNode.Count);
end;


end.
