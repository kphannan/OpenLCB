unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, ActnList, ComCtrls, ExtCtrls, Buttons, serialport_thread, datagram,
  olcb_app_common_settings, file_utilities, form_settings, form_about, lcltype, types,
  olcb_utilities, olcb_defines, form_messagelog, snii;

const
  BUNDLENAME = 'OpenLCB Commander';
  STR_NETWORKTREE_ROOT = 'Network Tree';
  STR_PROTOCOLSUPPORT = 'Supported Protocols';
  STR_SNII            = 'Simple Node Identification (SNIP/SNII)';

type
  TUpdateTreeReason = (
    utr_Add,
    utr_Remove
  );

type

  { TFormOLCB_Commander }

  TFormOLCB_Commander = class(TForm)
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
    BitBtnNetworkRefresh: TBitBtn;
    BitBtnNetworkRefresh1: TBitBtn;
    BitBtnNetworkRefresh2: TBitBtn;
    BitBtnNetworkRefresh3: TBitBtn;
    ImageListMainSmall: TImageList;
    LabelNetworkNodeCount: TLabel;
    LabelNetworkNodeCountValue: TLabel;
    MainMenu: TMainMenu;
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
    SplitterMain: TSplitter;
    TabSheetNetwork: TTabSheet;
    TabSheetTrain: TTabSheet;
    TreeViewNetwork: TTreeView;
    procedure ActionHelpAboutShowExecute(Sender: TObject);
    procedure ActionOpenLCBCommandAllExecute(Sender: TObject);
    procedure ActionOpenLCBCommandIdentifyIDGlobalExecute(Sender: TObject);
    procedure ActionOpenLCBCommandProtocolSupportExecute(Sender: TObject);
    procedure ActionOpenLCBCommandSNIIExecute(Sender: TObject);
    procedure ActionToolsComConnectExecute(Sender: TObject);
    procedure ActionToolsComDisconnectExecute(Sender: TObject);
    procedure ActionToolsMessageLogShowExecute(Sender: TObject);
    procedure ActionToolsPreferenceShowMacExecute(Sender: TObject);
    procedure ActionToolsSettingsShowWinExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TreeViewNetworkChange(Sender: TObject; Node: TTreeNode);
    procedure TreeViewNetworkChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
    procedure TreeViewNetworkSelectionChanged(Sender: TObject);
  private
    FComPortThread: TComPortThread;
    FDatagramManager: TDatagramReceiveManager;
    FMessageHelper: TOpenLCBMessageHelper;
    {$IFDEF DARWIN}
    FOSXMenu: TMenuItem;
    FOSXSep1Cmd: TMenuItem;
    FOSXPrefCmd: TMenuItem;
    {$ENDIF}
    FAppAboutCmd: TMenuItem;
    FRootNetworkNode: TTreeNode;
    FShownOnce: Boolean;
    FSniiManager: TSniiReceiveManager;
    { private declarations }
  protected
    procedure ComDisconnect;
    procedure SyncReceiveMessage(MessageStr: String);
    procedure SyncSendMessage(MessageStr: String);
    procedure SyncErrorMessage(MessageStr: String);
    procedure SyncMessageLogHide;
    procedure UpdateNodeTree(LocalHelper: TOpenLCBMessageHelper; Reason: TUpdateTreeReason);
    procedure UpdateProtocolSupport(LocalHelper: TOpenLCBMessageHelper);
    procedure UpdateSNII(LocalHelper: TOpenLCBMessageHelper);
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
    property DatagramManager: TDatagramReceiveManager read FDatagramManager write FDatagramManager;
    property SniiManager: TSniiReceiveManager read FSniiManager write FSniiManager;
  public
    { public declarations }
  end;

var
  FormOLCB_Commander: TFormOLCB_Commander;

implementation

{$R *.lfm}

{ TFormOLCB_Commander }

procedure TFormOLCB_Commander.ActionToolsSettingsShowWinExecute(Sender: TObject);
begin
  FormSettings.ShowModal;
end;

procedure TFormOLCB_Commander.ActionHelpAboutShowExecute(Sender: TObject);
begin
  FormAbout.ShowModal
end;

procedure TFormOLCB_Commander.ActionOpenLCBCommandAllExecute(Sender: TObject);
var
  Node: TTreeNode;
  i: Integer;
begin
  Node := RootNetworkNode.GetFirstChild;
  while Assigned(Node) do
  begin
    if GlobalSettings.General.SendPacketDelay > 0 then Sleep(GlobalSettings.General.SendPacketDelay);
    MessageHelper.Load(ol_OpenLCB, MTI_PROTOCOL_SUPPORT_INQUIRY, GlobalSettings.General.AliasIDAsVal, StrToInt( Node.Text), 2, 0, 0, 0, 0 ,0 ,0 ,0 ,0);
    ComPortThread.Add(MessageHelper.Encode);
    if GlobalSettings.General.SendPacketDelay > 0 then Sleep(GlobalSettings.General.SendPacketDelay);
    MessageHelper.Load(ol_OpenLCB, MTI_SIMPLE_NODE_INFO_REQUEST, GlobalSettings.General.AliasIDAsVal, StrToInt( Node.Text), 2, 0, 0, 0, 0 ,0 ,0 ,0 ,0);
    ComPortThread.Add(MessageHelper.Encode);
    Node := RootNetworkNode.GetNextChild(Node);
  end;
end;

procedure TFormOLCB_Commander.ActionOpenLCBCommandIdentifyIDGlobalExecute(Sender: TObject);
var
  i: Integer;
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

procedure TFormOLCB_Commander.ActionOpenLCBCommandProtocolSupportExecute(Sender: TObject);
var
  Node: TTreeNode;
  i: Integer;
begin
  for i := 0 to TreeViewNetwork.SelectionCount - 1 do
  begin
    if GlobalSettings.General.SendPacketDelay > 0 then Sleep(GlobalSettings.General.SendPacketDelay);
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
    if GlobalSettings.General.SendPacketDelay > 0 then Sleep(GlobalSettings.General.SendPacketDelay);
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
     ComPortThread.Port := 'dev/' + GlobalSettings.ComPort.Port;
     {$ELSE}
     ComPortThread.Port := '/dev/' + GlobalSettings.ComPort.Port;
     {$ENDIF}
   {$ENDIF}
   ComPortThread.BaudRate := GlobalSettings.ComPort.BaudRate;

   ComPortThread.SyncReceiveMessageFunc := @SyncReceiveMessage;
   ComPortThread.SyncSendMessageFunc := @SyncSendMessage;
   ComPortThread.SyncErrorMessageFunc := @SyncErrorMessage;
   DatagramManager := TDatagramReceiveManager.Create(GlobalSettings.General.AliasIDAsVal);
   SniiManager := TSniiReceiveManager.Create(GlobalSettings.General.AliasIDAsVal);
   ComPortThread.Suspended := False;
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
    GlobalSettings.LoadFromFile(UTF8ToSys( GetApplicationPath + 'settings.ini'));
    RootNetworkNode := TreeViewNetwork.Items.AddChild(nil, STR_NETWORKTREE_ROOT);
    RootNetworkNode.ImageIndex := 35;
    RootNetworkNode.SelectedIndex := 35;;
    UpdateUI;

    ShownOnce := True
  end;
end;

procedure TFormOLCB_Commander.TreeViewNetworkChange(Sender: TObject; Node: TTreeNode);
begin

end;

procedure TFormOLCB_Commander.TreeViewNetworkChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
begin

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
  FreeAndNil(FDatagramManager);
  FreeAndNil(FSniiManager);
  UpdateUI;
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
      MTI_VERIFY_NODE_ID_NUMBER, MTI_VERIFIED_NODE_ID_NUMBER : UpdateNodeTree(MessageHelper, utr_Add);
      MTI_AMD                                                : UpdateNodeTree(MessageHelper, utr_Add);
      MTI_AMR                                                : UpdateNodeTree(MessageHelper, utr_Remove);
      MTI_PROTOCOL_SUPPORT_REPLY                             : UpdateProtocolSupport(MessageHelper);
      MTI_SIMPLE_NODE_INFO_REPLY                             : UpdateSNII(MessageHelper);
    end;
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

procedure TFormOLCB_Commander.UpdateNodeTree(LocalHelper: TOpenLCBMessageHelper; Reason: TUpdateTreeReason);
var
  Node: TTreeNode;
begin
  TreeViewNetwork.BeginUpdate;
  try
    case Reason of
      utr_Add :
        begin
          Node := TreeViewNetwork.Items.FindNodeWithText('0x' + IntToHex(LocalHelper.SourceAliasID, 4));
          if not Assigned(Node) then
          begin
            Node := TreeViewNetwork.Items.AddChild(RootNetworkNode, '0x' + IntToHex(LocalHelper.SourceAliasID, 4));
            Node.ImageIndex := 34;
            Node.SelectedIndex := 34;;
          end
        end;
      utr_Remove :
        begin
          Node := TreeViewNetwork.Items.FindNodeWithText('0x' + IntToHex(LocalHelper.SourceAliasID, 4));
          if Assigned(Node) then
            TreeViewNetwork.Items.Delete(Node);
        end;
    end
  finally
    TreeViewNetwork.EndUpdate;
    UpdateUI
  end
end;

procedure TFormOLCB_Commander.UpdateProtocolSupport(LocalHelper: TOpenLCBMessageHelper);
var
  Node, ProtocolChild: TTreeNode;
  Mask, Protocols: QWord;
  i: Integer;
begin
  Node := TreeViewNetwork.Items.FindNodeWithText('0x' + IntToHex(LocalHelper.SourceAliasID, 4));
  if Assigned(Node) then
  begin
    ProtocolChild := Node.FindNode(STR_PROTOCOLSUPPORT);
    if Assigned(ProtocolChild) then
      ProtocolChild.DeleteChildren
    else
      ProtocolChild := TreeViewNetwork.Items.AddChild(Node, STR_PROTOCOLSUPPORT);
    ProtocolChild.ImageIndex := 41;
    ProtocolChild.SelectedIndex := 41;;
    Protocols := LocalHelper.ExtractDataBytesAsInt(2, 7);    // First 2 are the destination alias );
    Mask := $800000000000;
    for i := 0 to 47 do
    begin
      if Protocols and Mask <> 0 then
      begin
        Node := TreeViewNetwork.Items.AddChild(ProtocolChild, ProtocolSupportReplyToString( Mask));
        Node.ImageIndex := 40;
        Node.SelectedIndex := 40;
      end;
      Mask := Mask shr 1;
    end
  end;
end;

procedure TFormOLCB_Commander.UpdateSNII(LocalHelper: TOpenLCBMessageHelper);
var
  Snii: TSnii;
  Node, ProtocolChild: TTreeNode;
begin
  Snii := SniiManager.Process(LocalHelper);
  if Assigned(Snii) then
  begin
    Node := TreeViewNetwork.Items.FindNodeWithText('0x' + IntToHex(LocalHelper.SourceAliasID, 4));
    if Assigned(Node) then
    begin
      ProtocolChild := Node.FindNode(STR_SNII);
      if Assigned(ProtocolChild) then
        ProtocolChild.DeleteChildren
      else
        ProtocolChild := TreeViewNetwork.Items.AddChild(Node, STR_SNII);
      ProtocolChild.ImageIndex := 41;
      ProtocolChild.SelectedIndex := 41;
      Node := TreeViewNetwork.Items.AddChild(ProtocolChild, 'Content Version: ' + IntToStr(Snii.SniiMfgVersion));
      Node.ImageIndex := 40;
      Node.SelectedIndex := 40;
      Node := TreeViewNetwork.Items.AddChild(ProtocolChild, 'Mfg Name : ' + Snii.SniiMfgName);
      Node.ImageIndex := 40;
      Node.SelectedIndex := 40;
      Node := TreeViewNetwork.Items.AddChild(ProtocolChild, 'Mfg Model : ' + Snii.SniiMfgModel);
      Node.ImageIndex := 40;
      Node.SelectedIndex := 40;
      Node := TreeViewNetwork.Items.AddChild(ProtocolChild, 'Hardware Ver: ' + Snii.SniiHardwareVersion);
      Node.ImageIndex := 40;
      Node.SelectedIndex := 40;
      Node := TreeViewNetwork.Items.AddChild(ProtocolChild, 'Software Ver : ' + Snii.SniiSoftwareVersion);
      Node.ImageIndex := 40;
      Node.SelectedIndex := 40;
      Node := TreeViewNetwork.Items.AddChild(ProtocolChild, 'Content Version: ' + IntToStr(Snii.SniiMfgVersion));
      Node.ImageIndex := 40;
      Node.SelectedIndex := 40;
      Node := TreeViewNetwork.Items.AddChild(ProtocolChild, 'User Name : ' + Snii.SniiUserName);
      Node.ImageIndex := 40;
      Node.SelectedIndex := 40;
      Node := TreeViewNetwork.Items.AddChild(ProtocolChild, 'User Desc : ' + Snii.SniiUserDescription);
      Node.ImageIndex := 40;
      Node.SelectedIndex := 40;
    end;
  end;
end;

procedure TFormOLCB_Commander.UpdateUI;
begin
  ActionToolsComConnect.Enabled := not Assigned(FComPortThread);
  ActionToolsComDisconnect.Enabled := Assigned(FComPortThread);

  ActionOpenLCBCommandIdentifyIDGlobal.Enabled := Assigned(FComPortThread);
  ActionOpenLCBCommandProtocolSupport.Enabled := (TreeViewNetwork.SelectionCount > 0);
  ActionOpenLCBCommandSNII.Enabled := (TreeViewNetwork.SelectionCount > 0);
  ActionOpenLCBCommandAll.Enabled := (RootNetworkNode.Count > 0);

  LabelNetworkNodeCountValue.Caption := IntToStr(RootNetworkNode.Count);
end;


end.
