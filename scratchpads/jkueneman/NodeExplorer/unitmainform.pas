unit UnitMainForm;

// ******************************************************************************
//
// * Copyright:
//     (c) Mustangpeak Software 2012.
//
//     The contents of this file are subject to the GNU GPL v3 licence/ you maynot use
//     this file except in compliance with the License. You may obtain a copy of the
//     License at http://www.gnu.org/licenses/gpl.html
//
// * Revision History:
//     2012-08-05:   Created
//
// * Description:

//
// *****************************************************************************

{$mode objfpc}{$H+}

{.$DEFINE DISABLE_UI_UPDATE}
{.$DEFINE USE_DEBUG_LOGGER}

interface

uses
  Classes, SysUtils, FileUtil, SynHighlighterXML, SynEdit, RTTICtrls, Forms,
  Controls, Graphics, Dialogs, ComCtrls, StdCtrls, ActnList, Menus, ExtCtrls,
  synaser, lcltype, Buttons, unitlogwindow,
  unitsettings, DOM, XMLRead, XMLWrite, serialport_thread, olcb_testmatrix,
  {$IFDEF UNIX}
  unitLinuxFTDI,
  {$ENDIF}
  nodeexplorer_settings, olcb_utilities, unitolcb_defines,
  types, SynEditKeyCmds, unitAbout, unitrestartnodeform, unitrawmessagelog;


const
  BUNDLENAME = 'NodeExplorer';
  GROUPINDEX_CONNECTION_PORTS = 100;          // Way to identify Menu items with a Tag value set to i + OFFSET_CONNECTION_PORTS

type

  { TFormMain }

  TFormMain = class(TForm)
    ActionRawLogClear: TAction;
    ActionRawLogSelectAll: TAction;
    ActionRawLogPaste: TAction;
    ActionRawLogMemoCopy: TAction;
    ActionRawLogMemoCut: TAction;
    ActionRawLogShowGutter: TAction;
    ActionRawMessageLogShow: TAction;
    ActionExecuteTests: TAction;
    ActionCancelTests: TAction;
    ActionTerminateCurrentTest: TAction;
    ActionShowAbout: TAction;
    ActionLogMemoSelectAll: TAction;
    ActionLogMemoPaste: TAction;
    ActionLogMemoCopy: TAction;
    ActionLogMemoCut: TAction;
    ActionMemConfig: TAction;
    ActionReadPip: TAction;
    ActionLogShowGutter: TAction;
    ActionSaveTestMatrix: TAction;
    ActionLoadTestMatrix: TAction;
    ActionRescanPorts: TAction;
    ActionShowOptionsWin: TAction;
    ActionShowPreferencesMac: TAction;
    ActionDiscoverNode: TAction;
    ActionHideLog: TAction;
    ActionShowLog: TAction;
    ActionEventReader: TAction;
    ActionReadXML: TAction;
    ActionSendDatagramReply: TAction;
    ActionConnect: TAction;
    ActionSendPacket: TAction;
    ActionLogClear: TAction;
    ActionListMain: TActionList;
    ApplicationProperties1: TApplicationProperties;
    ButtonConnect: TButton;
    ButtonDiscoverNodes: TButton;
    ButtonExecuteTests: TButton;
    ButtonExecuteTests1: TButton;
    ButtonExecuteTests2: TButton;
    ButtonStopCurrenteTest: TButton;
    ButtonReadPIP: TButton;
    ButtonSaveTests: TButton;
    ButtonShowLog: TButton;
    ButtonRescanPorts: TButton;
    ButtonLoadTests: TButton;
    ButtonSendPacket: TButton;
    ButtonReadXML: TButton;
    ButtonReadEvents: TButton;
    ButtonSendDatagramReply: TButton;
    CheckBoxMemConfigReadOnly: TCheckBox;
    CheckBoxMemConfigImplied: TCheckBox;
    CheckBoxMemConfigPresent: TCheckBox;
    CheckGroupConfigMem: TCheckGroup;
    CheckGroupConfigMemWriteLen: TCheckGroup;
    CheckGroupPIP: TCheckGroup;
    ComboBoxBaud: TComboBox;
    ComboBoxPorts: TComboBox;
    EditMemConfgLowAddress: TEdit;
    EditMemConfigHiAddress: TEdit;
    EditPipRawMessage: TEdit;
    EditCustomBaudRate: TEdit;
    EditDiscoverNodeAlias: TEdit;
    EditDiscoverNodeID: TEdit;
    EditPacket: TEdit;
    EditSourceNodeAlias: TEdit;
    EditTargetNodeAlias: TEdit;
    GroupBoxConfigMemOptions: TGroupBox;
    GroupBoxComPort: TGroupBox;
    GroupBoxEventReaderConsumers: TGroupBox;
    GroupBoxEventReaderProducers: TGroupBox;
    ImageListSmall: TImageList;
    ImageOpenLCB: TImage;
    LabeTestlProgress: TLabel;
    LabelMemConfigHiAddress: TLabel;
    LabelMemConfigLowAddress: TLabel;
    LabelDiscoverMultiNode: TLabel;
    LabelPipPassFail: TLabel;
    LabelHomeMessage1: TLabel;
    LabelHomeMessage2: TLabel;
    LabelPipRawMessage: TLabel;
    LabelHomeMessage: TLabel;
    LabelHomeMessageNote: TLabel;
    LabelDiscoverNodeAlias: TLabel;
    LabelDiscoverNodeID: TLabel;
    LabelTargetAlias: TLabel;
    LabelSourceAlias: TLabel;
    LabelPacket: TLabel;
    LabelBaud: TLabel;
    LabelCustomBaud: TLabel;
    LabelPort: TLabel;
    ListViewConfigMem: TListView;
    ListViewNodeDiscovery: TListView;
    ListViewTestMatrix: TListView;
    ListViewConsumers: TListView;
    ListViewProducers: TListView;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItemRawMessageLog: TMenuItem;
    MenuItemToolsSeparator1: TMenuItem;
    MenuItemToolsDiscoverNodes: TMenuItem;
    MenuItemToolStopCurrentTest: TMenuItem;
    MenuItemToolsTerminateAllTests: TMenuItem;
    MenuItemToolsTerminateCurrentTest: TMenuItem;
    MenuItemToolsSeparatorWin: TMenuItem;
    MenuItemOptionsWin: TMenuItem;
    MenuItemTools: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemConnectionSeparator2: TMenuItem;
    MenuItemConnectionRescan: TMenuItem;
    MenuItemConnectionSeparator1: TMenuItem;
    MenuItemConnect: TMenuItem;
    MenuItemConnection: TMenuItem;
    MenuItemLoadTestMatrix: TMenuItem;
    MenuItemShowLog: TMenuItem;
    MenuItemWindow: TMenuItem;
    MenuItemFile: TMenuItem;
    OpenDialog: TOpenDialog;
    PageControl: TPageControl;
    PanelLogo: TPanel;
    ProgressBarTest: TProgressBar;
    SaveDialog: TSaveDialog;
    SynEditCDI: TSynEdit;
    SynXMLSyn: TSynXMLSyn;
    TabSheetMemConfiguration: TTabSheet;
    TabSheetProtocolID: TTabSheet;
    TabSheetVerification: TTabSheet;
    TabSheetDiscover: TTabSheet;
    TabSheetEventReader: TTabSheet;
    TabSheetHome: TTabSheet;
    TabSheetCustom: TTabSheet;
    TabSheetCDIReader: TTabSheet;
    procedure ActionLogClearExecute(Sender: TObject);
    procedure ActionConnectExecute(Sender: TObject);
    procedure ActionDiscoverNodeExecute(Sender: TObject);
    procedure ActionEventReaderExecute(Sender: TObject);
    procedure ActionExecuteTestsExecute(Sender: TObject);
    procedure ActionHideLogExecute(Sender: TObject);
    procedure ActionLoadTestMatrixExecute(Sender: TObject);
    procedure ActionLogMemoCopyExecute(Sender: TObject);
    procedure ActionLogMemoCutExecute(Sender: TObject);
    procedure ActionLogMemoPasteExecute(Sender: TObject);
    procedure ActionLogMemoSelectAllExecute(Sender: TObject);
    procedure ActionLogShowGutterExecute(Sender: TObject);
    procedure ActionMemConfigExecute(Sender: TObject);
    procedure ActionRawLogClearExecute(Sender: TObject);
    procedure ActionRawLogShowGutterExecute(Sender: TObject);
    procedure ActionRawLogMemoCopyExecute(Sender: TObject);
    procedure ActionRawLogMemoCutExecute(Sender: TObject);
    procedure ActionRawLogPasteExecute(Sender: TObject);
    procedure ActionRawLogSelectAllExecute(Sender: TObject);
    procedure ActionRawMessageLogShowExecute(Sender: TObject);
    procedure ActionReadPipExecute(Sender: TObject);
    procedure ActionReadXMLExecute(Sender: TObject);
    procedure ActionRescanPortsExecute(Sender: TObject);
    procedure ActionSaveTestMatrixExecute(Sender: TObject);
    procedure ActionSendDatagramReplyExecute(Sender: TObject);
    procedure ActionSendPacketExecute(Sender: TObject);
    procedure ActionShowAboutExecute(Sender: TObject);
    procedure ActionShowLogExecute(Sender: TObject);
    procedure ActionShowOptionsWinExecute(Sender: TObject);
    procedure ActionShowPreferencesMacExecute(Sender: TObject);
    procedure ActionStopAllTestsExecute(Sender: TObject);
    procedure ActionCancelTestsExecute(Sender: TObject);
    procedure ComboBoxBaudChange(Sender: TObject);
    procedure ComboBoxPortsChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListViewNodeDiscoverySelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure ListViewTestMatrixDeletion(Sender: TObject; Item: TListItem);
    procedure ListViewTestMatrixDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ListViewTestMatrixDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ListViewTestMatrixStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure MenuItemConnectionClick(Sender: TObject);
    procedure MenuItemOptionsWinClick(Sender: TObject);
    procedure SynEditCDIKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
    FShownOnce: Boolean;
    FTargetAliasInPeril: Word;
    FTestThread: TComPortThread;
    FTestStrings: TStringList;
    FXMLDocTestMatrix: TXMLDocument;
    FXMLDocTestResults: TXMLDocument;
    function GetConnected: Boolean;
  protected
    { protected declarations }
    {$IFDEF DARWIN}
      AppMenu     : TMenuItem;
      AppSep1Cmd  : TMenuItem;
      AppPrefCmd  : TMenuItem;
    {$ENDIF}
    AppAboutCmd : TMenuItem;
    procedure UpdateUI;
    procedure ClearTestResultsXML;
    property Connected: Boolean read GetConnected;
    procedure LoadTestMatrixListview;
    procedure MenuItemSelectPortClick(Sender: TObject);
    procedure ResetTestVerificationIcons;
    procedure SyncHideNodeResetMessage(Test: TTestBase);
    procedure SyncShowNodeResetMessage(Test: TTestBase);
    procedure SyncDiscoverNodes(Test: TTestBase);
    procedure SyncReadPIP(Test: TTestBase);
    procedure SyncVerificationTests(Test: TTestBase);
    procedure SyncTargetAliasChanged(Test: TTestBase);
    procedure SyncTargetAliasChanging(Test: TTestBase);
    procedure SyncUpdateProgressBar(Test: TTestBase);
    procedure SyncRawMessage(MessageStr: String);
    procedure CallbackRawMessageLogHiding;
    property TargetAliasInPeril: Word read FTargetAliasInPeril write FTargetAliasInPeril;
  public
    { public declarations }
    procedure Log(XML: TXMLDocument);
    property ShownOnce: Boolean read FShownOnce write FShownOnce;
    property TestStrings: TStringList read FTestStrings write FTestStrings;
    property TestThread: TComPortThread read FTestThread write FTestThread;
    property XMLDocTestMatrix: TXMLDocument read FXMLDocTestMatrix write FXMLDocTestMatrix;
    property XMLDocTestResults: TXMLDocument read FXMLDocTestResults write FXMLDocTestResults;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{$ifdef Windows}
uses Windows;
{$endif}
{$IFDEF Darwin}
uses
  MacOSAll;
{$ENDIF}

{ TFormMain }

procedure TFormMain.ActionLogClearExecute(Sender: TObject);
begin
  FormLog.SynMemo.Lines.BeginUpdate;
  try
    FormLog.SynMemo.Lines.Clear
  finally
    FormLog.SynMemo.Lines.EndUpdate
  end;
end;

procedure TFormMain.ActionConnectExecute(Sender: TObject);
begin
  if Connected then
  begin
    TestThread.Terminate;
    FTestThread := nil;
    ActionConnect.Caption:='Connect';
  end else
  begin
    FTestThread := TComPortThread.Create(True);
    try
      TestThread.FreeOnTerminate := True;
      {$IFDEF MSWINDOWS}
      TestThread.Port := ComboBoxPorts.Text;
      {$ENDIF}
      {$IFDEF DARWIN}
      TestThread.Port := 'dev/' + ComboBoxPorts.Text;
      {$ELSE}
      TestThread.Port := 'dev/' + ComboBoxPorts.Text;
      {$ENDIF}
      if ComboBoxBaud.ItemIndex = 0 then
        TestThread.BaudRate := StrToInt(EditCustomBaudRate.Text)
      else
        TestThread.BaudRate := StrToInt(ComboBoxBaud.Items[ComboBoxBaud.ItemIndex]);
      TestThread.EnableRawMessages := ActionRawMessageLogShow.Checked;
      TestThread.SyncRawMessageFunc := @SyncRawMessage;
      TestThread.Suspended := False;
      Sleep(500);
      if TestThread.Connected then
        ActionConnect.Caption:='Disconnect'
      else begin
     //   ShowMessage(TestThread.LastErrorDesc);
        TestThread.Terminate;
        TestThread := nil;
      end;
    except
      if Assigned(TestThread) then
      begin
        TestThread.Terminate;
        TestThread := nil;
      end;
      ActionConnect.Caption:='Connect';
    end;
  end;
  UpdateUI;
end;

procedure TFormMain.ActionDiscoverNodeExecute(Sender: TObject);
var
  Test: TTestGetNodesUnderTest;
begin
  // WARNING THIS DEPENDS ON XMLDocTestMatrix STAYING VALID UNTIL THE TEST IS COMPLETE
  Test := FindTestFromXML(XMLDocTestMatrix, STR_TEST_GET_NODES_UNDER_TEST_CLASS) as TTestGetNodesUnderTest;
  if Assigned(Test) then
  begin
    Test.FreeOnLog := True;
    Test.SyncTestCompleteFunc := @SyncDiscoverNodes;
    TestThread.Add(Test);
  end;
end;

procedure TFormMain.ActionEventReaderExecute(Sender: TObject);
begin
   ShowMessage('Not implemented yet');
end;

procedure TFormMain.ActionExecuteTestsExecute(Sender: TObject);
var
  i: Integer;
begin
  ClearTestResultsXML;
  ResetTestVerificationIcons;
  {$IFNDEF DISABLE_UI_UPDATE}
  if ListViewNodeDiscovery.SelCount = 1 then
  {$ENDIF}
  begin
     for i := 0 to ListViewTestMatrix.Items.Count - 1 do
    begin
      if ListViewTestMatrix.Items[i].Checked then
        TestThread.Add( TTestBase( ListViewTestMatrix.Items[i].Data));
    end;
  end {$IFNDEF DISABLE_UI_UPDATE} else
    ShowMessage('No node is selected to test. Run "Discover Nodes" in the "Discovery" tab and select the node to test');
  UpdateUI;   {$ENDIF}
end;

procedure TFormMain.ActionHideLogExecute(Sender: TObject);
begin
  FormLog.Hide;
end;

procedure TFormMain.ActionLoadTestMatrixExecute(Sender: TObject);
begin
  OpenDialog.DefaultExt := '*.xml';
  OpenDialog.Filter := 'XML Files|*.xml';
  OpenDialog.Options := [ofFileMustExist];
  if OpenDialog.Execute then
  begin
    if FileExistsUTF8(OpenDialog.FileName) then
    begin
      ReadXMLFile(FXMLDocTestMatrix, UTF8ToSys(OpenDialog.FileName));
      LoadTestMatrixListview;
    end;
  end;
end;

procedure TFormMain.ActionLogMemoCopyExecute(Sender: TObject);
begin
  FormLog.SynMemo.CommandProcessor(TSynEditorCommand(ecCopy), ' ', nil);
end;

procedure TFormMain.ActionLogMemoCutExecute(Sender: TObject);
begin
  FormLog.SynMemo.CommandProcessor(TSynEditorCommand(ecCut), ' ', nil);
end;

procedure TFormMain.ActionLogMemoPasteExecute(Sender: TObject);
begin
  FormLog.SynMemo.CommandProcessor(TSynEditorCommand(ecPaste), ' ', nil);
end;

procedure TFormMain.ActionLogMemoSelectAllExecute(Sender: TObject);
begin
  FormLog.SynMemo.SelectAll;
end;

procedure TFormMain.ActionLogShowGutterExecute(Sender: TObject);
begin
  FormLog.SynMemo.Gutter.Visible := FormLog.CheckBoxShowGutter.Checked;
end;

procedure TFormMain.ActionMemConfigExecute(Sender: TObject);
begin

end;

procedure TFormMain.ActionRawLogClearExecute(Sender: TObject);
begin
    FormRawMessageLog.SynMemo.Lines.BeginUpdate;
  try
    FormRawMessageLog.SynMemo.Lines.Clear
  finally
    FormRawMessageLog.SynMemo.Lines.EndUpdate
  end;
end;

procedure TFormMain.ActionRawLogShowGutterExecute(Sender: TObject);
begin
  FormRawMessageLog.SynMemo.Gutter.Visible := FormRawMessageLog.CheckBoxShowGutter.Checked;
end;

procedure TFormMain.ActionRawLogMemoCopyExecute(Sender: TObject);
begin
  FormRawMessageLog.SynMemo.CommandProcessor(TSynEditorCommand(ecCopy), ' ', nil);
end;

procedure TFormMain.ActionRawLogMemoCutExecute(Sender: TObject);
begin
  FormRawMessageLog.SynMemo.CommandProcessor(TSynEditorCommand(ecCut), ' ', nil);
end;

procedure TFormMain.ActionRawLogPasteExecute(Sender: TObject);
begin
  FormRawMessageLog.SynMemo.CommandProcessor(TSynEditorCommand(ecPaste), ' ', nil);
end;

procedure TFormMain.ActionRawLogSelectAllExecute(Sender: TObject);
begin
  FormRawMessageLog.SynMemo.SelectAll;
end;

procedure TFormMain.ActionRawMessageLogShowExecute(Sender: TObject);
begin
  FormRawMessageLog.Left := Left;
  FormRawMessageLog.Top := Top + Height + 20;
  FormRawMessageLog.Height := 160;
  FormRawMessageLog.Width := Width;
  FormRawMessageLog.Show;
  ActionRawMessageLogShow.Checked := True;
  if Assigned(TestThread) then
    TestThread.EnableRawMessages := True;
end;

procedure TFormMain.ActionReadPipExecute(Sender: TObject);
var
  Test: TTestProtocolSupport;
begin
  // WARNING THIS DEPENDS ON XMLDocTestMatrix STAYING VALID UNTIL THE TEST IS COMPLETE
  Test := FindTestFromXML(XMLDocTestMatrix, STR_PROTOCOL_IDENTIFICATION_PROTOCOL_CLASS) as TTestProtocolSupport;
  if Assigned(Test) then
  begin
    Test.FreeOnLog := True;
    Test.SyncTestCompleteFunc := @SyncReadPIP;
    TestThread.Add(Test);
  end;
end;

procedure TFormMain.ActionReadXMLExecute(Sender: TObject);
begin
   ShowMessage('Not implemented yet');
end;

procedure TFormMain.ActionRescanPortsExecute(Sender: TObject);
begin
  ComboBoxPorts.Items.Delimiter:=';';
  ComboBoxPorts.Items.DelimitedText := StringReplace(GetSerialPortNames, ',', ';', [rfReplaceAll, rfIgnoreCase]);
  if ComboBoxPorts.Items.Count > 0 then
    ComboBoxPorts.ItemIndex:= 0;
  UpdateUI
end;

procedure TFormMain.ActionSaveTestMatrixExecute(Sender: TObject);
var
  EnabledNode: TDOMNode;
  Test: TTestBase;
  i: Integer;
begin
  SaveDialog.DefaultExt := '*.xml';
  SaveDialog.Filter := 'XML Files|*.xml';
  if SaveDialog.Execute then
  begin
    for i := 0 to ListViewTestMatrix.Items.Count - 1 do
    begin
      Test := TTestBase( ListViewTestMatrix.Items[i].Data);
      EnabledNode := Test.XMLTests.FindNode('Enabled');
      if ListViewTestMatrix.Items[i].Checked then
        EnabledNode.FirstChild.NodeValue := 'True'
      else
        EnabledNode.FirstChild.NodeValue := 'False'
     end;
    WriteXMLFile(FXMLDocTestMatrix, SaveDialog.FileName);
  end;
end;

procedure TFormMain.ActionSendDatagramReplyExecute(Sender: TObject);
begin
  ShowMessage('Not implemented yet');
end;

procedure TFormMain.ActionSendPacketExecute(Sender: TObject);
begin
  ShowMessage('Not implemented yet');

end;

procedure TFormMain.ActionShowAboutExecute(Sender: TObject);
begin
  FormAbout.ShowModal;
end;

procedure TFormMain.ActionShowLogExecute(Sender: TObject);
begin
  FormLog.Left := Left+Width+4;
  FormLog.Top := Top;
  FormLog.Height := Height;
  FormLog.Width := 320;
  FormLog.Show;
end;

procedure TFormMain.ActionShowOptionsWinExecute(Sender: TObject);
begin
  FormSettings.Show;
end;

procedure TFormMain.ActionShowPreferencesMacExecute(Sender: TObject);
begin
  FormSettings.Show;
end;

procedure TFormMain.ActionStopAllTestsExecute(Sender: TObject);
begin

end;

procedure TFormMain.ActionCancelTestsExecute(Sender: TObject);
begin
  TestThread.TerminateTest := True;
end;


procedure TFormMain.ComboBoxBaudChange(Sender: TObject);
begin
  UpdateUI;
end;

procedure TFormMain.ComboBoxPortsChange(Sender: TObject);
begin
  if ActionConnect.Checked then
    ActionConnect.Execute;
  UpdateUI;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if Assigned(FTestThread) then
  begin
    TestThread.Terminate;
    FTestThread := nil;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  {$IFDEF DARWIN}
  AppMenu := TMenuItem.Create(Self);  {Application menu}
  AppMenu.Caption := #$EF#$A3#$BF;  {Unicode Apple logo char}
  MainMenu.Items.Insert(0, AppMenu);

  AppAboutCmd := TMenuItem.Create(Self);
  AppAboutCmd.Action := ActionShowAbout;
  AppAboutCmd.Caption := 'About ' + BUNDLENAME;
  AppMenu.Add(AppAboutCmd);  {Add About as item in application menu}

  AppSep1Cmd := TMenuItem.Create(Self);
  AppSep1Cmd.Caption := '-';
  AppMenu.Add(AppSep1Cmd);

  ActionShowPreferencesMac.ShortCut := ShortCut(VK_OEM_COMMA, [ssMeta]);
  AppPrefCmd := TMenuItem.Create(Self);
  AppPrefCmd.Action := ActionShowPreferencesMac;
  AppMenu.Add(AppPrefCmd);
  ActionShowOptionsWin.Visible := False;
  MenuItemToolsSeparatorWin.Visible := False;
  {$ELSE}
  AppAboutCmd := TMenuItem.Create(Self);
  AppAboutCmd.Action := ActionShowAbout;
  MenuItemHelp.Add(AppAboutCmd);
  {$ENDIF}


  ShownOnce := False;
  FTestStrings := TStringList.Create;
  FXMLDocTestResults := TXMLDocument.Create;
  ActionRescanPorts.Execute;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTestStrings);
  FreeAndNil(FXMLDocTestMatrix);
  FreeAndNil(FXMLDocTestResults);
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  if not ShownOnce then
  begin
    Settings.ReadSettings;
    EditDiscoverNodeAlias.Text := IntToHex(Settings.ProxyNodeAlias, 4);
    EditDiscoverNodeID.Text := IntToHex(Settings.ProxyNodeID, 6);
    ComboBoxPorts.ItemIndex := ComboBoxPorts.Items.IndexOf(Settings.ComPort);
    ComboBoxPorts.ItemIndex := ComboBoxPorts.Items.IndexOfName(Settings.ComPort);
    EditCustomBaudRate.Enabled := True;
    LabelCustomBaud.Enabled := True;
    ComboBoxBaud.ItemIndex := 0; // Custom
    ComboBoxBaud.ItemIndex := ComboBoxBaud.Items.IndexOf(IntToStr(Settings.BaudRate));
    ReadXMLFile(FXMLDocTestMatrix, Settings.TestMatrixFile);
    LoadTestMatrixListview;
    FormRawMessageLog.HidingCallback := @CallbackRawMessageLogHiding;
    UpdateUI;
    {$IFDEF USE_DEBUG_LOGGER}
    FormDebugLogger.Show;
    {$ENDIF}
  end;
  ShownOnce := True;
end;

procedure TFormMain.ListViewNodeDiscoverySelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if Selected then
  begin
    Settings.TargetNodeAlias := StrToInt('$' + Item.Caption);
    Settings.TargetNodeID := StrToInt64('$' + Item.SubItems[0]);
  end;
  ResetTestVerificationIcons;
  UpdateUI;
end;

procedure TFormMain.ListViewTestMatrixDeletion(Sender: TObject; Item: TListItem);
var
  Test: TTestBase;
begin
  Test := TTestBase( Item.Data);
  Test.XMLTests := nil;                    // The XML Document owns this
  FreeAndNil( Test);
end;

procedure TFormMain.ListViewTestMatrixDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  Item, ItemToMove: TListItem;
  i: Integer;
begin
  Item := ListViewTestMatrix.GetItemAt(X, Y);
  if Assigned(Item) then
  begin
    while ListViewTestMatrix.SelCount > 0 do
    begin
      i := 0;
      ItemToMove := nil;
      while not Assigned(ItemToMove) do
      begin
        if ListViewTestMatrix.Items[i].Selected then
          ItemToMove := ListViewTestMatrix.Items[i];
        Inc(i)
      end;
      ListViewTestMatrix.Items.Move(ItemToMove.Index, Item.Index);
      ItemToMove.Selected := False;
    end;
  end;
end;

procedure TFormMain.ListViewTestMatrixDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin

end;

procedure TFormMain.ListViewTestMatrixStartDrag(Sender: TObject; var DragObject: TDragObject);
begin

end;

procedure TFormMain.MenuItemConnectionClick(Sender: TObject);
var
  i: Integer;
  MenuItem: TMenuItem;
begin
  for i :=  MenuItemConnection.Count - 1  downto 0 do
  begin
    if MenuItemConnection.Items[i].GroupIndex = GROUPINDEX_CONNECTION_PORTS then
      MenuItemConnection.Delete(i);
  end;

  for i := 0 to ComboBoxPorts.Items.Count - 1 do
  begin
    MenuItem := TMenuItem.Create(Self);
    MenuItem.Caption := ComboBoxPorts.Items[i];
    MenuItem.Tag := i;
    if ComboBoxPorts.ItemIndex = i then
      MenuItem.Checked := True;
    MenuItem.OnClick := @MenuItemSelectPortClick;
    MenuItem.GroupIndex := GROUPINDEX_CONNECTION_PORTS;
    MenuItemConnection.Add(MenuItem);
  end;
end;

procedure TFormMain.MenuItemOptionsWinClick(Sender: TObject);
begin

end;

procedure TFormMain.SynEditCDIKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // Windows/Linux/OSX already handled by SynEdit using the Windows Shortcuts
  {$IFDEF darwin}
  if (Shift = [ssMeta]) then
  case Key of
    VK_C: SynEditCDI.CommandProcessor(TSynEditorCommand(ecCopy), ' ', nil);
    VK_V: SynEditCDI.CommandProcessor(TSynEditorCommand(ecPaste), ' ', nil);
    VK_X: SynEditCDI.CommandProcessor(TSynEditorCommand(ecCut), ' ', nil);
    end;
  {$ENDIF}
end;

function TFormMain.GetConnected: Boolean;
begin
  Result := Assigned(TestThread)
end;

procedure TFormMain.UpdateUI;
begin
  {$IFNDEF DISABLE_UI_UPDATE}
  if Assigned(TestThread) and Connected then
  begin
  ActionCancelTests.Enabled := TestThread.TestCount > 0;
  ActionExecuteTests.Enabled := (ListViewNodeDiscovery.SelCount = 1) and (TestThread.TestCount = 0);
  end else
  begin
    ActionCancelTests.Enabled := False;
    ActionExecuteTests.Enabled := False;
  end;
  ActionDiscoverNode.Enabled := Connected;
  ActionReadPip.Enabled := (ListViewNodeDiscovery.SelCount = 1) and Connected;
  ActionSaveTestMatrix.Enabled := Connected;
  ActionReadXML.Enabled := (ListViewNodeDiscovery.SelCount = 1) and Connected;
  ActionEventReader.Enabled := (ListViewNodeDiscovery.SelCount = 1) and Connected;
  ActionSendPacket.Enabled := (ListViewNodeDiscovery.SelCount = 1) and Connected;
  ActionSendDatagramReply.Enabled := (ListViewNodeDiscovery.SelCount = 1) and Connected;
  ActionConnect.Enabled :=  ComboBoxPorts.ItemIndex > -1;
  EditCustomBaudRate.Enabled := ComboBoxBaud.ItemIndex = 0;
  LabelCustomBaud.Enabled := ComboBoxBaud.ItemIndex = 0;
  if ComboBoxBaud.ItemIndex > 0 then EditCustomBaudRate.Text := '';
  {$ENDIF}
end;

procedure TFormMain.ClearTestResultsXML;
var
  Node: TDOMNode;
begin
  Node := XMLDocTestResults.FindNode('TestResults');
  if Assigned(Node) then
    XMLDocTestResults.RemoveChild(Node);
  Node := XMLDocTestResults.CreateElement('TestResult')
end;

procedure TFormMain.LoadTestMatrixListview;
var
  TestList: TList;
  Test: TTestBase;
  ListItem: TListItem;
  TestNode: TDOMNode;
  i: Integer;
begin
  if Assigned(XMLDocTestMatrix) then
  begin
    ListviewTestMatrix.Items.BeginUpdate;
    try
      ListviewTestMatrix.Items.Clear;
      TestList := TList.Create;
      try
        ExtractTestsFromXML(XMLDocTestMatrix, TestList);
        for i := 0 to TestList.Count - 1 do
        begin
          TestNode := TDOMNode( TestList[i]);
          Test := TTestBase.CreateInstanceFromString( TestClassnameFromTestNode(TestNode));  // Create a Test Object from the Classname in the XML

          if not Assigned(Test) then
              ShowMessage('Invalid Classname for Test ' + TestNameFromTestNode(TestNode))
          else begin
            if not (Test is TTestGetNodesUnderTest) then
            begin
              Test.SyncTestCompleteFunc := @SyncVerificationTests;
              Test.SyncAliasChangedFunc := @SyncTargetAliasChanged;
              Test.SyncAliasChangingFunc := @SyncTargetAliasChanging;
              Test.SyncUpdateProgressBarFunc := @SyncUpdateProgressBar;
              Test.SyncHideNodeResetMessageFunc := @SyncHideNodeResetMessage;
              Test.SyncShowNodeResetMessageFunc := @SyncShowNodeResetMessage;
              ListItem := ListviewTestMatrix.Items.Add;
              ListItem.Caption := TestNameFromTestNode(TestNode);
              ListItem.SubItems.Add(TestDescriptionFromTestNode(TestNode));
         //     ListItem.SubItems.Add(TestSpecDocFromTestNode(TestNode));
              ListItem.SubItems.Add(TestClassnameFromTestNode(TestNode));
              ListItem.Checked :=  TestEnabledStateFromTestNode(TestNode) = 'True';
              ListItem.ImageIndex := 2;
              ListItem.Data := Test;
              Test.XMLTests := TestNode;
              Test.ListItem := ListItem;   // Back Link to the Item
            end;
          end;
        end;
      finally
        TestList.Free;  // Does not own the nodes
        UpdateUI;
      end;
    finally
      ListviewTestMatrix.Items.EndUpdate
    end;
  end;
end;

procedure TFormMain.MenuItemSelectPortClick(Sender: TObject);
begin
  ComboBoxPorts.ItemIndex := (Sender as TMenuItem).Tag;  // Tag set when Menu List is created
end;

procedure TFormMain.ResetTestVerificationIcons;
var
  i: Integer;
begin
  for i := 0 to ListViewTestMatrix.Items.Count - 1 do
    ListViewTestMatrix.Items[i].ImageIndex := 2;
end;

procedure TFormMain.SyncHideNodeResetMessage(Test: TTestBase);
begin
  FormRestartNode.Hide;
end;

procedure TFormMain.SyncShowNodeResetMessage(Test: TTestBase);
begin
  FormRestartNode.Show;
end;

procedure TFormMain.SyncDiscoverNodes(Test: TTestBase);
var
  Helper: TOpenLCBMessageHelper;
  ResultStrings: TStringList;
  ListItem: TListItem;
  i: Integer;
begin
  Helper := TOpenLCBMessageHelper.Create;
  ListViewNodeDiscovery.Items.BeginUpdate;
  try
    ResultStrings := TStringList.Create;
    try
      ListViewNodeDiscovery.Items.Clear;

      ExtractResultsFromXML(Test.XMLResults, ResultStrings);

      for i := 0 to ResultStrings.Count - 1 do
      begin;
        Helper.Decompose(ResultStrings[i]);
        if Helper.MTI = MTI_VERIFIED_NODE_ID_NUMBER then
        begin
           ListItem := ListViewNodeDiscovery.Items.Add;
           ListItem.Caption := IntToHex(Helper.SourceAliasID, 3);
           ListItem.SubItems.Add(IntToHex(Helper.ExtractDataBytesAsInt(0, 5), 12));
        end;
      end;
    finally
      if ListViewNodeDiscovery.Items.Count > 0 then
      begin
        ListViewNodeDiscovery.Items[0].Focused := True;
        ListViewNodeDiscovery.Items[0].Selected  := True;
        if ListViewNodeDiscovery.Items.Count = 1 then
        begin
          Settings.MultiNodeTest := False;
          LabelDiscoverMultiNode.Caption := 'Mode: SingleNode test'
        end else
        begin
          Settings.MultiNodeTest := True;
          LabelDiscoverMultiNode.Caption := 'Mode: MultiNode test';
        end;
      end else
        LabelDiscoverMultiNode.Caption := 'Mode:';

      ResultStrings.Free
    end;
  finally
    ListViewNodeDiscovery.Items.EndUpdate;
    Helper.Free;
    Log(Test.XMLResults);
    if Test.FreeOnLog then
      Test.Free;
    if TestThread.TestCount = 0 then
      UpdateUI;
  end;
end;

procedure TFormMain.SyncReadPIP(Test: TTestBase);
var
  ReceiveResults: TStringList;
  Helper: TOpenLCBMessageHelper;
  Mask: QWord;
begin
  if Test.Passed then
  begin
    ReceiveResults := TStringList.Create;
    Helper := TOpenLCBMessageHelper.Create;
    try
      ExtractResultsFromXML(Test.XMLResults, ReceiveResults);
      if ReceiveResults.Count = 1 then
      begin
        Helper.Decompose(ReceiveResults[0]);
        Mask := Helper.ExtractDataBytesAsInt(2, 7);
        CheckGroupPIP.Checked[0] := Mask and PIP_PIP = PIP_PIP;
        CheckGroupPIP.Checked[1] := Mask and PIP_DATAGRAM = PIP_DATAGRAM;
        CheckGroupPIP.Checked[2] := Mask and PIP_STREAM = PIP_STREAM;
        CheckGroupPIP.Checked[3] := Mask and PIP_MEMORY_CONFIG = PIP_MEMORY_CONFIG;
        CheckGroupPIP.Checked[4] := Mask and PIP_RESERVATION = PIP_RESERVATION;
        CheckGroupPIP.Checked[5] := Mask and PIP_EVENT_EXCHANGE = PIP_EVENT_EXCHANGE;
        CheckGroupPIP.Checked[6] := Mask and PIP_IDENTIFCIATION = PIP_IDENTIFCIATION;
        CheckGroupPIP.Checked[7] := Mask and PIP_TEACH_LEARN = PIP_TEACH_LEARN;
        CheckGroupPIP.Checked[8] := Mask and PIP_REMOTE_BUTTON = PIP_REMOTE_BUTTON;
        CheckGroupPIP.Checked[9] := Mask and PIP_ABBREVIATED_CDI = PIP_ABBREVIATED_CDI;
        CheckGroupPIP.Checked[10] := Mask and PIP_DISPLAY = PIP_DISPLAY;
        CheckGroupPIP.Checked[11] := Mask and PIP_SIMPLE_NODE_ID = PIP_SIMPLE_NODE_ID;
        CheckGroupPIP.Checked[12] := Mask and PIP_CDI = PIP_CDI;
        CheckGroupPIP.Checked[13] := Mask and PIP_UNASSIGNED <> 0;
        CheckGroupPIP.Checked[14] := Mask and PIP_RESERVED <> 0;
        EditPipRawMessage.Text := ReceiveResults[0]
      end;
      LabelPipPassFail.Caption := 'PIP Test Passed'
    finally
      ReceiveResults.Free;
      Helper.Free
    end;
  end else
    LabelPipPassFail.Caption := 'PIP Test Failed';

  Log(Test.XMLResults);
  if Test.FreeOnLog then
    Test.Free;

  if TestThread.TestCount = 0 then
    UpdateUI;
end;

procedure TFormMain.SyncVerificationTests(Test: TTestBase);
 begin
  if Test.Passed then
    Test.ListItem.ImageIndex := 0
  else
    Test.ListItem.ImageIndex := 1;

  Log(Test.XMLResults);

  if Test.FreeOnLog then
    Test.Free;

  if TestThread.TestCount = 0 then
    UpdateUI;
end;

procedure TFormMain.SyncTargetAliasChanged(Test: TTestBase);
var
  AliasStr: String;
  i: Integer;
begin
  AliasStr := IntToHex(TargetAliasInPeril, 3);
  for i := 0 to ListViewNodeDiscovery.Items.Count - 1 do
  begin
    if strcomp(PChar( ListViewNodeDiscovery.Items[i].Caption) , PChar( AliasStr)) = 0 then
    begin
      ListViewNodeDiscovery.Items[i].Caption := IntToHex(Settings.TargetNodeAlias, 3);
      Break
    end
  end
end;

procedure TFormMain.SyncTargetAliasChanging(Test: TTestBase);
begin
  TargetAliasInPeril := Settings.TargetNodeAlias;
end;

procedure TFormMain.SyncUpdateProgressBar(Test: TTestBase);
begin
  ProgressBarTest.Max := Test.ProgressBarRange;
  ProgressBarTest.Position := Test.ProgressBarPos;
end;

procedure TFormMain.SyncRawMessage(MessageStr: String);
begin
  FormRawMessageLog.SynMemo.BeginUpdate;
  FormRawMessageLog.SynMemo.Lines.Add(MessageStr);
  FormRawMessageLog.SynMemo.CaretY := FormRawMessageLog.SynMemo.LineHeight * FormRawMessageLog.SynMemo.Lines.Count;
  FormRawMessageLog.SynMemo.EndUpdate;
end;

procedure TFormMain.CallbackRawMessageLogHiding;
begin
  if Assigned(TestThread) then
    TestThread.EnableRawMessages := False;
  ActionRawMessageLogShow.Checked := False;
end;

procedure TFormMain.Log(XML: TXMLDocument);
var
  MemStream: TMemoryStream;
  Str: String;
begin
  if FormLog.Visible then
  begin
    FormLog.SynMemo.Lines.BeginUpdate;
    try
      MemStream := TMemoryStream.Create;
      XMLWrite.WriteXMLFile(XML, MemStream);
      SetLength(Str, MemStream.Size);
      MemStream.Seek(0, soBeginning);
      MemStream.ReadBuffer(PChar( Str)^, MemStream.Size);
      FormLog.SynMemo.Text := FormLog.SynMemo.Text + Str;
      FormLog.SynMemo.CaretY := FormLog.SynMemo.LineHeight * FormLog.SynMemo.Lines.Count;
      MemStream.Free;
    finally
      FormLog.SynMemo.Lines.EndUpdate;
    end;
  end;
end;


end.

