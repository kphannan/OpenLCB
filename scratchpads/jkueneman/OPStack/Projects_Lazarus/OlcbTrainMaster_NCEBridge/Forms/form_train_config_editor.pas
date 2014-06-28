unit form_train_config_editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, laz2_DOM, laz2_XMLRead, com_port_hub, olcb_transport_layer,
  olcb_app_common_settings, olcb_utilities, olcb_defines, unit_cdi_parser, Buttons,
  ActnList, Menus, ethernet_hub;

type
  TFormTrainConfigEditor = class;

  TOnConfigEditorEvent = procedure(Throttle: TFormTrainConfigEditor) of object;

  { TFormTrainConfigEditor }

  TFormTrainConfigEditor = class(TForm)
    ActionClose: TAction;
    ActionWritePage: TAction;
    ActionReadAll: TAction;
    ActionReadPage: TAction;
    ActionStopRead: TAction;
    ActionStopWrite: TAction;
    ActionWriteAll: TAction;
    ActionList: TActionList;
    BitBtnWriteAll: TBitBtn;
    BitBtnWritePage: TBitBtn;
    BitBtnWriteStop: TBitBtn;
    BitBtnReadAll: TBitBtn;
    BitBtnReadPage: TBitBtn;
    BitBtnReadStop: TBitBtn;
    MenuItemClose: TMenuItem;
    MenuItemSep2: TMenuItem;
    MenuItemReadAll: TMenuItem;
    MenuItemWriteAll: TMenuItem;
    MenuItemStopRead: TMenuItem;
    MenuItemReadPage: TMenuItem;
    MenuItemSep1: TMenuItem;
    MenuItemStopWrite: TMenuItem;
    MenuItemWritePage: TMenuItem;
    PanelBkGnd: TPanel;
    PopupMenuMain: TPopupMenu;
    StatusBar: TStatusBar;
    procedure ActionReadAllExecute(Sender: TObject);
    procedure ActionReadPageExecute(Sender: TObject);
    procedure ActionStopReadExecute(Sender: TObject);
    procedure ActionStopWriteExecute(Sender: TObject);
    procedure ActionWriteAllExecute(Sender: TObject);
    procedure ActionWritePageExecute(Sender: TObject);

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FAliasID: Word;
    FCdiParser: TCdiParser;
    FComPortHub: TComPortHub;
    FConfigReadTaskQueue: TList;
    FConfigReadTaskRunning: Boolean;
    FConfigWriteTaskQueue: TList;
    FConfigWriteTaskRunning: Boolean;
    FEthernetHub: TEthernetHub;
    FImageList16x16: TImageList;
    FOnConfigEditorClose: TOnConfigEditorEvent;
    FOnConfigEditorHide: TOnConfigEditorEvent;
    FPageControl: TPageControl;
    FShownOnce: Boolean;
    procedure SetImageList(AValue: TImageList);
    { private declarations }
  protected
    function FindScrollBox(Page: TTabSheet): TScrollBox;
    function FindControlPageAndIndex(Control: TControl; var iPage: Word; var iControl: Word): Boolean;
    function FindControlByPageAndIndex(var Control: TControl; iPage, iControl: Word): Boolean;
    procedure OnSpeedButtonReadConfigClickCallback(Sender: TObject);
    procedure OnSpeedButtonWriteConfigClickCallback(Sender: TObject);
    procedure ReadConfiguration;
    procedure ReadConfigurationPage(iPage: Integer);
    procedure ReadConfigurationEdit(Edit: TOlcbEdit; iPage, iControl: Word);
    procedure ReadConfigurationSpinEdit(Edit: TOlcbSpinEdit; iPage, iControl: Word);
    procedure ReadConfigurationComboEdit(Edit: TOlcbComboBox; iPage, iControl: Word);
    procedure WriteConfiguration;
    procedure WriteConfigurationPage(iPage: Integer);
    procedure WriteConfigurationEdit(Edit: TOlcbEdit; iPage, iControl: Word);
    procedure WriteConfigurationSpinEdit(Edit: TOlcbSpinEdit; iPage, iControl: Word);
    procedure WriteConfigurationComboEdit(Edit: TOlcbComboBox; iPage, iControl: Word);
    procedure UpdateUI;

    property CdiParser: TCdiParser read FCdiParser write FCdiParser;
    property ComPortHub: TComPortHub read FComPortHub write FComPortHub;
    property ConfigReadTaskQueue: TList read FConfigReadTaskQueue write FConfigReadTaskQueue;
    property ConfigWriteTaskQueue: TList read FConfigWriteTaskQueue write FConfigWriteTaskQueue;
    property ConfigWriteTaskRunning: Boolean read FConfigWriteTaskRunning write FConfigWriteTaskRunning;
    property ConfigReadTaskRunning: Boolean read FConfigReadTaskRunning write FConfigReadTaskRunning;
    property EthernetHub: TEthernetHub read FEthernetHub write FEthernetHub;
    property ShownOnce: Boolean read FShownOnce write FShownOnce;
    property PageControl: TPageControl read FPageControl write FPageControl;
  public
    { public declarations }
    procedure FlushConfigWriteTasks;
    procedure FlushConfigReadTasks;
    procedure InitTransportLayers(AnEthernetHub: TEthernetHub; AComPortThread: TComPortHub);
    property AliasID: Word read FAliasID write FAliasID;
    property OnConfigEditorHide: TOnConfigEditorEvent read FOnConfigEditorHide write FOnConfigEditorHide;
    property OnConfigEditorClose: TOnConfigEditorEvent read FOnConfigEditorClose write FOnConfigEditorClose;
    property ImageList16x16: TImageList read FImageList16x16 write SetImageList;
  end;

  { TFormConfigEditorList }

  TFormConfigEditorList = class(TList)
  private
    function GetConfigEditor(Index: Integer): TFormTrainConfigEditor;
    procedure SetConfigEditor(Index: Integer; AValue: TFormTrainConfigEditor);
  public
    procedure Clear; override;
    function FindEditorByAlias(TestAlias: Word): TFormTrainConfigEditor;
    procedure HideAll;
    procedure CloseAll;
    procedure ShowAll;
    property ConfigEditors[Index: Integer]: TFormTrainConfigEditor read GetConfigEditor write SetConfigEditor;
  end;

implementation

{$R *.lfm}

{ TFormConfigEditorList }

function TFormConfigEditorList.GetConfigEditor(Index: Integer): TFormTrainConfigEditor;
begin
  Result := TFormTrainConfigEditor( Items[Index]);
end;

procedure TFormConfigEditorList.SetConfigEditor(Index: Integer; AValue: TFormTrainConfigEditor);
begin
  Items[Index] := AValue
end;

procedure TFormConfigEditorList.Clear;
begin
  inherited Clear;
end;

function TFormConfigEditorList.FindEditorByAlias(TestAlias: Word): TFormTrainConfigEditor;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  while not Assigned(Result) and (i < Count) do
  begin
    if TestAlias = ConfigEditors[i].AliasID then
      Result := ConfigEditors[i];
    Inc(i)
  end;
end;

procedure TFormConfigEditorList.HideAll;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    ConfigEditors[i].Hide
end;

procedure TFormConfigEditorList.CloseAll;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    ConfigEditors[i].Close;
  Clear;
end;

procedure TFormConfigEditorList.ShowAll;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    ConfigEditors[i].Show;
end;

{ TFormTrainConfigEditor }

procedure TFormTrainConfigEditor.FormCreate(Sender: TObject);
begin
  FComPortHub := nil;
  FEthernetHub := nil;
  FAliasID := 0;
  FShownOnce := False;
  FCdiParser := TCdiParser.Create;
  CdiParser.OnSpeedButtonReadConfigClickCallback := @OnSpeedButtonReadConfigClickCallback;
  CdiParser.OnSpeedButtonWriteConfigClickCallback := @OnSpeedButtonWriteConfigClickCallback;
  FPageControl := nil;
  FConfigWriteTaskQueue := TList.Create;
  FConfigReadTaskQueue := TList.Create;
  ConfigWriteTaskRunning := False;
  FConfigReadTaskRunning := False;
  ImageList16x16 := nil;
end;

procedure TFormTrainConfigEditor.FormDestroy(Sender: TObject);
begin
  FreeAndNil( FCdiParser);
  FreeAndNil(FConfigReadTaskQueue);
  FreeAndNil(FConfigWriteTaskQueue);
end;

procedure TFormTrainConfigEditor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(OnConfigEditorClose) then
    OnConfigEditorClose(Self);
  FlushConfigWriteTasks;
  FlushConfigReadTasks;;
  CloseAction := caFree;
end;

procedure TFormTrainConfigEditor.ActionWriteAllExecute(Sender: TObject);
begin
  WriteConfiguration;
  UpdateUI
end;

procedure TFormTrainConfigEditor.ActionWritePageExecute(Sender: TObject);
begin
 if Assigned(FPageControl) then
 begin
   if PageControl.ActivePageIndex > -1 then
     WriteConfigurationPage(PageControl.ActivePageIndex)
 end;
 UpdateUI
end;

procedure TFormTrainConfigEditor.ActionReadAllExecute(Sender: TObject);
begin
  ReadConfiguration;
  UpdateUI
end;

procedure TFormTrainConfigEditor.ActionReadPageExecute(Sender: TObject);
begin
 if Assigned(FPageControl) then
 begin
   if PageControl.ActivePageIndex > -1 then
     ReadConfigurationPage(PageControl.ActivePageIndex)
 end;
 UpdateUI
end;

procedure TFormTrainConfigEditor.ActionStopReadExecute(Sender: TObject);
begin

 FlushConfigReadTasks;
 UpdateUI
end;

procedure TFormTrainConfigEditor.ActionStopWriteExecute(Sender: TObject);
begin
 FlushConfigWriteTasks;
 UpdateUI
end;

procedure TFormTrainConfigEditor.FormHide(Sender: TObject);
begin
  if Assigned(OnConfigEditorHide) then
    OnConfigEditorHide(Self)
end;

procedure TFormTrainConfigEditor.FormShow(Sender: TObject);
begin
  if Assigned(ComPortHub) and not ShownOnce then
  begin
  //  Task := TTaskAddressSpaceMemoryReadWithDatagram.Create(GlobalSettings.General.AliasIDAsVal, AliasID, True, MSI_CDI, True);
  //  Task.RemoveKey := PtrInt( Self);
  //  Task.Terminator := #0;
  //  Task.ForceOptionalSpaceByte := False;
  //  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
 //   DispatchTask(Task);
    ActionStopRead.Enabled := False;
    ActionStopWrite.Enabled := False;
    ActionReadAll.Enabled := False;
    ActionReadPage.Enabled := False;
    ActionWriteAll.Enabled := False;
    ActionWritePage.Enabled := False;
  end;
end;

procedure TFormTrainConfigEditor.SetImageList(AValue: TImageList);
begin
  if FImageList16x16=AValue then Exit;
  FImageList16x16:=AValue;
  CdiParser.ImageList16x16 := AValue;
end;

function TFormTrainConfigEditor.FindScrollBox(Page: TTabSheet): TScrollBox;
var
  iControl: Integer;
begin
  Result := nil;
  for iControl := 0 to Page.ControlCount - 1 do
  begin
    if Page.Controls[iControl] is TScrollBox then
      Result := Page.Controls[iControl] as TScrollBox
  end;
end;

procedure TFormTrainConfigEditor.OnSpeedButtonReadConfigClickCallback(Sender: TObject);
var
  iPage, iControl: Word;
  Edit: TOlcbEdit;
  SpinEdit: TOlcbSpinEdit;
  ComboBox: TOlcbComboBox;
begin
  iPage := 0;
  iControl := 0;
  if (Sender as TSpeedButton).Owner is TOlcbEdit then
  begin
    Edit := (Sender as TSpeedButton).Owner as TOlcbEdit;
    if FindControlPageAndIndex(Edit, iPage, iControl) then
      ReadConfigurationEdit(Edit, iPage, iControl);
  end else
  if (Sender as TSpeedButton).Owner is TOlcbSpinEdit then
  begin
    SpinEdit := (Sender as TSpeedButton).Owner as TOlcbSpinEdit;
    if FindControlPageAndIndex(SpinEdit, iPage, iControl) then
      ReadConfigurationSpinEdit(SpinEdit, iPage, iControl);
  end else
  if (Sender as TSpeedButton).Owner is TOlcbComboBox then
  begin
    ComboBox := (Sender as TSpeedButton).Owner as TOlcbComboBox;
    if FindControlPageAndIndex(ComboBox, iPage, iControl) then
      ReadConfigurationComboEdit(ComboBox, iPage, iControl);
  end
end;

procedure TFormTrainConfigEditor.OnSpeedButtonWriteConfigClickCallback(Sender: TObject);
var
  iPage, iControl: Word;
  Edit: TOlcbEdit;
  SpinEdit: TOlcbSpinEdit;
  ComboBox: TOlcbComboBox;
begin
  iPage := 0;
  iControl := 0;
  if (Sender as TSpeedButton).Owner is TOlcbEdit then
  begin
    Edit := (Sender as TSpeedButton).Owner as TOlcbEdit;
    if FindControlPageAndIndex(Edit, iPage, iControl) then
      WriteConfigurationEdit(Edit, iPage, iControl);
  end else
  if (Sender as TSpeedButton).Owner is TOlcbSpinEdit then
  begin
    SpinEdit := (Sender as TSpeedButton).Owner as TOlcbSpinEdit;
    if FindControlPageAndIndex(SpinEdit, iPage, iControl) then
      WriteConfigurationSpinEdit(SpinEdit, iPage, iControl);
  end else
  if (Sender as TSpeedButton).Owner is TOlcbComboBox then
  begin
    ComboBox := (Sender as TSpeedButton).Owner as TOlcbComboBox;
    if FindControlPageAndIndex(ComboBox, iPage, iControl) then
      WriteConfigurationComboEdit(ComboBox, iPage, iControl);
  end
end;

procedure TFormTrainConfigEditor.ReadConfiguration;
var
  iPage: Integer;
begin
  if Assigned(FPageControl) then
  begin
    for iPage := 0 to PageControl.PageCount - 1 do
      ReadConfigurationPage(iPage)
  end;
end;

procedure TFormTrainConfigEditor.ReadConfigurationPage(iPage: Integer);
var
  Control: TControl;
  ScrollBox: TScrollBox;
  iControl: Integer;
begin
  ScrollBox := FindScrollBox(PageControl.Pages[iPage]);
  if Assigned(ScrollBox) then
  begin
    for iControl := 0 to ScrollBox.ControlCount - 1 do
    begin
      Control := ScrollBox.Controls[iControl];
      if Control is TOlcbEdit then
        ReadConfigurationEdit(Control as TOlcbEdit, iPage, iControl)
      else
      if Control is TOlcbSpinEdit then
        ReadConfigurationSpinEdit(Control as TOlcbSpinEdit, iPage, iControl)
      else
      if Control is TOlcbComboBox then
        ReadConfigurationComboEdit(Control as TOlcbComboBox, iPage, iControl)
    end;
  end;
end;

procedure TFormTrainConfigEditor.ReadConfigurationEdit(Edit: TOlcbEdit; iPage, iControl: Word);

begin

end;

procedure TFormTrainConfigEditor.ReadConfigurationSpinEdit(Edit: TOlcbSpinEdit; iPage, iControl: Word);
begin

end;

procedure TFormTrainConfigEditor.ReadConfigurationComboEdit(Edit: TOlcbComboBox; iPage, iControl: Word);
begin
end;

procedure TFormTrainConfigEditor.WriteConfigurationEdit(Edit: TOlcbEdit; iPage, iControl: Word);

begin

end;

procedure TFormTrainConfigEditor.WriteConfigurationSpinEdit(Edit: TOlcbSpinEdit; iPage, iControl: Word);
begin

end;

procedure TFormTrainConfigEditor.WriteConfigurationComboEdit(Edit: TOlcbComboBox; iPage, iControl: Word);
begin

end;

procedure TFormTrainConfigEditor.WriteConfiguration;
var
  iPage: Integer;
begin
  if Assigned(FPageControl) then
  begin
    for iPage := 0 to PageControl.PageCount - 1 do
      WriteConfigurationPage(iPage)
  end;
end;

procedure TFormTrainConfigEditor.WriteConfigurationPage(iPage: Integer);
var
  Control: TControl;
  ScrollBox: TScrollBox;
  iControl: Integer;
begin
  ScrollBox := FindScrollBox(PageControl.Pages[iPage]);
  if Assigned(ScrollBox) then
  begin
    for iControl := 0 to ScrollBox.ControlCount - 1 do
    begin
      Control := ScrollBox.Controls[iControl];
      if Control is TOlcbEdit then
        WriteConfigurationEdit(Control as TOlcbEdit, iPage, iControl)
      else
      if Control is TOlcbSpinEdit then
        WriteConfigurationSpinEdit(Control as TOlcbSpinEdit, iPage, iControl)
      else
      if Control is TOlcbComboBox then
        WriteConfigurationComboEdit(Control as TOlcbComboBox, iPage, iControl);
    end;
  end;
end;


function TFormTrainConfigEditor.FindControlPageAndIndex(Control: TControl; var iPage: Word; var iControl: Word): Boolean;
var
  ScrollBox: TScrollBox;
  i, j: Integer;
begin
  Result := False;
  iPage := 0;
  iControl := 0;
  if Assigned(PageControl) then
  begin
    i := 0;
    while (i < PageControl.PageCount) and not Result do
    begin
      ScrollBox := FindScrollBox(PageControl.Pages[i]);
      if Assigned(ScrollBox) then
      begin
        j := 0;
        while (j < ScrollBox.ControlCount) and not Result do
        begin
          if Control = ScrollBox.Controls[j] then
          begin
            iPage := i;
            iControl := j;
            Result := True
          end;
          Inc(j)
        end;
      end;
      Inc(i)
    end;
  end;
end;

function TFormTrainConfigEditor.FindControlByPageAndIndex(
  var Control: TControl; iPage, iControl: Word): Boolean;
var
  ScrollBox: TScrollBox;
begin
  Result := False;
  Control := nil;
  if Assigned(PageControl) then
  begin
    if iPage < PageControl.PageCount then
    begin
      ScrollBox := FindScrollBox(PageControl.Pages[iPage]);
      if Assigned(ScrollBox) then
      begin
        if iControl < ScrollBox.ControlCount then
        begin
          Control := ScrollBox.Controls[iControl];
          Result := True
        end;
      end;
    end;
  end;
end;

procedure TFormTrainConfigEditor.UpdateUI;
begin
  if ConfigReadTaskQueue.Count > 0 then
    StatusBar.Panels[1].Text := 'Configuration reads remaining: ' + IntToStr(ConfigReadTaskQueue.Count)
  else
    StatusBar.Panels[1].Text := '';

  if ConfigWriteTaskQueue.Count > 0 then
    StatusBar.Panels[0].Text := 'Configuration writes remaining: ' + IntToStr(ConfigWriteTaskQueue.Count)
  else
    StatusBar.Panels[0].Text := '';

  ActionStopRead.Enabled := ConfigReadTaskQueue.Count > 0;
  ActionStopWrite.Enabled := ConfigWriteTaskQueue.Count > 0;
  ActionReadAll.Enabled := (ConfigWriteTaskQueue.Count = 0) and (ConfigReadTaskQueue.Count = 0);
  ActionReadPage.Enabled := (ConfigWriteTaskQueue.Count = 0) and (ConfigReadTaskQueue.Count = 0);
  ActionWriteAll.Enabled := (ConfigWriteTaskQueue.Count = 0) and (ConfigReadTaskQueue.Count = 0);
  ActionWritePage.Enabled := (ConfigWriteTaskQueue.Count = 0) and (ConfigReadTaskQueue.Count = 0);
end;

procedure TFormTrainConfigEditor.FlushConfigWriteTasks;
var
  i: Integer;
begin
  try
    for i := 0 to ConfigWriteTaskQueue.Count - 1 do
      TObject( ConfigWriteTaskQueue[i]).Free;
  finally
    ConfigWriteTaskQueue.Clear;
    ConfigWriteTaskRunning := False;
  end;
end;

procedure TFormTrainConfigEditor.FlushConfigReadTasks;
var
  i: Integer;
begin
  try
    for i := 0 to ConfigReadTaskQueue.Count - 1 do
      TObject( ConfigReadTaskQueue[i]).Free;
  finally
    ConfigReadTaskQueue.Clear;
    ConfigReadTaskRunning := False;
  end;
end;

procedure TFormTrainConfigEditor.InitTransportLayers(AnEthernetHub: TEthernetHub; AComPortThread: TComPortHub);
begin
  FEthernetHub := AnEthernetHub;
  FComPortHub := AComPortThread;
end;

end.

