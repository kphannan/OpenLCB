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
    FDispatchTask: TDispatchTaskFunc;
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
    procedure OnBeforeDestroyTask(Sender: TOlcbTaskBase);
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
    procedure QueueConfigWriteTask(Task: TOlcbTaskBase);
    procedure QueueConfigReadTask(Task: TOlcbTaskBase);
    procedure UpdateUI;

    property CdiParser: TCdiParser read FCdiParser write FCdiParser;
    property ComPortHub: TComPortHub read FComPortHub write FComPortHub;
    property ConfigReadTaskQueue: TList read FConfigReadTaskQueue write FConfigReadTaskQueue;
    property ConfigWriteTaskQueue: TList read FConfigWriteTaskQueue write FConfigWriteTaskQueue;
    property ConfigWriteTaskRunning: Boolean read FConfigWriteTaskRunning write FConfigWriteTaskRunning;
    property ConfigReadTaskRunning: Boolean read FConfigReadTaskRunning write FConfigReadTaskRunning;
    property DispatchTask: TDispatchTaskFunc read FDispatchTask write FDispatchTask;
    property EthernetHub: TEthernetHub read FEthernetHub write FEthernetHub;
    property ShownOnce: Boolean read FShownOnce write FShownOnce;
    property PageControl: TPageControl read FPageControl write FPageControl;
  public
    { public declarations }
    procedure FlushConfigWriteTasks;
    procedure FlushConfigReadTasks;
    procedure InitTransportLayers(AnEthernetHub: TEthernetHub; AComPortThread: TComPortHub; ADispatchTaskFunc: TDispatchTaskFunc);
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
 ComPortHub.RemoveAndFreeTasks( PtrInt( Self));
 EthernetHub.RemoveAndFreeTasks( PtrInt(Self));
 FlushConfigReadTasks;
 UpdateUI
end;

procedure TFormTrainConfigEditor.ActionStopWriteExecute(Sender: TObject);
begin
 ComPortHub.RemoveAndFreeTasks( PtrInt( Self));
 EthernetHub.RemoveAndFreeTasks( PtrInt(Self));
 FlushConfigWriteTasks;
 UpdateUI
end;

procedure TFormTrainConfigEditor.FormHide(Sender: TObject);
begin
  if Assigned(OnConfigEditorHide) then
    OnConfigEditorHide(Self)
end;

procedure TFormTrainConfigEditor.FormShow(Sender: TObject);
var
  Task: TReadAddressSpaceMemoryTask;
begin
  if Assigned(ComPortHub) and not ShownOnce then
  begin
    Task := TReadAddressSpaceMemoryTask.Create(GlobalSettings.General.AliasIDAsVal, AliasID, True, MSI_CDI, True);
    Task.RemoveKey := PtrInt( Self);
    Task.Terminator := #0;
    Task.ForceOptionalSpaceByte := False;
    Task.OnBeforeDestroy := @OnBeforeDestroyTask;
    DispatchTask(Task);
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

procedure TFormTrainConfigEditor.OnBeforeDestroyTask(Sender: TOlcbTaskBase);
var
  MemTask: TReadAddressSpaceMemoryTask;
  ADoc: TXMLDocument;
  iPage, iControl: Integer;
  Control: TControl;
  ScrollBox: TScrollBox;
  TaskStream: TMemoryStream;
  Str: String;
  Relation: TMapRelation;
  Done: Boolean;
begin
  try
    if not Sender.ForceTermination then
    begin
      if Sender is TReadAddressSpaceMemoryTask then
      begin
        // This is the CDI read result
        MemTask := TReadAddressSpaceMemoryTask(Sender);
        if MemTask.DataStream.Size > 1 then
        begin
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
          PageControl := CdiParser.Build_CDI_Interface(PanelBkGnd, ADoc);
          ActionStopRead.Enabled := True;
          ActionStopWrite.Enabled := True;
          ActionReadAll.Enabled := True;
          ActionReadPage.Enabled := True;
          ActionWriteAll.Enabled := True;
          ActionWritePage.Enabled := True;
          ReadConfiguration;
          FreeAndNil(ADoc);
        end
      end else
      if Sender is TReadAddressSpaceMemoryRawTask then
      begin
        if TReadAddressSpaceMemoryRawTask(Sender).ErrorCode <> 0 then
        begin
          ShowMessage('Error Code: ' + IntToHex(TReadAddressSpaceMemoryRawTask(Sender).ErrorCode, 4) + #13 + #10 + TReadAddressSpaceMemoryRawTask(Sender).ErrorString);
        end else
        begin
          // This is a configuration read request
          iPage := Sender.Tag and $FFFF;
          iControl := (Sender.Tag shr 16) and $FFFF;

          if PageControl.PageCount > iPage then
          begin
            ScrollBox := FindScrollBox(PageControl.Pages[iPage]);
            if Assigned(ScrollBox) then
            begin
              if ScrollBox.ControlCount > iControl then
              begin
                Control := ScrollBox.Controls[iControl];
                if Control is TOlcbEdit then
                begin
                  if Control = Sender.OwnerControl then
                  begin
                    // Whew, success
                    TaskStream := TReadAddressSpaceMemoryRawTask(Sender).Stream;
                    TaskStream.Position := 0;
                    case (Control as TOlcbEdit).ConfigInfo.DataType of
                      cdt_EventID : begin
                                      Str := IntToHex(TaskStream.ReadByte, 2);
                                      while (TaskStream.Position < TaskStream.Size) do
                                        Str := Str + '.' + IntToHex(TaskStream.ReadByte, 2);
                                      (Control as TOlcbEdit).Text := Str;
                                    end;
                      cdt_Int     : begin
                                      Str := IntToHex(TaskStream.ReadByte, 2);
                                      while (TaskStream.Position < TaskStream.Size) do
                                        Str := Str + IntToHex(TaskStream.ReadByte, 2);
                                      (Control as TOlcbEdit).Text := Str;
                                    end;
                      cdt_String  : begin
                                      Str := Char( TaskStream.ReadByte);
                                      while (TaskStream.Position < TaskStream.Size) do
                                        Str := Str + Char( TaskStream.ReadByte);
                                      (Control as TOlcbEdit).Text := Str;
                                    end;
                    end;
                    (Control as TOlcbEdit).ConfigInfo.State := ocs_Current;
                  end
                end else
                if Control is TOlcbSpinEdit then
                begin
                   if Control = Sender.OwnerControl then
                  begin
                    // Whew, success
                    TaskStream := TReadAddressSpaceMemoryRawTask(Sender).Stream;
                    TaskStream.Position := 0;
                    case (Control as TOlcbSpinEdit).ConfigInfo.DataType of
                      cdt_Int     : begin
                                      Str := IntToHex( TaskStream.ReadByte, 2);
                                      while (TaskStream.Position < TaskStream.Size) do
                                        Str := Str + IntToHex( TaskStream.ReadByte, 2);
                                      Str := '0x' + Str;
                                      (Control as TOlcbSpinEdit).Value := StrToInt(Str);
                                    end;
                      cdt_Bit     : begin
                                      // TODO
                                    end;
                    end;
                    (Control as TOlcbSpinEdit).ConfigInfo.State := ocs_Current;
                  end
                end else
                if Control is TOlcbComboBox then
                begin
                  if Control = Sender.OwnerControl then
                  begin
                    // Whew, success
                    TaskStream := TReadAddressSpaceMemoryRawTask(Sender).Stream;
                    TaskStream.Position := 0;
                    case (Control as TOlcbComboBox).ConfigInfo.DataType of
                      cdt_Int,
                      cdt_EventID : begin
                                      Str := IntToHex( TaskStream.ReadByte, 2);
                                      while (TaskStream.Position < TaskStream.Size) do
                                        Str := Str + IntToHex( TaskStream.ReadByte, 2);
                                      Str := '0x' + Str;
                                      Relation := (Control as TOlcbComboBox).ConfigInfo.MapList.FindMapByProperty(IntToStr( StrToInt( Str)));
                                      if Assigned(Relation) then
                                        (Control as TOlcbComboBox).ItemIndex := (Control as TOlcbComboBox).Items.IndexOf( Relation.Value);
                                    end;
                      cdt_String  : begin
                                      Str := Char( TaskStream.ReadByte);
                                      while (TaskStream.Position < TaskStream.Size) do
                                        Str := Str + Char( TaskStream.ReadByte);
                                      Relation := (Control as TOlcbComboBox).ConfigInfo.MapList.FindMapByProperty(Str);
                                      if Assigned(Relation) then
                                        (Control as TOlcbComboBox).ItemIndex := (Control as TOlcbComboBox).Items.IndexOf( Relation.Value);
                                    end;
                      cdt_Bit     : begin
                                      // TODO
                                    end;
                    end;
                    (Control as TOlcbComboBox).ConfigInfo.State := ocs_Current;
                  end
                end
              end
            end;
          end;
          if ConfigReadTaskQueue.Count > 0 then
          begin
            DispatchTask( TOlcbTaskBase( ConfigReadTaskQueue[0]));
            ConfigReadTaskQueue.Delete(0);
          end else
            ConfigReadTaskRunning := False;
          UpdateUI
        end
      end else
      if Sender is TWriteAddressSpaceMemoryRawTask then
      begin
        // This is a configuration write request
        iPage := Sender.Tag and $FFFF;
        iControl := (Sender.Tag shr 16) and $FFFF;

        if FindControlByPageAndIndex(Control, iPage, iControl) then
        begin
          if Control is TOlcbEdit then
          begin
            (Control as TOlcbEdit).ConfigInfo.State := ocs_Current;
          end else
          if Control is TOlcbSpinEdit then
          begin
            (Control as TOlcbSpinEdit).ConfigInfo.State := ocs_Current;
          end else
          if Control is TOlcbComboBox then
          begin
            (Control as TOlcbComboBox).ConfigInfo.State := ocs_Current;
          end;
        end;

        // Writes must be serialize as the micro may not keep up as some will "freeze" when they write EEPROM
        if ConfigWriteTaskQueue.Count > 0 then
        begin
          DispatchTask( TOlcbTaskBase( ConfigReadTaskQueue[0]));
          ConfigWriteTaskQueue.Delete(0);
        end else
          ConfigWriteTaskRunning := False;
        UpdateUI;
      end;
    end;
  except
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
var
  Task: TOlcbTaskBase;
begin
  Task := TReadAddressSpaceMemoryRawTask.Create(GlobalSettings.General.AliasIDAsVal, AliasID, True, MSI_CONFIG, Edit.ConfigInfo.ConfigMemAddress, Edit.ConfigInfo.ConfigMemSize, False);
  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
  Task.Tag := iPage or (iControl shl 16);
  Task.RemoveKey := PtrInt( Self);
  Task.OwnerControl := Edit;
  QueueConfigReadTask(Task);
end;

procedure TFormTrainConfigEditor.ReadConfigurationSpinEdit(Edit: TOlcbSpinEdit; iPage, iControl: Word);
var
  Task: TOlcbTaskBase;
begin
  Task := TReadAddressSpaceMemoryRawTask.Create(GlobalSettings.General.AliasIDAsVal, AliasID, True, MSI_CONFIG, Edit.ConfigInfo.ConfigMemAddress, Edit.ConfigInfo.ConfigMemSize, False);
  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
  Task.Tag := iPage or (iControl shl 16);
  Task.RemoveKey := PtrInt( Self);
  Task.OwnerControl := Edit;
  QueueConfigReadTask(Task);
end;

procedure TFormTrainConfigEditor.ReadConfigurationComboEdit(Edit: TOlcbComboBox; iPage, iControl: Word);
var
  Task: TOlcbTaskBase;
begin
  Task := TReadAddressSpaceMemoryRawTask.Create(GlobalSettings.General.AliasIDAsVal, AliasID, True, MSI_CONFIG, Edit.ConfigInfo.ConfigMemAddress, Edit.ConfigInfo.ConfigMemSize, False);
  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
  Task.Tag := iPage or (iControl shl 16);
  Task.RemoveKey := PtrInt( Self);
  Task.OwnerControl := Edit;
  QueueConfigReadTask(Task);
end;

procedure TFormTrainConfigEditor.WriteConfigurationEdit(Edit: TOlcbEdit; iPage, iControl: Word);
var
  Stream: TMemoryStream;
  i: Integer;
  EventID: TEventID;
  HexArray: THexArray;
  Task: TOlcbTaskBase;
begin
  Stream := TMemoryStream.Create;
  try
    case Edit.ConfigInfo.DataType of
      cdt_EventID : begin
                      // Strip out the periods (should validate it too)
                      EventID := DotHexToEvent(Edit.Text);

                      for i := 0 to MAX_EVENT_LEN - 1 do
                        Stream.WriteByte( EventID[i]);
                      Stream.Position := 0;
                    end;
      cdt_Int     : begin
                      HexArray := StrToHexArray(Edit.Text);

                      for i := Edit.ConfigInfo.ConfigMemSize - 1 downto 0  do
                        Stream.WriteByte( HexArray[i]);
                      Stream.Position := 0;
                    end;
      cdt_String  : begin
                      for i := 1 to Length(Edit.Text) do
                        Stream.WriteByte( Ord(Edit.Text[i]));
                      // Add Null
                      Stream.WriteByte( Ord( #0));
                      Stream.Position := 0;
                    end;
      cdt_Bit     : begin
                    end;
    end;
    Task := TWriteAddressSpaceMemoryRawTask.Create(GlobalSettings.General.AliasIDAsVal, AliasID, True, MSI_CONFIG, Edit.ConfigInfo.ConfigMemAddress, Stream);
    Task.OnBeforeDestroy := @OnBeforeDestroyTask;
    Task.Tag := iPage or (iControl shl 16);
    Task.RemoveKey := PtrInt( Self);
    Task.OwnerControl := Edit;
    QueueConfigWriteTask(Task);
  finally
    Stream.Free
  end;
end;

procedure TFormTrainConfigEditor.WriteConfigurationSpinEdit(Edit: TOlcbSpinEdit; iPage, iControl: Word);
var
  Stream: TMemoryStream;
  i: Integer;
  HexArray: THexArray;
  Task: TOlcbTaskBase;
begin
  Stream := TMemoryStream.Create;
  try
    case Edit.ConfigInfo.DataType of
      cdt_Int     : begin
                      HexArray := IntToHexArray( Edit.Value);

                      for i := Edit.ConfigInfo.ConfigMemSize - 1 downto 0  do
                        Stream.WriteByte( HexArray[i]);
                      Stream.Position := 0;
                    end;
      cdt_Bit     : begin
                       // TODO
                    end;
    end;
    Task := TWriteAddressSpaceMemoryRawTask.Create(GlobalSettings.General.AliasIDAsVal, AliasID, True, MSI_CONFIG, Edit.ConfigInfo.ConfigMemAddress, Stream);
    Task.OnBeforeDestroy := @OnBeforeDestroyTask;
    Task.Tag := iPage or (iControl shl 16);
    Task.RemoveKey := PtrInt( Self);
    Task.OwnerControl := Edit;
    QueueConfigWriteTask(Task);
  finally
    Stream.Free
  end;
end;

procedure TFormTrainConfigEditor.WriteConfigurationComboEdit(Edit: TOlcbComboBox; iPage, iControl: Word);
var
  Stream: TMemoryStream;
  i, iComboBox: Integer;
  Relation: TMapRelation;
  EventID: TEventID;
  HexArray: THexArray;
  Task: TOlcbTaskBase;
begin
  iComboBox := Edit.ItemIndex;
  if iComboBox > -1 then
  begin
    Stream := TMemoryStream.Create;
    try
      case Edit.ConfigInfo.DataType of
        cdt_EventID : begin
                        Relation := Edit.ConfigInfo.MapList.FindMapByValue( Edit.Items[iComboBox]);
                        if Assigned(Relation) then
                        begin
                          EventID := DotHexToEvent(Relation.Prop);

                          for i := 0 to MAX_EVENT_LEN - 1 do
                            Stream.WriteByte( EventID[i]);
                          Stream.Position := 0;
                        end;
                      end;
        cdt_Int     : begin
                        Relation := Edit.ConfigInfo.MapList.FindMapByValue( Edit.Items[iComboBox]);
                        if Assigned(Relation) then
                        begin
                          HexArray := StrToHexArray( Relation.Prop);

                          for i := Edit.ConfigInfo.ConfigMemSize - 1 downto 0  do
                           Stream.WriteByte( HexArray[i]);
                          Stream.Position := 0;
                        end;
                      end;
        cdt_String  : begin
                        Relation := Edit.ConfigInfo.MapList.FindMapByValue( Edit.Items[iComboBox]);
                        if Assigned(Relation) then
                        begin
                          for i := 1 to Length(Relation.Prop) do
                            Stream.WriteByte( Ord( Relation.Prop[i]));
                          // Add Null
                          Stream.WriteByte( Ord( #0));
                          Stream.Position := 0;
                        end;
                      end;
        cdt_Bit     : begin
                        // TODO
                      end;
      end;
      Task := TWriteAddressSpaceMemoryRawTask.Create(GlobalSettings.General.AliasIDAsVal, AliasID, True, MSI_CONFIG, Edit.ConfigInfo.ConfigMemAddress, Stream);
      Task.OnBeforeDestroy := @OnBeforeDestroyTask;
      Task.Tag := iPage or (iControl shl 16);
      Task.RemoveKey := PtrInt( Self);
      Task.OwnerControl := Edit;
      QueueConfigWriteTask(Task);
    finally
      Stream.Free;
    end;
  end;
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

procedure TFormTrainConfigEditor.QueueConfigWriteTask(Task: TOlcbTaskBase);
begin
  if ConfigWriteTaskRunning then
    ConfigWriteTaskQueue.Add(Task)        // Need to allow node to finish write and send the ACK before sending next
  else begin
    DispatchTask(Task);
    ConfigWriteTaskRunning := True
  end;
end;

procedure TFormTrainConfigEditor.QueueConfigReadTask(Task: TOlcbTaskBase);
begin
  if ConfigReadTaskRunning then
    ConfigReadTaskQueue.Add(Task)        // Need to allow node to finish read and send the ACK before sending next
  else begin
    DispatchTask(Task);
    ConfigReadTaskRunning := True
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

procedure TFormTrainConfigEditor.InitTransportLayers(AnEthernetHub: TEthernetHub; AComPortThread: TComPortHub; ADispatchTaskFunc: TDispatchTaskFunc);
begin
  FDispatchTask := ADispatchTaskFunc;
  FEthernetHub := AnEthernetHub;
  FComPortHub := AComPortThread;
end;

end.

