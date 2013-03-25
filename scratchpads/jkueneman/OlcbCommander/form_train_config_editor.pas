unit form_train_config_editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, laz2_DOM, laz2_XMLRead,
  olcb_threaded_stack, olcb_common_tasks, olcb_app_common_settings,
  olcb_utilities, olcb_defines, unit_cdi_parser;

type
  TFormTrainConfigEditor = class;

  TOnConfigEditorEvent = procedure(Throttle: TFormTrainConfigEditor) of object;

  { TFormTrainConfigEditor }

  TFormTrainConfigEditor = class(TForm)
    ButtonReadAll: TButton;
    ButtonStopRead: TButton;
    ButtonWriteAll: TButton;
    ButtonReadPage: TButton;
    ButtonWritePage: TButton;
    PanelBkGnd: TPanel;
    procedure ButtonReadAllClick(Sender: TObject);
    procedure ButtonReadPageClick(Sender: TObject);
    procedure ButtonStopReadClick(Sender: TObject);

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FAliasID: Word;
    FCdiParser: TCdiParser;
    FComPortThread: TComPortThread;
    FOnConfigEditorClose: TOnConfigEditorEvent;
    FOnConfigEditorHide: TOnConfigEditorEvent;
    FPageControl: TPageControl;
    FShownOnce: Boolean;
    { private declarations }
  protected
    function FindScrollBox(Page: TTabSheet): TScrollBox;
    procedure OnBeforeDestroyTask(Sender: TOlcbTaskBase);
    procedure ReadConfiguration;
    procedure ReadConfigurationPage(iPage: Integer);

    property CdiParser: TCdiParser read FCdiParser write FCdiParser;
    property ShownOnce: Boolean read FShownOnce write FShownOnce;
    property PageControl: TPageControl read FPageControl write FPageControl;
  public
    { public declarations }
    property AliasID: Word read FAliasID write FAliasID;
    property ComPortThread: TComPortThread read FComPortThread write FComPortThread;
    property OnConfigEditorHide: TOnConfigEditorEvent read FOnConfigEditorHide write FOnConfigEditorHide;
    property OnConfigEditorClose: TOnConfigEditorEvent read FOnConfigEditorClose write FOnConfigEditorClose;
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
  FComPortThread := nil;
  FAliasID := 0;
  FShownOnce := False;
  FCdiParser := TCdiParser.Create;
  FPageControl := nil;
end;

procedure TFormTrainConfigEditor.FormDestroy(Sender: TObject);
begin
  FreeAndNil( FCdiParser);
end;

procedure TFormTrainConfigEditor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(OnConfigEditorClose) then
    OnConfigEditorClose(Self);
  CloseAction := caFree;
end;

procedure TFormTrainConfigEditor.ButtonReadPageClick(Sender: TObject);
begin
 if Assigned(FPageControl) then
  begin
    if PageControl.ActivePageIndex > -1 then
      ReadConfigurationPage(PageControl.ActivePageIndex)
  end;
end;

procedure TFormTrainConfigEditor.ButtonStopReadClick(Sender: TObject);
begin
  if Assigned(ComPortThread) then
    ComPortThread.RemoveTasks( PtrInt( Self));
end;

procedure TFormTrainConfigEditor.ButtonReadAllClick(Sender: TObject);
begin
  ReadConfiguration;
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
  if Assigned(ComPortThread) and not ShownOnce then
  begin
    Task := TReadAddressSpaceMemoryTask.Create(GlobalSettings.General.AliasIDAsVal, AliasID, True, MSI_CDI, True);
    Task.RemoveKey := PtrInt( Self);
    Task.Terminator := #0;
    Task.ForceOptionalSpaceByte := False;
    Task.OnBeforeDestroy := @OnBeforeDestroyTask;
    ComPortThread.AddTask(Task);
    ButtonReadAll.Enabled := False;
    ButtonReadPage.Enabled := False;
    ButtonStopRead.Enabled := False;
    ButtonWriteAll.Enabled := False;
    ButtonWritePage.Enabled := False;
  end;
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
          MemTask.DataStream.Position := MemTask.DataStream.Size - 1;
          if MemTask.DataStream.ReadByte = Ord( #0) then
            MemTask.DataStream.Size:=MemTask.Datastream.Size - 1;  // Strip the null
          MemTask.DataStream.Position := 0;
          ReadXMLFile(ADoc, MemTask.DataStream);                 // This corrupts the stream from its original contents
          PageControl := CdiParser.Build_CDI_Interface(PanelBkGnd, ADoc);
          ButtonReadAll.Enabled := True;
          ButtonReadPage.Enabled := True;
          ButtonStopRead.Enabled := True;
          ButtonWriteAll.Enabled := True;
          ButtonWritePage.Enabled := True;
          ReadConfiguration;
          FreeAndNil(ADoc);
        end
      end else
      if Sender is TReadAddressSpaceMemoryRawTask then
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
                if (Control as TOlcbEdit).ConfigInfo.Task = Sender then
                begin
                  // Whew, success
                  TaskStream := TReadAddressSpaceMemoryRawTask(Sender).Stream;
                  TaskStream.Position := 0;
                  case (Control as TOlcbEdit).ConfigInfo.DataType of
                    cdt_Int,
                    cdt_EventID : begin
                                    Str := IntToHex(TaskStream.ReadByte, 2);
                                    while (TaskStream.Position < TaskStream.Size) do
                                      Str := Str + '.' + IntToHex(TaskStream.ReadByte, 2);
                                    (Control as TOlcbEdit).Text := Str;
                                  end;
                    cdt_String  : begin
                                    Str := Char( TaskStream.ReadByte);
                                    while (TaskStream.Position < TaskStream.Size) do
                                      Str := Str + Char( TaskStream.ReadByte);
                                    (Control as TOlcbEdit).Text := Str;
                                  end;
                  end;
                end
              end else
              if Control is TOlcbSpinEdit then
              begin
                 if (Control as TOlcbSpinEdit).ConfigInfo.Task = Sender then
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
                  end;
                end
              end else
              if Control is TOlcbComboBox then
              begin
                if (Control as TOlcbComboBox).ConfigInfo.Task = Sender then
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
                  end
                end
              end
            end
          end;
        end;
      end;
    end;
  except
  end;
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
      begin
        (Control as TOlcbEdit).ConfigInfo.Task := TReadAddressSpaceMemoryRawTask.Create(GlobalSettings.General.AliasIDAsVal, AliasID, True, MSI_CONFIG, (Control as TOlcbEdit).ConfigInfo.ConfigMemAddress, (Control as TOlcbEdit).ConfigInfo.ConfigMemSize, False);
        (Control as TOlcbEdit).ConfigInfo.Task.OnBeforeDestroy := @OnBeforeDestroyTask;
        (Control as TOlcbEdit).ConfigInfo.Task.Tag := iPage or (iControl shl 16);
        (Control as TOlcbEdit).ConfigInfo.Task.RemoveKey := PtrInt( Self);
        ComPortThread.AddTask( (Control as TOlcbEdit).ConfigInfo.Task);
      end;
      if Control is TOlcbSpinEdit then
      begin
        (Control as TOlcbSpinEdit).ConfigInfo.Task := TReadAddressSpaceMemoryRawTask.Create(GlobalSettings.General.AliasIDAsVal, AliasID, True, MSI_CONFIG, (Control as TOlcbSpinEdit).ConfigInfo.ConfigMemAddress, (Control as TOlcbSpinEdit).ConfigInfo.ConfigMemSize, False);
        (Control as TOlcbSpinEdit).ConfigInfo.Task.OnBeforeDestroy := @OnBeforeDestroyTask;
        (Control as TOlcbSpinEdit).ConfigInfo.Task.Tag := iPage or (iControl shl 16);
        (Control as TOlcbSpinEdit).ConfigInfo.Task.RemoveKey := PtrInt( Self);
        ComPortThread.AddTask( (Control as TOlcbSpinEdit).ConfigInfo.Task);
      end;
      if Control is TOlcbComboBox then
      begin
        (Control as TOlcbComboBox).ConfigInfo.Task := TReadAddressSpaceMemoryRawTask.Create(GlobalSettings.General.AliasIDAsVal, AliasID, True, MSI_CONFIG, (Control as TOlcbComboBox).ConfigInfo.ConfigMemAddress, (Control as TOlcbComboBox).ConfigInfo.ConfigMemSize, False);
        (Control as TOlcbComboBox).ConfigInfo.Task.OnBeforeDestroy := @OnBeforeDestroyTask;
        (Control as TOlcbComboBox).ConfigInfo.Task.Tag := iPage or (iControl shl 16);
        (Control as TOlcbComboBox).ConfigInfo.Task.RemoveKey := PtrInt( Self);
        ComPortThread.AddTask( (Control as TOlcbComboBox).ConfigInfo.Task);
      end;
    end;
  end;
end;

end.

