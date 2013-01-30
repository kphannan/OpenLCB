unit form_effects_editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Buttons, StdCtrls, ActnList, Grids, laz2_DOM, laz2_XMLRead,
  laz2_XMLWrite, olcb_threaded_stack, form_settings, olcb_common_tasks,
  olcb_app_common_settings, olcb_defines;

type
  TFormEffectsEditor = class;

  TOnFormEffectsEditorEvent = procedure(EffectsEditor: TFormEffectsEditor) of object;

  { TFormEffectsEditor }

  TFormEffectsEditor = class(TForm)
    ActionGlobalClose: TAction;
    ActionGlobalSaveChanges: TAction;
    ActionListEffectsEditor: TActionList;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    LabelReadingConfigMem: TLabel;
    ListViewEffects: TListView;
    Panel1: TPanel;
    PanelSave: TPanel;
    ScrollBoxBkgnd: TScrollBox;
    SplitterEffectsEditor: TSplitter;
    procedure ActionGlobalCloseExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FCdiXML: TXMLDocument;
    FComPortThread: TComPortThread;
    FDirty: Boolean;
    FNodeAlias: Word;
    FOnEffectsEditorClose: TOnFormEffectsEditorEvent;
    { private declarations }
  protected
    property CdiXML: TXMLDocument read FCdiXML write FCdiXML;

    procedure OnBeforeDestroyTask(Sender: TOlcbTaskBase);
  public
    { public declarations }
    property ComPortThread: TComPortThread read FComPortThread write FComPortThread;
    property IsDirty: Boolean read FDirty;
    property NodeAlias: Word read FNodeAlias write FNodeAlias;
    property OnEffectsEditorClose: TOnFormEffectsEditorEvent read FOnEffectsEditorClose write FOnEffectsEditorClose;

    procedure ParseXML;
  end;

implementation

{$R *.lfm}

{ TFormEffectsEditor }

procedure TFormEffectsEditor.ActionGlobalCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormEffectsEditor.Button1Click(Sender: TObject);
begin
  // Save Changes here
end;

procedure TFormEffectsEditor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  if Assigned(OnEffectsEditorClose) then
    OnEffectsEditorClose(Self);
end;

procedure TFormEffectsEditor.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if IsDirty then
  begin
    // Show Dirty Message Box
  end;
end;

procedure TFormEffectsEditor.FormCreate(Sender: TObject);
begin
  FDirty := False;
  FNodeAlias := 0;
  FComPortThread := nil;
  FOnEffectsEditorClose := nil;
  CdiXML := TXMLDocument.Create;
end;

procedure TFormEffectsEditor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FCdiXML);
end;

procedure TFormEffectsEditor.FormShow(Sender: TObject);
var
  Task: TReadAddressSpaceMemoryTask;
begin
  Task := TReadAddressSpaceMemoryTask.Create(GlobalSettings.General.AliasIDAsVal, NodeAlias, True, MSI_CDI);
  Task.ForceOptionalSpaceByte := False;
  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
  ComPortThread.AddTask(Task);
end;

procedure TFormEffectsEditor.OnBeforeDestroyTask(Sender: TOlcbTaskBase);
var
  MemTask: TReadAddressSpaceMemoryTask;
begin
  if Sender is TReadAddressSpaceMemoryTask then
  begin
    MemTask := TReadAddressSpaceMemoryTask( Sender);
    try
    // MemConfigViewer.Caption := MemConfigViewer.Caption + ' - Alias:  0x' + IntToHex(MemTask.DestinationAlias, 4);
      MemTask.DataStream.Position := 0;
      MemTask.DataStream.Size := MemTask.Datastream.Size - 1;  // Strip the null
      ReadXMLFile(FCdiXML, MemTask.DataStream);                 // This corrupts the stream from its original contents
      WriteXMLFile(FCdiXML, MemTask.DataStream);
      ParseXML;
    except
      FreeAndNil(FCdiXML);
    end;
  end
end;

procedure TFormEffectsEditor.ParseXML;
var
  i: Integer;
begin
  ListViewEffects.Items.BeginUpdate;
  try
    ListViewEffects.Items.Clear;
  finally
    ListViewEffects.Items.EndUpdate;
  end;

  ScrollBoxBkgnd.BeginUpdateBounds;
  try
    for i := ScrollBoxBkgnd.ControlCount - 1 downto 0 do
      ScrollBoxBkgnd.Controls[i].Free;
  finally
    ScrollBoxBkgnd.EndUpdateBounds;
  end;

  ScrollBoxBkgnd.BeginUpdateBounds;
  try

    ShowMessage('Parsing XML');

       // Add Child Controls here.

  finally
    ScrollBoxBkgnd.EndUpdateBounds;
  end;
end;

end.

