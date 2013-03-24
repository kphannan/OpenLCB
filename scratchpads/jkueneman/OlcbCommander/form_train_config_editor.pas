unit form_train_config_editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, laz2_DOM, laz2_XMLRead, laz2_XMLWrite,
  olcb_threaded_stack, olcb_common_tasks, olcb_app_common_settings,
  olcb_utilities, olcb_defines, unit_cdi_parser;

type

  { TFormTrainConfigEditor }

  TFormTrainConfigEditor = class(TForm)
    ButtonReadCVs: TButton;
    PanelBkGnd: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FAliasID: Word;
    FComPortThread: TComPortThread;
    FShownOnce: Boolean;
    { private declarations }
  protected
    procedure OnBeforeDestroyTask(Sender: TOlcbTaskBase);

    property ShownOnce: Boolean read FShownOnce write FShownOnce;
  public
    { public declarations }
    property AliasID: Word read FAliasID write FAliasID;
    property ComPortThread: TComPortThread read FComPortThread write FComPortThread;
  end;

implementation

{$R *.lfm}

{ TFormTrainConfigEditor }

procedure TFormTrainConfigEditor.FormCreate(Sender: TObject);
begin
  FComPortThread := nil;
  FAliasID := 0;
  FShownOnce := False;
end;

procedure TFormTrainConfigEditor.FormShow(Sender: TObject);
var
  Task: TReadAddressSpaceMemoryTask;
begin
  if Assigned(ComPortThread) and not ShownOnce then
  begin
    Task := TReadAddressSpaceMemoryTask.Create(GlobalSettings.General.AliasIDAsVal, AliasID, True, MSI_CDI);
    Task.ForceOptionalSpaceByte := False;
    Task.OnBeforeDestroy := @OnBeforeDestroyTask;
    ComPortThread.AddTask(Task);
  end;
end;

procedure TFormTrainConfigEditor.OnBeforeDestroyTask(Sender: TOlcbTaskBase);
var
  MemTask: TReadAddressSpaceMemoryTask;
  ADoc: TXMLDocument;
begin
  if Sender is TReadAddressSpaceMemoryTask then
  begin
    MemTask := TReadAddressSpaceMemoryTask(Sender);
    MemTask.DataStream.Position := MemTask.DataStream.Size - 1;
    if MemTask.DataStream.ReadByte = Ord( #0) then
      MemTask.DataStream.Size:=MemTask.Datastream.Size - 1;  // Strip the null
    MemTask.DataStream.Position := 0;
    ReadXMLFile(ADoc, MemTask.DataStream);                 // This corrupts the stream from its original contents
    Build_CDI_Interface(PanelBkGnd, ADoc);
  //  ShowMessage('Read CDI');
    FreeAndNil(ADoc);
  end;
end;

end.

