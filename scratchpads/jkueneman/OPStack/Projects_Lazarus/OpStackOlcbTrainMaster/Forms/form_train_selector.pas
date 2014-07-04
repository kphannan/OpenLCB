unit form_train_selector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, olcb_transport_layer;

type

  { TFormTrainSelector }

  TFormTrainSelector = class(TForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    StatusBar: TStatusBar;
    TreeViewTrainList: TTreeView;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TreeViewTrainListChange(Sender: TObject; Node: TTreeNode);
  private
    { private declarations }
  public
    { public declarations }
    procedure UpdateStatus(iPanel: Integer; Status: String);
  end;

var
  FormTrainSelector: TFormTrainSelector;

implementation

{$R *.lfm}

{ TFormTrainSelector }

procedure TFormTrainSelector.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to TreeViewTrainList.Items.Count - 1 do
  begin
    TObject( TreeViewTrainList.Items[i].Data).Free;
    TreeViewTrainList.Items[i].Data := nil;
  end;
end;

procedure TFormTrainSelector.FormShow(Sender: TObject);
begin
  ButtonOk.Enabled := TreeViewTrainList.SelectionCount > 0;
end;

procedure TFormTrainSelector.TreeViewTrainListChange(Sender: TObject;Node: TTreeNode);
begin
  ButtonOk.Enabled := TreeViewTrainList.SelectionCount > 0;
end;

procedure TFormTrainSelector.UpdateStatus(iPanel: Integer; Status: String);
begin
  if iPanel < StatusBar.Panels.Count then
    StatusBar.Panels[iPanel].Text := Status;
end;

end.

