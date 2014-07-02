unit form_train_selector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls;

type

  { TFormTrainSelector }

  TFormTrainSelector = class(TForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    StatusBar: TStatusBar;
    TreeViewTrainList: TTreeView;
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

procedure TFormTrainSelector.UpdateStatus(iPanel: Integer; Status: String);
begin
  if iPanel < StatusBar.Panels.Count then
    StatusBar.Panels[iPanel].Text := Status;
end;

end.

