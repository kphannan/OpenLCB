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
    TreeViewTrainList: TTreeView;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormTrainSelector: TFormTrainSelector;

implementation

{$R *.lfm}

end.

