unit unitrestartnodeform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TFormRestartNode }

  TFormRestartNode = class(TForm)
    ImageOpenLCB: TImage;
    LabelRestart: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormRestartNode: TFormRestartNode;

implementation

{$R *.lfm}

end.

