unit form_aliastree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Buttons;

type

  { TFormAliasTree }

  TFormAliasTree = class(TForm)
    BitBtnClear: TBitBtn;
    BitBtnRebuild: TBitBtn;
    TreeView: TTreeView;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormAliasTree: TFormAliasTree;

implementation

{$R *.lfm}

{ TFormAliasTree }



end.

