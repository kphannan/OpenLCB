unit unitDebugLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormDebugLogger }

  TFormDebugLogger = class(TForm)
    Memo: TMemo;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormDebugLogger: TFormDebugLogger;

implementation

{$R *.lfm}

end.

