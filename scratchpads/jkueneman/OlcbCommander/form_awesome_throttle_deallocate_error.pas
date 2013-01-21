unit form_awesome_throttle_deallocate_error;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormDeallocationError }

  TFormDeallocationError = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    LabelErrorMsg: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end;


implementation

{$R *.lfm}

end.

