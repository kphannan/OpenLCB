unit form_fdi_picker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormFdiPicker }

  TFormFdiPicker = class(TForm)
    ButtonBrowser: TButton;
    ButtonUseThrottle: TButton;
    ButtonUseTrain: TButton;
    ButtonUseCustom: TButton;
    LabelHeader: TLabel;
    Label2: TLabel;
    OpenDialog: TOpenDialog;
    procedure ButtonUseCustomClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormFdiPicker: TFormFdiPicker;

implementation

{$R *.lfm}

{ TFormFdiPicker }

procedure TFormFdiPicker.ButtonUseCustomClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    ButtonBrowser.Click;
end;

end.

