unit form_comportsettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, synaser;

type

  { TFormComPort }

  TFormComPort = class(TForm)
    ButtonRescanPorts: TButton;
    CheckBoxFlowControl: TCheckBox;
    ComboBoxBaud: TComboBox;
    ComboBoxPorts: TComboBox;
    EditCustomBaudRate: TEdit;
    Label1: TLabel;
    LabelBaud: TLabel;
    LabelCustomBaud: TLabel;
    procedure ButtonRescanPortsClick(Sender: TObject);
    procedure ComboBoxBaudChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ScanPorts;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormComPort: TFormComPort;

implementation

{$R *.lfm}

{ TFormComPort }

procedure TFormComPort.FormShow(Sender: TObject);
begin
  ScanPorts
end;

procedure TFormComPort.ScanPorts;
begin
  ComboBoxPorts.Items.Delimiter:=';';
  ComboBoxPorts.Items.DelimitedText := StringReplace(GetSerialPortNames, ',', ';', [rfReplaceAll, rfIgnoreCase]);
  if ComboBoxPorts.Items.Count > 0 then
    ComboBoxPorts.ItemIndex:= 0;
end;

procedure TFormComPort.ButtonRescanPortsClick(Sender: TObject);
begin
  ScanPorts
end;

procedure TFormComPort.ComboBoxBaudChange(Sender: TObject);
begin
  if ComboBoxBaud.ItemIndex = 0 then
  begin
    EditCustomBaudRate.Enabled := True;
    LabelCustomBaud.Enabled := True;
  end else
  begin
    EditCustomBaudRate.Enabled := False;
    LabelCustomBaud.Enabled := False;
  end;
end;

end.
