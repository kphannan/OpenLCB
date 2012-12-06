unit unitsettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Spin,
  StdCtrls, nodeexplorer_settings;

type

  { TFormSettings }

  TFormSettings = class(TForm)
    CheckBoxSoftwareFlowControl: TCheckBox;
    CheckBoxPingPongStandardFrame: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    SpinEditDelayTimeout: TSpinEdit;
    SpinEditDelayTimeoutRID: TSpinEdit;
    SpinEditDelayUIRefreshRate: TSpinEdit;
    procedure CheckBoxSoftwareFlowControlChange(Sender: TObject);
    procedure CheckBoxPingPongStandardFrameChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinEditDelayTimeoutChange(Sender: TObject);
    procedure SpinEditDelayTimeoutRIDChange(Sender: TObject);
    procedure SpinEditDelayUIRefreshRateChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormSettings: TFormSettings;

implementation

{$R *.lfm}

{ TFormSettings }

procedure TFormSettings.SpinEditDelayTimeoutChange(Sender: TObject);
begin
  Settings.TimeoutComRead := SpinEditDelayTimeout.Value;
end;

procedure TFormSettings.SpinEditDelayTimeoutRIDChange(Sender: TObject);
begin
  Settings.TimeoutStartupRID := SpinEditDelayTimeoutRID.Value;
end;

procedure TFormSettings.SpinEditDelayUIRefreshRateChange(Sender: TObject);
begin
  Settings.UIRefreshRate := SpinEditDelayUIRefreshRate.Value;
end;

procedure TFormSettings.FormShow(Sender: TObject);
begin
  SpinEditDelayTimeout.Value := Settings.TimeoutComRead;
  SpinEditDelayTimeoutRID.Value := Settings.TimeoutStartupRID;
  SpinEditDelayUIRefreshRate.Value := Settings.UIRefreshRate;
  CheckBoxPingPongStandardFrame.Checked := Settings.PingPongStandardFrameTest;
  CheckBoxSoftwareFlowControl.Checked := Settings.SoftwareFlowControl;
end;


procedure TFormSettings.CheckBoxPingPongStandardFrameChange(Sender: TObject);
begin
  Settings.PingPongStandardFrameTest := CheckBoxPingPongStandardFrame.Checked;
end;

procedure TFormSettings.CheckBoxSoftwareFlowControlChange(Sender: TObject);
begin
  Settings.SoftwareFlowControl := CheckBoxSoftwareFlowControl.Checked;;
end;

end.

