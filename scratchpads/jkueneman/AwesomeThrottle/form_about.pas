unit form_about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, LCLIntf;

type

  { TFormAbout }

  TFormAbout = class(TForm)
    ImageOpenLCB: TImage;
    LabelBuild: TLabel;
    LabelBuildDate: TLabel;
    LabelCPU: TLabel;
    LabelIcon: TLabel;
    LabelMyName: TLabel;
    LabelNodeExplorer: TLabel;
    LabelTargetCPU: TLabel;
    LabelTargetOperatingSystem: TLabel;
    LabelTargetOS: TLabel;
    LabelURLFreePascal: TLabel;
    LabelURLIcons: TLabel;
    LabelURLLazarus: TLabel;
    LabelWrittenIn: TLabel;
    procedure FormShow(Sender: TObject);
    procedure ImageOpenLCBClick(Sender: TObject);
    procedure LabelURLFreePascalClick(Sender: TObject);
    procedure LabelURLFreePascalMouseEnter(Sender: TObject);
    procedure LabelURLFreePascalMouseLeave(Sender: TObject);
    procedure LabelURLIconsClick(Sender: TObject);
    procedure LabelURLIconsMouseEnter(Sender: TObject);
    procedure LabelURLIconsMouseLeave(Sender: TObject);
    procedure LabelURLLazarusClick(Sender: TObject);
    procedure LabelURLLazarusMouseEnter(Sender: TObject);
    procedure LabelURLLazarusMouseLeave(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormAbout: TFormAbout;

implementation

{$R *.lfm}

{ TFormAbout }

procedure TFormAbout.LabelURLFreePascalMouseEnter(Sender: TObject);
begin
  LabelURLFreePascal.Font.Style := [fsUnderline];
end;

procedure TFormAbout.LabelURLFreePascalClick(Sender: TObject);
begin
  OpenURL('http://' + LabelURLFreePascal.Caption);
end;

procedure TFormAbout.ImageOpenLCBClick(Sender: TObject);
begin
  OpenURL('http://www.openlcb.org');
end;

procedure TFormAbout.FormShow(Sender: TObject);
begin
  LabelBuildDate.Caption := {$I %DATE%} + ': ' + {$I %TIME%};
  LabelTargetOS.Caption := {$I %FPCTARGETOS%};
  LabelTargetCPU.Caption := {$I %FPCTARGETCPU%};
end;

procedure TFormAbout.LabelURLFreePascalMouseLeave(Sender: TObject);
begin
   LabelURLFreePascal.Font.Style := [];
end;

procedure TFormAbout.LabelURLIconsClick(Sender: TObject);
begin
  OpenURL('http://' + LabelURLIcons.Caption);
end;

procedure TFormAbout.LabelURLIconsMouseEnter(Sender: TObject);
begin
  LabelURLIcons.Font.Style := [fsUnderline];
end;

procedure TFormAbout.LabelURLIconsMouseLeave(Sender: TObject);
begin
  LabelURLIcons.Font.Style := [];
end;

procedure TFormAbout.LabelURLLazarusClick(Sender: TObject);
begin
  OpenURL('http://' + LabelURLLazarus.Caption);
end;

procedure TFormAbout.LabelURLLazarusMouseEnter(Sender: TObject);
begin
   LabelURLLazarus.Font.Style := [fsUnderline];
end;

procedure TFormAbout.LabelURLLazarusMouseLeave(Sender: TObject);
begin
   LabelURLLazarus.Font.Style := [];
end;

end.

