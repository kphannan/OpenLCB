unit form_thread_debug;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormThreadDebug }

  TFormThreadDebug = class(TForm)
    Label1: TLabel;
    Label10: TLabel;
    LabelMaxTime: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label9: TLabel;
    LabelDatagramReceiveObjectsMax: TLabel;
    LabelDatagramSendObjectsMax: TLabel;
    LabelLoopTime: TLabel;
    LabelTaskObjects: TLabel;
    LabelDatagramReceiveObjects: TLabel;
    Label3: TLabel;
    LabelDatagramSendObjects: TLabel;
    LabelTaskObjectsMax: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormThreadDebug: TFormThreadDebug;

implementation

{$R *.lfm}

end.

