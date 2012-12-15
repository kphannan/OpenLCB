unit form_thread_debug;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormThreadDebug }

  TFormThreadDebug = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    LabelDatagramReceiveObjectsMax: TLabel;
    LabelDatagramSendObjectsMax: TLabel;
    LabelTaskObjects: TLabel;
    LabelDatagramReceiveObjects: TLabel;
    Label3: TLabel;
    LabelDatagramSendObjects: TLabel;
    LabelSNIIObjects: TLabel;
    LabelTaskObjectsMax: TLabel;
    LabelSNIIObjectsMax: TLabel;
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

