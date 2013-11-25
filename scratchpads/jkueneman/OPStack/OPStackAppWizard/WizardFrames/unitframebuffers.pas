unit unitframebuffers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, unitframebase;

type

  { TFrameBufferManagement }

  TFrameBufferManagement = class(TFrameBase)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    SpinEditStreamSize: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEditSnipBuffers: TSpinEdit;
    SpinEditMessageBuffers: TSpinEdit;
    SpinEditCANBuffer: TSpinEdit;
    SpinEditDatagramBuffers: TSpinEdit;
    SpinEditStreamBuffers: TSpinEdit;
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

end.

