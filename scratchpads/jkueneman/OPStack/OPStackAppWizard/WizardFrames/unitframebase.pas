unit unitframebase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls;

type

  { TFrameBase }

  TFrameBase = class(TFrame)
  private
    { private declarations }
  public
    { public declarations }
    procedure UpdateUI; virtual;
  end;

implementation

{$R *.lfm}

{ TFrameBase }

procedure TFrameBase.UpdateUI;
begin

end;

end.

