unit unitframepip;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, unitframebase;

type

  { TFramePIP }

  TFramePIP = class(TFrameBase)
    CheckGroupPipNode: TCheckGroup;
    CheckGroupPipVirtualNodes: TCheckGroup;
    PageControlPIP: TPageControl;
    TabSheetNode: TTabSheet;
    TabSheetVirtualNodes: TTabSheet;
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

end.

