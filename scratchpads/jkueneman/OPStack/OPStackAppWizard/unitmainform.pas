unit unitmainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, unitframewelcome, unitframenodes, unitframevirtualnodes, unitframebase,
  unitframepip, unitframebuffers;

type

  { TFrameReel }

  TFrameReel = class
  private
    FReel: TList;
    function GetCount: Integer;
  protected
    procedure ClearReel;
    property Reel: TList read FReel write FReel;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddFrame(Frame: TFrame);
    function FrameByIndex(iFrame: Integer): TFrameBase;
    property Count: Integer read GetCount;
  end;

type

  { TFormMainForm }

  TFormMainForm = class(TForm)
    ButtonNextFrame: TButton;
    ButtonPreviousFrame: TButton;
    PanelWizardFrame: TPanel;
    procedure ButtonNextFrameClick(Sender: TObject);
    procedure ButtonPreviousFrameClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FCurrentFrame: Integer;
    FFrameReel: TFrameReel;
    { private declarations }
  public
    { public declarations }
    procedure ShowFrameByIndex(iFrame: Integer);
    procedure UpdateUI;
    property CurrentFrame: Integer read FCurrentFrame write FCurrentFrame;
    property FrameReel: TFrameReel read FFrameReel write FFrameReel;
  end;

var
  FormMainForm: TFormMainForm;

implementation

{$R *.lfm}

{ TFrameReel }

function TFrameReel.GetCount: Integer;
begin
  Result := Reel.Count;
end;

procedure TFrameReel.ClearReel;
var
  i: Integer;
begin
  try
    for i := 0 to Reel.Count - 1 do
      TObject( Reel[i]).Free;
  finally
    Reel.Clear;
  end;
end;

constructor TFrameReel.Create;
begin
  FReel := TList.Create;
end;

destructor TFrameReel.Destroy;
begin
  ClearReel;
  FreeAndNil(FReel);
  inherited Destroy;
end;

procedure TFrameReel.AddFrame(Frame: TFrame);
begin
  Reel.Add(Frame);
end;

function TFrameReel.FrameByIndex(iFrame: Integer): TFrameBase;
begin
  if (iFrame < Reel.Count) and (iFrame > -1) then
    Result := TFrameBase( Reel[iFrame])
  else
    Result := nil;
end;

{ TFormMainForm }

procedure TFormMainForm.FormShow(Sender: TObject);
begin
  ShowFrameByIndex(0);
end;

procedure TFormMainForm.ShowFrameByIndex(iFrame: Integer);
var
  Frame, OldFrame: TFrame;
begin
  Frame := FrameReel.FrameByIndex(iFrame);
  if Assigned(Frame) then
  begin
    OldFrame := FrameReel.FrameByIndex(CurrentFrame);
    if Assigned(OldFrame) then
    begin
      OldFrame.Hide;
      OldFrame.Parent := nil;
    end;
    CurrentFrame := iFrame;
    Frame.Parent := PanelWizardFrame;
    Frame.Align := alClient;
    Frame.Show;
    UpdateUI;
  end;
end;

procedure TFormMainForm.UpdateUI;
var
  LocalFrame: TFrameBase;
begin
  ButtonPreviousFrame.Enabled := CurrentFrame > 0;
  ButtonNextFrame.Enabled := CurrentFrame < FrameReel.Count - 1;
  LocalFrame := FrameReel.FrameByIndex(CurrentFrame);
  if Assigned(LocalFrame) then
    LocalFrame.UpdateUI;
end;

procedure TFormMainForm.FormCreate(Sender: TObject);
begin
  CurrentFrame := -1;
  FrameReel := TFrameReel.Create;
  FrameReel.AddFrame( TFrameWelcome.Create(nil));
  FrameReel.AddFrame( TFrameNodes.Create(nil));
  FrameReel.AddFrame( TFrameVirtualNodes.Create(nil));
  FrameReel.AddFrame( TFramePIP.Create(nil));
  FrameReel.AddFrame( TFrameBufferManagement.Create(nil));
end;

procedure TFormMainForm.ButtonNextFrameClick(Sender: TObject);
begin
  ShowFrameByIndex(CurrentFrame + 1);
end;

procedure TFormMainForm.ButtonPreviousFrameClick(Sender: TObject);
begin
  ShowFrameByIndex(CurrentFrame - 1);
end;

procedure TFormMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FFrameReel);
end;

end.

