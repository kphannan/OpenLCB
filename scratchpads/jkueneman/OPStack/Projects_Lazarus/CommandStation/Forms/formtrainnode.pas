unit formtrainnode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls,
  template_userstatemachine, Float16;

type
  TFormIsTrainNode = class;

  TOnTrainEvent = procedure(TrainNode: TFormIsTrainNode) of object;

  { TTrainNodeList }

  TTrainNodeList = class(TList)
  private
    FOnTrainClose: TOnTrainEvent;
    FOnTrainHide: TOnTrainEvent;
    function GetTrains(Index: Integer): TFormIsTrainNode;
    procedure SetTrains(Index: Integer; AValue: TFormIsTrainNode);
  protected
    procedure DoTrainClose(Train: TFormIsTrainNode);
    procedure DoTrainHide(Train: TFormIsTrainNode);
  public
    constructor Create; virtual;
    function CreateTrain(ImageList16x16: TImageList): TFormIsTrainNode;
    procedure Clear; override;
    procedure CloseTrain(Train: TFormIsTrainNode);
    procedure HideAll;
    procedure CloseAll;
    procedure ShowAll;
    property Trains[Index: Integer]: TFormIsTrainNode read GetTrains write SetTrains;
    property OnTrainHide: TOnTrainEvent read FOnTrainHide write FOnTrainHide;
    property OnTrainClose: TOnTrainEvent read FOnTrainClose write FOnTrainClose;
  end;

  { TFormIsTrainNode }

  TFormIsTrainNode = class(TForm)
    CheckBoxFakeTrainNodeConnectionFail: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    LabelAddress: TLabel;
    LabelSpeed: TLabel;
    LabelAlias: TLabel;
    LabelDirection: TLabel;
    LabelFunctions1: TLabel;
    LabelSpeedSteps: TLabel;
    LabelFunctions2: TLabel;
    LabelThrottleAlias: TLabel;
    StatusBar: TStatusBar;
  private
    FImageList16x16: TImageList;
    FOnTrainClose: TOnTrainEvent;
    FOnTrainHide: TOnTrainEvent;
    { private declarations }
  public
    { public declarations }
    property ImageList16x16: TImageList read FImageList16x16 write FImageList16x16;
    property OnTrainHide: TOnTrainEvent read FOnTrainHide write FOnTrainHide;
    property OnTrainClose: TOnTrainEvent read FOnTrainClose write FOnTrainClose;
    procedure UpdateStatus(NewStatus: string);
    procedure LoadTrainState(Link: PLinkRec);
  end;

var
  FormIsTrainNode: TFormIsTrainNode;

implementation

{$R *.lfm}

{ TTrainNodeList }

procedure TTrainNodeList.Clear;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
  begin
    Trains[i].OnTrainHide := nil;
    Trains[i].Close;
  end;
  inherited Clear;
end;

procedure TTrainNodeList.CloseAll;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
  begin
    Trains[i].OnTrainHide := nil;
    Trains[i].Close;
  end;
  Clear;
end;

procedure TTrainNodeList.CloseTrain(Train: TFormIsTrainNode);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Trains[i] = Train then
    begin
      Trains[i].OnTrainHide := nil;
      Train.Close;
      Break;
    end;
  end;
end;

constructor TTrainNodeList.Create;
begin
  inherited Create;
  OnTrainClose := nil;
  OnTrainHide := nil;
end;

function TTrainNodeList.CreateTrain(ImageList16x16: TImageList): TFormIsTrainNode;
begin
  Result := TFormIsTrainNode.Create(Application.MainForm);
  if Result <> nil then
  begin
    Self.Add(Result);
    Result.ImageList16x16 := ImageList16x16;
    Result.Show
  end;
end;

procedure TTrainNodeList.DoTrainClose(Train: TFormIsTrainNode);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Trains[i] = Train then
    begin
      if Assigned(OnTrainClose) then
        OnTrainClose(Train);
      Delete(i);
      Break;
    end;
  end;
end;

procedure TTrainNodeList.DoTrainHide(Train: TFormIsTrainNode);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Trains[i] = Train then
    begin
      if Assigned(OnTrainHide) then
        OnTrainHide(Train);
      Break;
    end;
  end;
end;

function TTrainNodeList.GetTrains(Index: Integer): TFormIsTrainNode;
begin
  Result := TFormIsTrainNode( Items[Index]);
end;

procedure TTrainNodeList.HideAll;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Trains[i].Hide
end;

procedure TTrainNodeList.SetTrains(Index: Integer; AValue: TFormIsTrainNode);
begin
  Items[Index] := AValue
end;

procedure TTrainNodeList.ShowAll;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Trains[i].Show;
end;

{ TFormIsTrainNode }

procedure TFormIsTrainNode.LoadTrainState(Link: PLinkRec);
begin
  LabelAlias.Caption := '0x' + IntToHex(Link^.Node.Info.AliasID, 2);
  if Link^.TrainState.SpeedDir and $80 = 0 then
    LabelDirection.Caption := 'Forward'
  else
    LabelDirection.Caption := 'Reverse';
  if Link^.TrainState.Address and $C000 = $C000 then
    LabelAddress.Caption := IntToStr(Link^.TrainState.Address and not $C000) + ' - Long'
  else
    LabelAddress.Caption := IntToStr(Link^.TrainState.Address) + ' - Short';
  LabelFunctions1.Caption := '00000000000000';
  LabelFunctions2.Caption := '00000000000000';
  LabelSpeedSteps.Caption := IntToStr(Link^.TrainState.SpeedSteps);
  LabelSpeed.Caption := IntToStr( Abs( Float16ToInt(Link^.TrainState.SpeedDir)));
end;

procedure TFormIsTrainNode.UpdateStatus(NewStatus: string);
begin
  Statusbar.Panels[1].Text:=NewStatus;
end;

end.

