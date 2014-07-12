unit formtrainnode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls,
  template_userstatemachine, Float16, opstackdefines, olcb_transport_layer;

type
  TFormIsTrainNode = class;

  TOnTrainEvent = procedure(TrainNode: TFormIsTrainNode) of object;

  { TTrainNodeList }

  TTrainNodeList = class(TList)
  private
    FNodeInfoCab: TNodeInfo;
    FNodeInfoTrain: TNodeInfo;
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
    function Find(Address: Word; SpeedSteps: Byte): TFormIsTrainNode;
    procedure HideAll;
    procedure CloseAll;
    procedure ShowAll;
    property NodeInfoTrain: TNodeInfo read FNodeInfoTrain write FNodeInfoTrain;
    property NodeInfoCab: TNodeInfo read FNodeInfoCab write FNodeInfoCab;
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
    LabelAlias: TLabel;
    LabelThrottleAlias: TLabel;
    LabelSpeed: TLabel;
    LabelDirection: TLabel;
    LabelFunctions1: TLabel;
    LabelSpeedSteps: TLabel;
    LabelFunctions2: TLabel;
    StatusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FImageList16x16: TImageList;
    FOnTrainClose: TOnTrainEvent;
    FOnTrainHide: TOnTrainEvent;
    FTrainState: TNodeEventTrainInfo;
    { private declarations }
  public
    { public declarations }
    procedure EventTrainInfo(Event: TNodeEventTrainInfo);
    procedure UpdateStatus(NewStatus: string);
    procedure UpdateUI;
    property ImageList16x16: TImageList read FImageList16x16 write FImageList16x16;
    property OnTrainHide: TOnTrainEvent read FOnTrainHide write FOnTrainHide;
    property OnTrainClose: TOnTrainEvent read FOnTrainClose write FOnTrainClose;
    property TrainState: TNodeEventTrainInfo read FTrainState write FTrainState;
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
  FNodeInfoCab.AliasID := 0;
  FNodeInfoCab.ID := NULL_NODE_ID;
  FNodeInfoTrain.AliasID := 0;
  FNodeInfoTrain.ID := NULL_NODE_ID;
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

function TTrainNodeList.Find(Address: Word; SpeedSteps: Byte): TFormIsTrainNode;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if (Trains[i].TrainState.Address = Address) and (Trains[i].TrainState.SpeedSteps = SpeedSteps) then
    begin
      Result := Trains[i];
      Break
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

procedure TFormIsTrainNode.EventTrainInfo(Event: TNodeEventTrainInfo);
begin
  TrainState.CopyTo(Event);
  UpdateUI
end;

procedure TFormIsTrainNode.FormCreate(Sender: TObject);
var
  Temp: TNodeInfo;
begin
  Temp.ID := NULL_NODE_ID;
  Temp.AliasID := 0;
  TrainState := TNodeEventTrainInfo.Create(Temp, nil);
end;

procedure TFormIsTrainNode.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTrainState);
end;

procedure TFormIsTrainNode.UpdateStatus(NewStatus: string);
begin
  Statusbar.Panels[1].Text:=NewStatus;
end;

procedure TFormIsTrainNode.UpdateUI;
var
  i: Integer;
  Temp: DWORD;
begin
  LabelAlias.Caption := '0x' + IntToHex(TrainState.NodeInfo.AliasID, 2);
  if TrainState.Speed and $8000 = 0 then
    LabelDirection.Caption := 'Forward'
  else
    LabelDirection.Caption := 'Reverse';

  if TrainState.Address and $C000 = $C000 then
    LabelAddress.Caption := IntToStr(TrainState.Address and not $C000) + ' - Long'
  else
    LabelAddress.Caption := IntToStr(TrainState.Address) + ' - Short';

  Temp := TrainState.Functions;
  LabelFunctions1.Caption := '';
  for i := 0 to 13 do
  begin
    if Temp and $00000001 > 0 then
      LabelFunctions1.Caption := LabelFunctions1.Caption + '1'
    else
      LabelFunctions1.Caption := LabelFunctions1.Caption + '0';
    Temp := Temp shr 1;
  end;

  LabelFunctions2.Caption := '';
  for i := 0 to 13 do
  begin
    if Temp and $00000001 > 0 then
      LabelFunctions2.Caption := LabelFunctions2.Caption + '1'
    else
      LabelFunctions2.Caption := LabelFunctions2.Caption + '0';
    Temp := Temp shr 1;
  end;

  LabelSpeedSteps.Caption := IntToStr(TrainState.SpeedSteps);
  LabelSpeed.Caption := IntToStr( Abs( Float16ToInt(TrainState.Speed)));
  LabelThrottleAlias.Caption := '0x' + IntToHex(TrainState.ControllerInfo.AliasID, 2);

  if (TrainState.TrainConfig.RoadName <> '') and (TrainState.TrainConfig.RoadNumber <> '') then
    Caption := TrainState.TrainConfig.RoadName + ' ' + TrainState.TrainConfig.RoadNumber
  else
  if TrainState.TrainConfig.RoadName <> '' then
    Caption := TrainState.TrainConfig.RoadName
  else begin
    if TrainState.TrainConfig.ShortLong = 0 then
      Caption := 'TrainID: ' + IntToStr(TrainState.TrainConfig.TrainID) + ' [S]'
    else
      Caption := 'TrainID: ' + IntToStr(TrainState.TrainConfig.TrainID) + ' [L]'
  end;
end;

end.

