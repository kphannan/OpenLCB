unit form_throttle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, ActnList, Spin, Buttons,
  olcb_transport_layer, olcb_app_common_settings,
  olcb_utilities, olcb_defines, Float16,
  laz2_DOM, laz2_XMLRead, laz2_XMLWrite, form_train_selector,
  form_train_config_editor, com_port_hub, ethernet_hub,
  template_userstatemachine, template_hardware, opstackdefines,
  nmranetutilities, form_fdi_picker;

const
  ANIMATION_DELTA = 50;
  TIME_QUERY_DCC_ADDRESS = 2000;   // Wait 1s to find Proxies that are assigned to the requested DCC address
  TIME_DEALLOCATE_ADDRESS = 2000;
  STR_UNASSIGNED = 'Unassigned';

type
  TFormThrottle = class;

  TOnThrottleEvent = procedure(Throttle: TFormThrottle) of object;

  TTimerType = (tt_None, tt_AllocateByList);

  TFunctionState = array[0..28] of Boolean;

  { TThrottleList }

  TThrottleList = class(TList)
  private
    FOnThrottleClose: TOnThrottleEvent;
    FOnThrottleHide: TOnThrottleEvent;
    function GetThrottles(Index: Integer): TFormThrottle;
    procedure SetThrottles(Index: Integer; AValue: TFormThrottle);
  protected
    procedure DoThrottleClose(Throttle: TFormThrottle);
    procedure DoThrottleHide(Throttle: TFormThrottle);
  public
    constructor Create; virtual;
    function CreateThrottle(AnEthernetHub: TEthernetHub; AComPortHub: TComPortHub; ImageList16x16: TImageList): TFormThrottle;
    procedure Clear; override;
    procedure CloseThrottle(Throttle: TFormThrottle);
    procedure HideAll;
    procedure CloseAll;
    procedure ShowAll;
    property Throttles[Index: Integer]: TFormThrottle read GetThrottles write SetThrottles; default;
    property OnThrottleHide: TOnThrottleEvent read FOnThrottleHide write FOnThrottleHide;
    property OnThrottleClose: TOnThrottleEvent read FOnThrottleClose write FOnThrottleClose;
  end;


  { TFormThrottle }

  TFormThrottle = class(TForm)
    ActionToggleDir: TAction;
    ActionQuerySpeed: TAction;
    ActionQueryFunctions: TAction;
    ActionFunction13: TAction;
    ActionFunction22: TAction;
    ActionFunction23: TAction;
    ActionFunction24: TAction;
    ActionFunction25: TAction;
    ActionFunction26: TAction;
    ActionFunction27: TAction;
    ActionFunction28: TAction;
    ActionFunction14: TAction;
    ActionFunction15: TAction;
    ActionFunction16: TAction;
    ActionFunction17: TAction;
    ActionFunction18: TAction;
    ActionFunction19: TAction;
    ActionFunction20: TAction;
    ActionFunction21: TAction;
    ActionControlEmergencyStop: TAction;
    ActionControlStop: TAction;
    ActionFunction1: TAction;
    ActionFunction10: TAction;
    ActionFunction11: TAction;
    ActionFunction12: TAction;
    ActionFunction2: TAction;
    ActionFunction3: TAction;
    ActionFunction4: TAction;
    ActionFunction5: TAction;
    ActionFunction6: TAction;
    ActionFunction7: TAction;
    ActionFunction8: TAction;
    ActionFunction9: TAction;
    ActionFunction0: TAction;
    ActionAllocationEditCustomization: TAction;
    ActionAllocationLoadEffectsFile: TAction;
    ActionAllocationRelease: TAction;
    ActionAllocationByList: TAction;
    ActionAllocationByAddress: TAction;
    ActionToggleAllocationPanel: TAction;
    ActionListThrottle: TActionList;
    ButtonAllocateTrainByAddress: TButton;
    ButtonEditConfiguration: TButton;
    ButtonEStop: TButton;
    ButtonQueryFunctions: TButton;
    ButtonQuerySpeed: TButton;
    ButtonReleaseTrain: TButton;
    ButtonSearchForTrain: TButton;
    ButtonShowHideAllocatePanel: TButton;
    ButtonStop: TButton;
    ButtonStop1: TButton;
    GroupBoxAddress: TGroupBox;
    GroupBoxAllocation: TGroupBox;
    GroupBoxConfiguration: TGroupBox;
    GroupBoxControl: TGroupBox;
    GroupBoxFunctions: TGroupBox;
    LabelAddress: TLabel;
    LabelAllocatedAddress: TLabel;
    LabelMaxSpeed: TLabel;
    LabelMinSpeed: TLabel;
    LabelPosValue: TLabel;
    LabelSpeedPos: TLabel;
    OpenDialog: TOpenDialog;
    PanelMain: TPanel;
    RadioGroupDirection: TRadioGroup;
    RadioGroupShortLong: TRadioGroup;
    RadioGroupSpeedScale: TRadioGroup;
    RadioGroupSpeedStep: TRadioGroup;
    ScrollBoxFunctions: TScrollBox;
    SpinEditAddress: TSpinEdit;
    StatusBar: TStatusBar;
    TimerGeneral: TTimer;
    TimerToggleAnimation: TTimer;
    TrackBarSpeed: TTrackBar;
    procedure ActionAllocationByAddressExecute(Sender: TObject);
    procedure ActionAllocationByListExecute(Sender: TObject);
    procedure ActionAllocationEditCustomizationExecute(Sender: TObject);
    procedure ActionAllocationFreeExecute(Sender: TObject);
    procedure ActionAllocationReleaseExecute(Sender: TObject);
    procedure ActionControlEmergencyStopExecute(Sender: TObject);
    procedure ActionControlStopExecute(Sender: TObject);
    procedure ActionFunction0Execute(Sender: TObject);
    procedure ActionFunction10Execute(Sender: TObject);
    procedure ActionFunction11Execute(Sender: TObject);
    procedure ActionFunction12Execute(Sender: TObject);
    procedure ActionFunction13Execute(Sender: TObject);
    procedure ActionFunction14Execute(Sender: TObject);
    procedure ActionFunction15Execute(Sender: TObject);
    procedure ActionFunction16Execute(Sender: TObject);
    procedure ActionFunction17Execute(Sender: TObject);
    procedure ActionFunction18Execute(Sender: TObject);
    procedure ActionFunction19Execute(Sender: TObject);
    procedure ActionFunction1Execute(Sender: TObject);
    procedure ActionFunction20Execute(Sender: TObject);
    procedure ActionFunction21Execute(Sender: TObject);
    procedure ActionFunction22Execute(Sender: TObject);
    procedure ActionFunction23Execute(Sender: TObject);
    procedure ActionFunction24Execute(Sender: TObject);
    procedure ActionFunction25Execute(Sender: TObject);
    procedure ActionFunction26Execute(Sender: TObject);
    procedure ActionFunction27Execute(Sender: TObject);
    procedure ActionFunction28Execute(Sender: TObject);
    procedure ActionFunction2Execute(Sender: TObject);
    procedure ActionFunction3Execute(Sender: TObject);
    procedure ActionFunction4Execute(Sender: TObject);
    procedure ActionFunction5Execute(Sender: TObject);
    procedure ActionFunction6Execute(Sender: TObject);
    procedure ActionFunction7Execute(Sender: TObject);
    procedure ActionFunction8Execute(Sender: TObject);
    procedure ActionFunction9Execute(Sender: TObject);
    procedure ActionQueryFunctionsExecute(Sender: TObject);
    procedure ActionQuerySpeedExecute(Sender: TObject);
    procedure ActionToggleAllocationPanelExecute(Sender: TObject);
    procedure ActionToggleDirExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RadioGroupDirectionClick(Sender: TObject);
    procedure RadioGroupShortLongClick(Sender: TObject);
    procedure RadioGroupSpeedStepClick(Sender: TObject);
    procedure TimerGeneralTimer(Sender: TObject);
    procedure TimerToggleAnimationTimer(Sender: TObject);
    procedure TrackBarSpeedChange(Sender: TObject);
  private
    FClosing: Boolean;
    FCurrentFunctions: DWord;
    FCurrentSpeed: THalfFloat;
    FDefaultFdiPath: string;
    FFormSelector: TFormTrainSelector;
    FThrottleNodeInfo: TNodeInfo;
    FTimerType: TTimerType;
    FTrainNodeInfo: TNodeInfo;
    FAllocationPanelToggleExpand: Boolean;
    FConfigurationViewer: TFormTrainConfigEditor;
    FImageList16x16: TImageList;
    { private declarations }
    FOnThrottleClose: TOnThrottleEvent;
    FOnThrottleHide: TOnThrottleEvent;
    procedure RunWriteFdiFile(NodeInfo: TNodeInfo; FileName: string);
    procedure RunTractionSpeed(NodeInfo: TNodeInfo; EmergencyStop: Boolean);
    procedure RunTractionFunction(NodeInfo: TNodeInfo; Address: DWord; Value: Word);
    procedure RunTractionQueryFunctions(NodeInfo: TNodeInfo; Address: DWord);
    procedure RunTractionQuerySpeed(NodeInfo: TNodeInfo);
    procedure SetTimerType(AValue: TTimerType);
  protected
    procedure CreateFunctionUIButton(ButtonLabel: string; Level: Integer; ButtonAction: TAction; ButtonIndex: Integer);
    procedure CreateFunctionUIGroup(GroupLabel: string; Level: Integer);
    function IsForward: Boolean;
    function IsShortAddress: Boolean;
    function SpeedStepRadioToSpeedStep: Byte;
    procedure ToggleTagOnComponent(Sender: TComponent);
    procedure UpdateAddressRange;
    procedure UpdateFunctionsClearControls;
    procedure UpdateFunctionsWithDefault;
    procedure UpdateFunctionsWithFDI(MemStream: TMemoryStream);
    property AllocationPanelToggleExpand: Boolean read FAllocationPanelToggleExpand write FAllocationPanelToggleExpand;
    property Closing: Boolean read FClosing write FClosing;
    property DefaultFdiPath: string read FDefaultFdiPath write FDefaultFdiPath;
    property FormSelector: TFormTrainSelector read FFormSelector write FFormSelector;
    property TimerType: TTimerType read FTimerType write SetTimerType;
  public
    { public declarations }
    property TrainNodeInfo: TNodeInfo read FTrainNodeInfo write FTrainNodeInfo;
    property ThrottleNodeInfo: TNodeInfo read FThrottleNodeInfo write FThrottleNodeInfo;
    property ConfigurationViewer: TFormTrainConfigEditor read FConfigurationViewer;
    property ImageList16x16: TImageList read FImageList16x16 write FImageList16x16;
    property OnThrottleHide: TOnThrottleEvent read FOnThrottleHide write FOnThrottleHide;
    property OnThrottleClose: TOnThrottleEvent read FOnThrottleClose write FOnThrottleClose;

    property CurrentSpeedDir: THalfFloat read FCurrentSpeed write FCurrentSpeed;
    property CurrentFunctions: DWord read FCurrentFunctions write FCurrentFunctions;

    procedure EventNodeAllocated(Event: TNodeEventNodeCreated);
    procedure EventTrainAllocated(Event: TNodeEventThrottleAssignedToTrain);
    procedure EventFunctionQuery(Event: TNodeEventFunctionQuery);
    procedure EventSpeedDirQuery(Event: TNodeEventSpeedDirQuery);
    procedure EventIsTrain(Event: TNodeEventIsTrain);
    procedure EventSimpleTrainNodeInfo(Event: TNodeEventSimpleTrainNodeInfo);
    procedure EventReleaseController(Event: TNodeEventReleaseController);
    procedure EventSupportsProtocols(Event: TNodeEventSupportsProtocols);
    procedure EventReadFDI(Event: TNodeEventReadFDI);
    procedure UpdateStatus(iPanel: Integer; NewStatus: string);
    procedure UpdateUI;
  end;

implementation

{$R *.lfm}

{ TThrottleList }

function TThrottleList.GetThrottles(Index: Integer): TFormThrottle;
begin
  Result := TFormThrottle( Items[Index]);
end;

procedure TThrottleList.SetThrottles(Index: Integer; AValue: TFormThrottle);
begin
  Items[Index] := AValue
end;

procedure TThrottleList.Clear;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
  begin
    Throttles[i].OnThrottleHide := nil;
    Throttles[i].Close;
  end;
  inherited Clear;
end;

procedure TThrottleList.HideAll;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Throttles[i].Hide
end;

procedure TThrottleList.CloseAll;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
  begin
    Throttles[i].OnThrottleHide := nil;
    Throttles[i].Close;
  end;
  Clear;
end;

function TThrottleList.CreateThrottle(AnEthernetHub: TEthernetHub; AComPortHub: TComPortHub; ImageList16x16: TImageList): TFormThrottle;
begin
  Result := TFormThrottle.Create(Application.MainForm);
  if Result <> nil then
  begin
    Self.Add(Result);
    Result.OnThrottleClose := @DoThrottleClose;
    Result.OnThrottleHide := @DoThrottleHide;
    Result.ImageList16x16 := ImageList16x16;
    Result.Show
  end;
end;

procedure TThrottleList.CloseThrottle(Throttle: TFormThrottle);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Throttles[i] = Throttle then
    begin
      Throttles[i].OnThrottleHide := nil;
      Throttle.Close;
      Break;
    end;
  end;
end;

constructor TThrottleList.Create;
begin
  inherited Create;
  OnThrottleClose := nil;
  OnThrottleHide := nil;
end;

procedure TThrottleList.DoThrottleClose(Throttle: TFormThrottle);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Throttles[i] = Throttle then
    begin
      if Assigned(OnThrottleClose) then
        OnThrottleClose(Throttle);
      Delete(i);
      Break;
    end;
  end;
end;

procedure TThrottleList.DoThrottleHide(Throttle: TFormThrottle);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Throttles[i] = Throttle then
    begin
      if Assigned(OnThrottleHide) then
        OnThrottleHide(Throttle);
      Break;
    end;
  end;
end;

procedure TThrottleList.ShowAll;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Throttles[i].Show;
end;

{ TFormThrottle }

procedure TFormThrottle.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(OnThrottleClose) then
    OnThrottleClose(Self);
  if not NMRAnetUtilities_NullNodeIDInfo(FTrainNodeInfo) then
    NodeThread.AddTask( TNodeTaskReleaseController.Create(ThrottleNodeInfo, TrainNodeInfo, STATE_THROTTLE_RELEASE_CONTROLLER, Self));
  NodeThread.AddTask( TNodeTaskAllocateDestroyNode.Create(FThrottleNodeInfo, NullNodeInfo, STATE_THROTTLE_FREE, Self));
  CloseAction := caFree;

end;

procedure TFormThrottle.FormCreate(Sender: TObject);
begin
  FConfigurationViewer := nil;
  FImageList16x16 := nil;
  FThrottleNodeInfo.AliasID := 0;
  FThrottleNodeInfo.ID := NULL_NODE_ID;
  FTrainNodeInfo.AliasID := 0;
  FTrainNodeInfo.ID := NULL_NODE_ID;
  FClosing := False;
  FCurrentSpeed := 0;
  FCurrentFunctions := 0;
  FFormSelector := nil;
  DefaultFdiPath := '';
end;

procedure TFormThrottle.ActionToggleAllocationPanelExecute(Sender: TObject);
begin
  AllocationPanelToggleExpand := Width = 392;
  TimerToggleAnimation.Enabled := True;
end;

procedure TFormThrottle.ActionToggleDirExecute(Sender: TObject);
begin
  RadioGroupDirection.OnClick := nil;
  if RadioGroupDirection.ItemIndex = 0 then
    RadioGroupDirection.ItemIndex := 1
  else
    RadioGroupDirection.ItemIndex := 0;
  if CurrentSpeedDir and $8000 <> 0 then
    CurrentSpeedDir := CurrentSpeedDir and not $8000
  else
    CurrentSpeedDir := CurrentSpeedDir or $8000;
  NodeThread.AddTask( TNodeTaskSpeedDir.Create( ThrottleNodeInfo, NullNodeInfo, STATE_THROTTLE_SPEED_CHANGE, Self, CurrentSpeedDir));
  RadioGroupDirection.OnClick := @RadioGroupDirectionClick;
end;

procedure TFormThrottle.ActionAllocationByAddressExecute(Sender: TObject);
begin
  NodeThread.AddTask(TNodeTaskAllocateTrainByAddress.Create(FThrottleNodeInfo, NullNodeInfo, STATE_THROTTLE_ALLOCATE_TRAIN_BY_ADDRESS, Self, SpinEditAddress.Value, SpeedStepRadioToSpeedStep, RadioGroupShortLong.ItemIndex = 1));
end;

procedure TFormThrottle.ActionAllocationByListExecute(Sender: TObject);
var
  Event: TNodeEventSimpleTrainNodeInfo;
  Task: TNodeTaskAllocateTrain;
begin
  NodeThread.AddTask(TNodeTaskFindTrains.Create(FThrottleNodeInfo, NullNodeInfo, STATE_THROTTLE_FIND_TRAINS, Self));
  TimerGeneral.Interval := 2000;
  TimerGeneral.Enabled := True;
  TimerType := tt_AllocateByList;
  FormSelector := TFormTrainSelector.Create(Self);
  try
    UpdateUI;
    FormSelector.TreeViewTrainList.Images := ImageList16x16;
    FormSelector.UpdateStatus(0, 'Status: Looking for Trains...');
    if FormSelector.ShowModal = mrOK then
    begin
      Event := TNodeEventSimpleTrainNodeInfo( FormSelector.TreeViewTrainList.Selected.Data);
      NodeThread.AddTask(TNodeTaskAllocateTrain.Create(ThrottleNodeInfo, Event.NodeInfo, STATE_THROTTLE_ALLOCATE_TRAIN, Self, 0, 0, False));
    end;
  finally
    FormSelector.Close;
    FormSelector := nil;
    UpdateUI;
  end;
end;

procedure TFormThrottle.ActionAllocationEditCustomizationExecute(Sender: TObject);
begin
  if not Assigned(ConfigurationViewer) then
  begin
    FConfigurationViewer := TFormTrainConfigEditor.Create(Application);
 //   ConfigurationViewer.AliasID := TrainNodeInfo;
    ConfigurationViewer.ImageList16x16 := ImageList16x16;
    ConfigurationViewer.Caption := 'Configuration Editor: Train ' + IntToStr(SpinEditAddress.Value);
    ConfigurationViewer.ShowModal;
    ConfigurationViewer.Release;
    FConfigurationViewer := nil;
  end;
end;

procedure TFormThrottle.ActionAllocationFreeExecute(Sender: TObject);
begin

  UpdateUI
end;

procedure TFormThrottle.ActionAllocationReleaseExecute(Sender: TObject);
begin
  NodeThread.AddTask( TNodeTaskReleaseController.Create(ThrottleNodeInfo, TrainNodeInfo, STATE_THROTTLE_RELEASE_CONTROLLER, Self));
end;

procedure TFormThrottle.ActionControlEmergencyStopExecute(Sender: TObject);
begin
  TrackBarSpeed.Position := 0;
  RunTractionSpeed(ThrottleNodeInfo, True);
  RunTractionSpeed(ThrottleNodeInfo, True);
  RunTractionSpeed(ThrottleNodeInfo, True);
end;

procedure TFormThrottle.ActionControlStopExecute(Sender: TObject);
begin
  TrackBarSpeed.Position := 0;
  RunTractionSpeed(ThrottleNodeInfo, False);
  RunTractionSpeed(ThrottleNodeInfo, False);
end;

procedure TFormThrottle.ActionFunction0Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(ThrottleNodeInfo, 0, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction10Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(ThrottleNodeInfo, 10, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction11Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(ThrottleNodeInfo, 11, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction12Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(ThrottleNodeInfo, 12, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction13Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(ThrottleNodeInfo, 13, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction14Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(ThrottleNodeInfo, 14, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction15Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(ThrottleNodeInfo, 15, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction16Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(ThrottleNodeInfo, 16, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction17Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(ThrottleNodeInfo, 17, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction18Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(ThrottleNodeInfo, 18, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction19Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(ThrottleNodeInfo, 19, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction1Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(ThrottleNodeInfo, 1, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction20Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(ThrottleNodeInfo, 20, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction21Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(ThrottleNodeInfo, 21, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction22Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(ThrottleNodeInfo, 22, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction23Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(ThrottleNodeInfo, 23, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction24Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(ThrottleNodeInfo, 24, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction25Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(ThrottleNodeInfo, 25, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction26Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(ThrottleNodeInfo, 26, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction27Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(ThrottleNodeInfo, 27, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction28Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(ThrottleNodeInfo, 28, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction2Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(ThrottleNodeInfo, 2, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction3Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(ThrottleNodeInfo, 3, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction4Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(ThrottleNodeInfo, 4, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction5Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(ThrottleNodeInfo, 5, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction6Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(ThrottleNodeInfo, 6, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction7Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(ThrottleNodeInfo, 7, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction8Execute(Sender: TObject);
begin
   ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(ThrottleNodeInfo, 8, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction9Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(ThrottleNodeInfo, 9, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionQueryFunctionsExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to 28 do
    RunTractionQueryFunctions(ThrottleNodeInfo, i);
end;

procedure TFormThrottle.ActionQuerySpeedExecute(Sender: TObject);
begin
  RunTractionQuerySpeed(ThrottleNodeInfo);
end;

procedure TFormThrottle.FormHide(Sender: TObject);
begin
  if Assigned(OnThrottleHide) then
    OnThrottleHide(Self)
end;

procedure TFormThrottle.FormShow(Sender: TObject);
begin
  UpdateFunctionsWithDefault;
  UpdateUI;
end;

procedure TFormThrottle.RadioGroupDirectionClick(Sender: TObject);
begin
  RunTractionSpeed(ThrottleNodeInfo, False);
end;

procedure TFormThrottle.RadioGroupShortLongClick(Sender: TObject);
begin
  UpdateAddressRange
end;

procedure TFormThrottle.RadioGroupSpeedStepClick(Sender: TObject);
begin
  UpdateAddressRange
end;

procedure TFormThrottle.TimerToggleAnimationTimer(Sender: TObject);
begin
  if AllocationPanelToggleExpand then
  begin
    if Width < 584 - ANIMATION_DELTA then
      Width := Width + ANIMATION_DELTA
    else begin
      Width := 584;
      TimerToggleAnimation.Enabled := False;
    end;
  end else
  begin
    if Width > 392 + ANIMATION_DELTA then
      Width := Width - ANIMATION_DELTA
    else begin
      Width := 392;
      TimerToggleAnimation.Enabled := False;
    end;
  end;
end;

procedure TFormThrottle.TrackBarSpeedChange(Sender: TObject);
var
  LastPos: Integer;
begin
  LastPos := StrToInt(LabelPosValue.Caption);
  if LastPos <> TrackBarSpeed.Position then
  begin
    RunTractionSpeed(ThrottleNodeInfo, False);
    LabelPosValue.Caption := IntToStr(TrackBarSpeed.Position);
  end;
end;

procedure TFormThrottle.RunWriteFdiFile(NodeInfo: TNodeInfo; FileName: string);
{var
  FileStream: TFileStream;
  MemStream, BufferStream: TMemoryStream;
  Task: TTaskAddressSpaceMemoryWriteRawWithDatagram;
  i, Offset: Integer;
  b: Byte;
  StartFDI: Integer;   }
begin
 { if Assigned(FComPortHub) then
  begin
    FileStream := TFileStream.Create(FileName, fmOpenRead);
    try
      StartFDI := -1;
      FileStream.Position := 0;
      i := 0;
      b := FileStream.ReadByte;
      while (b <> Ord ('<')) and (i < FileStream.Size) do
      begin
        b := FileStream.ReadByte;
        Inc(i);
      end;
      StartFDI := i;

      if (StartFDI > 0) then
      begin
        MemStream := TMemoryStream.Create;
        BufferStream := TMemoryStream.Create;
        try
          FileStream.Position := StartFDI;
          i := 0;
          while (FileStream.Position < FileStream.Size) do
          begin
            b := FileStream.ReadByte;
            if (b <> Ord(#13)) and (b <> Ord(#10)) then
              MemStream.WriteByte( b);
          end;
          MemStream.WriteByte( Ord(#0));  // Add null
          UpdateFunctionsWithFDI(MemStream);
          MemStream.Position := 0;
          Offset := 0;
          while (MemStream.Position < MemStream.Size) do
          begin
            BufferStream.Size := 0;
            while (MemStream.Position < MemStream.Size) and (BufferStream.Size < MAX_CONFIG_MEM_READWRITE_SIZE) do
              BufferStream.WriteByte( MemStream.ReadByte);

            Task := TTaskAddressSpaceMemoryWriteRawWithDatagram.Create(ThrottleNodeInfo, AliasID, True, MSI_FDI, Offset, BufferStream);
            if MemStream.Position >= MemStream.Size then
            Task.OnBeforeDestroy := @OnBeforeDestroyTask;  // The Last block signals the callback so we know we are done
            DispatchTask(Task);
            Offset := Offset + MAX_CONFIG_MEM_READWRITE_SIZE;
          end;
        finally
          MemStream.Free;
          BufferStream.Free;
        end;
      end;
    finally
      FileStream.Free;
    end;
  end;   }
end;

procedure TFormThrottle.SetTimerType(AValue: TTimerType);
begin
  if FTimerType <> AValue then
  begin
    FTimerType:=AValue;
    ActionAllocationByList.Enabled := AValue = tt_None;
  end;
end;

procedure TFormThrottle.RunTractionSpeed(NodeInfo: TNodeInfo; EmergencyStop: Boolean);
var
  Speed: single;
begin
  if EmergencyStop then
    NodeThread.AddTask( TNodeTask.Create( NodeInfo, NullNodeInfo, STATE_THROTTLE_E_STOP, Self))
  else begin
    Speed := TrackBarSpeed.Position/TrackBarSpeed.Max * 100;
    if not IsForward then
      Speed := -Speed;
    CurrentSpeedDir := FloatToHalf( Speed);
    NodeThread.AddTask( TNodeTaskSpeedDir.Create( NodeInfo, NullNodeInfo, STATE_THROTTLE_SPEED_CHANGE, Self, CurrentSpeedDir))
  end;
end;

procedure TFormThrottle.RunTractionFunction(NodeInfo: TNodeInfo; Address: DWord; Value: Word);
var
  Mask: DWord;
begin
  Mask := $00000001;
  Mask := Mask shl Address;
  if Value = 0 then
    FCurrentFunctions := CurrentFunctions and not Mask
  else
    FCurrentFunctions := CurrentFunctions or Mask;

  NodeThread.AddTask( TNodeTaskFunction.Create( NodeInfo, NullNodeInfo, STATE_THROTTLE_FUNCTION, Self, Address, Value))
end;

procedure TFormThrottle.RunTractionQueryFunctions(NodeInfo: TNodeInfo; Address: DWord);
begin
  NodeThread.AddTask( TNodeTaskFunctionQuery.Create(NodeInfo, NullNodeInfo, STATE_THROTTLE_QUERY_FUNCTION, Self, Address));
end;

procedure TFormThrottle.RunTractionQuerySpeed(NodeInfo: TNodeInfo);
begin
  NodeThread.AddTask( TNodeTaskSpeedDirQuery.Create(NodeInfo, NullNodeInfo, STATE_THROTTLE_QUERY_SPEED, Self));
end;

function TFormThrottle.SpeedStepRadioToSpeedStep: Byte;
begin
  case RadioGroupSpeedStep.ItemIndex of
    0 : Result := 14;
    1 : Result := 28;
    2 : Result := 128
  else
    Result := 28;
  end;
end;

procedure TFormThrottle.TimerGeneralTimer(Sender: TObject);
begin
  TimerGeneral.Enabled := False;
  if TimerType = tt_AllocateByList then
  begin
    if Assigned(FormSelector) then
    begin
      if FormSelector.TreeViewTrainList.Items.Count > 0 then
        FormSelector.UpdateStatus(0, 'Status: Please select a train to run')
      else
        FormSelector.UpdateStatus(0, 'Status: Sorry could not find any trains on the network')
    end
  end;
  TimerType := tt_None;
end;

procedure TFormThrottle.CreateFunctionUIButton(ButtonLabel: string;
  Level: Integer; ButtonAction: TAction; ButtonIndex: Integer);
var
  Button: TSpeedButton;
begin
  Button := TSpeedButton.Create(ScrollBoxFunctions);
  Button.AllowAllUp := True;
  Button.Top := MaxInt;
  Button.Align := alTop;
  Button.BorderSpacing.Left := Level * 4;
  Button.BorderSpacing.Top := 4;
  Button.BorderSpacing.Right := 4;
  Button.Height := 22;;
  Button.GroupIndex := ButtonIndex + 1;
  if ButtonAction.Checked then
    Button.Down := True;
  Button.Action := ButtonAction;
  ButtonAction.Caption := ButtonLabel;
  Button.Parent := ScrollBoxFunctions;
end;

procedure TFormThrottle.CreateFunctionUIGroup(GroupLabel: string; Level: Integer);
var
  GroupHeading: TLabel;
begin
  GroupHeading := TLabel.Create(ScrollBoxFunctions);
  GroupHeading.Top := MaxInt;
  GroupHeading.Align := alTop;
  GroupHeading.BorderSpacing.Left := Level * 4;
  GroupHeading.BorderSpacing.Top := 4;
  GroupHeading.BorderSpacing.Right := 4;
  GroupHeading.Caption := GroupLabel;
  GroupHeading.Parent := ScrollBoxFunctions;
end;

procedure TFormThrottle.EventFunctionQuery(Event: TNodeEventFunctionQuery);
var
  Mask: DWord;
  FunctionAction: TAction;
  OldEvent: TNotifyEvent;
begin
  FunctionAction := FindComponent('ActionFunction' + IntToStr(Event.Address)) as TAction;
  OldEvent := FunctionAction.OnExecute;
  FunctionAction.OnExecute := nil;
  if Event.Value = 0 then
    FunctionAction.Checked := False
  else
   FunctionAction.Checked := True;
  FunctionAction.OnExecute := OldEvent;
end;

procedure TFormThrottle.EventIsTrain(Event: TNodeEventIsTrain);
begin
  if TimerType = tt_AllocateByList then
  begin
    // If we are allocating by list then for any IsTrain Event send a STNIP request
    NodeThread.AddTask( TNodeTaskSimpleTrainNodeInfo.Create(ThrottleNodeInfo, Event.NodeInfo, STATE_THROTTLE_FIND_SIMPLE_TRAIN_INFO, Self));
  end;
end;

procedure TFormThrottle.EventNodeAllocated(Event: TNodeEventNodeCreated);
begin
  // update it all here
  ThrottleNodeInfo := Event.NodeInfo;
  PanelMain.Enabled := True;
  UpdateUI
end;

procedure TFormThrottle.EventReadFDI(Event: TNodeEventReadFDI);
var
  MemStream: TMemoryStream;
  i: Integer;
begin
  MemStream := TMemoryStream.Create;
  try
    for i := 1 to Length(Event.FDI) do
    begin
      if AnsiChar( Event.FDI[i]) <> #0 then
      MemStream.WriteByte( Ord( AnsiChar( Event.FDI[i])));
    end;
    MemStream.WriteByte( Ord(#0));
    UpdateFunctionsWithFDI(MemStream);
  finally
    MemStream.Free;
  end;
end;

procedure TFormThrottle.EventReleaseController(Event: TNodeEventReleaseController);
begin
  FTrainNodeInfo.AliasID := 0;
  FTrainNodeInfo.ID[0] := 0;
  FTrainNodeInfo.ID[1] := 0;
  UpdateUI;
end;

procedure TFormThrottle.EventSimpleTrainNodeInfo(Event: TNodeEventSimpleTrainNodeInfo);
var
  Node, Child: TTreeNode;
begin
  if TimerType = tt_AllocateByList then
  begin
    Node := FormSelector.TreeViewTrainList.Items.AddObject(nil, 'Road Name: ' + Event.RoadName, Event.Clone);
    Node.ImageIndex := 433;
    Node.SelectedIndex := 433;
    Child := FormSelector.TreeViewTrainList.Items.AddChild(Node, 'Road Class: ' + Event.TrainClass);
    Child.ImageIndex := 615;
    Child.SelectedIndex := 615;
    Child := FormSelector.TreeViewTrainList.Items.AddChild(Node, 'Road Number: ' + Event.RoadNumber);
    Child.ImageIndex := 615;
    Child.SelectedIndex := 615;
    Child := FormSelector.TreeViewTrainList.Items.AddChild(Node, 'Train Name: ' + Event.TrainName);
    Child.ImageIndex := 615;
    Child.SelectedIndex := 615;
    Child := FormSelector.TreeViewTrainList.Items.AddChild(Node, 'Manufacturer: ' + Event.Manufacturer);
    Child.ImageIndex := 615;
    Child.SelectedIndex := 615;
    Child := FormSelector.TreeViewTrainList.Items.AddChild(Node, 'Owner: ' + Event.Owner);
    Child.ImageIndex := 615;
    Child.SelectedIndex := 615;
  end;
end;

procedure TFormThrottle.EventSpeedDirQuery(Event: TNodeEventSpeedDirQuery);
begin
  TrackBarSpeed.OnChange := nil;
  RadioGroupDirection.OnClick := nil;
  CurrentSpeedDir := Event.SpeedDir;
  TrackBarSpeed.Position := Round( HalfToFloat( CurrentSpeedDir and not $8000));
  LabelPosValue.Caption := IntToStr(TrackBarSpeed.Position);
  if CurrentSpeedDir and $8000 <> 0 then
    RadioGroupDirection.ItemIndex := 1
  else
    RadioGroupDirection.ItemIndex := 0;
  TrackBarSpeed.OnChange := @TrackBarSpeedChange;
  RadioGroupDirection.OnClick := @RadioGroupDirectionClick;
end;

procedure TFormThrottle.EventSupportsProtocols(Event: TNodeEventSupportsProtocols);
var
  FdiPicker: TFormFdiPicker;
  FileStream: TFileStream;
  MemStream: TMemoryStream;
begin
  FdiPicker := TFormFdiPicker.Create(Self);
  try
    FdiPicker.ButtonUseTrain.Enabled := Event.FDI;
    if Event.FDI then
      FdiPicker.LabelHeader.Caption :=  'This train supports Function Definition Information (FDI) to add custom names and order to your Functions.'
    else
      FdiPicker.LabelHeader.Caption :=  'This train does not support Function Definition Information (FDI).';
    FdiPicker.OpenDialog.InitialDir := DefaultFdiPath;

    case FdiPicker.ShowModal of      // Can't use mrClose because "X"ing out of the dialog fires that modal result
      mrIgnore :           // do nothing
         begin
           UpdateFunctionsWithDefault;
         end;
      mrOK :              // Use the Trains
        begin
          NodeThread.AddTask(TNodeTaskReadConfigMemory.Create(ThrottleNodeInfo, TrainNodeInfo, STATE_THROTTLE_READ_FDI, Self, 0));
        end;
      mrAll :         // Use the custom file
        begin
          if FileExistsUTF8(FdiPicker.OpenDialog.FileName) then
          begin
            DefaultFdiPath := ExtractFilePath(FdiPicker.OpenDialog.FileName);
            FileStream := TFileStream.Create(FdiPicker.OpenDialog.FileName, fmOpenRead);
            MemStream := TMemoryStream.Create;
            try
              MemStream.CopyFrom(FileStream, FileStream.Size);
              MemStream.WriteByte( Ord(#0));
              UpdateFunctionsWithFDI(MemStream);
            finally
              FileStream.Free;
              MemStream.Free;
            end;
          end else
            UpdateFunctionsWithDefault;
        end;
    end;
    begin

    end;
  finally
    FdiPicker.Close;
  end;

end;

procedure TFormThrottle.EventTrainAllocated(Event: TNodeEventThrottleAssignedToTrain);
begin
  FTrainNodeInfo := Event.TrainNodeInfo;
  NodeThread.AddTask( TNodeTaskSupportsProtocols.Create(ThrottleNodeInfo, TrainNodeInfo, STATE_THROTTLE_PROTOCOL_SUPPORT, Self));
  UpdateUI;
end;

function TFormThrottle.IsForward: Boolean;
begin
  Result := RadioGroupDirection.ItemIndex = 0;
end;

function TFormThrottle.IsShortAddress: Boolean;
begin
  Result := RadioGroupShortLong.ItemIndex = 0;
end;

procedure TFormThrottle.ToggleTagOnComponent(Sender: TComponent);
begin
  if Sender.Tag = 0 then
  begin
   Sender.Tag := 1;
  end else
  begin
    Sender.Tag := 0;
  end;
end;

procedure TFormThrottle.UpdateAddressRange;
begin
  if IsShortAddress then
  begin
    SpinEditAddress.MinValue := 1;
    SpinEditAddress.MaxValue := 127;
  end else
  begin
    SpinEditAddress.MinValue := 0;
    SpinEditAddress.MaxValue := 16383;
  end;
end;

procedure TFormThrottle.UpdateFunctionsClearControls;
var
  i: Integer;
begin
  for i := ScrollBoxFunctions.ControlCount - 1 downto 0 do
    ScrollBoxFunctions.Controls[i].Free;
end;

procedure TFormThrottle.UpdateFunctionsWithDefault;
var
  i: Integer;
  FunctionsState: TFunctionState;
begin
  ScrollBoxFunctions.BeginUpdateBounds;
  try
    UpdateFunctionsClearControls;
    for i := 0 to 28 do
      CreateFunctionUIButton('Function ' + IntToStr(i), 0, FindComponent('ActionFunction' + IntToStr(i)) as TAction, i) ;
  finally
      ScrollBoxFunctions.EndUpdateBounds;
  end;
end;

procedure TFormThrottle.UpdateFunctionsWithFDI(MemStream: TMemoryStream);
//
// REQUIRES A NULL TERMINATOR!!!!
//

  procedure RunDownGroup(Parent: TDOMNode; Level: Integer);
  var
    Child, NameNode, OriginNode: TDOMNode;
    i, iButton: Integer;
    ActionName: string;
  begin
    if Assigned(Parent) then
    begin
      iButton := 0;
      i := Parent.ChildNodes.Count;
      Child := Parent.FirstChild;
      while Assigned(Child) do
      begin
        if LowerCase(Child.NodeName) = 'group' then
        begin
          NameNode := Child.FindNode('name');
          if Assigned(NameNode) then
            CreateFunctionUIGroup(NameNode.FirstChild.NodeValue, Level);
          RunDownGroup(Child, Level+1);
        end else
        if LowerCase(Child.NodeName) = 'function' then
        begin
          NameNode := Child.FindNode('name');
          OriginNode := Child.Attributes.GetNamedItem('origin');
          if Assigned(OriginNode) then
          begin
            ActionName := 'ActionFunction' + OriginNode.FirstChild.NodeValue;
            CreateFunctionUIButton(NameNode.FirstChild.NodeValue, Level, FindComponent(ActionName) as TAction, iButton);
            Inc(iButton);
          end else
            ShowMessage('Can not find <number> element to define the function');
        end;
        Child := Child.NextSibling;
      end;
    end;
  end;

var
  TrashStream: TMemoryStream;
  ADoc: TXMLDocument;
  SegNode: TDOMNode;
  Done: Boolean;
begin
  Done := False;
  MemStream.Position := 0;
  TrashStream := TMemoryStream.Create;
  try
    TrashStream.CopyFrom(MemStream, MemStream.Size);
    TrashStream.Position := 0;
    while (TrashStream.Position < TrashStream.Size) and not Done do
    begin
      if TrashStream.ReadByte = Ord( #0) then
      begin
        if TrashStream.Position > 0 then
        begin
          TrashStream.Size := TrashStream.Position - 1;   // Strip the null
          try
            TrashStream.Position := 0;
            ReadXMLFile(ADoc, TrashStream);                 // This corrupts the stream from its original contents
            WriteXMLFile(ADoc, TrashStream);
            SegNode := ADoc.DocumentElement.FindNode('segment');
            ScrollBoxFunctions.BeginUpdateBounds;
            try
              UpdateFunctionsClearControls;
              RunDownGroup(SegNode, 0);
            finally
              ScrollBoxFunctions.EndUpdateBounds;
            end;
          except
          end;
        end;
        Done := True;
      end;
    end;
  finally
    TrashStream.Free
  end;
end;

procedure TFormThrottle.UpdateStatus(iPanel: Integer; NewStatus: string);
begin
  StatusBar.Panels[iPanel].Text := NewStatus;
end;

procedure TFormThrottle.UpdateUI;
begin
  PanelMain.Enabled := FormSelector = nil;
  ActionAllocationEditCustomization.Enabled := not NMRAnetUtilities_NullNodeIDInfo(FTrainNodeInfo);
  ActionAllocationByList.Enabled := NMRAnetUtilities_NullNodeIDInfo(FTrainNodeInfo);
  ActionAllocationByAddress.Enabled := NMRAnetUtilities_NullNodeIDInfo(FTrainNodeInfo);
  ActionAllocationRelease.Enabled := not NMRAnetUtilities_NullNodeIDInfo(FTrainNodeInfo);
  GroupBoxFunctions.Enabled := not NMRAnetUtilities_NullNodeIDInfo(FTrainNodeInfo);
  GroupBoxControl.Enabled := not NMRAnetUtilities_NullNodeIDInfo(FTrainNodeInfo);
  GroupBoxConfiguration.Enabled := not NMRAnetUtilities_NullNodeIDInfo(FTrainNodeInfo);
  RadioGroupSpeedStep.Enabled := NMRAnetUtilities_NullNodeIDInfo(FTrainNodeInfo);
  SpinEditAddress.Enabled := NMRAnetUtilities_NullNodeIDInfo(FTrainNodeInfo);
  if NMRAnetUtilities_NullNodeIDInfo(FTrainNodeInfo) then
    LabelAllocatedAddress.Caption := STR_UNASSIGNED
  else
    LabelAllocatedAddress.Caption := IntToStr(SpinEditAddress.Value);

  Caption := 'Open LCB Throttle - ' + LabelAllocatedAddress.Caption;
  UpdateAddressRange;
  UpdateStatus(0, 'Throttle: 0x' + IntToHex(ThrottleNodeInfo.AliasID, 4) + ' [0x' + NodeIDToDotHex(ThrottleNodeInfo.ID)  + ']');
  if not NMRAnetUtilities_NullNodeIDInfo(FTrainNodeInfo) then
    UpdateStatus(1, 'Train: 0x' + IntToHex(TrainNodeInfo.AliasID, 4) + ' [0x' + NodeIDToDotHex(TrainNodeInfo.ID) + ']')
  else
    UpdateStatus(1, '');
end;

end.

