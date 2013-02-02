unit form_awesome_throttle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, ActnList, Spin, Buttons, math_float16,
  olcb_threaded_stack, olcb_common_tasks, olcb_app_common_settings,
  olcb_utilities, olcb_defines, form_awesome_throttle_duplicate_address,
  laz2_DOM, laz2_XMLRead, laz2_XMLWrite;

const
  ANIMATION_DELTA = 50;
  TIME_QUERY_DCC_ADDRESS = 1000;   // Wait 1s to find Proxies that are assigned to the requested DCC address
  TIME_DEALLOCATE_ADDRESS = 1000;
  STR_UNASSIGNED = 'Unassigned';

type
  TFormAwesomeThrottle = class;

  TOnThrottleEvent = procedure(Throttle: TFormAwesomeThrottle) of object;

  TGeneralWaitTimeTask = (
    gwttNone,
    gwttQueryAddress,
    gwttQueryIsIdle,
    gwttAllocateAddress,
    gwttDeallocateAddress
  );

  TThrottleWaitingAction = (
    wa_None,
    wa_FDItoFunctions
  );                      // Loading the FDI to update the Functions
  TThrottleWaitingActions = set of TThrottleWaitingAction;

  { TFormThrottleList }

  TFormThrottleList = class(TList)
  private
    function GetThrottles(Index: Integer): TFormAwesomeThrottle;
    procedure SetThrottles(Index: Integer; AValue: TFormAwesomeThrottle);
  public
    procedure HideAll;
    procedure CloseAll;
    procedure ShowAll;
    property Throttles[Index: Integer]: TFormAwesomeThrottle read GetThrottles write SetThrottles;
  end;

  TAliasList = class(TList)

  end;


  { TFormAwesomeThrottle }

  TFormAwesomeThrottle = class(TForm)
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
    ActionAllocationFree: TAction;
    ActionAllocationRelease: TAction;
    ActionAllocationSearchForTrain: TAction;
    ActionAllocationByAddress: TAction;
    ActionToggleAllocationPanel: TAction;
    ActionListThrottle: TActionList;
    ButtonAllocateTrainByAddress: TButton;
    ButtonShowHideAllocatePanel: TButton;
    ButtonEditConfiguration: TButton;
    ButtonSearchForTrain: TButton;
    ButtonReleaseTrain: TButton;
    ButtonLoadConfiguration: TButton;
    ButtonFreeTrain: TButton;
    ButtonStop: TButton;
    ButtonEStop: TButton;
    GroupBoxAddress: TGroupBox;
    GroupBoxAllocation: TGroupBox;
    GroupBoxControl: TGroupBox;
    GroupBoxFunctions: TGroupBox;
    GroupBoxConfiguration: TGroupBox;
    LabelAllocatedAddress: TLabel;
    LabelPosValue: TLabel;
    LabelSpeedPos: TLabel;
    LabelAddress: TLabel;
    LabelMaxSpeed: TLabel;
    LabelMinSpeed: TLabel;
    OpenDialog: TOpenDialog;
    RadioGroupShortLong: TRadioGroup;
    RadioGroupDirection: TRadioGroup;
    RadioGroupSpeedStep: TRadioGroup;
    RadioGroupSpeedScale: TRadioGroup;
    ScrollBoxFunctions: TScrollBox;
    SpinEditAddress: TSpinEdit;
    TimerGeneralTimeout: TTimer;
    TimerToggleAnimation: TTimer;
    TrackBarSpeed: TTrackBar;
    procedure ActionAllocationByAddressExecute(Sender: TObject);
    procedure ActionAllocationEditCustomizationExecute(Sender: TObject);
    procedure ActionAllocationFreeExecute(Sender: TObject);
    procedure ActionAllocationLoadEffectsFileExecute(Sender: TObject);
    procedure ActionAllocationReleaseExecute(Sender: TObject);
    procedure ActionAllocationSearchForTrainExecute(Sender: TObject);
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
    procedure ActionToggleAllocationPanelExecute(Sender: TObject);
    procedure ButtonAllocateTrainByAddressClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RadioGroupDirectionClick(Sender: TObject);
    procedure RadioGroupShortLongClick(Sender: TObject);
    procedure RadioGroupSpeedStepClick(Sender: TObject);
    procedure TimerGeneralTimeoutTimer(Sender: TObject);
    procedure TimerToggleAnimationTimer(Sender: TObject);
    procedure TrackBarSpeedChange(Sender: TObject);
  private
    FAliasList: TAliasList;
    FAllocated: Boolean;
    FAllocatedAlias: Word;
    FAllocationPanelToggleExpand: Boolean;
    FComPortThread: TComPortThread;
    { private declarations }
    FOnThrottleClose: TOnThrottleEvent;
    FOnThrottleHide: TOnThrottleEvent;
    FPotentialAlias: Word;
    FWaitingActions: TThrottleWaitingActions;
    FWaitTimeTask: TGeneralWaitTimeTask;
    procedure RunLoadFdiFile(AliasID: Word; FileName: string);
    procedure RunProtocolSupport(AliasID: Word; WaitTime: Cardinal);
    procedure RunReadMemorySpace(AliasID: Word; AddressSpace: Byte);
    procedure RunTractionAllocateTrainByAddress(AliasID: Word; WaitTime: Cardinal);
    procedure RunTractionDeAllocateTrainByAddress(AliasID: Word; WaitTime: Cardinal);
    procedure RunTractionQueryDccAddress(WaitTime: Cardinal);
    procedure RunTractionQueryIsIdle(WaitTime: Cardinal);
    procedure RunTractionSpeed(AliasID: Word; EmergencyStop: Boolean);
    procedure RunTractionFunction(AliasID: Word; Address: DWord; Value: Word);
    procedure SetAllocated(AValue: Boolean);
    procedure SetAllocatedAlias(AValue: Word);
    procedure SetComPortThread(AValue: TComPortThread);
  protected
    procedure CreateFunctionUIButton(ButtonLabel: string; Level: Integer; ButtonAction: TAction);
    procedure CreateFunctionUIGroup(GroupLabel: string; Level: Integer);
    procedure HandleGeneralTimerResults;
    function IsForward: Boolean;
    function IsShortAddress: Boolean;
    procedure OnBeforeDestroyTask(Sender: TOlcbTaskBase);
    procedure ToggleTagOnComponent(Sender: TComponent);
    procedure UpdateAddressRange;
    procedure UpdateFunctionsClearControls;
    procedure UpdateFunctionsWithDefault;
    procedure UpdateFunctionsWithFDI(MemTask: TReadAddressSpaceMemoryTask);
    property AllocationPanelToggleExpand: Boolean read FAllocationPanelToggleExpand write FAllocationPanelToggleExpand;
    property AliasList: TAliasList read FAliasList write FAliasList;
    property PotentialAlias: Word read FPotentialAlias write FPotentialAlias;
    property WaitTimeTask: TGeneralWaitTimeTask read FWaitTimeTask write FWaitTimeTask;
  public
    { public declarations }
    property Allocated: Boolean read FAllocated write SetAllocated;
    property AllocatedAlias: Word read FAllocatedAlias write SetAllocatedAlias;
    property ComPortThread: TComPortThread read FComPortThread write SetComPortThread;
    property OnThrottleHide: TOnThrottleEvent read FOnThrottleHide write FOnThrottleHide;
    property OnThrottleClose: TOnThrottleEvent read FOnThrottleClose write FOnThrottleClose;
    property WaitingActions: TThrottleWaitingActions read FWaitingActions write FWaitingActions;
    procedure EventTaskReceived(EventTask: TEventTask);
    procedure UpdateUI;
  end;


implementation

{$R *.lfm}

{ TFormThrottleList }

function TFormThrottleList.GetThrottles(Index: Integer): TFormAwesomeThrottle;
begin
  Result := TFormAwesomeThrottle( Items[Index]);
end;

procedure TFormThrottleList.SetThrottles(Index: Integer; AValue: TFormAwesomeThrottle);
begin
  Items[Index] := AValue
end;

procedure TFormThrottleList.HideAll;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Throttles[i].Hide
end;

procedure TFormThrottleList.CloseAll;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    Throttles[i].Close;
  Clear;
end;

procedure TFormThrottleList.ShowAll;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Throttles[i].Show;
  Clear;
end;

{ TFormAwesomeThrottle }

procedure TFormAwesomeThrottle.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(OnThrottleClose) then
    OnThrottleClose(Self);
  CloseAction := caFree;
end;

procedure TFormAwesomeThrottle.FormCreate(Sender: TObject);
begin
  Allocated := False;
  FWaitTimeTask := gwttNone;
  FWaitingActions := [wa_None];
  ComPortThread := nil;
  AliasList := TAliasList.Create;
end;

procedure TFormAwesomeThrottle.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FAliasList);
end;

procedure TFormAwesomeThrottle.ActionToggleAllocationPanelExecute(Sender: TObject);
begin
  AllocationPanelToggleExpand := Width = 392;
  TimerToggleAnimation.Enabled := True;
end;

procedure TFormAwesomeThrottle.ActionAllocationByAddressExecute(Sender: TObject);
begin
  AliasList.Clear;
  RunTractionQueryDccAddress(TIME_QUERY_DCC_ADDRESS);  // We need to query looking for any nodes that are current assigned for the address
end;

procedure TFormAwesomeThrottle.ActionAllocationEditCustomizationExecute(Sender: TObject);
begin

end;

procedure TFormAwesomeThrottle.ActionAllocationFreeExecute(Sender: TObject);
begin
  TrackBarSpeed.Position := 0;
  RunTractionDeAllocateTrainByAddress(AllocatedAlias, TIME_DEALLOCATE_ADDRESS);
end;

procedure TFormAwesomeThrottle.ActionAllocationLoadEffectsFileExecute(Sender: TObject);
begin
  if OpenDialog.Execute then
    RunLoadFdiFIle(AllocatedAlias, OpenDialog.FileName);
//  Include(FWaitingActions, wa_FDItoFunctions);
//  RunProtocolSupport(AllocatedAlias, 0);         // Kick it off
end;

procedure TFormAwesomeThrottle.ActionAllocationReleaseExecute(Sender: TObject);
begin
  AliasList.Clear;        // Just let it go
  Allocated := False;
  AllocatedAlias := 0;
  UpdateUI;
end;

procedure TFormAwesomeThrottle.ActionAllocationSearchForTrainExecute(Sender: TObject);
begin

end;

procedure TFormAwesomeThrottle.ActionControlEmergencyStopExecute(Sender: TObject);
begin
  TrackBarSpeed.Position := 0;
  RunTractionSpeed(AllocatedAlias, True);
  RunTractionSpeed(AllocatedAlias, True);
  RunTractionSpeed(AllocatedAlias, True);
end;

procedure TFormAwesomeThrottle.ActionControlStopExecute(Sender: TObject);
begin
  TrackBarSpeed.Position := 0;
end;

procedure TFormAwesomeThrottle.ActionFunction0Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(AllocatedAlias, 0, (Sender as TComponent).Tag)
end;

procedure TFormAwesomeThrottle.ActionFunction10Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(AllocatedAlias, 10, (Sender as TComponent).Tag)
end;

procedure TFormAwesomeThrottle.ActionFunction11Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(AllocatedAlias, 11, (Sender as TComponent).Tag)
end;

procedure TFormAwesomeThrottle.ActionFunction12Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(AllocatedAlias, 12, (Sender as TComponent).Tag)
end;

procedure TFormAwesomeThrottle.ActionFunction13Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(AllocatedAlias, 13, (Sender as TComponent).Tag)
end;

procedure TFormAwesomeThrottle.ActionFunction14Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(AllocatedAlias, 14, (Sender as TComponent).Tag)
end;

procedure TFormAwesomeThrottle.ActionFunction15Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(AllocatedAlias, 15, (Sender as TComponent).Tag)
end;

procedure TFormAwesomeThrottle.ActionFunction16Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(AllocatedAlias, 16, (Sender as TComponent).Tag)
end;

procedure TFormAwesomeThrottle.ActionFunction17Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(AllocatedAlias, 17, (Sender as TComponent).Tag)
end;

procedure TFormAwesomeThrottle.ActionFunction18Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(AllocatedAlias, 18, (Sender as TComponent).Tag)
end;

procedure TFormAwesomeThrottle.ActionFunction19Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(AllocatedAlias, 19, (Sender as TComponent).Tag)
end;

procedure TFormAwesomeThrottle.ActionFunction1Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(AllocatedAlias, 1, (Sender as TComponent).Tag)
end;

procedure TFormAwesomeThrottle.ActionFunction20Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(AllocatedAlias, 20, (Sender as TComponent).Tag)
end;

procedure TFormAwesomeThrottle.ActionFunction21Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(AllocatedAlias, 21, (Sender as TComponent).Tag)
end;

procedure TFormAwesomeThrottle.ActionFunction22Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(AllocatedAlias, 22, (Sender as TComponent).Tag)
end;

procedure TFormAwesomeThrottle.ActionFunction23Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(AllocatedAlias, 23, (Sender as TComponent).Tag)
end;

procedure TFormAwesomeThrottle.ActionFunction24Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(AllocatedAlias, 24, (Sender as TComponent).Tag)
end;

procedure TFormAwesomeThrottle.ActionFunction25Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(AllocatedAlias, 25, (Sender as TComponent).Tag)
end;

procedure TFormAwesomeThrottle.ActionFunction26Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(AllocatedAlias, 26, (Sender as TComponent).Tag)
end;

procedure TFormAwesomeThrottle.ActionFunction27Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(AllocatedAlias, 27, (Sender as TComponent).Tag)
end;

procedure TFormAwesomeThrottle.ActionFunction28Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(AllocatedAlias, 28, (Sender as TComponent).Tag)
end;

procedure TFormAwesomeThrottle.ActionFunction2Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(AllocatedAlias, 2, (Sender as TComponent).Tag)
end;

procedure TFormAwesomeThrottle.ActionFunction3Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(AllocatedAlias, 3, (Sender as TComponent).Tag)
end;

procedure TFormAwesomeThrottle.ActionFunction4Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(AllocatedAlias, 4, (Sender as TComponent).Tag)
end;

procedure TFormAwesomeThrottle.ActionFunction5Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(AllocatedAlias, 5, (Sender as TComponent).Tag)
end;

procedure TFormAwesomeThrottle.ActionFunction6Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(AllocatedAlias, 6, (Sender as TComponent).Tag)
end;

procedure TFormAwesomeThrottle.ActionFunction7Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(AllocatedAlias, 7, (Sender as TComponent).Tag)
end;

procedure TFormAwesomeThrottle.ActionFunction8Execute(Sender: TObject);
begin
   ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(AllocatedAlias, 8, (Sender as TComponent).Tag)
end;

procedure TFormAwesomeThrottle.ActionFunction9Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(AllocatedAlias, 9, (Sender as TComponent).Tag)
end;

procedure TFormAwesomeThrottle.ButtonAllocateTrainByAddressClick(Sender: TObject);
begin
  ;
end;

procedure TFormAwesomeThrottle.FormHide(Sender: TObject);
begin
  if Assigned(OnThrottleHide) then
    OnThrottleHide(Self)
end;

procedure TFormAwesomeThrottle.FormShow(Sender: TObject);
begin
  UpdateFunctionsWithDefault;
  UpdateUI;
end;

procedure TFormAwesomeThrottle.RadioGroupDirectionClick(Sender: TObject);
begin
  RunTractionSpeed(AllocatedAlias, False);
end;

procedure TFormAwesomeThrottle.RadioGroupShortLongClick(Sender: TObject);
begin
  UpdateAddressRange
end;

procedure TFormAwesomeThrottle.RadioGroupSpeedStepClick(Sender: TObject);
begin
  UpdateAddressRange
end;

procedure TFormAwesomeThrottle.TimerGeneralTimeoutTimer(Sender: TObject);
begin
  TimerGeneralTimeout.Enabled := False;
  HandleGeneralTimerResults;
end;

procedure TFormAwesomeThrottle.TimerToggleAnimationTimer(Sender: TObject);
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

procedure TFormAwesomeThrottle.TrackBarSpeedChange(Sender: TObject);
var
  LastPos: Cardinal;
begin
  LastPos := StrToInt(LabelPosValue.Caption);
  if LastPos <> TrackBarSpeed.Position then
  begin
    RunTractionSpeed(AllocatedAlias, False);
    LabelPosValue.Caption := IntToStr(TrackBarSpeed.Position);
  end;
end;

procedure TFormAwesomeThrottle.RunLoadFdiFile(AliasID: Word; FileName: string);
var
  FileStream: TFileStream;
  Task: TWriteAddressSpaceMemoryTask;
  s: string;
  i: Integer;

begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
 //   FileStream.Position := 0;
 //   for i := 0 to FileStream.Size - 1 do
 //     s := s + Char( FileStream.ReadByte);
 //   ShowMessage(s);
    Task := TWriteAddressSpaceMemoryTask.Create(GlobalSettings.General.AliasIDAsVal, AliasID, True, MSI_FDI, FileStream);
    ComPortThread.AddTask(Task);

    /// HOW DO I KNOW WHEN IT IS DONE??????
  finally
    FileStream.Free;
  end;
end;

procedure TFormAwesomeThrottle.RunProtocolSupport(AliasID: Word; WaitTime: Cardinal);
var
  Task: TProtocolSupportTask;
begin
  if Assigned(ComPortThread) then
  begin
    Task := TProtocolSupportTask.Create(GlobalSettings.General.AliasIDAsVal, AliasID, True);
    Task.OnBeforeDestroy := @OnBeforeDestroyTask;
    ComPortThread.AddTask(Task);
  end;
end;

procedure TFormAwesomeThrottle.RunReadMemorySpace(AliasID: Word; AddressSpace: Byte);
var
  Task: TReadAddressSpaceMemoryTask;
begin
  Task := TReadAddressSpaceMemoryTask.Create(GlobalSettings.General.AliasIDAsVal,AliasID, True, AddressSpace);
  Task.ForceOptionalSpaceByte := False;
  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
  ComPortThread.AddTask(Task);
end;

procedure TFormAwesomeThrottle.RunTractionAllocateTrainByAddress(AliasID: Word; WaitTime: Cardinal);
var
  Task: TTractionAllocateDccProxyTask;
  SpeedStep: Byte;
begin
  if Assigned(ComPortThread) then
  begin
    case RadioGroupSpeedStep.ItemIndex of
      0: SpeedStep := 14;
      1: SpeedStep := 28;
      2: SpeedStep := 128
    else
      SpeedStep := SPEEDSTEP_DEFAULT;
    end;
    Task := TTractionAllocateDccProxyTask.Create(GlobalSettings.General.AliasIDAsVal, AliasID, True, SpinEditAddress.Value, IsShortAddress, SpeedStep);
    Task.OnBeforeDestroy := @OnBeforeDestroyTask;
    ComPortThread.AddTask(Task);
    TimerGeneralTimeout.Enabled := False;
    TimerGeneralTimeout.Interval := WaitTime;
    TimerGeneralTimeout.Enabled := True;
    WaitTimeTask := gwttAllocateAddress;
  end;
end;

procedure TFormAwesomeThrottle.RunTractionDeAllocateTrainByAddress(AliasID: Word; WaitTime: Cardinal);
var
  Task: TTractionDeAllocateDccProxyTask;
begin
  if Assigned(ComPortThread) then
  begin
    Task := TTractionDeAllocateDccProxyTask.Create(GlobalSettings.General.AliasIDAsVal, AliasID, True);
    Task.OnBeforeDestroy := @OnBeforeDestroyTask;
    ComPortThread.AddTask(Task);
    TimerGeneralTimeout.Enabled := False;
    TimerGeneralTimeout.Interval := WaitTime;
    TimerGeneralTimeout.Enabled := True;
    WaitTimeTask := gwttDeallocateAddress;
  end;
end;

procedure TFormAwesomeThrottle.RunTractionQueryDccAddress(WaitTime: Cardinal);
var
  Task: TTractionQueryDccAddressProxyTask;
begin
  if Assigned(ComPortThread) then
  begin
    Task := TTractionQueryDccAddressProxyTask.Create(GlobalSettings.General.AliasIDAsVal, 0, True, SpinEditAddress.Value, IsShortAddress);
    Task.OnBeforeDestroy := @OnBeforeDestroyTask;
    ComPortThread.AddTask(Task);
    TimerGeneralTimeout.Enabled := False;
    TimerGeneralTimeout.Interval := WaitTime;
    TimerGeneralTimeout.Enabled := True;
    WaitTimeTask := gwttQueryAddress;
  end;
end;

procedure TFormAwesomeThrottle.RunTractionQueryIsIdle(WaitTime: Cardinal);
var
  Task: TIdentifyProducerTask;
begin
  if Assigned(ComPortThread) then
  begin
    Task := TIdentifyProducerTask.Create(GlobalSettings.General.AliasIDAsVal, 0, True, EVENT_TRAIN_PROXY_IDLE);
    Task.OnBeforeDestroy := @OnBeforeDestroyTask;
    ComPortThread.AddTask(Task);
    TimerGeneralTimeout.Enabled := False;
    TimerGeneralTimeout.Interval := WaitTime;
    TimerGeneralTimeout.Enabled := True;
    WaitTimeTask := gwttQueryIsIdle;
  end;
end;

procedure TFormAwesomeThrottle.RunTractionSpeed(AliasID: Word; EmergencyStop: Boolean);
var
  Task: TTractionSpeedTask;
  Speed: single;
  CalculatedSpeed: THalfFloat;
begin
  Speed := TrackBarSpeed.Position/TrackBarSpeed.Max * 100;
  if not IsForward then
    Speed := -Speed;
  CalculatedSpeed := FloatToHalf( Speed);
  Task := TTractionSpeedTask.Create(GlobalSettings.General.AliasIDAsVal, AliasID, True, CalculatedSpeed, EmergencyStop);
  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
  ComPortThread.AddTask(Task);
end;

procedure TFormAwesomeThrottle.RunTractionFunction(AliasID: Word; Address: DWord; Value: Word);
var
  Task: TTractionFunctionTask;
begin
  Task := TTractionFunctionTask.Create(GlobalSettings.General.AliasIDAsVal, AliasID, True, Address, Value);
  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
  ComPortThread.AddTask(Task);
end;

procedure TFormAwesomeThrottle.SetAllocated(AValue: Boolean);
begin
  if FAllocated=AValue then Exit;
  FAllocated:=AValue;
end;

procedure TFormAwesomeThrottle.SetAllocatedAlias(AValue: Word);
begin
  if FAllocatedAlias <> AValue then
  begin
    FAllocatedAlias:=AValue;
    if FAllocatedAlias = 0 then
      UpdateFunctionsWithDefault
    else begin
      if GlobalSettings.Throttle.AutoLoadFDI then
      begin
        Include(FWaitingActions, wa_FDItoFunctions);
        RunProtocolSupport(FAllocatedAlias, 0);         // Kick it off
      end;
      PotentialAlias := 0;
    end;
  end;
end;

procedure TFormAwesomeThrottle.SetComPortThread(AValue: TComPortThread);
begin
  FComPortThread:=AValue;
end;

procedure TFormAwesomeThrottle.CreateFunctionUIButton(ButtonLabel: string; Level: Integer; ButtonAction: TAction);
var
  Button: TButton;
begin
  Button := TButton.Create(ScrollBoxFunctions);
  Button.Top := MaxInt;
  Button.Align := alTop;
  Button.BorderSpacing.Left := Level * 4;
  Button.BorderSpacing.Top := 4;
  Button.BorderSpacing.Right := 4;
  Button.Height := 22;;
  Button.Action := ButtonAction;
  ButtonAction.Caption := ButtonLabel;
  Button.Parent := ScrollBoxFunctions;
end;

procedure TFormAwesomeThrottle.CreateFunctionUIGroup(GroupLabel: string; Level: Integer);
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

procedure TFormAwesomeThrottle.HandleGeneralTimerResults;
var
  FormMulitipleTrains: TFormMulitipleTrains;
begin
  case WaitTimeTask of
    gwttQueryAddress :
      begin
        // We have collected up the nodes that are assigned the addres
        if AliasList.Count > 0 then
        begin
          // Oops too many results, need user to choose
          FormMulitipleTrains := TFormMulitipleTrains.Create(Application);
          FormMulitipleTrains.LoadAliasList(AliasList);
          case FormMulitipleTrains.ShowModal of
            mrOk :
              begin
                // Dialog ensures ItemIndex is not -1
                AllocatedAlias := StrToInt( FormMulitipleTrains.ListBoxTrains.Items[FormMulitipleTrains.ListBoxTrains.ItemIndex]);
                Allocated := True;
                AliasList.Clear;
                WaitTimeTask := gwttNone;
                UpdateUI
              end;
            mrClose :
              begin
                // User wants to create a new one
                WaitTimeTask := gwttQueryIsIdle;
                AliasList.Clear;
                RunTractionQueryIsIdle(TIME_QUERY_DCC_ADDRESS);
              end;
            mrCancel :
              begin
                AliasList.Clear;
                WaitTimeTask := gwttNone;
              end;
          end;
          FormMulitipleTrains.Release
        end else
        begin
          WaitTimeTask := gwttQueryIsIdle;
          AliasList.Clear;
          RunTractionQueryIsIdle(TIME_QUERY_DCC_ADDRESS);
        end;
      end;
    gwttQueryIsIdle :
      begin
        if AliasList.Count = 0 then
        begin
          ShowMessage('Error encountered, could not find a free Proxy Node to allocate');
          WaitTimeTask := gwttNone;
        end;
      end;
    gwttAllocateAddress :
      begin
        if AliasList.Count = 0 then
        begin
          AllocatedAlias := 0;
          Allocated := False;
          ShowMessage('No response from the Node, target did not respond that allocation succeeded');
        end;
        WaitTimeTask := gwttNone;
      end;
    gwttDeallocateAddress :
      begin
        if AliasList.Count = 0 then
        begin
          // We failed, just warn the user and pretend it succeeded
          ShowMessage('No response from the Node, unsure if deallocation succeeded');
          Allocated := False;
          AllocatedAlias := 0;
          AliasList.Clear;
          WaitTimeTask := gwttNone;
          UpdateUI;
        end;
      end;
  end;
end;

function TFormAwesomeThrottle.IsForward: Boolean;
begin
  REsult := RadioGroupDirection.ItemIndex = 0;
end;

function TFormAwesomeThrottle.IsShortAddress: Boolean;
begin
  Result := RadioGroupShortLong.ItemIndex = 0;
end;

procedure TFormAwesomeThrottle.OnBeforeDestroyTask(Sender: TOlcbTaskBase);
begin
  if Sender is TReadAddressSpaceMemoryTask then
  begin
    if ((Sender as TReadAddressSpaceMemoryTask).AddressSpace = MSI_FDI) and (wa_FDItoFunctions in WaitingActions) then
      UpdateFunctionsWithFDI(Sender as TReadAddressSpaceMemoryTask);
  end else
  if Sender is TProtocolSupportTask then
  begin
    if (Sender as TProtocolSupportTask).Protocols and PIP_FDI = PIP_FDI then
    begin
      if wa_FDItoFunctions in WaitingActions then
        RunReadMemorySpace(AllocatedAlias, MSI_FDI);
    end;
  end else
  if Sender is TTractionAllocateDccProxyTask then
  begin

  end else
  if Sender is TTractionQueryDccAddressProxyTask then
  begin

  end;
end;

procedure TFormAwesomeThrottle.ToggleTagOnComponent(Sender: TComponent);
begin
   if Sender.Tag = 0 then
    Sender.Tag := 1
  else
    Sender.Tag := 0;
end;

procedure TFormAwesomeThrottle.UpdateAddressRange;
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

procedure TFormAwesomeThrottle.UpdateFunctionsClearControls;
var
  i: Integer;
begin
  for i := ScrollBoxFunctions.ControlCount - 1 downto 0 do
    ScrollBoxFunctions.Controls[i].Free;
end;

procedure TFormAwesomeThrottle.UpdateFunctionsWithDefault;
var
  i: Integer;
begin
  ScrollBoxFunctions.BeginUpdateBounds;
  try
    UpdateFunctionsClearControls;
    for i := 0 to 28 do
      CreateFunctionUIButton('F' + IntToStr(i), 0, FindComponent('ActionFunction' + IntToStr(i)) as TAction) ;
  finally
      ScrollBoxFunctions.EndUpdateBounds;
  end;
end;

procedure TFormAwesomeThrottle.UpdateFunctionsWithFDI(MemTask: TReadAddressSpaceMemoryTask);

  procedure RunDownGroup(Parent: TDOMNode; Level: Integer);
  var
    Child, NameNode, NumberNode: TDOMNode;
    i: Integer;
    FunctionButton: TButton;
    Action: TAction;
    ActionName: string;
  begin
    if Assigned(Parent) then
    begin
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
          NumberNode := Child.FindNode('number');
          if Assigned(NumberNode) then
          begin
            for i := 0 to Child.Attributes.Length - 1 do
            begin
              if LowerCase(Child.Attributes.Item[i].NodeName) = 'kind' then
              begin end;
              if LowerCase(Child.Attributes.Item[i].NodeName) = 'latch' then
              begin end;
            end;
            ActionName := 'ActionFunction' + NumberNode.FirstChild.NodeValue;
            CreateFunctionUIButton(NameNode.FirstChild.NodeValue, Level, FindComponent(ActionName) as TAction);
          end else
            ShowMessage('Can not find <number> element to define the function');
        end;
        Child := Child.NextSibling;
      end;
    end;
  end;

var
  ADoc: TXMLDocument;
  SegNode: TDOMNode;
begin
  Exclude(FWaitingActions, wa_FDItoFunctions);
  MemTask.DataStream.Position := 0;
  MemTask.DataStream.Size:=MemTask.Datastream.Size - 1;  // Strip the null
  ReadXMLFile(ADoc, MemTask.DataStream);                 // This corrupts the stream from its original contents
  WriteXMLFile(ADoc, MemTask.DataStream);
  SegNode := ADoc.DocumentElement.FindNode('segment');
  ScrollBoxFunctions.BeginUpdateBounds;
  try
    UpdateFunctionsClearControls;
    RunDownGroup(SegNode, 0);
  finally
    ScrollBoxFunctions.EndUpdateBounds;
  end;
end;

procedure TFormAwesomeThrottle.EventTaskReceived(EventTask: TEventTask);

    procedure ValidateDccAddressEvent(Event: TEventID; MatchAllocatedAlias: Boolean);
    var
      DoCompare: Boolean;
    begin
      DoCompare := True;
      if MatchAllocatedAlias then
        DoCompare := (AllocatedAlias = EventTask.MessageHelper.DestinationAliasID) or (PotentialAlias = EventTask.MessageHelper.DestinationAliasID);

      if DoCompare then
      begin
        case EventTask.MessageHelper.MTI of
            MTI_PRODUCER_IDENTIFIED_UNKNOWN,
            MTI_PRODUCER_IDENTIFIED_SET,
            MTI_PRODUCER_IDENTIFIED_CLEAR :
              begin
                if (Event[0] = $06) and (Event[1] = $01) and (Event[6] = $00) and (Event[7] = $01) then
                begin
                  if IsShortAddress then
                  begin
                    if ((Event[4] shl 8) or Event[5]) = SpinEditAddress.Value then
                      AliasList.Add( Pointer( EventTask.MessageHelper.SourceAliasID));
                  end else
                  begin
                    if ((Event[4] shl 8) or Event[5]) = (SpinEditAddress.Value or $C000) then
                      AliasList.Add( Pointer( EventTask.MessageHelper.SourceAliasID));
                  end;
                end;
              end;
          end;
        end
    end;

begin
  case WaitTimeTask of
    gwttQueryAddress :
      begin
        // Looking for any DCC Address Event with our requested Address, gather and decision made in the General Timer
        ValidateDccAddressEvent(EventTask.MessageHelper.Data, False);
      end;
    gwttQueryIsIdle :
      begin
        // Looking for any Idle Proxy, if found handle here and drop only errors to the General Timer
        if (EventTask.MessageHelper.MTI = MTI_PRODUCER_IDENTIFIED_SET) and EqualEvents(@EventTask.MessageHelper.Data, @EVENT_TRAIN_PROXY_IDLE) then
        begin
          TimerGeneralTimeout.Enabled := False;   // Done
          PotentialAlias := EventTask.MessageHelper.SourceAliasID;
          AliasList.Clear;
          WaitTimeTask := gwttNone;
          RunTractionAllocateTrainByAddress( PotentialAlias, TIME_QUERY_DCC_ADDRESS);
        end
      end;
    gwttAllocateAddress :
      begin
        // Looking for the DCC Address Event from our Allocated Alias and the correct Address, if found handle here and drop only errors to the General Timer
        ValidateDccAddressEvent(EventTask.MessageHelper.Data, True);
        if AliasList.Count > 0 then
        begin
          TimerGeneralTimeout.Enabled := False;   // Done
          AllocatedAlias := PotentialAlias;
          Allocated := True;
          AliasList.Clear;
          WaitTimeTask := gwttNone;
          UpdateUI;
        end;
      end;
    gwttDeallocateAddress :
      begin
        // Looking for our Allocated Alias and Events that show it is now not allocated
        if (AllocatedAlias = EventTask.MessageHelper.SourceAliasID) and (
            ((EventTask.MessageHelper.MTI = MTI_PRODUCER_IDENTIFIED_SET) and EqualEvents(@EventTask.MessageHelper.Data, @EVENT_TRAIN_PROXY_IDLE)) or
            ((EventTask.MessageHelper.MTI = MTI_PRODUCER_IDENTIFIED_CLEAR) and EqualEvents(@EventTask.MessageHelper.Data, @EVENT_TRAIN_PROXY_INUSE))
            ) then
            begin
              TimerGeneralTimeout.Enabled := False;  // Done
              Allocated := False;
              AllocatedAlias := 0;
              AliasList.Clear;
              WaitTimeTask := gwttNone;
              UpdateUI;
            end
      end;
  end;
end;

procedure TFormAwesomeThrottle.UpdateUI;
begin
  ActionAllocationSearchForTrain.Enabled := not Allocated;
  ActionAllocationByAddress.Enabled := not Allocated;
  ActionAllocationFree.Enabled := Allocated;
  ActionAllocationRelease.Enabled := Allocated;
  GroupBoxFunctions.Enabled := Allocated;
  GroupBoxControl.Enabled := Allocated;
  GroupBoxConfiguration.Enabled := Allocated;
  RadioGroupSpeedStep.Enabled := not Allocated;
  SpinEditAddress.Enabled := not Allocated;
  if Allocated then
    LabelAllocatedAddress.Caption := IntToStr(SpinEditAddress.Value)
  else
    LabelAllocatedAddress.Caption := STR_UNASSIGNED;

  Caption := 'Open LCB Awesome Throttle - ' + LabelAllocatedAddress.Caption;
  UpdateAddressRange;
end;

end.

