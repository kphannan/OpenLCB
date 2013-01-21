unit form_awesome_throttle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, ActnList, Spin, math_float16, olcb_threaded_stack,
  olcb_common_tasks, olcb_app_common_settings, olcb_utilities, olcb_defines,
  form_awesome_throttle_duplicate_address;

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
    ActionAllocationEditConfiguration: TAction;
    ActionAllocationLoadConfiguration: TAction;
    ActionAllocationFree: TAction;
    ActionAllocationRelease: TAction;
    ActionAllocationSearchForTrain: TAction;
    ActionAllocationByAddress: TAction;
    ActionToggleAllocationPanel: TAction;
    ActionListThrottle: TActionList;
    ButtonAllocateTrainByAddress: TButton;
    ButtonShowHideAllocatePanel: TButton;
    ButtonF7: TButton;
    ButtonF6: TButton;
    ButtonF5: TButton;
    ButtonF4: TButton;
    ButtonF11: TButton;
    ButtonF10: TButton;
    ButtonF9: TButton;
    ButtonF8: TButton;
    ButtonEditConfiguration: TButton;
    ButtonSearchForTrain: TButton;
    ButtonReleaseTrain: TButton;
    ButtonF12: TButton;
    ButtonLoadConfiguration: TButton;
    ButtonFreeTrain: TButton;
    ButtonStop: TButton;
    ButtonEStop: TButton;
    ButtonF0: TButton;
    ButtonF1: TButton;
    ButtonF3: TButton;
    ButtonF2: TButton;
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
    RadioGroupShortLong: TRadioGroup;
    RadioGroupDirection: TRadioGroup;
    RadioGroupSpeedStep: TRadioGroup;
    RadioGroupSpeedScale: TRadioGroup;
    SpinEditAddress: TSpinEdit;
    TimerGeneralTimeout: TTimer;
    TimerToggleAnimation: TTimer;
    TrackBarSpeed: TTrackBar;
    procedure ActionAllocationByAddressExecute(Sender: TObject);
    procedure ActionAllocationEditConfigurationExecute(Sender: TObject);
    procedure ActionAllocationFreeExecute(Sender: TObject);
    procedure ActionAllocationLoadConfigurationExecute(Sender: TObject);
    procedure ActionAllocationReleaseExecute(Sender: TObject);
    procedure ActionAllocationSearchForTrainExecute(Sender: TObject);
    procedure ActionControlStopExecute(Sender: TObject);
    procedure ActionFunction0Execute(Sender: TObject);
    procedure ActionFunction10Execute(Sender: TObject);
    procedure ActionFunction11Execute(Sender: TObject);
    procedure ActionFunction12Execute(Sender: TObject);
    procedure ActionFunction1Execute(Sender: TObject);
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
    FWaitTimeTask: TGeneralWaitTimeTask;
    procedure RunTractionAllocateTrainByAddress(AliasID: Word; WaitTime: Cardinal);
    procedure RunTractionDeAllocateTrainByAddress(AliasID: Word; WaitTime: Cardinal);
    procedure RunTractionQueryDccAddress(WaitTime: Cardinal);
    procedure RunTractionQueryIsIdle(WaitTime: Cardinal);
    procedure RunTractionSpeed(AliasID: Word; EmergencyStop: Boolean);
    procedure RunTractionFunction(AliasID: Word; Address: DWord; Value: Word);
    procedure SetAllocated(AValue: Boolean);
  protected
    procedure HandleGeneralTimerResults;
    function IsForward: Boolean;
    function IsShortAddress: Boolean;
    procedure OnBeforeDestroyTask(Sender: TOlcbTaskBase);
    procedure ToggleTagOnComponent(Sender: TComponent);
    procedure UpdateAddressRange;
    property AllocationPanelToggleExpand: Boolean read FAllocationPanelToggleExpand write FAllocationPanelToggleExpand;
    property AliasList: TAliasList read FAliasList write FAliasList;
    property WaitTimeTask: TGeneralWaitTimeTask read FWaitTimeTask write FWaitTimeTask;
  public
    { public declarations }
    property Allocated: Boolean read FAllocated write SetAllocated;
    property AllocatedAlias: Word read FAllocatedAlias;
    property ComPortThread: TComPortThread read FComPortThread write FComPortThread;
    property OnThrottleHide: TOnThrottleEvent read FOnThrottleHide write FOnThrottleHide;
    property OnThrottleClose: TOnThrottleEvent read FOnThrottleClose write FOnThrottleClose;
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
  for i := 0 to Count - 1 do
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

procedure TFormAwesomeThrottle.ActionAllocationEditConfigurationExecute(Sender: TObject);
begin

end;

procedure TFormAwesomeThrottle.ActionAllocationFreeExecute(Sender: TObject);
begin
  TrackBarSpeed.Position := 0;
  RunTractionDeAllocateTrainByAddress(AllocatedAlias, TIME_DEALLOCATE_ADDRESS);
end;

procedure TFormAwesomeThrottle.ActionAllocationLoadConfigurationExecute(Sender: TObject);
begin

end;

procedure TFormAwesomeThrottle.ActionAllocationReleaseExecute(Sender: TObject);
begin
  AliasList.Clear;        // Just let it go
  Allocated := False;
  FAllocatedAlias := 0;
  UpdateUI;
end;

procedure TFormAwesomeThrottle.ActionAllocationSearchForTrainExecute(Sender: TObject);
begin

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

procedure TFormAwesomeThrottle.ActionFunction1Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(AllocatedAlias, 1, (Sender as TComponent).Tag)
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
  UpdateUI
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
                FAllocatedAlias := StrToInt( FormMulitipleTrains.ListBoxTrains.Items[FormMulitipleTrains.ListBoxTrains.ItemIndex]);
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
          FAllocatedAlias := 0;
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
          FAllocatedAlias := 0;
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

procedure TFormAwesomeThrottle.EventTaskReceived(EventTask: TEventTask);

    procedure ValidateDccAddressEvent(Event: TEventID; MatchAllocatedAlias: Boolean);
    begin
      if MatchAllocatedAlias then
        if AllocatedAlias <> EventTask.MessageHelper.SourceAliasID then
          Exit;

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
          FAllocatedAlias := EventTask.MessageHelper.SourceAliasID;
          AliasList.Clear;
          WaitTimeTask := gwttNone;
          RunTractionAllocateTrainByAddress( AllocatedAlias, TIME_QUERY_DCC_ADDRESS);
        end
      end;
    gwttAllocateAddress :
      begin
        // Looking for the DCC Address Event from our Allocated Alias and the correct Address, if found handle here and drop only errors to the General Timer
        ValidateDccAddressEvent(EventTask.MessageHelper.Data, True);
        if AliasList.Count > 0 then
        begin
          TimerGeneralTimeout.Enabled := False;   // Done
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
              FAllocatedAlias := 0;
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
  GroupBoxConfiguration.Enabled := not Allocated;
  RadioGroupSpeedStep.Enabled := not Allocated;
  SpinEditAddress.Enabled := not Allocated;
  if Allocated then
    LabelAllocatedAddress.Caption := IntToStr(SpinEditAddress.Value)
  else
    LabelAllocatedAddress.Caption := STR_UNASSIGNED;

  UpdateAddressRange;
end;

end.

