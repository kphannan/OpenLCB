unit form_awesome_throttle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, ActnList, Spin, Buttons, math_float16,
  olcb_transport_layer, olcb_app_common_settings,
  olcb_utilities, olcb_defines, form_awesome_throttle_duplicate_address,
  form_config_mem_viewer, laz2_DOM, laz2_XMLRead, laz2_XMLWrite,
  form_train_config_editor, com_port_hub, ethernet_hub;

const
  ANIMATION_DELTA = 50;
  TIME_QUERY_DCC_ADDRESS = 2000;   // Wait 1s to find Proxies that are assigned to the requested DCC address
  TIME_DEALLOCATE_ADDRESS = 2000;
  STR_UNASSIGNED = 'Unassigned';

type
  TFormAwesomeThrottle = class;

  TOnThrottleEvent = procedure(Throttle: TFormAwesomeThrottle) of object;

  TGeneralWaitTimeTask = (
    gwttNone,
    gwttQueryAddress,
    gwttQueryIsIdle,
    gwttAttachAddress,
    gwttDeattachAddress
  );

  TThrottleWaitingAction = (
    wa_None
  );
  TThrottleWaitingActions = set of TThrottleWaitingAction;

  { TFormThrottleList }

  TFormThrottleList = class(TList)
  private
    function GetThrottles(Index: Integer): TFormAwesomeThrottle;
    procedure SetThrottles(Index: Integer; AValue: TFormAwesomeThrottle);
  public
    procedure Clear; override;
    procedure HideAll;
    procedure CloseAll;
    procedure ShowAll;
    property Throttles[Index: Integer]: TFormAwesomeThrottle read GetThrottles write SetThrottles;
  end;

  TAliasList = class(TList)

  end;


  { TFormAwesomeThrottle }

  TFormAwesomeThrottle = class(TForm)
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
    ActionAllocationFree: TAction;
    ActionAllocationRelease: TAction;
    ActionAllocationSearchForTrain: TAction;
    ActionAllocationByAddress: TAction;
    ActionToggleAllocationPanel: TAction;
    ActionListThrottle: TActionList;
    ButtonAllocateTrainByAddress: TButton;
    ButtonQueryFunctions: TButton;
    ButtonQuerySpeed: TButton;
    ButtonShowHideAllocatePanel: TButton;
    ButtonEditConfiguration: TButton;
    ButtonSearchForTrain: TButton;
    ButtonReleaseTrain: TButton;
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
    SpeedButton1: TSpeedButton;
    SpinEditAddress: TSpinEdit;
    TimerGeneralTimeout: TTimer;
    TimerToggleAnimation: TTimer;
    TrackBarSpeed: TTrackBar;
    procedure ActionAllocationByAddressExecute(Sender: TObject);
    procedure ActionAllocationEditCustomizationExecute(Sender: TObject);
    procedure ActionAllocationFreeExecute(Sender: TObject);
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
    procedure ActionQueryFunctionsExecute(Sender: TObject);
    procedure ActionQuerySpeedExecute(Sender: TObject);
    procedure ActionToggleAllocationPanelExecute(Sender: TObject);
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
    FComPortHub: TComPortHub;
    FConfigurationViewer: TFormTrainConfigEditor;
    FDispatchTask: TDispatchTaskFunc;
    FEthernetHub: TEthernetHub;
    FImageList16x16: TImageList;
    { private declarations }
    FOnThrottleClose: TOnThrottleEvent;
    FOnThrottleHide: TOnThrottleEvent;
    FPotentialAlias: Word;
    FWaitingActions: TThrottleWaitingActions;
    FWaitTimeTask: TGeneralWaitTimeTask;
    procedure RunWriteFdiFile(AliasID: Word; FileName: string);
    procedure RunProtocolSupport(AliasID: Word);
    procedure RunReadMemorySpace(AliasID: Word; AddressSpace: Byte);
    procedure RunReadMemorySpaceRaw(AliasID: Word; AddressSpace: Byte; StartAddress, ByteCount: DWord);
    procedure RunTractionReserveAndAttachTrainByAddress(AliasID: Word; WaitTime: Cardinal);
    procedure RunTractionDeAllocateTrainByAddress(AliasID: Word; WaitTime: Cardinal);
    procedure RunTractionQueryDccAddress(WaitTime: Cardinal);
    procedure RunTractionQueryIsIdle(WaitTime: Cardinal);
    procedure RunTractionSpeed(AliasID: Word; EmergencyStop: Boolean);
    procedure RunTractionFunction(AliasID: Word; Address: DWord; Value: Word);
    procedure RunTractionQueryFunctions(AliasID: Word; Address: DWord);
    procedure RunTractionQuerySpeed(AliasID: Word);
    procedure SetAllocated(AValue: Boolean);
    procedure SetAllocatedAlias(AValue: Word);
  protected
    procedure CreateFunctionUIButton(ButtonLabel: string; Level: Integer; ButtonAction: TAction; ButtonIndex: Integer);
    procedure CreateFunctionUIGroup(GroupLabel: string; Level: Integer);
    procedure HandleGeneralTimerResults;
    function IsForward: Boolean;
    function IsShortAddress: Boolean;
    procedure OnBeforeDestroyTask(Sender: TTaskOlcbBase);
    procedure ToggleTagOnComponent(Sender: TComponent);
    procedure UpdateAddressRange;
    procedure UpdateFunctionsClearControls;
    procedure UpdateFunctionsWithDefault;
    procedure UpdateFunctionsWithFDI(MemStream: TMemoryStream);
    property AllocationPanelToggleExpand: Boolean read FAllocationPanelToggleExpand write FAllocationPanelToggleExpand;
    property AliasList: TAliasList read FAliasList write FAliasList;
    property ComPortHub: TComPortHub read FComPortHub write FComPortHub;
    property DispatchTask: TDispatchTaskFunc read FDispatchTask write FDispatchTask;
    property EthernetHub: TEthernetHub read FEthernetHub write FEthernetHub;
    property PotentialAlias: Word read FPotentialAlias write FPotentialAlias;
    property WaitTimeTask: TGeneralWaitTimeTask read FWaitTimeTask write FWaitTimeTask;
  public
    { public declarations }
    property Allocated: Boolean read FAllocated write SetAllocated;
    property AllocatedAlias: Word read FAllocatedAlias write SetAllocatedAlias;
    property ConfigurationViewer: TFormTrainConfigEditor read FConfigurationViewer;
    property ImageList16x16: TImageList read FImageList16x16 write FImageList16x16;
    property OnThrottleHide: TOnThrottleEvent read FOnThrottleHide write FOnThrottleHide;
    property OnThrottleClose: TOnThrottleEvent read FOnThrottleClose write FOnThrottleClose;
    property WaitingActions: TThrottleWaitingActions read FWaitingActions write FWaitingActions;
    procedure EventTaskReceived(EventTask: TTaskOlcbBase);
    procedure InitTransportLayers(AnEthernetHub: TEthernetHub; AComPortHub: TComPortHub; ADispatchTaskFunc: TDispatchTaskFunc);
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

procedure TFormThrottleList.Clear;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    Throttles[i].Close;
  inherited Clear;
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
end;

{ TFormAwesomeThrottle }

procedure TFormAwesomeThrottle.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(OnThrottleClose) then
    OnThrottleClose(Self);
  ActionAllocationFree.Execute;
  CloseAction := caFree;
end;

procedure TFormAwesomeThrottle.FormCreate(Sender: TObject);
begin
  Allocated := False;
  FWaitTimeTask := gwttNone;
  FWaitingActions := [wa_None];
  FComPortHub := nil;
  FEthernetHub := nil;
  AliasList := TAliasList.Create;
  FConfigurationViewer := nil;
  FImageList16x16 := nil;
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
  if not Assigned(ConfigurationViewer) then
  begin
    FConfigurationViewer := TFormTrainConfigEditor.Create(Application);
    ConfigurationViewer.InitTransportLayers(EthernetHub, ComPortHub, DispatchTask);
    ConfigurationViewer.AliasID := AllocatedAlias;
    ConfigurationViewer.ImageList16x16 := ImageList16x16;
    ConfigurationViewer.Caption := 'Configuration Editor: Train ' + IntToStr(SpinEditAddress.Value);
    ConfigurationViewer.ShowModal;
    ConfigurationViewer.Release;
    FConfigurationViewer := nil;
  end;
end;

procedure TFormAwesomeThrottle.ActionAllocationFreeExecute(Sender: TObject);
begin
  TrackBarSpeed.Position := 0;
  RunTractionDeAllocateTrainByAddress(AllocatedAlias, TIME_DEALLOCATE_ADDRESS);
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
  RunTractionSpeed(AllocatedAlias, False);
  RunTractionSpeed(AllocatedAlias, False);
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

procedure TFormAwesomeThrottle.ActionQueryFunctionsExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to 28 do
   RunTractionQueryFunctions(AllocatedAlias, i);
end;

procedure TFormAwesomeThrottle.ActionQuerySpeedExecute(Sender: TObject);
begin
  RunTractionQuerySpeed(AllocatedAlias);
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
  LastPos: Integer;
begin
  LastPos := StrToInt(LabelPosValue.Caption);
  if LastPos <> TrackBarSpeed.Position then
  begin
    RunTractionSpeed(AllocatedAlias, False);
    LabelPosValue.Caption := IntToStr(TrackBarSpeed.Position);
  end;
end;

procedure TFormAwesomeThrottle.RunWriteFdiFile(AliasID: Word; FileName: string);
var
  FileStream: TFileStream;
  MemStream, BufferStream: TMemoryStream;
  Task: TTaskAddressSpaceMemoryWriteRawWithDatagram;
  i, Offset: Integer;
  b: Byte;
  StartFDI: Integer;
begin
  if Assigned(FComPortHub) then
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

            Task := TTaskAddressSpaceMemoryWriteRawWithDatagram.Create(GlobalSettings.General.AliasIDAsVal, AliasID, True, MSI_FDI, Offset, BufferStream);
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
  end;
end;

procedure TFormAwesomeThrottle.RunProtocolSupport(AliasID: Word);
var
  Task: TTaskProtocolSupport;
begin
  Task := TTaskProtocolSupport.Create(GlobalSettings.General.AliasIDAsVal, AliasID, True);
  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
  DispatchTask(Task);
end;

procedure TFormAwesomeThrottle.RunReadMemorySpace(AliasID: Word; AddressSpace: Byte);
var
  Task: TTaskAddressSpaceMemoryReadWithDatagram;
begin
  Task := TTaskAddressSpaceMemoryReadWithDatagram.Create(GlobalSettings.General.AliasIDAsVal,AliasID, True, AddressSpace, False);
  Task.ForceOptionalSpaceByte := False;
  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
  DispatchTask(Task);
end;

procedure TFormAwesomeThrottle.RunReadMemorySpaceRaw(AliasID: Word;  AddressSpace: Byte; StartAddress, ByteCount: DWord);
var
  Task: TTaskAddressSpaceMemoryReadRawWithDatagram;
begin
  Task := TTaskAddressSpaceMemoryReadRawWithDatagram.Create(GlobalSettings.General.AliasIDAsVal, AliasID, True, AddressSpace, StartAddress, ByteCount, True);
  Task.ForceOptionalSpaceByte := False;
  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
  DispatchTask(Task);
end;

procedure TFormAwesomeThrottle.RunTractionReserveAndAttachTrainByAddress(AliasID: Word; WaitTime: Cardinal);
var
  Task: TTaskTractionReserveAndAttachDccProxy;
  SpeedStep: Byte;
begin
  case RadioGroupSpeedStep.ItemIndex of
    0: SpeedStep := 14;
    1: SpeedStep := 28;
    2: SpeedStep := 128
  else
    SpeedStep := SPEEDSTEP_DEFAULT;
  end;
  Task := TTaskTractionReserveAndAttachDccProxy.Create(GlobalSettings.General.AliasIDAsVal, AliasID, True, SpinEditAddress.Value, IsShortAddress, SpeedStep);
  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
  if DispatchTask(Task) then
  begin
    TimerGeneralTimeout.Enabled := False;
    TimerGeneralTimeout.Interval := WaitTime;
    TimerGeneralTimeout.Enabled := True;
    WaitTimeTask := gwttAttachAddress;
  end;
end;

procedure TFormAwesomeThrottle.RunTractionDeAllocateTrainByAddress(AliasID: Word; WaitTime: Cardinal);
var
  Task: TTaskTractionReserveAndDetachDccProxy;
begin
  Task := TTaskTractionReserveAndDetachDccProxy.Create(GlobalSettings.General.AliasIDAsVal, AliasID, True, SpinEditAddress.Value, IsShortAddress);
  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
  if DispatchTask(Task) then
  begin
    TimerGeneralTimeout.Enabled := False;
    TimerGeneralTimeout.Interval := WaitTime;
    TimerGeneralTimeout.Enabled := True;
    WaitTimeTask := gwttDeattachAddress;
  end;
end;

procedure TFormAwesomeThrottle.RunTractionQueryDccAddress(WaitTime: Cardinal);
var
  Task: TTaskTractionQueryDccAddressProxy;
begin
  Task := TTaskTractionQueryDccAddressProxy.Create(GlobalSettings.General.AliasIDAsVal, 0, True, SpinEditAddress.Value, IsShortAddress);
  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
  if DispatchTask(Task) then
  begin
    TimerGeneralTimeout.Enabled := False;
    TimerGeneralTimeout.Interval := WaitTime;
    TimerGeneralTimeout.Enabled := True;
    WaitTimeTask := gwttQueryAddress;
  end;
end;

procedure TFormAwesomeThrottle.RunTractionQueryIsIdle(WaitTime: Cardinal);
var
  Task: TTaskIdentifyProducer;
begin
  Task := TTaskIdentifyProducer.Create(GlobalSettings.General.AliasIDAsVal, 0, True, EVENT_TRAIN_PROXY_IDLE);
  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
  if DispatchTask(Task) then
  begin
    TimerGeneralTimeout.Enabled := False;
    TimerGeneralTimeout.Interval := WaitTime;
    TimerGeneralTimeout.Enabled := True;
    WaitTimeTask := gwttQueryIsIdle;
  end;
end;

procedure TFormAwesomeThrottle.RunTractionSpeed(AliasID: Word; EmergencyStop: Boolean);
var
  Task: TTaskTractionSpeed;
  Speed: single;
  CalculatedSpeed: THalfFloat;
begin
  Speed := TrackBarSpeed.Position/TrackBarSpeed.Max * 100;
  if not IsForward then
    Speed := -Speed;
  CalculatedSpeed := FloatToHalf( Speed);
  Task := TTaskTractionSpeed.Create(GlobalSettings.General.AliasIDAsVal, AliasID, True, CalculatedSpeed, EmergencyStop);
  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
  DispatchTask(Task);
end;

procedure TFormAwesomeThrottle.RunTractionFunction(AliasID: Word; Address: DWord; Value: Word);
var
  Task: TTaskTractionFunction;
begin
  Task := TTaskTractionFunction.Create(GlobalSettings.General.AliasIDAsVal, AliasID, True, Address, Value);
  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
  DispatchTask(Task);
end;

procedure TFormAwesomeThrottle.RunTractionQueryFunctions(AliasID: Word; Address: DWord);
var
  Task: TTaskTractionQueryFunction;
begin
  Task := TTaskTractionQueryFunction.Create(GlobalSettings.General.AliasIDAsVal, AliasID, True, Address);
  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
  DispatchTask(Task);
end;

procedure TFormAwesomeThrottle.RunTractionQuerySpeed(AliasID: Word);
var
  Task: TTaskTractionQuerySpeed;
begin
  Task := TTaskTractionQuerySpeed.Create(GlobalSettings.General.AliasIDAsVal, AliasID, True);
  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
  DispatchTask(Task);
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
   //     Include(FWaitingActions, wa_FDItoFunctions);
   //     RunProtocolSupport(FAllocatedAlias);         // Kick it off
      end;
      PotentialAlias := 0;
    end;
  end;
end;

procedure TFormAwesomeThrottle.CreateFunctionUIButton(ButtonLabel: string;
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
    gwttAttachAddress :
      begin
        if AliasList.Count = 0 then
        begin
          AllocatedAlias := 0;
          Allocated := False;
          ShowMessage('No response from the Node, target did not respond that allocation succeeded');
        end;
        WaitTimeTask := gwttNone;
      end;
    gwttDeattachAddress :
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

procedure TFormAwesomeThrottle.OnBeforeDestroyTask(Sender: TTaskOlcbBase);
begin
  if Sender is TTaskAddressSpaceMemoryWriteRawWithDatagram then
  begin

  end else
  if Sender is TTaskAddressSpaceMemoryReadRawWithDatagram then
  begin

  end else
  if Sender is TTaskProtocolSupport then
  begin

  end else
  if Sender is TTaskTractionReserveAndAttachDccProxy then
  begin
    // Looking for the DCC Address Attach Result from our Allocated Alias and the correct Address, if found handle here and drop only errors to the General Timer
    if PotentialAlias = TTaskTractionReserveAndAttachDccProxy( Sender).MessageHelper.DestinationAliasID then
    begin
      TimerGeneralTimeout.Enabled := False;   // Done
      WaitTimeTask := gwttNone;
      AliasList.Clear;
      if TTaskTractionReserveAndAttachDccProxy( Sender).ReplyCode <= 0 then          // If it is zero or not sent then all is good .... for now..... this will change
      begin
        AliasList.Add( Pointer( TTaskTractionReserveAndAttachDccProxy( Sender).MessageHelper.SourceAliasID));
        AllocatedAlias := PotentialAlias;
        Allocated := True;
        UpdateUI;
      end else
      begin
        ShowMessage('Error Code: ' + IntToStr(TTaskTractionReserveAndAttachDccProxy( Sender).ReplyCode) + ': Unable to Attach the DCC Address: ' + IntToStr(TTaskTractionReserveAndAttachDccProxy( Sender).ReplyCode));
        AllocatedAlias := 0;
        Allocated := False;
        UpdateUI;
      end;
    end;
  end else
  if Sender is TTaskTractionReserveAndDetachDccProxy then
  begin
    // Looking for our Allocated Alias and Events that show it is now not allocated
    if AllocatedAlias = TTaskTractionReserveAndDetachDccProxy( Sender).MessageHelper.DestinationAliasID then
    begin
      TimerGeneralTimeout.Enabled := False;   // Done
      WaitTimeTask := gwttNone;
      AliasList.Clear;
      if TTaskTractionReserveAndDetachDccProxy( Sender).ReplyCode <= 0 then          // If it is zero or not sent then all is good .... for now..... this will change
      begin
        Allocated := False;
        AllocatedAlias := 0;
        UpdateUI;
      end else
      begin
        ShowMessage('Error Code: ' + IntToStr(TTaskTractionReserveAndDetachDccProxy( Sender).ReplyCode) + ': Unable to detach the DCC Address: ' + IntToStr(TTaskTractionReserveAndDetachDccProxy( Sender).ReplyAddress));
      end;
    end
  end else
  if Sender is TTaskTractionQueryDccAddressProxy then
  begin

  end;
end;

procedure TFormAwesomeThrottle.ToggleTagOnComponent(Sender: TComponent);
begin
  if Sender.Tag = 0 then
  begin
   Sender.Tag := 1;
  end else
  begin
    Sender.Tag := 0;
  end;
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
      CreateFunctionUIButton('F' + IntToStr(i), 0, FindComponent('ActionFunction' + IntToStr(i)) as TAction, i) ;
  finally
      ScrollBoxFunctions.EndUpdateBounds;
  end;
end;

procedure TFormAwesomeThrottle.UpdateFunctionsWithFDI(MemStream: TMemoryStream);
//
// REQUIRES A NULL TERMINATOR!!!!
//

  procedure RunDownGroup(Parent: TDOMNode; Level: Integer);
  var
    Child, NameNode, NumberNode: TDOMNode;
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

procedure TFormAwesomeThrottle.EventTaskReceived(EventTask: TTaskOlcbBase);

    procedure ValidateDccAddressEvent(Event: TEventID; MatchAllocatedAlias: Boolean);
    var
      DoCompare: Boolean;
    begin
      DoCompare := True;
      if MatchAllocatedAlias then
        DoCompare := (AllocatedAlias = EventTask.MessageHelper.SourceAliasID) or (PotentialAlias = EventTask.MessageHelper.SourceAliasID);

      if DoCompare then
      begin
        case EventTask.MessageHelper.MTI of
            MTI_PRODUCER_IDENTIFIED_UNKNOWN,
            MTI_PRODUCER_IDENTIFIED_SET,
            MTI_PRODUCER_IDENTIFIED_CLEAR :
              begin
                if (Event[0] = $06) and (Event[1] = $01) and (Event[6] = $03) and (Event[7] = $03) then
                begin
                  if IsShortAddress then
                  begin
                    if Integer((Event[4] shl 8) or Event[5]) = SpinEditAddress.Value then
                      AliasList.Add( Pointer( EventTask.MessageHelper.SourceAliasID));
                  end else
                  begin
                    if Integer((Event[4] shl 8) or Event[5]) = (SpinEditAddress.Value or $C000) then
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
          RunTractionReserveAndAttachTrainByAddress( PotentialAlias, TIME_QUERY_DCC_ADDRESS);
        end
      end;
  end;
end;

procedure TFormAwesomeThrottle.InitTransportLayers(AnEthernetHub: TEthernetHub; AComPortHub: TComPortHub; ADispatchTaskFunc: TDispatchTaskFunc);
begin
  FDispatchTask := ADispatchTaskFunc;
  FEthernetHub := AnEthernetHub;
  FComPortHub := AComPortHub;
  if Assigned(ConfigurationViewer) then
    ConfigurationViewer.InitTransportLayers(AnEthernetHub, AComPortHub, ADispatchTaskFunc)
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

