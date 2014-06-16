unit form_throttle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, ActnList, Spin, Buttons, math_float16,
  olcb_transport_layer, olcb_app_common_settings,
  olcb_utilities, olcb_defines,
  laz2_DOM, laz2_XMLRead, laz2_XMLWrite,
  form_train_config_editor, com_port_hub, ethernet_hub,
  template_userstatemachine, opstackdefines, opstacknode, opstackcore;

const
  ANIMATION_DELTA = 50;
  TIME_QUERY_DCC_ADDRESS = 2000;   // Wait 1s to find Proxies that are assigned to the requested DCC address
  TIME_DEALLOCATE_ADDRESS = 2000;
  STR_UNASSIGNED = 'Unassigned';

type
  TFormThrottle = class;

  TOnThrottleEvent = procedure(Throttle: TFormThrottle) of object;

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
    function CreateThrottle(AnEthernetHub: TEthernetHub; AComPortHub: TComPortHub; ADispatchTaskFunc: TDispatchTaskFunc; ImageList16x16: TImageList): TFormThrottle;
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
    ActionAllocationFree: TAction;
    ActionAllocationRelease: TAction;
    ActionAllocationByList: TAction;
    ActionAllocationByAddress: TAction;
    ActionToggleAllocationPanel: TAction;
    ActionListThrottle: TActionList;
    ButtonAllocateTrainByAddress: TButton;
    ButtonEditConfiguration: TButton;
    ButtonEStop: TButton;
    ButtonFreeTrain: TButton;
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
    procedure SpinEditAddressChange(Sender: TObject);
    procedure TimerGeneralTimer(Sender: TObject);
    procedure TimerToggleAnimationTimer(Sender: TObject);
    procedure TrackBarSpeedChange(Sender: TObject);
  private
    FAllocated: Boolean;
    FClosing: Boolean;
    FCurrentFunctions: DWord;
    FCurrentFuntions: DWord;
    FCurrentSpeed: THalfFloat;
    FOPStackNode: PNMRAnetNode;
    FPhysicalNodeState: TOlcbNodeState;
    FThrottleAlias: Word;
    FTrainAlias: Word;
    FAllocationPanelToggleExpand: Boolean;
    FComPortHub: TComPortHub;
    FConfigurationViewer: TFormTrainConfigEditor;
    FDispatchTask: TDispatchTaskFunc;
    FEthernetHub: TEthernetHub;
    FImageList16x16: TImageList;
    { private declarations }
    FOnThrottleClose: TOnThrottleEvent;
    FOnThrottleHide: TOnThrottleEvent;
    function GetCurrentFuntions: DWord;
    procedure RunWriteFdiFile(AliasID: Word; FileName: string);
    procedure RunProtocolSupport(AliasID: Word);
    procedure RunReadMemorySpace(AliasID: Word; AddressSpace: Byte);
    procedure RunReadMemorySpaceRaw(AliasID: Word; AddressSpace: Byte; StartAddress, ByteCount: DWord);
    procedure RunTractionSpeed(AliasID: Word; EmergencyStop: Boolean);
    procedure RunTractionFunction(Address: DWord; Value: Word);
    procedure RunTractionQueryFunctions(AliasID: Word; Address: DWord);
    procedure RunTractionQuerySpeed(AliasID: Word);
    procedure SetAllocatedAlias(AValue: Word);
    procedure SetPhysicalNodeState(AValue: TOlcbNodeState);
  protected
    procedure CreateFunctionUIButton(ButtonLabel: string; Level: Integer; ButtonAction: TAction; ButtonIndex: Integer);
    procedure CreateFunctionUIGroup(GroupLabel: string; Level: Integer);
    function GetSpeedStep: Byte;
    function IsForward: Boolean;
    function IsShortAddress: Boolean;
    procedure OnBeforeDestroyTask(Sender: TTaskOlcbBase);
    procedure ToggleTagOnComponent(Sender: TComponent);
    procedure UpdateAddressRange;
    procedure UpdateFunctionsClearControls;
    procedure UpdateFunctionsWithDefault;
    procedure UpdateFunctionsWithFDI(MemStream: TMemoryStream);
    property AllocationPanelToggleExpand: Boolean read FAllocationPanelToggleExpand write FAllocationPanelToggleExpand;
    property ComPortHub: TComPortHub read FComPortHub write FComPortHub;
    property DispatchTask: TDispatchTaskFunc read FDispatchTask write FDispatchTask;
    property EthernetHub: TEthernetHub read FEthernetHub write FEthernetHub;
    property Closing: Boolean read FClosing write FClosing;
  public
    { public declarations }
    property TrainAlias: Word read FTrainAlias write SetAllocatedAlias;
    property ThrottleAlias: Word read FThrottleAlias write FThrottleAlias;
    property ConfigurationViewer: TFormTrainConfigEditor read FConfigurationViewer;
    property ImageList16x16: TImageList read FImageList16x16 write FImageList16x16;
    property OPStackNode: PNMRAnetNode read FOPStackNode write FOPStackNode;
    property PhysicalNodeState: TOlcbNodeState read FPhysicalNodeState write SetPhysicalNodeState;
    property OnThrottleHide: TOnThrottleEvent read FOnThrottleHide write FOnThrottleHide;
    property OnThrottleClose: TOnThrottleEvent read FOnThrottleClose write FOnThrottleClose;

    property CurrentSpeedDir: THalfFloat read FCurrentSpeed write FCurrentSpeed;
    property CurrentFunctions: DWord read GetCurrentFuntions write FCurrentFunctions;

    procedure InitTransportLayers(AnEthernetHub: TEthernetHub; AComPortHub: TComPortHub; ADispatchTaskFunc: TDispatchTaskFunc);
    procedure UpdateStatus(NewStatus: string);
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

function TThrottleList.CreateThrottle(AnEthernetHub: TEthernetHub; AComPortHub: TComPortHub; ADispatchTaskFunc: TDispatchTaskFunc; ImageList16x16: TImageList): TFormThrottle;
begin
  Result := TFormThrottle.Create(Application.MainForm);
  if Result <> nil then
  begin
    Self.Add(Result);
    Result.OnThrottleClose := @DoThrottleClose;
    Result.OnThrottleHide := @DoThrottleHide;
    Result.InitTransportLayers(AnEthernetHub, AComPortHub, ADispatchTaskFunc);
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
  OPStackNode_MarkForRelease(OPStackNode);
  if Assigned(OnThrottleClose) then
    OnThrottleClose(Self);
  ActionAllocationFree.Execute;
  CloseAction := caFree;
end;

procedure TFormThrottle.FormCreate(Sender: TObject);
begin
  FComPortHub := nil;
  FEthernetHub := nil;
  FConfigurationViewer := nil;
  FImageList16x16 := nil;
  ThrottleAlias := 0;
  TrainAlias := 0;
  FClosing := False;
  FCurrentSpeed := $0000;
  FCurrentFunctions := 0;
  FOPStackNode := nil;
end;

procedure TFormThrottle.ActionToggleAllocationPanelExecute(Sender: TObject);
begin
  AllocationPanelToggleExpand := Width = 392;
  TimerToggleAnimation.Enabled := True;
end;

procedure TFormThrottle.ActionToggleDirExecute(Sender: TObject);
begin
  if RadioGroupDirection.ItemIndex = 0 then
    RadioGroupDirection.ItemIndex := 1
  else
    RadioGroupDirection.ItemIndex := 0;

end;

procedure TFormThrottle.ActionAllocationByAddressExecute(Sender: TObject);
var
  Task: TTaskTractionProxyAllocateByTrainID;
begin
  if Assigned(OPStackNode) and (OPstackNode^.TrainData.LinkedNode.AliasID <> 0) then
  begin
    // Linked Node is the Proxy....
    Task := TTaskTractionProxyAllocateByTrainID.Create(OPStackNode^.Info.AliasID, OPstackNode^.TrainData.LinkedNode.AliasID, SpinEditAddress.Value, GetSpeedStep, OPStackNode);
    Task.OnBeforeDestroy := @OnBeforeDestroyTask;
    EthernetHub.AddTask(Task);
    ComPortHub.AddTask(Task);
    FreeAndNil(Task);
  end;
end;

procedure TFormThrottle.ActionAllocationEditCustomizationExecute(Sender: TObject);
begin
  if not Assigned(ConfigurationViewer) then
  begin
    FConfigurationViewer := TFormTrainConfigEditor.Create(Application);
    ConfigurationViewer.InitTransportLayers(EthernetHub, ComPortHub, DispatchTask);
    ConfigurationViewer.AliasID := TrainAlias;
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
  UpdateUI;
end;

procedure TFormThrottle.ActionControlEmergencyStopExecute(Sender: TObject);
begin
  TrackBarSpeed.Position := 0;
  RunTractionSpeed(TrainAlias, True);
  RunTractionSpeed(TrainAlias, True);
  RunTractionSpeed(TrainAlias, True);
end;

procedure TFormThrottle.ActionControlStopExecute(Sender: TObject);
begin
  TrackBarSpeed.Position := 0;
  RunTractionSpeed(TrainAlias, False);
  RunTractionSpeed(TrainAlias, False);
end;

procedure TFormThrottle.ActionFunction0Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(0, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction10Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(10, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction11Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(11, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction12Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(12, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction13Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(13, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction14Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(14, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction15Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(15, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction16Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(16, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction17Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(17, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction18Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(18, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction19Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(19, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction1Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(1, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction20Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(20, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction21Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(21, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction22Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(22, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction23Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(23, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction24Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(24, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction25Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(25, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction26Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(26, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction27Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(27, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction28Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(28, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction2Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(2, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction3Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(3, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction4Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(4, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction5Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(5, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction6Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(6, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction7Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(7, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction8Execute(Sender: TObject);
begin
   ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(8, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionFunction9Execute(Sender: TObject);
begin
  ToggleTagOnComponent(Sender as TComponent);
  RunTractionFunction(9, (Sender as TComponent).Tag)
end;

procedure TFormThrottle.ActionQueryFunctionsExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to 28 do
   RunTractionQueryFunctions(TrainAlias, i);
end;

procedure TFormThrottle.ActionQuerySpeedExecute(Sender: TObject);
begin
  RunTractionQuerySpeed(TrainAlias);
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

function TFormThrottle.GetCurrentFuntions: DWord;
begin
  Result := FCurrentFunctions;
end;

function TFormThrottle.GetSpeedStep: Byte;
begin
  case RadioGroupSpeedStep.ItemIndex of
    0 : Result := 14;
    1 : Result := 28;
    2 : Result := 128
  else
    Result := 28;
  end;
end;

procedure TFormThrottle.RadioGroupDirectionClick(Sender: TObject);
begin
  RunTractionSpeed(TrainAlias, False);
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
    RunTractionSpeed(TrainAlias, False);
    LabelPosValue.Caption := IntToStr(TrackBarSpeed.Position);
  end;
end;

procedure TFormThrottle.RunWriteFdiFile(AliasID: Word; FileName: string);
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

            Task := TTaskAddressSpaceMemoryWriteRawWithDatagram.Create(ThrottleAlias, AliasID, True, MSI_FDI, Offset, BufferStream);
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

procedure TFormThrottle.RunProtocolSupport(AliasID: Word);
{var
  Task: TTaskProtocolSupport; }
begin
 { Task := TTaskProtocolSupport.Create(ThrottleAlias, AliasID, True);
  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
  DispatchTask(Task);   }
end;

procedure TFormThrottle.RunReadMemorySpace(AliasID: Word; AddressSpace: Byte);
{var
  Task: TTaskAddressSpaceMemoryReadWithDatagram; }
begin
 { Task := TTaskAddressSpaceMemoryReadWithDatagram.Create(ThrottleAlias, AliasID, True, AddressSpace, False);
  Task.ForceOptionalSpaceByte := False;
  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
  DispatchTask(Task);  }
end;

procedure TFormThrottle.RunReadMemorySpaceRaw(AliasID: Word;  AddressSpace: Byte; StartAddress, ByteCount: DWord);
{var
  Task: TTaskAddressSpaceMemoryReadRawWithDatagram;    }
begin
 { Task := TTaskAddressSpaceMemoryReadRawWithDatagram.Create(ThrottleAlias, AliasID, True, AddressSpace, StartAddress, ByteCount, True);
  Task.ForceOptionalSpaceByte := False;
  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
  DispatchTask(Task);  }
end;

procedure TFormThrottle.RunTractionSpeed(AliasID: Word; EmergencyStop: Boolean);
var
  Speed: single;
begin
  Speed := TrackBarSpeed.Position/TrackBarSpeed.Max * 100;
  if not IsForward then
    Speed := -Speed;
  CurrentSpeedDir := FloatToHalf( Speed);
end;

procedure TFormThrottle.RunTractionFunction(Address: DWord; Value: Word);
var
  Mask: DWord;
begin
  Mask := $00000001;
  Mask := Mask shl Address;
  if Value = 0 then
    FCurrentFunctions := CurrentFunctions and not Mask
  else
    FCurrentFunctions := CurrentFunctions or Mask
end;

procedure TFormThrottle.RunTractionQueryFunctions(AliasID: Word; Address: DWord);
{var
  Task: TTaskTractionQueryFunction;}
begin
  {
  Task := TTaskTractionQueryFunction.Create(ThrottleAlias, AliasID, True, Address);
  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
  DispatchTask(Task);   }
end;

procedure TFormThrottle.RunTractionQuerySpeed(AliasID: Word);
{var
  Task: TTaskTractionQuerySpeed;}
begin
{  Task := TTaskTractionQuerySpeed.Create(ThrottleAlias, AliasID, True);
  Task.OnBeforeDestroy := @OnBeforeDestroyTask;
  DispatchTask(Task);  }
end;

procedure TFormThrottle.SetAllocatedAlias(AValue: Word);
begin
  FTrainAlias := AValue;
end;

procedure TFormThrottle.SetPhysicalNodeState(AValue: TOlcbNodeState);
begin
  begin
    if FPhysicalNodeState <> AValue then
    begin
      FPhysicalNodeState := AValue;

      case PhysicalNodeState of
        ons_Disabled :
          begin
            OPStackNode^.Info.AliasID := 0;
            Statusbar.Panels[0].Text := 'NodeID: [None]';
            Statusbar.Panels[1].Text := 'Alias: [None]';
            UpdateUI;
          end;
        ons_Started :
          begin
            Statusbar.Panels[0].Text := 'Starting Node...';
            Statusbar.Panels[1].Text := '';
          end;
        ons_LoggingIn :
          begin
            Statusbar.Panels[0].Text := 'NodeID: [None]';
            Statusbar.Panels[1].Text := 'Allocating...';
          end;
        ons_Permitted :
          begin
            if Assigned(OPStackNode) then
            begin
              Statusbar.Panels[0].Text := 'NodeID: ' + NodeIDToDotHex(OPStackNode^.Info.ID);
              Statusbar.Panels[1].Text := 'Alias: 0x' + IntToHex(OPStackNode^.Info.AliasID, 4);
              PanelMain.Enabled := True;
            end else
              PhysicalNodeState := ons_Disabled;
            UpdateUI;
          end;
      end
    end;
  end;
end;

procedure TFormThrottle.SpinEditAddressChange(Sender: TObject);
begin
  UpdateAddressRange
end;

procedure TFormThrottle.TimerGeneralTimer(Sender: TObject);
begin
  if Assigned(OPStackNode) then
  begin
    if OPStackNode^.State and NS_PERMITTED <> 0 then
      PhysicalNodeState := ons_Permitted
    else
    if OPStackNode^.State and NS_INITIALIZED <> 0 then
      PhysicalNodeState := ons_LoggingIn
  end;
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

function TFormThrottle.IsForward: Boolean;
begin
  Result := RadioGroupDirection.ItemIndex = 0;
end;

function TFormThrottle.IsShortAddress: Boolean;
begin
  Result := RadioGroupShortLong.ItemIndex = 0;
end;

procedure TFormThrottle.OnBeforeDestroyTask(Sender: TTaskOlcbBase);
var
  ProxyAllocate: TTaskTractionProxyAllocateByTrainID;
begin
  if Sender is TTaskTractionProxyAllocateByTrainID then
  begin
    ProxyAllocate := TTaskTractionProxyAllocateByTrainID( Sender);
    if (ProxyAllocate.ErrorCode = TRACTION_PROXY_MANAGE_RESERVE_REPLY_OK) and (ProxyAllocate.TrainNodeID.AliasID <> 0) then
    begin
      TrainAlias := ProxyAllocate.TrainNodeID.AliasID;
      UpdateUI
    end else
      ShowMessage('Unable to Allocate Train, please try again');
  end;
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
  if SpinEditAddress.Value > 127 then
    RadioGroupShortLong.ItemIndex := 1;
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

procedure TFormThrottle.UpdateFunctionsWithFDI(MemStream: TMemoryStream);
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

procedure TFormThrottle.UpdateStatus(NewStatus: string);
begin
  StatusBar.Panels[0].Text := NewStatus;
end;

procedure TFormThrottle.InitTransportLayers(AnEthernetHub: TEthernetHub; AComPortHub: TComPortHub; ADispatchTaskFunc: TDispatchTaskFunc);
begin
  FDispatchTask := ADispatchTaskFunc;
  FEthernetHub := AnEthernetHub;
  FComPortHub := AComPortHub;
  if Assigned(ConfigurationViewer) then
    ConfigurationViewer.InitTransportLayers(AnEthernetHub, AComPortHub, ADispatchTaskFunc)
end;

procedure TFormThrottle.UpdateUI;
begin
  ActionAllocationByList.Enabled :=  TrainAlias = 0;
  ActionAllocationByAddress.Enabled := TrainAlias = 0;
//  ActionAllocationFree.Enabled := TrainAlias <> 0; ;
//  ActionAllocationRelease.Enabled := TrainAlias <> 0; ;
  GroupBoxFunctions.Enabled := TrainAlias <> 0;
  GroupBoxControl.Enabled := TrainAlias <> 0;
  GroupBoxConfiguration.Enabled := TrainAlias <> 0;
  RadioGroupSpeedStep.Enabled := TrainAlias = 0;
  SpinEditAddress.Enabled := TrainAlias = 0;
  if TrainAlias <> 0 then
    LabelAllocatedAddress.Caption := IntToStr(SpinEditAddress.Value)
  else
    LabelAllocatedAddress.Caption := STR_UNASSIGNED;

  Caption := 'Open LCB Throttle - ' + LabelAllocatedAddress.Caption;
  UpdateAddressRange;
end;

end.

