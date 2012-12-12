unit statemachine_engine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, serialport_thread, olcb_app_common_settings, file_utilities;

type

  { TStateMachineBase }

  TStateMachineBase = class
  private
    FiState: Integer;
    function GetDone: Boolean;
  protected
    constructor Create;
    destructor Destroy; override;
    procedure Process(ComPortThread: TComPortThread); virtual; abstract;                 // Must override this
    property iState: Integer read FiState write FiState;
    property Done: Boolean read GetDone;
  public

  end;

  { TStateMachineEngine }

  TStateMachineEngine = class
  private
    FComPortThread: TComPortThread;
    FThreadList: TThreadList;
    FTimer: TTimer;
    function GetCount: Integer;
  protected
    property ThreadList: TThreadList read FThreadList write FThreadList;
    property Timer: TTimer read FTimer write FTimer;
    procedure OnTimerTick(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(NewMachine: TStateMachineBase);
    procedure Clear;
    property ComPortThread: TComPortThread read FComPortThread write FComPortThread;
    property Count: Integer read GetCount;
  end;

var
  StateMachineEngine: TStateMachineEngine;

implementation

{ TStateMachineEngine }

function TStateMachineEngine.GetCount: Integer;
var
  List: TList;
begin
  List := ThreadList.LockList;
  try
    Result := List.Count;
  finally
    ThreadList.UnlockList
  end;
end;

procedure TStateMachineEngine.OnTimerTick(Sender: TObject);
var
  StateMachine: TStateMachineBase;
  List: TList;
begin
  List := ThreadList.LockList;
  try
 {   if List.Count > 0 then
    begin
      StateMachine := TStateMachineBase( ThreadList[0]);
      if Assigned(ComPortThread) then
        StateMachine.Process(ComPortThread);
      if StateMachine.Done then
        ThreadList.Remove(StateMachine);
    end else
      Timer.Enabled := False;  }
  finally
    ThreadList.UnlockList;
  end;
end;

constructor TStateMachineEngine.Create;
begin
  inherited;
  FThreadList := TThreadList.Create;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.OnTimer := @OnTimerTick;
end;

destructor TStateMachineEngine.Destroy;
begin
  Clear;
  FreeAndNil(FThreadList);
  inherited Destroy;
end;

procedure TStateMachineEngine.Add(NewMachine: TStateMachineBase);
var
  List: TList;
begin
  if not Timer.Enabled then
    Timer.Enabled := True;
  List := ThreadList.LockList;
  try
    List.Add(NewMachine);
  finally
    ThreadList.UnlockList;
  end;
end;

procedure TStateMachineEngine.Clear;
var
  i: Integer;
  List: TList;
begin
  List := ThreadList.LockList;
  try
    for i := 0 to List.Count - 1 do
      TObject( List[i]).Free
  finally
    List.Clear;
    ThreadList.UnLockList
  end;
end;

{ TStateMachineBase }

function TStateMachineBase.GetDone: Boolean;
begin

end;

constructor TStateMachineBase.Create;
begin
  inherited
end;

destructor TStateMachineBase.Destroy;
begin
  inherited Destroy;
end;

initialization
  StateMachineEngine := TStateMachineEngine.Create;

finalization
  FreeAndNil(StateMachineEngine);

end.

