unit hardware_template;

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  {$IFDEF FPC}
  Classes, SysUtils, blcksock, synsock,
  {$ENDIF}
  opstackbuffers,
  opstackdefines;

// *****************************************************************************
//  Lazarus Specific from here down
// *****************************************************************************
{$IFDEF FPC}
type
  TEthernetThreadType = (ETT_Reader, ETT_Writer);
  TOpStackTestCallbackMethod = procedure(ReceivedStr: ansistring) of object;
  TOpStackTestRunningCallbackMethod = procedure(EthernetThreadType: TEthernetThreadType) of object;

  { TOPStackTestListener }

  TOPStackTestListener = class(TThread)
  private
    FCallback: TOpStackTestCallbackMethod;
    FConnectionInputList: TList;
    FConnectionOutputList: TList;
    FListenStrings: TStringList;
    FReceiveStr: ansistring;
    FRunningCallback: TOpStackTestRunningCallbackMethod;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
    procedure Execute; override;
    procedure Send(StringList: TStringList);

    property ReceiveStr: ansistring read FReceiveStr write FReceiveStr;
    property ListenStrings: TStringList read FListenStrings write FListenStrings;
    property Callback: TOpStackTestCallbackMethod read FCallback write FCallback;
    property ConnectionInputList: TList read FConnectionInputList write FConnectionInputList;
    property ConnectionOutputList: TList read FConnectionOutputList write FConnectionOutputList;
    property RunningCallback: TOpStackTestRunningCallbackMethod read FRunningCallback write FRunningCallback;
  end;

  { TOPStackTestConnection }

  { TOPStackTestConnectionInput }

  TOPStackTestConnectionInput = class(TThread)
  private
    FCallback: TOpStackTestCallbackMethod;
    FhSocket: TSocket;
    FListenStrings: TStringList;
    FReceiveStr: ansistring;
    FRunningCallback: TOpStackTestRunningCallbackMethod;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
    procedure Execute; override;
    procedure Synchronizer;
    procedure RunningSynchronizer;
  public
    property hSocket: TSocket read FhSocket write FhSocket;
    property ReceiveStr: ansistring read FReceiveStr write FReceiveStr;
    property ListenStrings: TStringList read FListenStrings write FListenStrings;
    property Callback: TOpStackTestCallbackMethod read FCallback write FCallback;
    property RunningCallback: TOpStackTestRunningCallbackMethod read FRunningCallback write FRunningCallback;
  end;

  { TOPStackTestConnectionOutput }

  TOPStackTestConnectionOutput = class(TThread)
  private
    FEvent: PRTLEvent;
    FhSocket: TSocket;
    FRunningCallback: TOpStackTestRunningCallbackMethod;
    FSendList: TThreadList;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
    procedure Execute; override;
    procedure RunningSynchronizer;
  public
    procedure Send(StringList: TStringList);
    property hSocket: TSocket read FhSocket write FhSocket;
    property SendList: TThreadList read FSendList write FSendList;
    property Event: PRTLEvent read FEvent write FEvent;
    property RunningCallback: TOpStackTestRunningCallbackMethod read FRunningCallback write FRunningCallback;
  end;
{$ENDIF}
// *****************************************************************************
//  Lazarus Specific from here up
// *****************************************************************************


procedure Hardware_Initialize;

// The OPStack calles these functions to control and/or send messages through the hardware layer
procedure Hardware_DisableInterrupts;
procedure Hardware_EnableInterrupts;

procedure OutgoingMessage(AMessage: PSimpleMessage);                                   // Expects that IsOutgoingBufferAvailable was called and returned True to ensure success in transmitting
function IsOutgoingBufferAvailable: Boolean;

// Callback to push received messages into the OPStack
procedure IncomingMessageCallback(AMessage: PSimpleMessage); external;

implementation

type
  THardware = record
    InterruptDisableCount: Integer;                                                // Allows stacked calls to Disable/Enable Interrupts (if used)
  end;

var
  Hardware: THardware;

procedure Hardware_Initialize;
begin
  Hardware.InterruptDisableCount := 0;
end;

procedure Hardware_DisableInterrupts;
begin
  // called when lists or similar are being maniuplated that could be in an interterminate state if an interrupt
  // driven system is called in the middle of the manipulation.
  // Use this function to disable asyncronous access to library variable during this call

  // Disable Any interrupts here
  Inc(Hardware.InterruptDisableCount);
end;

procedure Hardware_EnableInterrupts;
begin
  // called when lists or similar are being maniuplated that could be in an interterminate state if an interrupt
  // driven system is called in the middle of the manipulation.
  // Use this function to enable asyncronous access to library variable during this call

  Dec(Hardware.InterruptDisableCount);
  if Hardware.InterruptDisableCount <= 0 then
  begin
    // Enable any Interrupts here
    Hardware.InterruptDisableCount := 0;
  end;
end;

procedure OutgoingMessage(AMessage: PSimpleMessage);
begin
  case AMessage^.MessageType of
    MT_SIMPLE :
        begin

        end;
    MT_CAN :
        begin

        end;
    MT_DATAGRAM :
        begin

        end;
    MT_STREAM :
        begin

        end;
  end;
  OPStack_DeAllocateMessage(AMessage);
end;

function IsOutgoingBufferAvailable: Boolean;
begin
  Result := True
end;


// *****************************************************************************
//  Lazarus Specific from here down
// *****************************************************************************

{$IFDEF FPC}
{ TOPStackTestConnectionOutput }

constructor TOPStackTestConnectionOutput.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);
  SendList := TThreadList.Create;
end;

destructor TOPStackTestConnectionOutput.Destroy;
var
  i: Integer;
  List: TList;
begin
  List := SendList.LockList;
  try
    for i := 0 to List.Count - 1 do
      TObject( List[i]).Free;
  finally
    List.Clear;
    SendList.UnlockList;
  end;
  inherited Destroy;
end;

procedure TOPStackTestConnectionOutput.Execute;
var
  Socket: TTCPBlockSocket;
  List: TList;
  SendString: ansistring;
  i: Integer;
begin
  Event := RTLEventCreate;
  Socket := TTCPBlockSocket.Create;
  try
    Socket.CreateSocket;
    Socket.Socket := hSocket;
    Socket.ConvertLineEnd := True;      // Use #10, #13, or both to be a "string"
    Socket.GetSins;                     // Back load the IP's / Ports information from the handle
    Synchronize(@RunningSynchronizer);
    while not Terminated do
    begin
      RTLEventWaitFor(Event);

      List := SendList.LockList;
      try
        SendString := '';
        for i := 0 to List.Count - 1 do
          SendString := SendString + TStringList( List[i]).Text;
      finally
        List.Clear;
        SendList.UnlockList;
      end;
      Socket.ResetLastError;
      Socket.SendString(SendString);
    end;
  finally
    Socket.CloseSocket;
    Socket.Free;
    RTLeventdestroy(Event);
  end;
end;

procedure TOPStackTestConnectionOutput.RunningSynchronizer;
begin
  if Assigned(RunningCallback) then
    RunningCallback(ETT_Writer)
end;

procedure TOPStackTestConnectionOutput.Send(StringList: TStringList);
var
  List: TList;
begin
  List := SendList.LockList;
  try
    List.Add(StringList);
  finally
    SendList.UnlockList;
  end;
  RTLeventSetEvent(Event);
end;

{ TOPStackTestConnectionInput }

constructor TOPStackTestConnectionInput.Create(CreateSuspended: Boolean; const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);
  Callback := nil;
end;

destructor TOPStackTestConnectionInput.Destroy;
begin
  Callback := nil;
  inherited Destroy;
end;

procedure TOPStackTestConnectionInput.Execute;
var
  Socket: TTCPBlockSocket;
begin
  Socket := TTCPBlockSocket.Create;
  try
    Socket.CreateSocket;
    Socket.Socket := hSocket;
    Socket.ConvertLineEnd := True;      // Use #10, #13, or both to be a "string"
    Socket.GetSins;                     // Back load the IP's / Ports information from the handle
    Synchronize(@RunningSynchronizer);

    while not Terminated do
    begin
      Socket.ResetLastError;
      if Socket.LastError = 0 then
      begin
        ReceiveStr := Socket.RecvPacket(-1);
        if Socket.LastError <> WSAETIMEDOUT then
        begin
          if (Socket.LastError = 0) and (ReceiveStr <> '') then
            if Assigned(Callback) then
              Synchronize(@Synchronizer);
        end
      end
    end;
  finally
    Socket.CloseSocket;
    Socket.Free;
  end;
end;

procedure TOPStackTestConnectionInput.Synchronizer;
begin
  if Assigned(Callback) then
    Callback(ReceiveStr)
end;

procedure TOPStackTestConnectionInput.RunningSynchronizer;
begin
  if Assigned(RunningCallback) then
    RunningCallback(ETT_Reader);
end;

{ TOPStackTestListener }

constructor TOPStackTestListener.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);
  Callback := nil;
  RunningCallback := nil;
  ConnectionOutputList := TList.Create;
  ConnectionInputList := TList.Create;
end;

destructor TOPStackTestListener.Destroy;
var
  i: Integer;
begin
  try
    for i := 0 to ConnectionOutputList.Count - 1 do
      TOPStackTestConnectionInput(ConnectionOutputList[i]).Terminate;
  finally
    ConnectionOutputList.Clear
  end;
  FreeAndNil(FConnectionInputList);
  try
    for i := 0 to ConnectionInputList.Count - 1 do
      TOPStackTestConnectionInput(ConnectionInputList[i]).Terminate;
  finally
    ConnectionInputList.Clear
  end;
  FreeAndNil(FConnectionOutputList);
  inherited Destroy;
end;

procedure TOPStackTestListener.Execute;
var
  Socket: TTCPBlockSocket;
  NewConnectionInput: TOPStackTestConnectionInput;
  NewConnectionOutput: TOPStackTestConnectionOutput;
begin
  Socket := TTCPBlockSocket.Create;
  try
    Socket.CreateSocket;
    Socket.Family := SF_IP4;
    Socket.Bind('0.0.0.0', '12022');
    Socket.SetLinger(True, 1000);
    Socket.ConvertLineEnd := True;      // Use #10, #13, or both to be a "string"
    Socket.Listen;
    while not Terminated do
    begin
      Socket.ResetLastError;
      if Socket.CanRead(-1) then
      begin
        if Socket.LastError = 0 then
        begin
          NewConnectionInput := TOPStackTestConnectionInput.Create(True);
          NewConnectionInput.hSocket := Socket.Accept;
          NewConnectionInput.Callback := Callback;
          NewConnectionInput.RunningCallback := RunningCallback;
          NewConnectionInput.Start;
          ConnectionInputList.Add(NewConnectionInput);
          NewConnectionOutput := TOPStackTestConnectionOutput.Create(True);
          NewConnectionOutput.hSocket := NewConnectionInput.hSocket;
          NewConnectionOutput.RunningCallback := RunningCallback;
          NewConnectionOutput.Start;
          ConnectionOutputList.Add(NewConnectionOutput);
        end
      end
    end;
  finally
    Socket.CloseSocket;
    Socket.Free;
  end;
end;

procedure TOPStackTestListener.Send(StringList: TStringList);
var
  NewStringList: TStringList;
  i: Integer;
begin
  if ConnectionOutputList.Count > 1 then
  begin
    for i := 1 to ConnectionOutputList.Count - 1 do
    begin
      NewStringList := TStringList.Create;
      NewStringList.Assign( StringList);
      TOPStackTestConnectionOutput( ConnectionOutputList[i]).Send(NewStringList);
    end;
  end;
  if ConnectionOutputList.Count > 0 then
    TOPStackTestConnectionOutput( ConnectionOutputList[0]).Send( StringList);
end;

{$ENDIF}


end.
