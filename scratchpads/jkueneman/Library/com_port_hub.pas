unit com_port_hub;

{$mode objfpc}{$H+}

{.$DEFINE DEBUG_THREAD}

interface

uses
  Classes, SysUtils, synaser, ExtCtrls, dialogs, olcb_utilities, olcb_defines,
  olcb_app_common_settings, Forms, blcksock, synsock, olcb_transport_layer;


type

{ TComPortThread }

  TComPortThread =  class(TTransportLayerThread)
  private
    FBaudRate: DWord;                                                           // Baud rate to connect with
    FPort: String;                                                              // Port to connect to
    FSerial: TBlockSerial;                                                      // Serial object
    protected
      procedure Execute; override;
      procedure ExecuteBegin; override;
      procedure ExecuteEnd; override;
    public
      constructor Create(CreateSuspended: Boolean; ANodeThread: TNodeThread); reintroduce;
      destructor Destroy; override;
      procedure SendMessage(AMessage: AnsiString); override;

      property Serial: TBlockSerial read FSerial write FSerial;
      property BaudRate: DWord read FBaudRate write FBaudRate;
      property Port: String read FPort write FPort;
  end;

  { TComPortThreadList }

  TComPortThreadList = class(TThreadList)      // Contains TClientSocketThread objects
  private
    function GetCount: Integer;
  public
    destructor Destroy; override;
    procedure ClearObjects;

    property Count: Integer read GetCount;
  end;

  { TComPortHub }

  TComPortHub = class
  private
    FComPortThreadList: TComPortThreadList;
    FNodeThread: TNodeThread;
    FSyncConnectionStateFunc: TOnConnectionStateChangeFunc;
    FOnErrorMessage: TOnRawMessageFunc;
    function GetConnected: Boolean;
    procedure SetNodeThread(AValue: TNodeThread);
  protected
    property ComPortThreadList: TComPortThreadList read FComPortThreadList write FComPortThreadList;
  public
    constructor Create(ANodeThread: TNodeThread);
    destructor Destroy; override;
    function AddComPort(BaudRate: DWord; Port: String): TComPortThread;
    procedure RemoveComPort(ComPort: TComPortThread);
    property Connected: Boolean read GetConnected;
    property NodeThread: TNodeThread read FNodeThread write SetNodeThread;
    property OnConnectionStateChange: TOnConnectionStateChangeFunc read FSyncConnectionStateFunc write FSyncConnectionStateFunc;
    property OnErrorMessage: TOnRawMessageFunc read FOnErrorMessage write FOnErrorMessage;
  end;


implementation

{ TComPortThreadList }

function TComPortThreadList.GetCount: Integer;
var
  L: TList;
begin
  L := LockList;
  try
    Result := L.Count
  finally
    UnlockList;
  end;
end;

destructor TComPortThreadList.Destroy;
begin
  ClearObjects;
  inherited Destroy;
end;

procedure TComPortThreadList.ClearObjects;
var
  i: Integer;
  L: TList;
  ComPortThread: TComPortThread;
begin
  L := LockList;
  try
    for i := 0 to L.Count - 1 do
    begin;
      ComPortThread := TComPortThread( L[i]);
      ComPortThread.Terminate;
    end;
  finally
    L.Clear;
    UnlockList;
  end;
end;

{ TComPortHub }

function TComPortHub.GetConnected: Boolean;
begin
  Result := ComPortThreadList.Count > 0;
end;

procedure TComPortHub.SetNodeThread(AValue: TNodeThread);
var
  List: TList;
  i: Integer;
begin
  if FNodeThread <> AValue then
  begin
    FNodeThread := AValue;
    List := ComPortThreadList.LockList;
    try
      for i := 0 to List.Count - 1 do
        TComPortThread( List[i]).NodeThread := FNodeThread;
    finally
      ComPortThreadList.UnlockList;
    end;
  end;
end;

constructor TComPortHub.Create(ANodeThread: TNodeThread);
begin
  inherited Create;
  ComPortThreadList := TComPortThreadList.Create;
  FOnErrorMessage := nil;
  FSyncConnectionStateFunc := nil;
  NodeThread := ANodeThread;
end;

destructor TComPortHub.Destroy;
begin
  RemoveComPort(nil);
  FreeAndNil(FComPortThreadList);
  inherited Destroy
end;

function TComPortHub.AddComPort(BaudRate: DWord; Port: String): TComPortThread;
var
  List: TList;
begin
  Result := TComPortThread.Create(True, NodeThread);
  Result.BaudRate := BaudRate;
  Result.Port := Port;
  Result.OnErrorMessage := OnErrorMessage;
  Result.OnConnectionStateChange := OnConnectionStateChange;
  ComPortThreadList.Add(Result);
  Result.Suspended := False;
end;

procedure TComPortHub.RemoveComPort(ComPort: TComPortThread);
var
  List: TList;
  i: Integer;
begin
  List := ComPortThreadList.LockList;
  try
    for i := List.Count - 1 downto 0  do
    begin
      if (ComPort = TComPortThread( List[i])) or (ComPort = nil) then
      begin
        TComPortThread( List[i]).Terminate;
        while not  TComPortThread( List[i]).TerminateComplete do
          Application.ProcessMessages;
        List.Remove(List[i]);
      end;
    end;
  finally
    ComPortThreadList.UnLockList;
  end;
end;

{ TComPortThread }

procedure TComPortThread.Execute;
var
  List: TList;
  RcvStr: AnsiString;
  Helper: TOpenLCBMessageHelper;
  i: Integer;
  GridConnectStrPtr: PGridConnectString;
begin
  ExecuteBegin;
  Helper := TOpenLCBMessageHelper.Create;
  Serial := TBlockSerial.Create;                                                // Create the Serial object in the context of the thread
  Serial.LinuxLock:=False;
  Serial.RaiseExcept:=False;
  Serial.Connect(Port);
  if Serial.LastError = 0 then
  begin
    Serial.Config(BaudRate, 8, 'N', 0, False, False);                         // FTDI Driver uses no stop bits for non-standard baud rates.
    if Serial.LastError = 0 then
      ConnectionState := csConnected;
  end;

  if Serial.LastError <> 0 then
  begin
    BufferRawMessage := Serial.LastErrorDesc;
    Synchronize(@DoErrorMessage);
  end;

  Synchronize(@DoConnectionState);

  try
    while not Terminated and (ConnectionState = csConnected) do
    begin
      ThreadSwitch;

      if GlobalSettings.General.SendPacketDelay > 0 then
        Sleep(GlobalSettings.General.SendPacketDelay);

      RcvStr := Serial.Recvstring(0);
      for i := 1 to Length(RcvStr) do
       begin
         if GridConnect_DecodeMachine(RcvStr[i], GridConnectStrPtr) then
           DecomposeAndDispatchGridConnectString(GridConnectStrPtr, Helper);
       end;
    end;
  finally
    if Serial.InstanceActive then
      Serial.CloseSocket;
    Serial.Free;
    Helper.Free;
    ExecuteEnd;
  end;
end;

procedure TComPortThread.ExecuteBegin;
begin
  inherited ExecuteBegin;
  if Assigned(NodeThread) then
    NodeThread.RegisterThread(Self);
end;

procedure TComPortThread.ExecuteEnd;
begin
  if Assigned(NodeThread) then
    NodeThread.UnRegisterThread(Self);
  inherited ExecuteEnd;
end;

procedure TComPortThread.SendMessage(AMessage: AnsiString);
begin
  Serial.SendString(AMessage);
end;


constructor TComPortThread.Create(CreateSuspended: Boolean; ANodeThread: TNodeThread);
begin
  inherited Create(CreateSuspended, ANodeThread);
  FBaudRate := 9600;
  FPort := '';
end;

destructor TComPortThread.Destroy;
begin
  inherited Destroy;
end;


end.

