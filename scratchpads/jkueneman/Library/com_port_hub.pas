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
    public
      constructor Create(CreateSuspended: Boolean);  override;
      destructor Destroy; override;

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
    FEnableReceiveMessages: Boolean;
    FEnableSendMessages: Boolean;
    FOnBeforeDestroyTask: TOlcbTaskBeforeDestroy;
    FSyncConnectionStateFunc: TSyncConnectionStateFunc;
    FSyncErrorMessageFunc: TSyncRawMessageFunc;
    FSyncReceiveMessageFunc: TSyncRawMessageFunc;
    FSyncSendMessageFunc: TSyncRawMessageFunc;
    function GetConnected: Boolean;
    procedure SetEnableReceiveMessages(AValue: Boolean);
    procedure SetEnableSendMessages(AValue: Boolean);
  protected
    procedure DoBeforeThreadDestroy(Sender: TObject); virtual;
    property ComPortThreadList: TComPortThreadList read FComPortThreadList write FComPortThreadList;
  public
    constructor Create;
    destructor Destroy; override;
    function AddTask(NewTask: TTaskOlcbBase): Boolean;
    function AddComPort(BaudRate: DWord; Port: String): TComPortThread;
    procedure RemoveAndFreeTasks(RemoveKey: PtrInt);
    procedure RemoveComPort(ComPort: TComPortThread);
    property Connected: Boolean read GetConnected;
    property EnableReceiveMessages: Boolean read FEnableReceiveMessages write SetEnableReceiveMessages;
    property EnableSendMessages: Boolean read FEnableSendMessages write SetEnableSendMessages;
    property OnBeforeDestroyTask: TOlcbTaskBeforeDestroy read FOnBeforeDestroyTask write FOnBeforeDestroyTask;
    property SyncConnectionStateFunc: TSyncConnectionStateFunc read FSyncConnectionStateFunc write FSyncConnectionStateFunc;
    property SyncErrorMessageFunc: TSyncRawMessageFunc read FSyncErrorMessageFunc write FSyncErrorMessageFunc;
    property SyncReceiveMessageFunc: TSyncRawMessageFunc read FSyncReceiveMessageFunc write FSyncReceiveMessageFunc;
    property SyncSendMessageFunc: TSyncRawMessageFunc read FSyncSendMessageFunc write FSyncSendMessageFunc;
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

procedure TComPortHub.SetEnableReceiveMessages(AValue: Boolean);
var
  List: TList;
  i: Integer;
begin
  if FEnableSendMessages=AValue then Exit;
  FEnableSendMessages:=AValue;
  List := ComPortThreadList.LockList;
  try
    for i := 0 to List.Count - 1 do
      TComPortThread( List[i]).EnableSendMessages := AValue;
  finally
    ComPortThreadList.UnlockList;
  end;
end;

function TComPortHub.GetConnected: Boolean;
begin
  Result := ComPortThreadList.Count > 0;
end;

procedure TComPortHub.SetEnableSendMessages(AValue: Boolean);
var
  List: TList;
  i: Integer;
begin
  if FEnableReceiveMessages=AValue then Exit;
  FEnableReceiveMessages:=AValue;
  List := ComPortThreadList.LockList;
  try
    for i := 0 to List.Count - 1 do
      TComPortThread( List[i]).EnableReceiveMessages := AValue;
  finally
    ComPortThreadList.UnlockList;
  end;
end;

constructor TComPortHub.Create;
begin
  inherited Create;
  ComPortThreadList := TComPortThreadList.Create;
  FOnBeforeDestroyTask := nil;
  FSyncErrorMessageFunc := nil;
  FSyncReceiveMessageFunc := nil;
  FSyncSendMessageFunc := nil;
  FSyncConnectionStateFunc := nil;
end;

destructor TComPortHub.Destroy;
begin
  RemoveComPort(nil);
  FreeAndNil(FComPortThreadList);
  inherited Destroy
end;

procedure TComPortHub.DoBeforeThreadDestroy(Sender: TObject);
var
  List: TList;
begin
  List := ComPortThreadList.LockList;
  try
    List.Remove(Sender);
  finally
    ComPortThreadList.UnlockList;
  end;
end;

function TComPortHub.AddTask(NewTask: TTaskOlcbBase): Boolean;
var
  List: TList;
  i: Integer;
  Done: Boolean;
begin
  Done := False;
  List := ComPortThreadList.LockList;
  try
    if NewTask.DestinationAlias = 0 then
    begin
      for i := 0 to List.Count - 1 do
        TComPortThread( List[i]).AddTask(NewTask, True);   // Broadcast, Thread will clone task
      Done := True;
    end else
    begin
      i := 0;
      while (i < List.Count) and not Done do
      begin
        Done := TComPortThread( List[i]).AddTask(NewTask, True);  // Thread will clone task
        Inc(i);
      end;
    end;
  finally
    ComPortThreadList.UnlockList;
    Result := Done;
  end;
end;

function TComPortHub.AddComPort(BaudRate: DWord; Port: String): TComPortThread;
var
  List: TList;
begin
  Result := TComPortThread.Create(True);
  Result.FreeOnTerminate := True;
  Result.BaudRate := BaudRate;
  Result.Port := Port;
  Result.SyncErrorMessageFunc := SyncErrorMessageFunc;
  Result.SyncReceiveMessageFunc := SyncReceiveMessageFunc;
  Result.SyncSendMessageFunc := SyncSendMessageFunc;
  Result.SyncConnectionStateFunc := SyncConnectionStateFunc;
  Result.OnBeforeDestroyTask := OnBeforeDestroyTask;
  Result.OnBeforeDestroy := @DoBeforeThreadDestroy;

  List := ComPortThreadList.LockList;
  try
    List.Add(Result);
  finally
    ComPortThreadList.UnlockList;
  end;
  Result.Suspended := False;
end;

procedure TComPortHub.RemoveAndFreeTasks(RemoveKey: PtrInt);
var
  List: TList;
  i: Integer;
begin
  List := ComPortThreadList.LockList;
  try
    for i := 0 to List.Count - 1 do
      TComPortThread( List[i]).RemoveAndFreeTasks(RemoveKey);   // UNKNWON IF THIS IS CORRECT >>>>>>>>>>>>>>>>>>
  finally
    ComPortThreadList.UnlockList;
  end;
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
        TComPortThread( List[i]).SyncReceiveMessageFunc := nil;
        TComPortThread( List[i]).SyncSendMessageFunc := nil;
        TComPortThread( List[i]).SyncErrorMessageFunc := nil;
        TComPortThread( List[i]).OnBeforeDestroyTask := nil;
        TComPortThread( List[i]).RemoveAndFreeTasks(0);
        TComPortThread( List[i]).Terminate;
        List.Delete(i);
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
  SendStr: AnsiString;
  Helper: TOpenLCBMessageHelper;
  TractionProtocolTask: TTaskTractionProtocol;
  InitializationCompleteTask: TTaskInitializationComplete;
begin
  ExecuteBegin;
  Helper := TOpenLCBMessageHelper.Create;
  Serial := TBlockSerial.Create;                           // Create the Serial object in the context of the thread
  Serial.LinuxLock:=False;
  Serial.RaiseExcept:=False;
  Serial.Connect(Port);
  try
    if Serial.InstanceActive then
    begin
      Serial.Config(BaudRate, 8, 'N', 0, False, False);                         // FTDI Driver uses no stop bits for non-standard baud rates.
      Connected:=True;
      ConnectionState := csConnected;
      Synchronize(@SyncConnectionState);
      while not Terminated do
      begin
        ThreadSwitch;

        List := ThreadListSendStrings.LockList;                                 // *** Pickup the next Message to Send ***
        try
          if List.Count > 0 then
          begin
            if TStringList( List[0]).Count > 0 then
            begin
              SendStr := TStringList( List[0])[0];
              TStringList( List[0]).Delete(0);
            end;
          end;
        finally
          ThreadListSendStrings.UnlockList;                                     // Deadlock if we don't do this here when the main thread blocks trying to add a new Task and we call Syncronize asking the main thread to run.....
        end;

        CANFrameParserDatagramSendManager.ProcessSend;                                        // *** See if there is a datagram that will add a message to send ***
        CANFrameParserStreamSendManager.ProcessSend;
        OlcbTaskManager.ProcessSending;                                         // *** See if there is a task what will add a message to send ***
        if SendStr <> '' then                                                   // *** Put the message on the wire and communicate back the raw message sent ***
        begin
          if Helper.Decompose(SendStr) then
          begin
            if GlobalSettings.General.SendPacketDelay > 0 then
              Sleep(GlobalSettings.General.SendPacketDelay);
            if EnableSendMessages then
            begin
              BufferRawMessage := SendStr;
              Synchronize(@SyncSendMessage);
            end;
            Serial.SendString(SendStr + LF);
          end;
          SendStr := '';
        end;

        if GlobalSettings.General.SendPacketDelay > 0 then                      // *** Grab the next message from the wire ***
          Sleep(GlobalSettings.General.SendPacketDelay);

        DecomposeAndDispatchGridConnectString(Serial.Recvstring(0), Helper);
      end;
    end else
    begin
      BufferRawMessage := Serial.LastErrorDesc;
      Synchronize(@SyncErrorMessage)
    end;
  finally
    if Connected then
      Serial.CloseSocket;
    Connected := False;
    Serial.Free;
    Helper.Free;
    ExecuteEnd;
  end;
end;


constructor TComPortThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FBaudRate := 9600;
  FPort := '';
end;

destructor TComPortThread.Destroy;
begin
  inherited Destroy;
end;


end.

