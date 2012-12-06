unit serialport_thread;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, synaser, ExtCtrls, dialogs;

type

{ TComPortThread }

  TSyncRawMessageFunc = procedure(MessageStr: String) of object;

  TComPortThread = class(TThread)
  private
    FBaudRate: DWord;                                                           // Baud rate to connect with
    FConnected: Boolean;                                                        // True if connected to the port
    FEnableReceiveMessages: Boolean;                                            // Callback through Syncronize with the message that was received
    FEnableSendMessages: Boolean;                                               // Callback through Syncronize with the message that is about to be sent
    FPort: String;                                                              // Port to connect to
    FRawMessageBuffer: string;                                                  // Shared data to pass string between thread and the Syncronized callbacks
    FSerial: TBlockSerial;                                                      // Serial object
    FSyncErrorMessageFunc: TSyncRawMessageFunc;                                 // Function to callback through Syncronize if an error connecting occured
    FSyncReceiveMessageFunc: TSyncRawMessageFunc;                               // Function to callback through Syncronize if EnableReceiveMessages is true
    FSyncSendMessageFunc: TSyncRawMessageFunc;                                  // Function to callback through Syncronize if EnableSendMessages is true
    FTerminatedThread: Boolean;                                                 // True if the thread has terminated
    FTerminateThread: Boolean;                                                  // Set to true to terminate the thread
    FThreadListSendStrings: TThreadList;                                        // List of strings waiting to be sent
    protected
      property RawMessageBuffer: string read FRawMessageBuffer write FRawMessageBuffer;
      procedure Execute; override;
      procedure SyncReceiveMessage;
      procedure SyncSendMessage;
      procedure SyncErrorMessage;
    public
      property Connected: Boolean read FConnected write FConnected;
      property Serial: TBlockSerial read FSerial write FSerial;
      property BaudRate: DWord read FBaudRate write FBaudRate;
      property Port: String read FPort write FPort;
      property ThreadListSendStrings: TThreadList read FThreadListSendStrings write FThreadListSendStrings;
      property TerminateThread: Boolean read FTerminateThread write FTerminateThread;
      property TerminatedThread: Boolean read FTerminatedThread;
      property SyncErrorMessageFunc: TSyncRawMessageFunc read FSyncErrorMessageFunc write FSyncErrorMessageFunc;
      property SyncReceiveMessageFunc: TSyncRawMessageFunc read FSyncReceiveMessageFunc write FSyncReceiveMessageFunc;
      property SyncSendMessageFunc: TSyncRawMessageFunc read FSyncSendMessageFunc write FSyncSendMessageFunc;
      property EnableReceiveMessages: Boolean read FEnableReceiveMessages write FEnableReceiveMessages;
      property EnableSendMessages: Boolean read FEnableSendMessages write FEnableSendMessages;
      constructor Create(CreateSuspended: Boolean);
      destructor Destroy; override;
      procedure Add(Msg: AnsiString);
  end;


implementation

{ TComPortThread }

procedure TComPortThread.Execute;
var
  List: TList;
  ReceiveStr, SendStr: AnsiString;
begin
  Serial := TBlockSerial.Create;                           // Create the Serial object in the context of the thread
  Serial.LinuxLock:=False;
  Serial.RaiseExcept:=False;
  Serial.Connect(Port);
  try
    if Serial.InstanceActive then
    begin
      Connected:=True;
      Serial.Config(BaudRate, 8, 'N', 0, False, False);      // FTDI Driver uses no stop bits for non-standard baud rates.
      while not Terminated do
      begin
        ThreadSwitch;
        List := ThreadListSendStrings.LockList;
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
          ThreadListSendStrings.UnlockList;        // Deadlock if we don't do this here when the main thread blocks trying to add a new Test and we call Syncronize asking the main thread to run.....
        end;

        if SendStr <> '' then
        begin
          if EnableSendMessages then
          begin
            RawMessageBuffer := SendStr;
            Synchronize(@SyncSendMessage);
          end;
          Serial.SendString(SendStr + LF);
          SendStr := '';
        end;

        ReceiveStr := Serial.Recvstring(0);
        ReceiveStr := Trim(ReceiveStr);
        if ReceiveStr <> '' then
        begin
          if EnableReceiveMessages then
          begin
            RawMessageBuffer := ReceiveStr;
            Synchronize(@SyncReceiveMessage);
          end;
        end;
      end;
    end else
    begin
      RawMessageBuffer := Serial.LastErrorDesc;
      Synchronize(@SyncErrorMessage)
    end;
  finally
    if Connected then
      Serial.CloseSocket;
    Connected := False;
  end;
end;

procedure TComPortThread.SyncReceiveMessage;
begin
  if Assigned(SyncReceiveMessageFunc) then
    SyncReceiveMessageFunc(RawMessageBuffer)
end;

procedure TComPortThread.SyncSendMessage;
begin
  if Assigned(SyncSendMessageFunc) then
    SyncSendMessageFunc(RawMessageBuffer)
end;

procedure TComPortThread.SyncErrorMessage;
begin
  if Assigned(SyncErrorMessageFunc) then
    SyncErrorMessageFunc(RawMessageBuffer)
end;

constructor TComPortThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FThreadListSendStrings := TThreadList.Create;
  FBaudRate := 9600;
  FPort := '';
  FTerminateThread := False;
  FTerminatedThread := False;
  FEnableReceiveMessages := True;
  FEnableSendMessages := True;
  FSyncErrorMessageFunc := nil;
  FSyncReceiveMessageFunc := nil;
  FSyncSendMessageFunc := nil;
end;

destructor TComPortThread.Destroy;
var
  List: TList;
begin
  List := ThreadListSendStrings.LockList;
  try
    if List.Count <> 0 then
      TStringList( List[0]).Free;
  finally
    ThreadListSendStrings.UnLockList;
  end;
  FreeAndNil(FThreadListSendStrings);   // Thread does not own the items so just empty the list
  inherited Destroy;
end;

procedure TComPortThread.Add(Msg: AnsiString);
var
  List: TList;
  StringList: TStringList;
begin
  List := ThreadListSendStrings.LockList;
  try
    if List.Count = 0 then
    begin
      StringList := TStringList.Create;
      StringList.Delimiter := ';';
      List.Add( StringList);
    end;
    StringList := TStringList( List[0]);
    StringList.Add(Msg);
  finally
    ThreadListSendStrings.UnLockList;
  end;
end;


end.
