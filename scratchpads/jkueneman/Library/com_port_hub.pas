unit com_port_hub;

{$mode objfpc}{$H+}

{.$DEFINE DEBUG_THREAD}

interface

uses
  Classes, SysUtils, synaser, ExtCtrls, dialogs, olcb_utilities, olcb_defines,
  olcb_app_common_settings, Forms, blcksock, synsock, olcb_transport_layer;


type

   {$IFDEF DEBUG_THREAD}
  TComPortThreadDebugRec = record
    ReceiveDatagramCount: Integer;
    MaxReceiveDatagramCount: Integer;
    SendDatagramCount: Integer;
    MaxSendDatagramCount: Integer;
    TaskCount: Integer;
    MaxTaskCount: Integer;
    ThreadTime: DWord;
    MaxThreadTime: DWord;
  end;
  {$ENDIF}

  {$IFDEF DEBUG_THREAD} TSyncDebugFunc = procedure(DebugInfo: TComPortThreadDebugRec) of object; {$ENDIF}

{ TComPortHub }

  TComPortHub =  class(TTransportLayerThread)
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


implementation

{ TComPortHub }

procedure TComPortHub.Execute;
var
  List: TList;
  SendStr: AnsiString;
  Helper: TOpenLCBMessageHelper;
  TractionProtocolTask: TTractionProtocolTask;
  InitializationCompleteTask: TInitializationCompleteTask;
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

        DatagramSendManager.ProcessSend;                                        // *** See if there is a datagram that will add a message to send ***
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


constructor TComPortHub.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FBaudRate := 9600;
  FPort := '';
end;

destructor TComPortHub.Destroy;
begin
  inherited Destroy;
end;


end.

