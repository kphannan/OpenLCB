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

{ TComPortThread }

  TComPortThread = class(TTransportLayerThread)
  private
    FBaudRate: DWord;                                                           // Baud rate to connect with
    FPort: String;                                                              // Port to connect to
    FSerial: TBlockSerial;                                                      // Serial object
    protected
      procedure Execute; override;
    public
      constructor Create(CreateSuspended: Boolean);
      destructor Destroy; override;

      property Serial: TBlockSerial read FSerial write FSerial;
      property BaudRate: DWord read FBaudRate write FBaudRate;
      property Port: String read FPort write FPort;
  end;


implementation

{ TComPortThread }

procedure TComPortThread.Execute;
var
  List: TList;
  ReceiveStr, SendStr: AnsiString;
  Helper: TOpenLCBMessageHelper;
  CompletedSendDatagram: TDatagramSend;
  T: DWord;
  CANLayerTask: TCANLayerTask;
  EventTask: TEventTask;
  VerifiedNodeIDTask: TVerifiedNodeIDTask;
  TractionProtocolTask: TTractionProtocolTask;
  InitializationCompleteTask: TInitializationCompleteTask;
  BufferDatagramReceive: TDatagramReceive;
begin
  ExecuteBegin;
  T := 0;
  CompletedSendDatagram := nil;
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
        T := GetTickCount;
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
        ReceiveStr := Serial.Recvstring(0);
        ReceiveStr := Trim(ReceiveStr);

        if Helper.Decompose(ReceiveStr) then
        begin
          if EnableReceiveMessages then                                         // *** Communicate back to the app the raw message string
          begin
            BufferRawMessage := ReceiveStr;
            Synchronize(@SyncReceiveMessage);
          end;

          if IsDatagramMTI(Helper.MTI, True) then                               // *** Test for a Datagram message that came in ***
          begin
            CompletedSendDatagram := DatagramSendManager.ProcessReceive(Helper);// Sending Datagrams are expecting replies from their destination Nodes
            if Assigned(CompletedSendDatagram) then
            begin
              OlcbTaskManager.ProcessReceiving(CompletedSendDatagram);          // Give the Task subsystem a crack at knowning about the sent datagram
              FreeAndNil(CompletedSendDatagram)
            end else
            begin
              BufferDatagramReceive := DatagramReceiveManager.Process(Helper);  // DatagramReceive object is created and given to the thread
              if Assigned(BufferDatagramReceive) then
              begin
                OlcbTaskManager.ProcessReceiving(BufferDatagramReceive);        // Give the Task subsystem a crack at knowning about the received datagram
                FreeAndNil(BufferDatagramReceive)
              end;
            end;
          end else                                                              // *** Test for a Datagram message that came in ***
            OlcbTaskManager.ProcessReceiving(Helper);

          if Helper.Layer = ol_CAN then
          begin
            CANLayerTask := TCANLayerTask.Create(Helper.DestinationAliasID, Helper.SourceAliasID, True);
            CANLayerTask.OnBeforeDestroy := OnBeforeDestroyTask;
            Helper.CopyTo(CANLayerTask.MessageHelper);
            AddTask(CANLayerTask);
          end;

          case Helper.MTI of
            MTI_INITIALIZATION_COMPLETE :
              begin
                InitializationCompleteTask := TInitializationCompleteTask.Create(Helper.DestinationAliasID, Helper.SourceAliasID, True);
                InitializationCompleteTask.OnBeforeDestroy := OnBeforeDestroyTask;
                Helper.CopyTo(InitializationCompleteTask.MessageHelper);
                AddTask(InitializationCompleteTask);
              end;
            MTI_VERIFIED_NODE_ID_NUMBER :
              begin
                VerifiedNodeIDTask := TVerifiedNodeIDTask.Create(Helper.DestinationAliasID, Helper.SourceAliasID, True);
                VerifiedNodeIDTask.OnBeforeDestroy := OnBeforeDestroyTask;
                Helper.CopyTo(VerifiedNodeIDTask.MessageHelper);
                AddTask(VerifiedNodeIDTask);
              end;
            MTI_CONSUMER_IDENTIFIED_CLEAR,
            MTI_CONSUMER_IDENTIFIED_SET,
            MTI_CONSUMER_IDENTIFIED_UNKNOWN,
            MTI_CONSUMER_IDENTIFIED_RESERVED,
            MTI_PRODUCER_IDENTIFIED_CLEAR,
            MTI_PRODUCER_IDENTIFIED_SET,
            MTI_PRODUCER_IDENTIFIED_UNKNOWN,
            MTI_PRODUCER_IDENTIFIED_RESERVED,
            MTI_PC_EVENT_REPORT :
              begin
                EventTask := TEventTask.Create(Helper.DestinationAliasID, Helper.SourceAliasID, True);
                EventTask.OnBeforeDestroy := OnBeforeDestroyTask;
                Helper.CopyTo(EventTask.MessageHelper);
                AddTask(EventTask);
              end;
            MTI_TRACTION_PROTOCOL :
              begin
                TractionProtocolTask := TTractionProtocolTask.Create(Helper.DestinationAliasID, Helper.SourceAliasID, True);
                TractionProtocolTask.OnBeforeDestroy := OnBeforeDestroyTask;
                Helper.CopyTo(TractionProtocolTask.MessageHelper);
                AddTask(TractionProtocolTask);
              end;
          end;
        end;

        LoopTime := GetTickCount - T;
        if LoopTime > MaxLoopTime then
          MaxLoopTime := LoopTime;
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

{$IFDEF DEBUG_THREAD}
procedure TComPortThread.SyncDebugMessage;
begin
  if Assigned(SyncDebugFunc) then
    SyncDebugFunc(DebugInfo);
end;
{$ENDIF}


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

