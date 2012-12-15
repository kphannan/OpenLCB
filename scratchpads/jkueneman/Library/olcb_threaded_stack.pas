unit olcb_threaded_stack;

{$mode objfpc}{$H+}

{.$DEFINE DEBUG_THREAD}

interface

uses
  Classes, SysUtils, synaser, ExtCtrls, dialogs, olcb_utilities, olcb_defines,
  {$IFDEF DEBUG_THREAD}
  form_thread_debug,
  {$ENDIF}
  olcb_app_common_settings ;

const
  DATAGRAM_MAX_RETRYS = 5;

  HEADER_MEMCONFIG_OPTIONS_REQUEST: TCANByteArray                     = ($20, MCP_OP_GET_CONFIG, $00, $00, $00, $00, $00, $00);
  HEADER_MEMCONFIG_SPACE_INFO_CDI_REQUEST: TCANByteArray              = ($20, MCP_OP_GET_ADD_SPACE_INFO, MSI_CDI, $00, $00, $00, $00, $00);
  HEADER_MEMCONFIG_SPACE_INFO_ALL_REQUEST: TCANByteArray              = ($20, MCP_OP_GET_ADD_SPACE_INFO, MSI_ALL, $00, $00, $00, $00, $00);
  HEADER_MEMCONFIG_SPACE_INFO_CONFIG_REQUEST: TCANByteArray           = ($20, MCP_OP_GET_ADD_SPACE_INFO, MSI_CONFIG, $00, $00, $00, $00, $00);
  HEADER_MEMCONFIG_SPACE_INFO_ACDI_READ_MFG_REQUEST: TCANByteArray    = ($20, MCP_OP_GET_ADD_SPACE_INFO, MSI_ACDI_MFG, $00, $00, $00, $00, $00);
  HEADER_MEMCONFIG_SPACE_INFO_ACDI_READ_USER_REQUEST: TCANByteArray   = ($20, MCP_OP_GET_ADD_SPACE_INFO, MSI_ACDI_USER, $00, $00, $00, $00, $00);
  HEADER_MEMCONFIG_SPACE_INFO_ACDI_WRITE_USER_REQUEST: TCANByteArray  = ($20, MCP_OP_GET_ADD_SPACE_INFO, MSI_FUNCTIONS, $00, $00, $00, $00, $00);
  HEADER_MEMCONFIG_SPACE_INFO_UNKNOWN_REQUEST: TCANByteArray          = ($20, MCP_OP_GET_ADD_SPACE_INFO, $00, $00, $00, $00, $00, $00);


type

  TDatagramReceiveManager = class;
  TDatagramSendManager    = class;
  TDatagramReceive        = class;
  TDatagramSend           = class;
  TSniiReceiveManager     = class;
  TSnii                   = class;
  TOlcbTaskEngine         = class;
  TOlcbTaskBase           = class;

  TSyncRawMessageFunc = procedure(MessageStr: String) of object;
  TSyncDatagramFunc = procedure(Datagram: TDatagramReceive) of object;
  TSyncSniiReplyFunc = procedure(Snii: TSnii) of object;


{ TComPortThread }

  TComPortThread = class(TThread)
  private
    FBaudRate: DWord;                                                           // Baud rate to connect with
    FBufferDatagramReceive: TDatagramReceive;
    FBufferRawMessage: string;                                                  // Shared data to pass string between thread and the Syncronized callbacks
    FBufferSniiReply: TSnii;
    FConnected: Boolean;                                                        // True if connected to the port
    FDatagramReceiveManager: TDatagramReceiveManager;
    FDatagramSendManager: TDatagramSendManager;
    FEnableReceiveMessages: Boolean;                                            // Callback through Syncronize with the message that was received
    FEnableSendMessages: Boolean;                                               // Callback through Syncronize with the message that is about to be sent
    FOlcbTaskManager: TOlcbTaskEngine;
    FPort: String;                                                              // Port to connect to
    FSerial: TBlockSerial;                                                      // Serial object
    FSniiManager: TSniiReceiveManager;
    FSyncDatagramMemConfigAddressSpaceInfoReplyFunc: TSyncDatagramFunc;
    FSyncDatagramMemConfigOperationReplyFunc: TSyncDatagramFunc;
    FSyncErrorMessageFunc: TSyncRawMessageFunc;                                 // Function to callback through Syncronize if an error connecting occured
    FSyncReceiveMessageFunc: TSyncRawMessageFunc;                               // Function to callback through Syncronize if EnableReceiveMessages is true
    FSyncSendMessageFunc: TSyncRawMessageFunc;                                  // Function to callback through Syncronize if EnableSendMessages is true
    FSyncSniiReplyFunc: TSyncSniiReplyFunc;
    FTerminatedThread: Boolean;                                                 // True if the thread has terminated
    FTerminateThread: Boolean;                                                  // Set to true to terminate the thread
    FThreadListSendStrings: TThreadList;                                        // List of strings waiting to be sent
    function GetSourceAlias: Word;
    protected
      procedure DispatchDatagramReceive(DatagramReceive: TDatagramReceive);
      procedure DispatchSniiReply;
      procedure Execute; override;
      procedure SyncDatagramMemConfigAddressSpaceInfoReply;
      procedure SyncDatagramMemConfigOperationReply;
      procedure SyncErrorMessage;
      procedure SyncReceiveMessage;
      procedure SyncSendMessage;
      procedure SyncSniiReply;
      procedure WriteDebugInfo;

      property BufferDatagramReceive: TDatagramReceive read FBufferDatagramReceive write FBufferDatagramReceive;
      property BufferRawMessage: string read FBufferRawMessage write FBufferRawMessage;
      property BufferSniiReply: TSnii read FBufferSniiReply write FBufferSniiReply;
      property DatagramReceiveManager: TDatagramReceiveManager read FDatagramReceiveManager;
      property DatagramSendManager: TDatagramSendManager read FDatagramSendManager write FDatagramSendManager;
      property OlcbTaskManager: TOlcbTaskEngine read FOlcbTaskManager write FOlcbTaskManager;
      property SniiManager: TSniiReceiveManager read FSniiManager write FSniiManager;
      property SourceAlias: Word read GetSourceAlias;
    public
      constructor Create(CreateSuspended: Boolean);
      destructor Destroy; override;
      procedure Add(Msg: AnsiString);
      procedure AddDatagramToSend(Datagram: TDatagramSend);
      procedure AddTask(NewTask: TOlcbTaskBase);

      property Connected: Boolean read FConnected write FConnected;
      property Serial: TBlockSerial read FSerial write FSerial;
      property BaudRate: DWord read FBaudRate write FBaudRate;
      property Port: String read FPort write FPort;
      property ThreadListSendStrings: TThreadList read FThreadListSendStrings write FThreadListSendStrings;
      property TerminateThread: Boolean read FTerminateThread write FTerminateThread;
      property TerminatedThread: Boolean read FTerminatedThread;
      property SyncDatagramMemConfigAddressSpaceInfoReplyFunc: TSyncDatagramFunc read FSyncDatagramMemConfigAddressSpaceInfoReplyFunc write FSyncDatagramMemConfigAddressSpaceInfoReplyFunc;
      property SyncDatagramMemConfigOperationReplyFunc: TSyncDatagramFunc read FSyncDatagramMemConfigOperationReplyFunc write FSyncDatagramMemConfigOperationReplyFunc;
      property SyncErrorMessageFunc: TSyncRawMessageFunc read FSyncErrorMessageFunc write FSyncErrorMessageFunc;
      property SyncReceiveMessageFunc: TSyncRawMessageFunc read FSyncReceiveMessageFunc write FSyncReceiveMessageFunc;
      property SyncSendMessageFunc: TSyncRawMessageFunc read FSyncSendMessageFunc write FSyncSendMessageFunc;
      property SyncSniiReplyFunc: TSyncSniiReplyFunc read FSyncSniiReplyFunc write FSyncSniiReplyFunc;
      property EnableReceiveMessages: Boolean read FEnableReceiveMessages write FEnableReceiveMessages;
      property EnableSendMessages: Boolean read FEnableSendMessages write FEnableSendMessages;
  end;

{ TDatagramReceive }

  TDatagramReceive = class( TOlcbMessage)
  private
    FDestinationAlias: Word;
    FLocalHelper: TOpenLCBMessageHelper;
    FRawDatagram: TDatagramArray;
    FFull: Boolean;
    FCurrentPos: Byte;
    FSourceAlias: Word;
  protected
    procedure Clear;
    procedure SendACK(ComPortThread: TComPortThread);
    property LocalHelper: TOpenLCBMessageHelper read FLocalHelper write FLocalHelper;   // Global object to work with OLCB messages
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word);
    destructor Destroy; override;
    function ExtractDataBytesAsInt(StartByteIndex, EndByteIndex: Integer): QWord;       // Helpers to pull information out of the datagram byte array
    function ExtractDataBytesAsString(StartIndex, Count: Integer): String;              // Helpers to pull information out of the datagram byte array
    procedure CopyToStream(Stream: TStream; StartIndex, Count: Integer);                // Helpers to move datagram bytes to a stream
    procedure Process(AHelper: TOpenLCBMessageHelper; ComPortThread: TComPortThread);   // Processes the message/
    property CurrentPos: Byte read FCurrentPos write FCurrentPos;                       // Running count of the number of bytes being received, once "Full" this is the number of byte in the datagram
    property DestinationAlias: Word read FDestinationAlias write FDestinationAlias;     // Node who is sending the Datagram to this object
    property RawDatagram: TDatagramArray read FRawDatagram write FRawDatagram;          // Raw datagram bytes received
    property Full: Boolean read FFull write FFull;                                      // Raw datagram bytes with a size of CurrentPos is valid
    property SourceAlias: Word read FSourceAlias write FSourceAlias;                    // Node who is looking for Datagrams assigned to it to come in
  end;


  { TDatagramReceiveManager }

  TDatagramReceiveManager = class
  private
    FDatagrams: TThreadList;
    FMaxCount: Integer;
    FOwner: TComPortThread;
  protected
    function FindInProcessDatagram(AHelper: TOpenLCBMessageHelper): TDatagramReceive;
    property Datagrams: TThreadList read FDatagrams write FDatagrams;
    property Owner: TComPortThread read FOwner write FOwner;
    property MaxCount: Integer read FMaxCount write FMaxCount;
  public
    constructor Create(AnOwner: TComPortThread);
    destructor Destroy; override;
    procedure Clear;
    function Process(AHelper: TOpenLCBMessageHelper): TDatagramReceive;
  end;

  { TDatagramSend }

  TDatagramSend = class( TOlcbMessage)
  private
    FAbandonTime: Cardinal;
    FCurrentPos: Byte;
    FDataBytesSent: TCANByteArray;
    FDataBytesSentLen: Byte;
    FDestinationAlias: Word;
    FEmpty: Boolean;
    FErrorCode: Word;
    FLocalHelper: TOpenLCBMessageHelper;
    FMTI: DWord;
    FProtocolHeader: TCANByteArray;
    FProtocolHeaderLen: Byte;
    FRetryCount: Byte;
    FSourceAlias: Word;
    FStream: TMemoryStream;
    FWaitingForACK: Boolean;
  protected
    procedure StreamBytesToByteArray(Offset: Byte; var ByteArray: TCANByteArray; var Count: Byte);
    property AbandonTime: Cardinal read FAbandonTime write FAbandonTime;
    property CurrentPos: Byte read FCurrentPos write FCurrentPos;
    property LocalHelper: TOpenLCBMessageHelper read FLocalHelper write FLocalHelper;
    property ProtocolHeader: TCANByteArray read FProtocolHeader write FProtocolHeader;
    property ProtocolHeaderLen: Byte read FProtocolHeaderLen write FProtocolHeaderLen;
    property DataBytesSent: TCANByteArray read FDataBytesSent write FDataBytesSent;
    property DataBytesSentLen: Byte read FDataBytesSentLen write FDataBytesSentLen;
    property MTI: DWord read FMTI write FMTI;
    property RetryCount: Byte read FRetryCount write FRetryCount;
    property Stream: TMemoryStream read FStream write FStream;
    property WaitingForACK: Boolean read FWaitingForACK write FWaitingForACK;             // After a frame is sent need to wait
  public
    constructor Create;
    destructor Destroy; override;
    procedure Initialize(AStream: TStream; AProtocolHeader: TCANByteArray; AProtocolHeaderLen: Byte; ASourceAlias, ADestinationAlias: Word);
    function ProcessSend(ComPortThread: TComPortThread): Boolean;
    function ProcessReceive(AHelper: TOpenLCBMessageHelper; ComPortThread: TComPortThread): Boolean;
    property DestinationAlias: Word read FDestinationAlias write FDestinationAlias;
    property Empty: Boolean read FEmpty write FEmpty;
    property ErrorCode: Word read FErrorCode write FErrorCode;    // One of the DATAGRAM_REJECTED_xxx constants, if $0000 then no error
    property SourceAlias: Word read FSourceAlias write FSourceAlias;
  end;

  { TDatagramSendManager }

  TDatagramSendManager = class
  private
    FAbandonDatagrams: TThreadList;
    FDatagrams: TThreadList;
    FMaxCount: Integer;
    FOwner: TComPortThread;
    FTimer: TTimer;
  protected
    procedure TimerTick(Sender: TObject);
    property AbandonDatagrams: TThreadList read FAbandonDatagrams write FAbandonDatagrams;
    property Datagrams: TThreadList read FDatagrams write FDatagrams;
    property MaxCount: Integer read FMaxCount write FMaxCount;
    property Owner: TComPortThread read FOwner write FOwner;
    property Timer: TTimer read FTimer write FTimer;
  public
    constructor Create(AnOwner: TComPortThread);
    destructor Destroy; override;
    procedure Clear;
    procedure ClearAbandon;
    function ProcessReceive(AHelper: TOpenLCBMessageHelper): TDatagramSend;
    procedure ProcessSend;
  end;


    { TSNII }

  TSNII = class( TOlcbMessage)
  private
    FDestinationAlias: Word;
    FFull: Boolean;
    FLocalHelper: TOpenLCBMessageHelper;
    FStateMachineIndex: Byte;
    FSniiHardwareVersion: string;
    FSniiMfgModel: string;
    FSniiMfgName: string;
    FSniiMfgVersion: Byte;
    FSniiSoftwareVersion: string;
    FSniiUserDesciption: string;
    FSniiUserDescription: string;
    FSniiUserName: string;
    FSniiUserVersion: Byte;
    FSourceAlias: Word;
  protected
    property LocalHelper: TOpenLCBMessageHelper read FLocalHelper write FLocalHelper;   // Global object to work with OLCB messages
    property StateMachineIndex: Byte read FStateMachineIndex write FStateMachineIndex;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word);
    destructor Destroy; override;
    function Duplicate: TSNII;
    function Process(AHelper: TOpenLCBMessageHelper): TSNII;

    property DestinationAlias: Word read FDestinationAlias write FDestinationAlias;
    property SourceAlias: Word read FSourceAlias write FSourceAlias;
    property Full: Boolean read FFull write FFull;
    property SniiMfgName: string read FSniiMfgName write FSniiMfgName;
    property SniiMfgModel: string read FSniiMfgModel write FSniiMfgModel;
    property SniiSoftwareVersion: string read FSniiSoftwareVersion write FSniiSoftwareVersion;
    property SniiHardwareVersion: string read FSniiHardwareVersion write FSniiHardwareVersion;
    property SniiUserName: string read FSniiUserName write FSniiUserName;
    property SniiUserDescription: string read FSniiUserDescription write FSniiUserDesciption;
    property SniiUserVersion: Byte read FSniiUserVersion write FSniiUserVersion;
    property SniiMfgVersion: Byte read FSniiMfgVersion write FSniiMfgVersion;
  end;

  { TSniiReceiveManager }

  TSniiReceiveManager = class
  private
    FMaxCount: Integer;
    FOwner: TComPortThread;
    FSniis: TList;
    function GetSnii(Index: Integer): TSNII;
  protected
    function FindInProcessSnii(AHelper: TOpenLCBMessageHelper): TSNII;
    property MaxCount: Integer read FMaxCount write FMaxCount;
    property Owner: TComPortThread read FOwner write FOwner;
    property Sniis: TList read FSniis write FSniis;
  public
    constructor Create(AnOwner: TComPortThread);
    destructor Destroy; override;
    procedure Clear;
    function Process(AHelper: TOpenLCBMessageHelper): TSNII;
    property Snii[Index: Integer]: TSNII read GetSnii;       // Inprocess and completed Datagrams, the order they are received is preserved
  end;


  { TOlcbTaskBase }

  TOlcbTaskBase = class
  private
    FSending: Boolean;
  protected
    FComPortThread: TComPortThread;
    FDestinationAlias: Word;
    FDone: Boolean;
    FiState: Integer;
    FSourceAlias: Word;
    procedure Process(MessageInfo: TOlcbMessage); virtual; abstract;                 // Must override this
    property iState: Integer read FiState write FiState;
    property Done: Boolean read FDone;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word); virtual;
    property ComPortThread: TComPortThread read FComPortThread;
    property DestinationAlias: Word read FDestinationAlias;
    property SourceAlias: Word read FSourceAlias;
    property Sending: Boolean read FSending write FSending;
  end;

  { TOlcbTaskEngine }

  TOlcbTaskEngine = class
  private
    FMaxCount: Integer;
    FOwner: TComPortThread;
    FTaskList: TThreadList;
  protected
    property MaxCount: Integer read FMaxCount write FMaxCount;
    property Owner: TComPortThread read FOwner write FOwner;
    property TaskList: TThreadList read FTaskList write FTaskList;
  public
    constructor Create(AnOwner: TComPortThread);
    procedure ProcessReceiving(MessageInfo: TOlcbMessage);
    procedure ProcessSending;
    destructor Destroy; override;
    procedure Clear;
  end;



implementation

const
  STATE_SNII_MFG_VERSION  = 0;
  STATE_SNII_MFG_NAME     = 1;
  STATE_SNII_MFG_MODEL    = 2;
  STATE_SNII_HARDWARE_VER = 3;
  STATE_SNII_SOFTWARE_VER = 4;
  STATE_SNII_USER_VERSION = 5;
  STATE_SNII_USER_NAME    = 6;
  STATE_SNII_USER_DESC    = 7;
  STATE_SNII_DONE         = 8;

{ TOlcbTaskBase }

constructor TOlcbTaskBase.Create(ASourceAlias, ADestinationAlias: Word);
begin
  inherited Create;
  FDestinationAlias := ADestinationAlias;
  FSourceAlias := ASourceAlias;
  FComPortThread := nil;
end;

{ TComPortThread }

function TComPortThread.GetSourceAlias: Word;
begin
  EnterCriticalsection(GlobalSettingLock);
  try
    Result := GlobalSettings.General.AliasIDAsVal;
  finally
    LeaveCriticalsection(GlobalSettingLock)
  end;
end;

procedure TComPortThread.DispatchDatagramReceive(DatagramReceive: TDatagramReceive);
begin
  if Assigned(DatagramReceive) then
  begin // Received a complete datagram need to dispatch it somewhere
    case DatagramReceive.RawDatagram[0] of    // Destination in the header for datagrams
      DATAGRAM_PROTOCOL_CONFIGURATION :
        begin
          if DatagramReceive.RawDatagram[1] and $FE = MCP_OP_GET_CONFIG_REPLY then            // Bit 0 is variable
            Synchronize( @SyncDatagramMemConfigOperationReply);
          if DatagramReceive.RawDatagram[1] and $FE = MCP_OP_GET_ADD_SPACE_INFO_REPLY then    // Bit 0 is variable
            Synchronize( @SyncDatagramMemConfigAddressSpaceInfoReply);
        end;
    end;
  end;
end;

procedure TComPortThread.DispatchSniiReply;
begin
  Synchronize( @SyncSniiReply);
end;

procedure TComPortThread.Execute;
var
  List: TList;
  ReceiveStr, SendStr: AnsiString;
  Helper: TOpenLCBMessageHelper;
  CompletedSendDatagram: TDatagramSend;
begin
  CompletedSendDatagram := nil;
  Helper := TOpenLCBMessageHelper.Create;
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

      {$IFDEF DEBUG_THREAD} WriteDebugInfo; {$ENDIF}

        // *** Pickup the next Message to Send ***
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
        // *** Pickup the next Message to Send ***

        // *** See if there is a datagram what will add a message to send ***
        DatagramSendManager.ProcessSend;
        // *** See if there is a datagram what will add a message to send ***

        // *** See if there is a task what will add a message to send ***
        OlcbTaskManager.ProcessSending;
        // *** See if there is a task what will add a message to send ***

        // *** Put the message on the wire and communicate back the raw message sent ***
        if SendStr <> '' then
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
        // *** Put the message on the wire and communicate back the raw message sent ***

        // *** Grab the next message from the wire ***
        if GlobalSettings.General.SendPacketDelay > 0 then
          Sleep(GlobalSettings.General.SendPacketDelay);
        ReceiveStr := Serial.Recvstring(0);
        ReceiveStr := Trim(ReceiveStr);
        // *** Grab the next message from the wire ***

        if Helper.Decompose(ReceiveStr) then
        begin

          // Communicate back to the app the raw message string
          if EnableReceiveMessages then
          begin
            BufferRawMessage := ReceiveStr;
            Synchronize(@SyncReceiveMessage);
          end;
          // Communicate back to the app the raw message string


          if IsDatagramMTI(Helper.MTI, True) then                                 // *** Test for a Datagram message that came in ***
          begin
            CompletedSendDatagram := DatagramSendManager.ProcessReceive(Helper);  // Sending Datagrams are expecting replies from their destination Nodes
            if Assigned(CompletedSendDatagram) then                               // Not sure if there is something interesting to do with this.....
            begin
              OlcbTaskManager.ProcessReceiving(CompletedSendDatagram);            // Give the Task subsystem a crack at knowning about the sent datagram
              FreeAndNil(CompletedSendDatagram)
            end else
            begin
              BufferDatagramReceive := DatagramReceiveManager.Process(Helper);
              if Assigned(BufferDatagramReceive) then
              begin
                DispatchDatagramReceive(BufferDatagramReceive);
                OlcbTaskManager.ProcessReceiving(BufferDatagramReceive);         // Give the Task subsystem a crack at knowning about the received datagram
                FreeAndNil(FBufferDatagramReceive)
              end;
            end;
          end else                                                                // *** Test for a Datagram message that came in ***
          begin
            BufferSniiReply := SniiManager.Process(Helper);                       // *** Test for a SNII meesage that came in ***
            if Assigned(BufferSniiReply) then
            begin
              DispatchSniiReply;
              OlcbTaskManager.ProcessReceiving(BufferSniiReply);                  // Give the Task subsystem a crack at knowning about the full recieved SNII message
              FreeAndNil(FBufferSniiReply);
            end else                                                              // *** Test for a SNII meesage that came in ***
              OlcbTaskManager.ProcessReceiving(Helper);
          end;
        end
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
  end;
end;

procedure TComPortThread.SyncDatagramMemConfigAddressSpaceInfoReply;
begin
  if Assigned(SyncDatagramMemConfigAddressSpaceInfoReplyFunc) then
    SyncDatagramMemConfigAddressSpaceInfoReplyFunc(BufferDatagramReceive)
end;

procedure TComPortThread.SyncDatagramMemConfigOperationReply;
begin
  if Assigned(SyncDatagramMemConfigOperationReplyFunc) then
    SyncDatagramMemConfigOperationReplyFunc(BufferDatagramReceive)
end;

procedure TComPortThread.SyncReceiveMessage;
begin
  if Assigned(SyncReceiveMessageFunc) then
    SyncReceiveMessageFunc(BufferRawMessage)
end;

procedure TComPortThread.SyncSendMessage;
begin
  if Assigned(SyncSendMessageFunc) then
    SyncSendMessageFunc(BufferRawMessage)
end;

procedure TComPortThread.SyncSniiReply;
begin
  if Assigned(SyncSniiReplyFunc) then
    SyncSniiReplyFunc(BufferSniiReply);
end;

procedure TComPortThread.WriteDebugInfo;
{$IFDEF DEBUG_THREAD}
var
  List: TList;
{$ENDIF}
begin
  {$IFDEF DEBUG_THREAD}
  List := DatagramReceiveManager.Datagrams.LockList;
  FormThreadDebug.LabelDatagramReceiveObjects.Caption := IntToStr( List.Count);
  DatagramReceiveManager.Datagrams.UnlockList;
  FormThreadDebug.LabelDatagramReceiveObjectsMax.Caption := IntToStr(DatagramReceiveManager.MaxCount);


  List := DatagramSendManager.Datagrams.LockList;
  FormThreadDebug.LabelDatagramSendObjects.Caption := IntToStr( List.Count);
  DatagramSendManager.Datagrams.UnlockList;
  FormThreadDebug.LabelDatagramSendObjectsMax.Caption := IntToStr( DatagramSendManager.MaxCount);

  List := OlcbTaskManager.TaskList.LockList;
  FormThreadDebug.LabelTaskObjects.Caption := IntToStr( List.Count);
  OlcbTaskManager.TaskList.UnlockList;
  FormThreadDebug.LabelTaskObjectsMax.Caption := IntToStr( OlcbTaskManager.MaxCount);

  FormThreadDebug.LabelSNIIObjects.Caption := IntToStr( SniiManager.Sniis.Count);
  FormThreadDebug.LabelSNIIObjectsMax.Caption := IntToStr( SniiManager.MaxCount);
  {$ENDIF}
end;

procedure TComPortThread.SyncErrorMessage;
begin
  if Assigned(SyncErrorMessageFunc) then
    SyncErrorMessageFunc(BufferRawMessage)
end;

constructor TComPortThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FThreadListSendStrings := TThreadList.Create;
  FDatagramReceiveManager := TDatagramReceiveManager.Create(Self);
  FDatagramSendManager := TDatagramSendManager.Create(Self);
  FSniiManager := TSniiReceiveManager.Create(Self);
  FOlcbTaskManager := TOlcbTaskEngine.Create(Self);
  FBaudRate := 9600;
  FPort := '';
  FTerminateThread := False;
  FTerminatedThread := False;
  FEnableReceiveMessages := True;
  FEnableSendMessages := True;
  FSyncErrorMessageFunc := nil;
  FSyncReceiveMessageFunc := nil;
  FSyncSendMessageFunc := nil;
  FSyncDatagramMemConfigOperationReplyFunc := nil;
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
  DatagramReceiveManager.Clear;
  FreeAndNil(FDatagramReceiveManager);
  DatagramSendManager.Clear;
  FreeAndNil(FDatagramSendManager);
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

procedure TComPortThread.AddDatagramToSend(Datagram: TDatagramSend);
var
  List: TList;
begin
  List := DatagramSendManager.Datagrams.LockList;
  try
    List.Add(Datagram);
    if List.Count > DatagramSendManager.MaxCount then DatagramSendManager.MaxCount := List.Count;
 //   if not DatagramSendManager.Timer.Enabled then
 //     DatagramSendManager.Timer.Enabled := True;
  finally
    DatagramSendManager.Datagrams.UnLockList
  end;
end;

procedure TComPortThread.AddTask(NewTask: TOlcbTaskBase);
var
  List: TList;
begin
  List := OlcbTaskManager.TaskList.LockList;
  try
    NewTask.FComPortThread := Self;
    List.Add(NewTask);
    if List.Count > OlcbTaskManager.MaxCount then OlcbTaskManager.MaxCount := List.Count;
  finally
    OlcbTaskManager.TaskList.UnlockList;
  end;

end;

{ TDatagramSendManager }

procedure TDatagramSendManager.TimerTick(Sender: TObject);
var
  SendDatagram: TDatagramSend;
  i: Integer;
  List: TList;
  AbandonList: TList;
begin
  List := Datagrams.LockList;
  AbandonList := AbandonDatagrams.LockList;
  try
  for i := List.Count - 1 downto 0 do     // May remove the item so need to go from the top down
  begin
    SendDatagram := TDatagramSend( List[i]);
    if not SendDatagram.Empty then
    begin
      if SendDatagram.AbandonTime > 4000 then
      begin
        List.Remove(SendDatagram);
        AbandonList.Add(SendDatagram);
      end;
      SendDatagram.AbandonTime := SendDatagram.AbandonTime + Timer.Interval;
    end;
  end;
  finally
    Datagrams.UnlockList;
    AbandonDatagrams.UnlockList;
  end;
end;


constructor TDatagramSendManager.Create(AnOwner: TComPortThread);
begin
  inherited Create;
  FDatagrams := TThreadList.Create;
  FAbandonDatagrams := TThreadList.Create;
  FOwner := AnOwner;
  Timer := TTimer.Create(nil);
  Timer.Interval := 500;         // Every 500m seconds
  Timer.OnTimer := @TimerTick;
  Timer.Enabled := False;
  FMaxCount := 0;
end;

destructor TDatagramSendManager.Destroy;
begin
  Clear;
  FreeAndNil(FDatagrams);
  ClearAbandon;
  FreeAndNil(FAbandonDatagrams);
  FreeAndNil(FTimer);
  inherited Destroy;
end;

procedure TDatagramSendManager.Clear;
var
  i: Integer;
  List: TList;
begin
  List := Datagrams.LockList;
  try
    for i := 0 to List.Count - 1 do
      TObject( List[i]).Free;
  finally
    List.Clear;
    Datagrams.UnlockList;
  end;
end;

procedure TDatagramSendManager.ClearAbandon;
var
  i: Integer;
  List: TList;
begin
  List := AbandonDatagrams.LockList;
  try
    for i := 0 to List.Count - 1 do
      TObject( List[i]).Free;
  finally
    List.Clear;
    AbandonDatagrams.UnlockList;
  end;
end;

function TDatagramSendManager.ProcessReceive(AHelper: TOpenLCBMessageHelper): TDatagramSend;
var
  Datagram: TDatagramSend;
  List: TList;
  i: Integer;
  Done: Boolean;
begin
  Result := nil;
  List := Datagrams.LockList;
  try
    i := 0;
    Done := False;
    while (i < List.Count) and not Done do
    begin
      Datagram := TDatagramSend( List[i]);
      if Datagram.ProcessReceive(AHelper, Owner) then
      begin
        Done := True;
        if Datagram.Empty then
        begin
          List.Remove(Datagram);
          Result := Datagram;
        end;
      end;
      Inc(i);
    end;
  finally
    Datagrams.UnlockList;
  end;
end;

procedure TDatagramSendManager.ProcessSend;
var
  Datagram: TDatagramSend;
  List: TList;
  i: Integer;
  Done: Boolean;
begin
  List := Datagrams.LockList;
  try
    i := 0;
    Done := False;
    while (i < List.Count) and not Done do
    begin
      Datagram := TDatagramSend( List[i]);
      Done := Datagram.ProcessSend(Owner);
      Inc(i);
    end;
  finally
    Datagrams.UnlockList;
  end;
end;

{ TDatagramReceiveManager }

function TDatagramReceiveManager.FindInProcessDatagram(AHelper: TOpenLCBMessageHelper): TDatagramReceive;
//
// Searches an in process datagram interaction between the nodes in the message
//
var
  i: Integer;
  List: TList;
  Datagram: TDatagramReceive;
begin
  i := 0;
  Result := nil;
  List := Datagrams.LockList;
  try
    while not Assigned(Result) and (i < List.Count) do
    begin
      Datagram := TDatagramReceive( List[i]);
      if not Datagram.Full and (Datagram.DestinationAlias = AHelper.DestinationAliasID) and (Datagram.SourceAlias = AHelper.SourceAliasID) then
        Result := Datagram;
      Inc(i)
    end;
  finally
    Datagrams.UnlockList;
  end;
end;

constructor TDatagramReceiveManager.Create(AnOwner: TComPortThread);
begin
  inherited Create;
  FOwner := AnOwner;
  FDatagrams := TThreadList.Create;
  FMaxCount := 0;
end;

destructor TDatagramReceiveManager.Destroy;
begin
  Clear;
  FreeAndNil(FDatagrams);
  inherited Destroy;
end;

procedure TDatagramReceiveManager.Clear;
var
  i: Integer;
  List: TList;
begin
  List := Datagrams.LockList;
  try
    for i := 0 to List.Count - 1 do
      TObject( List[i]).Free;
  finally
    List.Clear;
    Datagrams.UnlockList;
  end;
end;

function TDatagramReceiveManager.Process(AHelper: TOpenLCBMessageHelper): TDatagramReceive;
var
  TestDatagram: TDatagramReceive;
  List: TList;
begin
  Result := nil;
  if IsDatagramMTI(AHelper.MTI, False) then
  begin
    TestDatagram := FindInProcessDatagram(AHelper);
    if not Assigned(TestDatagram) then
    begin
      TestDatagram := TDatagramReceive.Create(Owner.SourceAlias, AHelper.SourceAliasID);  // Create a new receiving Datagram object for source alias of the message to us
      Datagrams.Add(TestDatagram);
      List := Datagrams.LockList;
      if List.Count > MaxCount then MaxCount := List.Count;
      Datagrams.UnlockList;
    end;
    TestDatagram.Process(AHelper, Owner);
    if TestDatagram.Full then
    begin
      Datagrams.Remove(TestDatagram);
      Result := TestDatagram
    end;
  end;
end;

{ TDatagramSend }

// *****************************************************************************
// Pulls the next 1..8 byte(s) from the Stream and puts them in the CAN Array to send, it always assumes the Stream Position of were to start
// in the Stream
//   Offset: [in] Where in the CAN array to use as the start of the transfer (may have some header bytes that need to be skipped)
//   ByteArray:  [out] CAN ByteArray that will be sent on the wire
//   Count    :  [out] The number of bytes that were transfered to the Byte Array
// *****************************************************************************
procedure TDatagramSend.StreamBytesToByteArray(Offset: Byte; var ByteArray: TCANByteArray; var Count: Byte);
begin
  Count := 0;
  // Tests for breaking out of loop:
  //   The Byte Array has 8 valid bytes (A)
  //   Finished moving all of the Stream Bytes (B)
  while (Offset < 8) {A} and (Stream.Position < Stream.Size) {B} do
  begin
    ByteArray[Offset] := Stream.ReadByte;
    Inc(Count);
    Inc(FCurrentPos);
    Inc(Offset);
  end;
end;

// *****************************************************************************
// Creates the datagram object
// *****************************************************************************
constructor TDatagramSend.Create;
begin
  inherited;
  FStream := TMemoryStream.Create;
  FLocalHelper := TOpenLCBMessageHelper.Create;
end;

// *****************************************************************************
// Destroys the datagram object
// *****************************************************************************
destructor TDatagramSend.Destroy;
begin
  FreeAndNil(FStream);
  FreeAndNil(FLocalHelper);
  inherited;
end;

// *****************************************************************************
// Loads and initalizes the object to start sending the datagram
// *****************************************************************************
procedure TDatagramSend.Initialize(AStream: TStream; AProtocolHeader: TCANByteArray; AProtocolHeaderLen: Byte; ASourceAlias, ADestinationAlias: Word);
begin
  Stream.Position := 0;
  if Assigned(AStream) then
  begin
    AStream.Position := 0;
    Stream.CopyFrom(AStream, AStream.Size);
  end;
  FProtocolHeader := AProtocolHeader;
  FProtocolHeaderLen := AProtocolHeaderLen;
  FSourceAlias := ASourceAlias;
  FDestinationAlias := ADestinationAlias;
  FCurrentPos := 0;
  FEmpty := False;
  FWaitingForACK := False;
  FErrorCode := $0000;
  FRetryCount := 0;
  FAbandonTime := 0;
end;

// *****************************************************************************
// Call repeatedly in the statemachine until Empty = True
//   ComPortThread: Thread to send the messages to
// *****************************************************************************
function TDatagramSend.ProcessSend(ComPortThread: TComPortThread): Boolean;
var
  Count: Byte;
begin
  Result := False;
  Count := 0;
  if not Empty and not WaitingForACK then
  begin
    if CurrentPos = 0 then
    begin
      // Starting a new packet, need to decide if it fits in a single frame
      if Stream.Size + ProtocolHeaderLen <= 8 then
      begin
        MTI := MTI_FRAME_TYPE_DATAGRAM_ONLY_FRAME;
        WaitingForACK := True;
      end else
        MTI := MTI_FRAME_TYPE_DATAGRAM_FRAME_START;
      DataBytesSent := ProtocolHeader;
      StreamBytesToByteArray(ProtocolHeaderLen, FDataBytesSent, Count);
      DataBytesSentLen := Count + ProtocolHeaderLen;
      LocalHelper.Load(ol_OpenLCB, MTI, SourceAlias, DestinationAlias, DataBytesSentLen, DataBytesSent[0], DataBytesSent[1], DataBytesSent[2], DataBytesSent[3], DataBytesSent[4], DataBytesSent[5], DataBytesSent[6], DataBytesSent[7]);
      ComPortThread.Add(LocalHelper.Encode);
    end else
    begin
      if (Stream.Size - Stream.Position <= 8) then
      begin
        MTI := MTI_FRAME_TYPE_DATAGRAM_FRAME_END;
        WaitingForACK := True;
      end else
        MTI := MTI_FRAME_TYPE_DATAGRAM_FRAME;
      StreamBytesToByteArray(0, FDataBytesSent, Count);
      DataBytesSentLen := Count;
      LocalHelper.Load(ol_OpenLCB, MTI, SourceAlias, DestinationAlias, DataBytesSentLen, DataBytesSent[0], DataBytesSent[1], DataBytesSent[2], DataBytesSent[3], DataBytesSent[4], DataBytesSent[5], DataBytesSent[6], DataBytesSent[7]);
    end;
    Result := True;
  end;
end;

function TDatagramSend.ProcessReceive(AHelper: TOpenLCBMessageHelper; ComPortThread: TComPortThread): Boolean;
//
// It is assumed that the message is actually for this object and the object is not empty, it is not checked.........
begin
  if not Empty and WaitingForACK then   // See if we are waiting for the node to sent us and ACK
  begin
    if (AHelper.SourceAliasID = DestinationAlias) and (AHelper.DestinationAliasID = SourceAlias) then
    begin
      case AHelper.MTI of
        MTI_DATAGRAM_OK_REPLY : Empty := True;
        MTI_DATAGRAM_REJECTED_REPLY :
          begin
             ErrorCode := AHelper.ExtractDataBytesAsInt(2, 3);
             if (ErrorCode and DATAGRAM_REJECTED_RESEND_MASK <> 0) and (RetryCount < DATAGRAM_MAX_RETRYS) then
             begin
               CurrentPos := 0;                                                 // Reset the statemachine to send it all over again
               if Assigned(Stream) then
                 Stream.Position := 0;
               ErrorCode := $0000;
               WaitingForACK := False;
               Empty := False;
               Inc(FRetryCount);                                                // Kick the resend off
             end else
               Empty := True                                                    // Error; don't resend and quit with ErrorCode set
          end;
      end;
    end;
  end
end;

{ TDatagramReceive }


// *****************************************************************************
// Clears the structures and fields of the object
// *****************************************************************************
procedure TDatagramReceive.Clear;
var
  i: Integer;
begin
  FCurrentPos := 0;
  FFull := False;
  for i := 0 to MAX_DATAGRAM_LENGTH - 1 do
    RawDatagram[i] := 0;
end;

procedure TDatagramReceive.SendACK(ComPortThread: TComPortThread);
begin
  LocalHelper.Load(ol_OpenLCB, MTI_DATAGRAM_OK_REPLY, SourceAlias, DestinationAlias, 2, 0, 0, 0, 0, 0, 0, 0, 0);
  ComPortThread.Add(LocalHelper.Encode);
end;

constructor TDatagramReceive.Create(ASourceAlias, ADestinationAlias: Word);
begin
  inherited Create;
  FLocalHelper := TOpenLCBMessageHelper.Create;
  FDestinationAlias := ADestinationAlias;
  FSourceAlias := ASourceAlias;
end;

destructor TDatagramReceive.Destroy;
begin
  FreeAndNil(FLocalHelper);
  inherited;
end;

// *****************************************************************************
// Helper to concat up to 4 bytes in the Datagram Data Array into an integer/QWord
//  StartByteIndex: The first byte to use in the result (most significant 8 bits of integer/QWord)
//  EndByteIndex  : The last byte to use in the result (least significant 8 bits of integer/QWord)
// *****************************************************************************
function TDatagramReceive.ExtractDataBytesAsInt(StartByteIndex, EndByteIndex: Integer): QWord;
var
  i, Offset, Shift: Integer;
  ByteAsQ, ShiftedByte: QWord;
begin
  Result := 0;
  Offset := EndByteIndex - StartByteIndex;
  for i := StartByteIndex to EndByteIndex do
  begin
    Shift := Offset * 8;
    ByteAsQ := QWord( RawDatagram[i]);
    ShiftedByte := ByteAsQ shl Shift;
    Result := Result or ShiftedByte;
    Dec(Offset)
  end;
end;

// *****************************************************************************
// Helper to convert a section of the Datagram Array bytes to a string
//  StartIndex: The index of the byte in the Datagram Array as the first character of the string
//  Count     : The number of character in the string (bytes)
// *****************************************************************************
function TDatagramReceive.ExtractDataBytesAsString(StartIndex, Count: Integer): String;
var
  i, Last: Integer;
begin
  Result := '';
  if Count = -1 then
    Last := CurrentPos
  else
    Last := StartIndex + Count;
  if Last > CurrentPos then
    Last := CurrentPos;

  for i := StartIndex to Last - 1 do
    Result := Result + Chr( RawDatagram[i]);
end;

// *****************************************************************************
// Helper to copy the contents of the Datagram Array to a Stream
//   Stream:  The stream to copy the bytes to
//   StartIndex: The index of the byte in the Datagram Array to copy into the stream
//   Count     : The number of bytes to copy into the stream
// *****************************************************************************
procedure TDatagramReceive.CopyToStream(Stream: TStream; StartIndex, Count: Integer);
var
  i, Last: Integer;
begin
  if Count = -1 then
    Last := CurrentPos
  else
    Last := StartIndex + Count;

  if Last > CurrentPos then
    Last := CurrentPos;

  for i := StartIndex to Last - 1 do
    Stream.WriteByte(RawDatagram[i]);
end;

// *****************************************************************************
// Call as part of the statemachine as new OLCB messages are received.
//   AHelper:  The message that was received on the OLCB wire
//
//  NOTE:  It assumes that checks for the correct Source and Destination Alias's
//         have already been made.
//
// *****************************************************************************
procedure TDatagramReceive.Process(AHelper: TOpenLCBMessageHelper; ComPortThread: TComPortThread);
var
  i: Integer;
begin
  case AHelper.MTI of
     MTI_FRAME_TYPE_DATAGRAM_ONLY_FRAME :
       begin
         Clear;
         for i := 0 to AHelper.DataCount - 1 do
           RawDatagram[i] := AHelper.Data[i];
         CurrentPos := AHelper.DataCount;
         SendACK(ComPortThread);
         Full := True
       end;
     MTI_FRAME_TYPE_DATAGRAM_FRAME_START :
       begin
         Full := False;
         for i := 0 to AHelper.DataCount - 1 do
           RawDatagram[i] := AHelper.Data[i];
         CurrentPos := AHelper.DataCount;
       end;
     MTI_FRAME_TYPE_DATAGRAM_FRAME :
       begin
         for i := 0 to AHelper.DataCount - 1 do
           RawDatagram[CurrentPos+i] := AHelper.Data[i];
         CurrentPos := CurrentPos + AHelper.DataCount;
       end;
    MTI_FRAME_TYPE_DATAGRAM_FRAME_END :
      begin
        for i := 0 to AHelper.DataCount - 1 do
          RawDatagram[CurrentPos+i] := AHelper.Data[i];
        CurrentPos := CurrentPos + AHelper.DataCount;
        SendACK(ComPortThread);
        Full := True;
      end;
  end;
end;

{ TSniiReceiveManager }

function TSniiReceiveManager.GetSnii(Index: Integer): TSNII;
begin
  if (Index > -1) and (Index < Sniis.Count) then
    Result := TSNII( Sniis[Index])
  else
    Result := nil
end;

function TSniiReceiveManager.FindInProcessSnii(AHelper: TOpenLCBMessageHelper): TSNII;
//
// Searches an in process SNII interaction between the nodes in the message
//
var
  i: Integer;
  LocalSnii: TSnii;
begin
  i := 0;
  Result := nil;
  while not Assigned(Result) and (i < Sniis.Count) do
  begin
    LocalSnii := Snii[i];
    if not LocalSnii.Full and (LocalSnii.SourceAlias = AHelper.DestinationAliasID) and (LocalSnii.DestinationAlias = AHelper.SourceAliasID)  then
      Result := LocalSnii;
    Inc(i)
  end;
end;

constructor TSniiReceiveManager.Create(AnOwner: TComPortThread);
begin
  inherited Create;
  FOwner := AnOwner;
  FSniis := TList.Create;
  FMaxCount := 0;
end;

destructor TSniiReceiveManager.Destroy;
begin
  Clear;
  FreeAndNil(FSniis);
  inherited Destroy;
end;

procedure TSniiReceiveManager.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to Sniis.Count - 1 do
      TObject( Sniis[i]).Free;
  finally
    Sniis.Clear;
  end;
end;

function TSniiReceiveManager.Process(AHelper: TOpenLCBMessageHelper): TSNII;
var
  TestSnii: TSNII;
begin
  Result := nil;
  if AHelper.MTI = MTI_SIMPLE_NODE_INFO_REPLY then
  begin
    TestSnii := FindInProcessSnii(AHelper);
    if not Assigned(TestSnii) then
    begin
      TestSnii := TSNII.Create(Owner.SourceAlias, AHelper.SourceAliasID);  // Create a new receiving Snii object for source alias of the message to us
      Sniis.Add(TestSnii);
      if Sniis.Count > MaxCount then MaxCount := Sniis.Count;
    end;
    Result := TestSnii.Process(AHelper);
    if Assigned(Result) then                                               // If it is complete then it is returned AND removed from the list, it is now owned by the caller
      Sniis.Remove(Result);
  end;
end;

{ TSNII }

constructor TSNII.Create(ASourceAlias, ADestinationAlias: Word);
begin
  inherited Create;
  FLocalHelper := TOpenLCBMessageHelper.Create;
  FDestinationAlias := ADestinationAlias;
  FSourceAlias := ASourceAlias;
  Full := False;
  SniiMfgName := '';
  SniiMfgModel := '';
  SniiSoftwareVersion := '';
  SniiHardwareVersion := '';
  SniiUserName := '';
  SniiUserDescription := '';
  SniiUserVersion := 0;
  SniiMfgVersion := 0;
  StateMachineIndex := 0;
end;

destructor TSNII.Destroy;
begin
  FreeAndNil(FLocalHelper);
  inherited;
end;

function TSNII.Duplicate: TSNII;
begin
  Result := TSNII.Create(0, 0);
  Result.SourceAlias := SourceAlias;
  Result.DestinationAlias := DestinationAlias;
  Result.Full := Full;
  Result.SniiHardwareVersion := SniiHardwareVersion;
  Result.SniiMfgModel := SniiMfgModel;
  Result.SniiMfgName := SniiMfgName;
  Result.SniiMfgVersion := SniiMfgVersion;
  Result.SniiSoftwareVersion := SniiSoftwareVersion;
  Result.SniiUserDescription := SniiUserDescription;
  Result.SniiUserName := SniiUserName;
  Result.SniiUserVersion := SniiUserVersion;
  Result.StateMachineIndex := StateMachineIndex;
end;

function TSNII.Process(AHelper: TOpenLCBMessageHelper): TSNII;
// It is assumed that the message is actually for this object and the object is not full, it is not checked........
var
  i: Integer;
begin
  Result := nil;
  i := 2;                                      // Strip off the destination Alias
  while i < AHelper.DataCount do
  begin
    case StateMachineIndex of
      STATE_SNII_MFG_VERSION :
        begin
          SniiMfgVersion := AHelper.Data[i];
          Inc(i);
          StateMachineIndex := STATE_SNII_MFG_NAME;
        end;
      STATE_SNII_MFG_NAME     :
        begin
          if Chr( AHelper.Data[i]) <> #0 then
          begin
            SniiMfgName := SniiMfgName + Chr( AHelper.Data[i]);
            Inc(i);
          end else
          begin
            Inc(i);
            StateMachineIndex := STATE_SNII_MFG_MODEL;
          end;
        end;
      STATE_SNII_MFG_MODEL     :
        begin
          if Chr( AHelper.Data[i]) <> #0 then
          begin
            SniiMfgModel := SniiMfgModel + Chr( AHelper.Data[i]);
            Inc(i);
          end else
          begin
            Inc(i);
            StateMachineIndex := STATE_SNII_HARDWARE_VER;
          end;
        end;
      STATE_SNII_HARDWARE_VER  :
        begin
          if Chr( AHelper.Data[i]) <> #0 then
          begin
            SniiHardwareVersion := SniiHardwareVersion + Chr( AHelper.Data[i]);
            Inc(i);
          end else
          begin
            Inc(i);
            StateMachineIndex := STATE_SNII_SOFTWARE_VER;
          end;
        end;
      STATE_SNII_SOFTWARE_VER  :
        begin
          if Chr( AHelper.Data[i]) <> #0 then
          begin
            SniiSoftwareVersion := SniiSoftwareVersion + Chr( AHelper.Data[i]);
            Inc(i);
          end else
          begin
            Inc(i);
            StateMachineIndex := STATE_SNII_USER_VERSION;
          end;
        end;
      STATE_SNII_USER_VERSION  :
        begin
          SniiUserVersion := AHelper.Data[i];
          Inc(i);
          StateMachineIndex := STATE_SNII_USER_NAME;
        end;
      STATE_SNII_USER_NAME     :
        begin
          if Chr( AHelper.Data[i]) <> #0 then
          begin
            SniiUserName := SniiUserName + Chr( AHelper.Data[i]);
            Inc(i);
          end else
          begin
            Inc(i);
            StateMachineIndex := STATE_SNII_USER_DESC;
          end;
        end;
      STATE_SNII_USER_DESC     :
        begin
          if Chr( AHelper.Data[i]) <> #0 then
          begin
            SniiUserDescription := SniiUserDescription + Chr( AHelper.Data[i]);
            Inc(i);
          end else
          begin
            FFull := True;
            Result := Self;
            Inc(i);
            StateMachineIndex := STATE_SNII_DONE;
          end;
        end;
      STATE_SNII_DONE          :
        begin
          FFull := True;
          Result := Self;
          Inc(i)
        end;
    end;
  end;
end;

{ TOlcbTaskEngine }

constructor TOlcbTaskEngine.Create(AnOwner: TComPortThread);
begin
  inherited Create;
  FOwner := AnOwner;
  FTaskList := TThreadList.Create;
  FMaxCount := 0;
end;

procedure TOlcbTaskEngine.ProcessReceiving(MessageInfo: TOlcbMessage);
var
  List: TLIst;
  Task: TOlcbTaskBase;
  i: Integer;
begin
  List := TaskList.LockList;
  try
    i := 0;
    while i < List.Count do
    begin
      Task := TOlcbTaskBase( List[i]);
      if not Task.Sending then
      begin
        Task.Process(MessageInfo);
        if Task.Done then
        begin
          List.Remove(Task);
          FreeAndNil(Task);
        end;
      end;
      Inc(i)
    end;
  finally
    TaskList.UnlockList;
  end;
end;

procedure TOlcbTaskEngine.ProcessSending;
var
  List: TLIst;
  Task: TOlcbTaskBase;
  i: Integer;
begin
  List := TaskList.LockList;
  try
    i := 0;
    while i < List.Count do
    begin
      Task := TOlcbTaskBase( List[i]);
      if Task.Sending then
      begin
        Task.Process(nil);
        if Task.Done then
        begin
          List.Remove(Task);     // If the last state of the task was to send something we need to free the task
          FreeAndNil(Task)
        end;
      end;
      Inc(i)
    end;
  finally
    TaskList.UnlockList;
  end;
end;

destructor TOlcbTaskEngine.Destroy;
begin
  Clear;
  FreeAndNil(FTaskList);
  inherited Destroy;
end;

procedure TOlcbTaskEngine.Clear;
var
  i: Integer;
  List: TList;
begin
  List := TaskList.LockList;
  try
    for i := 0 to List.Count - 1 do
      TObject( List[i]).Free
  finally
    List.Clear;
    TaskList.UnLockList
  end;
end;


end.

