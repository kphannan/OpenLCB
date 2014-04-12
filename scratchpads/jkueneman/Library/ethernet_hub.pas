unit ethernet_hub;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synsock, Forms, olcb_app_common_settings, Dialogs,
  common_utilities, olcb_utilities, olcb_transport_layer, olcb_defines, strutils;

type
  TClientSocketThread = class;
  TEthernetHub = class;
  TSocketThreadList = class;

  { TTCPMessage }

  TTCPMessage = class
  private
    FMessage: string;
    FSource: TClientSocketThread;
  public
    constructor Create;
    property Source: TClientSocketThread read FSource write FSource;   // which socket the message came from
    property Message: string read FMessage write FMessage;           // Grid Connect message received
  end;

  { TTCPMessageList }

  TTCPMessageList = class(TThreadList)
  private
    function GetCount: Integer;
  public
    destructor Destroy; override;
    procedure ClearObjects;

    property Count: Integer read GetCount;
  end;

  { TTCPMessageManager }

  TTCPMessageManager = class
  private
    FMessages: TTCPMessageList;
  public
    constructor Create;
    destructor Destroy; override;

    property Messages: TTCPMessageList read FMessages write FMessages;
  end;

  { TSocketThreadList }
  TSocketThreadList = class(TThreadList)      // Contains TClientSocketThread objects
  private
    function GetCount: Integer;
  public
    destructor Destroy; override;
    procedure ClearObjects;

    property Count: Integer read GetCount;
  end;

  { TClientSocketThread }
  TClientSocketThread = class(TTransportLayerThread)
  private
    FhSocketLocal: TSocket;
    FMaxLoopTime: DWord;
    FOwnerHub: TEthernetHub;
    FUseSocketHandle: Boolean;
  protected
    procedure Execute; override;
    procedure SyncOnConnectionState; override;
    property hSocketLocal: TSocket read FhSocketLocal write FhSocketLocal;
    property OwnerHub: TEthernetHub read FOwnerHub write FOwnerHub;
    property UseSocketHandle: Boolean read FUseSocketHandle write FUseSocketHandle;            // If true the socket will use the handle in hSocketLocal to create the connection, else it will use the GlobalSettings for the Listen and Client Ports
  public
    constructor Create(CreateSuspended: Boolean; UseSocketLocalParemeter: Boolean); reintroduce; virtual;
    destructor Destroy; override;
    property MaxLoopTime: DWord read FMaxLoopTime write FMaxLoopTIme;
  end;


  { TEthernetListenDameonThread }

  TEthernetListenDameonThread = class(TTransportLayerThread)
  private
    FOwnerHub: TEthernetHub;
    FListeningSocket: TTCPBlockSocket;
  protected
    procedure Execute; override;
    property OwnerHub: TEthernetHub read FOwnerHub write FOwnerHub;
    property ListeningSocket: TTCPBlockSocket read FListeningSocket write FListeningSocket;     // Caution, created in context of thread
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
  end;

  { TEthernetHub }
  TEthernetHub = class
  private
    FBufferRawMessage: string;
    FEnableOPStackCallback: Boolean;
    FEnableReceiveMessages: Boolean;
    FEnableSendMessages: Boolean;
    FListener: Boolean;
    FMessageManager: TTCPMessageManager;
    FOnBeforeDestroyTask: TOlcbTaskBeforeDestroy;
    FEnabled: Boolean;
    FListenDameon: TEthernetListenDameonThread;
    FClientThreadList: TSocketThreadList;
    FOnOPstackCallback: TOnOPStackCallback;
    FOnStatus: THookSocketStatus;
    FSingletonClient: TClientSocketThread;
    FOnConnectionStateChange: TOnConnectionStateChange;
    FOnErrorMessage: TOnRawMessage;
    FOnReceiveMessage: TOnRawMessage;
    FOnSendMessage: TOnRawMessage;
    procedure SetEnabled(AValue: Boolean);
    procedure SetEnableReceiveMessages(AValue: Boolean);
    procedure SetEnableSendMessages(AValue: Boolean);
    procedure SetListener(AValue: Boolean);
  protected
    procedure InternalAdd(Msg: AnsiString);
    procedure InternalAddDatagramToSendByCANParsing(Datagram: TCANFrameParserDatagramSend);
    property BufferRawMessage: string read FBufferRawMessage write FBufferRawMessage;
    property ListenDameon: TEthernetListenDameonThread read FListenDameon;
    property SingletonClient: TClientSocketThread read FSingletonClient write FSingletonClient;
  public
    constructor Create;
    destructor Destroy; override;

    function AddGridConnectStr(GridConnectStr: ansistring): Boolean;
    function AddTask(NewTask: TTaskOlcbBase): Boolean;
    procedure RemoveAndFreeTasks(RemoveKey: PtrInt);

    property ClientThreadList: TSocketThreadList read FClientThreadList write FClientThreadList;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property EnableReceiveMessages: Boolean read FEnableReceiveMessages write SetEnableReceiveMessages;
    property EnableSendMessages: Boolean read FEnableSendMessages write SetEnableSendMessages;
    property EnableOPStackCallback: Boolean read FEnableOPStackCallback write FEnableOPStackCallback;
    property Listener: Boolean read FListener write SetListener;
    property MessageManager: TTCPMessageManager read FMessageManager write FMessageManager;
    property OnBeforeDestroyTask: TOlcbTaskBeforeDestroy read FOnBeforeDestroyTask write FOnBeforeDestroyTask;
    property OnStatus: THookSocketStatus read FOnStatus write FOnStatus;
    property OnErrorMessage: TOnRawMessage read FOnErrorMessage write FOnErrorMessage;
    property OnReceiveMessage: TOnRawMessage read FOnReceiveMessage write FOnReceiveMessage;
    property OnSendMessage: TOnRawMessage read FOnSendMessage write FOnSendMessage;
    property OnConnectionStateChange: TOnConnectionStateChange read FOnConnectionStateChange write FOnConnectionStateChange;
    property OnOPStackCallback: TOnOPStackCallback read FOnOPstackCallback write FOnOPStackCallback;
  end;


implementation

const
  TCP_STATE_SYNC_START = 0;
  TCP_STATE_SYNC_FIND_X = 1;
  TCP_STATE_SYNC_FIND_HEADER = 2;
  TCP_STATE_SYNC_FIND_N = 3;
  TCP_STATE_SYNC_FIND_DATA = 4;

  const
  // :X19170640N0501010107015555;   Example.....
  // ^         ^                ^
  // 0         10               27
  MAX_GRID_CONNECT_LEN = 28;
  GRID_CONNECT_HEADER_OFFSET_HI = 2;
  GRID_CONNECT_HEADER_OFFSET_LO = 4;
  GRID_CONNECT_DATA_OFFSET = 11;

constructor TTCPMessage.Create;
begin
  FMessage := '';
  FSource := nil;
end;

{ TTCPMessageManager }

constructor TTCPMessageManager.Create;
begin
  FMessages := TTCPMessageList.Create;
end;

destructor TTCPMessageManager.Destroy;
begin
  FreeAndNil(FMessages);
  inherited Destroy;
end;

{ TTCPMessageList }

function TTCPMessageList.GetCount: Integer;
var
  L: TList;
begin
  L := LockList;
  try
    Result := L.Count
  finally
    UnLockList
  end;
end;

destructor TTCPMessageList.Destroy;
begin
  ClearObjects;
  inherited Destroy;
end;

procedure TTCPMessageList.ClearObjects;
var
  L: TList;
  i: Integer;
begin
  L := LockList;
  try
    for i := 0 to L.Count - 1 do
      TObject(L[i]).Free;
  finally
    L.Clear;
    UnLockList
  end;
end;

{ TClientSocketThread }

procedure TClientSocketThread.Execute;
var
  i: Integer;
  iSplit: Integer;
  List: TList;
  SendStr, RcvStr: AnsiString;
  Helper: TOpenLCBMessageHelper;
  SyncSendMessageList: TStringList;
  StrLen: Integer;
  ConnectedSocket: TTCPBlockSocket;
  DoClose: Boolean;
  GridConnectStrPtr: PGridConnectString;
begin
  Helper := nil;
  SyncSendMessageList := nil;
  ConnectedSocket := nil;

  // Startup, this sends the csConnecting state which will add the object to the Hub List of Clients
  ExecuteBegin;
  try
    // Setup the socket and try to create it
    ConnectedSocket := TTCPBlockSocket.Create;
    ConnectedSocket.OnStatus := OnStatus;
    ConnectedSocket.ConvertLineEnd := True;      // User #10, #13, or both to be a "string"
    ConnectedSocket.SetLinger(False, 0);
    ConnectedSocket.SetSendTimeout(0);
    ConnectedSocket.SetRecvTimeout(0);
    ConnectedSocket.SetTimeout(0);
    try
      // Create helpers
      Helper := TOpenLCBMessageHelper.Create;
      SyncSendMessageList := TStringList.Create;

      // The socket can be created from an existing handle from a remote IP:Port or from a direct connect
      if UseSocketHandle then
      begin
        ConnectedSocket.Socket := hSocketLocal;
        if ConnectedSocket.LastError = 0 then
        begin
          ConnectedSocket.GetSins;                     // Back load the IP's / Ports information from the handle
          if ConnectedSocket.LastError = 0 then
            ConnectionState := csConnected
        end
      end else
      begin
        ConnectedSocket.Bind(GlobalSettings.Ethernet.LocalIP, IntToStr(GlobalSettings.Ethernet.ClientPort));
        if ConnectedSocket.LastError = 0 then
        begin
          ConnectedSocket.Connect(GlobalSettings.Ethernet.LocalIP, IntToStr(GlobalSettings.Ethernet.ListenPort));
          if ConnectedSocket.LastError = 0 then
            ConnectionState := csConnected;
        end
      end;

      // If error show the message and terminate
      if ConnectedSocket.LastError <> 0 then
      begin
        if UseSocketHandle then
          ShowErrorMessageAndTerminate('Unable to connect to IP: ' + GlobalSettings.Ethernet.LocalIP + ':' + IntToStr(GlobalSettings.Ethernet.ListenPort) + #13 + #10 + ConnectedSocket.LastErrorDesc)
        else
          ShowErrorMessageAndTerminate('Unable to connect Local IP: ' + GlobalSettings.Ethernet.LocalIP + ':' + IntToStr(GlobalSettings.Ethernet.ClientPort) + #13 + #10
                                        + 'to Remote IP: ' + GlobalSettings.Ethernet.RemoteIP + ':' + IntToStr(GlobalSettings.Ethernet.ListenPort) + #13 + #10 + ConnectedSocket.LastErrorDesc)
      end;

      // This will either be csConnected or csConnecting if it failed
      Synchronize(@SyncOnConnectionState);

      while not Terminated and (ConnectionState = csConnected) do
      begin
        ThreadSwitch;
        List := ThreadListSendStrings.LockList;                                 // *** Pickup the next Message to Send ***
        try
          if List.Count > 0 then
          begin
            if TStringList( List[0]).Count > 0 then
            begin
              for i := 0 to TStringList( List[0]).Count - 1 do
              begin
                BufferRawMessage := TStringList( List[0])[i] ;
                if Helper.Decompose(BufferRawMessage) then
                begin
                  // Copy the message to a list that will be sent to the UI if enabled
                  if EnableSendMessages then
                    SyncSendMessageList.Add(BufferRawMessage);

                  if i < TStringList( List[0]).Count - 1 then
                    SendStr := SendStr + BufferRawMessage + #10
                  else
                    SendStr := SendStr + BufferRawMessage;
                end;
              end;
              TStringList( List[0]).Clear;
            end;
          end;
        finally
          ThreadListSendStrings.UnlockList;                                     // Deadlock if we don't do this here when the main thread blocks trying to add a new Task and we call Syncronize asking the main thread to run.....

          // Send the strings to the logging windows OUTSIDE of the locked input string list
          for i := 0 to SyncSendMessageList.Count - 1 do
          begin
            BufferRawMessage := SyncSendMessageList[i];
            Synchronize(@SyncOnSendMessage);
          end;
          SyncSendMessageList.Clear;
        end;

        CANFrameParserStreamSendManager.ProcessSend;                              // *** See if there is a stream that is being disceted and frame out on a CAN connection ***
        CANFrameParserDatagramSendManager.ProcessSend;                            // *** See if there is a datagram that is being disceted and frame out on a CAN connection ***
        OlcbTaskManager.ProcessSending;                                           // *** See if there is a task what will add a message to send ***
        if SendStr <> '' then                                                     // *** Put the message on the wire and communicate back the raw message sent ***
        begin
          ConnectedSocket.SendString(SendStr + LF);                               // Try to send the message
          SendStr := '';

          if ConnectedSocket.LastError <> 0 then
            ShowErrorMessageAndTerminate('Socket Error: ' + ConnectedSocket.LastErrorDesc);
        end;

        if not Terminated then
        begin
          RcvStr := ConnectedSocket.RecvPacket(1);
          case ConnectedSocket.LastError of
            S_OK         : begin
                             for i := 1 to Length(RcvStr) do
                             begin
                               if GridConnect_DecodeMachine(RcvStr[i], GridConnectStrPtr) then
                                 DecomposeAndDispatchGridConnectString(GridConnectStrPtr^, Helper);
                             end;
                           end;
            WSAETIMEDOUT : begin end; // Normal if nothing to read
          else
            ShowErrorMessageAndTerminate('Socket Error: ' + ConnectedSocket.LastErrorDesc)
          end;
        end;
      end;
    finally

      // Signal we are disconneting to the UI, will remove the thread from the Hub List
      DoClose := ConnectionState = csConnected;
      ConnectionState := csDisconnecting;
        Synchronize(@SyncOnConnectionState);

      // Close the socket if it was open
      if DoClose then
        ConnectedSocket.CloseSocket;

      // Clean up
      FreeAndNil(Helper);
      FreeAndNil(SyncSendMessageList);
    end;
  finally
    // Cleanup
    FreeAndNil(ConnectedSocket);
    // Will send csDisconnected state
    ExecuteEnd;
  end
end;

procedure TClientSocketThread.SyncOnConnectionState;
begin
  case ConnectionState of
    csConnecting : OwnerHub.ClientThreadList.Add(Self);
    csDisconnecting :
      begin
        OwnerHub.ClientThreadList.Remove(Self);
        FreeOnTerminate := True;
      end;
  end;
  inherited SyncOnConnectionState;
end;

constructor TClientSocketThread.Create(CreateSuspended: Boolean; UseSocketLocalParemeter: Boolean);
begin
  inherited Create(True);
  UseSocketHandle := UseSocketLocalParemeter;
  FMaxLoopTime := 0;
  FOwnerHub := nil;
end;

destructor TClientSocketThread.Destroy;
begin
  inherited Destroy;
end;

{ TSocketThreadList }


function TSocketThreadList.GetCount: Integer;
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

destructor TSocketThreadList.Destroy;
begin
  ClearObjects;
  inherited Destroy;
end;

procedure TSocketThreadList.ClearObjects;
var
  i: Integer;
  L: TList;
  SocketThread: TClientSocketThread;
begin
  L := LockList;
  try
    for i := 0 to L.Count - 1 do
    begin;
      SocketThread := TClientSocketThread( L[i]);
      SocketThread.Terminate;
    end;
  finally
    L.Clear;
    UnlockList;
  end;
end;

{ TEthernetListenDameonThread }

constructor TEthernetListenDameonThread.Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);
begin
  inherited Create(CreateSuspended);
  FOwnerHub := nil;
end;

destructor TEthernetListenDameonThread.Destroy;
begin
  inherited Destroy;
end;

procedure TEthernetListenDameonThread.Execute;
var
  hSocket: TSocket;
  ClientSocketThread: TClientSocketThread;
  List: TList;
  i: Integer;
  DoClose: Boolean;
begin
  // Startup, this will send the csConnecting state
  ExecuteBegin;
  try
    // Create what we need
    ListeningSocket := TTCPBlockSocket.Create;
    ListeningSocket.CreateSocket;

    // Make the Socket connection for the Listener
    ListeningSocket.Bind(GlobalSettings.Ethernet.LocalIP, IntToStr(GlobalSettings.Ethernet.ListenPort));  // Bind to this machine IP at Port 12021;
    if ListeningSocket.LastError = 0 then
    begin
      ListeningSocket.SetLinger(True, 1000);
      if ListeningSocket.LastError = 0 then
      begin
        ListeningSocket.Listen;                    // Listen for connections
        if ListeningSocket.LastError = 0 then
          ConnectionState := csConnected;
      end
    end;

    // If there was an error show it then get out
    if ListeningSocket.LastError <> 0 then
      ShowErrorMessageAndTerminate('Unable to connect to IP: ' + GlobalSettings.Ethernet.LocalIP + ':' + IntToStr(GlobalSettings.Ethernet.ListenPort) + #13 + #10 + ListeningSocket.LastErrorDesc);

    // This will either be csConnected or csConnecting if it failed
    Synchronize(@SyncOnConnectionState);

    // Run the loop looking for clients, but only if connected
    while not Terminated and (ConnectionState = csConnected) do
    begin
      if ListeningSocket.CanRead(1000) then
      begin
        if ListeningSocket.LastError = 0 then
        begin
          if not Terminated then
          begin
            hSocket := ListeningSocket.Accept;        // Get the handle of the new ListeningSocket for the client connection
            if ListeningSocket.LastError = 0 then
            begin
              ClientSocketThread := TClientSocketThread.Create(True, True);   // Use the hSocketLocal parameter to create socket
              ClientSocketThread.hSocketLocal := hSocket;
              ClientSocketThread.OwnerHub := OwnerHub;
              ClientSocketThread.OnReceiveMessage := OwnerHub.OnReceiveMessage;
              ClientSocketThread.OnSendMessage := OwnerHub.OnSendMessage;
              ClientSocketThread.OnErrorMessage := OwnerHub.OnErrorMessage;
              ClientSocketThread.OnConnectionStateChange := OwnerHub.OnConnectionStateChange;
              ClientSocketThread.OnOPStackCallback := OnOPStackCallback;
              ClientSocketThread.EnableReceiveMessages := OwnerHub.EnableReceiveMessages;
              ClientSocketThread.EnableSendMessages := OwnerHub.EnableSendMessages;
              ClientSocketThread.EnableOPStackCallback := EnableOPStackCallback;
              ClientSocketThread.OnBeforeDestroyTask := OwnerHub.OnBeforeDestroyTask;
              ClientSocketThread.OnStatus := OwnerHub.OnStatus;
              ClientSocketThread.Suspended := False;
            end else
              ShowErrorMessageAndTerminate('Error in connection to IP: ' + GlobalSettings.Ethernet.LocalIP + ':' + IntToStr(GlobalSettings.Ethernet.ListenPort) + #13 + #10 + ListeningSocket.LastErrorDesc);
          end;
        end else
          ShowErrorMessageAndTerminate('Error in connection to IP: ' + GlobalSettings.Ethernet.LocalIP + ':' + IntToStr(GlobalSettings.Ethernet.ListenPort) + #13 + #10 + ListeningSocket.LastErrorDesc);
      end;
    end;
  finally
    // If the Listener goes down take down the Clients.  They will remove themselves
    // from the list and free themselves
    List := OwnerHub.ClientThreadList.LockList;
    try
      for i := 0 to List.Count - 1 do
        TClientSocketThread( List.Items[i]).Terminate;
    finally
      OwnerHub.ClientThreadList.UnlockList;
    end;


    // Signal we are disconneting to the UI
    DoClose := ConnectionState = csConnected;
    ConnectionState := csDisconnecting;
      Synchronize(@SyncOnConnectionState);

    // Close the Socket
    if DoClose then
      ListeningSocket.CloseSocket;

    // Clean up
    FreeAndNil(FListeningSocket);

    // This will send the csDisconnected state
    ExecuteEnd;
  end
end;

{ TEthernetHub }

procedure TEthernetHub.SetEnabled(AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled:=AValue;
    if Listener then
    begin
      if FEnabled then
      begin
        FListenDameon := TEthernetListenDameonThread.Create(True);
        ListenDameon.OwnerHub := Self;
        ListenDameon.OnReceiveMessage := OnReceiveMessage;
        ListenDameon.OnSendMessage := OnSendMessage;
        ListenDameon.OnErrorMessage := OnErrorMessage;
        ListenDameon.OnConnectionStateChange := OnConnectionStateChange;
        ListenDameon.OnBeforeDestroyTask := OnBeforeDestroyTask;
        ListenDameon.OnStatus := OnStatus;
        ListenDameon.OnOPStackCallback := OnOPStackCallback;
        ListenDameon.EnableReceiveMessages := EnableReceiveMessages;
        ListenDameon.EnableSendMessages := EnableSendMessages;
        ListenDameon.EnableOPStackCallback := EnableOPStackCallback;
        ListenDameon.Suspended := False;
      end else
      begin
        ListenDameon.Terminate;
        while not ListenDameon.TerminateComplete do
          Application.ProcessMessages;         // Yuck, but Syncronize needs the main thread to pump messages to complete and not deadlock
        FreeAndNil(FListenDameon);             // Can't free on terminate because it may be gone when we check for IsTerminated....
        ClientThreadList.Clear;
      end;
    end else
    begin
      if FEnabled then
      begin
        FSingletonClient := TClientSocketThread.Create(True, False);            // Use the IP and Port to create the socket
        SingletonClient.OwnerHub := Self;
        SingletonClient.OnReceiveMessage := OnReceiveMessage;
        SingletonClient.OnSendMessage := OnSendMessage;
        SingletonClient.OnErrorMessage := OnErrorMessage;
        Singletonclient.OnConnectionStateChange := OnConnectionStateChange;
        SingletonClient.OnStatus := OnStatus;
        SingletonClient.OnOPStackCallback := OnOPStackCallback;
        SingletonClient.EnableReceiveMessages := EnableReceiveMessages;
        SingletonClient.EnableSendMessages := EnableSendMessages;
        SingletonClient.OnBeforeDestroyTask := OnBeforeDestroyTask;
        SingletonClient.EnableOPStackCallback := EnableOPStackCallback;
        SingletonClient.Suspended := False;
      end else
      begin
        SingletonClient.Terminate;
        while not SingletonClient.TerminateComplete do
          Application.ProcessMessages;
      end
    end;
  end;
end;

procedure TEthernetHub.SetEnableReceiveMessages(AValue: Boolean);
var
  List: TList;
  i: Integer;
begin
  if FEnableReceiveMessages=AValue then Exit;
  FEnableReceiveMessages:=AValue;
  List := ClientThreadList.LockList;
  try
    for i := 0 to List.Count - 1 do
      TClientSocketThread( List[i]).EnableReceiveMessages := AValue;
  finally
    ClientThreadList.UnlockList;
  end;
end;

procedure TEthernetHub.SetEnableSendMessages(AValue: Boolean);
var
  List: TList;
  i: Integer;
begin
  if FEnableSendMessages=AValue then Exit;
  FEnableSendMessages:=AValue;
  List := ClientThreadList.LockList;
  try
    for i := 0 to List.Count - 1 do
      TClientSocketThread( List[i]).EnableSendMessages := AValue;
  finally
    ClientThreadList.UnlockList;
  end;
end;

procedure TEthernetHub.SetListener(AValue: Boolean);
var
  WasEnabled: Boolean;
begin
  if FListener=AValue then Exit;
  WasEnabled := Enabled;
  Enabled := False;
  FListener:=AValue;
  Enabled := WasEnabled;
end;

constructor TEthernetHub.Create;
begin
  EnableReceiveMessages := False;
  EnableSendMessages := False;
  EnableOPStackCallback := False;
  FOnErrorMessage := nil;
  FOnReceiveMessage := nil;
  FOnSendMessage := nil;
  FOnBeforeDestroyTask := nil;
  FOnStatus := nil;
  FClientThreadList := TSocketThreadList.Create;
  FMessageManager := TTCPMessageManager.Create;
  FSingletonClient := nil;
  FOnConnectionStateChange := nil;
end;

destructor TEthernetHub.Destroy;
begin
  Enabled := False;                      // Destroy the thread, this ensures that no more client thread will be added to ClientThreadList
  FreeAndNil(FClientThreadList);
  // Possible issue here with Client threads silently quitting in the background, did set Hub to nil but they may be in a place where it is waiting to do something with Hub....
  FreeAndNil(FMessageManager);
  inherited Destroy;
end;

procedure TEthernetHub.InternalAdd(Msg: AnsiString);
var
  List: TList;
  i: Integer;
begin
  List := ClientThreadList.LockList;
  try
    for i := 0 to List.Count - 1 do
      TClientSocketThread( List[i]).InternalAdd(Msg);
  finally
    ClientThreadList.UnlockList;
  end;
end;

procedure TEthernetHub.InternalAddDatagramToSendByCANParsing(Datagram: TCANFrameParserDatagramSend);
var
  List: TList;
  i: Integer;
begin
  List := ClientThreadList.LockList;
  try
    for i := 0 to List.Count - 1 do
      TClientSocketThread( List[i]).InternalAddDatagramToSendByCANParsing(Datagram);
  finally
    ClientThreadList.UnlockList;
  end;
end;

function TEthernetHub.AddGridConnectStr(GridConnectStr: ansistring): Boolean;
var
  List: TList;
  i: Integer;
  Done: Boolean;
begin
  Done := False;
  List := ClientThreadList.LockList;
  try
    for i := 0 to List.Count - 1 do
      TClientSocketThread( List[i]).InternalAdd(GridConnectStr);
  finally
    ClientThreadList.UnlockList;
    Result := Done;
  end;
end;

function TEthernetHub.AddTask(NewTask: TTaskOlcbBase): Boolean;
var
  List: TList;
  i: Integer;
  Done: Boolean;
begin
  Done := False;
  List := ClientThreadList.LockList;
  try
    if NewTask.DestinationAlias = 0 then
    begin
      for i := 0 to List.Count - 1 do
        TClientSocketThread( List[i]).AddTask(NewTask, True);   // Broadcast, Task will be cloned in thread
      Done := True;
    end else
    begin
      i := 0;
      while (i < List.Count) and not Done do
      begin
        Done := TClientSocketThread( List[i]).AddTask(NewTask, True); // Task will be cloned in thread
        Inc(i);
      end;
    end;
  finally
    ClientThreadList.UnlockList;
    Result := Done;
  end;
end;

procedure TEthernetHub.RemoveAndFreeTasks(RemoveKey: PtrInt);
var
  List: TList;
  i: Integer;
begin
  List := ClientThreadList.LockList;
  try
    for i := 0 to List.Count - 1 do
      TClientSocketThread( List[i]).RemoveAndFreeTasks(RemoveKey);   // UNKNWON IF THIS IS CORRECT >>>>>>>>>>>>>>>>>>
  finally
    ClientThreadList.UnlockList;
  end;
end;

end.

