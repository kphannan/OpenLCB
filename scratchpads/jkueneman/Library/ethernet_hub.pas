unit ethernet_hub;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synsock, Forms, olcb_app_common_settings, Dialogs,
  common_utilities, olcb_utilities, olcb_transport_layer, olcb_defines, strutils;

type
  TSocketThread = class;
  TEthernetHub = class;
  TSocketThreadList = class;
  TEthernetListenDameonThread = class;

  { TSocketThreadList }
  TSocketThreadList = class(TThreadList)      // Contains TClientSocketThread objects
  private
    function GetCount: Integer;
  public
    destructor Destroy; override;
    procedure TerminateThreads;
    procedure TerminateAndWaitForThreads;

    property Count: Integer read GetCount;
  end;

  { TSocketThread }

  TSocketThread = class(TTransportLayerThread)
  private
    FhSocketLocal: TSocket;
    FListenDameon: TEthernetListenDameonThread;                                 // Used if the Thread is created by a Listen Dameon
    FMaxLoopTime: DWord;
    FUseSocketHandle: Boolean;
  protected
    Socket: TTCPBlockSocket;                                                    // thread created Socket
    procedure Execute; override;
    procedure ExecuteBegin; override;
    procedure ExecuteEnd; override;
    procedure SendMessage(AMessage: AnsiString); override;
    property hSocketLocal: TSocket read FhSocketLocal write FhSocketLocal;
    property ListenDameon: TEthernetListenDameonThread read FListenDameon write FListenDameon;
    property UseSocketHandle: Boolean read FUseSocketHandle write FUseSocketHandle;            // If true the socket will use the handle in hSocketLocal to create the connection, else it will use the GlobalSettings for the Listen and Client Ports
  public
    constructor Create(CreateSuspended: Boolean; UseSocketLocalParemeter: Boolean; AListenDameon: TEthernetListenDameonThread; ANodeThread: TNodeThread); reintroduce; virtual;
    destructor Destroy; override;
    property MaxLoopTime: DWord read FMaxLoopTime write FMaxLoopTIme;
  end;

  { TEthernetListenDameonThread }

  TEthernetListenDameonThread = class(TTransportLayerThread)
  private
    FOwnerHub: TEthernetHub;
    FListeningSocket: TTCPBlockSocket;
    FSocketThreadList: TSocketThreadList;
  protected
    procedure Execute; override;
    property OwnerHub: TEthernetHub read FOwnerHub write FOwnerHub;
    property ListeningSocket: TTCPBlockSocket read FListeningSocket write FListeningSocket;     // Caution, created in context of thread
    property SocketThreadList: TSocketThreadList read FSocketThreadList write FSocketThreadList;
  public
    constructor Create(CreateSuspended: Boolean; ANodeThread: TNodeThread; const StackSize: SizeUInt = DefaultStackSize); reintroduce;
    destructor Destroy; override;
  end;

  { TEthernetHub }

  TEthernetHub = class
  private
    FBufferRawMessage: string;
    FListener: Boolean;
    FNodeThread: TNodeThread;
    FEnabled: Boolean;
    FListenDameon: TEthernetListenDameonThread;
    FOnStatus: THookSocketStatus;
    FSingletonSocket: TSocketThread;
    FOnConnectionStateChange: TOnConnectionStateChangeFunc;
    FOnErrorMessage: TOnRawMessageFunc;
    procedure SetEnabled(AValue: Boolean);
    procedure SetListener(AValue: Boolean);
    procedure SetNodeThread(AValue: TNodeThread);
  protected
    property BufferRawMessage: string read FBufferRawMessage write FBufferRawMessage;
    property ListenDameon: TEthernetListenDameonThread read FListenDameon;
    property SingletonSocket: TSocketThread read FSingletonSocket write FSingletonSocket;
  public
    constructor Create(ANodeThread: TNodeThread);
    destructor Destroy; override;

    property Enabled: Boolean read FEnabled write SetEnabled;
    property Listener: Boolean read FListener write SetListener;
    property NodeThread: TNodeThread read FNodeThread write SetNodeThread;
  published
    property OnStatus: THookSocketStatus read FOnStatus write FOnStatus;
    property OnErrorMessage: TOnRawMessageFunc read FOnErrorMessage write FOnErrorMessage;
    property OnConnectionStateChange: TOnConnectionStateChangeFunc read FOnConnectionStateChange write FOnConnectionStateChange;
  end;


implementation

{ TSocketThread }

procedure TSocketThread.Execute;
var
  Helper: TOpenLCBMessageHelper;
  DoClose: Boolean;
  RcvStr: AnsiString;
  i: Integer;
  GridConnectStrPtr: PGridConnectString;
begin
  Helper := nil;
  Socket := nil;

  // Startup, this sends the csConnecting state which will add the object to the Hub List of Clients
  ExecuteBegin;
  try
    // Setup the socket and try to create it
    Socket := TTCPBlockSocket.Create;
    Socket.OnStatus := OnStatus;
    Socket.ConvertLineEnd := True;      // User #10, #13, or both to be a "string"
    Socket.SetLinger(False, 0);
    Socket.SetSendTimeout(0);
    Socket.SetRecvTimeout(0);
    Socket.SetTimeout(0);
    try
      // Create helpers
      Helper := TOpenLCBMessageHelper.Create;

      // The socket can be created from an existing handle from a remote IP:Port or from a direct connect
      if UseSocketHandle then
      begin
        Socket.Socket := hSocketLocal;
        if Socket.LastError = 0 then
        begin
          Socket.GetSins;                     // Back load the IP's / Ports information from the handle
          if Socket.LastError = 0 then
            ConnectionState := csConnected
        end
      end else
      begin
        Socket.Bind(GlobalSettings.Ethernet.LocalIP, IntToStr(GlobalSettings.Ethernet.ClientPort));
        if Socket.LastError = 0 then
        begin
          Socket.Connect(GlobalSettings.Ethernet.LocalIP, IntToStr(GlobalSettings.Ethernet.ListenPort));
          if Socket.LastError = 0 then
            ConnectionState := csConnected;
        end
      end;

      // If error show the message and terminate
      if Socket.LastError <> 0 then
      begin
        if UseSocketHandle then
          ShowErrorMessageAndTerminate('Unable to connect to IP: ' + GlobalSettings.Ethernet.LocalIP + ':' + IntToStr(GlobalSettings.Ethernet.ListenPort) + #13 + #10 + Socket.LastErrorDesc)
        else
          ShowErrorMessageAndTerminate('Unable to connect Local IP: ' + GlobalSettings.Ethernet.LocalIP + ':' + IntToStr(GlobalSettings.Ethernet.ClientPort) + #13 + #10
                                        + 'to Remote IP: ' + GlobalSettings.Ethernet.RemoteIP + ':' + IntToStr(GlobalSettings.Ethernet.ListenPort) + #13 + #10 + Socket.LastErrorDesc)
      end;

      // This will either be csConnected or csConnecting if it failed
      Synchronize(@DoConnectionState);

      while not Terminated and (ConnectionState = csConnected) do
      begin
        ThreadSwitch;

        if not Terminated then
        begin
          RcvStr := Socket.RecvPacket(1);
          case Socket.LastError of
            S_OK            : begin
                                for i := 1 to Length(RcvStr) do
                                begin
                                  if GridConnect_DecodeMachine(RcvStr[i], GridConnectStrPtr) then
                                    DecomposeAndDispatchGridConnectString(GridConnectStrPtr, Helper);
                                 end;
                               end;
            WSAETIMEDOUT    : begin end;                  // Normal if nothing to read
            WSAECONNRESET,
            WSAECONNABORTED : begin Terminate end;     // This is normal if the other app is shut down so don't waste time with errors and shutting us down
          else
            ShowErrorMessageAndTerminate('Socket Error: ' + Socket.LastErrorDesc)
          end;
        end;
      end;
    finally

      // Signal we are disconneting to the UI, will remove the thread from the Hub List
      DoClose := ConnectionState = csConnected;
      ConnectionState := csDisconnecting;
        Synchronize(@DoConnectionState);

      // Close the socket if it was open
      if DoClose then
        Socket.CloseSocket;

      // Clean up
      FreeAndNil(Helper);
    end;
  finally
    // Cleanup
    FreeAndNil(Socket);
    // Will send csDisconnected state
    ExecuteEnd;
  end
end;

procedure TSocketThread.ExecuteBegin;
begin
  inherited ExecuteBegin;
  if Assigned(NodeThread) then
    NodeThread.RegisterThread(Self);
end;

procedure TSocketThread.ExecuteEnd;
begin
  if Assigned(NodeThread) then
    NodeThread.UnRegisterThread(Self);
  inherited ExecuteEnd;
end;

procedure TSocketThread.SendMessage(AMessage: AnsiString);
begin
  Socket.SendString(AMessage);   // Is this ok to send from a different thread???
end;

constructor TSocketThread.Create(CreateSuspended: Boolean;
  UseSocketLocalParemeter: Boolean; AListenDameon: TEthernetListenDameonThread;
  ANodeThread: TNodeThread);
begin
  inherited Create(True, ANodeThread);
  UseSocketHandle := UseSocketLocalParemeter;
  FListenDameon := AListenDameon;
  if Assigned(ListenDameon) then
    ListenDameon.SocketThreadList.Add(Self);
  FMaxLoopTime := 0;
end;

destructor TSocketThread.Destroy;
begin
  if Assigned(ListenDameon) then
    ListenDameon.SocketThreadList.Remove(Self);
  if Assigned(NodeThread) then
    NodeThread.UnRegisterThread(Self);
  inherited Destroy;
end;

{ TSocketThreadList }

destructor TSocketThreadList.Destroy;
begin
  TerminateThreads;
  inherited Destroy;
end;

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

procedure TSocketThreadList.TerminateAndWaitForThreads;
var
  i: Integer;
  L: TList;
  SocketThread: TSocketThread;
begin
  L := LockList;
  try
    for i := L.Count - 1 downto 0 do         // Need to do this backwards because when they are freed they will remove themselves from this list
    begin;
      SocketThread := TSocketThread( L[i]);
      SocketThread.Terminate;
      while not SocketThread.TerminateComplete do
      begin
        Application.ProcessMessages;
      end;
      SocketThread.Free;
    end;
  finally
    L.Clear;
    UnlockList;
  end;
end;

procedure TSocketThreadList.TerminateThreads;
var
  i: Integer;
  L: TList;
  SocketThread: TSocketThread;
begin
  L := LockList;
  try
    for i := L.Count - 1 downto 0 do
    begin;
      SocketThread := TSocketThread( L[i]);
      SocketThread.Terminate;
    end;
  finally
    L.Clear;
    UnlockList;
  end;
end;

{ TEthernetListenDameonThread }

constructor TEthernetListenDameonThread.Create(CreateSuspended: Boolean;
  ANodeThread: TNodeThread; const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, ANodeThread);
  SocketThreadList := TSocketThreadList.Create;
  FOwnerHub := nil;
end;

destructor TEthernetListenDameonThread.Destroy;
begin
  SocketThreadList.TerminateAndWaitForThreads;
  inherited Destroy;
end;

procedure TEthernetListenDameonThread.Execute;
var
  hSocket: TSocket;
  NewSocketThread: TSocketThread;
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
    Synchronize(@DoConnectionState);

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
              NewSocketThread := TSocketThread.Create(True, True, Self, NodeThread);   // Use the hSocketLocal parameter to create socket
              NewSocketThread.hSocketLocal := hSocket;
              NewSocketThread.ListenDameon := Self;
              NewSocketThread.OnErrorMessage := OwnerHub.OnErrorMessage;
              NewSocketThread.OnConnectionStateChange := OwnerHub.OnConnectionStateChange;
              NewSocketThread.OnStatus := OwnerHub.OnStatus;
              NewSocketThread.Suspended := False;
            end else
              ShowErrorMessageAndTerminate('Error in connection to IP: ' + GlobalSettings.Ethernet.LocalIP + ':' + IntToStr(GlobalSettings.Ethernet.ListenPort) + #13 + #10 + ListeningSocket.LastErrorDesc);
          end;
        end else
          ShowErrorMessageAndTerminate('Error in connection to IP: ' + GlobalSettings.Ethernet.LocalIP + ':' + IntToStr(GlobalSettings.Ethernet.ListenPort) + #13 + #10 + ListeningSocket.LastErrorDesc);
      end;
    end;
  finally


    // Signal we are disconneting to the UI
    DoClose := ConnectionState = csConnected;
    ConnectionState := csDisconnecting;
      Synchronize(@DoConnectionState);

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
        FListenDameon := TEthernetListenDameonThread.Create(True, NodeThread);
        ListenDameon.OwnerHub := Self;
        ListenDameon.OnErrorMessage := OnErrorMessage;
        ListenDameon.OnConnectionStateChange := OnConnectionStateChange;
        ListenDameon.OnStatus := OnStatus;
        ListenDameon.Suspended := False;
      end else
      begin
        ListenDameon.Terminate;
        while not ListenDameon.TerminateComplete do
          Application.ProcessMessages;         // Yuck, but Syncronize needs the main thread to pump messages to complete and not deadlock
        FreeAndNil(FListenDameon);             // Can't free on terminate because it may be gone when we check for IsTerminated....
      end;
    end else
    begin
      if FEnabled then
      begin
        FSingletonSocket := TSocketThread.Create(True, False, nil, NodeThread);            // Use the IP and Port to create the socket
        SingletonSocket.OnErrorMessage := OnErrorMessage;
        SingletonSocket.OnConnectionStateChange := OnConnectionStateChange;
        SingletonSocket.OnStatus := OnStatus;
        SingletonSocket.Suspended := False;
      end else
      begin
        SingletonSocket.Terminate;
        while not SingletonSocket.TerminateComplete do
          Application.ProcessMessages;
        FreeAndNil(FSingletonSocket);
      end
    end;
  end;
end;

procedure TEthernetHub.SetListener(AValue: Boolean);
var
  WasEnabled: Boolean;
begin
  if FListener <>AValue then
  begin
    WasEnabled := Enabled;
    Enabled := False;
    FListener:=AValue;
    Enabled := WasEnabled;
  end;
end;

procedure TEthernetHub.SetNodeThread(AValue: TNodeThread);
begin
  if FNodeThread <> AValue then
    FNodeThread:=AValue;
end;

constructor TEthernetHub.Create(ANodeThread: TNodeThread);
begin
  FOnErrorMessage := nil;
  FOnStatus := nil;
  FSingletonSocket := nil;
  FOnConnectionStateChange := nil;
  FNodeThread := nil;
  NodeThread := ANodeThread;
end;

destructor TEthernetHub.Destroy;
begin
  Enabled := False;                      // Destroy the thread, this ensures that no more client thread will be added to SocketThreadList
  // Possible issue here with Client threads silently quitting in the background, did set Hub to nil but they may be in a place where it is waiting to do something with Hub....
  inherited Destroy;
end;

end.

