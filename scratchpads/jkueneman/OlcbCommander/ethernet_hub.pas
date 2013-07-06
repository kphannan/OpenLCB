unit ethernet_hub;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synsock, Forms;

type
  TConnectedSocketThread = class;
  TEthernetHub = class;

  { TSocketList }
  TSocketList = class
  private
    FList: TThreadList;
    function GetCount: Integer;
    function GetSocket(Index: Integer): TConnectedSocketThread;
    procedure SetSocket(Index: Integer; AValue: TConnectedSocketThread);
  protected
    property List: TThreadList read FList write FList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(hNewSocket: TSocket; Hub: TEthernetHub);
    procedure Remove(Socket: TConnectedSocketThread);
    procedure Clear;

    property Sockets[Index: Integer]: TConnectedSocketThread read GetSocket write SetSocket; default;
    property Count: Integer read GetCount;
  end;

  { TConnectedSocketThread }
  TConnectedSocketThread = class(TThread)
  private
    FConnectedSocket: TTCPBlockSocket;
    FhSocketLocal: TSocket;
    FOwnerHub: TEthernetHub;
  protected
    procedure Execute; override;
    property hSocketLocal: TSocket read FhSocketLocal write FhSocketLocal;
    property ConnectedSocket: TTCPBlockSocket read FConnectedSocket;
  public
    constructor Create(hSocket: TSocket);
    destructor Destroy; override;
    property OwnerHub: TEthernetHub read FOwnerHub write FOwnerHub;
  end;


  { TEthernetHubThread }

  TEthernetHubThread = class(TThread)
  private
    FIsTerminated: Boolean;
    FOwnerHub: TEthernetHub;
    FSocket: TTCPBlockSocket;
  protected
    procedure Execute; override;
    property OwnerHub: TEthernetHub read FOwnerHub write FOwnerHub;
    property Socket: TTCPBlockSocket read FSocket write FSocket;     // Caution, created in context of thread
  public
    constructor Create;
    destructor Destroy; override;
    property IsTerminated: Boolean read FIsTerminated;
  end;

  TConnectionCallbackFunc = procedure(HostIP: string; HostPort: Integer) of object;
  TConnectionChangeCallbackFunc = procedure(SocketCount: Integer) of object;

  { TEthernetHub }
  TEthernetHub = class
  private
    FOnConnectedCallback: TConnectionCallbackFunc;
    FOnDisconnectedCallback: TConnectionCallbackFunc;
    FEnabled: Boolean;
    FListenDameon: TEthernetHubThread;
    FOnDroppedConnection: TConnectionChangeCallbackFunc;
    FOnNewConnection: TConnectionChangeCallbackFunc;
    FSocketList: TSocketList;
    procedure SetEnabled(AValue: Boolean);
  protected
    procedure LocalConnectedCallback;
    procedure LocalDisconnectedCallback;
    procedure LocalNewConnectionCallback;
    procedure LocalDroppedConnectionCallback;
    property ListenDameon: TEthernetHubThread read FListenDameon;
  public
    constructor Create;
    destructor Destroy; override;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property OnConnectionConnected: TConnectionCallbackFunc read FOnConnectedCallback write FOnConnectedCallback;
    property OnConnectionDisconnected: TConnectionCallbackFunc read FOnDisconnectedCallback write FOnDisconnectedCallback;
    property OnNewConnection: TConnectionChangeCallbackFunc read FOnNewConnection write FOnNewConnection;
    property OnDroppedConnection: TConnectionChangeCallbackFunc read FOnDroppedConnection write FOnDroppedConnection;
    property SocketList: TSocketList read FSocketList write FSocketList;
  end;

implementation

{ TConnectedSocketThread }

procedure TConnectedSocketThread.Execute;
var
  ReceivedData: AnsiString;
begin
  FConnectedSocket := TTCPBlockSocket.Create;
  try
    ConnectedSocket.Socket := hSocketLocal;
    ConnectedSocket.GetSins;                     // Back load the IP's / Ports information from the handle
    while not Terminated do
    begin
       ReceivedData := ConnectedSocket.RecvPacket(1000);

       if not ConnectedSocket.CanRead(0) and (ConnectedSocket.WaitingData = 0) then
       begin
         OwnerHub.SocketList.Remove(Self);
         if Assigned(OwnerHub) then
           Synchronize(@OwnerHub.LocalDroppedConnectionCallback);
         OwnerHub := nil;      // Unlink as we may go away soon
         Terminate;
       end;
    end;
  finally
    ConnectedSocket.Free
  end;
end;

constructor TConnectedSocketThread.Create(hSocket: TSocket);
begin
  inherited Create(True);
  FhSocketLocal := hSocket;
  FOwnerHub := nil;
  FConnectedSocket := nil;
end;

destructor TConnectedSocketThread.Destroy;
begin
  inherited Destroy;
end;

{ TSocketList }

function TSocketList.GetSocket(Index: Integer): TConnectedSocketThread;
var
  L: TList;
begin
  L := List.LockList;
  try
    Result := TConnectedSocketThread( L[Index]);
  finally
    List.UnlockList;
  end;
end;

function TSocketList.GetCount: Integer;
var
  L: TList;
begin
  L := List.LockList;
  try
    Result := L.Count
  finally
    List.UnlockList;
  end;
end;

procedure TSocketList.SetSocket(Index: Integer; AValue: TConnectedSocketThread);
var
  L: TList;
begin
  L := List.LockList;
  try
    L[Index] := AValue
  finally
    List.UnlockList;
  end;
end;

constructor TSocketList.Create;
begin
  FList := TThreadList.Create;
end;

destructor TSocketList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TSocketList.Add(hNewSocket: TSocket; Hub: TEthernetHub);
var
  ConnectedSocketThread: TConnectedSocketThread;
begin
  if hNewSocket <> 0 then
  begin
    ConnectedSocketThread := TConnectedSocketThread.Create(hNewSocket);
    ConnectedSocketThread.OwnerHub := Hub;
    ConnectedSocketThread.FreeOnTerminate := True;
    List.Add(ConnectedSocketThread);
    ConnectedSocketThread.Suspended := False;
  end;
end;

procedure TSocketList.Remove(Socket: TConnectedSocketThread);
var
  L: TList;
begin
  if Assigned(Socket) then
  begin
    L := List.LockList;
    try
      L.Extract(Socket);
    finally
      List.UnlockList;
    end;
  end;
end;

procedure TSocketList.Clear;
var
  i: Integer;
  L: TList;
  SocketThread: TConnectedSocketThread;
begin
  L := List.LockList;
  try
    for i := 0 to L.Count - 1 do
    begin;
      SocketThread := Sockets[i];
      Remove(SocketThread);
      SocketThread.OwnerHub := nil;
      SocketThread.Terminate;
    end;
  finally
    List.UnlockList;
  end;
end;

{ TEthernetHubThread }

constructor TEthernetHubThread.Create;
begin
  inherited Create(True);
  FOwnerHub := nil;
  FIsTerminated := False;
end;

destructor TEthernetHubThread.Destroy;
begin
  inherited Destroy;
end;

procedure TEthernetHubThread.Execute;
var
  hSocket: TSocket;
begin
  Socket := TTCPBlockSocket.Create;
  Socket.Bind('0.0.0.0', '12021');  // Bind to this machine IP at Port 12021;
  Socket.SetLinger(True, 1000);
  Socket.Listen;                    // Listen for connections

  if Assigned(OwnerHub) then
    Synchronize(@OwnerHub.LocalConnectedCallback);

  while not Terminated do
  begin
    if Socket.CanRead(1000) then
    begin
      if Socket.LastError = 0 then
      begin
        hSocket := Socket.Accept;        // Get the handle of the new socket for the client connection
        if Socket.LastError = 0 then
        begin
          OwnerHub.SocketList.Add(hSocket, OwnerHub);       // add it to the list
          if Assigned(OwnerHub) then
            Synchronize(@OwnerHub.LocalNewConnectionCallback);
        end;
      end;
    end;
  end;
  if Assigned(OwnerHub) then
    Synchronize(@OwnerHub.LocalDisconnectedCallback);

  FIsTerminated := True;
end;

{ TEthernetHub }

procedure TEthernetHub.SetEnabled(AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled:=AValue;
    if FEnabled then
    begin
      FListenDameon := TEthernetHubThread.Create;
      ListenDameon.FreeOnTerminate := True;
      ListenDameon.OwnerHub := Self;
      ListenDameon.Suspended := False;
    end else
    begin
      ListenDameon.Terminate;
      while not ListenDameon.IsTerminated do
      begin
        Application.ProcessMessages;   // The Syncronize call needs to pump message to complete and not deadlock
        Sleep(100);
      end;
      FListenDameon := nil;
      SocketList.Clear;
      LocalDroppedConnectionCallback;
    end;
  end;
end;

procedure TEthernetHub.LocalConnectedCallback;
begin
  if Assigned(OnConnectionConnected) then
    OnConnectionConnected(ListenDameon.Socket.GetLocalSinIP, ListenDameon.Socket.GetLocalSinPort);
end;

procedure TEthernetHub.LocalDisconnectedCallback;
begin
  if Assigned(OnConnectionDisconnected) then
    OnConnectionDisconnected(ListenDameon.Socket.GetLocalSinIP, ListenDameon.Socket.GetLocalSinPort);
end;

procedure TEthernetHub.LocalNewConnectionCallback;
begin
  if Assigned(OnNewConnection) then
    OnNewConnection(SocketList.Count);
end;

procedure TEthernetHub.LocalDroppedConnectionCallback;
begin
  if Assigned(OnDroppedConnection) then
    OnDroppedConnection(SocketList.Count);
end;

constructor TEthernetHub.Create;
begin
  FListenDameon := nil;
  FOnNewConnection := nil;
  FOnConnectedCallback := nil;
  FOnDisconnectedCallback := nil;
  FOnDroppedConnection := nil;
  FSocketList := TSocketList.Create;
end;

destructor TEthernetHub.Destroy;
begin
  Enabled := False;
  FreeAndNil(FSocketList);
  inherited Destroy;
end;

end.

