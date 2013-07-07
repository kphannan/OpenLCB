unit ethernet_hub;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synsock, Forms, olcb_app_common_settings, Dialogs;

type
  TClientSocketThread = class;
  TEthernetHub = class;

  { TSocketThreadList }
  TSocketThreadList = class
  private
    FList: TThreadList;
    function GetCount: Integer;
    function GetSocket(Index: Integer): TClientSocketThread;
    procedure SetSocket(Index: Integer; AValue: TClientSocketThread);
  protected
    property List: TThreadList read FList write FList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(hNewSocket: TSocket; Hub: TEthernetHub);
    procedure Remove(Socket: TClientSocketThread);
    procedure Clear;

    property Sockets[Index: Integer]: TClientSocketThread read GetSocket write SetSocket; default;
    property Count: Integer read GetCount;
  end;

  { TClientSocketThread }
  TClientSocketThread = class(TThread)
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
    FListeningSocket: TTCPBlockSocket;
  protected
    procedure Execute; override;
    property OwnerHub: TEthernetHub read FOwnerHub write FOwnerHub;
    property ListeningSocket: TTCPBlockSocket read FListeningSocket write FListeningSocket;     // Caution, created in context of thread
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
    property IsTerminated: Boolean read FIsTerminated;
  end;

  TOnHubConnectFunc = procedure(HostIP: string; HostPort: Integer) of object;
  TOnClientConnectChangeFunc = procedure(SocketCount: Integer) of object;

  { TEthernetHub }
  TEthernetHub = class
  private
    FOnHubConnect: TOnHubConnectFunc;
    FOnHubDisconnect: TOnHubConnectFunc;
    FEnabled: Boolean;
    FListenDameon: TEthernetHubThread;
    FOnClientDisconnect: TOnClientConnectChangeFunc;
    FOnClientConnect: TOnClientConnectChangeFunc;
    FClientThreadList: TSocketThreadList;
    procedure SetEnabled(AValue: Boolean);
  protected
    procedure LocalOnHubConnect;
    procedure LocalOnHubDisconnect;
    procedure LocalOnClientConnect;
    procedure LocalOnClientDisconnect;
    property ListenDameon: TEthernetHubThread read FListenDameon;
  public
    constructor Create;
    destructor Destroy; override;
    property ClientThreadList: TSocketThreadList read FClientThreadList write FClientThreadList;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property OnHubConnect: TOnHubConnectFunc read FOnHubConnect write FOnHubConnect;
    property OnHubDisconnect: TOnHubConnectFunc read FOnHubDisconnect write FOnHubDisconnect;
    property OnClientClientConnect: TOnClientConnectChangeFunc read FOnClientConnect write FOnClientConnect;
    property OnClientDisconnect: TOnClientConnectChangeFunc read FOnClientDisconnect write FOnClientDisconnect;

  end;


implementation

{ TClientSocketThread }

procedure TClientSocketThread.Execute;
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

 {      if not ConnectedSocket.CanRead(0) and (ConnectedSocket.WaitingData = 0)  and Assigned(OwnerHub) then
       begin
         OwnerHub.ClientThreadList.Remove(Self);
         Synchronize(@OwnerHub.LocalOnClientDisconnect);
         OwnerHub := nil;      // Unlink as we may go away soon
         Terminate;
       end;         }
    end;
  finally
    ConnectedSocket.Free
  end;
end;

constructor TClientSocketThread.Create(hSocket: TSocket);
begin
  inherited Create(True);
  FhSocketLocal := hSocket;
  FOwnerHub := nil;
  FConnectedSocket := nil;
end;

destructor TClientSocketThread.Destroy;
begin
  inherited Destroy;
end;

{ TSocketThreadList }

function TSocketThreadList.GetSocket(Index: Integer): TClientSocketThread;
var
  L: TList;
begin
  L := List.LockList;
  try
    Result := TClientSocketThread( L[Index]);
  finally
    List.UnlockList;
  end;
end;

function TSocketThreadList.GetCount: Integer;
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

procedure TSocketThreadList.SetSocket(Index: Integer; AValue: TClientSocketThread);
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

constructor TSocketThreadList.Create;
begin
  FList := TThreadList.Create;
end;

destructor TSocketThreadList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TSocketThreadList.Add(hNewSocket: TSocket; Hub: TEthernetHub);
var
  ConnectedSocketThread: TClientSocketThread;
begin
  if hNewSocket <> 0 then
  begin
    ConnectedSocketThread := TClientSocketThread.Create(hNewSocket);
    ConnectedSocketThread.OwnerHub := Hub;
    ConnectedSocketThread.FreeOnTerminate := True;
    List.Add(ConnectedSocketThread);
    ConnectedSocketThread.Suspended := False;
  end;
end;

procedure TSocketThreadList.Remove(Socket: TClientSocketThread);
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

procedure TSocketThreadList.Clear;
var
  i: Integer;
  L: TList;
  SocketThread: TClientSocketThread;
begin
  L := List.LockList;
  try
    for i := 0 to L.Count - 1 do
    begin;
      SocketThread := Sockets[i];
      SocketThread.OwnerHub := nil;
      SocketThread.Terminate;
    end;
  finally
    List.Clear;
    List.UnlockList;
  end;
end;

{ TEthernetHubThread }

constructor TEthernetHubThread.Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);
begin
  inherited Create(CreateSuspended, StackSize);
  FOwnerHub := nil;
end;

destructor TEthernetHubThread.Destroy;
begin
  inherited Destroy;
end;

procedure TEthernetHubThread.Execute;
var
  hSocket: TSocket;
begin
  ListeningSocket := TTCPBlockSocket.Create;
  ListeningSocket.CreateSocket;
  ListeningSocket.Bind(GlobalSettings.Ethernet.LocalIP, IntToStr(GlobalSettings.Ethernet.LocalPort));  // Bind to this machine IP at Port 12021;
  ListeningSocket.SetLinger(True, 1000);
  ListeningSocket.Listen;                    // Listen for connections

  if Assigned(OwnerHub) and not Terminated then
    Synchronize(@OwnerHub.LocalOnHubConnect);

  while not Terminated do
  begin
    if ListeningSocket.CanRead(1000) then
    begin
      if (ListeningSocket.LastError = 0) and Assigned(OwnerHub) and not Terminated then
      begin
        hSocket := ListeningSocket.Accept;        // Get the handle of the new ListeningSocket for the client connection
        if ListeningSocket.LastError = 0 then
        begin
          OwnerHub.ClientThreadList.Add(hSocket, OwnerHub);       // add it to the list
          if Assigned(OwnerHub) and not Terminated then
            Synchronize(@OwnerHub.LocalOnClientConnect);
        end;
      end;
    end;
  end;
  FreeAndNil(FListeningSocket);
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
      FListenDameon := TEthernetHubThread.Create(True);
      ListenDameon.OwnerHub := Self;
      ListenDameon.Suspended := False;
    end else
    begin
      LocalOnHubDisconnect;
      ListenDameon.Terminate;
      while not ListenDameon.IsTerminated do
        Application.ProcessMessages;         // Yuck, but Syncronize needs the main thread to pump messages to complete and not deadlock
      FreeAndNil(FListenDameon);             // Can't free on terminate because it may be gone when we check for IsTerminated....
      ClientThreadList.Clear;
    end;
  end;
end;

procedure TEthernetHub.LocalOnHubConnect;
begin
  if Assigned(OnHubConnect) and Assigned(ListenDameon) then
    OnHubConnect(ListenDameon.ListeningSocket.GetLocalSinIP, ListenDameon.ListeningSocket.GetLocalSinPort);
end;

procedure TEthernetHub.LocalOnHubDisconnect;
begin
  if Assigned(OnHubDisconnect) and Assigned(ListenDameon) then
    OnHubDisconnect(ListenDameon.ListeningSocket.GetLocalSinIP, ListenDameon.ListeningSocket.GetLocalSinPort);
end;

procedure TEthernetHub.LocalOnClientConnect;
begin
  if Assigned(OnClientClientConnect) and Assigned(ClientThreadList) then
    OnClientClientConnect(ClientThreadList.Count);
end;

procedure TEthernetHub.LocalOnClientDisconnect;
begin
  if Assigned(OnClientDisconnect) and Assigned(ClientThreadList) then
    OnClientDisconnect(ClientThreadList.Count);
end;

constructor TEthernetHub.Create;
begin
  FOnClientConnect := nil;
  FOnHubConnect := nil;
  FOnHubDisconnect := nil;
  FOnClientDisconnect := nil;
  FClientThreadList := TSocketThreadList.Create;
end;

destructor TEthernetHub.Destroy;
begin
  Enabled := False;                      // Destroy the thread
  FreeAndNil(FClientThreadList);
  inherited Destroy;
end;

end.

