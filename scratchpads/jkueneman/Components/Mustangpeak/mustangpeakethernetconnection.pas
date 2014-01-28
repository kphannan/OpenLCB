unit mustangpeakethernetconnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  blcksock, synsock, LCLIntf, LCLType, LMessages, threadedstringlist,
  ExtCtrls, nodeidresolutionprotocol;

const
  LM_ETHERNET_RECEIVE      = LM_USER + 123;
  LM_ETHERNET_SOCKET_EVENT = LM_USER + 124;

const
  EVENT_AFTER_CONNECT = 0;
  EVENT_CREATE_SOCKET = 1;
  EVENT_READ_FILTER   = 2;
  EVENT_STATUS        = 3;
  EVENT_HEARTBEAT     = 4;
  EVENT_MONITOR       = 5;

const
  GRIDCONNECT_STATE_SYNC_START = 0;
  GRIDCONNECT_STATE_SYNC_FIND_X = 1;
  GRIDCONNECT_STATE_SYNC_FIND_HEADER = 2;
  GRIDCONNECT_STATE_SYNC_FIND_DATA = 4;

  RAWETHERNET_STATE_SYNC_START = 0;
  RAWETHERNET_STATE_SYNC_FIND_DATA = 1;

const
  // :X19170640N0501010107015555;#0  Example.....
  // ^         ^                  ^
  // 0         10                28
  MAX_GRID_CONNECT_LEN = 29;
  GRID_CONNECT_HEADER_OFFSET_HI = 2;
  GRID_CONNECT_HEADER_OFFSET_LO = 4;
  GRID_CONNECT_DATA_OFFSET = 11;

type
  TGridConnectString = array[0..MAX_GRID_CONNECT_LEN-1] of char;
  PGridConnectString = ^TGridConnectString;


type
  TSocketEvent = (se_AfterConnect, se_CreateSocket, se_ReadFilter, se_Status, se_HeartBeat, se_Monitor);
  TSocketEvents = set of TSocketEvent;

type
  TSocketEventBuffer = record
    ClassName,
    Local,
    Remote: ansistring;              // IP and Port of connection
    EventID: Integer;                // See EVENT_xxxx constants
    Sender: TObject;
    Value: ansistring;
    Reason: THookSocketReason;
    Writing: Boolean;
    Buffer: TMemory;
    Len: Integer;
  end;
  PSocketEventBuffer = ^TSocketEventBuffer;

type
  TMustangpeakEthernetConnection = class;
  TMustangpeakEthernetThread = class;
  TMustangpeakEthernetConnector = class;

  TMustangpeakEthernetConnectionLink = record
    ReceiveThread    : TMustangpeakEthernetThread;
    TransmitThread   : TMustangpeakEthernetThread;
    NrpList: TNrpList;
  end;
  PMustangpeakEthernetConnectionLink = ^TMustangpeakEthernetConnectionLink;

  TSocketType = (st_None, st_Listen, st_Connect);

  { TMustangpeakEthernetThread }

  TMustangpeakEthernetThread = class(TThread)       // This is a base class for a thread that can either read or write to a Socket
  private
    FConnector: TMustangpeakEthernetConnector;
    FEvent: PRTLEvent;
    FGridConnect: Boolean;
    FSocketEventList: TThreadList;
    FSocketEvents: TSocketEvents;
    FFinishedRunning: Boolean;
    FHeartbeatRate: Integer;
    FhSocket: TSocket;
    FIncomingPackets: TThreadStringList;
    FIpAddressLocal: string;
    FIpAddressTarget: string;
    FOutgoingPackets: TThreadStringList;
    FPortLocal: Word;
    FPortTarget: Word;
    FSocket: TTCPBlockSocket;
    FSocketType: TSocketType;
  protected
    property FinishedRunning: Boolean read FFinishedRunning write FFinishedRunning;
    property HeartbeatRate: Integer read FHeartbeatRate write FHeartbeatRate;
    property Socket: TTCPBlockSocket read FSocket write FSocket;

    procedure AddSocketEvent(NewEvent: PSocketEventBuffer);
    procedure Execute; override;
    procedure EthernetBeforeExecute; virtual;
    procedure EthernetAfterExecute; virtual;
    procedure EthernetAction; virtual; abstract;
    procedure LoadSocketEvents;
    procedure LocalOnAfterConnect(Sender: TObject);
    procedure LocalOnCreateSocket(Sender: TObject);
    procedure LocalOnReadFilter(Sender: TObject; var Value: AnsiString);
    procedure LocalOnStatus(Sender: TObject; Reason: THookSocketReason; const Value: String);
    procedure LocalOnHeartbeat(Sender: TObject);
    procedure LocalOnMonitor(Sender: TObject; Writing: Boolean; const Buffer: TMemory; Len: Integer);
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt=DefaultStackSize);
    destructor Destroy; override;
    function IsTerminated: Boolean;

    property Connector: TMustangpeakEthernetConnector read FConnector write FConnector;
    property Event: PRTLEvent read FEvent write FEvent;
    property GridConnect: Boolean read FGridConnect write FGridConnect;
    property SocketEventList: TThreadList read FSocketEventList write FSocketEventList;
    property SocketEvents: TSocketEvents read FSocketEvents write FSocketEvents;
    property hSocket: TSocket write FhSocket;                                    // Back create the socket to assign an existing socket handle to a Socket Object
    property IpAddressLocal: string read FIpAddressLocal write FIpAddressLocal;  // "0.0.0.0" = "localhost" or normal Ip format (i.e. "192.168.0.234")
    property IpAddressTarget: string read FIpAddressTarget write FIpAddressTarget;
    property PortLocal: Word read FPortLocal write FPortLocal;
    property PortTarget: Word read FPortTarget write FPortTarget;
    property SocketType: TSocketType read FSocketType write FSocketType;
    property IncomingPackets: TThreadStringList read FIncomingPackets write FIncomingPackets;
    property OutgoingPackets: TThreadStringList read FOutgoingPackets write FOutgoingPackets;
  end;

  { TMustangpeakEthernetRxThread }

  TMustangpeakEthernetRxThread = class(TMustangpeakEthernetThread)                            // Reads a Packet from the Socket (terminated with a #13, #10 or both
  private
    FGridConnectReceiveState: Integer;
    FRawEthernetReceiveState: Integer;
    FReceiveGridConnectBuffer: TGridConnectString;
    FReceiveGridConnectBufferIndex: Integer;
    FReceiveRawEthernetBuffer: TGridConnectString;
    FReceiveRawEthernetBufferIndex: Integer;
  protected
    property ReceiveGridConnectBufferIndex: Integer read FReceiveGridConnectBufferIndex write FReceiveGridConnectBufferIndex;
    property ReceiveGridConnectBuffer: TGridConnectString read FReceiveGridConnectBuffer write FReceiveGridConnectBuffer;
    property GridConnectReceiveState: Integer read FGridConnectReceiveState write FGridConnectReceiveState;
    property ReceiveRawEthernetBufferIndex: Integer read FReceiveRawEthernetBufferIndex write FReceiveRawEthernetBufferIndex;
    property ReceiveRawEthernetBuffer: TGridConnectString read FReceiveRawEthernetBuffer write FReceiveRawEthernetBuffer;
    property RawEthernetReceiveState: Integer read FRawEthernetReceiveState write FRawEthernetReceiveState;
    function GridConnect_DecodeMachine(NextChar: Char; var GridConnectStrPtr: PGridConnectString): Boolean;
    function RawEthernet_DecodeMachine(NextChar: Char; var Packet: string): Boolean;
    function IsValidHexChar(AChar: Char): Boolean;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt=DefaultStackSize);
    procedure EthernetAction; override;
  end;

  { TMustangpeakEthernetTxThread }

  TMustangpeakEthernetTxThread = class(TMustangpeakEthernetThread)
  protected
    procedure EthernetAction; override;
  end;

  { TMustangpeakEthernetConnector }

  TMustangpeakEthernetConnector = class(TMustangpeakEthernetThread)
  private
    FConnections: TThreadList;
    FOwnerComponent: TMustangpeakEthernetConnection;
  protected
    procedure ClearConnections;
    procedure EthernetAction; override;
    procedure CheckForSocketEventAndPostMessage;
    procedure CheckForReceivePacketAndPostMessage;
  public
    property Connections: TThreadList read FConnections write FConnections;
    property OwnerComponent: TMustangpeakEthernetConnection read FOwnerComponent write FOwnerComponent;

    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt=DefaultStackSize);
    destructor Destroy; override;
    procedure AddConnection(AConnection: PMustangpeakEthernetConnectionLink);
    function FindConnectionByRx(RxThread: TMustangpeakEthernetThread): PMustangpeakEthernetConnectionLink;
    function FindConnectionByTx(TxThread: TMustangpeakEthernetThread): PMustangpeakEthernetConnectionLink;
    function CreateNewConnectionPair(ASocketHandle: TSocket): PMustangpeakEthernetConnectionLink;
    procedure DestroyConnection(AConnection: PMustangpeakEthernetConnectionLink);
  end;

  { TMustangpeakListenerThread }

  TMustangpeakListenerThread = class(TMustangpeakEthernetThread)
  protected
    procedure EthernetAction; override;
  end;

  { TMustangpeakEthernetServerThread }

  TMustangpeakEthernetServerThread = class(TMustangpeakEthernetConnector)
  private
    FListener: TMustangpeakListenerThread;
  protected
    property Listener: TMustangpeakListenerThread read FListener write FListener;
    procedure EthernetBeforeExecute; override;
    procedure EthernetAfterExecute; override;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt=DefaultStackSize);
  end;

  { TMustangpeakEthernetClientThread }

  TMustangpeakEthernetClientThread = class(TMustangpeakEthernetConnector)
  protected
    procedure EthernetBeforeExecute; override;
    procedure EthernetAfterExecute; override;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt=DefaultStackSize);
  end;

  { TSyncronizeWindow }

  TSyncronizeWindow = class(TPanel)
  private
    FConnector: TMustangpeakEthernetConnection;
  public
    property Connector: TMustangpeakEthernetConnection read FConnector write FConnector;
    constructor Create(TheOwner: TComponent); override;
    procedure LMEthernetReceive(var Msg: TLMNoParams); message LM_ETHERNET_RECEIVE;
    procedure LMEthernetSocketEvent(var Msg: TLMNoParams); message LM_ETHERNET_SOCKET_EVENT;
  end;

  TMustangpeakEthernetConnectionType = (mec_Client, mec_Server);
  TMustangpeakEthernetOnEthenetReceiveFunc = procedure(Sender: TObject; Packet: string) of object;
  TMustangpeakEthernetOnEthenetSocketEventFunc = procedure(Sender: TObject; var Event: TSocketEventBuffer) of object;

  { TMustangpeakEthernetConnection }

  TMustangpeakEthernetConnection = class(TComponent)
  private
    FClientThread: TMustangpeakEthernetClientThread;
    FConnectionType: TMustangpeakEthernetConnectionType;
    FConnected: Boolean;
    FEvents: TSocketEvents;
    FGridConnect: Boolean;
    FHeartbeatRate: Integer;
    FHub: Boolean;
    FIpAddressLocal: string;
    FIpAddressTarget: string;
    FOnEthernetReceive: TMustangpeakEthernetOnEthenetReceiveFunc;
    FOnEthernetSocketEvent: TMustangpeakEthernetOnEthenetSocketEventFunc;
    FPortLocal: Word;
    FPortTarget: Word;
    FServerThread: TMustangpeakEthernetServerThread;
    FSyncronizeWindow: TSyncronizeWindow;
    function GetConnected: Boolean;
    function GetSocketEventList: TThreadList;
    procedure SetConnected(AValue: Boolean);
    { Private declarations }
  protected
    { Protected declarations }
    procedure KillThreads;
    property ClientThread: TMustangpeakEthernetClientThread read FClientThread write FClientThread;
    property ServerThread: TMustangpeakEthernetServerThread read FServerThread write FServerThread;
    property SyncronizeWindow: TSyncronizeWindow read FSyncronizeWindow write FSyncronizeWindow;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SendPacket(Packet: string);
    function ReceivePacket: string;
    property SocketEventList: TThreadList read GetSocketEventList;
  published
    { Published declarations }
    property GridConnect: Boolean read FGridConnect write FGridConnect default False;
    property IpAddressLocal: string read FIpAddressLocal write FIpAddressLocal;  // "0.0.0.0" = "localhost" or normal Ip format (i.e. "192.168.0.234")
    property IpAddressTarget: string read FIpAddressTarget write FIpAddressTarget;
    property PortLocal: Word read FPortLocal write FPortLocal default 12022;
    property PortTarget: Word read FPortTarget write FPortTarget default 12021;
    property ConnectionType: TMustangpeakEthernetConnectionType read FConnectionType write FConnectionType default mec_Client;
    property Connected: Boolean read GetConnected write SetConnected;
    property Events: TSocketEvents read FEvents write FEvents default [se_AfterConnect, se_CreateSocket, se_ReadFilter, se_Status, se_HeartBeat, se_Monitor];
    property HeartbeatRate: Integer read FHeartbeatRate write FHeartbeatRate default 0;
    property Hub: Boolean read FHub write FHub default False;
    property OnEthernetReceive: TMustangpeakEthernetOnEthenetReceiveFunc read FOnEthernetReceive write FOnEthernetReceive;
    property OnEthernetSocketEvent: TMustangpeakEthernetOnEthenetSocketEventFunc read FOnEthernetSocketEvent write FOnEthernetSocketEvent;
  end;

procedure Register;
function ConnectionStatusReasonToStr(Reason: THookSocketReason): string;


var
  ThreadObjectCount: Integer;


implementation

procedure Register;
begin
  {$I mustangpeakethernetconnection_icon.lrs}
  RegisterComponents('Mustangpeak',[TMustangpeakEthernetConnection]);
end;

function ConnectionStatusReasonToStr(Reason: THookSocketReason): string;
begin
  case Reason of
    HR_ResolvingBegin : Result := 'HR_ResolvingBegin';
    HR_ResolvingEnd   : Result := 'HR_ResolvingEnd';
    HR_SocketCreate   : Result := 'HR_SocketCreate';
    HR_SocketClose    : Result := 'HR_SocketClose';
    HR_Bind           : Result := 'HR_Bind';
    HR_Connect        : Result := 'HR_Connect';
    HR_CanRead        : Result := 'HR_CanRead';
    HR_CanWrite       : Result := 'HR_CanWrite';
    HR_Listen         : Result := 'HR_Listen';
    HR_Accept         : Result := 'HR_Accept';
    HR_ReadCount      : Result := 'HR_ReadCount';
    HR_WriteCount     : Result := 'HR_WriteCount';
    HR_Wait           : Result := 'HR_Wait';
    HR_Error          : Result := 'HR_Error';
  end;
end;

{ TSyncronizeWindow }

constructor TSyncronizeWindow.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Top := 0;
  Left := 0;
  Width := 0;
  Height := 0;
  Visible := False;
end;

procedure TSyncronizeWindow.LMEthernetReceive(var Msg: TLMNoParams);
var
  StringList: TStringList;
  ConnectionThread: TMustangpeakEthernetConnector;
begin
  if Assigned(Connector.ClientThread) then
    ConnectionThread := Connector.ClientThread
  else
  if Assigned(Connector.ServerThread) then
    ConnectionThread := Connector.ServerThread;
  if Assigned(ConnectionThread) then
  begin
    StringList := ConnectionThread.IncomingPackets.LockList;
    try
      if Assigned(Connector.OnEthernetReceive) then
        Connector.OnEthernetReceive(Connector, StringList.Text);
      StringList.Clear;
    finally
      ConnectionThread.IncomingPackets.UnlockList;
    end;
  end;
end;

procedure TSyncronizeWindow.LMEthernetSocketEvent(var Msg: TLMNoParams);
var
  j: Integer;
  List: TList;
  SocketEventList: TThreadList;
begin
  SocketEventList := Connector.SocketEventList;
  if Assigned(SocketEventList) then
  begin
    List := SocketEventList.LockList;
    try
      for j := 0 to List.Count - 1 do
      begin;
        if Assigned(Connector.OnEthernetSocketEvent) then
          Connector.OnEthernetSocketEvent(Connector, PSocketEventBuffer( List[j])^);
        Dispose(PSocketEventBuffer( List[j]));
      end;
    finally
      List.Clear;
      SocketEventList.UnlockList;
    end;
  end;
end;


{ TMustangpeakListenerThread }

procedure TMustangpeakListenerThread.EthernetAction;
var
  NewLink: PMustangpeakEthernetConnectionLink;
begin
  if not Terminated then
  begin
    if Socket.CanRead(-1) then
    begin
      if Socket.LastError <> WSAETIMEDOUT then
      begin
        if Socket.LastError = 0 then
        begin
          NewLink := Connector.CreateNewConnectionPair(Socket.Accept);
          if Assigned(NewLink) then
            Connector.AddConnection(NewLink);
        end else
          Terminate;
      end
    end
  end;
end;

{ TMustangpeakEthernetClientThread }

procedure TMustangpeakEthernetClientThread.EthernetBeforeExecute;
var
  Link: PMustangpeakEthernetConnectionLink;
begin
  Link := CreateNewConnectionPair(Socket.Socket);
  if Assigned(Link) then
    AddConnection(Link);
end;

procedure TMustangpeakEthernetClientThread.EthernetAfterExecute;
begin
  ClearConnections;
  CheckForSocketEventAndPostMessage;
end;

constructor TMustangpeakEthernetClientThread.Create(CreateSuspended: Boolean; const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);
  SocketType := st_Connect;
end;

{ TMustangpeakEthernetConnector }

procedure TMustangpeakEthernetConnector.ClearConnections;
var
  List: TList;
  i: Integer;
begin
  List := Connections.LockList;
  try
    for i := 0 to List.Count - 1 do
      DestroyConnection( PMustangpeakEthernetConnectionLink( List[i]));
  finally
    List.Clear;
    Connections.UnlockList;
  end;
end;

procedure TMustangpeakEthernetConnector.CheckForSocketEventAndPostMessage;
var
  List: TList;
begin
  List := SocketEventList.LockList;
  try
    if List.Count > 0 then
      PostMessage(OwnerComponent.SyncronizeWindow.Handle, LM_ETHERNET_SOCKET_EVENT, 0, 0);
  finally
    SocketEventList.UnlockList;
  end;
end;

procedure TMustangpeakEthernetConnector.CheckForReceivePacketAndPostMessage;
var
  List: TList;
  ReceiveThreadStringList, ConnectionStringList: TStringList;
  Link, HubLink: PMustangpeakEthernetConnectionLink;
  i, j: Integer;
  DoPost: Boolean;
begin
  DoPost := False;
  List := Connections.LockList;
  ConnectionStringList := IncomingPackets.LockList;                             // Incoming from ALL Rx threads!
  try
    for i := 0 to List.Count - 1 do
    begin
      Link := PMustangpeakEthernetConnectionLink( List[i]);
      ReceiveThreadStringList := Link^.ReceiveThread.IncomingPackets.LockList;  // All threads will block on incoming for this
      try
        if ReceiveThreadStringList.Count > 0 then
        begin
          ConnectionStringList.Add(ReceiveThreadStringList.Text);
          DoPost := True;
          if OwnerComponent.Hub then
          begin
            for j := 0 to List.Count - 1 do                                     // Stuff out other Ports as a hub
            begin
              if j <> i then
              begin
                HubLink := PMustangpeakEthernetConnectionLink( List[j]);
                HubLink^.TransmitThread.OutgoingPackets.Add(ReceiveThreadStringList.Text);
              end;
            end;
          end;
        end;
      finally
        ReceiveThreadStringList.Clear;                                          // Clear the worker RX threads
        Link^.ReceiveThread.IncomingPackets.UnlockList;
      end;
    end;
  finally
    IncomingPackets.UnlockList;
    Connections.UnlockList;
    if DoPost then
      PostMessage(OwnerComponent.SyncronizeWindow.Handle, LM_ETHERNET_RECEIVE, 0, 0);
  end;
end;

procedure TMustangpeakEthernetConnector.EthernetAction;

begin
  RTLeventWaitFor(Event);
  if not Terminated then
  begin
    CheckForReceivePacketAndPostMessage;
    CheckForSocketEventAndPostMessage;
  end;
end;

constructor TMustangpeakEthernetConnector.Create(CreateSuspended: Boolean;const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);
  Connections := TThreadList.Create;
  SocketEventList := TThreadList.Create;
end;

destructor TMustangpeakEthernetConnector.Destroy;
begin
  ClearConnections;
  FreeAndNil(FConnections);
  FreeAndNil(FSocketEventList);
  inherited Destroy;
end;

procedure TMustangpeakEthernetConnector.AddConnection(AConnection: PMustangpeakEthernetConnectionLink);
var
  List: TList;
begin
  List := Connections.LockList;
  try
    List.Add(AConnection);
  finally
    Connections.UnlockList;
  end;
end;

function TMustangpeakEthernetConnector.FindConnectionByRx(RxThread: TMustangpeakEthernetThread): PMustangpeakEthernetConnectionLink;
var
  List: TList;
  i: Integer;
begin
  Result := nil;
  List := Connections.LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      if PMustangpeakEthernetConnectionLink( List[i])^.ReceiveThread = RxThread then
        Result := PMustangpeakEthernetConnectionLink( List[i]);
    end;
  finally
    Connections.UnlockList;
  end;
end;

function TMustangpeakEthernetConnector.FindConnectionByTx(TxThread: TMustangpeakEthernetThread): PMustangpeakEthernetConnectionLink;
var
  List: TList;
  i: Integer;
begin
  Result := nil;
  List := Connections.LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      if PMustangpeakEthernetConnectionLink( List[i])^.TransmitThread = TxThread then
        Result := PMustangpeakEthernetConnectionLink( List[i]);
    end;
  finally
    Connections.UnlockList;
  end;
end;

function TMustangpeakEthernetConnector.CreateNewConnectionPair(ASocketHandle: TSocket): PMustangpeakEthernetConnectionLink;
begin
  New(Result);  //TMustangpeakEthernetConnectionLink
  Result^.NrpList := TNrpList.Create;
  Result^.ReceiveThread := TMustangpeakEthernetRxThread.Create(True);
  Result^.ReceiveThread.SocketType := st_Connect;
  Result^.ReceiveThread.hSocket := ASocketHandle;    // Back create the sockets with this handle
  Result^.ReceiveThread.Connector := Self;
  Result^.ReceiveThread.SocketEvents := SocketEvents;
  Result^.ReceiveThread.HeartbeatRate := HeartbeatRate;
  Result^.ReceiveThread.SocketEventList := SocketEventList;
  Result^.ReceiveThread.GridConnect := GridConnect;
  Result^.ReceiveThread.Start;
  Result^.TransmitThread := TMustangpeakEthernetTxThread.Create(True);
  Result^.TransmitThread.SocketType := st_Connect;
  Result^.TransmitThread.hSocket := ASocketHandle;   // Back create the sockets with this handle
  Result^.TransmitThread.Connector := Self;
  Result^.TransmitThread.SocketEvents := SocketEvents;
  Result^.TransmitThread.HeartbeatRate := HeartbeatRate;
  Result^.TransmitThread.SocketEventList := SocketEventList;
  Result^.TransmitThread.GridConnect := True;
  Result^.TransmitThread.Start;
end;

procedure TMustangpeakEthernetConnector.DestroyConnection(AConnection: PMustangpeakEthernetConnectionLink);
var
  List: TList;
  Link: PMustangpeakEthernetConnectionLink;
begin
  List := Connections.LockList;
  try
    Link := List.Extract(AConnection);
    if Assigned(Link) then
    begin
      try
        if not Link^.ReceiveThread.IsTerminated then
        begin
          Link^.ReceiveThread.Terminate;
          Link^.ReceiveThread.Socket.AbortSocket;
          while not Link^.ReceiveThread.IsTerminated do
            ThreadSwitch;
        end;
        FreeAndNil(Link^.ReceiveThread);
        if not Link^.TransmitThread.IsTerminated then
        begin
          Link^.TransmitThread.Terminate;
          RTLeventSetEvent(Link^.TransmitThread.Event);                          // Fire the event to get out of the wait state
          while not Link^.TransmitThread.IsTerminated do
            ThreadSwitch;
        end;
        FreeAndNil(Link^.TransmitThread);
        FreeAndNil(Link^.NrpList);
      finally
        Dispose(Link);
      end
    end;
  finally
    Connections.UnlockList;
  end;

end;

{ TMustangpeakEthernetServerThread }

procedure TMustangpeakEthernetServerThread.EthernetBeforeExecute;
begin
  FListener := TMustangpeakListenerThread.Create(True);
  Listener.SocketType := st_Listen;
  Listener.IpAddressTarget := IpAddressTarget;
  Listener.IpAddressLocal := IpAddressLocal;
  Listener.PortTarget := PortTarget;
  Listener.PortLocal := PortLocal;
  Listener.HeartbeatRate := HeartbeatRate;
  Listener.SocketEvents := SocketEvents;
  Listener.SocketEventList := SocketEventList;
  Listener.Connector := Self;
  Listener.Start;
end;

procedure TMustangpeakEthernetServerThread.EthernetAfterExecute;
begin
  ClearConnections;
  Listener.Terminate;
  Listener.Socket.AbortSocket;
  while not Listener.IsTerminated do
    ThreadSwitch;
  FreeAndNil(FListener);
  CheckForSocketEventAndPostMessage
end;

constructor TMustangpeakEthernetServerThread.Create(CreateSuspended: Boolean; const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);
  SocketType := st_None;
end;

{ TMustangpeakEthernetThread }

procedure TMustangpeakEthernetThread.LoadSocketEvents;
begin
  if se_AfterConnect in SocketEvents then
    Socket.OnAfterConnect := @LocalOnAfterConnect
  else
    Socket.OnAfterConnect := nil;

  if se_CreateSocket in SocketEvents then
    Socket.OnCreateSocket := @LocalOnCreateSocket
  else
    Socket.OnCreateSocket := nil;

  if se_HeartBeat in SocketEvents then
    Socket.OnHeartbeat := @LocalOnHeartbeat
  else
    Socket.OnHeartbeat := nil;

  if se_Monitor in SocketEvents then
    Socket.OnMonitor := @LocalOnMonitor
  else
    Socket.OnMonitor := nil;

  if se_ReadFilter in SocketEvents then
    Socket.OnReadFilter := @LocalOnReadFilter
  else
    Socket.OnReadFilter := nil;

  if se_Status in SocketEvents then
    Socket.OnStatus := @LocalOnStatus
  else
    Socket.OnStatus := nil;
end;

procedure TMustangpeakEthernetThread.AddSocketEvent(NewEvent: PSocketEventBuffer);
var
  List: TList;
begin
  if Assigned(SocketEventList) then
  begin
    List := SocketEventList.LockList;
    try
      NewEvent^.ClassName := ClassName;
      NewEvent^.Local := IpAddressLocal + ':' + IntToStr(PortLocal);
      NewEvent^.Remote := IpAddressTarget + ':' + IntToStr(PortTarget);
      List.Add(NewEvent);
    finally
      SocketEventList.UnLockList;
      RTLeventSetEvent(Connector.Event);
    end;
  end else
    Dispose(NewEvent)
end;

procedure TMustangpeakEthernetThread.Execute;
begin
  FinishedRunning := False;
  if SocketType = st_None then
  begin
    try
      EthernetBeforeExecute;
      while not Terminated do
      begin
        EthernetAction;
      end;
    finally
      EthernetAfterExecute;
      FinishedRunning := True;
    end;
  end else
  begin
    Socket := TTCPBlockSocket.Create;          // Created in context of the thread
    try
      LoadSocketEvents;
      Socket.Family := SF_IP4;                  // IP4
      Socket.ConvertLineEnd := True;            // Use #10, #13, or both to be a "string"
      Socket.HeartbeatRate := HeartbeatRate;
      if FhSocket > 0 then
        Socket.Socket := FhSocket                // This backreads the local and target IP and Port info in this Socket object
      else begin
        Socket.Bind(IpAddressLocal, IntToStr(PortLocal));
        if Socket.LastError = 0 then
        begin
          if SocketType = st_Listen then
            Socket.Listen
          else
            Socket.Connect(IpAddressTarget, IntToStr(PortTarget));
        end;
      end;
      if Socket.LastError = 0 then
      begin;
        // Back read connection parameters
        IpAddressLocal := Socket.GetLocalSinIP;
        IpAddressTarget := Socket.GetRemoteSinIP;
        PortLocal := Socket.GetLocalSinPort;
        PortTarget := Socket.GetRemoteSinPort;
        EthernetBeforeExecute;
        while not Terminated do
        begin
          EthernetAction;
        end;
      end;
    finally
      EthernetAfterExecute;
      Socket.CloseSocket;
      FinishedRunning := True;
    end;
  end;
end;

procedure TMustangpeakEthernetThread.EthernetBeforeExecute;
begin

end;

procedure TMustangpeakEthernetThread.EthernetAfterExecute;
begin

end;

procedure TMustangpeakEthernetThread.LocalOnAfterConnect(Sender: TObject);
var
  EventBuffer: PSocketEventBuffer;
begin
  New(EventBuffer);
  EventBuffer^.EventID := EVENT_AFTER_CONNECT;
  EventBuffer^.Sender := Sender;
  AddSocketEvent(EventBuffer);
end;

procedure TMustangpeakEthernetThread.LocalOnCreateSocket(Sender: TObject);
var
  EventBuffer: PSocketEventBuffer;
begin
  New(EventBuffer);
  EventBuffer^.EventID := EVENT_CREATE_SOCKET;
  EventBuffer^.Sender := Sender;
  AddSocketEvent(EventBuffer);
end;

procedure TMustangpeakEthernetThread.LocalOnReadFilter(Sender: TObject; var Value: AnsiString);
var
  EventBuffer: PSocketEventBuffer;
begin
  New(EventBuffer);
  EventBuffer^.EventID := EVENT_READ_FILTER;
  EventBuffer^.Sender := Sender;
  EventBuffer^.Value := Value;
  AddSocketEvent(EventBuffer);
end;

procedure TMustangpeakEthernetThread.LocalOnStatus(Sender: TObject; Reason: THookSocketReason; const Value: String);
var
  EventBuffer: PSocketEventBuffer;
begin
  New(EventBuffer);
  EventBuffer^.EventID := EVENT_STATUS;
  EventBuffer^.Sender := Sender;
  EventBuffer^.Value := Value;
  EventBuffer^.Reason := Reason;
  AddSocketEvent(EventBuffer);
end;

procedure TMustangpeakEthernetThread.LocalOnHeartbeat(Sender: TObject);
var
  EventBuffer: PSocketEventBuffer;
begin
  New(EventBuffer);
  EventBuffer^.EventID := EVENT_HEARTBEAT;
  EventBuffer^.Sender := Sender;
  AddSocketEvent(EventBuffer);
end;

procedure TMustangpeakEthernetThread.LocalOnMonitor(Sender: TObject; Writing: Boolean; const Buffer: TMemory; Len: Integer);
var
  EventBuffer: PSocketEventBuffer;
begin
  New(EventBuffer);
  EventBuffer^.EventID := EVENT_MONITOR;
  EventBuffer^.Sender := Sender;
  EventBuffer^.Writing := Writing;
  EventBuffer^.Buffer := Buffer;
  EventBuffer^.Len := Len;
  AddSocketEvent(EventBuffer);
end;

constructor TMustangpeakEthernetThread.Create(CreateSuspended: Boolean; const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);
  Event := RTLEventCreate;
  Connector := nil;
  FinishedRunning := False;
  IncomingPackets := TThreadStringList.Create;
  OutgoingPackets := TThreadStringList.Create;
  Inc(ThreadObjectCount);
end;

destructor TMustangpeakEthernetThread.Destroy;
begin
  RTLEventDestroy(FEvent);
  Socket.Free;
  Dec(ThreadObjectCount);
  FreeAndNil(FIncomingPackets);
  FreeAndNil(FOutgoingPackets);
  inherited Destroy;
end;

function TMustangpeakEthernetThread.IsTerminated: Boolean;
begin
  Result := FinishedRunning;
end;

{ TMustangpeakEthernetTxThread }

procedure TMustangpeakEthernetTxThread.EthernetAction;
var
  StringList: TStringList;
  i: Integer;
begin
  Socket.ResetLastError;
  RTLeventWaitFor(FEvent);                // Wait for something to Transmit
  if Socket.LastError = 0 then
  begin
    StringList := OutgoingPackets.LockList;
    try
      for i := 0 to StringList.Count - 1 do
        Socket.SendString(StringList[i]);
    finally
      StringList.Clear;
      OutgoingPackets.UnlockList;
    end;
  end else
  begin
    Terminate;
  end;

  // Write here
end;

{ TMustangpeakEthernetRxThread }

function TMustangpeakEthernetRxThread.GridConnect_DecodeMachine(NextChar: Char; var GridConnectStrPtr: PGridConnectString): Boolean;
var
  HeaderArray: array[0..7] of Char;
  i, j: Integer;
begin
 Result := False;
 case GridConnectReceiveState of
      GRIDCONNECT_STATE_SYNC_START :                                            // Find a starting ':'
        begin
          if NextChar = ':' then
          begin
            ReceiveGridConnectBufferIndex := 0;
            ReceiveGridConnectBuffer[ReceiveGridConnectBufferIndex] := ':';
            Inc(FReceiveGridConnectBufferIndex);
            GridConnectReceiveState := GRIDCONNECT_STATE_SYNC_FIND_X
          end
        end;
      GRIDCONNECT_STATE_SYNC_FIND_X :
        begin
          if NextChar <> ':' then                                               // Handle double ":"'s by doing nothing if the next byte is a ":", just wait for the next byte to see if it is a "X"
          begin
            if (NextChar = 'X') or (NextChar = 'x') then
            begin
              ReceiveGridConnectBuffer[ReceiveGridConnectBufferIndex] := 'X';
              Inc(FReceiveGridConnectBufferIndex);
              GridConnectReceiveState := GRIDCONNECT_STATE_SYNC_FIND_HEADER
            end else
               GridConnectReceiveState := GRIDCONNECT_STATE_SYNC_START          // Error, start over
          end
        end;
      GRIDCONNECT_STATE_SYNC_FIND_HEADER :
        begin
          if ReceiveGridConnectBufferIndex < 11 then
          begin
            if (NextChar = 'n') or (NextChar = 'N') then
            begin
              if ReceiveGridConnectBufferIndex = 10 then                        // Just right number of characters, all done
              begin
                ReceiveGridConnectBuffer[ReceiveGridConnectBufferIndex] := 'N';
                Inc(FReceiveGridConnectBufferIndex);                             // Skip over the "N"
                GridConnectReceiveState := GRIDCONNECT_STATE_SYNC_FIND_DATA;
              end else
              begin
                for i := 0 to 7 do
                  HeaderArray[i] := '0';
                j := 7;
                for i := ReceiveGridConnectBufferIndex - 1 downto (11 - ReceiveGridConnectBufferIndex) do
                begin
                  HeaderArray[j] := ReceiveGridConnectBuffer[i];
                  Dec(j);
                end;
                for i := 0 to 7 do
                  ReceiveGridConnectBuffer[2 + i] := HeaderArray[i];
                ReceiveGridConnectBuffer[10] := 'N';
                ReceiveGridConnectBufferIndex := 11;                             // Skip over the "N"
                GridConnectReceiveState := GRIDCONNECT_STATE_SYNC_FIND_DATA;
              end;
            end else
            begin
              if IsValidHexChar(NextChar) then
              begin
                ReceiveGridConnectBuffer[ReceiveGridConnectBufferIndex] := NextChar;
                Inc(FReceiveGridConnectBufferIndex);
              end else
              GridConnectReceiveState := GRIDCONNECT_STATE_SYNC_START         // Error start over
            end
          end else
            GridConnectReceiveState := GRIDCONNECT_STATE_SYNC_START         // Error start over
        end;
      GRIDCONNECT_STATE_SYNC_FIND_DATA :
        begin
           if NextChar = ';'then
           begin
             if (ReceiveGridConnectBufferIndex + 1) mod 2 = 0 then              // 0 index, add 1 for the actual character count, if not true the result is broken
             begin
               ReceiveGridConnectBuffer[ReceiveGridConnectBufferIndex] := ';';
               ReceiveGridConnectBuffer[ReceiveGridConnectBufferIndex + 1] := #0;
               GridConnectStrPtr := @ReceiveGridConnectBuffer;
               Result := True;
             end;
             GridConnectReceiveState := GRIDCONNECT_STATE_SYNC_START            // Done
           end else
           begin
             if IsValidHexChar(NextChar) then
             begin
               ReceiveGridConnectBuffer[ReceiveGridConnectBufferIndex] := NextChar;
               Inc(FReceiveGridConnectBufferIndex);
             end else
               GridConnectReceiveState := GRIDCONNECT_STATE_SYNC_START;         // Error start over
           end
        end else
          GridConnectReceiveState := GRIDCONNECT_STATE_SYNC_START;              // Invalidate State Index
    end;  // Case
end;

function TMustangpeakEthernetRxThread.RawEthernet_DecodeMachine(NextChar: Char; var Packet: string): Boolean;
begin
 Result := False;
 case RawEthernetReceiveState of
      RAWETHERNET_STATE_SYNC_START :                                            // Find a starting ':'
        begin
          if (NextChar <> #13) and (NextChar <> #10) then
          begin
            ReceiveRawEthernetBufferIndex := 0;
            ReceiveRawEthernetBuffer[ReceiveRawEthernetBufferIndex] := NextChar;
            Inc(FReceiveGridConnectBufferIndex);
            GridConnectReceiveState := RAWETHERNET_STATE_SYNC_FIND_DATA
          end
        end;
      RAWETHERNET_STATE_SYNC_FIND_DATA :
        begin
          if (NextChar <> #13) and (NextChar <> #10) then
          begin
            ReceiveRawEthernetBuffer[ReceiveRawEthernetBufferIndex] := NextChar;
            Inc(FReceiveGridConnectBufferIndex);
          end else
          begin
            ReceiveRawEthernetBuffer[ReceiveRawEthernetBufferIndex] := #0;
            RawEthernetReceiveState := RAWETHERNET_STATE_SYNC_START;
            Packet := ReceiveRawEthernetBuffer;
            Result := True;
          end
        end
  else
    RawEthernetReceiveState := RAWETHERNET_STATE_SYNC_START                     // Confused, reset
  end
end;

function TMustangpeakEthernetRxThread.IsValidHexChar(AChar: Char): Boolean;
begin
  Result := ((AChar >= '0') and (AChar <= '9')) or ((AChar >= 'A') and (AChar <= 'F')) or ((AChar >= 'a') and (AChar <= 'f'))
end;

constructor TMustangpeakEthernetRxThread.Create(CreateSuspended: Boolean; const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);
  FGridConnectReceiveState := GRIDCONNECT_STATE_SYNC_START;
  FReceiveGridConnectBufferIndex := 0;
  FReceiveRawEthernetBufferIndex := 0;
  FRawEthernetReceiveState := RAWETHERNET_STATE_SYNC_START;
end;

procedure TMustangpeakEthernetRxThread.EthernetAction;
var
  ReceiveStr: string;
  Packet: string;
  i: Integer;
  GridConnectStrPtr: PGridConnectString;
begin
  ReceiveStr := Socket.RecvPacket(-1);
  if not Terminated then
  begin
    if Socket.LastError <> WSAETIMEDOUT then
    begin
      if Socket.LastError = 0 then
      begin
        if GridConnect then
        begin
          GridConnectStrPtr := nil;
          for i := 1 to Length(ReceiveStr) do
          begin
            if GridConnect_DecodeMachine(ReceiveStr[i], GridConnectStrPtr) then
            begin
              IncomingPackets.Add(GridConnectStrPtr^);
              RTLeventSetEvent(Connector.Event);
            end;
          end;
        end else
        begin
          Packet := '';
          for i := 1 to Length(ReceiveStr) do
          begin
            if RawEthernet_DecodeMachine(ReceiveStr[i], Packet) then
            begin
              IncomingPackets.Add(Packet);
              RTLeventSetEvent(Connector.Event);
            end;
          end;
        end
      end else
        Terminate;
    end
  end;
end;


{ TMustangpeakEthernetConnection }

function TMustangpeakEthernetConnection.GetConnected: Boolean;
begin
  Result := FConnected;
end;

function TMustangpeakEthernetConnection.GetSocketEventList: TThreadList;
begin
  if Assigned(ClientThread) then
    Result := ClientThread.SocketEventList
  else
  if Assigned(ServerThread) then
    Result := ServerThread.SocketEventList
  else
    Result := nil
end;

procedure TMustangpeakEthernetConnection.SetConnected(AValue: Boolean);
begin
  if csDesigning in ComponentState then Exit;

  if AValue and not FConnected then
  begin
    if not Assigned( SyncronizeWindow.Parent) then                              // Create the window handle
      SyncronizeWindow.Parent := Application.MainForm;
    // Wants to connect and it it currently not
    if ConnectionType = mec_Client then
    begin
      // We are a client, easy
      KillThreads;
      ClientThread := TMustangpeakEthernetClientThread.Create(True);
      ClientThread.OwnerComponent := Self;
      ClientThread.SocketEvents := Events;
      ClientThread.HeartbeatRate := HeartbeatRate;
      ClientThread.IpAddressTarget := IpAddressTarget;
      ClientThread.IpAddressLocal := IpAddressLocal;
      ClientThread.PortTarget := PortTarget;
      ClientThread.PortLocal := PortLocal;
      ClientThread.Connector := ClientThread;
      ClientThread.GridConnect := GridConnect;
      ClientThread.Start;
      FConnected := True;                           // This assumed, really need to watch the thread to be sure...
    end else
    begin
      KillThreads;
      ServerThread := TMustangpeakEthernetServerThread.Create(True);
      ServerThread.OwnerComponent := Self;
      ServerThread.SocketEvents := Events;
      ServerThread.HeartbeatRate := HeartbeatRate;
      ServerThread.IpAddressTarget := IpAddressTarget;
      ServerThread.IpAddressLocal := IpAddressLocal;
      ServerThread.PortTarget := PortTarget;
      ServerThread.PortLocal := PortLocal;
      ServerThread.Connector := ServerThread;
      ServerThread.GridConnect := GridConnect;
      ServerThread.Start;
      FConnected := True;
    end;
  end else
  if not AValue and FConnected then
  begin
    // Wants to disconnect and it currently is
    FConnected := False;
    KillThreads;
  end;
end;

procedure TMustangpeakEthernetConnection.KillThreads;
begin
  if Assigned(FClientThread) then
  begin
    ClientThread.Terminate;
    RTLeventSetEvent(ClientThread.Event);      // Fire the event to get out of the wait state
    while not ClientThread.IsTerminated do
      ThreadSwitch;
  end;
  FreeAndNil(FClientThread);
  if Assigned(FServerThread) then
  begin
    ServerThread.Terminate;
    RTLeventSetEvent(ServerThread.Event);      // Fire the event to get out of the wait state
    while not ServerThread.IsTerminated do
      ThreadSwitch;
  end;
  FreeAndNil(FServerThread);
end;

constructor TMustangpeakEthernetConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FServerThread := nil;
  FClientThread := nil;
  FPortLocal := 12022;
  FPortTarget := 12021;
  FIpAddressLocal := 'localhost';          // "0.0.0.0" = "localhost" or normal Ip format (i.e. "192.168.0.234")
  FIpAddressTarget := '192.168.0.1';
  FEvents := [se_AfterConnect, se_CreateSocket, se_ReadFilter, se_Status, se_HeartBeat, se_Monitor];
  FGridConnect := False;
  SyncronizeWindow := TSyncronizeWindow.Create(Self);
  SyncronizeWindow.Connector := Self;
end;

destructor TMustangpeakEthernetConnection.Destroy;
begin
  KillThreads;
  FreeAndNil(FSyncronizeWindow);
  inherited Destroy;
end;

procedure TMustangpeakEthernetConnection.SendPacket(Packet: string);
var
  i: Integer;
  List: TList;
  StringList: TStringList;
  ConnectionThread: TMustangpeakEthernetConnector;
  Connection: PMustangpeakEthernetConnectionLink;
begin
  if Assigned(ClientThread) then
    ConnectionThread := ClientThread
  else
  if Assigned(ServerThread) then
    ConnectionThread := ServerThread;
  if Assigned(ConnectionThread) then
  begin
    List := ConnectionThread.Connections.LockList;
    try
      for i := 0 to List.Count - 1 do
      begin
        Connection := PMustangpeakEthernetConnectionLink( List[i]);
        if not Connection^.TransmitThread.IsTerminated then
        begin
          StringList := Connection^.TransmitThread.OutgoingPackets.LockList;
          try
            StringList.Add(Packet);
          finally
            Connection^.TransmitThread.OutgoingPackets.UnlockList;
            RTLeventSetEvent(Connection^.TransmitThread.Event);
          end;
        end;
      end;
    finally
      ConnectionThread.Connections.UnlockList;
    end;
  end;
end;

function TMustangpeakEthernetConnection.ReceivePacket: string;
var
  StringList: TStringList;
  ConnectionThread: TMustangpeakEthernetConnector;
begin
  if Assigned(ClientThread) then
    ConnectionThread := ClientThread
  else
  if Assigned(ServerThread) then
    ConnectionThread := ServerThread;
  if Assigned(ConnectionThread) then
  begin
    StringList := ConnectionThread.IncomingPackets.LockList;
    try
      Result := StringList.Text;
      StringList.Clear;
    finally
      ConnectionThread.IncomingPackets.UnlockList;
    end;
  end;
end;

initialization
  ThreadObjectCount := 0;

end.
