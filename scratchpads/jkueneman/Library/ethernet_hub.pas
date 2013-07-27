unit ethernet_hub;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synsock, Forms, olcb_app_common_settings, Dialogs,
  common_utilities, olcb_utilities, olcb_transport_layer, olcb_defines;

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

  TOnSyncStatus = procedure(Client: TClientSocketThread; Reason: THookSocketReason; Value: String) of object;

  { TClientSocketThread }
  TClientSocketThread = class(TTransportLayerThread)
  private
    FConnectedSocket: TTCPBlockSocket;
    FhSocketLocal: TSocket;
    FOnStatusReason: THookSocketReason;
    FOnStatusValue: string;
    FOnSyncStatus: TOnSyncStatus;
    FTCP_Receive_State: Integer;
  protected
    procedure Execute; override;
    procedure OnStatus(Sender: TObject; Reason: THookSocketReason; const Value: String);
    procedure SyncOnStatus;
    property hSocketLocal: TSocket read FhSocketLocal write FhSocketLocal;
    property ConnectedSocket: TTCPBlockSocket read FConnectedSocket;
    property TCP_Receive_State: Integer read FTCP_Receive_State write FTCP_Receive_State;    // Statemachine Index
    property OnStatusReason: THookSocketReason read FOnStatusReason write FOnStatusReason;
    property OnStatusValue: string read FOnStatusValue write FOnStatusValue;
  public
    constructor Create(CreateSuspended: Boolean); override;
    destructor Destroy; override;
    property OnSyncStatus: TOnSyncStatus read FOnSyncStatus write FOnSyncStatus;
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
    FBufferRawMessage: string;
    FEnableReceiveMessages: Boolean;
    FEnableSendMessages: Boolean;
    FMessageManager: TTCPMessageManager;
    FOnBeforeDestroyTask: TOlcbTaskBeforeDestroy;
    FOnHubConnect: TOnHubConnectFunc;
    FOnHubDisconnect: TOnHubConnectFunc;
    FEnabled: Boolean;
    FListenDameon: TEthernetHubThread;
    FOnClientDisconnect: TOnClientConnectChangeFunc;
    FOnClientConnect: TOnClientConnectChangeFunc;
    FClientThreadList: TSocketThreadList;
    FOnSyncStatus: TOnSyncStatus;
    FSyncErrorMessageFunc: TSyncRawMessageFunc;
    FSyncReceiveMessageFunc: TSyncRawMessageFunc;
    FSyncSendMessageFunc: TSyncRawMessageFunc;
    procedure SetEnabled(AValue: Boolean);
    procedure SetEnableReceiveMessages(AValue: Boolean);
    procedure SetEnableSendMessages(AValue: Boolean);
  protected
    procedure InternalAdd(Msg: AnsiString);
    procedure InternalAddDatagramToSend(Datagram: TDatagramSend);
    procedure LocalOnHubConnect;
    procedure LocalOnHubDisconnect;
    procedure LocalOnClientConnect;
    procedure LocalOnClientDisconnect;
    procedure SyncErrorMessage;
    procedure SyncReceiveMessage;
    procedure SyncSendMessage;
    property BufferRawMessage: string read FBufferRawMessage write FBufferRawMessage;
    property ListenDameon: TEthernetHubThread read FListenDameon;
  public
    constructor Create;
    destructor Destroy; override;

    function AddTask(NewTask: TOlcbTaskBase): Boolean;
    procedure RemoveAndFreeTasks(RemoveKey: PtrInt);

    property ClientThreadList: TSocketThreadList read FClientThreadList write FClientThreadList;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property EnableReceiveMessages: Boolean read FEnableReceiveMessages write SetEnableReceiveMessages;
    property EnableSendMessages: Boolean read FEnableSendMessages write SetEnableSendMessages;
    property MessageManager: TTCPMessageManager read FMessageManager write FMessageManager;
    property OnBeforeDestroyTask: TOlcbTaskBeforeDestroy read FOnBeforeDestroyTask write FOnBeforeDestroyTask;
    property OnSyncStatus: TOnSyncStatus read FOnSyncStatus write FOnSyncStatus;
    property OnHubConnect: TOnHubConnectFunc read FOnHubConnect write FOnHubConnect;
    property OnHubDisconnect: TOnHubConnectFunc read FOnHubDisconnect write FOnHubDisconnect;
    property OnClientClientConnect: TOnClientConnectChangeFunc read FOnClientConnect write FOnClientConnect;
    property OnClientDisconnect: TOnClientConnectChangeFunc read FOnClientDisconnect write FOnClientDisconnect;
    property SyncErrorMessageFunc: TSyncRawMessageFunc read FSyncErrorMessageFunc write FSyncErrorMessageFunc;
    property SyncReceiveMessageFunc: TSyncRawMessageFunc read FSyncReceiveMessageFunc write FSyncReceiveMessageFunc;
    property SyncSendMessageFunc: TSyncRawMessageFunc read FSyncSendMessageFunc write FSyncSendMessageFunc;
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
//  ReceivedData: AnsiString;
//  Receive_GridConnectBufferIndex: Integer;
//  Receive_GridConnectBuffer: array[0..MAX_GRID_CONNECT_LEN-1] of char;
//  PacketIndex, i: Integer;
//  Done: Boolean;
  i: Integer;
  List: TList;
  SendStr: AnsiString;
  Helper: TOpenLCBMessageHelper;
 // TCP_Receive_Char: char;
 // GridConnectMsg: TTCPMessage;
  SyncSendMessageList: TStringList;
begin
  ExecuteBegin;
  FConnectedSocket := TTCPBlockSocket.Create;
  ConnectedSocket.OnStatus := @OnStatus;
  ConnectedSocket.ConvertLineEnd := True;      // User #10, #13, or both to be a "string"
  try
    Helper := TOpenLCBMessageHelper.Create;
    ConnectedSocket.Socket := hSocketLocal;
    ConnectedSocket.GetSins;                     // Back load the IP's / Ports information from the handle
    while not Terminated do
    begin
      SyncSendMessageList := TStringList.Create;
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
          Synchronize(@SyncSendMessage);
        end;
        SyncSendMessageList.Clear;
      end;

      DatagramSendManager.ProcessSend;                                        // *** See if there is a datagram that will add a message to send ***
      OlcbTaskManager.ProcessSending;                                         // *** See if there is a task what will add a message to send ***
      if SendStr <> '' then                                                   // *** Put the message on the wire and communicate back the raw message sent ***
      begin
        ConnectedSocket.SendString(SendStr + LF);
        SendStr := '';
      end;

      DecomposeAndDispatchGridConnectString(ConnectedSocket.RecvString(1), Helper);

   {    if not ConnectedSocket.CanRead(0) and (ConnectedSocket.WaitingData = 0)  and Assigned(OwnerHub) then
       begin
         OwnerHub.ClientThreadList.Remove(Self);
         Synchronize(@OwnerHub.LocalOnClientDisconnect);
         OwnerHub := nil;      // Unlink as we may go away soon
         Terminate;
       end;      }
    end;
  finally
    ConnectedSocket.Free;
    Helper.Free;
  end;
  ExecuteEnd;
end;

procedure TClientSocketThread.OnStatus(Sender: TObject; Reason: THookSocketReason; const Value: String);
begin
  if Assigned(OnSyncStatus) then
  begin
    OnStatusReason := Reason;
    OnStatusValue := Value;
    Synchronize(@SyncOnStatus);
  end;
end;

procedure TClientSocketThread.SyncOnStatus;
begin
  if Assigned(OnSyncStatus) then
    OnSyncStatus(Self, OnStatusReason, OnStatusValue);
end;

constructor TClientSocketThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(True);
  FConnectedSocket := nil;
  FOnSyncStatus := nil;
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
  ClientSocketThread: TClientSocketThread;
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
          ClientSocketThread := TClientSocketThread.Create(True);
          ClientSocketThread.hSocketLocal := hSocket;
          ClientSocketThread.SyncReceiveMessageFunc := OwnerHub.SyncReceiveMessageFunc;
          ClientSocketThread.SyncSendMessageFunc := OwnerHub.SyncSendMessageFunc;
          ClientSocketThread.SyncErrorMessageFunc := OwnerHub.SyncErrorMessageFunc;
          ClientSocketThread.EnableReceiveMessages := OwnerHub.EnableReceiveMessages;
          ClientSocketThread.EnableSendMessages := OwnerHub.EnableSendMessages;
          ClientSocketThread.OnBeforeDestroyTask := OwnerHub.OnBeforeDestroyTask;
          ClientSocketThread.OnSyncStatus := OwnerHub.OnSyncStatus;
          ClientSocketThread.FreeOnTerminate := True;
          OwnerHub.ClientThreadList.Add(ClientSocketThread);       // add it to the list
          ClientSocketThread.Suspended := False;

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

procedure TEthernetHub.SyncErrorMessage;
begin
  if Assigned(SyncErrorMessageFunc) then
    SyncErrorMessageFunc(BufferRawMessage)
end;

procedure TEthernetHub.SyncReceiveMessage;
begin
  if Assigned(SyncReceiveMessageFunc) then
    SyncReceiveMessageFunc(BufferRawMessage)
end;

procedure TEthernetHub.SyncSendMessage;
begin
  if Assigned(SyncSendMessageFunc) then
    SyncSendMessageFunc(BufferRawMessage)
end;

constructor TEthernetHub.Create;
begin
  FOnClientConnect := nil;
  FOnHubConnect := nil;
  FOnHubDisconnect := nil;
  FOnClientDisconnect := nil;
  EnableReceiveMessages := False;
  EnableSendMessages := False;
  FSyncErrorMessageFunc := nil;
  FSyncReceiveMessageFunc := nil;
  FSyncSendMessageFunc := nil;
  FOnBeforeDestroyTask := nil;
  FOnSyncStatus := nil;
  FClientThreadList := TSocketThreadList.Create;
  FMessageManager := TTCPMessageManager.Create;
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

procedure TEthernetHub.InternalAddDatagramToSend(Datagram: TDatagramSend);
var
  List: TList;
  i: Integer;
begin
  List := ClientThreadList.LockList;
  try
    for i := 0 to List.Count - 1 do
      TClientSocketThread( List[i]).InternalAddDatagramToSend(Datagram);
  finally
    ClientThreadList.UnlockList;
  end;
end;

function TEthernetHub.AddTask(NewTask: TOlcbTaskBase): Boolean;
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
        TClientSocketThread( List[i]).AddTask(NewTask);   // Broadcast
      Done := True;
    end else
    begin
      i := 0;
      while (i < List.Count) and not Done do
      begin
        Done := TClientSocketThread( List[i]).AddTask(NewTask);
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

