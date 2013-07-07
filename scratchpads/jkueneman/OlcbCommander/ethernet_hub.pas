unit ethernet_hub;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synsock, Forms, olcb_app_common_settings, Dialogs,
  common_utilities, olcb_utilities;

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
  TClientSocketThread = class(TThread)
  private
    FConnectedSocket: TTCPBlockSocket;
    FhSocketLocal: TSocket;
    FOwnerHub: TEthernetHub;
    FTCP_Receive_State: Integer;
  protected
    procedure Execute; override;
    property hSocketLocal: TSocket read FhSocketLocal write FhSocketLocal;
    property ConnectedSocket: TTCPBlockSocket read FConnectedSocket;
    property TCP_Receive_State: Integer read FTCP_Receive_State write FTCP_Receive_State;    // Statemachine Index
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
    FBufferRawMessage: string;
    FEnableReceiveMessages: Boolean;
    FEnableSendMessages: Boolean;
    FMessageManager: TTCPMessageManager;
    FOnHubConnect: TOnHubConnectFunc;
    FOnHubDisconnect: TOnHubConnectFunc;
    FEnabled: Boolean;
    FListenDameon: TEthernetHubThread;
    FOnClientDisconnect: TOnClientConnectChangeFunc;
    FOnClientConnect: TOnClientConnectChangeFunc;
    FClientThreadList: TSocketThreadList;
    FSyncErrorMessageFunc: TSyncRawMessageFunc;
    FSyncReceiveMessageFunc: TSyncRawMessageFunc;
    FSyncSendMessageFunc: TSyncRawMessageFunc;
    procedure SetEnabled(AValue: Boolean);
  protected
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
    property ClientThreadList: TSocketThreadList read FClientThreadList write FClientThreadList;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property EnableReceiveMessages: Boolean read FEnableReceiveMessages write FEnableReceiveMessages;
    property EnableSendMessages: Boolean read FEnableSendMessages write FEnableSendMessages;
    property MessageManager: TTCPMessageManager read FMessageManager write FMessageManager;
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
  ReceivedData: AnsiString;
  Receive_GridConnectBufferIndex: Integer;
  Receive_GridConnectBuffer: array[0..MAX_GRID_CONNECT_LEN-1] of char;
  BytesRead, PacketIndex: Integer;
  Done: Boolean;
  TCP_Receive_Char: char;
  GridConnectMsg: TTCPMessage;
begin
  FConnectedSocket := TTCPBlockSocket.Create;
  try
    ConnectedSocket.Socket := hSocketLocal;
    ConnectedSocket.GetSins;                     // Back load the IP's / Ports information from the handle
    while not Terminated do
    begin
      ReceivedData := ConnectedSocket.RecvPacket(1000);

      BytesRead := 0;
      PacketIndex := 1;
      Receive_GridConnectBufferIndex := 0;
      while not Done and (BytesRead < Length(ReceivedData)) do
      begin
        TCP_Receive_Char := ReceivedData[PacketIndex];                       // Get the next byte from the stack
        case TCP_Receive_State of
          TCP_STATE_SYNC_START :                                                // Find a starting ':'
            begin
              if TCP_Receive_Char = ':' then
              begin
                Receive_GridConnectBufferIndex := 0;
                Receive_GridConnectBuffer[Receive_GridConnectBufferIndex] := ':';
                Inc(Receive_GridConnectBufferIndex);
                TCP_Receive_State := TCP_STATE_SYNC_FIND_X
              end
            end;
          TCP_STATE_SYNC_FIND_X :
            begin
              if TCP_Receive_Char <> ':' then   // Handle double ":"'s by doing nothing if the next byte is a ":", just wait for the next byte to see if it is a "X"
              begin
                if (TCP_Receive_Char = 'X') or (TCP_Receive_Char = 'x') then
                begin
                  Receive_GridConnectBuffer[Receive_GridConnectBufferIndex] := 'X';
                  Inc(Receive_GridConnectBufferIndex);
                  TCP_Receive_State := TCP_STATE_SYNC_FIND_HEADER
                end else
                   TCP_Receive_State := TCP_STATE_SYNC_START                    // Error, start over
              end
            end;
          TCP_STATE_SYNC_FIND_HEADER :
            begin
              if IsValidHexChar(TCP_Receive_Char) then
              begin
                Receive_GridConnectBuffer[Receive_GridConnectBufferIndex] := TCP_Receive_Char;
                if Receive_GridConnectBufferIndex = 9 then
                  TCP_Receive_State := TCP_STATE_SYNC_FIND_N;
                Inc(Receive_GridConnectBufferIndex);
              end else
                TCP_Receive_State := TCP_STATE_SYNC_START                       // Error start over
            end;
          TCP_STATE_SYNC_FIND_N :
            begin
              if (TCP_Receive_Char >= 'N') or (TCP_Receive_Char <= 'n') then
              begin
                Receive_GridConnectBuffer[Receive_GridConnectBufferIndex] := 'N';
                Inc(Receive_GridConnectBufferIndex);
                TCP_Receive_State := TCP_STATE_SYNC_FIND_DATA;
              end else
                TCP_Receive_State := TCP_STATE_SYNC_START                       // Error start over
            end;
          TCP_STATE_SYNC_FIND_DATA :
            begin
               if TCP_Receive_Char = ';'then
               begin
                 if (Receive_GridConnectBufferIndex + 1) mod 2 = 0 then           // 0 index, add 1 for the actual character count
                 begin
                   Receive_GridConnectBuffer[Receive_GridConnectBufferIndex] := ';';
                   Receive_GridConnectBuffer[Receive_GridConnectBufferIndex + 1] := #0;
                   if Assigned(OwnerHub) then
                   begin
                     GridConnectMsg := TTCPMessage.Create;
                     GridConnectMsg.Message := Receive_GridConnectBuffer;
                     GridConnectMsg.Source := Self;
                     OwnerHub.MessageManager.Messages.Add(GridConnectMsg);
                     if OwnerHub.EnableReceiveMessages then
                     begin
                       OwnerHub.BufferRawMessage := GridConnectMsg.Message;
                       Self.Synchronize(@OwnerHub.SyncReceiveMessage);
                     end;
                   end
                 end;
                 TCP_Receive_State := TCP_STATE_SYNC_START                      // Done
               end else
               begin
                 if IsValidHexChar(TCP_Receive_Char) then
                 begin
                   Receive_GridConnectBuffer[Receive_GridConnectBufferIndex] := TCP_Receive_Char;
                   Inc(Receive_GridConnectBufferIndex);
                 end else
                   TCP_Receive_State := TCP_STATE_SYNC_START;                   // Error start over
               end
            end else
              TCP_Receive_State := TCP_STATE_SYNC_START;                        // Invalidate State Index
        end;
        Inc(BytesRead);
      end;



   {    if not ConnectedSocket.CanRead(0) and (ConnectedSocket.WaitingData = 0)  and Assigned(OwnerHub) then
       begin
         OwnerHub.ClientThreadList.Remove(Self);
         Synchronize(@OwnerHub.LocalOnClientDisconnect);
         OwnerHub := nil;      // Unlink as we may go away soon
         Terminate;
       end;      }
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
      SocketThread.OwnerHub := nil;
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
          ClientSocketThread := TClientSocketThread.Create(hSocket);
          ClientSocketThread.OwnerHub := OwnerHub;
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
    if Assigned(SyncErrorMessageFunc) then
    SyncReceiveMessageFunc(BufferRawMessage)
end;

procedure TEthernetHub.SyncSendMessage;
begin
  if Assigned(SyncErrorMessageFunc) then
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

end.

