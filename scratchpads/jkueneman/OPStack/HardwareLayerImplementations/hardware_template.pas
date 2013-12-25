unit hardware_template;

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  {$IFDEF FPC}
  Classes, SysUtils, blcksock, synsock, Forms, Dialogs,
  {$ENDIF}
  gridconnect,
  opstackbuffers,
  opstackdefines,
  opstackcanstatemachinesbuffers;

// *****************************************************************************
//  Lazarus Specific from here down
// *****************************************************************************
{$IFDEF FPC}
type
  TEthernetThreadType = (ETT_Reader, ETT_Writer);
  TOpStackTestCallbackMethod = procedure(ReceivedStr: ansistring) of object;
  TOpStackTestRunningCallbackMethod = procedure(EthernetThreadType: TEthernetThreadType) of object;

  { TOPStackTestConnectionInput }

  TOPStackTestConnectionInput = class(TThread)
  private
    FCallback: TOpStackTestCallbackMethod;
    FHasTerminated: Boolean;
    FhSocket: TSocket;
    FReceiveStr: string;
    FRunningCallback: TOpStackTestRunningCallbackMethod;
  protected
    Socket: TTCPBlockSocket; // Created in context of Thread
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
    procedure Abort;
    procedure Execute; override;
    procedure Synchronizer;
    procedure RunningSynchronizer;
  public
    property hSocket: TSocket read FhSocket write FhSocket;
    property ReceiveStr: string read FReceiveStr write FReceiveStr;
    property Callback: TOpStackTestCallbackMethod read FCallback write FCallback;
    property RunningCallback: TOpStackTestRunningCallbackMethod read FRunningCallback write FRunningCallback;
    property HasTerminated: Boolean read FHasTerminated;
  end;

  { TOPStackTestConnectionOutput }

  TOPStackTestConnectionOutput = class(TThread)
  private
    FCallback: TOpStackTestCallbackMethod;
    FEvent: PRTLEvent;
    FHasTerminated: Boolean;
    FhSocket: TSocket;
    FRunningCallback: TOpStackTestRunningCallbackMethod;
    FSendList: TThreadList;
    FSendString: ansistring;
    FSocket: TTCPBlockSocket;
  protected
    property Socket: TTCPBlockSocket read FSocket write FSocket;
    property SendString: ansistring read FSendString write FSendString;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
    procedure Abort;
    procedure Execute; override;
    procedure RunningSynchronizer;
    procedure Synchronizer;
    procedure Send(StringList: TStringList);
    property hSocket: TSocket read FhSocket write FhSocket;
    property SendList: TThreadList read FSendList write FSendList;
    property Event: PRTLEvent read FEvent write FEvent;
    property Callback: TOpStackTestCallbackMethod read FCallback write FCallback;
    property RunningCallback: TOpStackTestRunningCallbackMethod read FRunningCallback write FRunningCallback;
    property HasTerminated: Boolean read FHasTerminated;
  end;

  { TOPStackTestListener }

  TOPStackTestListener = class(TThread)
  private
    FCallback: TOpStackTestCallbackMethod;
    FConnectionInput: TOPStackTestConnectionInput;
    FConnectionOutput: TOPStackTestConnectionOutput;
    FHasTerminated: Boolean;
    FRunningCallback: TOpStackTestRunningCallbackMethod;
  protected
     Socket: TTCPBlockSocket; // Created in context of thread
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
    procedure Abort;
    procedure Execute; override;
    procedure Send(MessageStr: ansistring);

    property Callback: TOpStackTestCallbackMethod read FCallback write FCallback;
    property ConnectionInput: TOPStackTestConnectionInput read FConnectionInput write FConnectionInput;
    property ConnectionOutput: TOPStackTestConnectionOutput read FConnectionOutput write FConnectionOutput;
    property RunningCallback: TOpStackTestRunningCallbackMethod read FRunningCallback write FRunningCallback;
    property HasTerminated: Boolean read FHasTerminated;
  end;

  { TOPstackTestClient }

  TOPstackTestClient = class(TOPStackTestListener)
  private
    FEvent: PRTLEvent;
  public
    destructor Destroy; override;
    procedure Execute; override;
    property Event: PRTLEvent read FEvent write FEvent;
  end;


var
  ListenerThread: TOPStackTestListener;
  ClientThread: TOPstackTestClient;

{$ENDIF}
// ***************************************************************************************************************************************************************************************************************************************
//  Lazarus Specific from here up
// ***************************************************************************************************************************************************************************************************************************************

procedure Hardware_Initialize;

// The OPStack calles these functions to control and/or send messages through the hardware layer
procedure Hardware_DisableInterrupts;
procedure Hardware_EnableInterrupts;

procedure OutgoingCriticalMessage(AMessage: POPStackMessage);                   // Called _back_ from within the IncomingMessageDispatch if we can't allocate buffers, unknown MTI's etc.  For CAN this is expected to be immediatly replied back to the sender as these are very high priority CAN headers
procedure OutgoingMessage(AMessage: POPStackMessage);                           // Expects that IsOutgoingBufferAvailable was called and returned True to ensure success in transmitting
procedure ProcessHardwareMessages;
function IsOutgoingBufferAvailable: Boolean;

{$IFNDEF FPC}
// Callback to push received messages into the OPStack
function OPStackCANStatemachine_GridConnectBufferToGridConnect(var GridConnectBuffer: TNMRAnetCanBuffer; var GridConnectStr: TGridConnectString): Integer; external;
procedure OPStackCANStatemachine_AddOutgoingDatagramMessage(OPStackDatagramMessage: POPStackMessage); external;
procedure OPStackCANStatemachine_AddOutgoingAcdiSnipMessage(OPStackAcdiSnipMessage: POPStackMessage); external;
procedure OPStackCANStatemachine_AddOutgoingStreamMessage(OPStackDatagramMessage: POPStackMessage); external;
procedure OPStackCANStatemachine_ProcessMessages; external;
{$ENDIF}

{$IFNDEF FPC}
// Callback to push received messages into the OPStack
procedure IncomingMessageDispatch(AMessage: POPStackMessage; DestNode: PNMRAnetNode); external;
function OPStackNode_FindByAlias(AliasID: Word): PNMRAnetNode; external;
{$ENDIF}

implementation

{$IFDEF FPC}
uses
  opstackcore,
  opstackcanstatemachines;
{$ENDIF}

type
  THardware = record
    InterruptDisableCount: Integer;                                                // Allows stacked calls to Disable/Enable Interrupts (if used)
  end;

var
  Hardware: THardware;

procedure Hardware_Initialize;
begin
  Hardware.InterruptDisableCount := 0;
  GridConnect_Initialize;                                                       // This is here because it only is needed if we are using a GridConnect interface
end;

// *****************************************************************************
//  procedure Hardware_DisableInterrupts
//    Parameters:
//    Result:
//    Description:   called when lists or similar are being maniuplated that
//                   could be in an interterminate state if an interrupt driven
//                   system is called in the middle of the manipulation. Use this
//                   function to disable asyncronous access to library variable
//                   during this call
// *****************************************************************************
procedure Hardware_DisableInterrupts;
begin
  // Disable Any interrupts here
  Inc(Hardware.InterruptDisableCount);
end;

// *****************************************************************************
//  procedure Hardware_EnableInterrupts
//    Parameters:
//    Result:
//    Description:   called when lists or similar are being maniuplated that
//                   could be in an interterminate state if an interrupt driven
//                   system is called in the middle of the manipulation. Use this
//                   function to re enable asyncronous access to library variable
//                   during this call
// *****************************************************************************
procedure Hardware_EnableInterrupts;
begin
  Dec(Hardware.InterruptDisableCount);
  if Hardware.InterruptDisableCount <= 0 then
  begin
    // Enable any Interrupts here
    Hardware.InterruptDisableCount := 0;
  end;
end;

procedure OutgoingCriticalMessage(AMessage: POPStackMessage);
begin
  OutgoingMessage(AMessage)  // For Ethernet this is no different than a nomral Message
end;

// *****************************************************************************
//  procedure OutgoingMessage
//    Parameters:
//    Result:
//    Description:   called to send a message on the physical layer, must call
//                   IsOutgoingBufferAvailable before to ensure a buffer is
//                   available to use
// *****************************************************************************
procedure OutgoingMessage(AMessage: POPStackMessage);
var
  GridConnectStr: TGridConnectString;
  NMRAnetCanBuffer: TNMRAnetCanBuffer;
begin
  GridConnectStr[0] := #0;                                                       // Quiet the compiler
  case AMessage^.MessageType and MT_MASK of
    MT_SIMPLE :
        begin
          OPStackCANStatemachine_OPStackMessageToNMRAnetCanBuffer(AMessage, @NMRAnetCanBuffer);
          GridConnect_BufferToGridConnect(NMRAnetCanBuffer, GridConnectStr);
          OPStackBuffers_DeAllocateMessage(AMessage);

          {$IFDEF FPC}
          if Assigned(ListenerThread) then
            ListenerThread.Send(GridConnectStr)
          else if Assigned(ClientThread) then
            ClientThread.Send(GridConnectStr);
          {$ENDIF}
        end;
    MT_DATAGRAM : OPStackCANStatemachineBuffers_AddOutgoingDatagramMessage(AMessage);  // CAN can't handle a full Datagram Message so we need to parse it up into MT_SIMPLE frames
    MT_ACDISNIP : OPStackCANStatemachineBuffers_AddOutgoingAcdiSnipMessage(AMessage);  // CAN can't handle a full Datagram Message so we need to parse it up into MT_SIMPLE frames
    {$IFDEF SUPPORT_STREAMS}
    MT_STREAM   : OPStackCANStatemachineBuffers_AddOutgoingStreamMessage(AMessage)     // CAN can't handle a full Datagram Message so we need to parse it up into MT_SIMPLE frames
    {$ENDIF}
  else
     OPStackBuffers_DeAllocateMessage(AMessage);
  end;
end;

function IsOutgoingBufferAvailable: Boolean;
begin
  Result := True
end;

procedure ProcessHardwareMessages;
begin
  if IsOutgoingBufferAvailable then
    OPStackCANStatemachine_ProcessMessages;
end;

// *****************************************************************************
//  procedure DispatchGridConnectStr
//    Parameters:
//    Result:
//    Description:   called to setup a received message in GridConnect format into
//                   a format the OPStack can use.  NOTE: The buffer that is sent to
//                   IncomingMessageCallback is NOT from the allocated buffer Pool
//                   and will be gone when this function returns.  You MUST copy
//                   the contents of it if needed
// *****************************************************************************
procedure DispatchGridConnectStr(GridConnectStrPtr: PGridConnectString);
var
  GridConnectBuffer: TNMRAnetCanBuffer;
  OPStackMessage: TOPStackMessage;
  OPStackMessagePtr: POPStackMessage;
  Buffer: TSimpleBuffer;
  SourceNode, DestNode: PNMRAnetNode;
begin
  GridConnectBuffer.MTI := 0;                                                   // Quite the compiler
  OPStackBuffers_ZeroMessage(@OPStackMessage);
  OPStackBuffers_ZeroSimpleBuffer(@Buffer, False);
  OPStackMessage.Buffer := @Buffer;
  GridConnect_ToGridConnectBuffer(GridConnectStrPtr, GridConnectBuffer);        // Parse the string into a Grid Connect Data structure
  OPStackMessagePtr := @OPStackMessage;                                         // The message object may change on us if it is a datagram, stream or SNIP/ACDI
  if OPStackCANStatemachine_NMRAnetCanBufferToOPStackBuffer(GridConnectBuffer, OPStackMessagePtr, DestNode, SourceNode) then // Convert the Grid Connect Data structure into an OPStack Message and dispatch it to the core case statement
  begin
    if OPStackMessagePtr^.MessageType and MT_HIGH_PRIORITY_SEND  <> 0 then      // The incoming message may not be able to be handled and we need to reply with a fast answer to a buffer that can never be full
      OutgoingCriticalMessage(OPStackMessagePtr)
    else
      IncomingMessageDispatch(OPStackMessagePtr, DestNode, SourceNode);
  end;
end;


// ***************************************************************************************************************************************************************************************************************************************
//  Lazarus Specific from here down
// ***************************************************************************************************************************************************************************************************************************************

{$IFDEF FPC}

{ TOPstackTestClient }

destructor TOPstackTestClient.Destroy;
begin
  RTLeventdestroy(Event);
  inherited Destroy;
end;

procedure TOPstackTestClient.Execute;
begin
 Event := RTLEventCreate;
 Socket := TTCPBlockSocket.Create;
  try
    Socket.CreateSocket;
    Socket.Family := SF_IP4;
    Socket.Bind('0.0.0.0', '12022');
    Socket.ConvertLineEnd := True;      // Use #10, #13, or both to be a "string"
    Socket.Connect('0.0.0.0', '12021');

    if Socket.LastError = 0 then
    begin
      ConnectionInput := TOPStackTestConnectionInput.Create(True);
      ConnectionInput.hSocket := Socket.Socket;
      ConnectionInput.Callback := Callback;
      ConnectionInput.RunningCallback := RunningCallback;
      ConnectionInput.Start;

      ConnectionOutput := TOPStackTestConnectionOutput.Create(True);
      ConnectionOutput.hSocket := Socket.Socket;
      ConnectionOutput.RunningCallback := RunningCallback;
      ConnectionOutput.Callback := Callback;
      ConnectionOutput.Start;
    end;


  RTLEventWaitFor(Event);

  finally
    if Assigned(ConnectionInput) then
    begin
      ConnectionInput.Terminate;
      ConnectionInput.Abort;
      while not ConnectionInput.HasTerminated do
        ThreadSwitch;
      FreeAndNil(FConnectionInput);
    end;
    if Assigned(ConnectionOutput) then
    begin
      ConnectionOutput.Terminate;
      RTLeventSetEvent(ConnectionOutput.Event);
      ConnectionOutput.Abort;
      ConnectionOutput.Terminate;
      while not ConnectionOutput.HasTerminated do
        ThreadSwitch;
      FreeAndNil(FConnectionOutput);
    end;
    Socket.CloseSocket;
    FHasTerminated := True;
  end;
end;

{ TOPStackTestConnectionOutput }

constructor TOPStackTestConnectionOutput.Create(CreateSuspended: Boolean; const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);
  SendList := TThreadList.Create;
  Callback := nil;
  RunningCallback := nil;
  FHasTerminated := False;
end;

destructor TOPStackTestConnectionOutput.Destroy;
var
  i: Integer;
  List: TList;
begin
  List := SendList.LockList;
  try
    for i := 0 to List.Count - 1 do
      TObject( List[i]).Free;
  finally
    List.Clear;
    SendList.UnlockList;
  end;
  FreeAndNil(FSocket);
  RTLeventdestroy(Event);
  inherited Destroy;
end;

procedure TOPStackTestConnectionOutput.Abort;
begin
  if Assigned(Socket) then
    Socket.AbortSocket;
end;

procedure TOPStackTestConnectionOutput.Execute;
var
  List: TList;
  i: Integer;
begin
  Event := RTLEventCreate;
  Socket := TTCPBlockSocket.Create;
  try
    Socket.CreateSocket;
    Socket.Socket := hSocket;
    Socket.ConvertLineEnd := True;      // Use #10, #13, or both to be a "string"
    Socket.GetSins;                     // Back load the IP's / Ports information from the handle
    Synchronize(@RunningSynchronizer);
    while not Terminated do
    begin
      RTLEventWaitFor(Event);
      if not Terminated then
      begin
        List := SendList.LockList;
        try
          SendString := '';
          for i := 0 to List.Count - 1 do
            SendString := SendString + TStringList( List[i]).Text;
          TStringList( List[i]).Free;
        finally
          List.Clear;
          SendList.UnlockList;
        end;
        Socket.ResetLastError;
        Socket.SendString(SendString);
        if Assigned(Callback) then
        begin
          SendString := SendString;
          Synchronize(@Synchronizer);
        end;
      end;
    end
  finally
    Socket.CloseSocket;
    FHasTerminated := True;
  end;
end;

procedure TOPStackTestConnectionOutput.RunningSynchronizer;
begin
  if Assigned(RunningCallback) then
    RunningCallback(ETT_Writer)
end;

procedure TOPStackTestConnectionOutput.Synchronizer;
begin
  if Assigned(Callback) then
    Callback(SendString)
end;

procedure TOPStackTestConnectionOutput.Send(StringList: TStringList);
var
  List: TList;
begin
  List := SendList.LockList;
  try
    List.Add(StringList);
  finally
    SendList.UnlockList;
  end;
  RTLeventSetEvent(Event);
end;

{ TOPStackTestConnectionInput }

constructor TOPStackTestConnectionInput.Create(CreateSuspended: Boolean; const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);
  Callback := nil;
  RunningCallback := nil;
  ReceiveStr := '';
  FHasTerminated := False;
end;

destructor TOPStackTestConnectionInput.Destroy;
begin
  Callback := nil;
  RunningCallback := nil;
  FreeAndNil(Socket);
  inherited Destroy;
end;

procedure TOPStackTestConnectionInput.Abort;
begin
  if Assigned(Socket) then
    Socket.AbortSocket;
end;

procedure TOPStackTestConnectionInput.Execute;
begin
  Socket := TTCPBlockSocket.Create;
  try
    Socket.CreateSocket;
    Socket.Socket := hSocket;
    Socket.ConvertLineEnd := True;      // Use #10, #13, or both to be a "string"
    Socket.GetSins;                     // Back load the IP's / Ports information from the handle
    Synchronize(@RunningSynchronizer);

    while not Terminated do
    begin
      Socket.ResetLastError;
      if Socket.LastError = 0 then
      begin
        ReceiveStr := Socket.RecvPacket(-1);
        if not Terminated then
        begin
          if Socket.LastError <> WSAETIMEDOUT then
          begin
            if (Socket.LastError = 0) and (ReceiveStr <> '') then
              if Assigned(Callback) then
                Synchronize(@Synchronizer);
          end
        end
      end
    end;
  finally
    Socket.CloseSocket;
    FHasTerminated := True;
  end;
end;

procedure TOPStackTestConnectionInput.Synchronizer;
var
  GridConnectStrPtr: PGridConnectString;
  Str: ansistring;
  i, StringLen: Integer;
begin
  for i := 1 to Length(ReceiveStr) do
  begin
    GridConnectStrPtr := nil;
    if GridConnect_DecodeMachine(ReceiveStr[i], GridConnectStrPtr) then
    begin
      DispatchGridConnectStr(GridConnectStrPtr);
      StringLen := strlen(GridConnectStrPtr^);
      SetLength(Str, StringLen);
      Str := GridConnectStrPtr^;
      if Assigned(Callback) then
        Callback(GridConnectStrPtr^ + LF)
    end
  end;
end;

procedure TOPStackTestConnectionInput.RunningSynchronizer;
begin
  if Assigned(RunningCallback) then
    RunningCallback(ETT_Reader);
end;


{ TOPStackTestListener }

constructor TOPStackTestListener.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);
  Callback := nil;
  RunningCallback := nil;
  ConnectionOutput := nil;
  ConnectionInput := nil;
  FHasTerminated := False;
end;

destructor TOPStackTestListener.Destroy;
begin
  FreeAndNil(Socket);
  inherited Destroy;
end;

procedure TOPStackTestListener.Abort;
begin
  if Assigned(Socket) then
    Socket.AbortSocket
end;

procedure TOPStackTestListener.Execute;
begin
  Socket := TTCPBlockSocket.Create;
  try
    Socket.CreateSocket;
    Socket.Family := SF_IP4;
    Socket.Bind('0.0.0.0', '12022');
    Socket.SetLinger(True, 1000);
    Socket.ConvertLineEnd := True;      // Use #10, #13, or both to be a "string"
    Socket.Listen;
    while not Terminated do
    begin
      Socket.ResetLastError;
      if Socket.CanRead(-1) then
      begin
        if Socket.LastError = 0 then
        begin
          if Assigned(ConnectionOutput) then
          begin
            ConnectionOutput.Abort;
            ConnectionOutput.Terminate;
            while not ConnectionOutput.HasTerminated do
              ThreadSwitch;
            FreeAndNil(FConnectionOutput);
          end;
          if Assigned(ConnectionInput) then
          begin
            ConnectionInput.Abort;
            ConnectionInput.Terminate;
            while not ConnectionInput.HasTerminated do
              ThreadSwitch;
            FreeAndNil(FConnectionInput);
          end;

          ConnectionInput := TOPStackTestConnectionInput.Create(True);
          ConnectionInput.hSocket := Socket.Accept;
          ConnectionInput.Callback := Callback;
          ConnectionInput.RunningCallback := RunningCallback;
          ConnectionInput.Start;

          ConnectionOutput := TOPStackTestConnectionOutput.Create(True);
          ConnectionOutput.hSocket := ConnectionInput.hSocket;
          ConnectionOutput.RunningCallback := RunningCallback;
          ConnectionOutput.Callback := Callback;
          ConnectionOutput.Start;
        end
      end
    end;
  finally
    if Assigned(ConnectionInput) then
    begin
      ConnectionInput.Terminate;
      ConnectionInput.Abort;
      while not ConnectionInput.HasTerminated do
        ThreadSwitch;
      FreeAndNil(FConnectionInput);
    end;
    if Assigned(ConnectionOutput) then
    begin
      ConnectionOutput.Terminate;
      RTLeventSetEvent(ConnectionOutput.Event);
      ConnectionOutput.Abort;
      ConnectionOutput.Terminate;
      while not ConnectionOutput.HasTerminated do
        ThreadSwitch;
      FreeAndNil(FConnectionOutput);
    end;
    Socket.CloseSocket;
    FHasTerminated := True;
  end;
end;

procedure TOPStackTestListener.Send(MessageStr: ansistring);
var
  NewStringList: TStringList;
begin
  if (MessageStr <> '') and not Terminated then
  begin
    NewStringList := TStringList.Create;
    NewStringList.Add( MessageStr);
    ConnectionOutput.Send(NewStringList);
  end;
end;

{$ENDIF}


end.
