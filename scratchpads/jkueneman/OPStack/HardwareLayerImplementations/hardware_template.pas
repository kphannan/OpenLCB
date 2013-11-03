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
  nmranetdefines,
  opstackbuffers,
  opstackdefines;

// *****************************************************************************
//  Lazarus Specific from here down
// *****************************************************************************
{$IFDEF FPC}
type
  TEthernetThreadType = (ETT_Reader, ETT_Writer);
  TOpStackTestCallbackMethod = procedure(ReceivedStr: ansistring) of object;
  TOpStackTestRunningCallbackMethod = procedure(EthernetThreadType: TEthernetThreadType) of object;

  { TOPStackTestListener }

  TOPStackTestListener = class(TThread)
  private
    FCallback: TOpStackTestCallbackMethod;
    FConnectionInputList: TList;
    FConnectionOutputList: TList;
    FRunningCallback: TOpStackTestRunningCallbackMethod;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
    procedure Execute; override;
    procedure Send(StringList: TStringList);

    property Callback: TOpStackTestCallbackMethod read FCallback write FCallback;
    property ConnectionInputList: TList read FConnectionInputList write FConnectionInputList;
    property ConnectionOutputList: TList read FConnectionOutputList write FConnectionOutputList;
    property RunningCallback: TOpStackTestRunningCallbackMethod read FRunningCallback write FRunningCallback;
  end;

  { TOPStackTestConnectionInput }

  TOPStackTestConnectionInput = class(TThread)
  private
    FCallback: TOpStackTestCallbackMethod;
    FhSocket: TSocket;
    FReceiveStr: string;
    FRunningCallback: TOpStackTestRunningCallbackMethod;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
    procedure Execute; override;
    procedure Synchronizer;
    procedure RunningSynchronizer;
  public
    property hSocket: TSocket read FhSocket write FhSocket;
    property ReceiveStr: string read FReceiveStr write FReceiveStr;
    property Callback: TOpStackTestCallbackMethod read FCallback write FCallback;
    property RunningCallback: TOpStackTestRunningCallbackMethod read FRunningCallback write FRunningCallback;
  end;

  { TOPStackTestConnectionOutput }

  TOPStackTestConnectionOutput = class(TThread)
  private
    FCallback: TOpStackTestCallbackMethod;
    FEvent: PRTLEvent;
    FhSocket: TSocket;
    FRunningCallback: TOpStackTestRunningCallbackMethod;
    FSendList: TThreadList;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
    procedure Execute; override;
    procedure RunningSynchronizer;
  public
    procedure Send(StringList: TStringList);
    property hSocket: TSocket read FhSocket write FhSocket;
    property SendList: TThreadList read FSendList write FSendList;
    property Event: PRTLEvent read FEvent write FEvent;
    property Callback: TOpStackTestCallbackMethod read FCallback write FCallback;
    property RunningCallback: TOpStackTestRunningCallbackMethod read FRunningCallback write FRunningCallback;
  end;

var
  ListenerThread: TOPStackTestListener;
{$ENDIF}
// ***************************************************************************************************************************************************************************************************************************************
//  Lazarus Specific from here up
// ***************************************************************************************************************************************************************************************************************************************


procedure Hardware_Initialize;

// The OPStack calles these functions to control and/or send messages through the hardware layer
procedure Hardware_DisableInterrupts;
procedure Hardware_EnableInterrupts;

procedure OutgoingMessage(AMessage: PSimpleMessage);                                   // Expects that IsOutgoingBufferAvailable was called and returned True to ensure success in transmitting
function IsOutgoingBufferAvailable: Boolean;

{$IFNDEF FPC}
// Callback to push received messages into the OPStack
procedure IncomingMessageCallback(AMessage: PSimpleMessage); external;
{$ENDIF}

implementation

{$IFDEF FPC}
uses
  opstackcore;
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

// *****************************************************************************
//  procedure OutgoingMessage
//    Parameters:
//    Result:
//    Description:   called to send a message on the physical layer, must call
//                   IsOutgoingBufferAvailable before to ensure a buffer is
//                   available to use
// *****************************************************************************
procedure OutgoingMessage(AMessage: PSimpleMessage);
var
  GridConnectStr: TGridConnectString;
  GridConnectBuffer: TGridConnectBuffer;
  StringList: TStringList;
  i: Integer;
begin
  case AMessage^.MessageType and MT_MASK of
    MT_SIMPLE:
        begin
          GridConnectBuffer.PayloadCount := 0;
          if AMessage^.MTI and MTI_ADDRESSED_MASK = MTI_ADDRESSED_MASK then
          begin
            GridConnectBuffer.Payload[0] := Hi( AMessage^.Dest.AliasID);
            GridConnectBuffer.Payload[1] := Lo( AMessage^.Dest.AliasID);
            GridConnectBuffer.PayloadCount := 2;
          end;
          for i := 0 to AMessage^.Buffer^.DataBufferSize - 1 do
          begin
            GridConnectBuffer.Payload[GridConnectBuffer.PayloadCount] := AMessage^.Buffer^.DataArray[i];
            Inc(GridConnectBuffer.PayloadCount);
          end;
          if AMessage^.MessageType and MT_CAN_TYPE = 0 then
            GridConnectBuffer.MTI := (AMessage^.MTI shl 12) or AMessage^.Source.AliasID or $19000000
          else begin
            case AMessage^.MTI of
              MTI_CAN_CID0 : GridConnectBuffer.MTI := DWord(AMessage^.MTI shl 12) or ((AMessage^.Source.ID[1] shr 12) and $00000FFF) or $10000000;
              MTI_CAN_CID1 : GridConnectBuffer.MTI := DWord(AMessage^.MTI shl 12) or (AMessage^.Source.ID[1] and $00000FFF) or $10000000;
              MTI_CAN_CID2 : GridConnectBuffer.MTI := DWord(AMessage^.MTI shl 12) or ((AMessage^.Source.ID[0] shr 12) and $00000FFF) or $10000000;
              MTI_CAN_CID3 : GridConnectBuffer.MTI := DWord(AMessage^.MTI shl 12) or (AMessage^.Source.ID[0] and $00000FFF) or $10000000;
            else
              GridConnectBuffer.MTI := (AMessage^.MTI shl 12) or AMessage^.Source.AliasID or $10000000;
            end;
          end;
          GridConnectBufferToGridConnect(GridConnectBuffer, GridConnectStr);
          {$IFDEF FPC}
          StringList := TStringList.Create;
          StringList.Add(GridConnectStr);
          ListenerThread.Send(StringList);
          {$ENDIF}
        end;
    MT_DATAGRAM :
        begin

        end;
    MT_STREAM :
        begin

        end;
  end;
  OPStack_DeAllocateMessage(AMessage);
end;

function IsOutgoingBufferAvailable: Boolean;
begin
  Result := True
end;

// *****************************************************************************
//  procedure GridConnectStrToIncomingMessageCallback
//    Parameters:
//    Result:
//    Description:   called to setup a received message in GridConnect format into
//                   a format the OPStack can use.  NOTE: The buffer that is sent to
//                   IncomingMessageCallback is NOT from the allocated buffer Pool
//                   and will be gone when this function returns.  You MUST copy
//                   the contents of it if needed
// *****************************************************************************
procedure GridConnectStrToIncomingMessageCallback(GridConnectStr: PGridConnectString);
var
  GridConnectBuffer: TGridConnectBuffer;
  SimpleMessage: TSimpleMessage;
  Buffer: TSimpleBuffer;
  i: Integer;
begin
  GridConnectToGridConnectBuffer(GridConnectStr, GridConnectBuffer);
  SimpleMessage.MTI := GridConnectBuffer.MTI shr 12;
  if SimpleMessage.MTI and MTI_FRAME_TYPE_CAN_GENERAL <= MTI_FRAME_TYPE_CAN_GENERAL then
  begin
    // These are the only messages that map to a full MTI
    SimpleMessage.MTI := SimpleMessage.MTI and not MTI_FRAME_TYPE_CAN_GENERAL;
    SimpleMessage.Source.AliasID := GridConnectBuffer.MTI and $00000FFF;
    SimpleMessage.Source.ID := NULL_NODE_ID;
    if SimpleMessage.MTI and MTI_ADDRESSED_MASK = MTI_ADDRESSED_MASK then
    begin
      SimpleMessage.Dest.AliasID := ((GridConnectBuffer.Payload[0] shl 8) or GridConnectBuffer.Payload[1]) and $0FFF;
      SimpleMessage.DestFlags := GridConnectBuffer.Payload[0] and $F0;
    end else
      SimpleMessage.Dest.AliasID := 0;
    SimpleMessage.Dest.ID := NULL_NODE_ID;
    SimpleMessage.MessageType := MT_SIMPLE or MT_ALLOCATED;
    SimpleMessage.Next := nil;
    Buffer.DataBufferSize := GridConnectBuffer.PayloadCount;
    for i := 0 to Buffer.DataBufferSize - 1 do
      Buffer.DataArray[i] := GridConnectBuffer.Payload[i];
    Buffer.iStateMachine := 0;
    Buffer.State := 0;
    SimpleMessage.Buffer := @Buffer;
    IncomingMessageCallback(@SimpleMessage);
  end else
  if SimpleMessage.MTI and MTI_FRAME_TYPE_CAN_STREAM_SEND = MTI_FRAME_TYPE_CAN_STREAM_SEND then
  begin
    // It is a CAN Stream Frame, need to collect them all then call into the library with a Stream full MTI
  end else
  if SimpleMessage.MTI and MTI_FRAME_TYPE_CAN_DATAGRAM_ONLY_FRAME >= MTI_FRAME_TYPE_CAN_DATAGRAM_ONLY_FRAME then
  begin
    // It is a CAN Datagram Frame, need to collect them all then call into the library with a Datagram full MTI
  end
end;


// ***************************************************************************************************************************************************************************************************************************************
//  Lazarus Specific from here down
// ***************************************************************************************************************************************************************************************************************************************

{$IFDEF FPC}
{ TOPStackTestConnectionOutput }

constructor TOPStackTestConnectionOutput.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);
  SendList := TThreadList.Create;
  Callback := nil;
  RunningCallback := nil;
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
  inherited Destroy;
end;

procedure TOPStackTestConnectionOutput.Execute;
var
  Socket: TTCPBlockSocket;
  List: TList;
  SendString: ansistring;
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
        Callback('Sent: '+SendString)
    end;
  finally
    Socket.CloseSocket;
    Socket.Free;
    RTLeventdestroy(Event);
  end;
end;

procedure TOPStackTestConnectionOutput.RunningSynchronizer;
begin
  if Assigned(RunningCallback) then
    RunningCallback(ETT_Writer)
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
end;

destructor TOPStackTestConnectionInput.Destroy;
begin
  Callback := nil;
  inherited Destroy;
end;

procedure TOPStackTestConnectionInput.Execute;
var
  Socket: TTCPBlockSocket;
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
        if Socket.LastError <> WSAETIMEDOUT then
        begin
          if (Socket.LastError = 0) and (ReceiveStr <> '') then
            if Assigned(Callback) then
              Synchronize(@Synchronizer);
        end
      end
    end;
  finally
    Socket.CloseSocket;
    Socket.Free;
  end;
end;

procedure TOPStackTestConnectionInput.Synchronizer;
var
  GridConnectStr: PGridConnectString;
  Str: string;
  i, StringLen: Integer;
begin
  for i := 1 to Length(ReceiveStr) do
  begin
    GridConnectStr := GridConnectDecodeMachine(ReceiveStr[i]);
    if GridConnectStr <> nil then
    begin
      GridConnectStrToIncomingMessageCallback(GridConnectStr);
      StringLen := strlen(GridConnectStr^);
      SetLength(Str, StringLen);
      Str := GridConnectStr^;
      if Assigned(Callback) then
        Callback('Received: '+GridConnectStr^+LF)
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
  ConnectionOutputList := TList.Create;
  ConnectionInputList := TList.Create;
end;

destructor TOPStackTestListener.Destroy;
var
  i: Integer;
begin
  try
    for i := 0 to ConnectionOutputList.Count - 1 do
      TOPStackTestConnectionOutput(ConnectionOutputList[i]).Terminate;
  finally
    ConnectionOutputList.Clear
  end;
  FreeAndNil(FConnectionOutputList);
  try
    for i := 0 to ConnectionInputList.Count - 1 do
      TOPStackTestConnectionInput(ConnectionInputList[i]).Terminate;
  finally
    ConnectionInputList.Clear
  end;
  FreeAndNil(FConnectionInputList);
  inherited Destroy;
end;

procedure TOPStackTestListener.Execute;
var
  Socket: TTCPBlockSocket;
  NewConnectionInput: TOPStackTestConnectionInput;
  NewConnectionOutput: TOPStackTestConnectionOutput;
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
          NewConnectionInput := TOPStackTestConnectionInput.Create(True);
          NewConnectionInput.hSocket := Socket.Accept;
          NewConnectionInput.Callback := Callback;
          NewConnectionInput.RunningCallback := RunningCallback;
          NewConnectionInput.Start;
          ConnectionInputList.Add(NewConnectionInput);
          NewConnectionOutput := TOPStackTestConnectionOutput.Create(True);
          NewConnectionOutput.hSocket := NewConnectionInput.hSocket;
          NewConnectionOutput.RunningCallback := RunningCallback;
          NewConnectionOutput.Callback := Callback;
          NewConnectionOutput.Start;
          ConnectionOutputList.Add(NewConnectionOutput);
        end
      end
    end;
  finally
    Socket.CloseSocket;
    Socket.Free;
  end;
end;

procedure TOPStackTestListener.Send(StringList: TStringList);
var
  NewStringList: TStringList;
  i: Integer;
begin
  if ConnectionOutputList.Count > 1 then
  begin
    for i := 1 to ConnectionOutputList.Count - 1 do
    begin
      NewStringList := TStringList.Create;
      NewStringList.Assign( StringList);
      TOPStackTestConnectionOutput( ConnectionOutputList[i]).Send(NewStringList);
    end;
  end;
  if ConnectionOutputList.Count > 0 then
    TOPStackTestConnectionOutput( ConnectionOutputList[0]).Send( StringList);
end;

{$ENDIF}


end.
