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
  nmranetutilities,
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
    FSendString: ansistring;
    FSocket: TTCPBlockSocket;
  protected
    property Socket: TTCPBlockSocket read FSocket write FSocket;
    property SendString: ansistring read FSendString write FSendString;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
    procedure Execute; override;
    procedure RunningSynchronizer;
    procedure Synchronizer;
    procedure Send(StringList: TStringList);
    property hSocket: TSocket read FhSocket write FhSocket;
    property SendList: TThreadList read FSendList write FSendList;
    property Event: PRTLEvent read FEvent write FEvent;
    property Callback: TOpStackTestCallbackMethod read FCallback write FCallback;
    property RunningCallback: TOpStackTestRunningCallbackMethod read FRunningCallback write FRunningCallback;
  end;

  { TOPStackTestListener }

  TOPStackTestListener = class(TThread)
  private
    FCallback: TOpStackTestCallbackMethod;
    FConnectionInput: TOPStackTestConnectionInput;
    FConnectionOutput: TOPStackTestConnectionOutput;
    FRunningCallback: TOpStackTestRunningCallbackMethod;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
    procedure Execute; override;
    procedure Send(MessageStr: ansistring);

    property Callback: TOpStackTestCallbackMethod read FCallback write FCallback;
    property ConnectionInput: TOPStackTestConnectionInput read FConnectionInput write FConnectionInput;
    property ConnectionOutput: TOPStackTestConnectionOutput read FConnectionOutput write FConnectionOutput;
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

procedure OutgoingCriticalMessage(AMessage: POPStackMessage);                   // Called _back_ from within the IncomingMessageDispatch if we can't allocate buffers, unknown MTI's etc.  For CAN this is expected to be immediatly replied back to the sender as these are very high priority CAN headers
procedure OutgoingMessage(AMessage: POPStackMessage);                           // Expects that IsOutgoingBufferAvailable was called and returned True to ensure success in transmitting
procedure ProcessOutgoingDatagrams;
procedure ProcessOutgoingStreams;
procedure ProcessOutgoingAcdiSnips;
function FindDatagramWaitingForAck(var SourceNodeID: TNodeInfo; var DestNodeID: TNodeInfo): POPStackMessage;
procedure AddOutgoingDatagramMessage(OPStackDatagramMessage: POPStackMessage);
procedure RemoveDatagramWaitingForAck(DatagramMessage: POPStackMessage);
function IsOutgoingBufferAvailable: Boolean;

{$IFNDEF FPC}
// Callback to push received messages into the OPStack
procedure IncomingMessageDispatch(AMessage: POPStackMessage; DestNode: PNMRAnetNode); external;
function OPStackCANStatemachine_ProcessIncomingDatagramMessage(OPStackMessage: POPStackMessage): POPStackMessage; external;
function OPStackNode_FindByAlias(AliasID: Word): PNMRAnetNode; external;
procedure OPStackCANStatemachine_ProcessOutgoingDatagramMessage; external;
procedure OPStackCANStatemachine_AddOutgoingDatagramMessage(OPStackDatagramMessage: POPStackMessage); external;
procedure OPStackCANStatemachine_RemoveOutgoingDatagramMessage(OPStackDatagramMessage: POPStackMessage); external;
procedure OPStackCANStatemachine_RemoveDatagramWaitingforACKStack(OPStackStreamMessage: POPStackMessage); external;
function OPStackCANStatemachine_FindDatagramWaitingforACKStack(var SourceNodeID: TNodeInfo; var DestNodeID: TNodeInfo): POPStackMessage; external;
{$ENDIF}

implementation

{$IFDEF FPC}
uses
  opstacknode,
  opstackcanstatemachines,
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
  GridConnectBuffer: TGridConnectBuffer;
  i: Integer;
begin
  case AMessage^.MessageType and MT_MASK of
    MT_SIMPLE :
        begin
          GridConnectBuffer.PayloadCount := 0;

          if AMessage^.MessageType and MT_CAN_TYPE <> 0 then
          begin
            // This is a special case CAN message
            case AMessage^.MTI of
              MTI_CAN_CID0 : GridConnectBuffer.MTI := DWord(AMessage^.MTI shl 12) or ((AMessage^.Source.ID[1] shr 12) and $00000FFF) or $10000000;
              MTI_CAN_CID1 : GridConnectBuffer.MTI := DWord(AMessage^.MTI shl 12) or (AMessage^.Source.ID[1] and $00000FFF) or $10000000;
              MTI_CAN_CID2 : GridConnectBuffer.MTI := DWord(AMessage^.MTI shl 12) or ((AMessage^.Source.ID[0] shr 12) and $00000FFF) or $10000000;
              MTI_CAN_CID3 : GridConnectBuffer.MTI := DWord(AMessage^.MTI shl 12) or (AMessage^.Source.ID[0] and $00000FFF) or $10000000
            else
              GridConnectBuffer.MTI := (AMessage^.MTI shl 12) or AMessage^.Source.AliasID or $10000000;
            end;
          end else
          if AMessage^.MTI and MTI_FRAME_TYPE_MASK <= MTI_FRAME_TYPE_CAN_GENERAL then
          begin
            // This is a general case MTI ($9xxx)
            GridConnectBuffer.MTI := (AMessage^.MTI shl 12) or AMessage^.Source.AliasID or $19000000;
            if AMessage^.MTI and MTI_ADDRESSED_MASK = MTI_ADDRESSED_MASK then
            begin
              GridConnectBuffer.Payload[0] := Hi( AMessage^.Dest.AliasID);
              GridConnectBuffer.Payload[1] := Lo( AMessage^.Dest.AliasID);
              GridConnectBuffer.PayloadCount := 2;
            end;
          end else
          if AMessage^.MTI and MTI_FRAME_TYPE_MASK <= MTI_FRAME_TYPE_CAN_STREAM_SEND then
          begin
            // This is a datagram MTI ($Axxx...$Dxxxx) or stream MTI ($Fxxx)
            GridConnectBuffer.MTI := (AMessage^.MTI shl 12) or (AMessage^.Dest.AliasID shl 12) or AMessage^.Source.AliasID or $10000000;
          end;

          for i := 0 to AMessage^.Buffer^.DataBufferSize - 1 do
          begin
            GridConnectBuffer.Payload[GridConnectBuffer.PayloadCount] := AMessage^.Buffer^.DataArray[i];
            Inc(GridConnectBuffer.PayloadCount);
          end;

          GridConnectBufferToGridConnect(GridConnectBuffer, GridConnectStr);
          OPStackBuffers_DeAllocateMessage(AMessage);

          {$IFDEF FPC}
          ListenerThread.Send(GridConnectStr);
          {$ENDIF}
        end;
    MT_DATAGRAM :
        begin
          OPStackCANStatemachine_AddOutgoingDatagramMessage(AMessage)           // CAN can't handle a full Datagram Message so we need to parse it up into 8 byte frames
        end;
    MT_STREAM :
        begin
          OPStackBuffers_DeAllocateMessage(AMessage);
        end;
    MT_ACDISNIP :
        begin
          OPStackCANStatemachine_AddOutgoingAcdiSnipMessage(AMessage)           // CAN can't handle a full Datagram Message so we need to parse it up into 8 byte frames
        end
  else
     OPStackBuffers_DeAllocateMessage(AMessage);
  end;
end;

procedure ProcessOutgoingDatagrams;
begin
  OPStackCANStatemachine_ProcessOutgoingDatagramMessage;
end;

procedure ProcessOutgoingStreams;
begin

end;

procedure ProcessOutgoingAcdiSnips;
begin
  OPStackCANStatemachine_ProcessOutgoingAcdiSnipMessage;
end;

function FindDatagramWaitingForAck(var SourceNodeID: TNodeInfo; var DestNodeID: TNodeInfo): POPStackMessage;
begin
  Result := OPStackCANStatemachine_FindDatagramWaitingforACKStack(SourceNodeID, DestNodeID)
end;

procedure AddOutgoingDatagramMessage(OPStackDatagramMessage: POPStackMessage);
begin
  OPStackCANStatemachine_AddOutgoingDatagramMessage(OPStackDatagramMessage);
end;

procedure RemoveDatagramWaitingForAck(DatagramMessage: POPStackMessage);
begin
  OPStackCANStatemachine_RemoveDatagramWaitingforACKStack(DatagramMessage)
end;

function IsOutgoingBufferAvailable: Boolean;
begin
  Result := True
end;

procedure GridConnectBufferToOPStackBufferAndDispatch(var GridConnectBuffer: TGridConnectBuffer; var OPStackMessage: TOPStackMessage);
var
  RawMTI, SourceAlias: Word;
  CanMessageType, DestAlias: Word;
  DatagramMessage, AcdiSnipMessage: POPStackMessage;
  DestinationNode: PNMRAnetNode;
  DatagramError: PSimpleDataArray;
  DatagramRejectedMessage: TOPStackMessage;
  DatagramRejectedBuffer: TDatagramBuffer;
  DatagramProcessErrorCode: Byte;
begin
  CanMessageType := (GridConnectBuffer.MTI shr 12) and $F000;                   // always true regardless
  SourceAlias := GridConnectBuffer.MTI and $00000FFF;                           // always true regardless
  if GridConnectBuffer.MTI and MTI_OLCB_MSG = 0 then                            // Is it a CAN message?
  begin
    DestinationNode := nil;
    OPStackMessage.MessageType := MT_SIMPLE or MT_CAN_TYPE;                     // Created on the heap not the pool
    RawMTI := (GridConnectBuffer.MTI shr 12) and $0FFF;
    OPStackBuffers_LoadMessage(@OPStackMessage, RawMTI, SourceAlias, NULL_NODE_ID, 0, NULL_NODE_ID, 0);
    OPStackBuffers_LoadSimpleBuffer(OPStackMessage.Buffer, 0, GridConnectBuffer.PayloadCount, @GridConnectBuffer.Payload, 0);
    case RawMTI of
        MTI_CAN_AMD,       // If another node has reset then we need to clear out any inprocess states with that node
        MTI_CAN_AMR :
            begin
              DatagramMessage := OPStackCANStatemachine_FindAnyDatagramOnOutgoingStack(OPStackMessage.Source.AliasID);
              while DatagramMessage <> nil do
              begin
                OPStackCANStatemachine_RemoveOutgoingDatagramMessage(DatagramMessage);
                OPStackBuffers_DeAllocateMessage(DatagramMessage);
                DatagramMessage := OPStackCANStatemachine_FindAnyDatagramOnOutgoingStack(OPStackMessage.Source.AliasID);
              end;

              DatagramMessage := OPStackCANStatemachine_FindAnyDatagramOnIncomingStack(OPStackMessage.Source.AliasID);
              while DatagramMessage <> nil do
              begin
                OPStackCANStatemachine_RemoveIncomingDatagramMessage(DatagramMessage);
                OPStackBuffers_DeAllocateMessage(DatagramMessage);
                DatagramMessage := OPStackCANStatemachine_FindAnyDatagramOnIncomingStack(OPStackMessage.Source.AliasID);
              end;

              DatagramMessage := OPStackCANStatemachine_FindAnyDatagramOnWaitingForAckStack(OPStackMessage.Source.AliasID);
              while DatagramMessage <> nil do
              begin
                OPStackCANStatemachine_RemoveDatagramWaitingforACKStack(DatagramMessage);
                OPStackBuffers_DeAllocateMessage(DatagramMessage);
                DatagramMessage := OPStackCANStatemachine_FindAnyDatagramOnWaitingForAckStack(OPStackMessage.Source.AliasID);
              end;

              AcdiSnipMessage := OPStackCANStatemachine_FindAnyAcdiSnipOnOutgoingStack(OPStackMessage.Source.AliasID);
              while AcdiSnipMessage <> nil do
              begin
                OPStackCANStatemachine_RemoveAcdiSnipDatagramMessage(AcdiSnipMessage);
                OPStackBuffers_DeAllocateMessage(AcdiSnipMessage);
                AcdiSnipMessage := OPStackCANStatemachine_FindAnyAcdiSnipOnOutgoingStack(OPStackMessage.Source.AliasID);
              end;
            end;
    end;
    IncomingMessageDispatch(@OPStackMessage, DestinationNode);
  end else
  begin
    OPStackMessage.MessageType := MT_SIMPLE;
    case CanMessageType of
      MTI_FRAME_TYPE_CAN_GENERAL :
          begin
            RawMTI := (GridConnectBuffer.MTI shr 12) and $0FFF;
            if RawMTI and MTI_ADDRESSED_MASK = MTI_ADDRESSED_MASK then
            begin                                                                 // It is an addressed message
              DestAlias := ((GridConnectBuffer.Payload[0] shl 8) or GridConnectBuffer.Payload[1]) and $0FFF;
              DestinationNode := OPStackNode_FindByAlias(DestAlias);
              if DestinationNode <> nil then
              begin
                OPStackBuffers_LoadMessage(@OPStackMessage, RawMTI, SourceAlias, NULL_NODE_ID, DestAlias, NULL_NODE_ID, GridConnectBuffer.Payload[0] and $F0);
                OPStackBuffers_LoadSimpleBuffer(OPStackMessage.Buffer, 0, GridConnectBuffer.PayloadCount-2, @GridConnectBuffer.Payload, 2);
                IncomingMessageDispatch(@OPStackMessage, DestinationNode);
              end else
                Exit;                                                             // It was an addressed message but not for any of our nodes
            end else
            begin                                                                 // It was not an addressed message
              OPStackBuffers_LoadMessage(@OPStackMessage, RawMTI, SourceAlias, NULL_NODE_ID, 0, NULL_NODE_ID, 0);
              OPStackBuffers_LoadSimpleBuffer(OPStackMessage.Buffer, 0, GridConnectBuffer.PayloadCount, @GridConnectBuffer.Payload, 0);
              IncomingMessageDispatch(@OPStackMessage, DestinationNode);
            end;
          end;
      MTI_FRAME_TYPE_CAN_DATAGRAM_ONLY_FRAME,
      MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME_START,
      MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME,
      MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME_END :
          begin
            DestAlias := (GridConnectBuffer.MTI shr 12) and $0FFF;
            DestinationNode := OPStackNode_FindByAlias(DestAlias);
            if DestinationNode <> nil then
            begin
              OPStackBuffers_LoadMessage(@OPStackMessage, CanMessageType, SourceAlias, NULL_NODE_ID, DestAlias, NULL_NODE_ID, 0);   // These messages do not have real "MTIs", they are based on the identifier
              OPStackBuffers_LoadSimpleBuffer(OPStackMessage.Buffer, 0, GridConnectBuffer.PayloadCount, @GridConnectBuffer.Payload, 0);
              DatagramProcessErrorCode := OPStackCANStatemachine_ProcessIncomingDatagramMessage(@OPStackMessage, DatagramMessage);
              case DatagramProcessErrorCode of
                DATAGRAM_PROCESS_ERROR_OK                  :
                    begin
                      if DatagramMessage <> nil then
                        IncomingMessageDispatch(DatagramMessage, DestinationNode);
                      Exit;
                    end
              else begin
                  if DatagramMessage <> nil then
                    OPStackBuffers_DeAllocateMessage(DatagramMessage);
                  case DatagramProcessErrorCode of
                    DATAGRAM_PROCESS_ERROR_BUFFER_FULL         : DatagramError := @DATAGRAM_RESULT_REJECTED_BUFFER_FULL;
                    DATAGRAM_PROCESS_ERROR_OUT_OF_ORDER        : DatagramError := @DATAGRAM_RESULT_REJECTED_OUT_OF_ORDER;
                    DATAGRAM_PROCESS_ERROR_SOURCE_NOT_ACCEPTED : DatagramError := @DATAGRAM_RESULT_REJECTED_SOURCE_DATAGRAMS_NOT_ACCEPTED;
                  end;
                  DatagramRejectedMessage.Buffer := @DatagramRejectedBuffer;
                  OPStackBuffers_LoadDatagramRejectedBuffer(@DatagramRejectedMessage, OPStackMessage.Dest.AliasID, OPStackMessage.Dest.ID, OPStackMessage.Source.AliasID, OPStackMessage.Source.ID, DatagramError);
                  OutgoingCriticalMessage(@DatagramRejectedMessage);
                  Exit;
                end;
              end;
            end
          end;
      MTI_FRAME_TYPE_CAN_STREAM_SEND :
          begin

          end;
    end;
  end;
end;

// *****************************************************************************
//  procedure GridConnectStrToIncomingMessageDispatch
//    Parameters:
//    Result:
//    Description:   called to setup a received message in GridConnect format into
//                   a format the OPStack can use.  NOTE: The buffer that is sent to
//                   IncomingMessageCallback is NOT from the allocated buffer Pool
//                   and will be gone when this function returns.  You MUST copy
//                   the contents of it if needed
// *****************************************************************************
procedure GridConnectStrToIncomingMessageDispatch(GridConnectStrPtr: PGridConnectString);
var
  GridConnectBuffer: TGridConnectBuffer;
  OPStackMessage: TOPStackMessage;
  Buffer: TSimpleBuffer;
begin
  OPStackMessage.Buffer := @Buffer;
  GridConnectToGridConnectBuffer(GridConnectStrPtr, GridConnectBuffer);           // Parse the string into a Grid Connect Data structure
  GridConnectBufferToOPStackBufferAndDispatch(GridConnectBuffer, OPStackMessage); // Convert the Grid Connect Data structure into an OPStack Message and dispatch it to the core case statement
end;


// ***************************************************************************************************************************************************************************************************************************************
//  Lazarus Specific from here down
// ***************************************************************************************************************************************************************************************************************************************

{$IFDEF FPC}

{ TOPStackTestConnectionOutput }

constructor TOPStackTestConnectionOutput.Create(CreateSuspended: Boolean; const StackSize: SizeUInt);
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
        SendString := 'Sent: ' + SendString;
        Synchronize(@Synchronizer);
      end;
    end
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
end;

destructor TOPStackTestConnectionInput.Destroy;
begin
  Callback := nil;
  RunningCallback := nil;
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
  GridConnectStrPtr: PGridConnectString;
  Str: ansistring;
  i, StringLen: Integer;
begin
  for i := 1 to Length(ReceiveStr) do
  begin
    if GridConnectDecodeMachine(ReceiveStr[i], GridConnectStrPtr) then
    begin
      GridConnectStrToIncomingMessageDispatch(GridConnectStrPtr);
      StringLen := strlen(GridConnectStrPtr^);
      SetLength(Str, StringLen);
      Str := GridConnectStrPtr^;
      if Assigned(Callback) then
        Callback('Received: ' + GridConnectStrPtr^ + LF)
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
end;

destructor TOPStackTestListener.Destroy;
begin
   if Assigned(ConnectionOutput) then
     ConnectionOutput.Terminate;
   if Assigned(ConnectionInput) then
     ConnectionInput.Terminate;
  inherited Destroy;
end;

procedure TOPStackTestListener.Execute;
var
  Socket: TTCPBlockSocket;
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
            ConnectionOutput.Terminate;
          if Assigned(ConnectionInput) then
            ConnectionInput.Terminate;

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
    Socket.CloseSocket;
    Socket.Free;
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
