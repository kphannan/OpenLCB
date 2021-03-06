unit olcb_transport_layer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, dialogs, olcb_utilities, olcb_defines,
  olcb_app_common_settings, math_float16, Forms, blcksock, synsock, contnrs,
  Controls, opstackdefines, nmranetutilities, threadedstringlist;


var
  LoopTime: DWord; // debugging global

const
  ERROR_NO_MEMORY_CONFIG_PROTOCOL = $00000001;
  ERROR_NO_CDI_PROTOCOL           = $00000002;
  ERROR_NO_CDI_ADDRESS_SPACE      = $00000004;
  ERROR_ADDRESS_SPACE_NOT_PRESENT = $00000008;
  ERROR_ADDRESS_SPACE_READ_ONLY   = $00000010;  // Trying to write to a read only space
  ERROR_ADDRESS_SPACE_WRITE_LARGER_THAN_SPACE = $00000020;
  SPEEDSTEP_DEFAULT = 28;

  DATAGRAM_MAX_RETRYS = 5;

  STATE_DONE = 1000;

  HEADER_MEMCONFIG_OPTIONS_REQUEST: TCANByteArray                     = (DATAGRAM_PROTOCOL_CONFIGURATION, MCP_OP_GET_CONFIG, $00, $00, $00, $00, $00, $00);
  HEADER_MEMCONFIG_SPACE_INFO_UNKNOWN_REQUEST: TCANByteArray          = (DATAGRAM_PROTOCOL_CONFIGURATION, MCP_OP_GET_ADD_SPACE_INFO, $00, $00, $00, $00, $00, $00);


type
  TGridConnectString = array[0..MAX_GRID_CONNECT_LEN-1] of char;
  PGridConnectString = ^TGridConnectString;

  TNodeSendFunc = procedure(AMessage: AnsiString) of object;

type
  TTransportLayerThread         = class;
  TNodeThread                   = class;

  { TNodeTask }

  TNodeTask = class   // Ojbects sent from the UI to the Node Thread to send changes in the statemachine for a node for actions the user wants to occur
  private
    FiStateMachine: Word;
    FiSubStateMachine: Word;
    FLinkedObj: TObject;
    FNextTask: TNodeTask;
    FNodeInfo: TNodeInfo;
    FWatchDog_1s: LongWord;
  public
    FDestNodeInfo: TNodeInfo;
    constructor Create(ANodeInfo: TNodeInfo; ADestNodeInfo: TNodeInfo; AniStateMachine: Word; ALinkedObj: TObject); virtual;
    property NextTask: TNodeTask read FNextTask write FNextTask;
    property iStateMachine: Word read FiStateMachine write FiStateMachine;
    property iSubStateMachine: Word read FiSubStateMachine write FiSubStateMachine;
    property LinkedObj: TObject read FLinkedObj write FLinkedObj;
    property NodeInfo: TNodeInfo read FNodeInfo write FNodeInfo;
    property Watchdog_1s: LongWord read FWatchDog_1s write FWatchDog_1s;
    property DestNodeInfo: TNodeInfo read FDestNodeInfo write FDestNodeInfo;
  end;

  { TNodeTaskAllocateNewNode }

  TNodeTaskAllocateNewNode = class(TNodeTask)
  end;

  { TNodeTaskAllocateDestroyNode }

  TNodeTaskAllocateDestroyNode = class(TNodeTask)
  end;

  { TNodeTaskAllocateTrainByAddress }

  TNodeTaskAllocateTrainByAddress = class(TNodeTask)
  private
    FAddress: Word;
    FFunctionIndex: Word;
    FLong: Boolean;
    FSpeedStep: Byte;
  public
    constructor Create(ANodeInfo: TNodeInfo; ADestNodeInfo: TNodeInfo; AniStateMachine: Word; ALinkedObj: TObject;AnAddress: Word; ASpeedStep: Byte; IsLong: Boolean);
    property Address: Word read FAddress write FAddress;
    property Long: Boolean read FLong write FLong;
    property SpeedStep: Byte read FSpeedStep write FSpeedStep;
    property FunctionIndex: Word read FFunctionIndex write FFunctionIndex;
  end;

  { TNodeTaskAllocateTrain }

  TNodeTaskAllocateTrain = class(TNodeTaskAllocateTrainByAddress)
  end;

  { TNodeTaskReleaseController }

  TNodeTaskReleaseController = class(TNodeTask)

  end;

  { TNodeTaskSpeedDir }

  TNodeTaskSpeedDir = class(TNodeTask)
  private
    FSpeedDir: THalfFloat;
  public
    constructor Create(ANodeInfo: TNodeInfo; ADestNodeInfo: TNodeInfo; AniStateMachine: Word; ALinkedObj: TObject; ASpeedDir: THalfFloat);
    property SpeedDir: THalfFloat read FSpeedDir write FSpeedDir;
  end;

  { TNodeTaskFunction }

  TNodeTaskFunction = class(TNodeTask)
  private
    FAddress: DWord;
    FValue: Word;
  public
    constructor Create(ANodeInfo: TNodeInfo; ADestNodeInfo: TNodeInfo; AniStateMachine: Word; ALinkedObj: TObject; AnAddress: DWord; AValue: Word);
    property Address: DWord read FAddress write FAddress;
    property Value: Word read FValue write FValue;
  end;

  { TNodeTaskFunctionQuery }

  TNodeTaskFunctionQuery = class(TNodeTask)
  private
    FAddress: DWord;
  public
    constructor Create(ANodeInfo: TNodeInfo; ADestNodeInfo: TNodeInfo; AniStateMachine: Word; ALinkedObj: TObject; AnAddress: DWord);
    property Address: DWord read FAddress write FAddress;
  end;

  { TNodeTaskSpeedDirQuery }

  TNodeTaskSpeedDirQuery = class(TNodeTask)

  end;

  { TNodeTaskFindTrains }

  TNodeTaskFindTrains = class(TNodeTask)

  end;

  { TNodeTaskSimpleTrainNodeInfo }

  TNodeTaskSimpleTrainNodeInfo = class(TNodeTask)

  end;

  { TNodeTaskSupportsProtocols }

  TNodeTaskSupportsProtocols = class(TNodeTask)

  end;

  { TNodeTaskReadConfigMemory }

  TNodeTaskReadConfigMemory = class(TNodeTask)
  private
    FAddressSpace: Byte;
    FControl: TObject;
    FCount: DWord;
    FiControl: Integer;
    FiPage: Integer;
    FProtocol: TMemoryStream;
    FCurrentAddress: DWord;
  public
    constructor Create(ANodeInfo: TNodeInfo; ADestNodeInfo: TNodeInfo; AniStateMachine: Word; ALinkedObj: TObject; AnAddressSpace: Byte; AStartAddress: DWord; ACount: DWord; AiPage, AiControl: Integer; AControl: TObject); reintroduce;
    destructor Destroy; override;
    property CurrentAddress: DWord read FCurrentAddress write FCurrentAddress;
    property Count: DWord read FCount write FCount;
    property Protocol: TMemoryStream read FProtocol write FProtocol;
    property AddressSpace: Byte read FAddressSpace write FAddressSpace;         // MSI_xxx contants
    property iControl: Integer read FiControl write FiControl;
    property iPage: Integer read FiPage write FiPage;
    property Control: TObject read FControl write FControl;
  end;

  { TNodeTaskWriteConfigMemory }

  TNodeTaskWriteConfigMemory = class(TNodeTaskReadConfigMemory)
  public
    constructor Create(ANodeInfo: TNodeInfo; ADestNodeInfo: TNodeInfo; AniStateMachine: Word; ALinkedObj: TObject; AnAddressSpace: Byte; AStartAddress: DWord; Stream: TMemoryStream; AiPage, AiControl: Integer; AControl: TObject); reintroduce;
  end;

  { TNodeEvent }

  TNodeEvent = class  // Objects sent from the Node Thread to the UI to signal the UI of events that occured in the Node Thread (mainly in the user statemachine)
  private
    FLinkedObj: TObject;
  public
    FNodeInfo: TNodeInfo;
    constructor Create(ANodeInfo: TNodeInfo; ALinkedObj: TObject); reintroduce;
    property NodeInfo: TNodeInfo read FNodeInfo write FNodeInfo;
    property LinkedObj: TObject read FLinkedObj write FLinkedObj;
  end;

  { TNodeEventProxyAssigned }

  TNodeEventProxyAssigned = class(TNodeEvent)  // Event that the Proxy Node was discovered and its details
  end;

  { TNodeEventNodeAssigned }

  TNodeEventNodeCreated = class(TNodeEvent)
  end;

  { TNodeEventThrottleAssignedToTrain }

  TNodeEventThrottleAssignedToTrain = class(TNodeEvent)
  private
    FTrainNodeInfo: TNodeInfo;
  public
    constructor Create(ANodeInfo: TNodeInfo; ALinkedObj: TObject; ATrainNodeInfo: TNodeInfo); reintroduce;
    property TrainNodeInfo: TNodeInfo read FTrainNodeInfo write FTrainNodeInfo;
  end;

  { TNodeEventFunctionQuery }

  TNodeEventFunctionQuery = class(TNodeEvent)
  private
    FAddress: DWord;
    FValue: Word;
  public
    constructor Create(ANodeInfo: TNodeInfo; ALinkedObj: TObject; AnAddress: DWord; AValue: Word); reintroduce;
    property Address: DWord read FAddress write FAddress;
    property Value: Word read FValue write FValue;
  end;

  { TNodeEventSpeedDirQuery }

  TNodeEventSpeedDirQuery = class(TNodeEvent)
  private
    FSpeedDir: THalfFloat;
  public
    constructor Create(ANodeInfo: TNodeInfo; ALinkedObj: TObject; ASpeedDir: THalfFloat); reintroduce;
    property SpeedDir: THalfFloat read FSpeedDir write FSpeedDir;
  end;

  { TNodeEventTrainInfo }

  TNodeEventTrainInfo = class(TNodeEvent)
  private
    FAddress: Word;
    FFunctions: DWord;
    FSpeed: THalfFloat;
    FSpeedSteps: Byte;
    FTrainConfigValid: Boolean;
  public
    FTrainConfig: TTrainConfig;
    FControllerInfo: TNodeInfo;
    procedure CopyTo(EventTrainInfo: TNodeEventTrainInfo);
    property Speed: THalfFloat read FSpeed write FSpeed;
    property Functions: DWord read FFunctions write FFunctions;
    property ControllerInfo: TNodeInfo read FControllerInfo write FControllerInfo;
    property Address: Word read FAddress write FAddress;
    property SpeedSteps: Byte read FSpeedSteps write FSpeedSteps;
    property TrainConfig: TTrainConfig read FTrainConfig write FTrainConfig;
    property TrainConfigValid: Boolean read FTrainConfigValid write FTrainConfigValid;
  end;

  { TNodeEventIsTrain }

  TNodeEventIsTrain = class(TNodeEvent)

  end;

  { TNodeEventSimpleTrainNodeInfo }

  TNodeEventSimpleTrainNodeInfo = class(TNodeEvent)
  private
    FManufacturer: TStnipBuffer;
    FOwner: TStnipBuffer;
    FRoadName: TStnipBuffer;
    FRoadNumber: TStnipBuffer;
    FTrainClass: TStnipBuffer;
    FTrainName: TStnipBuffer;
  public
    function Clone: TNodeEventSimpleTrainNodeInfo;
    procedure Decode(AMessage: POpStackMessage);
    property RoadName: TStnipBuffer read FRoadName write FRoadName;
    property TrainClass: TStnipBuffer read FTrainClass write FTrainClass;
    property RoadNumber: TStnipBuffer read FRoadNumber write FRoadNumber;
    property TrainName: TStnipBuffer read FTrainName write FTrainName;
    property Manufacturer: TStnipBuffer read FManufacturer write FManufacturer;
    property Owner: TStnipBuffer read FOwner write FOwner;
  end;

  { TNodeEventReleaseController }

  TNodeEventReleaseController = class(TNodeEvent)

  end;

  { TNodeEventSupportsProtocols }

  TNodeEventSupportsProtocols = class(TNodeEvent)
  private
    FACDI: Boolean;
    FCDI: Boolean;
    FDatagram: Boolean;
    FDisplay: Boolean;
    FEdentification: Boolean;
    FEventExchange: Boolean;
    FFDI: Boolean;
    FIdentification: Boolean;
    FMemConfig: Boolean;
    FProtocols: QWord;
    FRemoteButton: Boolean;
    FReservation: Boolean;
    FSimpleProtocol: Boolean;
    FSNIP: Boolean;
    FSTNIP: Boolean;
    FStream: Boolean;
    FTeaching: Boolean;
    FTraction: Boolean;
    FTractionProxy: Boolean;
  public
    constructor Create(ANodeInfo: TNodeInfo; ALinkedObj: TObject); reintroduce;
    property SimpleProtocol: Boolean read FSimpleProtocol write FSimpleProtocol;
    property Datagram: Boolean read FDatagram write FDatagram;
    property Stream: Boolean read FStream write FStream;
    property MemConfig: Boolean read FMemConfig write FMemConfig;
    property Reservation: Boolean read FReservation write FReservation;
    property EventExchange: Boolean read FEventExchange write FEventExchange;
    property Identification: Boolean read FIdentification write FEdentification;
    property Teaching: Boolean read FTeaching write FTeaching;
    property RemoteButton: Boolean read FRemoteButton write FRemoteButton;
    property ACDI: Boolean read FACDI write FACDI;
    property Display: Boolean read FDisplay write FDisplay;
    property SNIP: Boolean read FSNIP write FSNIP;
    property CDI: Boolean read FCDI write FCDI;
    property Traction: Boolean read FTraction write FTraction;
    property FDI: Boolean read FFDI write FFDI;
    property TractionProxy: Boolean read FTractionProxy write FTractionProxy;
    property STNIP: Boolean read FSTNIP write FSTNIP;
  end;

  { TNodeEventReadConfigMem }

  TNodeEventReadConfigMem = class(TNodeEvent)
  private
    FAddressSpace: Byte;
    FControl: TObject;
    FiControl: Integer;
    FiPage: Integer;
    FProtocol: TMemoryStream;
  public
    constructor Create(ANodeInfo: TNodeInfo; ALinkedObj: TObject; AProtocol: TMemoryStream; AnAddressSpace: Byte; AiPage, AiControl: Integer; AControl: TObject); reintroduce;
    destructor Destroy; override;
    procedure StripTrailingNull;
    procedure AddTrailingNull;
    property Protocol: TMemoryStream read FProtocol write FProtocol;
    property AddressSpace: Byte read FAddressSpace write FAddressSpace;
    property iPage: Integer read FiPage write FiPage;
    property iControl: Integer read FiControl write FiControl;
    property Control: TObject read FControl write FControl;
  end;

  { TNodeEventWriteConfigMem }

  TNodeEventWriteConfigMem = class(TNodeEventReadConfigMem)
  private
    FDummy: Byte;
  public
    property Dummy: Byte read FDummy write FDummy;
  end;

  { TNodeEventThread }

  TNodeEventThread = class(TThread)    // Gathers and sends Events (TNodeEvent) to the UI from the OPStack
  private
    FEventList: TThreadList;
    FOwner: TNodeThread;
    FTerminateCompleted: Boolean;
    FTriggerEvent: PRTLEvent;
  protected
    procedure DoNodeEvent;
    procedure Execute; override;
    property TriggerEvent: PRTLEvent read FTriggerEvent write FTriggerEvent;
  public
    constructor Create(CreateSuspended: Boolean; ANodeThread: TNodeThread);
    destructor Destroy; override;
    procedure Trigger(NodeEvent: TNodeEvent);
    property Owner: TNodeThread read FOwner;
    property TerminateCompleted: Boolean read FTerminateCompleted;
    property EventList: TThreadList read FEventList write FEventList;
  end;

  { TUserInterfaceThread }         // Gathers and sends GridConnect strings to the UI from the OPStack, which have received them from the transport layer interfaces (ethernet/CAN/etc) or have been produced by the OPStack as outgoing messages

  TUserInterfaceThread = class(TThread)
  private
    FLogMessage: AnsiString;
    FLogMessages: TThreadStringList;
    FOwner: TNodeThread;
    FTerminateCompleted: Boolean;
    FTriggerEvent: PRTLEvent;
  protected
    procedure DoHandleMessages;
    procedure Execute; override;
    property TriggerEvent: PRTLEvent read FTriggerEvent write FTriggerEvent;
    property LogMessages: TThreadStringList read FLogMessages write FLogMessages;
    property LogMessage: AnsiString read FLogMessage write FLogMessage;          // Holder of the Message string that is being transfered from the thread context to the main thread via Syncronize
  public
    constructor Create(CreateSuspended: Boolean; ANodeThread: TNodeThread);
    destructor Destroy; override;
    procedure AddMessage(NewMessage: AnsiString);
    procedure Trigger;
    property Owner: TNodeThread read FOwner;
    property TerminateCompleted: Boolean read FTerminateCompleted;
  end;

  { TNodeThread }

  TNodeThread = class(TThread)     // Thread that runs the OPStack code
  private
    FCriticalSection: TRTLCriticalSection;
    FNodeEventThread: TNodeEventThread;
    FOnLogMessages: TOnRawMessageFunc;
    FOnNodeEvent: TOnNodeEventFunc;
    FReceiveStrings: TThreadStringList;
    FStack100msTimer: TTimer;
    FNodeTaskList: TThreadList;
    FTerminateCompleted: Boolean;
    FRegisteredThread: TTransportLayerThread;
    FUserInterfaceThread: TUserInterfaceThread;
    function GetStackRunning: Boolean;
  protected
    procedure Execute; override;
    procedure On100msTimer(Sender: TObject);                                                                 // Called within the contenxt of the thread (timer create in the thread) for the 100ms ticker in the OPStack library
    function ProcessNode: Boolean;                                                                           // Main call for the threads Execute
    procedure CheckForAndLinkNewTasks;
    procedure CheckForAndHandleIncomingMessages;
    property Stack100msTimer: TTimer read FStack100msTimer write FStack100msTimer;                           // Timer for the 100ms OPStack ticker
    property ReceiveStrings: TThreadStringList read FReceiveStrings write FReceiveStrings;
    property NodeTaskList: TThreadList read FNodeTaskList write FNodeTaskList;                               // Hold the tasks the UI has requested the OPStack code to execute via the user statemachine
    property UserInterfaceThread: TUserInterfaceThread read FUserInterfaceThread write FUserInterfaceThread; // The thread that is giving message strings and call Syncronize to pass them into the UI
    property NodeEventThread: TNodeEventThread read FNodeEventThread write FNodeEventThread;                 // The thread that dispatches OPStack events to the UI via the TNodeEvent objects. when an event occurs in the OPStack (user statemachine) an object is created and places into this thread so it can pass it to the UI in order to keep it notified of what is going on.
  public
    constructor Create(CreateSuspended: Boolean); virtual;
    destructor Destroy; override;
    procedure AddEvent(AnEvent: TNodeEvent);                                                        // Places the new event in the list of the NodeEventThread and signals it to run
    procedure AddTask(NewTask: TNodeTask);
    procedure EnableNode(DoStart: Boolean);
    procedure InitializeNode;
    procedure ReceiveMessage(GridConnectStrPtr: PGridConnectString);
    procedure SendMessage(AMessage: AnsiString);
    property CriticalSection: TRTLCriticalSection read FCriticalSection write FCriticalSection;             // Critical Section that serializes the calls in from the hardware interfaces (ethernet/can/etx) with the OPStack code.  It ensures the in/out ports of the OPStack are only called after the OPStack is in its top level of the main polling loop so there are no race condition on buffers, etc
    property RegisteredThread: TTransportLayerThread read FRegisteredThread write FRegisteredThread;        // Holds the Registered Thread that will need to be sent any outgoing message from the OPStack
    property StackRunning: Boolean read GetStackRunning;
    property TerminateCompleted: Boolean read FTerminateCompleted;

  published
    property OnLogMessages: TOnRawMessageFunc read FOnLogMessages write FOnLogMessages;
    property OnNodeEvent: TOnNodeEventFunc read FOnNodeEvent write FOnNodeEvent;
  end;

  { TNodeThreadHub }

  TNodeThreadHub = class         // Manages the TNodeThread object
  private
    FEnabled: Boolean;
    FNodeThread: TNodeThread;
    procedure SetEnabled(AValue: Boolean);
  protected
    property NodeThread: TNodeThread read FNodeThread write FNodeThread;
  public
    constructor Create;
    destructor Destroy; override;
    property Enabled: Boolean read FEnabled write SetEnabled;
  end;

  { TTransportLayerThread }    // Base class for transport layer threads (ethernet/CAN/etc)

  TTransportLayerThread = class(TThread)
  private
    FBufferRawMessage: string;                                                  // Shared data to pass string between thread and the Syncronized callbacks
    FConnectionState: TConnectionState;
    FGridConnectReceiveBuffer: TGridConnectString;
    FGridConnectReceiveBufferIndex: Integer;
    FGridConnectReceiveState: Integer;
    FNodeThread: TNodeThread;
    FOnStatus: THookSocketStatus;
    FRunning: Boolean;
    FStatusReason: THookSocketReason;
    FStatusValue: string;
    FOnConnectionStateChange: TOnConnectionStateChangeFunc;
    FOnErrorMessage: TOnRawMessageFunc;                                 // Function to callback through Syncronize if an error connecting occured
    FTerminateComplete: Boolean;                                                 // True if the thread has terminated
    function GetIsTerminated: Boolean;
  protected
    procedure DecomposeAndDispatchGridConnectString(GridConnectStrPtr: PGridConnectString; Helper: TOpenLCBMessageHelper);
    procedure ExecuteBegin; virtual;
    procedure ExecuteEnd; virtual;
    function GridConnect_DecodeMachine(NextChar: Char; var GridConnectStrPtr: PGridConnectString): Boolean;
    function IsValidHexChar(AChar: Char): Boolean;
    procedure ShowErrorMessageAndTerminate(Message: string);
    procedure DoConnectionState; virtual;
    procedure DoErrorMessage; virtual;
    procedure DoStatus; virtual;
    procedure RelayMessage(AMessage: AnsiString; Source: TTransportLayerThread); virtual;

    property BufferRawMessage: string read FBufferRawMessage write FBufferRawMessage;
    property ConnectionState: TConnectionState read FConnectionState write FConnectionState;
    property GridConnectReceiveState: Integer read FGridConnectReceiveState write FGridConnectReceiveState;
    property GridConnectReceiveBuffer: TGridConnectString read FGridConnectReceiveBuffer write FGridConnectReceiveBuffer;
    property GridConnectReceiveBufferIndex: Integer read FGridConnectReceiveBufferIndex write FGridConnectReceiveBufferIndex;
    property StatusReason: THookSocketReason read FStatusReason write FStatusReason;
    property StatusValue: string read FStatusValue write FStatusValue;
  public
    constructor Create(CreateSuspended: Boolean; ANodeThread: TNodeThread); virtual;
    destructor Destroy; override;
    procedure SendMessage(AMessage: AnsiString); virtual;

    property NodeThread: TNodeThread read FNodeThread write FNodeThread;
    property TerminateComplete: Boolean read FTerminateComplete;
    property IsTerminated: Boolean read GetIsTerminated;

    property OnConnectionStateChange: TOnConnectionStateChangeFunc read FOnConnectionStateChange write FOnConnectionStateChange;
    property OnErrorMessage: TOnRawMessageFunc read FOnErrorMessage write FOnErrorMessage;
    property OnStatus: THookSocketStatus read FOnStatus write FOnStatus;
  end;

procedure LinkTaskToNode(Node: PNMRANetNode; NewTask: TNodeTask);
procedure UnLinkFirstTaskFromNode(Node: PNMRANetNode; FreeTask: Boolean);

var
  NullNodeInfo: TNodeInfo;


implementation

uses
  opstackcore, template_hardware, opstacknode, template_userstatemachine;

const
  GRIDCONNECT_STATE_SYNC_START = 0;
  GRIDCONNECT_STATE_SYNC_FIND_X = 1;
  GRIDCONNECT_STATE_SYNC_FIND_HEADER = 2;
  GRIDCONNECT_STATE_SYNC_FIND_DATA = 4;

procedure LinkTaskToNode(Node: PNMRANetNode; NewTask: TNodeTask);
var
  Task: TNodeTask;
begin
  if Assigned(Node^.UserData) then
  begin
    Task := TNodeTask( Node^.UserData);
    if Assigned(Task.NextTask) then
    begin
      while Assigned( Task.NextTask) do
      begin
        if Assigned(Task.NextTask.NextTask) then
          Task := Task.NextTask
        else begin
          Task.NextTask.NextTask := NewTask;
          Break;
        end;
      end;
    end else
       Task.NextTask := NewTask
  end else
    Node^.UserData := NewTask;
end;

procedure UnLinkFirstTaskFromNode(Node: PNMRANetNode; FreeTask: Boolean);
var
  Task: TNodeTask;
begin
  if Assigned(Node^.UserData) then
  begin
    Task := TNodeTask( Node^.UserData);
    Node^.UserData := Task.NextTask;
    if FreeTask then
      FreeAndNil(Task)
  end;
end;

{ TNodeTaskWriteConfigMemory }

constructor TNodeTaskWriteConfigMemory.Create(ANodeInfo: TNodeInfo;
  ADestNodeInfo: TNodeInfo; AniStateMachine: Word; ALinkedObj: TObject;
  AnAddressSpace: Byte; AStartAddress: DWord; Stream: TMemoryStream; AiPage,
  AiControl: Integer; AControl: TObject);
begin
  inherited Create(ANodeInfo, ADestNodeInfo, AniStateMachine, ALinkedObj, AnAddressSpace, AStartAddress, 0, AiPage, AiControl, AControl);
  if Assigned(Stream) then
  begin
    Protocol.Position := 0;
    Protocol.CopyFrom(Stream, Stream.Size);
  end;
end;

{ TNodeEventReadConfigMem }

procedure TNodeEventReadConfigMem.AddTrailingNull;
begin
  Protocol.WriteByte(Ord(#0));
end;

constructor TNodeEventReadConfigMem.Create(ANodeInfo: TNodeInfo;
  ALinkedObj: TObject; AProtocol: TMemoryStream; AnAddressSpace: Byte; AiPage,
  AiControl: Integer; AControl: TObject);
begin
  inherited Create(ANodeInfo, ALinkedObj);
  FAddressSpace := AnAddressSpace;
  FiControl := AiControl;
  FiPage := AiPage;
  FControl := AControl;
  FProtocol := TMemoryStream.Create;
  if AProtocol <> nil then
  begin
    AProtocol.Position := 0;
    Protocol.CopyFrom(AProtocol, AProtocol.Size);
  end;
end;

destructor TNodeEventReadConfigMem.Destroy;
begin
  FreeAndNil(FProtocol);
  inherited Destroy;
end;

procedure TNodeEventReadConfigMem.StripTrailingNull;
var
  Done: Boolean;
begin
  Done := False;
  Protocol.Position := 0;
  while not Done and (Protocol.Position < Protocol.Size) do
  begin
    if Char( Protocol.ReadByte) = #0 then
    begin
      // Strip the null and any trailing characters.
      Protocol.Size := Protocol.Position - 1;
      Done := True;
    end
  end;
end;

{ TNodeTaskReadConfigMemory }

constructor TNodeTaskReadConfigMemory.Create(ANodeInfo: TNodeInfo;
  ADestNodeInfo: TNodeInfo; AniStateMachine: Word; ALinkedObj: TObject;
  AnAddressSpace: Byte; AStartAddress: DWord; ACount: DWord; AiPage,
  AiControl: Integer; AControl: TObject);
begin
  inherited Create(ANodeInfo, ADestNodeInfo, AniStateMachine, ALinkedObj);
  FAddressSpace := AnAddressSpace;
  FCurrentAddress := AStartAddress;
  FiPage := AiPage;
  FiControl := AiControl;
  FCount := ACount;
  FControl := AControl;
  FProtocol := TMemoryStream.Create;
end;

destructor TNodeTaskReadConfigMemory.Destroy;
begin
  FreeAndNil(FProtocol);
  inherited Destroy;
end;

  { TNodeEventSupportsProtocols }

constructor TNodeEventSupportsProtocols.Create(ANodeInfo: TNodeInfo; ALinkedObj: TObject);
begin
  inherited Create(ANodeInfo, ALinkedObj);
  FACDI := False;
  FCDI := False;
  FDatagram := False;
  FDisplay := False;
  FEdentification := False;
  FEventExchange := False;
  FFDI := False;
  FIdentification := False;
  FMemConfig := False;
  FRemoteButton := False;
  FReservation := False;
  FSimpleProtocol := False;
  FSNIP := False;
  FSTNIP := False;
  FStream := False;
  FTraction := False;
  FTractionProxy := False;
  FTeaching := False;
end;

{ TNodeEventSimpleTrainNodeInfo }

function TNodeEventSimpleTrainNodeInfo.Clone: TNodeEventSimpleTrainNodeInfo;
begin
  Result := TNodeEventSimpleTrainNodeInfo.Create(FNodeInfo, LinkedObj);
  Result.FManufacturer := FManufacturer;
  Result.FOwner := FOwner;
  Result.FRoadName := FRoadName;
  Result.FRoadNumber := FRoadNumber;
  Result.FTrainClass := FTrainClass;
  Result.FTrainName := FTrainName;
end;

procedure TNodeEventSimpleTrainNodeInfo.Decode(AMessage: POpStackMessage);
var
  i: Integer;
  MultiFrameStringBufferPtr: PMultiFrameStringBuffer;
  Head: ^Char;
begin
  MultiFrameStringBufferPtr := PMultiFrameStringBuffer( PByte( AMessage^.Buffer));
  Head := @MultiFrameStringBufferPtr^.DataArray[1];  // Skip past the Version ID

  i := 0;
  while Head^ <> #0 do
  begin
    if i < STNIP_MAX_STR_LEN then
      FRoadName[i] := Head^;
    Head := Head + 1;
    Inc(i);
  end;
  if i < STNIP_MAX_STR_LEN then
    FRoadName[i] := #0
  else
    FRoadName[STNIP_MAX_STR_LEN] := #0;

  Head := Head + 1;  // Skip over the null and to the next string
  i := 0;
  while Head^ <> #0 do
  begin
    if i < STNIP_MAX_STR_LEN then
      FTrainClass[i] := Head^;
    Head := Head + 1;
    Inc(i);
  end;
  if i < STNIP_MAX_STR_LEN then
    FTrainClass[i] := #0
  else
    FTrainClass[STNIP_MAX_STR_LEN] := #0;

  Head := Head + 1;  // Skip over the null and to the next string
  i := 0;
  while Head^ <> #0 do
  begin
    if i < STNIP_MAX_STR_LEN then
      FRoadNumber[i] := Head^;
    Head := Head + 1;
    Inc(i);
  end;
  if i < STNIP_MAX_STR_LEN then
    FRoadNumber[i] := #0
  else
    FRoadNumber[STNIP_MAX_STR_LEN] := #0;

  Head := Head + 1;  // Skip over the null and to the next string
  i := 0;
  while Head^ <> #0 do
  begin
    if i < STNIP_MAX_STR_LEN then
      FTrainName[i] := Head^;
    Head := Head + 1;
    Inc(i);
  end;
  if i < STNIP_MAX_STR_LEN then
    FTrainName[i] := #0
  else
    FTrainName[STNIP_MAX_STR_LEN] := #0;

  Head := Head + 1;  // Skip over the null and to the next string
  i := 0;
  while Head^ <> #0 do
  begin
    if i < STNIP_MAX_STR_LEN then
      FManufacturer[i] := Head^;
    Head := Head + 1;
    Inc(i);
  end;
  if i < STNIP_MAX_STR_LEN then
    FManufacturer[i] := #0
  else
    FManufacturer[STNIP_MAX_STR_LEN] := #0;

  Head := Head + 1;  // Skip over the null and to the next string
  i := 0;
  while Head^ <> #0 do
  begin
    if i < STNIP_MAX_STR_LEN then
      FOwner[i] := Head^;
    Head := Head + 1;
    Inc(i);
  end;
  if i < STNIP_MAX_STR_LEN then
    FOwner[i] := #0
  else
    FOwner[STNIP_MAX_STR_LEN] := #0;
end;

{ TNodeEventTrainInfo }

procedure TNodeEventTrainInfo.CopyTo(EventTrainInfo: TNodeEventTrainInfo);
begin
  FAddress := EventTrainInfo.Address;
  FControllerInfo := EventTrainInfo.ControllerInfo;
  FFunctions := EventTrainInfo.Functions;
  FSpeed := EventTrainInfo.Speed;
  FSpeedSteps := EventTrainInfo.SpeedSteps;
  FNodeInfo := EventTrainInfo.NodeInfo;
  if EventTrainInfo.TrainConfigValid then
  begin
    FTrainConfig.RoadName := EventTrainInfo.TrainConfig.RoadName;
    FTrainConfig.TrainClass := EventTrainInfo.TrainConfig.TrainClass;
    FTrainConfig.RoadNumber := EventTrainInfo.TrainConfig.RoadNumber;
    FTrainConfig.Name := EventTrainInfo.TrainConfig.Name;
    FTrainConfig.Manufacturer := EventTrainInfo.TrainConfig.Manufacturer;
    FTrainConfig.Owner := EventTrainInfo.TrainConfig.Owner;
    FTrainConfig.TrainID := EventTrainInfo.TrainConfig.TrainID;
    FTrainConfig.SpeedStep := EventTrainInfo.TrainConfig.SpeedStep;
    FTrainConfig.ShortLong := EventTrainInfo.TrainConfig.ShortLong;
  end;
end;

{ TNodeTask }

constructor TNodeTask.Create(ANodeInfo: TNodeInfo; ADestNodeInfo: TNodeInfo;
  AniStateMachine: Word; ALinkedObj: TObject);
  begin
    FNodeInfo := ANodeInfo;
    FiStateMachine := AniStateMachine;
    FLinkedObj := ALinkedObj;
    FNextTask := nil;
    FiSubStateMachine := 0;
    FWatchDog_1s := 0;
    FDestNodeInfo := ADestNodeInfo;
  end;

{ TNodeTaskSpeedDir }

constructor TNodeTaskSpeedDir.Create(ANodeInfo: TNodeInfo;
  ADestNodeInfo: TNodeInfo; AniStateMachine: Word; ALinkedObj: TObject;
  ASpeedDir: THalfFloat);
begin
  inherited Create(ANodeInfo, ADestNodeInfo, AniStateMachine, ALinkedObj);
  FSpeedDir := ASpeedDir;
end;

{ TNodeTaskFunction }

constructor TNodeTaskFunction.Create(ANodeInfo: TNodeInfo;
  ADestNodeInfo: TNodeInfo; AniStateMachine: Word; ALinkedObj: TObject;
  AnAddress: DWord; AValue: Word);
begin
  inherited Create(ANodeInfo, ADestNodeInfo, AniStateMachine, ALinkedObj);
  FAddress := AnAddress;
  FValue := AValue;
end;

{ TNodeTaskAllocateTrainByAddress }

constructor TNodeTaskAllocateTrainByAddress.Create(ANodeInfo: TNodeInfo;
  ADestNodeInfo: TNodeInfo; AniStateMachine: Word; ALinkedObj: TObject;
  AnAddress: Word; ASpeedStep: Byte; IsLong: Boolean);
begin
  inherited Create(ANodeInfo, ADestNodeInfo, AniStateMachine, ALinkedObj);
  FLong := IsLong;
  FAddress := AnAddress;
  FSpeedStep := ASpeedStep;
end;

{ TNodeTaskFunctionQuery }

constructor TNodeTaskFunctionQuery.Create(ANodeInfo: TNodeInfo;
  ADestNodeInfo: TNodeInfo; AniStateMachine: Word; ALinkedObj: TObject;
  AnAddress: DWord);
begin
  inherited Create(ANodeInfo, ADestNodeInfo, AniStateMachine, ALinkedObj);
  FAddress := AnAddress;
end;

{ TNodeEvent }

constructor TNodeEvent.Create(ANodeInfo: TNodeInfo; ALinkedObj: TObject);
begin
  inherited Create;
  FNodeInfo := ANodeInfo;
  FLinkedObj := ALinkedObj;
end;


{ TNodeEventThrottleAssignedToTrain }

constructor TNodeEventThrottleAssignedToTrain.Create(ANodeInfo: TNodeInfo;
  ALinkedObj: TObject; ATrainNodeInfo: TNodeInfo);
begin
  inherited Create(ANodeInfo, ALinkedObj);
  FTrainNodeInfo := ATrainNodeInfo;
end;

{ TNodeEventFunctionQuery }

constructor TNodeEventFunctionQuery.Create(ANodeInfo: TNodeInfo;
  ALinkedObj: TObject; AnAddress: DWord; AValue: Word);
begin
  inherited Create(ANodeInfo, ALinkedObj);
  FAddress := AnAddress;
  FValue := AValue;
end;

{ TNodeEventSpeedDirQuery }

constructor TNodeEventSpeedDirQuery.Create(ANodeInfo: TNodeInfo;
  ALinkedObj: TObject; ASpeedDir: THalfFloat);
begin
  inherited Create(ANodeInfo, ALinkedObj);
  FSpeedDir := ASpeedDir;
end;

{ TNodeEventThread }

constructor TNodeEventThread.Create(CreateSuspended: Boolean; ANodeThread: TNodeThread);
begin
  inherited Create(CreateSuspended);
  FOwner := ANodeThread;
  FTerminateCompleted := False;
  TriggerEvent := RTLEventCreate;
  FEventList := TThreadList.Create;
end;

destructor TNodeEventThread.Destroy;
var
  i: Integer;
  List: TList;
begin
  List := EventList.LockList;
  try
    for i := 0 to List.Count - 1 do
      TObject( List[i]).Destroy;
    List.Clear;
  finally
    EventList.UnlockList;
  end;
  RTLeventdestroy(FTriggerEvent);
  inherited Destroy;
end;

procedure TNodeEventThread.DoNodeEvent;
var
  List, NewList: TList;
  i: Integer;
begin
  if Assigned(Owner.OnNodeEvent) then
  begin
    NewList := TList.Create;
    List := EventList.LockList;
    try
      for i := 0 to List.Count - 1 do
        NewList.Add( List[i]);
      List.Clear;
    finally
      EventList.UnlockList;
    end;
    Owner.OnNodeEvent(Owner, NewList);
  end;
end;

procedure TNodeEventThread.Execute;
begin
  while not Terminated do
  begin
    RTLeventWaitFor(TriggerEvent);
    if not Terminated then
    begin
      if Assigned(Owner.OnNodeEvent) then
        Synchronize(@DoNodeEvent);
    end;
  end;
  FTerminateCompleted := True;
end;

procedure TNodeEventThread.Trigger(NodeEvent: TNodeEvent);
begin
  if Assigned(NodeEvent) then
    EventList.Add(NodeEvent);
  RTLeventSetEvent(TriggerEvent);
end;

{ TUserInterfaceThread }

procedure TUserInterfaceThread.AddMessage(NewMessage: AnsiString);
begin
  LogMessages.Add( DeleteLineBreaks( NewMessage));
  RTLeventSetEvent(FTriggerEvent);
end;

constructor TUserInterfaceThread.Create(CreateSuspended: Boolean; ANodeThread: TNodeThread);
begin
  inherited Create(CreateSuspended);
  FOwner := ANodeThread;
  FTerminateCompleted := False;
  TriggerEvent := RTLEventCreate;
  FLogMessages := TThreadStringList.Create;
end;

destructor TUserInterfaceThread.Destroy;
begin
  RTLeventdestroy(FTriggerEvent);
  FreeAndNil(FLogMessages);
  inherited Destroy;
end;

procedure TUserInterfaceThread.DoHandleMessages;
begin
  if Assigned(Owner.OnLogMessages) then
    Owner.OnLogMessages(Owner, LogMessage);
end;

procedure TUserInterfaceThread.Execute;
var
  List: TStringList;
begin
  while not Terminated do
  begin
    RTLeventWaitFor(TriggerEvent);
    if not Terminated then
    begin
      List := LogMessages.LockList;          // Always remove them
      try
        RTLeventResetEvent(TriggerEvent);
        LogMessage := LogMessages.Text;
        List.Clear;
      finally
        LogMessages.UnlockList;
      end;
      if Assigned(Owner.OnLogMessages) then
        Synchronize(@DoHandleMessages);
    end;
  end;
  FTerminateCompleted := True;
end;

procedure TUserInterfaceThread.Trigger;
begin
  RTLeventSetEvent(TriggerEvent);
end;

{ TNodeThreadHub }

constructor TNodeThreadHub.Create;
begin

end;

destructor TNodeThreadHub.Destroy;
begin
  inherited Destroy;
end;

procedure TNodeThreadHub.SetEnabled(AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    if Assigned(FNodeThread) then
    begin
      NodeThread.Terminate;
    end else
    begin

    end;
  end;
end;


{ TNodeThread }

procedure TNodeThread.AddTask(NewTask: TNodeTask);
begin
  NodeTaskList.Add(NewTask);
end;

procedure TNodeThread.CheckForAndHandleIncomingMessages;
var
  GridConnectStr: TGridConnectString;
  List: TStringList;
begin
  List := ReceiveStrings.LockList;
  try
    if List.Count > 0 then
    begin
      GridConnectStr := List[0] + #0;
      template_hardware.DispatchGridConnectStr(@GridConnectStr);                  // This next code block is running in the OPStack Node code
      List.Delete(0);
    end;
  finally
    ReceiveStrings.UnlockList;
  end;
end;

constructor TNodeThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(False);
  FOnLogMessages := nil;
  RegisteredThread := nil;
  FStack100msTimer := TTimer.Create(nil);
  Stack100msTimer.Interval := 100;
  Stack100msTimer.OnTimer := @On100msTimer;
  System.InitCriticalSection(FCriticalSection);
  RegisteredThread := nil;
  if not CreateSuspended then
    Resume;
  FTerminateCompleted := False;
  FReceiveStrings := TThreadStringList.Create;
  UserInterfaceThread := TUserInterfaceThread.Create(False, Self);
  NodeEventThread := TNodeEventThread.Create(False, Self);
  NodeTaskList := TThreadList.Create;
end;

destructor TNodeThread.Destroy;
begin
  UserInterfaceThread.Terminate;
  UserInterfaceThread.Trigger;
  while not UserInterfaceThread.TerminateCompleted do;
  FreeAndNil(FUserInterfaceThread);

  NodeEventThread.Terminate;
  NodeEventThread.Trigger(nil);
  while not NodeEventThread.TerminateCompleted do;
  FreeAndNil(FNodeEventThread);

  System.DoneCriticalSection(FCriticalSection);

  FreeAndNil(FNodeTaskList);
  FreeAndNil(FReceiveStrings);

  inherited Destroy;
end;

procedure TNodeThread.EnableNode(DoStart: Boolean);
begin
  System.EnterCriticalsection(FCriticalSection);
  try
    OPStackCore_Enable(DoStart);
    Stack100msTimer.Enabled := DoStart;
  finally
    System.LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TNodeThread.Execute;
begin
  InitializeNode;
  while not Terminated do
  begin
    ThreadSwitch;
    ProcessNode;
    CheckForAndLinkNewTasks;
    CheckForAndHandleIncomingMessages;
  end;
  FTerminateCompleted := True;
end;

function TNodeThread.GetStackRunning: Boolean;
begin
  System.EnterCriticalsection(FCriticalSection);
  Result := OPStackCore_IsRunning;
  System.LeaveCriticalSection(FCriticalSection);
end;

procedure TNodeThread.InitializeNode;
begin
  System.EnterCriticalsection(FCriticalSection);
  try
    OPStackCore_Initialize;
    Hardware_Initialize;
  finally
    System.LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TNodeThread.On100msTimer(Sender: TObject);
begin
  System.EnterCriticalsection(FCriticalSection);
  OPStackCore_Timer;
  System.LeaveCriticalSection(FCriticalSection);
end;

function TNodeThread.ProcessNode: Boolean;
begin
  Result := False;
  System.EnterCriticalsection(FCriticalSection);
  try
    Result := True;
    OPStackCore_Process;
  finally
    System.LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TNodeThread.CheckForAndLinkNewTasks;
var
  List: TList;
  i: Integer;
  Node: PNMRANetNode;
  NextTask: TNodeTask;
begin
  List := NodeTaskList.LockList;
  try
    for i := List.Count - 1 downto 0 do
    begin
      NextTask := TNodeTask( List[i]);
      Node := OPStackNode_FindNodeByNodeInfo(NextTask.FNodeInfo);
      if Assigned(Node) then
        LinkTaskToNode(Node, NextTask)
      else
        FreeAndNil( NextTask);
    end;
  finally
    List.Clear;
    NodeTaskList.UnlockList;
  end;

end;

// **************************************************************
// Called out of the TSocketThread/ComPort hardware buffers.  This
//    is the main input port of the OPStack and gets called by all
//    clients.  It is called within the context of the
//    hardware buffers
// **************************************************************
procedure TNodeThread.ReceiveMessage(GridConnectStrPtr: PGridConnectString);
begin
  ReceiveStrings.Add(GridConnectStrPtr^);
  UserInterfaceThread.AddMessage(GridConnectStrPtr^);
end;

// **************************************************************
// Called out of the template_hardware file and the OPStack.  This
//    is the main output port of the OPStack and gets relayed to all
//    registered clients.  It is called within the context of the
//    NodeThread
// **************************************************************
procedure TNodeThread.SendMessage(AMessage: AnsiString);
var
  i: Integer;
  List: TList;
begin
  system.EnterCriticalsection(FCriticalSection);
  try
    if Assigned(RegisteredThread) then
      RegisteredThread.SendMessage(AMessage);              // If this is a Listener then it will be sent to all client, else it is just goes to the client thread
  finally
    system.LeaveCriticalsection(FCriticalSection);
  end;

  UserInterfaceThread.AddMessage(AMessage);
end;

procedure TNodeThread.AddEvent(AnEvent: TNodeEvent);
begin
  if Assigned(NodeEventThread) then
  begin
    NodeEventThread.Trigger(AnEvent);
  end;
end;


{ TTransportLayerThread }

constructor TTransportLayerThread.Create(CreateSuspended: Boolean; ANodeThread: TNodeThread);
begin
  inherited Create(CreateSuspended);
  FTerminateComplete := False;
  FOnErrorMessage := nil;
  FOnConnectionStateChange := nil;
  FOnStatus := nil;
  FGridConnectReceiveState := 0;
  FNodeThread := nil;
  FGridConnectReceiveBufferIndex := 0;
  FGridConnectReceiveState := 0;
  NodeThread := ANodeThread;
  FRunning := False;
end;

destructor TTransportLayerThread.Destroy;
begin
  inherited Destroy;
end;

function TTransportLayerThread.GetIsTerminated: Boolean;
begin
  Result := Terminated
end;

procedure TTransportLayerThread.DecomposeAndDispatchGridConnectString(GridConnectStrPtr: PGridConnectString; Helper: TOpenLCBMessageHelper);
begin
  if NodeThread.StackRunning then
  begin
    System.EnterCriticalsection(NodeThread.FCriticalSection);
    try
      NodeThread.ReceiveMessage(GridConnectStrPtr);
      RelayMessage(GridConnectStrPtr^, Self);
    finally
      System.LeaveCriticalsection(NodeThread.FCriticalSection);
    end;
  end;
end;

procedure TTransportLayerThread.DoConnectionState;
begin
  if Assigned(OnConnectionStateChange) then
    OnConnectionStateChange(Self, ConnectionState)
end;

procedure TTransportLayerThread.DoErrorMessage;
begin
  if Assigned(OnErrorMessage) then
    OnErrorMessage(Self, BufferRawMessage)
end;

procedure TTransportLayerThread.DoStatus;
begin
  if Assigned(OnStatus) then
    OnStatus(Self, StatusReason, StatusValue);
end;

procedure TTransportLayerThread.ExecuteBegin;
begin
  FRunning := True;
  ConnectionState := csConnecting;
  Synchronize(@DoConnectionState);
end;

procedure TTransportLayerThread.ExecuteEnd;
begin
  ConnectionState := csDisconnected;
  Synchronize(@DoConnectionState);
  FRunning := False;
  FTerminateComplete := True;
end;

function TTransportLayerThread.GridConnect_DecodeMachine(NextChar: Char; var GridConnectStrPtr: PGridConnectString): Boolean;
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
            GridConnectReceiveBufferIndex := 0;
            GridConnectReceiveBuffer[GridConnectReceiveBufferIndex] := ':';
            Inc(FGridConnectReceiveBufferIndex);
            GridConnectReceiveState := GRIDCONNECT_STATE_SYNC_FIND_X
          end
        end;
      GRIDCONNECT_STATE_SYNC_FIND_X :
        begin
          if NextChar <> ':' then                                               // Handle double ":"'s by doing nothing if the next byte is a ":", just wait for the next byte to see if it is a "X"
          begin
            if (NextChar = 'X') or (NextChar = 'x') then
            begin
              GridConnectReceiveBuffer[GridConnectReceiveBufferIndex] := 'X';
              Inc(FGridConnectReceiveBufferIndex);
              GridConnectReceiveState := GRIDCONNECT_STATE_SYNC_FIND_HEADER
            end else
               GridConnectReceiveState := GRIDCONNECT_STATE_SYNC_START          // Error, start over
          end
        end;
      GRIDCONNECT_STATE_SYNC_FIND_HEADER :
        begin
          if GridConnectReceiveBufferIndex < 11 then
          begin
            if (NextChar = 'n') or (NextChar = 'N') then
            begin
              if GridConnectReceiveBufferIndex = 10 then                        // Just right number of characters, all done
              begin
                GridConnectReceiveBuffer[GridConnectReceiveBufferIndex] := 'N';
                Inc(FGridConnectReceiveBufferIndex);                             // Skip over the "N"
                GridConnectReceiveState := GRIDCONNECT_STATE_SYNC_FIND_DATA;
              end else
              begin
                for i := 0 to 7 do
                  HeaderArray[i] := '0';
                j := 7;
                for i := GridConnectReceiveBufferIndex - 1 downto (11 - GridConnectReceiveBufferIndex) do
                begin
                  HeaderArray[j] := GridConnectReceiveBuffer[i];
                  Dec(j);
                end;
                for i := 0 to 7 do
                  GridConnectReceiveBuffer[2 + i] := HeaderArray[i];
                GridConnectReceiveBuffer[10] := 'N';
                GridConnectReceiveBufferIndex := 11;                             // Skip over the "N"
                GridConnectReceiveState := GRIDCONNECT_STATE_SYNC_FIND_DATA;
              end;
            end else
            begin
              if IsValidHexChar(NextChar) then
              begin
                GridConnectReceiveBuffer[GridConnectReceiveBufferIndex] := NextChar;
                Inc(FGridConnectReceiveBufferIndex);
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
             if (GridConnectReceiveBufferIndex + 1) mod 2 = 0 then              // 0 index, add 1 for the actual character count, if not true the result is broken
             begin
               GridConnectReceiveBuffer[GridConnectReceiveBufferIndex] := ';';
               GridConnectReceiveBuffer[GridConnectReceiveBufferIndex + 1] := #0;
               GridConnectStrPtr := @GridConnectReceiveBuffer;
               Result := True;
             end;
             GridConnectReceiveState := GRIDCONNECT_STATE_SYNC_START            // Done
           end else
           begin
             if IsValidHexChar(NextChar) then
             begin
               GridConnectReceiveBuffer[GridConnectReceiveBufferIndex] := NextChar;
               Inc(FGridConnectReceiveBufferIndex);
             end else
               GridConnectReceiveState := GRIDCONNECT_STATE_SYNC_START;         // Error start over
           end
        end else
          GridConnectReceiveState := GRIDCONNECT_STATE_SYNC_START;              // Invalidate State Index
    end;  // Case

end;

function TTransportLayerThread.IsValidHexChar(AChar: Char): Boolean;
begin
  Result := ((AChar >= '0') and (AChar <= '9')) or ((AChar >= 'A') and (AChar <= 'F')) or ((AChar >= 'a') and (AChar <= 'f'))
end;

procedure TTransportLayerThread.RelayMessage(AMessage: AnsiString;
  Source: TTransportLayerThread);
begin

end;

procedure TTransportLayerThread.SendMessage(AMessage: AnsiString);
begin

end;


procedure TTransportLayerThread.ShowErrorMessageAndTerminate(Message: string);
begin
  BufferRawMessage := Message;
  Synchronize(@DoErrorMessage);
  Terminate;
end;

initialization
  NullNodeInfo.AliasID := 0;
  NullNodeInfo.ID := NULL_NODE_ID;


finalization

end.

