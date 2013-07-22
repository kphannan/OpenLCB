unit olcb_transport_layer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, dialogs, olcb_utilities, olcb_defines,
  olcb_app_common_settings, math_float16, Forms, blcksock, synsock;

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
  TOlcbTaskBase           = class;
  TOlcbTaskEngine         = class;
  TDatagramReceiveManager = class;
  TDatagramSendManager    = class;
  TDatagramReceive        = class;
  TDatagramSend           = class;

  TOlcbTaskBeforeDestroy = procedure(Sender: TOlcbTaskBase) of object;

  { TTransportLayerThread }

  TTransportLayerThread = class(TThread)
  private
    FBufferRawMessage: string;                                                  // Shared data to pass string between thread and the Syncronized callbacks
    FConnected: Boolean;                                                        // True if connected to the port
    FDatagramReceiveManager: TDatagramReceiveManager;
    FDatagramSendManager: TDatagramSendManager;
    FEnableReceiveMessages: Boolean;                                            // Callback through Syncronize with the message that was received
    FEnableSendMessages: Boolean;                                               // Callback through Syncronize with the message that is about to be sent
    FLooptime: DWord;
    FMaxLoopTime: DWord;
    FOlcbTaskManager: TOlcbTaskEngine;
    FOnBeforeDestroyTask: TOlcbTaskBeforeDestroy;                               // Links the Task handler to this thread for Tasks that this thread creates when it receives unsolicited messages
    FRunning: Boolean;
    FSyncErrorMessageFunc: TSyncRawMessageFunc;                                 // Function to callback through Syncronize if an error connecting occured
    FSyncReceiveMessageFunc: TSyncRawMessageFunc;                               // Function to callback through Syncronize if EnableReceiveMessages is true
    FSyncSendMessageFunc: TSyncRawMessageFunc;                                  // Function to callback through Syncronize if EnableSendMessages is true
    FTerminateComplete: Boolean;                                                 // True if the thread has terminated
    FThreadListSendStrings: TThreadList;                                        // List of strings waiting to be sent
    function GetSourceAlias: Word;
    function GetTaskCount: Integer;
  protected
    procedure DecomposeAndDispatchGridConnectString(ReceiveStr: AnsiString; Helper: TOpenLCBMessageHelper);
    procedure ExecuteBegin;
    procedure ExecuteEnd;
    procedure SyncErrorMessage;
    procedure SyncReceiveMessage;
    procedure SyncSendMessage;

    property BufferRawMessage: string read FBufferRawMessage write FBufferRawMessage;
    property DatagramReceiveManager: TDatagramReceiveManager read FDatagramReceiveManager;
    property DatagramSendManager: TDatagramSendManager read FDatagramSendManager write FDatagramSendManager;
    property OlcbTaskManager: TOlcbTaskEngine read FOlcbTaskManager write FOlcbTaskManager;
  public
    constructor Create(CreateSuspended: Boolean); virtual;
    destructor Destroy; override;
    procedure Add(Msg: AnsiString);
    procedure AddDatagramToSend(Datagram: TDatagramSend);
    procedure AddTask(NewTask: TOlcbTaskBase);
    procedure RemoveAndFreeTasks(RemoveKey: PtrInt);

    property Connected: Boolean read FConnected write FConnected;
    property ThreadListSendStrings: TThreadList read FThreadListSendStrings write FThreadListSendStrings;
    property Terminated;
    property TerminateComplete: Boolean read FTerminateComplete;
    property SyncErrorMessageFunc: TSyncRawMessageFunc read FSyncErrorMessageFunc write FSyncErrorMessageFunc;
    property SyncReceiveMessageFunc: TSyncRawMessageFunc read FSyncReceiveMessageFunc write FSyncReceiveMessageFunc;
    property SyncSendMessageFunc: TSyncRawMessageFunc read FSyncSendMessageFunc write FSyncSendMessageFunc;
    property EnableReceiveMessages: Boolean read FEnableReceiveMessages write FEnableReceiveMessages;
    property EnableSendMessages: Boolean read FEnableSendMessages write FEnableSendMessages;
    property LoopTime: DWord read FLoopTime write FLoopTime;
    property MaxLoopTime: DWord read FMaxLoopTime write FMAxLoopTime;
    property OnBeforeDestroyTask: TOlcbTaskBeforeDestroy read FOnBeforeDestroyTask write FOnBeforeDestroyTask;
    property Running: Boolean read FRunning;
    property SourceAlias: Word read GetSourceAlias;
    property TaskCount: Integer read GetTaskCount;
  end;

  { TOlcbStructureHelperBase }

  TOlcbStructureHelperBase = class
  public
    procedure CopyTo(Target: TOlcbStructureHelperBase); virtual; abstract;
  end;

  { TOlcbMemOptions }

  TOlcbMemOptions = class(TOlcbStructureHelperBase)
  private
    FAddressSpaceHi: Byte;
    FAddressSpaceLo: Byte;
    FDescription: string;
    FOperationMask: Word;
    FWriteLengthMask: Byte;
    function GetReadFromMfgACDI: Boolean;
    function GetReadFromUserACDI: Boolean;
    function GetUnAlignedReads: Boolean;
    function GetUnAlignedWrites: Boolean;
    function GetWrite64Bytes: Boolean;
    function GetWriteArbitraryBytes: Boolean;
    function GetWriteFourBytes: Boolean;
    function GetWriteOneByte: Boolean;
    function GetWriteStreamBytes: Boolean;
    function GetWriteToUserACDI: Boolean;
    function GetWriteTwoBytes: Boolean;
    function GetWriteUnderMask: Boolean;
  protected
    property OperationMask: Word read FOperationMask write FOperationMask;
    property WriteLengthMask: Byte read FWriteLengthMask write FWriteLengthMask;
  public
    constructor Create;
    procedure CopyTo(Target: TOlcbStructureHelperBase); override;
    procedure LoadFromDatagram( Datagram: TDatagramReceive);
    property WriteUnderMask: Boolean read GetWriteUnderMask;
    property UnAlignedReads: Boolean read GetUnAlignedReads;
    property UnalignedWrites: Boolean read GetUnAlignedWrites;
    property ReadFromMfgACDI: Boolean read GetReadFromMfgACDI;
    property ReadFromUserACDI: Boolean read GetReadFromUserACDI;
    property WriteToUserACDI: Boolean read GetWriteToUserACDI;
    property WriteOneByte: Boolean read GetWriteOneByte;
    property WriteTwoBytes: Boolean read GetWriteTwoBytes;
    property WriteFourBytes: Boolean read GetWriteFourBytes;
    property Write64Bytes: Boolean read GetWrite64Bytes;
    property WriteArbitraryBytes: Boolean read GetWriteArbitraryBytes;
    property WriteStreamBytes: Boolean read GetWriteStreamBytes;
    property AddressSpaceHi: Byte read FAddressSpaceHi;
    property AddressSpaceLo: Byte read FAddressSpaceLo;
    property Description: string read FDescription;
  end;

  { TOlcbMemAddressSpace }

  TOlcbMemAddressSpace = class(TOlcbStructureHelperBase)
  private
    FAddressHi: DWord;
    FAddressLo: DWord;
    FAddressLoImpliedZero: Boolean;
    FDescription: string;
    FIsPresent: Boolean;
    FIsReadOnly: Boolean;
    FSpace: Byte;
    function GetAddressSize: DWord;
    function GetSpaceAsHex: string;
  public
    constructor Create;
    procedure CopyTo(Target: TOlcbStructureHelperBase); override;
    procedure LoadByDatagram(ADatagram: TDatagramReceive);
    property AddressLo: DWord read FAddressLo write FAddressLo;
    property AddressLoImpliedZero: Boolean read FAddressLoImpliedZero write FAddressLoImpliedZero;
    property AddressHi: DWord read FAddressHi write FAddressHi;
    property AddressSize: DWord read GetAddressSize;
    property Description: string read FDescription write FDescription;
    property IsReadOnly: Boolean read FIsReadOnly write FIsReadOnly;
    property IsPresent: Boolean read FIsPresent write FIsPresent;
    property Space: Byte read FSpace write FSpace;
    property SpaceAsHex: string read GetSpaceAsHex;
  end;

  { TOlcbMemConfig }

  TOlcbMemConfig = class(TOlcbStructureHelperBase)
  private
    FAddressSpaceList: TList;
    FOptions: TOlcbMemOptions;
    function GetAddressSpace(Index: Integer): TOlcbMemAddressSpace;
    function GetAddressCount: Integer;
    procedure SetAddressSpace(Index: Integer; AValue: TOlcbMemAddressSpace);
  protected
    procedure Clear;
    property AddressSpaceList: TList read FAddressSpaceList write FAddressSpaceList;
  public
    constructor Create;
    destructor Destroy; override;
    function AddAddressSpace: TOlcbMemAddressSpace;
    function AddAddressSpaceByDatagram(Datagram: TDatagramReceive): TOlcbMemAddressSpace;
    procedure CopyTo(Target: TOlcbStructureHelperBase); override;
    function FindAddressSpaceBySpaceID(AnAddress: Byte): TOlcbMemAddressSpace;

    property AddressSpace[Index: Integer]: TOlcbMemAddressSpace read GetAddressSpace write SetAddressSpace;
    property AddressSpaceCount: Integer read GetAddressCount;
    property Options: TOlcbMemOptions read FOptions;
  end;

  { TOlcbProtocolIdentification }

  TOlcbProtocolIdentification = class(TOlcbStructureHelperBase)
  private
    FMask: QWord;
    function GetAbbreviatedCDIProtocol: Boolean;
    function GetConfigDescriptionInfoProtocol: Boolean;
    function GetDatagramProtocol: Boolean;
    function GetFunctionStateInformationProtocol: Boolean;
    function GetDisplayProtocol: Boolean;
    function GetEventExchangeProtocol: Boolean;
    function GetFunctionDescriptionInfoProtocol: Boolean;
    function GetIdentificationProtocol: Boolean;
    function GetMemoryConfigProtocol: Boolean;
    function GetRemoteButtonProtocol: Boolean;
    function GetReservationProtocol: Boolean;
    function GetSimpleNodeInfoProtocol: Boolean;
    function GetSimpleProtocol: Boolean;
    function GetStreamProtocol: Boolean;
    function GetTeachingLearningConfigProtocol: Boolean;
    function GetTractionControlProtocol: Boolean;
  protected
    property Mask: QWord read FMask write FMask;
  public
    procedure CopyTo(Target: TOlcbStructureHelperBase); override;
    procedure LoadByMessage(AHelper: TOpenLCBMessageHelper);
    property SimpleProtocol: Boolean read GetSimpleProtocol;
    property DatagramProtocol: Boolean read GetDatagramProtocol;
    property StreamProtocol: Boolean read GetStreamProtocol;
    property MemoryConfigProtocol: Boolean read GetMemoryConfigProtocol;
    property ReservationProtocol: Boolean read GetReservationProtocol;
    property EventExchangeProtocol: Boolean read GetEventExchangeProtocol;
    property IdentificiationProtcol: Boolean read GetIdentificationProtocol;
    property TeachingLearningConfigProtocol: Boolean read GetTeachingLearningConfigProtocol;
    property RemoteButtonProtocol: Boolean read GetRemoteButtonProtocol;
    property AbbreviatedCDIProtocol: Boolean read GetAbbreviatedCDIProtocol;
    property DisplayProtocol: Boolean read GetDisplayProtocol;
    property SimpleNodeInfoProtocol: Boolean read GetSimpleNodeInfoProtocol;
    property ConfigDescriptionInfoProtocol: Boolean read GetConfigDescriptionInfoProtocol;
    property TractionControlProtocol: Boolean read GetTractionControlProtocol;
    property FunctionDescriptionInfoProtocol: Boolean read GetFunctionDescriptionInfoProtocol;
    property FunctionStateInformationProtocol: Boolean read GetFunctionStateInformationProtocol;
  end;

  { TOlcbSNIP }

  TOlcbSNIP = class(TOlcbStructureHelperBase)
  private
    FSniiHardwareVersion: string;
    FSniiMfgModel: string;
    FSniiMfgName: string;
    FSniiMfgVersion: Byte;
    FSniiSoftwareVersion: string;
    FSniiUserDescription: string;
    FSniiUserName: string;
    FSniiUserVersion: Byte;
  public
    constructor Create;
    procedure CopyTo(Target: TOlcbStructureHelperBase); override;
    property SniiMfgName: string read FSniiMfgName write FSniiMfgName;
    property SniiMfgModel: string read FSniiMfgModel write FSniiMfgModel;
    property SniiSoftwareVersion: string read FSniiSoftwareVersion write FSniiSoftwareVersion;
    property SniiHardwareVersion: string read FSniiHardwareVersion write FSniiHardwareVersion;
    property SniiUserName: string read FSniiUserName write FSniiUserName;
    property SniiUserDescription: string read FSniiUserDescription write FSniiUserDescription;
    property SniiUserVersion: Byte read FSniiUserVersion write FSniiUserVersion;
    property SniiMfgVersion: Byte read FSniiMfgVersion write FSniiMfgVersion;
  end;

  { TDatagramReceive }

  TDatagramReceive = class( TOlcbMessage)
  private
    FDestinationAlias: Word;
    FLocalHelper: TOpenLCBMessageHelper;
    FRawDatagram: TDatagramArray;
    FFull: Boolean;
    FCurrentPos: Byte;
    FSourceAlias: Word;
  protected
    procedure Clear;
    procedure SendACK(TransportLayerThread: TTransportLayerThread);
    property LocalHelper: TOpenLCBMessageHelper read FLocalHelper write FLocalHelper;   // Global object to work with OLCB messages
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word);
    destructor Destroy; override;
    function ExtractDataBytesAsInt(StartByteIndex, EndByteIndex: Integer): QWord;       // Helpers to pull information out of the datagram byte array
    function ExtractDataBytesAsString(StartIndex, Count: Integer): String;              // Helpers to pull information out of the datagram byte array
    procedure CopyToStream(Stream: TStream; StartIndex, Count: Integer);                // Helpers to move datagram bytes to a stream
    procedure Process(AHelper: TOpenLCBMessageHelper; TransportLayerThread: TTransportLayerThread);   // Processes the message/
    property CurrentPos: Byte read FCurrentPos write FCurrentPos;                       // Running count of the number of bytes being received, once "Full" this is the number of byte in the datagram
    property DestinationAlias: Word read FDestinationAlias write FDestinationAlias;     // Node who is sending the Datagram to this object
    property RawDatagram: TDatagramArray read FRawDatagram write FRawDatagram;          // Raw datagram bytes received
    property Full: Boolean read FFull write FFull;                                      // Raw datagram bytes with a size of CurrentPos is valid
    property SourceAlias: Word read FSourceAlias write FSourceAlias;                    // Node who is looking for Datagrams assigned to it to come in
  end;

  { TDatagramReceiveManager }

  TDatagramReceiveManager = class
  private
    FDatagrams: TThreadList;
    FMaxCount: Integer;
    FOwner: TTransportLayerThread;
  protected
    function FindInProcessDatagram(AHelper: TOpenLCBMessageHelper): TDatagramReceive;
    property Datagrams: TThreadList read FDatagrams write FDatagrams;
    property Owner: TTransportLayerThread read FOwner write FOwner;
    property MaxCount: Integer read FMaxCount write FMaxCount;
  public
    constructor Create(AnOwner: TTransportLayerThread);
    destructor Destroy; override;
    procedure Clear;
    function Process(AHelper: TOpenLCBMessageHelper): TDatagramReceive;
  end;

{ TDatagramSend }

TDatagramSend = class( TOlcbMessage)
private
  FAbandonTime: Cardinal;
  FBlockStartPos: Integer;
  FDataBytesSent: TCANByteArray;
  FDataBytesSentLen: Byte;
  FDestinationAlias: Word;
  FEmpty: Boolean;
  FErrorCode: Word;
  FLocalHelper: TOpenLCBMessageHelper;
  FMTI: DWord;
  FNewStartFrame: Boolean;
  FProtocolHeader: TCANByteArray;
  FProtocolHeaderLen: Byte;
  FRetryCount: Byte;
  FBlockByteCount: Integer;
  FSourceAlias: Word;
  FStream: TMemoryStream;
  FWaitingForACK: Boolean;
protected
  procedure StreamBytesToByteArray(Offset: Byte; var ByteArray: TCANByteArray; var Count: Byte);
  property AbandonTime: Cardinal read FAbandonTime write FAbandonTime;
  property BlockByteCount: Integer read FBlockByteCount write FBlockByteCount;   // Bytes sent counter for each datagram
  property BlockStartPos: Integer read FBlockStartPos write FBlockStartPos;
  property LocalHelper: TOpenLCBMessageHelper read FLocalHelper write FLocalHelper;
  property ProtocolHeader: TCANByteArray read FProtocolHeader write FProtocolHeader;
  property ProtocolHeaderLen: Byte read FProtocolHeaderLen write FProtocolHeaderLen;
  property DataBytesSent: TCANByteArray read FDataBytesSent write FDataBytesSent;
  property DataBytesSentLen: Byte read FDataBytesSentLen write FDataBytesSentLen;
  property MTI: DWord read FMTI write FMTI;
  property NewStartFrame: Boolean read FNewStartFrame write FNewStartFrame;
  property RetryCount: Byte read FRetryCount write FRetryCount;
  property WaitingForACK: Boolean read FWaitingForACK write FWaitingForACK;             // After a frame is sent need to wait
public
  constructor Create;
  destructor Destroy; override;
  procedure Initialize(AStream: TStream; AProtocolHeader: TCANByteArray; AProtocolHeaderLen: Byte; ASourceAlias, ADestinationAlias: Word);
  function ProcessSend(TransportLayerThread: TTransportLayerThread): Boolean;
  function ProcessReceive(AHelper: TOpenLCBMessageHelper): Boolean;
  property DestinationAlias: Word read FDestinationAlias write FDestinationAlias;
  property Empty: Boolean read FEmpty write FEmpty;
  property ErrorCode: Word read FErrorCode write FErrorCode;    // One of the DATAGRAM_REJECTED_xxx constants, if $0000 then no error
  property SourceAlias: Word read FSourceAlias write FSourceAlias;
  property Stream: TMemoryStream read FStream write FStream;
end;

{ TDatagramSendManager }

TDatagramSendManager = class
private
  FAbandonDatagrams: TThreadList;
  FDatagrams: TThreadList;
  FMaxCount: Integer;
  FOwner: TTransportLayerThread;
  FTimer: TTimer;
protected
  procedure TimerTick(Sender: TObject);
  property AbandonDatagrams: TThreadList read FAbandonDatagrams write FAbandonDatagrams;
  property Datagrams: TThreadList read FDatagrams write FDatagrams;
  property MaxCount: Integer read FMaxCount write FMaxCount;
  property Owner: TTransportLayerThread read FOwner write FOwner;
  property Timer: TTimer read FTimer write FTimer;
public
  constructor Create(AnOwner: TTransportLayerThread);
  destructor Destroy; override;
  procedure Clear;
  procedure ClearAbandon;
  function ProcessReceive(AHelper: TOpenLCBMessageHelper): TDatagramSend;
  procedure ProcessSend;
end;

{ TOlcbTaskBase }

  TOlcbTaskBase = class
  private
    FErrorCode: DWord;
    FLog: Boolean;
    FiLogState: Integer;
    FLogStrings: TStringList;
    FLogThreadName: string;
    FMessageHelper: TOpenLCBMessageHelper;
    FOnBeforeDestroy: TOlcbTaskBeforeDestroy;
    FSending: Boolean;
    FErrorString: string;
    FRemoveKey: PtrInt;
    FHasStarted: Boolean;
    FStartAsSending: Boolean;
    FTag: PtrInt;
    FForceTermination: Boolean;
    function SpaceToCommandByteEncoding(ASpace: Byte): Byte;
  protected
    FTransportLayerThread: TTransportLayerThread;
    FDestinationAlias: Word;
    FDone: Boolean;
    FiState: Integer;
    FSourceAlias: Word;
    procedure ExtractErrorInformation(DatagramReceive: TDatagramReceive);
    function IsDatagramAckFromDestination(MessageInfo: TOlcbMessage): Boolean;
    function IsConfigMemorySpaceInfoReplyFromDestination(MessageInfo: TOlcbMessage; AnAddress: Byte; var DatagramReceive: TDatagramReceive): Boolean;
    function IsConfigMemoryOptionsReplyFromDestination(MessageInfo: TOlcbMessage; var DatagramReceive: TDatagramReceive): Boolean;
    function IsConfigMemoryReadReplyFromDestination(MessageInfo: TOlcbMessage; var DatagramReceive: TDatagramReceive): Boolean;
    function IsProtocolIdentificationProcolReplyFromDestination(MessageInfo: TOlcbMessage): Boolean;
    function IsSnipMessageReply(MessageInfo: TOlcbMessage): Boolean;
    function IsTractionFunctionQueryReply(MessageInfo: TOlcbMessage): Boolean;
    function IsTractionSpeedsQueryFirstFrameReply(MessageInfo: TOlcbMessage): Boolean;
    function IsTractionSpeedsQuerySecondFrameReply(MessageInfo: TOlcbMessage): Boolean;
    function IsTractionAttachDCCAddressReply(MessageInfo: TOlcbMessage): Boolean;
    function IsTractionDetachDCCAddressReply(MessageInfo: TOlcbMessage): Boolean;
    function IsTractionAttachNodeQueryReply(MessageInfo: TOlcbMessage): Boolean;
    function IsTractionDetachNodeQueryReply(MessageInfo: TOlcbMessage): Boolean;
    function IsTractionQueryProxyReply(MessageInfo: TOlcbMessage): Boolean;
    function IsTractionReserveProxyReply(MessageInfo: TOlcbMessage): Boolean;
    procedure Process(MessageInfo: TOlcbMessage); virtual;                      // Must override this
    procedure SendIdentifyEventsMessage;
    procedure SendIdentifyEventsAddressedMessage;
    procedure SendIdentifyConsumerMessage(Event: TEventID);
    procedure SendIdentifyProducerMessage(Event: TEventID);
    procedure SendMemoryConfigurationOptions;
    procedure SendMemoryConfigurationSpaceInfo(Space: Byte);
    procedure SendMemoryConfigurationRead(Space: Byte; StartAddress: DWord; Count: Byte; ForceUseOfSpaceByte: Boolean);
    procedure SendMemoryConfigurationWrite(Space: Byte; StartAddress: DWord; MaxAddressSize: DWORD; ForceUseOfSpaceByte: Boolean; AStream: TStream);
    procedure SendProtocolIdentificationProtocolMessage;
    procedure SendSnipMessage;
    procedure SendTractionAttachDccProxyMessage(Address: Word; Short: Boolean; SpeedStep: Byte);
    procedure SendTractionDetachDccAddressProxyMessage(Address: Word; Short: Boolean);
    procedure SendTractionEStopMessage;
    procedure SendTractionFunction(FunctionAddress: DWord; Value: Word);
    procedure SendTractionQueryFunction(FunctionAddress: DWord);
    procedure SendTractionQueryDccAddressProxyMessage(Address: Word; Short: Boolean);
    procedure SendTractionQuerySpeeds;
    procedure SendTractionQueryProxyMessage;
    procedure SendTractionReleaseProxyMessage;
    procedure SendTractionReserveProxyMessage;
    procedure SendTractionSpeedMessage(Speed: THalfFloat);
    procedure SendVerifyNodeIDGlobalMessage;
    procedure SendVerifyNodeIDToDestinationMessage;
    procedure SyncOnBeforeTaskDestroy;
    property Done: Boolean read FDone;
    property iLogState: Integer read FiLogState write FiLogState;
    property iState: Integer read FiState write FiState;
    property LogStrings: TStringList read FLogStrings write FLogStrings;
    property LogTheadName: string read FLogThreadName write FLogThreadName;
    property StartAsSending: Boolean read FStartAsSending write FStartAsSending;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean); virtual;
    destructor Destroy; override;
    function Clone: TOlcbTaskBase; virtual; abstract;
    procedure CopyTo(Target: TOlcbTaskBase); virtual;
    procedure LogMsg(Msg: string); virtual;
    property TransportLayerThread: TTransportLayerThread read FTransportLayerThread;
    property DestinationAlias: Word read FDestinationAlias;
    property OnBeforeDestroy: TOlcbTaskBeforeDestroy read FOnBeforeDestroy write FOnBeforeDestroy;
    property ErrorCode: DWord read FErrorCode write FErrorCode;
    property ErrorString: string read FErrorString write FErrorString;
    property Log: Boolean read FLog write FLog;
    property MessageHelper: TOpenLCBMessageHelper read FMessageHelper write FMessageHelper;
    property SourceAlias: Word read FSourceAlias;
    property Sending: Boolean read FSending write FSending;
    property Tag: PtrInt read FTag write FTag;
    property RemoveKey: PtrInt read FRemoveKey write FRemoveKey;
    property HasStarted: Boolean read FHasStarted;
    property ForceTermination: Boolean read FForceTermination write FForceTermination;
  end;

  { TVerifyNodeIDGlobalTask }

  TVerifyNodeIDGlobalTask = class(TOlcbTaskBase)
  public
   function Clone: TOlcbTaskBase; override;
   procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TVerifyNodeIDTask }

  TVerifyNodeIDTask = class(TOlcbTaskBase)
  public
    function Clone: TOlcbTaskBase; override;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TProtocolSupportTask }

  TProtocolSupportTask = class(TOlcbTaskBase)
  private
    FProtocols: QWord;
  public
    function Clone: TOlcbTaskBase; override;
    procedure CopyTo(Target: TOlcbTaskBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;
    property Protocols: QWord read FProtocols;
  end;

  { TSimpleNodeInformationTask }

  TSimpleNodeInformationTask = class(TOlcbTaskBase)
  private
    FiLogStateSnip: Integer;
    FSnip: TOlcbSNIP;
    FiSnipState: Integer;  // for inner SNIP statemachine
  protected
    procedure LogSnipMsg(Msg: string);
    property iLogStateSnip: Integer read FiLogStateSnip write FiLogStateSnip;
    property iSnipState: Integer read FiSnipState write FiSnipState;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean); override;
    destructor Destroy; override;
    function Clone: TOlcbTaskBase; override;
    procedure CopyTo(Target: TOlcbTaskBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;
    property Snip: TOlcbSNIP read FSnip;
  end;

  { TConfigMemoryOptionsTask }

  TConfigMemoryOptionsTask = class(TOlcbTaskBase)
  private
    FConfigMemoryOptions: TOlcbMemOptions;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean); override;
    destructor Destroy; override;
    function Clone: TOlcbTaskBase; override;
    procedure CopyTo(Target: TOlcbTaskBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;
    property ConfigMemoryOptions: TOlcbMemOptions read FConfigMemoryOptions write FConfigMemoryOptions;
  end;

  { TConfigMemoryAddressSpaceInfoTask }

  TConfigMemoryAddressSpaceInfoTask = class(TOlcbTaskBase)
  private
    FAddressSpace: Byte;
    FConfigMemoryAddressSpace: TOlcbMemAddressSpace;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddressSpace: Byte); reintroduce; virtual;
    destructor Destroy; override;
    function Clone: TOlcbTaskBase; override;
    procedure CopyTo(Target: TOlcbTaskBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;
    property ConfigMemoryAddressSpace: TOlcbMemAddressSpace read FConfigMemoryAddressSpace write FConfigMemoryAddressSpace;
    property AddressSpace: Byte read FAddressSpace;
  end;


  { TEnumAllConfigMemoryAddressSpaceInfoTask }

  TEnumAllConfigMemoryAddressSpaceInfoTask = class(TOlcbTaskBase)
  private
    FConfigMemAddressInfo: TOlcbMemConfig;
    FCurrentAddressSpace: Byte;
    FMaxAddressSpace: Byte;
    FMinAddressSpace: Byte;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean); override;
    destructor Destroy; override;
    function Clone: TOlcbTaskBase; override;
    procedure CopyTo(Target: TOlcbTaskBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;
    property MinAddressSpace: Byte read FMinAddressSpace write FMinAddressSpace;
    property MaxAddressSpace: Byte read FMaxAddressSpace write FMaxAddressSpace;
    property CurrentAddressSpace: Byte read FCurrentAddressSpace write FCurrentAddressSpace;
    property ConfigMemAddressInfo: TOlcbMemConfig read FConfigMemAddressInfo write FConfigMemAddressInfo;
  end;

  { TBaseAddressSpaceMemoryTask }

  TBaseAddressSpaceMemoryTask = class(TOlcbTaskBase)
  private
    FAddressSpace: Byte;
    FCurrentAddress: DWord;
    FCurrentSendSize: Byte;
    FDataStream: TMemoryStream;
    FForceOptionalSpaceByte: Boolean;
    FMaxAddress: DWord;
    FMinAddress: DWord;
    FTerminator: Char;
    FUsingTerminator: Boolean;
    FWritingToAddress: Boolean;
    function GetMaxPayloadSize: DWord;
  protected
    property CurrentAddress: DWord read FCurrentAddress write FCurrentAddress;
    property CurrentSendSize: Byte read FCurrentSendSize write FCurrentSendSize;
    property UsingTerminator: Boolean read FUsingTerminator write FUsingTerminator;
    property MaxPayloadSize: DWord read GetMaxPayloadSize;
    property WritingToAddress: Boolean read FWritingToAddress write FWritingToAddress;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddressSpace: Byte; UseTerminatorChar: Boolean); reintroduce;
    destructor Destroy; override;
    function Clone: TOlcbTaskBase; override;
    procedure CopyTo(Target: TOlcbTaskBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;
    property AddressSpace: Byte read FAddressSpace;
    property DataStream: TMemoryStream read FDataStream;
    property ForceOptionalSpaceByte: Boolean read FForceOptionalSpaceByte write FForceOptionalSpaceByte;
    property MinAddress: DWord read FMinAddress;
    property MaxAddress: DWord read FMaxAddress;
    property Terminator: Char read FTerminator write FTerminator;
  end;

  { TReadAddressSpaceMemoryTask }

  TReadAddressSpaceMemoryTask = class(TBaseAddressSpaceMemoryTask)
  public
    function Clone: TOlcbTaskBase; override;
  end;

  { TWriteAddressSpaceMemoryTask }

  TWriteAddressSpaceMemoryTask = class(TBaseAddressSpaceMemoryTask)
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddressSpace: Byte; AStream: TStream); reintroduce;
    function Clone: TOlcbTaskBase; override;
  end;

  { TWriteAddressSpaceMemoryRawTask }

  TWriteAddressSpaceMemoryRawTask = class(TOlcbTaskBase)
  private
    FAddressSpace: Byte;
    FForceOptionalSpaceByte: Boolean;
    FStream: TMemoryStream;
    FWriteAddress: DWord;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddressSpace: Byte; AWriteAddress: DWord; AStream: TStream); reintroduce;
    destructor Destroy; override;
    function Clone: TOlcbTaskBase; override;
    procedure CopyTo(Target: TOlcbTaskBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;

    property AddressSpace: Byte read FAddressSpace;
    property ForceOptionalSpaceByte: Boolean read FForceOptionalSpaceByte write FForceOptionalSpaceByte;
    property Stream: TMemoryStream read FStream;
    property WriteAddress: DWord read FWriteAddress write FWriteAddress;
  end;

  { TReadAddressSpaceMemoryRawTask }

  TReadAddressSpaceMemoryRawTask = class(TOlcbTaskBase)
  private
    FAddressSpace: Byte;
    FCurrentOffset: DWord;
    FForceOptionalSpaceByte: Boolean;
    FIncludeTerminator: Boolean;
    FReadByteCount: DWord;
    FStream: TMemoryStream;
    FReadAddress: DWord;
    FTerminator: Char;
    FUsingTerminator: Boolean;
    function GetPayloadSize: Integer;
  protected
    property CurrentOffset: DWord read FCurrentOffset write FCurrentOffset;
    property PayloadSize: Integer read GetPayloadSize;
    property UsingTerminator: Boolean read FUsingTerminator write FUsingTerminator;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddressSpace: Byte; AReadAddress, AReadByteCount: DWord; UseTerminatorChar: Boolean); reintroduce;
    destructor Destroy; override;
    function Clone: TOlcbTaskBase; override;
    procedure CopyTo(Target: TOlcbTaskBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;

    property AddressSpace: Byte read FAddressSpace;
    property ForceOptionalSpaceByte: Boolean read FForceOptionalSpaceByte write FForceOptionalSpaceByte;
    property IncludeTerminator: Boolean read FIncludeTerminator write FIncludeTerminator;  // Include the terminator in the Stream result
    property Stream: TMemoryStream read FStream;
    property ReadAddress: DWord read FReadAddress;
    property ReadByteCount: DWord read FReadByteCount;
    property Terminator: Char read FTerminator write FTerminator;
  end;

  { TIdentifyEventsTask }

  TIdentifyEventsTask = class(TOlcbTaskBase)
  public
    function Clone: TOlcbTaskBase; override;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TIdentifyEventsAddressedTask }

  TIdentifyEventsAddressedTask = class(TOlcbTaskBase)
  public
    function Clone: TOlcbTaskBase; override;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TIdentifyProducerTask }

  TIdentifyProducerTask = class(TOlcbTaskBase)
  private
    FEvent: TEventID;
  protected
    property Event: TEventID read FEvent write FEvent;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnEvent: TEventID); reintroduce;
    function Clone: TOlcbTaskBase; override;
    procedure CopyTo(Target: TOlcbTaskBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TIdentifyConsumerTask }

  TIdentifyConsumerTask = class(TOlcbTaskBase)
  private
    FEvent: TEventID;
  protected
    property Event: TEventID read FEvent write FEvent;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnEvent: TEventID); reintroduce;
    function Clone: TOlcbTaskBase; override;
    procedure CopyTo(Target: TOlcbTaskBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TCANLayerTask }
  TCANLayerTask = class(TOlcbTaskBase)
  public
    function Clone: TOlcbTaskBase; override;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TEventTask }

  TEventTask = class(TOlcbTaskBase)
  public
    function Clone: TOlcbTaskBase; override;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TVerifiedNodeIDTask }

  TVerifiedNodeIDTask = class(TOlcbTaskBase)
  public
    function Clone: TOlcbTaskBase; override;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TTractionProtocolTask }

  TTractionProtocolTask = class(TOlcbTaskBase)
  public
    function Clone: TOlcbTaskBase; override;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TInitializationCompleteTask }

  TInitializationCompleteTask = class(TOlcbTaskBase)
  public
    function Clone: TOlcbTaskBase; override;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TTractionReserveAndAttachDccProxyTask }

  TTractionReserveAndAttachDccProxyTask = class(TOlcbTaskBase)
  private
    FAddress: Word;
    FReplyAddress: Word;
    FReplyCode: Integer;
    FReplySpeedSteps: Byte;
    FIsShort: Boolean;
    FSpeedStep: Byte;
  protected
    property Address: Word read FAddress write FAddress;
    property IsShort: Boolean read FIsShort write FIsShort;
    property SpeedStep: Byte read FSpeedStep write FSpeedStep;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddress: Word; IsShortAddress: Boolean; ASpeedStep: Byte); reintroduce;
    function Clone: TOlcbTaskBase; override;
    procedure CopyTo(Target: TOlcbTaskBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;

    property ReplyCode: Integer read FReplyCode;        // -1 if the Reply Code was not sent
    property ReplySpeedSteps: Byte read FReplySpeedSteps;
    property ReplyAddress: Word read FReplyAddress;
  end;

  { TTractionReserveAndDetachDccProxyTask }

  TTractionReserveAndDetachDccProxyTask = class(TOlcbTaskBase)
  private
    FAddress: Word;
    FIsShort: Boolean;
    FReplyAddress: Word;
    FReplyCode: Integer;
    FReplySpeedSteps: Byte;
  protected
    property Address: Word read FAddress write FAddress;
    property IsShort: Boolean read FIsShort write FIsShort;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddress: Word; IsShortAddress: Boolean); reintroduce;
    function Clone: TOlcbTaskBase; override;
    procedure CopyTo(Target: TOlcbTaskBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;

    property ReplyCode: Integer read FReplyCode;        // -1 if the Reply Code was not sent
    property ReplySpeedSteps: Byte read FReplySpeedSteps;
    property ReplyAddress: Word read FReplyAddress;
  end;

  { TTractionQueryDccAddressProxyTask }

  TTractionQueryDccAddressProxyTask = class(TOlcbTaskBase)
  private
    FAddress: Word;
    FIsShort: Boolean;
  protected
    property Address: Word read FAddress write FAddress;
    property IsShort: Boolean read FIsShort write FIsShort;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddress: Word; IsShortAddress: Boolean); reintroduce;
    function Clone: TOlcbTaskBase; override;
    procedure CopyTo(Target: TOlcbTaskBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TTractionSpeedTask }

  TTractionSpeedTask = class(TOlcbTaskBase)
  private
    FEStop: Boolean;
    FSpeed: THalfFloat;
  protected
    property Speed: THalfFloat read FSpeed write FSpeed;  // Dir is wrapped up in the neg sign
    property EStop: Boolean read FEStop write FEStop;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; ASpeed: THalfFloat; IsEStop: Boolean); reintroduce;
    function Clone: TOlcbTaskBase; override;
    procedure CopyTo(Target: TOlcbTaskBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TTractionFunctionTask }

  TTractionFunctionTask = class(TOlcbTaskBase)
  private
    FAddress: DWord;
    FWord: Word;
  protected
    property Address: DWord read FAddress write FAddress;
    property Value: Word read FWord write FWord;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddress: DWord; AValue: Word); reintroduce;
    function Clone: TOlcbTaskBase; override;
    procedure CopyTo(Target: TOlcbTaskBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TTractionQueryFunctionTask }

  TTractionQueryFunctionTask = class(TOlcbTaskBase)
  private
    FAddress: DWord;
    FValue: Integer;
  protected
    property Address: DWord read FAddress write FAddress;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddress: DWord); reintroduce;
    function Clone: TOlcbTaskBase; override;
    procedure CopyTo(Target: TOlcbTaskBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;

    property Value: Integer read FValue write FValue;
  end;

  { TTractionQuerySpeedTask }

  TTractionQuerySpeedTask = class(TOlcbTaskBase)
  private
    FActualSpeed: Word;
    FCommandedSpeed: Word;
    FSetSpeed: Word;
    FStatus: Byte;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean); reintroduce;
    function Clone: TOlcbTaskBase; override;
    procedure CopyTo(Target: TOlcbTaskBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;

    property SetSpeed: Word read FSetSpeed write FSetSpeed;
    property CommandedSpeed: Word read FCommandedSpeed write FCommandedSpeed;
    property ActualSpeed: Word read FActualSpeed write FActualSpeed;
    property Staus: Byte read FStatus write FStatus;
  end;

  { TOlcbTaskEngine }

  TOlcbTaskEngine = class
  private
    FMaxCount: Integer;
    FOwner: TTransportLayerThread;
    FTaskList: TThreadList;
  protected
    property MaxCount: Integer read FMaxCount write FMaxCount;
    property Owner: TTransportLayerThread read FOwner write FOwner;
    property TaskList: TThreadList read FTaskList write FTaskList;
  public
    constructor Create(AnOwner: TTransportLayerThread);
    procedure ProcessReceiving(MessageInfo: TOlcbMessage);
    procedure ProcessSending;
    destructor Destroy; override;
    procedure Clear;
  end;


var
  TaskObjects: DWord;

implementation

{ TReadAddressSpaceMemoryTask }

function TReadAddressSpaceMemoryTask.Clone: TOlcbTaskBase;
begin
  Result := TReadAddressSpaceMemoryTask.Create(SourceAlias, DestinationAlias, Sending, AddressSpace, UsingTerminator);
end;

{ TTransportLayerThread }

function TTransportLayerThread.GetSourceAlias: Word;
begin
  EnterCriticalsection(GlobalSettingLock);
  try
    Result := GlobalSettings.General.AliasIDAsVal;
  finally
    LeaveCriticalsection(GlobalSettingLock)
  end;
end;

function TTransportLayerThread.GetTaskCount: Integer;
var
  List: TList;
begin
  List := OlcbTaskManager.TaskList.LockList;
  try
    Result := List.Count
  finally
    OlcbTaskManager.TaskList.UnlockList;
  end;
end;

procedure TTransportLayerThread.DecomposeAndDispatchGridConnectString(ReceiveStr: AnsiString; Helper: TOpenLCBMessageHelper);
var
  CANLayerTask: TCANLayerTask;
  EventTask: TEventTask;
  VerifiedNodeIDTask: TVerifiedNodeIDTask;
  TractionProtocolTask: TTractionProtocolTask;
  InitializationCompleteTask: TInitializationCompleteTask;
  CompletedSendDatagram: TDatagramSend;
  BufferDatagramReceive: TDatagramReceive;
begin
  ReceiveStr := Trim(ReceiveStr);
  if Helper.Decompose(ReceiveStr) then
  begin
    if EnableReceiveMessages then                                         // *** Communicate back to the app the raw message string
    begin
      BufferRawMessage := ReceiveStr;
      Synchronize(@SyncReceiveMessage);
    end;

    if IsDatagramMTI(Helper.MTI, True) then                               // *** Test for a Datagram message that came in ***
    begin
      CompletedSendDatagram := DatagramSendManager.ProcessReceive(Helper);// Sending Datagrams are expecting replies from their destination Nodes
      if Assigned(CompletedSendDatagram) then
      begin
        OlcbTaskManager.ProcessReceiving(CompletedSendDatagram);          // Give the Task subsystem a crack at knowning about the sent datagram
        FreeAndNil(CompletedSendDatagram)
      end else
      begin
        BufferDatagramReceive := DatagramReceiveManager.Process(Helper);  // DatagramReceive object is created and given to the thread
        if Assigned(BufferDatagramReceive) then
        begin
          OlcbTaskManager.ProcessReceiving(BufferDatagramReceive);        // Give the Task subsystem a crack at knowning about the received datagram
          FreeAndNil(BufferDatagramReceive)
        end;
      end;
    end else                                                              // *** Test for a Datagram message that came in ***
      OlcbTaskManager.ProcessReceiving(Helper);

    if Helper.Layer = ol_CAN then
    begin
      CANLayerTask := TCANLayerTask.Create(Helper.DestinationAliasID, Helper.SourceAliasID, True);
      CANLayerTask.OnBeforeDestroy := OnBeforeDestroyTask;
      Helper.CopyTo(CANLayerTask.MessageHelper);
      AddTask(CANLayerTask);
    end;

    case Helper.MTI of
      MTI_INITIALIZATION_COMPLETE :
        begin
          InitializationCompleteTask := TInitializationCompleteTask.Create(Helper.DestinationAliasID, Helper.SourceAliasID, True);
          InitializationCompleteTask.OnBeforeDestroy := OnBeforeDestroyTask;
          Helper.CopyTo(InitializationCompleteTask.MessageHelper);
          AddTask(InitializationCompleteTask);
        end;
      MTI_VERIFIED_NODE_ID_NUMBER :
        begin
          VerifiedNodeIDTask := TVerifiedNodeIDTask.Create(Helper.DestinationAliasID, Helper.SourceAliasID, True);
          VerifiedNodeIDTask.OnBeforeDestroy := OnBeforeDestroyTask;
          Helper.CopyTo(VerifiedNodeIDTask.MessageHelper);
          AddTask(VerifiedNodeIDTask);
        end;
      MTI_CONSUMER_IDENTIFIED_CLEAR,
      MTI_CONSUMER_IDENTIFIED_SET,
      MTI_CONSUMER_IDENTIFIED_UNKNOWN,
      MTI_CONSUMER_IDENTIFIED_RESERVED,
      MTI_PRODUCER_IDENTIFIED_CLEAR,
      MTI_PRODUCER_IDENTIFIED_SET,
      MTI_PRODUCER_IDENTIFIED_UNKNOWN,
      MTI_PRODUCER_IDENTIFIED_RESERVED,
      MTI_PC_EVENT_REPORT :
        begin
          EventTask := TEventTask.Create(Helper.DestinationAliasID, Helper.SourceAliasID, True);
          EventTask.OnBeforeDestroy := OnBeforeDestroyTask;
          Helper.CopyTo(EventTask.MessageHelper);
          AddTask(EventTask);
        end;
      MTI_TRACTION_PROTOCOL :
        begin
          TractionProtocolTask := TTractionProtocolTask.Create(Helper.DestinationAliasID, Helper.SourceAliasID, True);
          TractionProtocolTask.OnBeforeDestroy := OnBeforeDestroyTask;
          Helper.CopyTo(TractionProtocolTask.MessageHelper);
          AddTask(TractionProtocolTask);
        end;
    end;
  end;
end;

procedure TTransportLayerThread.ExecuteBegin;
begin
  FRunning := True;
end;

procedure TTransportLayerThread.ExecuteEnd;
begin
  FRunning := False;
  FTerminateComplete := True;
end;

procedure TTransportLayerThread.SyncErrorMessage;
begin
  if Assigned(SyncErrorMessageFunc) then
    SyncErrorMessageFunc(BufferRawMessage)
end;

procedure TTransportLayerThread.SyncReceiveMessage;
begin
  if Assigned(SyncReceiveMessageFunc) then
    SyncReceiveMessageFunc(BufferRawMessage)
end;

procedure TTransportLayerThread.SyncSendMessage;
begin
  if Assigned(SyncSendMessageFunc) then
    SyncSendMessageFunc(BufferRawMessage)
end;

constructor TTransportLayerThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FThreadListSendStrings := TThreadList.Create;
  FDatagramReceiveManager := TDatagramReceiveManager.Create(Self);
  FDatagramSendManager := TDatagramSendManager.Create(Self);
  FOlcbTaskManager := TOlcbTaskEngine.Create(Self);
  FTerminateComplete := False;
  FEnableReceiveMessages := True;
  FEnableSendMessages := True;
  FSyncErrorMessageFunc := nil;
  FSyncReceiveMessageFunc := nil;
  FSyncSendMessageFunc := nil;
  FLoopTime := 0;
  FMaxLoopTime := 0;
  OnBeforeDestroyTask := nil;
end;

destructor TTransportLayerThread.Destroy;
var
  List: TList;
begin
  List := ThreadListSendStrings.LockList;
  try
    if List.Count <> 0 then
      TStringList( List[0]).Free;
  finally
    ThreadListSendStrings.UnLockList;
  end;
  FreeAndNil(FThreadListSendStrings);   // Thread does not own the items so just empty the list
  DatagramReceiveManager.Clear;
  FreeAndNil(FDatagramReceiveManager);
  DatagramSendManager.Clear;
  FreeAndNil(FDatagramSendManager);
  inherited Destroy;
end;

procedure TTransportLayerThread.Add(Msg: AnsiString);
var
  List: TList;
  StringList: TStringList;
begin
  List := ThreadListSendStrings.LockList;
  try
    if List.Count = 0 then
    begin
      StringList := TStringList.Create;
      StringList.Delimiter := ';';
      List.Add( StringList);
    end;
    StringList := TStringList( List[0]);
    StringList.Add(Msg);
  finally
    ThreadListSendStrings.UnLockList;
  end;
end;

procedure TTransportLayerThread.AddDatagramToSend(Datagram: TDatagramSend);
var
  List: TList;
begin
  List := DatagramSendManager.Datagrams.LockList;
  try
    List.Add(Datagram);
    if List.Count > DatagramSendManager.MaxCount then
      DatagramSendManager.MaxCount := List.Count;
  finally
    DatagramSendManager.Datagrams.UnLockList
  end;
end;

procedure TTransportLayerThread.AddTask(NewTask: TOlcbTaskBase);
var
  List: TList;
begin
  List := OlcbTaskManager.TaskList.LockList;
  try
    NewTask.FTransportLayerThread := Self;
    List.Add(NewTask);
    if List.Count > OlcbTaskManager.MaxCount then
      OlcbTaskManager.MaxCount := List.Count;
  finally
    OlcbTaskManager.TaskList.UnlockList;
  end;
end;

procedure TTransportLayerThread.RemoveAndFreeTasks(RemoveKey: PtrInt);
var
  List: TList;
  i: Integer;
  Wait: Boolean;
begin
  List := OlcbTaskManager.TaskList.LockList;
  try
    i := List.count;
    for i := List.Count - 1 downto 0 do
    begin
      if (TOlcbTaskBase( List[i]).RemoveKey = RemoveKey) then
      begin
        TOlcbTaskBase( List[i]).ForceTermination := True;
        TOlcbTaskBase( List[i]).Free;
        List.Delete(i);
      end;
    end;
  finally
    OlcbTaskManager.TaskList.UnlockList;
  end;

  // How to make sure Tasks are freed before Config Editor is released?
  // Deadlock if Syncronize is called in the thread if we are waiting here for the Task to complete......

  repeat
    Application.ProcessMessages;
    ThreadSwitch;
    List := OlcbTaskManager.TaskList.LockList;
    try
      i := List.count;
      Wait := False;
      for i := List.Count - 1 downto 0 do
      begin
        if (TOlcbTaskBase( List[i]).RemoveKey = RemoveKey) then
        begin
          if TOlcbTaskBase( List[i]).Done or not TOlcbTaskBase( List[i]).HasStarted then
          begin
            TOlcbTaskBase( List[i]).Free;
            List.Delete(i);
          end else
            Wait := True;
        end;
      end;
    finally
    OlcbTaskManager.TaskList.UnlockList;
    end;
  until Wait = False;
end;


{ TOlcbSNIP }

constructor TOlcbSNIP.Create;
begin
  FSniiHardwareVersion := '';
  FSniiMfgModel := '';
  FSniiMfgName := '';
  FSniiMfgVersion := 0;
  FSniiSoftwareVersion := '';
  FSniiUserDescription := '';
  FSniiUserName := '';
  FSniiUserVersion := 0;
end;

procedure TOlcbSNIP.CopyTo(Target: TOlcbStructureHelperBase);
var
  X: TOlcbSNIP;
begin
  if Target is TOlcbSNIP then
  begin
    X := TOlcbSNIP( Target);
    X.FSniiHardwareVersion := SniiHardwareVersion;
    X.FSniiMfgModel := SniiMfgModel;
    X.FSniiMfgName := SniiMfgName;
    X.FSniiMfgVersion := SniiMfgVersion;
    X.FSniiSoftwareVersion := SniiSoftwareVersion;
    X.FSniiUserDescription := SniiUserDescription;
    X.FSniiUserName := SniiUserName;
    X.FSniiUserVersion := SniiUserVersion;
  end;
end;

{ TOlcbProtocolIdentification }

function TOlcbProtocolIdentification.GetAbbreviatedCDIProtocol: Boolean;
begin
  Result := Mask and PIP_ABBREVIATED_CDI = PIP_ABBREVIATED_CDI;
end;

function TOlcbProtocolIdentification.GetConfigDescriptionInfoProtocol: Boolean;
begin
  Result := Mask and PIP_CDI = PIP_CDI;
end;

function TOlcbProtocolIdentification.GetDatagramProtocol: Boolean;
begin
  Result := Mask and PIP_DATAGRAM = PIP_DATAGRAM;
end;

function TOlcbProtocolIdentification.GetFunctionStateInformationProtocol: Boolean;
begin
  Result := Mask and PIP_FSI = PIP_FSI
end;

function TOlcbProtocolIdentification.GetDisplayProtocol: Boolean;
begin
  Result := Mask and PIP_DISPLAY = PIP_DISPLAY;
end;

function TOlcbProtocolIdentification.GetEventExchangeProtocol: Boolean;
begin
  Result := Mask and PIP_EVENT_EXCHANGE = PIP_EVENT_EXCHANGE;
end;

function TOlcbProtocolIdentification.GetFunctionDescriptionInfoProtocol: Boolean;
begin
  Result := Mask and PIP_FDI = PIP_FDI;
end;

function TOlcbProtocolIdentification.GetIdentificationProtocol: Boolean;
begin
  Result := Mask and PIP_IDENTIFCIATION = PIP_IDENTIFCIATION;
end;

function TOlcbProtocolIdentification.GetMemoryConfigProtocol: Boolean;
begin
  Result := Mask and PIP_MEMORY_CONFIG = PIP_MEMORY_CONFIG;
end;

function TOlcbProtocolIdentification.GetRemoteButtonProtocol: Boolean;
begin
  Result := Mask and PIP_REMOTE_BUTTON = PIP_REMOTE_BUTTON;
end;

function TOlcbProtocolIdentification.GetReservationProtocol: Boolean;
begin
  Result := Mask and PIP_RESERVATION = PIP_RESERVATION;
end;

function TOlcbProtocolIdentification.GetSimpleNodeInfoProtocol: Boolean;
begin
  Result := Mask and PIP_SIMPLE_NODE_ID = PIP_SIMPLE_NODE_ID;
end;

function TOlcbProtocolIdentification.GetSimpleProtocol: Boolean;
begin
  Result := Mask and PIP_PIP = PIP_PIP;
end;

function TOlcbProtocolIdentification.GetStreamProtocol: Boolean;
begin
  Result := Mask and PIP_STREAM = PIP_STREAM;
end;

function TOlcbProtocolIdentification.GetTeachingLearningConfigProtocol: Boolean;
begin
  Result := Mask and PIP_TEACH_LEARN = PIP_TEACH_LEARN;
end;

function TOlcbProtocolIdentification.GetTractionControlProtocol: Boolean;
begin
  Result := Mask and PIP_TRACTION = PIP_TRACTION;
end;

procedure TOlcbProtocolIdentification.CopyTo(Target: TOlcbStructureHelperBase);
var
  X: TOlcbProtocolIdentification;
begin
  if Target is TOlcbProtocolIdentification then
  begin
    X := TOlcbProtocolIdentification( Target);
    X.Mask := Mask;
  end;
end;

procedure TOlcbProtocolIdentification.LoadByMessage(AHelper: TOpenLCBMessageHelper);
begin
  FMask := AHelper.ExtractDataBytesAsInt(2, 7);
end;

{ TOlcbMemOptions }

function TOlcbMemOptions.GetReadFromMfgACDI: Boolean;
begin
  Result := OperationMask and MCO_ACDI_USER_READS = MCO_ACDI_USER_READS
end;

function TOlcbMemOptions.GetReadFromUserACDI: Boolean;
begin
  Result := OperationMask and MCO_ACDI_MFG_READS = MCO_ACDI_MFG_READS
end;

function TOlcbMemOptions.GetUnAlignedReads: Boolean;
begin
  Result := OperationMask and MCO_UNALIGNED_READS = MCO_UNALIGNED_READS
end;

function TOlcbMemOptions.GetUnAlignedWrites: Boolean;
begin
  Result := OperationMask and MCO_UNALIGNED_WRITES = MCO_UNALIGNED_WRITES
end;

function TOlcbMemOptions.GetWrite64Bytes: Boolean;
begin
  Result := WriteLengthMask and MCWL_64_BYTE = MCWL_64_BYTE
end;

function TOlcbMemOptions.GetWriteArbitraryBytes: Boolean;
begin
  Result := WriteLengthMask and MCWL_ARBITRARY_BYTE = MCWL_ARBITRARY_BYTE
end;

function TOlcbMemOptions.GetWriteFourBytes: Boolean;
begin
  Result := WriteLengthMask and MCWL_FOUR_BYTE = MCWL_FOUR_BYTE
end;

function TOlcbMemOptions.GetWriteOneByte: Boolean;
begin
  Result := WriteLengthMask and MCWL_ONE_BYTE = MCWL_ONE_BYTE
end;

function TOlcbMemOptions.GetWriteStreamBytes: Boolean;
begin
  Result := WriteLengthMask and MCWL_STREAM_WRITE_SUPPORTED = MCWL_STREAM_WRITE_SUPPORTED;
end;

function TOlcbMemOptions.GetWriteToUserACDI: Boolean;
begin
   Result := OperationMask and MCO_ACDI_USER_WRITES  = MCO_ACDI_USER_WRITES;
end;

function TOlcbMemOptions.GetWriteTwoBytes: Boolean;
begin
  Result := WriteLengthMask and MCWL_TWO_BYTE = MCWL_TWO_BYTE
end;

function TOlcbMemOptions.GetWriteUnderMask: Boolean;
begin
  Result := OperationMask and MCO_WRITE_UNDER_MASK = MCO_WRITE_UNDER_MASK
end;

constructor TOlcbMemOptions.Create;
begin
  FAddressSpaceHi := 0;
  FAddressSpaceLo := 0;
  FDescription := '';
  FOperationMask := 0;
  FWriteLengthMask := 0;
end;

procedure TOlcbMemOptions.CopyTo(Target: TOlcbStructureHelperBase);
var
  X: TOlcbMemOptions;
begin
  if Target is TOlcbMemOptions then
  begin
    X := TOlcbMemOptions( Target);
    X.FAddressSpaceHi := AddressSpaceHi;
    X.FAddressSpaceLo := AddressSpaceLo;
    X.FDescription := Description;
    X.FOperationMask := OperationMask;
    X.FWriteLengthMask := WriteLengthMask;
  end;
end;

procedure TOlcbMemOptions.LoadFromDatagram(Datagram: TDatagramReceive);
var
  i: Integer;
begin
  if (Datagram.RawDatagram[0] = DATAGRAM_PROTOCOL_CONFIGURATION) and (Datagram.RawDatagram[1] and MCP_OP_GET_CONFIG_REPLY = MCP_OP_GET_CONFIG_REPLY) then
  begin
    FOperationMask := Datagram.ExtractDataBytesAsInt(2, 3);
    FWriteLengthMask := Datagram.RawDatagram[4];
    FAddressSpaceHi := Datagram.RawDatagram[5];
    FAddressSpaceLo := Datagram.RawDatagram[6];
    i := 7;
    while (Chr( Datagram.RawDatagram[i]) <> #0) and (i < MAX_DATAGRAM_LENGTH) do
    begin
      FDescription[i - 7] := Chr( Datagram.RawDatagram[i]);
      Inc(i)
    end;
  end;
end;

{ TOlcbMemConfig }

function TOlcbMemConfig.GetAddressSpace(Index: Integer): TOlcbMemAddressSpace;
begin
  Result := nil;
  if (Index > -1) and (Index < AddressSpaceList.Count) then
    Result := TOlcbMemAddressSpace( AddressSpaceList[Index])
end;

function TOlcbMemConfig.GetAddressCount: Integer;
begin
  Result := AddressSpaceList.Count
end;

procedure TOlcbMemConfig.SetAddressSpace(Index: Integer; AValue: TOlcbMemAddressSpace);
begin
  if (Index > -1) and (Index < AddressSpaceList.Count) then
    AddressSpaceList[Index] := AValue                      // We don't free it
end;

procedure TOlcbMemConfig.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to AddressSpaceList.Count - 1 do
      TObject( FAddressSpaceList[i]).Free;
  finally
    AddressSpaceList.Clear;
  end;
end;

constructor TOlcbMemConfig.Create;
begin
  inherited Create;
  FAddressSpaceList := TList.Create;
  FOptions := TOlcbMemOptions.Create;
end;

destructor TOlcbMemConfig.Destroy;
begin
  Clear;
  FreeAndNil(FAddressSpaceList);
  FreeAndNil(FOptions);
  inherited Destroy;
end;

function TOlcbMemConfig.AddAddressSpace: TOlcbMemAddressSpace;
begin
  Result := TOlcbMemAddressSpace.Create;
  AddressSpaceList.Add(Result);
end;

function TOlcbMemConfig.AddAddressSpaceByDatagram(Datagram: TDatagramReceive): TOlcbMemAddressSpace;
begin
  Result := FindAddressSpaceBySpaceID(Datagram.RawDatagram[2]);
  if not Assigned(Result) then
  begin
    Result := TOlcbMemAddressSpace.Create;
    AddressSpaceList.Add(Result);
  end;
  Result.LoadByDatagram(Datagram);
end;

procedure TOlcbMemConfig.CopyTo(Target: TOlcbStructureHelperBase);
var
  X: TOlcbMemConfig;
  i: Integer;
  NewSpace: TOlcbMemAddressSpace;
begin
  if Target is TOlcbMemConfig then
  begin
    X := TOlcbMemConfig( Target);
    X.Clear;
    for i := 0 to AddressSpaceCount - 1 do
    begin
      NewSpace := TOlcbMemAddressSpace.Create;
      AddressSpace[i].CopyTo(NewSpace);
      X.AddressSpaceList.Add(NewSpace);
    end;
  end;
  Options.CopyTo(X.Options);
end;

function TOlcbMemConfig.FindAddressSpaceBySpaceID(AnAddress: Byte): TOlcbMemAddressSpace;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  while (i < AddressSpaceList.Count) and not Assigned(Result) do
  begin
    if AddressSpace[i].Space = AnAddress then
      Result := AddressSpace[i];
    Inc(i);
  end;
end;


{ TOlcbMemAddressSpace }

function TOlcbMemAddressSpace.GetAddressSize: DWord;
begin
  Result := AddressHi-AddressLo
end;

function TOlcbMemAddressSpace.GetSpaceAsHex: string;
begin
  Result := IntToHex(FSpace, 2);
end;

constructor TOlcbMemAddressSpace.Create;
begin
  FAddressHi := 0;
  FAddressLo := 0;
  FAddressLoImpliedZero := True;
  FDescription := '';
  FIsReadOnly := False;
  FSpace := 0;
  FIsPresent := False;
end;

procedure TOlcbMemAddressSpace.CopyTo(Target: TOlcbStructureHelperBase);
var
  X: TOlcbMemAddressSpace;
begin
  if Target is TOlcbMemAddressSpace then
  begin
    X := TOlcbMemAddressSpace( Target);
    X.FAddressHi := AddressHi;
    X.FAddressLo := AddressLo;
    X.FAddressLoImpliedZero := AddressLoImpliedZero;
    X.FDescription := Description;
    X.FIsPresent := IsPresent;
    X.FIsReadOnly := IsReadOnly;
    X.FSpace := Space;
  end;
end;

procedure TOlcbMemAddressSpace.LoadByDatagram(ADatagram: TDatagramReceive);
var
  DescriptionStart: Integer;
  Done: Boolean;
begin
  FAddressHi := ADatagram.ExtractDataBytesAsInt(3, 6);
  FAddressLo := 0;
  FAddressLoImpliedZero := True;
  FIsPresent := ADatagram.RawDatagram[1] and MCP_OP_GET_ADD_SPACE_INFO_REPLY_PRESENT = MCP_OP_GET_ADD_SPACE_INFO_REPLY_PRESENT;
  FIsReadOnly := ADatagram.RawDatagram[7] and $01 = $01;
  FSpace := ADatagram.RawDatagram[2];
  if ADatagram.CurrentPos > 8 then
  begin
    if ADatagram.RawDatagram[7] and $02 = $02 then
    begin
      FAddressLo := ADatagram.ExtractDataBytesAsInt(8, 11);
      DescriptionStart := 12;
    end else
      DescriptionStart := 8;
    Done := False;
    while not Done and (DescriptionStart < MAX_DATAGRAM_LENGTH) do
    begin
      FDescription := FDescription + Chr( ADatagram.RawDatagram[DescriptionStart]);
      Done := Chr( ADatagram.RawDatagram[DescriptionStart]) = #0;
      Inc(DescriptionStart);
    end;
  end;
end;

{ TDatagramSendManager }

procedure TDatagramSendManager.TimerTick(Sender: TObject);
var
  SendDatagram: TDatagramSend;
  i: Integer;
  List: TList;
  AbandonList: TList;
begin
  List := Datagrams.LockList;
  AbandonList := AbandonDatagrams.LockList;
  try
  for i := List.Count - 1 downto 0 do     // May remove the item so need to go from the top down
  begin
    SendDatagram := TDatagramSend( List[i]);
    if not SendDatagram.Empty then
    begin
      if SendDatagram.AbandonTime > 4000 then
      begin
        List.Remove(SendDatagram);
        AbandonList.Add(SendDatagram);
      end;
      SendDatagram.AbandonTime := SendDatagram.AbandonTime + Timer.Interval;
    end;
  end;
  finally
    Datagrams.UnlockList;
    AbandonDatagrams.UnlockList;
  end;
end;


constructor TDatagramSendManager.Create(AnOwner: TTransportLayerThread);
begin
  inherited Create;
  FDatagrams := TThreadList.Create;
  FAbandonDatagrams := TThreadList.Create;
  FOwner := AnOwner;
  Timer := TTimer.Create(nil);
  Timer.Interval := 500;         // Every 500m seconds
  Timer.OnTimer := @TimerTick;
  Timer.Enabled := True;
  FMaxCount := 0;
end;

destructor TDatagramSendManager.Destroy;
begin
  Clear;
  FreeAndNil(FDatagrams);
  ClearAbandon;
  FreeAndNil(FAbandonDatagrams);
  FreeAndNil(FTimer);
  inherited Destroy;
end;

procedure TDatagramSendManager.Clear;
var
  i: Integer;
  List: TList;
begin
  List := Datagrams.LockList;
  try
    for i := 0 to List.Count - 1 do
      TObject( List[i]).Free;
  finally
    List.Clear;
    Datagrams.UnlockList;
  end;
end;

procedure TDatagramSendManager.ClearAbandon;
var
  i: Integer;
  List: TList;
begin
  List := AbandonDatagrams.LockList;
  try
    for i := 0 to List.Count - 1 do
      TObject( List[i]).Free;
  finally
    List.Clear;
    AbandonDatagrams.UnlockList;
  end;
end;

function TDatagramSendManager.ProcessReceive(AHelper: TOpenLCBMessageHelper): TDatagramSend;
var
  Datagram: TDatagramSend;
  List: TList;
  i: Integer;
  Done: Boolean;
begin
  Result := nil;
  List := Datagrams.LockList;
  try
    i := 0;
    Done := False;
    while (i < List.Count) and not Done do
    begin
      Datagram := TDatagramSend( List[i]);
      if Datagram.ProcessReceive(AHelper) then
      begin
        Done := True;
        if Datagram.Empty then
        begin
          List.Remove(Datagram);
          Result := Datagram;
        end;
      end;
      Inc(i);
    end;
  finally
    Datagrams.UnlockList;
  end;
end;

procedure TDatagramSendManager.ProcessSend;
var
  Datagram: TDatagramSend;
  List: TList;
  i: Integer;
  Done: Boolean;
begin
  List := Datagrams.LockList;
  try
    i := 0;
    Done := False;
    while (i < List.Count) and not Done do
    begin
      Datagram := TDatagramSend( List[i]);
      Done := Datagram.ProcessSend(Owner);
      Inc(i);
    end;
  finally
    Datagrams.UnlockList;
  end;
end;

{ TDatagramReceiveManager }

function TDatagramReceiveManager.FindInProcessDatagram(AHelper: TOpenLCBMessageHelper): TDatagramReceive;
//
// Searches an in process datagram interaction between the nodes in the message
//
var
  i: Integer;
  List: TList;
  Datagram: TDatagramReceive;
begin
  i := 0;
  Result := nil;
  List := Datagrams.LockList;
  try
    while not Assigned(Result) and (i < List.Count) do
    begin
      Datagram := TDatagramReceive( List[i]);
      if not Datagram.Full and (Datagram.DestinationAlias = AHelper.SourceAliasID) and (Datagram.SourceAlias = AHelper.DestinationAliasID) then
        Result := Datagram;
      Inc(i)
    end;
  finally
    Datagrams.UnlockList;
  end;
end;

constructor TDatagramReceiveManager.Create(AnOwner: TTransportLayerThread);
begin
  inherited Create;
  FOwner := AnOwner;
  FDatagrams := TThreadList.Create;
  FMaxCount := 0;
end;

destructor TDatagramReceiveManager.Destroy;
begin
  Clear;
  FreeAndNil(FDatagrams);
  inherited Destroy;
end;

procedure TDatagramReceiveManager.Clear;
var
  i: Integer;
  List: TList;
begin
  List := Datagrams.LockList;
  try
    for i := 0 to List.Count - 1 do
      TObject( List[i]).Free;
  finally
    List.Clear;
    Datagrams.UnlockList;
  end;
end;

function TDatagramReceiveManager.Process(AHelper: TOpenLCBMessageHelper): TDatagramReceive;
var
  TestDatagram: TDatagramReceive;
  List: TList;
begin
  Result := nil;
  if IsDatagramMTI(AHelper.MTI, False) then
  begin
    TestDatagram := FindInProcessDatagram(AHelper);
    if not Assigned(TestDatagram) then
    begin
      TestDatagram := TDatagramReceive.Create(Owner.SourceAlias, AHelper.SourceAliasID);  // Create a new receiving Datagram object for source alias of the message to us
      Datagrams.Add(TestDatagram);
      List := Datagrams.LockList;
      if List.Count > MaxCount then MaxCount := List.Count;
      Datagrams.UnlockList;
    end;
    TestDatagram.Process(AHelper, Owner);
    if TestDatagram.Full then
    begin
      Datagrams.Remove(TestDatagram);
      Result := TestDatagram
    end;
  end;
end;

{ TDatagramSend }

// *****************************************************************************
// Pulls the next 1..8 byte(s) from the Stream and puts them in the CAN Array to send, it always assumes the Stream Position of were to start
// in the Stream
//   Offset: [in] Where in the CAN array to use as the start of the transfer (may have some header bytes that need to be skipped)
//   ByteArray:  [out] CAN ByteArray that will be sent on the wire
//   Count    :  [out] The number of bytes that were transfered to the Byte Array
// *****************************************************************************
procedure TDatagramSend.StreamBytesToByteArray(Offset: Byte; var ByteArray: TCANByteArray; var Count: Byte);
begin
  Count := 0;
  // Tests for breaking out of loop:
  //   The Byte Array has 8 valid bytes (A)
  //   Finished moving all of the Stream Bytes (B)
  //   Sent a full 64 byte datagram, need to send another one (C)
  while (Offset < 8) {A} and (Stream.Position < Stream.Size) {B} and (BlockByteCount < MAX_DATAGRAM_LENGTH) do
  begin
    ByteArray[Offset] := Stream.ReadByte;
    Inc(Count);
    Inc(Offset);
    Inc(FBlockByteCount);
  end;
end;

// *****************************************************************************
// Creates the datagram object
// *****************************************************************************
constructor TDatagramSend.Create;
begin
  inherited;
  FStream := TMemoryStream.Create;
  FLocalHelper := TOpenLCBMessageHelper.Create;
end;

// *****************************************************************************
// Destroys the datagram object
// *****************************************************************************
destructor TDatagramSend.Destroy;
begin
  FreeAndNil(FStream);
  FreeAndNil(FLocalHelper);
  inherited;
end;

// *****************************************************************************
// Loads and initalizes the object to start sending the datagram
// *****************************************************************************
procedure TDatagramSend.Initialize(AStream: TStream; AProtocolHeader: TCANByteArray; AProtocolHeaderLen: Byte; ASourceAlias, ADestinationAlias: Word);
begin
  Stream.Position := 0;
  if Assigned(AStream) then
  begin
    Assert(AStream.Size > 64 - Int64( AProtocolHeaderLen), 'Stream in Datagram Send too long, 64 bytes max');
    AStream.Position := 0;
    Stream.CopyFrom(AStream, AStream.Size);
    Stream.Position := 0;
  end;
  FProtocolHeader := AProtocolHeader;
  FProtocolHeaderLen := AProtocolHeaderLen;
  FSourceAlias := ASourceAlias;
  FDestinationAlias := ADestinationAlias;
  FBlockStartPos := 0;
  FEmpty := False;
  FWaitingForACK := False;
  FErrorCode := $0000;
  FRetryCount := 0;
  FAbandonTime := 0;
  FNewStartFrame := True;
  FBlockByteCount := 0;
end;

// *****************************************************************************
// Call repeatedly in the statemachine until Empty = True
//   TransportLayerThread: Thread to send the messages to
// *****************************************************************************
function TDatagramSend.ProcessSend(TransportLayerThread: TTransportLayerThread): Boolean;
var
  Count: Byte;
begin
  Result := False;
  Count := 0;
  if not Empty and not WaitingForACK then
  begin
    if NewStartFrame then
    begin
      // Starting a new packet, need to decide if it fits in a single frame
      if Stream.Size + ProtocolHeaderLen <= 8 then
      begin
        MTI := MTI_FRAME_TYPE_DATAGRAM_ONLY_FRAME;
        WaitingForACK := True;
      end else
        MTI := MTI_FRAME_TYPE_DATAGRAM_FRAME_START;
      NewStartFrame := False;
      BlockStartPos := Stream.Position;
      DataBytesSent := ProtocolHeader;
      BlockByteCount := DataBytesSentLen;
      StreamBytesToByteArray(ProtocolHeaderLen, FDataBytesSent, Count);
      DataBytesSentLen := Count + ProtocolHeaderLen;
      LocalHelper.Load(ol_OpenLCB, MTI, SourceAlias, DestinationAlias, DataBytesSentLen, DataBytesSent[0], DataBytesSent[1], DataBytesSent[2], DataBytesSent[3], DataBytesSent[4], DataBytesSent[5], DataBytesSent[6], DataBytesSent[7]);
      TransportLayerThread.Add(LocalHelper.Encode);
    end else
    begin
      if (Stream.Size - Stream.Position <= 8) or (MAX_DATAGRAM_LENGTH - BlockByteCount <= 8) then
      begin
        MTI := MTI_FRAME_TYPE_DATAGRAM_FRAME_END;
        WaitingForACK := True;
      end else
        MTI := MTI_FRAME_TYPE_DATAGRAM_FRAME;
      StreamBytesToByteArray(0, FDataBytesSent, Count);
      DataBytesSentLen := Count;
      LocalHelper.Load(ol_OpenLCB, MTI, SourceAlias, DestinationAlias, DataBytesSentLen, DataBytesSent[0], DataBytesSent[1], DataBytesSent[2], DataBytesSent[3], DataBytesSent[4], DataBytesSent[5], DataBytesSent[6], DataBytesSent[7]);
      TransportLayerThread.Add(LocalHelper.Encode);
    end;
    Result := True;
  end;
end;

function TDatagramSend.ProcessReceive(AHelper: TOpenLCBMessageHelper): Boolean;
//
// It is assumed that the message is actually for this object and the object is not empty, it is not checked.........
begin
  Result := False;
  if not Empty and WaitingForACK then   // See if we are waiting for the node to sent us and ACK
  begin
    if (AHelper.SourceAliasID = DestinationAlias) and (AHelper.DestinationAliasID = SourceAlias) then
    begin
      case AHelper.MTI of
        MTI_DATAGRAM_OK_REPLY :
          begin
            if Stream.Position < Stream.Size then
            begin
              NewStartFrame := True;
              WaitingForACK := False;
            end else
              Empty := True
          end;
        MTI_DATAGRAM_REJECTED_REPLY :
          begin
             ErrorCode := AHelper.ExtractDataBytesAsInt(2, 3);
             if (ErrorCode and DATAGRAM_REJECTED_RESEND_MASK <> 0) and (RetryCount < DATAGRAM_MAX_RETRYS) then
             begin
               NewStartFrame := True;
               if Assigned(Stream) then
                 Stream.Position := BlockStartPos;
               ErrorCode := $0000;
               WaitingForACK := False;
               Empty := False;
               Inc(FRetryCount);                                                // Kick the resend off
             end else
               Empty := True                                                    // Error; don't resend and quit with ErrorCode set
          end;
      end;
      Result := True
    end;
  end
end;

{ TDatagramReceive }


// *****************************************************************************
// Clears the structures and fields of the object
// *****************************************************************************
procedure TDatagramReceive.Clear;
var
  i: Integer;
begin
  FCurrentPos := 0;
  FFull := False;
  for i := 0 to MAX_DATAGRAM_LENGTH - 1 do
    RawDatagram[i] := 0;
end;

procedure TDatagramReceive.SendACK(TransportLayerThread: TTransportLayerThread);
begin
  LocalHelper.Load(ol_OpenLCB, MTI_DATAGRAM_OK_REPLY, SourceAlias, DestinationAlias, 2, 0, 0, 0, 0, 0, 0, 0, 0);
  TransportLayerThread.Add(LocalHelper.Encode);
end;

constructor TDatagramReceive.Create(ASourceAlias, ADestinationAlias: Word);
begin
  inherited Create;
  FLocalHelper := TOpenLCBMessageHelper.Create;
  FDestinationAlias := ADestinationAlias;
  FSourceAlias := ASourceAlias;
end;

destructor TDatagramReceive.Destroy;
begin
  FreeAndNil(FLocalHelper);
  inherited;
end;

// *****************************************************************************
// Helper to concat up to 4 bytes in the Datagram Data Array into an integer/QWord
//  StartByteIndex: The first byte to use in the result (most significant 8 bits of integer/QWord)
//  EndByteIndex  : The last byte to use in the result (least significant 8 bits of integer/QWord)
// *****************************************************************************
function TDatagramReceive.ExtractDataBytesAsInt(StartByteIndex, EndByteIndex: Integer): QWord;
var
  i, Offset, Shift: Integer;
  ByteAsQ, ShiftedByte: QWord;
begin
  Result := 0;
  Offset := EndByteIndex - StartByteIndex;
  for i := StartByteIndex to EndByteIndex do
  begin
    Shift := Offset * 8;
    ByteAsQ := QWord( RawDatagram[i]);
    ShiftedByte := ByteAsQ shl Shift;
    Result := Result or ShiftedByte;
    Dec(Offset)
  end;
end;

// *****************************************************************************
// Helper to convert a section of the Datagram Array bytes to a string
//  StartIndex: The index of the byte in the Datagram Array as the first character of the string
//  Count     : The number of character in the string (bytes)
// *****************************************************************************
function TDatagramReceive.ExtractDataBytesAsString(StartIndex, Count: Integer): String;
var
  i, Last: Integer;
begin
  Result := '';
  if Count = -1 then
    Last := CurrentPos
  else
    Last := StartIndex + Count;
  if Last > CurrentPos then
    Last := CurrentPos;

  for i := StartIndex to Last - 1 do
    Result := Result + Chr( RawDatagram[i]);
end;

// *****************************************************************************
// Helper to copy the contents of the Datagram Array to a Stream
//   Stream:  The stream to copy the bytes to
//   StartIndex: The index of the byte in the Datagram Array to copy into the stream
//   Count     : The number of bytes to copy into the stream
// *****************************************************************************
procedure TDatagramReceive.CopyToStream(Stream: TStream; StartIndex, Count: Integer);
var
  i, Last: Integer;
begin
  if Count = -1 then
    Last := CurrentPos
  else
    Last := StartIndex + Count;

  if Last > CurrentPos then
    Last := CurrentPos;

  for i := StartIndex to Last - 1 do
    Stream.WriteByte(RawDatagram[i]);
end;

// *****************************************************************************
// Call as part of the statemachine as new OLCB messages are received.
//   AHelper:  The message that was received on the OLCB wire
//
//  NOTE:  It assumes that checks for the correct Source and Destination Alias's
//         have already been made.
//
// *****************************************************************************
procedure TDatagramReceive.Process(AHelper: TOpenLCBMessageHelper; TransportLayerThread: TTransportLayerThread);
var
  i: Integer;
begin
  case AHelper.MTI of
     MTI_FRAME_TYPE_DATAGRAM_ONLY_FRAME :
       begin
         Clear;
         for i := 0 to AHelper.DataCount - 1 do
           RawDatagram[i] := AHelper.Data[i];
         CurrentPos := AHelper.DataCount;
         SendACK(TransportLayerThread);
         Full := True
       end;
     MTI_FRAME_TYPE_DATAGRAM_FRAME_START :
       begin
         Full := False;
         for i := 0 to AHelper.DataCount - 1 do
           RawDatagram[i] := AHelper.Data[i];
         CurrentPos := AHelper.DataCount;
       end;
     MTI_FRAME_TYPE_DATAGRAM_FRAME :
       begin
         for i := 0 to AHelper.DataCount - 1 do
           RawDatagram[CurrentPos+i] := AHelper.Data[i];
         CurrentPos := CurrentPos + AHelper.DataCount;
       end;
    MTI_FRAME_TYPE_DATAGRAM_FRAME_END :
      begin
        for i := 0 to AHelper.DataCount - 1 do
          RawDatagram[CurrentPos+i] := AHelper.Data[i];
        CurrentPos := CurrentPos + AHelper.DataCount;
        SendACK(TransportLayerThread);
        Full := True;
      end;
  end;
end;

{ TOlcbTaskBase }

function TOlcbTaskBase.SpaceToCommandByteEncoding(ASpace: Byte): Byte;
begin
  case ASpace of
    MSI_CDI     : Result := MCP_CDI;
    MSI_ALL     : Result := MCP_ALL;
    MSI_CONFIG  : Result := MCP_CONFIGURATION
  else
    Result := MCP_NONE
  end;
end;

procedure TOlcbTaskBase.ExtractErrorInformation(DatagramReceive: TDatagramReceive);
var
  i, iChar: Integer;
begin
  if DatagramReceive.RawDatagram[1] and $03 = 0 then    // If using the {Space} byte need to skip over it
    i := 7
  else
    i := 6;
  ErrorCode := DatagramReceive.ExtractDataBytesAsInt(i, i+1);
  Inc(i, 2);
  if DatagramReceive.CurrentPos > i then
  begin
    // We have a Error String
    ErrorString := '';
    for iChar := i to DatagramReceive.CurrentPos - 1 do
    begin
      if Char( DatagramReceive.RawDatagram[iChar]) <> #0 then
        ErrorString := ErrorString + Char( DatagramReceive.RawDatagram[iChar]);
    end;
  end;
end;

function TOlcbTaskBase.IsDatagramAckFromDestination(MessageInfo: TOlcbMessage): Boolean;
var
  DatagramSend: TDatagramSend;
begin
  Result := False;
  if Assigned(MessageInfo) then
  begin
    if MessageInfo is TDatagramSend then                                 // Wait for the ACK from the send
    begin
      DatagramSend := TDatagramSend( MessageInfo);
      if DatagramSend.Empty and
         (DatagramSend.SourceAlias = SourceAlias) and
         (DatagramSend.DestinationAlias = DestinationAlias) then
      Result := True
    end;
  end;
end;

function TOlcbTaskBase.IsConfigMemorySpaceInfoReplyFromDestination(MessageInfo: TOlcbMessage; AnAddress: Byte; var DatagramReceive: TDatagramReceive): Boolean;
begin
  Result := False;
  DatagramReceive := nil;
  if Assigned(MessageInfo) then
  begin
    if MessageInfo is TDatagramReceive then
    begin
      DatagramReceive := TDatagramReceive(MessageInfo);
      if (DatagramReceive.RawDatagram[0] and
          DATAGRAM_PROTOCOL_CONFIGURATION = DATAGRAM_PROTOCOL_CONFIGURATION) and
         (DatagramReceive.RawDatagram[1] and $FE = MCP_OP_GET_ADD_SPACE_INFO_REPLY) and
         (DatagramReceive.RawDatagram[2] = AnAddress) and
         (DatagramReceive.SourceAlias = SourceAlias) and
         (DatagramReceive.DestinationAlias = DestinationAlias) then
      begin
        Result := True
      end;
    end;
  end;
end;

function TOlcbTaskBase.IsConfigMemoryOptionsReplyFromDestination(MessageInfo: TOlcbMessage; var DatagramReceive: TDatagramReceive): Boolean;
begin
  Result := False;
  DatagramReceive := nil;
  if Assigned(MessageInfo) then
  begin
    if MessageInfo is TDatagramReceive then
    begin
      DatagramReceive := TDatagramReceive(MessageInfo);
      if  (DatagramReceive.RawDatagram[0] and
           DATAGRAM_PROTOCOL_CONFIGURATION = DATAGRAM_PROTOCOL_CONFIGURATION) and
          (DatagramReceive.RawDatagram[1] and $FE = MCP_OP_GET_CONFIG_REPLY) and
          (DatagramReceive.SourceAlias = SourceAlias) and
          (DatagramReceive.DestinationAlias = DestinationAlias) then
        Result := True
    end;
  end;
end;

function TOlcbTaskBase.IsConfigMemoryReadReplyFromDestination(MessageInfo: TOlcbMessage; var DatagramReceive: TDatagramReceive): Boolean;
begin
  Result := False;
  DatagramReceive := nil;
  if Assigned(MessageInfo) then
  begin
    if MessageInfo is TDatagramReceive then
    begin
      DatagramReceive := TDatagramReceive(MessageInfo);
      if  (DatagramReceive.RawDatagram[0] and
          DATAGRAM_PROTOCOL_CONFIGURATION = DATAGRAM_PROTOCOL_CONFIGURATION) and
          (DatagramReceive.RawDatagram[1] and MCP_READ_DATAGRAM_REPLY = MCP_READ_DATAGRAM_REPLY) and
          (DatagramReceive.SourceAlias = SourceAlias) and
          (DatagramReceive.DestinationAlias = DestinationAlias) then
        Result := True
    end;
  end;
end;

function TOlcbTaskBase.IsProtocolIdentificationProcolReplyFromDestination(MessageInfo: TOlcbMessage): Boolean;
var
  Helper: TOpenLCBMessageHelper;
begin
  Result := False;
  if Assigned(MessageInfo) then
  begin
    if MessageInfo is TOpenLCBMessageHelper then
    begin
      Helper := TOpenLCBMessageHelper( MessageInfo);
      if (Helper.MTI = MTI_PROTOCOL_SUPPORT_REPLY) and
         (Helper.SourceAliasID = DestinationAlias) and
         (Helper.DestinationAliasID = SourceAlias) then
        Result := True;
    end;
  end;
end;

function TOlcbTaskBase.IsSnipMessageReply(MessageInfo: TOlcbMessage): Boolean;
var
  Helper: TOpenLCBMessageHelper;
begin
  Result := False;
  if Assigned(MessageInfo) then
  begin
    if MessageInfo is TOpenLCBMessageHelper then
    begin
      Helper := TOpenLCBMessageHelper( MessageInfo);
      if (Helper.MTI = MTI_SIMPLE_NODE_INFO_REPLY) and
         (Helper.SourceAliasID = DestinationAlias) and
         (Helper.DestinationAliasID = SourceAlias) then
        Result := True;
    end;
  end;
end;

function TOlcbTaskBase.IsTractionFunctionQueryReply(MessageInfo: TOlcbMessage): Boolean;
var
  Helper: TOpenLCBMessageHelper;
begin
  Result := False;
  if Assigned(MessageInfo) then
  begin
    if MessageInfo is TOpenLCBMessageHelper then
    begin
      Helper := TOpenLCBMessageHelper( MessageInfo);
      if (Helper.MTI = MTI_TRACTION_REPLY) and
         (Helper.SourceAliasID = DestinationAlias) and
         (Helper.DestinationAliasID = SourceAlias) and
         (Helper.Data[2] = TRACTION_QUERY_FUNCTION_REPLY) then
        Result := True;
    end;
  end;

end;

function TOlcbTaskBase.IsTractionSpeedsQueryFirstFrameReply(MessageInfo: TOlcbMessage): Boolean;
var
  Helper: TOpenLCBMessageHelper;
begin
  Result := False;
  if Assigned(MessageInfo) then
  begin
    if MessageInfo is TOpenLCBMessageHelper then
    begin
      Helper := TOpenLCBMessageHelper( MessageInfo);
      if (Helper.MTI = MTI_TRACTION_REPLY) and
         (Helper.SourceAliasID = DestinationAlias) and
         (Helper.DestinationAliasID = SourceAlias) and
         (Helper.Data[2] = TRACTION_QUERY_SPEED_REPLY) and
         (Helper.Data[0] and $F0 = $10) then
        Result := True;
    end;
  end;
end;

function TOlcbTaskBase.IsTractionSpeedsQuerySecondFrameReply(MessageInfo: TOlcbMessage): Boolean;
var
  Helper: TOpenLCBMessageHelper;
begin
  Result := False;
  if Assigned(MessageInfo) then
  begin
    if MessageInfo is TOpenLCBMessageHelper then
    begin
      Helper := TOpenLCBMessageHelper( MessageInfo);
      if (Helper.MTI = MTI_TRACTION_REPLY) and
         (Helper.SourceAliasID = DestinationAlias) and
         (Helper.DestinationAliasID = SourceAlias) and
         (Helper.Data[0] and $F0 = $20) then
        Result := True;
    end;
  end;

end;

function TOlcbTaskBase.IsTractionAttachDCCAddressReply(MessageInfo: TOlcbMessage): Boolean;
var
  Helper: TOpenLCBMessageHelper;
begin
  Result := False;
  if Assigned(MessageInfo) then
  begin
    if MessageInfo is TOpenLCBMessageHelper then
    begin
      Helper := TOpenLCBMessageHelper( MessageInfo);
      if (Helper.MTI = MTI_TRACTION_REPLY) and
         (Helper.SourceAliasID = DestinationAlias) and
         (Helper.DestinationAliasID = SourceAlias) and
         (Helper.Data[2] = TRACTION_CONFIGURE_PROXY_REPLY) and
         (Helper.Data[3] = TRACTION_ATTACH_DCC_ADDRESS_REPLY) then
        Result := True;
    end;
  end;
end;

function TOlcbTaskBase.IsTractionDetachDCCAddressReply(MessageInfo: TOlcbMessage): Boolean;
var
  Helper: TOpenLCBMessageHelper;
begin
  Result := False;
  if Assigned(MessageInfo) then
  begin
    if MessageInfo is TOpenLCBMessageHelper then
    begin
      Helper := TOpenLCBMessageHelper( MessageInfo);
      if (Helper.MTI = MTI_TRACTION_REPLY) and
         (Helper.SourceAliasID = DestinationAlias) and
         (Helper.DestinationAliasID = SourceAlias) and
         (Helper.Data[2] = TRACTION_CONFIGURE_PROXY_REPLY) and
         (Helper.Data[3] = TRACTION_DETACH_DCC_ADDRESS_REPLY) then
        Result := True;
    end;
  end;
end;

function TOlcbTaskBase.IsTractionAttachNodeQueryReply(MessageInfo: TOlcbMessage): Boolean;
var
  Helper: TOpenLCBMessageHelper;
begin
  Result := False;
  if Assigned(MessageInfo) then
  begin
    if MessageInfo is TOpenLCBMessageHelper then
    begin
      Helper := TOpenLCBMessageHelper( MessageInfo);
      if (Helper.MTI = MTI_TRACTION_REPLY) and
         (Helper.SourceAliasID = DestinationAlias) and
         (Helper.DestinationAliasID = SourceAlias) and
         (Helper.Data[2] = TRACTION_CONFIGURE_PROXY_REPLY) and
         (Helper.Data[3] = TRACTION_ATTACH_NODE_REPLY) then
        Result := True;
    end;
  end;
end;

function TOlcbTaskBase.IsTractionDetachNodeQueryReply(MessageInfo: TOlcbMessage): Boolean;
var
  Helper: TOpenLCBMessageHelper;
begin
  Result := False;
  if Assigned(MessageInfo) then
  begin
    if MessageInfo is TOpenLCBMessageHelper then
    begin
      Helper := TOpenLCBMessageHelper( MessageInfo);
      if (Helper.MTI = MTI_TRACTION_REPLY) and
         (Helper.SourceAliasID = DestinationAlias) and
         (Helper.DestinationAliasID = SourceAlias) and
         (Helper.Data[2] = TRACTION_CONFIGURE_PROXY_REPLY) and
         (Helper.Data[3] = TRACTION_DETACH_NODE_REPLY) then
        Result := True;
    end;
  end;
end;

function TOlcbTaskBase.IsTractionQueryProxyReply(MessageInfo: TOlcbMessage): Boolean;
var
  Helper: TOpenLCBMessageHelper;
begin
  Result := False;
  if Assigned(MessageInfo) then
  begin
    if MessageInfo is TOpenLCBMessageHelper then
    begin
      Helper := TOpenLCBMessageHelper( MessageInfo);
      if (Helper.MTI = MTI_TRACTION_REPLY) and
         (Helper.SourceAliasID = DestinationAlias) and
         (Helper.DestinationAliasID = SourceAlias) and
         (Helper.Data[2] = TRACTION_MANAGE_PROXY_REPLY) and
         (Helper.Data[3] = TRACTION_MANAGE_PROXY_QUERY) then
        Result := True;
    end;
  end;
end;

function TOlcbTaskBase.IsTractionReserveProxyReply(MessageInfo: TOlcbMessage): Boolean;
var
  Helper: TOpenLCBMessageHelper;
begin
  Result := False;
  if Assigned(MessageInfo) then
  begin
    if MessageInfo is TOpenLCBMessageHelper then
    begin
      Helper := TOpenLCBMessageHelper( MessageInfo);
      if (Helper.MTI = MTI_TRACTION_REPLY) and
         (Helper.SourceAliasID = DestinationAlias) and
         (Helper.DestinationAliasID = SourceAlias) and
         (Helper.Data[2] = TRACTION_MANAGE_PROXY_REPLY) and
         (Helper.Data[3] = TRACTION_MANAGE_PROXY_RESERVE) then
        Result := True;
    end;
  end;
end;

procedure TOlcbTaskBase.LogMsg(Msg: string);
begin
  if Log then
  begin
    if iState <> iLogState then
    begin
      if iState = STATE_DONE then
        LogStrings.Add('Class: ' +  Self.ClassName + #9 + '; State = STATE_DONE : ' + Msg)
      else
        LogStrings.Add('Class: ' +  Self.ClassName + #9 + '; State = ' + IntToStr(iState) + ': ' + Msg);
      iLogState := iState
    end;
  end;
end;

procedure TOlcbTaskBase.Process(MessageInfo: TOlcbMessage);
begin
  if not HasStarted then
  begin
    FHasStarted := True;
    LogStrings.Add('................................');
    LogMsg('Task starting');
    LogStrings.Add('................................');
  end;
  if ForceTermination then
  begin
    Sending := True;
    iState := STATE_DONE;
    LogMsg('Task force terminated');
    LogStrings.Add('...............................');
  end;
end;

procedure TOlcbTaskBase.SendIdentifyEventsMessage;
begin
  MessageHelper.Load(ol_OpenLCB, MTI_EVENTS_IDENTIFY, SourceAlias, 0, 0, 0, 0, 0, 0 ,0 ,0 ,0 ,0);
  TransportLayerThread.Add(MessageHelper.Encode);
end;

procedure TOlcbTaskBase.SendIdentifyEventsAddressedMessage;
begin
  MessageHelper.Load(ol_OpenLCB, MTI_EVENTS_IDENTIFY_DEST, SourceAlias, DestinationAlias, 2, 0, 0, 0, 0 ,0 ,0 ,0 ,0);
  TransportLayerThread.Add(MessageHelper.Encode);
end;

procedure TOlcbTaskBase.SendIdentifyConsumerMessage(Event: TEventID);
begin
  MessageHelper.Load(ol_OpenLCB, MTI_CONSUMER_IDENTIFY, SourceAlias, 0, 8, Event[0], Event[1], Event[2], Event[3], Event[4], Event[5], Event[6], Event[7]);
  TransportLayerThread.Add(MessageHelper.Encode);
end;

procedure TOlcbTaskBase.SendIdentifyProducerMessage(Event: TEventID);
begin
  MessageHelper.Load(ol_OpenLCB, MTI_PRODUCER_IDENDIFY, SourceAlias, 0, 8, Event[0], Event[1], Event[2], Event[3], Event[4], Event[5], Event[6], Event[7]);
  TransportLayerThread.Add(MessageHelper.Encode);
end;

procedure TOlcbTaskBase.SendMemoryConfigurationOptions;
var
  DatagramSend: TDatagramSend;
begin
  DatagramSend := TDatagramSend.Create;
  DatagramSend.Initialize(nil, HEADER_MEMCONFIG_OPTIONS_REQUEST, 2, SourceAlias, DestinationAlias);
  TransportLayerThread.AddDatagramToSend(DatagramSend);
end;

procedure TOlcbTaskBase.SendMemoryConfigurationSpaceInfo(Space: Byte);
var
  DatagramSend: TDatagramSend;
  CANByteArray: TCANByteArray;
begin
  DatagramSend := TDatagramSend.Create;
  CANByteArray := HEADER_MEMCONFIG_SPACE_INFO_UNKNOWN_REQUEST;
  CANByteArray[2] := Space;                                     // Set the address
  DatagramSend.Initialize(nil, CANByteArray, 3, SourceAlias, DestinationAlias);
  TransportLayerThread.AddDatagramToSend(DatagramSend);
end;

procedure TOlcbTaskBase.SendMemoryConfigurationRead(Space: Byte; StartAddress: DWord; Count: Byte; ForceUseOfSpaceByte: Boolean);
var
  DatagramSend: TDatagramSend;
  CANByteArray: TCANByteArray;
  HeaderByteCount: Byte;
begin
  DatagramSend := TDatagramSend.Create;
  CANByteArray[0] := DATAGRAM_PROTOCOL_CONFIGURATION;
  if ForceUseOfSpaceByte or (Space < MSI_CONFIG) then
  begin
    CANByteArray[1] := MCP_READ;
    CANByteArray[6] := Space;
    CANByteArray[7] := Count;
    HeaderByteCount := 8;
  end else
  begin
    CANByteArray[1] := MCP_READ or SpaceToCommandByteEncoding(Space);
    CANByteArray[6] := Count;
    HeaderByteCount := 7;
  end;
  CANByteArray[2] := (StartAddress shr 24) and $000000FF;
  CANByteArray[3] := (StartAddress shr 16) and $000000FF;
  CANByteArray[4] := (StartAddress shr 8) and $000000FF;
  CANByteArray[5] := StartAddress and $000000FF;

  DatagramSend.Initialize(nil, CANByteArray, HeaderByteCount, SourceAlias, DestinationAlias);
  TransportLayerThread.AddDatagramToSend(DatagramSend);
end;

procedure TOlcbTaskBase.SendMemoryConfigurationWrite(Space: Byte; StartAddress: DWord; MaxAddressSize: DWORD; ForceUseOfSpaceByte: Boolean; AStream: TStream);
var
  DatagramSend: TDatagramSend;
  CANByteArray: TCANByteArray;
  HeaderByteCount: Byte;
begin
  DatagramSend := TDatagramSend.Create;
  CANByteArray[0] := DATAGRAM_PROTOCOL_CONFIGURATION;
  if ForceUseOfSpaceByte or (Space < MSI_CONFIG) then
  begin
    CANByteArray[1] := MCP_WRITE;
    CANByteArray[6] := Space;
    HeaderByteCount := 7;
  end else
  begin
    CANByteArray[1] := MCP_WRITE or SpaceToCommandByteEncoding(Space);
    HeaderByteCount := 6;
  end;
  CANByteArray[2] := (StartAddress shr 24) and $000000FF;
  CANByteArray[3] := (StartAddress shr 16) and $000000FF;
  CANByteArray[4] := (StartAddress shr 8) and $000000FF;
  CANByteArray[5] := StartAddress and $000000FF;

  if AStream.Size > MaxAddressSize then
    AStream.Size := MaxAddressSize;
  DatagramSend.Initialize(AStream, CANByteArray, HeaderByteCount, SourceAlias, DestinationAlias);
  TransportLayerThread.AddDatagramToSend(DatagramSend);
end;

procedure TOlcbTaskBase.SendProtocolIdentificationProtocolMessage;
begin
  MessageHelper.Load(ol_OpenLCB, MTI_PROTOCOL_SUPPORT_INQUIRY, SourceAlias, DestinationAlias, 2, 0, 0, 0, 0 ,0 ,0 ,0 ,0);
  TransportLayerThread.Add(MessageHelper.Encode);
end;

procedure TOlcbTaskBase.SendSnipMessage;
begin
  MessageHelper.Load(ol_OpenLCB, MTI_SIMPLE_NODE_INFO_REQUEST, SourceAlias, DestinationAlias, 2, 0, 0, 0, 0 ,0 ,0 ,0 ,0);
  TransportLayerThread.Add(MessageHelper.Encode);
end;

procedure TOlcbTaskBase.SendTractionAttachDccProxyMessage(Address: Word; Short: Boolean; SpeedStep: Byte);
begin
  if not Short then
    Address := Address or $C000;
  MessageHelper.Load(ol_OpenLCB, MTI_TRACTION_PROTOCOL, SourceAlias, DestinationAlias, 7, $00, $00, TRACTION_CONFIGURE_PROXY, TRACTION_ATTACH_DCC_ADDRESS, Hi(Address), Lo(Address), SpeedStep, $00);
  TransportLayerThread.Add(MessageHelper.Encode);
end;

procedure TOlcbTaskBase.SendTractionDetachDccAddressProxyMessage(Address: Word; Short: Boolean);
begin
  MessageHelper.Load(ol_OpenLCB, MTI_TRACTION_PROTOCOL, SourceAlias, DestinationAlias, 6, $00, $00, TRACTION_CONFIGURE_PROXY, TRACTION_DETACH_DCC_ADDRESS_REPLY, Hi(Address), Lo(Address), $00, $00);
  TransportLayerThread.Add(MessageHelper.Encode);
end;

procedure TOlcbTaskBase.SendTractionEStopMessage;
begin
  MessageHelper.Load(ol_OpenLCB, MTI_TRACTION_PROTOCOL, SourceAlias, DestinationAlias, 3, $00, $00, TRACTION_E_STOP, $00, $00, $00, $00, $00);
  TransportLayerThread.Add(MessageHelper.Encode);
end;

procedure TOlcbTaskBase.SendTractionFunction(FunctionAddress: DWord; Value: Word);
begin
  MessageHelper.Load(ol_OpenLCB, MTI_TRACTION_PROTOCOL, SourceAlias, DestinationAlias, 8, $00, $00, TRACTION_FUNCTION, (FunctionAddress shr 16) and $00FF, (FunctionAddress shr 8) and $00FF, FunctionAddress and $00FF, Hi(Value), Lo(Value));
  TransportLayerThread.Add(MessageHelper.Encode);
end;

procedure TOlcbTaskBase.SendTractionQueryFunction(FunctionAddress: DWord);
begin
  MessageHelper.Load(ol_OpenLCB, MTI_TRACTION_PROTOCOL, SourceAlias, DestinationAlias, 6, $00, $00, TRACTION_QUERY_FUNCTION, (FunctionAddress shr 16) and $00FF, (FunctionAddress shr 8) and $00FF, FunctionAddress and $00FF, 0, 0);
  TransportLayerThread.Add(MessageHelper.Encode);
end;

procedure TOlcbTaskBase.SendTractionQueryDccAddressProxyMessage(Address: Word; Short: Boolean);
begin
  if not Short then
    Address := Address or $C000;
  MessageHelper.Load(ol_OpenLCB, MTI_PRODUCER_IDENDIFY, SourceAlias, 0, 8, $06, $01, $00, $00, Hi(Address), Lo(Address), $00, $01);
  TransportLayerThread.Add(MessageHelper.Encode);
end;

procedure TOlcbTaskBase.SendTractionQuerySpeeds;
begin
  MessageHelper.Load(ol_OpenLCB, MTI_TRACTION_PROTOCOL, SourceAlias, DestinationAlias, 3, $00, $00, TRACTION_QUERY_SPEED, $00, $00, $00, $00, $00);
  TransportLayerThread.Add(MessageHelper.Encode);
end;

procedure TOlcbTaskBase.SendTractionQueryProxyMessage;
begin
  MessageHelper.Load(ol_OpenLCB, MTI_TRACTION_PROTOCOL, SourceAlias, DestinationAlias, 4, $00, $00, TRACTION_MANAGE_PROXY, TRACTION_MANAGE_PROXY_QUERY, $00, $00, $00, $00);
  TransportLayerThread.Add(MessageHelper.Encode);
end;

procedure TOlcbTaskBase.SendTractionReleaseProxyMessage;
begin
  MessageHelper.Load(ol_OpenLCB, MTI_TRACTION_PROTOCOL, SourceAlias, DestinationAlias, 4, $00, $00, TRACTION_MANAGE_PROXY, TRACTION_MANAGE_PROXY_RELEASE, $00, $00, $00, $00);
  TransportLayerThread.Add(MessageHelper.Encode);
end;

procedure TOlcbTaskBase.SendTractionReserveProxyMessage;
begin
  MessageHelper.Load(ol_OpenLCB, MTI_TRACTION_PROTOCOL, SourceAlias, DestinationAlias, 4, $00, $00, TRACTION_MANAGE_PROXY, TRACTION_MANAGE_PROXY_RESERVE, $00, $00, $00, $00);
  TransportLayerThread.Add(MessageHelper.Encode);
end;

procedure TOlcbTaskBase.SendTractionSpeedMessage(Speed: THalfFloat);
begin
  MessageHelper.Load(ol_OpenLCB, MTI_TRACTION_PROTOCOL, SourceAlias, DestinationAlias, 5, $00, $00, TRACTION_SPEED_DIR, Hi(Speed), Lo(Speed), $00, $00, $00);
  TransportLayerThread.Add(MessageHelper.Encode);
end;

procedure TOlcbTaskBase.SendVerifyNodeIDGlobalMessage;
begin
  MessageHelper.Load(ol_OpenLCB, MTI_VERIFY_NODE_ID_NUMBER_DEST, SourceAlias, DestinationAlias, 2, 0, 0, 0, 0 ,0 ,0 ,0 ,0);
  TransportLayerThread.Add(MessageHelper.Encode);
end;

procedure TOlcbTaskBase.SendVerifyNodeIDToDestinationMessage;
begin
  MessageHelper.Load(ol_OpenLCB, MTI_VERIFY_NODE_ID_NUMBER, SourceAlias, DestinationAlias, 0, 0, 0, 0, 0 ,0 ,0 ,0 ,0);
  TransportLayerThread.Add(MessageHelper.Encode);
end;

procedure TOlcbTaskBase.SyncOnBeforeTaskDestroy;
begin
  if Assigned(OnBeforeDestroy) then
    OnBeforeDestroy(Self)
end;

constructor TOlcbTaskBase.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean);
begin
  inherited Create;
  Inc(TaskObjects);
  FStartAsSending := DoesStartAsSending;
  FSending := FStartAsSending;
  FDestinationAlias := ADestinationAlias;
  FSourceAlias := ASourceAlias;
  FMessageHelper := TOpenLCBMessageHelper.Create;
  FTransportLayerThread := nil;
  FErrorCode := 0;
  FSending := StartAsSending;
  FTag := 0;
  FRemoveKey := 0;
  FForceTermination := False;
  FHasStarted := False;
  FErrorString := '';
  FLogStrings := TStringList.Create;
  FLog := False;
  FiLogState := 0;
end;

destructor TOlcbTaskBase.Destroy;
begin
  Dec(TaskObjects);
  FreeAndNil(FMessageHelper);
  FreeAndNil(FLogStrings);
  inherited Destroy;
end;

procedure TOlcbTaskBase.CopyTo(Target: TOlcbTaskBase);
begin
  // don't copy the State Machine Index!!!
  Target.FErrorCode := FErrorCode;
  Target.FOnBeforeDestroy := FOnBeforeDestroy;
  Target.FSending := FSending;
  Target.FErrorString := FErrorString;
  Target.FRemoveKey := FRemoveKey;
  Target.FHasStarted := FHasStarted;
  Target.FTag := FTag;
  Target.FForceTermination := FForceTermination;
  Target.FTransportLayerThread := FTransportLayerThread;
  Target.FDestinationAlias := FDestinationAlias;
  Target.FDone := FDone;
  Target.FSourceAlias := FSourceAlias;
  Target.FStartAsSending := FStartAsSending;
  Target.Sending := FStartAsSending;
end;

{ TReadAddressSpaceMemoryRawTask }

function TReadAddressSpaceMemoryRawTask.GetPayloadSize: Integer;
begin
  if ReadByteCount - Stream.Size < MAX_CONFIG_MEM_READWRITE_SIZE then
    Result := ReadByteCount - Stream.Size
  else
    Result := MAX_CONFIG_MEM_READWRITE_SIZE;
end;

constructor TReadAddressSpaceMemoryRawTask.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddressSpace: Byte; AReadAddress, AReadByteCount: DWord; UseTerminatorChar: Boolean);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending);
  FStream := TMemoryStream.Create;
  FAddressSpace := AnAddressSpace;
  FForceOptionalSpaceByte := False;
  FReadAddress := AReadAddress;
  FReadByteCount := AReadByteCount;
  FCurrentOffset := ReadAddress;
  FUsingTerminator := UseTerminatorChar;
  FIncludeTerminator := True;
  FTerminator := #0;
end;

destructor TReadAddressSpaceMemoryRawTask.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

function TReadAddressSpaceMemoryRawTask.Clone: TOlcbTaskBase;
begin
  Result := TReadAddressSpaceMemoryRawTask.Create(SourceAlias, DestinationAlias, Sending, AddressSpace, ReadAddress, ReadByteCount, UsingTerminator);
end;

procedure TReadAddressSpaceMemoryRawTask.CopyTo(Target: TOlcbTaskBase);
begin
  inherited CopyTo(Target);
  (Target as TReadAddressSpaceMemoryRawTask).FAddressSpace := FAddressSpace;
  (Target as TReadAddressSpaceMemoryRawTask).FCurrentOffset := FCurrentOffset;
  (Target as TReadAddressSpaceMemoryRawTask).FForceOptionalSpaceByte := FForceOptionalSpaceByte;
  (Target as TReadAddressSpaceMemoryRawTask).FIncludeTerminator := FIncludeTerminator;
  (Target as TReadAddressSpaceMemoryRawTask).FReadByteCount := FReadByteCount;
  (Target as TReadAddressSpaceMemoryRawTask).FStream.CopyFrom(FStream, FStream.Size);
  (Target as TReadAddressSpaceMemoryRawTask).FReadAddress := FReadAddress;
  (Target as TReadAddressSpaceMemoryRawTask).FTerminator := FTerminator;
  (Target as TReadAddressSpaceMemoryRawTask).UsingTerminator := FUsingTerminator;
end;

procedure TReadAddressSpaceMemoryRawTask.Process(MessageInfo: TOlcbMessage);
var
  DatagramReceive: TDatagramReceive;
  i: Integer;
  Finished: Boolean;
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
        // Ask for a read from the node
        LogMsg('SendMemoryConfigurationRead');
         SendMemoryConfigurationRead(AddressSpace, CurrentOffset, PayloadSize, ForceOptionalSpaceByte);
         Sending := False;
         Inc(FiState);
       end;
    1: begin
         // Node received the datagram
         LogMsg('Waiting for SendMemoryConfigurationRead Datagram ACK Reply');
         if IsDatagramAckFromDestination(MessageInfo) then
         begin
           Sending := False;
           Inc(FiState);
         end;
       end;
    2: begin
          // Node sending frame of data
          LogMsg('Waiting for SendMemoryConfigurationRead Datagram Reply');
          DatagramReceive := nil;
          if IsConfigMemoryReadReplyFromDestination(MessageInfo, DatagramReceive) then
          begin
            if DatagramReceive.RawDatagram[1] and MCP_READ_ERROR = MCP_READ_ERROR then
            begin
              ExtractErrorInformation(DatagramReceive);
              Sending := True;
              iState := STATE_DONE;
            end else
            begin
              Finished := False;
              if DatagramReceive.RawDatagram[1] and $03 = 0 then    // If using the {Space} byte need to skip over it
                i := 7
              else
                i := 6;
              while not Finished and (i < DatagramReceive.CurrentPos) do
              begin
                if UsingTerminator then
                begin
                  if DatagramReceive.RawDatagram[i] = Ord( Terminator) then
                  begin
                    if IncludeTerminator then
                      Stream.WriteByte( DatagramReceive.RawDatagram[i]);
                    Finished := True;
                  end else
                    Stream.WriteByte( DatagramReceive.RawDatagram[i]);
                end else
                  Stream.WriteByte( DatagramReceive.RawDatagram[i]);
                Inc(i);
              end;

              if Finished or (PayloadSize = 0) then
              begin
                Sending := True;
                iState := STATE_DONE;
              end else
              begin
                CurrentOffset := CurrentOffset + MAX_CONFIG_MEM_READWRITE_SIZE;
                Sending := True;
                iState := 0;
              end;
            end
          end
        end;
    STATE_DONE : begin
       // Done
         LogMsg('Done');
         FDone := True
       end;
  end;

end;

{ TWriteAddressSpaceMemoryRawTask }

constructor TWriteAddressSpaceMemoryRawTask.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddressSpace: Byte; AWriteAddress: DWord; AStream: TStream);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending);
  FStream := TMemoryStream.Create;
  if Assigned(AStream) then
  begin
    AStream.Position := 0;
    Stream.CopyFrom(AStream, AStream.Size);
    Stream.Position := 0;
  end;
  FAddressSpace := AnAddressSpace;
  FForceOptionalSpaceByte := False;
  FWriteAddress := AWriteAddress;
end;

destructor TWriteAddressSpaceMemoryRawTask.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

function TWriteAddressSpaceMemoryRawTask.Clone: TOlcbTaskBase;
begin
  Result := TWriteAddressSpaceMemoryRawTask.Create(SourceAlias, DestinationAlias, Sending, AddressSpace, WriteAddress, Stream);
end;

procedure TWriteAddressSpaceMemoryRawTask.CopyTo(Target: TOlcbTaskBase);
begin
  inherited CopyTo(Target);
  (Target as TWriteAddressSpaceMemoryRawTask).FAddressSpace := FAddressSpace;
  (Target as TWriteAddressSpaceMemoryRawTask).ForceOptionalSpaceByte := FForceOptionalSpaceByte;
  (Target as TWriteAddressSpaceMemoryRawTask).FStream.CopyFrom(FStream, FStream.Size);
  (Target as TWriteAddressSpaceMemoryRawTask).FWriteAddress := FWriteAddress;
end;

procedure TWriteAddressSpaceMemoryRawTask.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  case iState of
    0 : begin
          LogMsg('SendMemoryConfigurationWrite');
          SendMemoryConfigurationWrite(AddressSpace, WriteAddress, $FFFFFFFF, ForceOptionalSpaceByte, Stream);
          Sending := False;
          Inc(FiState);
        end;
    1 : begin
          // Node received the request datagram
           LogMsg('Waiting for SendMemoryConfigurationWrite Datagram ACK Reply');
           if IsDatagramAckFromDestination(MessageInfo) then
           begin
             Sending := True;
             FiState := STATE_DONE;
           end;
        end;
    STATE_DONE :
       begin
       // Done
         LogMsg('Done');
         FDone := True
       end;
  end;
end;

{ TWriteAddressSpaceMemoryTask }

constructor TWriteAddressSpaceMemoryTask.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddressSpace: Byte; AStream: TStream);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending, AnAddressSpace, False);
  DataStream.CopyFrom(AStream, AStream.Size);
  FWritingToAddress := True;
end;

function TWriteAddressSpaceMemoryTask.Clone: TOlcbTaskBase;
begin
  Result := TWriteAddressSpaceMemoryTask.Create(SourceAlias, DestinationAlias, Sending, AddressSpace, DataStream);
end;

{ TIdentifyConsumerTask }

constructor TIdentifyConsumerTask.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnEvent: TEventID);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending);
  FEvent := AnEvent;
end;

function TIdentifyConsumerTask.Clone: TOlcbTaskBase;
begin
  Result := TIdentifyConsumerTask.Create(SourceAlias, DestinationAlias, Sending, Event);
end;

procedure TIdentifyConsumerTask.CopyTo(Target: TOlcbTaskBase);
begin
  inherited CopyTo(Target);
  (Target as TIdentifyConsumerTask).FEvent := FEvent;
end;

procedure TIdentifyConsumerTask.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         LogMsg('SendIdentifyConsumerMessage');
         SendIdentifyConsumerMessage(Event);
         iState := STATE_DONE;
       end;
    STATE_DONE:
       begin
         LogMsg('Done');
         FDone := True;
       end;
  end;
end;

{ TIdentifyProducerTask }

constructor TIdentifyProducerTask.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnEvent: TEventID);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending);
  FEvent := AnEvent;
end;

function TIdentifyProducerTask.Clone: TOlcbTaskBase;
begin
  Result := TIdentifyProducerTask.Create(SourceAlias, DestinationAlias, Sending, Event);
end;

procedure TIdentifyProducerTask.CopyTo(Target: TOlcbTaskBase);
begin
  inherited CopyTo(Target);
  (Target as TIdentifyProducerTask).FEvent := FEvent;
end;

procedure TIdentifyProducerTask.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
   case iState of
    0: begin
         LogMsg('SendIdentifyProducerMessage');
         SendIdentifyProducerMessage(Event);
         iState := STATE_DONE;
       end;
    STATE_DONE:
       begin
         LogMsg('Done');
         FDone := True;
       end;
  end;
end;

{ TTractionReserveAndAttachDccProxyTask }

constructor TTractionReserveAndAttachDccProxyTask.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddress: Word; IsShortAddress: Boolean; ASpeedStep: Byte);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending);
  FSpeedStep := ASpeedStep;
  FAddress := AnAddress;
  FIsShort := IsShortAddress;
  FReplyCode := -1;
  FReplyAddress := 0;
  FReplySpeedSteps := 0;
end;

function TTractionReserveAndAttachDccProxyTask.Clone: TOlcbTaskBase;
begin
  Result := TTractionReserveAndAttachDccProxyTask.Create(SourceAlias, DestinationAlias, Sending, Address, IsShort, SpeedStep);
end;

procedure TTractionReserveAndAttachDccProxyTask.CopyTo(Target: TOlcbTaskBase);
begin
  inherited CopyTo(Target);
  (Target as TTractionReserveAndAttachDccProxyTask).FAddress := FAddress;
  (Target as TTractionReserveAndAttachDccProxyTask).FReplyAddress := FReplyAddress;
  (Target as TTractionReserveAndAttachDccProxyTask).FReplyCode := FReplyCode;
  (Target as TTractionReserveAndAttachDccProxyTask).FReplySpeedSteps := FReplySpeedSteps;
  (Target as TTractionReserveAndAttachDccProxyTask).FIsShort := FIsShort;
  (Target as TTractionReserveAndAttachDccProxyTask).FSpeedStep := FSpeedStep;
end;

procedure TTractionReserveAndAttachDccProxyTask.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         LogMsg('SendTractionReserveProxyMessage');
         FReplyCode := TRACTION_MANAGE_RESERVE_REPLY_OK;
         SendTractionReserveProxyMessage;
         Sending := False;
         iState := 1;
       end;
    1: begin
         LogMsg('Waiting for SendTractionReserveProxyMessage Reply');
         if IsTractionReserveProxyReply(MessageInfo) then
         begin
           Sending := True;
           if TOpenLCBMessageHelper(MessageInfo).Data[4] = TRACTION_MANAGE_RESERVE_REPLY_OK then
           begin
             Sending := True;
             iState := 2;
           end else
           begin
             FReplyCode := TOpenLCBMessageHelper(MessageInfo).Data[4];
             iState := STATE_DONE
           end
         end
       end;
    2: begin
         LogMsg('SendTractionAttachDccProxyMessage');
         SendTractionAttachDccProxyMessage(Address, IsShort, SpeedStep);
         Sending := False;
         iState := 3;
       end;
    3: begin
         LogMsg('Waiting for SendTractionAttachDccProxyMessage Reply');
         if IsTractionAttachDCCAddressReply(MessageInfo) then
         begin
           if TOpenLCBMessageHelper( MessageInfo).DataCount = 8 then
             FReplyCode := TOpenLCBMessageHelper( MessageInfo).Data[7];
           FReplySpeedSteps := TOpenLCBMessageHelper( MessageInfo).Data[6];
           FReplyAddress := (TOpenLCBMessageHelper( MessageInfo).Data[4] shl 8) or TOpenLCBMessageHelper( MessageInfo).Data[5];
           Sending := True;
           iState := 4;
         end;
       end;
    4: begin
         LogMsg('SendTractionReleaseProxyMessage');
         SendTractionReleaseProxyMessage;
         iState := STATE_DONE;
       end;
    STATE_DONE: begin
         FDone := True;
       end;
  end;
end;

{ TTractionReserveAndDetachDccProxyTask }

constructor TTractionReserveAndDetachDccProxyTask.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddress: Word; IsShortAddress: Boolean);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending);
  FAddress := AnAddress;
  FIsShort := IsShortAddress;
  FReplyCode := -1;
  FReplyAddress := 0;
  FReplySpeedSteps := 0;
end;

function TTractionReserveAndDetachDccProxyTask.Clone: TOlcbTaskBase;
begin
  Result := TTractionReserveAndDetachDccProxyTask.Create(SourceAlias, DestinationAlias, Sending, Address, IsShort);
end;

procedure TTractionReserveAndDetachDccProxyTask.CopyTo(Target: TOlcbTaskBase);
begin
  inherited CopyTo(Target);
  (Target as TTractionReserveAndDetachDccProxyTask).FAddress := FAddress;
  (Target as TTractionReserveAndDetachDccProxyTask).FIsShort := FIsShort;
  (Target as TTractionReserveAndDetachDccProxyTask).FReplyAddress := FReplyAddress;
  (Target as TTractionReserveAndDetachDccProxyTask).FReplyCode := FReplyCode;
  (Target as TTractionReserveAndDetachDccProxyTask).FReplySpeedSteps := FReplySpeedSteps
end;

procedure TTractionReserveAndDetachDccProxyTask.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         LogMsg('SendTractionReserveProxyMessage');
         FReplyCode := TRACTION_MANAGE_RESERVE_REPLY_OK;
         SendTractionReserveProxyMessage;
         Sending := False;
         iState := 1;
       end;
    1: begin
         LogMsg('Waiting for SendTractionReserveProxyMessage Reply');
         if IsTractionReserveProxyReply(MessageInfo) then
         begin
           Sending := True;
           if TOpenLCBMessageHelper(MessageInfo).Data[4] = TRACTION_MANAGE_RESERVE_REPLY_OK then
           begin
             Sending := True;
             iState := 2;
           end else
           begin
             FReplyCode := TOpenLCBMessageHelper(MessageInfo).Data[4];
             iState := STATE_DONE
           end
         end
       end;
    2: begin
         LogMsg('SendTractionDetachDccAddressProxyMessage');
         SendTractionDetachDccAddressProxyMessage(Address, IsShort);
         Sending := False;
         iState := 3;
       end;
    3: begin
         LogMsg('Waiting for SendTractionDetachDccAddressProxyMessage Reply');
         if IsTractionDetachDCCAddressReply(MessageInfo) then
         begin
           if TOpenLCBMessageHelper( MessageInfo).DataCount = 7 then
             FReplyCode := TOpenLCBMessageHelper( MessageInfo).Data[6];
           FReplyAddress := (TOpenLCBMessageHelper( MessageInfo).Data[4] shl 8) or TOpenLCBMessageHelper( MessageInfo).Data[5];
           Sending := True;
           iState := 4;
         end;
       end;
    4: begin
         LogMsg('SendTractionReleaseProxyMessage');
         SendTractionReleaseProxyMessage;
         iState := STATE_DONE;
       end;
    STATE_DONE:
       begin
         LogMsg('Done');
         FDone := True;
       end;
  end;
end;

{ TTractionQueryDccAddressProxyTask }

constructor TTractionQueryDccAddressProxyTask.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddress: Word; IsShortAddress: Boolean);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending);
  FAddress := AnAddress;
  FIsShort := IsShortAddress;
end;

function TTractionQueryDccAddressProxyTask.Clone: TOlcbTaskBase;
begin
  Result := TTractionQueryDccAddressProxyTask.Create(SourceAlias, DestinationAlias, Sending, Address, IsShort);
end;

procedure TTractionQueryDccAddressProxyTask.CopyTo(Target: TOlcbTaskBase);
begin
  inherited CopyTo(Target);
  (Target as TTractionQueryDccAddressProxyTask).FAddress := FAddress;
  (Target as TTractionQueryDccAddressProxyTask).FIsShort := FIsShort;
end;

procedure TTractionQueryDccAddressProxyTask.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         LogMsg('SendTractionQueryDccAddressProxyMessage');
         SendTractionQueryDccAddressProxyMessage(Address, IsShort);
         iState := STATE_DONE;
       end;
    STATE_DONE:
       begin
         LogMsg('Done');
         FDone := True;
       end;
  end;
end;

{ TTractionFunctionTask }

constructor TTractionFunctionTask.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddress: DWord; AValue: Word);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending);
  Address := AnAddress;
  Value := AValue;
end;

function TTractionFunctionTask.Clone: TOlcbTaskBase;
begin
  Result := TTractionFunctionTask.Create(SourceAlias, DestinationAlias, Sending, Address, Value);
end;

procedure TTractionFunctionTask.CopyTo(Target: TOlcbTaskBase);
begin
  inherited CopyTo(Target);
  (Target as TTractionFunctionTask).FAddress := FAddress;
  (Target as TTractionFunctionTask).FWord := FWord;
end;

procedure TTractionFunctionTask.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         LogMsg('SendTractionFunction');
         SendTractionFunction(Address, Value);
         iState := STATE_DONE;
       end;
    STATE_DONE:
       begin
         LogMsg('Done');
         FDone := True;
       end;
  end;
end;

{ TTractionSpeedTask }

constructor TTractionSpeedTask.Create(ASourceAlias, ADestinationAlias: Word;  DoesStartAsSending: Boolean; ASpeed: THalfFloat; IsEStop: Boolean);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending);
  FSpeed := ASpeed;
  FEStop := IsEStop;
end;

function TTractionSpeedTask.Clone: TOlcbTaskBase;
begin
  Result := TTractionSpeedTask.Create(SourceAlias, DestinationAlias, Sending, Speed, EStop);
end;

procedure TTractionSpeedTask.CopyTo(Target: TOlcbTaskBase);
begin
  inherited CopyTo(Target);
  (Target as TTractionSpeedTask).EStop := EStop;
  (Target as TTractionSpeedTask).FSpeed := FSpeed;
end;

procedure TTractionSpeedTask.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         if EStop then
           SendTractionEStopMessage
         else begin
           SendTractionSpeedMessage(Speed)
         end;
         iState := STATE_DONE;
       end;
    STATE_DONE: begin
         FDone := True;
       end;
  end;
end;

{ TTractionQuerySpeedTask }

constructor TTractionQuerySpeedTask.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending);
  FActualSpeed := 0;
  FCommandedSpeed := 0;
  FSetSpeed := 0;
  FStatus := 0;
end;

function TTractionQuerySpeedTask.Clone: TOlcbTaskBase;
begin
  Result := TTractionQuerySpeedTask.Create(SourceAlias, DestinationAlias, Sending);
end;

procedure TTractionQuerySpeedTask.CopyTo(Target: TOlcbTaskBase);
begin
  inherited CopyTo(Target);
  (Target as TTractionQuerySpeedTask).FActualSpeed := FActualSpeed;
  (Target as TTractionQuerySpeedTask).FCommandedSpeed := FCommandedSpeed;
  (Target as TTractionQuerySpeedTask).FSetSpeed := FSetSpeed;
  (Target as TTractionQuerySpeedTask).FStatus := FStatus;
end;

procedure TTractionQuerySpeedTask.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         LogMsg('SendTractionQuerySpeeds');
         SendTractionQuerySpeeds;
         Sending := False;
         iState := 1;
       end;
    1: begin
         LogMsg('Waiting for SendTractionQuerySpeeds first frame Reply');
         if IsTractionSpeedsQueryFirstFrameReply(MessageInfo) then
         begin
           FSetSpeed := TOpenLCBMessageHelper( MessageInfo).ExtractDataBytesAsInt(3, 4);
           FCommandedSpeed := TOpenLCBMessageHelper( MessageInfo).ExtractDataBytesAsInt(6, 7);
           FStatus := TOpenLCBMessageHelper( MessageInfo).Data[5];
           iState := 2;
         end;
       end;
    2: begin
         LogMsg('Waiting for SendTractionQuerySpeeds second frame Reply');
         if IsTractionSpeedsQuerySecondFrameReply(MessageInfo) then
         begin
           FActualSpeed := TOpenLCBMessageHelper( MessageInfo).ExtractDataBytesAsInt(2, 3);
           Sending := True;
           iState := STATE_DONE;
         end;
       end;
    STATE_DONE:
       begin
         LogMsg('Done');
         FDone := True;
       end;
  end;
end;

{ TTractionQueryFunctionTask }

constructor TTractionQueryFunctionTask.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddress: DWord);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending);
  FAddress := AnAddress;
  FValue := -1;
end;

function TTractionQueryFunctionTask.Clone: TOlcbTaskBase;
begin
  Result := TTractionQueryFunctionTask.Create(SourceAlias, DestinationAlias, Sending, Address);
end;

procedure TTractionQueryFunctionTask.CopyTo(Target: TOlcbTaskBase);
begin
  inherited CopyTo(Target);
  (Target as TTractionQueryFunctionTask).FAddress := FAddress;
  (Target as TTractionQueryFunctionTask).FValue := FValue;
end;

procedure TTractionQueryFunctionTask.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         LogMsg('SendTractionQueryFunction');
         SendTractionQueryFunction(Address);
         Sending := False;
         iState := 1;
       end;
    1: begin
         LogMsg('Waiting for SendTractionQueryFunction Reply');
         if IsTractionFunctionQueryReply(MessageInfo) then
         begin
           FValue := TOpenLCBMessageHelper( MessageInfo).ExtractDataBytesAsInt(6, 7);
           Sending := True;
           iState := STATE_DONE
         end;
       end;
    STATE_DONE:
       begin
         LogMsg('Done');
         FDone := True;
       end;
  end;
end;


{ TIdentifyEventsAddressedTask }

function TIdentifyEventsAddressedTask.Clone: TOlcbTaskBase;
begin
  Result := TIdentifyEventsAddressedTask.Create(SourceAlias, DestinationAlias, Sending);
end;

procedure TIdentifyEventsAddressedTask.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         LogMsg('SendIdentifyEventsAddressedMessage');
         SendIdentifyEventsAddressedMessage;
         iState := STATE_DONE;
       end;
    STATE_DONE:
      begin
         LogMsg('Done');
         FDone := True;
       end;
  end;
end;

{ TIdentifyEventsTask }

function TIdentifyEventsTask.Clone: TOlcbTaskBase;
begin
  Result := TIdentifyEventsTask.Create(SourceAlias, DestinationAlias, Sending);
end;

procedure TIdentifyEventsTask.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         LogMsg('SendIdentifyEventsMessage');
         SendIdentifyEventsMessage;
         iState := STATE_DONE;
       end;
    STATE_DONE:
       begin
         LogMsg('Done');
         FDone := True;
       end;
  end;
end;

{ TVerifiedNodeIDTask }

function TVerifiedNodeIDTask.Clone: TOlcbTaskBase;
begin
  Result := TVerifiedNodeIDTask.Create(SourceAlias, DestinationAlias, Sending);
end;

procedure TVerifiedNodeIDTask.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  FDone := True;  // Done before we start....
end;

{ TTractionProtocolTask }

function TTractionProtocolTask.Clone: TOlcbTaskBase;
begin
  Result := TTractionProtocolTask.Create(SourceAlias, DestinationAlias, Sending);
end;

procedure TTractionProtocolTask.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  FDone := True;  // Done before we start....
end;

{ TInitializationCompleteTask }

function TInitializationCompleteTask.Clone: TOlcbTaskBase;
begin
  Result := TInitializationCompleteTask.Create(SourceAlias, DestinationAlias, Sending);
end;

procedure TInitializationCompleteTask.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  FDone := True;  // Done before we start....
end;

{ TEventTask }

function TEventTask.Clone: TOlcbTaskBase;
begin
  Result := TEventTask.Create(SourceAlias, DestinationAlias, Sending);
end;

procedure TEventTask.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  FDone := True;  // Done before we start....
end;

{ TCANLayerTask }

function TCANLayerTask.Clone: TOlcbTaskBase;
begin
  Result := TCANLayerTask.Create(SourceAlias, DestinationAlias, Sending);
end;

procedure TCANLayerTask.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  FDone := True;  // Done before we start....
end;

{ TSimpleNodeInformationTask }

procedure TSimpleNodeInformationTask.LogSnipMsg(Msg: string);
begin
  if iSnipState <> iLogStateSnip then
  begin
    iLogStateSnip := iSnipState;
    LogMsg(Msg)
  end;
end;

constructor TSimpleNodeInformationTask.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending);
  FSnip := TOlcbSNIP.Create;
  FiSnipState := 0;
  iSnipState := 0;
end;

destructor TSimpleNodeInformationTask.Destroy;
begin
  FreeAndNil(FSnip);
  inherited Destroy;
end;

function TSimpleNodeInformationTask.Clone: TOlcbTaskBase;
begin
  Result := TSimpleNodeInformationTask.Create(SourceAlias, DestinationAlias, Sending);
end;

procedure TSimpleNodeInformationTask.CopyTo(Target: TOlcbTaskBase);
begin
  inherited CopyTo(Target);
  Snip.CopyTo( (Target as TSimpleNodeInformationTask).FSnip);
end;

procedure TSimpleNodeInformationTask.Process(MessageInfo: TOlcbMessage);
const
  STATE_SNII_MFG_VERSION  = 0;
  STATE_SNII_MFG_NAME     = 1;
  STATE_SNII_MFG_MODEL    = 2;
  STATE_SNII_HARDWARE_VER = 3;
  STATE_SNII_SOFTWARE_VER = 4;
  STATE_SNII_USER_VERSION = 5;
  STATE_SNII_USER_NAME    = 6;
  STATE_SNII_USER_DESC    = 7;
var
  i: Integer;
  LocalMessageHelper: TOpenLCBMessageHelper;
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         LogMsg('SendSnipMessage');
         SendSnipMessage;
         iSnipState := 0;
         Sending := False;
         Inc(FiState);
       end;
    1: begin
         LogMsg('Waiting for SendSnipMessage Reply');
         if IsSnipMessageReply(MessageInfo) then
         begin
           LocalMessageHelper := TOpenLCBMessageHelper( MessageInfo);  // Already know this is true
           i := 2;                                               // Strip off the destination Alias
           while i < LocalMessageHelper.DataCount do
           begin
             case iSnipState of
               STATE_SNII_MFG_VERSION :
                 begin
                   LogSnipMsg('STATE_SNII_MFG_VERSION');
                   Snip.SniiMfgVersion := LocalMessageHelper.Data[i];
                   Inc(i);
                   iSnipState := STATE_SNII_MFG_NAME;
                 end;
               STATE_SNII_MFG_NAME     :
                 begin
                   LogSnipMsg('STATE_SNII_MFG_NAME');
                   if Chr( LocalMessageHelper.Data[i]) <> #0 then
                   begin
                     Snip.SniiMfgName := Snip.SniiMfgName + Chr( LocalMessageHelper.Data[i]);
                     Inc(i);
                   end else
                   begin
                     Inc(i);
                     iSnipState := STATE_SNII_MFG_MODEL;
                   end;
                 end;
               STATE_SNII_MFG_MODEL     :
                 begin
                   LogSnipMsg('STATE_SNII_MFG_MODEL');
                   if Chr( LocalMessageHelper.Data[i]) <> #0 then
                   begin
                     Snip.SniiMfgModel := Snip.SniiMfgModel + Chr( LocalMessageHelper.Data[i]);
                     Inc(i);
                   end else
                   begin
                     Inc(i);
                     iSnipState := STATE_SNII_HARDWARE_VER;
                   end;
                 end;
               STATE_SNII_HARDWARE_VER  :
                 begin
                   LogSnipMsg('STATE_SNII_HARDWARE_VER');
                   if Chr( LocalMessageHelper.Data[i]) <> #0 then
                   begin
                     Snip.SniiHardwareVersion := Snip.SniiHardwareVersion + Chr( LocalMessageHelper.Data[i]);
                     Inc(i);
                   end else
                   begin
                     Inc(i);
                     iSnipState := STATE_SNII_SOFTWARE_VER;
                   end;
                 end;
               STATE_SNII_SOFTWARE_VER  :
                 begin
                   LogSnipMsg('STATE_SNII_SOFTWARE_VER');
                   if Chr( LocalMessageHelper.Data[i]) <> #0 then
                   begin
                     Snip.SniiSoftwareVersion := Snip.SniiSoftwareVersion + Chr( LocalMessageHelper.Data[i]);
                     Inc(i);
                   end else
                   begin
                     Inc(i);
                     iSnipState := STATE_SNII_USER_VERSION;
                   end;
                 end;
               STATE_SNII_USER_VERSION  :
                 begin
                   LogSnipMsg('STATE_SNII_USER_VERSION');
                   Snip.SniiUserVersion := LocalMessageHelper.Data[i];
                   Inc(i);
                   iSnipState := STATE_SNII_USER_NAME;
                 end;
               STATE_SNII_USER_NAME     :
                 begin
                   LogSnipMsg('STATE_SNII_USER_NAME');
                   if Chr( LocalMessageHelper.Data[i]) <> #0 then
                   begin
                     Snip.SniiUserName := Snip.SniiUserName + Chr( LocalMessageHelper.Data[i]);
                     Inc(i);
                   end else
                   begin
                     Inc(i);
                     iSnipState := STATE_SNII_USER_DESC;
                   end;
                 end;
               STATE_SNII_USER_DESC     :
                 begin
                   LogSnipMsg('STATE_SNII_USER_DESC');
                   if Chr( LocalMessageHelper.Data[i]) <> #0 then
                   begin
                     Snip.SniiUserDescription := Snip.SniiUserDescription + Chr( LocalMessageHelper.Data[i]);
                     Inc(i);
                   end else
                   begin
                     Inc(i);
                     Sending := True;
                     iState := STATE_DONE;
                   end;
                 end;
             end;
           end
         end;
       end;
    STATE_DONE:
       begin
         LogMsg('Done');
         FDone := True;
       end;
  end;
end;

{ TProtocolSupportTask }

function TProtocolSupportTask.Clone: TOlcbTaskBase;
begin
  Result := TProtocolSupportTask.Create(SourceAlias, DestinationAlias, Sending);
end;

procedure TProtocolSupportTask.CopyTo(Target: TOlcbTaskBase);
begin
  inherited CopyTo(Target);
  (Target as TProtocolSupportTask).FProtocols := FProtocols;
end;

procedure TProtocolSupportTask.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         LogMsg('SendProtocolIdentificationProtocolMessage');
         SendProtocolIdentificationProtocolMessage;
         FProtocols := 0;
         Sending := False;
         Inc(FiState);
       end;
    1: begin
         LogMsg('Waiting for SendProtocolIdentificationProtocolMessage Reply');
         if IsProtocolIdentificationProcolReplyFromDestination(MessageInfo) then
         begin
           FProtocols := TOpenLCBMessageHelper( MessageInfo).ExtractDataBytesAsInt(2, 7);
           Sending := True;
           iState := STATE_DONE;
         end;
       end;
    STATE_DONE: begin
         LogMsg('Done');
         FDone := True;
       end;
  end;
end;

{ TConfigMemoryAddressSpaceInfoTask }

constructor TConfigMemoryAddressSpaceInfoTask.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddressSpace: Byte);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending);
  FAddressSpace := AnAddressSpace;
  ConfigMemoryAddressSpace := TOlcbMemAddressSpace.Create;
end;

destructor TConfigMemoryAddressSpaceInfoTask.Destroy;
begin
  FreeAndNil(FConfigMemoryAddressSpace);
  inherited Destroy;
end;

function TConfigMemoryAddressSpaceInfoTask.Clone: TOlcbTaskBase;
begin
  Result := TConfigMemoryAddressSpaceInfoTask.Create(SourceAlias, DestinationAlias, Sending, AddressSpace);
end;

procedure TConfigMemoryAddressSpaceInfoTask.CopyTo(Target: TOlcbTaskBase);
begin
  inherited CopyTo(Target);
  (Target as TConfigMemoryAddressSpaceInfoTask).FAddressSpace := AddressSpace;
  ConfigMemoryAddressSpace.CopyTo( (Target as TConfigMemoryAddressSpaceInfoTask).ConfigMemoryAddressSpace);
end;

procedure TConfigMemoryAddressSpaceInfoTask.Process(MessageInfo: TOlcbMessage);
var
  DatagramReceive: TDatagramReceive;
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         LogMsg('SendMemoryConfigurationSpaceInfo');
         SendMemoryConfigurationSpaceInfo(AddressSpace);
         Sending := False;
         Inc(FiState);
       end;
    1: begin
         LogMsg('Waiting for SendMemoryConfigurationSpaceInfo Datagram ACK Reply');
         if IsDatagramAckFromDestination(MessageInfo) then
         begin
           Sending := False;
           Inc(FiState);
         end;
       end;
    2: begin
         LogMsg('Waiting for SendMemoryConfigurationSpaceInfo Datagram Reply');
         DatagramReceive := nil;
         if IsConfigMemorySpaceInfoReplyFromDestination(MessageInfo, AddressSpace, DatagramReceive) then
         begin
           ConfigMemoryAddressSpace.LoadByDatagram(DatagramReceive);
           Sending := True;
           iState := STATE_DONE;
         end;
       end;
    STATE_DONE:
       begin
         LogMsg('Done');
         FDone := True;
       end;
  end;
end;

{ TConfigMemoryOptionsTask }

constructor TConfigMemoryOptionsTask.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending);
  FConfigMemoryOptions := TOlcbMemOptions.Create;
end;

destructor TConfigMemoryOptionsTask.Destroy;
begin
  FreeAndNil(FConfigMemoryOptions);
  inherited Destroy;
end;

function TConfigMemoryOptionsTask.Clone: TOlcbTaskBase;
begin
  Result := TConfigMemoryOptionsTask.Create(SourceAlias, DestinationAlias, Sending);
end;

procedure TConfigMemoryOptionsTask.CopyTo(Target: TOlcbTaskBase);
begin
  inherited CopyTo(Target);
  ConfigMemoryOptions.CopyTo( (Target as TConfigMemoryOptionsTask).ConfigMemoryOptions);
end;

procedure TConfigMemoryOptionsTask.Process(MessageInfo: TOlcbMessage);
var
  DatagramReceive: TDatagramReceive;
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         LogMsg('SendMemoryConfigurationOptions');
         SendMemoryConfigurationOptions;
         Sending := False;
         Inc(FiState);
       end;
    1: begin
         LogMsg('Waiting for SendMemoryConfigurationOptions Datagram ACK Reply');
         if IsDatagramAckFromDestination(MessageInfo) then
         begin
           Sending := False;
           Inc(FiState);
         end;
       end;
    2: begin
         LogMsg('Waiting for SendMemoryConfigurationOptions Datagram Reply');
         DatagramReceive := nil;
         if IsConfigMemoryOptionsReplyFromDestination(MessageInfo, DatagramReceive) then
         begin
           ConfigMemoryOptions.LoadFromDatagram(DatagramReceive);
           Sending := True;
           iState := STATE_DONE;
         end;
       end;
    STATE_DONE:
       begin
         LogMsg('Done');
         FDone := True;
       end;
  end;
end;

{ TVerifyNodeIDTask }

function TVerifyNodeIDTask.Clone: TOlcbTaskBase;
begin
  Result := TVerifyNodeIDTask.Create(SourceAlias, DestinationAlias, Sending);
end;

procedure TVerifyNodeIDTask.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         LogMsg('SendVerifyNodeIDGlobalMessage');
         SendVerifyNodeIDGlobalMessage;
         iState := STATE_DONE;
       end;
    STATE_DONE:
       begin
         LogMsg('Done');
         FDone := True;
       end;
  end;
end;

{ TVerifyNodeIDGlobalTask }

function TVerifyNodeIDGlobalTask.Clone: TOlcbTaskBase;
begin
  Result := TVerifyNodeIDGlobalTask.Create(SourceAlias, DestinationAlias, Sending);
end;

procedure TVerifyNodeIDGlobalTask.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         LogMsg('SendVerifyNodeIDToDestinationMessage');
         SendVerifyNodeIDToDestinationMessage;
         iState := STATE_DONE;
       end;
    STATE_DONE: begin
         LogMsg('Done');
         FDone := True;
       end;
  end;
end;

{ TBaseAddressSpaceMemoryTask }

function TBaseAddressSpaceMemoryTask.GetMaxPayloadSize: DWord;
begin
  Result := MAX_CONFIG_MEM_READWRITE_SIZE;
end;

constructor TBaseAddressSpaceMemoryTask.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddressSpace: Byte;
  UseTerminatorChar: Boolean);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending);
  FAddressSpace := AnAddressSpace;
  ForceOptionalSpaceByte := False;
  FWritingToAddress := False;
  FDataStream := TMemoryStream.Create;
  FUsingTerminator := UseTerminatorChar;
end;

destructor TBaseAddressSpaceMemoryTask.Destroy;
begin
  FreeAndNil(FDataStream);
  inherited Destroy;
end;

function TBaseAddressSpaceMemoryTask.Clone: TOlcbTaskBase;
begin
  Result := TBaseAddressSpaceMemoryTask.Create(SourceAlias, DestinationAlias, Sending, AddressSpace, UsingTerminator);
end;

procedure TBaseAddressSpaceMemoryTask.CopyTo(Target: TOlcbTaskBase);
begin
  inherited CopyTo(Target);
  (Target as TBaseAddressSpaceMemoryTask).FAddressSpace := FAddressSpace;
  (Target as TBaseAddressSpaceMemoryTask).FCurrentAddress := FCurrentAddress;
  (Target as TBaseAddressSpaceMemoryTask).FCurrentSendSize := FCurrentSendSize;
  (Target as TBaseAddressSpaceMemoryTask).FDataStream.CopyFrom(FDataStream, FDataStream.Size);
  (Target as TBaseAddressSpaceMemoryTask).FForceOptionalSpaceByte := FForceOptionalSpaceByte;
  (Target as TBaseAddressSpaceMemoryTask).FMaxAddress := FMaxAddress;
  (Target as TBaseAddressSpaceMemoryTask).FMinAddress := FMinAddress;
  (Target as TBaseAddressSpaceMemoryTask).FTerminator := FTerminator;
  (Target as TBaseAddressSpaceMemoryTask).FUsingTerminator := FUsingTerminator;
  (Target as TBaseAddressSpaceMemoryTask).FWritingToAddress := FWritingToAddress;
end;

procedure TBaseAddressSpaceMemoryTask.Process(MessageInfo: TOlcbMessage);
// Outline:
//    Read Protocols to see if the Memory Protocol is supported
//    Read Memory Protocol Options to read the Min/Max supported Spaces to see if $FF is supported
//    Read Memory Protocol Space Info to read size of the CDI Space
//    Read Memory Protocol at Space $FF until done
//

const
  STATE_READ_START = 8;
  STATE_WRITE_START = 20;
var
  DatagramReceive: TDatagramReceive;
  PIP: TOlcbProtocolIdentification;
  Space: TOlcbMemAddressSpace;
  Options: TOlcbMemOptions;
  DatagramResultStart: DWord;
  i: Integer;
  Terminated: Boolean;
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         // Ask for the protocols the node supports
         LogMsg('SendProtocolIdentificationProtocolMessage');
         SendProtocolIdentificationProtocolMessage;
         Inc(FiState);
         Sending := False;
       end;
    1: begin
         // First see if the node even supports the Memory Configuration Protocol
         LogMsg('Waiting for SendProtocolIdentificationProtocolMessage Datagram ACK Reply');
         if IsProtocolIdentificationProcolReplyFromDestination(MessageInfo) then
         begin
           PIP := TOlcbProtocolIdentification.Create;
           try
             PIP.LoadByMessage( TOpenLCBMessageHelper( MessageInfo));   // Already know that MessageInfo is a TOpenLCBMessageHelper by this point
             if PIP.MemoryConfigProtocol then
             begin
               Sending := True;
               Inc(FiState);
             end else
             begin
               if not PIP.MemoryConfigProtocol then
                 ErrorCode := ErrorCode or ERROR_NO_MEMORY_CONFIG_PROTOCOL;
               Sending := True;
               iState := STATE_DONE;
             end
           finally
             FreeAndNil(PIP)
           end
         end
       end;
    2: begin
         // Ask for what Address Spaces the node supports
         LogMsg('SendMemoryConfigurationOptions');
         SendMemoryConfigurationOptions;
         Inc(FiState);
         Sending := False;
       end;
    3: begin
         // Node received the request datagram
         LogMsg('Waiting for SendMemoryConfigurationOptions Datagram ACK Reply');
         if IsDatagramAckFromDestination(MessageInfo) then
         begin
           Sending := False;
           Inc(FiState);
         end;
       end;
    4: begin
         // Is the address space we are asking for supported?
         LogMsg('Waiting for SendMemoryConfigurationOptions Datagram Reply');
         DatagramReceive := nil;
         if IsConfigMemoryOptionsReplyFromDestination(MessageInfo, DatagramReceive) then
         begin
           Options := TOlcbMemOptions.Create;
           try
             Options.LoadFromDatagram(DatagramReceive);
             if (AddressSpace <= Options.AddressSpaceHi) and (AddressSpace >= Options.AddressSpaceLo) then
             begin
               Sending := True;
               Inc(FiState);
             end else
             begin
               Sending := True;
               ErrorCode := ERROR_NO_CDI_ADDRESS_SPACE;
               iState := STATE_DONE;
             end;
           finally
             FreeAndNil(Options);
           end;
         end;
       end;
    5: begin
        // Ask for details about the address space we are interested in
         LogMsg('SendMemoryConfigurationSpaceInfo');
         SendMemoryConfigurationSpaceInfo(AddressSpace);
         Sending := False;
         Inc(FiState);
       end;
    6: begin
        // Node received the datagram
         LogMsg('Waiting for SendMemoryConfigurationSpaceInfo Datagram ACK Reply');
         if IsDatagramAckFromDestination(MessageInfo) then
         begin
           Sending := False;
           Inc(FiState);
         end;
       end;
    7: begin
         LogMsg('Waiting for SendMemoryConfigurationSpaceInfo Datagram Reply');
         if IsConfigMemorySpaceInfoReplyFromDestination(MessageInfo, AddressSpace, DatagramReceive) then
         begin
           Space := TOlcbMemAddressSpace.Create;
           try
             Space.LoadByDatagram(DatagramReceive);
             if Space.IsPresent then
             begin
               if (WritingToAddress and Space.IsReadOnly) then
               begin
                 ErrorCode := ErrorCode or ERROR_ADDRESS_SPACE_NOT_PRESENT;
                 Sending := True;
                 iState := STATE_DONE;
               end else
               begin
                 FMinAddress := Space.AddressLo;
                 FMaxAddress := Space.AddressHi;
                 CurrentAddress := MinAddress;
                 DataStream.Position := 0;
                 Sending := True;
                 if WritingToAddress then
                   FiState := STATE_WRITE_START
                 else
                   FiState := STATE_READ_START;
               end
             end else
             begin
               ErrorCode := ErrorCode or ERROR_ADDRESS_SPACE_NOT_PRESENT;
               Sending := True;
               iState := STATE_DONE;
             end
           finally
             FreeAndNil(Space);
           end
         end;
       end;
    STATE_READ_START : begin
         // Calculate how many bytes to read in this frame (depends on if the address space is carried in the frame or if at the end of the mem space)
         LogMsg('STATE_READ_START');
         if MaxAddress - CurrentAddress > MaxPayloadSize then
            CurrentSendSize := MaxPayloadSize
          else
            CurrentSendSize := MaxAddress - CurrentAddress;
         Sending := True;
         Inc(FiState);
       end;
    9: begin
        // Ask for a read from the node
         LogMsg('SendMemoryConfigurationRead');
         SendMemoryConfigurationRead(AddressSpace, CurrentAddress, CurrentSendSize, ForceOptionalSpaceByte);
         Sending := False;
         Inc(FiState);
       end;
    10: begin
         // Node received the datagram
         LogMsg('Waiting for SendMemoryConfigurationRead Datagram ACK Reply');
         if IsDatagramAckFromDestination(MessageInfo) then
         begin
           Sending := False;
           Inc(FiState);
         end;
       end;
    11: begin
          // Node sending frame of data
          LogMsg('Waiting for SendMemoryConfigurationRead Datagram Reply');
          DatagramReceive := nil;
          if IsConfigMemoryReadReplyFromDestination(MessageInfo, DatagramReceive) then
          begin
            if DatagramReceive.RawDatagram[1] and MCP_READ_ERROR = MCP_READ_ERROR then
            begin
              ExtractErrorInformation(DatagramReceive);
              Sending := True;
              iState := STATE_DONE;
            end else
            begin
              if DatagramReceive.RawDatagram[1] and $03 = 0 then    // If using the {Space} byte need to skip over it
                DatagramResultStart := 7
              else
                DatagramResultStart := 6;
              Terminated := False;
              for i := DatagramResultStart to DatagramReceive.CurrentPos - 1 do
              begin
                DataStream.WriteByte( DatagramReceive.RawDatagram[i]);
                if UsingTerminator then
                begin
                  if DatagramReceive.RawDatagram[i] = Ord( Terminator) then
                    Terminated := True;
                end;
              end;
              CurrentAddress := CurrentAddress + DWord( (DatagramReceive.CurrentPos - DatagramResultStart));
              if (CurrentAddress = MaxAddress) or Terminated then
              begin
                Sending := True;
                iState := STATE_DONE;
              end else
              begin
                Sending := True;
                iState := STATE_READ_START;
              end;
            end
          end
        end;
    STATE_WRITE_START :
        begin
          LogMsg('STATE_WRITE_START');
          if DataStream.Size > Space.AddressHi - Space.AddressLo then
            ErrorCode := ERROR_ADDRESS_SPACE_WRITE_LARGER_THAN_SPACE
          else
            SendMemoryConfigurationWrite(AddressSpace, CurrentAddress, Space.AddressHi - Space.AddressLo, ForceOptionalSpaceByte, DataStream);
          iState := STATE_DONE
        end;
    STATE_DONE : begin
       // Done
         LogMsg('Done');
         FDone := True
       end;
  end;
end;

{ TEnumAllConfigMemoryAddressSpaceInfoTask }

constructor TEnumAllConfigMemoryAddressSpaceInfoTask.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending);
  FCurrentAddressSpace := 0;
  FMaxAddressSpace := 0;
  FMinAddressSpace := 0;
  FConfigMemAddressInfo := TOlcbMemConfig.Create;
end;

destructor TEnumAllConfigMemoryAddressSpaceInfoTask.Destroy;
begin
  FreeAndNil(FConfigMemAddressInfo);
  inherited Destroy;
end;

function TEnumAllConfigMemoryAddressSpaceInfoTask.Clone: TOlcbTaskBase;
begin
  Result := TEnumAllConfigMemoryAddressSpaceInfoTask.Create(SourceAlias, DestinationAlias, Sending);
end;

procedure TEnumAllConfigMemoryAddressSpaceInfoTask.CopyTo(Target: TOlcbTaskBase);
begin
  inherited CopyTo(Target);
  ConfigMemAddressInfo.CopyTo( (Target as TEnumAllConfigMemoryAddressSpaceInfoTask).ConfigMemAddressInfo);
  (Target as TEnumAllConfigMemoryAddressSpaceInfoTask).CurrentAddressSpace := CurrentAddressSpace;
  (Target as TEnumAllConfigMemoryAddressSpaceInfoTask).MaxAddressSpace := MaxAddressSpace;
  (Target as TEnumAllConfigMemoryAddressSpaceInfoTask).MinAddressSpace := MinAddressSpace;
end;

procedure TEnumAllConfigMemoryAddressSpaceInfoTask.Process(MessageInfo: TOlcbMessage);
var
  DatagramReceive: TDatagramReceive;
  NewSpace: TOlcbMemAddressSpace;
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         LogMsg('SendMemoryConfigurationOptions');
         SendMemoryConfigurationOptions;
         Inc(FiState);
         Sending := False;
       end;
    1: begin
         LogMsg('Waiting for SendMemoryConfigurationOptions Datagram ACK Reply');
         if IsDatagramAckFromDestination(MessageInfo) then
         begin
           Sending := False;
           Inc(FiState);
         end;
       end;
    2: begin
         LogMsg('Waiting for SendMemoryConfigurationOptions Datagram Reply');
         DatagramReceive := nil;
         if IsConfigMemoryOptionsReplyFromDestination(MessageInfo, DatagramReceive) then
         begin
           MinAddressSpace := DatagramReceive.RawDatagram[6];
           MaxAddressSpace := DatagramReceive.RawDatagram[5];
           CurrentAddressSpace := MaxAddressSpace;
           Sending := True;
           Inc(FiState);
         end;
       end;
    3: begin
         LogMsg('SendMemoryConfigurationSpaceInfo');
         SendMemoryConfigurationSpaceInfo(CurrentAddressSpace);
         Sending := False;
         Inc(FiState);
       end;
    4: begin
         LogMsg('Waiting for SendMemoryConfigurationSpaceInfo Datagram ACK Reply');
         if IsDatagramAckFromDestination(MessageInfo) then
         begin
           Sending := False;
           Inc(FiState);
         end;
       end;
    5: begin
         LogMsg('Waiting for SendMemoryConfigurationSpaceInfo Datagram Reply');
         DatagramReceive := nil;
         if IsConfigMemorySpaceInfoReplyFromDestination(MessageInfo, CurrentAddressSpace, DatagramReceive) then
         begin
           NewSpace := ConfigMemAddressInfo.AddAddressSpace;
           NewSpace.LoadByDatagram(DatagramReceive);
           Dec(FCurrentAddressSpace);
           if CurrentAddressSpace < MinAddressSpace then
           begin
             Sending := True;
             iState := STATE_DONE;
           end else
           begin
             Sending := True;
             iState := 3;
           end;
         end;
       end;
    STATE_DONE: begin
       // Done
         LogMsg('Done');
         FDone := True
       end;
  end;
end;

{ TOlcbTaskEngine }

constructor TOlcbTaskEngine.Create(AnOwner: TTransportLayerThread);
begin
  inherited Create;
  FOwner := AnOwner;
  FTaskList := TThreadList.Create;
  FMaxCount := 0;
end;

procedure TOlcbTaskEngine.ProcessReceiving(MessageInfo: TOlcbMessage);
var
  List: TLIst;
  Task, CompletedTask: TOlcbTaskBase;
begin
  CompletedTask := nil;
  List := TaskList.LockList;
  try
    if List.Count > 0 then
    begin
      Task := TOlcbTaskBase( List[0]);
      if not Task.Sending then
      begin
        Task.Process(MessageInfo);
        if Task.Done then
        begin
          CompletedTask := Task;
          List.Delete(0);
        end;
      end;
    end;
  finally
    TaskList.UnlockList;
  end;

  // Do this outside of the List Locking so we don't deadlock with the main thread
  if Assigned(CompletedTask) then
  begin
    Owner.Synchronize(@CompletedTask.SyncOnBeforeTaskDestroy);
    CompletedTask.Free;
  end;
end;

procedure TOlcbTaskEngine.ProcessSending;
var
  List: TLIst;
  Task, CompletedTask: TOlcbTaskBase;
begin
  CompletedTask := nil;
  List := TaskList.LockList;
  try
    if List.Count > 0 then
    begin
      Task := TOlcbTaskBase( List[0]);
      if Task.Sending then
      begin
        Task.Process(nil);
        if Task.Done then
        begin
          CompletedTask := Task;
          List.Delete(0);
        end;
      end;
    end;
  finally
    TaskList.UnlockList;
  end;

  // Do this outside the locked list so we don't deadlock if the Syncronize tries to add a new Task to the list!
  if Assigned(CompletedTask) then
  begin
    Owner.Synchronize(@CompletedTask.SyncOnBeforeTaskDestroy);
    CompletedTask.Free;
  end;
end;

destructor TOlcbTaskEngine.Destroy;
begin
  Clear;
  FreeAndNil(FTaskList);
  inherited Destroy;
end;

procedure TOlcbTaskEngine.Clear;
var
  i: Integer;
  List: TList;
begin
  List := TaskList.LockList;
  try
    for i := 0 to List.Count - 1 do
      TObject( List[i]).Free
  finally
    List.Clear;
    TaskList.LockList;
  end;
end;

initialization
  TaskObjects := 0;

finalization

end.

