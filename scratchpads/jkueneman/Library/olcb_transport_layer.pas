unit olcb_transport_layer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, dialogs, olcb_utilities, olcb_defines,
  olcb_app_common_settings, math_float16, Forms, blcksock, synsock, contnrs,
  Controls;


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


const
  GRIDCONNECT_STATE_SYNC_START = 0;
  GRIDCONNECT_STATE_SYNC_FIND_X = 1;
  GRIDCONNECT_STATE_SYNC_FIND_HEADER = 2;
  GRIDCONNECT_STATE_SYNC_FIND_DATA = 4;

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
  TTransportLayerThread                 = class;
  TTaskOlcbBase                         = class;
  TOlcbTaskEngine                       = class;
  TCANFrameParserDatagramReceiveManager = class;
  TCANFrameParserDatagramSendManager    = class;
  TCANFrameParserDatagramReceive        = class;
  TCANFrameParserDatagramSend           = class;
  TCANFrameParserStreamSend             = class;
  TCANFrameParserStreamBase             = class;
  TCANFrameParserStreamSendManager      = class;
  TCANFrameParserStreamReceiveManager   = class;

  TOlcbTaskBeforeDestroy = procedure(Sender: TTaskOlcbBase) of object;
  TDispatchTaskFunc = function(Task: TTaskOlcbBase): Boolean of object;
  TStreamDataArray = array of byte;

  { TAliasTaskContainer }

  TAliasTaskContainer = class
  private
    FAliasID: Word;
    FAttemptCount: Integer;
  public
    constructor Create(AnAliasID: Word);
    destructor Destroy; override;
    property AliasID: Word read FAliasID;
    property AttemptCount: Integer read FAttemptCount write FAttemptCount;
  end;

  { TAliasTaskContainerList }

  TAliasTaskContainerList = class(TFPObjectList)
  private
    function GetAliasTaskContainer(Index: Integer): TAliasTaskContainer;
    procedure SetAliasTaskContainer(Index: Integer; AValue: TAliasTaskContainer);
  public
    function FindByAlias(AnAlias: Word): TAliasTaskContainer;
    procedure RemoveByAlias(AnAlias: Word);
    property AliasTaskContainer[Index: Integer]: TAliasTaskContainer read GetAliasTaskContainer write SetAliasTaskContainer;
  end;

  { TTransportLayerThread }

  TTransportLayerThread = class(TThread)
  private
    FAliasList: TAliasTaskContainerList;
    FBufferRawMessage: string;                                                  // Shared data to pass string between thread and the Syncronized callbacks
    FCANFrameParserDatagramReceiveManager: TCANFrameParserDatagramReceiveManager;
    FCANFrameParserDatagramSendManager: TCANFrameParserDatagramSendManager;
    FConnectionState: TConnectionState;
    FEnableOPStackCallback: Boolean;
    FEnableReceiveMessages: Boolean;                                            // Callback through Syncronize with the message that was received
    FEnableSendMessages: Boolean;                                               // Callback through Syncronize with the message that is about to be sen
    FGridConnectReceiveState: Integer;
    FOlcbTaskManager: TOlcbTaskEngine;
    FOnBeforeDestroy: TNotifyEvent;
    FOnBeforeDestroyTask: TOlcbTaskBeforeDestroy;                               // Links the Task handler to this thread for Tasks that this thread creates when it receives unsolicited messages
    FOnOPstackCallback: TOnOPStackCallback;
    FOnStatus: THookSocketStatus;
    FReceiveGridConnectBuffer: TGridConnectString;
    FReceiveGridConnectBufferIndex: Integer;
    FRunning: Boolean;
    FCANFrameParserStreamReceiveManager: TCANFrameParserStreamReceiveManager;
    FCANFrameParserStreamSendManager: TCANFrameParserStreamSendManager;
    FStatusReason: THookSocketReason;
    FStatusValue: string;
    FOnConnectionStateChange: TOnConnectionStateChange;
    FOnErrorMessage: TOnRawMessage;                                 // Function to callback through Syncronize if an error connecting occured
    FOnReceiveMessage: TOnRawMessage;                               // Function to callback through Syncronize if EnableReceiveMessages is true
    FOnSendMessage: TOnRawMessage;                                  // Function to callback through Syncronize if EnableSendMessages is true
    FTerminateComplete: Boolean;                                                 // True if the thread has terminated
    FThreadListSendStrings: TThreadList;                                        // List of strings waiting to be sent
    function GetSourceAlias: Word;
    function GetTaskCount: Integer;
  protected
    procedure DecomposeAndDispatchGridConnectString(ReceiveStr: AnsiString; Helper: TOpenLCBMessageHelper);
    procedure ExecuteBegin;
    procedure ExecuteEnd;
    function GridConnect_DecodeMachine(NextChar: Char; var GridConnectStrPtr: PGridConnectString): Boolean;
    procedure InternalAdd(Msg: AnsiString);
    procedure InternalAddDatagramToSendByCANParsing(Datagram: TCANFrameParserDatagramSend);
    function IsValidHexChar(AChar: Char): Boolean;
    procedure ShowErrorMessageAndTerminate(Message: string);
    procedure SyncOnConnectionState; virtual;
    procedure SyncOnErrorMessage;
    procedure SyncOnReceiveMessage;
    procedure SyncOnSendMessage;
    procedure SyncOnStatus;
    procedure SyncOnOPStackCallback;

    property AliasList: TAliasTaskContainerList read FAliasList write FAliasList;                 // Tracks the Alias that have been registered with Olcb through this Transport Thread
    property BufferRawMessage: string read FBufferRawMessage write FBufferRawMessage;
    property CANFrameParserDatagramReceiveManager: TCANFrameParserDatagramReceiveManager read FCANFrameParserDatagramReceiveManager;
    property CANFrameParserDatagramSendManager: TCANFrameParserDatagramSendManager read FCANFrameParserDatagramSendManager write FCANFrameParserDatagramSendManager;
    property CANFrameParserStreamReceiveManager: TCANFrameParserStreamReceiveManager read FCANFrameParserStreamReceiveManager write FCANFrameParserStreamReceiveManager;
    property CANFrameParserStreamSendManager: TCANFrameParserStreamSendManager read FCANFrameParserStreamSendManager write FCANFrameParserStreamSendManager;
    property ConnectionState: TConnectionState read FConnectionState write FConnectionState;
    property GridConnectReceiveState: Integer read FGridConnectReceiveState write FGridConnectReceiveState;
    property ReceiveGridConnectBuffer: TGridConnectString read FReceiveGridConnectBuffer write FReceiveGridConnectBuffer;
    property ReceiveGridConnectBufferIndex: Integer read FReceiveGridConnectBufferIndex write FReceiveGridConnectBufferIndex;
    property OlcbTaskManager: TOlcbTaskEngine read FOlcbTaskManager write FOlcbTaskManager;
    property StatusReason: THookSocketReason read FStatusReason write FStatusReason;
    property StatusValue: string read FStatusValue write FStatusValue;
  public
    constructor Create(CreateSuspended: Boolean); virtual;
    destructor Destroy; override;
    function AddTask(NewTask: TTaskOlcbBase; CopyTask: Boolean): Boolean;
    procedure RemoveAndFreeTasks(RemoveKey: PtrInt);

    property EnableReceiveMessages: Boolean read FEnableReceiveMessages write FEnableReceiveMessages;
    property EnableSendMessages: Boolean read FEnableSendMessages write FEnableSendMessages;
    property EnableOPStackCallback: Boolean read FEnableOPStackCallback write FEnableOPStackCallback;
    property ThreadListSendStrings: TThreadList read FThreadListSendStrings write FThreadListSendStrings;
    property Terminated;
    property TerminateComplete: Boolean read FTerminateComplete;
    property OnConnectionStateChange: TOnConnectionStateChange read FOnConnectionStateChange write FOnConnectionStateChange;
    property OnErrorMessage: TOnRawMessage read FOnErrorMessage write FOnErrorMessage;
    property OnReceiveMessage: TOnRawMessage read FOnReceiveMessage write FOnReceiveMessage;
    property OnSendMessage: TOnRawMessage read FOnSendMessage write FOnSendMessage;
    property OnBeforeDestroyTask: TOlcbTaskBeforeDestroy read FOnBeforeDestroyTask write FOnBeforeDestroyTask;
    property OnBeforeDestroy: TNotifyEvent read FOnBeforeDestroy write FOnBeforeDestroy;
    property OnStatus: THookSocketStatus read FOnStatus write FOnStatus;
    property OnOPStackCallback: TOnOPStackCallback read FOnOPStackCallback write FOnOPStackCallback;
    property Running: Boolean read FRunning;
    property SourceAlias: Word read GetSourceAlias;
    property TaskCount: Integer read GetTaskCount;
  end;

  { TOlcbStructureHelperBase }

  TOlcbStructureHelperBase = class
  public
    procedure CopyTo(Target: TOlcbStructureHelperBase); virtual; abstract;
  end;

  { TOlcbStructureMemOptions }

  TOlcbStructureMemOptions = class(TOlcbStructureHelperBase)
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
    procedure LoadFromDatagram( Datagram: TCANFrameParserDatagramReceive);
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

  { TOlcbStructureMemAddressSpace }

  TOlcbStructureMemAddressSpace = class(TOlcbStructureHelperBase)
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
    procedure LoadByDatagram(ADatagram: TCANFrameParserDatagramReceive);
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

  { TOlcbStructureMemConfig }

  TOlcbStructureMemConfig = class(TOlcbStructureHelperBase)
  private
    FAddressSpaceList: TList;
    FOptions: TOlcbStructureMemOptions;
    function GetAddressSpace(Index: Integer): TOlcbStructureMemAddressSpace;
    function GetAddressCount: Integer;
    procedure SetAddressSpace(Index: Integer; AValue: TOlcbStructureMemAddressSpace);
  protected
    procedure Clear;
    property AddressSpaceList: TList read FAddressSpaceList write FAddressSpaceList;
  public
    constructor Create;
    destructor Destroy; override;
    function AddAddressSpace: TOlcbStructureMemAddressSpace;
    function AddAddressSpaceByDatagram(Datagram: TCANFrameParserDatagramReceive): TOlcbStructureMemAddressSpace;
    procedure CopyTo(Target: TOlcbStructureHelperBase); override;
    function FindAddressSpaceBySpaceID(AnAddress: Byte): TOlcbStructureMemAddressSpace;

    property AddressSpace[Index: Integer]: TOlcbStructureMemAddressSpace read GetAddressSpace write SetAddressSpace;
    property AddressSpaceCount: Integer read GetAddressCount;
    property Options: TOlcbStructureMemOptions read FOptions;
  end;

  { TOlcbStructureProtocolIdentification }

  TOlcbStructureProtocolIdentification = class(TOlcbStructureHelperBase)
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

  { TOlcbStructureSNIP }

  TOlcbStructureSNIP = class(TOlcbStructureHelperBase)
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

  { TCANFrameParserDatagramReceive }

  TCANFrameParserDatagramReceive = class( TOlcbMessage)
  private
    FCreateTime: DWord;
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
    property CreateTime: DWord read FCreateTime write FCreateTime;
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

  { TCANFrameParserDatagramReceiveManager }

  TCANFrameParserDatagramReceiveManager = class
  private
    FDatagrams: TThreadList;
    FMaxCount: Integer;
    FOwner: TTransportLayerThread;
  protected
    function FindInProcessDatagramAndCheckForAbandonDatagrams(AHelper: TOpenLCBMessageHelper): TCANFrameParserDatagramReceive;
    property Datagrams: TThreadList read FDatagrams write FDatagrams;
    property Owner: TTransportLayerThread read FOwner write FOwner;
    property MaxCount: Integer read FMaxCount write FMaxCount;
  public
    constructor Create(AnOwner: TTransportLayerThread);
    destructor Destroy; override;
    procedure Clear;
    function Process(AHelper: TOpenLCBMessageHelper): TCANFrameParserDatagramReceive;
  end;


  { TCANFrameParserStreamBase }

  TCANFrameParserStreamBase = class( TOlcbMessage)
  private
    FAdditionalFlags: Byte;
    FCreateTime: DWord;
    FDestinationAlias: Word;
    FDestStreamID: Byte;
    FEmpty: Boolean;
    FFlags: Byte;
    FHasUniqueStreamUID: Boolean;
    FLocalHelper: TOpenLCBMessageHelper;
    FMaxBufferSize: Word;
    FSourceAlias: Word;
    FSourceStreamID: Byte;
    FStream: TMemoryStream;
    FStreamData: TStreamDataArray;
    FUniqueStreamUID: DWord;
  protected
    property LocalHelper: TOpenLCBMessageHelper read FLocalHelper write FLocalHelper;   // Global object to work with OLCB messages
    property CreateTime: DWord read FCreateTime write FCreateTime;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word);
    destructor Destroy; override;
    function ProcessReceive(AHelper: TOpenLCBMessageHelper; TransportLayerThread: TTransportLayerThread): Boolean; virtual; abstract;  // Processes the message/
    function ProcessSend(TransportLayerThread: TTransportLayerThread): Boolean; virtual; abstract;
    property Empty: Boolean read FEmpty write FEmpty;
    property MaxBufferSize: Word read FMaxBufferSize write FMaxBufferSize;
    property Flags: Byte read FFlags write FFlags;
    property AdditionalFlags: Byte read FAdditionalFlags write FAdditionalFlags;
    property HasUniqueStreamUID: Boolean read FHasUniqueStreamUID write FHasUniqueStreamUID;
    property UniqueStreamUID: DWord read FUniqueStreamUID write FUniqueStreamUID;
    property SourceStreamID: Byte read FSourceStreamID write FSourceStreamID;
    property DestStreamID: Byte read FDestStreamID write FDestStreamID;
    property StreamData: TStreamDataArray read FStreamData write FStreamData;
    property DestinationAlias: Word read FDestinationAlias write FDestinationAlias;
    property SourceAlias: Word read FSourceAlias write FSourceAlias;
    property Stream: TMemoryStream read FStream write FStream;
  end;

  { TCANFrameParserStreamReceive }

  TCANFrameParserStreamReceive = class(TCANFrameParserStreamBase)
  public
    function ProcessReceive(AHelper: TOpenLCBMessageHelper; TransportLayerThread: TTransportLayerThread): Boolean; override;   // Processes the message/
    function ProcessSend(TransportLayerThread: TTransportLayerThread): Boolean; override;
  end;

  { TCANFrameParserStreamReceiveManager }

  TCANFrameParserStreamReceiveManager = class
  private
    FMaxCount: Integer;
    FOwner: TTransportLayerThread;
    FStreams: TThreadList;
    v: TThreadList;
  protected
    function FindInProcessStreamAndCheckForAbandonStream(AHelper: TOpenLCBMessageHelper): TCANFrameParserStreamReceive;
    property Streams: TThreadList read FStreams write v;
    property Owner: TTransportLayerThread read FOwner write FOwner;
    property MaxCount: Integer read FMaxCount write FMaxCount;
  public
    constructor Create(AnOwner: TTransportLayerThread);
    destructor Destroy; override;
    procedure Clear;
    function ProcessReceive(AHelper: TOpenLCBMessageHelper): TCANFrameParserStreamBase;
    function ProcessSend(AHelper: TOpenLCBMessageHelper): TCANFrameParserStreamBase;
  end;

{ TCANFrameParserDatagramSend }

TCANFrameParserDatagramSend = class( TOlcbMessage)
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
  constructor Create; virtual;
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

{ TCANFrameParserDatagramSendManager }

TCANFrameParserDatagramSendManager = class
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
  function ProcessReceive(AHelper: TOpenLCBMessageHelper): TCANFrameParserDatagramSend;
  procedure ProcessSend;
end;

{ TCANFrameParserStreamSend }

TCANFrameParserStreamSend = class( TCANFrameParserStreamBase)
public
  function ProcessSend(TransportLayerThread: TTransportLayerThread): Boolean; override;
  function ProcessReceive(AHelper: TOpenLCBMessageHelper; TransportLayerThread: TTransportLayerThread): Boolean; override;
end;

{ TCANFrameParserStreamSendManager }

TCANFrameParserStreamSendManager = class
private
  FAbandonStreams: TThreadList;
  FMaxCount: Integer;
  FOwner: TTransportLayerThread;
  FStreams: TThreadList;
  FTimer: TTimer;
protected
protected
  procedure TimerTick(Sender: TObject);
  property AbandonStreams: TThreadList read FAbandonStreams write FAbandonStreams;
  property Streams: TThreadList read FStreams write FStreams;
  property MaxCount: Integer read FMaxCount write FMaxCount;
  property Owner: TTransportLayerThread read FOwner write FOwner;
  property Timer: TTimer read FTimer write FTimer;
public
  constructor Create(AnOwner: TTransportLayerThread);
  destructor Destroy; override;
  procedure Clear;
  procedure ClearAbandon;
  function ProcessReceive(AHelper: TOpenLCBMessageHelper): TCANFrameParserStreamSend;
  procedure ProcessSend;
end;

{ TTaskOlcbBase }

  TTaskOlcbBase = class
  private
    FErrorCode: DWord;
    FiLogState: Integer;
    FLogThreadName: string;
    FMessageHelper: TOpenLCBMessageHelper;
    FMessageWaitTimeStart: DWord;
    FOnBeforeDestroy: TOlcbTaskBeforeDestroy;
    FOwnerControl: TControl;
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
    procedure ExtractErrorInformation(DatagramReceive: TCANFrameParserDatagramReceive);
    function IsDatagramAckFromDestination(MessageInfo: TOlcbMessage): Boolean;
    function IsConfigMemorySpaceInfoReplyFromDestination(MessageInfo: TOlcbMessage; AnAddress: Byte; var DatagramReceive: TCANFrameParserDatagramReceive): Boolean;
    function IsConfigMemoryOptionsReplyFromDestination(MessageInfo: TOlcbMessage; var DatagramReceive: TCANFrameParserDatagramReceive): Boolean;
    function IsConfigMemoryReadReplyFromDestination(MessageInfo: TOlcbMessage; var DatagramReceive: TCANFrameParserDatagramReceive): Boolean;
    function IsProtocolIdentificationProcolReplyFromDestination(MessageInfo: TOlcbMessage): Boolean;
    function IsStreamInitializationRequest(MessageInfo: TOlcbMessage): Boolean;
    function IsStreamSendFromDestination(MessageInfo: TOlcbMessage; StreamReceive: TCANFrameParserStreamBase): Boolean;
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
    procedure MessageWaitTimerReset;
    function MessageWaitTimerCheckTimeout: Boolean;
    procedure Process(MessageInfo: TOlcbMessage); virtual;                      // Must override this
    procedure SendIdentifyEventsMessage;
    procedure SendIdentifyEventsAddressedMessage;
    procedure SendIdentifyConsumerMessage(Event: TEventID);
    procedure SendIdentifyProducerMessage(Event: TEventID);
    procedure SendMemoryConfigurationOptionsByCANParsing;
    procedure SendMemoryConfigurationSpaceInfoByCANParsing(Space: Byte);
    procedure SendMemoryConfigurationReadByCANParsing(Space: Byte; StartAddress: DWord; Count: DWord; ForceUseOfSpaceByte: Boolean; UseStream: Boolean);
    procedure SendMemoryConfigurationWriteByCANParsing(Space: Byte; StartAddress: DWord; MaxAddressSize: DWORD; ForceUseOfSpaceByte: Boolean; AStream: TStream);
    procedure SendProtocolIdentificationProtocolMessage;
    procedure SendStreamInitReply(NegotiatedBuffer: Word; Flag, AdditionalFlags, SourceID, DestID: Byte);
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
    property LogTheadName: string read FLogThreadName write FLogThreadName;
    property MessageWaitTimeStart: DWord read FMessageWaitTimeStart write FMessageWaitTimeStart;
    property StartAsSending: Boolean read FStartAsSending write FStartAsSending;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean); virtual;
    destructor Destroy; override;
    function Clone: TTaskOlcbBase; virtual; abstract;
    procedure CopyTo(Target: TTaskOlcbBase); virtual;
    property DestinationAlias: Word read FDestinationAlias;
    property ErrorCode: DWord read FErrorCode write FErrorCode;
    property ErrorString: string read FErrorString write FErrorString;
    property ForceTermination: Boolean read FForceTermination write FForceTermination;
    property HasStarted: Boolean read FHasStarted;
    property MessageHelper: TOpenLCBMessageHelper read FMessageHelper write FMessageHelper;
    property OnBeforeDestroy: TOlcbTaskBeforeDestroy read FOnBeforeDestroy write FOnBeforeDestroy;
    property OwnerControl: TControl read FOwnerControl write FOwnerControl;
    property SourceAlias: Word read FSourceAlias;
    property Sending: Boolean read FSending write FSending;
    property Tag: PtrInt read FTag write FTag;
    property TransportLayerThread: TTransportLayerThread read FTransportLayerThread;
    property RemoveKey: PtrInt read FRemoveKey write FRemoveKey;
  end;

  { TTaskVerifyNodeIDGlobal }

  TTaskVerifyNodeIDGlobal = class(TTaskOlcbBase)
  public
   function Clone: TTaskOlcbBase; override;
   procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TTaskVerifyNodeID }

  TTaskVerifyNodeID = class(TTaskOlcbBase)
  public
    function Clone: TTaskOlcbBase; override;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TTaskProtocolSupport }

  TTaskProtocolSupport = class(TTaskOlcbBase)
  private
    FProtocols: QWord;
  public
    function Clone: TTaskOlcbBase; override;
    procedure CopyTo(Target: TTaskOlcbBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;
    property Protocols: QWord read FProtocols;
  end;

  { TTaskSimpleNodeInformation }

  TTaskSimpleNodeInformation = class(TTaskOlcbBase)
  private
    FSnip: TOlcbStructureSNIP;
    FiSnipState: Integer;  // for inner SNIP statemachine
  protected
    property iSnipState: Integer read FiSnipState write FiSnipState;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean); override;
    destructor Destroy; override;
    function Clone: TTaskOlcbBase; override;
    procedure CopyTo(Target: TTaskOlcbBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;
    property Snip: TOlcbStructureSNIP read FSnip;
  end;

  { TTaskConfigMemoryOptions }

  TTaskConfigMemoryOptions = class(TTaskOlcbBase)
  private
    FConfigMemoryOptions: TOlcbStructureMemOptions;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean); override;
    destructor Destroy; override;
    function Clone: TTaskOlcbBase; override;
    procedure CopyTo(Target: TTaskOlcbBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;
    property ConfigMemoryOptions: TOlcbStructureMemOptions read FConfigMemoryOptions write FConfigMemoryOptions;
  end;

  { TTaskConfigMemoryAddressSpaceInfo }

  TTaskConfigMemoryAddressSpaceInfo = class(TTaskOlcbBase)
  private
    FAddressSpace: Byte;
    FConfigMemoryAddressSpace: TOlcbStructureMemAddressSpace;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddressSpace: Byte); reintroduce; virtual;
    destructor Destroy; override;
    function Clone: TTaskOlcbBase; override;
    procedure CopyTo(Target: TTaskOlcbBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;
    property ConfigMemoryAddressSpace: TOlcbStructureMemAddressSpace read FConfigMemoryAddressSpace write FConfigMemoryAddressSpace;
    property AddressSpace: Byte read FAddressSpace;
  end;


  { TTaskConfigMemoryAddressEnumAllSpaceInfo }

  TTaskConfigMemoryAddressEnumAllSpaceInfo = class(TTaskOlcbBase)
  private
    FConfigMemAddressInfo: TOlcbStructureMemConfig;
    FCurrentAddressSpace: Byte;
    FMaxAddressSpace: Byte;
    FMinAddressSpace: Byte;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean); override;
    destructor Destroy; override;
    function Clone: TTaskOlcbBase; override;
    procedure CopyTo(Target: TTaskOlcbBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;
    property MinAddressSpace: Byte read FMinAddressSpace write FMinAddressSpace;
    property MaxAddressSpace: Byte read FMaxAddressSpace write FMaxAddressSpace;
    property CurrentAddressSpace: Byte read FCurrentAddressSpace write FCurrentAddressSpace;
    property ConfigMemAddressInfo: TOlcbStructureMemConfig read FConfigMemAddressInfo write FConfigMemAddressInfo;
  end;

  { TTaskAddressSpaceMemoryCommonWithDatagram }

  TTaskAddressSpaceMemoryCommonWithDatagram = class(TTaskOlcbBase)
  private
    FAddressSpace: Byte;
    FCurrentAddress: DWord;
    FCurrentSendSize: DWord;
    FDataStream: TMemoryStream;
    FForceOptionalSpaceByte: Boolean;
    FLocalLoopTime: DWord;
    FMaxAddress: DWord;
    FMinAddress: DWord;
    FTerminator: Char;
    FUsingTerminator: Boolean;
    FWritingToAddress: Boolean;
    function GetMaxPayloadSize: DWord;
  protected
    property CurrentAddress: DWord read FCurrentAddress write FCurrentAddress;
    property CurrentSendSize: DWord read FCurrentSendSize write FCurrentSendSize;
    property LocalLoopTime: DWord read FLocalLoopTime write FLocalLoopTime;
    property UsingTerminator: Boolean read FUsingTerminator write FUsingTerminator;
    property MaxPayloadSize: DWord read GetMaxPayloadSize;
    property WritingToAddress: Boolean read FWritingToAddress write FWritingToAddress;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddressSpace: Byte; UseTerminatorChar: Boolean); reintroduce;
    destructor Destroy; override;
    function Clone: TTaskOlcbBase; override;
    procedure CopyTo(Target: TTaskOlcbBase); override;
    property AddressSpace: Byte read FAddressSpace;
    property DataStream: TMemoryStream read FDataStream;
    property ForceOptionalSpaceByte: Boolean read FForceOptionalSpaceByte write FForceOptionalSpaceByte;
    property MinAddress: DWord read FMinAddress;
    property MaxAddress: DWord read FMaxAddress;
    property Terminator: Char read FTerminator write FTerminator;
  end;

  { TTaskAddressSpaceMemoryBaseWithDatagram }

  TTaskAddressSpaceMemoryBaseWithDatagram = class(TTaskAddressSpaceMemoryCommonWithDatagram)
  public
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TTaskAddressSpaceMemoryReadWithDatagram }

  TTaskAddressSpaceMemoryReadWithDatagram = class(TTaskAddressSpaceMemoryBaseWithDatagram)
  public
    function Clone: TTaskOlcbBase; override;
  end;

  { TTaskAddressSpaceMemoryWriteWithDatagram }

  TTaskAddressSpaceMemoryWriteWithDatagram = class(TTaskAddressSpaceMemoryBaseWithDatagram)
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddressSpace: Byte; AStream: TStream); reintroduce;
    function Clone: TTaskOlcbBase; override;
  end;


  { TTaskAddressSpaceMemoryBaseWithStream }

  TTaskAddressSpaceMemoryBaseWithStream = class(TTaskAddressSpaceMemoryCommonWithDatagram)
  private
    FDestID: Byte;
    FNegotiatedBuffer: Word;
    FSourceID: Byte;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddressSpace: Byte; UseTerminatorChar: Boolean); reintroduce;
    destructor Destroy; override;
    function Clone: TTaskOlcbBase; override;
    procedure CopyTo(Target: TTaskOlcbBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;
    property NegotiatedBuffer: Word read FNegotiatedBuffer write FNegotiatedBuffer;
    property SourceID: Byte read FSourceID write FSourceID;
    property DestID: Byte read FDestID write FDestID;
  end;

  TTaskAddressSpaceMemoryReadWithStream = class(TTaskAddressSpaceMemoryBaseWithStream)

  end;

  TTaskAddressSpaceMemoryWriteWithStream = class(TTaskAddressSpaceMemoryBaseWithStream)

  end;

  { TTaskAddressSpaceMemoryWriteRawWithDatagram }

  TTaskAddressSpaceMemoryWriteRawWithDatagram = class(TTaskOlcbBase)
  private
    FAddressSpace: Byte;
    FForceOptionalSpaceByte: Boolean;
    FStream: TMemoryStream;
    FWriteAddress: DWord;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddressSpace: Byte; AWriteAddress: DWord; AStream: TStream); reintroduce;
    destructor Destroy; override;
    function Clone: TTaskOlcbBase; override;
    procedure CopyTo(Target: TTaskOlcbBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;

    property AddressSpace: Byte read FAddressSpace;
    property ForceOptionalSpaceByte: Boolean read FForceOptionalSpaceByte write FForceOptionalSpaceByte;
    property Stream: TMemoryStream read FStream;
    property WriteAddress: DWord read FWriteAddress write FWriteAddress;
  end;

  { TTaskAddressSpaceMemoryReadRawWithDatagram }

  TTaskAddressSpaceMemoryReadRawWithDatagram = class(TTaskOlcbBase)
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
    function Clone: TTaskOlcbBase; override;
    procedure CopyTo(Target: TTaskOlcbBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;

    property AddressSpace: Byte read FAddressSpace;
    property ForceOptionalSpaceByte: Boolean read FForceOptionalSpaceByte write FForceOptionalSpaceByte;
    property IncludeTerminator: Boolean read FIncludeTerminator write FIncludeTerminator;  // Include the terminator in the Stream result
    property Stream: TMemoryStream read FStream;
    property ReadAddress: DWord read FReadAddress;
    property ReadByteCount: DWord read FReadByteCount;
    property Terminator: Char read FTerminator write FTerminator;
  end;

  { TTaskIdentifyEvents }

  TTaskIdentifyEvents = class(TTaskOlcbBase)
  public
    function Clone: TTaskOlcbBase; override;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TTaskIdentifyEventsAddressed }

  TTaskIdentifyEventsAddressed = class(TTaskOlcbBase)
  public
    function Clone: TTaskOlcbBase; override;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TTaskIdentifyProducer }

  TTaskIdentifyProducer = class(TTaskOlcbBase)
  private
    FEvent: TEventID;
  protected
    property Event: TEventID read FEvent write FEvent;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnEvent: TEventID); reintroduce;
    function Clone: TTaskOlcbBase; override;
    procedure CopyTo(Target: TTaskOlcbBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TTaskIdentifyConsumer }

  TTaskIdentifyConsumer = class(TTaskOlcbBase)
  private
    FEvent: TEventID;
  protected
    property Event: TEventID read FEvent write FEvent;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnEvent: TEventID); reintroduce;
    function Clone: TTaskOlcbBase; override;
    procedure CopyTo(Target: TTaskOlcbBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TTaskCANLayer }
  TTaskCANLayer = class(TTaskOlcbBase)
  public
    function Clone: TTaskOlcbBase; override;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TTaskEvent }

  TTaskEvent = class(TTaskOlcbBase)
  public
    function Clone: TTaskOlcbBase; override;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TTaskVerifiedNodeID }

  TTaskVerifiedNodeID = class(TTaskOlcbBase)
  public
    function Clone: TTaskOlcbBase; override;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TTaskTractionProtocol }

  TTaskTractionProtocol = class(TTaskOlcbBase)
  public
    function Clone: TTaskOlcbBase; override;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TTaskInitializationComplete }

  TTaskInitializationComplete = class(TTaskOlcbBase)
  public
    function Clone: TTaskOlcbBase; override;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TTaskTractionReserveAndAttachDccProxy }

  TTaskTractionReserveAndAttachDccProxy = class(TTaskOlcbBase)
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
    function Clone: TTaskOlcbBase; override;
    procedure CopyTo(Target: TTaskOlcbBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;

    property ReplyCode: Integer read FReplyCode;        // -1 if the Reply Code was not sent
    property ReplySpeedSteps: Byte read FReplySpeedSteps;
    property ReplyAddress: Word read FReplyAddress;
  end;

  { TTaskTractionReserveAndDetachDccProxy }

  TTaskTractionReserveAndDetachDccProxy = class(TTaskOlcbBase)
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
    function Clone: TTaskOlcbBase; override;
    procedure CopyTo(Target: TTaskOlcbBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;

    property ReplyCode: Integer read FReplyCode;        // -1 if the Reply Code was not sent
    property ReplySpeedSteps: Byte read FReplySpeedSteps;
    property ReplyAddress: Word read FReplyAddress;
  end;

  { TTaskTractionQueryDccAddressProxy }

  TTaskTractionQueryDccAddressProxy = class(TTaskOlcbBase)
  private
    FAddress: Word;
    FIsShort: Boolean;
  protected
    property Address: Word read FAddress write FAddress;
    property IsShort: Boolean read FIsShort write FIsShort;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddress: Word; IsShortAddress: Boolean); reintroduce;
    function Clone: TTaskOlcbBase; override;
    procedure CopyTo(Target: TTaskOlcbBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TTaskTractionSpeed }

  TTaskTractionSpeed = class(TTaskOlcbBase)
  private
    FEStop: Boolean;
    FSpeed: THalfFloat;
  protected
    property Speed: THalfFloat read FSpeed write FSpeed;  // Dir is wrapped up in the neg sign
    property EStop: Boolean read FEStop write FEStop;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; ASpeed: THalfFloat; IsEStop: Boolean); reintroduce;
    function Clone: TTaskOlcbBase; override;
    procedure CopyTo(Target: TTaskOlcbBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TTaskTractionFunction }

  TTaskTractionFunction = class(TTaskOlcbBase)
  private
    FAddress: DWord;
    FWord: Word;
  protected
    property Address: DWord read FAddress write FAddress;
    property Value: Word read FWord write FWord;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddress: DWord; AValue: Word); reintroduce;
    function Clone: TTaskOlcbBase; override;
    procedure CopyTo(Target: TTaskOlcbBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TTaskTractionQueryFunction }

  TTaskTractionQueryFunction = class(TTaskOlcbBase)
  private
    FAddress: DWord;
    FValue: Integer;
  protected
    property Address: DWord read FAddress write FAddress;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddress: DWord); reintroduce;
    function Clone: TTaskOlcbBase; override;
    procedure CopyTo(Target: TTaskOlcbBase); override;
    procedure Process(MessageInfo: TOlcbMessage); override;

    property Value: Integer read FValue write FValue;
  end;

  { TTaskTractionQuerySpeed }

  TTaskTractionQuerySpeed = class(TTaskOlcbBase)
  private
    FActualSpeed: Word;
    FCommandedSpeed: Word;
    FSetSpeed: Word;
    FStatus: Byte;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean); reintroduce;
    function Clone: TTaskOlcbBase; override;
    procedure CopyTo(Target: TTaskOlcbBase); override;
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

var
  DestStreamIdPool, SourceStreamIdPool: Byte;

function GenerateDestID: Byte;
begin
  if DestStreamIdPool = 0 then
    Inc(DestStreamIdPool);
  Result := DestStreamIdPool;
  Inc(DestStreamIdPool);
end;

function GenerateSourceID: Byte;
begin
  if SourceStreamIdPool = 0 then
    Inc(SourceStreamIdPool);
  Result := SourceStreamIdPool;
  Inc(SourceStreamIdPool);
end;

{ TCANFrameParserStreamReceive }

function TCANFrameParserStreamReceive.ProcessReceive(AHelper: TOpenLCBMessageHelper; TransportLayerThread: TTransportLayerThread): Boolean;
var
  LocalFlags: Byte;
  LocalAdditionalFlags: Byte;
begin
  Result := False;
  case AHelper.MTI of
      MTI_FRAME_TYPE_CAN_STREAM_SEND :
         begin
           if Empty then
           begin
  //         SetLength(FStreamData, MaxBufferSize + Length(FStreamData));  // We can handle anything the node can throw at us
           end
         end;
      MTI_STREAM_COMPLETE :
         begin
           Empty := False;
         end;
  end;
  // Wait for some data
end;

function TCANFrameParserStreamReceive.ProcessSend(TransportLayerThread: TTransportLayerThread): Boolean;
begin
  Result := False
end;

{ TCANFrameParserStreamSend }

function TCANFrameParserStreamSend.ProcessSend(TransportLayerThread: TTransportLayerThread): Boolean;
begin
  Result := False;
end;

function TCANFrameParserStreamSend.ProcessReceive(AHelper: TOpenLCBMessageHelper; TransportLayerThread: TTransportLayerThread): Boolean;
begin
  Result := False;
  // Is this needed?  Not sure.  In Datagram this is where we look for the ACK after it is sent.  Streams don't have ACKs
end;


{ TCANFrameParserStreamSendManager }

procedure TCANFrameParserStreamSendManager.TimerTick(Sender: TObject);
begin

end;

constructor TCANFrameParserStreamSendManager.Create(AnOwner: TTransportLayerThread);
begin
  inherited Create;
  FStreams := TThreadList.Create;
  FAbandonStreams := TThreadList.Create;
  FOwner := AnOwner;
  Timer := TTimer.Create(nil);
  Timer.Interval := 500;         // Every 500m seconds
  Timer.OnTimer := @TimerTick;
  Timer.Enabled := True;
  FMaxCount := 0;
end;

destructor TCANFrameParserStreamSendManager.Destroy;
begin
  Clear;
  FreeAndNil(FStreams);
  ClearAbandon;
  FreeAndNil(FAbandonStreams);
  FreeAndNil(FTimer);
  inherited Destroy;
end;

procedure TCANFrameParserStreamSendManager.Clear;
var
  i: Integer;
  List: TList;
begin
  List := Streams.LockList;
  try
    for i := 0 to List.Count - 1 do
      TObject( List[i]).Free;
  finally
    List.Clear;
    Streams.UnlockList;
  end;
end;

procedure TCANFrameParserStreamSendManager.ClearAbandon;
var
  i: Integer;
  List: TList;
begin
  List := AbandonStreams.LockList;
  try
    for i := 0 to List.Count - 1 do
      TObject( List[i]).Free;
  finally
    List.Clear;
    AbandonStreams.UnlockList;
  end;
end;

function TCANFrameParserStreamSendManager.ProcessReceive(AHelper: TOpenLCBMessageHelper): TCANFrameParserStreamSend;
var
  Stream: TCANFrameParserStreamSend;
  List: TList;
  i: Integer;
  Done: Boolean;
begin
  Result := nil;
  List := Streams.LockList;
  try
    i := 0;
    Done := False;
    while (i < List.Count) and not Done do
    begin
      Stream := TCANFrameParserStreamSend( List[i]);
      if Stream.ProcessReceive(AHelper, Owner) then
      begin
        Done := True;
        if Stream.Empty then
        begin
          List.Remove(Stream);
          Result := Stream;
        end;
      end;
      Inc(i);
    end;
  finally
    Streams.UnlockList;
  end;
end;

procedure TCANFrameParserStreamSendManager.ProcessSend;
var
  Stream: TCANFrameParserStreamSend;
  List: TList;
  i: Integer;
  Done: Boolean;
begin
  List := Streams.LockList;
  try
    i := 0;
    Done := False;
    while (i < List.Count) and not Done do
    begin
      Stream := TCANFrameParserStreamSend( List[i]);
      Done := Stream.ProcessSend(Owner);
      Inc(i);
    end;
  finally
    Streams.UnlockList;
  end;
end;

{ TCANFrameParserStreamReceiveManager }

function TCANFrameParserStreamReceiveManager.FindInProcessStreamAndCheckForAbandonStream(AHelper: TOpenLCBMessageHelper): TCANFrameParserStreamReceive;
//
// Searches an in process stream interaction between the nodes in the message
//
var
  i: Integer;
  List: TList;
  Stream: TCANFrameParserStreamReceive;
begin
  Result := nil;
  List := Streams.LockList;
  try
    for i := List.Count-1 downto 0 do    // So we can delete abandon items from the top of the list
    begin
      Stream := TCANFrameParserStreamReceive( List[i]);
      if Stream.Empty and (Stream.DestinationAlias = AHelper.SourceAliasID) and (Stream.SourceAlias = AHelper.DestinationAliasID) then
        Result := Stream;

      if Stream.CreateTime + GlobalSettings.General.StreamWaitTime > GetTickCount then
      begin
        // Abandon Stream
    //    Stream.Free;
   //     List.Delete(i);
      end;
    end;
  finally
    Streams.UnlockList;
  end;

end;

constructor TCANFrameParserStreamReceiveManager.Create(AnOwner: TTransportLayerThread);
begin
  inherited Create;
  FOwner := AnOwner;
  FStreams := TThreadList.Create;
  FMaxCount := 0;
end;

destructor TCANFrameParserStreamReceiveManager.Destroy;
begin
  Clear;
  FreeAndNil(FStreams);
  inherited Destroy;
end;

procedure TCANFrameParserStreamReceiveManager.Clear;
var
  i: Integer;
  List: TList;
begin
  List := Streams.LockList;
  try
    for i := 0 to List.Count - 1 do
      TObject( List[i]).Free;
  finally
    List.Clear;
    Streams.UnlockList;
  end;
end;

function TCANFrameParserStreamReceiveManager.ProcessReceive(AHelper: TOpenLCBMessageHelper): TCANFrameParserStreamBase;
var
  TestStream: TCANFrameParserStreamReceive;
  List: TList;
begin
  Result := nil;
  if IsStreamMTI(AHelper.MTI, True) then
  begin
    TestStream := FindInProcessStreamAndCheckForAbandonStream(AHelper);
    if not Assigned(TestStream) then
    begin
      TestStream := TCANFrameParserStreamReceive.Create(Owner.SourceAlias, AHelper.SourceAliasID);  // Create a new receiving Datagram object for source alias of the message to us
      Streams.Add(TestStream);
      List := Streams.LockList;
      if List.Count > MaxCount then
        MaxCount := List.Count;
      Streams.UnlockList;
    end;
    TestStream.ProcessReceive(AHelper, Owner);
    if not TestStream.Empty then
    begin
      Streams.Remove(TestStream);
      Result := TestStream
    end;
  end;
end;

function TCANFrameParserStreamReceiveManager.ProcessSend(AHelper: TOpenLCBMessageHelper): TCANFrameParserStreamBase;
begin

end;

{ TCANFrameParserStreamBase }

constructor TCANFrameParserStreamBase.Create(ASourceAlias, ADestinationAlias: Word);
begin
  inherited Create;
  FLocalHelper := TOpenLCBMessageHelper.Create;
  FStream := TMemoryStream.Create;
  FAdditionalFlags := 0;
  FCreateTime :=   GetTickCount;
  FDestinationAlias := ADestinationAlias;
  FDestStreamID := 0;
  FEmpty := True;
  FFlags := 0;
  FHasUniqueStreamUID := False;
  FMaxBufferSize := 0;
  FSourceAlias := ASourceAlias;
  FSourceStreamID := 0;
  FStreamData := nil;
  FUniqueStreamUID := 0;
end;

destructor TCANFrameParserStreamBase.Destroy;
begin
  FreeAndNil(FLocalHelper);
  FreeAndNil(FStream);
  inherited Destroy;
end;


{ TTaskAddressSpaceMemoryBaseWithDatagram }

procedure TTaskAddressSpaceMemoryBaseWithDatagram.Process(MessageInfo: TOlcbMessage);
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
  DatagramReceive: TCANFrameParserDatagramReceive;
  PIP: TOlcbStructureProtocolIdentification;
  Space: TOlcbStructureMemAddressSpace;
  Options: TOlcbStructureMemOptions;
  DatagramResultStart: DWord;
  i: Integer;
  Terminated: Boolean;
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         // Ask for the protocols the node supports
         SendProtocolIdentificationProtocolMessage;
         Inc(FiState);
         Sending := False;
       end;
    1: begin
         // First see if the node even supports the Memory Configuration Protocol
         if IsProtocolIdentificationProcolReplyFromDestination(MessageInfo) then
         begin
           PIP := TOlcbStructureProtocolIdentification.Create;
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
         SendMemoryConfigurationOptionsByCANParsing;
         Inc(FiState);
         Sending := False;
       end;
    3: begin
         // Node received the request datagram
         if IsDatagramAckFromDestination(MessageInfo) then
         begin
           Sending := False;
           Inc(FiState);
         end;
       end;
    4: begin
         // Is the address space we are asking for supported?
         DatagramReceive := nil;
         if IsConfigMemoryOptionsReplyFromDestination(MessageInfo, DatagramReceive) then
         begin
           Options := TOlcbStructureMemOptions.Create;
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
         SendMemoryConfigurationSpaceInfoByCANParsing(AddressSpace);
         Sending := False;
         Inc(FiState);
       end;
    6: begin
        // Node received the datagram
         if IsDatagramAckFromDestination(MessageInfo) then
         begin
           Sending := False;
           Inc(FiState);
         end;
       end;
    7: begin
         if IsConfigMemorySpaceInfoReplyFromDestination(MessageInfo, AddressSpace, DatagramReceive) then
         begin
           Space := TOlcbStructureMemAddressSpace.Create;
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

         LocalLoopTime := GetTickCount;

         if MaxAddress - CurrentAddress > MaxPayloadSize then
            CurrentSendSize := MaxPayloadSize
          else
            CurrentSendSize := MaxAddress - CurrentAddress;
         Sending := True;
         Inc(FiState);
       end;
    9: begin
        // Ask for a read from the node
         SendMemoryConfigurationReadByCANParsing(AddressSpace, CurrentAddress, CurrentSendSize, ForceOptionalSpaceByte, False);
         Sending := False;
         Inc(FiState);
       end;
    10: begin
         // Node received the datagram
         if IsDatagramAckFromDestination(MessageInfo) then
         begin
           Sending := False;
           Inc(FiState);
         end;
       end;
    11: begin
          // Node sending frame of data
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

                LocalLoopTime := GetTickCount - LoopTime;
                if LocalLoopTime > LoopTime then
                  LoopTime := LocalLoopTime;

              end;
            end
          end
        end;
    STATE_WRITE_START :
        begin
          if DataStream.Size > Space.AddressHi - Space.AddressLo then
            ErrorCode := ERROR_ADDRESS_SPACE_WRITE_LARGER_THAN_SPACE
          else
            SendMemoryConfigurationWriteByCANParsing(AddressSpace, CurrentAddress, Space.AddressHi - Space.AddressLo, ForceOptionalSpaceByte, DataStream);
          iState := STATE_DONE
        end;
    STATE_DONE : begin
       // Done
         FDone := True
       end;
  end;
end;

{ TTaskAddressSpaceMemoryBaseWithStream }

constructor TTaskAddressSpaceMemoryBaseWithStream.Create(ASourceAlias,
  ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddressSpace: Byte;
  UseTerminatorChar: Boolean);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending, AnAddressSpace, UseTerminatorChar);
  FDestID := 0;
  FSourceID := 0;
  FNegotiatedBuffer := 0;
end;

destructor TTaskAddressSpaceMemoryBaseWithStream.Destroy;
begin
  inherited Destroy;
end;

function TTaskAddressSpaceMemoryBaseWithStream.Clone: TTaskOlcbBase;
begin
  Result := TTaskAddressSpaceMemoryBaseWithStream.Create(SourceAlias, DestinationAlias, True, AddressSpace, UsingTerminator);
  (Result as TTaskAddressSpaceMemoryBaseWithStream).Terminator := Terminator;
end;

procedure TTaskAddressSpaceMemoryBaseWithStream.CopyTo(Target: TTaskOlcbBase);
begin
  inherited CopyTo(Target);
  if Target is TTaskAddressSpaceMemoryBaseWithStream then
  begin
    TTaskAddressSpaceMemoryBaseWithStream( Target).DestID := DestID;
    TTaskAddressSpaceMemoryBaseWithStream( Target).SourceID := SourceID;
    TTaskAddressSpaceMemoryBaseWithStream( Target).NegotiatedBuffer := NegotiatedBuffer;
  end;
end;

procedure TTaskAddressSpaceMemoryBaseWithStream.Process(MessageInfo: TOlcbMessage);
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
  DatagramReceive: TCANFrameParserDatagramReceive;
  StreamReceive: TCANFrameParserStreamBase;
  PIP: TOlcbStructureProtocolIdentification;
  Space: TOlcbStructureMemAddressSpace;
  Options: TOlcbStructureMemOptions;
  DatagramResultStart: DWord;
  i: Integer;
  Terminated: Boolean;
  Flags, AdditionalFlags: Byte;
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         // Ask for the protocols the node supports
         SendProtocolIdentificationProtocolMessage;
         Inc(FiState);
         Sending := False;
       end;
    1: begin
         // First see if the node even supports the Memory Configuration Protocol
         if IsProtocolIdentificationProcolReplyFromDestination(MessageInfo) then
         begin
           PIP := TOlcbStructureProtocolIdentification.Create;
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
         SendMemoryConfigurationOptionsByCANParsing;
         Inc(FiState);
         Sending := False;
       end;
    3: begin
         // Node received the request datagram
         if IsDatagramAckFromDestination(MessageInfo) then
         begin
           Sending := False;
           Inc(FiState);
         end;
       end;
    4: begin
         // Is the address space we are asking for supported?
         DatagramReceive := nil;
         if IsConfigMemoryOptionsReplyFromDestination(MessageInfo, DatagramReceive) then
         begin
           Options := TOlcbStructureMemOptions.Create;
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
         SendMemoryConfigurationSpaceInfoByCANParsing(AddressSpace);
         Sending := False;
         Inc(FiState);
       end;
    6: begin
        // Node received the datagram
         if IsDatagramAckFromDestination(MessageInfo) then
         begin
           Sending := False;
           Inc(FiState);
         end;
       end;
    7: begin
         // Receive detailed information about the memory space
         if IsConfigMemorySpaceInfoReplyFromDestination(MessageInfo, AddressSpace, DatagramReceive) then
         begin
           Space := TOlcbStructureMemAddressSpace.Create;
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
         LocalLoopTime := GetTickCount;
         CurrentSendSize := MaxAddress - CurrentAddress;                        // Streams can just say "read it all"
         Sending := True;
         Inc(FiState);
       end;
    9: begin
        // Ask for a read from the node
         SendMemoryConfigurationReadByCANParsing(AddressSpace, CurrentAddress, CurrentSendSize, ForceOptionalSpaceByte, True);
         Sending := False;
         Inc(FiState);
       end;
    10: begin
         // Node received the datagram
         if IsDatagramAckFromDestination(MessageInfo) then
         begin
           Sending := False;
           Inc(FiState);
         end;
       end;
    11: begin
          // Node sending frame of data
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
              Inc(FiState);
              // Not much to do here.  If no error then the node will start a Stream Initilization with us next
            end
          end
        end;
    12: begin
          if IsStreamInitializationRequest(MessageInfo) then
          begin
            NegotiatedBuffer := (TOpenLCBMessageHelper( MessageInfo).Data[2] shl 8) or TOpenLCBMessageHelper( MessageInfo).Data[3];
            SourceID := TOpenLCBMessageHelper( MessageInfo).Data[6];
            DestID := GenerateDestID;                                           // Accept any size the node wants
            Flags := 0;
            AdditionalFlags := 0;
            SendStreamInitReply(NegotiatedBuffer, Flags, AdditionalFlags, SourceID, DestID);
            Inc(FiState);
          end;
        end;
    13: begin
          // Get ready for Data
          if IsStreamSendFromDestination(MessageInfo, StreamReceive) then
          begin

            // here I create a TCANFrameParserStreamReceive and wait for the sending node to send me the data and the object to recreat it from CAN frames
          end;
        end;

    STATE_WRITE_START :
        begin
          if DataStream.Size > Space.AddressHi - Space.AddressLo then
            ErrorCode := ERROR_ADDRESS_SPACE_WRITE_LARGER_THAN_SPACE
          else
            SendMemoryConfigurationWriteByCANParsing(AddressSpace, CurrentAddress, Space.AddressHi - Space.AddressLo, ForceOptionalSpaceByte, DataStream);
          iState := STATE_DONE
        end;
    STATE_DONE : begin
       // Done
         FDone := True
       end;
  end;
end;

{ TAliasTaskContainerList }

function TAliasTaskContainerList.GetAliasTaskContainer(Index: Integer): TAliasTaskContainer;
begin
  Result := TAliasTaskContainer( Items[Index])
end;

procedure TAliasTaskContainerList.SetAliasTaskContainer(Index: Integer; AValue: TAliasTaskContainer);
begin
  Items[Index] := AValue
end;

function TAliasTaskContainerList.FindByAlias(AnAlias: Word): TAliasTaskContainer;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  while (Result = nil) and (i < Count) do
  begin
    if AliasTaskContainer[i].AliasID = AnAlias then
      Result := AliasTaskContainer[i];
    Inc(i)
  end;
end;

procedure TAliasTaskContainerList.RemoveByAlias(AnAlias: Word);
var
  AliasTask: TAliasTaskContainer;
begin
  AliasTask := FindByAlias(AnAlias);
  if AliasTask <> nil then
    Remove(AliasTask)
end;


{ TAliasTaskContainer }

constructor TAliasTaskContainer.Create(AnAliasID: Word);
begin
  FAliasID := AnAliasID;
  FAttemptCount := 0;
end;

destructor TAliasTaskContainer.Destroy;
begin
  inherited Destroy;
end;

{ TTaskAddressSpaceMemoryReadWithDatagram }

function TTaskAddressSpaceMemoryReadWithDatagram.Clone: TTaskOlcbBase;
begin
  Result := TTaskAddressSpaceMemoryReadWithDatagram.Create(SourceAlias, DestinationAlias, Sending, AddressSpace, UsingTerminator);
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

procedure TTransportLayerThread.DecomposeAndDispatchGridConnectString(ReceiveStr: AnsiString; Helper: TOpenLCBMessageHelper);
var
  CANLayerTask: TTaskCANLayer;
  EventTask: TTaskEvent;
  VerifiedNodeIDTask: TTaskVerifiedNodeID;
  TractionProtocolTask: TTaskTractionProtocol;
  InitializationCompleteTask: TTaskInitializationComplete;
  CompletedSendDatagram: TCANFrameParserDatagramSend;
  BufferDatagramReceive: TCANFrameParserDatagramReceive;
  CompletedSendStream: TCANFrameParserStreamSend;
  BufferStreamReceive: TCANFrameParserStreamBase;
begin
  ReceiveStr := Trim(ReceiveStr);
  if Helper.Decompose(ReceiveStr) then
  begin

    if EnableOPStackCallback then
    begin
      BufferRawMessage := ReceiveStr;         // NEED TO DECIDE IF WE BYBASS THE TASK CODE BELOW IF WE DO THIS>>>>>>
      Synchronize(@SyncOnOPStackCallback);
    end;

    if EnableReceiveMessages then                                         // *** Communicate back to the app the raw message string
    begin
      BufferRawMessage := ReceiveStr;
      Synchronize(@SyncOnReceiveMessage);
    end;

    if AliasList.FindByAlias(Helper.SourceAliasID) = nil then             // Any message from a node is on our segement
      AliasList.Add(TAliasTaskContainer.Create(Helper.SourceAliasID));

    if IsDatagramMTI(Helper.MTI, True) then                               // *** Test for a Datagram message that came in ***
    begin
      CompletedSendDatagram := CANFrameParserDatagramSendManager.ProcessReceive(Helper);  // Sending Datagrams are expecting replies from their destination Nodes
      if Assigned(CompletedSendDatagram) then
      begin
        OlcbTaskManager.ProcessReceiving(CompletedSendDatagram);                // Give the Task subsystem a crack at knowning about the sent datagram
        FreeAndNil(CompletedSendDatagram)
      end else
      begin
        BufferDatagramReceive := CANFrameParserDatagramReceiveManager.Process(Helper);  // DatagramReceive object is created and given to the thread
        if Assigned(BufferDatagramReceive) then
        begin
          OlcbTaskManager.ProcessReceiving(BufferDatagramReceive);              // Give the Task subsystem a crack at knowning about the received datagram
          FreeAndNil(BufferDatagramReceive)
        end;
      end;
    end else                                                                    // *** Test for a Datagram message that came in ***
    if IsStreamMTI(Helper.MTI, False) then                                      // Only send the Stream_Send message to the parser as that is all that needs parsing for Streams
    begin
      CompletedSendStream := CANFrameParserStreamSendManager.ProcessReceive(Helper); // Sending Streams are expecting replies from their destination Nodes, not sure if this is needed yet.......
      if Assigned(CompletedSendStream) then
      begin
        OlcbTaskManager.ProcessReceiving(CompletedSendStream);                  // Give the Task subsystem a crack at knowning about the sent Stream
        FreeAndNil(CompletedSendStream)
      end else
      begin
        BufferStreamReceive := CANFrameParserStreamReceiveManager.ProcessReceive(Helper);    // Stream object is created and given to the thread
        if Assigned(BufferStreamReceive) then
        begin
          OlcbTaskManager.ProcessReceiving(BufferStreamReceive);                // Give the Task subsystem a crack at knowning about the received Stream
          FreeAndNil(BufferStreamReceive)
        end;
      end;
    end else
      OlcbTaskManager.ProcessReceiving(Helper);

    if Helper.Layer = ol_CAN then
    begin
      case Helper.MTI of
        MTI_AMR :
          begin
            AliasList.RemoveByAlias(Helper.DestinationAliasID);
          end;
      end;
      CANLayerTask := TTaskCANLayer.Create(Helper.DestinationAliasID, Helper.SourceAliasID, True);
      CANLayerTask.OnBeforeDestroy := OnBeforeDestroyTask;
      Helper.CopyTo(CANLayerTask.MessageHelper);
      AddTask(CANLayerTask, False);
    end;

    case Helper.MTI of
      MTI_INITIALIZATION_COMPLETE :
        begin
          InitializationCompleteTask := TTaskInitializationComplete.Create(Helper.DestinationAliasID, Helper.SourceAliasID, True);
          InitializationCompleteTask.OnBeforeDestroy := OnBeforeDestroyTask;
          Helper.CopyTo(InitializationCompleteTask.MessageHelper);
          AddTask(InitializationCompleteTask, False);
        end;
      MTI_VERIFIED_NODE_ID_NUMBER :
        begin
          VerifiedNodeIDTask := TTaskVerifiedNodeID.Create(Helper.DestinationAliasID, Helper.SourceAliasID, True);
          VerifiedNodeIDTask.OnBeforeDestroy := OnBeforeDestroyTask;
          Helper.CopyTo(VerifiedNodeIDTask.MessageHelper);
          AddTask(VerifiedNodeIDTask, False);
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
          EventTask := TTaskEvent.Create(Helper.DestinationAliasID, Helper.SourceAliasID, True);
          EventTask.OnBeforeDestroy := OnBeforeDestroyTask;
          Helper.CopyTo(EventTask.MessageHelper);
          AddTask(EventTask, False);
        end;
      MTI_TRACTION_PROTOCOL :
        begin
          TractionProtocolTask := TTaskTractionProtocol.Create(Helper.DestinationAliasID, Helper.SourceAliasID, True);
          TractionProtocolTask.OnBeforeDestroy := OnBeforeDestroyTask;
          Helper.CopyTo(TractionProtocolTask.MessageHelper);
          AddTask(TractionProtocolTask, False);
        end;
    end;
  end;
end;

procedure TTransportLayerThread.ExecuteBegin;
begin
  FRunning := True;
  ConnectionState := csConnecting;
  Synchronize(@SyncOnConnectionState);
end;

procedure TTransportLayerThread.ExecuteEnd;
begin
  ConnectionState := csDisconnected;
  Synchronize(@SyncOnConnectionState);
  if Assigned(OnBeforeDestroy) then
    OnBeforeDestroy(Self);
  FRunning := False;
  FTerminateComplete := True;
end;

procedure TTransportLayerThread.SyncOnErrorMessage;
begin
  if Assigned(OnErrorMessage) then
    OnErrorMessage(Self, BufferRawMessage)
end;

procedure TTransportLayerThread.SyncOnOPStackCallback;
begin
  if Assigned(FOnOPstackCallback) then
    OnOPStackCallback(@BufferRawMessage[1]);
end;

procedure TTransportLayerThread.SyncOnStatus;
begin
  if Assigned(OnStatus) then
    OnStatus(Self, StatusReason, StatusValue);
end;

procedure TTransportLayerThread.SyncOnReceiveMessage;
begin
  if Assigned(OnReceiveMessage) then
    OnReceiveMessage(Self, BufferRawMessage)
end;

procedure TTransportLayerThread.SyncOnSendMessage;
begin
  if Assigned(OnSendMessage) then
    OnSendMessage(Self, BufferRawMessage)
end;

constructor TTransportLayerThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FThreadListSendStrings := TThreadList.Create;
  FCANFrameParserDatagramReceiveManager := TCANFrameParserDatagramReceiveManager.Create(Self);
  FCANFrameParserDatagramSendManager := TCANFrameParserDatagramSendManager.Create(Self);
  FCANFrameParserStreamReceiveManager := TCANFrameParserStreamReceiveManager.Create(Self);
  FCANFrameParserStreamSendManager := TCANFrameParserStreamSendManager.Create(Self);
  FOlcbTaskManager := TOlcbTaskEngine.Create(Self);
  FAliasList := TAliasTaskContainerList.Create;
  FTerminateComplete := False;
  FEnableReceiveMessages := True;
  FEnableOPStackCallback := False;
  FEnableSendMessages := True;
  FOnErrorMessage := nil;
  FOnReceiveMessage := nil;
  FOnSendMessage := nil;
  FOnBeforeDestroyTask := nil;
  FOnBeforeDestroy := nil;
  FOnConnectionStateChange := nil;
  FOnStatus := nil;
  FOnOPstackCallback := nil;
  FGridConnectReceiveState := 0;
  FReceiveGridConnectBufferIndex := 0;
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
  CANFrameParserDatagramReceiveManager.Clear;
  FreeAndNil(FCANFrameParserDatagramReceiveManager);
  CANFrameParserDatagramSendManager.Clear;
  FreeAndNil(FCANFrameParserDatagramSendManager);
  CANFrameParserStreamReceiveManager.Clear;
  FreeAndNil(FCANFrameParserStreamReceiveManager);
  CANFrameParserStreamSendManager.Clear;
  FreeAndNil(FCANFrameParserStreamSendManager);
  FreeAndNil(FAliasList);
  inherited Destroy;
end;


procedure TTransportLayerThread.InternalAdd(Msg: AnsiString);
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

procedure TTransportLayerThread.InternalAddDatagramToSendByCANParsing(Datagram: TCANFrameParserDatagramSend);
var
  List: TList;
begin
  if AliasList.FindByAlias(Datagram.DestinationAlias) <> nil then
  begin
    List := CANFrameParserDatagramSendManager.Datagrams.LockList;
    try
      List.Add(Datagram);
      if List.Count > CANFrameParserDatagramSendManager.MaxCount then
        CANFrameParserDatagramSendManager.MaxCount := List.Count;
    finally
      CANFrameParserDatagramSendManager.Datagrams.UnLockList
    end;
  end else
    Datagram.Free
end;

function TTransportLayerThread.IsValidHexChar(AChar: Char): Boolean;
begin
  Result := ((AChar >= '0') and (AChar <= '9')) or ((AChar >= 'A') and (AChar <= 'F')) or ((AChar >= 'a') and (AChar <= 'f'))
end;

function TTransportLayerThread.AddTask(NewTask: TTaskOlcbBase; CopyTask: Boolean): Boolean;
var
  List: TList;
  CloneTask: TTaskOlcbBase;
begin
  Result := False;
  if (NewTask.DestinationAlias = 0) or (AliasList.FindByAlias(NewTask.DestinationAlias) <> nil) then
  begin
    List := OlcbTaskManager.TaskList.LockList;
    try
      if CopyTask then
      begin
        CloneTask := NewTask.Clone;
        NewTask.CopyTo(CloneTask);
      end else
        CloneTask := NewTask;

      CloneTask.FTransportLayerThread := Self;
      List.Add(CloneTask);
      if List.Count > OlcbTaskManager.MaxCount then
        OlcbTaskManager.MaxCount := List.Count;
    finally
      OlcbTaskManager.TaskList.UnlockList;
      Result := True;
    end;
  end
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
      if (TTaskOlcbBase( List[i]).RemoveKey = RemoveKey) or (TTaskOlcbBase( List[i]).RemoveKey = 0) then
      begin
        TTaskOlcbBase( List[i]).ForceTermination := True;
        TTaskOlcbBase( List[i]).Free;
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
        if (TTaskOlcbBase( List[i]).RemoveKey = RemoveKey) then
        begin
          if TTaskOlcbBase( List[i]).Done or not TTaskOlcbBase( List[i]).HasStarted then
          begin
            TTaskOlcbBase( List[i]).Free;
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

procedure TTransportLayerThread.ShowErrorMessageAndTerminate(Message: string);
begin
  BufferRawMessage := Message;
  Synchronize(@SyncOnErrorMessage);
  Terminate;
end;

procedure TTransportLayerThread.SyncOnConnectionState;
begin
  if Assigned(OnConnectionStateChange) then
    OnConnectionStateChange(Self, ConnectionState)
end;


{ TOlcbStructureSNIP }

constructor TOlcbStructureSNIP.Create;
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

procedure TOlcbStructureSNIP.CopyTo(Target: TOlcbStructureHelperBase);
var
  X: TOlcbStructureSNIP;
begin
  if Target is TOlcbStructureSNIP then
  begin
    X := TOlcbStructureSNIP( Target);
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

{ TOlcbStructureProtocolIdentification }

function TOlcbStructureProtocolIdentification.GetAbbreviatedCDIProtocol: Boolean;
begin
  Result := Mask and PIP_ABBREVIATED_CDI = PIP_ABBREVIATED_CDI;
end;

function TOlcbStructureProtocolIdentification.GetConfigDescriptionInfoProtocol: Boolean;
begin
  Result := Mask and PIP_CDI = PIP_CDI;
end;

function TOlcbStructureProtocolIdentification.GetDatagramProtocol: Boolean;
begin
  Result := Mask and PIP_DATAGRAM = PIP_DATAGRAM;
end;

function TOlcbStructureProtocolIdentification.GetFunctionStateInformationProtocol: Boolean;
begin
  Result := Mask and PIP_FSI = PIP_FSI
end;

function TOlcbStructureProtocolIdentification.GetDisplayProtocol: Boolean;
begin
  Result := Mask and PIP_DISPLAY = PIP_DISPLAY;
end;

function TOlcbStructureProtocolIdentification.GetEventExchangeProtocol: Boolean;
begin
  Result := Mask and PIP_EVENT_EXCHANGE = PIP_EVENT_EXCHANGE;
end;

function TOlcbStructureProtocolIdentification.GetFunctionDescriptionInfoProtocol: Boolean;
begin
  Result := Mask and PIP_FDI = PIP_FDI;
end;

function TOlcbStructureProtocolIdentification.GetIdentificationProtocol: Boolean;
begin
  Result := Mask and PIP_IDENTIFCIATION = PIP_IDENTIFCIATION;
end;

function TOlcbStructureProtocolIdentification.GetMemoryConfigProtocol: Boolean;
begin
  Result := Mask and PIP_MEMORY_CONFIG = PIP_MEMORY_CONFIG;
end;

function TOlcbStructureProtocolIdentification.GetRemoteButtonProtocol: Boolean;
begin
  Result := Mask and PIP_REMOTE_BUTTON = PIP_REMOTE_BUTTON;
end;

function TOlcbStructureProtocolIdentification.GetReservationProtocol: Boolean;
begin
  Result := Mask and PIP_RESERVATION = PIP_RESERVATION;
end;

function TOlcbStructureProtocolIdentification.GetSimpleNodeInfoProtocol: Boolean;
begin
  Result := Mask and PIP_SIMPLE_NODE_ID = PIP_SIMPLE_NODE_ID;
end;

function TOlcbStructureProtocolIdentification.GetSimpleProtocol: Boolean;
begin
  Result := Mask and PIP_PIP = PIP_PIP;
end;

function TOlcbStructureProtocolIdentification.GetStreamProtocol: Boolean;
begin
  Result := Mask and PIP_STREAM = PIP_STREAM;
end;

function TOlcbStructureProtocolIdentification.GetTeachingLearningConfigProtocol: Boolean;
begin
  Result := Mask and PIP_TEACH_LEARN = PIP_TEACH_LEARN;
end;

function TOlcbStructureProtocolIdentification.GetTractionControlProtocol: Boolean;
begin
  Result := Mask and PIP_TRACTION = PIP_TRACTION;
end;

procedure TOlcbStructureProtocolIdentification.CopyTo(Target: TOlcbStructureHelperBase);
var
  X: TOlcbStructureProtocolIdentification;
begin
  if Target is TOlcbStructureProtocolIdentification then
  begin
    X := TOlcbStructureProtocolIdentification( Target);
    X.Mask := Mask;
  end;
end;

procedure TOlcbStructureProtocolIdentification.LoadByMessage(AHelper: TOpenLCBMessageHelper);
begin
  FMask := AHelper.ExtractDataBytesAsInt(2, 7);
end;

{ TOlcbStructureMemOptions }

function TOlcbStructureMemOptions.GetReadFromMfgACDI: Boolean;
begin
  Result := OperationMask and MCO_ACDI_USER_READS = MCO_ACDI_USER_READS
end;

function TOlcbStructureMemOptions.GetReadFromUserACDI: Boolean;
begin
  Result := OperationMask and MCO_ACDI_MFG_READS = MCO_ACDI_MFG_READS
end;

function TOlcbStructureMemOptions.GetUnAlignedReads: Boolean;
begin
  Result := OperationMask and MCO_UNALIGNED_READS = MCO_UNALIGNED_READS
end;

function TOlcbStructureMemOptions.GetUnAlignedWrites: Boolean;
begin
  Result := OperationMask and MCO_UNALIGNED_WRITES = MCO_UNALIGNED_WRITES
end;

function TOlcbStructureMemOptions.GetWrite64Bytes: Boolean;
begin
  Result := WriteLengthMask and MCWL_64_BYTE = MCWL_64_BYTE
end;

function TOlcbStructureMemOptions.GetWriteArbitraryBytes: Boolean;
begin
  Result := WriteLengthMask and MCWL_ARBITRARY_BYTE = MCWL_ARBITRARY_BYTE
end;

function TOlcbStructureMemOptions.GetWriteFourBytes: Boolean;
begin
  Result := WriteLengthMask and MCWL_FOUR_BYTE = MCWL_FOUR_BYTE
end;

function TOlcbStructureMemOptions.GetWriteOneByte: Boolean;
begin
  Result := WriteLengthMask and MCWL_ONE_BYTE = MCWL_ONE_BYTE
end;

function TOlcbStructureMemOptions.GetWriteStreamBytes: Boolean;
begin
  Result := WriteLengthMask and MCWL_STREAM_WRITE_SUPPORTED = MCWL_STREAM_WRITE_SUPPORTED;
end;

function TOlcbStructureMemOptions.GetWriteToUserACDI: Boolean;
begin
   Result := OperationMask and MCO_ACDI_USER_WRITES  = MCO_ACDI_USER_WRITES;
end;

function TOlcbStructureMemOptions.GetWriteTwoBytes: Boolean;
begin
  Result := WriteLengthMask and MCWL_TWO_BYTE = MCWL_TWO_BYTE
end;

function TOlcbStructureMemOptions.GetWriteUnderMask: Boolean;
begin
  Result := OperationMask and MCO_WRITE_UNDER_MASK = MCO_WRITE_UNDER_MASK
end;

constructor TOlcbStructureMemOptions.Create;
begin
  FAddressSpaceHi := 0;
  FAddressSpaceLo := 0;
  FDescription := '';
  FOperationMask := 0;
  FWriteLengthMask := 0;
end;

procedure TOlcbStructureMemOptions.CopyTo(Target: TOlcbStructureHelperBase);
var
  X: TOlcbStructureMemOptions;
begin
  if Target is TOlcbStructureMemOptions then
  begin
    X := TOlcbStructureMemOptions( Target);
    X.FAddressSpaceHi := AddressSpaceHi;
    X.FAddressSpaceLo := AddressSpaceLo;
    X.FDescription := Description;
    X.FOperationMask := OperationMask;
    X.FWriteLengthMask := WriteLengthMask;
  end;
end;

procedure TOlcbStructureMemOptions.LoadFromDatagram(Datagram: TCANFrameParserDatagramReceive);
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

{ TOlcbStructureMemConfig }

function TOlcbStructureMemConfig.GetAddressSpace(Index: Integer): TOlcbStructureMemAddressSpace;
begin
  Result := nil;
  if (Index > -1) and (Index < AddressSpaceList.Count) then
    Result := TOlcbStructureMemAddressSpace( AddressSpaceList[Index])
end;

function TOlcbStructureMemConfig.GetAddressCount: Integer;
begin
  Result := AddressSpaceList.Count
end;

procedure TOlcbStructureMemConfig.SetAddressSpace(Index: Integer; AValue: TOlcbStructureMemAddressSpace);
begin
  if (Index > -1) and (Index < AddressSpaceList.Count) then
    AddressSpaceList[Index] := AValue                      // We don't free it
end;

procedure TOlcbStructureMemConfig.Clear;
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

constructor TOlcbStructureMemConfig.Create;
begin
  inherited Create;
  FAddressSpaceList := TList.Create;
  FOptions := TOlcbStructureMemOptions.Create;
end;

destructor TOlcbStructureMemConfig.Destroy;
begin
  Clear;
  FreeAndNil(FAddressSpaceList);
  FreeAndNil(FOptions);
  inherited Destroy;
end;

function TOlcbStructureMemConfig.AddAddressSpace: TOlcbStructureMemAddressSpace;
begin
  Result := TOlcbStructureMemAddressSpace.Create;
  AddressSpaceList.Add(Result);
end;

function TOlcbStructureMemConfig.AddAddressSpaceByDatagram(Datagram: TCANFrameParserDatagramReceive): TOlcbStructureMemAddressSpace;
begin
  Result := FindAddressSpaceBySpaceID(Datagram.RawDatagram[2]);
  if not Assigned(Result) then
  begin
    Result := TOlcbStructureMemAddressSpace.Create;
    AddressSpaceList.Add(Result);
  end;
  Result.LoadByDatagram(Datagram);
end;

procedure TOlcbStructureMemConfig.CopyTo(Target: TOlcbStructureHelperBase);
var
  X: TOlcbStructureMemConfig;
  i: Integer;
  NewSpace: TOlcbStructureMemAddressSpace;
begin
  if Target is TOlcbStructureMemConfig then
  begin
    X := TOlcbStructureMemConfig( Target);
    X.Clear;
    for i := 0 to AddressSpaceCount - 1 do
    begin
      NewSpace := TOlcbStructureMemAddressSpace.Create;
      AddressSpace[i].CopyTo(NewSpace);
      X.AddressSpaceList.Add(NewSpace);
    end;
  end;
  Options.CopyTo(X.Options);
end;

function TOlcbStructureMemConfig.FindAddressSpaceBySpaceID(AnAddress: Byte): TOlcbStructureMemAddressSpace;
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


{ TOlcbStructureMemAddressSpace }

function TOlcbStructureMemAddressSpace.GetAddressSize: DWord;
begin
  Result := AddressHi-AddressLo
end;

function TOlcbStructureMemAddressSpace.GetSpaceAsHex: string;
begin
  Result := IntToHex(FSpace, 2);
end;

constructor TOlcbStructureMemAddressSpace.Create;
begin
  FAddressHi := 0;
  FAddressLo := 0;
  FAddressLoImpliedZero := True;
  FDescription := '';
  FIsReadOnly := False;
  FSpace := 0;
  FIsPresent := False;
end;

procedure TOlcbStructureMemAddressSpace.CopyTo(Target: TOlcbStructureHelperBase);
var
  X: TOlcbStructureMemAddressSpace;
begin
  if Target is TOlcbStructureMemAddressSpace then
  begin
    X := TOlcbStructureMemAddressSpace( Target);
    X.FAddressHi := AddressHi;
    X.FAddressLo := AddressLo;
    X.FAddressLoImpliedZero := AddressLoImpliedZero;
    X.FDescription := Description;
    X.FIsPresent := IsPresent;
    X.FIsReadOnly := IsReadOnly;
    X.FSpace := Space;
  end;
end;

procedure TOlcbStructureMemAddressSpace.LoadByDatagram(ADatagram: TCANFrameParserDatagramReceive);
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

{ TCANFrameParserDatagramSendManager }

procedure TCANFrameParserDatagramSendManager.TimerTick(Sender: TObject);
var
  SendDatagram: TCANFrameParserDatagramSend;
  i: Integer;
  List: TList;
  AbandonList: TList;
begin
  List := Datagrams.LockList;
  AbandonList := AbandonDatagrams.LockList;
  try
  for i := List.Count - 1 downto 0 do     // May remove the item so need to go from the top down
  begin
    SendDatagram := TCANFrameParserDatagramSend( List[i]);
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


constructor TCANFrameParserDatagramSendManager.Create(AnOwner: TTransportLayerThread);
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

destructor TCANFrameParserDatagramSendManager.Destroy;
begin
  Clear;
  FreeAndNil(FDatagrams);
  ClearAbandon;
  FreeAndNil(FAbandonDatagrams);
  FreeAndNil(FTimer);
  inherited Destroy;
end;

procedure TCANFrameParserDatagramSendManager.Clear;
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

procedure TCANFrameParserDatagramSendManager.ClearAbandon;
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

function TCANFrameParserDatagramSendManager.ProcessReceive(AHelper: TOpenLCBMessageHelper): TCANFrameParserDatagramSend;
var
  Datagram: TCANFrameParserDatagramSend;
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
      Datagram := TCANFrameParserDatagramSend( List[i]);
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

procedure TCANFrameParserDatagramSendManager.ProcessSend;
var
  Datagram: TCANFrameParserDatagramSend;
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
      Datagram := TCANFrameParserDatagramSend( List[i]);
      Done := Datagram.ProcessSend(Owner);
      Inc(i);
    end;
  finally
    Datagrams.UnlockList;
  end;
end;

{ TCANFrameParserDatagramReceiveManager }

function TCANFrameParserDatagramReceiveManager.FindInProcessDatagramAndCheckForAbandonDatagrams(AHelper: TOpenLCBMessageHelper): TCANFrameParserDatagramReceive;
//
// Searches an in process datagram interaction between the nodes in the message
//
var
  i: Integer;
  List: TList;
  Datagram: TCANFrameParserDatagramReceive;
begin
  Result := nil;
  List := Datagrams.LockList;
  try
    for i := List.Count-1 downto 0 do    // So we can delete abandon items from the top of the list
    begin
      Datagram := TCANFrameParserDatagramReceive( List[i]);
      if not Datagram.Full and (Datagram.DestinationAlias = AHelper.SourceAliasID) and (Datagram.SourceAlias = AHelper.DestinationAliasID) then
        Result := Datagram;

      if Datagram.CreateTime + GlobalSettings.General.DatagramWaitTime > GetTickCount then
      begin
        // Abandon Datagram
    //    Datagram.Free;
   //     List.Delete(i);
      end;
    end;
  finally
    Datagrams.UnlockList;
  end;
end;

constructor TCANFrameParserDatagramReceiveManager.Create(AnOwner: TTransportLayerThread);
begin
  inherited Create;
  FOwner := AnOwner;
  FDatagrams := TThreadList.Create;
  FMaxCount := 0;
end;

destructor TCANFrameParserDatagramReceiveManager.Destroy;
begin
  Clear;
  FreeAndNil(FDatagrams);
  inherited Destroy;
end;

procedure TCANFrameParserDatagramReceiveManager.Clear;
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

function TCANFrameParserDatagramReceiveManager.Process(AHelper: TOpenLCBMessageHelper): TCANFrameParserDatagramReceive;
var
  TestDatagram: TCANFrameParserDatagramReceive;
  List: TList;
begin
  Result := nil;
  if IsDatagramMTI(AHelper.MTI, False) then
  begin
    TestDatagram := FindInProcessDatagramAndCheckForAbandonDatagrams(AHelper);
    if not Assigned(TestDatagram) then
    begin
      TestDatagram := TCANFrameParserDatagramReceive.Create(Owner.SourceAlias, AHelper.SourceAliasID);  // Create a new receiving Datagram object for source alias of the message to us
      Datagrams.Add(TestDatagram);
      List := Datagrams.LockList;
      if List.Count > MaxCount then
        MaxCount := List.Count;
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

{ TCANFrameParserDatagramSend }

// *****************************************************************************
// Pulls the next 1..8 byte(s) from the Stream and puts them in the CAN Array to send, it always assumes the Stream Position of were to start
// in the Stream
//   Offset: [in] Where in the CAN array to use as the start of the transfer (may have some header bytes that need to be skipped)
//   ByteArray:  [out] CAN ByteArray that will be sent on the wire
//   Count    :  [out] The number of bytes that were transfered to the Byte Array
// *****************************************************************************
procedure TCANFrameParserDatagramSend.StreamBytesToByteArray(Offset: Byte; var ByteArray: TCANByteArray; var Count: Byte);
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
constructor TCANFrameParserDatagramSend.Create;
var
  i: Integer;
begin
  FAbandonTime := 0;
  FBlockStartPos := 0;
  for i := 0 to 7 do
   FDataBytesSent[i] := 0;
  FDataBytesSentLen := 0;
  FDestinationAlias := 0;
  FEmpty := True;
  FErrorCode := 0;
  FMTI := 0;
  FNewStartFrame := False;
  for i := 0 to 7 do
   FProtocolHeader[i] := 0;
  FProtocolHeaderLen := 0;
  FRetryCount := 0;
  FBlockByteCount := 0;
  FSourceAlias := 0;
  FWaitingForACK := False;
  FStream := TMemoryStream.Create;
  FLocalHelper := TOpenLCBMessageHelper.Create;
end;

// *****************************************************************************
// Destroys the datagram object
// *****************************************************************************
destructor TCANFrameParserDatagramSend.Destroy;
begin
  FreeAndNil(FStream);
  FreeAndNil(FLocalHelper);
  inherited;
end;

// *****************************************************************************
// Loads and initalizes the object to start sending the datagram
// *****************************************************************************
procedure TCANFrameParserDatagramSend.Initialize(AStream: TStream; AProtocolHeader: TCANByteArray; AProtocolHeaderLen: Byte; ASourceAlias, ADestinationAlias: Word);
begin
  Stream.Size := 0;
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
function TCANFrameParserDatagramSend.ProcessSend(TransportLayerThread: TTransportLayerThread): Boolean;
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
      TransportLayerThread.InternalAdd(LocalHelper.Encode);
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
      TransportLayerThread.InternalAdd(LocalHelper.Encode);
    end;
    Result := True;
  end;
end;

function TCANFrameParserDatagramSend.ProcessReceive(AHelper: TOpenLCBMessageHelper): Boolean;
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

{ TCANFrameParserDatagramReceive }


// *****************************************************************************
// Clears the structures and fields of the object
// *****************************************************************************
procedure TCANFrameParserDatagramReceive.Clear;
var
  i: Integer;
begin
  FCurrentPos := 0;
  FFull := False;
  for i := 0 to MAX_DATAGRAM_LENGTH - 1 do
    RawDatagram[i] := 0;
end;

procedure TCANFrameParserDatagramReceive.SendACK(TransportLayerThread: TTransportLayerThread);
begin
  LocalHelper.Load(ol_OpenLCB, MTI_DATAGRAM_OK_REPLY, SourceAlias, DestinationAlias, 2, 0, 0, 0, 0, 0, 0, 0, 0);
  TransportLayerThread.InternalAdd(LocalHelper.Encode);
end;

constructor TCANFrameParserDatagramReceive.Create(ASourceAlias, ADestinationAlias: Word);
begin
  inherited Create;
  FLocalHelper := TOpenLCBMessageHelper.Create;
  FDestinationAlias := ADestinationAlias;
  FSourceAlias := ASourceAlias;
  FCreateTime := GetTickCount;
end;

destructor TCANFrameParserDatagramReceive.Destroy;
begin
  FreeAndNil(FLocalHelper);
  inherited;
end;

// *****************************************************************************
// Helper to concat up to 4 bytes in the Datagram Data Array into an integer/QWord
//  StartByteIndex: The first byte to use in the result (most significant 8 bits of integer/QWord)
//  EndByteIndex  : The last byte to use in the result (least significant 8 bits of integer/QWord)
// *****************************************************************************
function TCANFrameParserDatagramReceive.ExtractDataBytesAsInt(StartByteIndex, EndByteIndex: Integer): QWord;
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
function TCANFrameParserDatagramReceive.ExtractDataBytesAsString(StartIndex, Count: Integer): String;
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
procedure TCANFrameParserDatagramReceive.CopyToStream(Stream: TStream; StartIndex, Count: Integer);
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
procedure TCANFrameParserDatagramReceive.Process(AHelper: TOpenLCBMessageHelper; TransportLayerThread: TTransportLayerThread);
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

{ TTaskOlcbBase }

function TTaskOlcbBase.SpaceToCommandByteEncoding(ASpace: Byte): Byte;
begin
  case ASpace of
    MSI_CDI     : Result := MCP_CDI;
    MSI_ALL     : Result := MCP_ALL;
    MSI_CONFIG  : Result := MCP_CONFIGURATION
  else
    Result := MCP_NONE
  end;
end;

procedure TTaskOlcbBase.ExtractErrorInformation(DatagramReceive: TCANFrameParserDatagramReceive);
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

function TTaskOlcbBase.IsDatagramAckFromDestination(MessageInfo: TOlcbMessage): Boolean;
var
  DatagramSend: TCANFrameParserDatagramSend;
begin
  Result := False;
  if Assigned(MessageInfo) then
  begin
    if MessageInfo is TCANFrameParserDatagramSend then                                 // Wait for the ACK from the send
    begin
      DatagramSend := TCANFrameParserDatagramSend( MessageInfo);
      if DatagramSend.Empty and
         (DatagramSend.SourceAlias = SourceAlias) and
         (DatagramSend.DestinationAlias = DestinationAlias) then
      begin
        MessageWaitTimerReset;
        Result := True
      end;
    end;
  end;
end;

function TTaskOlcbBase.IsConfigMemorySpaceInfoReplyFromDestination(MessageInfo: TOlcbMessage; AnAddress: Byte; var DatagramReceive: TCANFrameParserDatagramReceive): Boolean;
begin
  Result := False;
  DatagramReceive := nil;
  if Assigned(MessageInfo) then
  begin
    if MessageInfo is TCANFrameParserDatagramReceive then
    begin
      DatagramReceive := TCANFrameParserDatagramReceive(MessageInfo);
      if (DatagramReceive.RawDatagram[0] and
          DATAGRAM_PROTOCOL_CONFIGURATION = DATAGRAM_PROTOCOL_CONFIGURATION) and
         (DatagramReceive.RawDatagram[1] and $FE = MCP_OP_GET_ADD_SPACE_INFO_REPLY) and
         (DatagramReceive.RawDatagram[2] = AnAddress) and
         (DatagramReceive.SourceAlias = SourceAlias) and
         (DatagramReceive.DestinationAlias = DestinationAlias) then
      begin
        MessageWaitTimerReset;
        Result := True
      end;
    end;
  end;
end;

function TTaskOlcbBase.IsConfigMemoryOptionsReplyFromDestination(MessageInfo: TOlcbMessage; var DatagramReceive: TCANFrameParserDatagramReceive): Boolean;
begin
  Result := False;
  DatagramReceive := nil;
  if Assigned(MessageInfo) then
  begin
    if MessageInfo is TCANFrameParserDatagramReceive then
    begin
      DatagramReceive := TCANFrameParserDatagramReceive(MessageInfo);
      if  (DatagramReceive.RawDatagram[0] and
           DATAGRAM_PROTOCOL_CONFIGURATION = DATAGRAM_PROTOCOL_CONFIGURATION) and
          (DatagramReceive.RawDatagram[1] and $FE = MCP_OP_GET_CONFIG_REPLY) and
          (DatagramReceive.SourceAlias = SourceAlias) and
          (DatagramReceive.DestinationAlias = DestinationAlias) then
      begin
        MessageWaitTimerReset;
        Result := True
      end;
    end;
  end;
end;

function TTaskOlcbBase.IsConfigMemoryReadReplyFromDestination(MessageInfo: TOlcbMessage; var DatagramReceive: TCANFrameParserDatagramReceive): Boolean;
begin
  Result := False;
  DatagramReceive := nil;
  if Assigned(MessageInfo) then
  begin
    if MessageInfo is TCANFrameParserDatagramReceive then
    begin
      DatagramReceive := TCANFrameParserDatagramReceive(MessageInfo);
      if  (DatagramReceive.RawDatagram[0] and
          DATAGRAM_PROTOCOL_CONFIGURATION = DATAGRAM_PROTOCOL_CONFIGURATION) and
          (DatagramReceive.RawDatagram[1] and MCP_READ_DATAGRAM_REPLY = MCP_READ_DATAGRAM_REPLY) and
          (DatagramReceive.SourceAlias = SourceAlias) and
          (DatagramReceive.DestinationAlias = DestinationAlias) then
      begin
        MessageWaitTimerReset;
        Result := True
      end;
    end;
  end;
end;

function TTaskOlcbBase.IsProtocolIdentificationProcolReplyFromDestination(MessageInfo: TOlcbMessage): Boolean;
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
      begin
        MessageWaitTimerReset;
        Result := True;
      end;
    end;
  end;
end;

function TTaskOlcbBase.IsStreamInitializationRequest(MessageInfo: TOlcbMessage): Boolean;
var
  Helper: TOpenLCBMessageHelper;
begin
  Result := False;
  if Assigned(MessageInfo) then
  begin
    if MessageInfo is TOpenLCBMessageHelper then
    begin
      Helper := TOpenLCBMessageHelper( MessageInfo);
      if (Helper.MTI = MTI_STREAM_INIT_REQUEST) and
         (Helper.SourceAliasID = DestinationAlias) and
         (Helper.DestinationAliasID = SourceAlias) then
      begin
        MessageWaitTimerReset;
        Result := True;
      end;
    end;
  end;
end;

function TTaskOlcbBase.IsStreamSendFromDestination(MessageInfo: TOlcbMessage; StreamReceive: TCANFrameParserStreamBase): Boolean;
begin
  Result := False
end;

function TTaskOlcbBase.IsSnipMessageReply(MessageInfo: TOlcbMessage): Boolean;
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
      begin
        MessageWaitTimerReset;
        Result := True;
      end;
    end;
  end;
end;

function TTaskOlcbBase.IsTractionFunctionQueryReply(MessageInfo: TOlcbMessage): Boolean;
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
      begin
        MessageWaitTimerReset;
        Result := True;
      end;
    end;
  end;

end;

function TTaskOlcbBase.IsTractionSpeedsQueryFirstFrameReply(MessageInfo: TOlcbMessage): Boolean;
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
         (Helper.FramingBits = $10) then
      begin
        MessageWaitTimerReset;
        Result := True;
      end;
    end;
  end;
end;

function TTaskOlcbBase.IsTractionSpeedsQuerySecondFrameReply(MessageInfo: TOlcbMessage): Boolean;
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
         (Helper.FramingBits = $20) then
      begin
        MessageWaitTimerReset;
        Result := True;
      end;
    end;
  end;

end;

function TTaskOlcbBase.IsTractionAttachDCCAddressReply(MessageInfo: TOlcbMessage): Boolean;
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
      begin
        MessageWaitTimerReset;
        Result := True;
      end;
    end;
  end;
end;

function TTaskOlcbBase.IsTractionDetachDCCAddressReply(MessageInfo: TOlcbMessage): Boolean;
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
      begin
        MessageWaitTimerReset;
        Result := True;
      end;
    end;
  end;
end;

function TTaskOlcbBase.IsTractionAttachNodeQueryReply(MessageInfo: TOlcbMessage): Boolean;
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
      begin
        MessageWaitTimerReset;
        Result := True;
      end;
    end;
  end;
end;

function TTaskOlcbBase.IsTractionDetachNodeQueryReply(MessageInfo: TOlcbMessage): Boolean;
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
       begin
         MessageWaitTimerReset;
         Result := True;
       end;
    end;
  end;
end;

function TTaskOlcbBase.IsTractionQueryProxyReply(MessageInfo: TOlcbMessage): Boolean;
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
      begin
        MessageWaitTimerReset;
        Result := True;
      end;
    end;
  end;
end;

function TTaskOlcbBase.IsTractionReserveProxyReply(MessageInfo: TOlcbMessage): Boolean;
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
      begin
        MessageWaitTimerReset;
        Result := True;
      end;
    end;
  end;
end;
procedure TTaskOlcbBase.MessageWaitTimerReset;
begin
  MessageWaitTimeStart := GetTickCount;
end;

function TTaskOlcbBase.MessageWaitTimerCheckTimeout: Boolean;
begin
  if HasStarted then
    Result := GetTickCount - MessageWaitTimeStart > GlobalSettings.General.MessageWaitTime
  else
    Result := True
end;

procedure TTaskOlcbBase.Process(MessageInfo: TOlcbMessage);
begin
  if not HasStarted then
  begin
    FHasStarted := True;
    MessageWaitTimerReset;
  end else
  begin
    if MessageWaitTimerCheckTimeout then
      ForceTermination := True;
  end;

  if ForceTermination then
  begin
    Sending := True;
    iState := STATE_DONE;
  end;
end;

procedure TTaskOlcbBase.SendIdentifyEventsMessage;
begin
  MessageHelper.Load(ol_OpenLCB, MTI_EVENTS_IDENTIFY, SourceAlias, 0, 0, 0, 0, 0, 0 ,0 ,0 ,0 ,0);
  TransportLayerThread.InternalAdd(MessageHelper.Encode);
  MessageWaitTimerReset;
end;

procedure TTaskOlcbBase.SendIdentifyEventsAddressedMessage;
begin
  MessageHelper.Load(ol_OpenLCB, MTI_EVENTS_IDENTIFY_DEST, SourceAlias, DestinationAlias, 2, 0, 0, 0, 0 ,0 ,0 ,0 ,0);
  TransportLayerThread.InternalAdd(MessageHelper.Encode);
  MessageWaitTimerReset;
end;

procedure TTaskOlcbBase.SendIdentifyConsumerMessage(Event: TEventID);
begin
  MessageHelper.Load(ol_OpenLCB, MTI_CONSUMER_IDENTIFY, SourceAlias, 0, 8, Event[0], Event[1], Event[2], Event[3], Event[4], Event[5], Event[6], Event[7]);
  TransportLayerThread.InternalAdd(MessageHelper.Encode);
  MessageWaitTimerReset;
end;

procedure TTaskOlcbBase.SendIdentifyProducerMessage(Event: TEventID);
begin
  MessageHelper.Load(ol_OpenLCB, MTI_PRODUCER_IDENDIFY, SourceAlias, 0, 8, Event[0], Event[1], Event[2], Event[3], Event[4], Event[5], Event[6], Event[7]);
  TransportLayerThread.InternalAdd(MessageHelper.Encode);
  MessageWaitTimerReset;
end;

procedure TTaskOlcbBase.SendMemoryConfigurationOptionsByCANParsing;
var
  DatagramSend: TCANFrameParserDatagramSend;
begin
  DatagramSend := TCANFrameParserDatagramSend.Create;
  DatagramSend.Initialize(nil, HEADER_MEMCONFIG_OPTIONS_REQUEST, 2, SourceAlias, DestinationAlias);
  TransportLayerThread.InternalAddDatagramToSendByCANParsing(DatagramSend);
  MessageWaitTimerReset;
end;

procedure TTaskOlcbBase.SendMemoryConfigurationSpaceInfoByCANParsing(Space: Byte);
var
  DatagramSend: TCANFrameParserDatagramSend;
  CANByteArray: TCANByteArray;
begin
  DatagramSend := TCANFrameParserDatagramSend.Create;
  CANByteArray := HEADER_MEMCONFIG_SPACE_INFO_UNKNOWN_REQUEST;
  CANByteArray[2] := Space;                                     // Set the address
  DatagramSend.Initialize(nil, CANByteArray, 3, SourceAlias, DestinationAlias);
  TransportLayerThread.InternalAddDatagramToSendByCANParsing(DatagramSend);
  MessageWaitTimerReset;
end;

procedure TTaskOlcbBase.SendMemoryConfigurationReadByCANParsing(Space: Byte; StartAddress: DWord; Count: DWord; ForceUseOfSpaceByte: Boolean; UseStream: Boolean);
var
  DatagramSendObject: TCANFrameParserDatagramSend;
  CANByteArray: TCANByteArray;
  HeaderByteCount: Byte;
  Stream: TMemoryStream;
begin
  Stream := nil;
  DatagramSendObject := TCANFrameParserDatagramSend.Create;
  CANByteArray[0] := DATAGRAM_PROTOCOL_CONFIGURATION;
  if UseStream then
    CANByteArray[1] := MCP_READ_STREAM
  else
    CANByteArray[1] := MCP_READ;
  CANByteArray[2] := (StartAddress shr 24) and $000000FF;
  CANByteArray[3] := (StartAddress shr 16) and $000000FF;
  CANByteArray[4] := (StartAddress shr 8) and $000000FF;
  CANByteArray[5] := StartAddress and $000000FF;
  if ForceUseOfSpaceByte or (Space < MSI_CONFIG) then
  begin
    CANByteArray[6] := Space;
    if UseStream then
    begin
      Stream := TMemoryStream.Create;
      Stream.WriteByte( Byte( (Count shr 24) and $000000FF));
      Stream.WriteByte( Byte( (Count shr 16) and $000000FF));
      Stream.WriteByte( Byte( (Count shr 8) and $000000FF));
      Stream.WriteByte( Byte( Count and $000000FF));
      HeaderByteCount := 7;
    end else
    begin
      CANByteArray[7] := Count;
      HeaderByteCount := 8
    end;
  end
  else begin
    CANByteArray[1] := CANByteArray[1] or SpaceToCommandByteEncoding(Space);
    if UseStream then
    begin
      Stream := TMemoryStream.Create;
      Stream.WriteByte( Byte( (Count shr 24) and $000000FF));
      Stream.WriteByte( Byte( (Count shr 16) and $000000FF));
      Stream.WriteByte( Byte( (Count shr 8) and $000000FF));
      Stream.WriteByte( Byte( Count and $000000FF));
      HeaderByteCount := 6;
    end else
    begin
      CANByteArray[6] := Count;
      HeaderByteCount := 7;
    end;
  end;
  DatagramSendObject.Initialize(Stream, CANByteArray, HeaderByteCount, SourceAlias, DestinationAlias);
  Stream.Free;
  TransportLayerThread.InternalAddDatagramToSendByCANParsing(DatagramSendObject);
  MessageWaitTimerReset;
end;

procedure TTaskOlcbBase.SendMemoryConfigurationWriteByCANParsing(Space: Byte; StartAddress: DWord; MaxAddressSize: DWORD; ForceUseOfSpaceByte: Boolean; AStream: TStream);
var
  DatagramSend: TCANFrameParserDatagramSend;
  CANByteArray: TCANByteArray;
  HeaderByteCount: Byte;
begin      //CAUTION SEE SendMemoryConfigurationRead before getting too cocky adding a "UseStreams" parameter
  DatagramSend := TCANFrameParserDatagramSend.Create;
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
  TransportLayerThread.InternalAddDatagramToSendByCANParsing(DatagramSend);
  MessageWaitTimerReset;
end;

procedure TTaskOlcbBase.SendProtocolIdentificationProtocolMessage;
begin
  MessageHelper.Load(ol_OpenLCB, MTI_PROTOCOL_SUPPORT_INQUIRY, SourceAlias, DestinationAlias, 2, 0, 0, 0, 0 ,0 ,0 ,0 ,0);
  TransportLayerThread.InternalAdd(MessageHelper.Encode);
  MessageWaitTimerReset;
end;

procedure TTaskOlcbBase.SendStreamInitReply(NegotiatedBuffer: Word; Flag,
  AdditionalFlags, SourceID, DestID: Byte);
begin
  MessageHelper.Load(ol_OpenLCB, MTI_STREAM_INIT_REPLY, SourceAlias, DestinationAlias, 8, 0, 0, Hi(NegotiatedBuffer), Lo(NegotiatedBuffer), Flag, AdditionalFlags, SourceID, DestID);
  TransportLayerThread.InternalAdd(MessageHelper.Encode);
end;

procedure TTaskOlcbBase.SendSnipMessage;
begin
  MessageHelper.Load(ol_OpenLCB, MTI_SIMPLE_NODE_INFO_REQUEST, SourceAlias, DestinationAlias, 2, 0, 0, 0, 0 ,0 ,0 ,0 ,0);
  TransportLayerThread.InternalAdd(MessageHelper.Encode);
  MessageWaitTimerReset;
end;

procedure TTaskOlcbBase.SendTractionAttachDccProxyMessage(Address: Word; Short: Boolean; SpeedStep: Byte);
begin
  if not Short then
    Address := Address or $C000;
  MessageHelper.Load(ol_OpenLCB, MTI_TRACTION_PROTOCOL, SourceAlias, DestinationAlias, 7, $00, $00, TRACTION_CONFIGURE_PROXY, TRACTION_ATTACH_DCC_ADDRESS, Hi(Address), Lo(Address), SpeedStep, $00);
  TransportLayerThread.InternalAdd(MessageHelper.Encode);
  MessageWaitTimerReset;
end;

procedure TTaskOlcbBase.SendTractionDetachDccAddressProxyMessage(Address: Word; Short: Boolean);
begin
  MessageHelper.Load(ol_OpenLCB, MTI_TRACTION_PROTOCOL, SourceAlias, DestinationAlias, 6, $00, $00, TRACTION_CONFIGURE_PROXY, TRACTION_DETACH_DCC_ADDRESS_REPLY, Hi(Address), Lo(Address), $00, $00);
  TransportLayerThread.InternalAdd(MessageHelper.Encode);
  MessageWaitTimerReset;
end;

procedure TTaskOlcbBase.SendTractionEStopMessage;
begin
  MessageHelper.Load(ol_OpenLCB, MTI_TRACTION_PROTOCOL, SourceAlias, DestinationAlias, 3, $00, $00, TRACTION_E_STOP, $00, $00, $00, $00, $00);
  TransportLayerThread.InternalAdd(MessageHelper.Encode);
  MessageWaitTimerReset;
end;

procedure TTaskOlcbBase.SendTractionFunction(FunctionAddress: DWord; Value: Word);
begin
  MessageHelper.Load(ol_OpenLCB, MTI_TRACTION_PROTOCOL, SourceAlias, DestinationAlias, 8, $00, $00, TRACTION_FUNCTION, (FunctionAddress shr 16) and $00FF, (FunctionAddress shr 8) and $00FF, FunctionAddress and $00FF, Hi(Value), Lo(Value));
  TransportLayerThread.InternalAdd(MessageHelper.Encode);
  MessageWaitTimerReset;
end;

procedure TTaskOlcbBase.SendTractionQueryFunction(FunctionAddress: DWord);
begin
  MessageHelper.Load(ol_OpenLCB, MTI_TRACTION_PROTOCOL, SourceAlias, DestinationAlias, 6, $00, $00, TRACTION_QUERY_FUNCTION, (FunctionAddress shr 16) and $00FF, (FunctionAddress shr 8) and $00FF, FunctionAddress and $00FF, 0, 0);
  TransportLayerThread.InternalAdd(MessageHelper.Encode);
  MessageWaitTimerReset;
end;

procedure TTaskOlcbBase.SendTractionQueryDccAddressProxyMessage(Address: Word; Short: Boolean);
begin
  if not Short then
    Address := Address or $C000;
  MessageHelper.Load(ol_OpenLCB, MTI_PRODUCER_IDENDIFY, SourceAlias, 0, 8, $06, $01, $00, $00, Hi(Address), Lo(Address), $03, $03);
  TransportLayerThread.InternalAdd(MessageHelper.Encode);
  MessageWaitTimerReset;
end;

procedure TTaskOlcbBase.SendTractionQuerySpeeds;
begin
  MessageHelper.Load(ol_OpenLCB, MTI_TRACTION_PROTOCOL, SourceAlias, DestinationAlias, 3, $00, $00, TRACTION_QUERY_SPEED, $00, $00, $00, $00, $00);
  TransportLayerThread.InternalAdd(MessageHelper.Encode);
  MessageWaitTimerReset;
end;

procedure TTaskOlcbBase.SendTractionQueryProxyMessage;
begin
  MessageHelper.Load(ol_OpenLCB, MTI_TRACTION_PROTOCOL, SourceAlias, DestinationAlias, 4, $00, $00, TRACTION_MANAGE_PROXY, TRACTION_MANAGE_PROXY_QUERY, $00, $00, $00, $00);
  TransportLayerThread.InternalAdd(MessageHelper.Encode);
  MessageWaitTimerReset;
end;

procedure TTaskOlcbBase.SendTractionReleaseProxyMessage;
begin
  MessageHelper.Load(ol_OpenLCB, MTI_TRACTION_PROTOCOL, SourceAlias, DestinationAlias, 4, $00, $00, TRACTION_MANAGE_PROXY, TRACTION_MANAGE_PROXY_RELEASE, $00, $00, $00, $00);
  TransportLayerThread.InternalAdd(MessageHelper.Encode);
  MessageWaitTimerReset;
end;

procedure TTaskOlcbBase.SendTractionReserveProxyMessage;
begin
  MessageHelper.Load(ol_OpenLCB, MTI_TRACTION_PROTOCOL, SourceAlias, DestinationAlias, 4, $00, $00, TRACTION_MANAGE_PROXY, TRACTION_MANAGE_PROXY_RESERVE, $00, $00, $00, $00);
  TransportLayerThread.InternalAdd(MessageHelper.Encode);
  MessageWaitTimerReset;
end;

procedure TTaskOlcbBase.SendTractionSpeedMessage(Speed: THalfFloat);
begin
  MessageHelper.Load(ol_OpenLCB, MTI_TRACTION_PROTOCOL, SourceAlias, DestinationAlias, 5, $00, $00, TRACTION_SPEED_DIR, Hi(Speed), Lo(Speed), $00, $00, $00);
  TransportLayerThread.InternalAdd(MessageHelper.Encode);
  MessageWaitTimerReset;
end;

procedure TTaskOlcbBase.SendVerifyNodeIDGlobalMessage;
begin
  MessageHelper.Load(ol_OpenLCB, MTI_VERIFY_NODE_ID_NUMBER_DEST, SourceAlias, DestinationAlias, 2, 0, 0, 0, 0 ,0 ,0 ,0 ,0);
  TransportLayerThread.InternalAdd(MessageHelper.Encode);
  MessageWaitTimerReset;
end;

procedure TTaskOlcbBase.SendVerifyNodeIDToDestinationMessage;
begin
  MessageHelper.Load(ol_OpenLCB, MTI_VERIFY_NODE_ID_NUMBER, SourceAlias, DestinationAlias, 0, 0, 0, 0, 0 ,0 ,0 ,0 ,0);
  TransportLayerThread.InternalAdd(MessageHelper.Encode);
  MessageWaitTimerReset;
end;

procedure TTaskOlcbBase.SyncOnBeforeTaskDestroy;
begin
  if Assigned(OnBeforeDestroy) then
    OnBeforeDestroy(Self)
end;

constructor TTaskOlcbBase.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean);
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
  FiLogState := 0;
  FMessageWaitTimeStart := 0;
  FOwnerControl := nil;
end;

destructor TTaskOlcbBase.Destroy;
begin
  Dec(TaskObjects);
  FreeAndNil(FMessageHelper);
  inherited Destroy;
end;

procedure TTaskOlcbBase.CopyTo(Target: TTaskOlcbBase);
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
  Target.OwnerControl := FOwnerControl;
end;

{ TTaskAddressSpaceMemoryReadRawWithDatagram }

function TTaskAddressSpaceMemoryReadRawWithDatagram.GetPayloadSize: Integer;
begin
  if ReadByteCount - Stream.Size < MAX_CONFIG_MEM_READWRITE_SIZE then
    Result := ReadByteCount - Stream.Size
  else
    Result := MAX_CONFIG_MEM_READWRITE_SIZE;
end;

constructor TTaskAddressSpaceMemoryReadRawWithDatagram.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddressSpace: Byte; AReadAddress, AReadByteCount: DWord; UseTerminatorChar: Boolean);
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

destructor TTaskAddressSpaceMemoryReadRawWithDatagram.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

function TTaskAddressSpaceMemoryReadRawWithDatagram.Clone: TTaskOlcbBase;
begin
  Result := TTaskAddressSpaceMemoryReadRawWithDatagram.Create(SourceAlias, DestinationAlias, Sending, AddressSpace, ReadAddress, ReadByteCount, UsingTerminator);
end;

procedure TTaskAddressSpaceMemoryReadRawWithDatagram.CopyTo(Target: TTaskOlcbBase);
begin
  inherited CopyTo(Target);
  (Target as TTaskAddressSpaceMemoryReadRawWithDatagram).FAddressSpace := FAddressSpace;
  (Target as TTaskAddressSpaceMemoryReadRawWithDatagram).FCurrentOffset := FCurrentOffset;
  (Target as TTaskAddressSpaceMemoryReadRawWithDatagram).FForceOptionalSpaceByte := FForceOptionalSpaceByte;
  (Target as TTaskAddressSpaceMemoryReadRawWithDatagram).FIncludeTerminator := FIncludeTerminator;
  (Target as TTaskAddressSpaceMemoryReadRawWithDatagram).FReadByteCount := FReadByteCount;
  (Target as TTaskAddressSpaceMemoryReadRawWithDatagram).FStream.CopyFrom(FStream, FStream.Size);
  (Target as TTaskAddressSpaceMemoryReadRawWithDatagram).FReadAddress := FReadAddress;
  (Target as TTaskAddressSpaceMemoryReadRawWithDatagram).FTerminator := FTerminator;
  (Target as TTaskAddressSpaceMemoryReadRawWithDatagram).UsingTerminator := FUsingTerminator;
end;

procedure TTaskAddressSpaceMemoryReadRawWithDatagram.Process(MessageInfo: TOlcbMessage);
var
  DatagramReceive: TCANFrameParserDatagramReceive;
  i: Integer;
  Finished: Boolean;
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
        // Ask for a read from the node
         SendMemoryConfigurationReadByCANParsing(AddressSpace, CurrentOffset, PayloadSize, ForceOptionalSpaceByte, False);
         Sending := False;
         Inc(FiState);
       end;
    1: begin
         // Node received the datagram
         if IsDatagramAckFromDestination(MessageInfo) then
         begin
           Sending := False;
           Inc(FiState);
         end;
       end;
    2: begin
          // Node sending frame of data
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
         FDone := True
       end;
  end;

end;

{ TTaskAddressSpaceMemoryWriteRawWithDatagram }

constructor TTaskAddressSpaceMemoryWriteRawWithDatagram.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddressSpace: Byte; AWriteAddress: DWord; AStream: TStream);
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

destructor TTaskAddressSpaceMemoryWriteRawWithDatagram.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

function TTaskAddressSpaceMemoryWriteRawWithDatagram.Clone: TTaskOlcbBase;
begin
  Result := TTaskAddressSpaceMemoryWriteRawWithDatagram.Create(SourceAlias, DestinationAlias, Sending, AddressSpace, WriteAddress, Stream);
end;

procedure TTaskAddressSpaceMemoryWriteRawWithDatagram.CopyTo(Target: TTaskOlcbBase);
begin
  inherited CopyTo(Target);
  (Target as TTaskAddressSpaceMemoryWriteRawWithDatagram).FAddressSpace := FAddressSpace;
  (Target as TTaskAddressSpaceMemoryWriteRawWithDatagram).ForceOptionalSpaceByte := FForceOptionalSpaceByte;
  (Target as TTaskAddressSpaceMemoryWriteRawWithDatagram).FStream.Position := 0;
  FStream.Position := 0;
  (Target as TTaskAddressSpaceMemoryWriteRawWithDatagram).FStream.CopyFrom(FStream, FStream.Size);
  (Target as TTaskAddressSpaceMemoryWriteRawWithDatagram).FWriteAddress := FWriteAddress;
end;

procedure TTaskAddressSpaceMemoryWriteRawWithDatagram.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  case iState of
    0 : begin
          SendMemoryConfigurationWriteByCANParsing(AddressSpace, WriteAddress, $FFFFFFFF, ForceOptionalSpaceByte, Stream);
          Sending := False;
          Inc(FiState);
        end;
    1 : begin
          // Node received the request datagram
           if IsDatagramAckFromDestination(MessageInfo) then
           begin
             Sending := True;
             FiState := STATE_DONE;
           end;
        end;
    STATE_DONE :
       begin
       // Done
         FDone := True
       end;
  end;
end;

{ TTaskAddressSpaceMemoryWriteWithDatagram }

constructor TTaskAddressSpaceMemoryWriteWithDatagram.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddressSpace: Byte; AStream: TStream);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending, AnAddressSpace, False);
  DataStream.CopyFrom(AStream, AStream.Size);
  FWritingToAddress := True;
end;

function TTaskAddressSpaceMemoryWriteWithDatagram.Clone: TTaskOlcbBase;
begin
  Result := TTaskAddressSpaceMemoryWriteWithDatagram.Create(SourceAlias, DestinationAlias, Sending, AddressSpace, DataStream);
end;

{ TTaskIdentifyConsumer }

constructor TTaskIdentifyConsumer.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnEvent: TEventID);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending);
  FEvent := AnEvent;
end;

function TTaskIdentifyConsumer.Clone: TTaskOlcbBase;
begin
  Result := TTaskIdentifyConsumer.Create(SourceAlias, DestinationAlias, Sending, Event);
end;

procedure TTaskIdentifyConsumer.CopyTo(Target: TTaskOlcbBase);
begin
  inherited CopyTo(Target);
  (Target as TTaskIdentifyConsumer).FEvent := FEvent;
end;

procedure TTaskIdentifyConsumer.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         SendIdentifyConsumerMessage(Event);
         iState := STATE_DONE;
       end;
    STATE_DONE:
       begin
         FDone := True;
       end;
  end;
end;

{ TTaskIdentifyProducer }

constructor TTaskIdentifyProducer.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnEvent: TEventID);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending);
  FEvent := AnEvent;
end;

function TTaskIdentifyProducer.Clone: TTaskOlcbBase;
begin
  Result := TTaskIdentifyProducer.Create(SourceAlias, DestinationAlias, Sending, Event);
end;

procedure TTaskIdentifyProducer.CopyTo(Target: TTaskOlcbBase);
begin
  inherited CopyTo(Target);
  (Target as TTaskIdentifyProducer).FEvent := FEvent;
end;

procedure TTaskIdentifyProducer.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
   case iState of
    0: begin
         SendIdentifyProducerMessage(Event);
         iState := STATE_DONE;
       end;
    STATE_DONE:
       begin
         FDone := True;
       end;
  end;
end;

{ TTaskTractionReserveAndAttachDccProxy }

constructor TTaskTractionReserveAndAttachDccProxy.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddress: Word; IsShortAddress: Boolean; ASpeedStep: Byte);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending);
  FSpeedStep := ASpeedStep;
  FAddress := AnAddress;
  FIsShort := IsShortAddress;
  FReplyCode := -1;
  FReplyAddress := 0;
  FReplySpeedSteps := 0;
end;

function TTaskTractionReserveAndAttachDccProxy.Clone: TTaskOlcbBase;
begin
  Result := TTaskTractionReserveAndAttachDccProxy.Create(SourceAlias, DestinationAlias, Sending, Address, IsShort, SpeedStep);
end;

procedure TTaskTractionReserveAndAttachDccProxy.CopyTo(Target: TTaskOlcbBase);
begin
  inherited CopyTo(Target);
  (Target as TTaskTractionReserveAndAttachDccProxy).FAddress := FAddress;
  (Target as TTaskTractionReserveAndAttachDccProxy).FReplyAddress := FReplyAddress;
  (Target as TTaskTractionReserveAndAttachDccProxy).FReplyCode := FReplyCode;
  (Target as TTaskTractionReserveAndAttachDccProxy).FReplySpeedSteps := FReplySpeedSteps;
  (Target as TTaskTractionReserveAndAttachDccProxy).FIsShort := FIsShort;
  (Target as TTaskTractionReserveAndAttachDccProxy).FSpeedStep := FSpeedStep;
end;

procedure TTaskTractionReserveAndAttachDccProxy.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         FReplyCode := TRACTION_MANAGE_RESERVE_REPLY_OK;
         SendTractionReserveProxyMessage;
         Sending := False;
         iState := 1;
       end;
    1: begin
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
         SendTractionAttachDccProxyMessage(Address, IsShort, SpeedStep);
         Sending := False;
         iState := 3;
       end;
    3: begin
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
         SendTractionReleaseProxyMessage;
         iState := STATE_DONE;
       end;
    STATE_DONE: begin
         FDone := True;
       end;
  end;
end;

{ TTaskTractionReserveAndDetachDccProxy }

constructor TTaskTractionReserveAndDetachDccProxy.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddress: Word; IsShortAddress: Boolean);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending);
  FAddress := AnAddress;
  FIsShort := IsShortAddress;
  FReplyCode := -1;
  FReplyAddress := 0;
  FReplySpeedSteps := 0;
end;

function TTaskTractionReserveAndDetachDccProxy.Clone: TTaskOlcbBase;
begin
  Result := TTaskTractionReserveAndDetachDccProxy.Create(SourceAlias, DestinationAlias, Sending, Address, IsShort);
end;

procedure TTaskTractionReserveAndDetachDccProxy.CopyTo(Target: TTaskOlcbBase);
begin
  inherited CopyTo(Target);
  (Target as TTaskTractionReserveAndDetachDccProxy).FAddress := FAddress;
  (Target as TTaskTractionReserveAndDetachDccProxy).FIsShort := FIsShort;
  (Target as TTaskTractionReserveAndDetachDccProxy).FReplyAddress := FReplyAddress;
  (Target as TTaskTractionReserveAndDetachDccProxy).FReplyCode := FReplyCode;
  (Target as TTaskTractionReserveAndDetachDccProxy).FReplySpeedSteps := FReplySpeedSteps
end;

procedure TTaskTractionReserveAndDetachDccProxy.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         FReplyCode := TRACTION_MANAGE_RESERVE_REPLY_OK;
         SendTractionReserveProxyMessage;
         Sending := False;
         iState := 1;
       end;
    1: begin
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
         SendTractionDetachDccAddressProxyMessage(Address, IsShort);
         Sending := False;
         iState := 3;
       end;
    3: begin
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
         SendTractionReleaseProxyMessage;
         iState := STATE_DONE;
       end;
    STATE_DONE:
       begin
         FDone := True;
       end;
  end;
end;

{ TTaskTractionQueryDccAddressProxy }

constructor TTaskTractionQueryDccAddressProxy.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddress: Word; IsShortAddress: Boolean);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending);
  FAddress := AnAddress;
  FIsShort := IsShortAddress;
end;

function TTaskTractionQueryDccAddressProxy.Clone: TTaskOlcbBase;
begin
  Result := TTaskTractionQueryDccAddressProxy.Create(SourceAlias, DestinationAlias, Sending, Address, IsShort);
end;

procedure TTaskTractionQueryDccAddressProxy.CopyTo(Target: TTaskOlcbBase);
begin
  inherited CopyTo(Target);
  (Target as TTaskTractionQueryDccAddressProxy).FAddress := FAddress;
  (Target as TTaskTractionQueryDccAddressProxy).FIsShort := FIsShort;
end;

procedure TTaskTractionQueryDccAddressProxy.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         SendTractionQueryDccAddressProxyMessage(Address, IsShort);
         iState := STATE_DONE;
       end;
    STATE_DONE:
       begin
         FDone := True;
       end;
  end;
end;

{ TTaskTractionFunction }

constructor TTaskTractionFunction.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddress: DWord; AValue: Word);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending);
  Address := AnAddress;
  Value := AValue;
end;

function TTaskTractionFunction.Clone: TTaskOlcbBase;
begin
  Result := TTaskTractionFunction.Create(SourceAlias, DestinationAlias, Sending, Address, Value);
end;

procedure TTaskTractionFunction.CopyTo(Target: TTaskOlcbBase);
begin
  inherited CopyTo(Target);
  (Target as TTaskTractionFunction).FAddress := FAddress;
  (Target as TTaskTractionFunction).FWord := FWord;
end;

procedure TTaskTractionFunction.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         SendTractionFunction(Address, Value);
         iState := STATE_DONE;
       end;
    STATE_DONE:
       begin
         FDone := True;
       end;
  end;
end;

{ TTaskTractionSpeed }

constructor TTaskTractionSpeed.Create(ASourceAlias, ADestinationAlias: Word;  DoesStartAsSending: Boolean; ASpeed: THalfFloat; IsEStop: Boolean);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending);
  FSpeed := ASpeed;
  FEStop := IsEStop;
end;

function TTaskTractionSpeed.Clone: TTaskOlcbBase;
begin
  Result := TTaskTractionSpeed.Create(SourceAlias, DestinationAlias, Sending, Speed, EStop);
end;

procedure TTaskTractionSpeed.CopyTo(Target: TTaskOlcbBase);
begin
  inherited CopyTo(Target);
  (Target as TTaskTractionSpeed).EStop := EStop;
  (Target as TTaskTractionSpeed).FSpeed := FSpeed;
end;

procedure TTaskTractionSpeed.Process(MessageInfo: TOlcbMessage);
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

{ TTaskTractionQuerySpeed }

constructor TTaskTractionQuerySpeed.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending);
  FActualSpeed := 0;
  FCommandedSpeed := 0;
  FSetSpeed := 0;
  FStatus := 0;
end;

function TTaskTractionQuerySpeed.Clone: TTaskOlcbBase;
begin
  Result := TTaskTractionQuerySpeed.Create(SourceAlias, DestinationAlias, Sending);
end;

procedure TTaskTractionQuerySpeed.CopyTo(Target: TTaskOlcbBase);
begin
  inherited CopyTo(Target);
  (Target as TTaskTractionQuerySpeed).FActualSpeed := FActualSpeed;
  (Target as TTaskTractionQuerySpeed).FCommandedSpeed := FCommandedSpeed;
  (Target as TTaskTractionQuerySpeed).FSetSpeed := FSetSpeed;
  (Target as TTaskTractionQuerySpeed).FStatus := FStatus;
end;

procedure TTaskTractionQuerySpeed.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         SendTractionQuerySpeeds;
         Sending := False;
         iState := 1;
       end;
    1: begin
         if IsTractionSpeedsQueryFirstFrameReply(MessageInfo) then
         begin
           FSetSpeed := TOpenLCBMessageHelper( MessageInfo).ExtractDataBytesAsInt(3, 4);
           FCommandedSpeed := TOpenLCBMessageHelper( MessageInfo).ExtractDataBytesAsInt(6, 7);
           FStatus := TOpenLCBMessageHelper( MessageInfo).Data[5];
           iState := 2;
         end;
       end;
    2: begin
         if IsTractionSpeedsQuerySecondFrameReply(MessageInfo) then
         begin
           FActualSpeed := TOpenLCBMessageHelper( MessageInfo).ExtractDataBytesAsInt(2, 3);
           Sending := True;
           iState := STATE_DONE;
         end;
       end;
    STATE_DONE:
       begin
         FDone := True;
       end;
  end;
end;

{ TTaskTractionQueryFunction }

constructor TTaskTractionQueryFunction.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddress: DWord);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending);
  FAddress := AnAddress;
  FValue := -1;
end;

function TTaskTractionQueryFunction.Clone: TTaskOlcbBase;
begin
  Result := TTaskTractionQueryFunction.Create(SourceAlias, DestinationAlias, Sending, Address);
end;

procedure TTaskTractionQueryFunction.CopyTo(Target: TTaskOlcbBase);
begin
  inherited CopyTo(Target);
  (Target as TTaskTractionQueryFunction).FAddress := FAddress;
  (Target as TTaskTractionQueryFunction).FValue := FValue;
end;

procedure TTaskTractionQueryFunction.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         SendTractionQueryFunction(Address);
         Sending := False;
         iState := 1;
       end;
    1: begin
         if IsTractionFunctionQueryReply(MessageInfo) then
         begin
           FValue := TOpenLCBMessageHelper( MessageInfo).ExtractDataBytesAsInt(6, 7);
           Sending := True;
           iState := STATE_DONE
         end;
       end;
    STATE_DONE:
       begin
         FDone := True;
       end;
  end;
end;


{ TTaskIdentifyEventsAddressed }

function TTaskIdentifyEventsAddressed.Clone: TTaskOlcbBase;
begin
  Result := TTaskIdentifyEventsAddressed.Create(SourceAlias, DestinationAlias, Sending);
end;

procedure TTaskIdentifyEventsAddressed.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         SendIdentifyEventsAddressedMessage;
         iState := STATE_DONE;
       end;
    STATE_DONE:
      begin
         FDone := True;
       end;
  end;
end;

{ TTaskIdentifyEvents }

function TTaskIdentifyEvents.Clone: TTaskOlcbBase;
begin
  Result := TTaskIdentifyEvents.Create(SourceAlias, DestinationAlias, Sending);
end;

procedure TTaskIdentifyEvents.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         SendIdentifyEventsMessage;
         iState := STATE_DONE;
       end;
    STATE_DONE:
       begin
         FDone := True;
       end;
  end;
end;

{ TTaskVerifiedNodeID }

function TTaskVerifiedNodeID.Clone: TTaskOlcbBase;
begin
  Result := TTaskVerifiedNodeID.Create(SourceAlias, DestinationAlias, Sending);
end;

procedure TTaskVerifiedNodeID.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  FDone := True;  // Done before we start....
end;

{ TTaskTractionProtocol }

function TTaskTractionProtocol.Clone: TTaskOlcbBase;
begin
  Result := TTaskTractionProtocol.Create(SourceAlias, DestinationAlias, Sending);
end;

procedure TTaskTractionProtocol.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  FDone := True;  // Done before we start....
end;

{ TTaskInitializationComplete }

function TTaskInitializationComplete.Clone: TTaskOlcbBase;
begin
  Result := TTaskInitializationComplete.Create(SourceAlias, DestinationAlias, Sending);
end;

procedure TTaskInitializationComplete.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  FDone := True;  // Done before we start....
end;

{ TTaskEvent }

function TTaskEvent.Clone: TTaskOlcbBase;
begin
  Result := TTaskEvent.Create(SourceAlias, DestinationAlias, Sending);
end;

procedure TTaskEvent.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  FDone := True;  // Done before we start....
end;

{ TTaskCANLayer }

function TTaskCANLayer.Clone: TTaskOlcbBase;
begin
  Result := TTaskCANLayer.Create(SourceAlias, DestinationAlias, Sending);
end;

procedure TTaskCANLayer.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  FDone := True;  // Done before we start....
end;

{ TTaskSimpleNodeInformation }

constructor TTaskSimpleNodeInformation.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending);
  FSnip := TOlcbStructureSNIP.Create;
  FiSnipState := 0;
  iSnipState := 0;
end;

destructor TTaskSimpleNodeInformation.Destroy;
begin
  FreeAndNil(FSnip);
  inherited Destroy;
end;

function TTaskSimpleNodeInformation.Clone: TTaskOlcbBase;
begin
  Result := TTaskSimpleNodeInformation.Create(SourceAlias, DestinationAlias, Sending);
end;

procedure TTaskSimpleNodeInformation.CopyTo(Target: TTaskOlcbBase);
begin
  inherited CopyTo(Target);
  Snip.CopyTo( (Target as TTaskSimpleNodeInformation).FSnip);
end;

procedure TTaskSimpleNodeInformation.Process(MessageInfo: TOlcbMessage);
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
         SendSnipMessage;
         iSnipState := 0;
         Sending := False;
         Inc(FiState);
       end;
    1: begin
         if IsSnipMessageReply(MessageInfo) then
         begin
           LocalMessageHelper := TOpenLCBMessageHelper( MessageInfo);  // Already know this is true
           i := 2;                                               // Strip off the destination Alias
           while i < LocalMessageHelper.DataCount do
           begin
             case iSnipState of
               STATE_SNII_MFG_VERSION :
                 begin
                   Snip.SniiMfgVersion := LocalMessageHelper.Data[i];
                   Inc(i);
                   iSnipState := STATE_SNII_MFG_NAME;
                 end;
               STATE_SNII_MFG_NAME     :
                 begin
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
                   Snip.SniiUserVersion := LocalMessageHelper.Data[i];
                   Inc(i);
                   iSnipState := STATE_SNII_USER_NAME;
                 end;
               STATE_SNII_USER_NAME     :
                 begin
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
         FDone := True;
       end;
  end;
end;

{ TTaskProtocolSupport }

function TTaskProtocolSupport.Clone: TTaskOlcbBase;
begin
  Result := TTaskProtocolSupport.Create(SourceAlias, DestinationAlias, Sending);
end;

procedure TTaskProtocolSupport.CopyTo(Target: TTaskOlcbBase);
begin
  inherited CopyTo(Target);
  (Target as TTaskProtocolSupport).FProtocols := FProtocols;
end;

procedure TTaskProtocolSupport.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         SendProtocolIdentificationProtocolMessage;
         FProtocols := 0;
         Sending := False;
         Inc(FiState);
       end;
    1: begin
         if IsProtocolIdentificationProcolReplyFromDestination(MessageInfo) then
         begin
           FProtocols := TOpenLCBMessageHelper( MessageInfo).ExtractDataBytesAsInt(2, 7);
           Sending := True;
           iState := STATE_DONE;
         end;
       end;
    STATE_DONE: begin
         FDone := True;
       end;
  end;
end;

{ TTaskConfigMemoryAddressSpaceInfo }

constructor TTaskConfigMemoryAddressSpaceInfo.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddressSpace: Byte);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending);
  FAddressSpace := AnAddressSpace;
  ConfigMemoryAddressSpace := TOlcbStructureMemAddressSpace.Create;
end;

destructor TTaskConfigMemoryAddressSpaceInfo.Destroy;
begin
  FreeAndNil(FConfigMemoryAddressSpace);
  inherited Destroy;
end;

function TTaskConfigMemoryAddressSpaceInfo.Clone: TTaskOlcbBase;
begin
  Result := TTaskConfigMemoryAddressSpaceInfo.Create(SourceAlias, DestinationAlias, Sending, AddressSpace);
end;

procedure TTaskConfigMemoryAddressSpaceInfo.CopyTo(Target: TTaskOlcbBase);
begin
  inherited CopyTo(Target);
  (Target as TTaskConfigMemoryAddressSpaceInfo).FAddressSpace := AddressSpace;
  ConfigMemoryAddressSpace.CopyTo( (Target as TTaskConfigMemoryAddressSpaceInfo).ConfigMemoryAddressSpace);
end;

procedure TTaskConfigMemoryAddressSpaceInfo.Process(MessageInfo: TOlcbMessage);
var
  DatagramReceive: TCANFrameParserDatagramReceive;
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         SendMemoryConfigurationSpaceInfoByCANParsing(AddressSpace);
         Sending := False;
         Inc(FiState);
       end;
    1: begin
         if IsDatagramAckFromDestination(MessageInfo) then
         begin
           Sending := False;
           Inc(FiState);
         end;
       end;
    2: begin
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
         FDone := True;
       end;
  end;
end;

{ TTaskConfigMemoryOptions }

constructor TTaskConfigMemoryOptions.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending);
  FConfigMemoryOptions := TOlcbStructureMemOptions.Create;
end;

destructor TTaskConfigMemoryOptions.Destroy;
begin
  FreeAndNil(FConfigMemoryOptions);
  inherited Destroy;
end;

function TTaskConfigMemoryOptions.Clone: TTaskOlcbBase;
begin
  Result := TTaskConfigMemoryOptions.Create(SourceAlias, DestinationAlias, Sending);
end;

procedure TTaskConfigMemoryOptions.CopyTo(Target: TTaskOlcbBase);
begin
  inherited CopyTo(Target);
  ConfigMemoryOptions.CopyTo( (Target as TTaskConfigMemoryOptions).ConfigMemoryOptions);
end;

procedure TTaskConfigMemoryOptions.Process(MessageInfo: TOlcbMessage);
var
  DatagramReceive: TCANFrameParserDatagramReceive;
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         SendMemoryConfigurationOptionsByCANParsing;
         Sending := False;
         Inc(FiState);
       end;
    1: begin
         if IsDatagramAckFromDestination(MessageInfo) then
         begin
           Sending := False;
           Inc(FiState);
         end;
       end;
    2: begin
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
         FDone := True;
       end;
  end;
end;

{ TTaskVerifyNodeID }

function TTaskVerifyNodeID.Clone: TTaskOlcbBase;
begin
  Result := TTaskVerifyNodeID.Create(SourceAlias, DestinationAlias, Sending);
end;

procedure TTaskVerifyNodeID.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         SendVerifyNodeIDGlobalMessage;
         iState := STATE_DONE;
       end;
    STATE_DONE:
       begin
         FDone := True;
       end;
  end;
end;

{ TTaskVerifyNodeIDGlobal }

function TTaskVerifyNodeIDGlobal.Clone: TTaskOlcbBase;
begin
  Result := TTaskVerifyNodeIDGlobal.Create(SourceAlias, DestinationAlias, Sending);
end;

procedure TTaskVerifyNodeIDGlobal.Process(MessageInfo: TOlcbMessage);
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         SendVerifyNodeIDToDestinationMessage;
         iState := STATE_DONE;
       end;
    STATE_DONE: begin
         FDone := True;
       end;
  end;
end;

{ TTaskAddressSpaceMemoryCommonWithDatagram }

function TTaskAddressSpaceMemoryCommonWithDatagram.GetMaxPayloadSize: DWord;
begin
  Result := MAX_CONFIG_MEM_READWRITE_SIZE;
end;

constructor TTaskAddressSpaceMemoryCommonWithDatagram.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean; AnAddressSpace: Byte;
  UseTerminatorChar: Boolean);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending);
  FAddressSpace := AnAddressSpace;
  ForceOptionalSpaceByte := False;
  FWritingToAddress := False;
  FDataStream := TMemoryStream.Create;
  FUsingTerminator := UseTerminatorChar;
  FLocalLoopTime := 0;
end;

destructor TTaskAddressSpaceMemoryCommonWithDatagram.Destroy;
begin
  FreeAndNil(FDataStream);
  inherited Destroy;
end;

function TTaskAddressSpaceMemoryCommonWithDatagram.Clone: TTaskOlcbBase;
begin
  Result := TTaskAddressSpaceMemoryCommonWithDatagram.Create(SourceAlias, DestinationAlias, Sending, AddressSpace, UsingTerminator);
end;

procedure TTaskAddressSpaceMemoryCommonWithDatagram.CopyTo(Target: TTaskOlcbBase);
begin
  inherited CopyTo(Target);
  (Target as TTaskAddressSpaceMemoryCommonWithDatagram).FAddressSpace := FAddressSpace;
  (Target as TTaskAddressSpaceMemoryCommonWithDatagram).FCurrentAddress := FCurrentAddress;
  (Target as TTaskAddressSpaceMemoryCommonWithDatagram).FCurrentSendSize := FCurrentSendSize;
  (Target as TTaskAddressSpaceMemoryCommonWithDatagram).FDataStream.CopyFrom(FDataStream, FDataStream.Size);
  (Target as TTaskAddressSpaceMemoryCommonWithDatagram).FForceOptionalSpaceByte := FForceOptionalSpaceByte;
  (Target as TTaskAddressSpaceMemoryCommonWithDatagram).FMaxAddress := FMaxAddress;
  (Target as TTaskAddressSpaceMemoryCommonWithDatagram).FMinAddress := FMinAddress;
  (Target as TTaskAddressSpaceMemoryCommonWithDatagram).FTerminator := FTerminator;
  (Target as TTaskAddressSpaceMemoryCommonWithDatagram).FUsingTerminator := FUsingTerminator;
  (Target as TTaskAddressSpaceMemoryCommonWithDatagram).FWritingToAddress := FWritingToAddress;
end;

{ TTaskConfigMemoryAddressEnumAllSpaceInfo }

constructor TTaskConfigMemoryAddressEnumAllSpaceInfo.Create(ASourceAlias, ADestinationAlias: Word; DoesStartAsSending: Boolean);
begin
  inherited Create(ASourceAlias, ADestinationAlias, DoesStartAsSending);
  FCurrentAddressSpace := 0;
  FMaxAddressSpace := 0;
  FMinAddressSpace := 0;
  FConfigMemAddressInfo := TOlcbStructureMemConfig.Create;
end;

destructor TTaskConfigMemoryAddressEnumAllSpaceInfo.Destroy;
begin
  FreeAndNil(FConfigMemAddressInfo);
  inherited Destroy;
end;

function TTaskConfigMemoryAddressEnumAllSpaceInfo.Clone: TTaskOlcbBase;
begin
  Result := TTaskConfigMemoryAddressEnumAllSpaceInfo.Create(SourceAlias, DestinationAlias, Sending);
end;

procedure TTaskConfigMemoryAddressEnumAllSpaceInfo.CopyTo(Target: TTaskOlcbBase);
begin
  inherited CopyTo(Target);
  ConfigMemAddressInfo.CopyTo( (Target as TTaskConfigMemoryAddressEnumAllSpaceInfo).ConfigMemAddressInfo);
  (Target as TTaskConfigMemoryAddressEnumAllSpaceInfo).CurrentAddressSpace := CurrentAddressSpace;
  (Target as TTaskConfigMemoryAddressEnumAllSpaceInfo).MaxAddressSpace := MaxAddressSpace;
  (Target as TTaskConfigMemoryAddressEnumAllSpaceInfo).MinAddressSpace := MinAddressSpace;
end;

procedure TTaskConfigMemoryAddressEnumAllSpaceInfo.Process(MessageInfo: TOlcbMessage);
var
  DatagramReceive: TCANFrameParserDatagramReceive;
  NewSpace: TOlcbStructureMemAddressSpace;
begin
  inherited Process(MessageInfo);
  case iState of
    0: begin
         SendMemoryConfigurationOptionsByCANParsing;
         Inc(FiState);
         Sending := False;
       end;
    1: begin
         if IsDatagramAckFromDestination(MessageInfo) then
         begin
           Sending := False;
           Inc(FiState);
         end;
       end;
    2: begin
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
         SendMemoryConfigurationSpaceInfoByCANParsing(CurrentAddressSpace);
         Sending := False;
         Inc(FiState);
       end;
    4: begin
         if IsDatagramAckFromDestination(MessageInfo) then
         begin
           Sending := False;
           Inc(FiState);
         end;
       end;
    5: begin
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
  Task, CompletedTask: TTaskOlcbBase;
begin
  CompletedTask := nil;
  List := TaskList.LockList;
  try
    if List.Count > 0 then
    begin
      Task := TTaskOlcbBase( List[0]);
      if not Task.Sending then
      begin
        Task.Process(MessageInfo);
        if Task.Done or Task.ForceTermination then
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
  Task, CompletedTask: TTaskOlcbBase;
begin
  CompletedTask := nil;
  List := TaskList.LockList;
  try
    if List.Count > 0 then
    begin
      Task := TTaskOlcbBase( List[0]);
      if Task.Sending then
      begin
        Task.Process(nil);
        if Task.Done or Task.ForceTermination then
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
  LoopTime := 0;
  DestStreamIdPool := 0;
  SourceStreamIdPool := 0;


finalization

end.

