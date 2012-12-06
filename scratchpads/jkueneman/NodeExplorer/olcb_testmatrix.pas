unit olcb_testmatrix;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, olcb_utilities, unitolcb_defines, nodeexplorer_settings,
  DOM, XMLRead, XMLWrite, ComCtrls, unitDebugLogger, unitintegerlist;

const
  MAX_MTI_VALUE             = $09FFF000;
  MIN_MTI_VALUE             = $09000000;
  MTI_INC_OFFSET            = $00001000;
  MTI_ADDRESSED_BIT         = $00008000;

  PIP_UNASSIGNED_MASK       = $0007FFFFFFF0;
  PIP_RESERVED_MASK         = $00000000000F;
  PIP_EXTENSION_BIT_START   = $1000;                 // Active 0 so "xx01" were the 0 is the set bit and MSB it on the left... a network thing I guess
  PIP_EXTENSION_BIT_END     = $2000;                 // Active 0 so "xx10" were the 0 is the set bit and MSB is on the left... a network thing I guess

  STR_PROTOCOL_IDENTIFICATION_PROTOCOL_CLASS = 'TTestProtocolSupport';
  STR_TEST_GET_NODES_UNDER_TEST_CLASS        = 'TTestGetNodesUnderTest';

  XML_ERROR_UNEXPECTED_REPLY = 'The expected number of messages received for the objective was not met';
  XML_ERROR_NO_REPLY         = 'A reply was expected from the node but did not occur';
  XML_ERROR_INVALID_NODE_ID = 'The full node ID received did not match the Full Node Alias that Node Explorer was expecting for the test node';
  XML_ERROR_STANDARD_FRAME = 'The node should not respond to CAN standard frame (11 bit) messages';

  XML_ERROR_FORMAT_UNUSED_BITS_SET = 'The top 2 bits in the extended CAN header are not set to "0"';
  XML_ERROR_FORMAT_FORWARDING_BIT_NOT_SET = 'Bit 28 in the CAN header should be set if the node is not used to forward across network segments';
  XML_ERROR_FORMAT_INVALID_MTI = 'Bits 12-23 (MTI) in the CAN header were not correct in the reply';
  XML_ERROR_FORMAT_INVALID_SOURCE_ALIAS = 'Bits 0-11 defining the Node Alias did not match the stored Node Alias that Node Explorer was expecting for the test node';
  XML_ERROR_FORMAT_INVALID_DEST_ALIAS = 'Destination Alias (either in the CAN header or first 2 data bytes) did not match Proxy Alias of Node Explorer';

  XML_ERROR_PIP_UNASSIGNED_BITS = 'The node is using the bits in the Protocol Identification Protocol that are current unassigned';
  XML_ERROR_PIP_RESERVED_BITS = 'The node is using the bits in the Protocol Identification Protocol defined as reserved';
  XML_ERROR_PIP_START_END_BIT_SUPPORT = 'The node does not support the Protocol Identification Protocol start-end bit for future expansion, it is suggested this be implemented';
  XML_ERROR_PIP_UNEXPECTED_RESPONSE_TO_START_BIT = 'The node should not have responded to the PIP expansion start bit, it should have waited until the stop bit is sent';

  XML_ERROR_UNKOWN_MTI_OPTIONAL_CODE = 'The unknown MTI Error Code should be returned in the optional data payload defined by Optional Interaction Rejected';
  XML_ERROR_UNKOWN_MTI_ERROR_CODE = 'The error code sent by Optional Interaction Rejected should have been Permenemt Error ($2000) in the result';

  XML_ERROR_NO_STARTUP_NO_AMR = 'No Alias Mapping Reset (AMR) was detected';
  XML_ERROR_NO_STARTUP_NO_CID0 = 'The first Check ID (CID0) message was not detected';
  XML_ERROR_NO_STARTUP_NO_CID1 = 'The second Check ID (CID1) message was not detected';
  XML_ERROR_NO_STARTUP_NO_CID2 = 'The third Check ID (CID2) message was not detected';
  XML_ERROR_NO_STARTUP_NO_CID3 = 'The fourth Check ID (CID3) message was not detected';
  XML_ERROR_NO_STARTUP_NO_RID = 'The Reserve ID message was not detected';
  XML_ERROR_NO_STARTUP_NO_AMD = 'The Alias Mapping Definition (AMD) was not detected';
  XML_ERROR_NO_STARTUP_NO_INITIALZIED = 'The Node Initialized message was not detected';

  XML_ERROR_ALIAS_ZERO = 'The node generated an Alias with a value of 0';
  XML_ERROR_ALIAS_DUPLICATE = 'The node generated a duplicate Alias value which is not recommended';

  MAX_ALIAS_VALUES = 4095; // = $FFF
  MAX_STANDARD_FRAME = 2047; // $7FF


type
  TTestErrorCode = (
    teUnExpectedReply,         // Got a reply we were not expecting
    teNoReply,                 // No Reply was found
    teFullNodeIDInvalid,       // Full NodeID in the Data Bytes does not match what is stored in Settings for the node under test
    teStandardFrameResponse,   // Node replied to a Standard Frame Message, it should igore them all
    teCancelled                // The test was cancled
  );
  TTestErrorCodes = set of TTestErrorCode;

  TTestErrorFormatCode = (
    tefUnusedBitsSet,           // The node set one of the 3 unused upper bits (not accessible in CAN?)
    tefForwardingBitNotSet,     // Reserved bit was not a 1
    tefInvalidMTI,              // MTI was incorrect (includes the Frame Type set for OpenLCB messages)
    tefInvalidSourceAlias,      // Source Alias of the message sent back to Node Explorer was incorrect (assumes a single node under test)
    tefInvalidDestAlias        // Dest Alias of the message sent back to Node Explorer was incorrect (assumes a single node under test)
  );
  TTestErrorFormatCodes = set of TTestErrorFormatCode;

  TTestErrorPipCode = (
    tepPipUsingReservedBits,    // Using Reserved Bits in the PIP
    tepPipUsingUnassignedBits,  // Using Unassigned Bits in the PIP
    tepPipRespondedToStartBit,  // Unexpected response to start bit of extended PIP support
    tepPipStartEndBitSupport    // PIP implementation does not support the start and stop bit convention
  );
  TTestErrorPipCodes = set of TTestErrorPipCode;

  TTestUnknownMTICode = (
    tuErrorCode,                // OIR Error Code
    tuOptionalCode              // OIR Optional Info was incorrect
  );
  TTestUnknownMTICodes = set of TTestUnknownMTICode;

  TTestNodeStartupCode = (
    tsNoAMR,                     // No AMR detected
    tsNoCID0,                    // No Check ID 0
    tsNoCID1,                    // No Check ID 1
    tsNoCID2,                    // No Check ID 2
    tsNoCID3,                    // No Check ID 3
    tsNoRID,                     // No Reserve ID
    tsNoAMD,                     // No Alias Map Definition
    tsNoInitialized              // No Node Initalized
  );
  TTestNodeStartupCodes = set of TTestNodeStartupCode;

  TTestAliasConflictCode = (
    taZeroAlias,                // Node generated a 0 Alias
    taDuplicateAlias            // Node generated a duplicate Alias
  );
  TTestAliasConflictCodes = set of TTestAliasConflictCode;

type
  TTestState = (ts_Idle, ts_Initialize, ts_ObjectiveStart, ts_Sending, ts_Receiving, ts_ObjectiveEnd, ts_Complete);

  TMTIArray = array of DWord;

  { TKnownMTI }

  TKnownMTI = class
  private
    FMTI: DWord;
    FMTIArray: TMTIArray;
    FMTIList: TStringList;
  protected
    function DuplicateMTI(TestMTI: DWord): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function FirstUnknownMTI: DWord;
    function NextUnknownMTI: DWord;
    procedure ResetMTI;
    property MTIList: TStringList read FMTIList write FMTIList;
    property MTI: DWord read FMTI write FMTI;
    property MTIArray: TMTIArray read FMTIArray write FMTIArray;
  end;

  TTestBase = class;
  TTestSyncronizationFunc = procedure(Test: TTestBase) of object;

  { TTestBase }

  TTestBase = class(TPersistent)
  private
    FErrorAliasConflict: TTestAliasConflictCodes;
    FErrorCodes: TTestErrorCodes;
    FErrorCodesFormat: TTestErrorFormatCodes;
    FErrorCodesPip: TTestErrorPipCodes;
    FErrorCodesStartup: TTestNodeStartupCodes;
    FErrorCodesUnknownMTI: TTestUnknownMTICodes;
    FErrorCodesUnknownMTIStrings: TStringList;
    FFilterRepliesForNUT: Boolean;
    FFreeOnLog: Boolean;
    FListItem: TListItem;
    FMessageHelper: TOpenLCBMessageHelper;
    FProgressBarPos: Integer;
    FProgressBarRange: Integer;
    FReceiveCount: Integer;
    FStateMachineIndex: Integer;
    FSyncAliasChangedFunc: TTestSyncronizationFunc;
    FSyncAliasChangingFunc: TTestSyncronizationFunc;
    FSyncHideNodeResetMessageFunc: TTestSyncronizationFunc;
    FSyncShowNodeResetMessageFunc: TTestSyncronizationFunc;
    FSyncTestCompleteFunc: TTestSyncronizationFunc;
    FSyncUpdateProgressBarFunc: TTestSyncronizationFunc;
    FTestState: TTestState;
    FXMLResults: TXMLDocument;
    FXMLTests: TDOMNode;
    function GetPassed: Boolean;
  protected
    property MessageHelper: TOpenLCBMessageHelper read FMessageHelper write FMessageHelper;
    property ReceiveCount: Integer read FReceiveCount write FReceiveCount;
    procedure UpdateProgressBar(Range, Position: Integer);
  public
    property ErrorCodes: TTestErrorCodes read FErrorCodes write FErrorCodes;
    property ErrorCodesFormat: TTestErrorFormatCodes read FErrorCodesFormat write FErrorCodesFormat;
    property ErrorCodesPip: TTestErrorPipCodes read FErrorCodesPip write FErrorCodesPip;
    property ErrorCodesUnknownMTIStrings: TStringList read FErrorCodesUnknownMTIStrings write FErrorCodesUnknownMTIStrings;
    property ErrorCodesUnknownMTI: TTestUnknownMTICodes read FErrorCodesUnknownMTI write FErrorCodesUnknownMTI;
    property ErrorCodesStartup: TTestNodeStartupCodes read FErrorCodesStartup write FErrorCodesStartup;
    property ErrorAliasConflict: TTestAliasConflictCodes read FErrorAliasConflict write FErrorAliasConflict;
    property FilterRepliesForNUT: Boolean read FFilterRepliesForNUT write FFilterRepliesForNUT;
    property FreeOnLog: Boolean read FFreeOnLog write FFreeOnLog;
    property ListItem: TListItem read FListItem write FListItem;
    property Passed: Boolean read GetPassed;
    property ProgressBarRange: Integer read FProgressBarRange write FProgressBarRange;
    property ProgressBarPos: Integer read FProgressBarPos write FProgressBarPos;
    property StateMachineIndex: Integer read FStateMachineIndex write FStateMachineIndex;
    property TestState: TTestState read FTestState write FTestState;
    property XMLTests: TDOMNode  read FXMLTests write FXMLTests;                // Node that describes the test from the Test Matrix XML file ( <test>...</test> )
    property XMLResults: TXMLDocument read FXMLResults write FXMLResults;

    property SyncAliasChangingFunc: TTestSyncronizationFunc read FSyncAliasChangingFunc write FSyncAliasChangingFunc;
    property SyncAliasChangedFunc: TTestSyncronizationFunc read FSyncAliasChangedFunc write FSyncAliasChangedFunc;
    property SyncTestCompleteFunc: TTestSyncronizationFunc read FSyncTestCompleteFunc write FSyncTestCompleteFunc;
    property SyncShowNodeResetMessageFunc: TTestSyncronizationFunc read FSyncShowNodeResetMessageFunc write FSyncShowNodeResetMessageFunc;
    property SyncHideNodeResetMessageFunc: TTestSyncronizationFunc read FSyncHideNodeResetMessageFunc write FSyncHideNodeResetMessageFunc;
    property SyncUpdateProgressBarFunc: TTestSyncronizationFunc read FSyncUpdateProgressBarFunc write FSyncUpdateProgressBarFunc;

    constructor Create; virtual;
    destructor Destroy; override;
    class function CreateInstanceFromString(AClassname: String): TTestBase;
    procedure ProcessObjectives(Thread: TThread; SendStrings: TStringList; ReceiveString: String; var iObjective: Integer; var ReadTimeout: Integer; ReceiveTime: Integer; var SendNext: Boolean); virtual;
    procedure InitTest; virtual;
    function IsMessageForNodeUnderTest(MessageStr: String): Boolean;
    function HasAnyErrorCodeSet: Boolean;
    procedure ValidateBasicReturnMessage(ExpectedMTI: DWord; Helper: TOpenLCBMessageHelper); virtual;
    procedure ClearErrorCodes;
    procedure SyncAliasChanging;
    procedure SyncAliasChanged;
    procedure SyncTestComplete;
    procedure SyncShowNodeResetMessage;
    procedure SyncHideNodeResetMessage;
    procedure SyncUpdateProgressBar;

  end;
  TTestBaseClass = class of TTestBase;

  { TTestGetNodesUnderTest }

  TTestGetNodesUnderTest = class(TTestBase)
  public
    constructor Create; override;
    procedure ProcessObjectives(Thread: TThread; SendStrings: TStringList; ReceiveString: String; var iObjective: Integer; var ReadTimeout: Integer; ReceiveTime: Integer; var SendNext: Boolean); override;
  end;
  TTestVerifyNodesIDClass = class of TTestGetNodesUnderTest;

  { TTestAliasMapEnquiry }

  TTestAliasMapEnquiry = class(TTestBase)
  public
    procedure ProcessObjectives(Thread: TThread; SendStrings: TStringList; ReceiveString: String; var iObjective: Integer; var ReadTimeout: Integer; ReceiveTime: Integer; var SendNext: Boolean); override;
  end;
  TTestAliasMapEnquiryClass = class of TTestAliasMapEnquiry;

  { TTestVerifyNodeID }

  TTestVerifyNodeID = class(TTestBase)
  public
    procedure ProcessObjectives(Thread: TThread; SendStrings: TStringList; ReceiveString: String; var iObjective: Integer; var ReadTimeout: Integer; ReceiveTime: Integer; var SendNext: Boolean); override;
  end;
  TTestVerifyNodeIDClass = class of TTestVerifyNodeID;

  { TTestProtocolSupport }

  TTestProtocolSupport = class(TTestBase)
  private
    FStartEndResponse: Boolean;
  public
    procedure InitTest; override;
    procedure ProcessObjectives(Thread: TThread; SendStrings: TStringList; ReceiveString: String; var iObjective: Integer; var ReadTimeout: Integer; ReceiveTime: Integer; var SendNext: Boolean); override;
    property StartEndResponse: Boolean read FStartEndResponse write FStartEndResponse;
  end;
  TTestProtocolSupportClass = class of TTestProtocolSupport;

  { TTestStandardFrame }

  TTestStandardFrame = class(TTestBase)
  private
    FiFrame: Integer;
  public
    procedure InitTest; override;
    procedure ProcessObjectives(Thread: TThread; SendStrings: TStringList; ReceiveString: String; var iObjective: Integer; var ReadTimeout: Integer; ReceiveTime: Integer; var SendNext: Boolean); override;
    property iFrame: Integer read FiFrame write FiFrame;
  end;
  TTestStandardFrameClass = class of TTestStandardFrame;

  { TTestUnknownMTIAddressed }

  TTestUnknownMTIAddressed = class(TTestBase)
  private
    FMTIManager: TKnownMTI;
    FScrollbarIndex: Word;
    FScrollbarMax: Word;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ProcessObjectives(Thread: TThread; SendStrings: TStringList; ReceiveString: String; var iObjective: Integer; var ReadTimeout: Integer; ReceiveTime: Integer; var SendNext: Boolean); override;
    procedure InitTest; override;
    property MTIManager: TKnownMTI read FMTIManager write FMTIManager;
    property ScrollbarMax: Word read FScrollbarMax write FScrollbarMax;
    property ScrollbarIndex: Word read FScrollbarIndex write FScrollbarIndex;
  end;
  TTestUnknownMTIAddressedClass = class of TTestUnknownMTIAddressed;

  { TTestAliasConflict }

  TTestAliasConflict = class(TTestBase)
  public
    procedure ProcessObjectives(Thread: TThread; SendStrings: TStringList; ReceiveString: String; var iObjective: Integer; var ReadTimeout: Integer; ReceiveTime: Integer; var SendNext: Boolean); override;
  end;
  TTestAliasConflictClass = class of TTestAliasConflict;

  { TTestZeroOrRepeatedAllocation }

  TTestZeroOrRepeatedAllocation = class(TTestBase)
  private
    FAliasList: TIntegerList;
    FReAllocationCount: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure InitTest; override;
    procedure ProcessObjectives(Thread: TThread; SendStrings: TStringList; ReceiveString: String; var iObjective: Integer; var ReadTimeout: Integer; ReceiveTime: Integer; var SendNext: Boolean); override;
    property ReAllocationCount: Integer read FReAllocationCount write FReAllocationCount;
    property AliasList: TIntegerList read FAliasList write FAliasList;
  end;
  TTestZeroOrRepeatedAllocationClass = class of TTestZeroOrRepeatedAllocation;

  { TTestStartup }

  TTestStartup = class(TTestBase)
  public
    procedure ProcessObjectives(Thread: TThread; SendStrings: TStringList; ReceiveString: String; var iObjective: Integer; var ReadTimeout: Integer; ReceiveTime: Integer; var SendNext: Boolean); override;
  end;
  TTestStartupClass = class of TTestStartup;

  { TTestEvents }

  TTestEvents = class(TTestBase)
  public
    procedure ProcessObjectives(Thread: TThread; SendStrings: TStringList; ReceiveString: String; var iObjective: Integer; var ReadTimeout: Integer; ReceiveTime: Integer; var SendNext: Boolean); override;
  end;
  TTestEventsClass = class of TTestEvents;

  function FindTestFromXML(XMLDocTests: TXMLDocument; TestClassName: String): TTestBase;
  procedure ExtractResultsFromXML(XMLDoc: TXMLDocument; ReceiveResults: TStringList);

implementation

function FindTestFromXML(XMLDocTests: TXMLDocument; TestClassName: String): TTestBase;
var
  ClassNode: TDOMNode;
  TestClass: TTestBaseClass;
  i: Integer;
begin
  Result := nil;
  TestClass := nil;
  if Assigned(XMLDocTests) then
  begin
    for i := 0 to XMLDocTests.DocumentElement.ChildNodes.Count - 1 do
    begin
      if XMLDocTests.DocumentElement.ChildNodes[i].NodeName = XML_ELEMENT_TEST then
      begin
        ClassNode := XMLDocTests.DocumentElement.ChildNodes[i].FindNode(XML_ELEMENT_CLASSNAME);
        if Assigned(ClassNode) then
        begin
          if TestClassName = ClassNode.FirstChild.NodeValue then
          begin
            Result := TestClass.CreateInstanceFromString(TestClassName);
            Result.XMLTests := XMLDocTests.DocumentElement.ChildNodes[i];
          end;
        end;
      end;
    end;
  end;
end;

procedure ExtractResultsFromXML(XMLDoc: TXMLDocument; ReceiveResults: TStringList);
var
  TestResult, Test, TestObjective, Results: TDOMNode;
  i: Integer;
begin
  ReceiveResults.Clear;
  if Assigned(XMLDoc) then
  begin
    TestResult := XMLDoc.FindNode(XML_ELEMENT_TEST_RESULT_ROOT);
    if Assigned(TestResult) then
    begin
      Test := TestResult.FindNode(XML_ELEMENT_TEST);
      if Assigned(Test) then
      begin
        TestObjective := Test.FindNode(XML_ELEMENT_TESTOBJECTIVE);
        if Assigned(TestObjective) then
        begin
          Results := TestObjective.FindNode(XML_ELEMENT_OBJECTIVERESULTS);
          if Assigned(Results) then
          begin
            for i := 0 to Results.ChildNodes.Count - 1 do
            begin
              if Results.ChildNodes[i].NodeName = XML_ELEMENT_RECEIVE then
                ReceiveResults.Add(Results.ChildNodes[i].FirstChild.NodeValue);
            end;
          end;
        end;
      end;
    end;
  end;
end;



{ TKnownMTI }

function TKnownMTI.DuplicateMTI(TestMTI: DWord): Boolean;
var
  i: Integer;
begin
  Result := False;
  i := 0;
  while (i < Length(FMTIArray)) and not Result do
  begin
    Result := TestMTI = MTIArray[i];
    Inc(i);
  end;
end;

constructor TKnownMTI.Create;
begin
  inherited Create;
  FMTIList := TStringList.Create;
  ResetMTI;
end;

destructor TKnownMTI.Destroy;
begin
  FreeAndNil(FMTIList);
end;

function TKnownMTI.FirstUnknownMTI: DWord;
begin
  FMTI := MIN_MTI_VALUE;
  // Stop when the max value is but keep going if it is a Known MTI or it does not have the Addressed Bit set
  while (MTI <= MAX_MTI_VALUE) and (DuplicateMTI( MTI) or (MTI and MTI_ADDRESSED_BIT = 0)) do
    FMTI := FMTI + MTI_INC_OFFSET;
  Result := FMTI;
end;

function TKnownMTI.NextUnknownMTI: DWord;
begin
  FMTI := FMTI + MTI_INC_OFFSET;
  // Stop when the max value is but keep going if it is a Known MTI or it does not have the Addressed Bit set
  while (MTI <= MAX_MTI_VALUE) and (DuplicateMTI( MTI) or (MTI and MTI_ADDRESSED_BIT = 0)) do
    FMTI := FMTI + MTI_INC_OFFSET;
  Result := FMTI;
end;

procedure TKnownMTI.ResetMTI;
var
  i: Integer;
begin
  FMTI := MIN_MTI_VALUE;
  {$IFDEF Windows}
  MTIList.LoadFromFile(Settings.ApplicationPath + FILENAME_KNOWN_MTI_WIN);
  {$ENDIF}
  {$IFDEF darwin}
  MTIList.LoadFromFile(Settings.ApplicationPath + FILENAME_KNOWN_MTI_UNIX);
  {$ENDIF}
  SetLength(FMTIArray, MTIList.Count);
  for i := 0 to MTIList.Count - 1 do
    MTIArray[i] := StrToInt( MTIList[i]);
end;


{ TTestUnknownMTIAddressed }

constructor TTestUnknownMTIAddressed.Create;
begin
  inherited Create;
  FMTIManager := TKnownMTI.Create;
end;

destructor TTestUnknownMTIAddressed.Destroy;
begin
  FreeAndNil(FMTIManager);
  inherited Destroy;
end;

procedure TTestUnknownMTIAddressed.ProcessObjectives(Thread: TThread; SendStrings: TStringList; ReceiveString: String; var iObjective: Integer; var ReadTimeout: Integer; ReceiveTime: Integer; var SendNext: Boolean);
var
  UnknownMTI: DWord;
  x, y: Word;
begin
  inherited ProcessObjectives(Thread, SendStrings, ReceiveString, iObjective, ReadTimeout, ReceiveTime, SendNext);
  case StateMachineIndex of
    0 : begin
          // Send Unknown MTI Messages:
          if ScrollbarIndex mod Settings.UIRefreshRate = 0 then
            UpdateProgressBar(ScrollbarMax, ScrollbarIndex);
          UnknownMTI := MTIManager.FirstUnknownMTI;
          UpdateProgressBar(ScrollbarMax, ScrollbarIndex);
          MessageHelper.Load(ol_OpenLCB, UnknownMTI, Settings.ProxyNodeAlias, Settings.TargetNodeAlias, 2, 0, 0, 0, 0, 0 ,0 ,0 ,0);
          SendStrings.Add(MessageHelper.Encode);

          ReceiveCount := 0;
          Inc(FStateMachineIndex);
        end;
    1 : begin
          if ReceiveString <> '' then
          begin
            if ReceiveCount = 0 then
            begin
              MessageHelper.Decompose(ReceiveString);
              ValidateBasicReturnMessage(MTI_OPTIONAL_INTERACTION_REJECTED, MessageHelper);
              if MessageHelper.ExtractDataBytesAsInt(2, 3) <> OIR_PERMANENT_ERROR then
                Include(FErrorCodesUnknownMTI, tuErrorCode);
              if MessageHelper.ExtractDataBytesAsInt(4, 5) <> ((MTIManager.MTI and $00FFF000) shr 12) then
                Include(FErrorCodesUnknownMTI, tuOptionalCode);
            end;
            Inc(FReceiveCount);
            SendNext := False;        // Spin on receive until nothing is returned
          end else
          begin
            if ReceiveCount = 0 then
              Include(FErrorCodes, teNoReply)
            else
            if ReceiveCount > 1 then
              Include(FErrorCodes, teUnExpectedReply);
            SendNext := True;
            Inc(FStateMachineIndex);
          end
        end;
    2 : begin
          // Send Unknown MTI Messages:
          UnknownMTI := MTIManager.NextUnknownMTI;
          if MTIManager.MTI <= MAX_MTI_VALUE then
          begin
            MessageHelper.Load(ol_OpenLCB, UnknownMTI, Settings.ProxyNodeAlias, Settings.TargetNodeAlias, 2, 0, 0, 0, 0, 0 ,0 ,0 ,0);
            SendStrings.Add(MessageHelper.Encode);
            ReceiveCount := 0;
            StateMachineIndex := 1;
            Inc(FScrollbarIndex);
           end else
          begin
            UpdateProgressBar(ScrollbarMax, ScrollbarMax);
            Inc(FStateMachineIndex);
            Inc(iObjective);
          end;
          if ScrollbarIndex mod Settings.UIRefreshRate = 0 then
            UpdateProgressBar(ScrollbarMax, ScrollbarIndex);
        end;
  end;
end;

procedure TTestUnknownMTIAddressed.InitTest;
begin
  inherited InitTest;
  MTIManager.ResetMTI;
  ScrollbarMax := (((MAX_MTI_VALUE and $00FFF000) shr 12) div 2) - MTIManager.FMTIList.Count;    // This includes MTI the are excluded because their Addressed Bit is set
  ScrollbarIndex := 0;
end;

{ TTestStandardFrame }

procedure TTestStandardFrame.InitTest;
begin
  inherited InitTest;
  FiFrame := 0;
  UpdateProgressBar(MAX_STANDARD_FRAME, iFrame);
end;

procedure TTestStandardFrame.ProcessObjectives(Thread: TThread; SendStrings: TStringList; ReceiveString: String; var iObjective: Integer; var ReadTimeout: Integer; ReceiveTime: Integer; var SendNext: Boolean);
var
  i: Integer;
begin
  inherited ProcessObjectives(Thread, SendStrings, ReceiveString, iObjective, ReadTimeout, ReceiveTime, SendNext);
  case StateMachineIndex of
    0 : begin
          // Send all Standard Messages in one shot and see what comes back
          if Settings.PingPongStandardFrameTest then
          begin
            if iFrame mod Settings.UIRefreshRate = 0 then
              UpdateProgressBar(MAX_STANDARD_FRAME, iFrame);
            SendStrings.Add(':S' + IntToHex(iFrame, 3) + 'N;')
          end else
          begin
            for i := 0 to MAX_STANDARD_FRAME do
            begin
              if i mod Settings.UIRefreshRate = 0 then
                UpdateProgressBar(MAX_STANDARD_FRAME, i);
              SendStrings.Add(':S' + IntToHex(i, 3) + 'N;');
            end
          end;
          ReceiveCount := 0;
          Inc(FStateMachineIndex);

        end;
    1 : begin
          // Send Standard Messages:
          if ReceiveString <> '' then                                           // No responses to Standard CAN message expected
            Include(FErrorCodes, teStandardFrameResponse);

          if Settings.PingPongStandardFrameTest then
          begin
            if iFrame >= MAX_STANDARD_FRAME then
            begin
              Inc(FStateMachineIndex);
              Inc(iObjective)                  // Done
            end else
            begin
              Inc(FiFrame);
              StateMachineIndex := 0;         // Next Standard frame if in ping pong
            end;
          end else
          begin
            Inc(FStateMachineIndex);           // Done
            Inc(iObjective)
          end;
        end;
  end;
end;


{ TTestProtocolSupport }

procedure TTestProtocolSupport.InitTest;
begin
  inherited InitTest;
  StartEndResponse := False;
end;

procedure TTestProtocolSupport.ProcessObjectives(Thread: TThread; SendStrings: TStringList; ReceiveString: String; var iObjective: Integer; var ReadTimeout: Integer; ReceiveTime: Integer; var SendNext: Boolean);
var
  PipMask: QWord;
begin
  inherited ProcessObjectives(Thread, SendStrings, ReceiveString, iObjective, ReadTimeout, ReceiveTime, SendNext);
  case StateMachineIndex of
    0 : begin
          // Send the Protocol Identification message
          MessageHelper.Load(ol_OpenLCB, MTI_PROTOCOL_SUPPORT_INQUIRY, Settings.ProxyNodeAlias, Settings.TargetNodeAlias, 2, 0, 0, 0, 0, 0 ,0 ,0 ,0);
          SendStrings.Add(MessageHelper.Encode);

          ReceiveCount := 0;
          Inc(FStateMachineIndex);
        end;
    1 : begin
          // Receive Node that responded with Protocol Identification message
          if ReceiveString <> '' then
          begin
            if ReceiveCount = 0 then
            begin
              MessageHelper.Decompose(ReceiveString);
              ValidateBasicReturnMessage(MTI_PROTOCOL_SUPPORT_REPLY, MessageHelper);
              PipMask := MessageHelper.ExtractDataBytesAsInt(2, 7);
              if PipMask and PIP_RESERVED_MASK <> 0 then
                Include(FErrorCodesPip, tepPipUsingReservedBits);
              if PipMask and PIP_UNASSIGNED_MASK <> 0 then
                Include(FErrorCodesPip, tepPipUsingUnassignedBits);
            end;
            Inc(FReceiveCount);
            SendNext := False;        // Spin on receive until nothing is returned
          end else
          begin
            if ReceiveCount = 0 then
              Include(FErrorCodes, teNoReply)
            else
            if ReceiveCount > 1 then
              Include(FErrorCodes, teUnExpectedReply);
            SendNext := True;
            Inc(FStateMachineIndex);
            Inc(iObjective);
          end
        end;
    2 : begin
          MessageHelper.Load(ol_OpenLCB, MTI_PROTOCOL_SUPPORT_INQUIRY, Settings.ProxyNodeAlias, $001, 2, 0, 0, 0, 0, 0 ,0 ,0 ,0);  // TODO: NEED UNIQUE ALIAS NODE ID HERE
          SendStrings.Add(MessageHelper.Encode);

          ReceiveCount := 0;
          Inc(FStateMachineIndex);
        end;
    3 : begin
          // Should be no response from any node
          if ReceiveString <> '' then
          begin
            Inc(FReceiveCount);
            SendNext := False;        // Spin on receive until nothing is returned
          end else
          begin
            if ReceiveCount > 0 then
              Include(FErrorCodes, teUnExpectedReply);
            SendNext := True;
            Inc(FStateMachineIndex);
            Inc(iObjective);
          end
        end;
    4 : begin
          // Send the Protocol Identification message start bit for expansion
          MessageHelper.Load(ol_OpenLCB, MTI_PROTOCOL_SUPPORT_INQUIRY, Settings.ProxyNodeAlias, (Settings.TargetNodeAlias) or PIP_EXTENSION_BIT_START, 2, 0, 0, 0, 0, 0 ,0 ,0 ,0);
          SendStrings.Add(MessageHelper.Encode);

          ReceiveCount := 0;
          Inc(FStateMachineIndex);
        end;
    5 : begin
          if ReceiveString <> '' then
          begin
            if ReceiveCount = 0 then
            begin
              MessageHelper.Decompose(ReceiveString);
              ValidateBasicReturnMessage(MTI_PROTOCOL_SUPPORT_REPLY, MessageHelper);
              PipMask := MessageHelper.ExtractDataBytesAsInt(2, 7);
              if PipMask and PIP_RESERVED_MASK <> 0 then
                Include(FErrorCodesPip, tepPipUsingReservedBits);
              if PipMask and PIP_UNASSIGNED_MASK <> 0 then
                Include(FErrorCodesPip, tepPipUsingUnassignedBits);
              StartEndResponse := True;
            end;
            Inc(FReceiveCount);
            SendNext := False;        // Spin on receive until nothing is returned
          end else
          begin
            if ReceiveCount > 1 then       // Zero or One is okay
              Include(FErrorCodes, teUnExpectedReply);
            SendNext := True;
            Inc(FStateMachineIndex);
          end
        end;
    6 : begin
          // Send the Protocol Identification message end bit for expansion
          MessageHelper.Load(ol_OpenLCB, MTI_PROTOCOL_SUPPORT_INQUIRY, Settings.ProxyNodeAlias, (Settings.TargetNodeAlias) or PIP_EXTENSION_BIT_END, 2, 0, 0, 0, 0, 0 ,0 ,0 ,0);
          SendStrings.Add(MessageHelper.Encode);

          ReceiveCount := 0;
          Inc(FStateMachineIndex);
        end;
    7 : begin
          if ReceiveString <> '' then
          begin
            if (ReceiveCount = 0) then
            begin
              if StartEndResponse then
              begin
                Include(FErrorCodesPip, tepPipStartEndBitSupport)
              end else
              begin
                MessageHelper.Decompose(ReceiveString);
                ValidateBasicReturnMessage(MTI_PROTOCOL_SUPPORT_REPLY, MessageHelper);
                PipMask := MessageHelper.ExtractDataBytesAsInt(2, 7);
                if PipMask and PIP_RESERVED_MASK <> 0 then
                  Include(FErrorCodesPip, tepPipUsingReservedBits);
                if PipMask and PIP_UNASSIGNED_MASK <> 0 then
                  Include(FErrorCodesPip, tepPipUsingUnassignedBits);
              end
            end;
            Inc(FReceiveCount);
            SendNext := False;        // Spin on receive until nothing is returned
          end else
          begin
            if ReceiveCount > 1 then       // Zero or One is okay
              Include(FErrorCodes, teUnExpectedReply);
            SendNext := True;
            Inc(FStateMachineIndex);
            Inc(iObjective);
          end
        end;
  end;
  UpdateProgressBar(7, StateMachineIndex);
end;

{ TTestVerifyNodeID }

procedure TTestVerifyNodeID.ProcessObjectives(Thread: TThread; SendStrings: TStringList; ReceiveString: String; var iObjective: Integer; var ReadTimeout: Integer; ReceiveTime: Integer; var SendNext: Boolean);
begin
  inherited ProcessObjectives(Thread, SendStrings, ReceiveString, iObjective, ReadTimeout, ReceiveTime, SendNext);
  case StateMachineIndex of
    0 : begin
          // Send a Global Verify Node ID with no target Full Node ID in the Data
          MessageHelper.Load(ol_OpenLCB, MTI_VERIFY_NODE_ID_NUMBER, Settings.ProxyNodeAlias, 0, 0, 0, 0, 0, 0, 0 ,0 ,0 ,0);
          SendStrings.Add(MessageHelper.Encode);

          ReceiveCount := 0;
          Inc(FStateMachineIndex);
        end;
    1: begin
          // Receive Nodes that responded with Global Verifed Node ID
          if ReceiveString <> '' then
          begin
            if ReceiveCount = 0 then
            begin
              MessageHelper.Decompose(ReceiveString);
              ValidateBasicReturnMessage(MTI_VERIFIED_NODE_ID_NUMBER, MessageHelper);
              if MessageHelper.ExtractDataBytesAsInt(0, 5) <> Settings.TargetNodeID then
                Include(FErrorCodes, teFullNodeIDInvalid);
            end;
            Inc(FReceiveCount);
            SendNext := False;        // Spin on receive until nothing is returned
          end else
          begin
            if ReceiveCount = 0 then
              Include(FErrorCodes, teNoReply)
            else
            if ReceiveCount > 1 then
              Include(FErrorCodes, teUnExpectedReply);
            SendNext := True;
            Inc(FStateMachineIndex);
            Inc(iObjective);
          end
       end;
    2 : begin
          // Send a Global Verify Node ID with our target Full Node ID in the Data
          MessageHelper.Load(ol_OpenLCB, MTI_VERIFY_NODE_ID_NUMBER, Settings.ProxyNodeAlias, 0, 0, 0, 0, 0, 0, 0 ,0 ,0 ,0);
          MessageHelper.StoreNodeIDToData(Settings.TargetNodeID, False);
          SendStrings.Add(MessageHelper.Encode);

          ReceiveCount := 0;
          Inc(FStateMachineIndex);
        end;
    3 : begin
          // Receive Nodes only our test node should respond with Global Verifed Node ID
          if ReceiveString <> '' then
          begin
            if ReceiveCount = 0 then
            begin
              MessageHelper.Decompose(ReceiveString);
              ValidateBasicReturnMessage(MTI_VERIFIED_NODE_ID_NUMBER, MessageHelper);
              if MessageHelper.ExtractDataBytesAsInt(0, 5) <> Settings.TargetNodeID then
                Include(FErrorCodes, teFullNodeIDInvalid);
            end;
            Inc(FReceiveCount);
            SendNext := False;        // Spin on receive until nothing is returned
          end else
          begin
            if ReceiveCount = 0 then
              Include(FErrorCodes, teNoReply)
            else
            if ReceiveCount > 1 then
              Include(FErrorCodes, teUnExpectedReply);
            SendNext := True;
            Inc(FStateMachineIndex);
            Inc(iObjective);
          end
        end;
    4 : begin
          // Send a Global Verify Node ID with a non-existent target Full Node ID in the Data
          MessageHelper.Load(ol_OpenLCB, MTI_VERIFY_NODE_ID_NUMBER, Settings.ProxyNodeAlias, 0, 6, 0, 0, 0, 0, 0 ,1 ,0 ,0);  // TODO: NEED UNIQUE NODE ID HERE
          SendStrings.Add(MessageHelper.Encode);

          ReceiveCount := 0;
          Inc(FStateMachineIndex);
        end;
    5: begin
          // Should be no response from any node
          if ReceiveString <> '' then
          begin
            Inc(FReceiveCount);
            SendNext := False;        // Spin on receive until nothing is returned
          end else
          begin
            if ReceiveCount > 0 then
              Include(FErrorCodes, teUnExpectedReply);
            SendNext := True;
            Inc(FStateMachineIndex);
            Inc(iObjective);
          end
       end;
    6 : begin
          // Send a Addressed Verify Node ID with no data
          MessageHelper.Load(ol_OpenLCB, MTI_VERIFY_NODE_ID_NUMBER_DEST, Settings.ProxyNodeAlias, Settings.TargetNodeAlias, 2, 0, 0, 0, 0, 0 ,0 ,0 ,0);
          SendStrings.Add(MessageHelper.Encode);

          ReceiveCount := 0;
          Inc(FStateMachineIndex);
        end;
    7: begin
          // Should be one and only one response from any node
          if ReceiveString <> '' then
          begin
            if ReceiveCount = 0 then
            begin
              MessageHelper.Decompose(ReceiveString);
              ValidateBasicReturnMessage(MTI_VERIFIED_NODE_ID_NUMBER, MessageHelper);
              if MessageHelper.ExtractDataBytesAsInt(0, 5) <> Settings.TargetNodeID then
                Include(FErrorCodes, teFullNodeIDInvalid);
            end;
            Inc(FReceiveCount);
            SendNext := False;        // Spin on receive until nothing is returned
          end else
          begin
            if ReceiveCount = 0 then
              Include(FErrorCodes, teNoReply)
            else
            if ReceiveCount > 1 then
              Include(FErrorCodes, teUnExpectedReply);
            SendNext := True;
            Inc(FStateMachineIndex);
            Inc(iObjective);
          end
        end;
    8 : begin
          // Send a Addressed Verify Node ID with target Full Node ID in the Data
          MessageHelper.Load(ol_OpenLCB, MTI_VERIFY_NODE_ID_NUMBER_DEST, Settings.ProxyNodeAlias, Settings.TargetNodeAlias, 2, 0, 0, 0, 0, 0 ,0 ,0 ,0);
          MessageHelper.StoreNodeIDToData(Settings.TargetNodeID, True);
          SendStrings.Add(MessageHelper.Encode);

          ReceiveCount := 0;
          Inc(FStateMachineIndex);
        end;
    9: begin
          // Should be one and only one response from any node
          if ReceiveString <> '' then
          begin
            if ReceiveCount = 0 then
            begin
              MessageHelper.Decompose(ReceiveString);
              ValidateBasicReturnMessage(MTI_VERIFIED_NODE_ID_NUMBER, MessageHelper);
              if MessageHelper.ExtractDataBytesAsInt(0, 5) <> Settings.TargetNodeID then
                Include(FErrorCodes, teFullNodeIDInvalid);
            end;
            Inc(FReceiveCount);
            SendNext := False;        // Spin on receive until nothing is returned
          end else
          begin
            if ReceiveCount = 0 then
              Include(FErrorCodes, teNoReply)
            else
            if ReceiveCount > 1 then
              Include(FErrorCodes, teUnExpectedReply);
            SendNext := True;
            Inc(FStateMachineIndex);
            Inc(iObjective);
          end
        end;
   10 : begin
          // Send a Addressed Verify Node ID with non-existent Full Node ID in the Data
          MessageHelper.Load(ol_OpenLCB, MTI_VERIFY_NODE_ID_NUMBER_DEST, Settings.ProxyNodeAlias, Settings.TargetNodeAlias, 8, 0, 0, 0, 0, 0 ,0 ,0 ,1);  // TODO: NEED UNIQUE NODE ID HERE
          SendStrings.Add(MessageHelper.Encode);

          ReceiveCount := 0;
          Inc(FStateMachineIndex);
        end;
    11: begin
          // Should be one and only one response from any node
          if ReceiveString <> '' then
          begin
            if ReceiveCount = 0 then
            begin
              MessageHelper.Decompose(ReceiveString);
              ValidateBasicReturnMessage(MTI_VERIFIED_NODE_ID_NUMBER, MessageHelper);
              if MessageHelper.ExtractDataBytesAsInt(0, 5) <> Settings.TargetNodeID then
                Include(FErrorCodes, teFullNodeIDInvalid);
            end;
            Inc(FReceiveCount);
            SendNext := False;        // Spin on receive until nothing is returned
          end else
          begin
            if ReceiveCount = 0 then
              Include(FErrorCodes, teNoReply)
            else
            if ReceiveCount > 1 then
              Include(FErrorCodes, teUnExpectedReply);
            SendNext := True;
            Inc(FStateMachineIndex);
            Inc(iObjective);
          end
       end;
    12 : begin
          // Send an incorrectly Addressed Verify Node ID with no data
          MessageHelper.Load(ol_OpenLCB, MTI_VERIFY_NODE_ID_NUMBER_DEST, Settings.ProxyNodeAlias, $001, 2, 0, 0, 0, 0, 0 ,0 ,0 ,0);   // TODO: NEED UNIQUE ALIAS NODE ID HERE
          SendStrings.Add(MessageHelper.Encode);

          ReceiveCount := 0;
          Inc(FStateMachineIndex);
        end;
    13: begin
          // Should be no response from any node
          if ReceiveString <> '' then
          begin
            Inc(FReceiveCount);
            SendNext := False;        // Spin on receive until nothing is returned
          end else
          begin
            if ReceiveCount > 0 then
              Include(FErrorCodes, teUnExpectedReply);
            SendNext := True;
            Inc(FStateMachineIndex);
            Inc(iObjective);
          end
       end;
    14 : begin
          // Send an incorrectly Addressed Verify Node ID with target Full Node ID in the Data
          MessageHelper.Load(ol_OpenLCB, MTI_VERIFY_NODE_ID_NUMBER_DEST, Settings.ProxyNodeAlias, $001, 2, 0, 0, 0, 0, 0 ,0 ,0 ,0);   // TODO: NEED UNIQUE ALIAS NODE ID HERE
          MessageHelper.StoreNodeIDToData(Settings.TargetNodeID, True);
          SendStrings.Add(MessageHelper.Encode);

          ReceiveCount := 0;
          Inc(FStateMachineIndex);
        end;
    15: begin
          // Should be no response from any node
          if ReceiveString <> '' then
          begin
            Inc(FReceiveCount);
            SendNext := False;        // Spin on receive until nothing is returned
          end else
          begin
            if ReceiveCount > 0 then
              Include(FErrorCodes, teUnExpectedReply);
            SendNext := True;
            Inc(FStateMachineIndex);
            Inc(iObjective);
          end
        end;
   16 : begin
          // Send an incorrectly Addressed Verify Node ID with non-existent Full Node ID in the Data
          MessageHelper.Load(ol_OpenLCB, MTI_VERIFY_NODE_ID_NUMBER_DEST, Settings.ProxyNodeAlias, $001, 8, 0, 0, 0, 0, 0 ,0 ,0 ,1);  // TODO: NEED UNIQUE ALIAS NODE ID HERE
          SendStrings.Add(MessageHelper.Encode);                                                                                  // TODO: NEED ALIAS NODE ID HERE

          ReceiveCount := 0;
          Inc(FStateMachineIndex);
        end;
    17: begin
          // Should be no response from any node
          if ReceiveString <> '' then
          begin
            Inc(FReceiveCount);
            SendNext := False;        // Spin on receive until nothing is returned
          end else
          begin
            if ReceiveCount > 0 then
              Include(FErrorCodes, teUnExpectedReply);
            SendNext := True;
            Inc(FStateMachineIndex);
            Inc(iObjective);
          end
       end;
  end;
  UpdateProgressBar(17, StateMachineIndex);
end;

{ TTestAliasMapEnquiry }

procedure TTestAliasMapEnquiry.ProcessObjectives(Thread: TThread; SendStrings: TStringList; ReceiveString: String; var iObjective: Integer; var ReadTimeout: Integer; ReceiveTime: Integer; var SendNext: Boolean);
begin
  inherited ProcessObjectives(Thread, SendStrings, ReceiveString, iObjective, ReadTimeout, ReceiveTime, SendNext);
  case StateMachineIndex of
    0 : begin
          // Send Alias Mapping Enquiry with nothing in the CAN data bytes, all nodes should respond
          MessageHelper.Load(ol_CAN, MTI_AME, Settings.ProxyNodeAlias, 0, 0, 0, 0, 0, 0, 0 ,0 ,0 ,0);
          SendSTrings.Add(MessageHelper.Encode);

          ReceiveCount := 0;
          Inc(FStateMachineIndex);
        end;
    1 : begin
          if ReceiveString <> '' then
          begin
            if ReceiveCount = 0 then
            begin
              MessageHelper.Decompose(ReceiveString);
              ValidateBasicReturnMessage(MTI_AMD, MessageHelper);
              if MessageHelper.ExtractDataBytesAsInt(0, 5) <> Settings.TargetNodeID then
                Include(FErrorCodes, teFullNodeIDInvalid);
            end;
            Inc(FReceiveCount);
            SendNext := False;        // Spin on receive until nothing is returned
          end else
          begin
            if ReceiveCount = 0 then
              Include(FErrorCodes, teNoReply)
            else
            if ReceiveCount > 1 then
              Include(FErrorCodes, teUnExpectedReply);
            SendNext := True;
            Inc(FStateMachineIndex);
            Inc(iObjective);
          end
       end;
    2 : begin
         // Send Alias Mapping Enquiry with a full Node ID in the CAN data bytes
          MessageHelper.Load(ol_CAN, MTI_AME, Settings.ProxyNodeAlias, 0, 0, 0, 0, 0, 0, 0 ,0 ,0 ,0);
          MessageHelper.StoreNodeIDToData(Settings.TargetNodeID, False);
          SendStrings.Add(MessageHelper.Encode);

          ReceiveCount := 0;
          Inc(FStateMachineIndex);
        end;
    3 : begin
          // Should be one and only one response from node node under test
          if ReceiveString <> '' then
          begin
            if ReceiveCount = 0 then
            begin
              MessageHelper.Decompose(ReceiveString);
              ValidateBasicReturnMessage(MTI_AMD, MessageHelper);
              if MessageHelper.ExtractDataBytesAsInt(0, 5) <> Settings.TargetNodeID then
                Include(FErrorCodes, teFullNodeIDInvalid);
            end;
            Inc(FReceiveCount);
            SendNext := False;        // Spin on receive until nothing is returned
          end else
          begin
            if ReceiveCount = 0 then
              Include(FErrorCodes, teNoReply)
            else
            if ReceiveCount > 1 then
              Include(FErrorCodes, teUnExpectedReply);
            SendNext := True;
            Inc(FStateMachineIndex);
            Inc(iObjective);
          end
       end;
    4 : begin
          // Send ProcessObjectives
          MessageHelper.Load(ol_CAN, MTI_AME, Settings.ProxyNodeAlias, 0, 6, 0, 0, 0, 0, 0 ,1 ,0 ,0);   // NEED TO FIND A UNIQUE NODE ID HERE.....
          SendStrings.Add(MessageHelper.Encode);

          ReceiveCount := 0;
          Inc(FStateMachineIndex);
        end;
    5 : begin
          // Should be no response from any node
          if ReceiveString <> '' then
          begin
            Inc(FReceiveCount);
            SendNext := False;        // Spin on receive until nothing is returned
          end else
          begin
            if ReceiveCount > 0 then
              Include(FErrorCodes, teUnExpectedReply);
            SendNext := True;
            Inc(FStateMachineIndex);
            Inc(iObjective);
          end
        end;
  end;
  UpdateProgressBar(5, StateMachineIndex);
end;

{ TTestGetNodesUnderTest }

constructor TTestGetNodesUnderTest.Create;
begin
  inherited Create;
  FFilterRepliesForNUT := False;
end;

procedure TTestGetNodesUnderTest.ProcessObjectives(Thread: TThread; SendStrings: TStringList; ReceiveString: String; var iObjective: Integer; var ReadTimeout: Integer; ReceiveTime: Integer; var SendNext: Boolean);
begin
  inherited ProcessObjectives(Thread, SendStrings, ReceiveString, iObjective, ReadTimeout, ReceiveTime, SendNext);
  case StateMachineIndex of
    0 : begin
          // Send Global Verify Nodes to collect all nodes on the bus
          MessageHelper.Load(ol_OpenLCB, MTI_VERIFY_NODE_ID_NUMBER, Settings.ProxyNodeAlias, 0, 0, 0, 0, 0, 0, 0 ,0 ,0 ,0);
          SendStrings.Add(MessageHelper.Encode);

          ReceiveCount := 0;
          Inc(FStateMachineIndex);
        end;
    1 : begin
          // There is no pass fail here we are just collecting the nodes
          if ReceiveString <> '' then
          begin
            Inc(FReceiveCount);
            SendNext := False;
          end else
          begin
            Inc(FStateMachineIndex);
            Inc(iObjective);
          end;
        end;
  end;
  UpdateProgressBar(1, StateMachineIndex);
end;

{ TTestStartup }

procedure TTestStartup.ProcessObjectives(Thread: TThread; SendStrings: TStringList; ReceiveString: String; var iObjective: Integer; var ReadTimeout: Integer; ReceiveTime: Integer; var SendNext: Boolean);
begin
  inherited ProcessObjectives(Thread, SendStrings, ReceiveString, iObjective, ReadTimeout, ReceiveTime, SendNext);
  case StateMachineIndex of
    0 : begin
          Thread.Synchronize(Thread, @SyncShowNodeResetMessage);
          ReadTimeout := 5000;  // 5 seconds to reset
          ReceiveCount := 0;
          Inc(FStateMachineIndex);
        end;
    1 : begin
          Thread.Synchronize(Thread, @SyncHideNodeResetMessage);
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
            ValidateBasicReturnMessage(MTI_CID0, MessageHelper);
          end else
            Include(FErrorCodesStartup, tsNoCID0);

          if HasAnyErrorCodeSet then
          begin
            StateMachineIndex := 9;
            Inc(iObjective);
          end else
          begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    2 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
            ValidateBasicReturnMessage(MTI_CID1, MessageHelper);
          end else
            Include(FErrorCodesStartup, tsNoCID1);

          if HasAnyErrorCodeSet then
          begin
            StateMachineIndex := 9;
            Inc(iObjective);
          end else
          begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    3 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
            ValidateBasicReturnMessage(MTI_CID2, MessageHelper);
          end else
            Include(FErrorCodesStartup, tsNoCID2);

          if HasAnyErrorCodeSet then
          begin
            StateMachineIndex := 9;
            Inc(iObjective);
          end else
          begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    4 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
            ValidateBasicReturnMessage(MTI_CID3, MessageHelper);
            ReadTimeout := Settings.TimeoutStartupRID;         // Wait extra for the RID per the spec
          end else
            Include(FErrorCodesStartup, tsNoCID3);

          if HasAnyErrorCodeSet then
          begin
            StateMachineIndex := 9;
            Inc(iObjective);
          end else
          begin
            ReceiveTime := Settings.TimeoutStartupRID;
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    5 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_RID, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
          end else
            Include(FErrorCodesStartup, tsNoRID);

          if HasAnyErrorCodeSet then
          begin
            StateMachineIndex := 9;
            Inc(iObjective);
          end else
          begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    6 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_AMD, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
            if not HasAnyErrorCodeSet then
            begin
              Thread.Synchronize(Thread, @SyncAliasChanging);
              Settings.TargetNodeAlias := MessageHelper.SourceAliasID;
              Settings.TargetNodeID := MessageHelper.ExtractDataBytesAsInt(0, 5);
              Thread.Synchronize(Thread, @SyncAliasChanged);
            end;
          end else
            Include(FErrorCodesStartup, tsNoAMD);

          if HasAnyErrorCodeSet then
          begin
            StateMachineIndex := 9;
            Inc(iObjective);
          end else
          begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    7 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_INITIALIZATION_COMPLETE, MessageHelper);
          end else
            Include(FErrorCodesStartup, tsNoInitialized);

          if HasAnyErrorCodeSet then
          begin
            StateMachineIndex := 9;
            Inc(iObjective);
          end else
          begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
            Inc(iObjective);
          end
        end;
  end;
  UpdateProgressBar(7, StateMachineIndex);
end;

{ TTestZeroOrRepeatedAllocation }

constructor TTestZeroOrRepeatedAllocation.Create;
begin
  inherited Create;
  FAliasList := TIntegerList.Create;
  AliasList.Count := MAX_ALIAS_VALUES;
end;

destructor TTestZeroOrRepeatedAllocation.Destroy;
begin
  FreeAndNil(FAliasList);
  inherited Destroy;
end;

procedure TTestZeroOrRepeatedAllocation.InitTest;
begin
  inherited InitTest;
  FReAllocationCount := 0;
  AliasList.Clear;
  AliasList.Capacity := MAX_ALIAS_VALUES;
  AliasList.SortType := IntegerSortNone;
  AliasList.Add( Settings.TargetNodeAlias);
end;

procedure TTestZeroOrRepeatedAllocation.ProcessObjectives(Thread: TThread; SendStrings: TStringList; ReceiveString: String; var iObjective: Integer; var ReadTimeout: Integer; ReceiveTime: Integer; var SendNext: Boolean);
var
  i: Integer;
begin
  inherited ProcessObjectives(Thread, SendStrings, ReceiveString, iObjective, ReadTimeout, ReceiveTime, SendNext);
  case StateMachineIndex of
    0 : begin
          // Send Global Verify Nodes but use the NUT Alias as the source from NodeExplorer to force the NUT to reallocate its Alias
          if ReAllocationCount mod Settings.UIRefreshRate = 0 then
            UpdateProgressBar(MAX_ALIAS_VALUES, ReAllocationCount);
          MessageHelper.Load(ol_OpenLCB, MTI_VERIFY_NODE_ID_NUMBER, Settings.TargetNodeAlias, 0, 0, 0, 0, 0, 0, 0 ,0 ,0 ,0);
          SendStrings.Add(MessageHelper.Encode);

          ReceiveCount := 0;
          Inc(FStateMachineIndex);
        end;
    1 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_AMR, MessageHelper);
          end else
            Include(FErrorCodesStartup, tsNoAMR);

          if HasAnyErrorCodeSet then
          begin
            StateMachineIndex := 9;
            Inc(iObjective);
          end else
          begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    2 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_CID0, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
          end else
            Include(FErrorCodesStartup, tsNoCID0);

          if HasAnyErrorCodeSet then
          begin
            StateMachineIndex := 9;
            Inc(iObjective);
          end else
          begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    3 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_CID1, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
          end else
            Include(FErrorCodesStartup, tsNoCID1);

          if HasAnyErrorCodeSet then
          begin
            StateMachineIndex := 9;
            Inc(iObjective);
          end else
          begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    4 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_CID2, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
          end else
            Include(FErrorCodesStartup, tsNoCID2);

          if HasAnyErrorCodeSet then
          begin
            StateMachineIndex := 9;
            Inc(iObjective);
          end else
          begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    5 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_CID3, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
            ReadTimeout := Settings.TimeoutStartupRID;         // Wait extra for the RID per the spec
          end else
            Include(FErrorCodesStartup, tsNoCID3);

          if HasAnyErrorCodeSet then
          begin
            StateMachineIndex := 9;
            Inc(iObjective);
          end else
          begin
            ReceiveTime := Settings.TimeoutStartupRID;
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    6 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_RID, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
          end else
            Include(FErrorCodesStartup, tsNoRID);
          if HasAnyErrorCodeSet then
          begin
            StateMachineIndex := 9;
            Inc(iObjective);
          end else
          begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    7 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_AMD, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
            if not HasAnyErrorCodeSet then
            begin
              Thread.Synchronize(Thread, @SyncAliasChanging);
              Settings.TargetNodeAlias := MessageHelper.SourceAliasID;
              Settings.TargetNodeID := MessageHelper.ExtractDataBytesAsInt(0, 5);
              Thread.Synchronize(Thread, @SyncAliasChanged);
              AliasList.Add( Settings.TargetNodeAlias);
            end;
          end else
            Include(FErrorCodesStartup, tsNoAMD);

          if HasAnyErrorCodeSet then
          begin
            StateMachineIndex := 9;
            Inc(iObjective);
          end else
          begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    8 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_INITIALIZATION_COMPLETE, MessageHelper);
          end else
            Include(FErrorCodesStartup, tsNoInitialized);

          if HasAnyErrorCodeSet then
          begin
            Inc(FStateMachineIndex);
            Inc(iObjective);
          end else
          begin
            SendNext := False;
            Inc(FReAllocationCount);
            Inc(FStateMachineIndex);
          end
        end;
    9 : begin
          if ReceiveString <> '' then
          begin
            SendNext := False;  // Pump it until done
          end else
          begin
            if ReAllocationCount < MAX_ALIAS_VALUES then  // Node started with an alias so it is MAX_ALIAS_VALUES - 1
              StateMachineIndex := 0
            else begin
              for i := 0 to AliasList.Count - 1 do
              begin
                if AliasList[i] = 0 then
                  Include(FErrorAliasConflict, taZeroAlias);
                if i > 0 then
                begin
                  if AliasList[i-1] = AliasList[i] then
                    Include(FErrorAliasConflict, taDuplicateAlias);
                end
              end;
              UpdateProgressBar(MAX_ALIAS_VALUES, MAX_ALIAS_VALUES);
              Inc(FStateMachineIndex);
              Inc(iObjective)
            end;
          end
        end;
  end;
end;

{ TTestAliasConflict }

procedure TTestAliasConflict.ProcessObjectives(Thread: TThread; SendStrings: TStringList; ReceiveString: String; var iObjective: Integer; var ReadTimeout: Integer; ReceiveTime: Integer; var SendNext: Boolean);

  procedure BypassTest;
  begin
    StateMachineIndex := 1000;
    iObjective := 1000;
  end;

begin
  inherited ProcessObjectives(Thread, SendStrings, ReceiveString, iObjective, ReadTimeout, ReceiveTime, SendNext);
  case StateMachineIndex of
    0 : begin
          // Send Global Verify Nodes but use the NUT Alias as the source from NodeExplorer to force the NUT to reallocate its Alias
          MessageHelper.Load(ol_OpenLCB, MTI_VERIFY_NODE_ID_NUMBER, Settings.TargetNodeAlias, 0, 6, 0, 0, 0, 0, 0 ,1 ,0 ,0);  // TODO: NEED UNIQUE NODE ID HERE
          SendStrings.Add(MessageHelper.Encode);

          ReceiveCount := 0;
          Inc(FStateMachineIndex);
        end;
    1 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_AMR, MessageHelper);
          end else
            Include(FErrorCodesStartup, tsNoAMR);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    2 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_CID0, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
          end else
            Include(FErrorCodesStartup, tsNoCID0);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    3 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_CID1, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
          end else
            Include(FErrorCodesStartup, tsNoCID1);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    4 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_CID2, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
          end else
            Include(FErrorCodesStartup, tsNoCID2);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    5 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_CID3, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
            ReadTimeout := Settings.TimeoutStartupRID;         // Wait extra for the RID per the spec
          end else
            Include(FErrorCodesStartup, tsNoCID3);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            ReceiveTime := Settings.TimeoutStartupRID;
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    6 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_RID, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
          end else
            Include(FErrorCodesStartup, tsNoRID);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    7 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_AMD, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
            if not HasAnyErrorCodeSet then
            begin
              Thread.Synchronize(Thread, @SyncAliasChanging);
              Settings.TargetNodeAlias := MessageHelper.SourceAliasID;
              Settings.TargetNodeID := MessageHelper.ExtractDataBytesAsInt(0, 5);
              Thread.Synchronize(Thread, @SyncAliasChanged);
            end;
          end else
            Include(FErrorCodesStartup, tsNoAMD);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    8 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_INITIALIZATION_COMPLETE, MessageHelper);
          end else
            Include(FErrorCodesStartup, tsNoInitialized);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            SendNext := False;
            Inc(FStateMachineIndex);
          end
        end;
    9 : begin
          if ReceiveString <> '' then
          begin
            SendNext := False;  // Pump it until done
          end else
          begin
            Inc(FStateMachineIndex);
            Inc(iObjective)
          end
        end;


    10 : begin
          // Send Global Verify Nodes but use the NUT Alias as the source from NodeExplorer to force the NUT to reallocate its Alias
          MessageHelper.Load(ol_OpenLCB, MTI_VERIFY_NODE_ID_NUMBER, Settings.TargetNodeAlias, 0, 0, 0, 0, 0, 0, 0 ,0 ,0 ,0);
          SendStrings.Add(MessageHelper.Encode);

          ReceiveCount := 0;
          Inc(FStateMachineIndex);
        end;
    11 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_AMR, MessageHelper);
          end else
            Include(FErrorCodesStartup, tsNoAMR);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    12 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_CID0, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
          end else
            Include(FErrorCodesStartup, tsNoCID0);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    13 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_CID1, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
          end else
            Include(FErrorCodesStartup, tsNoCID1);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    14 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_CID2, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
          end else
            Include(FErrorCodesStartup, tsNoCID2);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    15 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_CID3, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
            ReadTimeout := Settings.TimeoutStartupRID;         // Wait extra for the RID per the spec
          end else
            Include(FErrorCodesStartup, tsNoCID3);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            ReceiveTime := Settings.TimeoutStartupRID;
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    16 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_RID, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
          end else
            Include(FErrorCodesStartup, tsNoRID);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    17 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_AMD, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
            if not HasAnyErrorCodeSet then
            begin
              Thread.Synchronize(Thread, @SyncAliasChanging);
              Settings.TargetNodeAlias := MessageHelper.SourceAliasID;
              Settings.TargetNodeID := MessageHelper.ExtractDataBytesAsInt(0, 5);
              Thread.Synchronize(Thread, @SyncAliasChanged);
            end;
          end else
            Include(FErrorCodesStartup, tsNoAMD);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    18 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_INITIALIZATION_COMPLETE, MessageHelper);
          end else
            Include(FErrorCodesStartup, tsNoInitialized);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            SendNext := False;
            Inc(FStateMachineIndex);
          end
        end;
    19 : begin
          if ReceiveString <> '' then
          begin
            SendNext := False;  // Pump it until done
          end else
          begin
            Inc(FStateMachineIndex);
            Inc(iObjective)
          end
        end;

    20 : begin
          // Send a Addressed Verify Node ID with no data and the Target/Proxy reversed
          MessageHelper.Load(ol_OpenLCB, MTI_VERIFY_NODE_ID_NUMBER_DEST, Settings.TargetNodeAlias, Settings.ProxyNodeAlias, 2, 0, 0, 0, 0, 0 ,0 ,0 ,0);
          SendStrings.Add(MessageHelper.Encode);

          ReceiveCount := 0;
          Inc(FStateMachineIndex);
        end;
    21 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_AMR, MessageHelper);
          end else
            Include(FErrorCodesStartup, tsNoAMR);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    22 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_CID0, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
          end else
            Include(FErrorCodesStartup, tsNoCID0);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    23 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_CID1, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
          end else
            Include(FErrorCodesStartup, tsNoCID1);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    24 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_CID2, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
          end else
            Include(FErrorCodesStartup, tsNoCID2);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    25 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_CID3, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
            ReadTimeout := Settings.TimeoutStartupRID;         // Wait extra for the RID per the spec
          end else
            Include(FErrorCodesStartup, tsNoCID3);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            ReceiveTime := Settings.TimeoutStartupRID;
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    26 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_RID, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
          end else
            Include(FErrorCodesStartup, tsNoRID);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    27 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_AMD, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
            if not HasAnyErrorCodeSet then
            begin
              Thread.Synchronize(Thread, @SyncAliasChanging);
              Settings.TargetNodeAlias := MessageHelper.SourceAliasID;
              Settings.TargetNodeID := MessageHelper.ExtractDataBytesAsInt(0, 5);
              Thread.Synchronize(Thread, @SyncAliasChanged);
            end;
          end else
            Include(FErrorCodesStartup, tsNoAMD);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    28 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_INITIALIZATION_COMPLETE, MessageHelper);
          end else
            Include(FErrorCodesStartup, tsNoInitialized);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            SendNext := False;
            Inc(FStateMachineIndex);
          end
        end;
    29 : begin
          if ReceiveString <> '' then
          begin
            SendNext := False;  // Pump it until done
          end else
          begin
            Inc(FStateMachineIndex);
            Inc(iObjective)
          end

        end;

    30 : begin
          // Pretend we want to use the Target Alias with our Node ID and try to register it
          MessageHelper.Load(ol_CAN, MTI_CID0 or ((QWord(Settings.ProxyNodeID) and $FFF000000000) shr 24), Settings.TargetNodeAlias, 0, 0, 0, 0, 0, 0, 0 ,0 ,0 ,0);
          SendStrings.Add(MessageHelper.Encode);

          ReceiveCount := 0;
          Inc(FStateMachineIndex);
        end;
    31 : begin
          if ReceiveString <> '' then
          begin
            if ReceiveCount = 0 then
            begin
              MessageHelper.Decompose(ReceiveString);
              ValidateBasicReturnMessage(MTI_RID, MessageHelper);
            end;
            Inc(FReceiveCount);
            SendNext := False;        // Spin on receive until nothing is returned
          end else
          begin
            if ReceiveCount = 0 then
              Include(FErrorCodes, teNoReply)
            else
            if ReceiveCount > 1 then
              Include(FErrorCodes, teUnExpectedReply);
            SendNext := True;
            Inc(FStateMachineIndex);
            Inc(iObjective);
          end
        end;

    32 : begin
          // Send a Reserve ID with the NUT Alias in conflict
          MessageHelper.Load(ol_OpenLCB, MTI_RID, Settings.TargetNodeAlias, 0, 0, 0, 0, 0, 0, 0 ,0 ,0 ,0);
          MessageHelper.StoreNodeIDToData(Settings.TargetNodeAlias, False);
          SendStrings.Add(MessageHelper.Encode);

          ReceiveCount := 0;
          Inc(FStateMachineIndex);
        end;
    33 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_AMR, MessageHelper);
          end else
            Include(FErrorCodesStartup, tsNoAMR);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    34 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_CID0, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
          end else
            Include(FErrorCodesStartup, tsNoCID0);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    35 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_CID1, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
          end else
            Include(FErrorCodesStartup, tsNoCID1);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    36 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_CID2, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
          end else
            Include(FErrorCodesStartup, tsNoCID2);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    37 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_CID3, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
            ReadTimeout := Settings.TimeoutStartupRID;         // Wait extra for the RID per the spec
          end else
            Include(FErrorCodesStartup, tsNoCID3);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            ReceiveTime := Settings.TimeoutStartupRID;
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    38 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_RID, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
          end else
            Include(FErrorCodesStartup, tsNoRID);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    39 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_AMD, MessageHelper);
            Exclude(FErrorCodesFormat, tefInvalidSourceAlias); // Ignore the Alias for now
            if not HasAnyErrorCodeSet then
            begin
              Thread.Synchronize(Thread, @SyncAliasChanging);
              Settings.TargetNodeAlias := MessageHelper.SourceAliasID;
              Settings.TargetNodeID := MessageHelper.ExtractDataBytesAsInt(0, 5);
              Thread.Synchronize(Thread, @SyncAliasChanged);
            end;
          end else
            Include(FErrorCodesStartup, tsNoAMD);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            SendNext := False;       // Only listening in this test
            Inc(FStateMachineIndex);
          end
        end;
    40 : begin
          if ReceiveString <> '' then
          begin
            MessageHelper.Decompose(ReceiveString);
            ValidateBasicReturnMessage(MTI_INITIALIZATION_COMPLETE, MessageHelper);
          end else
            Include(FErrorCodesStartup, tsNoInitialized);

          if HasAnyErrorCodeSet then
            BypassTest
          else begin
            SendNext := False;
            Inc(FStateMachineIndex);
          end
        end;
    41 : begin
          if ReceiveString <> '' then
          begin
            SendNext := False;  // Pump it until done
          end else
          begin
            Inc(FStateMachineIndex);
            Inc(iObjective)
          end

        end;
  end;
  UpdateProgressBar(1, StateMachineIndex);
end;

{ TTestEvents }

procedure TTestEvents.ProcessObjectives(Thread: TThread; SendStrings: TStringList; ReceiveString: String; var iObjective: Integer; var ReadTimeout: Integer; ReceiveTime: Integer; var SendNext: Boolean);
begin
  inherited ProcessObjectives(Thread, SendStrings, ReceiveString, iObjective, ReadTimeout, ReceiveTime, SendNext);
  case StateMachineIndex of
    0 : begin
          // Send Global Verify Nodes to collect all nodes on the bus
     //     MessageHelper.Load(ol_OpenLCB, MTI_VERIFY_NODE_ID_NUMBER, Settings.ProxyNodeAlias, 0, 0, 0, 0, 0, 0, 0 ,0 ,0 ,0);
    //      SendStrings.Add(MessageHelper.Encode);

          ReceiveCount := 0;
          Inc(FStateMachineIndex);
        end;
    1 : begin
          // There is no pass fail here we are just collecting the nodes
          if ReceiveString <> '' then
          begin
            Inc(FReceiveCount);
            SendNext := False;
          end else
          begin
            Inc(FStateMachineIndex);
            Inc(iObjective);
          end;
        end
    else begin
      Inc(iObjective);
    end
  end;
  UpdateProgressBar(1, StateMachineIndex);
end;

{ TTestBase }

function TTestBase.IsMessageForNodeUnderTest(MessageStr: String): Boolean;
var
  TargetNodeAlias: String;
begin
  if MessageStr <> '' then
  begin
    if Settings.MultiNodeTest then
    begin
      TargetNodeAlias := IntToHex(Settings.TargetNodeAlias, 3);
        // example - :X19170aaaN
      Result := (MessageStr[8] = TargetNodeAlias[1]) and (MessageStr[9] = TargetNodeAlias[2]) and (MessageStr[10] = TargetNodeAlias[3])
     end else
       Result := True
  end else
    Result := True
end;

function TTestBase.HasAnyErrorCodeSet: Boolean;
begin
  Result := (ErrorCodes <> []) or (ErrorCodesFormat <> []) or (ErrorCodesPip <> []) or (ErrorCodesUnknownMTI <> []) or
            (ErrorCodesStartup <> []) or (ErrorAliasConflict <> [])
end;

function TTestBase.GetPassed: Boolean;
var
  TestResult, Test, TestObjective, Results, PassFail: TDOMNode;
begin
  if  teCancelled in ErrorCodes then
    Result := False
  else begin
    Result := True;
    TestResult := XMLResults.FindNode(XML_ELEMENT_TEST_RESULT_ROOT);
    if Assigned(TestResult) then
    begin
      Test := TestResult.FindNode(XML_ELEMENT_TEST);
      if Assigned(Test) then
      begin
        TestObjective := Test.FirstChild;
        while Assigned(TestObjective) and Result do
        begin
          if TestObjective.NodeName = XML_ELEMENT_TESTOBJECTIVE then
          begin
            Results := TestObjective.FindNode(XML_ELEMENT_OBJECTIVERESULTS);
            if Assigned(Results) then
            begin
              PassFail := Results.FindNode(XML_ELEMENT_PASS_FAIL);
              if Assigned(PassFail) then
                Result := PassFail.FirstChild.NodeValue = XML_NAME_PASS;
            end;
          end;
          TestObjective := TestObjective.NextSibling;
        end;
      end;
    end;
  end;
end;

procedure TTestBase.UpdateProgressBar(Range, Position: Integer);
begin
  ProgressBarPos := Position;
  ProgressBarRange := Range;
  SyncUpdateProgressBar;
end;

procedure TTestBase.SyncAliasChanging;
begin
  if Assigned(SyncAliasChangingFunc) then
    SyncAliasChangingFunc(Self)
end;

procedure TTestBase.SyncAliasChanged;
begin
  if Assigned(SyncAliasChangedFunc) then
    SyncAliasChangedFunc(Self)
end;

procedure TTestBase.SyncTestComplete;
begin
  if Assigned(SyncTestCompleteFunc) then
    SyncTestCompleteFunc(Self)
end;

procedure TTestBase.SyncShowNodeResetMessage;
begin
  if Assigned(SyncShowNodeResetMessageFunc) then
    SyncShowNodeResetMessageFunc(Self)
end;

procedure TTestBase.SyncHideNodeResetMessage;
begin
   if Assigned(SyncHideNodeResetMessageFunc) then
     SyncHideNodeResetMessageFunc(Self)
end;

procedure TTestBase.SyncUpdateProgressBar;
begin
  if Assigned(SyncUpdateProgressBarFunc) then
    SyncUpdateProgressBarFunc(Self)
end;

constructor TTestBase.Create;
begin
  inherited Create;
  FFreeOnLog := False;
  FMessageHelper := TOpenLCBMessageHelper.Create;
  FXMLResults := TXMLDocument.Create;
  ErrorCodesUnknownMTIStrings := TStringList.Create;
  FTestState := ts_Idle;
  FStateMachineIndex := 0;
  FListItem := nil;
  FErrorCodes := [];
  FSyncAliasChangingFunc := nil;
  FSyncAliasChangedFunc := nil;
  FSyncTestCompleteFunc := nil;
  FFilterRepliesForNUT := True;  // Typical
end;

destructor TTestBase.Destroy;
begin
  FreeAndNil(FMessageHelper);
  FreeAndNil(FXMLResults);
  FreeAndNil(FErrorCodesUnknownMTIStrings);
  FStateMachineIndex := 0;
  inherited Destroy;
end;

procedure TTestBase.ProcessObjectives(Thread: TThread; SendStrings: TStringList; ReceiveString: String; var iObjective: Integer; var ReadTimeout: Integer; ReceiveTime: Integer; var SendNext: Boolean);
begin
  ReadTimeout := Settings.TimeoutComRead;
end;

procedure TTestBase.InitTest;
begin
  ClearErrorCodes;
  FStateMachineIndex := 0;
end;

class function TTestBase.CreateInstanceFromString(AClassname: String): TTestBase;
var
  TestBaseClass: TTestBaseClass;
begin
  Result := nil;
  TestBaseClass := TTestBaseClass( FindClass(AClassname));
  if Assigned(TestBaseClass) then
    Result := TestBaseClass.Create;
end;

procedure TTestBase.ValidateBasicReturnMessage(ExpectedMTI: DWord; Helper: TOpenLCBMessageHelper);
begin
  if Helper.UnimplementedBitsSet then
    Include(FErrorCodesFormat, tefUnusedBitsSet);

  if Helper.ForwardingBitNotSet then
    Include(FErrorCodesFormat, tefForwardingBitNotSet);

  if Helper.MTI <> ExpectedMTI then
    Include(FErrorCodesFormat, tefInvalidMTI);

  if Helper.SourceAliasID <> Settings.TargetNodeAlias then
    Include(FErrorCodesFormat, tefInvalidSourceAlias);

  if Helper.HasDestinationAddress then
    if Helper.DestinationAliasID <> Settings.ProxyNodeAlias then
      Include(FErrorCodesFormat, tefInvalidDestAlias);
end;

procedure TTestBase.ClearErrorCodes;
begin
  ErrorCodes := [];
  ErrorCodesFormat := [];
  ErrorCodesPip := [];
  ErrorCodesUnknownMTI := [];
  ErrorCodesStartup := [];
  ErrorAliasConflict := [];;
  ErrorCodesUnknownMTIStrings.Clear;
end;


initialization
  RegisterClass(TTestGetNodesUnderTest);
  RegisterClass(TTestAliasMapEnquiry);
  RegisterClass(TTestVerifyNodeID);
  RegisterClass(TTestProtocolSupport);
  RegisterClass(TTestStandardFrame);
  RegisterClass(TTestUnknownMTIAddressed);
  RegisterClass(TTestAliasConflict);
  RegisterClass(TTestZeroOrRepeatedAllocation);
  RegisterClass(TTestStartup);
  RegisterClass(TTestEvents);

finalization

end.

