unit serialport_thread;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, synaser, olcb_testmatrix, ExtCtrls, olcb_utilities, DOM, XMLRead, XMLWrite, unitDebugLogger,
  nodeexplorer_settings, dialogs;

const
  UI_UPDATE_RATE = 20;

type

{ TComPortThread }

  TSyncRawMessageFunc = procedure(MessageStr: String) of object;

  TComPortThread = class(TThread)
  private
    FActiveTest: TTestBase;
    FBaudRate: DWord;
    FConnected: Boolean;
    FEnableRawMessages: Boolean;
    FLastErrorDesc: string;
    FPort: String;
    FRawMessageBuffer: string;
    FSerial: TBlockSerial;
    FSyncRawMessageFunc: TSyncRawMessageFunc;
    FTerminatedTest: Boolean;
    FTerminateTest: Boolean;
    FTestCount: Integer;
    FThreadTestList: TThreadList;
    protected
      property ActiveTest: TTestBase read FActiveTest write FActiveTest;
      property RawMessageBuffer: string read FRawMessageBuffer write FRawMessageBuffer;
      procedure Execute; override;
      procedure ErrorCodesToXML(RootXMLElement: TDOMNode);
      procedure ErrorCodesFormatToXML(RootXMLElement: TDOMNode);
      procedure ErrorCodesPipToXML(RootXMLElement: TDOMNode);
      procedure ErrorCodesUnknownMTIToXML(RootXMLElement: TDOMNode);
      procedure ErrorCodesStartupToXML(RootXMLElement: TDOMNode);
      procedure ErrorCodesAliasToXML(RootXMLElement: TDOMNode);
      procedure SyncronizeUpdateUI;
      procedure SyncRawMessage;
    public
      property Connected: Boolean read FConnected write FConnected;
      property Serial: TBlockSerial read FSerial write FSerial;
      property BaudRate: DWord read FBaudRate write FBaudRate;
      property Port: String read FPort write FPort;
      property ThreadTestList: TThreadList read FThreadTestList write FThreadTestList;
      property TerminateTest: Boolean read FTerminateTest write FTerminateTest;
      property TerminatedTest: Boolean read FTerminatedTest;
      property TestCount: Integer read FTestCount;
      property SyncRawMessageFunc: TSyncRawMessageFunc read FSyncRawMessageFunc write FSyncRawMessageFunc;
      property EnableRawMessages: Boolean read FEnableRawMessages write FEnableRawMessages;
      property LastErrorDesc: string read FLastErrorDesc write FLastErrorDesc;
      constructor Create(CreateSuspended: Boolean);
      destructor Destroy; override;
      procedure Add(Test: TTestBase);
  end;


implementation

{ TComPortThread }

procedure TComPortThread.Execute;
var
  List: TList;
  SendNext, ProcessReply: Boolean;
  i, TimeoutRead: Integer;
  ReceiveStr: AnsiString;
  SendStrings, ReceiveStrings: TStringList;
  Objectives: TList;
  iNextObjective, iCurrentObjective: Integer;
  XMLRoot, XMLTestNode, XMLNode, XMLTestObjectiveNode, XMLObjectiveNode, XMLObjectiveResultsNode, XMLFailureCodes: TDOMNode;
  TimeSent, TimeReceived: DWORD;
begin
  Serial := TBlockSerial.Create;                           // Create the Serial object in the context of the thread
  Serial.LinuxLock:=False;
  Serial.RaiseExcept:=False;
  Serial.Connect(Port);
  SendStrings := TStringList.Create;
  ReceiveStrings := TStringList.Create;
  Objectives := TList.Create;
  try
    Connected:=True;
    Serial.Config(BaudRate, 8, 'N', 0, Settings.SoftwareFlowControl, False);      // FTDI Driver uses no stop bits for non-standard baud rates.
    while not Terminated do
    begin

      ThreadSwitch;

      // Pickup a new ActiveTest if needed
      if ActiveTest = nil then
      begin
        List := ThreadTestList.LockList;
        try
          if List.Count > 0 then
          begin
            ActiveTest := TTestBase( List[0]);
            List.Delete(0);
          end else
            ActiveTest := nil;
          FTestCount := List.Count;
        finally
          ThreadTestList.UnlockList;        // Deadlock if we don't do this here when the main thread blocks trying to add a new Test and we call Syncronize asking the main thread to run.....
        end;
      end;


      if Assigned(ActiveTest) then
      begin
        case ActiveTest.TestState of
          ts_Initialize     : begin
                                if Assigned(ActiveTest.ListItem) then Synchronize(@SyncronizeUpdateUI);
                                Objectives.Clear;
                                ActiveTest.InitTest;
                                iCurrentObjective := 0;
                                iNextObjective := 0;
                                ExtractTestObjectivesFromTestNode(ActiveTest.XMLTests, Objectives);

                                if ActiveTest.XMLResults.DocumentElement <> nil then
                                  ActiveTest.XMLResults.RemoveChild(ActiveTest.XMLResults.DocumentElement);
                                ActiveTest.XMLResults.AppendChild(ActiveTest.XMLResults.CreateElement(XML_ELEMENT_TEST_RESULT_ROOT));

                                XMLRoot := ActiveTest.XMLResults.FindNode(XML_ELEMENT_TEST_RESULT_ROOT);

                                XMLNode := ActiveTest.XMLResults.CreateElement(XML_ELEMENT_NAME);
                                XMLRoot.AppendChild(XMLNode);
                                XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(TestNameFromTestNode(ActiveTest.XMLTests)));

                                XMLTestNode := ActiveTest.XMLResults.CreateElement(XML_ELEMENT_TEST);
                                XMLRoot.AppendChild(XMLTestNode);

                                XMLNode := ActiveTest.XMLResults.CreateElement(XML_ELEMENT_DESCRIPTION);
                                XMLTestNode.AppendChild(XMLNode);
                                XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(TestDescriptionFromTestNode(ActiveTest.XMLTests)));

                                XMLNode := ActiveTest.XMLResults.CreateElement(XML_ELEMENT_CLASSNAME);
                                XMLTestNode.AppendChild(XMLNode);
                                XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(TestClassnameFromTestNode(ActiveTest.XMLTests)));

                                if TerminateTest then
                                  ActiveTest.TestState := ts_Complete
                                else
                                  ActiveTest.TestState := ts_ObjectiveStart;
                              end;
          ts_ObjectiveStart : begin
                                XMLTestObjectiveNode := ActiveTest.XMLResults.CreateElement(XML_ELEMENT_TESTOBJECTIVE);
                                XMLTestNode.AppendChild(XMLTestObjectiveNode);

                                if iCurrentObjective < Objectives.Count then
                                begin
                                  XMLObjectiveNode := ActiveTest.XMLResults.CreateElement(XML_ELEMENT_OBJECTIVE);
                                  XMLTestObjectiveNode.AppendChild(XMLObjectiveNode);

                                  XMLNode := ActiveTest.XMLResults.CreateElement(XML_ELEMENT_SPECDOC);
                                  XMLObjectiveNode.AppendChild(XMLNode);
                                  XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(SpecDocFromObjectiveNode(TDOMNode( Objectives[iCurrentObjective]))));

                                  XMLNode := ActiveTest.XMLResults.CreateElement(XML_ELEMENT_SEND);
                                  XMLObjectiveNode.AppendChild(XMLNode);
                                  XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(ObjectiveFromObjectiveNode(TDOMNode( Objectives[iCurrentObjective]))));

                                  XMLNode := ActiveTest.XMLResults.CreateElement(XML_ELEMENT_RECEIVE);
                                  XMLObjectiveNode.AppendChild(XMLNode);
                                  XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(ObjectiveResultFromObjectiveNode(TDOMNode( Objectives[iCurrentObjective]))));

                                  XMLObjectiveResultsNode := ActiveTest.XMLResults.CreateElement(XML_ELEMENT_OBJECTIVERESULTS);
                                  XMLTestObjectiveNode.AppendChild(XMLObjectiveResultsNode);
                                end;

                                ActiveTest.ClearErrorCodes;                     // Clear out the errors at the start of an objective so they can accumulate throught the objective
                                SendStrings.Clear;
                                TimeoutRead := Settings.TimeoutComRead;

                                if TerminateTest then
                                  ActiveTest.TestState := ts_Complete
                                else
                                  ActiveTest.TestState := ts_Sending;
                              end;
          ts_Sending :        begin
                                ActiveTest.ProcessObjectives(Self, SendStrings, '', iNextObjective, TimeoutRead, 0, SendNext);
                                for i := 0 to SendStrings.Count - 1 do                           // Start with the next objective information
                                begin
                                  if TerminateTest then
                                    Break;
                                  if EnableRawMessages then
                                  begin
                                    RawMessageBuffer := SendStrings[i];
                                    Synchronize(@SyncRawMessage);
                                  end;
                                  Serial.SendString(SendStrings[i] + LF);
                                  XMLNode := ActiveTest.XMLResults.CreateElement(XML_ELEMENT_SEND);
                                  XMLObjectiveResultsNode.AppendChild(XMLNode);
                                  XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(SendStrings[i]));
                                end;
                                SendStrings.Clear;
                                while Serial.SendingData > 0 do
                                  ThreadSwitch;                                    // Wait till "done" transmitting
                                TimeSent := GetTickCount;

                                if TerminateTest then
                                  ActiveTest.TestState := ts_Complete
                                else
                                  ActiveTest.TestState := ts_Receiving;            // Receive what we asked for before terminating
                              end;
          ts_Receiving      : begin
                                ReceiveStr := Trim( UpperCase(Serial.Recvstring(TimeoutRead))) ;  // Try to get something from the CAN
                                TimeReceived := GetTickCount;

                                if ActiveTest.FilterRepliesForNUT then
                                  ProcessReply := ActiveTest.IsMessageForNodeUnderTest(ReceiveStr)
                                else
                                  ProcessReply := True;

                                if ProcessReply then   // Filter out messages not for this node if in MultiNode Mode
                                begin
                                  if ReceiveStr <> '' then
                                  begin
                                    if EnableRawMessages then
                                    begin
                                    RawMessageBuffer := ReceiveStr;
                                    Synchronize(@SyncRawMessage);
                                    end;
                                    // Save messages received for the node in the XML file
                                    XMLNode := ActiveTest.XMLResults.CreateElement(XML_ELEMENT_RECEIVE);
                                    XMLObjectiveResultsNode.AppendChild(XMLNode);
                                    XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(ReceiveStr));
                                  end;

                                  SendNext := True;
                                  ActiveTest.ProcessObjectives(Self, SendStrings, ReceiveStr, iNextObjective, TimeoutRead, TimeReceived-TimeSent, SendNext);

                                  if SendNext then
                                    ActiveTest.TestState := ts_Sending;

                                  if iNextObjective > iCurrentObjective then
                                  begin
                                    XMLNode := ActiveTest.XMLResults.CreateElement(XML_ELEMENT_PASS_FAIL);
                                    XMLObjectiveResultsNode.AppendChild(XMLNode);
                                    if not ActiveTest.HasAnyErrorCodeSet then
                                      XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(XML_NAME_PASS))
                                    else begin
                                      XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(XML_NAME_FAIL));

                                      XMLFailureCodes := ActiveTest.XMLResults.CreateElement(XML_NAME_FAILURE_CODES);
                                      XMLObjectiveResultsNode.AppendChild(XMLFailureCodes);

                                      ErrorCodesToXML(XMLFailureCodes);
                                      ErrorCodesFormatToXML(XMLFailureCodes);
                                      ErrorCodesPipToXML(XMLFailureCodes);
                                      ErrorCodesStartupToXML(XMLFailureCodes);
                                      ErrorCodesUnknownMTIToXML(XMLFailureCodes);
                                      ErrorCodesAliasToXML(XMLFailureCodes);
                                    end;
                                    iCurrentObjective := iNextObjective;
                                    if TerminateTest then
                                      ActiveTest.TestState := ts_Complete
                                    else
                                      ActiveTest.TestState := ts_ObjectiveEnd;    // Start next objective
                                  end;
                                end;
                              end;
          ts_ObjectiveEnd :   begin
                                if iCurrentObjective < Objectives.Count then
                                begin
                                  if TerminateTest then
                                    ActiveTest.TestState := ts_Complete
                                  else
                                    ActiveTest.TestState := ts_ObjectiveStart
                                end else
                                  ActiveTest.TestState := ts_Complete;
                              end;
          ts_Complete       : begin
                                if TerminateTest then
                                begin
                                  List := ThreadTestList.LockList;
                                  try
                                    for i := List.Count - 1 downto 0 do
                                    begin
                                      if TTestBase( List[i]).FreeOnLog then
                                        TTestBase( List[i]).Free;
                                    end
                                  finally
                                    List.Clear;
                                    FTestCount := List.Count;
                                    ThreadTestList.UnlockList;
                                  end;
                                  FTerminatedTest := True;
                                  ActiveTest.ErrorCodes := ActiveTest.ErrorCodes + [teCancelled];
                                  Synchronize(@ActiveTest.SyncTestComplete);
                                  ActiveTest := nil;
                                  FTerminatedTest := False;
                                  FTerminateTest := False;
                                end else
                                begin
                                  Synchronize(@ActiveTest.SyncTestComplete);
                                  ActiveTest := nil;
                                end
                              end;
        end;
      end else
      begin
        // Unsolicited Information
        ReceiveStr := Serial.Recvstring(0);
        if ReceiveStr <> '' then
        begin
          if EnableRawMessages then
          begin
            RawMessageBuffer := ReceiveStr;
            Synchronize(@SyncRawMessage);
          end;
        end;
      end;


    end;
  finally
    if Connected then
      Serial.CloseSocket;
    Connected := False;
    FreeAndNil(SendStrings);
    FreeAndNil(ReceiveStrings);
    FreeandNil(Objectives);
  end;
end;

procedure TComPortThread.ErrorCodesToXML(RootXMLElement: TDOMNode);
var
  XMLNode: TDOMNode;
begin
  if ActiveTest.ErrorCodes <> [] then
  begin
    if teNoReply in ActiveTest.ErrorCodes then
    begin
      XMLNode := ActiveTest.XMLResults.CreateElement(XML_NAME_FAILURE_CODE);
      RootXMLElement.AppendChild(XMLNode);
      XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(XML_ERROR_NO_REPLY));
    end;
    if teUnExpectedReply in ActiveTest.ErrorCodes then
    begin
      XMLNode := ActiveTest.XMLResults.CreateElement(XML_NAME_FAILURE_CODE);
      RootXMLElement.AppendChild(XMLNode);
      XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(XML_ERROR_UNEXPECTED_REPLY));
    end;
    if teFullNodeIDInvalid in ActiveTest.ErrorCodes then
    begin
      XMLNode := ActiveTest.XMLResults.CreateElement(XML_NAME_FAILURE_CODE);
      RootXMLElement.AppendChild(XMLNode);
      XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(XML_ERROR_INVALID_NODE_ID));
    end;
    if teStandardFrameResponse in ActiveTest.ErrorCodes then
    begin
      XMLNode := ActiveTest.XMLResults.CreateElement(XML_NAME_FAILURE_CODE);
      RootXMLElement.AppendChild(XMLNode);
      XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(XML_ERROR_STANDARD_FRAME));
    end;
  end;
end;

procedure TComPortThread.ErrorCodesFormatToXML(RootXMLElement: TDOMNode);
var
  XMLNode: TDOMNode;
begin
  if ActiveTest.ErrorCodesFormat <> [] then
  begin
    if tefUnusedBitsSet in ActiveTest.ErrorCodesFormat then
    begin
      XMLNode := ActiveTest.XMLResults.CreateElement(XML_NAME_FAILURE_CODE);
      RootXMLElement.AppendChild(XMLNode);
      XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(XML_ERROR_FORMAT_UNUSED_BITS_SET));
     end;
    if tefForwardingBitNotSet in ActiveTest.ErrorCodesFormat then
    begin
      XMLNode := ActiveTest.XMLResults.CreateElement(XML_NAME_FAILURE_CODE);
       RootXMLElement.AppendChild(XMLNode);
      XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(XML_ERROR_FORMAT_FORWARDING_BIT_NOT_SET));
    end;
    if tefInvalidMTI in ActiveTest.ErrorCodesFormat then
    begin
      XMLNode := ActiveTest.XMLResults.CreateElement(XML_NAME_FAILURE_CODE);
      RootXMLElement.AppendChild(XMLNode);
       XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(XML_ERROR_FORMAT_INVALID_MTI));
    end;
    if tefInvalidSourceAlias in ActiveTest.ErrorCodesFormat then
    begin
      XMLNode := ActiveTest.XMLResults.CreateElement(XML_NAME_FAILURE_CODE);
      RootXMLElement.AppendChild(XMLNode);
      XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(XML_ERROR_FORMAT_INVALID_SOURCE_ALIAS));
    end;
    if tefInvalidDestAlias in ActiveTest.ErrorCodesFormat then
     begin
      XMLNode := ActiveTest.XMLResults.CreateElement(XML_NAME_FAILURE_CODE);
      RootXMLElement.AppendChild(XMLNode);
      XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(XML_ERROR_FORMAT_INVALID_DEST_ALIAS));
    end;
  end;
end;

procedure TComPortThread.ErrorCodesPipToXML(RootXMLElement: TDOMNode);
var
  XMLNode: TDOMNode;
begin
  if ActiveTest.ErrorCodesPip <> [] then
  begin
    if tepPipUsingUnassignedBits in ActiveTest.ErrorCodesPip then
    begin
      XMLNode := ActiveTest.XMLResults.CreateElement(XML_NAME_FAILURE_CODE);
      RootXMLElement.AppendChild(XMLNode);
      XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(XML_ERROR_PIP_UNASSIGNED_BITS));
    end;
    if tepPipUsingReservedBits in ActiveTest.ErrorCodesPip then
    begin
      XMLNode := ActiveTest.XMLResults.CreateElement(XML_NAME_FAILURE_CODE);
       RootXMLElement.AppendChild(XMLNode);
      XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(XML_ERROR_PIP_RESERVED_BITS));
    end;
    if tepPipStartEndBitSupport in ActiveTest.ErrorCodesPip then
    begin
      XMLNode := ActiveTest.XMLResults.CreateElement(XML_NAME_FAILURE_CODE);
      RootXMLElement.AppendChild(XMLNode);
      XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(XML_ERROR_PIP_START_END_BIT_SUPPORT));
     end;
    if tepPipRespondedToStartBit in ActiveTest.ErrorCodesPip then
    begin
      XMLNode := ActiveTest.XMLResults.CreateElement(XML_NAME_FAILURE_CODE);
      RootXMLElement.AppendChild(XMLNode);
      XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(XML_ERROR_PIP_UNEXPECTED_RESPONSE_TO_START_BIT));
    end;
  end;
end;

procedure TComPortThread.ErrorCodesUnknownMTIToXML(RootXMLElement: TDOMNode);
var
  XMLNode: TDOMNode;
begin
  if ActiveTest.ErrorCodesUnknownMTI <> [] then
  begin
    if tuOptionalCode in ActiveTest.ErrorCodesUnknownMTI then
    begin
      XMLNode := ActiveTest.XMLResults.CreateElement(XML_NAME_FAILURE_CODE);
      RootXMLElement.AppendChild(XMLNode);
      XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(XML_ERROR_UNKOWN_MTI_OPTIONAL_CODE));
    end;
    if tuErrorCode in ActiveTest.ErrorCodesUnknownMTI then
    begin
      XMLNode := ActiveTest.XMLResults.CreateElement(XML_NAME_FAILURE_CODE);
      RootXMLElement.AppendChild(XMLNode);
      XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(XML_ERROR_UNKOWN_MTI_ERROR_CODE));
    end;
  end;

end;

procedure TComPortThread.ErrorCodesStartupToXML(RootXMLElement: TDOMNode);
var
  XMLNode: TDOMNode;
begin
  if ActiveTest.ErrorCodesStartup <> [] then
  begin
    if tsNoAMR in ActiveTest.ErrorCodesStartup then
    begin
      XMLNode := ActiveTest.XMLResults.CreateElement(XML_NAME_FAILURE_CODE);
      RootXMLElement.AppendChild(XMLNode);
      XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(XML_ERROR_NO_STARTUP_NO_AMR));
    end;
    if tsNoCID0 in ActiveTest.ErrorCodesStartup then
    begin
      XMLNode := ActiveTest.XMLResults.CreateElement(XML_NAME_FAILURE_CODE);
      RootXMLElement.AppendChild(XMLNode);
      XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(XML_ERROR_NO_STARTUP_NO_CID0));
    end;
    if tsNoCID1 in ActiveTest.ErrorCodesStartup then
    begin
      XMLNode := ActiveTest.XMLResults.CreateElement(XML_NAME_FAILURE_CODE);
      RootXMLElement.AppendChild(XMLNode);
      XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(XML_ERROR_NO_STARTUP_NO_CID1));
    end;
    if tsNoCID2 in ActiveTest.ErrorCodesStartup then
    begin
      XMLNode := ActiveTest.XMLResults.CreateElement(XML_NAME_FAILURE_CODE);
      RootXMLElement.AppendChild(XMLNode);
      XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(XML_ERROR_NO_STARTUP_NO_CID2));
    end;
    if tsNoCID3 in ActiveTest.ErrorCodesStartup then
    begin
      XMLNode := ActiveTest.XMLResults.CreateElement(XML_NAME_FAILURE_CODE);
      RootXMLElement.AppendChild(XMLNode);
      XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(XML_ERROR_NO_STARTUP_NO_CID3));
    end;
    if tsNoRID in ActiveTest.ErrorCodesStartup then
    begin
      XMLNode := ActiveTest.XMLResults.CreateElement(XML_NAME_FAILURE_CODE);
      RootXMLElement.AppendChild(XMLNode);
      XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(XML_ERROR_NO_STARTUP_NO_RID));
    end;
    if tsNoAMD in ActiveTest.ErrorCodesStartup then
    begin
      XMLNode := ActiveTest.XMLResults.CreateElement(XML_NAME_FAILURE_CODE);
      RootXMLElement.AppendChild(XMLNode);
      XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(XML_ERROR_NO_STARTUP_NO_AMD));
    end;
    if tsNoInitialized in ActiveTest.ErrorCodesStartup then
    begin
      XMLNode := ActiveTest.XMLResults.CreateElement(XML_NAME_FAILURE_CODE);
      RootXMLElement.AppendChild(XMLNode);
      XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(XML_ERROR_NO_STARTUP_NO_INITIALZIED));
    end;
  end
end;

procedure TComPortThread.ErrorCodesAliasToXML(RootXMLElement: TDOMNode);
var
  XMLNode: TDOMNode;
begin
  if ActiveTest.ErrorAliasConflict <> [] then
  begin
    if taZeroAlias in ActiveTest.ErrorAliasConflict then
    begin
      XMLNode := ActiveTest.XMLResults.CreateElement(XML_NAME_FAILURE_CODE);
      RootXMLElement.AppendChild(XMLNode);
      XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(XML_ERROR_ALIAS_ZERO));
    end;
    if taDuplicateAlias in ActiveTest.ErrorAliasConflict then
    begin
      XMLNode := ActiveTest.XMLResults.CreateElement(XML_NAME_FAILURE_CODE);
      RootXMLElement.AppendChild(XMLNode);
      XMLNode.AppendChild(ActiveTest.XMLResults.CreateTextNode(XML_ERROR_ALIAS_DUPLICATE));
    end;
  end
end;


procedure TComPortThread.SyncronizeUpdateUI;
begin
  if Assigned(ActiveTest) then
  begin
    if Assigned(ActiveTest.ListItem) then
      ActiveTest.ListItem.ImageIndex := 21
  end;
 end;

procedure TComPortThread.SyncRawMessage;
begin
  if Assigned(SyncRawMessageFunc) then
    SyncRawMessageFunc(RawMessageBuffer)
end;

constructor TComPortThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FActiveTest := nil;
  FThreadTestList := TThreadList.Create;
  FBaudRate := 9600;
  FPort := '';
  FTestCount := 0;
  FTerminateTest := False;
  FTerminatedTest := False;
  FEnableRawMessages := False;
  FLastErrorDesc := '';
end;

destructor TComPortThread.Destroy;
begin
  FreeAndNil(FThreadTestList);   // Thread does not own the items so just empty the list
  inherited Destroy;
end;

procedure TComPortThread.Add(Test: TTestBase);
var
  List: TList;
begin
  Test.TestState := ts_Initialize;
  List := ThreadTestList.LockList;
  try
    List.Add(Test);     // Add it to the Thread List
    FTestCount := List.Count;
  finally
    ThreadTestList.UnLockList;
  end;
end;


end.
