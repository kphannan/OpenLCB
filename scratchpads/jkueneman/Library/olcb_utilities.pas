unit olcb_utilities;

// ******************************************************************************
//
// * Copyright:
//     (c) Mustangpeak Software 2012.
//
//     The contents of this file are subject to the GNU GPL v3 licence/ you maynot use
//     this file except in compliance with the License. You may obtain a copy of the
//     License at http://www.gnu.org/licenses/gpl.html
//
// * Revision History:
//     2012-08-05:   Created
//
// * Description:

//
// *****************************************************************************

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, ExtCtrls, olcb_defines, common_utilities,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ELSE}
  LclIntf,
  {$ENDIF}
  Math, DOM, XMLRead, math_float16, opstackdefines, SynMemo;

const
  LF = #13+#10;
  CAN_BYTE_COUNT = 8;

  XML_ELEMENT_NAME               = 'Name';
  XML_ELEMENT_DESCRIPTION        = 'Description';
  XML_ELEMENT_SPECDOC            = 'SpecDoc';
  XML_ELEMENT_CLASSNAME          = 'Classname';
  XML_ELEMENT_ENABLED            = 'Enabled';
  XML_ELEMENT_TESTOBJECTIVE      = 'TestObjective';
  XML_ELEMENT_OBJECTIVE          = 'Objective';
  XML_ELEMENT_OBJECTIVERESULTS   = 'Results';
  XML_ELEMENT_TEST               = 'Test';
  XML_ELEMENT_SEND               = 'Send';
  XML_ELEMENT_RECEIVE            = 'Receive';
  XML_ELEMENT_TEST_RESULT_ROOT   = 'TestResult';
  XML_ELEMENT_PASS_FAIL          = 'PassFail';
  XML_NAME_PASS                  = 'Pass';
  XML_NAME_FAIL                  = 'Fail';
  XML_NAME_FAILURE_CODES          = 'FailureCodes';
  XML_NAME_FAILURE_CODE           = 'Code';
  XML_ELECMENT_TEST_MATRIX        = 'TestMatrix';

type
  TConnectionState = (csConnecting, csConnected, csDisconnecting, csDisconnected);

type
    TOlcbNodeState = (ons_Disabled, ons_Started, ons_LoggingIn, ons_Permitted );

type
  TOnRawMessageFunc = procedure(Sender: TObject; MessageStr: String) of object;
  TOnConnectionStateChangeFunc = procedure (Sender: TObject; ConnectionState: TConnectionState) of object;
  TOnNodeEventFunc = procedure(Sender: TObject; EventList: TList) of object;

  TCANByteArray = array[0..CAN_BYTE_COUNT-1] of Byte;
  PCANByteArray = ^TCANByteArray;

  TOpenLCBLayer = (ol_CAN, ol_OpenLCB);

  { TOlcbMessage }

   TOlcbMessage = class
     // Core base for objects that hold OpenLCB messages,
   end;

  { TOpenLCBMessageHelper }

  TOpenLCBMessageHelper = class( TOlcbMessage)
  private
    FDestinationAliasID: Word;
    FFramingBits: Byte;
    FHasDestinationAddress: Boolean;
    FForwardingBitNotSet: Boolean;
    FSourceAliasID: Word;
    FData: TCANByteArray;
    FDataCount: Integer;
    FLayer: TOpenLCBLayer;
    FMTI: DWord;
    FUnimplementedBitsSet: Boolean;
    procedure SetData(AValue: TCANByteArray);
    procedure SetLayer(AValue: TOpenLCBLayer);
  public
    property Layer: TOpenLCBLayer read FLayer write SetLayer;
    property MTI: DWord read FMTI write FMTI;
    property Data: TCANByteArray read FData write SetData;
    property DataCount: Integer read FDataCount write FDataCount;
    property SourceAliasID: Word read FSourceAliasID write FSourceAliasID;
    property DestinationAliasID: Word read FDestinationAliasID write FDestinationAliasID;
    property ForwardingBitNotSet: Boolean read FForwardingBitNotSet write FForwardingBitNotSet;
    property FramingBits: Byte read FFramingBits write FFramingBits;
    property UnimplementedBitsSet: Boolean read FUnimplementedBitsSet write FUnimplementedBitsSet;
    property HasDestinationAddress: Boolean read FHasDestinationAddress write FHasDestinationAddress;

    constructor Create;
    destructor Destroy; override;
    procedure CopyTo(Target: TOpenLCBMessageHelper);
    function Decompose(MessageStr: AnsiString): Boolean;
    function Encode: AnsiString;
    procedure Load(ALayer: TOpenLCBLayer; AMTI: DWord; ASourceAlias: Word; ADestinationAlias: Word; ADataCount: Integer; AData0, AData1, AData2, AData3, AData4, AData5, AData6, AData7: Byte);
    procedure StoreNodeIDToData(NodeID: Int64; IsAddressed: Boolean);
    function ExtractDataBytesAsInt(StartByteIndex, EndByteIndex: Integer): QWord;
    function ExtractDataBytesAsString(StartIndex, Count: Integer): String;
    function ExtractDataBytesAsEventID: PEventID;
    procedure IntToByteArray(Int: QWord; var ByteArray: TCANByteArray);
  end;

  procedure ExtractTestsFromXML(XMLDoc: TXMLDocument; TestList: TList);
  procedure ExtractTestObjectivesFromTestNode(TestNode: TDOMNode; TestList: TList);
  function TestNameFromTestNode(TestNode: TDOMNode): WideString;
  function TestDescriptionFromTestNode(TestNode: TDOMNode): WideString;
  function TestSpecDocFromTestNode(TestNode: TDOMNode): WideString;
  function TestClassnameFromTestNode(TestNode: TDOMNode): WideString;
  function TestEnabledStateFromTestNode(TestNode: TDOMNode): WideString;
  function ObjectiveFromObjectiveNode(ObjectiveNode: TDOMNode): WideString;
  function ObjectiveResultFromObjectiveNode(ObjectiveNode: TDOMNode): WideString;
  function SpecDocFromObjectiveNode(ObjectiveNode: TDOMNode): WideString;
  function ExtractElementValue(Node: TDOMNode; ElementName: WideString): WideString;
  function MTI_ToString(MTI: DWord): WideString;
  function TractionProxyTechnologyToStr(Technology: Byte): WideString;
  function EventIDToString(EventID: PEventID): WideString;
  function EqualEvents(Event1, Event2: PEventID): Boolean;
  function IsDatagramMTI(MTI: DWord; IncludeReplies: Boolean): Boolean;
  function IsStreamMTI(MTI: DWord; IncludeSetupTeardowns: Boolean): Boolean;
  function MessageToDetailedMessage(MessageString: string; Sending: Boolean): string;
  function GridConnectToJMRI(GridStr: AnsiString): AnsiString;
  function ProtocolSupportReplyToString(Mask: QWord): string;
  function AddressSpaceToString(AddressSpace: Byte): string;
  function NodeIDToDotHex(NodeID: QWord): string; overload;
  function NodeIDToDotHex(NodeInfo: TNodeID): string; overload;
  function DotHexToNodeID(NodeID: string): QWord;
  function EventToDoxHex(Event: TEventID): string;
  function DotHexToEvent(Event: string): TEventID;
  function IntToHexArray(Value: Integer): THexArray;
  function StrToHexArray(Value: string): THexArray;
  function RawHelperDataToStr(HelperData: TOpenLCBMessageHelper; ASCII: Boolean): string;
  procedure PrintToSynMemo(MessageStr: AnsiString; SynMemoLog: TSynMemo; Paused: Boolean; Detailed: Boolean; JMRIFormat: Boolean);
  function DeleteLineBreaks(const S: Ansistring): Ansistring;


  function GetTickCount : DWORD;

implementation

type

  { TMultiFrameBuffer }

  TMultiFrameBuffer = class
  private
    FAliasID: Word;
    FCurrentIndex: Integer;
    FDataArray: TMultiFrameArray;
    FDataArraySize: Integer;
  public
    constructor Create;
    function ExtractDataBytesAsInt(StartByteIndex, EndByteIndex: Integer): QWord;
    function ExtractDataBytesAsHex(StartByteIndex, EndByteIndex: Integer): string;
    property AliasID: Word read FAliasID write FAliasID;
    property DataArray: TMultiFrameArray read FDataArray write FDataArray;
    property DataArraySize: Integer read FDataArraySize;
    property CurrentIndex: Integer read FCurrentIndex write FCurrentIndex;
  end;

  { TMultiFrameBufferList }

  TMultiFrameBufferList = class
  private
    FList: TList;
    function GetMultiFrameBuffer(Index: Integer): TMultiFrameBuffer;
    procedure SetMultiFrameBuffer(Index: Integer; AValue: TMultiFrameBuffer);
  protected
    property List: TList read FList write FList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function FindByAlias(TestAliasID: Word): TMultiFrameBuffer;
    function ProcessFrame(NewFrame: TOpenLCBMessageHelper): TMultiFrameBuffer;
    property MultiFrameBuffers[Index: Integer]: TMultiFrameBuffer read GetMultiFrameBuffer write SetMultiFrameBuffer;
  end;

var
  LocalHelper: TOpenLCBMessageHelper;
  MultiFrames: TMultiFrameBufferList;
  LogStrings: TStringList;

function GetTickCount : DWORD;
 {On Windows, this is number of milliseconds since Windows was
   started. On non-Windows platforms, LCL returns number of
   milliseconds since Dec. 30, 1899, wrapped by size of DWORD.
   This value can overflow LongInt variable when checks turned on,
   so "wrap" value here so it fits within LongInt.
  Also, since same thing could happen with Windows that has been
   running for at least approx. 25 days, override it too.}
begin
{$IFDEF MSWINDOWS}
  Result := Windows.GetTickCount mod High(LongInt);
{$ELSE}
  Result := LclIntf.GetTickCount mod High(LongInt);
{$ENDIF}
end;

procedure ExtractTestsFromXML(XMLDoc: TXMLDocument; TestList: TList);
var
  TestMatrixNode, Child: TDOMNode;
begin
  TestMatrixNode := XMLDoc.FindNode(XML_ELECMENT_TEST_MATRIX);
  Child := TestMatrixNode.FirstChild;
  while Assigned(Child) do
  begin
    if Child.HasChildNodes then
    begin
      if Child.NodeName = XML_ELEMENT_TEST then
        TestList.Add(Child);
    end;
    Child := Child.NextSibling;
  end;
end;

procedure ExtractTestObjectivesFromTestNode(TestNode: TDOMNode; TestList: TList);
var
   Child: TDOMNode;
begin
  Child := TestNode.FirstChild;
  while Assigned(Child) do
  begin
    if Child.HasChildNodes then
    begin
      if Child.NodeName = XML_ELEMENT_TESTOBJECTIVE then
        TestList.Add(Child);
    end;
    Child := Child.NextSibling;
  end;
end;

function TestNameFromTestNode(TestNode: TDOMNode): WideString;
begin
  Result := ExtractElementValue(TestNode, XML_ELEMENT_NAME)
end;

function TestDescriptionFromTestNode(TestNode: TDOMNode): WideString;
begin
  Result := ExtractElementValue(TestNode, XML_ELEMENT_DESCRIPTION)
end;

function TestSpecDocFromTestNode(TestNode: TDOMNode): WideString;
begin
  Result := ExtractElementValue(TestNode, XML_ELEMENT_SPECDOC)
end;

function TestClassnameFromTestNode(TestNode: TDOMNode): WideString;
begin
  Result := ExtractElementValue(TestNode, XML_ELEMENT_CLASSNAME)
end;

function TestEnabledStateFromTestNode(TestNode: TDOMNode): WideString;
begin
  Result := ExtractElementValue(TestNode, XML_ELEMENT_ENABLED)
end;

function ObjectiveFromObjectiveNode(ObjectiveNode: TDOMNode): WideString;
begin
  Result := ExtractElementValue(ObjectiveNode, XML_ELEMENT_OBJECTIVE)
end;

function ObjectiveResultFromObjectiveNode(ObjectiveNode: TDOMNode): WideString;
begin
  Result := ExtractElementValue(ObjectiveNode, XML_ELEMENT_OBJECTIVERESULTS)
end;

function SpecDocFromObjectiveNode(ObjectiveNode: TDOMNode): WideString;
begin
  Result := ExtractElementValue(ObjectiveNode, XML_ELEMENT_SPECDOC)
end;

function ExtractElementValue(Node: TDOMNode; ElementName: WideString): WideString;
var
  Child: TDOMNode;
begin
  Result := '';
  Child := Node.FindNode(ElementName);
  if Assigned(Child) then
  begin
    if Child.HasChildNodes then
      Result := Child.FirstChild.NodeValue;
  end;
end;

function MTI_ToString(MTI: DWord): WideString;

  procedure ConfigurationDatagram;
  begin
    if LocalHelper.DataCount > 0 then
        begin
          if LocalHelper.Data[0] = DATAGRAM_PROTOCOL_CONFIGURATION then
          begin
            case LocalHelper.Data[1] and $F8 of
              MCP_WRITE :
                  begin
                    case LocalHelper.Data[1] and $07 of
                      MCP_CDI            : Result := Result + ' Write Command, Address Space = CDI';
                      MCP_ALL            : Result := Result + ' Write Command, Address Space = All';
                      MCP_CONFIGURATION  : Result := Result + ' Write Command, Address Space = Configuration';
                      MCP_NONE           : begin
                                             case LocalHelper.Data[6] of
                                               MSI_CDI         : Result := Result + ' Write Command, Address Space = CDI';
                                               MSI_ALL         : Result := Result + ' Write Command, Address Space = All';
                                               MSI_CONFIG      : Result := Result + ' Write Command, Address Space = Configuration';
                                               MSI_ACDI_MFG    : Result := Result + ' Write Command, Address Space = ACDI Manufacturer';
                                               MSI_ACDI_USER   : Result := Result + ' Write Command, Address Space = ACDI User';
                                               MSI_FDI         : Result := Result + ' Write Command, Address Space = Function Definition Info';
                                               MSI_FSI         : Result := Result + ' Write Command, Address Space = Function State Info'
                                             else
                                               Result := Result + ' Write Command, Address Space = [Unknown]'      ;
                                             end;
                                           end
                    end; // Case
                    Result := Result + ', Starting Address = ' + IntToHex( LocalHelper.ExtractDataBytesAsInt(2, 5), 8);
                  end;
              MCP_READ  :
                  begin
                    case LocalHelper.Data[1] and $07 of
                      MCP_CDI            : Result := Result + ' Read Command, Address Space = CDI';
                      MCP_ALL            : Result := Result + ' Read Command, Address Space = All';
                      MCP_CONFIGURATION  : Result := Result + ' Read Command, Address Space = Configuration';
                      MCP_NONE           : begin
                                             case LocalHelper.Data[6] of
                                               MSI_CDI         : Result := Result + ' Read Command, Address Space = CDI';
                                               MSI_ALL         : Result := Result + ' Read Command, Address Space = All';
                                               MSI_CONFIG      : Result := Result + ' Read Command, Address Space = Configuration';
                                               MSI_ACDI_MFG    : Result := Result + ' Read Command, Address Space = ACDI Manufacturer';
                                               MSI_ACDI_USER   : Result := Result + ' Read Command, Address Space = ACDI User';
                                               MSI_FDI         : Result := Result + ' Read Command, Address Space = Function Definition Info';
                                               MSI_FSI         : Result := Result + ' Read Command, Address Space = Function State Info'
                                             else
                                               Result := Result + ' Read Command, Address Space = [Unknown]'      ;
                                             end;
                                           end
                    end; // Case
                    Result := Result + ', Starting Address = ' + IntToHex( LocalHelper.ExtractDataBytesAsInt(2, 5), 8);
                  end;
              MCP_OPERATION  :
                  begin
                  end;
              MCP_WRITE_DATAGRAM_REPLY :
                  begin
                    case LocalHelper.Data[1] and $07 of
                      MCP_CDI            : Result := Result + ' Write Reply, Address Space = CDI';
                      MCP_ALL            : Result := Result + ' Write Reply, Address Space = All';
                      MCP_CONFIGURATION  : Result := Result + ' Write Reply, Address Space = Configuration';
                      MCP_NONE           : begin
                                             case LocalHelper.Data[6] of
                                               MSI_CDI         : Result := Result + ' Write Reply, Address Space = CDI';
                                               MSI_ALL         : Result := Result + ' Write Reply, Address Space = All';
                                               MSI_CONFIG      : Result := Result + ' Write Reply, Address Space = Configuration';
                                               MSI_ACDI_MFG    : Result := Result + ' Write Reply, Address Space = ACDI Manufacturer';
                                               MSI_ACDI_USER   : Result := Result + ' Write Reply, Address Space = ACDI User';
                                               MSI_FDI         : Result := Result + ' Write Reply, Address Space = Function Definition Info';
                                               MSI_FSI         : Result := Result + ' Write Reply, Address Space = Function State Info'
                                             else
                                               Result := Result + ' Write Reply, Address Space = [Unknown]'      ;
                                             end;
                                           end
                    end; // Case
                    Result := Result + ', Starting Address = ' + IntToHex( LocalHelper.ExtractDataBytesAsInt(2, 5), 8);
                    if LocalHelper.Data[1] and $F8 = MCP_WRITE_OK then
                      Result := Result + ', Success'
                    else
                    if LocalHelper.Data[1] and $F8 = MCP_WRITE_ERROR then
                      Result := Result + ', Error'
                  end;
              MCP_READ_DATAGRAM_REPLY  :
                  begin
                    case LocalHelper.Data[1] and $07 of
                      MCP_CDI            : Result := Result + ' Read Reply, Address Space = CDI';
                      MCP_ALL            : Result := Result + ' Read Reply, Address Space = All';
                      MCP_CONFIGURATION  : Result := Result + ' Read Reply, Address Space = Configuration';
                      MCP_NONE           : begin
                                             case LocalHelper.Data[6] of
                                               MSI_CDI         : Result := Result + ' Read Reply, Address Space = CDI';
                                               MSI_ALL         : Result := Result + ' Read Reply, Address Space = All';
                                               MSI_CONFIG      : Result := Result + ' Read Reply, Address Space = Configuration';
                                               MSI_ACDI_MFG    : Result := Result + ' Read Reply, Address Space = ACDI Manufacturer';
                                               MSI_ACDI_USER   : Result := Result + ' Read Reply, Address Space = ACDI User';
                                               MSI_FDI         : Result := Result + ' Read Reply, Address Space = Function Definition Info';
                                               MSI_FSI         : Result := Result + ' Read Reply, Address Space = Function State Info'
                                             else
                                               Result := Result + ' Read Reply, Address Space = [Unknown]'      ;
                                             end;
                                           end
                    end; // Case
                    Result := Result + ', Starting Address = ' + IntToHex( LocalHelper.ExtractDataBytesAsInt(2, 5), 8);
                    if LocalHelper.Data[1] and $F8 = MCP_READ_OK then
                      Result := Result + ', Success'
                    else
                    if LocalHelper.Data[1] and $F8 = MCP_READ_ERROR then
                      Result := Result + ', Error'
                  end;
              MCP_READ_STREAM_REPLY  :
                  begin

                  end;
            end;
          end;
        end;

  end;

begin
  case MTI of
    MTI_CID0 : Result := 'Check ID 0';
    MTI_CID1 : Result := 'Check ID 1';
    MTI_CID2 : Result := 'Check ID 2';
    MTI_CID3 : Result := 'Check ID 3';
    MTI_CID4 : Result := 'Check ID 4';
    MTI_CID5 : Result := 'Check ID 5';
    MTI_CID6 : Result := 'Check ID 6';

    MTI_RID : Result := 'Reserve ID [RID]';
    MTI_AMD : Result := 'Alias Map Definition [AMD]';
    MTI_AME : Result := 'Alias Map Enquiry [AME]';
    MTI_AMR : Result := 'Alias Map Reset [AMR]';

    MTI_FRAME_TYPE_DATAGRAM_ONLY_FRAME : begin
                                           Result := 'Datagram Single Frame:';
                                           ConfigurationDatagram;
                                         end;
    MTI_FRAME_TYPE_DATAGRAM_FRAME_START : begin
                                           Result := 'Datagram Start Frame:';
                                           ConfigurationDatagram;
                                         end;
    MTI_FRAME_TYPE_DATAGRAM_FRAME : Result := 'Datagram Frame';
    MTI_FRAME_TYPE_DATAGRAM_FRAME_END : Result := 'Datagram End Frame';

    MTI_INITIALIZATION_COMPLETE : Result := 'Initialization Complete';
    MTI_VERIFY_NODE_ID_NUMBER_DEST : Result := 'Verify Node ID with Destination Address';
    MTI_VERIFY_NODE_ID_NUMBER      : Result := 'Verify Node ID Global';
    MTI_VERIFIED_NODE_ID_NUMBER    : Result := 'Verified Node ID';
    MTI_OPTIONAL_INTERACTION_REJECTED : Result := 'Optional Interaction Rejected';
    MTI_TERMINATE_DUE_TO_ERROR        : Result := 'Terminate Due to Error';

    MTI_PROTOCOL_SUPPORT_INQUIRY  : Result := 'Protocol Support Inquiry';
    MTI_PROTOCOL_SUPPORT_REPLY    : Result := 'Protocol Support Reply';

    MTI_TRACTION_PROXY_PROTOCOL   : Result := 'Protocol Traction Proxy';
    MTI_TRACTION_PROXY_REPLY      : Result := 'Protocol Traction Proxy Reply';

    MTI_CONSUMER_IDENTIFY              : Result := 'Consumer Identify';
    MTI_CONSUMER_IDENTIFY_RANGE        : Result := 'Consumer Identify Range';
    MTI_CONSUMER_IDENTIFIED_UNKNOWN    : Result := 'Consumer Identified Unknown';
    MTI_CONSUMER_IDENTIFIED_SET        : Result := 'Consumer Identified Valid';
    MTI_CONSUMER_IDENTIFIED_CLEAR      : Result := 'Consumer Identified Clear';
    MTI_CONSUMER_IDENTIFIED_RESERVED   : Result := 'Consumer Identified Reserved';
    MTI_PRODUCER_IDENDIFY              : Result := 'Producer Identify';
    MTI_PRODUCER_IDENTIFY_RANGE        : Result := 'Producer Identify Range';
    MTI_PRODUCER_IDENTIFIED_UNKNOWN    : Result := 'Producer Identified Unknown';
    MTI_PRODUCER_IDENTIFIED_SET        : Result := 'Producer Identified Valid';
    MTI_PRODUCER_IDENTIFIED_CLEAR      : Result := 'Producer Identified Clear';
    MTI_PRODUCER_IDENTIFIED_RESERVED   : Result := 'Producer Identified Reserved';
    MTI_EVENTS_IDENTIFY_DEST           : Result := 'Events Identify with Destination Address';
    MTI_EVENTS_IDENTIFY                : Result := 'Events Identify Global';
    MTI_EVENT_LEARN                    : Result := 'Event Learn';
    MTI_PC_EVENT_REPORT                : Result := 'Producer/Consumer Event Report [PCER] ';

    MTI_SIMPLE_NODE_INFO_REQUEST       : Result := 'Simple Node Info Request [SNIP]';
    MTI_SIMPLE_NODE_INFO_REPLY         : Result := 'Simple Node Info Reply [SNIP]';

    MTI_SIMPLE_TRAIN_INFO_REQUEST       : Result := 'Simple Train Node Info Request [STNIP]';
    MTI_SIMPLE_TRAIN_INFO_REPLY         : Result := 'Simple Train Node Info Reply [STNIP]';

    MTI_DATAGRAM_OK_REPLY              : begin
                                           Result := 'Datagram Reply OK';
                                           if LocalHelper.DataCount > 2 then
                                           begin
                                             if LocalHelper.Data[2] and DATAGRAM_OK_ACK_REPLY_PENDING = DATAGRAM_OK_ACK_REPLY_PENDING then
                                             begin
                                               if LocalHelper.Data[2] and $7F = 0 then
                                                 Result := Result + ' - Reply Is Pending - Maximum wait time = Infinity'
                                               else
                                                 Result := Result + ' - Reply Is Pending - Maximum wait time = ' + IntToStr( Round( Power(2, LocalHelper.Data[2] and $7F))) + ' seconds'
                                             end else
                                               Result := Result + ' - Reply Is Not Pending'
                                           end else
                                             Result := Result + ' - Does not include Extended Flags';
                                         end;
    MTI_DATAGRAM_REJECTED_REPLY        : Result := 'Datagram Rejected Reply';

    MTI_TRACTION_PROTOCOL              : Result := 'Traction Protocol';
    MTI_TRACTION_REPLY                 : Result := 'Traction Reply';
    MTI_STREAM_INIT_REQUEST            : Result := 'Stream Init Request';
    MTI_STREAM_INIT_REPLY              : Result := 'Stream Init Reply';
    MTI_FRAME_TYPE_CAN_STREAM_SEND     : Result := 'Stream Send - CAN Frame';
    MTI_STREAM_PROCEED                 : Result := 'Stream Proceed';
    MTI_STREAM_COMPLETE                : Result := 'Stream Complete';
   else
    Result := 'Unknown MTI';
  end;
  if LocalHelper.HasDestinationAddress and not IsDatagramMTI(LocalHelper.MTI, False) then
  begin
    if LocalHelper.FramingBits = $00 then
      Result := Result + ' Only Frame'
    else
    if LocalHelper.FramingBits = $10 then
      Result := Result + ' First Frame'
    else
    if LocalHelper.FramingBits = $20 then
      Result := Result + ' Last Frame'
    else
    if LocalHelper.FramingBits = $30 then
      Result := Result + ' Middle Frame'
    else
  end;
end;

function TractionProxyTechnologyToStr(Technology: Byte): WideString;
begin
  case Technology of
    TRACTION_PROXY_TECH_ID_DCC              : Result := 'DCC';
    TRACTION_PROXY_TECH_ID_DC               : Result := 'DC';
    TRACTION_PROXY_TECH_ID_MARKLIN_DIG      : Result := 'Marklin Digital';
    TRACTION_PROXY_TECH_ID_MARKLIN_DELTA    : Result := 'Marklin Delta';
    TRACTION_PROXY_TECH_ID_MARKLIN_DIG_ESU  : Result := 'Marklin Digital (ESU)';
    TRACTION_PROXY_TECH_ID_SELECTRIX        : Result := 'Selectrix';
    TRACTION_PROXY_TECH_ID_MTH_DCS          : Result := 'MTH DCS';
    TRACTION_PROXY_TECH_ID_LIONEL_TMCC      : Result := 'Lionel TMCC';
  end;
end;

function EventIDToString(EventID: PEventID): WideString;
var
  Address: Word;
begin
  if EqualEvents(EventID, @EVENT_IS_TRAIN) then
    Result := 'EVENT_TRAIN'
  else
  if EqualEvents(EventID, @EVENT_IS_PROXY) then
    Result := 'EVENT_PROXY'
  else
  if EqualEvents(EventID, @EVENT_DUPLICATE_ID_DETECTED) then
    Result := 'EVENT_DUPLICATE_NODE_ID'
  else
  if EqualEvents(EventID, @EVENT_EMERGENCY_STOP) then
    Result := 'EVENT_EMERGENCY_STOP'
  else
  if EqualEvents(EventID, @EVENT_NEW_LOG_ENTRY) then
    Result := 'EVENT_NEW_LOG_ENTRY'
  else
  if EqualEvents(EventID, @EVENT_IDENT_BUTTON_PRESSED) then
    Result := 'EVENT_IDENT_BUTTON_PRESSED'
  else
  if (EventID^[0] = $06) and (EventID^[1] = $01) then
  begin
    Address := ((EventID^[4] shl 8) or EventID^[5]) and $3FFF;  // Strip off the Extended bits if there are there
    if EventID^[4] and $C0 = $C0 then
      Result := 'EVENT_TRAIN_QUERY_DCC_ADDRESS : Extended Address = ' + IntToStr(Address) + ', (0x' + IntToHex(Address, 4) + ')'
    else
      Result := 'EVENT_TRAIN_QUERY_DCC_ADDRESS : Short Address = ' + IntToStr(Address) + ', (0x' + IntToHex(Address, 4) + ')'
  end
  else
    Result := 'Unique Event'
end;

function EqualEvents(Event1, Event2: PEventID): Boolean;
begin
  Result := (Event1^[0] = Event2^[0]) and (Event1^[1] = Event2^[1]) and (Event1^[2] = Event2^[2]) and (Event1^[3] = Event2^[3]) and
            (Event1^[4] = Event2^[4]) and (Event1^[5] = Event2^[5]) and (Event1^[6] = Event2^[6]) and (Event1^[7] = Event2^[7])
end;

constructor TMultiFrameBuffer.Create;
begin
   FAliasID := 0;
   FDataArraySize := 0;
   FCurrentIndex := 0;
end;

function TMultiFrameBuffer.ExtractDataBytesAsHex(StartByteIndex, EndByteIndex: Integer): string;
begin
  Result := IntToHex(ExtractDataBytesAsInt(StartByteIndex, EndByteIndex), EndByteIndex-StartByteIndex);
end;

function TMultiFrameBuffer.ExtractDataBytesAsInt(StartByteIndex,EndByteIndex: Integer): QWord;
var
  i, Offset, Shift: Integer;
  ByteAsQ, ShiftedByte: QWord;
begin
  Result := 0;
  Offset := EndByteIndex - StartByteIndex;
  for i := StartByteIndex to EndByteIndex do
  begin
    Shift := Offset * 8;
    ByteAsQ := QWord( DataArray[i]);
    ShiftedByte := ByteAsQ shl Shift;
    Result := Result or ShiftedByte;
    Dec(Offset)
  end;
end;

procedure TMultiFrameBufferList.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to List.Count - 1 do
      TObject( List[i]).Free;
    List.Count := 0;
  finally
    FreeAndNil(FList);
  end;
end;

constructor TMultiFrameBufferList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TMultiFrameBufferList.Destroy;
begin
  Clear;
  inherited Destroy
end;

function TMultiFrameBufferList.FindByAlias(TestAliasID: Word): TMultiFrameBuffer;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to List.Count - 1 do
  begin
    if MultiFrameBuffers[i].AliasID = TestAliasID then
    begin
      Result := MultiFrameBuffers[i];
      Exit;
    end;
  end;
end;

function TMultiFrameBufferList.GetMultiFrameBuffer(Index: Integer): TMultiFrameBuffer;
begin
  Result := TMultiFrameBuffer( List[Index])
end;

function TMultiFrameBufferList.ProcessFrame(NewFrame: TOpenLCBMessageHelper): TMultiFrameBuffer;
var
  Buffer: TMultiFrameBuffer;
  i: Integer;
begin
  Result := nil;

  Buffer := FindByAlias(NewFrame.DestinationAliasID);
  if not Assigned(Buffer) then
  begin
    Buffer := TMultiFrameBuffer.Create;
    Buffer.AliasID := NewFrame.DestinationAliasID;
    List.Add(Buffer);
  end;

  for i := 2 to NewFrame.DataCount - 1 do          // Skip the Alias
  begin
    Buffer.DataArray[Buffer.CurrentIndex] := NewFrame.Data[i];
    Inc(Buffer.FDataArraySize);
    Inc(Buffer.FCurrentIndex);
  end;

  if (NewFrame.FramingBits = $20) or (NewFrame.FramingBits = $00) then
  begin
    List.Remove(Buffer);
    Result := Buffer
  end;
end;

procedure TMultiFrameBufferList.SetMultiFrameBuffer(Index: Integer; AValue: TMultiFrameBuffer);
begin
  List[Index] := AValue
end;


{ TOpenLCBMessageHelper }

procedure TOpenLCBMessageHelper.SetData(AValue: TCANByteArray);
begin
  FData:=AValue;
end;

procedure TOpenLCBMessageHelper.SetLayer(AValue: TOpenLCBLayer);
begin
  if FLayer=AValue then Exit;
  FLayer:=AValue;
end;

constructor TOpenLCBMessageHelper.Create;
var
  i: Integer;
begin
  inherited Create;
  FLayer := ol_CAN;
  FMTI := 0;
  for i := 0 to CAN_BYTE_COUNT - 1 do
    FData[i] := 0;
  FDataCount := 0;
  FSourceAliasID := 0;
  FDestinationAliasID := 0;
  FForwardingBitNotSet := False;
  FUnimplementedBitsSet := False;
  FFramingBits := 0;
end;

destructor TOpenLCBMessageHelper.Destroy;
begin
  inherited Destroy
end;

procedure TOpenLCBMessageHelper.CopyTo(Target: TOpenLCBMessageHelper);
begin
  Target.FDestinationAliasID := DestinationAliasID;
  Target.FHasDestinationAddress := HasDestinationAddress;
  Target.FForwardingBitNotSet := ForwardingBitNotSet;
  Target.FSourceAliasID := SourceAliasID;
  Target.FData := Data;
  Target.FDataCount := DataCount;
  Target.FLayer := Layer;
  Target.FMTI := MTI;
  Target.FUnimplementedBitsSet := UnimplementedBitsSet;
end;

function TOpenLCBMessageHelper.Decompose(MessageStr: AnsiString): Boolean;
var
  x, n, SemiColon, i: Integer;
  ByteStr: AnsiString;
begin
  Result := False;
  if MessageStr <> '' then
  begin
    MessageStr := UpperCase(MessageStr);

    x := Pos('X', MessageStr);         // Find were the "X" is in the string
    if x > 0 then
    begin
      n := PosEx('N', MessageStr, x);  // Find where the "N" is in the string
      if n > 0 then
      begin
        Result := True;           // At least it has an X and a N
        MessageStr[n] := #0;           // Set the "N" to a null to create a null string of the MTI
        Inc(n);                        // Move just pass where the "N" was
        SemiColon := PosEx(';', MessageStr, n);  // Look for the terminating ";"
        if SemiColon > 0 then
        begin
          MTI := StrToInt('$' + PAnsiChar( @MessageStr[x+1])); // Convert the string MTI into a number
          SourceAliasID := MTI and $00000FFF;                  // Strip off the Source Alias
          if MTI and $08000000 = $08000000 then                // Was this an OpenLCB or CAN message?
            Layer := ol_OpenLCB
          else
            Layer := ol_CAN;

          FForwardingBitNotSet := MTI and $10000000 = $00000000;    // Check if the Forwarding Bit was set
          FUnimplementedBitsSet := MTI and $E0000000 <> $00000000;  // Check to see the state of the unimplemented bits

          MTI := MTI and not $10000000;    // Strip off the reserved bits
          MTI := MTI and $FFFFF000;        // Strip off the Source Alias

          if Layer = ol_CAN then
          begin
            if MTI and MTI_CID_MASK <> 0 then
              MTI := MTI and MTI_CID_MASK;
          end;

          for i := 0 to CAN_BYTE_COUNT - 1 do
            Data[i] := 0;

          // Convert the CAN payload bytes into numbers
          FDataCount := 0;
          i := n;
          while i < SemiColon do
          begin
            ByteStr := MessageStr[i] + MessageStr[i+1];
            Data[FDataCount] := StrToInt('$'+ByteStr);
            Inc(i, 2);
            Inc(FDataCount);
          end;

          // Determine if the message has a destination address and if so store it
          HasDestinationAddress := False;
          FramingBits := 0;
          if Layer = ol_OpenLCB then
          begin
            if MTI and MTI_FRAME_TYPE_MASK > MTI_FRAME_TYPE_GENERAL then        // See if the destination Alias is in the MTI
            begin
              DestinationAliasID := (MTI and $00FFF000) shr 12;
              MTI := MTI and $FF000FFF;
              HasDestinationAddress := True;
            end else
            begin
              if MTI and MTI_ADDRESS_PRESENT = MTI_ADDRESS_PRESENT then
              begin
                DestinationAliasID := Word( (Data[0] shl 8) and $0FFF) or (Data[1]);
                FramingBits := Data[0] and $30;
                HasDestinationAddress := True;
              end
            end
          end
        end
      end
    end
  end;
end;

function TOpenLCBMessageHelper.Encode: AnsiString;
var
  i: Integer;
  FullMTI: DWord;
begin
  FullMTI := MTI or SourceAliasID;
  FullMTI := FullMTI or $10000000;
  if Layer = ol_OpenLCB then
    FullMTI := FullMTI or $08000000;

  if MTI and MTI_FRAME_TYPE_MASK > MTI_FRAME_TYPE_GENERAL then
  begin
    // Datagram or Stream
    FullMTI := FullMTI or (DWord( DestinationAliasID) shl 12);
    Result := ':X' + IntToHex(FullMTI, 8) + 'N';
    for i := 0 to DataCount - 1 do
      Result := Result + IntToHex(Data[i], 2);
  end else
  begin
    Result := ':X' + IntToHex(FullMTI, 8) + 'N';
    for i := 0 to DataCount - 1 do
    begin
      if (i < 2) and (DestinationAliasID <> 0) then
      begin
        if i = 0 then
          Result := Result + IntToHex(((DestinationAliasID shr 8) or FramingBits) and $00FF, 2)
        else
          Result := Result + IntToHex(DestinationAliasID and $00FF, 2)
      end else
        Result := Result + IntToHex(Data[i], 2);
    end;
  end;
  Result := Result  + ';'
end;

function TOpenLCBMessageHelper.ExtractDataBytesAsEventID: PEventID;
begin
  Result := @Data;
end;

procedure TOpenLCBMessageHelper.Load(ALayer: TOpenLCBLayer; AMTI: DWord;
  ASourceAlias: Word; ADestinationAlias: Word; ADataCount: Integer; AData0,
  AData1, AData2, AData3, AData4, AData5, AData6, AData7: Byte);
begin
  Layer := ALayer;
  MTI := AMTI;
  DataCount := ADataCount;
  SourceAliasID := ASourceAlias;
  DestinationAliasID := ADestinationAlias;
  Data[0] := AData0;
  Data[1] := AData1;
  Data[2] := AData2;
  Data[3] := AData3;
  Data[4] := AData4;
  Data[5] := AData5;
  Data[6] := AData6;
  Data[7] := AData7;
end;

procedure TOpenLCBMessageHelper.StoreNodeIDToData(NodeID: Int64; IsAddressed: Boolean);
var
  Offset: Integer;
begin
  if IsAddressed then
    Offset := 2
  else
    Offset := 0;
  Data[0+Offset] := (NodeID shr 40) and $000000FF;
  Data[1+Offset] := (NodeID shr 32) and $000000FF;
  Data[2+Offset] := (NodeID shr 24) and $000000FF;
  Data[3+Offset] := (NodeID shr 16) and $000000FF;
  Data[4+Offset] := (NodeID shr 8) and $000000FF;
  Data[5+Offset] := (NodeID) and $000000FF;
  DataCount := 6 + Offset;
end;

function TOpenLCBMessageHelper.ExtractDataBytesAsInt(StartByteIndex, EndByteIndex: Integer): QWord;
var
  i, Offset, Shift: Integer;
  ByteAsQ, ShiftedByte: QWord;
begin
  Result := 0;
  Offset := EndByteIndex - StartByteIndex;
  for i := StartByteIndex to EndByteIndex do
  begin
    Shift := Offset * 8;
    ByteAsQ := QWord( Data[i]);
    ShiftedByte := ByteAsQ shl Shift;
    Result := Result or ShiftedByte;
    Dec(Offset)
  end;
end;

function TOpenLCBMessageHelper.ExtractDataBytesAsString(StartIndex, Count: Integer): String;
var
  i: Integer;
begin
  Result := '';
  for i := StartIndex to Count - 1 do
    Result := Result + Chr( Data[i]);
end;

procedure TOpenLCBMessageHelper.IntToByteArray(Int: QWord; var ByteArray: TCANByteArray);
begin
  ByteArray[0] := Int and $000000FF;
  ByteArray[1] := (Int shr 8) and $000000FF;
  ByteArray[2] := (Int shr 16) and $000000FF;
  ByteArray[3] := (Int shr 24) and $000000FF;
  ByteArray[4] := (Int shr 32) and $000000FF;
  ByteArray[5] := (Int shr 40) and $000000FF;
  ByteArray[6] := (Int shr 48) and $000000FF;
  ByteArray[7] := (Int shr 56) and $000000FF;
end;

function IsDatagramMTI(MTI: DWord; IncludeReplies: Boolean): Boolean;
begin
  Result := (MTI = MTI_FRAME_TYPE_DATAGRAM_FRAME_END) or (MTI = MTI_FRAME_TYPE_DATAGRAM_ONLY_FRAME) or (MTI = MTI_FRAME_TYPE_DATAGRAM_FRAME) or (MTI = MTI_FRAME_TYPE_DATAGRAM_FRAME_START);
  if IncludeReplies then
    Result := Result or (MTI = MTI_DATAGRAM_OK_REPLY) or (MTI = MTI_DATAGRAM_REJECTED_REPLY)
end;

function IsStreamMTI(MTI: DWord; IncludeSetupTeardowns: Boolean): Boolean;
begin
  if IncludeSetupTeardowns then
    Result := (MTI = MTI_FRAME_TYPE_CAN_STREAM_SEND) or  (MTI = MTI_STREAM_COMPLETE) or  (MTI = MTI_STREAM_INIT_REPLY) or  (MTI = MTI_STREAM_INIT_REQUEST) or  (MTI = MTI_STREAM_PROCEED)
  else
    Result := (MTI = MTI_FRAME_TYPE_CAN_STREAM_SEND)
end;

function GridConnectToJMRI(GridStr: AnsiString): AnsiString;
var
  NPos: integer;
  Header: PChar;
  i: Integer;
begin
  Result := GridStr;
  NPos := Pos('N', GridStr);
  GridStr[NPos] := #0;
  Header := @GridStr[3];
  Result := '[' + Header + ']';
  Header := @GridStr[NPos] + 1;
  if Header^ <> ';' then
    Result := Result + ' ';
  while Header^ <> ';' do
  begin
    Result := Result + Header^;
    Inc(Header);
    if Header^ = ';' then
      Break;
    Result := Result + Header^ + ' ';
    Inc(Header);
  end;
  Result := Trim(Result);
  for i := 0 to (40 - Length(Result)) do
    Result := Result + ' ';  // Pad them all to the same length
end;

function MessageToDetailedMessage(MessageString: string; Sending: Boolean): string;
var
  j, S_Len: Integer;
  f: single;
  Half: Word;
  MultiFrame: TMultiFrameBuffer;
begin
  if LocalHelper.Decompose(MessageString) then
  begin
    Result := MessageString;
    S_Len := Length(Result);
    for j := 0 to (28-S_Len) do
      Result := Result + ' ' ;

    if Sending then
      Result := Result + '  Send:   '
    else
      Result := Result + '  Receive: ';

    Result := Result + 'From = 0x' + IntToHex( LocalHelper.SourceAliasID, 4);

    if IsDatagramMTI(LocalHelper.MTI, False) then
      Result := Result + RawHelperDataToStr(LocalHelper, True) + ' MTI: ' + MTI_ToString(LocalHelper.MTI)
    else
      Result := Result + '   MTI: ' + MTI_ToString(LocalHelper.MTI) + ' - ';

    if IsStreamMTI( LocalHelper.MTI, True) then
    begin
      case LocalHelper.MTI of
        MTI_STREAM_INIT_REQUEST            : Result := Result + ' Suggested Bufer Size: ' + IntToStr((Localhelper.Data[2] shl 8) or LocalHelper.Data[3]) + ' Flags: 0x' + IntToHex(LocalHelper.Data[4], 2) + ' Additional Flags: 0x' + IntToHex(LocalHelper.Data[5], 2) + ' Source Stream ID: ' + IntToStr(LocalHelper.Data[6]);
        MTI_STREAM_INIT_REPLY              : Result := Result + ' Negotiated Bufer Size: ' + IntToStr((Localhelper.Data[2] shl 8) or LocalHelper.Data[3]) + ' Flags: 0x' + IntToHex(LocalHelper.Data[4], 2) + ' Additional Flags: 0x' + IntToHex(LocalHelper.Data[5], 2) + ' Source Stream ID: ' + IntToStr(LocalHelper.Data[6]) + ' Destination Stream ID: ' + IntToStr(LocalHelper.Data[7]);
        MTI_FRAME_TYPE_CAN_STREAM_SEND     : begin end;
        MTI_STREAM_PROCEED                 : Result := Result + ' Source Stream ID: ' + IntToStr(Localhelper.Data[2]) + ' Destination Stream ID: ' + IntToStr(LocalHelper.Data[3]) + ' Flags: 0x' + IntToHex(LocalHelper.Data[4], 2) + ' Additional Flags: 0x' + IntToHex(LocalHelper.Data[5], 2);
        MTI_STREAM_COMPLETE                : Result := Result + ' Source Stream ID: ' + IntToStr(Localhelper.Data[2]) + ' Destination Stream ID: ' + IntToStr(LocalHelper.Data[3]) + ' Flags: 0x' + IntToHex(LocalHelper.Data[4], 2) + ' Additional Flags: 0x' + IntToHex(LocalHelper.Data[5], 2);
      end
    end;

    if LocalHelper.MTI = MTI_OPTIONAL_INTERACTION_REJECTED then
    begin
    end;

    // SNII/SNIP
    if LocalHelper.MTI = MTI_SIMPLE_NODE_INFO_REPLY then
      Result := Result + RawHelperDataToStr(LocalHelper, True);

    // STNIP
    if LocalHelper.MTI = MTI_SIMPLE_TRAIN_INFO_REPLY then
      Result := Result + RawHelperDataToStr(LocalHelper, True);

    // Events
    if (LocalHelper.MTI = MTI_PRODUCER_IDENDIFY) or (LocalHelper.MTI = MTI_PRODUCER_IDENTIFIED_SET) or (LocalHelper.MTI = MTI_PRODUCER_IDENTIFIED_CLEAR) or
      (LocalHelper.MTI = MTI_PRODUCER_IDENTIFIED_UNKNOWN) or (LocalHelper.MTI = MTI_CONSUMER_IDENTIFY) or (LocalHelper.MTI = MTI_CONSUMER_IDENTIFIED_SET) or
      (LocalHelper.MTI = MTI_CONSUMER_IDENTIFIED_CLEAR) or (LocalHelper.MTI = MTI_CONSUMER_IDENTIFIED_UNKNOWN) or (LocalHelper.MTI = MTI_PC_EVENT_REPORT)
    then begin
        Result := Result + 'EventID: ' + EventIDToString(@LocalHelper.Data);
    end;

    // Traction Protocol
    if LocalHelper.MTI = MTI_TRACTION_PROTOCOL then
    begin
      MultiFrame := MultiFrames.ProcessFrame(LocalHelper);
      if Assigned(MultiFrame) then
      begin
        case MultiFrame.DataArray[0] of
            TRACTION_SPEED_DIR :
              begin
                Result := Result + 'OLCB Speed/Dir Operation; Speed = ';
                f := HalfToFloat( (MultiFrame.DataArray[1] shl 8) or MultiFrame.DataArray[2]);
                if f= 0 then
                begin
                  if DWord( f) and $80000000 = $80000000 then
                    Result := Result + '-0.0'
                  else
                    Result := Result + '+0.0'
                end else
                  Result := Result + IntToStr( round(f));
              end;
            TRACTION_FUNCTION : Result := Result + 'OLCB Traction Operation, Function Address: ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(1, 3)) + ' [0x' + IntToHex( MultiFrame.ExtractDataBytesAsInt(1, 3), 4) + '], Value: ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(4, 5)) + ' [0x' + IntToHex( MultiFrame.ExtractDataBytesAsInt(4, 5), 2) + ']';
            TRACTION_E_STOP : Result := Result + 'OLCB Traction Emergency Stop';
            TRACTION_QUERY_SPEED : Result := Result + 'Query Speeds';
            TRACTION_QUERY_FUNCTION : Result := Result + 'Query Function - Address: ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(1, 3));
            TRACTION_CONTROLLER_CONFIG :
              begin;
                case MultiFrame.DataArray[1] of
                  TRACTION_CONTROLLER_CONFIG_ASSIGN : Result := Result + 'Controller Config - Assign - Flags: ' + MultiFrame.ExtractDataBytesAsHex(2, 2) + ' Controller ID ' + MultiFrame.ExtractDataBytesAsHex(3, 8) + ' [Alias: ' + MultiFrame.ExtractDataBytesAsHex(9, 10) + ']';
                  TRACTION_CONTROLLER_CONFIG_RELEASE : Result := Result + 'Controller Config - Release - Flags: ' + MultiFrame.ExtractDataBytesAsHex(2, 2) + ' Controller ID ' + MultiFrame.ExtractDataBytesAsHex(3, 8) + ' [Alias: ' + MultiFrame.ExtractDataBytesAsHex(9, 10) + ']';
                  TRACTION_CONTROLLER_CONFIG_QUERY : Result := Result + 'Controller Config - Query';
                  TRACTION_CONTROLLER_CONFIG_NOTIFY : Result := Result + 'Controller Config - Notify - Flags: ' + MultiFrame.ExtractDataBytesAsHex(2, 2) + ' Controller ID ' + MultiFrame.ExtractDataBytesAsHex(3, 8) + ' [Alias: ' + MultiFrame.ExtractDataBytesAsHex(9, 10) + ']'
                end
              end;
            TRACTION_CONSIST :
              begin
                case MultiFrame.DataArray[1] of
                  TRACTION_CONSIST_ATTACH : Result := Result + 'Consist Config Attach';
                  TRACTION_CONSIST_DETACH : Result := Result + 'Consist Config Detach';
                  TRACTION_CONSIST_QUERY : Result := Result + 'Consit Config Query';
                end
              end;
            TRACTION_MANAGE :
              begin
                case MultiFrame.DataArray[1] of
                    TRACTION_MANAGE_RESERVE : Result := Result + 'Traction Management - Reserve';
                    TRACTION_MANAGE_RELEASE : Result := Result + 'Traction Management - Release'
                end
              end
        else
          Result := Result + 'Unknown Traction Operation';
        end;

        FreeAndNil(MultiFrame);
      end;
    end;

    // Traction Protocol Reply
    if LocalHelper.MTI = MTI_TRACTION_REPLY then
    begin
      MultiFrame := MultiFrames.ProcessFrame(LocalHelper);
      if Assigned(MultiFrame) then
      begin
        case MultiFrame.DataArray[0] of
            TRACTION_SPEED_DIR :
              begin
                Result := Result + 'OLCB Speed/Dir Operation; Speed = ';
                f := HalfToFloat( (MultiFrame.DataArray[1] shl 8) or MultiFrame.DataArray[2]);
                if f= 0 then
                begin
                  if DWord( f) and $80000000 = $80000000 then
                    Result := Result + '-0.0'
                  else
                    Result := Result + '+0.0'
                end else
                  Result := Result + IntToStr( round(f));
              end;
            TRACTION_FUNCTION : Result := Result + 'OLCB Traction Operation, Function Address = ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(1, 3)) + ' [0x' + IntToHex( MultiFrame.ExtractDataBytesAsInt(1, 3), 4) + '], Value = ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(4, 5)) + ' [0x' + IntToHex( MultiFrame.ExtractDataBytesAsInt(4, 5), 2) + ']';
            TRACTION_E_STOP : Result := Result + 'OLCB Traction Emergency Stop';
            TRACTION_QUERY_SPEED :
              begin
                Result := Result + 'Query Speed Reply : Set Speed = ';
                  Half := (MultiFrame.DataArray[1] shl 8) or MultiFrame.DataArray[2];
                  if Half = $FFFF then
                  begin
                    Result := Result + 'NaN'
                  end else
                  begin
                    f := HalfToFloat( Half);
                    if f = 0 then
                    begin
                      if DWord( f) and $80000000 = $80000000 then
                        Result := Result + '-0.0'
                      else
                        Result := Result + '+0.0'
                    end else
                      Result := Result + IntToStr( round(f));
                  end;

                  Result := Result + ': Status = ' + MultiFrame.ExtractDataBytesAsHex(3, 3);

                  Result := Result + ': Commanded Speed = ';
                  Half := (MultiFrame.DataArray[4] shl 8) or MultiFrame.DataArray[5];
                  if Half = $FFFF then
                  begin
                    Result := Result + 'NaN'
                  end else
                  begin
                    f := HalfToFloat( Half);
                    if f = 0 then
                    begin
                      if DWord( f) and $80000000 = $80000000 then
                        Result := Result + '-0.0'
                      else
                        Result := Result + '+0.0'
                    end else
                      Result := Result + IntToStr( round(f));
                  end;

                  Result := Result + ': Actual Speed = ';
                  Half := (MultiFrame.DataArray[6] shl 8) or MultiFrame.DataArray[7];
                  if Half = $FFFF then
                  begin
                    Result := Result + 'NaN'
                  end else
                  begin
                    f := HalfToFloat( Half);
                    if f = 0 then
                    begin
                      if DWord( f) and $80000000 = $80000000 then
                        Result := Result + '-0.0'
                      else
                        Result := Result + '+0.0'
                    end else
                      Result := Result + IntToStr( round(f));
                  end
              end;
            TRACTION_QUERY_FUNCTION : Result := Result + 'Query Function - Address: ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(1, 3)) + ', Value: ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(4, 5));
            TRACTION_CONTROLLER_CONFIG :
              begin;
                case MultiFrame.DataArray[1] of
                  TRACTION_CONTROLLER_CONFIG_ASSIGN : Result := Result + 'Controller Config Reply - Assign - Flags = ' + MultiFrame.ExtractDataBytesAsHex(2, 2);
                  TRACTION_CONTROLLER_CONFIG_QUERY : Result := Result + 'Controller Config Reply - Query - Flags = ' + MultiFrame.ExtractDataBytesAsHex(2, 2) + ' Controller ID ' + MultiFrame.ExtractDataBytesAsHex(3, 8) + ' [Alias: ' + MultiFrame.ExtractDataBytesAsHex(9, 10) + ']';
                  TRACTION_CONTROLLER_CONFIG_NOTIFY : Result := Result + 'Controller Config Reply - Notify - Flags = ' + MultiFrame.ExtractDataBytesAsHex(2, 2) + ' New Controller ID ' + MultiFrame.ExtractDataBytesAsHex(3, 8) + ' [Alias: ' + MultiFrame.ExtractDataBytesAsHex(9, 10) + ']'
                end
              end;
            TRACTION_CONSIST :
              begin
                case MultiFrame.DataArray[1] of
                  TRACTION_CONSIST_ATTACH : Result := Result + 'Consist Config Reply Attach';
                  TRACTION_CONSIST_DETACH : Result := Result + 'Consist Config Reply Detach';
                  TRACTION_CONSIST_QUERY : Result := Result + 'Consit Config Reply Query';
                end
              end;
            TRACTION_MANAGE :
              begin
                case MultiFrame.DataArray[1] of
                    TRACTION_MANAGE_RESERVE : Result := Result + 'Traction Management Reply - Reserve';
                end
              end
        else
          Result := Result + 'Unknown Traction Reply Operation';
        end;

        FreeAndNil(MultiFrame);
      end;
    end;

    if LocalHelper.MTI = MTI_TRACTION_PROXY_PROTOCOL then
    begin
      MultiFrame := MultiFrames.ProcessFrame(LocalHelper);
      if Assigned(MultiFrame) then
      begin
        case MultiFrame.DataArray[0] of
            TRACTION_PROXY_ALLOCATE : Result := Result + 'Allocate: Technology = ' + TractionProxyTechnologyToStr(MultiFrame.DataArray[1]) + ' Train ID = ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(2, 3) and not $C000) + ' [0x' + IntToHex( MultiFrame.ExtractDataBytesAsInt(2, 3), 4) + '] Speed Steps = ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(4, 4));
            TRACTION_PROXY_ATTACH   : Result := Result + 'Attach: Technology = ' + TractionProxyTechnologyToStr(MultiFrame.DataArray[1]) + ' Train ID = ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(1, 2) and not $C000) + ' [0x' + IntToHex( MultiFrame.ExtractDataBytesAsInt(1, 2), 4) + ']' ;
            TRACTION_PROXY_DETACH   : Result := Result + 'Detach: Technology = ' + TractionProxyTechnologyToStr(MultiFrame.DataArray[1]) + ' Train ID = ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(1, 2) and not $C000) + ' [0x' + IntToHex( MultiFrame.ExtractDataBytesAsInt(1, 2), 4) + ']' ;
            TRACTION_PROXY_MANAGE   :
              begin
                case LocalHelper.Data[3] of
                    TRACTION_PROXY_MANAGE_RESERVE : Result := Result + 'Manage: Reserve';
                    TRACTION_PROXY_MANAGE_RELEASE : Result := Result + 'Manage: Release'
                else
                  Result := Result + 'Unknown Traction Manage Command'
                end;
              end
        else
          Result := Result + 'Unknown Traction Proxy Command';
        end;

        FreeAndNil(MultiFrame);
      end;
    end;

    if LocalHelper.MTI = MTI_TRACTION_PROXY_REPLY then
    begin
      MultiFrame := MultiFrames.ProcessFrame(LocalHelper);
      if Assigned(MultiFrame) then
      begin
         case MultiFrame.DataArray[0] of
            TRACTION_PROXY_ALLOCATE : Result := Result + 'Flags = ' + IntToStr(MultiFrame.DataArray[1]) + ', Allocate: Technology = ' + TractionProxyTechnologyToStr(MultiFrame.DataArray[2]) + ', Train ID = ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(3, 4)  and not $C000) +  ' [0x' + IntToHex( MultiFrame.ExtractDataBytesAsInt(3, 4), 4) + '], Train NodeID ' + MultiFrame.ExtractDataBytesAsHex(5, 10) + ' [Alias: ' + MultiFrame.ExtractDataBytesAsHex(11, 12) + ']';
            TRACTION_PROXY_ATTACH   : Result := Result + 'Attach: ReplyCode = ' + IntToHex(MultiFrame.DataArray[1], 2);
            TRACTION_PROXY_MANAGE   :
              begin
                case LocalHelper.Data[3] of
                    TRACTION_PROXY_MANAGE_RESERVE : Result := Result + 'Manage: Reserve';
                else
                  Result := Result + 'Unknown Traction Manage Command'
                end;
              end
        else
          Result := Result + 'Unknown Traction Proxy Command';
        end;

        FreeAndNil(MultiFrame);
      end;
    end;
  end;
end;

function ProtocolSupportReplyToString(Mask: QWord): string;
begin
  case Mask of
    PIP_PIP                    : Result := STR_PIP_PIP;
    PIP_DATAGRAM               : Result := STR_PIP_DATAGRAM;
    PIP_STREAM                 : Result := STR_PIP_STREAM;
    PIP_MEMORY_CONFIG          : Result := STR_PIP_MEMORY_CONFIG;
    PIP_RESERVATION            : Result := STR_PIP_RESERVATION;
    PIP_EVENT_EXCHANGE         : Result := STR_PIP_EVENT_EXCHANGE;
    PIP_IDENTIFCIATION         : Result := STR_PIP_IDENTIFCIATION;
    PIP_TEACH_LEARN            : Result := STR_PIP_TEACH_LEARN;
    PIP_REMOTE_BUTTON          : Result := STR_PIP_REMOTE_BUTTON;
    PIP_ABBREVIATED_CDI        : Result := STR_PIP_ABBREVIATED_CDI;
    PIP_DISPLAY                : Result := STR_PIP_DISPLAY;
    PIP_SIMPLE_NODE_ID         : Result := STR_PIP_SIMPLE_NODE_ID;
    PIP_CDI                    : Result := STR_PIP_CDI;
    PIP_TRACTION               : Result := STR_PIP_TRACTION;
    PIP_FDI                    : Result := STR_PIP_FDI;
    PIP_TRACTION_PROXY         : Result := STR_PIP_TRACTION_PROTOCOL;
  else
    Result := '[Unknown Protocol]';
  end;
end;

function AddressSpaceToString(AddressSpace: Byte): string;
begin
  case AddressSpace of
    $FF : Result := 'Configuration Definition Info (CDI)';
    $FE : Result := 'All Memory';
    $FD : Result := 'Configruation Memory';
    $FC : Result := 'Mfg ACDI Memory';
    $FB : Result := 'User ACDI Memory';
    $FA : Result := 'Function Definition Information Memory';
    $F9 : Result := 'Function State Information Memory';
  else
    Result := '[Unknown Memory]';
  end;
end;

function NodeIDToDotHex(NodeID: QWord): string;
var
  Dot: string;
begin
  Dot := '.';
  Result := IntToHex(NodeID, 12);
  Insert(Dot, Result, 3);
  Insert(Dot, Result, 6);
  Insert(Dot, Result, 9);
  Insert(Dot, Result, 12);
  Insert(Dot, Result, 15);
end;

function NodeIDToDotHex(NodeInfo: TNodeID): string;
begin
  Result := NodeIDToDotHex((QWord( NodeInfo[1]) shl 24 ) or NodeInfo[0])
end;

function DotHexToNodeID(NodeID: string): QWord;
begin
  Result := StrToInt( StringReplace(NodeID, '.', '', [rfReplaceAll, rfIgnoreCase]));
end;

function EventToDoxHex(Event: TEventID): string;
var
  i: Integer;
  ByteStr: string;
begin
  Result := '';
  for i := 0 to MAX_EVENT_LEN - 1 do
  begin
    ByteStr := IntToHex(Event[i], 2);
    if Length(ByteStr) < 1 then
      ByteStr := '0' + ByteStr;
    if i < MAX_EVENT_LEN - 1 then
      Result := Result + ByteStr + '.'
    else
      Result := Result + ByteStr
  end;
end;

function DotHexToEvent(Event: string): TEventID;
var
  SubStr: string;
  i, iByteCount: Integer;
begin
  Result[0] := 0;
  Result[1] := 0;
  Result[2] := 0;
  Result[3] := 0;
  Result[4] := 0;
  Result[5] := 0;
  Result[6] := 0;
  Result[7] := 0;
  iByteCount := 0;
  SubStr := '';
  Event := Event + #0;
  for i := 1 to Length(Event) do
  begin
    if (Event[i] <> '.') and (Event[i] <> #0) then
    begin
      SubStr := SubStr + Event[i];
    end else
    begin
      Result[iByteCount] := Hex2Dec(SubStr);
      Inc(iByteCount);
      SubStr := '';
    end;
  end;

end;

function IntToHexArray(Value: Integer): THexArray;
var
  Str: string;
begin
  Str := IntToHex(Value, 16);
  Result[0] := Hex2Dec( Str[15]+Str[16]);
  Result[1] := Hex2Dec( Str[13]+Str[14]);
  Result[2] := Hex2Dec( Str[11]+Str[12]);
  Result[3] := Hex2Dec( Str[9]+Str[10]);
  Result[4] := Hex2Dec( Str[7]+Str[8]);
  Result[5] := Hex2Dec( Str[5]+Str[6]);
  Result[6] := Hex2Dec( Str[3]+Str[4]);
  Result[7] := Hex2Dec( Str[1]+Str[2]);
end;

function StrToHexArray(Value: string): THexArray;
begin
  Result := IntToHexArray( StrToInt(Value))
end;

function RawHelperDataToStr(HelperData: TOpenLCBMessageHelper; ASCII: Boolean): string;
var
  j, iStart: Integer;
begin
  Result := '';
//  if HelperData.HasDestinationAddress then
 //   iStart := 2
//  else

  iStart := 0;
  Result := Result + ' [';
  for j := iStart to HelperData.DataCount - 1 do                     // Skip the Address
  begin
    if ASCII then
    begin
      if IsPrintableChar( Chr( HelperData.Data[j])) then
        Result := Result + Chr( HelperData.Data[j])
      else
        Result := Result + '.';
    end else
    begin
      Result := Result + IntToHex(HelperData.Data[j], 2);
      if j < HelperData.DataCount then
        Result := Result + '.'
    end;
  end;
  Result := Result + ']';
end;

procedure PrintToSynMemo(MessageStr: AnsiString; SynMemoLog: TSynMemo; Paused: Boolean; Detailed: Boolean; JMRIFormat: Boolean);
var
  SemiColonPos: Integer;
  Header: PChar;
  i: Integer;
begin
  if not Paused then
  begin
     SynMemoLog.BeginUpdate();
     try
       LogStrings.Text := MessageStr;
       for i := 0 to LogStrings.Count - 1 do
       begin
         if JMRIFormat then
         begin
           if Detailed then
           begin
              LogStrings[i] := MessageToDetailedMessage(LogStrings[i], True);
              SemiColonPos := Pos(';',  LogStrings[i]);
              Header := @LogStrings[i][SemiColonPos] + 1;
              SynMemoLog.Lines.Add( GridConnectToJMRI(LogStrings[i]) + Header);
           end else
             SynMemoLog.Lines.Add( GridConnectToJMRI(LogStrings[i]));
         end else
         begin
           if Detailed then
             SynMemoLog.Lines.Add(MessageToDetailedMessage(LogStrings[i], True))
           else
             SynMemoLog.Lines.Add(LogStrings[i]);
         end;
       end;
     finally
       SynMemoLog.CaretY := SynMemoLog.LineHeight * SynMemoLog.Lines.Count;
       SynMemoLog.EndUpdate;
     end;
  end;
end;

function DeleteLineBreaks(const S: Ansistring): Ansistring;
var
   Source, SourceEnd: PChar;
begin
   Source := Pointer(S) ;
   SourceEnd := Source + Length(S) ;
   while Source < SourceEnd do
   begin
     case Source^ of
       #10: Source^ := #32;
       #13: Source^ := #32;
     end;
     Inc(Source) ;
   end;
   Result := Trim(S);
end;


initialization
  LocalHelper := TOpenLCBMessageHelper.Create;
  MultiFrames := TMultiFrameBufferList.Create;
  LogStrings := TStringList.Create;

finalization
  FreeAndNil(MultiFrames);
  FreeAndNil(LocalHelper);
  FreeAndNil(LogStrings);


end.

