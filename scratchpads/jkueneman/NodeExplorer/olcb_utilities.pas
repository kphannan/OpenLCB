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
  Classes, SysUtils, strutils, ExtCtrls, unitolcb_defines,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ELSE}
  LclIntf,
  {$ENDIF}
  DOM, XMLRead, XMLWrite;

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
  TByteArray = array[0..CAN_BYTE_COUNT-1] of Byte;
  PByteArray = ^TByteArray;

  TOpenLCBLayer = (ol_CAN, ol_OpenLCB);

  { TOpenLCBMessage }

  { TOpenLCBMessageHelper }

  TOpenLCBMessageHelper = class
  private
    FDestinationAliasID: Word;
    FHasDestinationAddress: Boolean;
    FForwardingBitNotSet: Boolean;
    FSourceAliasID: Word;
    FData: TByteArray;
    FDataCount: Integer;
    FLayer: TOpenLCBLayer;
    FMTI: DWord;
    FUnimplementedBitsSet: Boolean;
    procedure SetData(AValue: TByteArray);
    procedure SetLayer(AValue: TOpenLCBLayer);
  public
    property Layer: TOpenLCBLayer read FLayer write SetLayer;
    property MTI: DWord read FMTI write FMTI;
    property Data: TByteArray read FData write SetData;
    property DataCount: Integer read FDataCount write FDataCount;
    property SourceAliasID: Word read FSourceAliasID write FSourceAliasID;
    property DestinationAliasID: Word read FDestinationAliasID write FDestinationAliasID;
    property ForwardingBitNotSet: Boolean read FForwardingBitNotSet write FForwardingBitNotSet;
    property UnimplementedBitsSet: Boolean read FUnimplementedBitsSet write FUnimplementedBitsSet;
    property HasDestinationAddress: Boolean read FHasDestinationAddress write FHasDestinationAddress;

    constructor Create;
    destructor Destroy; override;
    procedure Decompose(MessageStr: AnsiString);
    function Encode: AnsiString;
    procedure Load(ALayer: TOpenLCBLayer; AMTI: DWord; ASourceAlias: Word; ADestinationAlias: Word; ADataCount: Integer; AData0, AData1, AData2, AData3, AData4, AData5, AData6, AData7: Byte);
    procedure StoreNodeIDToData(NodeID: Int64; IsAddressed: Boolean);
    function ExtractDataBytesAsInt(StartByteIndex, EndByteIndex: Integer): QWord;
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

  function GetTickCount : DWORD;

implementation

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

{ TOpenLCBMessageHelper }

procedure TOpenLCBMessageHelper.SetData(AValue: TByteArray);
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
end;

destructor TOpenLCBMessageHelper.Destroy;
begin
  inherited Destroy
end;

procedure TOpenLCBMessageHelper.Decompose(MessageStr: AnsiString);
var
  x, n, SemiColon, i: Integer;
  ByteStr: AnsiString;
begin
  MessageStr := UpperCase(MessageStr);

  x := Pos('X', MessageStr);         // Find were the "X" is in the string
  if x > 0 then
  begin
    n := PosEx('N', MessageStr, x);  // Find where the "N" is in the string
    if n > 0 then
    begin
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
        if Layer = ol_OpenLCB then
        begin
          if MTI and $07000000 > $01000000 then        // See if the destination Alias is in the MTI
          begin
            DestinationAliasID := (MTI and $00FFF000) shr 12;
            HasDestinationAddress := True;
          end else
          begin
            if MTI and MTI_ADDRESS_PRESENT = MTI_ADDRESS_PRESENT then
            begin
              DestinationAliasID := Word( (Data[0] shl 8)) or (Data[1]);
              HasDestinationAddress := True;
            end
          end
        end
      end
    end;
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
  Result := ':X' + IntToHex(FullMTI, 8) + 'N';
  for i := 0 to DataCount - 1 do
  begin
    if (i < 2) and (DestinationAliasID <> 0) then
    begin
      if i = 0 then
        Result := Result + IntToHex((DestinationAliasID shr 8) and $00FF, 2)
      else
        Result := Result + IntToHex(DestinationAliasID and $00FF, 2)
    end else
      Result := Result + IntToHex(Data[i], 2);
  end;
  Result := Result  + ';'
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

end.

