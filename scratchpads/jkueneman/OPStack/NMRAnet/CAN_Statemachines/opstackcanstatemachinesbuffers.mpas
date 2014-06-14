unit opstackcanstatemachinesbuffers;

//TODO:  The FirstInProcess functions will always find the first one in the list regardless of the order it was put in.  Need to make this a indexed list

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  {$IFDEF FPC}
  Classes, SysUtils,
  {$ENDIF}
  nmranetutilities,
  template_buffers,
  opstackdefines,
  opstacktypes;

const
  MAX_PROCESS_STACK_ARRAY = 10;
type
  TProcessStackArray = array[0..MAX_PROCESS_STACK_ARRAY-1] of POPStackMessage;
  TProcessStack = record
    Stack: TProcessStackArray;
    Count: Word;
  end;
  PProcessStack = ^TProcessStack;


// Genearal
procedure OPStackCANStatemachineBuffers_Initialize;

// Outgoing Datagrams
procedure OPStackCANStatemachineBuffers_AddOutgoingDatagramMessage(OPStackDatagramMessage: POPStackMessage);
function OPStackCANStatemachineBuffers_FindAnyDatagramOnOutgoingStack(NodeAlias: Word): POPStackMessage;
function OPStackCANStatemachineBuffers_FindMessageOnOutgoingDatagramStack(OPStackDatagramMessage: POPStackMessage): POPStackMessage;
function OPStackCANStatemachineBuffers_FirstMessageOnOutgoingDatagramStack(Dummy: Word): POPStackMessage;
procedure OPStackCANStatemachineBuffers_RemoveOutgoingDatagramMessage(OPStackDatagramMessage: POPStackMessage);

// Incoming Datagrams
procedure OPStackCANStatemachineBuffers_AddIncomingDatagramMessage(OPStackDatagramMessage: POPStackMessage);
function OPStackCANStatemachineBuffers_FindAnyDatagramOnIncomingStack(NodeAlias: Word): POPStackMessage;
function OPStackCANStatemachineBuffers_FindMessageOnIncomingDatagramStack(OPStackDatagramMessage: POPStackMessage): POPStackMessage;
function OPStackCANStatemachineBuffers_FirstMessageOnIncomingDatagramStack(Dummy: Word): POPStackMessage;
procedure OPStackCANStatemachineBuffers_RemoveIncomingDatagramMessage(OPStackDatagramMessage: POPStackMessage);

// Outgoing MultiFrame Messages
procedure OPStackCANStateMachineBuffer_AddOutgoingMultiFrameMessage(OPStackMultiFrameMessage: POPStackMessage);
function OPStackCANStatemachineBuffers_FindAnyMultiFrameOnOutgoingStack(DestNodeAlias: Word): POPStackMessage;
function OPStackCANStatemachineBuffers_FindMessageOnOutgoingMultiFrameStack(OPStackMultiFrameMessage: POPStackMessage): POPStackMessage;
function OPStackCANStatemachineBuffers_FirstMessageOnOutgoingMultiFrameStack(Dummy: Word): POPStackMessage;
procedure OPStackCANStatemachineBuffers_RemoveMultiFrameMessage(OPStackMultiFrameMessage: POPStackMessage);

// Incoming MultiFrame Messages
procedure OPStackCANStatemachineBuffers_AddIncomingMultiFrameMessage(OPStackInProcessMessage: POPStackMessage);
function OPStackCANStatemachineBuffers_FindAnyMultiFrameOnIncomingStack(NodeAlias: Word): POPStackMessage;
function OPStackCANStatemachineBuffers_FindMessageOnIncomingMultiFrameStack(OPStackInProcessMessage: POPStackMessage): POPStackMessage;
function OPStackCANStatemachineBuffers_FirstMessageOnIncomingFrameMessageStack(Dummy: Word): POPStackMessage;
procedure OPStackCANStatemachineBuffers_RemoveIncomingMultiFrameMessage(OPStackInProcessMessage: POPStackMessage);

// Outgoing SNIP/ACDI
procedure OPStackCANStatemachineBuffers_AddOutgoingAcdiSnipMessage(OPStackAcdiSnipMessage: POPStackMessage);
function OPStackCANStatemachineBuffers_FindAnyAcdiSnipOnOutgoingStack(NodeAlias: Word): POPStackMessage;
function OPStackCANStatemachineBuffers_FindMessageOnOutgoingAcdiSnipStack(OPStackDatagramMessage: POPStackMessage): POPStackMessage;
function OPStackCANStatemachineBuffers_FirstMessageOnOutgoingAcdiSnipStack(Dummy: Word): POPStackMessage;
procedure OPStackCANStatemachineBuffers_RemoveAcdiSnipMessage(OPStackAcdiSnipMessage: POPStackMessage);

// Incoming SNIP/ACDI Messages
procedure OPStackCANStatemachineBuffers_AddIncomingAcdiSnipMessage(OPStackInProcessMessage: POPStackMessage);
function OPStackCANStatemachineBuffers_FindAnyAcdiSnipIncomingStack(NodeAlias: Word): POPStackMessage;
function OPStackCANStatemachineBuffers_FindMessageOnIncomingAcdiSnipFrameStack(OPStackInProcessMessage: POPStackMessage): POPStackMessage;
function OPStackCANStatemachineBuffers_FirstMessageOnIncomingAcdiSnipMessageStack(Dummy: Word): POPStackMessage;
procedure OPStackCANStatemachineBuffers_RemoveIncomingAcdiSnipMessage(OPStackInProcessMessage: POPStackMessage);

{$IFDEF SUPPORT_STREAMS}
// Outgoing Stream
procedure OPStackCANStatemachineBuffers_AddOutgoingStreamMessage(OPStackStreamMessage: POPStackMessage);
function OPStackCANStatemachineBuffers_FindAnyStreamOnOutgoingStack(DestNodeAlias: Word; SourceStreamID, DestStreamID: Integer): POPStackMessage;
function OPStackCANStatemachineBuffers_FindMessageOnOutgoingStreamStack(OPStackDatagramMessage: POPStackMessage): POPStackMessage;
function OPStackCANStatemachineBuffers_FirstMessageOnOutgoingStreamStack(Dummy: Word): POPStackMessage;
procedure OPStackCANStatemachineBuffers_RemoveStreamMessage(OPStackStreamMessage: POPStackMessage);
{$ENDIF}


implementation

var
  DatagramInProcessStack: TProcessStack;
  DatagramOutgoingProcessStack: TProcessStack;
  AcdiSnipOutgoingProcessStack: TProcessStack;
  AcdiSnipInProcessStack: TProcessStack;
  MultiFrameOutgoingProcessStack: TProcessStack;
  MultiFrameInProcessStack: TProcessStack;
  {$IFDEF SUPPORT_STREAMS}
  StreamInProcessStack: TProcessStack;
  StreamOutgoingProcessStack: TProcessStack;
  {$ENDIF}

// *****************************************************************************
//  procedure OPStackCANStatemachines_Initialize;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCANStatemachineBuffers_Initialize;
var
  i: Integer;
begin
  for i := 0 to MAX_PROCESS_STACK_ARRAY - 1 do
    DatagramInProcessStack.Stack[i] := nil;
  DatagramInProcessStack.Count := 0;
  for i := 0 to MAX_PROCESS_STACK_ARRAY - 1 do
    DatagramOutgoingProcessStack.Stack[i] := nil;
  DatagramOutgoingProcessStack.Count := 0;
  for i := 0 to MAX_PROCESS_STACK_ARRAY - 1 do
    AcdiSnipOutgoingProcessStack.Stack[i] := nil;
  AcdiSnipOutgoingProcessStack.Count := 0;
  {$IFDEF SUPPORT_STREAMS}
  for i := 0 to MAX_PROCESS_STACK_ARRAY - 1 do
    StreamInProcessStack.Stack[i] := nil;
  StreamInProcessStack.Count := 0;
  for i := 0 to MAX_PROCESS_STACK_ARRAY - 1 do
    StreamOutgoingProcessStack.Stack[i] := nil;
  StreamOutgoingProcessStack.Count := 0;
  {$ENDIF}
  for i := 0 to MAX_PROCESS_STACK_ARRAY - 1 do
    MultiFrameOutgoingProcessStack.Stack[i] := nil;
  MultiFrameOutgoingProcessStack.Count := 0;
  for i := 0 to MAX_PROCESS_STACK_ARRAY - 1 do
    MultiFrameInProcessStack.Stack[i] := nil;
  MultiFrameInProcessStack.Count := 0;
end;


// *****************************************************************************
//  procedure FirstInprocessMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function FirstInprocessMessage(MessageStackRoot: PProcessStack): POPStackMessage;
var
  i: Integer;
begin
  Result := nil;
  if MessageStackRoot^.Count > 0 then
  begin
    i := 0;
    while i < MAX_PROCESS_STACK_ARRAY do
    begin
      if MessageStackRoot^.Stack[i] <> nil then
      begin
        Result := MessageStackRoot^.Stack[i];
        Exit;
      end;
      Inc(i);
    end;
  end;
end;

// *****************************************************************************
//  procedure FindInprocessMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function FindInprocessMessage(OPStackMessage: POPStackMessage; MessageStackRoot: PProcessStack): POPStackMessage;
var
  i: Integer;
begin
  Result := nil;
  if MessageStackRoot^.Count > 0 then
  begin
    i := 0;
    while i < MAX_PROCESS_STACK_ARRAY do
    begin
      if MessageStackRoot^.Stack[i] <> nil then
      begin
        if NMRAnetUtilities_EqualNodeIDInfo(MessageStackRoot^.Stack[i]^.Dest, OPStackMessage^.Dest) then
          if NMRAnetUtilities_EqualNodeIDInfo(MessageStackRoot^.Stack[i]^.Source, OPStackMessage^.Source) then
          begin
            Result := MessageStackRoot^.Stack[i];
            Exit;
          end;
      end;
      Inc(i);
    end;
  end;
end;

// *****************************************************************************
//  procedure AddInprocessMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure AddInprocessMessage(OPStackMessage: POPStackMessage; MessageStackRoot: PProcessStack);
var
  i: Integer;
begin
  i := 0;
  while i < MAX_PROCESS_STACK_ARRAY do
  begin
    if MessageStackRoot^.Stack[i] = nil then
    begin
      Inc(MessageStackRoot^.Count);
      MessageStackRoot^.Stack[i] := OPStackMessage;
      Exit;
    end;
    Inc(i);
  end;
end;

// *****************************************************************************
//  procedure RemoveInprocessMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure RemoveInprocessMessage(OPStackMessage: POPStackMessage; MessageStackRoot: PProcessStack);
var
  i: Integer;
begin
  if MessageStackRoot^.Count > 0 then
  begin
    i := 0;
    while i < MAX_PROCESS_STACK_ARRAY do
    begin
      if MessageStackRoot^.Stack[i] <> nil then
      begin
        if NMRAnetUtilities_EqualNodeIDInfo(MessageStackRoot^.Stack[i]^.Dest, OPStackMessage^.Dest) then
         if NMRAnetUtilities_EqualNodeIDInfo(MessageStackRoot^.Stack[i]^.Source, OPStackMessage^.Source) then
          begin
            MessageStackRoot^.Stack[i] := nil;
            Dec(MessageStackRoot^.Count);
            Exit;
          end;
      end;
      Inc(i);
    end;
  end;
end;

// *****************************************************************************
//  procedure FindAnyMessageByNodeID;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function FindAnyMessageByNodeID(NodeAlias: Word; MessageStackRoot: PProcessStack): POPStackMessage;
var
  i: Integer;
begin
  Result := nil;
  if MessageStackRoot^.Count > 0 then
  begin
    i := 0;
    while i < MAX_PROCESS_STACK_ARRAY do
    begin
      if MessageStackRoot^.Stack[i] <> nil then
      begin
        if (NodeAlias = MessageStackRoot^.Stack[i]^.Dest.AliasID) or (NodeAlias = MessageStackRoot^.Stack[i]^.Source.AliasID) then
        begin
          Result := MessageStackRoot^.Stack[i];
          Exit;
        end;
      end;
      Inc(i);
    end;
  end;
end;

procedure OPStackCANStatemachineBuffers_AddIncomingDatagramMessage(OPStackDatagramMessage: POPStackMessage);
begin
  AddInprocessMessage(OPStackDatagramMessage, @DatagramInProcessStack);
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_FindAnyDatagramOnIncomingStack;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackCANStatemachineBuffers_FindAnyDatagramOnIncomingStack(NodeAlias: Word): POPStackMessage;
begin
  Result := FindAnyMessageByNodeID(NodeAlias, @DatagramInProcessStack)
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_FindMessageOnIncomingDatagramStack;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackCANStatemachineBuffers_FindMessageOnIncomingDatagramStack(OPStackDatagramMessage: POPStackMessage): POPStackMessage;
begin
   Result := FindInprocessMessage(OPStackDatagramMessage, @DatagramInProcessStack)
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_FirstMessageOnIncomingDatagramStack;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackCANStatemachineBuffers_FirstMessageOnIncomingDatagramStack(Dummy: Word): POPStackMessage;
begin
  Result := FirstInprocessMessage(@DatagramInProcessStack);
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_RemoveIncomingDatagramMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCANStatemachineBuffers_RemoveIncomingDatagramMessage(OPStackDatagramMessage: POPStackMessage);
begin
  RemoveInprocessMessage(OPStackDatagramMessage, @DatagramInProcessStack);
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_AddOutgoingDatagramMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCANStatemachineBuffers_AddOutgoingDatagramMessage(OPStackDatagramMessage: POPStackMessage);
begin
  PDatagramBuffer( PByte(OPStackDatagramMessage^.Buffer))^.CurrentCount := 0;   // Make sure the counter is reset
  AddInprocessMessage(OPStackDatagramMessage, @DatagramOutgoingProcessStack);
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_FindAnyDatagramOnOutgoingStack;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackCANStatemachineBuffers_FindAnyDatagramOnOutgoingStack(NodeAlias: Word): POPStackMessage;
begin
   Result := FindAnyMessageByNodeID(NodeAlias, @DatagramOutgoingProcessStack);
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_FindMessageOnOutgoingDatagramStack;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackCANStatemachineBuffers_FindMessageOnOutgoingDatagramStack(OPStackDatagramMessage: POPStackMessage): POPStackMessage;
begin
   Result := FindInprocessMessage(OPStackDatagramMessage, @DatagramOutgoingProcessStack)
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_FirstMessageOnOutgoingDatagramStack;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackCANStatemachineBuffers_FirstMessageOnOutgoingDatagramStack(Dummy: Word): POPStackMessage;
begin
  Result := FirstInprocessMessage(@DatagramOutgoingProcessStack);
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_RemoveOutgoingDatagramMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCANStatemachineBuffers_RemoveOutgoingDatagramMessage(OPStackDatagramMessage: POPStackMessage);
begin
  RemoveInprocessMessage(OPStackDatagramMessage, @DatagramOutgoingProcessStack);
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_AddOutgoingAcdiSnipMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCANStatemachineBuffers_AddOutgoingAcdiSnipMessage(OPStackAcdiSnipMessage: POPStackMessage);
begin
  PAcdiSnipBuffer( PByte(OPStackAcdiSnipMessage^.Buffer))^.CurrentCount := 0;   // Make sure the counter is reset
  AddInprocessMessage(OPStackAcdiSnipMessage, @AcdiSnipOutgoingProcessStack);
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_FindAnyAcdiSnipOnOutgoingStack;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackCANStatemachineBuffers_FindAnyAcdiSnipOnOutgoingStack(NodeAlias: Word): POPStackMessage;
begin
  Result := FindAnyMessageByNodeID(NodeAlias, @AcdiSnipOutgoingProcessStack);
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_FindMessageOnOutgoingAcdiSnipStack;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackCANStatemachineBuffers_FindMessageOnOutgoingAcdiSnipStack(OPStackDatagramMessage: POPStackMessage): POPStackMessage;
begin
  Result := FindInprocessMessage(OPStackDatagramMessage, @AcdiSnipOutgoingProcessStack)
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_FirstMessageOnOutgoingAcdiSnipStack;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackCANStatemachineBuffers_FirstMessageOnOutgoingAcdiSnipStack(Dummy: Word): POPStackMessage;
begin
  Result := FirstInprocessMessage(@AcdiSnipOutgoingProcessStack);
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_RemoveAcdiSnipDatagramMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCANStatemachineBuffers_RemoveAcdiSnipMessage(OPStackAcdiSnipMessage: POPStackMessage);
begin
  RemoveInprocessMessage(OPStackAcdiSnipMessage, @AcdiSnipOutgoingProcessStack);
end;

procedure OPStackCANStatemachineBuffers_AddIncomingAcdiSnipMessage(OPStackInProcessMessage: POPStackMessage);
begin
  AddInprocessMessage(OPStackInProcessMessage, @AcdiSnipInProcessStack);
end;

function OPStackCANStatemachineBuffers_FindAnyAcdiSnipIncomingStack(NodeAlias: Word): POPStackMessage;
begin
  Result := FindAnyMessageByNodeID(NodeAlias, @AcdiSnipInProcessStack)
end;

function OPStackCANStatemachineBuffers_FindMessageOnIncomingAcdiSnipFrameStack(OPStackInProcessMessage: POPStackMessage): POPStackMessage;
begin
  Result := FindInprocessMessage(OPStackInProcessMessage, @AcdiSnipInProcessStack)
end;

function OPStackCANStatemachineBuffers_FirstMessageOnIncomingAcdiSnipMessageStack(Dummy: Word): POPStackMessage;
begin
  Result := FirstInprocessMessage(@AcdiSnipInProcessStack);
end;

procedure OPStackCANStatemachineBuffers_RemoveIncomingAcdiSnipMessage(OPStackInProcessMessage: POPStackMessage);
begin
  RemoveInprocessMessage(OPStackInProcessMessage, @AcdiSnipInProcessStack);
end;

{$IFDEF SUPPORT_STREAMS}
// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_AddOutgoingStreamMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCANStatemachineBuffers_AddOutgoingStreamMessage(OPStackStreamMessage: POPStackMessage);
begin
  PStreamBuffer( PByte(OPStackStreamMessage^.Buffer))^.CurrentCount := 0;   // Make sure the counter is reset
  AddInprocessMessage(OPStackStreamMessage, @StreamOutgoingProcessStack);
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_FindAnyStreamOnOutgoingStack;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackCANStatemachineBuffers_FindAnyStreamOnOutgoingStack(DestNodeAlias: Word; SourceStreamID, DestStreamID: Integer): POPStackMessage;
var
  LocalStack: POPStackMessage;
begin
  Result := nil;
  LocalStack := FirstInprocessMessage(@StreamOutgoingProcessStack);
  while LocalStack <> nil do
  begin
    if (LocalStack^.Dest.AliasID = DestNodeAlias) then
      if (DestStreamID < 0) or (PStreamBuffer( PByte( LocalStack^.Buffer))^.DestStreamID = DestStreamID) then
        if (SourceStreamID < 0) or (PStreamBuffer( PByte( LocalStack^.Buffer))^.SourceStreamID = SourceStreamID) then
        begin
          Result := LocalStack;
          Exit;
        end;
    LocalStack := LocalStack^.NextIncoming;
  end;
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_FindMessageOnOutgoingStreamStack;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackCANStatemachineBuffers_FindMessageOnOutgoingStreamStack(OPStackDatagramMessage: POPStackMessage): POPStackMessage;
begin
  Result := FindInprocessMessage(OPStackDatagramMessage, @StreamOutgoingProcessStack)
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_FirstMessageOnOutgoingStreamStack;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackCANStatemachineBuffers_FirstMessageOnOutgoingStreamStack(Dummy: Word): POPStackMessage;
begin
  Result := FirstInprocessMessage(@StreamOutgoingProcessStack);
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_RemoveStreamDatagramMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCANStatemachineBuffers_RemoveStreamMessage(OPStackStreamMessage: POPStackMessage);
begin
  RemoveInprocessMessage(OPStackStreamMessage, @StreamOutgoingProcessStack);
end;

{$ENDIF}

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_RemoveStreamDatagramMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCANStateMachineBuffer_AddOutgoingMultiFrameMessage(OPStackMultiFrameMessage: POPStackMessage);
begin
  PMultiFrameBuffer( PByte(OPStackMultiFrameMessage^.Buffer))^.CurrentCount := 0;   // Make sure the counter is reset
  AddInprocessMessage(OPStackMultiFrameMessage, @MultiFrameOutgoingProcessStack);
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_FindAnyMultiFrameOnOutgoingStack;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackCANStatemachineBuffers_FindAnyMultiFrameOnOutgoingStack(DestNodeAlias: Word): POPStackMessage;
begin
  Result := FindAnyMessageByNodeID(DestNodeAlias, @MultiFrameOutgoingProcessStack);
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_FindMessageOnOutgoingMultiFrameStack;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackCANStatemachineBuffers_FindMessageOnOutgoingMultiFrameStack(OPStackMultiFrameMessage: POPStackMessage): POPStackMessage;
begin
  Result := FindInprocessMessage(OPStackMultiFrameMessage, @MultiFrameOutgoingProcessStack)
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_FirstMessageOnOutgoingMultiFrameStack;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackCANStatemachineBuffers_FirstMessageOnOutgoingMultiFrameStack(Dummy: Word): POPStackMessage;
begin
  Result := FirstInprocessMessage(@MultiFrameOutgoingProcessStack);
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_RemoveMultiFrameMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCANStatemachineBuffers_RemoveMultiFrameMessage(OPStackMultiFrameMessage: POPStackMessage);
begin
  RemoveInprocessMessage(OPStackMultiFrameMessage, @MultiFrameOutgoingProcessStack);
end;

procedure OPStackCANStatemachineBuffers_AddIncomingMultiFrameMessage(OPStackInProcessMessage: POPStackMessage);
begin
  AddInprocessMessage(OPStackInProcessMessage, @MultiFrameInProcessStack);
end;

function OPStackCANStatemachineBuffers_FindAnyMultiFrameOnIncomingStack(NodeAlias: Word): POPStackMessage;
begin
  Result := FindAnyMessageByNodeID(NodeAlias, @MultiFrameInProcessStack)
end;

function OPStackCANStatemachineBuffers_FindMessageOnIncomingMultiFrameStack(OPStackInProcessMessage: POPStackMessage): POPStackMessage;
begin
  Result := FindInprocessMessage(OPStackInProcessMessage, @MultiFrameInProcessStack)
end;

function OPStackCANStatemachineBuffers_FirstMessageOnIncomingFrameMessageStack(Dummy: Word): POPStackMessage;
begin
  Result := FirstInprocessMessage(@MultiFrameInProcessStack);
end;

procedure OPStackCANStatemachineBuffers_RemoveIncomingMultiFrameMessage(OPStackInProcessMessage: POPStackMessage);
begin
  RemoveInprocessMessage(OPStackInProcessMessage, @MultiFrameInProcessStack);
end;

end.