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
  opstackbuffers,
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
procedure OPStackCANStatemachineBuffers_AddOutgoingDatagramMessage(OPStackInProcessMessage: POPStackMessage);
function OPStackCANStatemachineBuffers_FindAnyDatagramOnOutgoingStack(NodeAlias: Word): POPStackMessage;
function OPStackCANStatemachineBuffers_FindMessageOnOutgoingDatagramStack(OPStackInProcessMessage: POPStackMessage): POPStackMessage;
function OPStackCANStatemachineBuffers_FirstMessageOnOutgoingDatagramStack(Dummy: Word): POPStackMessage;
procedure OPStackCANStatemachineBuffers_RemoveOutgoingDatagramMessage(OPStackInProcessMessage: POPStackMessage);
procedure OPStackCANStatemachineBuffers_SearchAndDestroyAbandonOutgoingDatagramMessages;

// Incoming Datagrams
procedure OPStackCANStatemachineBuffers_AddIncomingDatagramMessage(OPStackInProcessMessage: POPStackMessage);
function OPStackCANStatemachineBuffers_FindAnyDatagramOnIncomingStack(NodeAlias: Word): POPStackMessage;
function OPStackCANStatemachineBuffers_FindMessageOnIncomingDatagramStack(OPStackInProcessMessage: POPStackMessage): POPStackMessage;
function OPStackCANStatemachineBuffers_FirstMessageOnIncomingDatagramStack(Dummy: Word): POPStackMessage;
procedure OPStackCANStatemachineBuffers_RemoveIncomingDatagramMessage(OPStackInProcessMessage: POPStackMessage);
procedure OPStackCANStatemachineBuffers_SearchAndDestroyAbandonIncomingDatagramMessages;

// Outgoing MultiFrame Messages
procedure OPStackCANStateMachineBuffer_AddOutgoingMultiFrameMessage(OPStackInProcessMessage: POPStackMessage);
function OPStackCANStatemachineBuffers_FindAnyMultiFrameOnOutgoingStack(DestNodeAlias: Word): POPStackMessage;
function OPStackCANStatemachineBuffers_FindMessageOnOutgoingMultiFrameStack(OPStackInProcessMessage: POPStackMessage): POPStackMessage;
function OPStackCANStatemachineBuffers_FirstMessageOnOutgoingMultiFrameStack(Dummy: Word): POPStackMessage;
procedure OPStackCANStatemachineBuffers_RemoveMultiFrameMessage(OPStackInProcessMessage: POPStackMessage);
procedure OPStackCANStatemachineBuffers_SearchAndDestroyAbandonOutgoingMultiFrameMessages;

// Incoming MultiFrame Messages
procedure OPStackCANStatemachineBuffers_AddIncomingMultiFrameMessage(OPStackInProcessMessage: POPStackMessage);
function OPStackCANStatemachineBuffers_FindAnyMultiFrameOnIncomingStack(NodeAlias: Word): POPStackMessage;
function OPStackCANStatemachineBuffers_FindMessageOnIncomingMultiFrameStack(OPStackInProcessMessage: POPStackMessage): POPStackMessage;
function OPStackCANStatemachineBuffers_FirstMessageOnIncomingFrameMessageStack(Dummy: Word): POPStackMessage;
procedure OPStackCANStatemachineBuffers_RemoveIncomingMultiFrameMessage(OPStackInProcessMessage: POPStackMessage);
procedure OPStackCANStatemachineBuffers_SearchAndDestroyAbandonIncomingMultiFrameMessages;

// Outgoing SNIP/ACDI
procedure OPStackCANStatemachineBuffers_AddOutgoingMultiFrameStringMessage(OPStackInProcessMessage: POPStackMessage);
function OPStackCANStatemachineBuffers_FindAnyMultiFrameStringOnOutgoingStack(NodeAlias: Word): POPStackMessage;
function OPStackCANStatemachineBuffers_FindMessageOnOutgoingMultiFrameStringStack(OPStackInProcessMessage: POPStackMessage): POPStackMessage;
function OPStackCANStatemachineBuffers_FirstMessageOnOutgoingMultiFrameStringStack(Dummy: Word): POPStackMessage;
procedure OPStackCANStatemachineBuffers_RemoveMultiFrameStringMessage(OPStackInProcessMessage: POPStackMessage);
procedure OPStackCANStatemachineBuffers_SearchAndDestroyAbandonOutgoingMultiFrameStringMessages;

// Incoming SNIP/ACDI Messages
procedure OPStackCANStatemachineBuffers_AddIncomingMultiFrameStringMessage(OPStackInProcessMessage: POPStackMessage);
function OPStackCANStatemachineBuffers_FindAnyMultiFrameStringIncomingStack(NodeAlias: Word): POPStackMessage;
function OPStackCANStatemachineBuffers_FindMessageOnIncomingMultiFrameStringFrameStack(OPStackInProcessMessage: POPStackMessage): POPStackMessage;
function OPStackCANStatemachineBuffers_FirstMessageOnIncomingMultiFrameStringMessageStack(Dummy: Word): POPStackMessage;
procedure OPStackCANStatemachineBuffers_RemoveIncomingMultiFrameStringMessage(OPStackInProcessMessage: POPStackMessage);
procedure OPStackCANStatemachineBuffers_SearchAndDestroyAbandonIncomingMultiFrameStringMessages;

{$IFDEF SUPPORT_STREAMS}
// Outgoing Stream
procedure OPStackCANStatemachineBuffers_AddOutgoingStreamMessage(OPStackStreamMessage: POPStackMessage);
function OPStackCANStatemachineBuffers_FindAnyStreamOnOutgoingStack(DestNodeAlias: Word; SourceStreamID, DestStreamID: Integer): POPStackMessage;
function OPStackCANStatemachineBuffers_FindMessageOnOutgoingStreamStack(OPStackDatagramMessage: POPStackMessage): POPStackMessage;
function OPStackCANStatemachineBuffers_FirstMessageOnOutgoingStreamStack(Dummy: Word): POPStackMessage;
procedure OPStackCANStatemachineBuffers_RemoveStreamMessage(OPStackStreamMessage: POPStackMessage);
procedure OPStackCANStatemachineBuffers_SearchAndDestroyAbandonStreamMessages;
{$ENDIF}


implementation

var
  DatagramIncomingProcessStack: TProcessStack;
  DatagramOutgoingProcessStack: TProcessStack;
  MultiFrameStringOutgoingProcessStack: TProcessStack;
  MultiFrameStringIncomingProcessStack: TProcessStack;
  MultiFrameOutgoingProcessStack: TProcessStack;
  MultiFrameIncomingProcessStack: TProcessStack;
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
    DatagramIncomingProcessStack.Stack[i] := nil;
  DatagramIncomingProcessStack.Count := 0;
  for i := 0 to MAX_PROCESS_STACK_ARRAY - 1 do
    DatagramOutgoingProcessStack.Stack[i] := nil;
  DatagramOutgoingProcessStack.Count := 0;

  for i := 0 to MAX_PROCESS_STACK_ARRAY - 1 do
    MultiFrameStringOutgoingProcessStack.Stack[i] := nil;
  MultiFrameStringOutgoingProcessStack.Count := 0;
  for i := 0 to MAX_PROCESS_STACK_ARRAY - 1 do
    MultiFrameIncomingProcessStack.Stack[i] := nil;
  MultiFrameStringIncomingProcessStack.Count := 0;
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
    MultiFrameIncomingProcessStack.Stack[i] := nil;
  MultiFrameIncomingProcessStack.Count := 0;
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

procedure SearchAndRemoveAbandonMessage(MessageStackRoot: PProcessStack);
var
  i: Integer;
begin
  for i := 0 to MessageStackRoot^.Count - 1 do
  begin
    if MessageStackRoot^.Stack[i]^.WatchDog_1s > TIMEOUT_ABANDON_RESOURCE then
    begin
      OPStackBuffers_DeAllocateMessage(MessageStackRoot^.Stack[i]);
      Dec(MessageStackRoot^.Count);
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

procedure OPStackCANStatemachineBuffers_AddIncomingDatagramMessage(OPStackInProcessMessage: POPStackMessage);
begin
  AddInprocessMessage(OPStackInProcessMessage, @DatagramIncomingProcessStack);
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_FindAnyDatagramOnIncomingStack;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackCANStatemachineBuffers_FindAnyDatagramOnIncomingStack(NodeAlias: Word): POPStackMessage;
begin
  Result := FindAnyMessageByNodeID(NodeAlias, @DatagramIncomingProcessStack)
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_FindMessageOnIncomingDatagramStack;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackCANStatemachineBuffers_FindMessageOnIncomingDatagramStack(OPStackInProcessMessage: POPStackMessage): POPStackMessage;
begin
   Result := FindInprocessMessage(OPStackInProcessMessage, @DatagramIncomingProcessStack)
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_FirstMessageOnIncomingDatagramStack;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackCANStatemachineBuffers_FirstMessageOnIncomingDatagramStack(Dummy: Word): POPStackMessage;
begin
  Result := FirstInprocessMessage(@DatagramIncomingProcessStack);
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_RemoveIncomingDatagramMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCANStatemachineBuffers_RemoveIncomingDatagramMessage(OPStackInProcessMessage: POPStackMessage);
begin
  RemoveInprocessMessage(OPStackInProcessMessage, @DatagramIncomingProcessStack);
end;

procedure OPStackCANStatemachineBuffers_SearchAndDestroyAbandonIncomingDatagramMessages;
begin
  SearchAndRemoveAbandonMessage(@DatagramIncomingProcessStack);
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_AddOutgoingDatagramMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCANStatemachineBuffers_AddOutgoingDatagramMessage(OPStackInProcessMessage: POPStackMessage);
begin
  PDatagramBuffer( PByte(OPStackInProcessMessage^.Buffer))^.CurrentCount := 0;   // Make sure the counter is reset
  AddInprocessMessage(OPStackInProcessMessage, @DatagramOutgoingProcessStack);
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
function OPStackCANStatemachineBuffers_FindMessageOnOutgoingDatagramStack(OPStackInProcessMessage: POPStackMessage): POPStackMessage;
begin
   Result := FindInprocessMessage(OPStackInProcessMessage, @DatagramOutgoingProcessStack)
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
procedure OPStackCANStatemachineBuffers_RemoveOutgoingDatagramMessage(OPStackInProcessMessage: POPStackMessage);
begin
  RemoveInprocessMessage(OPStackInProcessMessage, @DatagramOutgoingProcessStack);
end;

procedure OPStackCANStatemachineBuffers_SearchAndDestroyAbandonOutgoingDatagramMessages;
begin
  SearchAndRemoveAbandonMessage(@DatagramOutgoingProcessStack);
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_AddOutgoingMultiFrameStringMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCANStatemachineBuffers_AddOutgoingMultiFrameStringMessage(OPStackInProcessMessage: POPStackMessage);
begin
  PMultiFrameStringBuffer( PByte(OPStackInProcessMessage^.Buffer))^.CurrentCount := 0;   // Make sure the counter is reset
  AddInprocessMessage(OPStackInProcessMessage, @MultiFrameStringOutgoingProcessStack);
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_FindAnyMultiFrameStringOnOutgoingStack;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackCANStatemachineBuffers_FindAnyMultiFrameStringOnOutgoingStack(NodeAlias: Word): POPStackMessage;
begin
  Result := FindAnyMessageByNodeID(NodeAlias, @MultiFrameStringOutgoingProcessStack);
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_FindMessageOnOutgoingMultiFrameStringStack;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackCANStatemachineBuffers_FindMessageOnOutgoingMultiFrameStringStack(OPStackInProcessMessage: POPStackMessage): POPStackMessage;
begin
  Result := FindInprocessMessage(OPStackInProcessMessage, @MultiFrameStringOutgoingProcessStack)
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_FirstMessageOnOutgoingMultiFrameStringStack;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackCANStatemachineBuffers_FirstMessageOnOutgoingMultiFrameStringStack(Dummy: Word): POPStackMessage;
begin
  Result := FirstInprocessMessage(@MultiFrameStringOutgoingProcessStack);
end;

// *****************************************************************************
//  procedure OPStackCANStatemachineBuffers_RemoveAcdiSnipDatagramMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCANStatemachineBuffers_RemoveMultiFrameStringMessage(OPStackInProcessMessage: POPStackMessage);
begin
  RemoveInprocessMessage(OPStackInProcessMessage, @MultiFrameStringOutgoingProcessStack);
end;

procedure OPStackCANStatemachineBuffers_SearchAndDestroyAbandonOutgoingMultiFrameStringMessages;
begin
  SearchAndRemoveAbandonMessage(@MultiFrameStringOutgoingProcessStack);
end;

procedure OPStackCANStatemachineBuffers_AddIncomingMultiFrameStringMessage(OPStackInProcessMessage: POPStackMessage);
begin
  AddInprocessMessage(OPStackInProcessMessage, @MultiFrameStringIncomingProcessStack);
end;

function OPStackCANStatemachineBuffers_FindAnyMultiFrameStringIncomingStack(NodeAlias: Word): POPStackMessage;
begin
  Result := FindAnyMessageByNodeID(NodeAlias, @MultiFrameStringIncomingProcessStack)
end;

function OPStackCANStatemachineBuffers_FindMessageOnIncomingMultiFrameStringFrameStack(OPStackInProcessMessage: POPStackMessage): POPStackMessage;
begin
  Result := FindInprocessMessage(OPStackInProcessMessage, @MultiFrameStringIncomingProcessStack)
end;

function OPStackCANStatemachineBuffers_FirstMessageOnIncomingMultiFrameStringMessageStack(Dummy: Word): POPStackMessage;
begin
  Result := FirstInprocessMessage(@MultiFrameStringIncomingProcessStack);
end;

procedure OPStackCANStatemachineBuffers_RemoveIncomingMultiFrameStringMessage(OPStackInProcessMessage: POPStackMessage);
begin
  RemoveInprocessMessage(OPStackInProcessMessage, @MultiFrameStringIncomingProcessStack);
end;

procedure OPStackCANStatemachineBuffers_SearchAndDestroyAbandonIncomingMultiFrameStringMessages;
begin
  SearchAndRemoveAbandonMessage(@MultiFrameStringIncomingProcessStack);
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
procedure OPStackCANStateMachineBuffer_AddOutgoingMultiFrameMessage(OPStackInProcessMessage: POPStackMessage);
begin
  PMultiFrameBuffer( PByte(OPStackInProcessMessage^.Buffer))^.CurrentCount := 0;   // Make sure the counter is reset
  AddInprocessMessage(OPStackInProcessMessage, @MultiFrameOutgoingProcessStack);
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
function OPStackCANStatemachineBuffers_FindMessageOnOutgoingMultiFrameStack(OPStackInProcessMessage: POPStackMessage): POPStackMessage;
begin
  Result := FindInprocessMessage(OPStackInProcessMessage, @MultiFrameOutgoingProcessStack)
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
procedure OPStackCANStatemachineBuffers_RemoveMultiFrameMessage( OPStackInProcessMessage: POPStackMessage);
begin
  RemoveInprocessMessage(OPStackInProcessMessage, @MultiFrameOutgoingProcessStack);
end;

procedure OPStackCANStatemachineBuffers_SearchAndDestroyAbandonOutgoingMultiFrameMessages;
begin
  SearchAndRemoveAbandonMessage(@MultiFrameOutgoingProcessStack);
end;

procedure OPStackCANStatemachineBuffers_AddIncomingMultiFrameMessage(OPStackInProcessMessage: POPStackMessage);
begin
  AddInprocessMessage(OPStackInProcessMessage, @MultiFrameIncomingProcessStack);
end;

function OPStackCANStatemachineBuffers_FindAnyMultiFrameOnIncomingStack(NodeAlias: Word): POPStackMessage;
begin
  Result := FindAnyMessageByNodeID(NodeAlias, @MultiFrameIncomingProcessStack)
end;

function OPStackCANStatemachineBuffers_FindMessageOnIncomingMultiFrameStack(OPStackInProcessMessage: POPStackMessage): POPStackMessage;
begin
  Result := FindInprocessMessage(OPStackInProcessMessage, @MultiFrameIncomingProcessStack)
end;

function OPStackCANStatemachineBuffers_FirstMessageOnIncomingFrameMessageStack(Dummy: Word): POPStackMessage;
begin
  Result := FirstInprocessMessage(@MultiFrameIncomingProcessStack);
end;

procedure OPStackCANStatemachineBuffers_RemoveIncomingMultiFrameMessage(OPStackInProcessMessage: POPStackMessage);
begin
  RemoveInprocessMessage(OPStackInProcessMessage, @MultiFrameIncomingProcessStack);
end;

procedure OPStackCANStatemachineBuffers_SearchAndDestroyAbandonIncomingMultiFrameMessages;
begin
  SearchAndRemoveAbandonMessage(@MultiFrameIncomingProcessStack);
end;

end.