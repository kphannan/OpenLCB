unit opstackcanstatemachines;

//TODO:  The FirstInProcess functions will always find the first one in the list regardless of the order it was put in.  Need to make this a indexed list


{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  {$IFDEF FPC}
  Classes, SysUtils,
  {$ENDIF}
  hardware_template,
  template_buffers,
  nmranetutilities,
  nmranetdefines,
  opstackdefines,
  opstackbuffers,
  opstacknode,
  opstacktypes;

procedure OPStackCANStatemachines_Initialize;

function OPStackCANStatemachine_ProcessIncomingDatagramMessage(OPStackMessage: POPStackMessage; var DatagramMessage: POPStackMessage): Byte;
function OPStackCANStatemachine_FindAnyDatagramOnIncomingStack(NodeAlias: Word): POPStackMessage;
procedure OPStackCANStatemachine_RemoveIncomingDatagramMessage(OPStackDatagramMessage: POPStackMessage);

procedure OPStackCANStatemachine_ProcessOutgoingDatagramMessage;
procedure OPStackCANStatemachine_AddOutgoingDatagramMessage(OPStackDatagramMessage: POPStackMessage);
function OPStackCANStatemachine_FindAnyDatagramOnOutgoingStack(NodeAlias: Word): POPStackMessage;
procedure OPStackCANStatemachine_RemoveOutgoingDatagramMessage(OPStackDatagramMessage: POPStackMessage);

{$IFDEF SUPPORT_STREAMS}
procedure OPStackCANStatemachine_ProcessOutgoingStreamMessage;
procedure OPStackCANStatemachine_AddOutgoingStreamMessage(OPStackStreamMessage: POPStackMessage);
function OPStackCANStatemachine_FindAnyStreamOnOutgoingStack(DestNodeAlias: Word; SourceStreamID, DestStreamID: Byte; iStateMachine: Byte): POPStackMessage;
procedure OPStackCANStatemachine_RemoveStreamDatagramMessage(OPStackStreamMessage: POPStackMessage);
{$ENDIF}

procedure OPStackCANStatemachine_ProcessOutgoingAcdiSnipMessage;
procedure OPStackCANStatemachine_AddOutgoingAcdiSnipMessage(OPStackAcdiSnipMessage: POPStackMessage);
function OPStackCANStatemachine_FindAnyAcdiSnipOnOutgoingStack(NodeAlias: Word): POPStackMessage;
procedure OPStackCANStatemachine_RemoveAcdiSnipMessage(OPStackAcdiSnipMessage: POPStackMessage);

implementation

type
  TProcessStackArray = array[0..USER_MAX_DATAGRAM_ARRAY_BUFFERS-1] of POPStackMessage;
  TProcessStack = record
    Stack: TProcessStackArray;
    Count: Word;
  end;
  PProcessStack = ^TProcessStack;

var
  DatagramInProcessStack: TProcessStack;
  DatagramOutgoingProcessStack: TProcessStack;
  AcdiSnipOutgoingProcessStack: TProcessStack;
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
procedure OPStackCANStatemachines_Initialize;
var
  i: Integer;
begin
  for i := 0 to USER_MAX_DATAGRAM_ARRAY_BUFFERS - 1 do
    DatagramInProcessStack.Stack[i] := nil;
  DatagramInProcessStack.Count := 0;
  for i := 0 to USER_MAX_DATAGRAM_ARRAY_BUFFERS - 1 do
    DatagramOutgoingProcessStack.Stack[i] := nil;
  DatagramOutgoingProcessStack.Count := 0;
  for i := 0 to USER_MAX_DATAGRAM_ARRAY_BUFFERS - 1 do
    AcdiSnipOutgoingProcessStack.Stack[i] := nil;
  AcdiSnipOutgoingProcessStack.Count := 0;
  {$IFDEF SUPPORT_STREAMS}
  for i := 0 to USER_MAX_DATAGRAM_ARRAY_BUFFERS - 1 do
    StreamInProcessStack.Stack[i] := nil;
  StreamInProcessStack.Count := 0;
  for i := 0 to USER_MAX_DATAGRAM_ARRAY_BUFFERS - 1 do
    StreamOutgoingProcessStack.Stack[i] := nil;
  StreamOutgoingProcessStack.Count := 0;
  {$ENDIF}
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
    while i < USER_MAX_DATAGRAM_ARRAY_BUFFERS do
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
    while i < USER_MAX_DATAGRAM_ARRAY_BUFFERS do
    begin
      if OPStackNode_Equal(MessageStackRoot^.Stack[i], OPStackMessage) then
      begin
        Result := MessageStackRoot^.Stack[i];
        Exit;
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
//  if MessageStackRoot.Count < USER_MAX_DATAGRAM_ARRAY_BUFFERS then
  begin
    i := 0;
    while i < USER_MAX_DATAGRAM_ARRAY_BUFFERS do
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
    while i < USER_MAX_DATAGRAM_ARRAY_BUFFERS do
    begin
      if OPStackNode_Equal(MessageStackRoot^.Stack[i], OPStackMessage) then
      begin
        MessageStackRoot^.Stack[i] := nil;
        Dec(MessageStackRoot^.Count);
        Exit;
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
    while i < USER_MAX_DATAGRAM_ARRAY_BUFFERS do
    begin
      if (NodeAlias = MessageStackRoot^.Stack[i]^.Dest.AliasID) or (NodeAlias = MessageStackRoot^.Stack[i]^.Source.AliasID) then
      begin
        Result := MessageStackRoot^.Stack[i];
        Exit;
      end;
      Inc(i);
    end;
  end;
end;

// *****************************************************************************
//  procedure OPStackCANStatemachine_ProcessIncomingDatagramMessage;
//    Parameters: OPStackMessage - Message incoming
//    Result:     ErrorCode -
//                Result - DATAGRAM_PROCESS_ERROR_xxxx constant
//    Description:
// *****************************************************************************
function OPStackCANStatemachine_ProcessIncomingDatagramMessage(OPStackMessage: POPStackMessage; var DatagramMessage: POPStackMessage): Byte;
var
  InProcessMessage: POPStackmessage;
begin
  Result := DATAGRAM_PROCESS_ERROR_OK;
  DatagramMessage := nil;
  InProcessMessage := FindInprocessMessage(OPStackMessage, @DatagramInProcessStack);
  case OPStackMessage^.MTI of
    MTI_FRAME_TYPE_CAN_DATAGRAM_ONLY_FRAME :
        begin
          if InProcessMessage = nil then
          begin
            case OPStackMessage^.Buffer^.DataArray[0] of
      //        DATAGRAM_TYPE_BOOTLOADER,
              DATAGRAM_TYPE_MEMORY_CONFIGURATION,
              DATAGRAM_TYPE_TRAIN_CONTROL :
                  begin                                                         // Allocate a message for a full MTI_DATRGRAM and return the pointer to the message
                    if OPStackBuffers_AllocateDatagramMessage(InProcessMessage, OPStackMessage^.Source.AliasID, OPStackMessage^.Source.ID, OPStackMessage^.Dest.AliasID, OPStackMessage^.Dest.ID, 0) then
                    begin
                      InProcessMessage^.Buffer^.DataBufferSize := OPStackMessage^.Buffer^.DataBufferSize;
                      OPStackBuffers_CopyData(InProcessMessage^.Buffer, OPStackMessage^.Buffer);
                      PDatagramBuffer( PByte( InProcessMessage^.Buffer))^.CurrentCount := 0;
                      DatagramMessage := InProcessMessage;
                      Exit;
                    end  else                                                    // No Buffer available, try again
                      Result := DATAGRAM_PROCESS_ERROR_BUFFER_FULL;
                  end
              else
                Result := DATAGRAM_PROCESS_ERROR_SOURCE_NOT_ACCEPTED;           // Unknowns Datagram type
            end;
          end else
          begin
            // The node has a DG connection already, can't have two just drop it
          end;
        end;
    MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME_START :
        begin
          if InProcessMessage = nil then
          begin
            if OPStackBuffers_AllocateDatagramMessage(InProcessMessage, OPStackMessage^.Source.AliasID, OPStackMessage^.Source.ID, OPStackMessage^.Dest.AliasID, OPStackMessage^.Dest.ID, 0) then
            begin
              InProcessMessage^.Buffer^.DataBufferSize := OPStackMessage^.Buffer^.DataBufferSize;
              OPStackBuffers_CopyData(InProcessMessage^.Buffer, OPStackMessage^.Buffer);
              PDatagramBuffer( PByte( InProcessMessage^.Buffer))^.CurrentCount := OPStackMessage^.Buffer^.DataBufferSize;
              AddInprocessMessage(InProcessMessage, @DatagramInProcessStack);
              Exit
            end //else                                                            // No Buffer available, try again
           //   Result := Result;
           //   Result := DATAGRAM_PROCESS_ERROR_BUFFER_FULL;                     // Don't agree with this but Python test fails for overlapped datagram if I return this
          end else
          begin
            // The node has a DG connection already, can't have two just drop it
          end
        end;
    MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME       :
        begin
          if InProcessMessage <> nil then
          begin
            OPStackBuffers_CopyDataArrayWithDestOffset(InProcessMessage^.Buffer, @OPStackMessage^.Buffer^.DataArray, OPStackMessage^.Buffer^.DataBufferSize, False);
            PDatagramBuffer( PByte( InProcessMessage^.Buffer))^.CurrentCount := PDatagramBuffer( PByte( InProcessMessage^.Buffer))^.CurrentCount + OPStackMessage^.Buffer^.DataBufferSize;
            Exit;
          end //else
          //  Result := DATAGRAM_PROCESS_ERROR_OUT_OF_ORDER;
        end;
    MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME_END   :
        begin
          if InProcessMessage <> nil then
          begin
            OPStackBuffers_CopyDataArrayWithDestOffset(InProcessMessage^.Buffer, @OPStackMessage^.Buffer^.DataArray, OPStackMessage^.Buffer^.DataBufferSize, False);
            PDatagramBuffer( PByte( InProcessMessage^.Buffer))^.CurrentCount := 0;      // Wooh Hoo, we are done
            RemoveInprocessMessage(InProcessMessage, @DatagramInProcessStack);                 // Pull it out of the working stack
            DatagramMessage := InProcessMessage;
            case InProcessMessage^.Buffer^.DataArray[0] of
     //         DATAGRAM_TYPE_BOOTLOADER,
              DATAGRAM_TYPE_MEMORY_CONFIGURATION,
              DATAGRAM_TYPE_TRAIN_CONTROL :  Result := DATAGRAM_PROCESS_ERROR_OK // Send it back to be dispatched
            else
              Result := DATAGRAM_PROCESS_ERROR_SOURCE_NOT_ACCEPTED;              // Unknown Datagram type
            end
          end else
            Result := DATAGRAM_PROCESS_ERROR_OUT_OF_ORDER;
        end;
    end; {case}
end;

// *****************************************************************************
//  procedure OPStackCANStatemachine_FindAnyDatagramOnIncomingStack;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackCANStatemachine_FindAnyDatagramOnIncomingStack(NodeAlias: Word): POPStackMessage;
begin
  Result := FindAnyMessageByNodeID(NodeAlias, @DatagramInProcessStack)
end;

// *****************************************************************************
//  procedure OPStackCANStatemachine_RemoveIncomingDatagramMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCANStatemachine_RemoveIncomingDatagramMessage(OPStackDatagramMessage: POPStackMessage);
begin
  RemoveInprocessMessage(OPStackDatagramMessage, @DatagramInProcessStack);
end;

// *****************************************************************************
//  procedure OPStackCANStatemachine_AddOutgoingDatagramMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCANStatemachine_AddOutgoingDatagramMessage(OPStackDatagramMessage: POPStackMessage);
begin
  PDatagramBuffer( PByte(OPStackDatagramMessage^.Buffer))^.CurrentCount := 0;   // Make sure the counter is reset
  AddInprocessMessage(OPStackDatagramMessage, @DatagramOutgoingProcessStack);
end;

// *****************************************************************************
//  procedure OPStackCANStatemachine_FindAnyDatagramOnOutgoingStack;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackCANStatemachine_FindAnyDatagramOnOutgoingStack(NodeAlias: Word): POPStackMessage;
begin
   Result := FindAnyMessageByNodeID(NodeAlias, @DatagramOutgoingProcessStack);
end;

// *****************************************************************************
//  procedure OPStackCANStatemachine_RemoveOutgoingDatagramMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCANStatemachine_RemoveOutgoingDatagramMessage(OPStackDatagramMessage: POPStackMessage);
begin
  RemoveInprocessMessage(OPStackDatagramMessage, @DatagramOutgoingProcessStack);
end;

// *****************************************************************************
//  procedure OPStackCANStatemachine_ProcessOutgoingDatagramMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCANStatemachine_ProcessOutgoingDatagramMessage;
var
  NewMessage: TOPStackMessage;
  NewBuffer: TSimpleBuffer;
  LocalOutgoingMessage: POPStackMessage;
  DatagramBuffer: PDatagramBuffer;
  MTI: Word;
begin
  LocalOutgoingMessage := FirstInprocessMessage(@DatagramOutgoingProcessStack);
  if LocalOutgoingMessage <> nil then                                 // We just work this stack from the top down, for now
    if IsOutgoingBufferAvailable then
    begin
      DatagramBuffer := PDatagramBuffer( PByte( LocalOutgoingMessage^.Buffer));
      OPStackBuffers_ZeroMessage(@NewMessage);
      OPStackBuffers_ZeroSimpleBuffer(@NewBuffer, False);
      if LocalOutgoingMessage^.Buffer^.DataBufferSize <= 8 then
      begin
        OPStackBuffers_LoadMessage(@NewMessage, MTI_FRAME_TYPE_CAN_DATAGRAM_ONLY_FRAME, LocalOutgoingMessage^.Source.AliasID, LocalOutgoingMessage^.Source.ID, LocalOutgoingMessage^.Dest.AliasID, LocalOutgoingMessage^.Dest.ID, 0);
        NewMessage.MessageType := MT_SIMPLE;
        NewMessage.Buffer := @NewBuffer;
        OPStackBuffers_CopyDataArray(@NewBuffer, @DatagramBuffer^.DataArray, LocalOutgoingMessage^.Buffer^.DataBufferSize, True);
        NewMessage.Buffer^.DataBufferSize := LocalOutgoingMessage^.Buffer^.DataBufferSize;
        RemoveInprocessMessage(LocalOutgoingMessage, @DatagramOutgoingProcessStack);
        OutgoingMessage(@NewMessage);
        Exit;
      end else
      if PDatagramBuffer( PByte( LocalOutgoingMessage^.Buffer))^.CurrentCount = 0 then
        MTI := MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME_START
      else
      if LocalOutgoingMessage^.Buffer^.DataBufferSize - PDatagramBuffer( PByte( LocalOutgoingMessage^.Buffer))^.CurrentCount > 8 then
        MTI := MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME
      else begin
        MTI := MTI_FRAME_TYPE_CAN_DATAGRAM_FRAME_END;
        RemoveInprocessMessage(LocalOutgoingMessage, @DatagramOutgoingProcessStack);
      end;

      OPStackBuffers_LoadMessage(@NewMessage, MTI, LocalOutgoingMessage^.Source.AliasID, LocalOutgoingMessage^.Source.ID, LocalOutgoingMessage^.Dest.AliasID, LocalOutgoingMessage^.Dest.ID, 0);
      OPStackBuffers_ZeroSimpleBuffer(@NewBuffer, False);
      NewMessage.MessageType := MT_SIMPLE;
      NewMessage.Buffer := @NewBuffer;
      while DatagramBuffer^.CurrentCount < DatagramBuffer^.DataBufferSize do
      begin
        NewMessage.Buffer^.DataArray[NewBuffer.DataBufferSize] := DatagramBuffer^.DataArray[DatagramBuffer^.CurrentCount];
        Inc(DatagramBuffer^.CurrentCount);
        Inc(NewBuffer.DataBufferSize);
        if NewBuffer.DataBufferSize = 8 then
          Break;
      end;
      OutgoingMessage(@NewMessage);
  end;
end;


// *****************************************************************************
//  procedure OPStackCANStatemachine_ProcessOutgoingAcdiSnipMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCANStatemachine_ProcessOutgoingAcdiSnipMessage;
var
  LocalMessage: TOPStackMessage;
  LocalOutgoingMessagePtr: POPStackMessage;
  LocalBuffer: TSimpleBuffer;
  AcdiSnipBufferPtr: PAcdiSnipBuffer;
begin
  LocalOutgoingMessagePtr := FirstInprocessMessage(@AcdiSnipOutgoingProcessStack);
  if IsOutgoingBufferAvailable then
    if LocalOutgoingMessagePtr <> nil then
    begin
      AcdiSnipBufferPtr := PAcdiSnipBuffer( PByte( LocalOutgoingMessagePtr^.Buffer));
      OPStackBuffers_LoadMessage(@LocalMessage, MTI_SIMPLE_NODE_INFO_REPLY, LocalOutgoingMessagePtr^.Source.AliasID, LocalOutgoingMessagePtr^.Source.ID, LocalOutgoingMessagePtr^.Dest.AliasID, LocalOutgoingMessagePtr^.Dest.ID, 0);
      OPStackBuffers_ZeroSimpleBuffer(@LocalBuffer, False);
      LocalMessage.MessageType := MT_SIMPLE;
      LocalMessage.Buffer := @LocalBuffer;
      LocalBuffer.DataBufferSize := 0;
      while AcdiSnipBufferPtr^.CurrentCount < AcdiSnipBufferPtr^.DataBufferSize do
      begin
        LocalBuffer.DataArray[LocalBuffer.DataBufferSize] := AcdiSnipBufferPtr^.DataArray[AcdiSnipBufferPtr^.CurrentCount];
        Inc(LocalBuffer.DataBufferSize );
        Inc(AcdiSnipBufferPtr^.CurrentCount);
        if LocalBuffer.DataBufferSize = 6 then
          Break;
      end;
      OutgoingMessage(@LocalMessage);

      if AcdiSnipBufferPtr^.CurrentCount >= AcdiSnipBufferPtr^.DataBufferSize then
      begin
        OPStackCANStatemachine_RemoveAcdiSnipMessage(LocalOutgoingMessagePtr);
        OPStackBuffers_DeAllocateMessage(LocalOutgoingMessagePtr);
      end;
    end;
end;

// *****************************************************************************
//  procedure OPStackCANStatemachine_AddOutgoingAcdiSnipMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCANStatemachine_AddOutgoingAcdiSnipMessage(OPStackAcdiSnipMessage: POPStackMessage);
begin
  PAcdiSnipBuffer( PByte(OPStackAcdiSnipMessage^.Buffer))^.CurrentCount := 0;   // Make sure the counter is reset
  AddInprocessMessage(OPStackAcdiSnipMessage, @AcdiSnipOutgoingProcessStack);
end;

// *****************************************************************************
//  procedure OPStackCANStatemachine_FindAnyAcdiSnipOnOutgoingStack;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackCANStatemachine_FindAnyAcdiSnipOnOutgoingStack(NodeAlias: Word): POPStackMessage;
begin
  Result := FindAnyMessageByNodeID(NodeAlias, @AcdiSnipOutgoingProcessStack);
end;

// *****************************************************************************
//  procedure OPStackCANStatemachine_RemoveAcdiSnipDatagramMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCANStatemachine_RemoveAcdiSnipMessage(OPStackAcdiSnipMessage: POPStackMessage);
begin
  RemoveInprocessMessage(OPStackAcdiSnipMessage, @AcdiSnipOutgoingProcessStack);
end;

{$IFDEF SUPPORT_STREAMS}
// *****************************************************************************
//  procedure OPStackCANStatemachine_ProcessOutgoingStreamMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCANStatemachine_ProcessOutgoingStreamMessage;
var
  NewMessage: TOPStackMessage;
  NewBuffer: TSimpleBuffer;
  LocalOutgoingMessage: POPStackMessage;
  DatagramBuffer: PDatagramBuffer;
  MTI: Word;
begin
  LocalOutgoingMessage := FirstInprocessMessage(@DatagramOutgoingProcessStack);
  if LocalOutgoingMessage <> nil then                                           // We just work this stack from the top down, for now
    if IsOutgoingBufferAvailable then
    begin

    end;
end;

// *****************************************************************************
//  procedure OPStackCANStatemachine_AddOutgoingStreamMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCANStatemachine_AddOutgoingStreamMessage(OPStackStreamMessage: POPStackMessage);
begin
  PStreamBuffer( PByte(OPStackStreamMessage^.Buffer))^.CurrentCount := 0;   // Make sure the counter is reset
  AddInprocessMessage(OPStackStreamMessage, @StreamOutgoingProcessStack);
end;

// *****************************************************************************
//  procedure OPStackCANStatemachine_FindAnyStreamOnOutgoingStack;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackCANStatemachine_FindAnyStreamOnOutgoingStack(DestNodeAlias: Word; SourceStreamID, DestStreamID: Byte; iStateMachine: Byte): POPStackMessage;
var
  LocalStack: POPStackMessage;
begin
  Result := nil;
  LocalStack := FirstInprocessMessage(@StreamOutgoingProcessStack);
  while LocalStack <> nil do
  begin
    if (LocalStack^.Dest.AliasID = DestNodeAlias) then
      if (PStreamBuffer( PByte( LocalStack^.Buffer))^.DestStreamID = DestStreamID) then
        if (PStreamBuffer( PByte( LocalStack^.Buffer))^.SourceStreamID = SourceStreamID) then
        if (PStreamBuffer( PByte( LocalStack^.Buffer))^.iStateMachine = iStateMachine) then
        begin
          Result := LocalStack;
          Exit;
        end;
    LocalStack := LocalStack^.Next;
  end;
end;

// *****************************************************************************
//  procedure OPStackCANStatemachine_RemoveStreamDatagramMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackCANStatemachine_RemoveStreamDatagramMessage(OPStackStreamMessage: POPStackMessage);
begin
  RemoveInprocessMessage(OPStackStreamMessage, @StreamOutgoingProcessStack);
end;

{$ENDIF}


{   SNIP STATEMACHINE  }


 { DATAGRAM }

{


}

(*
   ConfigMemBuffer := nil;
  if NMRABusTxBufferAvailable then
      if NMRAnetBufferPools_AllocateConfigMemBuffer(ConfigMemBuffer) then
      begin
        i := 0;
        while (BaseBuffer^.StateMachine <> STATE_ACDI_DONE) and (i < 6) do   // All messages have the Destination Alias as the first 2 bytes so only 6 left to use
        begin
          case BaseBuffer^.StateMachine of
            STATE_ACDI_MFG_VERSION  :
                begin      {$IFDEF TRACE_SNIP}UART1_Write_Text('STATE_ACDI_MFG_VERSION'+LF); {$ENDIF}
                  DataBytes[i] := ACDI_MFG_VERSION;
                  Inc(i);
                  BaseBuffer^.Tag := 0;
                  BaseBuffer^.StateMachine := STATE_ACDI_MFG_INFO;
                end;
            STATE_ACDI_MFG_INFO :
                begin  {$IFDEF TRACE_SNIP}UART1_Write_Text('STATE_ACDI_MFG_INFO'+LF); {$ENDIF}
                  {$IFDEF SUPPORT_VIRTUAL_NODES}
                  if Node^.State and NS_VIRTUAL <> 0 then
                  begin
                    if BaseBuffer^.Tag < MAX_ACDI_MFG_ARRAY_VNODE then
                    begin
                      DataBytes[i] := ACDI_MFG_STRINGS_VNODE[BaseBuffer^.Tag];
                      Inc(BaseBuffer^.Tag);
                      Inc(i);
                    end else
                      BaseBuffer^.StateMachine := STATE_ACDI_USER_VERSION;
                  end else {$ENDIF}
                  begin
                    if BaseBuffer^.Tag < MAX_ACDI_MFG_ARRAY then
                    begin
                      DataBytes[i] := ACDI_MFG_STRINGS[BaseBuffer^.Tag];
                      Inc(BaseBuffer^.Tag);
                      Inc(i);
                    end else
                      BaseBuffer^.StateMachine := STATE_ACDI_USER_VERSION;
                  end;
                end;
            STATE_ACDI_USER_VERSION :
                begin    {$IFDEF TRACE_SNIP}UART1_Write_Text('STATE_ACDI_USER_VERSION'+LF); {$ENDIF}
                  DataBytes[i] := ACDI_USER_VERSION;
                  Inc(i);
                  BaseBuffer^.StateMachine := STATE_ACDI_USER_NAME;
                  BaseBuffer^.Tag := 1;  // EEPROM layout start at offset 1
                end;
            STATE_ACDI_USER_NAME :
                begin      {$IFDEF TRACE_SNIP}UART1_Write_Text('STATE_ACDI_USER_NAME'+LF); {$ENDIF}
                  if BaseBuffer^.Tag < MAX_USER_NAME then
                  begin
                    // Very wasteful and slow 1 at a time but it is easy
                    ConfigMemBuffer^.Address := BaseBuffer^.Tag;
                    ConfigMemBuffer^.DataCount := 1;
                    ConfigMemBuffer^.AddressSpace := MSI_CONFIG;
                    AppCallback_Configuration_Read(Node, ConfigMemBuffer);
                    DataBytes[i] := ConfigMemBuffer^.DataBytes[0];

                    if DataBytes[i] = #0 then
                      BaseBuffer^.StateMachine := STATE_ACDI_START_DESC
                    else
                    if BaseBuffer^.Tag = MAX_USER_NAME - 1 then
                      DataBytes[i] := #0;
                    Inc(i);
                    Inc(BaseBuffer^.Tag);
                  end else
                     BaseBuffer^.StateMachine := STATE_ACDI_START_DESC;
                end;
            STATE_ACDI_START_DESC :
                begin      {$IFDEF TRACE_SNIP}UART1_Write_Text('STATE_ACDI_START_DESC'+LF); {$ENDIF}
                  BaseBuffer^.Tag := MAX_USER_NAME + 1;  // EEPROM layout start at offset 1
                  BaseBuffer^.StateMachine := STATE_ACDI_USER_DESC;
                end;
            STATE_ACDI_USER_DESC :
                begin     {$IFDEF TRACE_SNIP}UART1_Write_Text('STATE_ACDI_USER_DESC'+LF); {$ENDIF}
                  if BaseBuffer^.Tag < MAX_USER_CONFIG_DATA then
                  begin
                    // Very wasteful and slow 1 at a time but it is easy
                    ConfigMemBuffer^.Address := BaseBuffer^.Tag;
                    ConfigMemBuffer^.DataCount := 1;
                    AppCallback_Configuration_Read(Node, ConfigMemBuffer);
                    DataBytes[i] := ConfigMemBuffer^.DataBytes[0];

                    if DataBytes[i] = #0 then
                      BaseBuffer^.StateMachine := STATE_ACDI_DONE
                    else
                    if BaseBuffer^.Tag = MAX_USER_CONFIG_DATA - 1 then
                      DataBytes[i] := #0;
                    Inc(i);
                    Inc(BaseBuffer^.Tag);
                  end else
                     BaseBuffer^.StateMachine := STATE_ACDI_DONE;
                end;
            STATE_ACDI_DONE :
                begin      {$IFDEF TRACE_SNIP}UART1_Write_Text('STATE_ACDI_DONE'+LF); {$ENDIF}
                  {$IFDEF ETHERNET_BUS}
                  CAN_Engine.TransmitImmediately := True;
                  {$ENDIF}
                end;
          end;
        end;

      if i > 0 then
        TransmitNMRABusLayerMsg(Node, CANBuffer, MTI_SIMPLE_NODE_INFO_REPLY, BaseBuffer^.Alias, i, @DataBytes, False, $00);

      if BaseBuffer^.StateMachine >= STATE_ACDI_DONE then
      begin
        NMRAnetUtilities_BaseBufferUnLink(Node, BaseBuffer);
        NMRAnetBufferPools_ReleaseBaseBuffer(BaseBuffer);
      end;

      NMRAnetBufferPools_ReleaseConfigMemBuffer(ConfigMemBuffer);
    end   *)

{ STREAM }
{
          MTI_FRAME_TYPE_STREAM_INIT_REQUEST  : begin  // Remote is asking to send data to us (we are the receiver)
          Node := NMRAnetNode_FindByAlias(NMRAnetUtilities_ExtractDestinationAlias(CANBuffer));
          if Node <> nil then
          begin
            if NMRAnetBufferPools_AllocateStreamBuffer(StreamBuffer, False) then
            begin
              StreamBuffer^.Alias := ((CANBuffer^.DataBytes[0] shl 8) or CANBuffer^.DataBytes[1]) and $0FFF;
              StreamBuffer^.NegotiatedTransferSize := (CANBuffer^.DataBytes[2] shl 8) or CANBuffer^.DataBytes[3];
              StreamBuffer^.Content.TypeIncluded := (CANBuffer^.DataBytes[4] and $01) <> 0;
              StreamBuffer^.RemoteStreamID := CANBuffer^.DataBytes[6];
              StreamBuffer^.State := StreamBuffer^.State or CBS_PROCESSING or CBS_OUTGOING; // Need to get the buffer into a Transmit mode
              StreamBuffer^.StateMachine := STATE_STREAM_INITIATE_REQEUST_WAIT_FOR_REPLY;  // Now let the ProcessMessages decide how to reply to the other node
              NMRAnetUtilities_StreamBufferLink(Node, StreamBuffer);
            end else
            begin
              // High priority fail reply
            end;
          end;
          end;
          MTI_FRAME_TYPE_STREAM_INIT_REPLY    : begin  // We asked to send data to the Remote and has replied
          Node := NMRAnetNode_FindByAlias(NMRAnetUtilities_ExtractDestinationAlias(CANBuffer));
          if Node <> nil then
          begin
             // Find and inprocess Stream that matches the SID that we sent first, we don't know what the remote SID is yet
             if NMRAnetUtilities_FindInProcessStreamInNode(Node, StreamBuffer, CANBuffer^.DataBytes[6], -1) then
             begin
               StreamBuffer^.iWatchdog := 0;
               StreamBuffer^.NegotiatedTransferSize := (CANBuffer^.DataBytes[2] shl 8) or CANBuffer^.DataBytes[3];
               StreamBuffer^.RemoteStreamID := CANBuffer^.DataBytes[7];
               StreamBuffer^.StateMachine := STATE_STREAM_SEND;  // Start Sending to the Remote node
               StreamBuffer^.State := StreamBuffer^.State or CBS_OUTGOING;
               // NEED TO FIND THE "ACCEPT" BIT SOMEWHERE
               if StreamBuffer^.NegotiatedTransferSize > 0 then
               begin
               end else
               begin
                 // The remote node did not want to play with us
                 NMRAnetUtilities_StreamBufferUnLink(Node, StreamBuffer);
                 NMRAnetBufferPools_ReleaseStreamBuffer(StreamBuffer);
               end
             end
          end;
          end;
          MTI_FRAME_TYPE_STREAM_SEND          : begin  // Remote is asked (and is) to send data to us (we are the receiver)
          Node := NMRAnetNode_FindByAlias( NMRAnetUtilities_ExtractDestinationCodedInMTIAlias(CANBuffer));
          if Node <> nil then
          begin
            // HOW DO WE KNOW WHAT STREAM IS WHAT HERE?????
            if NMRAnetUtilities_FindInProcessStreamInNode(Node, StreamBuffer, -1, -1) then
            begin
              StreamBuffer^.iWatchdog := 0;
              for i := 0 to CANBuffer^.DataCount - 1 do
              begin
                StreamBuffer^.DataBytes[StreamBuffer^.iByteCount] := CANBuffer^.DataBytes[i];
                Inc(StreamBuffer^.iByteCount)
              end;
              if StreamBuffer^.iByteCount >= StreamBuffer^.NegotiatedTransferSize then
              begin
          //      StreamBuffer^.StateMachine :=
              end
            end
          end
          end;
          MTI_FRAME_TYPE_STREAM_PROCEED       : begin
          Node := NMRAnetNode_FindByAlias(NMRAnetUtilities_ExtractDestinationAlias(CANBuffer));
          if Node <> nil then
          begin
          end;
          end;
          MTI_FRAME_TYPE_STREAM_COMPLETE      : begin
          Node := NMRAnetNode_FindByAlias(NMRAnetUtilities_ExtractDestinationAlias(CANBuffer));
          if Node <> nil then
          begin
          end;
          end;
      }
end.
