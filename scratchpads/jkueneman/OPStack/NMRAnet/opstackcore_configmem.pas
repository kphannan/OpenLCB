unit opstackcore_configmem;

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  template_node,
  template_vnode,
  template_configuration,
  template_configmem,
  nmranetdefines,
  opstackdefines,
  opstackbuffers,
  opstacktypes;

function CommandReadReply(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
function CommandReadStreamReply(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
function CommandWriteReply(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
function CommandWriteStreamReply(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;

function OperationGetConfigurationReply(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
function OperationGetSpaceInfoReply(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
function OperationLockReply(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
function OperationGetUniqueIDReply(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
function OperationFreezeReply(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
function OperationIndicateReply(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
function OperationUpdateCompleteReply(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
function OperationResetReply(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;


implementation

// *****************************************************************************
//  procedure MaxAddressByAddressSpace
//     Parameters:
//     Returns:
//
//     Description:
//
// *****************************************************************************
function MaxAddressByAddressSpace(Node: PNMRAnetNode; AddressSpace: Byte): DWord;
begin
  case AddressSpace of
    MSI_CDI       : begin
                      {$IFDEF SUPPORT_VIRTUAL_NODES}
                      if Node^.State and NS_VIRTUAL <> 0 then
                        Result := USER_MAX_VNODE_CDI_ARRAY
                      else {$ENDIF}
                        Result := USER_MAX_CDI_ARRAY;
                    end;
    MSI_ALL       : Result := $FFFFFFFF;
    MSI_ACDI_MFG  : begin
                      {$IFDEF SUPPORT_VIRTUAL_NODES}
                      if Node^.State and NS_VIRTUAL <> 0 then
                        Result := USER_VNODE_MAX_ACDI_MFG_ARRAY
                      else {$ENDIF}
                        Result := USER_MAX_ACDI_MFG_ARRAY
                    end;
    MSI_ACDI_USER : begin
                      {$IFDEF SUPPORT_VIRTUAL_NODES}
                      if Node^.State and NS_VIRTUAL <> 0 then
                        Result := USER_MAX_VNODE_ACDI_USER_NAME_CONFIG_DATA + USER_MAX_VNODE_ACDI_USER_DESC_CONFIG_DATA + 1 // for the Version ID Byte
                      else {$ENDIF}
                        Result := USER_MAX_ACDI_USER_NAME_CONFIG_DATA + USER_MAX_ACDI_USER_DESC_CONFIG_DATA + 1 // for the Version ID Byte
                    end;
    MSI_CONFIG,
    MSI_FDI       : begin
                      Result := AppCallback_AddressSpaceSize(Node, AddressSpace);
                    end
  else
    Result := 0;
  end;
end;

// *****************************************************************************
//  procedure DecodeConfigMemReadWriteHeader
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure DecodeConfigMemReadWriteHeader(Node: PNMRAnetNode; Buffer: PDatagramDataArray; var AddressSpace: Byte; var ConfigAddress: DWord; var ReadCount: DWord; var DataOffset: Byte);
var
  MaxSpaceSize: DWord;
begin
  // Decode the Memory Space and where the Data starts
  DataOffset := 6;
  case Buffer^[1] and $03 of      // Strip off bottom two bits
    MCP_CDI            : AddressSpace := MSI_CDI;
    MCP_ALL            : AddressSpace := MSI_ALL;
    MCP_CONFIGURATION  : AddressSpace := MSI_CONFIG;
    MCP_NONE           :
      begin
        Inc(DataOffset);
        AddressSpace := Buffer^[6]
       end;
  end;
  ConfigAddress := DWord( Buffer^[2] shl 24) or DWord( Buffer^[3] shl 16) or DWord( Buffer^[4] shl 8) or DWord( Buffer^[5]);

  case Buffer^[1] and $F0 of
    MCP_COMMAND_READ_STREAM  : ReadCount := DWord( Buffer^[DataOffset] shl 24) or DWord( Buffer^[DataOffset+1] shl 16) or DWord( Buffer^[DataOffset+2] shl 8) or DWord( Buffer^[DataOffset+3]);
    MCP_COMMAND_READ         : ReadCount := Buffer^[DataOffset] and $7F         // Ignore the upper bit per the spec
  else
     ReadCount := 0;
  end;

     // Test the size against the size of the Address Space and adjust to the Max size if necessary
   MaxSpaceSize := MaxAddressByAddressSpace(Node, AddressSpace);
   if ConfigAddress >= MaxSpaceSize then                               // If the caller overruns the address we are done
     ReadCount := 0
   else begin
     if ConfigAddress + ReadCount > MaxSpaceSize then
       ReadCount := MaxSpaceSize - ConfigAddress;
   end
end;

// *****************************************************************************
//  procedure EncodeConfigMemReadWriteHeader
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure EncodeConfigMemReadWriteHeader(Buffer: PDatagramDataArray; IsRead, IsStream, IsForReply, IsReplyOK: Boolean; AddressSpace: Byte; ConfigAddress: DWord; ReadCount: DWord; UseAddressSpaceByte: Boolean; var DataOffset: Byte);
begin
  Buffer^[0] := DATAGRAM_TYPE_MEMORY_CONFIGURATION;

  // Setup the Command
  if IsRead then
  begin
    if IsStream then
      Buffer^[1] := MCP_COMMAND_READ_STREAM
    else
      Buffer^[1] := MCP_COMMAND_READ;
  end else
  begin
    if IsStream then
      Buffer^[1] := MCP_COMMAND_WRITE_STREAM
    else
      Buffer^[1] := MCP_COMMAND_WRITE;
  end;

  DataOffset := 6;
  if UseAddressSpaceByte or (AddressSpace < MSI_CONFIG) then
  begin
    Inc(DataOffset);
    Buffer^[6] := AddressSpace
  end else
  begin
    case AddressSpace of
      MSI_CDI            : Buffer^[1] := Buffer^[1] or MCP_CDI;
      MSI_ALL            : Buffer^[1] := Buffer^[1] or MCP_ALL;
      MSI_CONFIG         : Buffer^[1] := Buffer^[1] or MCP_CONFIGURATION;
    end
  end;

  Buffer^[2] := ConfigAddress shr 24;
  Buffer^[3] := ConfigAddress shr 16;
  Buffer^[4] := ConfigAddress shr 8;
  Buffer^[5] := ConfigAddress;

  if IsForReply then
  begin
    if IsRead then
    begin
      if IsReplyOK then
        Buffer^[1] := Buffer^[1] or MCP_COMMAND_READ_REPLY_OK
      else
        Buffer^[1] := Buffer^[1] or MCP_COMMAND_READ_REPLY_FAIL;
    end else
    begin
      if IsReplyOK then
        Buffer^[1] := Buffer^[1] or MCP_COMMAND_WRITE_REPLY_OK
      else
        Buffer^[1] := Buffer^[1] or MCP_COMMAND_WRITE_REPLY_FAIL;
    end;
  end else
  begin
    if IsRead then
    begin
      if IsStream then
      begin
        Buffer^[DataOffset] := ReadCount shr 24;
        Inc(DataOffset);
        Buffer^[DataOffset] := ReadCount shr 16;
        Inc(DataOffset);
        Buffer^[DataOffset] := ReadCount shr 8;
        Inc(DataOffset);
        Buffer^[DataOffset] := ReadCount;
        Inc(DataOffset);
      end else
      begin
        Buffer^[DataOffset] := ReadCount;
        Inc(DataOffset);
      end;
    end;
  end;
end;

// *****************************************************************************
//  procedure CommandReadReply
//     Parameters: Node: Node that message was sent to
//                 OPStackMessage: Message to fill in to be sent, if no message reply
//                                 is required the message should be DeAllocated with
//                                 OPStackBuffers_DeAllocateMessage
//     Returns:     True  - if OPStackMessage is filled and needs to be sent
//                  False - if there is no message to send and the function has DeAllocated
//                        message
//     Description: The Message is recycled from the incoming datagram so it is
//                  allocated from the message pool
// *****************************************************************************
function CommandReadReply(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
var
  DatagramBufferPtr: PDatagramBuffer;
  AddressSpace, DataOffset: Byte;
  ConfigAddress, ReadCount: DWord;
  i: Integer;
begin
  DatagramBufferPtr := PDatagramBuffer( PByte( OPStackMessage^.Buffer));
  DecodeConfigMemReadWriteHeader(Node, @DatagramBufferPtr^.DataArray, AddressSpace, ConfigAddress, ReadCount, DataOffset);
  for i := 0 to ReadCount - 1 do
    DatagramBufferPtr^.DataArray[i] := 0;
  EncodeConfigMemReadWriteHeader(@DatagramBufferPtr^.DataArray, True, False, True, True, AddressSpace, ConfigAddress, ReadCount, AddressSpace < MSI_CONFIG, DataOffset);
  DatagramBufferPtr^.DataBufferSize := ReadCount+DataOffset;
  DatagramBufferPtr^.CurrentCount := 0;

  if Node^.iIndex > 0 then
    ConfigAddress := USER_CONFIGURATION_MEMORY_SIZE + (Node^.iIndex - 1)*USER_VNODE_CONFIGURATION_MEMORY_SIZE;

  case AddressSpace of
      MSI_CDI :
          begin
            {$IFDEF SUPPORT_VIRTUAL_NODES}
            if Node^.State and NS_VIRTUAL <> 0 then
            begin
               for i := 0 to ReadCount - 1 do
                DatagramBufferPtr^.DataArray[DataOffset+i] := USER_CDI_VNODE_ARRAY[i+ConfigAddress]
            end else {$ENDIF}
            begin
              for i := 0 to ReadCount - 1 do
                 DatagramBufferPtr^.DataArray[DataOffset+i] := USER_CDI_ARRAY[i+ConfigAddress];
            end;
          end;
       MSI_ALL :
         begin
            for i := 0 to ReadCount - 1 do
              DatagramBufferPtr^.DataArray[DataOffset+i] := PByte(i)^
          end;
      MSI_CONFIG :
          begin
            AppCallback_ReadConfiguration(ConfigAddress, ReadCount, PByte( @DatagramBufferPtr^.DataArray[DataOffset]));
          end;
      MSI_ACDI_MFG :
          begin
            {$IFDEF SUPPORT_VIRTUAL_NODES}
            if Node^.State and NS_VIRTUAL <> 0 then
            begin
              for i := 0 to ReadCount - 1 do
                DatagramBufferPtr^.DataArray[DataOffset+i] := USER_VNODE_ACDI_MFG_STRINGS[ConfigAddress + i];
            end else {$ENDIF}
            begin
              for i := 0 to ReadCount - 1 do
                DatagramBufferPtr^.DataArray[DataOffset+i] := USER_ACDI_MFG_STRINGS[ConfigAddress + i];
            end;
          end;
      MSI_ACDI_USER :
          begin
            AppCallback_ReadAcdiUser(ConfigAddress, ReadCount, PByte( @DatagramBufferPtr^.DataArray[DataOffset]));
          end;
      MSI_FDI :
          begin
      //      AppCallback_ReadFDI(ConfigAddress, ReadCount, PByte( @DatagramBufferPtr^.DataArray[DataOffset]));
            for i := 0 to ReadCount - 1 do
              DatagramBufferPtr^.DataArray[DataOffset+i] := $AA;      // TEMPORARY
          end;
  end;
  Result := True;
end;

// *****************************************************************************
//  procedure CommandReadStreamReply
//     Parameters: Node: Node that message was sent to
//                 OPStackMessage: Message to fill in to be sent, if no message reply
//                                 is required the message should be DeAllocated with
//                                 OPStackBuffers_DeAllocateMessage
//     Returns:     True  - if OPStackMessage is filled and needs to be sent
//                  False - if there is no message to send and the function has DeAllocated
//                        message
//     Description: The Message is recycled from the incoming datagram so it is
//                  allocated from the message pool
// *****************************************************************************
function CommandReadStreamReply(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
var
  DatagramBufferPtr: PDatagramBuffer;
  AddressSpace, DataOffset: Byte;
  ConfigAddress, ReadCount: DWord;
begin
  DatagramBufferPtr := PDatagramBuffer( PByte( OPStackMessage^.Buffer));
  DecodeConfigMemReadWriteHeader(Node, @DatagramBufferPtr^.DataArray, AddressSpace, ConfigAddress, ReadCount, DataOffset);
   // Option here, I could have failed the datagram ACK with "try again later" which is currently defined with error codes.
   // This method is acceptable but error codes are not defined yet.
   // Also do I do this on the Receive Side?  Is there any reason to let this propogate to here?  All this does is try to
   // allocate a stream buffer then wait for the caller to reply before setting up the stream.  This could be all done when
   // the message is first received on the other end....  It is constistent if done here with the datagram read/writes....

  // Zero the array to avoid confusion on debugging

(*
   if AddressSpace < MSI_CONFIG then
     EncodeConfigMemReadWriteHeader(@DatagramBufferPtr^.DataArray, True, True, AddressSpace, ConfigAddress, ReadCount, True, DataOffset)
   else
     EncodeConfigMemReadWriteHeader(@DatagramBufferPtr^.DataArray, True, True, AddressSpace, ConfigAddress, ReadCount, False, DataOffset);

   DatagramBufferPtr^.DataBufferSize := DataOffset;    // Just the header, no data that comes in the stream
   DatagramBufferPtr^.CurrentCount := 0;
   DatagramBufferPtr^.iStateMachine := 0;

   if OPStackBuffers_AllcoateStreamMessage(NewMessage, MTI_FRAME_TYPE_CAN_STREAM_SEND, DatagramMessage^.Source.AliasID, DatagramMessage^.Source.ID, DatagramMessage^.Dest.AliasID, DatagramMessage^.Dest.ID, True) then
   begin
     // Streams are handled by their StateMacheines (NodeRunOutgoingStreamStateMachine).  We link to the node and allow the state
     // machine to move through the build up/sending/teardown of the stream.  The datagram ACK from the Datagram Read Reply will move this statemachine
     // into its first active state.
     EncodeConfigMemReadWriteHeaderReply(@DatagramBufferPtr^.DataArray, True, True) ;
     OPStackNode_StateMachineMessageLink(Node, NewMessage);      // No Stream ID's yet just waiting for the Stream link to be created
   end else
     EncodeConfigMemReadWriteHeaderReply(@DatagramBufferPtr^.DataArray, False, True);
 *)
 Result := True;
end;

// *****************************************************************************
//  procedure CommandWriteReply
//     Parameters: Node: Node that message was sent to
//                 OPStackMessage: Message to fill in to be sent, if no message reply
//                                 is required the message should be DeAllocated with
//                                 OPStackBuffers_DeAllocateMessage
//     Returns:     True  - if OPStackMessage is filled and needs to be sent
//                  False - if there is no message to send and the function has DeAllocated
//                        message
//     Description: The Message is recycled from the incoming datagram so it is
//                  allocated from the message pool
// *****************************************************************************
function CommandWriteReply(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
var
  DatagramBufferPtr: PDatagramBuffer;
  AddressSpace, DataOffset: Byte;
  ConfigAddress, ReadCount: DWord;
  i: Integer;
  WriteCount: Byte;
begin
  DatagramBufferPtr := PDatagramBuffer( PByte( OPStackMessage^.Buffer));
  DecodeConfigMemReadWriteHeader(Node, @DatagramBufferPtr^.DataArray, AddressSpace, ConfigAddress, ReadCount, DataOffset);
  WriteCount := DatagramBufferPtr^.DataBufferSize-DataOffset;

  if Node^.iIndex > 0 then
    ConfigAddress := USER_CONFIGURATION_MEMORY_SIZE + (Node^.iIndex - 1)*USER_VNODE_CONFIGURATION_MEMORY_SIZE;

  case AddressSpace of
    MSI_CDI,
    MSI_ALL,
    MSI_ACDI_MFG,
    MSI_FDI :
       begin
         OPStackBuffers_DeAllocateMessage(OPStackMessage);    // These are Read Only
         Exit;
       end;
    MSI_CONFIG :
       begin
         AppCallback_WriteConfiguration(ConfigAddress, WriteCount, PByte( @DatagramBufferPtr^.DataArray[DataOffset]));
       end;
    MSI_ACDI_USER :
       begin
      // How do I know were to write this in the Configuration Memory?
      //   AppCallback_WriteConfiguration(ConfigAddress, DatagramBufferPtr^.DataBufferSize-DataOffset, PByte( @DatagramBufferPtr^.DataArray[DataOffset]));
      //   for i := 0 to WriteCount - 1 do
      //     DatagramBufferPtr^.DataArray[DataOffset+i] := $AA;      // TEMPORARY
       end;
    end;

    for i := 0 to (WriteCount+DataOffset) - 1 do
      DatagramBufferPtr^.DataArray[i] := 0;

    EncodeConfigMemReadWriteHeader(@DatagramBufferPtr^.DataArray, False, False, True, True, AddressSpace, ConfigAddress, WriteCount, AddressSpace < MSI_CONFIG, DataOffset);
    DatagramBufferPtr^.DataBufferSize := DataOffset;
    DatagramBufferPtr^.CurrentCount := 0;
    Result := True;
end;

// *****************************************************************************
//  procedure CommandWriteStreamReply
//     Parameters: Node: Node that message was sent to
//                 OPStackMessage: Message to fill in to be sent, if no message reply
//                                 is required the message should be DeAllocated with
//                                 OPStackBuffers_DeAllocateMessage
//     Returns:     True  - if OPStackMessage is filled and needs to be sent
//                  False - if there is no message to send and the function has DeAllocated
//                        message
//     Description: The Message is recycled from the incoming datagram so it is
//                  allocated from the message pool
// *****************************************************************************
function CommandWriteStreamReply(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
begin
  OPStackBuffers_DeAllocateMessage(OPStackMessage);
  Result := False;
end;

// *****************************************************************************
//  procedure OperationGetConfigurationReply
//     Parameters: Node: Node that message was sent to
//                 OPStackMessage: Message to fill in to be sent, if no message reply
//                                 is required the message should be DeAllocated with
//                                 OPStackBuffers_DeAllocateMessage
//     Returns:     True  - if OPStackMessage is filled and needs to be sent
//                  False - if there is no message to send and the function has DeAllocated
//                        message
//     Description: The Message is recycled from the incoming datagram so it is
//                  allocated from the message pool
// *****************************************************************************
function OperationGetConfigurationReply(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
var
  DatagramBufferPtr: PDatagramBuffer;
begin
  DatagramBufferPtr := PDatagramBuffer( PByte( OPStackMessage^.Buffer));
  DatagramBufferPtr^.DataArray[0] := DATAGRAM_TYPE_MEMORY_CONFIGURATION;
  DatagramBufferPtr^.DataArray[1] := MCP_OP_GET_CONFIG_REPLY;
  {$IFDEF SUPPORT_VIRTUAL_NODES}
  if Node^.State and NS_VIRTUAL <> 0 then
  begin
   DatagramBufferPtr^.DataArray[2] := Hi( USER_VNODE_CONFIGMEM_OPTIONS);
   DatagramBufferPtr^.DataArray[3] := Lo( USER_VNODE_CONFIGMEM_OPTIONS);
   DatagramBufferPtr^.DataArray[4] := USER_VNODE_CONFIGMEM_WRITE_LENGTH;
   DatagramBufferPtr^.DataArray[5] := USER_VNODE_CONFIGMEM_HIGHEST_SPACE;
   DatagramBufferPtr^.DataArray[6] := USER_VNODE_CONFIGMEM_LOWEST_SPACE;
  end else
  {$ENDIF}
  begin
   DatagramBufferPtr^.DataArray[2] := Hi( USER_CONFIGMEM_OPTIONS);
   DatagramBufferPtr^.DataArray[3] := Lo( USER_CONFIGMEM_OPTIONS);
   DatagramBufferPtr^.DataArray[4] := USER_CONFIGMEM_WRITE_LENGTH;
   DatagramBufferPtr^.DataArray[5] := USER_CONFIGMEM_HIGHEST_SPACE;
   DatagramBufferPtr^.DataArray[6] := USER_CONFIGMEM_LOWEST_SPACE;
  end;
  DatagramBufferPtr^.DataBufferSize := 7;
  DatagramBufferPtr^.CurrentCount := 0;
  Result := True;
end;

// *****************************************************************************
//  procedure OperationGetSpaceInfoReply
//     Parameters: Node: Node that message was sent to
//                 OPStackMessage: Message to fill in to be sent, if no message reply
//                                 is required the message should be DeAllocated with
//                                 OPStackBuffers_DeAllocateMessage
//     Returns:     True  - if OPStackMessage is filled and needs to be sent
//                  False - if there is no message to send and the function has DeAllocated
//                        message
//     Description: The Message is recycled from the incoming datagram so it is
//                  allocated from the message pool
// *****************************************************************************
function OperationGetSpaceInfoReply(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
var
  DatagramBufferPtr: PDatagramBuffer;
  MemorySpaceMaxAddress: DWord;
begin
  DatagramBufferPtr := PDatagramBuffer( PByte( OPStackMessage^.Buffer));
  DatagramBufferPtr^.DataArray[0] := DATAGRAM_TYPE_MEMORY_CONFIGURATION;
  DatagramBufferPtr^.DataArray[1] := MCP_OP_GET_ADD_SPACE_INFO_REPLY;
  if AppCallback_AddressSpacePresent(Node, DatagramBufferPtr^.DataArray[2]) then
    DatagramBufferPtr^.DataArray[1] := DatagramBufferPtr^.DataArray[1] or MCP_OP_GET_ADD_SPACE_INFO_REPLY_PRESENT;
  DatagramBufferPtr^.DataArray[2] := DatagramBufferPtr^.DataArray[2];
    // I am not supporting the ability to return anything but a $0 for the lower address so we only deal with offsets from zero in these calls
  MemorySpaceMaxAddress := MaxAddressByAddressSpace(Node, DatagramBufferPtr^.DataArray[2]);
  DatagramBufferPtr^.DataArray[3] := (DWord(MemorySpaceMaxAddress) shr 24) and $000000FF;
  DatagramBufferPtr^.DataArray[4] := (DWord(MemorySpaceMaxAddress) shr 16) and $000000FF;
  DatagramBufferPtr^.DataArray[5] := (DWord(MemorySpaceMaxAddress) shr 8) and $000000FF;
  DatagramBufferPtr^.DataArray[6] := DWord(MemorySpaceMaxAddress) and $000000FF;
  if AppCallback_AddressSpaceReadOnly(Node, DatagramBufferPtr^.DataArray[2]) then
    DatagramBufferPtr^.DataArray[7] := $01
  else
    DatagramBufferPtr^.DataArray[7] := $00;
  DatagramBufferPtr^.DataBufferSize := 8;
  DatagramBufferPtr^.CurrentCount := 0;
  Result := True;
end;

// *****************************************************************************
//  procedure OperationLockReply
//     Parameters: Node: Node that message was sent to
//                 OPStackMessage: Message to fill in to be sent, if no message reply
//                                 is required the message should be DeAllocated with
//                                 OPStackBuffers_DeAllocateMessage
//     Returns:     True  - if OPStackMessage is filled and needs to be sent
//                  False - if there is no message to send and the function has DeAllocated
//                        message
//     Description: The Message is recycled from the incoming datagram so it is
//                  allocated from the message pool
// *****************************************************************************
function OperationLockReply(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
begin
  OPStackBuffers_DeAllocateMessage(OPStackMessage);
  Result := False;
end;

// *****************************************************************************
//  procedure OperationGetUniqueIDReply
//     Parameters: Node: Node that message was sent to
//                 OPStackMessage: Message to fill in to be sent, if no message reply
//                                 is required the message should be DeAllocated with
//                                 OPStackBuffers_DeAllocateMessage
//     Returns:     True  - if OPStackMessage is filled and needs to be sent
//                  False - if there is no message to send and the function has DeAllocated
//                        message
//     Description: The Message is recycled from the incoming datagram so it is
//                  allocated from the message pool
// *****************************************************************************
function OperationGetUniqueIDReply(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
begin
  OPStackBuffers_DeAllocateMessage(OPStackMessage);
  Result := False;
end;

// *****************************************************************************
//  procedure OperationFreezeReply
//     Parameters: Node: Node that message was sent to
//                 OPStackMessage: Message to fill in to be sent, if no message reply
//                                 is required the message should be DeAllocated with
//                                 OPStackBuffers_DeAllocateMessage
//     Returns:     True  - if OPStackMessage is filled and needs to be sent
//                  False - if there is no message to send and the function has DeAllocated
//                        message
//     Description: The Message is recycled from the incoming datagram so it is
//                  allocated from the message pool
// *****************************************************************************
function OperationFreezeReply(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
begin
  OPStackBuffers_DeAllocateMessage(OPStackMessage);
  Result := False;
end;

// *****************************************************************************
//  procedure OperationIndicateReply
//     Parameters: Node: Node that message was sent to
//                 OPStackMessage: Message to fill in to be sent, if no message reply
//                                 is required the message should be DeAllocated with
//                                 OPStackBuffers_DeAllocateMessage
//     Returns:     True  - if OPStackMessage is filled and needs to be sent
//                  False - if there is no message to send and the function has DeAllocated
//                        message
//     Description: The Message is recycled from the incoming datagram so it is
//                  allocated from the message pool
// *****************************************************************************
function OperationIndicateReply(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
begin
  OPStackBuffers_DeAllocateMessage(OPStackMessage);
  Result := False;
end;

// *****************************************************************************
//  procedure OperationUpdateCompleteReply
//     Parameters: Node: Node that message was sent to
//                 OPStackMessage: Message to fill in to be sent, if no message reply
//                                 is required the message should be DeAllocated with
//                                 OPStackBuffers_DeAllocateMessage
//     Returns:     True  - if OPStackMessage is filled and needs to be sent
//                  False - if there is no message to send and the function has DeAllocated
//                        message
//     Description: The Message is recycled from the incoming datagram so it is
//                  allocated from the message pool
// *****************************************************************************
function OperationUpdateCompleteReply(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
begin
 OPStackBuffers_DeAllocateMessage(OPStackMessage);
 Result := False;
end;

// *****************************************************************************
//  procedure OperationResetReply
//     Parameters: Node: Node that message was sent to
//                 OPStackMessage: Message to fill in to be sent, if no message reply
//                                 is required the message should be DeAllocated with
//                                 OPStackBuffers_DeAllocateMessage
//     Returns:     True  - if OPStackMessage is filled and needs to be sent
//                  False - if there is no message to send and the function has DeAllocated
//                        message
//     Description: The Message is recycled from the incoming datagram so it is
//                  allocated from the message pool
// *****************************************************************************
function OperationResetReply(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
begin
  {$IFNDEF FPC}
    reset();
  {$ENDIF}
  Result := False;
end;

end.
