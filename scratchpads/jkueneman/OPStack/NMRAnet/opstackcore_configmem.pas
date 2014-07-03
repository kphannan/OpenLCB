unit opstackcore_configmem;

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  template_node,
  {$IFDEF SUPPORT_NODE_FDI}
  template_node_fdi,
  {$ENDIF}
  {$IFDEF SUPPORT_VIRTUAL_NODES}
  template_vnode,
  {$IFDEF SUPPORT_VIRTUAL_NODE_FDI}
  template_vnode_fdi,
  {$ENDIF}
  {$ENDIF}
  template_configuration,
  template_configmem,
  nmranetdefines,
  opstackdefines,
  opstackbuffers,
  opstacktypes;

function CommandReadReplyHandler(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
function CommandReadStreamReplyHandler(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
function CommandWriteReplyHandler(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
function CommandWriteStreamReplyHandler(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;

function OperationGetConfigurationReplyHandler(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
function OperationGetSpaceInfoReplyHandler(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
function OperationLockReplyHandler(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
function OperationGetUniqueIDReplyHandler(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
function OperationFreezeReplyHandler(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
function OperationIndicateReplyHandler(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
function OperationUpdateCompleteReplyHandler(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
function OperationResetReplyHandler(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;


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
procedure DecodeConfigMemReadWriteHeader(Node: PNMRAnetNode; OPStackMessage: POPStackMessage; var AddressSpace: Byte; var ConfigAddress: DWord; var DataCount: DWord; var DataOffset: Byte);
var
  MaxSpaceSize: DWord;
  Buffer: PDatagramDataArray;
begin
  Buffer := PDatagramDataArray( PByte( @OPStackMessage^.Buffer^.DataArray));
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
    MCP_COMMAND_READ_STREAM  : DataCount := DWord( Buffer^[DataOffset] shl 24) or DWord( Buffer^[DataOffset+1] shl 16) or DWord( Buffer^[DataOffset+2] shl 8) or DWord( Buffer^[DataOffset+3]);
    MCP_COMMAND_READ         : DataCount := Buffer^[DataOffset] and $7F;         // Ignore the upper bit per the spec
    MCP_COMMAND_WRITE_STREAM : DataCount := 0;   // TODO
    MCP_COMMAND_WRITE        : DataCount := OPStackMessage^.Buffer^.DataBufferSize - DataOffset // The size of the datagram minus the size of the Header is the number of bytes to write
  else
    DataCount := 0;
  end;

     // Test the size against the size of the Address Space and adjust to the Max size if necessary
   MaxSpaceSize := MaxAddressByAddressSpace(Node, AddressSpace);
   if ConfigAddress >= MaxSpaceSize then                               // If the caller overruns the address we are done
     DataCount := 0
   else begin
     if ConfigAddress + DataCount > MaxSpaceSize then
       DataCount := MaxSpaceSize - ConfigAddress;
   end
end;

// *****************************************************************************
//  procedure EncodeConfigMemReadWriteHeader
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
procedure EncodeConfigMemReadWriteHeader(Node: PNMRAnetNode; OPStackMessage: POPStackMessage; IsRead, IsStream, IsForReply, IsReplyOK: Boolean; AddressSpace: Byte; ConfigAddress: DWord; ReadCount: DWord; UseAddressSpaceByte: Boolean; var DataOffset: Byte);
var
  Buffer: PDatagramDataArray;
begin
  Buffer := PDatagramDataArray( PByte( @OPStackMessage^.Buffer^.DataArray));

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

procedure InvertDatagramMessage(DatagramMessage: POPStackMessage);
var
  Temp: TNodeInfo;
  DatagramBufferPtr: PDatagramBuffer;
  i: Integer;
begin
  // Swap Source and Destination Node IDs
  Temp.AliasID := DatagramMessage^.Dest.AliasID;
  Temp.ID := DatagramMessage^.Dest.ID;
  DatagramMessage^.Dest.AliasID := DatagramMessage^.Source.AliasID;
  DatagramMessage^.Dest.ID := DatagramMessage^.Source.ID;
  DatagramMessage^.Source.AliasID := Temp.AliasID;
  DatagramMessage^.Source.ID := Temp.ID;

  // Reset a few key parameters
  DatagramBufferPtr := PDatagramBuffer( PByte(  DatagramMessage^.Buffer));
  DatagramMessage^.FramingBits := 0;
  DatagramBufferPtr^.CurrentCount := 0;
  DatagramBufferPtr^.ResendCount := 0;

 // for i := 0 to MAX_DATAGRAM_BYTES - 1 do
  //  DatagramBufferPtr^.DataArray[i] := 0;
end;

// *****************************************************************************
//  procedure CommandReadReplyHandler
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
function CommandReadReplyHandler(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
var
  DatagramBufferPtr: PDatagramBuffer;
  AddressSpace, DataOffset: Byte;
  ConfigAddress, ReadCount, OffsetAddress: DWord;
  i: Integer;
begin
  // Extract the information needed from the Datagram
  DecodeConfigMemReadWriteHeader(Node, OPStackMessage, AddressSpace, ConfigAddress, ReadCount, DataOffset);
  // Reuse the Message so invert it and reset a few key parameters
  InvertDatagramMessage(OPStackMessage);
  // Rebuild the Datagram to do what this functions task is...
  EncodeConfigMemReadWriteHeader(Node, OPStackMessage, True, False, True, True, AddressSpace, ConfigAddress, ReadCount, AddressSpace < MSI_CONFIG, DataOffset);

  DatagramBufferPtr := PDatagramBuffer( PByte( OPStackMessage^.Buffer));
  DatagramBufferPtr^.DataBufferSize := ReadCount + DataOffset;                  // Number to send back + the size of the header bytes

  {$IFDEF SUPPORT_VIRTUAL_NODES}
  if Node^.iIndex > 0 then
    OffsetAddress := ConfigAddress + (USER_CONFIGURATION_MEMORY_SIZE + (Node^.iIndex - 1)*USER_VNODE_CONFIGURATION_MEMORY_SIZE)
  else
  {$ENDIF}
    OffsetAddress := ConfigAddress;
    
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
            AppCallback_ReadConfiguration(OffsetAddress, ReadCount, PByte( @DatagramBufferPtr^.DataArray[DataOffset]));
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
            if ConfigAddress = 0 then
            begin
              DatagramBufferPtr^.DataArray[DataOffset] := USER_ACDI_USER_VERSION;
              Inc(DataOffset);
              Dec(ReadCount);
            end;
            if ReadCount > 0 then
              AppCallback_ReadAcdiUser(OffsetAddress, ReadCount, PByte( @DatagramBufferPtr^.DataArray[DataOffset]));
          end;
      MSI_FDI :
          begin
            {$IFDEF SUPPORT_VIRTUAL_NODES}
            {$IFDEF SUPPORT_VIRTUAL_NODE_FDI}
            if Node^.State and NS_VIRTUAL <> 0 then
            begin
               for i := 0 to ReadCount - 1 do
                DatagramBufferPtr^.DataArray[DataOffset+i] := USER_FDI_VNODE_ARRAY[i+ConfigAddress]
            end else {$ENDIF}{$ENDIF}
            begin
              {$IFDEF SUPPORT_NODE_FDI}
              for i := 0 to ReadCount - 1 do
                 DatagramBufferPtr^.DataArray[DataOffset+i] := USER_FDI_ARRAY[i+ConfigAddress];
              {$ENDIF}
            end;
          end;
  end;
  DatagramBufferPtr^.iStateMachine := STATE_DATAGRAM_SEND;
  Result := True;
end;

// *****************************************************************************
//  procedure CommandReadStreamReplyHandler
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
function CommandReadStreamReplyHandler(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
var
  DatagramBufferPtr: PDatagramBuffer;
{  AddressSpace, DataOffset: Byte;
  ConfigAddress, ReadCount: DWord;     }
begin
  DatagramBufferPtr := PDatagramBuffer( PByte( OPStackMessage^.Buffer));
//  DecodeConfigMemReadWriteHeader(Node, OPStackMessage, AddressSpace, ConfigAddress, ReadCount, DataOffset);
   // Option here, I could have failed the datagram ACK with "try again later" which is currently defined with error codes.
   // This method is acceptable but error codes are not defined yet.
   // Also do I do this on the Receive Side?  Is there any reason to let this propogate to here?  All this does is try to
   // allocate a stream buffer then wait for the caller to reply before setting up the stream.  This could be all done when
   // the message is first received on the other end....  It is constistent if done here with the datagram read/writes....
  
  // Zero the array to avoid confusion on debugging

(*
   if AddressSpace < MSI_CONFIG then
     EncodeConfigMemReadWriteHeader(Node, OPStackMessage, True, True, AddressSpace, ConfigAddress, ReadCount, True, DataOffset)
   else
     EncodeConfigMemReadWriteHeader(Node, OPStackMessage, True, True, AddressSpace, ConfigAddress, ReadCount, False, DataOffset);

   DatagramBufferPtr^.DataBufferSize := DataOffset;    // Just the header, no data that comes in the stream
   DatagramBufferPtr^.CurrentCount := 0;
   DatagramBufferPtr^.iStateMachine := 0;

   if OPStackBuffers_AllcoateStreamMessage(NewMessage, MTI_FRAME_TYPE_CAN_STREAM_SEND, DatagramMessage^.Source.AliasID, DatagramMessage^.Source.ID, DatagramMessage^.Dest.AliasID, DatagramMessage^.Dest.ID, True) then
   begin
     // Streams are handled by their StateMacheines (NodeRunOutgoingStreamStateMachine).  We link to the node and allow the state
     // machine to move through the build up/sending/teardown of the stream.  The datagram ACK from the Datagram Read Reply will move this statemachine
     // into its first active state.
     EncodeConfigMemReadWriteHeaderReply(Node, OPStackMessage, True, True) ;
     OPStackNode_StateMachineMessageLink(Node, NewMessage);      // No Stream ID's yet just waiting for the Stream link to be created
   end else
     EncodeConfigMemReadWriteHeaderReply(Node, OPStackMessage, False, True);
 *)
  DatagramBufferPtr^.iStateMachine := STATE_DATAGRAM_DONE;
  Result := False;
end;

// *****************************************************************************
//  procedure CommandWriteReplyHandler
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
function CommandWriteReplyHandler(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
var
  DatagramBufferPtr: PDatagramBuffer;
  AddressSpace, DataOffset: Byte;
  ConfigAddress, OffsetAddress: DWord;
  i: Integer;
  WriteCount: DWord;
begin
   // Extract the information needed from the Datagram
  DecodeConfigMemReadWriteHeader(Node, OPStackMessage, AddressSpace, ConfigAddress, WriteCount, DataOffset); // WriteCount is not used in a Write call to this function

  DatagramBufferPtr := PDatagramBuffer( PByte( OPStackMessage^.Buffer));
  
  {$IFDEF SUPPORT_VIRTUAL_NODES}
  if Node^.iIndex > 0 then
    OffsetAddress := ConfigAddress + (USER_CONFIGURATION_MEMORY_SIZE + (Node^.iIndex - 1)*USER_VNODE_CONFIGURATION_MEMORY_SIZE)
  else
  {$ENDIF}
    OffsetAddress := ConfigAddress;

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
         AppCallback_WriteConfiguration(OffsetAddress, WriteCount, PByte( @DatagramBufferPtr^.DataArray[DataOffset]));
       end;
    MSI_ACDI_USER :
       begin
         if ConfigAddress > 0 then
           AppCallback_WriteAcdiUser(OffsetAddress, WriteCount, PByte( @DatagramBufferPtr^.DataArray[DataOffset]));
       end;
    end;
    
    for i := 0 to (WriteCount+DataOffset) - 1 do
      DatagramBufferPtr^.DataArray[i] := 0;

  // Reuse the Message so invert it and reset a few key parameters
  InvertDatagramMessage(OPStackMessage);
  // Rebuild the Datagram to do what this functions task is...
  EncodeConfigMemReadWriteHeader(Node, OPStackMessage, False, False, True, True, AddressSpace, ConfigAddress, WriteCount, AddressSpace < MSI_CONFIG, DataOffset);
  DatagramBufferPtr^.DataBufferSize := DataOffset;
  DatagramBufferPtr^.iStateMachine := STATE_DATAGRAM_SEND;
  Result := True;
end;

// *****************************************************************************
//  procedure CommandWriteStreamReplyHandler
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
function CommandWriteStreamReplyHandler(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
var
  DatagramBufferPtr: PDatagramBuffer;
begin
  DatagramBufferPtr := PDatagramBuffer( PByte( OPStackMessage^.Buffer));
  DatagramBufferPtr^.iStateMachine := STATE_DATAGRAM_DONE;
  Result := False;
end;

// *****************************************************************************
//  procedure OperationGetConfigurationReplyHandler
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
function OperationGetConfigurationReplyHandler(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
var
  DatagramBufferPtr: PDatagramBuffer;
begin
  // Reuse the Message so invert it and reset a few key parameters
  InvertDatagramMessage(OPStackMessage);

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
  DatagramBufferPtr^.iStateMachine := STATE_DATAGRAM_SEND;
  Result := True;
end;

// *****************************************************************************
//  procedure OperationGetSpaceInfoReplyHandler
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
function OperationGetSpaceInfoReplyHandler(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
var
  DatagramBufferPtr: PDatagramBuffer;
  MemorySpaceMaxAddress: DWord;
begin
  // Reuse the Message so invert it and reset a few key parameters
  InvertDatagramMessage(OPStackMessage);

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
  DatagramBufferPtr^.iStateMachine := STATE_DATAGRAM_SEND;
  Result := True;
end;

// *****************************************************************************
//  procedure OperationLockReplyHandler
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
function OperationLockReplyHandler(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
var
  DatagramBufferPtr: PDatagramBuffer;
begin
  DatagramBufferPtr := PDatagramBuffer( PByte( OPStackMessage^.Buffer));
  DatagramBufferPtr^.iStateMachine := STATE_DATAGRAM_DONE;
  Result := False;
end;

// *****************************************************************************
//  procedure OperationGetUniqueIDReplyHandler
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
function OperationGetUniqueIDReplyHandler(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
var
  DatagramBufferPtr: PDatagramBuffer;
begin
  DatagramBufferPtr := PDatagramBuffer( PByte( OPStackMessage^.Buffer));
  DatagramBufferPtr^.iStateMachine := STATE_DATAGRAM_DONE;
  Result := False;
end;

// *****************************************************************************
//  procedure OperationFreezeReplyHandler
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
function OperationFreezeReplyHandler(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
var
  DatagramBufferPtr: PDatagramBuffer;
begin
  DatagramBufferPtr := PDatagramBuffer( PByte( OPStackMessage^.Buffer));
  DatagramBufferPtr^.iStateMachine := STATE_DATAGRAM_DONE;
  Result := False;
end;

// *****************************************************************************
//  procedure OperationIndicateReplyHandler
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
function OperationIndicateReplyHandler(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
var
  DatagramBufferPtr: PDatagramBuffer;
begin
  DatagramBufferPtr := PDatagramBuffer( PByte( OPStackMessage^.Buffer));
  DatagramBufferPtr^.iStateMachine := STATE_DATAGRAM_DONE;
  Result := False;
end;

// *****************************************************************************
//  procedure OperationUpdateCompleteReplyHandler
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
function OperationUpdateCompleteReplyHandler(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
var
  DatagramBufferPtr: PDatagramBuffer;
begin
  DatagramBufferPtr := PDatagramBuffer( PByte( OPStackMessage^.Buffer));
  DatagramBufferPtr^.iStateMachine := STATE_DATAGRAM_DONE;
  Result := False;
end;

// *****************************************************************************
//  procedure OperationResetReplyHandler
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
function OperationResetReplyHandler(Node: PNMRAnetNode; OPStackMessage: POPStackMessage): Boolean;
begin
  {$IFNDEF FPC}
    reset();
  {$ENDIF}
  Result := False;
end;

end.
