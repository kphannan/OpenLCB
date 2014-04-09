unit opstackcore_traction;


{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  {$IFNDEF FPC}
  NMRAnetDCC,
  {$ENDIF}
  template_hardware,
  Float16,
  opstacknode,
  opstackcore_basic,
  nmranetdefines,
  opstackdefines,
  opstackbuffers,
  opstacktypes;

procedure TractionProtocol(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
procedure TractionProtocolReply(Node: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage);

function TrySendFunctionSet(SourceNode, DestNode: PNMRAnetNode; FunctionAddress: DWord; Value: Word): Boolean;
function TrySendSpeedSet(SourceNode, DestNode: PNMRAnetNode; Speed: Byte): Boolean;
function TrySendDirectionSet(SourceNode, DestNode: PNMRAnetNode; IsForward: Boolean): Boolean;

implementation

function TrySendFunctionSet(SourceNode, DestNode: PNMRAnetNode; FunctionAddress: DWord; Value: Word): Boolean;
var
  NewMessage: POPStackMessage;
begin
  Result := False;
  NewMessage := nil;
  if IsOutgoingBufferAvailable then
    if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI_TRACTION_PROTOCOL, SourceNode^.Info.AliasID, SourceNode^.Info.ID, DestNode^.Info.AliasID, DestNode^.Info.ID) then
    begin
      OutgoingMessage(NewMessage);
      Result := True;
    end
end;

function TrySendSpeedSet(SourceNode, DestNode: PNMRAnetNode; Speed: Byte): Boolean;
var
  NewMessage: POPStackMessage;
begin
  Result := False;
  NewMessage := nil;
  if IsOutgoingBufferAvailable then
    if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI_TRACTION_PROTOCOL, SourceNode^.Info.AliasID, SourceNode^.Info.ID, DestNode^.Info.AliasID, DestNode^.Info.ID) then
    begin
      OutgoingMessage(NewMessage);
      Result := True;
    end
end;

function TrySendDirectionSet(SourceNode, DestNode: PNMRAnetNode; IsForward: Boolean): Boolean;
var
  NewMessage: POPStackMessage;
begin
  Result := False;
  NewMessage := nil;
  if IsOutgoingBufferAvailable then
    if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI_TRACTION_PROTOCOL, SourceNode^.Info.AliasID, SourceNode^.Info.ID, DestNode^.Info.AliasID, DestNode^.Info.ID) then
    begin
      OutgoingMessage(NewMessage);
      Result := True;
    end
end;

//******************************************************************************
// procedure TractionProtocol
// Parameters:
//    AMessage: The incoming OPStack Message
//    DestNode: The node the message is meant for
// Description:
//    Takes incoming Traction Protocol and posts it to be disected and handled
//    later in the Reply
//******************************************************************************
procedure TractionProtocol(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
var
  NewMessage: POPStackMessage;
begin
  NewMessage := nil;
  if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI_TRACTION_PROTOCOL, AMessage^.Source.AliasID, AMessage^.Source.ID, AMessage^.Dest.AliasID, AMessage^.Dest.ID) then
  begin
    OPStackBuffers_CopyData(NewMessage^.Buffer, AMessage^.Buffer);
    OPStackNode_IncomingMessageLink(DestNode, NewMessage)
  end else
    OptionalInteractionRejected(AMessage, DestNode, False);                            // Try again if you wish
end;

procedure TractionProtocolReplySpeedDir(Node: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage);
var
  IsForward: Boolean;
  AbsoluteSpeed: Real;
  SpeedStep: Word;
  AddressHi, AddressLo: Byte;
  {$IFNDEF FPC}
  DCCPacket: TDCCPacket;
  {$ENDIF}
begin
  if OPStackBuffers_AllocateOPStackMessage(MessageToSend, MTI_TRACTION_REPLY, NextMessage^.Dest.AliasID, NextMessage^.Dest.ID, NextMessage^.Source.AliasID, NextMessage^.Source.ID) then
  begin
    AddressHi := (Node^.TrainData.Address shr 8) and $00FF;                                                        // Split the address to make clear when loading bytes
    AddressLo := Node^.TrainData.Address and $00FF;
    Node^.TrainData.SpeedDir := (NextMessage^.Buffer^.DataArray[1] shl 8) or (NextMessage^.Buffer^.DataArray[2]);  // Update with the new Speed
    IsForward := Node^.TrainData.SpeedDir and $8000 <> $8000;                                                      // Split the Speed and Direction
    AbsoluteSpeed := HalfToFloat(Node^.TrainData.SpeedDir and not $8000);
    case Node^.TrainData.SpeedSteps of
      14  : begin
              AbsoluteSpeed := (14/100) * AbsoluteSpeed;
              {$IFDEF FPC}
              SpeedStep := Trunc(AbsoluteSpeed);
              {$ELSE}
              SpeedStep := Word( AbsoluteSpeed);
              {$ENDIF}
              SpeedStep := _14_STEP_TABLE[SpeedStep];
              if IsForward then
                SpeedStep := SpeedStep or $60
              else
                SpeedStep := SpeedStep or $40;
              {$IFNDEF FPC}
              if AddressHi and NMRA_LONGADDRESS_MASK_BYTE = NMRA_LONGADDRESS_MASK_BYTE then
                NMRA_DCC_LoadPacket(@DCCPacket, AddressHi, AddressLo, SpeedStep, 0, 0, 3)
              else
                NMRA_DCC_LoadPacket(@DCCPacket, AddressLo, SpeedStep, 0, 0, 0, 2);
              NMRA_DCC_QueuePacket(@Track, @DCCPacket, False);
              {$ENDIF}
            end;
      28  : begin
              AbsoluteSpeed := (28/100) * AbsoluteSpeed;
              {$IFDEF FPC}
              SpeedStep := Trunc(AbsoluteSpeed);
              {$ELSE}
              SpeedStep := Word( AbsoluteSpeed);
              {$ENDIF}
              SpeedStep := _28_STEP_TABLE[SpeedStep];
              if IsForward then
                SpeedStep := SpeedStep or $60
              else
                SpeedStep := SpeedStep or $40;
              {$IFNDEF FPC}
              if AddressHi and NMRA_LONGADDRESS_MASK_BYTE = NMRA_LONGADDRESS_MASK_BYTE then
                NMRA_DCC_LoadPacket(@DCCPacket, AddressHi, AddressLo, SpeedStep, 0, 0, 3)
              else
                NMRA_DCC_LoadPacket(@DCCPacket, AddressLo, SpeedStep, 0, 0, 0, 2);
              NMRA_DCC_QueuePacket(@Track, @DCCPacket, False);
              {$ENDIF}
            end;
      128 : begin
              AddressHi := AddressHi or NMRA_LONGADDRESS_MASK_BYTE;               // Allow a mistaken short address to work here by adding the $C0  Per Tim
              AbsoluteSpeed := (127/100) * AbsoluteSpeed;
              {$IFDEF FPC}
              SpeedStep := Trunc(AbsoluteSpeed);
              {$ELSE}
              SpeedStep := Word( AbsoluteSpeed);
              {$ENDIF}
              if SpeedStep > 0 then
                Inc(SpeedStep);   // 1 = EStop
              if IsForward then
                SpeedStep := SpeedStep or $80;
              {$IFNDEF FPC}
              NMRA_DCC_LoadPacket(@DCCPacket, AddressHi, AddressLo, %00111111, SpeedStep, 0, 4);
              NMRA_DCC_QueuePacket(@Track, @DCCPacket, False);
              {$ENDIF}
            end;
    end;
  end;
end;

procedure TractionProtocolReplyFunction(Node: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage);
var
  FunctionAddress: DWord;
  FunctionValue: Word;
  WideFunctionMask: DWord;
  FunctionMask, FunctionExtendedCode: Byte;
  AddressHi, AddressLo: Byte;
  {$IFNDEF FPC}
  DCCPacket: TDCCPacket;
  {$ENDIF}
begin
  if OPStackBuffers_AllocateOPStackMessage(MessageToSend, MTI_TRACTION_REPLY, NextMessage^.Dest.AliasID, NextMessage^.Dest.ID, NextMessage^.Source.AliasID, NextMessage^.Source.ID) then
  begin
    // Split the address to make clear when loading bytes
    AddressHi := (Node^.TrainData.Address shr 8) and $00FF;
    AddressLo := Node^.TrainData.Address and $00FF;

    // Get the new values
    FunctionAddress := (DWord( NextMessage^.Buffer^.DataArray[1]) shl 16) or (DWord( NextMessage^.Buffer^.DataArray[2]) shl 8) or DWord( NextMessage^.Buffer^.DataArray[3]);
    FunctionValue := (DWord( NextMessage^.Buffer^.DataArray[4]) shl 8) or DWord( NextMessage^.Buffer^.DataArray[5]);

    // Decode and update the proxy
    WideFunctionMask := $00000001;
    WideFunctionMask := WideFunctionMask shl FunctionAddress;                     // Set the correct Function Bit
    Node^.TrainData.Functions := Node^.TrainData.Functions and not WideFunctionMask;        // Clear the bit
    if FunctionValue > 0 then
      Node^.TrainData.Functions := Node^.TrainData.Functions or WideFunctionMask;      // Set the bit if needed

    if FunctionAddress < 29 then
    begin
      if FunctionAddress < 5 then
      begin
        FunctionMask := (Node^.TrainData.Functions shr 1) and $0F;
        if Node^.TrainData.Functions and $00000001 = 0 then
          FunctionMask := FunctionMask and not $10                                // Clear Bit 4
        else
          FunctionMask := FunctionMask or $10;                                    // Set Bit 4
        FunctionMask := FunctionMask or %10000000;                                // Opcode bits
      end else
      if FunctionAddress < 9 then
      begin
        FunctionMask := (Node^.TrainData.Functions shr 5) and $0F;
        FunctionMask := FunctionMask or %10110000;                                // Opcode bits
      end else
      if FunctionAddress < 13 then
      begin
        FunctionMask := (Node^.TrainData.Functions shr 9) and $0F;
        FunctionMask := FunctionMask or %10100000;                                // Opcode bits
      end else
      if FunctionAddress < 21 then
      begin
        FunctionMask := Node^.TrainData.Functions shr 13;
        FunctionExtendedCode := %11011110
      end
    end else
    begin
      FunctionMask := Node^.TrainData.Functions shr 21;
      FunctionExtendedCode := %11011111
    end;

    {$IFNDEF FPC}
    // Now create the DCC Packet
    if AddressHi and NMRA_LONGADDRESS_MASK_BYTE = NMRA_LONGADDRESS_MASK_BYTE then
    begin
      if FunctionAddress < 13 then
        NMRA_DCC_LoadPacket(@DCCPacket, AddressHi, AddressLo, FunctionMask, 0, 0, 3)
      else
        NMRA_DCC_LoadPacket(@DCCPacket, AddressHi, AddressLo, FunctionExtendedCode, FunctionMask, 0, 4)
    end else
    begin
      if FunctionAddress < 13 then
        NMRA_DCC_LoadPacket(@DCCPacket, AddressLo, FunctionMask, 0, 0, 0, 3)
      else
        NMRA_DCC_LoadPacket(@DCCPacket, AddressLo, FunctionExtendedCode, FunctionMask, 0, 0, 4)
    end;

    // Queue the Packet
    NMRA_DCC_QueuePacket(@Track, @DCCPacket, False);
    {$ENDIF}
  end;
end;

procedure TractionProtocolReplyEmergencyStop(Node: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage);
var
  IsForward: Boolean;
  SpeedStep: Byte;
  AddressHi, AddressLo: Byte;
  {$IFNDEF FPC}
  DCCPacket: TDCCPacket;
  {$ENDIF}
begin
  if OPStackBuffers_AllocateOPStackMessage(MessageToSend, MTI_TRACTION_REPLY, NextMessage^.Dest.AliasID, NextMessage^.Dest.ID, NextMessage^.Source.AliasID, NextMessage^.Source.ID) then
  begin
    // Split the address to make clear when loading bytes
    AddressHi := (Node^.TrainData.Address shr 8) and $00FF;
    AddressLo := Node^.TrainData.Address and $00FF;

    // Update the Speed to 0
    IsForward := Node^.TrainData.SpeedDir and $8000 <> $8000;
    if IsForward then
      Node^.TrainData.SpeedDir := $0000
    else
      Node^.TrainData.SpeedDir := $8000;
    case Node^.TrainData.SpeedSteps of
      14, 28 :
        begin
          SpeedStep := $01;
          if IsForward then
            SpeedStep := SpeedStep or $60
          else
            SpeedStep := SpeedStep or $40;
          {$IFNDEF FPC}
          if AddressHi and NMRA_LONGADDRESS_MASK_BYTE = NMRA_LONGADDRESS_MASK_BYTE then
            NMRA_DCC_LoadPacket(@DCCPacket, AddressHi, AddressLo, SpeedStep, 0, 0, 3)
          else
            NMRA_DCC_LoadPacket(@DCCPacket, AddressLo, SpeedStep, 0, 0, 0, 2);

          NMRA_DCC_QueuePacket(@Track, @DCCPacket, False);
          {$ENDIF}
        end;
      128 :
        begin
          SpeedStep := $01;
          if IsForward then
            SpeedStep := SpeedStep or $80;
          {$IFNDEF FPC}
          NMRA_DCC_LoadPacket(@DCCPacket, AddressHi, AddressLo, %00111111, SpeedStep, 0, 4);

          NMRA_DCC_QueuePacket(@Track, @DCCPacket, False);
          {$ENDIF}
        end;
    end
  end;
end;

procedure TractionProtocolReplyQuerySpeed(Node: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage);
begin
  MessageToSend := nil;
  if OPStackBuffers_AllocateMultiFrameMessage(MessageToSend, MTI_TRACTION_REPLY, NextMessage^.Dest.AliasID, NextMessage^.Dest.ID, NextMessage^.Source.AliasID, NextMessage^.Source.ID) then
  begin
    MessageToSend^.Buffer^.DataArray[0] := TRACTION_QUERY_SPEED_REPLY;
    MessageToSend^.Buffer^.DataArray[1] := Hi( Node^.TrainData.SpeedDir);
    MessageToSend^.Buffer^.DataArray[2] := Lo( Node^.TrainData.SpeedDir);
    MessageToSend^.Buffer^.DataArray[3] := $00;                                 // Result Reply
    MessageToSend^.Buffer^.DataArray[4] := $FF;                                 // Not a Number (NaN) for Commanded Speed (not supported in DCC)
    MessageToSend^.Buffer^.DataArray[5] := $FF;                                 // Not a Number (NaN) for Commanded Speed (not supported in DCC)
    MessageToSend^.Buffer^.DataArray[6] := $FF;                                 // Not a Number (NaN) for Actual Speed (not supported in DCC)
    MessageToSend^.Buffer^.DataArray[7] := $FF;                                 // Not a Number (NaN) for Actual Speed (not supported in DCC)
    MessageToSend^.Buffer^.DataBufferSize := 8;
  end;
end;

procedure TractionProtocolReplyQueryFunction(Node: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage);
var
  FunctionAddress: DWord;
begin
  MessageToSend := nil;
  if OPStackBuffers_AllocateOPStackMessage(MessageToSend, MTI_TRACTION_REPLY, NextMessage^.Dest.AliasID, NextMessage^.Dest.ID, NextMessage^.Source.AliasID, NextMessage^.Source.ID) then
  begin
    MessageToSend^.Buffer^.DataArray[0] := TRACTION_QUERY_FUNCTION_REPLY;
    MessageToSend^.Buffer^.DataArray[1] := NextMessage^.Buffer^.DataArray[1];   // Reuse Address
    MessageToSend^.Buffer^.DataArray[2] := NextMessage^.Buffer^.DataArray[2];
    MessageToSend^.Buffer^.DataArray[3] := NextMessage^.Buffer^.DataArray[3];
    MessageToSend^.Buffer^.DataArray[4] := 0;
    MessageToSend^.Buffer^.DataBufferSize := 5;
    FunctionAddress := (DWord( NextMessage^.Buffer^.DataArray[1]) shl 16) or (DWord( NextMessage^.Buffer^.DataArray[2]) shl 8) or DWord( NextMessage^.Buffer^.DataArray[3]);
    MessageToSend^.Buffer^.DataArray[5] := Byte( (Node^.TrainData.Functions shr FunctionAddress) and $00000001);
  end;
end;

procedure TractionProtocolReplyConfigureDCCProxy(Node: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage);
begin
  if OPStackBuffers_AllocateOPStackMessage(MessageToSend, MTI_TRACTION_REPLY, NextMessage^.Dest.AliasID, NextMessage^.Dest.ID, NextMessage^.Source.AliasID, NextMessage^.Source.ID) then
  begin
    case NextMessage^.Buffer^.DataArray[1] of
      TRACTION_ATTACH_NODE_REPLY         :
          begin
            // TODO
            OPStackBuffers_DeAllocateMessage(MessageToSend);
            MessageToSend := nil;
          end;
      TRACTION_DETACH_NODE_REPLY         :
          begin
            // TODO
            OPStackBuffers_DeAllocateMessage(MessageToSend);
            MessageToSend := nil;
          end;
      TRACTION_ATTACH_DCC_ADDRESS_REPLY  :
          begin
            if Node^.TrainData.State and TS_ALLOCATED = 0 then
            begin
              Node^.TrainData.State := Node^.TrainData.State or TS_ALLOCATED;
              Node^.TrainData.Address := NextMessage^.Buffer^.DataArray[3] or (NextMessage^.Buffer^.DataArray[2]  shl 8);
              Node^.TrainData.SpeedSteps := NextMessage^.Buffer^.DataArray[4];
              MessageToSend^.Buffer^.DataArray := NextMessage^.Buffer^.DataArray;
              MessageToSend^.Buffer^.DataArray[5] := TRACTION_ATTACH_NODE_REPLY_CODE_OK;
              MessageToSend^.Buffer^.DataBufferSize := 6;
              
              // HOW DO I GENERALIZE WHERE THESE EVENT ARE BY INDEX??????????
              OPStackNode_SetEventState(Node^.Events.ProducedState, 2, EVENT_STATE_INVALID);       // IsIdleProxy is not true now
              OPStackNode_SetEventState(Node^.Events.ProducedState, 3, EVENT_STATE_VALID);         // IsInUseProxy is true now
              OPStackNode_SetPCER_Flag(Node, 3, False);                                            // Fire the InUse PCER

              // NOW DO I TRIGGER THE DYNAMIC ADDRESS PROXY EVENT?????????

              OPStackNode_Allocate;                                               // Allocate a node to replace it
            end else
            begin
              // TODO
              OPStackBuffers_DeAllocateMessage(MessageToSend);
              MessageToSend := nil;
            end;
          end;
      TRACTION_DETACH_DCC_ADDRESS_REPLY  :
          begin
            if Node^.TrainData.State and TS_ALLOCATED <> 0 then
            begin
              Node^.TrainData.State := Node^.TrainData.State and not TS_ALLOCATED;
              Node^.TrainData.Address := 0;
              Node^.TrainData.SpeedSteps := 0;
              MessageToSend^.Buffer^.DataArray := NextMessage^.Buffer^.DataArray;
              MessageToSend^.Buffer^.DataArray[5] := TRACTION_DETTACH_NODE_REPLY_CODE_OK;
              MessageToSend^.Buffer^.DataBufferSize := 6;

              // HOW DO I GENERALIZE WHERE THESE EVENT ARE BY INDEX??????????
              OPStackNode_SetEventState(Node^.Events.ProducedState, 2, EVENT_STATE_VALID);         // IsIdleProxy is true now
              OPStackNode_SetEventState(Node^.Events.ProducedState, 3, EVENT_STATE_INVALID);       // IsInUseProxy is not true now
              OPStackNode_SetPCER_Flag(Node, 2, False);                                            // Fire the IsIdle PCER

              // NOW DO I TRIGGER THE DYNAMIC ADDRESS PROXY EVENT?????????

              OPStackNode_MarkForRelease(Node);                                   // Allocate a node to replace it
            end else
            begin
              // TODO
              OPStackBuffers_DeAllocateMessage(MessageToSend);
              MessageToSend := nil;
            end;
          end;
    end;
  end;
end;

procedure TractionProtocolReplyManageDCCProxy(Node: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage);
begin
  case NextMessage^.Buffer^.DataArray[1] of
    TRACTION_MANAGE_PROXY_RESERVE   :
        begin
          if OPStackBuffers_AllocateOPStackMessage(MessageToSend, MTI_TRACTION_REPLY, NextMessage^.Dest.AliasID, NextMessage^.Dest.ID, NextMessage^.Source.AliasID, NextMessage^.Source.ID) then
          begin
            MessageToSend^.Buffer^.DataArray := NextMessage^.Buffer^.DataArray;
            MessageToSend^.Buffer^.DataBufferSize := 3;
            if Node^.TrainData.State and TS_RESERVED <> 0 then
              MessageToSend^.Buffer^.DataArray[2] := TRACTION_MANAGE_RESERVE_REPLY_FAIL
            else begin
              Node^.TrainData.State := Node^.TrainData.State or TS_RESERVED;
              MessageToSend^.Buffer^.DataArray[2] := TRACTION_MANAGE_RESERVE_REPLY_OK
            end;
          end;
        end;
    TRACTION_MANAGE_PROXY_RELEASE   :
        begin
          Node^.TrainData.State := Node^.TrainData.State and not TS_RESERVED;
        end;
    TRACTION_MANAGE_PROXY_QUERY     :
        begin
           if OPStackBuffers_AllocateOPStackMessage(MessageToSend, MTI_TRACTION_REPLY, NextMessage^.Dest.AliasID, NextMessage^.Dest.ID, NextMessage^.Source.AliasID, NextMessage^.Source.ID) then
           begin
             // TODO
            OPStackBuffers_DeAllocateMessage(MessageToSend);
            MessageToSend := nil;
           end;
        end;
  end;
end;

procedure TractionProtocolReply(Node: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage);
begin
  MessageToSend := nil;
  case NextMessage^.Buffer^.DataArray[0] and TRACTION_OPERATION_MASK of
      TRACTION_CMD :
            begin
               case NextMessage^.Buffer^.DataArray[0] of
                 TRACTION_SPEED_DIR  : begin TractionProtocolReplySpeedDir(Node, MessageToSend, NextMessage); Exit; end;
                 TRACTION_FUNCTION   : begin TractionProtocolReplyFunction(Node, MessageToSend, NextMessage); Exit; end;
                 TRACTION_E_STOP     : begin TractionProtocolReplyEmergencyStop(Node, MessageToSend, NextMessage); Exit; end;
               end;
            end;
      TRACTION_QUERY :
            begin
              case NextMessage^.Buffer^.DataArray[0] of
                 TRACTION_QUERY_SPEED    : begin TractionProtocolReplyQuerySpeed(Node, MessageToSend, NextMessage); Exit; end;
                 TRACTION_QUERY_FUNCTION : begin TractionProtocolReplyQueryFunction(Node, MessageToSend, NextMessage); Exit; end;
               end;
            end;
      TRACTION_DCC_PROXY :
            begin
               case NextMessage^.Buffer^.DataArray[0] of
                 TRACTION_CONFIGURE_DCC_PROXY : begin TractionProtocolReplyConfigureDCCProxy(Node, MessageToSend, NextMessage); Exit; end;
                 TRACTION_MANAGE_DCC_PROXY    : begin TractionProtocolReplyManageDCCProxy(Node, MessageToSend, NextMessage); Exit; end;
               end;
            end;
  end;
end;

end.
