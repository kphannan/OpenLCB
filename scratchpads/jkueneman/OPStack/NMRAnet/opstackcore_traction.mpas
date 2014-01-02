unit opstackcore_traction;


{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  {$IFNDEF FPC}
  NMRAnetDCC,
  {$ENDIF}
  Float16,
  opstacknode,
  opstackcore_basic,
  nmranetdefines,
  opstackdefines,
  opstackbuffers,
  opstacktypes;

procedure TractionProtocol(AMessage: POPStackMessage;DestNode: PNMRAnetNode);
procedure TractionProtocolReply(Node: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage);

implementation

procedure TractionProtocol(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
var
  NewMessage: POPStackMessage;
begin
  NewMessage := nil;
  if OPStackBuffers_AllocateOPStackMessage(NewMessage, MTI_TRACTION_PROTOCOL, AMessage^.Source.AliasID, AMessage^.Source.ID, AMessage^.Dest.AliasID, AMessage^.Dest.ID) then
  begin
    OPStackBuffers_CopyData(AMessage^.Buffer, NewMessage^.Buffer);
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

procedure TractionProtocolReplyEmergencyStop(Node: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage);
var
  IsForward: Boolean;
  SpeedStep: Byte;
  AddressHi, AddressLo: Byte;
  {$IFNDEF FPC}
  DCCPacket: TDCCPacket;
  {$ENDIF}
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
    FunctionAddress := (DWord( NextMessage^.Buffer^.DataArray[1]) shl 16) or (DWord( NextMessage^.Buffer^.DataArray[2]) shl 8) or DWord( NextMessage^.Buffer^.DataArray[3]);
    MessageToSend^.Buffer^.DataArray[5] := Byte( (Node^.TrainData.Functions shr FunctionAddress) and $00000001);
  end;
end;

procedure TractionProtocolReplyConfigureDCCProxy(Node: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage);
begin

end;

procedure TractionProtocolReplyManageDCCProxy(Node: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage);
begin

end;

procedure TractionProtocolReply(Node: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage);
begin
  MessageToSend := nil;
  if OPStackBuffers_AllocateOPStackMessage(MessageToSend, MTI_TRACTION_REPLY, NextMessage^.Dest.AliasID, NextMessage^.Dest.ID, NextMessage^.Source.AliasID, NextMessage^.Source.ID) then
  begin
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
end;

end.
