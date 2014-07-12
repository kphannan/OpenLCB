unit opstackcore_traction;


{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  {$IFNDEF FPC}
  {$IFDEF SUPPORT_DCC}
  NMRAnetDCC,
  {$ENDIF}
  {$ENDIF}
  template_hardware,
  Float16,
  opstacknode,
  opstackcore_basic,
  nmranetdefines,
  opstackdefines,
  opstackbuffers,
  nmranetutilities,
  template_userstatemachine,
  opstacktypes;

const
  MAX_CONTROLLER_NOTIFY_TIME = 10;  // 10 * 100ms = 1 second to wait for controller that is being stolen from to reply if it allows the steal

procedure TractionProtocolMessage(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
function TractionProtocolHandler(DestNode: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage): Boolean;
procedure TractionProtocolReply(DestNode: PNMRAnetNode; AMessage: POPStackMessage);
procedure TractionProtocolTimerTick(Node: PNMRAnetNode);

implementation

procedure TractionProtocolSpeedDirReplyHandler(DestNode: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage);
var
  IsForward: Boolean;
  AbsoluteSpeed: Real;
  SpeedStep: Word;
  {$IFNDEF FPC}
  {$IFDEF SUPPORT_DCC}
  AddressHi, AddressLo: Byte;
  DCCPacket: TDCCPacket;
  {$ENDIF}
  {$ENDIF}
begin
  MessageToSend := nil;
  if NMRAnetUtilities_EqualNodeIDInfo(DestNode^.TrainData.Controller, NextMessage^.Source) then
  begin
    {$IFNDEF FPC}
    {$IFDEF SUPPORT_DCC}
    AddressHi := (DestNode^.TrainData.Address shr 8) and $00FF;                                                        // Split the address to make clear when loading bytes
    AddressLo := DestNode^.TrainData.Address and $00FF;
    {$ENDIF}
    {$ENDIF}
    DestNode^.TrainData.SpeedDir := (NextMessage^.Buffer^.DataArray[1] shl 8) or (NextMessage^.Buffer^.DataArray[2]);  // Update with the new Speed
    IsForward := DestNode^.TrainData.SpeedDir and $8000 <> $8000;                                                      // Split the Speed and Direction
    AbsoluteSpeed := HalfToFloat(DestNode^.TrainData.SpeedDir and not $8000);
    case DestNode^.TrainData.SpeedSteps of
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
              {$IFDEF SUPPORT_DCC}
              if AddressHi and NMRA_LONGADDRESS_MASK_BYTE = NMRA_LONGADDRESS_MASK_BYTE then
                NMRA_DCC_LoadPacket(@DCCPacket, AddressHi, AddressLo, SpeedStep, 0, 0, 3)
              else
                NMRA_DCC_LoadPacket(@DCCPacket, AddressLo, SpeedStep, 0, 0, 0, 2);
              NMRA_DCC_QueuePacket(@Track, @DCCPacket, False);
              {$ENDIF}
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
              {$IFDEF SUPPORT_DCC}
              if AddressHi and NMRA_LONGADDRESS_MASK_BYTE = NMRA_LONGADDRESS_MASK_BYTE then
                NMRA_DCC_LoadPacket(@DCCPacket, AddressHi, AddressLo, SpeedStep, 0, 0, 3)
              else
                NMRA_DCC_LoadPacket(@DCCPacket, AddressLo, SpeedStep, 0, 0, 0, 2);
              NMRA_DCC_QueuePacket(@Track, @DCCPacket, False)
              {$ENDIF}
              {$ENDIF}
            end;
      128 : begin
              {$IFNDEF FPC}
              {$IFDEF SUPPORT_DCC}
              AddressHi := AddressHi or NMRA_LONGADDRESS_MASK_BYTE;               // Allow a mistaken short address to work here by adding the $C0  Per Tim
              {$ENDIF}
              {$ENDIF}
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
              {$IFDEF SUPPORT_DCC}
              NMRA_DCC_LoadPacket(@DCCPacket, AddressHi, AddressLo, %00111111, SpeedStep, 0, 4);
              NMRA_DCC_QueuePacket(@Track, @DCCPacket, False);
              {$ENDIF}
              {$ENDIF}
            end;
    end;
    AppCallback_TractionProtocol(DestNode, NextMessage);
  end;
  UnLinkDeAllocateAndTestForMessageToSend(DestNode, MessageToSend, NextMessage);
end;

procedure TractionProtocolFunctionReplyHandler(DestNode: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage);
var
  FunctionAddress: DWord;
  FunctionValue: Word;
  WideFunctionMask: DWord;
  FunctionMask, FunctionExtendedCode: Byte;
  {$IFNDEF FPC}
  {$IFDEF SUPPORT_DCC}
  AddressHi, AddressLo: Byte;
  DCCPacket: TDCCPacket;
  {$ENDIF}
  {$ENDIF}
begin
  MessageToSend := nil;
  if NMRAnetUtilities_EqualNodeIDInfo(DestNode^.TrainData.Controller, NextMessage^.Source) then
  begin

    {$IFNDEF FPC}
    {$IFDEF SUPPORT_DCC}
    // Split the address to make clear when loading bytes
    AddressHi := (DestNode^.TrainData.Address shr 8) and $00FF;
    AddressLo := DestNode^.TrainData.Address and $00FF;
    {$ENDIF}
    {$ENDIF}

    // Get the new values
    FunctionAddress := (DWord( NextMessage^.Buffer^.DataArray[1]) shl 16) or (DWord( NextMessage^.Buffer^.DataArray[2]) shl 8) or DWord( NextMessage^.Buffer^.DataArray[3]);
    FunctionValue := (DWord( NextMessage^.Buffer^.DataArray[4]) shl 8) or DWord( NextMessage^.Buffer^.DataArray[5]);

    // Decode and update the proxy
    WideFunctionMask := $00000001;
    WideFunctionMask := WideFunctionMask shl FunctionAddress;                     // Set the correct Function Bit
    DestNode^.TrainData.Functions := DestNode^.TrainData.Functions and not WideFunctionMask;        // Clear the bit
    if FunctionValue > 0 then
      DestNode^.TrainData.Functions := DestNode^.TrainData.Functions or WideFunctionMask;      // Set the bit if needed

    if FunctionAddress < 29 then
    begin
      if FunctionAddress < 5 then
      begin
        FunctionMask := (DestNode^.TrainData.Functions shr 1) and $0F;
        if DestNode^.TrainData.Functions and $00000001 = 0 then
          FunctionMask := FunctionMask and not $10                                // Clear Bit 4
        else
          FunctionMask := FunctionMask or $10;                                    // Set Bit 4
        FunctionMask := FunctionMask or %10000000;                                // Opcode bits
      end else
      if FunctionAddress < 9 then
      begin
        FunctionMask := (DestNode^.TrainData.Functions shr 5) and $0F;
        FunctionMask := FunctionMask or %10110000;                                // Opcode bits
      end else
      if FunctionAddress < 13 then
      begin
        FunctionMask := (DestNode^.TrainData.Functions shr 9) and $0F;
        FunctionMask := FunctionMask or %10100000;                                // Opcode bits
      end else
      if FunctionAddress < 21 then
      begin
        FunctionMask := DestNode^.TrainData.Functions shr 13;
        FunctionExtendedCode := %11011110
      end
    end else
    begin
      FunctionMask := DestNode^.TrainData.Functions shr 21;
      FunctionExtendedCode := %11011111
    end;

    {$IFNDEF FPC}
    {$IFDEF SUPPORT_DCC}
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
    {$ENDIF}
    AppCallback_TractionProtocol(DestNode, NextMessage);
  end;
  UnLinkDeAllocateAndTestForMessageToSend(DestNode, MessageToSend, NextMessage);
end;

procedure TractionProtocolEmergencyStopReplyHandler(DestNode: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage);
var
  IsForward: Boolean;
  SpeedStep: Byte;
  {$IFNDEF FPC}
  {$IFDEF SUPPORT_DCC}
  AddressHi, AddressLo: Byte;
  DCCPacket: TDCCPacket;
  {$ENDIF}
  {$ENDIF}
begin
  MessageToSend := nil;
  if NMRAnetUtilities_EqualNodeIDInfo(DestNode^.TrainData.Controller, NextMessage^.Source) then
  begin

    {$IFNDEF FPC}
    {$IFDEF SUPPORT_DCC}
    // Split the address to make clear when loading bytes
    AddressHi := (DestNode^.TrainData.Address shr 8) and $00FF;
    AddressLo := DestNode^.TrainData.Address and $00FF;
    {$ENDIF}
    {$ENDIF}

    // Update the Speed to 0
    IsForward := DestNode^.TrainData.SpeedDir and $8000 <> $8000;
    if IsForward then
      DestNode^.TrainData.SpeedDir := $0000
    else
      DestNode^.TrainData.SpeedDir := $8000;
    case DestNode^.TrainData.SpeedSteps of
      14, 28 :
        begin
          SpeedStep := $01;
          if IsForward then
            SpeedStep := SpeedStep or $60
          else
            SpeedStep := SpeedStep or $40;
          {$IFNDEF FPC}
          {$IFDEF SUPPORT_DCC}
          if AddressHi and NMRA_LONGADDRESS_MASK_BYTE = NMRA_LONGADDRESS_MASK_BYTE then
            NMRA_DCC_LoadPacket(@DCCPacket, AddressHi, AddressLo, SpeedStep, 0, 0, 3)
          else
            NMRA_DCC_LoadPacket(@DCCPacket, AddressLo, SpeedStep, 0, 0, 0, 2);

          NMRA_DCC_QueuePacket(@Track, @DCCPacket, False);
          {$ENDIF}
          {$ENDIF}
        end;
      128 :
        begin
          SpeedStep := $01;
          if IsForward then
            SpeedStep := SpeedStep or $80;
          {$IFNDEF FPC}
          {$IFDEF SUPPORT_DCC}
          NMRA_DCC_LoadPacket(@DCCPacket, AddressHi, AddressLo, %00111111, SpeedStep, 0, 4);

          NMRA_DCC_QueuePacket(@Track, @DCCPacket, False);
          {$ENDIF}
          {$ENDIF}
        end;
    end;
    AppCallback_TractionProtocol(DestNode, NextMessage);
  end;
  UnLinkDeAllocateAndTestForMessageToSend(DestNode, MessageToSend, NextMessage);
end;

function TractionProtocolQuerySpeedReplyHandler(DestNode: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage): Boolean;
begin
  Result := False;
  MessageToSend := nil;
  if OPStackBuffers_AllocateMultiFrameMessage(MessageToSend, MTI_TRACTION_REPLY, NextMessage^.Dest, NextMessage^.Source) then
  begin
    MessageToSend^.Buffer^.DataArray[0] := TRACTION_QUERY_SPEED;
    MessageToSend^.Buffer^.DataArray[1] := Hi( DestNode^.TrainData.SpeedDir);
    MessageToSend^.Buffer^.DataArray[2] := Lo( DestNode^.TrainData.SpeedDir);
    MessageToSend^.Buffer^.DataArray[3] := $00;                                 // Result Reply
    MessageToSend^.Buffer^.DataArray[4] := $FF;                                 // Not a Number (NaN) for Commanded Speed (not supported in DCC)
    MessageToSend^.Buffer^.DataArray[5] := $FF;                                 // Not a Number (NaN) for Commanded Speed (not supported in DCC)
    MessageToSend^.Buffer^.DataArray[6] := $FF;                                 // Not a Number (NaN) for Actual Speed (not supported in DCC)
    MessageToSend^.Buffer^.DataArray[7] := $FF;                                 // Not a Number (NaN) for Actual Speed (not supported in DCC)
    MessageToSend^.Buffer^.DataBufferSize := 8;
    AppCallback_TractionProtocol(DestNode, NextMessage);
    Result := UnLinkDeAllocateAndTestForMessageToSend(DestNode, MessageToSend, NextMessage);
  end;
end;

function TractionProtocolQueryFunctionReplyHandler(DestNode: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage): Boolean;
var
  FunctionAddress: DWord;
begin
  Result := False;
  MessageToSend := nil;
  if OPStackBuffers_AllocateOPStackMessage(MessageToSend, MTI_TRACTION_REPLY, NextMessage^.Dest, NextMessage^.Source, False) then
  begin
    MessageToSend^.Buffer^.DataArray[0] := TRACTION_QUERY_FUNCTION;
    MessageToSend^.Buffer^.DataArray[1] := NextMessage^.Buffer^.DataArray[1];   // Reuse Address
    MessageToSend^.Buffer^.DataArray[2] := NextMessage^.Buffer^.DataArray[2];
    MessageToSend^.Buffer^.DataArray[3] := NextMessage^.Buffer^.DataArray[3];
    MessageToSend^.Buffer^.DataArray[4] := 0;
    MessageToSend^.Buffer^.DataArray[5] := 0;
    MessageToSend^.Buffer^.DataBufferSize := 6;
    FunctionAddress := (DWord( NextMessage^.Buffer^.DataArray[1]) shl 16) or (DWord( NextMessage^.Buffer^.DataArray[2]) shl 8) or DWord( NextMessage^.Buffer^.DataArray[3]);
    MessageToSend^.Buffer^.DataArray[5] := Byte( (DestNode^.TrainData.Functions shr FunctionAddress) and $00000001);
    AppCallback_TractionProtocol(DestNode, NextMessage);
    Result := UnLinkDeAllocateAndTestForMessageToSend(DestNode, MessageToSend, NextMessage);
  end;
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

function TractionProtocolManageReplyHandler(DestNode: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage): Boolean;
begin
  Result := False;
  MessageToSend := nil;
  if NextMessage^.Buffer^.DataArray[1] = TRACTION_MANAGE_RESERVE then
  begin
    if OPStackBuffers_AllocateOPStackMessage(MessageToSend, MTI_TRACTION_REPLY, NextMessage^.Dest, NextMessage^.Source, False) then
    begin
      MessageToSend^.Buffer^.DataBufferSize := 3;
      MessageToSend^.Buffer^.DataArray[0] := TRACTION_MANAGE;
      MessageToSend^.Buffer^.DataArray[1] := TRACTION_MANAGE_RESERVE;
      if NMRAnetUtilities_NullNodeIDInfo(DestNode^.TrainData.Lock) or NMRAnetUtilities_EqualNodeIDInfo(DestNode^.TrainData.Lock, NextMessage^.Source) then
      begin
        MessageToSend^.Buffer^.DataArray[2] := TRACTION_MANAGE_RESERVE_REPLY_OK;
        DestNode^.TrainData.Lock.AliasID := NextMessage^.Source.AliasID;
        DestNode^.TrainData.Lock.ID[0] := NextMessage^.Source.ID[0];
        DestNode^.TrainData.Lock.ID[1] := NextMessage^.Source.ID[1];
        DestNode^.TrainData.State := DestNode^.TrainData.State or TS_LOCKED;
      end else
        MessageToSend^.Buffer^.DataArray[2] := TRACTION_MANAGE_RESERVE_REPLY_FAIL;
      AppCallback_TractionProtocol(DestNode, NextMessage);
      Result := UnLinkDeAllocateAndTestForMessageToSend(DestNode, MessageToSend, NextMessage);
    end
  end else
  if NextMessage^.Buffer^.DataArray[1] = TRACTION_MANAGE_RELEASE then
  begin
    if NMRAnetUtilities_EqualNodeIDInfo(DestNode^.TrainData.Lock, NextMessage^.Source) then
    begin
      DestNode^.TrainData.Lock.AliasID := 0;
      DestNode^.TrainData.Lock.ID[0] := 0;
      DestNode^.TrainData.Lock.ID[1] := 0;
      DestNode^.TrainData.State := DestNode^.TrainData.State and not TS_LOCKED;
    end;
    AppCallback_TractionProtocol(DestNode, NextMessage);
    Result := UnLinkDeAllocateAndTestForMessageToSend(DestNode, MessageToSend, NextMessage);
  end;
end;

function TractionProtocolControllerReplyHandler(DestNode: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage): Boolean;
var
  i: Integer;
  MultiFrameBuffer: PMultiFrameBuffer;
begin
  MessageToSend := nil;
  Result := False;
  if DestNode^.TrainData.State and TS_LOCKED <> 0 then
  begin
    case NextMessage^.Buffer^.DataArray[1] of
      TRACTION_CONTROLLER_CONFIG_ASSIGN :
          begin
            if NMRAnetUtilities_NullNodeIDInfo(DestNode^.TrainData.Controller) or NMRAnetUtilities_EqualNodeIDInfo(DestNode^.TrainData.Controller, NextMessage^.Source) then
            begin
              // The Controller is not set to another node.......

              // Need to test if the controller is allowed to connect to this Train by this Train
              if OPStackBuffers_AllocateOPStackMessage(MessageToSend, MTI_TRACTION_REPLY, NextMessage^.Dest, NextMessage^.Source, False) then
              begin
                MessageToSend^.Buffer^.DataBufferSize := 3;
                MessageToSend^.Buffer^.DataArray[0] := TRACTION_CONTROLLER_CONFIG;
                MessageToSend^.Buffer^.DataArray[1] := TRACTION_CONTROLLER_CONFIG_ASSIGN;
                MessageToSend^.Buffer^.DataArray[2] := TRACTION_CONTROLLER_ASSIGN_REPLY_OK;

                MultiFrameBuffer := PMultiFrameBuffer( PByte( NextMessage^.Buffer));
                NMRAnetUtilities_Load48BitNodeIDWithSimpleData(DestNode^.TrainData.Controller.ID, PSimpleDataArray( @MultiFrameBuffer^.DataArray[3])^);
                if MultiFrameBuffer^.DataArray[2] and TRACTION_FLAGS_ALIAS_INCLUDED <> 0 then
                  DestNode^.TrainData.Controller.AliasID := Word(MultiFrameBuffer^.DataArray[9] shl 8) or Word(MultiFrameBuffer^.DataArray[10]);
                AppCallback_TractionProtocol(DestNode, NextMessage);
                Result := UnLinkDeAllocateAndTestForMessageToSend(DestNode, MessageToSend, NextMessage);
              end
            end else
            begin
              // The Controller is set to another node, need to ask that node if it will release the Train

              // Need to ask the assigned controller if we should allow the assignment

              if OPStackBuffers_AllocateMultiFrameMessage(MessageToSend, MTI_TRACTION_PROTOCOL, DestNode^.Info, DestNode^.TrainData.Controller) then
              begin
                MessageToSend^.Buffer^.DataBufferSize := 11;
                MessageToSend^.Buffer^.DataArray[0] := TRACTION_CONTROLLER_CONFIG;
                MessageToSend^.Buffer^.DataArray[1] := TRACTION_CONTROLLER_CONFIG_NOTIFY;
                for i := 2 to 10 do
                  MessageToSend^.Buffer^.DataArray[i] := NextMessage^.Buffer^.DataArray[i];
                DestNode^.TrainData.State := DestNode^.TrainData.State or TS_WAITING_FOR_CONTROLLER_NOTIFY;
                DestNode^.TrainData.Timer := 0;
                DestNode^.TrainData.LinkedNode := NextMessage^.Source;    // Store the Controller who is asking for control
                AppCallback_TractionProtocol(DestNode, NextMessage);
                Result := UnLinkDeAllocateAndTestForMessageToSend(DestNode, MessageToSend, NextMessage);
                // Timer started, either the current controller replies or it times out and the new controller is assigned
              end
            end
          end;
      TRACTION_CONTROLLER_CONFIG_RELEASE :
          begin
            if NMRAnetUtilities_EqualNodeIDInfo(DestNode^.TrainData.Controller, NextMessage^.Source) then
            begin
              DestNode^.TrainData.Controller.AliasID := 0;
              DestNode^.TrainData.Controller.ID[0] := 0;
              DestNode^.TrainData.Controller.ID[1] := 0;
            end;
            AppCallback_TractionProtocol(DestNode, NextMessage);
            Result := UnLinkDeAllocateAndTestForMessageToSend(DestNode, MessageToSend, NextMessage);
          end;
      TRACTION_CONTROLLER_CONFIG_QUERY :
          begin
            if OPStackBuffers_AllocateMultiFrameMessage(MessageToSend, MTI_TRACTION_REPLY, NextMessage^.Dest, NextMessage^.Source) then
            begin
              MultiFrameBuffer := PMultiFrameBuffer( PByte( MessageToSend^.Buffer));
              MultiFrameBuffer^.DataBufferSize := 11;
              MultiFrameBuffer^.DataArray[0] := TRACTION_CONTROLLER_CONFIG;
              MultiFrameBuffer^.DataArray[1] := TRACTION_CONTROLLER_CONFIG_QUERY;
              MultiFrameBuffer^.DataArray[2] := $01;  // Alias included
              NMRAnetUtilities_LoadSimpleDataWith48BitNodeID(@DestNode^.TrainData.Controller.ID, PSimpleDataArray( PByte( @MessageToSend^.Buffer^.DataArray[3])));
              MultiFrameBuffer^.DataArray[9] := Hi( DestNode^.TrainData.Controller.AliasID);
              MultiFrameBuffer^.DataArray[10] := Lo( DestNode^.TrainData.Controller.AliasID);
              AppCallback_TractionProtocol(DestNode, NextMessage);
              Result := UnLinkDeAllocateAndTestForMessageToSend(DestNode, MessageToSend, NextMessage);
            end;
          end
    else
      Result := UnLinkDeAllocateAndTestForMessageToSend(DestNode, MessageToSend, NextMessage);
    end
  end else
  begin
    // Handle messages that don't need a lock
    case NextMessage^.Buffer^.DataArray[1] of
      TRACTION_CONTROLLER_CONFIG_NOTIFY :
          begin
            AppCallback_TractionProtocol(DestNode, NextMessage);
            Result := UnLinkDeAllocateAndTestForMessageToSend(DestNode, MessageToSend, NextMessage);
          end
    else
      Result := UnLinkDeAllocateAndTestForMessageToSend(DestNode, MessageToSend, NextMessage);
    end;
  end
end;

function TractionProtocolConsistReplyHandler(DestNode: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage): Boolean;
begin
  MessageToSend := nil;
  if NMRAnetUtilities_EqualNodeIDInfo(DestNode^.TrainData.Lock, NextMessage^.Source) then
  begin
   // if DestNode^.TrainData.State and TS_LOCKED <> 0 then
   /// begin
      // Only manage if the DestNode is locked
   // end;
    AppCallback_TractionProtocol(DestNode, NextMessage);
  end;
  Result := UnLinkDeAllocateAndTestForMessageToSend(DestNode, MessageToSend, NextMessage);
end;

procedure TractionProtocolMessage(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
begin
  OPStackNode_IncomingMessageLink(DestNode, AMessage)
end;

function TractionProtocolHandler(DestNode: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage): Boolean;
begin
  Result := False;
  MessageToSend := nil;
  case NextMessage^.Buffer^.DataArray[0] of
    TRACTION_SPEED_DIR         : begin TractionProtocolSpeedDirReplyHandler(DestNode, MessageToSend, NextMessage); Exit; end;
    TRACTION_FUNCTION          : begin TractionProtocolFunctionReplyHandler(DestNode, MessageToSend, NextMessage); Exit; end;
    TRACTION_E_STOP            : begin TractionProtocolEmergencyStopReplyHandler(DestNode, MessageToSend, NextMessage); Exit; end;
    TRACTION_QUERY_SPEED       : begin Result := TractionProtocolQuerySpeedReplyHandler(DestNode, MessageToSend, NextMessage); Exit; end;
    TRACTION_QUERY_FUNCTION    : begin Result := TractionProtocolQueryFunctionReplyHandler(DestNode, MessageToSend, NextMessage); Exit; end;
    TRACTION_CONTROLLER_CONFIG : begin Result := TractionProtocolControllerReplyHandler(DestNode, MessageToSend, NextMessage); Exit; end;
    TRACTION_CONSIST           : begin Result := TractionProtocolConsistReplyHandler(DestNode, MessageToSend, NextMessage); Exit; end;
    TRACTION_MANAGE            : begin Result := TractionProtocolManageReplyHandler(DestNode, MessageToSend, NextMessage); Exit; end
  else
    UnLinkDeAllocateAndTestForMessageToSend(DestNode, MessageToSend, NextMessage);
  end;
end;

procedure TractionProtocolReply(DestNode: PNMRAnetNode; AMessage: POPStackMessage);
var
  MessageToSend: POPStackMessage;
begin
  if DestNode^.TrainData.State and TS_WAITING_FOR_CONTROLLER_NOTIFY <> 0 then
    if AMessage^.Buffer^.DataArray[0] = TRACTION_CONTROLLER_CONFIG then
      if AMessage^.Buffer^.DataArray[1] = TRACTION_CONTROLLER_CONFIG_NOTIFY then
      begin
        MessageToSend := nil;
        if OPStackBuffers_AllocateOPStackMessage(MessageToSend, MTI_TRACTION_REPLY, AMessage^.Dest, AMessage^.Source, False) then
        begin
          MessageToSend^.Buffer^.DataBufferSize := 3;
          MessageToSend^.Buffer^.DataArray[0] := TRACTION_CONTROLLER_CONFIG;
          MessageToSend^.Buffer^.DataArray[1] := TRACTION_CONTROLLER_CONFIG_ASSIGN;
          MessageToSend^.Buffer^.DataArray[2] := AMessage^.Buffer^.DataArray[2];
          if MessageToSend^.Buffer^.DataArray[2] = TRACTION_CONTROLLER_ASSIGN_REPLY_OK then
            DestNode^.TrainData.Controller := DestNode^.TrainData.LinkedNode;
          DestNode^.TrainData.LinkedNode.ID := NULL_NODE_ID;
          DestNode^.TrainData.LinkedNode.AliasID := 0;
          DestNode^.TrainData.State := DestNode^.TrainData.State and not TS_WAITING_FOR_CONTROLLER_NOTIFY;
          AppCallback_TractionProtocolReply(DestNode, AMessage);
          UnLinkDeAllocateAndTestForMessageToSend(DestNode, nil, AMessage);
        end;
        Exit;
      end;
  AppCallback_TractionProtocolReply(DestNode, AMessage);
  UnLinkDeAllocateAndTestForMessageToSend(DestNode, nil, AMessage);
end;

procedure TractionProtocolTimerTick(Node: PNMRAnetNode);
var
  MessageToSend: POPStackMessage;
begin
  if Node^.TrainData.State and TS_WAITING_FOR_CONTROLLER_NOTIFY <> 0 then
  begin
    Inc(Node^.TrainData.Timer);
    if Node^.TrainData.Timer > MAX_CONTROLLER_NOTIFY_TIME then
    begin
      MessageToSend := nil;
      // The last controller did not reply so just take it
      if IsOutgoingBufferAvailable then
        if OPStackBuffers_AllocateOPStackMessage(MessageToSend, MTI_TRACTION_REPLY, Node^.Info, Node^.TrainData.LinkedNode, False) then
        begin
          Node^.TrainData.State := Node^.TrainData.State and not TS_WAITING_FOR_CONTROLLER_NOTIFY;
          MessageToSend^.Buffer^.DataBufferSize := 3;
          MessageToSend^.Buffer^.DataArray[0] := TRACTION_CONTROLLER_CONFIG;
          MessageToSend^.Buffer^.DataArray[1] := TRACTION_CONTROLLER_CONFIG_ASSIGN;
          MessageToSend^.Buffer^.DataArray[2] := TRACTION_CONTROLLER_ASSIGN_REPLY_OK;
          Node^.TrainData.Controller := Node^.TrainData.LinkedNode;
          Node^.TrainData.LinkedNode.AliasID := 0;
          Node^.TrainData.LinkedNode.ID := NULL_NODE_ID;
          AppCallback_TractionProtocol(Node, MessageToSend);
          OutgoingMessage(MessageToSend, True);
        end
    end;
  end;
end;


end.