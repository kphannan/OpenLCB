
dsPIC33_DMA_MapToChannelReg:

;dsPIC33_DMA.mpas,141 :: 		begin
;dsPIC33_DMA.mpas,142 :: 		Result := Channel_0_Reg;
; Result start address is: 2 (W1)
	MOV	W11, W1
;dsPIC33_DMA.mpas,143 :: 		Result := Result + (DMA_CHANNEL_STRUCTURE_SIZE * ChannelNumber);    // Compiler known the operand is a word so the Size is the number of words to increment
	SL	W10, #3, W0
	SL	W0, #1, W0
	ADD	W1, W0, W1
;dsPIC33_DMA.mpas,144 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_MapToChannelReg:
	RETURN
; end of dsPIC33_DMA_MapToChannelReg

_dsPIC33_DMA_Enable:

;dsPIC33_DMA.mpas,159 :: 		begin
;dsPIC33_DMA.mpas,160 :: 		RegPtr := MapToChannelReg(ChannelNumber, @DMA0CON);
	PUSH	W11
	MOV	#lo_addr(DMA0CON), W11
	CALL	dsPIC33_DMA_MapToChannelReg
	POP	W11
; RegPtr start address is: 0 (W0)
;dsPIC33_DMA.mpas,161 :: 		if Enable then
	CP0	W11
	BRA NZ	L__dsPIC33_DMA_Enable45
	GOTO	L__dsPIC33_DMA_Enable3
L__dsPIC33_DMA_Enable45:
;dsPIC33_DMA.mpas,162 :: 		RegPtr^.CHEN := 1
	BSET	[W0], #15
; RegPtr end address is: 0 (W0)
	GOTO	L__dsPIC33_DMA_Enable4
;dsPIC33_DMA.mpas,163 :: 		else
L__dsPIC33_DMA_Enable3:
;dsPIC33_DMA.mpas,164 :: 		RegPtr^.CHEN := 0
; RegPtr start address is: 0 (W0)
	BCLR	[W0], #15
; RegPtr end address is: 0 (W0)
L__dsPIC33_DMA_Enable4:
;dsPIC33_DMA.mpas,165 :: 		end;
L_end_dsPIC33_DMA_Enable:
	RETURN
; end of _dsPIC33_DMA_Enable

_dsPIC33_DMA_DataSize:

;dsPIC33_DMA.mpas,180 :: 		begin
;dsPIC33_DMA.mpas,181 :: 		RegPtr := MapToChannelReg(ChannelNumber, @DMA0CON);
	PUSH	W11
	MOV	#lo_addr(DMA0CON), W11
	CALL	dsPIC33_DMA_MapToChannelReg
	POP	W11
; RegPtr start address is: 0 (W0)
;dsPIC33_DMA.mpas,182 :: 		if _Size = DATASIZE_BYTE then
	CP	W11, #0
	BRA Z	L__dsPIC33_DMA_DataSize47
	GOTO	L__dsPIC33_DMA_DataSize7
L__dsPIC33_DMA_DataSize47:
;dsPIC33_DMA.mpas,183 :: 		RegPtr^.SIZE := 1
	BSET	[W0], #14
; RegPtr end address is: 0 (W0)
	GOTO	L__dsPIC33_DMA_DataSize8
;dsPIC33_DMA.mpas,184 :: 		else
L__dsPIC33_DMA_DataSize7:
;dsPIC33_DMA.mpas,185 :: 		RegPtr^.SIZE := 0
; RegPtr start address is: 0 (W0)
	BCLR	[W0], #14
; RegPtr end address is: 0 (W0)
L__dsPIC33_DMA_DataSize8:
;dsPIC33_DMA.mpas,186 :: 		end;
L_end_dsPIC33_DMA_DataSize:
	RETURN
; end of _dsPIC33_DMA_DataSize

_dsPIC33_DMA_Direction:

;dsPIC33_DMA.mpas,202 :: 		begin
;dsPIC33_DMA.mpas,203 :: 		RegPtr := MapToChannelReg(ChannelNumber, @DMA0CON);
	PUSH	W11
	MOV	#lo_addr(DMA0CON), W11
	CALL	dsPIC33_DMA_MapToChannelReg
	POP	W11
; RegPtr start address is: 0 (W0)
;dsPIC33_DMA.mpas,204 :: 		if Direction = DIRECTION_RAM_TO_PERIPHERAL then
	CP	W11, #0
	BRA Z	L__dsPIC33_DMA_Direction49
	GOTO	L__dsPIC33_DMA_Direction11
L__dsPIC33_DMA_Direction49:
;dsPIC33_DMA.mpas,205 :: 		RegPtr^.DIR_ := 1
	BSET	[W0], #13
; RegPtr end address is: 0 (W0)
	GOTO	L__dsPIC33_DMA_Direction12
;dsPIC33_DMA.mpas,206 :: 		else
L__dsPIC33_DMA_Direction11:
;dsPIC33_DMA.mpas,207 :: 		RegPtr^.DIR_ := 0
; RegPtr start address is: 0 (W0)
	BCLR	[W0], #13
; RegPtr end address is: 0 (W0)
L__dsPIC33_DMA_Direction12:
;dsPIC33_DMA.mpas,208 :: 		end;
L_end_dsPIC33_DMA_Direction:
	RETURN
; end of _dsPIC33_DMA_Direction

_dsPIC33_DMA_HalfBlockTransferInterrupt:

;dsPIC33_DMA.mpas,227 :: 		begin
;dsPIC33_DMA.mpas,228 :: 		RegPtr := MapToChannelReg(ChannelNumber, @DMA0CON);
	PUSH	W11
	MOV	#lo_addr(DMA0CON), W11
	CALL	dsPIC33_DMA_MapToChannelReg
	POP	W11
; RegPtr start address is: 0 (W0)
;dsPIC33_DMA.mpas,229 :: 		if HalfBlockInterrupt then
	CP0	W11
	BRA NZ	L__dsPIC33_DMA_HalfBlockTransferInterrupt51
	GOTO	L__dsPIC33_DMA_HalfBlockTransferInterrupt15
L__dsPIC33_DMA_HalfBlockTransferInterrupt51:
;dsPIC33_DMA.mpas,230 :: 		RegPtr^.HALF := 1
	BSET	[W0], #12
; RegPtr end address is: 0 (W0)
	GOTO	L__dsPIC33_DMA_HalfBlockTransferInterrupt16
;dsPIC33_DMA.mpas,231 :: 		else
L__dsPIC33_DMA_HalfBlockTransferInterrupt15:
;dsPIC33_DMA.mpas,232 :: 		RegPtr^.HALF := 0
; RegPtr start address is: 0 (W0)
	BCLR	[W0], #12
; RegPtr end address is: 0 (W0)
L__dsPIC33_DMA_HalfBlockTransferInterrupt16:
;dsPIC33_DMA.mpas,233 :: 		end;
L_end_dsPIC33_DMA_HalfBlockTransferInterrupt:
	RETURN
; end of _dsPIC33_DMA_HalfBlockTransferInterrupt

_dsPIC33_DMA_NullWrite:

;dsPIC33_DMA.mpas,248 :: 		begin
;dsPIC33_DMA.mpas,249 :: 		RegPtr := MapToChannelReg(ChannelNumber, @DMA0CON);
	PUSH	W11
	MOV	#lo_addr(DMA0CON), W11
	CALL	dsPIC33_DMA_MapToChannelReg
	POP	W11
; RegPtr start address is: 0 (W0)
;dsPIC33_DMA.mpas,250 :: 		if Enable then
	CP0	W11
	BRA NZ	L__dsPIC33_DMA_NullWrite53
	GOTO	L__dsPIC33_DMA_NullWrite19
L__dsPIC33_DMA_NullWrite53:
;dsPIC33_DMA.mpas,251 :: 		RegPtr^.NULLW := 1
	BSET	[W0], #11
; RegPtr end address is: 0 (W0)
	GOTO	L__dsPIC33_DMA_NullWrite20
;dsPIC33_DMA.mpas,252 :: 		else
L__dsPIC33_DMA_NullWrite19:
;dsPIC33_DMA.mpas,253 :: 		RegPtr^.NULLW := 0
; RegPtr start address is: 0 (W0)
	BCLR	[W0], #11
; RegPtr end address is: 0 (W0)
L__dsPIC33_DMA_NullWrite20:
;dsPIC33_DMA.mpas,254 :: 		end;
L_end_dsPIC33_DMA_NullWrite:
	RETURN
; end of _dsPIC33_DMA_NullWrite

_dsPIC33_DMA_AddressMode:

;dsPIC33_DMA.mpas,269 :: 		begin
;dsPIC33_DMA.mpas,270 :: 		RegPtr := MapToChannelReg(ChannelNumber, @DMA0CON);
	PUSH	W11
	MOV	#lo_addr(DMA0CON), W11
	CALL	dsPIC33_DMA_MapToChannelReg
	POP	W11
; RegPtr start address is: 6 (W3)
	MOV	W0, W3
;dsPIC33_DMA.mpas,271 :: 		RegPtr^ := RegPtr^ and $FFCF;                                                 // Clear the Address Mode Bits
	MOV	[W0], W2
	MOV	#65487, W1
	AND	W2, W1, [W0]
;dsPIC33_DMA.mpas,272 :: 		RegPtr^ := RegPtr^ or (Mode shl 4);
	SL	W11, #4, W0
	IOR	W0, [W3], [W3]
; RegPtr end address is: 6 (W3)
;dsPIC33_DMA.mpas,273 :: 		end;
L_end_dsPIC33_DMA_AddressMode:
	RETURN
; end of _dsPIC33_DMA_AddressMode

_dsPIC33_DMA_OperatingMode:

;dsPIC33_DMA.mpas,288 :: 		begin
;dsPIC33_DMA.mpas,289 :: 		RegPtr := MapToChannelReg(ChannelNumber, @DMA0CON);
	PUSH	W11
	MOV	#lo_addr(DMA0CON), W11
	CALL	dsPIC33_DMA_MapToChannelReg
	POP	W11
; RegPtr start address is: 6 (W3)
	MOV	W0, W3
;dsPIC33_DMA.mpas,290 :: 		RegPtr^ := RegPtr^ and $FFFC;                                                 // Clear the Operating Mode Bits
	MOV	[W0], W2
	MOV	#65532, W1
	AND	W2, W1, [W0]
;dsPIC33_DMA.mpas,291 :: 		RegPtr^ := RegPtr^ or Mode;
	IOR	W11, [W3], [W3]
; RegPtr end address is: 6 (W3)
;dsPIC33_DMA.mpas,292 :: 		end;
L_end_dsPIC33_DMA_OperatingMode:
	RETURN
; end of _dsPIC33_DMA_OperatingMode

_dsPIC33_DMA_InterruptSelect:

;dsPIC33_DMA.mpas,308 :: 		begin
;dsPIC33_DMA.mpas,309 :: 		RegPtr := MapToChannelReg(ChannelNumber, @DMA0REQ);
	PUSH	W11
	MOV	#lo_addr(DMA0REQ), W11
	CALL	dsPIC33_DMA_MapToChannelReg
	POP	W11
; RegPtr start address is: 6 (W3)
	MOV	W0, W3
;dsPIC33_DMA.mpas,310 :: 		RegPtr^ := RegPtr^ and $FF80;                                                 // Clear the bottom 6 bits
	MOV	[W0], W2
	MOV	#65408, W1
	AND	W2, W1, [W0]
;dsPIC33_DMA.mpas,311 :: 		RegPtr^ := RegPtr^ or IRQ_Select
	IOR	W11, [W3], [W3]
; RegPtr end address is: 6 (W3)
;dsPIC33_DMA.mpas,312 :: 		end;
L_end_dsPIC33_DMA_InterruptSelect:
	RETURN
; end of _dsPIC33_DMA_InterruptSelect

_dsPIC33_DMA_ManualDMATransfer:

;dsPIC33_DMA.mpas,327 :: 		begin
;dsPIC33_DMA.mpas,328 :: 		RegPtr := MapToChannelReg(ChannelNumber, @DMA0REQ);
	PUSH	W11
	MOV	#lo_addr(DMA0REQ), W11
	CALL	dsPIC33_DMA_MapToChannelReg
	POP	W11
; RegPtr start address is: 0 (W0)
;dsPIC33_DMA.mpas,329 :: 		if Manual then
	CP0	W11
	BRA NZ	L__dsPIC33_DMA_ManualDMATransfer58
	GOTO	L__dsPIC33_DMA_ManualDMATransfer26
L__dsPIC33_DMA_ManualDMATransfer58:
;dsPIC33_DMA.mpas,330 :: 		RegPtr^.FORCE := 1
	BSET	[W0], #15
; RegPtr end address is: 0 (W0)
	GOTO	L__dsPIC33_DMA_ManualDMATransfer27
;dsPIC33_DMA.mpas,331 :: 		else
L__dsPIC33_DMA_ManualDMATransfer26:
;dsPIC33_DMA.mpas,332 :: 		RegPtr^.FORCE := 0
; RegPtr start address is: 0 (W0)
	BCLR	[W0], #15
; RegPtr end address is: 0 (W0)
L__dsPIC33_DMA_ManualDMATransfer27:
;dsPIC33_DMA.mpas,333 :: 		end;
L_end_dsPIC33_DMA_ManualDMATransfer:
	RETURN
; end of _dsPIC33_DMA_ManualDMATransfer

_dsPIC33_DMA_AddressOffsetA:

;dsPIC33_DMA.mpas,348 :: 		begin
;dsPIC33_DMA.mpas,350 :: 		RegPtr := MapToChannelReg(ChannelNumber, @DMA0STAL);
	PUSH	W11
	MOV	#lo_addr(DMA0STAL), W11
	CALL	dsPIC33_DMA_MapToChannelReg
	POP	W11
;dsPIC33_DMA.mpas,351 :: 		RegPtr^ := Address
	MOV	W11, [W0]
;dsPIC33_DMA.mpas,357 :: 		end;
L_end_dsPIC33_DMA_AddressOffsetA:
	RETURN
; end of _dsPIC33_DMA_AddressOffsetA

_dsPIC33_DMA_AddressOffsetB:

;dsPIC33_DMA.mpas,372 :: 		begin
;dsPIC33_DMA.mpas,374 :: 		RegPtr := MapToChannelReg(ChannelNumber, @DMA0STBL);
	PUSH	W11
	MOV	#lo_addr(DMA0STBL), W11
	CALL	dsPIC33_DMA_MapToChannelReg
	POP	W11
;dsPIC33_DMA.mpas,375 :: 		RegPtr^ := Address
	MOV	W11, [W0]
;dsPIC33_DMA.mpas,381 :: 		end;
L_end_dsPIC33_DMA_AddressOffsetB:
	RETURN
; end of _dsPIC33_DMA_AddressOffsetB

_dsPIC33_DMA_PeripheralAddress:

;dsPIC33_DMA.mpas,399 :: 		begin
;dsPIC33_DMA.mpas,400 :: 		RegPtr := MapToChannelReg(ChannelNumber, @DMA0PAD);
	PUSH	W11
	MOV	#lo_addr(DMA0PAD), W11
	CALL	dsPIC33_DMA_MapToChannelReg
	POP	W11
;dsPIC33_DMA.mpas,401 :: 		RegPtr^ := Address
	MOV	W11, [W0]
;dsPIC33_DMA.mpas,402 :: 		end;
L_end_dsPIC33_DMA_PeripheralAddress:
	RETURN
; end of _dsPIC33_DMA_PeripheralAddress

_dsPIC33_DMA_TransferCount:

;dsPIC33_DMA.mpas,446 :: 		begin
;dsPIC33_DMA.mpas,447 :: 		RegPtr := MapToChannelReg(ChannelNumber, @DMA0CNT);
	PUSH	W11
	MOV	#lo_addr(DMA0CNT), W11
	CALL	dsPIC33_DMA_MapToChannelReg
	POP	W11
;dsPIC33_DMA.mpas,448 :: 		RegPtr^ := Count - 1                                                          // 0 Counts!  An 8 Bit transfer = 7 in this register
	SUB	W11, #1, W1
	MOV	W1, [W0]
;dsPIC33_DMA.mpas,449 :: 		end;
L_end_dsPIC33_DMA_TransferCount:
	RETURN
; end of _dsPIC33_DMA_TransferCount

_dsPIC33_DMA_MostRecentRAM_Address:

;dsPIC33_DMA.mpas,462 :: 		begin
;dsPIC33_DMA.mpas,464 :: 		Result := 0;
; Result start address is: 2 (W1)
	CLR	W1
;dsPIC33_DMA.mpas,469 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_DMA_MostRecentRAM_Address:
	RETURN
; end of _dsPIC33_DMA_MostRecentRAM_Address

_dsPIC33_DMA_PeripheralWriteCollisionDetected:

;dsPIC33_DMA.mpas,482 :: 		begin
;dsPIC33_DMA.mpas,484 :: 		Result := DMAPWC.ChannelNumber = 1
	MOV	#1, W0
	SL	W0, W10, W1
	MOV	#lo_addr(DMAPWC), W0
	AND	W1, [W0], W0
	CLR	W1
	CP	W0, #0
	BRA NZ	L__dsPIC33_DMA_PeripheralWriteCollisionDetected65
	GOTO	L__dsPIC33_DMA_PeripheralWriteCollisionDetected34
L__dsPIC33_DMA_PeripheralWriteCollisionDetected65:
	MOV.B	#1, W0
	MOV.B	W0, W1
L__dsPIC33_DMA_PeripheralWriteCollisionDetected34:
; Result start address is: 2 (W1)
	CP.B	W1, #1
	CLR	W1
	BRA NZ	L__dsPIC33_DMA_PeripheralWriteCollisionDetected66
	COM	W1
L__dsPIC33_DMA_PeripheralWriteCollisionDetected66:
;dsPIC33_DMA.mpas,489 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_DMA_PeripheralWriteCollisionDetected:
	RETURN
; end of _dsPIC33_DMA_PeripheralWriteCollisionDetected

_dsPIC33_DMA_DSPRAMWriteCollisionDetected:

;dsPIC33_DMA.mpas,506 :: 		begin
;dsPIC33_DMA.mpas,508 :: 		Result := DMARQC.ChannelNumber = 1
	MOV	#1, W0
	SL	W0, W10, W1
	MOV	#lo_addr(DMARQC), W0
	AND	W1, [W0], W0
	CLR	W1
	CP	W0, #0
	BRA NZ	L__dsPIC33_DMA_DSPRAMWriteCollisionDetected68
	GOTO	L__dsPIC33_DMA_DSPRAMWriteCollisionDetected36
L__dsPIC33_DMA_DSPRAMWriteCollisionDetected68:
	MOV.B	#1, W0
	MOV.B	W0, W1
L__dsPIC33_DMA_DSPRAMWriteCollisionDetected36:
; Result start address is: 2 (W1)
	CP.B	W1, #1
	CLR	W1
	BRA NZ	L__dsPIC33_DMA_DSPRAMWriteCollisionDetected69
	COM	W1
L__dsPIC33_DMA_DSPRAMWriteCollisionDetected69:
;dsPIC33_DMA.mpas,514 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_DMA_DSPRAMWriteCollisionDetected:
	RETURN
; end of _dsPIC33_DMA_DSPRAMWriteCollisionDetected

_dsPIC33_DMA_CurrentPingPongRegister:

;dsPIC33_DMA.mpas,527 :: 		begin
;dsPIC33_DMA.mpas,529 :: 		if DMAPPS.ChannelNumber = 1 then
	MOV	#1, W0
	SL	W0, W10, W1
	MOV	#lo_addr(DMAPPS), W0
	AND	W1, [W0], W0
	CLR	W1
	CP	W0, #0
	BRA NZ	L__dsPIC33_DMA_CurrentPingPongRegister71
	GOTO	L__dsPIC33_DMA_CurrentPingPongRegister41
L__dsPIC33_DMA_CurrentPingPongRegister71:
	MOV.B	#1, W0
	MOV.B	W0, W1
L__dsPIC33_DMA_CurrentPingPongRegister41:
	CP.B	W1, #1
	BRA Z	L__dsPIC33_DMA_CurrentPingPongRegister72
	GOTO	L__dsPIC33_DMA_CurrentPingPongRegister39
L__dsPIC33_DMA_CurrentPingPongRegister72:
;dsPIC33_DMA.mpas,530 :: 		Result := PING_PONG_REG_B
; Result start address is: 2 (W1)
	MOV	#1, W1
; Result end address is: 2 (W1)
	GOTO	L__dsPIC33_DMA_CurrentPingPongRegister40
;dsPIC33_DMA.mpas,531 :: 		else
L__dsPIC33_DMA_CurrentPingPongRegister39:
;dsPIC33_DMA.mpas,532 :: 		Result := PING_PONG_REG_A
; Result start address is: 2 (W1)
	CLR	W1
; Result end address is: 2 (W1)
L__dsPIC33_DMA_CurrentPingPongRegister40:
;dsPIC33_DMA.mpas,540 :: 		end;
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_DMA_CurrentPingPongRegister:
	RETURN
; end of _dsPIC33_DMA_CurrentPingPongRegister

_dsPIC33_DMA_LastActiveChannel:

;dsPIC33_DMA.mpas,552 :: 		begin
;dsPIC33_DMA.mpas,554 :: 		Result := DMALCA
; Result start address is: 2 (W1)
	MOV	DMALCA, W1
;dsPIC33_DMA.mpas,559 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_DMA_LastActiveChannel:
	RETURN
; end of _dsPIC33_DMA_LastActiveChannel
