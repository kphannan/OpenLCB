
_NMRA_DCC_Initialize:

;NMRAnetDCC.mpas,215 :: 		begin
;NMRAnetDCC.mpas,216 :: 		InitializeBuffer(@Track, @TrackQueue, @TrackQueuePriority, MAX_TRACK_BUFFER_DEPTH, MAX_TRACK_PRIORITY_BUFFER_DEPTH);
	PUSH	W10
	PUSH	W11
	PUSH	W12
	PUSH	W13
	MOV.B	#16, W13
	MOV	#lo_addr(NMRAnetDCC_TrackQueuePriority), W12
	MOV	#lo_addr(NMRAnetDCC_TrackQueue), W11
	MOV	#lo_addr(_Track), W10
	MOV	#16, W0
	PUSH	W0
	CALL	NMRAnetDCC_InitializeBuffer
	SUB	#2, W15
;NMRAnetDCC.mpas,217 :: 		end;
L_end_NMRA_DCC_Initialize:
	POP	W13
	POP	W12
	POP	W11
	POP	W10
	RETURN
; end of _NMRA_DCC_Initialize

NMRAnetDCC_InitializeBuffer:
	LNK	#0

;NMRAnetDCC.mpas,240 :: 		begin
;NMRAnetDCC.mpas,241 :: 		NMRA_DCC_ResetTransmitter(ABuffer);
; QueuePrioritySize start address is: 8 (W4)
	MOV.B	[W14-8], W4
	CALL	NMRAnetDCC_NMRA_DCC_ResetTransmitter
;NMRAnetDCC.mpas,243 :: 		ABuffer^.Main.Slots := SlotQueue;
	MOV	W11, [W10]
;NMRAnetDCC.mpas,244 :: 		ABuffer^.Main.Count := 0;
	ADD	W10, #2, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,245 :: 		ABuffer^.Main.MaxCount := QueueSize;
	ADD	W10, #3, W0
	MOV.B	W13, [W0]
;NMRAnetDCC.mpas,246 :: 		ABuffer^.Main.Head := 0;
	ADD	W10, #4, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,247 :: 		ABuffer^.Main.Tail := 0;
	ADD	W10, #5, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,248 :: 		ABuffer^.Main.Peak := 0;
	ADD	W10, #6, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,249 :: 		ABuffer^.Main.TotalSent := 0;
	ADD	W10, #8, W1
	CLR	W0
	MOV	W0, [W1]
;NMRAnetDCC.mpas,250 :: 		for i := 0 to ABuffer^.Main.MaxCount - 1 do
; i start address is: 10 (W5)
	CLR	W5
; QueuePrioritySize end address is: 8 (W4)
; i end address is: 10 (W5)
L_NMRAnetDCC_InitializeBuffer2:
; i start address is: 10 (W5)
; QueuePrioritySize start address is: 8 (W4)
	ADD	W10, #3, W0
	ZE	[W0], W0
	SUB	W0, #1, W3
	CP	W5, W3
	BRA LE	L_NMRAnetDCC_InitializeBuffer224
	GOTO	L_NMRAnetDCC_InitializeBuffer6
L_NMRAnetDCC_InitializeBuffer224:
;NMRAnetDCC.mpas,252 :: 		ABuffer^.Main.Slots^[i].Flags := MASK_DCC_PACKET_INITIALIZE_FLAGS;
	MOV	[W10], W0
	MOV	W0, W2
	MOV	#6, W0
	MUL.UU	W0, W5, W0
	ADD	W2, W0, W0
	ADD	W0, #5, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,253 :: 		end;
	CP	W5, W3
	BRA NZ	L_NMRAnetDCC_InitializeBuffer225
	GOTO	L_NMRAnetDCC_InitializeBuffer6
L_NMRAnetDCC_InitializeBuffer225:
; i start address is: 10 (W5)
	INC	W5
; i end address is: 10 (W5)
; i end address is: 10 (W5)
	GOTO	L_NMRAnetDCC_InitializeBuffer2
L_NMRAnetDCC_InitializeBuffer6:
;NMRAnetDCC.mpas,256 :: 		ABuffer^.Priority.Slots := PrioritySlotQueue;
	ADD	W10, #10, W0
	MOV	W12, [W0]
;NMRAnetDCC.mpas,257 :: 		ABuffer^.Priority.Count := 0;
	ADD	W10, #10, W0
	ADD	W0, #2, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,258 :: 		ABuffer^.Priority.MaxCount := QueuePrioritySize;
	ADD	W10, #10, W0
	ADD	W0, #3, W0
	MOV.B	W4, [W0]
; QueuePrioritySize end address is: 8 (W4)
;NMRAnetDCC.mpas,259 :: 		ABuffer^.Priority.Head := 0;
	ADD	W10, #10, W0
	ADD	W0, #4, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,260 :: 		ABuffer^.Priority.Tail := 0;
	ADD	W10, #10, W0
	ADD	W0, #5, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,261 :: 		ABuffer^.Priority.Peak := 0;
	ADD	W10, #10, W0
	ADD	W0, #6, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,262 :: 		ABuffer^.Priority.TotalSent := 0;
	ADD	W10, #10, W0
	ADD	W0, #8, W1
	CLR	W0
	MOV	W0, [W1]
;NMRAnetDCC.mpas,263 :: 		for i := 0 to ABuffer^.Priority.MaxCount - 1 do
; i start address is: 8 (W4)
	CLR	W4
; i end address is: 8 (W4)
L_NMRAnetDCC_InitializeBuffer7:
; i start address is: 8 (W4)
	ADD	W10, #10, W0
	ADD	W0, #3, W0
	ZE	[W0], W0
	SUB	W0, #1, W3
	CP	W4, W3
	BRA LE	L_NMRAnetDCC_InitializeBuffer226
	GOTO	L_NMRAnetDCC_InitializeBuffer11
L_NMRAnetDCC_InitializeBuffer226:
;NMRAnetDCC.mpas,264 :: 		ABuffer^.Priority.Slots^[i].Flags := MASK_DCC_PACKET_INITIALIZE_FLAGS;
	ADD	W10, #10, W0
	MOV	[W0], W0
	MOV	W0, W2
	MOV	#6, W0
	MUL.UU	W0, W4, W0
	ADD	W2, W0, W0
	ADD	W0, #5, W1
	CLR	W0
	MOV.B	W0, [W1]
	CP	W4, W3
	BRA NZ	L_NMRAnetDCC_InitializeBuffer227
	GOTO	L_NMRAnetDCC_InitializeBuffer11
L_NMRAnetDCC_InitializeBuffer227:
; i start address is: 8 (W4)
	INC	W4
; i end address is: 8 (W4)
; i end address is: 8 (W4)
	GOTO	L_NMRAnetDCC_InitializeBuffer7
L_NMRAnetDCC_InitializeBuffer11:
;NMRAnetDCC.mpas,265 :: 		end;
L_end_InitializeBuffer:
	ULNK
	RETURN
; end of NMRAnetDCC_InitializeBuffer

_NMRA_DCC_Packet_Init:
	LNK	#6

;NMRAnetDCC.mpas,275 :: 		begin
;NMRAnetDCC.mpas,276 :: 		NMRA_DCC_LoadPacket(@NewDCCMessage, MESSAGE_IDLE_0, MESSAGE_IDLE_1, 0, 0, 0, 2);
	PUSH	W10
	PUSH	W11
	PUSH	W12
	PUSH	W13
	ADD	W14, #0, W0
	CLR	W13
	CLR	W12
	MOV.B	#255, W11
	MOV	W0, W10
	MOV	#2, W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	CALL	_NMRA_DCC_LoadPacket
	SUB	#6, W15
;NMRAnetDCC.mpas,278 :: 		for i := 0 to MAX_NMRA_DCC_STARTUP_IDLE - 1 do
; i start address is: 0 (W0)
	CLR	W0
; i end address is: 0 (W0)
	MOV	W0, W8
L__NMRA_DCC_Packet_Init14:
;NMRAnetDCC.mpas,279 :: 		while not NMRA_DCC_QueuePacket(@Track, @NewDCCMessage, True) do;
; i start address is: 16 (W8)
; i start address is: 16 (W8)
; i end address is: 16 (W8)
	GOTO	L__NMRA_DCC_Packet_Init19
L__NMRA_DCC_Packet_Init216:
L__NMRA_DCC_Packet_Init19:
; i start address is: 16 (W8)
; i end address is: 16 (W8)
	ADD	W14, #0, W0
	MOV	#65535, W12
	MOV	W0, W11
	MOV	#lo_addr(_Track), W10
	CALL	_NMRA_DCC_QueuePacket
	COM	W0
	CP0	W0
	BRA Z	L__NMRA_DCC_Packet_Init229
	GOTO	L__NMRA_DCC_Packet_Init216
L__NMRA_DCC_Packet_Init229:
; i end address is: 16 (W8)
; i start address is: 16 (W8)
	CP	W8, #19
	BRA NZ	L__NMRA_DCC_Packet_Init230
	GOTO	L__NMRA_DCC_Packet_Init17
L__NMRA_DCC_Packet_Init230:
; i start address is: 16 (W8)
	INC	W8
; i end address is: 16 (W8)
; i end address is: 16 (W8)
	GOTO	L__NMRA_DCC_Packet_Init14
L__NMRA_DCC_Packet_Init17:
;NMRAnetDCC.mpas,280 :: 		end;
L_end_NMRA_DCC_Packet_Init:
	POP	W13
	POP	W12
	POP	W11
	POP	W10
	ULNK
	RETURN
; end of _NMRA_DCC_Packet_Init

_NMRA_DCC_56us_TimeTick:

;NMRAnetDCC.mpas,300 :: 		begin
;NMRAnetDCC.mpas,301 :: 		if ABuffer^.TX_TimerCount > 0 then
	MOV	#32, W0
	ADD	W10, W0, W0
	MOV	[W0], W0
	CP	W0, #0
	BRA GTU	L__NMRA_DCC_56us_TimeTick232
	GOTO	L__NMRA_DCC_56us_TimeTick25
L__NMRA_DCC_56us_TimeTick232:
;NMRAnetDCC.mpas,302 :: 		Dec(ABuffer^.TX_TimerCount);
	MOV	#32, W0
	ADD	W10, W0, W1
	MOV	[W1], W0
	DEC	W0
	MOV	W0, [W1]
L__NMRA_DCC_56us_TimeTick25:
;NMRAnetDCC.mpas,303 :: 		end;
L_end_NMRA_DCC_56us_TimeTick:
	RETURN
; end of _NMRA_DCC_56us_TimeTick

NMRAnetDCC_NMRA_DCC_ResetTransmitter:

;NMRAnetDCC.mpas,319 :: 		begin
;NMRAnetDCC.mpas,321 :: 		ABuffer^.TX_iStateMachine := STATE_NMRA_DCC_PREAMBLE;
	ADD	W10, #27, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,322 :: 		ABuffer^.TX_iBit := STATE_NMRA_DCC_BIT_7;
	ADD	W10, #28, W1
	MOV.B	#7, W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,323 :: 		ABuffer^.TX_iDCC_Pin_StateMachine := STATE_NMRA_DCC_PIN_0;
	ADD	W10, #29, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,325 :: 		ABuffer^.TX_PreambleBitCount := $0;
	ADD	W10, #26, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,326 :: 		ABuffer^.TX_Flags := 0;
	MOV	#35, W0
	ADD	W10, W0, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,327 :: 		ABuffer^.TX_LastAddress := MESSAGE_IDLE_0;
	MOV	#34, W0
	ADD	W10, W0, W1
	MOV.B	#255, W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,328 :: 		end;
L_end_NMRA_DCC_ResetTransmitter:
	RETURN
; end of NMRAnetDCC_NMRA_DCC_ResetTransmitter

NMRAnetDCC_NMRA_DCCClassifyAddress:

;NMRAnetDCC.mpas,342 :: 		begin
;NMRAnetDCC.mpas,344 :: 		AMessage^.Flags := AMessage^.Flags and not MASK_DCC_PACKET_ADDRESS_BITS;
	ADD	W10, #5, W1
	ZE	[W1], W0
	AND	W0, #7, W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,346 :: 		if (AMessage^.PacketBytes[0] = MESSAGE_RESET_0) or (AMessage^.PacketBytes[0] = MESSAGE_IDLE_0) or (AMessage^.PacketBytes[0] = $FE) then
	MOV.B	[W10], W2
	CP.B	W2, #0
	CLR	W1
	BRA NZ	L_NMRAnetDCC_NMRA_DCCClassifyAddress235
	COM	W1
L_NMRAnetDCC_NMRA_DCCClassifyAddress235:
	MOV.B	#255, W0
	CP.B	W2, W0
	CLR	W0
	BRA NZ	L_NMRAnetDCC_NMRA_DCCClassifyAddress236
	COM	W0
L_NMRAnetDCC_NMRA_DCCClassifyAddress236:
	IOR	W1, W0, W1
	MOV.B	#254, W0
	CP.B	W2, W0
	CLR	W0
	BRA NZ	L_NMRAnetDCC_NMRA_DCCClassifyAddress237
	COM	W0
L_NMRAnetDCC_NMRA_DCCClassifyAddress237:
	IOR	W1, W0, W0
	BRA NZ	L_NMRAnetDCC_NMRA_DCCClassifyAddress238
	GOTO	L_NMRAnetDCC_NMRA_DCCClassifyAddress30
L_NMRAnetDCC_NMRA_DCCClassifyAddress238:
;NMRAnetDCC.mpas,347 :: 		AMessage^.Flags := AMessage^.Flags or MASK_DCC_PACKET_SPECIAL
	ADD	W10, #5, W2
	ZE	[W2], W1
	MOV	#128, W0
	IOR	W1, W0, W0
	MOV.B	W0, [W2]
	GOTO	L_NMRAnetDCC_NMRA_DCCClassifyAddress31
;NMRAnetDCC.mpas,348 :: 		else
L_NMRAnetDCC_NMRA_DCCClassifyAddress30:
;NMRAnetDCC.mpas,349 :: 		if AMessage^.PacketBytes[0] < 128 then
	MOV.B	[W10], W1
	MOV.B	#128, W0
	CP.B	W1, W0
	BRA LTU	L_NMRAnetDCC_NMRA_DCCClassifyAddress239
	GOTO	L_NMRAnetDCC_NMRA_DCCClassifyAddress33
L_NMRAnetDCC_NMRA_DCCClassifyAddress239:
;NMRAnetDCC.mpas,350 :: 		AMessage^.Flags := AMessage^.Flags or MASK_DCC_PACKET_SHORT_MULTI_FUNCTION_ADDRESS
	ADD	W10, #5, W1
	ZE	[W1], W0
	IOR	W0, #8, W0
	MOV.B	W0, [W1]
	GOTO	L_NMRAnetDCC_NMRA_DCCClassifyAddress34
;NMRAnetDCC.mpas,351 :: 		else
L_NMRAnetDCC_NMRA_DCCClassifyAddress33:
;NMRAnetDCC.mpas,352 :: 		if AMessage^.PacketBytes[0] < 192 then
	MOV.B	[W10], W1
	MOV.B	#192, W0
	CP.B	W1, W0
	BRA LTU	L_NMRAnetDCC_NMRA_DCCClassifyAddress240
	GOTO	L_NMRAnetDCC_NMRA_DCCClassifyAddress36
L_NMRAnetDCC_NMRA_DCCClassifyAddress240:
;NMRAnetDCC.mpas,353 :: 		AMessage^.Flags := AMessage^.Flags or MASK_DCC_PACKET_ACCESSORY_ADDRESS
	ADD	W10, #5, W1
	ZE	[W1], W0
	IOR	W0, #16, W0
	MOV.B	W0, [W1]
	GOTO	L_NMRAnetDCC_NMRA_DCCClassifyAddress37
;NMRAnetDCC.mpas,354 :: 		else
L_NMRAnetDCC_NMRA_DCCClassifyAddress36:
;NMRAnetDCC.mpas,355 :: 		if AMessage^.PacketBytes[0] < 232 then
	MOV.B	[W10], W1
	MOV.B	#232, W0
	CP.B	W1, W0
	BRA LTU	L_NMRAnetDCC_NMRA_DCCClassifyAddress241
	GOTO	L_NMRAnetDCC_NMRA_DCCClassifyAddress39
L_NMRAnetDCC_NMRA_DCCClassifyAddress241:
;NMRAnetDCC.mpas,356 :: 		AMessage^.Flags := AMessage^.Flags or MASK_DCC_PACKET_LONG_MULTI_FUNCTION_ADDRESS
	ADD	W10, #5, W2
	ZE	[W2], W1
	MOV	#32, W0
	IOR	W1, W0, W0
	MOV.B	W0, [W2]
	GOTO	L_NMRAnetDCC_NMRA_DCCClassifyAddress40
;NMRAnetDCC.mpas,357 :: 		else
L_NMRAnetDCC_NMRA_DCCClassifyAddress39:
;NMRAnetDCC.mpas,358 :: 		AMessage^.Flags := AMessage^.Flags or MASK_DCC_PACKET_RESERVED
	ADD	W10, #5, W2
	ZE	[W2], W1
	MOV	#64, W0
	IOR	W1, W0, W0
	MOV.B	W0, [W2]
L_NMRAnetDCC_NMRA_DCCClassifyAddress40:
L_NMRAnetDCC_NMRA_DCCClassifyAddress37:
L_NMRAnetDCC_NMRA_DCCClassifyAddress34:
L_NMRAnetDCC_NMRA_DCCClassifyAddress31:
;NMRAnetDCC.mpas,359 :: 		end;
L_end_NMRA_DCCClassifyAddress:
	RETURN
; end of NMRAnetDCC_NMRA_DCCClassifyAddress

_NMRA_DCC_LoadPacket:
	LNK	#0

;NMRAnetDCC.mpas,380 :: 		begin
;NMRAnetDCC.mpas,381 :: 		NewMessage^.PacketBytes[0] := Data1;
; Data4 start address is: 2 (W1)
	MOV.B	[W14-8], W1
; Data5 start address is: 4 (W2)
	MOV.B	[W14-10], W2
; ValidDataByes start address is: 6 (W3)
	MOV.B	[W14-12], W3
	MOV.B	W11, [W10]
;NMRAnetDCC.mpas,382 :: 		NewMessage^.PacketBytes[1] := Data2;
	ADD	W10, #1, W0
	MOV.B	W12, [W0]
;NMRAnetDCC.mpas,383 :: 		NewMessage^.PacketBytes[2] := Data3;
	ADD	W10, #2, W0
	MOV.B	W13, [W0]
;NMRAnetDCC.mpas,384 :: 		NewMessage^.PacketBytes[3] := Data4;
	ADD	W10, #3, W0
	MOV.B	W1, [W0]
; Data4 end address is: 2 (W1)
;NMRAnetDCC.mpas,385 :: 		NewMessage^.PacketBytes[4] := Data5;
	ADD	W10, #4, W0
	MOV.B	W2, [W0]
; Data5 end address is: 4 (W2)
;NMRAnetDCC.mpas,386 :: 		NewMessage^.Flags := ValidDataByes;
	ADD	W10, #5, W0
	MOV.B	W3, [W0]
; ValidDataByes end address is: 6 (W3)
;NMRAnetDCC.mpas,387 :: 		end;
L_end_NMRA_DCC_LoadPacket:
	ULNK
	RETURN
; end of _NMRA_DCC_LoadPacket

_NMRA_DCC_QueuePacket:

;NMRAnetDCC.mpas,419 :: 		begin
;NMRAnetDCC.mpas,420 :: 		Result := False;
	PUSH	W11
	PUSH	W12
	PUSH	W13
; Result start address is: 4 (W2)
	CLR	W2
;NMRAnetDCC.mpas,422 :: 		if HighPriority then
	CP0	W12
	BRA NZ	L__NMRA_DCC_QueuePacket244
	GOTO	L__NMRA_DCC_QueuePacket44
L__NMRA_DCC_QueuePacket244:
;NMRAnetDCC.mpas,424 :: 		if ABuffer^.Priority.Count < ABuffer^.Priority.MaxCount then
	ADD	W10, #10, W1
	ADD	W1, #2, W0
	ADD	W1, #3, W1
	MOV.B	[W0], W0
	CP.B	W0, [W1]
	BRA LTU	L__NMRA_DCC_QueuePacket245
	GOTO	L__NMRA_DCC_QueuePacket213
L__NMRA_DCC_QueuePacket245:
; Result end address is: 4 (W2)
;NMRAnetDCC.mpas,426 :: 		iNextHead := ABuffer^.Priority.Head;
	ADD	W10, #10, W3
	ADD	W3, #4, W0
; iNextHead start address is: 8 (W4)
	ZE	[W0], W4
;NMRAnetDCC.mpas,427 :: 		QueueTarget := @ABuffer^.Priority.Slots^[iNextHead];
	MOV	[W3], W0
	MOV	W0, W2
	MOV	#6, W0
	MUL.UU	W0, W4, W0
; QueueTarget start address is: 12 (W6)
	ADD	W2, W0, W6
;NMRAnetDCC.mpas,428 :: 		Inc(iNextHead);
	ADD	W4, #1, W1
; iNextHead end address is: 8 (W4)
; iNextHead start address is: 14 (W7)
	MOV	W1, W7
;NMRAnetDCC.mpas,429 :: 		if iNextHead >= ABuffer^.Priority.MaxCount then
	ADD	W3, #3, W0
	MOV.B	[W0], W0
	ZE	W0, W0
	CP	W1, W0
	BRA GE	L__NMRA_DCC_QueuePacket246
	GOTO	L__NMRA_DCC_QueuePacket212
L__NMRA_DCC_QueuePacket246:
;NMRAnetDCC.mpas,430 :: 		iNextHead := 0;
	CLR	W7
; iNextHead end address is: 14 (W7)
	GOTO	L__NMRA_DCC_QueuePacket50
L__NMRA_DCC_QueuePacket212:
;NMRAnetDCC.mpas,429 :: 		if iNextHead >= ABuffer^.Priority.MaxCount then
;NMRAnetDCC.mpas,430 :: 		iNextHead := 0;
L__NMRA_DCC_QueuePacket50:
;NMRAnetDCC.mpas,432 :: 		NMRA_DCC_LoadPacket(QueueTarget, PacketBytes[0], PacketBytes[1], PacketBytes[2], PacketBytes[3], PacketBytes[4], NewMessage^.Flags and MASK_DCC_PACKET_COUNT);
; iNextHead start address is: 14 (W7)
	ADD	W11, #1, W5
	ADD	W11, #2, W4
	ADD	W11, #3, W3
	ADD	W11, #4, W1
	ADD	W11, #5, W0
	ZE	[W0], W0
	AND	W0, #7, W2
	MOV.B	[W1], W0
	MOV.B	[W3], W1
	PUSH	W10
	MOV.B	[W4], W13
	MOV.B	[W5], W12
	MOV.B	[W11], W11
	MOV	W6, W10
	PUSH	W2
	ZE	W0, W0
	PUSH	W0
	ZE	W1, W0
	PUSH	W0
	CALL	_NMRA_DCC_LoadPacket
	SUB	#6, W15
;NMRAnetDCC.mpas,433 :: 		NMRA_DCCClassifyAddress(QueueTarget);
; QueueTarget end address is: 12 (W6)
	MOV	W6, W10
	CALL	NMRAnetDCC_NMRA_DCCClassifyAddress
	POP	W10
;NMRAnetDCC.mpas,434 :: 		ABuffer^.Priority.Head := iNextHead;
	ADD	W10, #10, W0
	ADD	W0, #4, W0
	MOV.B	W7, [W0]
; iNextHead end address is: 14 (W7)
;NMRAnetDCC.mpas,435 :: 		if ABuffer^.Priority.Count + 1 > ABuffer^.Priority.Peak then   // Need to do this before actually increasing the count, see the next comment
	ADD	W10, #10, W2
	ADD	W2, #2, W0
	ZE	[W0], W0
	ADD	W0, #1, W1
	ADD	W2, #6, W0
	MOV.B	[W0], W0
	ZE	W0, W0
	CP	W1, W0
	BRA GTU	L__NMRA_DCC_QueuePacket247
	GOTO	L__NMRA_DCC_QueuePacket53
L__NMRA_DCC_QueuePacket247:
;NMRAnetDCC.mpas,436 :: 		ABuffer^.Priority.Peak := ABuffer^.Priority.Count + 1;
	ADD	W10, #10, W0
	ADD	W0, #6, W1
	INC2	W0
	ZE	[W0], W0
	INC	W0
	MOV.B	W0, [W1]
L__NMRA_DCC_QueuePacket53:
;NMRAnetDCC.mpas,437 :: 		Inc(ABuffer^.Priority.Count);                                   // Everything must be valid by this point as the interrupt can cut in after this and transfer this immediately to the transmitter
	ADD	W10, #10, W0
	ADD	W0, #2, W1
	ZE	[W1], W0
	INC	W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,438 :: 		Result := True;
; Result start address is: 0 (W0)
	MOV	#65535, W0
; Result end address is: 0 (W0)
	MOV	W0, W1
;NMRAnetDCC.mpas,439 :: 		end
	GOTO	L__NMRA_DCC_QueuePacket47
L__NMRA_DCC_QueuePacket213:
;NMRAnetDCC.mpas,424 :: 		if ABuffer^.Priority.Count < ABuffer^.Priority.MaxCount then
	MOV	W2, W1
;NMRAnetDCC.mpas,439 :: 		end
L__NMRA_DCC_QueuePacket47:
;NMRAnetDCC.mpas,440 :: 		end else
; Result start address is: 2 (W1)
; Result end address is: 2 (W1)
	GOTO	L__NMRA_DCC_QueuePacket45
L__NMRA_DCC_QueuePacket44:
;NMRAnetDCC.mpas,442 :: 		if ABuffer^.Main.Count < ABuffer^.Main.MaxCount then
; Result start address is: 4 (W2)
	ADD	W10, #2, W0
	ADD	W10, #3, W1
	MOV.B	[W0], W0
	CP.B	W0, [W1]
	BRA LTU	L__NMRA_DCC_QueuePacket248
	GOTO	L__NMRA_DCC_QueuePacket215
L__NMRA_DCC_QueuePacket248:
; Result end address is: 4 (W2)
;NMRAnetDCC.mpas,444 :: 		iNextHead := ABuffer^.Main.Head;
	ADD	W10, #4, W0
; iNextHead start address is: 6 (W3)
	ZE	[W0], W3
;NMRAnetDCC.mpas,445 :: 		QueueTarget := @ABuffer^.Main.Slots^[iNextHead];
	MOV	[W10], W0
	MOV	W0, W2
	MOV	#6, W0
	MUL.UU	W0, W3, W0
; QueueTarget start address is: 12 (W6)
	ADD	W2, W0, W6
;NMRAnetDCC.mpas,446 :: 		Inc(iNextHead);
	ADD	W3, #1, W1
; iNextHead end address is: 6 (W3)
; iNextHead start address is: 14 (W7)
	MOV	W1, W7
;NMRAnetDCC.mpas,447 :: 		if iNextHead >= ABuffer^.Main.MaxCount then
	ADD	W10, #3, W0
	MOV.B	[W0], W0
	ZE	W0, W0
	CP	W1, W0
	BRA GE	L__NMRA_DCC_QueuePacket249
	GOTO	L__NMRA_DCC_QueuePacket214
L__NMRA_DCC_QueuePacket249:
;NMRAnetDCC.mpas,448 :: 		iNextHead := 0;
	CLR	W7
; iNextHead end address is: 14 (W7)
	GOTO	L__NMRA_DCC_QueuePacket59
L__NMRA_DCC_QueuePacket214:
;NMRAnetDCC.mpas,447 :: 		if iNextHead >= ABuffer^.Main.MaxCount then
;NMRAnetDCC.mpas,448 :: 		iNextHead := 0;
L__NMRA_DCC_QueuePacket59:
;NMRAnetDCC.mpas,450 :: 		NMRA_DCC_LoadPacket(QueueTarget, PacketBytes[0], PacketBytes[1], PacketBytes[2], PacketBytes[3], PacketBytes[4], NewMessage^.Flags and MASK_DCC_PACKET_COUNT);
; iNextHead start address is: 14 (W7)
	ADD	W11, #1, W5
	ADD	W11, #2, W4
	ADD	W11, #3, W3
	ADD	W11, #4, W1
	ADD	W11, #5, W0
	ZE	[W0], W0
	AND	W0, #7, W2
	MOV.B	[W1], W0
	MOV.B	[W3], W1
	PUSH	W10
	MOV.B	[W4], W13
	MOV.B	[W5], W12
	MOV.B	[W11], W11
	MOV	W6, W10
	PUSH	W2
	ZE	W0, W0
	PUSH	W0
	ZE	W1, W0
	PUSH	W0
	CALL	_NMRA_DCC_LoadPacket
	SUB	#6, W15
;NMRAnetDCC.mpas,451 :: 		NMRA_DCCClassifyAddress(QueueTarget);
; QueueTarget end address is: 12 (W6)
	MOV	W6, W10
	CALL	NMRAnetDCC_NMRA_DCCClassifyAddress
	POP	W10
;NMRAnetDCC.mpas,452 :: 		ABuffer^.Main.Head := iNextHead;
	ADD	W10, #4, W0
	MOV.B	W7, [W0]
; iNextHead end address is: 14 (W7)
;NMRAnetDCC.mpas,453 :: 		if ABuffer^.Main.Count + 1 > ABuffer^.Main.Peak then  // Need to do this before actually increasing the count, see the next comment
	ADD	W10, #2, W0
	ZE	[W0], W0
	ADD	W0, #1, W1
	ADD	W10, #6, W0
	MOV.B	[W0], W0
	ZE	W0, W0
	CP	W1, W0
	BRA GTU	L__NMRA_DCC_QueuePacket250
	GOTO	L__NMRA_DCC_QueuePacket62
L__NMRA_DCC_QueuePacket250:
;NMRAnetDCC.mpas,454 :: 		ABuffer^.Main.Peak := ABuffer^.Main.Count + 1;
	ADD	W10, #6, W1
	ADD	W10, #2, W0
	ZE	[W0], W0
	INC	W0
	MOV.B	W0, [W1]
L__NMRA_DCC_QueuePacket62:
;NMRAnetDCC.mpas,455 :: 		Inc(ABuffer^.Main.Count);                              // Everything must be valid by this point as the interrupt can cut in after this and transfer this immediately to the transmitter
	ADD	W10, #2, W1
	ZE	[W1], W0
	INC	W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,456 :: 		Result := True;
; Result start address is: 0 (W0)
	MOV	#65535, W0
; Result end address is: 0 (W0)
	MOV	W0, W1
;NMRAnetDCC.mpas,457 :: 		end;
	GOTO	L__NMRA_DCC_QueuePacket56
L__NMRA_DCC_QueuePacket215:
;NMRAnetDCC.mpas,442 :: 		if ABuffer^.Main.Count < ABuffer^.Main.MaxCount then
	MOV	W2, W1
;NMRAnetDCC.mpas,457 :: 		end;
L__NMRA_DCC_QueuePacket56:
;NMRAnetDCC.mpas,458 :: 		end
; Result start address is: 2 (W1)
; Result end address is: 2 (W1)
L__NMRA_DCC_QueuePacket45:
;NMRAnetDCC.mpas,459 :: 		end;
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRA_DCC_QueuePacket:
	POP	W13
	POP	W12
	POP	W11
	RETURN
; end of _NMRA_DCC_QueuePacket

NMRAnetDCC_NMRA_DCC_LoadIdlePacketIntoTransmitter:

;NMRAnetDCC.mpas,475 :: 		begin
;NMRAnetDCC.mpas,476 :: 		Buffer^.TX_TransmittingPacket.PacketBytes[0] := MESSAGE_IDLE_0;
	ADD	W10, #20, W1
	MOV.B	#255, W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,477 :: 		Buffer^.TX_TransmittingPacket.PacketBytes[1] := MESSAGE_IDLE_1;
	ADD	W10, #20, W0
	ADD	W0, #1, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,478 :: 		Buffer^.TX_XOR_Byte := MESSAGE_IDLE_XOR;
	ADD	W10, #30, W1
	MOV.B	#255, W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,479 :: 		Buffer^.TX_TransmittingPacket.Flags := MASK_DCC_PACKET_IDLE_MESSAGE;      // 2 Bytes and Address Is Special
	ADD	W10, #20, W0
	ADD	W0, #5, W1
	MOV.B	#130, W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,480 :: 		Buffer^.TX_PreambleBitCount := PreambleCount;
	ADD	W10, #26, W0
	MOV.B	W11, [W0]
;NMRAnetDCC.mpas,481 :: 		end;
L_end_NMRA_DCC_LoadIdlePacketIntoTransmitter:
	RETURN
; end of NMRAnetDCC_NMRA_DCC_LoadIdlePacketIntoTransmitter

NMRAnetDCC_NMRA_DCC_LoadResetPacketIntoTransmitter:

;NMRAnetDCC.mpas,497 :: 		begin
;NMRAnetDCC.mpas,498 :: 		Buffer^.TX_TransmittingPacket.PacketBytes[0] := MESSAGE_RESET_0;
	ADD	W10, #20, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,499 :: 		Buffer^.TX_TransmittingPacket.PacketBytes[1] := MESSAGE_RESET_1;
	ADD	W10, #20, W0
	ADD	W0, #1, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,500 :: 		Buffer^.TX_XOR_Byte := MESSAGE_RESET_XOR;
	ADD	W10, #30, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,501 :: 		Buffer^.TX_TransmittingPacket.Flags := MASK_DCC_PACKET_RESET_MESSAGE;
	ADD	W10, #20, W0
	ADD	W0, #5, W1
	MOV.B	#130, W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,502 :: 		Buffer^.TX_PreambleBitCount := PreambleCount;
	ADD	W10, #26, W0
	MOV.B	W11, [W0]
;NMRAnetDCC.mpas,503 :: 		end;
L_end_NMRA_DCC_LoadResetPacketIntoTransmitter:
	RETURN
; end of NMRAnetDCC_NMRA_DCC_LoadResetPacketIntoTransmitter

NMRAnetDCC_CanTransmitMessage:

;NMRAnetDCC.mpas,523 :: 		begin
;NMRAnetDCC.mpas,525 :: 		if AMessage^.Flags and MASK_DCC_PACKET_SHORT_MULTI_FUNCTION_ADDRESS <> 0 then
	ADD	W11, #5, W0
	ZE	[W0], W0
	AND	W0, #8, W0
	CP	W0, #0
	BRA NZ	L_NMRAnetDCC_CanTransmitMessage254
	GOTO	L_NMRAnetDCC_CanTransmitMessage68
L_NMRAnetDCC_CanTransmitMessage254:
;NMRAnetDCC.mpas,527 :: 		if AMessage^.PacketBytes[0] = ABuffer^.TX_LastAddress then
	MOV	#34, W0
	ADD	W10, W0, W1
	MOV.B	[W11], W0
	CP.B	W0, [W1]
	BRA Z	L_NMRAnetDCC_CanTransmitMessage255
	GOTO	L_NMRAnetDCC_CanTransmitMessage71
L_NMRAnetDCC_CanTransmitMessage255:
;NMRAnetDCC.mpas,529 :: 		if AMessage^.PacketBytes[0] >= 112 then
	MOV.B	[W11], W1
	MOV.B	#112, W0
	CP.B	W1, W0
	BRA GEU	L_NMRAnetDCC_CanTransmitMessage256
	GOTO	L_NMRAnetDCC_CanTransmitMessage74
L_NMRAnetDCC_CanTransmitMessage256:
;NMRAnetDCC.mpas,530 :: 		if AMessage^.PacketBytes[0] <= 127 then
	MOV.B	[W11], W1
	MOV.B	#127, W0
	CP.B	W1, W0
	BRA LEU	L_NMRAnetDCC_CanTransmitMessage257
	GOTO	L_NMRAnetDCC_CanTransmitMessage77
L_NMRAnetDCC_CanTransmitMessage257:
;NMRAnetDCC.mpas,535 :: 		if ABuffer^.TX_TimerCount < _5ms_PERIOD then
	MOV	#32, W0
	ADD	W10, W0, W0
	MOV	[W0], W1
	MOV	#90, W0
	CP	W1, W0
	BRA LTU	L_NMRAnetDCC_CanTransmitMessage258
	GOTO	L_NMRAnetDCC_CanTransmitMessage80
L_NMRAnetDCC_CanTransmitMessage258:
;NMRAnetDCC.mpas,536 :: 		ABuffer^.TX_TimerCount := _5ms_PERIOD;          // 5ms/56us = 89.3
	MOV	#32, W0
	ADD	W10, W0, W1
	MOV	#90, W0
	MOV	W0, [W1]
L_NMRAnetDCC_CanTransmitMessage80:
;NMRAnetDCC.mpas,537 :: 		end
L_NMRAnetDCC_CanTransmitMessage77:
L_NMRAnetDCC_CanTransmitMessage74:
;NMRAnetDCC.mpas,538 :: 		end
L_NMRAnetDCC_CanTransmitMessage71:
;NMRAnetDCC.mpas,539 :: 		end;
L_NMRAnetDCC_CanTransmitMessage68:
;NMRAnetDCC.mpas,541 :: 		if ABuffer^.TX_LastAddress = MESSAGE_RESET_0 then
	MOV	#34, W0
	ADD	W10, W0, W0
	MOV.B	[W0], W0
	CP.B	W0, #0
	BRA Z	L_NMRAnetDCC_CanTransmitMessage259
	GOTO	L_NMRAnetDCC_CanTransmitMessage83
L_NMRAnetDCC_CanTransmitMessage259:
;NMRAnetDCC.mpas,543 :: 		if (AMessage^.PacketBytes[0] >= 100) then
	MOV.B	[W11], W1
	MOV.B	#100, W0
	CP.B	W1, W0
	BRA GEU	L_NMRAnetDCC_CanTransmitMessage260
	GOTO	L_NMRAnetDCC_CanTransmitMessage86
L_NMRAnetDCC_CanTransmitMessage260:
;NMRAnetDCC.mpas,544 :: 		if AMessage^.PacketBytes[0] <= 127 then
	MOV.B	[W11], W1
	MOV.B	#127, W0
	CP.B	W1, W0
	BRA LEU	L_NMRAnetDCC_CanTransmitMessage261
	GOTO	L_NMRAnetDCC_CanTransmitMessage89
L_NMRAnetDCC_CanTransmitMessage261:
;NMRAnetDCC.mpas,549 :: 		if ABuffer^.TX_TimerCount < _20ms_PERIOD then      // 20ms/56us = 357.1
	MOV	#32, W0
	ADD	W10, W0, W0
	MOV	[W0], W1
	MOV	#360, W0
	CP	W1, W0
	BRA LTU	L_NMRAnetDCC_CanTransmitMessage262
	GOTO	L_NMRAnetDCC_CanTransmitMessage92
L_NMRAnetDCC_CanTransmitMessage262:
;NMRAnetDCC.mpas,550 :: 		ABuffer^.TX_TimerCount := _20ms_PERIOD;
	MOV	#32, W0
	ADD	W10, W0, W1
	MOV	#360, W0
	MOV	W0, [W1]
L_NMRAnetDCC_CanTransmitMessage92:
;NMRAnetDCC.mpas,551 :: 		end;
L_NMRAnetDCC_CanTransmitMessage89:
L_NMRAnetDCC_CanTransmitMessage86:
;NMRAnetDCC.mpas,552 :: 		end;
L_NMRAnetDCC_CanTransmitMessage83:
;NMRAnetDCC.mpas,553 :: 		Result := ABuffer^.TX_TimerCount = 0
	MOV	#32, W0
	ADD	W10, W0, W0
	MOV	[W0], W0
; Result start address is: 2 (W1)
	CP	W0, #0
	CLR	W1
	BRA NZ	L_NMRAnetDCC_CanTransmitMessage263
	COM	W1
L_NMRAnetDCC_CanTransmitMessage263:
;NMRAnetDCC.mpas,554 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_CanTransmitMessage:
	RETURN
; end of NMRAnetDCC_CanTransmitMessage

_NMRA_DCC_LoadPacketIntoTransmitterStateMachine:
	LNK	#2

;NMRAnetDCC.mpas,582 :: 		begin
;NMRAnetDCC.mpas,583 :: 		AQueue := nil;
; AQueue start address is: 8 (W4)
	CLR	W4
;NMRAnetDCC.mpas,584 :: 		if ABuffer^.TX_Flags.TRANSMITTING_FLAG_STOP_BIT = 1 then                      // If previous packet is on the stop bit then time to load the next packet
	MOV	#35, W0
	ADD	W10, W0, W0
	MOV.B	[W0], W0
	BTSS.B	W0, #1
	GOTO	L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine96
;NMRAnetDCC.mpas,586 :: 		if ABuffer^.Priority.Count > 0 then
	ADD	W10, #10, W0
	INC2	W0
	MOV.B	[W0], W0
	CP.B	W0, #0
	BRA GTU	L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine265
	GOTO	L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine218
L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine265:
;NMRAnetDCC.mpas,588 :: 		AMessage := @ABuffer^.Priority.Slots^[ABuffer^.Priority.Tail];
	ADD	W10, #10, W0
	MOV	[W0], W1
	ADD	W0, #5, W0
	ZE	[W0], W3
	MOV	W1, W2
	MOV	#6, W0
	MUL.UU	W0, W3, W0
	ADD	W2, W0, W0
	MOV	W0, [W14+0]
;NMRAnetDCC.mpas,589 :: 		if CanTransmitMessage(ABuffer, AMessage) then
	PUSH	W11
	MOV	W0, W11
	CALL	NMRAnetDCC_CanTransmitMessage
	POP	W11
	CP0	W0
	BRA NZ	L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine266
	GOTO	L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine217
L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine266:
; AQueue end address is: 8 (W4)
;NMRAnetDCC.mpas,590 :: 		AQueue := @ABuffer^.Priority
; AQueue start address is: 0 (W0)
	ADD	W10, #10, W0
	MOV	W0, W4
; AQueue end address is: 0 (W0)
	GOTO	L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine102
L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine217:
;NMRAnetDCC.mpas,589 :: 		if CanTransmitMessage(ABuffer, AMessage) then
;NMRAnetDCC.mpas,590 :: 		AQueue := @ABuffer^.Priority
L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine102:
;NMRAnetDCC.mpas,591 :: 		end;
; AQueue start address is: 8 (W4)
; AQueue end address is: 8 (W4)
	GOTO	L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine99
L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine218:
;NMRAnetDCC.mpas,586 :: 		if ABuffer^.Priority.Count > 0 then
;NMRAnetDCC.mpas,591 :: 		end;
L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine99:
;NMRAnetDCC.mpas,593 :: 		if AQueue = nil then
; AQueue start address is: 8 (W4)
	CP	W4, #0
	BRA Z	L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine267
	GOTO	L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine221
L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine267:
;NMRAnetDCC.mpas,595 :: 		if ABuffer^.Main.Count > 0 then
	ADD	W10, #2, W0
	MOV.B	[W0], W0
	CP.B	W0, #0
	BRA GTU	L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine268
	GOTO	L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine220
L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine268:
;NMRAnetDCC.mpas,597 :: 		AMessage := @ABuffer^.Main.Slots^[ABuffer^.Main.Tail];
	MOV	[W10], W1
	ADD	W10, #5, W0
	ZE	[W0], W3
	MOV	W1, W2
	MOV	#6, W0
	MUL.UU	W0, W3, W0
	ADD	W2, W0, W0
	MOV	W0, [W14+0]
;NMRAnetDCC.mpas,598 :: 		if CanTransmitMessage(ABuffer, AMessage) then
	PUSH	W11
	MOV	W0, W11
	CALL	NMRAnetDCC_CanTransmitMessage
	POP	W11
	CP0	W0
	BRA NZ	L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine269
	GOTO	L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine219
L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine269:
; AQueue end address is: 8 (W4)
;NMRAnetDCC.mpas,599 :: 		AQueue := @ABuffer^.Main
; AQueue start address is: 0 (W0)
	MOV	W10, W0
; AQueue end address is: 0 (W0)
	GOTO	L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine111
L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine219:
;NMRAnetDCC.mpas,598 :: 		if CanTransmitMessage(ABuffer, AMessage) then
	MOV	W4, W0
;NMRAnetDCC.mpas,599 :: 		AQueue := @ABuffer^.Main
L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine111:
;NMRAnetDCC.mpas,600 :: 		end
; AQueue start address is: 0 (W0)
	MOV	W0, W6
; AQueue end address is: 0 (W0)
	GOTO	L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine108
L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine220:
;NMRAnetDCC.mpas,595 :: 		if ABuffer^.Main.Count > 0 then
	MOV	W4, W6
;NMRAnetDCC.mpas,600 :: 		end
L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine108:
;NMRAnetDCC.mpas,601 :: 		end;
; AQueue start address is: 12 (W6)
; AQueue end address is: 12 (W6)
	GOTO	L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine105
L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine221:
;NMRAnetDCC.mpas,593 :: 		if AQueue = nil then
	MOV	W4, W6
;NMRAnetDCC.mpas,601 :: 		end;
L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine105:
;NMRAnetDCC.mpas,604 :: 		if AQueue <> nil then
; AQueue start address is: 12 (W6)
	CP	W6, #0
	BRA NZ	L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine270
	GOTO	L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine114
L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine270:
;NMRAnetDCC.mpas,609 :: 		ABuffer^.TX_TransmittingPacket := AMessage^;                               // Copy the message to the Transmit Buffer
	ADD	W10, #20, W1
	MOV	[W14+0], W0
	REPEAT	#5
	MOV.B	[W0++], [W1++]
;NMRAnetDCC.mpas,610 :: 		ABuffer^.TX_XOR_Byte := 0;                                                  // Build the XOR Byte
	ADD	W10, #30, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,611 :: 		i := 0;
; i start address is: 10 (W5)
	CLR	W5
;NMRAnetDCC.mpas,612 :: 		Count := ABuffer^.TX_TransmittingPacket.Flags and MASK_DCC_PACKET_COUNT;
	ADD	W10, #20, W0
	ADD	W0, #5, W0
	MOV.B	[W0], W0
; Count start address is: 8 (W4)
	AND.B	W0, #7, W4
; Count end address is: 8 (W4)
; i end address is: 10 (W5)
; AQueue end address is: 12 (W6)
;NMRAnetDCC.mpas,613 :: 		while i < Count do
L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine117:
; Count start address is: 8 (W4)
; i start address is: 10 (W5)
; AQueue start address is: 12 (W6)
	CP.B	W5, W4
	BRA LTU	L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine271
	GOTO	L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine118
L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine271:
;NMRAnetDCC.mpas,615 :: 		ABuffer^.TX_XOR_Byte := ABuffer^.TX_XOR_Byte xor ABuffer^.TX_TransmittingPacket.PacketBytes[i];
	ADD	W10, #30, W3
	ADD	W10, #20, W1
	ZE	W5, W0
	ADD	W1, W0, W2
	MOV.B	[W3], W0
	ZE	W0, W1
	ZE	[W2], W0
	XOR	W1, W0, W0
	MOV.B	W0, [W3]
;NMRAnetDCC.mpas,616 :: 		Inc(i)
; i start address is: 0 (W0)
	ADD.B	W5, #1, W0
; i end address is: 10 (W5)
;NMRAnetDCC.mpas,617 :: 		end;
; Count end address is: 8 (W4)
; i end address is: 0 (W0)
	MOV.B	W0, W5
	GOTO	L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine117
L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine118:
;NMRAnetDCC.mpas,618 :: 		ABuffer^.TX_PreambleBitCount := PreambleCount;                              // Update the Preamble Count
	ADD	W10, #26, W0
	MOV.B	W11, [W0]
;NMRAnetDCC.mpas,619 :: 		ABuffer^.TX_LastAddress := ABuffer^.TX_TransmittingPacket.PacketBytes[0];   // Update the Last Address
	MOV	#34, W0
	ADD	W10, W0, W1
	ADD	W10, #20, W0
	MOV.B	[W0], W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,621 :: 		Inc(AQueue^.Tail);
	ADD	W6, #5, W1
	ZE	[W1], W0
	INC	W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,622 :: 		if AQueue^.Tail >= AQueue^.MaxCount then                                    // Remove the message from the Queue
	ADD	W6, #5, W0
	ADD	W6, #3, W1
	MOV.B	[W0], W0
	CP.B	W0, [W1]
	BRA GEU	L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine272
	GOTO	L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine122
L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine272:
;NMRAnetDCC.mpas,623 :: 		AQueue^.Tail := 0;
	ADD	W6, #5, W1
	CLR	W0
	MOV.B	W0, [W1]
L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine122:
;NMRAnetDCC.mpas,624 :: 		Inc(AQueue^.TotalSent);
	ADD	W6, #8, W1
	MOV	[W1], W0
	INC	W0
	MOV	W0, [W1]
;NMRAnetDCC.mpas,625 :: 		Dec(AQueue^.Count);
	ADD	W6, #2, W1
; AQueue end address is: 12 (W6)
	ZE	[W1], W0
	DEC	W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,626 :: 		end else
	GOTO	L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine115
L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine114:
;NMRAnetDCC.mpas,627 :: 		NMRA_DCC_LoadIdlePacketIntoTransmitter(ABuffer, PreambleCount);
	CALL	NMRAnetDCC_NMRA_DCC_LoadIdlePacketIntoTransmitter
L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine115:
;NMRAnetDCC.mpas,628 :: 		end
L__NMRA_DCC_LoadPacketIntoTransmitterStateMachine96:
;NMRAnetDCC.mpas,629 :: 		end;
L_end_NMRA_DCC_LoadPacketIntoTransmitterStateMachine:
	ULNK
	RETURN
; end of _NMRA_DCC_LoadPacketIntoTransmitterStateMachine

NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandlePreamble:

;NMRAnetDCC.mpas,640 :: 		begin
;NMRAnetDCC.mpas,641 :: 		ABuffer^.TX_Flags.TRANSMITTING_FLAG_CURRENT_BIT := 1;
	PUSH	W11
	MOV	#35, W0
	ADD	W10, W0, W0
	BSET.B	[W0], #0
;NMRAnetDCC.mpas,642 :: 		if ABuffer^.TX_PreambleBitCount > 0 then
	ADD	W10, #26, W0
	MOV.B	[W0], W0
	CP.B	W0, #0
	BRA GTU	L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandlePreamble274
	GOTO	L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandlePreamble126
L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandlePreamble274:
;NMRAnetDCC.mpas,644 :: 		Dec(ABuffer^.TX_PreambleBitCount);
	ADD	W10, #26, W1
	ZE	[W1], W0
	DEC	W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,645 :: 		if ABuffer^.TX_PreambleBitCount = 0 then
	ADD	W10, #26, W0
	MOV.B	[W0], W0
	CP.B	W0, #0
	BRA Z	L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandlePreamble275
	GOTO	L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandlePreamble129
L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandlePreamble275:
;NMRAnetDCC.mpas,646 :: 		Inc(ABuffer^.TX_iStateMachine);
	ADD	W10, #27, W1
	ZE	[W1], W0
	INC	W0
	MOV.B	W0, [W1]
L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandlePreamble129:
;NMRAnetDCC.mpas,647 :: 		end else
	GOTO	L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandlePreamble127
L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandlePreamble126:
;NMRAnetDCC.mpas,649 :: 		NMRA_DCC_LoadIdlePacketIntoTransmitter(ABuffer, PREAMBLE_BIT_COUNT_NORMAL);
	MOV.B	#14, W11
	CALL	NMRAnetDCC_NMRA_DCC_LoadIdlePacketIntoTransmitter
;NMRAnetDCC.mpas,650 :: 		ABuffer^.TX_LastAddress := MESSAGE_IDLE_0;
	MOV	#34, W0
	ADD	W10, W0, W1
	MOV.B	#255, W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,651 :: 		end
L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandlePreamble127:
;NMRAnetDCC.mpas,652 :: 		end;
L_end_NMRA_DCC_TransmitterStateMachineHandlePreamble:
	POP	W11
	RETURN
; end of NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandlePreamble

NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleByte:

;NMRAnetDCC.mpas,664 :: 		begin
;NMRAnetDCC.mpas,665 :: 		Offset := ABuffer^.TX_iBit;
	ADD	W10, #28, W0
	MOV.B	[W0], W2
;NMRAnetDCC.mpas,666 :: 		ABuffer^.TX_Flags.TRANSMITTING_FLAG_CURRENT_BIT := ABuffer^.TX_TransmittingPacket.PacketBytes[ByteIndex].Offset;
	MOV	#35, W0
	ADD	W10, W0, W4
	ADD	W10, #20, W1
	ZE	W11, W0
	ADD	W1, W0, W3
	MOV	#1, W0
	ZE	W2, W2
	SL	W0, W2, W0
	ZE	[W3], W1
	ZE	W0, W0
	AND	W1, W0, W0
	CLR	W1
	CP	W0, #0
	BRA NZ	L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleByte277
	GOTO	L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleByte132
L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleByte277:
	MOV.B	#1, W0
	MOV.B	W0, W1
L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleByte132:
	BSET.B	[W4], #0
	BTSS	W1, #0
	BCLR.B	[W4], #0
;NMRAnetDCC.mpas,667 :: 		Dec(ABuffer^.TX_iBit);
	ADD	W10, #28, W1
	ZE	[W1], W0
	DEC	W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,668 :: 		if (ABuffer^.TX_iBit > STATE_NMRA_DCC_BIT_7) or (ABuffer^.TX_iBit < STATE_NMRA_DCC_BIT_0) then
	ADD	W10, #28, W2
	MOV.B	[W2], W0
	CP.B	W0, #7
	CLR	W1
	BRA LEU	L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleByte278
	COM	W1
L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleByte278:
	MOV.B	[W2], W0
	CP.B	W0, #0
	CLR	W0
	BRA GEU	L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleByte279
	COM	W0
L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleByte279:
	IOR	W1, W0, W0
	BRA NZ	L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleByte280
	GOTO	L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleByte134
L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleByte280:
;NMRAnetDCC.mpas,670 :: 		if (ABuffer^.TX_TransmittingPacket.Flags and MASK_DCC_PACKET_COUNT) = ByteIndex + 1 then
	ADD	W10, #20, W0
	ADD	W0, #5, W0
	ZE	[W0], W0
	AND	W0, #7, W1
	ZE	W11, W0
	INC	W0
	CP	W1, W0
	BRA Z	L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleByte281
	GOTO	L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleByte137
L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleByte281:
;NMRAnetDCC.mpas,671 :: 		ABuffer^.TX_iStateMachine := STATE_NMRA_DCC_START_BIT_XOR
	ADD	W10, #27, W1
	MOV.B	#11, W0
	MOV.B	W0, [W1]
	GOTO	L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleByte138
;NMRAnetDCC.mpas,672 :: 		else
L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleByte137:
;NMRAnetDCC.mpas,673 :: 		Inc(ABuffer^.TX_iStateMachine)
	ADD	W10, #27, W1
	ZE	[W1], W0
	INC	W0
	MOV.B	W0, [W1]
L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleByte138:
;NMRAnetDCC.mpas,674 :: 		end
L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleByte134:
;NMRAnetDCC.mpas,675 :: 		end;
L_end_NMRA_DCC_TransmitterStateMachineHandleByte:
	RETURN
; end of NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleByte

NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleStartBit:

;NMRAnetDCC.mpas,685 :: 		begin
;NMRAnetDCC.mpas,686 :: 		ABuffer^.TX_iBit := STATE_NMRA_DCC_BIT_7;                                              // Reset Bit index for the next Byte
	ADD	W10, #28, W1
	MOV.B	#7, W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,687 :: 		ABuffer^.TX_Flags.TRANSMITTING_FLAG_CURRENT_BIT := 0;                         // Current Bit = 0 (start bit)
	MOV	#35, W0
	ADD	W10, W0, W0
	BCLR.B	[W0], #0
;NMRAnetDCC.mpas,688 :: 		Inc(ABuffer^.TX_iStateMachine)                                                // Move to the next State
	ADD	W10, #27, W1
	ZE	[W1], W0
	INC	W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,689 :: 		end;
L_end_NMRA_DCC_TransmitterStateMachineHandleStartBit:
	RETURN
; end of NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleStartBit

NMRAnetDCC_NMRA_DCC_TransmitterStateMachineXORByte:

;NMRAnetDCC.mpas,701 :: 		begin
;NMRAnetDCC.mpas,702 :: 		Offset := ABuffer^.TX_iBit;
	ADD	W10, #28, W0
	MOV.B	[W0], W2
;NMRAnetDCC.mpas,703 :: 		ABuffer^.TX_Flags.TRANSMITTING_FLAG_CURRENT_BIT := ABuffer^.TX_XOR_Byte.Offset;
	MOV	#35, W0
	ADD	W10, W0, W3
	ADD	W10, #30, W1
	MOV	#1, W0
	ZE	W2, W2
	SL	W0, W2, W0
	ZE	[W1], W1
	ZE	W0, W0
	AND	W1, W0, W0
	CLR	W1
	CP	W0, #0
	BRA NZ	L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineXORByte284
	GOTO	L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineXORByte141
L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineXORByte284:
	MOV.B	#1, W0
	MOV.B	W0, W1
L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineXORByte141:
	BSET.B	[W3], #0
	BTSS	W1, #0
	BCLR.B	[W3], #0
;NMRAnetDCC.mpas,704 :: 		Dec(ABuffer^.TX_iBit);
	ADD	W10, #28, W1
	ZE	[W1], W0
	DEC	W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,705 :: 		if (ABuffer^.TX_iBit > STATE_NMRA_DCC_BIT_7) or (ABuffer^.TX_iBit < STATE_NMRA_DCC_BIT_0) then
	ADD	W10, #28, W2
	MOV.B	[W2], W0
	CP.B	W0, #7
	CLR	W1
	BRA LEU	L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineXORByte285
	COM	W1
L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineXORByte285:
	MOV.B	[W2], W0
	CP.B	W0, #0
	CLR	W0
	BRA GEU	L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineXORByte286
	COM	W0
L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineXORByte286:
	IOR	W1, W0, W0
	BRA NZ	L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineXORByte287
	GOTO	L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineXORByte143
L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineXORByte287:
;NMRAnetDCC.mpas,706 :: 		Inc(ABuffer^.TX_iStateMachine)
	ADD	W10, #27, W1
	ZE	[W1], W0
	INC	W0
	MOV.B	W0, [W1]
L_NMRAnetDCC_NMRA_DCC_TransmitterStateMachineXORByte143:
;NMRAnetDCC.mpas,707 :: 		end;
L_end_NMRA_DCC_TransmitterStateMachineXORByte:
	RETURN
; end of NMRAnetDCC_NMRA_DCC_TransmitterStateMachineXORByte

NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleStopBit:

;NMRAnetDCC.mpas,717 :: 		begin
;NMRAnetDCC.mpas,718 :: 		ABuffer^.TX_iBit := STATE_NMRA_DCC_BIT_7;                                     // Reset Bit index for the next Byte
	ADD	W10, #28, W1
	MOV.B	#7, W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,719 :: 		ABuffer^.TX_Flags.TRANSMITTING_FLAG_CURRENT_BIT := 1;                         // Current Bit = 1 (stop bit)
	MOV	#35, W0
	ADD	W10, W0, W0
	BSET.B	[W0], #0
;NMRAnetDCC.mpas,720 :: 		ABuffer^.TX_Flags.TRANSMITTING_FLAG_STOP_BIT := 1;                            // Flag we are transmitting the Stop Bit
	MOV	#35, W0
	ADD	W10, W0, W0
	BSET.B	[W0], #1
;NMRAnetDCC.mpas,721 :: 		end;
L_end_NMRA_DCC_TransmitterStateMachineHandleStopBit:
	RETURN
; end of NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleStopBit

_NMRA_DCC_TransmitterStateMachine:
	LNK	#4

;NMRAnetDCC.mpas,742 :: 		begin
;NMRAnetDCC.mpas,743 :: 		case ABuffer^.TX_iDCC_Pin_StateMachine of
	PUSH	W11
	ADD	W10, #29, W0
	MOV.B	[W0], W0
	MOV.B	W0, [W14+2]
;NMRAnetDCC.mpas,744 :: 		STATE_NMRA_DCC_PIN_0 :     // Pin State 0 is the first "positive" 56us of a new bit being sent on the DCC line
	CP.B	W0, #0
	BRA Z	L__NMRA_DCC_TransmitterStateMachine290
	GOTO	L__NMRA_DCC_TransmitterStateMachine150
L__NMRA_DCC_TransmitterStateMachine290:
;NMRAnetDCC.mpas,746 :: 		case ABuffer^.TX_iStateMachine of
	ADD	W10, #27, W0
	MOV.B	[W0], W0
	MOV.B	W0, [W14+0]
;NMRAnetDCC.mpas,747 :: 		STATE_NMRA_DCC_PREAMBLE          : NMRA_DCC_TransmitterStateMachineHandlePreamble(ABuffer);
	CP.B	W0, #0
	BRA Z	L__NMRA_DCC_TransmitterStateMachine291
	GOTO	L__NMRA_DCC_TransmitterStateMachine154
L__NMRA_DCC_TransmitterStateMachine291:
	CALL	NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandlePreamble
	GOTO	L__NMRA_DCC_TransmitterStateMachine151
L__NMRA_DCC_TransmitterStateMachine154:
;NMRAnetDCC.mpas,748 :: 		STATE_NMRA_DCC_START_BIT_0       : NMRA_DCC_TransmitterStateMachineHandleStartBit(ABuffer);
	MOV.B	[W14+0], W0
	CP.B	W0, #1
	BRA Z	L__NMRA_DCC_TransmitterStateMachine292
	GOTO	L__NMRA_DCC_TransmitterStateMachine157
L__NMRA_DCC_TransmitterStateMachine292:
	CALL	NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleStartBit
	GOTO	L__NMRA_DCC_TransmitterStateMachine151
L__NMRA_DCC_TransmitterStateMachine157:
;NMRAnetDCC.mpas,749 :: 		STATE_NMRA_DCC_BYTE_0            : NMRA_DCC_TransmitterStateMachineHandleByte(ABuffer, 0);
	MOV.B	[W14+0], W0
	CP.B	W0, #2
	BRA Z	L__NMRA_DCC_TransmitterStateMachine293
	GOTO	L__NMRA_DCC_TransmitterStateMachine160
L__NMRA_DCC_TransmitterStateMachine293:
	CLR	W11
	CALL	NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleByte
	GOTO	L__NMRA_DCC_TransmitterStateMachine151
L__NMRA_DCC_TransmitterStateMachine160:
;NMRAnetDCC.mpas,750 :: 		STATE_NMRA_DCC_START_BIT_1       : NMRA_DCC_TransmitterStateMachineHandleStartBit(ABuffer);
	MOV.B	[W14+0], W0
	CP.B	W0, #3
	BRA Z	L__NMRA_DCC_TransmitterStateMachine294
	GOTO	L__NMRA_DCC_TransmitterStateMachine163
L__NMRA_DCC_TransmitterStateMachine294:
	CALL	NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleStartBit
	GOTO	L__NMRA_DCC_TransmitterStateMachine151
L__NMRA_DCC_TransmitterStateMachine163:
;NMRAnetDCC.mpas,751 :: 		STATE_NMRA_DCC_BYTE_1            : NMRA_DCC_TransmitterStateMachineHandleByte(ABuffer, 1);
	MOV.B	[W14+0], W0
	CP.B	W0, #4
	BRA Z	L__NMRA_DCC_TransmitterStateMachine295
	GOTO	L__NMRA_DCC_TransmitterStateMachine166
L__NMRA_DCC_TransmitterStateMachine295:
	MOV.B	#1, W11
	CALL	NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleByte
	GOTO	L__NMRA_DCC_TransmitterStateMachine151
L__NMRA_DCC_TransmitterStateMachine166:
;NMRAnetDCC.mpas,752 :: 		STATE_NMRA_DCC_START_BIT_2       : NMRA_DCC_TransmitterStateMachineHandleStartBit(ABuffer);
	MOV.B	[W14+0], W0
	CP.B	W0, #5
	BRA Z	L__NMRA_DCC_TransmitterStateMachine296
	GOTO	L__NMRA_DCC_TransmitterStateMachine169
L__NMRA_DCC_TransmitterStateMachine296:
	CALL	NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleStartBit
	GOTO	L__NMRA_DCC_TransmitterStateMachine151
L__NMRA_DCC_TransmitterStateMachine169:
;NMRAnetDCC.mpas,753 :: 		STATE_NMRA_DCC_BYTE_2            : NMRA_DCC_TransmitterStateMachineHandleByte(ABuffer, 2);
	MOV.B	[W14+0], W0
	CP.B	W0, #6
	BRA Z	L__NMRA_DCC_TransmitterStateMachine297
	GOTO	L__NMRA_DCC_TransmitterStateMachine172
L__NMRA_DCC_TransmitterStateMachine297:
	MOV.B	#2, W11
	CALL	NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleByte
	GOTO	L__NMRA_DCC_TransmitterStateMachine151
L__NMRA_DCC_TransmitterStateMachine172:
;NMRAnetDCC.mpas,754 :: 		STATE_NMRA_DCC_START_BIT_3       : NMRA_DCC_TransmitterStateMachineHandleStartBit(ABuffer);
	MOV.B	[W14+0], W0
	CP.B	W0, #7
	BRA Z	L__NMRA_DCC_TransmitterStateMachine298
	GOTO	L__NMRA_DCC_TransmitterStateMachine175
L__NMRA_DCC_TransmitterStateMachine298:
	CALL	NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleStartBit
	GOTO	L__NMRA_DCC_TransmitterStateMachine151
L__NMRA_DCC_TransmitterStateMachine175:
;NMRAnetDCC.mpas,755 :: 		STATE_NMRA_DCC_BYTE_3            : NMRA_DCC_TransmitterStateMachineHandleByte(ABuffer, 3);
	MOV.B	[W14+0], W0
	CP.B	W0, #8
	BRA Z	L__NMRA_DCC_TransmitterStateMachine299
	GOTO	L__NMRA_DCC_TransmitterStateMachine178
L__NMRA_DCC_TransmitterStateMachine299:
	MOV.B	#3, W11
	CALL	NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleByte
	GOTO	L__NMRA_DCC_TransmitterStateMachine151
L__NMRA_DCC_TransmitterStateMachine178:
;NMRAnetDCC.mpas,756 :: 		STATE_NMRA_DCC_START_BIT_4       : NMRA_DCC_TransmitterStateMachineHandleStartBit(ABuffer);
	MOV.B	[W14+0], W0
	CP.B	W0, #9
	BRA Z	L__NMRA_DCC_TransmitterStateMachine300
	GOTO	L__NMRA_DCC_TransmitterStateMachine181
L__NMRA_DCC_TransmitterStateMachine300:
	CALL	NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleStartBit
	GOTO	L__NMRA_DCC_TransmitterStateMachine151
L__NMRA_DCC_TransmitterStateMachine181:
;NMRAnetDCC.mpas,757 :: 		STATE_NMRA_DCC_BYTE_4            : NMRA_DCC_TransmitterStateMachineHandleByte(ABuffer, 4);
	MOV.B	[W14+0], W0
	CP.B	W0, #10
	BRA Z	L__NMRA_DCC_TransmitterStateMachine301
	GOTO	L__NMRA_DCC_TransmitterStateMachine184
L__NMRA_DCC_TransmitterStateMachine301:
	MOV.B	#4, W11
	CALL	NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleByte
	GOTO	L__NMRA_DCC_TransmitterStateMachine151
L__NMRA_DCC_TransmitterStateMachine184:
;NMRAnetDCC.mpas,758 :: 		STATE_NMRA_DCC_START_BIT_XOR     : NMRA_DCC_TransmitterStateMachineHandleStartBit(ABuffer);
	MOV.B	[W14+0], W0
	CP.B	W0, #11
	BRA Z	L__NMRA_DCC_TransmitterStateMachine302
	GOTO	L__NMRA_DCC_TransmitterStateMachine187
L__NMRA_DCC_TransmitterStateMachine302:
	CALL	NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleStartBit
	GOTO	L__NMRA_DCC_TransmitterStateMachine151
L__NMRA_DCC_TransmitterStateMachine187:
;NMRAnetDCC.mpas,759 :: 		STATE_NMRA_DCC_XOR_BYTE          : NMRA_DCC_TransmitterStateMachineXORByte(ABuffer);
	MOV.B	[W14+0], W0
	CP.B	W0, #12
	BRA Z	L__NMRA_DCC_TransmitterStateMachine303
	GOTO	L__NMRA_DCC_TransmitterStateMachine190
L__NMRA_DCC_TransmitterStateMachine303:
	CALL	NMRAnetDCC_NMRA_DCC_TransmitterStateMachineXORByte
	GOTO	L__NMRA_DCC_TransmitterStateMachine151
L__NMRA_DCC_TransmitterStateMachine190:
;NMRAnetDCC.mpas,760 :: 		STATE_NMRA_DCC_STOP_BIT          : NMRA_DCC_TransmitterStateMachineHandleStopBit(ABuffer);
	MOV.B	[W14+0], W0
	CP.B	W0, #13
	BRA Z	L__NMRA_DCC_TransmitterStateMachine304
	GOTO	L__NMRA_DCC_TransmitterStateMachine193
L__NMRA_DCC_TransmitterStateMachine304:
	CALL	NMRAnetDCC_NMRA_DCC_TransmitterStateMachineHandleStopBit
	GOTO	L__NMRA_DCC_TransmitterStateMachine151
L__NMRA_DCC_TransmitterStateMachine193:
L__NMRA_DCC_TransmitterStateMachine151:
;NMRAnetDCC.mpas,763 :: 		ABuffer^.TX_Flags.TRANSMITTING_FLAG_DCC_PIN_BIT := 1;                   // Set the I/O Pin High for the start of the bit
	MOV	#35, W0
	ADD	W10, W0, W0
	BSET.B	[W0], #2
;NMRAnetDCC.mpas,765 :: 		if ABuffer^.Tx_Flags.TRANSMITTING_FLAG_CURRENT_BIT = 0 then             // If is a zero we need to add a 56us wait state to make a 112us wide pulse
	MOV	#35, W0
	ADD	W10, W0, W0
	MOV.B	[W0], W0
	BTSC.B	W0, #0
	GOTO	L__NMRA_DCC_TransmitterStateMachine195
;NMRAnetDCC.mpas,766 :: 		Inc(ABuffer^.TX_iDCC_Pin_StateMachine)
	ADD	W10, #29, W1
	ZE	[W1], W0
	INC	W0
	MOV.B	W0, [W1]
	GOTO	L__NMRA_DCC_TransmitterStateMachine196
;NMRAnetDCC.mpas,767 :: 		else
L__NMRA_DCC_TransmitterStateMachine195:
;NMRAnetDCC.mpas,768 :: 		ABuffer^.TX_iDCC_Pin_StateMachine := STATE_NMRA_DCC_PIN_2             // If is a one then we jump right to the back edge of the pulse width at 56us
	ADD	W10, #29, W1
	MOV.B	#2, W0
	MOV.B	W0, [W1]
L__NMRA_DCC_TransmitterStateMachine196:
;NMRAnetDCC.mpas,769 :: 		end;
	GOTO	L__NMRA_DCC_TransmitterStateMachine147
L__NMRA_DCC_TransmitterStateMachine150:
;NMRAnetDCC.mpas,770 :: 		STATE_NMRA_DCC_PIN_1 : Inc(ABuffer^.TX_iDCC_Pin_StateMachine);                // Pin State 1 is the second "positive" 56us of a new "0" bit being sent on the DCC line, if the bit was a 1 in Pin State 0 then this state is skipped
	MOV.B	[W14+2], W2
	CP.B	W2, #1
	BRA Z	L__NMRA_DCC_TransmitterStateMachine305
	GOTO	L__NMRA_DCC_TransmitterStateMachine199
L__NMRA_DCC_TransmitterStateMachine305:
	ADD	W10, #29, W1
	ZE	[W1], W0
	INC	W0
	MOV.B	W0, [W1]
	GOTO	L__NMRA_DCC_TransmitterStateMachine147
L__NMRA_DCC_TransmitterStateMachine199:
;NMRAnetDCC.mpas,771 :: 		STATE_NMRA_DCC_PIN_2 :                                                        // Pin State 2 is the first "negative" 56us of a new bit being sent on the DCC line
	CP.B	W2, #2
	BRA Z	L__NMRA_DCC_TransmitterStateMachine306
	GOTO	L__NMRA_DCC_TransmitterStateMachine202
L__NMRA_DCC_TransmitterStateMachine306:
;NMRAnetDCC.mpas,773 :: 		ABuffer^.TX_Flags.TRANSMITTING_FLAG_DCC_PIN_BIT := 0;                     // Set the I/O Pin Low (negative half of the DCC cycle)
	MOV	#35, W0
	ADD	W10, W0, W0
	BCLR.B	[W0], #2
;NMRAnetDCC.mpas,775 :: 		if ABuffer^.Tx_Flags.TRANSMITTING_FLAG_CURRENT_BIT = 0 then
	MOV	#35, W0
	ADD	W10, W0, W0
	MOV.B	[W0], W0
	BTSC.B	W0, #0
	GOTO	L__NMRA_DCC_TransmitterStateMachine204
;NMRAnetDCC.mpas,776 :: 		Inc(ABuffer^.TX_iDCC_Pin_StateMachine)                                  // It is a "0" so jump to the next half for a wait state to make it 112us wide
	ADD	W10, #29, W1
	ZE	[W1], W0
	INC	W0
	MOV.B	W0, [W1]
	GOTO	L__NMRA_DCC_TransmitterStateMachine205
;NMRAnetDCC.mpas,777 :: 		else begin
L__NMRA_DCC_TransmitterStateMachine204:
;NMRAnetDCC.mpas,778 :: 		ABuffer^.TX_iDCC_Pin_StateMachine := STATE_NMRA_DCC_PIN_0;              // It is a "1" so start the next bit from the beginning
	ADD	W10, #29, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,780 :: 		if ABuffer^.TX_Flags.TRANSMITTING_FLAG_STOP_BIT = 1 then                // If transmitting the second half of the stop bit then reset everything and get ready for the next message
	MOV	#35, W0
	ADD	W10, W0, W0
	MOV.B	[W0], W0
	BTSS.B	W0, #1
	GOTO	L__NMRA_DCC_TransmitterStateMachine207
;NMRAnetDCC.mpas,782 :: 		ABuffer^.TX_Flags.TRANSMITTING_FLAG_STOP_BIT := 0;
	MOV	#35, W0
	ADD	W10, W0, W0
	BCLR.B	[W0], #1
;NMRAnetDCC.mpas,783 :: 		ABuffer^.TX_iStateMachine := STATE_NMRA_DCC_PREAMBLE;                 // New message starting
	ADD	W10, #27, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetDCC.mpas,784 :: 		end
L__NMRA_DCC_TransmitterStateMachine207:
;NMRAnetDCC.mpas,785 :: 		end;
L__NMRA_DCC_TransmitterStateMachine205:
;NMRAnetDCC.mpas,786 :: 		end;
	GOTO	L__NMRA_DCC_TransmitterStateMachine147
L__NMRA_DCC_TransmitterStateMachine202:
;NMRAnetDCC.mpas,787 :: 		STATE_NMRA_DCC_PIN_3 : ABuffer^.TX_iDCC_Pin_StateMachine := STATE_NMRA_DCC_PIN_0; // Pin State 3 is the second "negative" 56us of a new "0" bit being sent on the DCC line, if the bit was a 1 in Pin State 2 then this state is skipped
	CP.B	W2, #3
	BRA Z	L__NMRA_DCC_TransmitterStateMachine307
	GOTO	L__NMRA_DCC_TransmitterStateMachine211
L__NMRA_DCC_TransmitterStateMachine307:
	ADD	W10, #29, W1
	CLR	W0
	MOV.B	W0, [W1]
	GOTO	L__NMRA_DCC_TransmitterStateMachine147
L__NMRA_DCC_TransmitterStateMachine211:
L__NMRA_DCC_TransmitterStateMachine147:
;NMRAnetDCC.mpas,789 :: 		end;
L_end_NMRA_DCC_TransmitterStateMachine:
	POP	W11
	ULNK
	RETURN
; end of _NMRA_DCC_TransmitterStateMachine
