
dsPIC33_CAN_StartTransmission:

;dsPIC33_CAN.mpas,221 :: 		begin
;dsPIC33_CAN.mpas,222 :: 		Result := False;
	PUSH	W10
	PUSH	W11
	PUSH	W12
; Result start address is: 4 (W2)
	CLR	W2
;dsPIC33_CAN.mpas,223 :: 		Buffer := CANStorage_NextToSend;                                              // Get a pointer to the Buffer to transmit
	CALL	_CANStorage_NextToSend
; Buffer start address is: 14 (W7)
	MOV	W0, W7
;dsPIC33_CAN.mpas,224 :: 		if Buffer <> nil then                                                         // If there is something to send then send it
	CP	W0, #0
	BRA NZ	L_dsPIC33_CAN_StartTransmission290
	GOTO	L_dsPIC33_CAN_StartTransmission287
L_dsPIC33_CAN_StartTransmission290:
; Result end address is: 4 (W2)
;dsPIC33_CAN.mpas,264 :: 		dsPIC33_DMA_Style_Buffer(CAN_DIRECTION_WRITE, @TX_Main_RawBufferArray[CAN_TX_0_BUFFER].Word0, Buffer^);      // Convert it into a version that matches the registers
	MOV	W7, W12
	MOV	#lo_addr(_TX_Main_RawBufferArray), W11
	CLR	W10
	CALL	dsPIC33_CAN_dsPIC33_DMA_Style_Buffer
;dsPIC33_CAN.mpas,302 :: 		dsPIC33_CAN_RequestTransmit(CAN_TX_0_BUFFER);                               // Set the Flag to start the transmission
	CLR	W10
	CALL	_dsPIC33_CAN_RequestTransmit
;dsPIC33_CAN.mpas,303 :: 		Buffer^.State := Buffer^.State and not BS_ALLOCATED;                        // Release the Buffer from the List
	ADD	W7, #13, W2
; Buffer end address is: 14 (W7)
	ZE	[W2], W1
	MOV	#253, W0
	AND	W1, W0, W0
	MOV.B	W0, [W2]
;dsPIC33_CAN.mpas,304 :: 		CAN_Engine.State := CAN_Engine.State or CES_TRANSMITTING;
	MOV	#lo_addr(_CAN_Engine), W0
	MOV.B	[W0], W1
	MOV	#lo_addr(_CAN_Engine), W0
	IOR.B	W1, #1, [W0]
;dsPIC33_CAN.mpas,305 :: 		Result := True;
; Result start address is: 2 (W1)
	MOV	#65535, W1
; Result end address is: 2 (W1)
;dsPIC33_CAN.mpas,306 :: 		end
	GOTO	L_dsPIC33_CAN_StartTransmission2
L_dsPIC33_CAN_StartTransmission287:
;dsPIC33_CAN.mpas,224 :: 		if Buffer <> nil then                                                         // If there is something to send then send it
	MOV	W2, W1
;dsPIC33_CAN.mpas,306 :: 		end
L_dsPIC33_CAN_StartTransmission2:
;dsPIC33_CAN.mpas,307 :: 		end;
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_StartTransmission:
	POP	W12
	POP	W11
	POP	W10
	RETURN
; end of dsPIC33_CAN_StartTransmission

_StartCANHighPriorityMessageEngine:

;dsPIC33_CAN.mpas,320 :: 		begin
;dsPIC33_CAN.mpas,321 :: 		Buffer := CANStorage_NextHighPriorityToSend;                                  // Pull the next item to send out of the list
	PUSH	W10
	PUSH	W11
	PUSH	W12
	CALL	_CANStorage_NextHighPriorityToSend
; Buffer start address is: 14 (W7)
	MOV	W0, W7
;dsPIC33_CAN.mpas,323 :: 		if Buffer <> nil then
	CP	W0, #0
	BRA NZ	L__StartCANHighPriorityMessageEngine292
	GOTO	L__StartCANHighPriorityMessageEngine6
L__StartCANHighPriorityMessageEngine292:
;dsPIC33_CAN.mpas,362 :: 		dsPIC33_DMA_Style_Buffer(CAN_DIRECTION_WRITE, @TX_Main_RawBufferArray[CAN_TX_1_BUFFER].Word0, Buffer^);      // Convert it into a version that matches the registers
	MOV	W7, W12
	MOV	#lo_addr(_TX_Main_RawBufferArray+16), W11
	CLR	W10
	CALL	dsPIC33_CAN_dsPIC33_DMA_Style_Buffer
;dsPIC33_CAN.mpas,400 :: 		dsPIC33_CAN_RequestTransmit(CAN_TX_1_BUFFER);                             // Set the Flag to start the transmission
	MOV	#1, W10
	CALL	_dsPIC33_CAN_RequestTransmit
;dsPIC33_CAN.mpas,401 :: 		Buffer^.State := Buffer^.State and not BS_ALLOCATED;                           // Release the Buffer from the List
	ADD	W7, #13, W2
; Buffer end address is: 14 (W7)
	ZE	[W2], W1
	MOV	#253, W0
	AND	W1, W0, W0
	MOV.B	W0, [W2]
;dsPIC33_CAN.mpas,402 :: 		end
L__StartCANHighPriorityMessageEngine6:
;dsPIC33_CAN.mpas,403 :: 		end;
L_end_StartCANHighPriorityMessageEngine:
	POP	W12
	POP	W11
	POP	W10
	RETURN
; end of _StartCANHighPriorityMessageEngine

_StartCANMessageEngine:

;dsPIC33_CAN.mpas,412 :: 		begin
;dsPIC33_CAN.mpas,417 :: 		LockCANInterrupt;
	CALL	_LockCANInterrupt
;dsPIC33_CAN.mpas,422 :: 		if CAN_Engine.State and CES_TRANSMITTING = 0 then
	MOV	#lo_addr(_CAN_Engine), W0
	ZE	[W0], W0
	AND	W0, #1, W0
	CP	W0, #0
	BRA Z	L__StartCANMessageEngine294
	GOTO	L__StartCANMessageEngine10
L__StartCANMessageEngine294:
;dsPIC33_CAN.mpas,424 :: 		StartTransmission;
	CALL	dsPIC33_CAN_StartTransmission
;dsPIC33_CAN.mpas,425 :: 		end;
L__StartCANMessageEngine10:
;dsPIC33_CAN.mpas,428 :: 		UnLockCANInterrupt
	CALL	_UnLockCANInterrupt
;dsPIC33_CAN.mpas,429 :: 		end;
L_end_StartCANMessageEngine:
	RETURN
; end of _StartCANMessageEngine

_dsPIC33_CAN_GlobalInterruptCAN_Event:

;dsPIC33_CAN.mpas,432 :: 		begin
;dsPIC33_CAN.mpas,433 :: 		if Enable then
	CP0	W10
	BRA NZ	L__dsPIC33_CAN_GlobalInterruptCAN_Event296
	GOTO	L__dsPIC33_CAN_GlobalInterruptCAN_Event14
L__dsPIC33_CAN_GlobalInterruptCAN_Event296:
;dsPIC33_CAN.mpas,434 :: 		C1IE_bit := 1
	BSET	C1IE_bit, BitPos(C1IE_bit+0)
	GOTO	L__dsPIC33_CAN_GlobalInterruptCAN_Event15
;dsPIC33_CAN.mpas,435 :: 		else
L__dsPIC33_CAN_GlobalInterruptCAN_Event14:
;dsPIC33_CAN.mpas,436 :: 		C1IE_bit := 0;
	BCLR	C1IE_bit, BitPos(C1IE_bit+0)
L__dsPIC33_CAN_GlobalInterruptCAN_Event15:
;dsPIC33_CAN.mpas,437 :: 		end;
L_end_dsPIC33_CAN_GlobalInterruptCAN_Event:
	RETURN
; end of _dsPIC33_CAN_GlobalInterruptCAN_Event

dsPIC33_CAN_Interrupt_CAN_Event:
	LNK	#14
	PUSH	DSWPAG
	PUSH	50
	PUSH	RCOUNT
	PUSH	W0
	MOV	#2, W0
	REPEAT	#12
	PUSH	[W0++]

;dsPIC33_CAN.mpas,443 :: 		begin
;dsPIC33_CAN.mpas,444 :: 		dsPIC33_CAN_GlobalInterruptFlagCAN_Event(True);                               // Reset the Global CAN Event Interrupt Flag
	PUSH	W10
	PUSH	W11
	PUSH	W12
	MOV	#65535, W10
	CALL	_dsPIC33_CAN_GlobalInterruptFlagCAN_Event
;dsPIC33_CAN.mpas,445 :: 		IntCode := dsPIC33_CAN_InterruptCode;
	CALL	_dsPIC33_CAN_InterruptCode
; IntCode start address is: 2 (W1)
	MOV	W0, W1
; IntCode end address is: 2 (W1)
;dsPIC33_CAN.mpas,446 :: 		while IntCode <> ICODE_NO_INTERRUPT do                                        // Use the Interrupt Code to decode the Events that are both Enabled and Flags are set
L_dsPIC33_CAN_Interrupt_CAN_Event18:
; IntCode start address is: 2 (W1)
	MOV	#64, W0
	CP	W1, W0
	BRA NZ	L_dsPIC33_CAN_Interrupt_CAN_Event298
	GOTO	L_dsPIC33_CAN_Interrupt_CAN_Event19
L_dsPIC33_CAN_Interrupt_CAN_Event298:
;dsPIC33_CAN.mpas,449 :: 		ICODE_TRB0 :                                                              // Tx Rx Buffer 0
	CP	W1, #0
	BRA Z	L_dsPIC33_CAN_Interrupt_CAN_Event299
	GOTO	L_dsPIC33_CAN_Interrupt_CAN_Event25
L_dsPIC33_CAN_Interrupt_CAN_Event299:
; IntCode end address is: 2 (W1)
;dsPIC33_CAN.mpas,451 :: 		dsPIC33_CAN_InterruptFlagTXBuffer(True);                                // TX Interrupt Flag Reset
	MOV	#65535, W10
	CALL	_dsPIC33_CAN_InterruptFlagTxBuffer
;dsPIC33_CAN.mpas,452 :: 		if not StartTransmission then
	CALL	dsPIC33_CAN_StartTransmission
	COM	W0
	BRA NZ	L_dsPIC33_CAN_Interrupt_CAN_Event300
	GOTO	L_dsPIC33_CAN_Interrupt_CAN_Event27
L_dsPIC33_CAN_Interrupt_CAN_Event300:
;dsPIC33_CAN.mpas,453 :: 		CAN_Engine.State := CAN_Engine.State and not CES_TRANSMITTING;
	MOV	#lo_addr(_CAN_Engine), W0
	MOV.B	[W0], W2
	MOV.B	#254, W1
	MOV	#lo_addr(_CAN_Engine), W0
	AND.B	W2, W1, [W0]
L_dsPIC33_CAN_Interrupt_CAN_Event27:
;dsPIC33_CAN.mpas,454 :: 		end;
	GOTO	L_dsPIC33_CAN_Interrupt_CAN_Event22
L_dsPIC33_CAN_Interrupt_CAN_Event25:
;dsPIC33_CAN.mpas,455 :: 		ICODE_TRB1 :                                                              // Tx Rx Buffer 0
; IntCode start address is: 2 (W1)
	CP	W1, #1
	BRA Z	L_dsPIC33_CAN_Interrupt_CAN_Event301
	GOTO	L_dsPIC33_CAN_Interrupt_CAN_Event31
L_dsPIC33_CAN_Interrupt_CAN_Event301:
; IntCode end address is: 2 (W1)
;dsPIC33_CAN.mpas,457 :: 		dsPIC33_CAN_InterruptFlagTXBuffer(True);                                // TX Interrupt Flag Reset
	MOV	#65535, W10
	CALL	_dsPIC33_CAN_InterruptFlagTxBuffer
;dsPIC33_CAN.mpas,458 :: 		end;
	GOTO	L_dsPIC33_CAN_Interrupt_CAN_Event22
L_dsPIC33_CAN_Interrupt_CAN_Event31:
;dsPIC33_CAN.mpas,459 :: 		ICODE_TRB2 :                                                              // Tx Rx Buffer 0
; IntCode start address is: 2 (W1)
	CP	W1, #2
	BRA Z	L_dsPIC33_CAN_Interrupt_CAN_Event302
	GOTO	L_dsPIC33_CAN_Interrupt_CAN_Event34
L_dsPIC33_CAN_Interrupt_CAN_Event302:
; IntCode end address is: 2 (W1)
;dsPIC33_CAN.mpas,479 :: 		dsPIC33_DMA_Style_Buffer(CAN_DIRECTION_READ, @RX_Main_RawBufferArray[0].Word0, Buffer);      // Convert it into a version that matches the registers
	ADD	W14, #0, W0
	MOV	W0, W12
	MOV	#lo_addr(_RX_Main_RawBufferArray), W11
	MOV.B	#1, W10
	CALL	dsPIC33_CAN_dsPIC33_DMA_Style_Buffer
;dsPIC33_CAN.mpas,481 :: 		0: ReceivedOnFilter0(@Buffer);
	MOV	#lo_addr(_RX_Main_RawBufferArray+15), W0
	MOV.B	[W0], W0
	CP.B	W0, #0
	BRA Z	L_dsPIC33_CAN_Interrupt_CAN_Event303
	GOTO	L_dsPIC33_CAN_Interrupt_CAN_Event38
L_dsPIC33_CAN_Interrupt_CAN_Event303:
	ADD	W14, #0, W0
	MOV	W0, W10
	CALL	_ReceivedOnFilter0
	GOTO	L_dsPIC33_CAN_Interrupt_CAN_Event35
L_dsPIC33_CAN_Interrupt_CAN_Event38:
;dsPIC33_CAN.mpas,482 :: 		1: begin  ReceivedOnFilter1(@Buffer);  end;
	MOV	#lo_addr(_RX_Main_RawBufferArray+15), W0
	MOV.B	[W0], W0
	CP.B	W0, #1
	BRA Z	L_dsPIC33_CAN_Interrupt_CAN_Event304
	GOTO	L_dsPIC33_CAN_Interrupt_CAN_Event41
L_dsPIC33_CAN_Interrupt_CAN_Event304:
	ADD	W14, #0, W0
	MOV	W0, W10
	CALL	_ReceivedOnFilter1
	GOTO	L_dsPIC33_CAN_Interrupt_CAN_Event35
L_dsPIC33_CAN_Interrupt_CAN_Event41:
;dsPIC33_CAN.mpas,483 :: 		2: ReceivedOnFilter2(@Buffer);
	MOV	#lo_addr(_RX_Main_RawBufferArray+15), W0
	MOV.B	[W0], W0
	CP.B	W0, #2
	BRA Z	L_dsPIC33_CAN_Interrupt_CAN_Event305
	GOTO	L_dsPIC33_CAN_Interrupt_CAN_Event44
L_dsPIC33_CAN_Interrupt_CAN_Event305:
	ADD	W14, #0, W0
	MOV	W0, W10
	CALL	_ReceivedOnFilter2
	GOTO	L_dsPIC33_CAN_Interrupt_CAN_Event35
L_dsPIC33_CAN_Interrupt_CAN_Event44:
;dsPIC33_CAN.mpas,484 :: 		3: ReceivedOnFilter3(@Buffer);
	MOV	#lo_addr(_RX_Main_RawBufferArray+15), W0
	MOV.B	[W0], W0
	CP.B	W0, #3
	BRA Z	L_dsPIC33_CAN_Interrupt_CAN_Event306
	GOTO	L_dsPIC33_CAN_Interrupt_CAN_Event47
L_dsPIC33_CAN_Interrupt_CAN_Event306:
	ADD	W14, #0, W0
	MOV	W0, W10
	CALL	_ReceivedOnFilter3
	GOTO	L_dsPIC33_CAN_Interrupt_CAN_Event35
L_dsPIC33_CAN_Interrupt_CAN_Event47:
;dsPIC33_CAN.mpas,485 :: 		4: ReceivedOnFilter4(@Buffer);
	MOV	#lo_addr(_RX_Main_RawBufferArray+15), W0
	MOV.B	[W0], W0
	CP.B	W0, #4
	BRA Z	L_dsPIC33_CAN_Interrupt_CAN_Event307
	GOTO	L_dsPIC33_CAN_Interrupt_CAN_Event50
L_dsPIC33_CAN_Interrupt_CAN_Event307:
	ADD	W14, #0, W0
	MOV	W0, W10
	CALL	_ReceivedOnFilter4
	GOTO	L_dsPIC33_CAN_Interrupt_CAN_Event35
L_dsPIC33_CAN_Interrupt_CAN_Event50:
;dsPIC33_CAN.mpas,486 :: 		5: ReceivedOnFilter5(@Buffer);
	MOV	#lo_addr(_RX_Main_RawBufferArray+15), W0
	MOV.B	[W0], W0
	CP.B	W0, #5
	BRA Z	L_dsPIC33_CAN_Interrupt_CAN_Event308
	GOTO	L_dsPIC33_CAN_Interrupt_CAN_Event53
L_dsPIC33_CAN_Interrupt_CAN_Event308:
	ADD	W14, #0, W0
	MOV	W0, W10
	CALL	_ReceivedOnFilter5
	GOTO	L_dsPIC33_CAN_Interrupt_CAN_Event35
L_dsPIC33_CAN_Interrupt_CAN_Event53:
;dsPIC33_CAN.mpas,487 :: 		6: ReceivedOnFilter6(@Buffer);
	MOV	#lo_addr(_RX_Main_RawBufferArray+15), W0
	MOV.B	[W0], W0
	CP.B	W0, #6
	BRA Z	L_dsPIC33_CAN_Interrupt_CAN_Event309
	GOTO	L_dsPIC33_CAN_Interrupt_CAN_Event56
L_dsPIC33_CAN_Interrupt_CAN_Event309:
	ADD	W14, #0, W0
	MOV	W0, W10
	CALL	_ReceivedOnFilter6
	GOTO	L_dsPIC33_CAN_Interrupt_CAN_Event35
L_dsPIC33_CAN_Interrupt_CAN_Event56:
L_dsPIC33_CAN_Interrupt_CAN_Event35:
;dsPIC33_CAN.mpas,489 :: 		dsPIC33_CAN_RX_Full(CAN_RX_0_BUFFER, True);                             // Reset the Full Flag
	MOV	#65535, W11
	MOV	#2, W10
	CALL	_dsPIC33_CAN_RX_Full
;dsPIC33_CAN.mpas,491 :: 		if RBOVIF_bit = 1 then
	BTSS	RBOVIF_bit, BitPos(RBOVIF_bit+0)
	GOTO	L_dsPIC33_CAN_Interrupt_CAN_Event58
;dsPIC33_CAN.mpas,492 :: 		begin {LATA4_bit := 1; delay_us(7); LATA4_bit := 0; delay_us(7);} RBOVIF_bit := 0; end;
	BCLR	RBOVIF_bit, BitPos(RBOVIF_bit+0)
L_dsPIC33_CAN_Interrupt_CAN_Event58:
;dsPIC33_CAN.mpas,494 :: 		dsPIC33_CAN_InterruptFlagRXBuffer(True);                                // RX Interrupt Flag Reset
	MOV	#65535, W10
	CALL	_dsPIC33_CAN_InterruptFlagRXBuffer
;dsPIC33_CAN.mpas,496 :: 		end;
	GOTO	L_dsPIC33_CAN_Interrupt_CAN_Event22
L_dsPIC33_CAN_Interrupt_CAN_Event34:
;dsPIC33_CAN.mpas,497 :: 		ICODE_RX_OVERFLOW :
; IntCode start address is: 2 (W1)
	MOV	#67, W0
	CP	W1, W0
	BRA Z	L_dsPIC33_CAN_Interrupt_CAN_Event310
	GOTO	L_dsPIC33_CAN_Interrupt_CAN_Event62
L_dsPIC33_CAN_Interrupt_CAN_Event310:
; IntCode end address is: 2 (W1)
;dsPIC33_CAN.mpas,500 :: 		dsPIC33_CAN_InterruptFlagRXBufferOverflow(True);
	MOV	#65535, W10
	CALL	_dsPIC33_CAN_InterruptFlagRXBufferOverflow
;dsPIC33_CAN.mpas,502 :: 		end else
	GOTO	L_dsPIC33_CAN_Interrupt_CAN_Event22
L_dsPIC33_CAN_Interrupt_CAN_Event62:
;dsPIC33_CAN.mpas,505 :: 		end;
L_dsPIC33_CAN_Interrupt_CAN_Event22:
;dsPIC33_CAN.mpas,507 :: 		IntCode := dsPIC33_CAN_InterruptCode;                                       // Get the next code (or no code) flag
	CALL	_dsPIC33_CAN_InterruptCode
; IntCode start address is: 2 (W1)
	MOV	W0, W1
;dsPIC33_CAN.mpas,508 :: 		end;
; IntCode end address is: 2 (W1)
	GOTO	L_dsPIC33_CAN_Interrupt_CAN_Event18
L_dsPIC33_CAN_Interrupt_CAN_Event19:
;dsPIC33_CAN.mpas,511 :: 		end;
L_end_Interrupt_CAN_Event:
	POP	W12
	POP	W11
	POP	W10
	MOV	#26, W0
	REPEAT	#12
	POP	[W0--]
	POP	W0
	POP	RCOUNT
	POP	50
	POP	DSWPAG
	ULNK
	RETFIE
; end of dsPIC33_CAN_Interrupt_CAN_Event

_dsPIC33_CAN_Initialize:

;dsPIC33_CAN.mpas,516 :: 		begin
;dsPIC33_CAN.mpas,517 :: 		for i := 0 to MAX_ECAN_TX_BUFFER - 1 do
; i start address is: 4 (W2)
	CLR	W2
; i end address is: 4 (W2)
L__dsPIC33_CAN_Initialize65:
;dsPIC33_CAN.mpas,519 :: 		TX_Main_RawBufferArray[i].Word0 := 0;                                                        // Allow the compiler to account for this ram
; i start address is: 4 (W2)
	SL	W2, #4, W1
	MOV	#lo_addr(_TX_Main_RawBufferArray), W0
	ADD	W0, W1, W1
	CLR	W0
	MOV	W0, [W1]
;dsPIC33_CAN.mpas,520 :: 		TX_Main_RawBufferArray[i].Word1 := 0;
	SL	W2, #4, W1
	MOV	#lo_addr(_TX_Main_RawBufferArray), W0
	ADD	W0, W1, W0
	ADD	W0, #2, W1
	CLR	W0
	MOV	W0, [W1]
;dsPIC33_CAN.mpas,521 :: 		TX_Main_RawBufferArray[i].Word2 := 0;
	SL	W2, #4, W1
	MOV	#lo_addr(_TX_Main_RawBufferArray), W0
	ADD	W0, W1, W0
	ADD	W0, #4, W1
	CLR	W0
	MOV	W0, [W1]
;dsPIC33_CAN.mpas,522 :: 		TX_Main_RawBufferArray[i].Word3 := 0;
	SL	W2, #4, W1
	MOV	#lo_addr(_TX_Main_RawBufferArray), W0
	ADD	W0, W1, W0
	ADD	W0, #6, W1
	CLR	W0
	MOV	W0, [W1]
;dsPIC33_CAN.mpas,523 :: 		TX_Main_RawBufferArray[i].Word4 := 0;
	SL	W2, #4, W1
	MOV	#lo_addr(_TX_Main_RawBufferArray), W0
	ADD	W0, W1, W0
	ADD	W0, #8, W1
	CLR	W0
	MOV	W0, [W1]
;dsPIC33_CAN.mpas,524 :: 		TX_Main_RawBufferArray[i].Word5 := 0;
	SL	W2, #4, W1
	MOV	#lo_addr(_TX_Main_RawBufferArray), W0
	ADD	W0, W1, W0
	ADD	W0, #10, W1
	CLR	W0
	MOV	W0, [W1]
;dsPIC33_CAN.mpas,525 :: 		TX_Main_RawBufferArray[i].Word6 := 0;
	SL	W2, #4, W1
	MOV	#lo_addr(_TX_Main_RawBufferArray), W0
	ADD	W0, W1, W0
	ADD	W0, #12, W1
	CLR	W0
	MOV	W0, [W1]
;dsPIC33_CAN.mpas,526 :: 		TX_Main_RawBufferArray[i].Word7 := 0;
	SL	W2, #4, W1
	MOV	#lo_addr(_TX_Main_RawBufferArray), W0
	ADD	W0, W1, W0
	ADD	W0, #14, W1
	CLR	W0
	MOV	W0, [W1]
;dsPIC33_CAN.mpas,527 :: 		end;
	CP	W2, #1
	BRA NZ	L__dsPIC33_CAN_Initialize312
	GOTO	L__dsPIC33_CAN_Initialize68
L__dsPIC33_CAN_Initialize312:
; i start address is: 4 (W2)
	INC	W2
; i end address is: 4 (W2)
; i end address is: 4 (W2)
	GOTO	L__dsPIC33_CAN_Initialize65
L__dsPIC33_CAN_Initialize68:
;dsPIC33_CAN.mpas,528 :: 		for i := 0 to MAX_ECAN_RX_BUFFER - 1 do
; i start address is: 4 (W2)
	CLR	W2
; i end address is: 4 (W2)
L__dsPIC33_CAN_Initialize70:
;dsPIC33_CAN.mpas,530 :: 		RX_Main_RawBufferArray[i].Word0 := 0;                                                        // Allow the compiler to account for this ram
; i start address is: 4 (W2)
	SL	W2, #4, W1
	MOV	#lo_addr(_RX_Main_RawBufferArray), W0
	ADD	W0, W1, W1
	CLR	W0
	MOV	W0, [W1]
;dsPIC33_CAN.mpas,531 :: 		RX_Main_RawBufferArray[i].Word1 := 0;
	SL	W2, #4, W1
	MOV	#lo_addr(_RX_Main_RawBufferArray), W0
	ADD	W0, W1, W0
	ADD	W0, #2, W1
	CLR	W0
	MOV	W0, [W1]
;dsPIC33_CAN.mpas,532 :: 		RX_Main_RawBufferArray[i].Word2 := 0;
	SL	W2, #4, W1
	MOV	#lo_addr(_RX_Main_RawBufferArray), W0
	ADD	W0, W1, W0
	ADD	W0, #4, W1
	CLR	W0
	MOV	W0, [W1]
;dsPIC33_CAN.mpas,533 :: 		RX_Main_RawBufferArray[i].Word3 := 0;
	SL	W2, #4, W1
	MOV	#lo_addr(_RX_Main_RawBufferArray), W0
	ADD	W0, W1, W0
	ADD	W0, #6, W1
	CLR	W0
	MOV	W0, [W1]
;dsPIC33_CAN.mpas,534 :: 		RX_Main_RawBufferArray[i].Word4 := 0;
	SL	W2, #4, W1
	MOV	#lo_addr(_RX_Main_RawBufferArray), W0
	ADD	W0, W1, W0
	ADD	W0, #8, W1
	CLR	W0
	MOV	W0, [W1]
;dsPIC33_CAN.mpas,535 :: 		RX_Main_RawBufferArray[i].Word5 := 0;
	SL	W2, #4, W1
	MOV	#lo_addr(_RX_Main_RawBufferArray), W0
	ADD	W0, W1, W0
	ADD	W0, #10, W1
	CLR	W0
	MOV	W0, [W1]
;dsPIC33_CAN.mpas,536 :: 		RX_Main_RawBufferArray[i].Word6 := 0;
	SL	W2, #4, W1
	MOV	#lo_addr(_RX_Main_RawBufferArray), W0
	ADD	W0, W1, W0
	ADD	W0, #12, W1
	CLR	W0
	MOV	W0, [W1]
;dsPIC33_CAN.mpas,537 :: 		RX_Main_RawBufferArray[i].Word7 := 0;
	SL	W2, #4, W1
	MOV	#lo_addr(_RX_Main_RawBufferArray), W0
	ADD	W0, W1, W0
	ADD	W0, #14, W1
	CLR	W0
	MOV	W0, [W1]
;dsPIC33_CAN.mpas,538 :: 		end;
	CP	W2, #1
	BRA NZ	L__dsPIC33_CAN_Initialize313
	GOTO	L__dsPIC33_CAN_Initialize73
L__dsPIC33_CAN_Initialize313:
; i start address is: 4 (W2)
	INC	W2
; i end address is: 4 (W2)
; i end address is: 4 (W2)
	GOTO	L__dsPIC33_CAN_Initialize70
L__dsPIC33_CAN_Initialize73:
;dsPIC33_CAN.mpas,539 :: 		end;
L_end_dsPIC33_CAN_Initialize:
	RETURN
; end of _dsPIC33_CAN_Initialize

dsPIC33_CAN_dsPIC33_DMA_Style_Buffer:

;dsPIC33_CAN.mpas,564 :: 		begin
;dsPIC33_CAN.mpas,565 :: 		if Direction = CAN_DIRECTION_WRITE then
	CP.B	W10, #0
	BRA Z	L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer315
	GOTO	L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer76
L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer315:
;dsPIC33_CAN.mpas,568 :: 		if Buffer.State and BS_EXTENDED <> 0 then
	ADD	W12, #13, W0
	ZE	[W0], W0
	AND	W0, #1, W0
	CP	W0, #0
	BRA NZ	L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer316
	GOTO	L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer79
L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer316:
;dsPIC33_CAN.mpas,570 :: 		CANPtr^ := DWORD(Buffer.ID shr 16) and $1FFC;                             // Setup the Extended iD 7..17
	MOV.D	[W12], W0
	MOV.D	W0, W2
	MOV	W3, W2
	CLR	W3
	MOV	#8188, W0
	AND	W2, W0, W0
	MOV	W0, [W11]
;dsPIC33_CAN.mpas,571 :: 		CANPtr^ := CANPtr^ or $0001;                                              // Setup if it is an extended ID
	MOV	[W11], W0
	IOR	W0, #1, W0
	MOV	W0, [W11]
;dsPIC33_CAN.mpas,572 :: 		Inc(CANPtr);                                                              // Move to the Extended ID
	ADD	W11, #2, W5
	MOV	W5, W11
;dsPIC33_CAN.mpas,573 :: 		CANPtr^ := DWORD(Buffer.ID shr 6) and $0FFF;                              // put SID 0..6 into bits 5..11 and SID 6..10 in bits 0..4
	MOV	[W12++], W3
	MOV	[W12--], W4
	MOV	#6, W0
	MOV	W3, W1
	MOV	W4, W2
L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer317:
	DEC	W0, W0
	BRA LT	L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer318
	LSR	W2, W2
	RRC	W1, W1
	BRA	L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer317
L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer318:
	MOV	#4095, W0
	AND	W1, W0, W0
	MOV	W0, [W5]
;dsPIC33_CAN.mpas,574 :: 		Inc(CANPtr);                                                              // Move to the DLC and the rest of the EID
	ADD	W11, #2, W6
	MOV	W6, W11
;dsPIC33_CAN.mpas,575 :: 		CANPtr^ := DWORD((Buffer.ID shl 10) and $FC00);                           // Put SID 0..5 into bits 10..15
	MOV.D	[W12], W4
	MOV	#10, W0
	MOV.D	W4, W2
L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer319:
	DEC	W0, W0
	BRA LT	L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer320
	SL	W2, W2
	RLC	W3, W3
	BRA	L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer319
L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer320:
	MOV	#64512, W0
	MOV	#0, W1
	AND	W2, W0, W0
	AND	W3, W1, W1
	MOV	W0, [W6]
;dsPIC33_CAN.mpas,576 :: 		CANPtr^ := CANPtr^ or (Word( Buffer.DataCount) and $000F);                // Put Data Length in the last 4 bits
	ADD	W12, #4, W0
	ZE	[W0], W0
	AND	W0, #15, W0
	IOR	W0, [W11], [W11]
;dsPIC33_CAN.mpas,577 :: 		end else
	GOTO	L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer80
L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer79:
;dsPIC33_CAN.mpas,579 :: 		CANPtr^ := DWORD(Buffer.ID shl 2) and $007FF;                             // SID
	MOV.D	[W12], W0
	MOV.D	W0, W2
	SL	W2, W2
	RLC	W3, W3
	SL	W2, W2
	RLC	W3, W3
	MOV	#2047, W0
	AND	W2, W0, W0
	MOV	W0, [W11]
;dsPIC33_CAN.mpas,580 :: 		Inc(CANPtr);                                                              // EID is not used
	ADD	W11, #2, W1
	MOV	W1, W11
;dsPIC33_CAN.mpas,581 :: 		CANPtr^ := Buffer.DataCount;                                              // EID/DLC does not use the EID bits
	ADD	W12, #4, W0
	ZE	[W0], W0
	MOV	W0, [W1]
;dsPIC33_CAN.mpas,582 :: 		end;
L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer80:
;dsPIC33_CAN.mpas,583 :: 		Inc(CANPtr);                                                                // Move to Byte 1 and Byte 2
	ADD	W11, #2, W3
	MOV	W3, W11
;dsPIC33_CAN.mpas,584 :: 		CANPtr^ := Buffer.DataBytes[0] or (Buffer.DataBytes[1] shl 8);
	ADD	W12, #5, W2
	ADD	W12, #5, W0
	INC	W0
	MOV.B	[W0], W0
	ZE	W0, W0
	SL	W0, #8, W1
	MOV.B	[W2], W0
	ZE	W0, W0
	IOR	W0, W1, W0
	MOV	W0, [W3]
;dsPIC33_CAN.mpas,585 :: 		Inc(CANPtr);                                                                // Move to Byte 3 and Byte 4
	ADD	W11, #2, W3
	MOV	W3, W11
;dsPIC33_CAN.mpas,586 :: 		CANPtr^ := Buffer.DataBytes[2] or (Buffer.DataBytes[3] shl 8);
	ADD	W12, #5, W0
	ADD	W0, #2, W2
	ADD	W12, #5, W0
	ADD	W0, #3, W0
	MOV.B	[W0], W0
	ZE	W0, W0
	SL	W0, #8, W1
	ZE	[W2], W0
	IOR	W0, W1, W0
	MOV	W0, [W3]
;dsPIC33_CAN.mpas,587 :: 		Inc(CANPtr);                                                                // Move to Byte 5 and Byte 6
	ADD	W11, #2, W3
	MOV	W3, W11
;dsPIC33_CAN.mpas,588 :: 		CANPtr^ := Buffer.DataBytes[4] or (Buffer.DataBytes[5] shl 8);
	ADD	W12, #5, W0
	ADD	W0, #4, W2
	ADD	W12, #5, W0
	ADD	W0, #5, W0
	MOV.B	[W0], W0
	ZE	W0, W0
	SL	W0, #8, W1
	ZE	[W2], W0
	IOR	W0, W1, W0
	MOV	W0, [W3]
;dsPIC33_CAN.mpas,589 :: 		Inc(CANPtr);                                                                // Move to Byte 7 and Byte 8
	ADD	W11, #2, W3
	MOV	W3, W11
;dsPIC33_CAN.mpas,590 :: 		CANPtr^ := Buffer.DataBytes[6] or (Buffer.DataBytes[7] shl 8);
	ADD	W12, #5, W0
	ADD	W0, #6, W2
	ADD	W12, #5, W0
	ADD	W0, #7, W0
	MOV.B	[W0], W0
	ZE	W0, W0
	SL	W0, #8, W1
	ZE	[W2], W0
	IOR	W0, W1, W0
	MOV	W0, [W3]
;dsPIC33_CAN.mpas,591 :: 		Inc(CANPtr);
	ADD	W11, #2, W0
	MOV	W0, W11
;dsPIC33_CAN.mpas,592 :: 		end else
	GOTO	L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer77
L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer76:
;dsPIC33_CAN.mpas,596 :: 		if CANPtr^ and $0001 <> 0 then                                              // Word 0
	MOV	[W11], W0
	AND	W0, #1, W0
	CP	W0, #0
	BRA NZ	L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer321
	GOTO	L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer82
L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer321:
;dsPIC33_CAN.mpas,597 :: 		Buffer.State := Buffer.State or BS_EXTENDED
	ADD	W12, #13, W1
	ADD	W12, #13, W0
	ZE	[W0], W0
	IOR	W0, #1, W0
	MOV.B	W0, [W1]
	GOTO	L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer83
;dsPIC33_CAN.mpas,598 :: 		else
L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer82:
;dsPIC33_CAN.mpas,599 :: 		Buffer.State := Buffer.State and not BS_EXTENDED;
	ADD	W12, #13, W2
	ADD	W12, #13, W0
	ZE	[W0], W1
	MOV	#254, W0
	AND	W1, W0, W0
	MOV.B	W0, [W2]
L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer83:
;dsPIC33_CAN.mpas,600 :: 		if Buffer.State and BS_EXTENDED <> 0 then
	ADD	W12, #13, W0
	ZE	[W0], W0
	AND	W0, #1, W0
	CP	W0, #0
	BRA NZ	L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer322
	GOTO	L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer85
L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer322:
;dsPIC33_CAN.mpas,602 :: 		Buffer.ID := CANPtr^ and $1FFC;
	MOV	[W11], W1
	MOV	#8188, W0
	AND	W1, W0, W0
	CLR	W1
	MOV.D	W0, [W12]
;dsPIC33_CAN.mpas,603 :: 		Buffer.ID := Buffer.ID shl 16;
	MOV.D	[W12], W0
	MOV	W0, W1
	CLR	W0
	MOV.D	W0, [W12]
;dsPIC33_CAN.mpas,604 :: 		Inc(CANPtr);                                                              // Word 1
	ADD	W11, #2, W0
	MOV	W0, W11
;dsPIC33_CAN.mpas,605 :: 		Buffer.ID := Buffer.ID or DWORD((CANPtr^ and $0FFF) shl 6);
	MOV	[W0], W1
	MOV	#4095, W0
	AND	W1, W0, W0
	MOV	W0, W4
	CLR	W5
	MOV	#6, W0
	MOV.D	W4, W2
L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer323:
	DEC	W0, W0
	BRA LT	L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer324
	SL	W2, W2
	RLC	W3, W3
	BRA	L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer323
L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer324:
	MOV.D	[W12], W0
	IOR	W0, W2, [W12++]
	IOR	W1, W3, [W12--]
;dsPIC33_CAN.mpas,606 :: 		Inc(CANPtr);
	ADD	W11, #2, W0
	MOV	W0, W11
;dsPIC33_CAN.mpas,607 :: 		Buffer.ID := Buffer.ID or DWORD((CANPtr^ and $FC00) shr 10);               // Word 2
	MOV	[W0], W1
	MOV	#64512, W0
	AND	W1, W0, W0
	LSR	W0, #10, W0
	MOV	W0, W2
	CLR	W3
	MOV.D	[W12], W0
	IOR	W0, W2, [W12++]
	IOR	W1, W3, [W12--]
;dsPIC33_CAN.mpas,608 :: 		end else
	GOTO	L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer86
L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer85:
;dsPIC33_CAN.mpas,610 :: 		Buffer.ID := (CANPtr^ and $1FFC) shr 2;                                   // Word 0
	MOV	[W11], W1
	MOV	#8188, W0
	AND	W1, W0, W0
	LSR	W0, #2, W0
	CLR	W1
	MOV.D	W0, [W12]
;dsPIC33_CAN.mpas,611 :: 		Inc(CANPtr);                                                              // Word 1
	ADD	W11, #2, W0
	MOV	W0, W11
;dsPIC33_CAN.mpas,612 :: 		end;
L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer86:
;dsPIC33_CAN.mpas,613 :: 		Buffer.DataCount := CANPtr^ and $000F;                                      // Word 2
	ADD	W12, #4, W1
	MOV	[W11], W0
	AND	W0, #15, W0
	MOV.B	W0, [W1]
;dsPIC33_CAN.mpas,614 :: 		Inc(CANPtr);                                                                // Move to C1RXnB1
	ADD	W11, #2, W1
	MOV	W1, W11
;dsPIC33_CAN.mpas,615 :: 		Buffer.DataBytes[0] := CANPtr^;
	ADD	W12, #5, W0
	MOV.B	[W1], [W0]
;dsPIC33_CAN.mpas,616 :: 		Buffer.DataBytes[1] := CANPtr^ shr 8;
	ADD	W12, #5, W0
	ADD	W0, #1, W1
	MOV	[W11], W0
	LSR	W0, #8, W0
	MOV.B	W0, [W1]
;dsPIC33_CAN.mpas,617 :: 		Inc(CANPtr);                                                                // Move to C1RXnB2
	ADD	W11, #2, W1
	MOV	W1, W11
;dsPIC33_CAN.mpas,618 :: 		Buffer.DataBytes[2] := CANPtr^;
	ADD	W12, #5, W0
	INC2	W0
	MOV.B	[W1], [W0]
;dsPIC33_CAN.mpas,619 :: 		Buffer.DataBytes[3] := CANPtr^ shr 8;
	ADD	W12, #5, W0
	ADD	W0, #3, W1
	MOV	[W11], W0
	LSR	W0, #8, W0
	MOV.B	W0, [W1]
;dsPIC33_CAN.mpas,620 :: 		Inc(CANPtr);                                                                // Move to C1RXnB3
	ADD	W11, #2, W1
	MOV	W1, W11
;dsPIC33_CAN.mpas,621 :: 		Buffer.DataBytes[4] := CANPtr^;
	ADD	W12, #5, W0
	ADD	W0, #4, W0
	MOV.B	[W1], [W0]
;dsPIC33_CAN.mpas,622 :: 		Buffer.DataBytes[5] := CANPtr^ shr 8;
	ADD	W12, #5, W0
	ADD	W0, #5, W1
	MOV	[W11], W0
	LSR	W0, #8, W0
	MOV.B	W0, [W1]
;dsPIC33_CAN.mpas,623 :: 		Inc(CANPtr);                                                                // Move to C1RXnB4
	ADD	W11, #2, W1
	MOV	W1, W11
;dsPIC33_CAN.mpas,624 :: 		Buffer.DataBytes[6] := CANPtr^;
	ADD	W12, #5, W0
	ADD	W0, #6, W0
	MOV.B	[W1], [W0]
;dsPIC33_CAN.mpas,625 :: 		Buffer.DataBytes[7] := CANPtr^ shr 8;
	ADD	W12, #5, W0
	ADD	W0, #7, W1
	MOV	[W11], W0
	LSR	W0, #8, W0
	MOV.B	W0, [W1]
;dsPIC33_CAN.mpas,626 :: 		Inc(CANPtr);                                                                // Move to C1RXnCON
	ADD	W11, #2, W0
	MOV	W0, W11
;dsPIC33_CAN.mpas,627 :: 		end
L_dsPIC33_CAN_dsPIC33_DMA_Style_Buffer77:
;dsPIC33_CAN.mpas,628 :: 		end;
L_end_dsPIC33_DMA_Style_Buffer:
	RETURN
; end of dsPIC33_CAN_dsPIC33_DMA_Style_Buffer

_LockCANInterrupt:

;dsPIC33_CAN.mpas,632 :: 		begin
;dsPIC33_CAN.mpas,633 :: 		if CAN_Engine.InterruptLockCount = 0 then
	MOV	#lo_addr(_CAN_Engine+1), W0
	MOV.B	[W0], W0
	CP.B	W0, #0
	BRA Z	L__LockCANInterrupt326
	GOTO	L__LockCANInterrupt89
L__LockCANInterrupt326:
;dsPIC33_CAN.mpas,634 :: 		C1IE_bit := 0;
	BCLR	C1IE_bit, BitPos(C1IE_bit+0)
L__LockCANInterrupt89:
;dsPIC33_CAN.mpas,635 :: 		Inc(CAN_Engine.InterruptLockCount);
	MOV	#lo_addr(_CAN_Engine+1), W0
	ZE	[W0], W0
	ADD	W0, #1, W1
	MOV	#lo_addr(_CAN_Engine+1), W0
	MOV.B	W1, [W0]
;dsPIC33_CAN.mpas,636 :: 		end;
L_end_LockCANInterrupt:
	RETURN
; end of _LockCANInterrupt

_UnLockCANInterrupt:

;dsPIC33_CAN.mpas,639 :: 		begin
;dsPIC33_CAN.mpas,640 :: 		Dec(CAN_Engine.InterruptLockCount);
	MOV	#lo_addr(_CAN_Engine+1), W0
	ZE	[W0], W0
	SUB	W0, #1, W1
	MOV	#lo_addr(_CAN_Engine+1), W0
	MOV.B	W1, [W0]
;dsPIC33_CAN.mpas,641 :: 		if CAN_Engine.InterruptLockCount = 0 then
	MOV	#lo_addr(_CAN_Engine+1), W0
	MOV.B	[W0], W0
	CP.B	W0, #0
	BRA Z	L__UnLockCANInterrupt328
	GOTO	L__UnLockCANInterrupt93
L__UnLockCANInterrupt328:
;dsPIC33_CAN.mpas,642 :: 		C1IE_bit := 1;
	BSET	C1IE_bit, BitPos(C1IE_bit+0)
L__UnLockCANInterrupt93:
;dsPIC33_CAN.mpas,643 :: 		end;
L_end_UnLockCANInterrupt:
	RETURN
; end of _UnLockCANInterrupt

dsPIC33_CAN_SetWindowSelectBit:

;dsPIC33_CAN.mpas,655 :: 		begin
;dsPIC33_CAN.mpas,656 :: 		WIN_bit := 1;
	BSET	WIN_bit, BitPos(WIN_bit+0)
;dsPIC33_CAN.mpas,657 :: 		end;
L_end_SetWindowSelectBit:
	RETURN
; end of dsPIC33_CAN_SetWindowSelectBit

dsPIC33_CAN_ClearWindowSelectBit:

;dsPIC33_CAN.mpas,669 :: 		begin
;dsPIC33_CAN.mpas,670 :: 		WIN_bit := 0;
	BCLR	WIN_bit, BitPos(WIN_bit+0)
;dsPIC33_CAN.mpas,671 :: 		end;
L_end_ClearWindowSelectBit:
	RETURN
; end of dsPIC33_CAN_ClearWindowSelectBit

dsPIC33_CAN_SetFilterMaskBits:
	LNK	#0

;dsPIC33_CAN.mpas,690 :: 		begin
;dsPIC33_CAN.mpas,691 :: 		RegPtrEID^ := Filter;                                               // Setup SID and EID 0..4
; ExtendedOnly start address is: 8 (W4)
	MOV	[W14-8], W4
	MOV	W12, [W11]
;dsPIC33_CAN.mpas,692 :: 		RegPtrSID^ := DWord(Filter shr 16) and $0003;                       // Setup EID 5..6 and clear the upper 14 bits
	MOV.D	W12, W0
	MOV	W1, W0
	CLR	W1
	AND	W0, #3, W0
	MOV	W0, [W10]
;dsPIC33_CAN.mpas,693 :: 		RegPtrSID^ := RegPtrSID^ or (DWord(Filter shr 13) and $FFE0);       // Set up EID 7..17
	MOV	#13, W0
	MOV.D	W12, W2
L_dsPIC33_CAN_SetFilterMaskBits332:
	DEC	W0, W0
	BRA LT	L_dsPIC33_CAN_SetFilterMaskBits333
	LSR	W3, W3
	RRC	W2, W2
	BRA	L_dsPIC33_CAN_SetFilterMaskBits332
L_dsPIC33_CAN_SetFilterMaskBits333:
	MOV	#65504, W0
	MOV	#0, W1
	AND	W2, W0, W2
	AND	W3, W1, W3
	IOR	W2, [W10], W0
	MOV	W0, [W10]
;dsPIC33_CAN.mpas,694 :: 		if ExtendedOnly then
	CP0	W4
	BRA NZ	L_dsPIC33_CAN_SetFilterMaskBits334
	GOTO	L_dsPIC33_CAN_SetFilterMaskBits99
L_dsPIC33_CAN_SetFilterMaskBits334:
; ExtendedOnly end address is: 8 (W4)
;dsPIC33_CAN.mpas,695 :: 		RegPtrSID^ := RegPtrSID^ or $0008;
	MOV	[W10], W0
	IOR	W0, #8, W0
	MOV	W0, [W10]
L_dsPIC33_CAN_SetFilterMaskBits99:
;dsPIC33_CAN.mpas,696 :: 		end;
L_end_SetFilterMaskBits:
	ULNK
	RETURN
; end of dsPIC33_CAN_SetFilterMaskBits

dsPIC33_CAN_ValidateCAN_ID:

;dsPIC33_CAN.mpas,709 :: 		begin
;dsPIC33_CAN.mpas,710 :: 		ID := ID and $1FFFFFFF;
	MOV.D	[W10], W2
	MOV	#65535, W0
	MOV	#8191, W1
	AND	W2, W0, [W10++]
	AND	W3, W1, [W10--]
;dsPIC33_CAN.mpas,711 :: 		end;
L_end_ValidateCAN_ID:
	RETURN
; end of dsPIC33_CAN_ValidateCAN_ID

dsPIC33_CAN_ManipulateTXBit:

;dsPIC33_CAN.mpas,732 :: 		begin
;dsPIC33_CAN.mpas,733 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,734 :: 		RegPtr := @C1TR01CON;
; RegPtr start address is: 2 (W1)
	MOV	#lo_addr(C1TR01CON), W1
;dsPIC33_CAN.mpas,735 :: 		Offset := BufferNumber div 2;       // Is it C1TRO1CON, C1TR23CON, C1TR34CON or C1TR67CON?
	LSR	W10, #1, W0
;dsPIC33_CAN.mpas,736 :: 		RegPtr := RegPtr + Offset;          // Get to the right Register
	SL	W0, #1, W0
; RegPtr start address is: 4 (W2)
	ADD	W1, W0, W2
; RegPtr end address is: 2 (W1)
;dsPIC33_CAN.mpas,737 :: 		Offset := BufferNumber mod 2;       // Is it the first Byte (0,2,4,5) or Second (1,3,5,7) Byte?
	AND	W10, #1, W0
;dsPIC33_CAN.mpas,738 :: 		Offset := (Offset * 8) + BitIndex;
	SL	W0, #3, W1
	ZE	W11, W0
; Offset start address is: 2 (W1)
	ADD	W1, W0, W1
;dsPIC33_CAN.mpas,739 :: 		if DoSet then
	CP0	W12
	BRA NZ	L_dsPIC33_CAN_ManipulateTXBit337
	GOTO	L_dsPIC33_CAN_ManipulateTXBit104
L_dsPIC33_CAN_ManipulateTXBit337:
;dsPIC33_CAN.mpas,740 :: 		RegPtr^.Offset := 1
	MOV	#1, W0
	SL	W0, W1, W0
; Offset end address is: 2 (W1)
	IOR	W0, [W2], [W2]
; RegPtr end address is: 4 (W2)
	GOTO	L_dsPIC33_CAN_ManipulateTXBit105
;dsPIC33_CAN.mpas,741 :: 		else
L_dsPIC33_CAN_ManipulateTXBit104:
;dsPIC33_CAN.mpas,742 :: 		RegPtr^.Offset := 0;
; Offset start address is: 2 (W1)
; RegPtr start address is: 4 (W2)
	MOV	#1, W0
	SL	W0, W1, W0
; Offset end address is: 2 (W1)
	COM	W0
	AND	W0, [W2], [W2]
; RegPtr end address is: 4 (W2)
L_dsPIC33_CAN_ManipulateTXBit105:
;dsPIC33_CAN.mpas,743 :: 		end;
L_end_ManipulateTXBit:
	RETURN
; end of dsPIC33_CAN_ManipulateTXBit

dsPIC33_CAN_ReadTXBit:

;dsPIC33_CAN.mpas,763 :: 		begin
;dsPIC33_CAN.mpas,764 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,765 :: 		RegPtr := @C1TR01CON;
; RegPtr start address is: 2 (W1)
	MOV	#lo_addr(C1TR01CON), W1
;dsPIC33_CAN.mpas,766 :: 		Offset := BufferNumber div 2;       // Is it C1TRO1CON, C1TR23CON, C1TR34CON or C1TR67CON?
	LSR	W10, #1, W0
;dsPIC33_CAN.mpas,767 :: 		RegPtr := RegPtr + Offset;          // Get to the right Register
	SL	W0, #1, W0
	ADD	W1, W0, W2
; RegPtr end address is: 2 (W1)
;dsPIC33_CAN.mpas,768 :: 		Offset := BufferNumber mod 2;       // Is it the first Byte (0,2,4,5) or Second (1,3,5,7) Byte?
	AND	W10, #1, W0
;dsPIC33_CAN.mpas,769 :: 		Offset := (Offset * 8) + BitIndex;
	SL	W0, #3, W1
	ZE	W11, W0
	ADD	W1, W0, W1
;dsPIC33_CAN.mpas,770 :: 		Result := RegPtr^.Offset = 1
	MOV	#1, W0
	SL	W0, W1, W0
	AND	W0, [W2], W0
	CLR	W1
	CP	W0, #0
	BRA NZ	L_dsPIC33_CAN_ReadTXBit339
	GOTO	L_dsPIC33_CAN_ReadTXBit111
L_dsPIC33_CAN_ReadTXBit339:
	MOV.B	#1, W0
	MOV.B	W0, W1
L_dsPIC33_CAN_ReadTXBit111:
; Result start address is: 2 (W1)
	CP.B	W1, #1
	CLR	W1
	BRA NZ	L_dsPIC33_CAN_ReadTXBit340
	COM	W1
L_dsPIC33_CAN_ReadTXBit340:
;dsPIC33_CAN.mpas,771 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_ReadTXBit:
	RETURN
; end of dsPIC33_CAN_ReadTXBit

dsPIC33_CAN_ReadRXBit:

;dsPIC33_CAN.mpas,786 :: 		begin
;dsPIC33_CAN.mpas,787 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,788 :: 		Offset := BufferNumber div 16;       // Is it First or Second Register?
	LSR	W11, #4, W0
;dsPIC33_CAN.mpas,789 :: 		RegPtr := RegPtr + Offset;          // Get to the right Register
	SL	W0, #1, W0
	ADD	W10, W0, W0
	MOV	W0, W10
;dsPIC33_CAN.mpas,790 :: 		Offset := BufferNumber;
; Offset start address is: 4 (W2)
	MOV	W11, W2
;dsPIC33_CAN.mpas,791 :: 		if Offset > 15 then
	CP	W11, #15
	BRA GTU	L_dsPIC33_CAN_ReadRXBit342
	GOTO	L_dsPIC33_CAN_ReadRXBit288
L_dsPIC33_CAN_ReadRXBit342:
;dsPIC33_CAN.mpas,792 :: 		Offset := Offset - 16; // Convert to the Bit Offset
; Offset start address is: 4 (W2)
	SUB	W2, #16, W2
; Offset end address is: 4 (W2)
; Offset end address is: 4 (W2)
	GOTO	L_dsPIC33_CAN_ReadRXBit114
L_dsPIC33_CAN_ReadRXBit288:
;dsPIC33_CAN.mpas,791 :: 		if Offset > 15 then
;dsPIC33_CAN.mpas,792 :: 		Offset := Offset - 16; // Convert to the Bit Offset
L_dsPIC33_CAN_ReadRXBit114:
;dsPIC33_CAN.mpas,793 :: 		Result := RegPtr^.Offset = 1;
; Offset start address is: 4 (W2)
	MOV	#1, W0
	SL	W0, W2, W0
	AND	W0, [W10], W0
	CLR	W1
	CP	W0, #0
	BRA NZ	L_dsPIC33_CAN_ReadRXBit343
	GOTO	L_dsPIC33_CAN_ReadRXBit116
L_dsPIC33_CAN_ReadRXBit343:
	MOV.B	#1, W0
	MOV.B	W0, W1
L_dsPIC33_CAN_ReadRXBit116:
; Result start address is: 2 (W1)
	CP.B	W1, #1
	CLR	W1
	BRA NZ	L_dsPIC33_CAN_ReadRXBit344
	COM	W1
L_dsPIC33_CAN_ReadRXBit344:
;dsPIC33_CAN.mpas,794 :: 		if Clear then
	CP0	W12
	BRA NZ	L_dsPIC33_CAN_ReadRXBit345
	GOTO	L_dsPIC33_CAN_ReadRXBit118
L_dsPIC33_CAN_ReadRXBit345:
;dsPIC33_CAN.mpas,795 :: 		RegPtr^.Offset := 0
	MOV	#1, W0
	SL	W0, W2, W0
; Offset end address is: 4 (W2)
	COM	W0
	AND	W0, [W10], [W10]
L_dsPIC33_CAN_ReadRXBit118:
;dsPIC33_CAN.mpas,796 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_ReadRXBit:
	RETURN
; end of dsPIC33_CAN_ReadRXBit

_dsPIC33_CAN_TX_Aborted:

;dsPIC33_CAN.mpas,815 :: 		begin
;dsPIC33_CAN.mpas,816 :: 		Result := ReadTXBit(BufferNumber, TX_ABORTED_BIT)
	PUSH	W11
	MOV.B	#6, W11
	CALL	dsPIC33_CAN_ReadTXBit
; Result start address is: 2 (W1)
	MOV	W0, W1
;dsPIC33_CAN.mpas,817 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_CAN_TX_Aborted:
	POP	W11
	RETURN
; end of _dsPIC33_CAN_TX_Aborted

_dsPIC33_CAN_TX_Requested:

;dsPIC33_CAN.mpas,835 :: 		begin
;dsPIC33_CAN.mpas,836 :: 		Result := ReadTXBit(BufferNumber, TX_REQUEST_BIT)
	PUSH	W11
	MOV.B	#3, W11
	CALL	dsPIC33_CAN_ReadTXBit
; Result start address is: 2 (W1)
	MOV	W0, W1
;dsPIC33_CAN.mpas,837 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_CAN_TX_Requested:
	POP	W11
	RETURN
; end of _dsPIC33_CAN_TX_Requested

_dsPIC33_CAN_TX_ArbitrationLost:

;dsPIC33_CAN.mpas,855 :: 		begin
;dsPIC33_CAN.mpas,856 :: 		Result := ReadTXBit(BufferNumber, TX_LOST_ARBITRATION_BIT)
	PUSH	W11
	MOV.B	#5, W11
	CALL	dsPIC33_CAN_ReadTXBit
; Result start address is: 2 (W1)
	MOV	W0, W1
;dsPIC33_CAN.mpas,857 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_CAN_TX_ArbitrationLost:
	POP	W11
	RETURN
; end of _dsPIC33_CAN_TX_ArbitrationLost

_dsPIC33_CAN_TX_ErrorDetected:

;dsPIC33_CAN.mpas,875 :: 		begin
;dsPIC33_CAN.mpas,876 :: 		Result := ReadTXBit(BufferNumber, TX_ERROR_DETECTED_BIT)
	PUSH	W11
	MOV.B	#4, W11
	CALL	dsPIC33_CAN_ReadTXBit
; Result start address is: 2 (W1)
	MOV	W0, W1
;dsPIC33_CAN.mpas,877 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_CAN_TX_ErrorDetected:
	POP	W11
	RETURN
; end of _dsPIC33_CAN_TX_ErrorDetected

_dsPIC33_CAN_SetBufferAsTransmitter:

;dsPIC33_CAN.mpas,896 :: 		begin
;dsPIC33_CAN.mpas,897 :: 		ManipulateTXBit(BufferNumber, TX_ENABLE_BIT, DoSet);
	PUSH	W11
	PUSH	W12
	MOV	W11, W12
	MOV.B	#7, W11
	CALL	dsPIC33_CAN_ManipulateTXBit
;dsPIC33_CAN.mpas,898 :: 		end;
L_end_dsPIC33_CAN_SetBufferAsTransmitter:
	POP	W12
	POP	W11
	RETURN
; end of _dsPIC33_CAN_SetBufferAsTransmitter

_dsPIC33_CAN_SetTransmitterPriority:

;dsPIC33_CAN.mpas,904 :: 		begin
;dsPIC33_CAN.mpas,905 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,906 :: 		RegPtr := @C1TR01CON;
; RegPtr start address is: 4 (W2)
	MOV	#lo_addr(C1TR01CON), W2
;dsPIC33_CAN.mpas,907 :: 		Offset := BufferNumber div 2;       // Is it C1TRO1CON, C1TR23CON, C1TR34CON or C1TR67CON?
	LSR	W10, #1, W0
;dsPIC33_CAN.mpas,908 :: 		RegPtr := RegPtr + Offset;          // Get to the right Register
	SL	W0, #1, W0
	ADD	W2, W0, W1
	MOV	W1, W2
;dsPIC33_CAN.mpas,909 :: 		Offset := BufferNumber mod 2;       // Is it the first Byte (0,2,4,5) or Second (1,3,5,7) Byte?
	AND	W10, #1, W0
;dsPIC33_CAN.mpas,910 :: 		Offset := (Offset * 8);
	SL	W0, #3, W0
; Offset start address is: 6 (W3)
	MOV	W0, W3
;dsPIC33_CAN.mpas,911 :: 		Mask := $0003;
; Mask start address is: 8 (W4)
	MOV	#3, W4
;dsPIC33_CAN.mpas,912 :: 		Mask := Mask shl Offset;
	SL	W4, W0, W0
; Mask end address is: 8 (W4)
;dsPIC33_CAN.mpas,913 :: 		Mask := not Mask;
	COM	W0
;dsPIC33_CAN.mpas,914 :: 		RegPtr^ := RegPtr^ and Mask;
	AND	W0, [W1], [W1]
;dsPIC33_CAN.mpas,916 :: 		Mask := Mask shl Offset;
	SL	W11, W3, W0
; Offset end address is: 6 (W3)
;dsPIC33_CAN.mpas,917 :: 		RegPtr^ := RegPtr^ or Mask
	IOR	W0, [W2], [W2]
; RegPtr end address is: 4 (W2)
;dsPIC33_CAN.mpas,918 :: 		end;
L_end_dsPIC33_CAN_SetTransmitterPriority:
	RETURN
; end of _dsPIC33_CAN_SetTransmitterPriority

_dsPIC33_CAN_RequestTransmit:

;dsPIC33_CAN.mpas,938 :: 		begin
;dsPIC33_CAN.mpas,939 :: 		ClearWindowSelectBit;
	PUSH	W11
	PUSH	W12
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,940 :: 		ManipulateTXBit(BufferNumber, TX_REQUEST_BIT, True);
	MOV	#65535, W12
	MOV.B	#3, W11
	CALL	dsPIC33_CAN_ManipulateTXBit
;dsPIC33_CAN.mpas,942 :: 		end;
L_end_dsPIC33_CAN_RequestTransmit:
	POP	W12
	POP	W11
	RETURN
; end of _dsPIC33_CAN_RequestTransmit

_dsPIC33_CAN_ClearTransmit:

;dsPIC33_CAN.mpas,962 :: 		begin
;dsPIC33_CAN.mpas,963 :: 		ClearWindowSelectBit;
	PUSH	W11
	PUSH	W12
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,964 :: 		ManipulateTXBit(BufferNumber, TX_REQUEST_BIT, False);
	CLR	W12
	MOV.B	#3, W11
	CALL	dsPIC33_CAN_ManipulateTXBit
;dsPIC33_CAN.mpas,965 :: 		end;
L_end_dsPIC33_CAN_ClearTransmit:
	POP	W12
	POP	W11
	RETURN
; end of _dsPIC33_CAN_ClearTransmit

_dsPIC33_CAN_RX_Full:

;dsPIC33_CAN.mpas,977 :: 		begin
;dsPIC33_CAN.mpas,978 :: 		ClearWindowSelectBit;
	PUSH	W10
	PUSH	W11
	PUSH	W12
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,979 :: 		Result := ReadRXBit(@C1RXFUL1, BufferNumber, Clear);
	MOV	W11, W12
	MOV	W10, W11
	MOV	#lo_addr(C1RXFUL1), W10
	CALL	dsPIC33_CAN_ReadRXBit
; Result start address is: 2 (W1)
	MOV	W0, W1
;dsPIC33_CAN.mpas,980 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_CAN_RX_Full:
	POP	W12
	POP	W11
	POP	W10
	RETURN
; end of _dsPIC33_CAN_RX_Full

_dsPIC33_CAN_RX_Overflow:

;dsPIC33_CAN.mpas,992 :: 		begin
;dsPIC33_CAN.mpas,993 :: 		ClearWindowSelectBit;
	PUSH	W10
	PUSH	W11
	PUSH	W12
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,994 :: 		Result := ReadRXBit(@C1RXOVF1, BufferNumber, Clear);
	MOV	W11, W12
	MOV	W10, W11
	MOV	#lo_addr(C1RXOVF1), W10
	CALL	dsPIC33_CAN_ReadRXBit
; Result start address is: 2 (W1)
	MOV	W0, W1
;dsPIC33_CAN.mpas,995 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_CAN_RX_Overflow:
	POP	W12
	POP	W11
	POP	W10
	RETURN
; end of _dsPIC33_CAN_RX_Overflow

_dsPIC33_CAN_InterruptCodeWithFilterHit:

;dsPIC33_CAN.mpas,1012 :: 		begin
;dsPIC33_CAN.mpas,1013 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,1014 :: 		IntCode := C1VEC and $007F;
	MOV	#127, W1
	MOV	#lo_addr(C1VEC), W0
	AND	W1, [W0], W0
	MOV	W0, [W10]
;dsPIC33_CAN.mpas,1015 :: 		FilterHit := C1VEC shr 8
	MOV	C1VEC, WREG
	LSR	W0, #8, W0
	MOV	W0, [W11]
;dsPIC33_CAN.mpas,1016 :: 		end;
L_end_dsPIC33_CAN_InterruptCodeWithFilterHit:
	RETURN
; end of _dsPIC33_CAN_InterruptCodeWithFilterHit

_dsPIC33_CAN_GlobalInterruptCAN_RX_Ready:

;dsPIC33_CAN.mpas,1029 :: 		begin
;dsPIC33_CAN.mpas,1030 :: 		if Enable then
	CP0	W10
	BRA NZ	L__dsPIC33_CAN_GlobalInterruptCAN_RX_Ready358
	GOTO	L__dsPIC33_CAN_GlobalInterruptCAN_RX_Ready135
L__dsPIC33_CAN_GlobalInterruptCAN_RX_Ready358:
;dsPIC33_CAN.mpas,1031 :: 		C1RXIE_bit := 1
	BSET	C1RXIE_bit, BitPos(C1RXIE_bit+0)
	GOTO	L__dsPIC33_CAN_GlobalInterruptCAN_RX_Ready136
;dsPIC33_CAN.mpas,1032 :: 		else
L__dsPIC33_CAN_GlobalInterruptCAN_RX_Ready135:
;dsPIC33_CAN.mpas,1033 :: 		C1RXIE_bit := 0;
	BCLR	C1RXIE_bit, BitPos(C1RXIE_bit+0)
L__dsPIC33_CAN_GlobalInterruptCAN_RX_Ready136:
;dsPIC33_CAN.mpas,1034 :: 		end;
L_end_dsPIC33_CAN_GlobalInterruptCAN_RX_Ready:
	RETURN
; end of _dsPIC33_CAN_GlobalInterruptCAN_RX_Ready

_dsPIC33_CAN_GlobalInterruptCAN_TX_Request:

;dsPIC33_CAN.mpas,1046 :: 		begin
;dsPIC33_CAN.mpas,1047 :: 		if Enable then
	CP0	W10
	BRA NZ	L__dsPIC33_CAN_GlobalInterruptCAN_TX_Request360
	GOTO	L__dsPIC33_CAN_GlobalInterruptCAN_TX_Request139
L__dsPIC33_CAN_GlobalInterruptCAN_TX_Request360:
;dsPIC33_CAN.mpas,1048 :: 		C1RXIE_bit := 1
	BSET	C1RXIE_bit, BitPos(C1RXIE_bit+0)
	GOTO	L__dsPIC33_CAN_GlobalInterruptCAN_TX_Request140
;dsPIC33_CAN.mpas,1049 :: 		else
L__dsPIC33_CAN_GlobalInterruptCAN_TX_Request139:
;dsPIC33_CAN.mpas,1050 :: 		C1RXIE_bit := 0;
	BCLR	C1RXIE_bit, BitPos(C1RXIE_bit+0)
L__dsPIC33_CAN_GlobalInterruptCAN_TX_Request140:
;dsPIC33_CAN.mpas,1051 :: 		end;
L_end_dsPIC33_CAN_GlobalInterruptCAN_TX_Request:
	RETURN
; end of _dsPIC33_CAN_GlobalInterruptCAN_TX_Request

_dsPIC33_CAN_GlobalInterruptFlagCAN_Event:

;dsPIC33_CAN.mpas,1064 :: 		begin
;dsPIC33_CAN.mpas,1065 :: 		Result := C1IF_bit = 1;
	CLR.B	W0
	BTSC	C1IF_bit, BitPos(C1IF_bit+0)
	INC.B	W0
; Result start address is: 2 (W1)
	CP.B	W0, #1
	CLR	W1
	BRA NZ	L__dsPIC33_CAN_GlobalInterruptFlagCAN_Event362
	COM	W1
L__dsPIC33_CAN_GlobalInterruptFlagCAN_Event362:
;dsPIC33_CAN.mpas,1066 :: 		if DoReset then C1IF_Bit := 0
	CP0	W10
	BRA NZ	L__dsPIC33_CAN_GlobalInterruptFlagCAN_Event363
	GOTO	L__dsPIC33_CAN_GlobalInterruptFlagCAN_Event143
L__dsPIC33_CAN_GlobalInterruptFlagCAN_Event363:
	BCLR	C1IF_bit, BitPos(C1IF_bit+0)
L__dsPIC33_CAN_GlobalInterruptFlagCAN_Event143:
;dsPIC33_CAN.mpas,1067 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_CAN_GlobalInterruptFlagCAN_Event:
	RETURN
; end of _dsPIC33_CAN_GlobalInterruptFlagCAN_Event

_dsPIC33_CAN_GlobalInterruptFlagCAN_RX_Ready:

;dsPIC33_CAN.mpas,1082 :: 		begin
;dsPIC33_CAN.mpas,1083 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,1084 :: 		Result := C1RXIF_bit = 1;
	CLR.B	W0
	BTSC	C1RXIF_bit, BitPos(C1RXIF_bit+0)
	INC.B	W0
; Result start address is: 2 (W1)
	CP.B	W0, #1
	CLR	W1
	BRA NZ	L__dsPIC33_CAN_GlobalInterruptFlagCAN_RX_Ready365
	COM	W1
L__dsPIC33_CAN_GlobalInterruptFlagCAN_RX_Ready365:
;dsPIC33_CAN.mpas,1085 :: 		if DoReset then C1RXIF_Bit := 0
	CP0	W10
	BRA NZ	L__dsPIC33_CAN_GlobalInterruptFlagCAN_RX_Ready366
	GOTO	L__dsPIC33_CAN_GlobalInterruptFlagCAN_RX_Ready147
L__dsPIC33_CAN_GlobalInterruptFlagCAN_RX_Ready366:
	BCLR	C1RXIF_bit, BitPos(C1RXIF_bit+0)
L__dsPIC33_CAN_GlobalInterruptFlagCAN_RX_Ready147:
;dsPIC33_CAN.mpas,1086 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_CAN_GlobalInterruptFlagCAN_RX_Ready:
	RETURN
; end of _dsPIC33_CAN_GlobalInterruptFlagCAN_RX_Ready

_dsPIC33_CAN_GlobalInterruptFlagCAN_TX_Request:

;dsPIC33_CAN.mpas,1101 :: 		begin
;dsPIC33_CAN.mpas,1102 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,1103 :: 		Result := C1TXIF_bit = 1;
	CLR.B	W0
	BTSC	C1TXIF_bit, BitPos(C1TXIF_bit+0)
	INC.B	W0
; Result start address is: 2 (W1)
	CP.B	W0, #1
	CLR	W1
	BRA NZ	L__dsPIC33_CAN_GlobalInterruptFlagCAN_TX_Request368
	COM	W1
L__dsPIC33_CAN_GlobalInterruptFlagCAN_TX_Request368:
;dsPIC33_CAN.mpas,1104 :: 		if DoReset then C1TXIF_Bit := 0
	CP0	W10
	BRA NZ	L__dsPIC33_CAN_GlobalInterruptFlagCAN_TX_Request369
	GOTO	L__dsPIC33_CAN_GlobalInterruptFlagCAN_TX_Request151
L__dsPIC33_CAN_GlobalInterruptFlagCAN_TX_Request369:
	BCLR	C1TXIF_bit, BitPos(C1TXIF_bit+0)
L__dsPIC33_CAN_GlobalInterruptFlagCAN_TX_Request151:
;dsPIC33_CAN.mpas,1105 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_CAN_GlobalInterruptFlagCAN_TX_Request:
	RETURN
; end of _dsPIC33_CAN_GlobalInterruptFlagCAN_TX_Request

_dsPIC33_CAN_GlobalInterruptCAN_EventPriority:

;dsPIC33_CAN.mpas,1117 :: 		begin
;dsPIC33_CAN.mpas,1118 :: 		C1IP_0_bit := Priority.0;
	BTSS	W10, #0
	BCLR	C1IP_0_bit, BitPos(C1IP_0_bit+0)
	BTSC	W10, #0
	BSET	C1IP_0_bit, BitPos(C1IP_0_bit+0)
;dsPIC33_CAN.mpas,1119 :: 		C1IP_1_bit := Priority.1;
	BTSS	W10, #1
	BCLR	C1IP_1_bit, BitPos(C1IP_1_bit+0)
	BTSC	W10, #1
	BSET	C1IP_1_bit, BitPos(C1IP_1_bit+0)
;dsPIC33_CAN.mpas,1120 :: 		C1IP_2_bit := Priority.2;
	BTSS	W10, #2
	BCLR	C1IP_2_bit, BitPos(C1IP_2_bit+0)
	BTSC	W10, #2
	BSET	C1IP_2_bit, BitPos(C1IP_2_bit+0)
;dsPIC33_CAN.mpas,1121 :: 		end;
L_end_dsPIC33_CAN_GlobalInterruptCAN_EventPriority:
	RETURN
; end of _dsPIC33_CAN_GlobalInterruptCAN_EventPriority

_dsPIC33_CAN_GlobalInterruptCAN_RX_ReadyPriority:

;dsPIC33_CAN.mpas,1136 :: 		begin
;dsPIC33_CAN.mpas,1137 :: 		C1RXIP_0_bit := Priority.0;
	BTSS	W10, #0
	BCLR	C1RXIP_0_bit, BitPos(C1RXIP_0_bit+0)
	BTSC	W10, #0
	BSET	C1RXIP_0_bit, BitPos(C1RXIP_0_bit+0)
;dsPIC33_CAN.mpas,1138 :: 		C1RXIP_1_bit := Priority.1;
	BTSS	W10, #1
	BCLR	C1RXIP_1_bit, BitPos(C1RXIP_1_bit+0)
	BTSC	W10, #1
	BSET	C1RXIP_1_bit, BitPos(C1RXIP_1_bit+0)
;dsPIC33_CAN.mpas,1139 :: 		C1RXIP_2_bit := Priority.2;
	BTSS	W10, #2
	BCLR	C1RXIP_2_bit, BitPos(C1RXIP_2_bit+0)
	BTSC	W10, #2
	BSET	C1RXIP_2_bit, BitPos(C1RXIP_2_bit+0)
;dsPIC33_CAN.mpas,1140 :: 		end;
L_end_dsPIC33_CAN_GlobalInterruptCAN_RX_ReadyPriority:
	RETURN
; end of _dsPIC33_CAN_GlobalInterruptCAN_RX_ReadyPriority

_dsPIC33_CAN_GlobalInterruptCAN_TX_RequestPriority:

;dsPIC33_CAN.mpas,1155 :: 		begin
;dsPIC33_CAN.mpas,1156 :: 		C1TXIP_0_bit := Priority.0;
	BTSS	W10, #0
	BCLR	C1TXIP_0_bit, BitPos(C1TXIP_0_bit+0)
	BTSC	W10, #0
	BSET	C1TXIP_0_bit, BitPos(C1TXIP_0_bit+0)
;dsPIC33_CAN.mpas,1157 :: 		C1TXIP_1_bit := Priority.1;
	BTSS	W10, #1
	BCLR	C1TXIP_1_bit, BitPos(C1TXIP_1_bit+0)
	BTSC	W10, #1
	BSET	C1TXIP_1_bit, BitPos(C1TXIP_1_bit+0)
;dsPIC33_CAN.mpas,1158 :: 		C1TXIP_2_bit := Priority.2;
	BTSS	W10, #2
	BCLR	C1TXIP_2_bit, BitPos(C1TXIP_2_bit+0)
	BTSC	W10, #2
	BSET	C1TXIP_2_bit, BitPos(C1TXIP_2_bit+0)
;dsPIC33_CAN.mpas,1159 :: 		end;
L_end_dsPIC33_CAN_GlobalInterruptCAN_TX_RequestPriority:
	RETURN
; end of _dsPIC33_CAN_GlobalInterruptCAN_TX_RequestPriority

_dsPIC33_CAN_InterruptCode:

;dsPIC33_CAN.mpas,1176 :: 		begin
;dsPIC33_CAN.mpas,1177 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,1178 :: 		Result := C1VEC and $007F
	MOV	#127, W1
	MOV	#lo_addr(C1VEC), W0
; Result start address is: 2 (W1)
	AND	W1, [W0], W1
;dsPIC33_CAN.mpas,1179 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_CAN_InterruptCode:
	RETURN
; end of _dsPIC33_CAN_InterruptCode

_dsPIC33_CAN_InterruptFlagTXInErrorStateBusOff:

;dsPIC33_CAN.mpas,1191 :: 		begin
;dsPIC33_CAN.mpas,1192 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,1193 :: 		Result := TXBO_bit = 1;
	CLR.B	W0
	BTSC	TXBO_bit, BitPos(TXBO_bit+0)
	INC.B	W0
; Result start address is: 2 (W1)
	CP.B	W0, #1
	CLR	W1
	BRA NZ	L__dsPIC33_CAN_InterruptFlagTXInErrorStateBusOff375
	COM	W1
L__dsPIC33_CAN_InterruptFlagTXInErrorStateBusOff375:
;dsPIC33_CAN.mpas,1194 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_CAN_InterruptFlagTXInErrorStateBusOff:
	RETURN
; end of _dsPIC33_CAN_InterruptFlagTXInErrorStateBusOff

_dsPIC33_CAN_InterruptFlagTXInErrorStateBusPassive:

;dsPIC33_CAN.mpas,1206 :: 		begin
;dsPIC33_CAN.mpas,1207 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,1208 :: 		Result := TXBP_bit = 1;
	CLR.B	W0
	BTSC	TXBP_bit, BitPos(TXBP_bit+0)
	INC.B	W0
; Result start address is: 2 (W1)
	CP.B	W0, #1
	CLR	W1
	BRA NZ	L__dsPIC33_CAN_InterruptFlagTXInErrorStateBusPassive377
	COM	W1
L__dsPIC33_CAN_InterruptFlagTXInErrorStateBusPassive377:
;dsPIC33_CAN.mpas,1209 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_CAN_InterruptFlagTXInErrorStateBusPassive:
	RETURN
; end of _dsPIC33_CAN_InterruptFlagTXInErrorStateBusPassive

_dsPIC33_CAN_InterruptFlagRXInErrorBusPassive:

;dsPIC33_CAN.mpas,1221 :: 		begin
;dsPIC33_CAN.mpas,1222 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,1223 :: 		Result := RXBP_bit = 1;
	CLR.B	W0
	BTSC	RXBP_bit, BitPos(RXBP_bit+0)
	INC.B	W0
; Result start address is: 2 (W1)
	CP.B	W0, #1
	CLR	W1
	BRA NZ	L__dsPIC33_CAN_InterruptFlagRXInErrorBusPassive379
	COM	W1
L__dsPIC33_CAN_InterruptFlagRXInErrorBusPassive379:
;dsPIC33_CAN.mpas,1224 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_CAN_InterruptFlagRXInErrorBusPassive:
	RETURN
; end of _dsPIC33_CAN_InterruptFlagRXInErrorBusPassive

_dsPIC33_CAN_InterruptFlagTXInErrorStateWarning:

;dsPIC33_CAN.mpas,1236 :: 		begin
;dsPIC33_CAN.mpas,1237 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,1238 :: 		Result := TXWAR_bit = 1;
	CLR.B	W0
	BTSC	TXWAR_bit, BitPos(TXWAR_bit+0)
	INC.B	W0
; Result start address is: 2 (W1)
	CP.B	W0, #1
	CLR	W1
	BRA NZ	L__dsPIC33_CAN_InterruptFlagTXInErrorStateWarning381
	COM	W1
L__dsPIC33_CAN_InterruptFlagTXInErrorStateWarning381:
;dsPIC33_CAN.mpas,1239 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_CAN_InterruptFlagTXInErrorStateWarning:
	RETURN
; end of _dsPIC33_CAN_InterruptFlagTXInErrorStateWarning

_dsPIC33_CAN_InterruptFlagRXInErrorStateWarning:

;dsPIC33_CAN.mpas,1251 :: 		begin
;dsPIC33_CAN.mpas,1252 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,1253 :: 		Result := RXWAR_bit = 1;
	CLR.B	W0
	BTSC	RXWAR_bit, BitPos(RXWAR_bit+0)
	INC.B	W0
; Result start address is: 2 (W1)
	CP.B	W0, #1
	CLR	W1
	BRA NZ	L__dsPIC33_CAN_InterruptFlagRXInErrorStateWarning383
	COM	W1
L__dsPIC33_CAN_InterruptFlagRXInErrorStateWarning383:
;dsPIC33_CAN.mpas,1254 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_CAN_InterruptFlagRXInErrorStateWarning:
	RETURN
; end of _dsPIC33_CAN_InterruptFlagRXInErrorStateWarning

_dsPIC33_CAN_InterruptFlagTXOrRXInErrorStateWarning:

;dsPIC33_CAN.mpas,1266 :: 		begin
;dsPIC33_CAN.mpas,1267 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,1268 :: 		Result := EWARN_bit = 1;
	CLR.B	W0
	BTSC	EWARN_bit, BitPos(EWARN_bit+0)
	INC.B	W0
; Result start address is: 2 (W1)
	CP.B	W0, #1
	CLR	W1
	BRA NZ	L__dsPIC33_CAN_InterruptFlagTXOrRXInErrorStateWarning385
	COM	W1
L__dsPIC33_CAN_InterruptFlagTXOrRXInErrorStateWarning385:
;dsPIC33_CAN.mpas,1269 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_CAN_InterruptFlagTXOrRXInErrorStateWarning:
	RETURN
; end of _dsPIC33_CAN_InterruptFlagTXOrRXInErrorStateWarning

_dsPIC33_CAN_InterruptFlagInvalidMessage:

;dsPIC33_CAN.mpas,1281 :: 		begin
;dsPIC33_CAN.mpas,1282 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,1283 :: 		Result := IVRIF_bit = 1;
	CLR.B	W0
	BTSC	IVRIF_bit, BitPos(IVRIF_bit+0)
	INC.B	W0
; Result start address is: 2 (W1)
	CP.B	W0, #1
	CLR	W1
	BRA NZ	L__dsPIC33_CAN_InterruptFlagInvalidMessage387
	COM	W1
L__dsPIC33_CAN_InterruptFlagInvalidMessage387:
;dsPIC33_CAN.mpas,1284 :: 		if DoReset then IVRIF_bit := 0;
	CP0	W10
	BRA NZ	L__dsPIC33_CAN_InterruptFlagInvalidMessage388
	GOTO	L__dsPIC33_CAN_InterruptFlagInvalidMessage165
L__dsPIC33_CAN_InterruptFlagInvalidMessage388:
	BCLR	IVRIF_bit, BitPos(IVRIF_bit+0)
L__dsPIC33_CAN_InterruptFlagInvalidMessage165:
;dsPIC33_CAN.mpas,1285 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_CAN_InterruptFlagInvalidMessage:
	RETURN
; end of _dsPIC33_CAN_InterruptFlagInvalidMessage

_dsPIC33_CAN_InterruptFlagBusWakeupActivity:

;dsPIC33_CAN.mpas,1297 :: 		begin
;dsPIC33_CAN.mpas,1298 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,1299 :: 		Result := WAKIF_bit = 1;
	CLR.B	W0
	BTSC	WAKIF_bit, BitPos(WAKIF_bit+0)
	INC.B	W0
; Result start address is: 2 (W1)
	CP.B	W0, #1
	CLR	W1
	BRA NZ	L__dsPIC33_CAN_InterruptFlagBusWakeupActivity390
	COM	W1
L__dsPIC33_CAN_InterruptFlagBusWakeupActivity390:
;dsPIC33_CAN.mpas,1300 :: 		if DoReset then WAKIF_bit := 0;
	CP0	W10
	BRA NZ	L__dsPIC33_CAN_InterruptFlagBusWakeupActivity391
	GOTO	L__dsPIC33_CAN_InterruptFlagBusWakeupActivity169
L__dsPIC33_CAN_InterruptFlagBusWakeupActivity391:
	BCLR	WAKIF_bit, BitPos(WAKIF_bit+0)
L__dsPIC33_CAN_InterruptFlagBusWakeupActivity169:
;dsPIC33_CAN.mpas,1301 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_CAN_InterruptFlagBusWakeupActivity:
	RETURN
; end of _dsPIC33_CAN_InterruptFlagBusWakeupActivity

_dsPIC33_CAN_InterruptFlagError:

;dsPIC33_CAN.mpas,1313 :: 		begin
;dsPIC33_CAN.mpas,1314 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,1315 :: 		Result := ERRIF_bit = 1;
	CLR.B	W0
	BTSC	ERRIF_bit, BitPos(ERRIF_bit+0)
	INC.B	W0
; Result start address is: 2 (W1)
	CP.B	W0, #1
	CLR	W1
	BRA NZ	L__dsPIC33_CAN_InterruptFlagError393
	COM	W1
L__dsPIC33_CAN_InterruptFlagError393:
;dsPIC33_CAN.mpas,1316 :: 		if DoReset then ERRIF_bit := 0;
	CP0	W10
	BRA NZ	L__dsPIC33_CAN_InterruptFlagError394
	GOTO	L__dsPIC33_CAN_InterruptFlagError173
L__dsPIC33_CAN_InterruptFlagError394:
	BCLR	ERRIF_bit, BitPos(ERRIF_bit+0)
L__dsPIC33_CAN_InterruptFlagError173:
;dsPIC33_CAN.mpas,1317 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_CAN_InterruptFlagError:
	RETURN
; end of _dsPIC33_CAN_InterruptFlagError

_dsPIC33_CAN_InterruptFlagFIFO_AlmostFull:

;dsPIC33_CAN.mpas,1329 :: 		begin
;dsPIC33_CAN.mpas,1330 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,1331 :: 		Result := FIFOIF_bit = 1;
	CLR.B	W0
	BTSC	FIFOIF_bit, BitPos(FIFOIF_bit+0)
	INC.B	W0
; Result start address is: 2 (W1)
	CP.B	W0, #1
	CLR	W1
	BRA NZ	L__dsPIC33_CAN_InterruptFlagFIFO_AlmostFull396
	COM	W1
L__dsPIC33_CAN_InterruptFlagFIFO_AlmostFull396:
;dsPIC33_CAN.mpas,1332 :: 		if DoReset then FIFOIF_bit := 0;
	CP0	W10
	BRA NZ	L__dsPIC33_CAN_InterruptFlagFIFO_AlmostFull397
	GOTO	L__dsPIC33_CAN_InterruptFlagFIFO_AlmostFull177
L__dsPIC33_CAN_InterruptFlagFIFO_AlmostFull397:
	BCLR	FIFOIF_bit, BitPos(FIFOIF_bit+0)
L__dsPIC33_CAN_InterruptFlagFIFO_AlmostFull177:
;dsPIC33_CAN.mpas,1333 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_CAN_InterruptFlagFIFO_AlmostFull:
	RETURN
; end of _dsPIC33_CAN_InterruptFlagFIFO_AlmostFull

_dsPIC33_CAN_InterruptFlagRXBufferOverflow:

;dsPIC33_CAN.mpas,1345 :: 		begin
;dsPIC33_CAN.mpas,1346 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,1347 :: 		Result := RBOVIF_bit = 1;
	CLR.B	W0
	BTSC	RBOVIF_bit, BitPos(RBOVIF_bit+0)
	INC.B	W0
; Result start address is: 2 (W1)
	CP.B	W0, #1
	CLR	W1
	BRA NZ	L__dsPIC33_CAN_InterruptFlagRXBufferOverflow399
	COM	W1
L__dsPIC33_CAN_InterruptFlagRXBufferOverflow399:
;dsPIC33_CAN.mpas,1348 :: 		if DoReset then RBOVIF_bit := 0;
	CP0	W10
	BRA NZ	L__dsPIC33_CAN_InterruptFlagRXBufferOverflow400
	GOTO	L__dsPIC33_CAN_InterruptFlagRXBufferOverflow181
L__dsPIC33_CAN_InterruptFlagRXBufferOverflow400:
	BCLR	RBOVIF_bit, BitPos(RBOVIF_bit+0)
L__dsPIC33_CAN_InterruptFlagRXBufferOverflow181:
;dsPIC33_CAN.mpas,1349 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_CAN_InterruptFlagRXBufferOverflow:
	RETURN
; end of _dsPIC33_CAN_InterruptFlagRXBufferOverflow

_dsPIC33_CAN_InterruptFlagRXBuffer:

;dsPIC33_CAN.mpas,1361 :: 		begin
;dsPIC33_CAN.mpas,1362 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,1363 :: 		Result := RBIF_bit = 1;
	CLR.B	W0
	BTSC	RBIF_bit, BitPos(RBIF_bit+0)
	INC.B	W0
; Result start address is: 2 (W1)
	CP.B	W0, #1
	CLR	W1
	BRA NZ	L__dsPIC33_CAN_InterruptFlagRXBuffer402
	COM	W1
L__dsPIC33_CAN_InterruptFlagRXBuffer402:
;dsPIC33_CAN.mpas,1364 :: 		if DoReset then
	CP0	W10
	BRA NZ	L__dsPIC33_CAN_InterruptFlagRXBuffer403
	GOTO	L__dsPIC33_CAN_InterruptFlagRXBuffer185
L__dsPIC33_CAN_InterruptFlagRXBuffer403:
;dsPIC33_CAN.mpas,1365 :: 		RBIF_bit := 0;
	BCLR	RBIF_bit, BitPos(RBIF_bit+0)
L__dsPIC33_CAN_InterruptFlagRXBuffer185:
;dsPIC33_CAN.mpas,1366 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_CAN_InterruptFlagRXBuffer:
	RETURN
; end of _dsPIC33_CAN_InterruptFlagRXBuffer

_dsPIC33_CAN_InterruptFlagTXBuffer:

;dsPIC33_CAN.mpas,1378 :: 		begin
;dsPIC33_CAN.mpas,1379 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,1380 :: 		Result := TBIF_bit = 1;
	CLR.B	W0
	BTSC	TBIF_bit, BitPos(TBIF_bit+0)
	INC.B	W0
; Result start address is: 2 (W1)
	CP.B	W0, #1
	CLR	W1
	BRA NZ	L__dsPIC33_CAN_InterruptFlagTXBuffer405
	COM	W1
L__dsPIC33_CAN_InterruptFlagTXBuffer405:
;dsPIC33_CAN.mpas,1381 :: 		if DoReset then
	CP0	W10
	BRA NZ	L__dsPIC33_CAN_InterruptFlagTXBuffer406
	GOTO	L__dsPIC33_CAN_InterruptFlagTXBuffer189
L__dsPIC33_CAN_InterruptFlagTXBuffer406:
;dsPIC33_CAN.mpas,1382 :: 		TBIF_bit := 0;
	BCLR	TBIF_bit, BitPos(TBIF_bit+0)
L__dsPIC33_CAN_InterruptFlagTXBuffer189:
;dsPIC33_CAN.mpas,1383 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_CAN_InterruptFlagTXBuffer:
	RETURN
; end of _dsPIC33_CAN_InterruptFlagTXBuffer

_dsPIC33_CAN_InvalidMessageInterrupt:

;dsPIC33_CAN.mpas,1396 :: 		begin
;dsPIC33_CAN.mpas,1397 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,1398 :: 		if Enable then IVRIE_bit := 1 else IVRIE_bit := 0
	CP0	W10
	BRA NZ	L__dsPIC33_CAN_InvalidMessageInterrupt408
	GOTO	L__dsPIC33_CAN_InvalidMessageInterrupt193
L__dsPIC33_CAN_InvalidMessageInterrupt408:
	BSET	IVRIE_bit, BitPos(IVRIE_bit+0)
	GOTO	L__dsPIC33_CAN_InvalidMessageInterrupt194
L__dsPIC33_CAN_InvalidMessageInterrupt193:
	BCLR	IVRIE_bit, BitPos(IVRIE_bit+0)
L__dsPIC33_CAN_InvalidMessageInterrupt194:
;dsPIC33_CAN.mpas,1399 :: 		end;
L_end_dsPIC33_CAN_InvalidMessageInterrupt:
	RETURN
; end of _dsPIC33_CAN_InvalidMessageInterrupt

_dsPIC33_CAN_BusWakeUpInterrupt:

;dsPIC33_CAN.mpas,1411 :: 		begin
;dsPIC33_CAN.mpas,1412 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,1413 :: 		if Enable then WAKIE_bit := 1 else WAKIE_bit := 0
	CP0	W10
	BRA NZ	L__dsPIC33_CAN_BusWakeUpInterrupt410
	GOTO	L__dsPIC33_CAN_BusWakeUpInterrupt197
L__dsPIC33_CAN_BusWakeUpInterrupt410:
	BSET	WAKIE_bit, BitPos(WAKIE_bit+0)
	GOTO	L__dsPIC33_CAN_BusWakeUpInterrupt198
L__dsPIC33_CAN_BusWakeUpInterrupt197:
	BCLR	WAKIE_bit, BitPos(WAKIE_bit+0)
L__dsPIC33_CAN_BusWakeUpInterrupt198:
;dsPIC33_CAN.mpas,1414 :: 		end;
L_end_dsPIC33_CAN_BusWakeUpInterrupt:
	RETURN
; end of _dsPIC33_CAN_BusWakeUpInterrupt

_dsPIC33_CAN_ErrorInterrupt:

;dsPIC33_CAN.mpas,1426 :: 		begin
;dsPIC33_CAN.mpas,1427 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,1428 :: 		if Enable then ERRIE_bit := 1 else ERRIE_bit := 0
	CP0	W10
	BRA NZ	L__dsPIC33_CAN_ErrorInterrupt412
	GOTO	L__dsPIC33_CAN_ErrorInterrupt201
L__dsPIC33_CAN_ErrorInterrupt412:
	BSET	ERRIE_bit, BitPos(ERRIE_bit+0)
	GOTO	L__dsPIC33_CAN_ErrorInterrupt202
L__dsPIC33_CAN_ErrorInterrupt201:
	BCLR	ERRIE_bit, BitPos(ERRIE_bit+0)
L__dsPIC33_CAN_ErrorInterrupt202:
;dsPIC33_CAN.mpas,1429 :: 		end;
L_end_dsPIC33_CAN_ErrorInterrupt:
	RETURN
; end of _dsPIC33_CAN_ErrorInterrupt

_dsPIC33_CAN_FIFO_AlmostFullInterrupt:

;dsPIC33_CAN.mpas,1441 :: 		begin
;dsPIC33_CAN.mpas,1442 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,1443 :: 		if Enable then FIFOIE_bit := 1 else FIFOIE_bit := 0
	CP0	W10
	BRA NZ	L__dsPIC33_CAN_FIFO_AlmostFullInterrupt414
	GOTO	L__dsPIC33_CAN_FIFO_AlmostFullInterrupt205
L__dsPIC33_CAN_FIFO_AlmostFullInterrupt414:
	BSET	FIFOIE_bit, BitPos(FIFOIE_bit+0)
	GOTO	L__dsPIC33_CAN_FIFO_AlmostFullInterrupt206
L__dsPIC33_CAN_FIFO_AlmostFullInterrupt205:
	BCLR	FIFOIE_bit, BitPos(FIFOIE_bit+0)
L__dsPIC33_CAN_FIFO_AlmostFullInterrupt206:
;dsPIC33_CAN.mpas,1444 :: 		end;
L_end_dsPIC33_CAN_FIFO_AlmostFullInterrupt:
	RETURN
; end of _dsPIC33_CAN_FIFO_AlmostFullInterrupt

_dsPIC33_CAN_RXBufferOverflowInterrupt:

;dsPIC33_CAN.mpas,1456 :: 		begin
;dsPIC33_CAN.mpas,1457 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,1458 :: 		if Enable then
	CP0	W10
	BRA NZ	L__dsPIC33_CAN_RXBufferOverflowInterrupt416
	GOTO	L__dsPIC33_CAN_RXBufferOverflowInterrupt209
L__dsPIC33_CAN_RXBufferOverflowInterrupt416:
;dsPIC33_CAN.mpas,1459 :: 		RBOVIE_bit := 1
	BSET	RBOVIE_bit, BitPos(RBOVIE_bit+0)
	GOTO	L__dsPIC33_CAN_RXBufferOverflowInterrupt210
;dsPIC33_CAN.mpas,1460 :: 		else
L__dsPIC33_CAN_RXBufferOverflowInterrupt209:
;dsPIC33_CAN.mpas,1461 :: 		RBOVIE_bit := 0
	BCLR	RBOVIE_bit, BitPos(RBOVIE_bit+0)
L__dsPIC33_CAN_RXBufferOverflowInterrupt210:
;dsPIC33_CAN.mpas,1462 :: 		end;
L_end_dsPIC33_CAN_RXBufferOverflowInterrupt:
	RETURN
; end of _dsPIC33_CAN_RXBufferOverflowInterrupt

_dsPIC33_CAN_RXBufferInterrupt:

;dsPIC33_CAN.mpas,1474 :: 		begin
;dsPIC33_CAN.mpas,1475 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,1476 :: 		if Enable then RBIE_bit := 1 else RBIE_bit := 0
	CP0	W10
	BRA NZ	L__dsPIC33_CAN_RXBufferInterrupt418
	GOTO	L__dsPIC33_CAN_RXBufferInterrupt213
L__dsPIC33_CAN_RXBufferInterrupt418:
	BSET	RBIE_bit, BitPos(RBIE_bit+0)
	GOTO	L__dsPIC33_CAN_RXBufferInterrupt214
L__dsPIC33_CAN_RXBufferInterrupt213:
	BCLR	RBIE_bit, BitPos(RBIE_bit+0)
L__dsPIC33_CAN_RXBufferInterrupt214:
;dsPIC33_CAN.mpas,1477 :: 		end;
L_end_dsPIC33_CAN_RXBufferInterrupt:
	RETURN
; end of _dsPIC33_CAN_RXBufferInterrupt

_dsPIC33_CAN_TXBufferInterrupt:

;dsPIC33_CAN.mpas,1489 :: 		begin
;dsPIC33_CAN.mpas,1490 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,1491 :: 		if Enable then TBIE_bit := 1 else TBIE_bit := 0
	CP0	W10
	BRA NZ	L__dsPIC33_CAN_TXBufferInterrupt420
	GOTO	L__dsPIC33_CAN_TXBufferInterrupt217
L__dsPIC33_CAN_TXBufferInterrupt420:
	BSET	TBIE_bit, BitPos(TBIE_bit+0)
	GOTO	L__dsPIC33_CAN_TXBufferInterrupt218
L__dsPIC33_CAN_TXBufferInterrupt217:
	BCLR	TBIE_bit, BitPos(TBIE_bit+0)
L__dsPIC33_CAN_TXBufferInterrupt218:
;dsPIC33_CAN.mpas,1492 :: 		end;
L_end_dsPIC33_CAN_TXBufferInterrupt:
	RETURN
; end of _dsPIC33_CAN_TXBufferInterrupt

_dsPIC33_CAN_SetBufferSize:

;dsPIC33_CAN.mpas,1511 :: 		begin
;dsPIC33_CAN.mpas,1512 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,1513 :: 		C1FCTRL := C1FCTRL and $1FFF;  // Clear the bottom bits which = 4 Buffers reserved
	MOV	#8191, W1
	MOV	#lo_addr(C1FCTRL), W0
	AND	W1, [W0], [W0]
;dsPIC33_CAN.mpas,1515 :: 		6: C1FCTRL :=  C1FCTRL or $2000;   // 6 Buffers
	CP	W10, #6
	BRA Z	L__dsPIC33_CAN_SetBufferSize422
	GOTO	L__dsPIC33_CAN_SetBufferSize223
L__dsPIC33_CAN_SetBufferSize422:
	MOV	#8192, W1
	MOV	#lo_addr(C1FCTRL), W0
	IOR	W1, [W0], [W0]
	GOTO	L__dsPIC33_CAN_SetBufferSize220
L__dsPIC33_CAN_SetBufferSize223:
;dsPIC33_CAN.mpas,1516 :: 		8: C1FCTRL :=  C1FCTRL or $4000;   // 8 Buffers
	CP	W10, #8
	BRA Z	L__dsPIC33_CAN_SetBufferSize423
	GOTO	L__dsPIC33_CAN_SetBufferSize226
L__dsPIC33_CAN_SetBufferSize423:
	MOV	#16384, W1
	MOV	#lo_addr(C1FCTRL), W0
	IOR	W1, [W0], [W0]
	GOTO	L__dsPIC33_CAN_SetBufferSize220
L__dsPIC33_CAN_SetBufferSize226:
;dsPIC33_CAN.mpas,1517 :: 		12: C1FCTRL := C1FCTRL or $6000;   // 12 Buffers
	CP	W10, #12
	BRA Z	L__dsPIC33_CAN_SetBufferSize424
	GOTO	L__dsPIC33_CAN_SetBufferSize229
L__dsPIC33_CAN_SetBufferSize424:
	MOV	#24576, W1
	MOV	#lo_addr(C1FCTRL), W0
	IOR	W1, [W0], [W0]
	GOTO	L__dsPIC33_CAN_SetBufferSize220
L__dsPIC33_CAN_SetBufferSize229:
;dsPIC33_CAN.mpas,1518 :: 		16: C1FCTRL := C1FCTRL or $8000;   // 16 Buffers
	CP	W10, #16
	BRA Z	L__dsPIC33_CAN_SetBufferSize425
	GOTO	L__dsPIC33_CAN_SetBufferSize232
L__dsPIC33_CAN_SetBufferSize425:
	MOV	#32768, W1
	MOV	#lo_addr(C1FCTRL), W0
	IOR	W1, [W0], [W0]
	GOTO	L__dsPIC33_CAN_SetBufferSize220
L__dsPIC33_CAN_SetBufferSize232:
;dsPIC33_CAN.mpas,1519 :: 		24: C1FCTRL := C1FCTRL or $A000;   // 24 Buffers
	CP	W10, #24
	BRA Z	L__dsPIC33_CAN_SetBufferSize426
	GOTO	L__dsPIC33_CAN_SetBufferSize235
L__dsPIC33_CAN_SetBufferSize426:
	MOV	#40960, W1
	MOV	#lo_addr(C1FCTRL), W0
	IOR	W1, [W0], [W0]
	GOTO	L__dsPIC33_CAN_SetBufferSize220
L__dsPIC33_CAN_SetBufferSize235:
;dsPIC33_CAN.mpas,1520 :: 		32: C1FCTRL := C1FCTRL or $C000;   // 32 Buffers
	MOV	#32, W0
	CP	W10, W0
	BRA Z	L__dsPIC33_CAN_SetBufferSize427
	GOTO	L__dsPIC33_CAN_SetBufferSize238
L__dsPIC33_CAN_SetBufferSize427:
	MOV	#49152, W1
	MOV	#lo_addr(C1FCTRL), W0
	IOR	W1, [W0], [W0]
	GOTO	L__dsPIC33_CAN_SetBufferSize220
L__dsPIC33_CAN_SetBufferSize238:
L__dsPIC33_CAN_SetBufferSize220:
;dsPIC33_CAN.mpas,1522 :: 		end;
L_end_dsPIC33_CAN_SetBufferSize:
	RETURN
; end of _dsPIC33_CAN_SetBufferSize

_dsPIC33_CAN_CalculateTransmitCountForRegAddressWithAutoIncrement:

;dsPIC33_CAN.mpas,1536 :: 		begin
;dsPIC33_CAN.mpas,1537 :: 		if Buffer^.Word0 and $0001 = 0 then
	MOV	[W10], W0
	AND	W0, #1, W0
	CP	W0, #0
	BRA Z	L__dsPIC33_CAN_CalculateTransmitCountForRegAddressWithAutoIncrement429
	GOTO	L__dsPIC33_CAN_CalculateTransmitCountForRegAddressWithAutoIncrement241
L__dsPIC33_CAN_CalculateTransmitCountForRegAddressWithAutoIncrement429:
;dsPIC33_CAN.mpas,1539 :: 		Result := 2;
; Result start address is: 2 (W1)
	MOV	#2, W1
;dsPIC33_CAN.mpas,1540 :: 		ByteCount := Buffer^.Word1 and $000F;
	ADD	W10, #2, W0
	MOV	[W0], W0
; ByteCount start address is: 0 (W0)
	AND	W0, #15, W0
;dsPIC33_CAN.mpas,1541 :: 		end else
; ByteCount end address is: 0 (W0)
; Result end address is: 2 (W1)
	GOTO	L__dsPIC33_CAN_CalculateTransmitCountForRegAddressWithAutoIncrement242
L__dsPIC33_CAN_CalculateTransmitCountForRegAddressWithAutoIncrement241:
;dsPIC33_CAN.mpas,1543 :: 		Result := 3;
; Result start address is: 2 (W1)
	MOV	#3, W1
;dsPIC33_CAN.mpas,1544 :: 		ByteCount := Buffer^.Word2 and $000F;
	ADD	W10, #4, W0
	MOV	[W0], W0
; ByteCount start address is: 0 (W0)
	AND	W0, #15, W0
; ByteCount end address is: 0 (W0)
; Result end address is: 2 (W1)
;dsPIC33_CAN.mpas,1545 :: 		end;
L__dsPIC33_CAN_CalculateTransmitCountForRegAddressWithAutoIncrement242:
;dsPIC33_CAN.mpas,1547 :: 		if ByteCount > 5 then
; ByteCount start address is: 0 (W0)
; Result start address is: 2 (W1)
	CP	W0, #5
	BRA GTU	L__dsPIC33_CAN_CalculateTransmitCountForRegAddressWithAutoIncrement430
	GOTO	L__dsPIC33_CAN_CalculateTransmitCountForRegAddressWithAutoIncrement244
L__dsPIC33_CAN_CalculateTransmitCountForRegAddressWithAutoIncrement430:
; ByteCount end address is: 0 (W0)
;dsPIC33_CAN.mpas,1548 :: 		Result := Result + 4
; Result start address is: 2 (W1)
	ADD	W1, #4, W1
; Result end address is: 2 (W1)
	GOTO	L__dsPIC33_CAN_CalculateTransmitCountForRegAddressWithAutoIncrement245
;dsPIC33_CAN.mpas,1549 :: 		else if ByteCount > 3 then
L__dsPIC33_CAN_CalculateTransmitCountForRegAddressWithAutoIncrement244:
; ByteCount start address is: 0 (W0)
	CP	W0, #3
	BRA GTU	L__dsPIC33_CAN_CalculateTransmitCountForRegAddressWithAutoIncrement431
	GOTO	L__dsPIC33_CAN_CalculateTransmitCountForRegAddressWithAutoIncrement247
L__dsPIC33_CAN_CalculateTransmitCountForRegAddressWithAutoIncrement431:
; ByteCount end address is: 0 (W0)
;dsPIC33_CAN.mpas,1550 :: 		Result := Result + 3
; Result start address is: 0 (W0)
	ADD	W1, #3, W0
; Result end address is: 2 (W1)
	MOV	W0, W1
; Result end address is: 0 (W0)
	GOTO	L__dsPIC33_CAN_CalculateTransmitCountForRegAddressWithAutoIncrement248
;dsPIC33_CAN.mpas,1551 :: 		else if ByteCount > 1 then
L__dsPIC33_CAN_CalculateTransmitCountForRegAddressWithAutoIncrement247:
; Result start address is: 2 (W1)
; ByteCount start address is: 0 (W0)
	CP	W0, #1
	BRA GTU	L__dsPIC33_CAN_CalculateTransmitCountForRegAddressWithAutoIncrement432
	GOTO	L__dsPIC33_CAN_CalculateTransmitCountForRegAddressWithAutoIncrement250
L__dsPIC33_CAN_CalculateTransmitCountForRegAddressWithAutoIncrement432:
; ByteCount end address is: 0 (W0)
;dsPIC33_CAN.mpas,1552 :: 		Result := Result + 2
; Result start address is: 0 (W0)
	ADD	W1, #2, W0
; Result end address is: 2 (W1)
; Result end address is: 0 (W0)
	GOTO	L__dsPIC33_CAN_CalculateTransmitCountForRegAddressWithAutoIncrement251
;dsPIC33_CAN.mpas,1553 :: 		else
L__dsPIC33_CAN_CalculateTransmitCountForRegAddressWithAutoIncrement250:
;dsPIC33_CAN.mpas,1554 :: 		Result := Result + 1;
; Result start address is: 2 (W1)
; Result start address is: 0 (W0)
	ADD	W1, #1, W0
; Result end address is: 2 (W1)
; Result end address is: 0 (W0)
L__dsPIC33_CAN_CalculateTransmitCountForRegAddressWithAutoIncrement251:
; Result start address is: 0 (W0)
	MOV	W0, W1
; Result end address is: 0 (W0)
L__dsPIC33_CAN_CalculateTransmitCountForRegAddressWithAutoIncrement248:
; Result start address is: 2 (W1)
; Result end address is: 2 (W1)
L__dsPIC33_CAN_CalculateTransmitCountForRegAddressWithAutoIncrement245:
;dsPIC33_CAN.mpas,1555 :: 		end;
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_CAN_CalculateTransmitCountForRegAddressWithAutoIncrement:
	RETURN
; end of _dsPIC33_CAN_CalculateTransmitCountForRegAddressWithAutoIncrement

_dsPIC33_CAN_HardwareFIFO_SetFilter:

;dsPIC33_CAN.mpas,1570 :: 		begin
;dsPIC33_CAN.mpas,1571 :: 		dsPIC33_CAN_RegisterBufferWithFilter(FilterNumber, $000F);
	PUSH	W11
	MOV	#15, W11
	CALL	_dsPIC33_CAN_RegisterBufferWithFilter
;dsPIC33_CAN.mpas,1572 :: 		end;
L_end_dsPIC33_CAN_HardwareFIFO_SetFilter:
	POP	W11
	RETURN
; end of _dsPIC33_CAN_HardwareFIFO_SetFilter

_dsPIC33_CAN_HardwareFIFO_SetStartBuffer:

;dsPIC33_CAN.mpas,1586 :: 		begin
;dsPIC33_CAN.mpas,1587 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,1588 :: 		C1FCTRL := (C1FCTRL and $FFE0) or StartBuffer
	MOV	C1FCTRL, W1
	MOV	#65504, W0
	AND	W1, W0, W1
	MOV	#lo_addr(C1FCTRL), W0
	IOR	W1, W10, [W0]
;dsPIC33_CAN.mpas,1589 :: 		end;
L_end_dsPIC33_CAN_HardwareFIFO_SetStartBuffer:
	RETURN
; end of _dsPIC33_CAN_HardwareFIFO_SetStartBuffer

_dsPIC33_CAN_HardwareFIFO_GetCurrentBuffer:

;dsPIC33_CAN.mpas,1603 :: 		begin
;dsPIC33_CAN.mpas,1604 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,1605 :: 		Result := (C1FIFO shr 8) and $0007
	MOV	C1FIFO, WREG
	LSR	W0, #8, W0
; Result start address is: 2 (W1)
	AND	W0, #7, W1
;dsPIC33_CAN.mpas,1606 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_CAN_HardwareFIFO_GetCurrentBuffer:
	RETURN
; end of _dsPIC33_CAN_HardwareFIFO_GetCurrentBuffer

_dsPIC33_CAN_HardwareFIFO_GetNextBuffer:

;dsPIC33_CAN.mpas,1621 :: 		begin
;dsPIC33_CAN.mpas,1622 :: 		ClearWindowSelectBit;
	CALL	dsPIC33_CAN_ClearWindowSelectBit
;dsPIC33_CAN.mpas,1623 :: 		Result := C1FIFO and $001F;
	MOV	C1FIFO, WREG
; Result start address is: 2 (W1)
	AND	W0, #31, W1
;dsPIC33_CAN.mpas,1624 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_dsPIC33_CAN_HardwareFIFO_GetNextBuffer:
	RETURN
; end of _dsPIC33_CAN_HardwareFIFO_GetNextBuffer

_dsPIC33_CAN_RegisterBufferWithFilter:

;dsPIC33_CAN.mpas,1648 :: 		begin
;dsPIC33_CAN.mpas,1649 :: 		SetWindowSelectBit;
	CALL	dsPIC33_CAN_SetWindowSelectBit
;dsPIC33_CAN.mpas,1650 :: 		RegPtr := @C1BUFPNT1;
; RegPtr start address is: 4 (W2)
	MOV	#lo_addr(C1BUFPNT1), W2
;dsPIC33_CAN.mpas,1651 :: 		Offset := (FilterNumber div 4);
	LSR	W10, #2, W0
;dsPIC33_CAN.mpas,1652 :: 		RegPtr := RegPtr + Offset;  // Offset to the right C1BUFPNT register
	SL	W0, #1, W0
	ADD	W2, W0, W1
	MOV	W1, W2
;dsPIC33_CAN.mpas,1653 :: 		Offset := (FilterNumber mod 4) * 4;                  // Find the offset into the register
	AND	W10, #3, W0
	SL	W0, #2, W0
; Offset start address is: 6 (W3)
	MOV	W0, W3
;dsPIC33_CAN.mpas,1654 :: 		Mask := $000F;
; Mask start address is: 8 (W4)
	MOV	#15, W4
;dsPIC33_CAN.mpas,1655 :: 		Mask := Mask shl Offset;
	SL	W4, W0, W0
; Mask end address is: 8 (W4)
;dsPIC33_CAN.mpas,1656 :: 		Mask := not Mask;
	COM	W0
;dsPIC33_CAN.mpas,1657 :: 		RegPtr^ := RegPtr^ and Mask;                    // Clear the Pointer
	AND	W0, [W1], [W1]
;dsPIC33_CAN.mpas,1659 :: 		Mask := Mask shl Offset;
	SL	W11, W3, W0
; Offset end address is: 6 (W3)
;dsPIC33_CAN.mpas,1660 :: 		RegPtr^ := RegPtr^ or Mask;
	IOR	W0, [W2], [W2]
; RegPtr end address is: 4 (W2)
;dsPIC33_CAN.mpas,1661 :: 		end;
L_end_dsPIC33_CAN_RegisterBufferWithFilter:
	RETURN
; end of _dsPIC33_CAN_RegisterBufferWithFilter

_dsPIC33_CAN_AssociateFilterWithMask:

;dsPIC33_CAN.mpas,1678 :: 		begin
;dsPIC33_CAN.mpas,1680 :: 		if (FilterNumber < 16) and (MaskNumber < 3) then
	CP	W10, #16
	CLR	W1
	BRA GEU	L__dsPIC33_CAN_AssociateFilterWithMask439
	COM	W1
L__dsPIC33_CAN_AssociateFilterWithMask439:
	CP	W11, #3
	CLR	W0
	BRA GEU	L__dsPIC33_CAN_AssociateFilterWithMask440
	COM	W0
L__dsPIC33_CAN_AssociateFilterWithMask440:
	AND	W1, W0, W0
	BRA NZ	L__dsPIC33_CAN_AssociateFilterWithMask441
	GOTO	L__dsPIC33_CAN_AssociateFilterWithMask259
L__dsPIC33_CAN_AssociateFilterWithMask441:
;dsPIC33_CAN.mpas,1682 :: 		Offset := FilterNumber * 2;
	SL	W10, #1, W3
;dsPIC33_CAN.mpas,1683 :: 		M := $00000003;
; M start address is: 8 (W4)
	MOV	#3, W4
	MOV	#0, W5
;dsPIC33_CAN.mpas,1684 :: 		M := M shl Offset;
	MOV	W3, W2
	MOV.D	W4, W0
L__dsPIC33_CAN_AssociateFilterWithMask442:
	DEC	W2, W2
	BRA LT	L__dsPIC33_CAN_AssociateFilterWithMask443
	SL	W0, W0
	RLC	W1, W1
	BRA	L__dsPIC33_CAN_AssociateFilterWithMask442
L__dsPIC33_CAN_AssociateFilterWithMask443:
; M end address is: 8 (W4)
;dsPIC33_CAN.mpas,1685 :: 		M := not M;
; M start address is: 8 (W4)
	COM	W0, W4
	COM	W1, W5
;dsPIC33_CAN.mpas,1686 :: 		C1FMSKSEL1 := C1FMSKSEL1 and LoWord(M);      // Clear the bits for the Filter Association
	MOV	#lo_addr(C1FMSKSEL1), W0
	AND	W4, [W0], [W0]
;dsPIC33_CAN.mpas,1687 :: 		C1FMSKSEL2 := C1FMSKSEL2 and HiWord(M);
	MOV	#lo_addr(C1FMSKSEL2), W0
	AND	W5, [W0], [W0]
; M end address is: 8 (W4)
;dsPIC33_CAN.mpas,1689 :: 		M := MaskNumber;
; M start address is: 2 (W1)
	MOV	W11, W1
	CLR	W2
;dsPIC33_CAN.mpas,1690 :: 		M := M shl Offset;
	MOV	W3, W0
L__dsPIC33_CAN_AssociateFilterWithMask444:
	DEC	W0, W0
	BRA LT	L__dsPIC33_CAN_AssociateFilterWithMask445
	SL	W1, W1
	RLC	W2, W2
	BRA	L__dsPIC33_CAN_AssociateFilterWithMask444
L__dsPIC33_CAN_AssociateFilterWithMask445:
;dsPIC33_CAN.mpas,1691 :: 		C1FMSKSEL1 := C1FMSKSEL1 or LoWord(M);
	MOV	#lo_addr(C1FMSKSEL1), W0
	IOR	W1, [W0], [W0]
;dsPIC33_CAN.mpas,1692 :: 		C1FMSKSEL2 := C1FMSKSEL2 or HiWord(M);
	MOV	#lo_addr(C1FMSKSEL2), W0
	IOR	W2, [W0], [W0]
; M end address is: 2 (W1)
;dsPIC33_CAN.mpas,1693 :: 		end
L__dsPIC33_CAN_AssociateFilterWithMask259:
;dsPIC33_CAN.mpas,1694 :: 		end;
L_end_dsPIC33_CAN_AssociateFilterWithMask:
	RETURN
; end of _dsPIC33_CAN_AssociateFilterWithMask

_dsPIC33_CAN_SetFilter:
	LNK	#4

;dsPIC33_CAN.mpas,1711 :: 		begin
;dsPIC33_CAN.mpas,1712 :: 		if FilterNumber < 16 then
	PUSH	W10
	PUSH	W11
	PUSH	W12
	PUSH	W13
	MOV	W11, [W14+0]
	MOV	W12, [W14+2]
	CP.B	W10, #16
	BRA LTU	L__dsPIC33_CAN_SetFilter447
	GOTO	L__dsPIC33_CAN_SetFilter263
L__dsPIC33_CAN_SetFilter447:
;dsPIC33_CAN.mpas,1714 :: 		SetWindowSelectBit;
	CALL	dsPIC33_CAN_SetWindowSelectBit
;dsPIC33_CAN.mpas,1715 :: 		SIDRegPtr := @C1RXF0SID + (FilterNumber * 2);   // Increases by Word not bytes so 2 not 4
	ZE	W10, W0
	SL	W0, #1, W0
	SL	W0, #1, W1
	MOV	#lo_addr(C1RXF0SID), W0
; SIDRegPtr start address is: 8 (W4)
	ADD	W0, W1, W4
;dsPIC33_CAN.mpas,1716 :: 		EIDRegPtr := @C1RXF0EID + (FilterNumber * 2);   // Increases by Word not bytes so 2 not 4
	MOV	#lo_addr(C1RXF0EID), W0
; EIDRegPtr start address is: 10 (W5)
	ADD	W0, W1, W5
;dsPIC33_CAN.mpas,1718 :: 		ValidateCAN_ID(Filter);
	ADD	W14, #0, W0
	MOV	W0, W10
	CALL	dsPIC33_CAN_ValidateCAN_ID
;dsPIC33_CAN.mpas,1719 :: 		SetFilterMaskBits(SIDRegPtr, EIDRegPtr, Filter, ExtendedOnly);
	PUSH	W13
	MOV	W5, W11
	MOV	W4, W10
; EIDRegPtr end address is: 10 (W5)
; SIDRegPtr end address is: 8 (W4)
	MOV	[W14+0], W12
	MOV	[W14+2], W13
	CALL	dsPIC33_CAN_SetFilterMaskBits
	SUB	#2, W15
;dsPIC33_CAN.mpas,1720 :: 		end
L__dsPIC33_CAN_SetFilter263:
;dsPIC33_CAN.mpas,1721 :: 		end;
L_end_dsPIC33_CAN_SetFilter:
	POP	W13
	POP	W12
	POP	W11
	POP	W10
	ULNK
	RETURN
; end of _dsPIC33_CAN_SetFilter

_dsPIC33_CAN_SetMask:
	LNK	#4

;dsPIC33_CAN.mpas,1737 :: 		begin
;dsPIC33_CAN.mpas,1738 :: 		if MaskNumber < 3 then
	PUSH	W10
	PUSH	W11
	PUSH	W12
	PUSH	W13
	MOV	W11, [W14+0]
	MOV	W12, [W14+2]
	CP.B	W10, #3
	BRA LTU	L__dsPIC33_CAN_SetMask449
	GOTO	L__dsPIC33_CAN_SetMask267
L__dsPIC33_CAN_SetMask449:
;dsPIC33_CAN.mpas,1740 :: 		SetWindowSelectBit;
	CALL	dsPIC33_CAN_SetWindowSelectBit
;dsPIC33_CAN.mpas,1741 :: 		SIDRegPtr := @C1RXM0SID + (MaskNumber * 2);    // Increases by Word not bytes so 2 not 4
	ZE	W10, W0
	SL	W0, #1, W0
	SL	W0, #1, W1
	MOV	#lo_addr(C1RXM0SID), W0
; SIDRegPtr start address is: 8 (W4)
	ADD	W0, W1, W4
;dsPIC33_CAN.mpas,1742 :: 		EIDRegPtr := @C1RXM0EID + (MaskNumber * 2);   // Increases by Word not bytes so 2 not 4
	MOV	#lo_addr(C1RXM0EID), W0
; EIDRegPtr start address is: 10 (W5)
	ADD	W0, W1, W5
;dsPIC33_CAN.mpas,1744 :: 		ValidateCAN_ID(Mask);
	ADD	W14, #0, W0
	MOV	W0, W10
	CALL	dsPIC33_CAN_ValidateCAN_ID
;dsPIC33_CAN.mpas,1745 :: 		SetFilterMaskBits(SIDRegPtr, EIDRegPtr, Mask, ExtendedOnly);
	PUSH	W13
	MOV	W5, W11
	MOV	W4, W10
; EIDRegPtr end address is: 10 (W5)
; SIDRegPtr end address is: 8 (W4)
	MOV	[W14+0], W12
	MOV	[W14+2], W13
	CALL	dsPIC33_CAN_SetFilterMaskBits
	SUB	#2, W15
;dsPIC33_CAN.mpas,1746 :: 		end
L__dsPIC33_CAN_SetMask267:
;dsPIC33_CAN.mpas,1747 :: 		end;
L_end_dsPIC33_CAN_SetMask:
	POP	W13
	POP	W12
	POP	W11
	POP	W10
	ULNK
	RETURN
; end of _dsPIC33_CAN_SetMask

_dsPIC33_CAN_EnableDisableRXFilters:

;dsPIC33_CAN.mpas,1759 :: 		begin
;dsPIC33_CAN.mpas,1760 :: 		C1FEN1 := FilterMask
	MOV	W10, C1FEN1
;dsPIC33_CAN.mpas,1761 :: 		end;
L_end_dsPIC33_CAN_EnableDisableRXFilters:
	RETURN
; end of _dsPIC33_CAN_EnableDisableRXFilters

_dsPIC33_CAN_SetBaud:
	LNK	#0

;dsPIC33_CAN.mpas,1781 :: 		begin
;dsPIC33_CAN.mpas,1782 :: 		C1CFG1 := BRP or (SWJ shl 6);
; PRSEG start address is: 4 (W2)
	MOV	[W14-8], W2
; Sample3Times start address is: 6 (W3)
	MOV	[W14-10], W3
	SL	W10, #6, W1
	MOV	#lo_addr(C1CFG1), W0
	IOR	W11, W1, [W0]
;dsPIC33_CAN.mpas,1783 :: 		C1CFG2 := PRSEG or (SEG1PH shl 3) or (SEG2PH shl 8) or PROGRAMMABLE_SEG2PHTS;
	SL	W13, #3, W0
	IOR	W2, W0, W1
; PRSEG end address is: 4 (W2)
	SL	W12, #8, W0
	IOR	W1, W0, W2
	MOV	#128, W1
	MOV	#lo_addr(C1CFG2), W0
	IOR	W2, W1, [W0]
;dsPIC33_CAN.mpas,1784 :: 		if Sample3Times then
	CP0	W3
	BRA NZ	L__dsPIC33_CAN_SetBaud452
	GOTO	L__dsPIC33_CAN_SetBaud272
L__dsPIC33_CAN_SetBaud452:
; Sample3Times end address is: 6 (W3)
;dsPIC33_CAN.mpas,1785 :: 		C1CFG2 := C1CFG2 or SAMPLE_3_TIMES
	MOV	#64, W1
	MOV	#lo_addr(C1CFG2), W0
	IOR	W1, [W0], [W0]
L__dsPIC33_CAN_SetBaud272:
;dsPIC33_CAN.mpas,1786 :: 		end;
L_end_dsPIC33_CAN_SetBaud:
	ULNK
	RETURN
; end of _dsPIC33_CAN_SetBaud

_dsPIC33_CAN_EnterConfigMode:

;dsPIC33_CAN.mpas,1799 :: 		begin
;dsPIC33_CAN.mpas,1800 :: 		C1CTRL1 := (C1CTRL1 and $F8FF) or $0400;                                      // Set REQOP to Config (100)
	MOV	C1CTRL1, W1
	MOV	#63743, W0
	AND	W1, W0, W2
	MOV	#1024, W1
	MOV	#lo_addr(C1CTRL1), W0
	IOR	W2, W1, [W0]
;dsPIC33_CAN.mpas,1801 :: 		while (C1CTRL1 and $00E0) <> $0080 do;                                        // Poll OPMODE until it equals (100)
L__dsPIC33_CAN_EnterConfigMode276:
	MOV	#224, W1
	MOV	#lo_addr(C1CTRL1), W0
	AND	W1, [W0], W1
	MOV	#128, W0
	CP	W1, W0
	BRA Z	L__dsPIC33_CAN_EnterConfigMode454
	GOTO	L__dsPIC33_CAN_EnterConfigMode276
L__dsPIC33_CAN_EnterConfigMode454:
;dsPIC33_CAN.mpas,1802 :: 		end;
L_end_dsPIC33_CAN_EnterConfigMode:
	RETURN
; end of _dsPIC33_CAN_EnterConfigMode

_dsPIC33_CAN_EnterNormalMode:

;dsPIC33_CAN.mpas,1815 :: 		begin
;dsPIC33_CAN.mpas,1816 :: 		C1CTRL1 := C1CTRL1 and $F8FF;                                                 // Set REQOP to Normal (000)
	MOV	#63743, W1
	MOV	#lo_addr(C1CTRL1), W0
	AND	W1, [W0], [W0]
;dsPIC33_CAN.mpas,1817 :: 		while (C1CTRL1 and $00E0) <> $0000 do;                                        // Poll OPMODE until it equals (000)
L__dsPIC33_CAN_EnterNormalMode282:
	MOV	#224, W1
	MOV	#lo_addr(C1CTRL1), W0
	AND	W1, [W0], W0
	CP	W0, #0
	BRA Z	L__dsPIC33_CAN_EnterNormalMode456
	GOTO	L__dsPIC33_CAN_EnterNormalMode282
L__dsPIC33_CAN_EnterNormalMode456:
;dsPIC33_CAN.mpas,1818 :: 		end;
L_end_dsPIC33_CAN_EnterNormalMode:
	RETURN
; end of _dsPIC33_CAN_EnterNormalMode

_dsPIC33_CAN_AbortPendingTransmissions:

;dsPIC33_CAN.mpas,1830 :: 		begin
;dsPIC33_CAN.mpas,1831 :: 		ABAT_bit := 1;
	BSET	ABAT_bit, BitPos(ABAT_bit+0)
;dsPIC33_CAN.mpas,1832 :: 		end;
L_end_dsPIC33_CAN_AbortPendingTransmissions:
	RETURN
; end of _dsPIC33_CAN_AbortPendingTransmissions
