
_CANStorage_Initialize:

;CANStorage.mpas,112 :: 		begin
;CANStorage.mpas,113 :: 		CAN_Engine.State := 0;
	MOV	#lo_addr(_CAN_Engine), W1
	CLR	W0
	MOV.B	W0, [W1]
;CANStorage.mpas,114 :: 		CAN_Engine.InterruptLockCount := 0;
	MOV	#lo_addr(_CAN_Engine+1), W1
	CLR	W0
	MOV.B	W0, [W1]
;CANStorage.mpas,116 :: 		CAN_Engine.TX_CANBuffer.ID := 0;
	CLR	W0
	CLR	W1
	MOV	W0, _CAN_Engine+2
	MOV	W1, _CAN_Engine+4
;CANStorage.mpas,117 :: 		CAN_Engine.TX_CANBuffer.DataCount := 0;
	MOV	#lo_addr(_CAN_Engine+6), W1
	CLR	W0
	MOV.B	W0, [W1]
;CANStorage.mpas,118 :: 		CAN_Engine.TX_CANBuffer.State := 0;
	MOV	#lo_addr(_CAN_Engine+15), W1
	CLR	W0
	MOV.B	W0, [W1]
;CANStorage.mpas,120 :: 		CAN_Engine.TX_NMRAnetBuffer.ID := 0;
	CLR	W0
	CLR	W1
	MOV	W0, _CAN_Engine+16
	MOV	W1, _CAN_Engine+18
;CANStorage.mpas,121 :: 		CAN_Engine.TX_NMRAnetBuffer.DataCount := 0;
	MOV	#lo_addr(_CAN_Engine+20), W1
	CLR	W0
	MOV.B	W0, [W1]
;CANStorage.mpas,122 :: 		CAN_Engine.TX_NMRAnetBuffer.State := 0;
	MOV	#lo_addr(_CAN_Engine+29), W1
	CLR	W0
	MOV.B	W0, [W1]
;CANStorage.mpas,124 :: 		CAN_Engine.TX_AddressedErrorBuffer.ID := 0;
	CLR	W0
	CLR	W1
	MOV	W0, _CAN_Engine+30
	MOV	W1, _CAN_Engine+32
;CANStorage.mpas,125 :: 		CAN_Engine.TX_AddressedErrorBuffer.DataCount := 0;
	MOV	#lo_addr(_CAN_Engine+34), W1
	CLR	W0
	MOV.B	W0, [W1]
;CANStorage.mpas,126 :: 		CAN_Engine.TX_AddressedErrorBuffer.State := 0;
	MOV	#lo_addr(_CAN_Engine+43), W1
	CLR	W0
	MOV.B	W0, [W1]
;CANStorage.mpas,128 :: 		CAN_Engine.TX_DatagramRejected.ID := 0;
	CLR	W0
	CLR	W1
	MOV	W0, _CAN_Engine+44
	MOV	W1, _CAN_Engine+46
;CANStorage.mpas,129 :: 		CAN_Engine.TX_DatagramRejected.DataCount := 0;
	MOV	#lo_addr(_CAN_Engine+48), W1
	CLR	W0
	MOV.B	W0, [W1]
;CANStorage.mpas,130 :: 		CAN_Engine.TX_DatagramRejected.State := 0;
	MOV	#lo_addr(_CAN_Engine+57), W1
	CLR	W0
	MOV.B	W0, [W1]
;CANStorage.mpas,131 :: 		end;
L_end_CANStorage_Initialize:
	RETURN
; end of _CANStorage_Initialize

_CANStorage_FlushBuffers:

;CANStorage.mpas,134 :: 		begin
;CANStorage.mpas,135 :: 		if AliasID = 0 then
	CP	W10, #0
	BRA Z	L__CANStorage_FlushBuffers33
	GOTO	L__CANStorage_FlushBuffers3
L__CANStorage_FlushBuffers33:
;CANStorage.mpas,137 :: 		CAN_Engine.TX_CANBuffer.State := 0;
	MOV	#lo_addr(_CAN_Engine+15), W1
	CLR	W0
	MOV.B	W0, [W1]
;CANStorage.mpas,138 :: 		CAN_Engine.TX_NMRAnetBuffer.State := 0;
	MOV	#lo_addr(_CAN_Engine+29), W1
	CLR	W0
	MOV.B	W0, [W1]
;CANStorage.mpas,139 :: 		CAN_Engine.TX_AddressedErrorBuffer.State := 0;
	MOV	#lo_addr(_CAN_Engine+43), W1
	CLR	W0
	MOV.B	W0, [W1]
;CANStorage.mpas,140 :: 		CAN_Engine.TX_DatagramRejected.State := 0;
	MOV	#lo_addr(_CAN_Engine+57), W1
	CLR	W0
	MOV.B	W0, [W1]
;CANStorage.mpas,141 :: 		end else
	GOTO	L__CANStorage_FlushBuffers4
L__CANStorage_FlushBuffers3:
;CANStorage.mpas,143 :: 		if CAN_Engine.TX_CANBuffer.ID and MASK_SOURCE_ALIAS = AliasID then
	MOV	#4095, W4
	MOV	#0, W5
	MOV	#lo_addr(_CAN_Engine+2), W0
	AND	W4, [W0++], W2
	AND	W5, [W0--], W3
	MOV	W10, W0
	CLR	W1
	CP	W2, W0
	CPB	W3, W1
	BRA Z	L__CANStorage_FlushBuffers34
	GOTO	L__CANStorage_FlushBuffers6
L__CANStorage_FlushBuffers34:
;CANStorage.mpas,144 :: 		CAN_Engine.TX_CANBuffer.State := 0;
	MOV	#lo_addr(_CAN_Engine+15), W1
	CLR	W0
	MOV.B	W0, [W1]
L__CANStorage_FlushBuffers6:
;CANStorage.mpas,145 :: 		if CAN_Engine.TX_NMRAnetBuffer.ID and MASK_SOURCE_ALIAS = AliasID then
	MOV	#4095, W4
	MOV	#0, W5
	MOV	#lo_addr(_CAN_Engine+16), W0
	AND	W4, [W0++], W2
	AND	W5, [W0--], W3
	MOV	W10, W0
	CLR	W1
	CP	W2, W0
	CPB	W3, W1
	BRA Z	L__CANStorage_FlushBuffers35
	GOTO	L__CANStorage_FlushBuffers9
L__CANStorage_FlushBuffers35:
;CANStorage.mpas,146 :: 		CAN_Engine.TX_NMRAnetBuffer.State := 0;
	MOV	#lo_addr(_CAN_Engine+29), W1
	CLR	W0
	MOV.B	W0, [W1]
L__CANStorage_FlushBuffers9:
;CANStorage.mpas,147 :: 		if CAN_Engine.TX_AddressedErrorBuffer.ID and MASK_SOURCE_ALIAS = AliasID then
	MOV	#4095, W4
	MOV	#0, W5
	MOV	#lo_addr(_CAN_Engine+30), W0
	AND	W4, [W0++], W2
	AND	W5, [W0--], W3
	MOV	W10, W0
	CLR	W1
	CP	W2, W0
	CPB	W3, W1
	BRA Z	L__CANStorage_FlushBuffers36
	GOTO	L__CANStorage_FlushBuffers12
L__CANStorage_FlushBuffers36:
;CANStorage.mpas,148 :: 		CAN_Engine.TX_AddressedErrorBuffer.State := 0;
	MOV	#lo_addr(_CAN_Engine+43), W1
	CLR	W0
	MOV.B	W0, [W1]
L__CANStorage_FlushBuffers12:
;CANStorage.mpas,149 :: 		if CAN_Engine.TX_DatagramRejected.ID and MASK_SOURCE_ALIAS = AliasID then
	MOV	#4095, W4
	MOV	#0, W5
	MOV	#lo_addr(_CAN_Engine+44), W0
	AND	W4, [W0++], W2
	AND	W5, [W0--], W3
	MOV	W10, W0
	CLR	W1
	CP	W2, W0
	CPB	W3, W1
	BRA Z	L__CANStorage_FlushBuffers37
	GOTO	L__CANStorage_FlushBuffers15
L__CANStorage_FlushBuffers37:
;CANStorage.mpas,150 :: 		CAN_Engine.TX_DatagramRejected.State := 0;
	MOV	#lo_addr(_CAN_Engine+57), W1
	CLR	W0
	MOV.B	W0, [W1]
L__CANStorage_FlushBuffers15:
;CANStorage.mpas,151 :: 		end
L__CANStorage_FlushBuffers4:
;CANStorage.mpas,152 :: 		end;
L_end_CANStorage_FlushBuffers:
	RETURN
; end of _CANStorage_FlushBuffers

_CANStorage_NextToSend:

;CANStorage.mpas,155 :: 		begin
;CANStorage.mpas,157 :: 		if CAN_Engine.TX_CANBuffer.State and BS_ALLOCATED = BS_ALLOCATED then
	MOV	#lo_addr(_CAN_Engine+15), W0
	ZE	[W0], W0
	AND	W0, #2, W0
	CP	W0, #2
	BRA Z	L__CANStorage_NextToSend39
	GOTO	L__CANStorage_NextToSend19
L__CANStorage_NextToSend39:
;CANStorage.mpas,158 :: 		Result := @CAN_Engine.TX_CANBuffer
; Result start address is: 2 (W1)
	MOV	#lo_addr(_CAN_Engine+2), W1
; Result end address is: 2 (W1)
	GOTO	L__CANStorage_NextToSend20
;CANStorage.mpas,159 :: 		else
L__CANStorage_NextToSend19:
;CANStorage.mpas,160 :: 		if CAN_Engine.TX_NMRAnetBuffer.State and BS_ALLOCATED = BS_ALLOCATED then
	MOV	#lo_addr(_CAN_Engine+29), W0
	ZE	[W0], W0
	AND	W0, #2, W0
	CP	W0, #2
	BRA Z	L__CANStorage_NextToSend40
	GOTO	L__CANStorage_NextToSend22
L__CANStorage_NextToSend40:
;CANStorage.mpas,161 :: 		Result := @CAN_Engine.TX_NMRAnetBuffer
; Result start address is: 0 (W0)
	MOV	#lo_addr(_CAN_Engine+16), W0
	MOV	W0, W1
; Result end address is: 0 (W0)
	GOTO	L__CANStorage_NextToSend23
;CANStorage.mpas,162 :: 		else
L__CANStorage_NextToSend22:
;CANStorage.mpas,163 :: 		Result := PCANBuffer( nil);
; Result start address is: 0 (W0)
	MOV	#0, W0
; Result end address is: 0 (W0)
	MOV	W0, W1
L__CANStorage_NextToSend23:
; Result start address is: 2 (W1)
; Result end address is: 2 (W1)
L__CANStorage_NextToSend20:
;CANStorage.mpas,164 :: 		end;
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_CANStorage_NextToSend:
	RETURN
; end of _CANStorage_NextToSend

_CANStorage_NextHighPriorityToSend:

;CANStorage.mpas,167 :: 		begin
;CANStorage.mpas,168 :: 		if CAN_Engine.TX_AddressedErrorBuffer.State and BS_ALLOCATED = BS_ALLOCATED then
	MOV	#lo_addr(_CAN_Engine+43), W0
	ZE	[W0], W0
	AND	W0, #2, W0
	CP	W0, #2
	BRA Z	L__CANStorage_NextHighPriorityToSend42
	GOTO	L__CANStorage_NextHighPriorityToSend26
L__CANStorage_NextHighPriorityToSend42:
;CANStorage.mpas,169 :: 		Result := @CAN_Engine.TX_AddressedErrorBuffer
; Result start address is: 2 (W1)
	MOV	#lo_addr(_CAN_Engine+30), W1
; Result end address is: 2 (W1)
	GOTO	L__CANStorage_NextHighPriorityToSend27
;CANStorage.mpas,170 :: 		else
L__CANStorage_NextHighPriorityToSend26:
;CANStorage.mpas,171 :: 		if CAN_Engine.TX_DatagramRejected.State and BS_ALLOCATED = BS_ALLOCATED then
	MOV	#lo_addr(_CAN_Engine+57), W0
	ZE	[W0], W0
	AND	W0, #2, W0
	CP	W0, #2
	BRA Z	L__CANStorage_NextHighPriorityToSend43
	GOTO	L__CANStorage_NextHighPriorityToSend29
L__CANStorage_NextHighPriorityToSend43:
;CANStorage.mpas,172 :: 		Result := @CAN_Engine.TX_DatagramRejected
; Result start address is: 0 (W0)
	MOV	#lo_addr(_CAN_Engine+44), W0
	MOV	W0, W1
; Result end address is: 0 (W0)
	GOTO	L__CANStorage_NextHighPriorityToSend30
;CANStorage.mpas,173 :: 		else
L__CANStorage_NextHighPriorityToSend29:
;CANStorage.mpas,174 :: 		Result := PCANBuffer( nil);
; Result start address is: 0 (W0)
	MOV	#0, W0
; Result end address is: 0 (W0)
	MOV	W0, W1
L__CANStorage_NextHighPriorityToSend30:
; Result start address is: 2 (W1)
; Result end address is: 2 (W1)
L__CANStorage_NextHighPriorityToSend27:
;CANStorage.mpas,175 :: 		end;
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_CANStorage_NextHighPriorityToSend:
	RETURN
; end of _CANStorage_NextHighPriorityToSend
