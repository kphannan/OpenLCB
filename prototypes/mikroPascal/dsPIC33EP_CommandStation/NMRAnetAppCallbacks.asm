
_GetProxyData:

;NMRAnetAppCallbacks.mpas,135 :: 		begin
;NMRAnetAppCallbacks.mpas,136 :: 		Result := nil;
; Result start address is: 2 (W1)
	CLR	W1
;NMRAnetAppCallbacks.mpas,137 :: 		if Node <> nil then
	CP	W10, #0
	BRA NZ	L__GetProxyData181
	GOTO	L__GetProxyData178
L__GetProxyData181:
; Result end address is: 2 (W1)
;NMRAnetAppCallbacks.mpas,138 :: 		Result := PDccProxyData( Node^.RAMAddress);
	MOV	#42, W0
	ADD	W10, W0, W0
; Result start address is: 2 (W1)
	MOV	[W0], W1
; Result end address is: 2 (W1)
	GOTO	L__GetProxyData2
L__GetProxyData178:
;NMRAnetAppCallbacks.mpas,137 :: 		if Node <> nil then
;NMRAnetAppCallbacks.mpas,138 :: 		Result := PDccProxyData( Node^.RAMAddress);
L__GetProxyData2:
;NMRAnetAppCallbacks.mpas,139 :: 		end;
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_GetProxyData:
	RETURN
; end of _GetProxyData

_AppCallback_ConfigurationWrite:
	LNK	#0

;NMRAnetAppCallbacks.mpas,151 :: 		begin
;NMRAnetAppCallbacks.mpas,152 :: 		while _25AAxxxx_Busy(EEPROM_BANK_0) do
; MaxCount start address is: 6 (W3)
	MOV.B	[W14-8], W3
; MaxCount end address is: 6 (W3)
L__AppCallback_ConfigurationWrite6:
; MaxCount start address is: 6 (W3)
	PUSH	W10
	CLR	W10
	CALL	__25AAxxxx_Busy
	POP	W10
	CP0	W0
	BRA NZ	L__AppCallback_ConfigurationWrite183
	GOTO	L__AppCallback_ConfigurationWrite7
L__AppCallback_ConfigurationWrite183:
;NMRAnetAppCallbacks.mpas,153 :: 		Delay_us(10);
	MOV	#64, W7
L__AppCallback_ConfigurationWrite10:
	DEC	W7
	BRA NZ	L__AppCallback_ConfigurationWrite10
	NOP
	NOP
	GOTO	L__AppCallback_ConfigurationWrite6
L__AppCallback_ConfigurationWrite7:
;NMRAnetAppCallbacks.mpas,154 :: 		_25AAxxxx_Write(EEPROM_BANK_0, Node^.ConfigurationAddress + StartAddress, MaxCount, DataTarget);
	ADD	W10, #30, W2
	ADD	W12, [W2++], W0
	ADDC	W13, [W2--], W1
	PUSH.D	W12
; MaxCount end address is: 6 (W3)
	PUSH.D	W10
	PUSH	W11
	ZE	W3, W13
	CLR	W10
	MOV	W0, W11
	MOV	W1, W12
	CALL	__25AAxxxx_Write
	SUB	#2, W15
	POP.D	W10
	POP.D	W12
;NMRAnetAppCallbacks.mpas,155 :: 		end;
L_end_AppCallback_ConfigurationWrite:
	ULNK
	RETURN
; end of _AppCallback_ConfigurationWrite

_AppCallback_ConfigurationRead:
	LNK	#0

;NMRAnetAppCallbacks.mpas,167 :: 		begin
;NMRAnetAppCallbacks.mpas,168 :: 		while _25AAxxxx_Busy(EEPROM_BANK_0) do
; MaxCount start address is: 6 (W3)
	MOV.B	[W14-8], W3
; MaxCount end address is: 6 (W3)
L__AppCallback_ConfigurationRead14:
; MaxCount start address is: 6 (W3)
	PUSH	W10
	CLR	W10
	CALL	__25AAxxxx_Busy
	POP	W10
	CP0	W0
	BRA NZ	L__AppCallback_ConfigurationRead185
	GOTO	L__AppCallback_ConfigurationRead15
L__AppCallback_ConfigurationRead185:
;NMRAnetAppCallbacks.mpas,169 :: 		Delay_us(10);
	MOV	#64, W7
L__AppCallback_ConfigurationRead18:
	DEC	W7
	BRA NZ	L__AppCallback_ConfigurationRead18
	NOP
	NOP
	GOTO	L__AppCallback_ConfigurationRead14
L__AppCallback_ConfigurationRead15:
;NMRAnetAppCallbacks.mpas,171 :: 		Result := MaxCount;
; Result start address is: 8 (W4)
	MOV.B	W3, W4
;NMRAnetAppCallbacks.mpas,172 :: 		_25AAxxxx_Read(EEPROM_BANK_0, Node^.ConfigurationAddress + StartAddress, MaxCount, DataTarget);
	ADD	W10, #30, W2
	ADD	W12, [W2++], W0
	ADDC	W13, [W2--], W1
	PUSH.D	W12
; MaxCount end address is: 6 (W3)
	PUSH.D	W10
	PUSH	W11
	ZE	W3, W13
	CLR	W10
	MOV	W0, W11
	MOV	W1, W12
	CALL	__25AAxxxx_Read
	SUB	#2, W15
	POP.D	W10
	POP.D	W12
;NMRAnetAppCallbacks.mpas,173 :: 		end;
	MOV.B	W4, W0
; Result end address is: 8 (W4)
L_end_AppCallback_ConfigurationRead:
	ULNK
	RETURN
; end of _AppCallback_ConfigurationRead

_AppCallback_ConfigurationSize:

;NMRAnetAppCallbacks.mpas,179 :: 		begin
;NMRAnetAppCallbacks.mpas,180 :: 		Result := MAX_CONFIG_DATA;
; Result start address is: 2 (W1)
	MOV	#60, W1
;NMRAnetAppCallbacks.mpas,181 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_AppCallback_ConfigurationSize:
	RETURN
; end of _AppCallback_ConfigurationSize

_AppCallback_AssignConfigurationAddress:

;NMRAnetAppCallbacks.mpas,187 :: 		begin
;NMRAnetAppCallbacks.mpas,188 :: 		Node^.ConfigurationAddress := Generic32BitPointer( iNode * MAX_DCC_CFG_DATA);
	ADD	W10, #30, W5
	MOV	W11, W3
	CLR	W4
	MOV	#10, W2
	MOV	W3, W0
	MOV	W4, W1
L__AppCallback_AssignConfigurationAddress188:
	DEC	W2, W2
	BRA LT	L__AppCallback_AssignConfigurationAddress189
	SL	W0, W0
	RLC	W1, W1
	BRA	L__AppCallback_AssignConfigurationAddress188
L__AppCallback_AssignConfigurationAddress189:
	MOV	W0, [W5++]
	MOV	W1, [W5--]
;NMRAnetAppCallbacks.mpas,189 :: 		end;
L_end_AppCallback_AssignConfigurationAddress:
	RETURN
; end of _AppCallback_AssignConfigurationAddress

_AppCallback_AssignRAMAddress:

;NMRAnetAppCallbacks.mpas,195 :: 		begin
;NMRAnetAppCallbacks.mpas,196 :: 		Node^.RAMAddress := Generic32BitPointer( @VolatileData[iNode]);
	MOV	#42, W0
	ADD	W10, W0, W4
	MOV	#10, W0
	MUL.UU	W0, W11, W2
	MOV	#lo_addr(NMRAnetAppCallbacks_VolatileData), W0
	ADD	W0, W2, W0
	CLR	W1
	MOV.D	W0, [W4]
;NMRAnetAppCallbacks.mpas,197 :: 		end;
L_end_AppCallback_AssignRAMAddress:
	RETURN
; end of _AppCallback_AssignRAMAddress

_NMRAnetAppCallbacks_Initialize:

;NMRAnetAppCallbacks.mpas,205 :: 		begin
;NMRAnetAppCallbacks.mpas,206 :: 		for i := 0 to MAX_NODE_COUNT - 1 do
; i start address is: 8 (W4)
	CLR	W4
; i end address is: 8 (W4)
L__NMRAnetAppCallbacks_Initialize25:
;NMRAnetAppCallbacks.mpas,208 :: 		VolatileData[i].State := 0;
; i start address is: 8 (W4)
	MOV	#10, W0
	MUL.UU	W0, W4, W2
	MOV	#lo_addr(NMRAnetAppCallbacks_VolatileData), W0
	ADD	W0, W2, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetAppCallbacks.mpas,209 :: 		VolatileData[i].Speed := 0;
	MOV	#10, W0
	MUL.UU	W0, W4, W2
	MOV	#lo_addr(NMRAnetAppCallbacks_VolatileData), W0
	ADD	W0, W2, W0
	ADD	W0, #2, W1
	CLR	W0
	MOV	W0, [W1]
;NMRAnetAppCallbacks.mpas,210 :: 		VolatileData[i].Functions := 0;
	MOV	#10, W0
	MUL.UU	W0, W4, W2
	MOV	#lo_addr(NMRAnetAppCallbacks_VolatileData), W0
	ADD	W0, W2, W0
	ADD	W0, #4, W2
	CLR	W0
	CLR	W1
	MOV.D	W0, [W2]
;NMRAnetAppCallbacks.mpas,211 :: 		VolatileData[i].MsgFlags := 0;
	MOV	#10, W0
	MUL.UU	W0, W4, W2
	MOV	#lo_addr(NMRAnetAppCallbacks_VolatileData), W0
	ADD	W0, W2, W0
	ADD	W0, #1, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetAppCallbacks.mpas,212 :: 		VolatileData[i].Address := 0;
	MOV	#10, W0
	MUL.UU	W0, W4, W2
	MOV	#lo_addr(NMRAnetAppCallbacks_VolatileData), W0
	ADD	W0, W2, W0
	ADD	W0, #8, W1
	CLR	W0
	MOV	W0, [W1]
;NMRAnetAppCallbacks.mpas,213 :: 		end
	MOV	#64, W0
	CP	W4, W0
	BRA NZ	L__NMRAnetAppCallbacks_Initialize192
	GOTO	L__NMRAnetAppCallbacks_Initialize28
L__NMRAnetAppCallbacks_Initialize192:
; i start address is: 8 (W4)
	INC	W4
; i end address is: 8 (W4)
; i end address is: 8 (W4)
	GOTO	L__NMRAnetAppCallbacks_Initialize25
L__NMRAnetAppCallbacks_Initialize28:
;NMRAnetAppCallbacks.mpas,214 :: 		end;
L_end_NMRAnetAppCallbacks_Initialize:
	RETURN
; end of _NMRAnetAppCallbacks_Initialize

_AppCallback_NodeAllocate:

;NMRAnetAppCallbacks.mpas,219 :: 		begin
;NMRAnetAppCallbacks.mpas,220 :: 		ProxyData := GetProxyData(Node);
	CALL	_GetProxyData
; ProxyData start address is: 6 (W3)
	MOV	W0, W3
;NMRAnetAppCallbacks.mpas,221 :: 		ProxyData^.State := 0;
	CLR	W1
	MOV.B	W1, [W0]
;NMRAnetAppCallbacks.mpas,222 :: 		ProxyData^.Address := 0;
	ADD	W3, #8, W1
	CLR	W0
	MOV	W0, [W1]
;NMRAnetAppCallbacks.mpas,223 :: 		ProxyData^.MsgFlags := 0;
	ADD	W3, #1, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetAppCallbacks.mpas,224 :: 		ProxyData^.Speed := 0;
	ADD	W3, #2, W1
	CLR	W0
	MOV	W0, [W1]
;NMRAnetAppCallbacks.mpas,225 :: 		ProxyData^.Functions := 0;
	ADD	W3, #4, W2
; ProxyData end address is: 6 (W3)
	CLR	W0
	CLR	W1
	MOV.D	W0, [W2]
;NMRAnetAppCallbacks.mpas,226 :: 		end;
L_end_AppCallback_NodeAllocate:
	RETURN
; end of _AppCallback_NodeAllocate

NMRAnetAppCallbacks_SetProxyNodeDccAddressFlags:

;NMRAnetAppCallbacks.mpas,236 :: 		begin
;NMRAnetAppCallbacks.mpas,237 :: 		TestForSetFlag := True;
; TestForSetFlag start address is: 0 (W0)
	MOV	#65535, W0
;NMRAnetAppCallbacks.mpas,238 :: 		if Event <> nil then
	CP	W11, #0
	BRA NZ	L_NMRAnetAppCallbacks_SetProxyNodeDccAddressFlags195
	GOTO	L_NMRAnetAppCallbacks_SetProxyNodeDccAddressFlags179
L_NMRAnetAppCallbacks_SetProxyNodeDccAddressFlags195:
; TestForSetFlag end address is: 0 (W0)
;NMRAnetAppCallbacks.mpas,239 :: 		TestForSetFlag := (Event^[0] = $06) and (Event^[1] = $01);
	MOV.B	[W11], W0
	CP.B	W0, #6
	CLR	W1
	BRA NZ	L_NMRAnetAppCallbacks_SetProxyNodeDccAddressFlags196
	COM	W1
L_NMRAnetAppCallbacks_SetProxyNodeDccAddressFlags196:
	ADD	W11, #1, W0
	MOV.B	[W0], W0
	CP.B	W0, #1
	CLR	W0
	BRA NZ	L_NMRAnetAppCallbacks_SetProxyNodeDccAddressFlags197
	COM	W0
L_NMRAnetAppCallbacks_SetProxyNodeDccAddressFlags197:
; TestForSetFlag start address is: 0 (W0)
	AND	W1, W0, W0
; TestForSetFlag end address is: 0 (W0)
	GOTO	L_NMRAnetAppCallbacks_SetProxyNodeDccAddressFlags32
L_NMRAnetAppCallbacks_SetProxyNodeDccAddressFlags179:
;NMRAnetAppCallbacks.mpas,238 :: 		if Event <> nil then
;NMRAnetAppCallbacks.mpas,239 :: 		TestForSetFlag := (Event^[0] = $06) and (Event^[1] = $01);
L_NMRAnetAppCallbacks_SetProxyNodeDccAddressFlags32:
;NMRAnetAppCallbacks.mpas,241 :: 		if TestForSetFlag then
; TestForSetFlag start address is: 0 (W0)
	CP0	W0
	BRA NZ	L_NMRAnetAppCallbacks_SetProxyNodeDccAddressFlags198
	GOTO	L_NMRAnetAppCallbacks_SetProxyNodeDccAddressFlags35
L_NMRAnetAppCallbacks_SetProxyNodeDccAddressFlags198:
; TestForSetFlag end address is: 0 (W0)
;NMRAnetAppCallbacks.mpas,243 :: 		ProxyData := GetProxyData(Node);
	CALL	_GetProxyData
; ProxyData start address is: 4 (W2)
	MOV	W0, W2
;NMRAnetAppCallbacks.mpas,244 :: 		if ProxyData^.State and PS_ALLOCATED = PS_ALLOCATED then
	MOV.B	[W0], W0
	ZE	W0, W0
	AND	W0, #1, W0
	CP	W0, #1
	BRA Z	L_NMRAnetAppCallbacks_SetProxyNodeDccAddressFlags199
	GOTO	L_NMRAnetAppCallbacks_SetProxyNodeDccAddressFlags38
L_NMRAnetAppCallbacks_SetProxyNodeDccAddressFlags199:
;NMRAnetAppCallbacks.mpas,246 :: 		Address := (Event^[4] shl 8) or Event^[5];
	ADD	W11, #4, W0
	MOV.B	[W0], W0
	ZE	W0, W0
	SL	W0, #8, W1
	ADD	W11, #5, W0
	ZE	[W0], W0
	IOR	W1, W0, W1
;NMRAnetAppCallbacks.mpas,247 :: 		if ProxyData^.Address = Address then
	ADD	W2, #8, W0
	MOV	[W0], W0
	CP	W0, W1
	BRA Z	L_NMRAnetAppCallbacks_SetProxyNodeDccAddressFlags200
	GOTO	L_NMRAnetAppCallbacks_SetProxyNodeDccAddressFlags41
L_NMRAnetAppCallbacks_SetProxyNodeDccAddressFlags200:
;NMRAnetAppCallbacks.mpas,248 :: 		ProxyData^.MsgFlags := ProxyData^.MsgFlags or MSG_DCC_ADDRESS_ALLOCATED;
	ADD	W2, #1, W1
; ProxyData end address is: 4 (W2)
	ZE	[W1], W0
	IOR	W0, #1, W0
	MOV.B	W0, [W1]
L_NMRAnetAppCallbacks_SetProxyNodeDccAddressFlags41:
;NMRAnetAppCallbacks.mpas,249 :: 		end
L_NMRAnetAppCallbacks_SetProxyNodeDccAddressFlags38:
;NMRAnetAppCallbacks.mpas,250 :: 		end
L_NMRAnetAppCallbacks_SetProxyNodeDccAddressFlags35:
;NMRAnetAppCallbacks.mpas,251 :: 		end;
L_end_SetProxyNodeDccAddressFlags:
	RETURN
; end of NMRAnetAppCallbacks_SetProxyNodeDccAddressFlags

NMRAnetAppCallbacks_SetProxyNodeProducerFlags:

;NMRAnetAppCallbacks.mpas,257 :: 		begin
;NMRAnetAppCallbacks.mpas,260 :: 		EVENT_TRAIN_INDEX           : NMRAnetNode_SetProducerEventFlag(Node, EventIndex, EVENT_STATE_VALID);
	PUSH	W12
	CP	W11, #0
	BRA Z	L_NMRAnetAppCallbacks_SetProxyNodeProducerFlags202
	GOTO	L_NMRAnetAppCallbacks_SetProxyNodeProducerFlags47
L_NMRAnetAppCallbacks_SetProxyNodeProducerFlags202:
	MOV.B	#1, W12
	CALL	_NMRAnetNode_SetProducerEventFlag
	GOTO	L_NMRAnetAppCallbacks_SetProxyNodeProducerFlags44
L_NMRAnetAppCallbacks_SetProxyNodeProducerFlags47:
;NMRAnetAppCallbacks.mpas,261 :: 		EVENT_TRAIN_DCC_IDLE_INDEX  : begin
	CP	W11, #1
	BRA Z	L_NMRAnetAppCallbacks_SetProxyNodeProducerFlags203
	GOTO	L_NMRAnetAppCallbacks_SetProxyNodeProducerFlags50
L_NMRAnetAppCallbacks_SetProxyNodeProducerFlags203:
;NMRAnetAppCallbacks.mpas,262 :: 		if GetProxyData(Node)^.State and PS_ALLOCATED = PS_ALLOCATED then
	CALL	_GetProxyData
	MOV.B	[W0], W0
	ZE	W0, W0
	AND	W0, #1, W0
	CP	W0, #1
	BRA Z	L_NMRAnetAppCallbacks_SetProxyNodeProducerFlags204
	GOTO	L_NMRAnetAppCallbacks_SetProxyNodeProducerFlags52
L_NMRAnetAppCallbacks_SetProxyNodeProducerFlags204:
;NMRAnetAppCallbacks.mpas,263 :: 		NMRAnetNode_SetProducerEventFlag(Node, EventIndex, EVENT_STATE_INVALID)
	MOV.B	#2, W12
	CALL	_NMRAnetNode_SetProducerEventFlag
	GOTO	L_NMRAnetAppCallbacks_SetProxyNodeProducerFlags53
;NMRAnetAppCallbacks.mpas,264 :: 		else
L_NMRAnetAppCallbacks_SetProxyNodeProducerFlags52:
;NMRAnetAppCallbacks.mpas,265 :: 		NMRAnetNode_SetProducerEventFlag(Node, EventIndex, EVENT_STATE_VALID);
	MOV.B	#1, W12
	CALL	_NMRAnetNode_SetProducerEventFlag
L_NMRAnetAppCallbacks_SetProxyNodeProducerFlags53:
;NMRAnetAppCallbacks.mpas,266 :: 		end;
	GOTO	L_NMRAnetAppCallbacks_SetProxyNodeProducerFlags44
L_NMRAnetAppCallbacks_SetProxyNodeProducerFlags50:
;NMRAnetAppCallbacks.mpas,267 :: 		EVENT_TRAIN_DCC_INUSE_INDEX : begin
	CP	W11, #2
	BRA Z	L_NMRAnetAppCallbacks_SetProxyNodeProducerFlags205
	GOTO	L_NMRAnetAppCallbacks_SetProxyNodeProducerFlags56
L_NMRAnetAppCallbacks_SetProxyNodeProducerFlags205:
;NMRAnetAppCallbacks.mpas,268 :: 		if GetProxyData(Node)^.State and PS_ALLOCATED = PS_ALLOCATED then
	CALL	_GetProxyData
	MOV.B	[W0], W0
	ZE	W0, W0
	AND	W0, #1, W0
	CP	W0, #1
	BRA Z	L_NMRAnetAppCallbacks_SetProxyNodeProducerFlags206
	GOTO	L_NMRAnetAppCallbacks_SetProxyNodeProducerFlags58
L_NMRAnetAppCallbacks_SetProxyNodeProducerFlags206:
;NMRAnetAppCallbacks.mpas,269 :: 		NMRAnetNode_SetProducerEventFlag(Node, EventIndex, EVENT_STATE_VALID)
	MOV.B	#1, W12
	CALL	_NMRAnetNode_SetProducerEventFlag
	GOTO	L_NMRAnetAppCallbacks_SetProxyNodeProducerFlags59
;NMRAnetAppCallbacks.mpas,270 :: 		else
L_NMRAnetAppCallbacks_SetProxyNodeProducerFlags58:
;NMRAnetAppCallbacks.mpas,271 :: 		NMRAnetNode_SetProducerEventFlag(Node, EventIndex, EVENT_STATE_INVALID);
	MOV.B	#2, W12
	CALL	_NMRAnetNode_SetProducerEventFlag
L_NMRAnetAppCallbacks_SetProxyNodeProducerFlags59:
;NMRAnetAppCallbacks.mpas,273 :: 		else
	GOTO	L_NMRAnetAppCallbacks_SetProxyNodeProducerFlags44
L_NMRAnetAppCallbacks_SetProxyNodeProducerFlags56:
;NMRAnetAppCallbacks.mpas,274 :: 		NMRAnetNode_SetProducerEventFlag(Node, EventIndex, EVENT_STATE_UNKOWN);
	MOV.B	#3, W12
	CALL	_NMRAnetNode_SetProducerEventFlag
L_NMRAnetAppCallbacks_SetProxyNodeProducerFlags44:
;NMRAnetAppCallbacks.mpas,276 :: 		end;
L_end_SetProxyNodeProducerFlags:
	POP	W12
	RETURN
; end of NMRAnetAppCallbacks_SetProxyNodeProducerFlags

_AppCallback_ProducerIdentify:
	LNK	#6

;NMRAnetAppCallbacks.mpas,286 :: 		begin
;NMRAnetAppCallbacks.mpas,287 :: 		Result := True;                                                               // We handle the message
	PUSH	W10
	PUSH	W12
; Result start address is: 16 (W8)
	MOV	#65535, W8
;NMRAnetAppCallbacks.mpas,288 :: 		VNodeEventIndex := -1;
	MOV	#65535, W0
	MOV	W0, [W14+0]
;NMRAnetAppCallbacks.mpas,289 :: 		NodeEventIndex := -1;
	MOV	#65535, W0
	MOV	W0, [W14+2]
;NMRAnetAppCallbacks.mpas,290 :: 		VNodeEvent := NMRAnetUtilities_SupportsVNodeEventAsProducer(Event, VNodeEventIndex);
	ADD	W14, #0, W0
	PUSH	W11
	MOV	W11, W10
	MOV	W0, W11
	CALL	_NMRAnetUtilities_SupportsVNodeEventAsProducer
	POP	W11
; VNodeEvent start address is: 14 (W7)
	MOV	W0, W7
;NMRAnetAppCallbacks.mpas,291 :: 		NodeEvent := NMRAnetUtilities_SupportsEventAsProducer(Event, NodeEventIndex);
	ADD	W14, #2, W0
	PUSH	W11
	MOV	W11, W10
	MOV	W0, W11
	CALL	_NMRAnetUtilities_SupportsEventAsProducer
	POP	W11
; NodeEvent start address is: 12 (W6)
	MOV	W0, W6
;NMRAnetAppCallbacks.mpas,292 :: 		for i := 0 to Nodes.AllocatedCount - 1 do
; i start address is: 18 (W9)
	CLR	W9
; Result end address is: 16 (W8)
; i end address is: 18 (W9)
L__AppCallback_ProducerIdentify61:
; i start address is: 18 (W9)
; NodeEvent start address is: 12 (W6)
; NodeEvent end address is: 12 (W6)
; VNodeEvent start address is: 14 (W7)
; VNodeEvent end address is: 14 (W7)
; Result start address is: 16 (W8)
	MOV	_Nodes+3120, W0
	DEC	W0
	MOV	W0, [W14+4]
	CP	W9, W0
	BRA LE	L__AppCallback_ProducerIdentify208
	GOTO	L__AppCallback_ProducerIdentify65
L__AppCallback_ProducerIdentify208:
; NodeEvent end address is: 12 (W6)
; VNodeEvent end address is: 14 (W7)
;NMRAnetAppCallbacks.mpas,294 :: 		if NMRAnetNode_TestStateFlag(Nodes.AllocatedList[i], NS_VIRTUAL) then
; VNodeEvent start address is: 14 (W7)
; NodeEvent start address is: 12 (W6)
	SL	W9, #1, W1
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W0
	PUSH.D	W10
	MOV.B	#8, W11
	MOV	[W0], W10
	CALL	_NMRAnetNode_TestStateFlag
	POP.D	W10
	CP0	W0
	BRA NZ	L__AppCallback_ProducerIdentify209
	GOTO	L__AppCallback_ProducerIdentify67
L__AppCallback_ProducerIdentify209:
;NMRAnetAppCallbacks.mpas,296 :: 		if VNodeEvent then
	CP0	W7
	BRA NZ	L__AppCallback_ProducerIdentify210
	GOTO	L__AppCallback_ProducerIdentify70
L__AppCallback_ProducerIdentify210:
;NMRAnetAppCallbacks.mpas,297 :: 		SetProxyNodeProducerFlags(Nodes.AllocatedList[i], VNodeEventIndex);
	SL	W9, #1, W1
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W0
	PUSH.D	W10
	MOV	[W14+0], W11
	MOV	[W0], W10
	CALL	NMRAnetAppCallbacks_SetProxyNodeProducerFlags
	POP.D	W10
L__AppCallback_ProducerIdentify70:
;NMRAnetAppCallbacks.mpas,298 :: 		SetProxyNodeDccAddressFlags(Nodes.AllocatedList[i], Event);
	SL	W9, #1, W1
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W0
	PUSH	W10
	MOV	[W0], W10
	CALL	NMRAnetAppCallbacks_SetProxyNodeDccAddressFlags
	POP	W10
;NMRAnetAppCallbacks.mpas,299 :: 		end else
	GOTO	L__AppCallback_ProducerIdentify68
L__AppCallback_ProducerIdentify67:
;NMRAnetAppCallbacks.mpas,301 :: 		if NodeEvent then
	CP0	W6
	BRA NZ	L__AppCallback_ProducerIdentify211
	GOTO	L__AppCallback_ProducerIdentify73
L__AppCallback_ProducerIdentify211:
;NMRAnetAppCallbacks.mpas,302 :: 		NMRAnetNode_SetProducerEventFlag(Nodes.AllocatedList[0], NodeEventIndex, EVENT_STATE_UNKOWN);
	PUSH.D	W10
	MOV.B	#3, W12
	MOV	[W14+2], W11
	MOV	_Nodes+2990, W10
	CALL	_NMRAnetNode_SetProducerEventFlag
	POP.D	W10
L__AppCallback_ProducerIdentify73:
;NMRAnetAppCallbacks.mpas,303 :: 		end
L__AppCallback_ProducerIdentify68:
;NMRAnetAppCallbacks.mpas,304 :: 		end;
	MOV	[W14+4], W0
	CP	W9, W0
	BRA NZ	L__AppCallback_ProducerIdentify212
	GOTO	L__AppCallback_ProducerIdentify65
L__AppCallback_ProducerIdentify212:
; i start address is: 18 (W9)
	INC	W9
; i end address is: 18 (W9)
; NodeEvent end address is: 12 (W6)
; VNodeEvent end address is: 14 (W7)
; i end address is: 18 (W9)
	GOTO	L__AppCallback_ProducerIdentify61
L__AppCallback_ProducerIdentify65:
;NMRAnetAppCallbacks.mpas,305 :: 		end;
	MOV	W8, W0
; Result end address is: 16 (W8)
L_end_AppCallback_ProducerIdentify:
	POP	W12
	POP	W10
	ULNK
	RETURN
; end of _AppCallback_ProducerIdentify

_AppCallback_ConsumerIdentify:

;NMRAnetAppCallbacks.mpas,311 :: 		begin
;NMRAnetAppCallbacks.mpas,312 :: 		Result := False;                                                              // Do the default
; Result start address is: 2 (W1)
	CLR	W1
;NMRAnetAppCallbacks.mpas,313 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_AppCallback_ConsumerIdentify:
	RETURN
; end of _AppCallback_ConsumerIdentify

_AppCallback_EventsIdentify:
	LNK	#10

;NMRAnetAppCallbacks.mpas,319 :: 		begin
;NMRAnetAppCallbacks.mpas,320 :: 		Result := True;                                                               // We handled it
	PUSH	W10
	PUSH	W11
	MOV	#65535, W0
	MOV	W0, [W14+0]
;NMRAnetAppCallbacks.mpas,321 :: 		for j := 0 to Nodes.AllocatedCount - 1 do
; j start address is: 14 (W7)
	CLR	W7
; j end address is: 14 (W7)
L__AppCallback_EventsIdentify77:
; j start address is: 14 (W7)
	MOV	_Nodes+3120, W0
	DEC	W0
	MOV	W0, [W14+8]
	CP	W7, W0
	BRA LE	L__AppCallback_EventsIdentify215
	GOTO	L__AppCallback_EventsIdentify81
L__AppCallback_EventsIdentify215:
;NMRAnetAppCallbacks.mpas,323 :: 		Node := Nodes.AllocatedList[j];
	SL	W7, #1, W1
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W0
	MOV	[W0], W0
	MOV	W0, [W14+4]
;NMRAnetAppCallbacks.mpas,324 :: 		if NMRAnetNode_TestStateFlag(Node, NS_VIRTUAL) then
	MOV.B	#8, W11
	MOV	W0, W10
	CALL	_NMRAnetNode_TestStateFlag
	CP0	W0
	BRA NZ	L__AppCallback_EventsIdentify216
	GOTO	L__AppCallback_EventsIdentify83
L__AppCallback_EventsIdentify216:
;NMRAnetAppCallbacks.mpas,326 :: 		NMRAnetNode_SetConsumerEventFlags(Node, EVENT_STATE_UNKOWN);              // Consumers are eaay.
	MOV.B	#3, W11
	MOV	[W14+4], W10
	CALL	_NMRAnetNode_SetConsumerEventFlags
;NMRAnetAppCallbacks.mpas,327 :: 		for ProducerIndex := 0 to MAX_VNODE_SUPPORTED_EVENTS_PRODUCED - 1 do      // Producers take some work
	CLR	W0
	MOV	W0, [W14+2]
; j end address is: 14 (W7)
	MOV	W7, W6
L__AppCallback_EventsIdentify86:
;NMRAnetAppCallbacks.mpas,329 :: 		SetProxyNodeProducerFlags(Node, ProducerIndex);
; j start address is: 12 (W6)
	MOV	[W14+2], W11
	MOV	[W14+4], W10
	CALL	NMRAnetAppCallbacks_SetProxyNodeProducerFlags
;NMRAnetAppCallbacks.mpas,330 :: 		SetProxyNodeDccAddressFlags(Node, nil);
	CLR	W11
	MOV	[W14+4], W10
	CALL	NMRAnetAppCallbacks_SetProxyNodeDccAddressFlags
;NMRAnetAppCallbacks.mpas,331 :: 		end
	MOV	[W14+2], W0
	CP	W0, #2
	BRA NZ	L__AppCallback_EventsIdentify217
	GOTO	L__AppCallback_EventsIdentify89
L__AppCallback_EventsIdentify217:
	MOV	#1, W1
	ADD	W14, #2, W0
	ADD	W1, [W0], [W0]
	GOTO	L__AppCallback_EventsIdentify86
L__AppCallback_EventsIdentify89:
;NMRAnetAppCallbacks.mpas,332 :: 		end else
	MOV	W6, W1
; j end address is: 12 (W6)
	GOTO	L__AppCallback_EventsIdentify84
L__AppCallback_EventsIdentify83:
;NMRAnetAppCallbacks.mpas,334 :: 		NMRAnetNode_SetProducerEventFlags(Node, EVENT_STATE_UNKOWN);
; j start address is: 14 (W7)
	MOV.B	#3, W11
	MOV	[W14+4], W10
	CALL	_NMRAnetNode_SetProducerEventFlags
;NMRAnetAppCallbacks.mpas,335 :: 		NMRAnetNode_SetConsumerEventFlags(Node, EVENT_STATE_UNKOWN);
	MOV.B	#3, W11
	MOV	[W14+4], W10
	CALL	_NMRAnetNode_SetConsumerEventFlags
; j end address is: 14 (W7)
	MOV	W7, W1
;NMRAnetAppCallbacks.mpas,336 :: 		end
L__AppCallback_EventsIdentify84:
;NMRAnetAppCallbacks.mpas,337 :: 		end;
; j start address is: 2 (W1)
	MOV	[W14+8], W0
	CP	W1, W0
	BRA NZ	L__AppCallback_EventsIdentify218
	GOTO	L__AppCallback_EventsIdentify81
L__AppCallback_EventsIdentify218:
; j start address is: 14 (W7)
	ADD	W1, #1, W7
; j end address is: 2 (W1)
; j end address is: 14 (W7)
	GOTO	L__AppCallback_EventsIdentify77
L__AppCallback_EventsIdentify81:
;NMRAnetAppCallbacks.mpas,338 :: 		end;
	MOV	[W14+0], W0
L_end_AppCallback_EventsIdentify:
	POP	W11
	POP	W10
	ULNK
	RETURN
; end of _AppCallback_EventsIdentify

_AppCallback_EventsIdentifyByDest:

;NMRAnetAppCallbacks.mpas,346 :: 		begin
;NMRAnetAppCallbacks.mpas,347 :: 		Result := True;                                                               // We handled it
	PUSH	W11
; Result start address is: 14 (W7)
	MOV	#65535, W7
;NMRAnetAppCallbacks.mpas,348 :: 		NMRAnetNode_SetConsumerEventFlags(Node, EVENT_STATE_UNKOWN);                  // Consumers are eaay.
	MOV.B	#3, W11
	CALL	_NMRAnetNode_SetConsumerEventFlags
;NMRAnetAppCallbacks.mpas,350 :: 		if NMRAnetNode_TestStateFlag(Node, NS_VIRTUAL) then
	MOV.B	#8, W11
	CALL	_NMRAnetNode_TestStateFlag
	CP0	W0
	BRA NZ	L__AppCallback_EventsIdentifyByDest220
	GOTO	L__AppCallback_EventsIdentifyByDest92
L__AppCallback_EventsIdentifyByDest220:
;NMRAnetAppCallbacks.mpas,352 :: 		for ProducerIndex := 0 to MAX_VNODE_SUPPORTED_EVENTS_PRODUCED - 1 do        // Producers take some work
; ProducerIndex start address is: 0 (W0)
	CLR	W0
; ProducerIndex end address is: 0 (W0)
; Result end address is: 14 (W7)
	MOV	W0, W6
L__AppCallback_EventsIdentifyByDest95:
;NMRAnetAppCallbacks.mpas,353 :: 		SetProxyNodeProducerFlags(Node, ProducerIndex);
; ProducerIndex start address is: 12 (W6)
; Result start address is: 14 (W7)
	MOV	W6, W11
	CALL	NMRAnetAppCallbacks_SetProxyNodeProducerFlags
	CP	W6, #2
	BRA NZ	L__AppCallback_EventsIdentifyByDest221
	GOTO	L__AppCallback_EventsIdentifyByDest98
L__AppCallback_EventsIdentifyByDest221:
; ProducerIndex start address is: 12 (W6)
	INC	W6
; ProducerIndex end address is: 12 (W6)
; ProducerIndex end address is: 12 (W6)
	GOTO	L__AppCallback_EventsIdentifyByDest95
L__AppCallback_EventsIdentifyByDest98:
;NMRAnetAppCallbacks.mpas,354 :: 		SetProxyNodeDccAddressFlags(Node, nil);
	CLR	W11
	CALL	NMRAnetAppCallbacks_SetProxyNodeDccAddressFlags
;NMRAnetAppCallbacks.mpas,355 :: 		end else
	MOV	W7, W1
	GOTO	L__AppCallback_EventsIdentifyByDest93
L__AppCallback_EventsIdentifyByDest92:
;NMRAnetAppCallbacks.mpas,357 :: 		NMRAnetNode_SetProducerEventFlags(Node, EVENT_STATE_UNKOWN);
	MOV.B	#3, W11
	CALL	_NMRAnetNode_SetProducerEventFlags
;NMRAnetAppCallbacks.mpas,358 :: 		NMRAnetNode_SetConsumerEventFlags(Node, EVENT_STATE_UNKOWN);
	MOV.B	#3, W11
	CALL	_NMRAnetNode_SetConsumerEventFlags
; Result end address is: 14 (W7)
	MOV	W7, W1
;NMRAnetAppCallbacks.mpas,359 :: 		end;
L__AppCallback_EventsIdentifyByDest93:
;NMRAnetAppCallbacks.mpas,360 :: 		end;
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_AppCallback_EventsIdentifyByDest:
	POP	W11
	RETURN
; end of _AppCallback_EventsIdentifyByDest

_AppCallback_TractionControl:
	LNK	#32

;NMRAnetAppCallbacks.mpas,373 :: 		begin
;NMRAnetAppCallbacks.mpas,374 :: 		ProxyData := GetProxyData(Node);
	PUSH	W10
	PUSH	W11
	PUSH	W12
	PUSH	W13
	CALL	_GetProxyData
; ProxyData start address is: 16 (W8)
	MOV	W0, W8
;NMRAnetAppCallbacks.mpas,375 :: 		AddressHi := (ProxyDAta^.Address shr 8) and $00FF;
	ADD	W0, #8, W0
	MOV	[W0], W2
	LSR	W2, #8, W1
	MOV.B	#255, W0
; AddressHi start address is: 8 (W4)
	AND.B	W1, W0, W4
;NMRAnetAppCallbacks.mpas,376 :: 		AddressLo := ProxyDAta^.Address and $00FF;
	MOV.B	#255, W0
; AddressLo start address is: 18 (W9)
	AND.B	W2, W0, W9
;NMRAnetAppCallbacks.mpas,377 :: 		case CANBuffer^.DataBytes[2] and TRACTION_PROTOCOL_MASK of
	ADD	W11, #5, W0
	INC2	W0
	ZE	[W0], W1
	MOV	#240, W0
	AND	W1, W0, W0
	MOV	W0, [W14+30]
;NMRAnetAppCallbacks.mpas,378 :: 		TRACTION_OLCB :
	CP	W0, #0
	BRA Z	L__AppCallback_TractionControl223
	GOTO	L__AppCallback_TractionControl103
L__AppCallback_TractionControl223:
; ProxyData end address is: 16 (W8)
; AddressHi end address is: 8 (W4)
; AddressLo end address is: 18 (W9)
;NMRAnetAppCallbacks.mpas,380 :: 		case CANBuffer^.DataBytes[2] and TRACTION_OP_MASK of
	ADD	W11, #5, W0
	INC2	W0
	ZE	[W0], W0
	AND	W0, #15, W0
	MOV	W0, [W14+28]
;NMRAnetAppCallbacks.mpas,381 :: 		TRACTION_OP_SPEED_DIR :
	CP	W0, #0
	BRA Z	L__AppCallback_TractionControl224
	GOTO	L__AppCallback_TractionControl107
L__AppCallback_TractionControl224:
;NMRAnetAppCallbacks.mpas,384 :: 		SpeedInt := HalfToFloat( (CANBuffer^.DataBytes[3] shl 8) or (CANBuffer^.DataBytes[4]));
	ADD	W11, #5, W2
	ADD	W2, #3, W0
	MOV.B	[W0], W0
	ZE	W0, W0
	SL	W0, #8, W1
	ADD	W2, #4, W0
	ZE	[W0], W0
	IOR	W1, W0, W0
	MOV	W0, W10
	CALL	_HalfToFloat
;NMRAnetAppCallbacks.mpas,386 :: 		end;
	GOTO	L__AppCallback_TractionControl104
L__AppCallback_TractionControl107:
;NMRAnetAppCallbacks.mpas,387 :: 		TRACTION_OP_FUNCTION :
	MOV	[W14+28], W0
	CP	W0, #1
	BRA Z	L__AppCallback_TractionControl225
	GOTO	L__AppCallback_TractionControl110
L__AppCallback_TractionControl225:
;NMRAnetAppCallbacks.mpas,389 :: 		end;
	GOTO	L__AppCallback_TractionControl104
L__AppCallback_TractionControl110:
L__AppCallback_TractionControl104:
;NMRAnetAppCallbacks.mpas,391 :: 		end;
	GOTO	L__AppCallback_TractionControl100
L__AppCallback_TractionControl103:
;NMRAnetAppCallbacks.mpas,392 :: 		TRACTION_DCC :
; AddressLo start address is: 18 (W9)
; AddressHi start address is: 8 (W4)
; ProxyData start address is: 16 (W8)
	MOV	#128, W1
	MOV	[W14+30], W0
	CP	W0, W1
	BRA Z	L__AppCallback_TractionControl226
	GOTO	L__AppCallback_TractionControl113
L__AppCallback_TractionControl226:
;NMRAnetAppCallbacks.mpas,394 :: 		case CANBuffer^.DataBytes[2] and $0F of
	ADD	W11, #5, W0
	INC2	W0
	ZE	[W0], W0
	AND	W0, #15, W0
	MOV	W0, [W14+30]
;NMRAnetAppCallbacks.mpas,395 :: 		TRACTION_OP_SPEED_DIR :
	CP	W0, #0
	BRA Z	L__AppCallback_TractionControl227
	GOTO	L__AppCallback_TractionControl117
L__AppCallback_TractionControl227:
;NMRAnetAppCallbacks.mpas,397 :: 		ProxyData^.Speed := CANBuffer^.DataBytes[3];
	ADD	W8, #2, W1
	ADD	W11, #5, W0
	ADD	W0, #3, W0
	ZE	[W0], W0
	MOV	W0, [W1]
;NMRAnetAppCallbacks.mpas,398 :: 		if AddressHi and $C0 = $C0 then
	ZE	W4, W1
	MOV	#192, W0
	AND	W1, W0, W1
	MOV	#192, W0
	CP	W1, W0
	BRA Z	L__AppCallback_TractionControl228
	GOTO	L__AppCallback_TractionControl119
L__AppCallback_TractionControl228:
;NMRAnetAppCallbacks.mpas,400 :: 		if CANBuffer^.DataBytes[4] = 128 then
	ADD	W11, #5, W0
	ADD	W0, #4, W0
	MOV.B	[W0], W1
	MOV.B	#128, W0
	CP.B	W1, W0
	BRA Z	L__AppCallback_TractionControl229
	GOTO	L__AppCallback_TractionControl122
L__AppCallback_TractionControl229:
;NMRAnetAppCallbacks.mpas,403 :: 		NMRA_DCC_LoadPacket(@DCCPacket, AddressHi, AddressLo, %00111111, ProxyData^.Speed, 0, 4);
	ADD	W14, #0, W2
	ADD	W8, #2, W0
; ProxyData end address is: 16 (W8)
	MOV	[W0], W1
	MOV.B	#63, W13
	MOV.B	W9, W12
; AddressLo end address is: 18 (W9)
	MOV.B	W4, W11
; AddressHi end address is: 8 (W4)
	MOV	W2, W10
	MOV	#4, W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	PUSH	W1
	CALL	_NMRA_DCC_LoadPacket
	SUB	#6, W15
;NMRAnetAppCallbacks.mpas,404 :: 		end
	GOTO	L__AppCallback_TractionControl123
;NMRAnetAppCallbacks.mpas,405 :: 		else
L__AppCallback_TractionControl122:
;NMRAnetAppCallbacks.mpas,408 :: 		NMRA_DCC_LoadPacket(@DCCPacket, AddressHi, AddressLo, ProxyData^.Speed, 0, 0, 3);
; AddressLo start address is: 18 (W9)
; AddressHi start address is: 8 (W4)
; ProxyData start address is: 16 (W8)
	ADD	W14, #0, W1
	ADD	W8, #2, W0
; ProxyData end address is: 16 (W8)
	MOV.B	[W0], W13
	MOV.B	W9, W12
; AddressLo end address is: 18 (W9)
	MOV.B	W4, W11
; AddressHi end address is: 8 (W4)
	MOV	W1, W10
	MOV	#3, W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	CALL	_NMRA_DCC_LoadPacket
	SUB	#6, W15
;NMRAnetAppCallbacks.mpas,409 :: 		end;
L__AppCallback_TractionControl123:
;NMRAnetAppCallbacks.mpas,410 :: 		NMRA_DCC_QueuePacket(@Track, @DCCPacket, False);
	ADD	W14, #0, W0
	CLR	W12
	MOV	W0, W11
	MOV	#lo_addr(_Track), W10
	CALL	_NMRA_DCC_QueuePacket
;NMRAnetAppCallbacks.mpas,411 :: 		end else
	GOTO	L__AppCallback_TractionControl120
L__AppCallback_TractionControl119:
;NMRAnetAppCallbacks.mpas,413 :: 		if CANBuffer^.DataBytes[4] < 128 then // Can't do short address with 128 Step
; AddressLo start address is: 18 (W9)
; ProxyData start address is: 16 (W8)
	ADD	W11, #5, W0
	ADD	W0, #4, W0
	MOV.B	[W0], W1
	MOV.B	#128, W0
	CP.B	W1, W0
	BRA LTU	L__AppCallback_TractionControl230
	GOTO	L__AppCallback_TractionControl125
L__AppCallback_TractionControl230:
;NMRAnetAppCallbacks.mpas,416 :: 		NMRA_DCC_LoadPacket(@DCCPacket, AddressLo, ProxyData^.Speed, 0, 0, 0, 2);
	ADD	W14, #0, W1
	ADD	W8, #2, W0
; ProxyData end address is: 16 (W8)
	CLR	W13
	MOV.B	[W0], W12
	MOV.B	W9, W11
; AddressLo end address is: 18 (W9)
	MOV	W1, W10
	MOV	#2, W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	CALL	_NMRA_DCC_LoadPacket
	SUB	#6, W15
;NMRAnetAppCallbacks.mpas,417 :: 		NMRA_DCC_QueuePacket(@Track, @DCCPacket, False);
	ADD	W14, #0, W0
	CLR	W12
	MOV	W0, W11
	MOV	#lo_addr(_Track), W10
	CALL	_NMRA_DCC_QueuePacket
;NMRAnetAppCallbacks.mpas,418 :: 		end else
	GOTO	L__AppCallback_TractionControl126
L__AppCallback_TractionControl125:
;NMRAnetAppCallbacks.mpas,421 :: 		end
L__AppCallback_TractionControl126:
;NMRAnetAppCallbacks.mpas,422 :: 		end;
L__AppCallback_TractionControl120:
;NMRAnetAppCallbacks.mpas,423 :: 		end;
	GOTO	L__AppCallback_TractionControl114
L__AppCallback_TractionControl117:
;NMRAnetAppCallbacks.mpas,424 :: 		TRACTION_OP_FUNCTION :
; AddressLo start address is: 18 (W9)
; AddressHi start address is: 8 (W4)
; ProxyData start address is: 16 (W8)
	MOV	[W14+30], W0
	CP	W0, #1
	BRA Z	L__AppCallback_TractionControl231
	GOTO	L__AppCallback_TractionControl129
L__AppCallback_TractionControl231:
;NMRAnetAppCallbacks.mpas,426 :: 		FunctionAddress := (CANBuffer^.DataBytes[4] shl 8) or CANBuffer^.DataBytes[5];
	ADD	W11, #5, W2
	ADD	W2, #4, W0
	MOV.B	[W0], W0
	ZE	W0, W0
	SL	W0, #8, W1
	ADD	W2, #5, W0
	ZE	[W0], W0
; FunctionAddress start address is: 10 (W5)
	IOR	W1, W0, W5
;NMRAnetAppCallbacks.mpas,427 :: 		FunctionValue := CANBuffer^.DataBytes[6];
	ADD	W2, #6, W0
; FunctionValue start address is: 6 (W3)
	MOV.B	[W0], W3
;NMRAnetAppCallbacks.mpas,429 :: 		case CANBuffer^.DataBytes[3] of
	ADD	W2, #3, W0
	MOV.B	[W0], W0
	MOV.B	W0, [W14+28]
;NMRAnetAppCallbacks.mpas,430 :: 		DCC_FUNCTION_28 :
	CP.B	W0, #0
	BRA Z	L__AppCallback_TractionControl232
	GOTO	L__AppCallback_TractionControl133
L__AppCallback_TractionControl232:
;NMRAnetAppCallbacks.mpas,434 :: 		WideFunctionMask := $00000001;
; WideFunctionMask start address is: 12 (W6)
	MOV	#1, W6
	MOV	#0, W7
;NMRAnetAppCallbacks.mpas,435 :: 		WideFunctionMask := WideFunctionMask shl FunctionAddress;   // Set the correct Function Bit
	MOV	W5, W2
	MOV.D	W6, W0
L__AppCallback_TractionControl233:
	DEC	W2, W2
	BRA LT	L__AppCallback_TractionControl234
	SL	W0, W0
	RLC	W1, W1
	BRA	L__AppCallback_TractionControl233
L__AppCallback_TractionControl234:
	MOV.D	W0, W6
;NMRAnetAppCallbacks.mpas,436 :: 		ProxyData^.Functions := ProxyData^.Functions and not WideFunctionMask; // Clear the bit
	ADD	W8, #4, W2
	COM	W0
	COM	W1
	AND	W0, [W2], [W2++]
	AND	W1, [W2], [W2--]
;NMRAnetAppCallbacks.mpas,437 :: 		if FunctionValue > 0 then
	CP.B	W3, #0
	BRA GTU	L__AppCallback_TractionControl235
	GOTO	L__AppCallback_TractionControl135
L__AppCallback_TractionControl235:
; FunctionValue end address is: 6 (W3)
;NMRAnetAppCallbacks.mpas,438 :: 		ProxyData^.Functions := ProxyData^.Functions or WideFunctionMask;      // Set the bit if needed
	ADD	W8, #4, W0
	IOR	W6, [W0], [W0++]
	IOR	W7, [W0], [W0--]
; WideFunctionMask end address is: 12 (W6)
L__AppCallback_TractionControl135:
;NMRAnetAppCallbacks.mpas,440 :: 		if FunctionAddress < 29 then
	CP	W5, #29
	BRA LTU	L__AppCallback_TractionControl236
	GOTO	L__AppCallback_TractionControl138
L__AppCallback_TractionControl236:
;NMRAnetAppCallbacks.mpas,442 :: 		if FunctionAddress < 5 then
	CP	W5, #5
	BRA LTU	L__AppCallback_TractionControl237
	GOTO	L__AppCallback_TractionControl141
L__AppCallback_TractionControl237:
;NMRAnetAppCallbacks.mpas,444 :: 		FunctionMask := (ProxyData^.Functions shr 1) and $0F;
	ADD	W8, #4, W3
; ProxyData end address is: 16 (W8)
	MOV	[W3++], W1
	MOV	[W3--], W2
	LSR	W2, W2
	RRC	W1, W1
	ADD	W14, #6, W0
	AND.B	W1, #15, [W0]
;NMRAnetAppCallbacks.mpas,445 :: 		FunctionMask.4 := ProxyData^.Functions.0;
	MOV	[W3++], W1
	MOV	[W3--], W2
	ADD	W14, #6, W0
	BSET.B	[W0], #4
	BTSS	W1, #0
	BCLR.B	[W0], #4
;NMRAnetAppCallbacks.mpas,446 :: 		FunctionMask := FunctionMask or %10000000;                    // Opcode bits
	MOV.B	#128, W1
	ADD	W14, #6, W0
	IOR.B	W1, [W0], [W0]
;NMRAnetAppCallbacks.mpas,447 :: 		end else
	GOTO	L__AppCallback_TractionControl142
L__AppCallback_TractionControl141:
;NMRAnetAppCallbacks.mpas,448 :: 		if FunctionAddress < 9 then
; ProxyData start address is: 16 (W8)
	CP	W5, #9
	BRA LTU	L__AppCallback_TractionControl238
	GOTO	L__AppCallback_TractionControl144
L__AppCallback_TractionControl238:
;NMRAnetAppCallbacks.mpas,450 :: 		FunctionMask := (ProxyData^.Functions shr 5) and $0F;
	ADD	W8, #4, W3
; ProxyData end address is: 16 (W8)
	MOV	#5, W0
	MOV	[W3++], W1
	MOV	[W3--], W2
L__AppCallback_TractionControl239:
	DEC	W0, W0
	BRA LT	L__AppCallback_TractionControl240
	LSR	W2, W2
	RRC	W1, W1
	BRA	L__AppCallback_TractionControl239
L__AppCallback_TractionControl240:
	ADD	W14, #6, W0
	AND.B	W1, #15, [W0]
;NMRAnetAppCallbacks.mpas,451 :: 		FunctionMask := FunctionMask or %10110000;                    // Opcode bits
	MOV.B	[W14+6], W2
	MOV.B	#176, W1
	ADD	W14, #6, W0
	IOR.B	W2, W1, [W0]
;NMRAnetAppCallbacks.mpas,452 :: 		end else
	GOTO	L__AppCallback_TractionControl145
L__AppCallback_TractionControl144:
;NMRAnetAppCallbacks.mpas,453 :: 		if FunctionAddress < 13 then
; ProxyData start address is: 16 (W8)
	CP	W5, #13
	BRA LTU	L__AppCallback_TractionControl241
	GOTO	L__AppCallback_TractionControl147
L__AppCallback_TractionControl241:
;NMRAnetAppCallbacks.mpas,455 :: 		FunctionMask := (ProxyData^.Functions shr 9) and $0F;
	ADD	W8, #4, W3
; ProxyData end address is: 16 (W8)
	MOV	#9, W0
	MOV	[W3++], W1
	MOV	[W3--], W2
L__AppCallback_TractionControl242:
	DEC	W0, W0
	BRA LT	L__AppCallback_TractionControl243
	LSR	W2, W2
	RRC	W1, W1
	BRA	L__AppCallback_TractionControl242
L__AppCallback_TractionControl243:
	ADD	W14, #6, W0
	AND.B	W1, #15, [W0]
;NMRAnetAppCallbacks.mpas,456 :: 		FunctionMask := FunctionMask or %10100000;                    // Opcode bits
	MOV.B	[W14+6], W2
	MOV.B	#160, W1
	ADD	W14, #6, W0
	IOR.B	W2, W1, [W0]
;NMRAnetAppCallbacks.mpas,457 :: 		end else
	GOTO	L__AppCallback_TractionControl148
L__AppCallback_TractionControl147:
;NMRAnetAppCallbacks.mpas,458 :: 		if FunctionAddress < 21 then
; ProxyData start address is: 16 (W8)
	CP	W5, #21
	BRA LTU	L__AppCallback_TractionControl244
	GOTO	L__AppCallback_TractionControl150
L__AppCallback_TractionControl244:
;NMRAnetAppCallbacks.mpas,460 :: 		FunctionMask := ProxyData^.Functions shr 13;
	ADD	W8, #4, W3
; ProxyData end address is: 16 (W8)
	MOV	#13, W2
	MOV.D	[W3], W0
L__AppCallback_TractionControl245:
	DEC	W2, W2
	BRA LT	L__AppCallback_TractionControl246
	LSR	W1, W1
	RRC	W0, W0
	BRA	L__AppCallback_TractionControl245
L__AppCallback_TractionControl246:
	MOV.B	W0, [W14+6]
;NMRAnetAppCallbacks.mpas,461 :: 		FunctionExtendedCode := %11011110
	MOV.B	#222, W0
	MOV.B	W0, [W14+7]
;NMRAnetAppCallbacks.mpas,462 :: 		end else
	GOTO	L__AppCallback_TractionControl151
L__AppCallback_TractionControl150:
;NMRAnetAppCallbacks.mpas,464 :: 		FunctionMask := ProxyData^.Functions shr 21;
; ProxyData start address is: 16 (W8)
	ADD	W8, #4, W3
; ProxyData end address is: 16 (W8)
	MOV	#21, W2
	MOV.D	[W3], W0
L__AppCallback_TractionControl247:
	DEC	W2, W2
	BRA LT	L__AppCallback_TractionControl248
	LSR	W1, W1
	RRC	W0, W0
	BRA	L__AppCallback_TractionControl247
L__AppCallback_TractionControl248:
	MOV.B	W0, [W14+6]
;NMRAnetAppCallbacks.mpas,465 :: 		FunctionExtendedCode := %11011111
	MOV.B	#223, W0
	MOV.B	W0, [W14+7]
;NMRAnetAppCallbacks.mpas,466 :: 		end;
L__AppCallback_TractionControl151:
L__AppCallback_TractionControl148:
L__AppCallback_TractionControl145:
L__AppCallback_TractionControl142:
;NMRAnetAppCallbacks.mpas,469 :: 		if AddressHi and $C0 = $C0 then
	ZE	W4, W1
	MOV	#192, W0
	AND	W1, W0, W1
	MOV	#192, W0
	CP	W1, W0
	BRA Z	L__AppCallback_TractionControl249
	GOTO	L__AppCallback_TractionControl153
L__AppCallback_TractionControl249:
;NMRAnetAppCallbacks.mpas,471 :: 		if FunctionAddress < 13 then
	CP	W5, #13
	BRA LTU	L__AppCallback_TractionControl250
	GOTO	L__AppCallback_TractionControl156
L__AppCallback_TractionControl250:
; FunctionAddress end address is: 10 (W5)
;NMRAnetAppCallbacks.mpas,472 :: 		NMRA_DCC_LoadPacket(@DCCPacket, AddressHi, AddressLo, FunctionMask, 0, 0, 3)
	ADD	W14, #0, W0
	MOV.B	[W14+6], W13
	MOV.B	W9, W12
; AddressLo end address is: 18 (W9)
	MOV.B	W4, W11
; AddressHi end address is: 8 (W4)
	MOV	W0, W10
	MOV	#3, W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	CALL	_NMRA_DCC_LoadPacket
	SUB	#6, W15
	GOTO	L__AppCallback_TractionControl157
;NMRAnetAppCallbacks.mpas,473 :: 		else
L__AppCallback_TractionControl156:
;NMRAnetAppCallbacks.mpas,474 :: 		NMRA_DCC_LoadPacket(@DCCPacket, AddressHi, AddressLo, FunctionExtendedCode, FunctionMask, 0, 4)
; AddressLo start address is: 18 (W9)
; AddressHi start address is: 8 (W4)
	ADD	W14, #0, W0
	MOV.B	[W14+7], W13
	MOV.B	W9, W12
; AddressLo end address is: 18 (W9)
	MOV.B	W4, W11
; AddressHi end address is: 8 (W4)
	MOV	W0, W10
	MOV	#4, W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	ADD	W14, #6, W0
	ZE	[W0], W0
	PUSH	W0
	CALL	_NMRA_DCC_LoadPacket
	SUB	#6, W15
L__AppCallback_TractionControl157:
;NMRAnetAppCallbacks.mpas,475 :: 		end else
	GOTO	L__AppCallback_TractionControl154
L__AppCallback_TractionControl153:
;NMRAnetAppCallbacks.mpas,477 :: 		if FunctionAddress < 13 then
; FunctionAddress start address is: 10 (W5)
; AddressLo start address is: 18 (W9)
	CP	W5, #13
	BRA LTU	L__AppCallback_TractionControl251
	GOTO	L__AppCallback_TractionControl159
L__AppCallback_TractionControl251:
; FunctionAddress end address is: 10 (W5)
;NMRAnetAppCallbacks.mpas,478 :: 		NMRA_DCC_LoadPacket(@DCCPacket, AddressLo, FunctionMask, 0, 0, 0, 3)
	ADD	W14, #0, W0
	CLR	W13
	MOV.B	[W14+6], W12
	MOV.B	W9, W11
; AddressLo end address is: 18 (W9)
	MOV	W0, W10
	MOV	#3, W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	CALL	_NMRA_DCC_LoadPacket
	SUB	#6, W15
	GOTO	L__AppCallback_TractionControl160
;NMRAnetAppCallbacks.mpas,479 :: 		else
L__AppCallback_TractionControl159:
;NMRAnetAppCallbacks.mpas,480 :: 		NMRA_DCC_LoadPacket(@DCCPacket, AddressLo, FunctionExtendedCode, FunctionMask, 0, 0, 4)
; AddressLo start address is: 18 (W9)
	ADD	W14, #0, W0
	MOV.B	[W14+6], W13
	MOV.B	[W14+7], W12
	MOV.B	W9, W11
; AddressLo end address is: 18 (W9)
	MOV	W0, W10
	MOV	#4, W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	CALL	_NMRA_DCC_LoadPacket
	SUB	#6, W15
L__AppCallback_TractionControl160:
;NMRAnetAppCallbacks.mpas,481 :: 		end;
L__AppCallback_TractionControl154:
;NMRAnetAppCallbacks.mpas,483 :: 		NMRA_DCC_QueuePacket(@Track, @DCCPacket, False);
	ADD	W14, #0, W0
	CLR	W12
	MOV	W0, W11
	MOV	#lo_addr(_Track), W10
	CALL	_NMRA_DCC_QueuePacket
;NMRAnetAppCallbacks.mpas,484 :: 		end
L__AppCallback_TractionControl138:
;NMRAnetAppCallbacks.mpas,485 :: 		end;
	GOTO	L__AppCallback_TractionControl130
L__AppCallback_TractionControl133:
;NMRAnetAppCallbacks.mpas,486 :: 		DCC_FUNCTION_32k :
	MOV.B	[W14+28], W0
	CP.B	W0, #1
	BRA Z	L__AppCallback_TractionControl252
	GOTO	L__AppCallback_TractionControl163
L__AppCallback_TractionControl252:
;NMRAnetAppCallbacks.mpas,488 :: 		UART1_Write_Text('DCC_FUNCTION_32k'+LF);
	ADD	W14, #8, W1
	MOV.B	#68, W0
	MOV.B	W0, [W1++]
	MOV.B	#67, W0
	MOV.B	W0, [W1++]
	MOV.B	#67, W0
	MOV.B	W0, [W1++]
	MOV.B	#95, W0
	MOV.B	W0, [W1++]
	MOV.B	#70, W0
	MOV.B	W0, [W1++]
	MOV.B	#85, W0
	MOV.B	W0, [W1++]
	MOV.B	#78, W0
	MOV.B	W0, [W1++]
	MOV.B	#67, W0
	MOV.B	W0, [W1++]
	MOV.B	#84, W0
	MOV.B	W0, [W1++]
	MOV.B	#73, W0
	MOV.B	W0, [W1++]
	MOV.B	#79, W0
	MOV.B	W0, [W1++]
	MOV.B	#78, W0
	MOV.B	W0, [W1++]
	MOV.B	#95, W0
	MOV.B	W0, [W1++]
	MOV.B	#51, W0
	MOV.B	W0, [W1++]
	MOV.B	#50, W0
	MOV.B	W0, [W1++]
	MOV.B	#107, W0
	MOV.B	W0, [W1++]
	MOV.B	#13, W0
	MOV.B	W0, [W1++]
	MOV.B	#10, W0
	MOV.B	W0, [W1++]
	MOV.B	#0, W0
	MOV.B	W0, [W1++]
	ADD	W14, #8, W0
	MOV	W0, W10
	CALL	_UART1_Write_Text
;NMRAnetAppCallbacks.mpas,489 :: 		end;
	GOTO	L__AppCallback_TractionControl130
L__AppCallback_TractionControl163:
L__AppCallback_TractionControl130:
;NMRAnetAppCallbacks.mpas,491 :: 		end;
	GOTO	L__AppCallback_TractionControl114
L__AppCallback_TractionControl129:
;NMRAnetAppCallbacks.mpas,492 :: 		TRACTION_OP_PROXY_MGMT :
; ProxyData start address is: 16 (W8)
	MOV	[W14+30], W0
	CP	W0, #2
	BRA Z	L__AppCallback_TractionControl253
	GOTO	L__AppCallback_TractionControl166
L__AppCallback_TractionControl253:
;NMRAnetAppCallbacks.mpas,494 :: 		case CANBuffer^.DataBytes[3] of
	ADD	W11, #5, W0
	ADD	W0, #3, W0
;NMRAnetAppCallbacks.mpas,495 :: 		DCC_ALLOCATE_ADDRESS :
	MOV.B	[W0], W0
	CP.B	W0, #1
	BRA Z	L__AppCallback_TractionControl254
	GOTO	L__AppCallback_TractionControl170
L__AppCallback_TractionControl254:
;NMRAnetAppCallbacks.mpas,497 :: 		ProxyData^.State := ProxyData^.State or PS_ALLOCATED;
	MOV.B	[W8], W0
	ZE	W0, W0
	IOR	W0, #1, W0
	MOV.B	W0, [W8]
;NMRAnetAppCallbacks.mpas,498 :: 		ProxyData^.MsgFlags := ProxyData^.MsgFlags or MSG_DCC_ADDRESS_ALLOCATED;     // Changed State so notify the system
	ADD	W8, #1, W1
	ZE	[W1], W0
	IOR	W0, #1, W0
	MOV.B	W0, [W1]
;NMRAnetAppCallbacks.mpas,499 :: 		SetProxyNodeProducerFlags(Node, EVENT_TRAIN_DCC_IDLE_INDEX);                 // Changed State so notify the system
	PUSH	W11
	MOV	#1, W11
	CALL	NMRAnetAppCallbacks_SetProxyNodeProducerFlags
;NMRAnetAppCallbacks.mpas,500 :: 		SetProxyNodeProducerFlags(Node, EVENT_TRAIN_DCC_INUSE_INDEX);                // Changed State so notify the system
	MOV	#2, W11
	CALL	NMRAnetAppCallbacks_SetProxyNodeProducerFlags
	POP	W11
;NMRAnetAppCallbacks.mpas,501 :: 		ProxyData^.Address := Word ((CANBuffer^.DataBytes[4] shl 8)) or CANBuffer^.DataBytes[5];  // This is in NMRA DCC format for short/long address
	ADD	W8, #8, W3
; ProxyData end address is: 16 (W8)
	ADD	W11, #5, W2
	ADD	W2, #4, W0
	MOV.B	[W0], W0
	ZE	W0, W0
	SL	W0, #8, W1
	ADD	W2, #5, W0
	ZE	[W0], W0
	IOR	W1, W0, W0
	MOV	W0, [W3]
;NMRAnetAppCallbacks.mpas,502 :: 		end;
	GOTO	L__AppCallback_TractionControl167
L__AppCallback_TractionControl170:
L__AppCallback_TractionControl167:
;NMRAnetAppCallbacks.mpas,504 :: 		end;
	GOTO	L__AppCallback_TractionControl114
L__AppCallback_TractionControl166:
L__AppCallback_TractionControl114:
;NMRAnetAppCallbacks.mpas,506 :: 		end;
	GOTO	L__AppCallback_TractionControl100
L__AppCallback_TractionControl113:
L__AppCallback_TractionControl100:
;NMRAnetAppCallbacks.mpas,508 :: 		end;
L_end_AppCallback_TractionControl:
	POP	W13
	POP	W12
	POP	W11
	POP	W10
	ULNK
	RETURN
; end of _AppCallback_TractionControl

_AppCallback_StateMachine:
	LNK	#2

;NMRAnetAppCallbacks.mpas,516 :: 		begin
;NMRAnetAppCallbacks.mpas,518 :: 		Result := False;
	PUSH	W12
	PUSH	W13
	CLR	W0
	MOV	W0, [W14+0]
;NMRAnetAppCallbacks.mpas,519 :: 		ProxyData := GetProxyData(Node);
	CALL	_GetProxyData
; ProxyData start address is: 6 (W3)
	MOV	W0, W3
;NMRAnetAppCallbacks.mpas,520 :: 		if ProxyData^.MsgFlags and MSG_DCC_ADDRESS_ALLOCATED = MSG_DCC_ADDRESS_ALLOCATED then
	INC	W0
	ZE	[W0], W0
	AND	W0, #1, W0
	CP	W0, #1
	BRA Z	L__AppCallback_StateMachine256
	GOTO	L__AppCallback_StateMachine173
L__AppCallback_StateMachine256:
;NMRAnetAppCallbacks.mpas,522 :: 		DataBytesPtr^[0] := $06;
	MOV.B	#6, W0
	MOV.B	W0, [W12]
;NMRAnetAppCallbacks.mpas,523 :: 		DataBytesPtr^[1] := $01;
	ADD	W12, #1, W1
	MOV.B	#1, W0
	MOV.B	W0, [W1]
;NMRAnetAppCallbacks.mpas,524 :: 		DataBytesPtr^[2] := $00;
	ADD	W12, #2, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetAppCallbacks.mpas,525 :: 		DataBytesPtr^[3] := $00;
	ADD	W12, #3, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetAppCallbacks.mpas,526 :: 		DataBytesPtr^[4] := (ProxyData^.Address shr 8) and $00FF;
	ADD	W12, #4, W2
	ADD	W3, #8, W0
	MOV	[W0], W0
	LSR	W0, #8, W1
	MOV	#255, W0
	AND	W1, W0, W0
	MOV.B	W0, [W2]
;NMRAnetAppCallbacks.mpas,527 :: 		DataBytesPtr^[5] := ProxyData^.Address and $00FF;
	ADD	W12, #5, W2
	ADD	W3, #8, W1
	MOV	#255, W0
	AND	W0, [W1], W0
	MOV.B	W0, [W2]
;NMRAnetAppCallbacks.mpas,528 :: 		DataBytesPtr^[6] := $00;
	ADD	W12, #6, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetAppCallbacks.mpas,529 :: 		DataBytesPtr^[7] := $01;
	ADD	W12, #7, W1
	MOV.B	#1, W0
	MOV.B	W0, [W1]
;NMRAnetAppCallbacks.mpas,530 :: 		if TransmitNMRABusLayerMsg(Node, CANBUffer, MTI_PRODUCER_IDENTIFIED_SET, 0, 8, DataBytesPtr, False) then
	PUSH	W3
	CLR	W0
	PUSH	W0
	PUSH	W12
	MOV	#8, W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	MOV	#16384, W12
	MOV	#2388, W13
	CALL	_TransmitNMRABusLayerMsg
	SUB	#8, W15
	POP	W3
	CP0	W0
	BRA NZ	L__AppCallback_StateMachine257
	GOTO	L__AppCallback_StateMachine176
L__AppCallback_StateMachine257:
;NMRAnetAppCallbacks.mpas,532 :: 		ProxyData^.MsgFlags := ProxyData^.MsgFlags and not MSG_DCC_ADDRESS_ALLOCATED;
	ADD	W3, #1, W2
; ProxyData end address is: 6 (W3)
	ZE	[W2], W1
	MOV	#254, W0
	AND	W1, W0, W0
	MOV.B	W0, [W2]
;NMRAnetAppCallbacks.mpas,533 :: 		Result := True;
	MOV	#65535, W0
	MOV	W0, [W14+0]
;NMRAnetAppCallbacks.mpas,534 :: 		end
L__AppCallback_StateMachine176:
;NMRAnetAppCallbacks.mpas,535 :: 		end;
L__AppCallback_StateMachine173:
;NMRAnetAppCallbacks.mpas,536 :: 		end;
	MOV	[W14+0], W0
L_end_AppCallback_StateMachine:
	POP	W13
	POP	W12
	ULNK
	RETURN
; end of _AppCallback_StateMachine
