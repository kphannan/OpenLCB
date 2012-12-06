
_NMRAnetBufferPools_100ms_TimeTick:

;NMRAnetBufferPools.mpas,107 :: 		begin
;NMRAnetBufferPools.mpas,108 :: 		for i := 0 to DatagramPool.Count - 1 do
; i start address is: 10 (W5)
	CLR	W5
; i end address is: 10 (W5)
L__NMRAnetBufferPools_100ms_TimeTick1:
; i start address is: 10 (W5)
	MOV	#lo_addr(_DatagramPool+185), W0
	ZE	[W0], W0
	SUB	W0, #1, W4
	CP	W5, W4
	BRA LE	L__NMRAnetBufferPools_100ms_TimeTick62
	GOTO	L__NMRAnetBufferPools_100ms_TimeTick5
L__NMRAnetBufferPools_100ms_TimeTick62:
;NMRAnetBufferPools.mpas,110 :: 		if DatagramPool.Pool[i].State and CBS_ALLOCATED = CBS_ALLOCATED then
	MOV	#92, W0
	MUL.UU	W0, W5, W2
	MOV	#lo_addr(_DatagramPool), W0
	ADD	W0, W2, W0
	MOV.B	[W0], W0
	ZE	W0, W0
	AND	W0, #1, W0
	CP	W0, #1
	BRA Z	L__NMRAnetBufferPools_100ms_TimeTick63
	GOTO	L__NMRAnetBufferPools_100ms_TimeTick7
L__NMRAnetBufferPools_100ms_TimeTick63:
;NMRAnetBufferPools.mpas,112 :: 		if DatagramPool.Pool[i].iWatchdog < DATAGRAM_WATCHDOG_MAX then
	MOV	#92, W0
	MUL.UU	W0, W5, W2
	MOV	#lo_addr(_DatagramPool), W0
	ADD	W0, W2, W1
	MOV	#88, W0
	ADD	W1, W0, W0
	MOV	[W0], W0
	CP	W0, #30
	BRA LTU	L__NMRAnetBufferPools_100ms_TimeTick64
	GOTO	L__NMRAnetBufferPools_100ms_TimeTick10
L__NMRAnetBufferPools_100ms_TimeTick64:
;NMRAnetBufferPools.mpas,113 :: 		Inc(DatagramPool.Pool[i].iWatchdog);
	MOV	#92, W0
	MUL.UU	W0, W5, W2
	MOV	#lo_addr(_DatagramPool), W0
	ADD	W0, W2, W1
	MOV	#88, W0
	ADD	W1, W0, W1
	MOV	[W1], W0
	INC	W0
	MOV	W0, [W1]
L__NMRAnetBufferPools_100ms_TimeTick10:
;NMRAnetBufferPools.mpas,114 :: 		end;
L__NMRAnetBufferPools_100ms_TimeTick7:
;NMRAnetBufferPools.mpas,115 :: 		end;
	CP	W5, W4
	BRA NZ	L__NMRAnetBufferPools_100ms_TimeTick65
	GOTO	L__NMRAnetBufferPools_100ms_TimeTick5
L__NMRAnetBufferPools_100ms_TimeTick65:
; i start address is: 10 (W5)
	INC	W5
; i end address is: 10 (W5)
; i end address is: 10 (W5)
	GOTO	L__NMRAnetBufferPools_100ms_TimeTick1
L__NMRAnetBufferPools_100ms_TimeTick5:
;NMRAnetBufferPools.mpas,116 :: 		end;
L_end_NMRAnetBufferPools_100ms_TimeTick:
	RETURN
; end of _NMRAnetBufferPools_100ms_TimeTick

_NMRAnetBufferPools_InitializeBaseBuffer:

;NMRAnetBufferPools.mpas,128 :: 		begin
;NMRAnetBufferPools.mpas,129 :: 		Buffer^.State := 0;
	CLR	W0
	MOV.B	W0, [W10]
;NMRAnetBufferPools.mpas,130 :: 		Buffer^.Alias := 0;
	ADD	W10, #2, W1
	CLR	W0
	MOV	W0, [W1]
;NMRAnetBufferPools.mpas,131 :: 		Buffer^.Next := nil;
	ADD	W10, #4, W1
	CLR	W0
	MOV	W0, [W1]
;NMRAnetBufferPools.mpas,132 :: 		Buffer^.mCode := 0;
	ADD	W10, #6, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetBufferPools.mpas,133 :: 		Buffer^.Tag := 0;
	ADD	W10, #8, W1
	CLR	W0
	MOV	W0, [W1]
;NMRAnetBufferPools.mpas,134 :: 		Buffer^.StateMachine := 0;
	ADD	W10, #7, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetBufferPools.mpas,135 :: 		end;
L_end_NMRAnetBufferPools_InitializeBaseBuffer:
	RETURN
; end of _NMRAnetBufferPools_InitializeBaseBuffer

_NMRAnetBufferPools_Initialize:

;NMRAnetBufferPools.mpas,149 :: 		begin
;NMRAnetBufferPools.mpas,150 :: 		for i := 0 to MAX_BASE_BUFFER_POOL - 1 do
	PUSH	W10
; i start address is: 8 (W4)
	CLR	W4
; i end address is: 8 (W4)
L__NMRAnetBufferPools_Initialize15:
;NMRAnetBufferPools.mpas,151 :: 		NMRAnetBufferPools_InitializeBaseBuffer(@BaseBufferPool.Pool[i]);
; i start address is: 8 (W4)
	MOV	#10, W0
	MUL.UU	W0, W4, W2
	MOV	#lo_addr(_BaseBufferPool), W0
	ADD	W0, W2, W0
	MOV	W0, W10
	CALL	_NMRAnetBufferPools_InitializeBaseBuffer
	MOV	#129, W0
	CP	W4, W0
	BRA NZ	L__NMRAnetBufferPools_Initialize68
	GOTO	L__NMRAnetBufferPools_Initialize18
L__NMRAnetBufferPools_Initialize68:
; i start address is: 8 (W4)
	INC	W4
; i end address is: 8 (W4)
; i end address is: 8 (W4)
	GOTO	L__NMRAnetBufferPools_Initialize15
L__NMRAnetBufferPools_Initialize18:
;NMRAnetBufferPools.mpas,152 :: 		BaseBufferPool.Count := 0;
	MOV	#lo_addr(_BaseBufferPool+1301), W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetBufferPools.mpas,153 :: 		BaseBufferPool.MaxCount := 0;
	MOV	#lo_addr(_BaseBufferPool+1300), W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetBufferPools.mpas,155 :: 		for i := 0 to MAX_DATAGRAM_BUFFER_POOL - 1 do
; i start address is: 2 (W1)
	CLR	W1
; i end address is: 2 (W1)
L__NMRAnetBufferPools_Initialize20:
;NMRAnetBufferPools.mpas,156 :: 		NMRAnetDatagrams_InitializeDatagramBuffer(@DatagramPool.Pool[i]);
; i start address is: 2 (W1)
	MOV	#92, W0
	MUL.UU	W0, W1, W2
	MOV	#lo_addr(_DatagramPool), W0
	ADD	W0, W2, W0
	PUSH	W1
	MOV	W0, W10
	CALL	_NMRAnetDatagrams_InitializeDatagramBuffer
	POP	W1
	CP	W1, #1
	BRA NZ	L__NMRAnetBufferPools_Initialize69
	GOTO	L__NMRAnetBufferPools_Initialize23
L__NMRAnetBufferPools_Initialize69:
; i start address is: 2 (W1)
	INC	W1
; i end address is: 2 (W1)
; i end address is: 2 (W1)
	GOTO	L__NMRAnetBufferPools_Initialize20
L__NMRAnetBufferPools_Initialize23:
;NMRAnetBufferPools.mpas,157 :: 		DatagramPool.Count := 0;
	MOV	#lo_addr(_DatagramPool+185), W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetBufferPools.mpas,158 :: 		DatagramPool.MaxCount := 0;
	MOV	#lo_addr(_DatagramPool+184), W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetBufferPools.mpas,159 :: 		end;
L_end_NMRAnetBufferPools_Initialize:
	POP	W10
	RETURN
; end of _NMRAnetBufferPools_Initialize

_NMRAnetBufferPools_AllocateBaseBuffer:

;NMRAnetBufferPools.mpas,174 :: 		begin
;NMRAnetBufferPools.mpas,175 :: 		Result := False;
; Result start address is: 2 (W1)
	CLR	W1
;NMRAnetBufferPools.mpas,176 :: 		for i := 0 to MAX_BASE_BUFFER_POOL - 1 do
; i start address is: 8 (W4)
	CLR	W4
; i end address is: 8 (W4)
L__NMRAnetBufferPools_AllocateBaseBuffer26:
;NMRAnetBufferPools.mpas,178 :: 		if BaseBufferPool.Pool[i].State = 0 then
; i start address is: 8 (W4)
; Result start address is: 2 (W1)
; Result end address is: 2 (W1)
	MOV	#10, W0
	MUL.UU	W0, W4, W2
	MOV	#lo_addr(_BaseBufferPool), W0
	ADD	W0, W2, W0
	MOV.B	[W0], W0
	CP.B	W0, #0
	BRA Z	L__NMRAnetBufferPools_AllocateBaseBuffer71
	GOTO	L__NMRAnetBufferPools_AllocateBaseBuffer31
L__NMRAnetBufferPools_AllocateBaseBuffer71:
; Result end address is: 2 (W1)
;NMRAnetBufferPools.mpas,180 :: 		Buffer := @BaseBufferPool.Pool[i];
	MOV	#10, W0
	MUL.UU	W0, W4, W2
; i end address is: 8 (W4)
	MOV	#lo_addr(_BaseBufferPool), W0
	ADD	W0, W2, [W10]
;NMRAnetBufferPools.mpas,182 :: 		NMRAnetBufferPools_InitializeBaseBuffer(Buffer);
	PUSH	W10
	MOV	[W10], W10
	CALL	_NMRAnetBufferPools_InitializeBaseBuffer
	POP	W10
;NMRAnetBufferPools.mpas,183 :: 		Buffer^.State := CBS_ALLOCATED;
	MOV	[W10], W1
	MOV.B	#1, W0
	MOV.B	W0, [W1]
;NMRAnetBufferPools.mpas,184 :: 		Inc(BaseBufferPool.Count);
	MOV	#lo_addr(_BaseBufferPool+1301), W0
	ZE	[W0], W0
	ADD	W0, #1, W1
	MOV	#lo_addr(_BaseBufferPool+1301), W0
	MOV.B	W1, [W0]
;NMRAnetBufferPools.mpas,185 :: 		if BaseBufferPool.Count >= BaseBufferPool.MaxCount then
	MOV	#lo_addr(_BaseBufferPool+1301), W0
	MOV.B	[W0], W1
	MOV	#lo_addr(_BaseBufferPool+1300), W0
	CP.B	W1, [W0]
	BRA GEU	L__NMRAnetBufferPools_AllocateBaseBuffer72
	GOTO	L__NMRAnetBufferPools_AllocateBaseBuffer34
L__NMRAnetBufferPools_AllocateBaseBuffer72:
;NMRAnetBufferPools.mpas,186 :: 		BaseBufferPool.MaxCount := BaseBufferPool.Count;
	MOV	#lo_addr(_BaseBufferPool+1300), W1
	MOV	#lo_addr(_BaseBufferPool+1301), W0
	MOV.B	[W0], [W1]
L__NMRAnetBufferPools_AllocateBaseBuffer34:
;NMRAnetBufferPools.mpas,187 :: 		Result := True;
; Result start address is: 2 (W1)
	MOV	#65535, W1
;NMRAnetBufferPools.mpas,189 :: 		Break
	GOTO	L__NMRAnetBufferPools_AllocateBaseBuffer29
;NMRAnetBufferPools.mpas,190 :: 		end
L__NMRAnetBufferPools_AllocateBaseBuffer31:
;NMRAnetBufferPools.mpas,191 :: 		end;
; i start address is: 8 (W4)
	MOV	#129, W0
	CP	W4, W0
	BRA NZ	L__NMRAnetBufferPools_AllocateBaseBuffer73
	GOTO	L__NMRAnetBufferPools_AllocateBaseBuffer59
L__NMRAnetBufferPools_AllocateBaseBuffer73:
; i start address is: 8 (W4)
	INC	W4
; i end address is: 8 (W4)
; i end address is: 8 (W4)
; Result end address is: 2 (W1)
	GOTO	L__NMRAnetBufferPools_AllocateBaseBuffer26
L__NMRAnetBufferPools_AllocateBaseBuffer59:
L__NMRAnetBufferPools_AllocateBaseBuffer29:
;NMRAnetBufferPools.mpas,193 :: 		end;
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetBufferPools_AllocateBaseBuffer:
	RETURN
; end of _NMRAnetBufferPools_AllocateBaseBuffer

_NMRAnetBufferPools_ReleaseBaseBuffer:

;NMRAnetBufferPools.mpas,205 :: 		begin
;NMRAnetBufferPools.mpas,206 :: 		if Buffer <> nil then
	CP	W10, #0
	BRA NZ	L__NMRAnetBufferPools_ReleaseBaseBuffer75
	GOTO	L__NMRAnetBufferPools_ReleaseBaseBuffer38
L__NMRAnetBufferPools_ReleaseBaseBuffer75:
;NMRAnetBufferPools.mpas,209 :: 		Buffer^.State := 0;
	CLR	W0
	MOV.B	W0, [W10]
;NMRAnetBufferPools.mpas,210 :: 		Dec(BaseBufferPool.Count);
	MOV	#lo_addr(_BaseBufferPool+1301), W0
	ZE	[W0], W0
	SUB	W0, #1, W1
	MOV	#lo_addr(_BaseBufferPool+1301), W0
	MOV.B	W1, [W0]
;NMRAnetBufferPools.mpas,211 :: 		end;
L__NMRAnetBufferPools_ReleaseBaseBuffer38:
;NMRAnetBufferPools.mpas,212 :: 		end;
L_end_NMRAnetBufferPools_ReleaseBaseBuffer:
	RETURN
; end of _NMRAnetBufferPools_ReleaseBaseBuffer

_NMRAnetBufferPools_AllocateDatagramBuffer:

;NMRAnetBufferPools.mpas,226 :: 		begin
;NMRAnetBufferPools.mpas,227 :: 		Result := False;
; Result start address is: 2 (W1)
	CLR	W1
;NMRAnetBufferPools.mpas,228 :: 		for i := 0 to MAX_DATAGRAM_BUFFER_POOL - 1 do
; i start address is: 8 (W4)
	CLR	W4
; i end address is: 8 (W4)
L__NMRAnetBufferPools_AllocateDatagramBuffer42:
;NMRAnetBufferPools.mpas,230 :: 		if DatagramPool.Pool[i].State = 0 then
; i start address is: 8 (W4)
; Result start address is: 2 (W1)
; Result end address is: 2 (W1)
	MOV	#92, W0
	MUL.UU	W0, W4, W2
	MOV	#lo_addr(_DatagramPool), W0
	ADD	W0, W2, W0
	MOV.B	[W0], W0
	CP.B	W0, #0
	BRA Z	L__NMRAnetBufferPools_AllocateDatagramBuffer77
	GOTO	L__NMRAnetBufferPools_AllocateDatagramBuffer47
L__NMRAnetBufferPools_AllocateDatagramBuffer77:
; Result end address is: 2 (W1)
;NMRAnetBufferPools.mpas,232 :: 		Buffer := @DatagramPool.Pool[i];
	MOV	#92, W0
	MUL.UU	W0, W4, W2
; i end address is: 8 (W4)
	MOV	#lo_addr(_DatagramPool), W0
	ADD	W0, W2, [W10]
;NMRAnetBufferPools.mpas,234 :: 		NMRAnetDatagrams_InitializeDatagramBuffer(Buffer);
	PUSH.D	W10
	MOV	[W10], W10
	CALL	_NMRAnetDatagrams_InitializeDatagramBuffer
	POP.D	W10
;NMRAnetBufferPools.mpas,235 :: 		Buffer^.State := CBS_ALLOCATED;
	MOV	[W10], W1
	MOV.B	#1, W0
	MOV.B	W0, [W1]
;NMRAnetBufferPools.mpas,236 :: 		if ForTransmit then
	CP0	W11
	BRA NZ	L__NMRAnetBufferPools_AllocateDatagramBuffer78
	GOTO	L__NMRAnetBufferPools_AllocateDatagramBuffer50
L__NMRAnetBufferPools_AllocateDatagramBuffer78:
;NMRAnetBufferPools.mpas,237 :: 		Buffer^.State := Buffer^.State or CBS_OUTGOING;
	MOV	[W10], W1
	MOV.B	[W1], W0
	ZE	W0, W0
	IOR	W0, #4, W0
	MOV.B	W0, [W1]
L__NMRAnetBufferPools_AllocateDatagramBuffer50:
;NMRAnetBufferPools.mpas,238 :: 		Inc(DatagramPool.Count);
	MOV	#lo_addr(_DatagramPool+185), W0
	ZE	[W0], W0
	ADD	W0, #1, W1
	MOV	#lo_addr(_DatagramPool+185), W0
	MOV.B	W1, [W0]
;NMRAnetBufferPools.mpas,239 :: 		if DatagramPool.Count >= DatagramPool.MaxCount then
	MOV	#lo_addr(_DatagramPool+185), W0
	MOV.B	[W0], W1
	MOV	#lo_addr(_DatagramPool+184), W0
	CP.B	W1, [W0]
	BRA GEU	L__NMRAnetBufferPools_AllocateDatagramBuffer79
	GOTO	L__NMRAnetBufferPools_AllocateDatagramBuffer53
L__NMRAnetBufferPools_AllocateDatagramBuffer79:
;NMRAnetBufferPools.mpas,240 :: 		DatagramPool.MaxCount := DatagramPool.Count;
	MOV	#lo_addr(_DatagramPool+184), W1
	MOV	#lo_addr(_DatagramPool+185), W0
	MOV.B	[W0], [W1]
L__NMRAnetBufferPools_AllocateDatagramBuffer53:
;NMRAnetBufferPools.mpas,242 :: 		Result := True;
; Result start address is: 2 (W1)
	MOV	#65535, W1
;NMRAnetBufferPools.mpas,243 :: 		Break
	GOTO	L__NMRAnetBufferPools_AllocateDatagramBuffer45
;NMRAnetBufferPools.mpas,244 :: 		end
L__NMRAnetBufferPools_AllocateDatagramBuffer47:
;NMRAnetBufferPools.mpas,245 :: 		end;
; i start address is: 8 (W4)
	CP	W4, #1
	BRA NZ	L__NMRAnetBufferPools_AllocateDatagramBuffer80
	GOTO	L__NMRAnetBufferPools_AllocateDatagramBuffer60
L__NMRAnetBufferPools_AllocateDatagramBuffer80:
; i start address is: 8 (W4)
	INC	W4
; i end address is: 8 (W4)
; i end address is: 8 (W4)
; Result end address is: 2 (W1)
	GOTO	L__NMRAnetBufferPools_AllocateDatagramBuffer42
L__NMRAnetBufferPools_AllocateDatagramBuffer60:
L__NMRAnetBufferPools_AllocateDatagramBuffer45:
;NMRAnetBufferPools.mpas,247 :: 		end;
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetBufferPools_AllocateDatagramBuffer:
	RETURN
; end of _NMRAnetBufferPools_AllocateDatagramBuffer

_NMRAnetBufferPools_ReleaseDatagramBuffer:

;NMRAnetBufferPools.mpas,259 :: 		begin
;NMRAnetBufferPools.mpas,260 :: 		if Buffer <> nil then
	CP	W10, #0
	BRA NZ	L__NMRAnetBufferPools_ReleaseDatagramBuffer82
	GOTO	L__NMRAnetBufferPools_ReleaseDatagramBuffer57
L__NMRAnetBufferPools_ReleaseDatagramBuffer82:
;NMRAnetBufferPools.mpas,263 :: 		Buffer^.State := 0;
	CLR	W0
	MOV.B	W0, [W10]
;NMRAnetBufferPools.mpas,264 :: 		Dec(DatagramPool.Count);
	MOV	#lo_addr(_DatagramPool+185), W0
	ZE	[W0], W0
	SUB	W0, #1, W1
	MOV	#lo_addr(_DatagramPool+185), W0
	MOV.B	W1, [W0]
;NMRAnetBufferPools.mpas,265 :: 		end;
L__NMRAnetBufferPools_ReleaseDatagramBuffer57:
;NMRAnetBufferPools.mpas,266 :: 		end;
L_end_NMRAnetBufferPools_ReleaseDatagramBuffer:
	RETURN
; end of _NMRAnetBufferPools_ReleaseDatagramBuffer
