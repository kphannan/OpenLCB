
_NMRAnetUtilities_LoadFrameCANBufferDataWith48BitNodeID:

;NMRAnetUtilities.mpas,87 :: 		begin
;NMRAnetUtilities.mpas,88 :: 		CANBuffer^.DataCount := 6;
	ADD	W11, #4, W1
	MOV.B	#6, W0
	MOV.B	W0, [W1]
;NMRAnetUtilities.mpas,89 :: 		CANBuffer^.DataBytes[0] := NodeBuffer^.Info.ID[1] shr 16;  // But these all need the 48 Bit Full ID in the Byte Fields
	ADD	W11, #5, W3
	ADD	W10, #2, W0
	ADD	W0, #4, W2
	MOV.D	[W2], W0
	MOV	W1, W0
	CLR	W1
	MOV.B	W0, [W3]
;NMRAnetUtilities.mpas,90 :: 		CANBuffer^.DataBytes[1] := NodeBuffer^.Info.ID[1] shr 8;
	ADD	W11, #5, W0
	ADD	W0, #1, W4
	ADD	W10, #2, W0
	ADD	W0, #4, W3
	MOV	#8, W2
	MOV.D	[W3], W0
L__NMRAnetUtilities_LoadFrameCANBufferDataWith48BitNodeID249:
	DEC	W2, W2
	BRA LT	L__NMRAnetUtilities_LoadFrameCANBufferDataWith48BitNodeID250
	LSR	W1, W1
	RRC	W0, W0
	BRA	L__NMRAnetUtilities_LoadFrameCANBufferDataWith48BitNodeID249
L__NMRAnetUtilities_LoadFrameCANBufferDataWith48BitNodeID250:
	MOV.B	W0, [W4]
;NMRAnetUtilities.mpas,91 :: 		CANBuffer^.DataBytes[2] := NodeBuffer^.Info.ID[1];
	ADD	W11, #5, W0
	ADD	W0, #2, W1
	ADD	W10, #2, W0
	ADD	W0, #4, W0
	MOV.B	[W0], [W1]
;NMRAnetUtilities.mpas,92 :: 		CANBuffer^.DataBytes[3] := NodeBuffer^.Info.ID[0] shr 16;
	ADD	W11, #5, W0
	ADD	W0, #3, W3
	ADD	W10, #2, W2
	MOV.D	[W2], W0
	MOV	W1, W0
	CLR	W1
	MOV.B	W0, [W3]
;NMRAnetUtilities.mpas,93 :: 		CANBuffer^.DataBytes[4] := NodeBuffer^.Info.ID[0] shr 8;
	ADD	W11, #5, W0
	ADD	W0, #4, W5
	ADD	W10, #2, W0
	MOV	[W0++], W3
	MOV	[W0--], W4
	MOV	#8, W2
	MOV	W3, W0
	MOV	W4, W1
L__NMRAnetUtilities_LoadFrameCANBufferDataWith48BitNodeID251:
	DEC	W2, W2
	BRA LT	L__NMRAnetUtilities_LoadFrameCANBufferDataWith48BitNodeID252
	LSR	W1, W1
	RRC	W0, W0
	BRA	L__NMRAnetUtilities_LoadFrameCANBufferDataWith48BitNodeID251
L__NMRAnetUtilities_LoadFrameCANBufferDataWith48BitNodeID252:
	MOV.B	W0, [W5]
;NMRAnetUtilities.mpas,94 :: 		CANBuffer^.DataBytes[5] := NodeBuffer^.Info.ID[0];
	ADD	W11, #5, W0
	ADD	W0, #5, W3
	ADD	W10, #2, W2
	MOV.D	[W2], W0
	MOV.B	W0, [W3]
;NMRAnetUtilities.mpas,95 :: 		end;
L_end_NMRAnetUtilities_LoadFrameCANBufferDataWith48BitNodeID:
	RETURN
; end of _NMRAnetUtilities_LoadFrameCANBufferDataWith48BitNodeID

_NMRAnetUtilities_CreateCANControlFrameCANBuffer:

;NMRAnetUtilities.mpas,106 :: 		begin
;NMRAnetUtilities.mpas,107 :: 		CANBuffer^.DataCount := 0;
	ADD	W11, #4, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetUtilities.mpas,108 :: 		CANBuffer^.State := CANBuffer^.State or BS_EXTENDED;
	ADD	W11, #13, W1
	ZE	[W1], W0
	IOR	W0, #1, W0
	MOV.B	W0, [W1]
;NMRAnetUtilities.mpas,109 :: 		CANBuffer^.ID := $10000000 or MTI or NodeBuffer^.Info.AliasID;  // RID, AMD, AME, AMR are all covered with the Reserved bit, Variable Field value and Source Node Alias
	MOV	#0, W0
	MOV	#4096, W1
	IOR	W0, W12, W2
	IOR	W1, W13, W3
	ADD	W10, #2, W0
	ADD	W0, #16, W0
	MOV	[W0], W0
	CLR	W1
	IOR	W2, W0, W0
	IOR	W3, W1, W1
	MOV.D	W0, [W11]
;NMRAnetUtilities.mpas,110 :: 		if MTI = MTI_CID0 then CANBuffer^.ID := CANBuffer^.ID or (NodeBuffer^.Info.ID[1] and $00FFF000) else
	MOV	#0, W0
	MOV	#1792, W1
	CP	W12, W0
	CPB	W13, W1
	BRA Z	L__NMRAnetUtilities_CreateCANControlFrameCANBuffer254
	GOTO	L__NMRAnetUtilities_CreateCANControlFrameCANBuffer3
L__NMRAnetUtilities_CreateCANControlFrameCANBuffer254:
	ADD	W10, #2, W0
	ADD	W0, #4, W0
	MOV.D	[W0], W2
	MOV	#61440, W0
	MOV	#255, W1
	AND	W2, W0, W2
	AND	W3, W1, W3
	MOV.D	[W11], W0
	IOR	W0, W2, [W11++]
	IOR	W1, W3, [W11--]
	GOTO	L__NMRAnetUtilities_CreateCANControlFrameCANBuffer4
L__NMRAnetUtilities_CreateCANControlFrameCANBuffer3:
;NMRAnetUtilities.mpas,111 :: 		if MTI = MTI_CID1 then CANBuffer^.ID := CANBuffer^.ID or ((NodeBuffer^.Info.ID[1] shl 12) and $00FFF000) else
	MOV	#0, W0
	MOV	#1536, W1
	CP	W12, W0
	CPB	W13, W1
	BRA Z	L__NMRAnetUtilities_CreateCANControlFrameCANBuffer255
	GOTO	L__NMRAnetUtilities_CreateCANControlFrameCANBuffer6
L__NMRAnetUtilities_CreateCANControlFrameCANBuffer255:
	ADD	W10, #2, W0
	ADD	W0, #4, W1
	MOV	#12, W0
	MOV.D	[W1], W2
L__NMRAnetUtilities_CreateCANControlFrameCANBuffer256:
	DEC	W0, W0
	BRA LT	L__NMRAnetUtilities_CreateCANControlFrameCANBuffer257
	SL	W2, W2
	RLC	W3, W3
	BRA	L__NMRAnetUtilities_CreateCANControlFrameCANBuffer256
L__NMRAnetUtilities_CreateCANControlFrameCANBuffer257:
	MOV	#61440, W0
	MOV	#255, W1
	AND	W2, W0, W2
	AND	W3, W1, W3
	MOV.D	[W11], W0
	IOR	W0, W2, [W11++]
	IOR	W1, W3, [W11--]
	GOTO	L__NMRAnetUtilities_CreateCANControlFrameCANBuffer7
L__NMRAnetUtilities_CreateCANControlFrameCANBuffer6:
;NMRAnetUtilities.mpas,112 :: 		if MTI = MTI_CID2 then CANBuffer^.ID := CANBuffer^.ID or (NodeBuffer^.Info.ID[0] and $00FFF000) else
	MOV	#0, W0
	MOV	#1280, W1
	CP	W12, W0
	CPB	W13, W1
	BRA Z	L__NMRAnetUtilities_CreateCANControlFrameCANBuffer258
	GOTO	L__NMRAnetUtilities_CreateCANControlFrameCANBuffer9
L__NMRAnetUtilities_CreateCANControlFrameCANBuffer258:
	ADD	W10, #2, W0
	MOV.D	[W0], W2
	MOV	#61440, W0
	MOV	#255, W1
	AND	W2, W0, W2
	AND	W3, W1, W3
	MOV.D	[W11], W0
	IOR	W0, W2, [W11++]
	IOR	W1, W3, [W11--]
	GOTO	L__NMRAnetUtilities_CreateCANControlFrameCANBuffer10
L__NMRAnetUtilities_CreateCANControlFrameCANBuffer9:
;NMRAnetUtilities.mpas,113 :: 		if MTI = MTI_CID3 then CANBuffer^.ID := CANBuffer^.ID or ((NodeBuffer^.Info.ID[0] shl 12) and $00FFF000);
	MOV	#0, W0
	MOV	#1024, W1
	CP	W12, W0
	CPB	W13, W1
	BRA Z	L__NMRAnetUtilities_CreateCANControlFrameCANBuffer259
	GOTO	L__NMRAnetUtilities_CreateCANControlFrameCANBuffer12
L__NMRAnetUtilities_CreateCANControlFrameCANBuffer259:
	ADD	W10, #2, W0
	MOV.D	[W0], W4
	MOV	#12, W0
	MOV.D	W4, W2
L__NMRAnetUtilities_CreateCANControlFrameCANBuffer260:
	DEC	W0, W0
	BRA LT	L__NMRAnetUtilities_CreateCANControlFrameCANBuffer261
	SL	W2, W2
	RLC	W3, W3
	BRA	L__NMRAnetUtilities_CreateCANControlFrameCANBuffer260
L__NMRAnetUtilities_CreateCANControlFrameCANBuffer261:
	MOV	#61440, W0
	MOV	#255, W1
	AND	W2, W0, W2
	AND	W3, W1, W3
	MOV.D	[W11], W0
	IOR	W0, W2, [W11++]
	IOR	W1, W3, [W11--]
L__NMRAnetUtilities_CreateCANControlFrameCANBuffer12:
L__NMRAnetUtilities_CreateCANControlFrameCANBuffer10:
L__NMRAnetUtilities_CreateCANControlFrameCANBuffer7:
L__NMRAnetUtilities_CreateCANControlFrameCANBuffer4:
;NMRAnetUtilities.mpas,114 :: 		if (MTI = MTI_AMD) or (MTI = MTI_AME) or (MTI = MTI_AMR) then
	MOV	#4096, W0
	MOV	#112, W1
	CP	W12, W0
	CPB	W13, W1
	CLR	W2
	BRA NZ	L__NMRAnetUtilities_CreateCANControlFrameCANBuffer262
	COM	W2
L__NMRAnetUtilities_CreateCANControlFrameCANBuffer262:
	MOV	#8192, W0
	MOV	#112, W1
	CP	W12, W0
	CPB	W13, W1
	CLR	W0
	BRA NZ	L__NMRAnetUtilities_CreateCANControlFrameCANBuffer263
	COM	W0
L__NMRAnetUtilities_CreateCANControlFrameCANBuffer263:
	IOR	W2, W0, W2
	MOV	#12288, W0
	MOV	#112, W1
	CP	W12, W0
	CPB	W13, W1
	CLR	W0
	BRA NZ	L__NMRAnetUtilities_CreateCANControlFrameCANBuffer264
	COM	W0
L__NMRAnetUtilities_CreateCANControlFrameCANBuffer264:
	IOR	W2, W0, W0
	BRA NZ	L__NMRAnetUtilities_CreateCANControlFrameCANBuffer265
	GOTO	L__NMRAnetUtilities_CreateCANControlFrameCANBuffer15
L__NMRAnetUtilities_CreateCANControlFrameCANBuffer265:
;NMRAnetUtilities.mpas,115 :: 		NMRAnetUtilities_LoadFrameCANBufferDataWith48BitNodeID(NodeBuffer, CANBuffer);
	CALL	_NMRAnetUtilities_LoadFrameCANBufferDataWith48BitNodeID
L__NMRAnetUtilities_CreateCANControlFrameCANBuffer15:
;NMRAnetUtilities.mpas,116 :: 		end;
L_end_NMRAnetUtilities_CreateCANControlFrameCANBuffer:
	RETURN
; end of _NMRAnetUtilities_CreateCANControlFrameCANBuffer

_NMRAnetUtilities_CreateNMRABusMessageCANBuffer:
	LNK	#0

;NMRAnetUtilities.mpas,130 :: 		begin
;NMRAnetUtilities.mpas,131 :: 		Offset := 0;
; DestinationAlias start address is: 2 (W1)
	MOV	[W14-8], W1
; ByteCount start address is: 6 (W3)
	MOV.B	[W14-10], W3
; DataBytes start address is: 4 (W2)
	MOV	[W14-12], W2
; Offset start address is: 0 (W0)
	CLR	W0
;NMRAnetUtilities.mpas,132 :: 		if DataBytes <> nil then
	CP	W2, #0
	BRA NZ	L__NMRAnetUtilities_CreateNMRABusMessageCANBuffer267
	GOTO	L__NMRAnetUtilities_CreateNMRABusMessageCANBuffer19
L__NMRAnetUtilities_CreateNMRABusMessageCANBuffer267:
;NMRAnetUtilities.mpas,134 :: 		if DestinationAlias <> 0 then
	CP	W1, #0
	BRA NZ	L__NMRAnetUtilities_CreateNMRABusMessageCANBuffer268
	GOTO	L__NMRAnetUtilities_CreateNMRABusMessageCANBuffer236
L__NMRAnetUtilities_CreateNMRABusMessageCANBuffer268:
; Offset end address is: 0 (W0)
;NMRAnetUtilities.mpas,136 :: 		NMRAnetUtilities_LoadDestinationAlias(DestinationAlias, @Buffer^.DataBytes);
	ADD	W11, #5, W0
	PUSH.D	W10
	MOV	W0, W11
	MOV	W1, W10
; DestinationAlias end address is: 2 (W1)
	CALL	_NMRAnetUtilities_LoadDestinationAlias
	POP.D	W10
;NMRAnetUtilities.mpas,137 :: 		ByteCount := ByteCount + 2;
; ByteCount start address is: 2 (W1)
	ADD.B	W3, #2, W1
; ByteCount end address is: 6 (W3)
;NMRAnetUtilities.mpas,138 :: 		Offset := 2;
; Offset start address is: 6 (W3)
	MOV	#2, W3
; ByteCount end address is: 2 (W1)
; Offset end address is: 6 (W3)
;NMRAnetUtilities.mpas,139 :: 		end;
	GOTO	L__NMRAnetUtilities_CreateNMRABusMessageCANBuffer22
L__NMRAnetUtilities_CreateNMRABusMessageCANBuffer236:
;NMRAnetUtilities.mpas,134 :: 		if DestinationAlias <> 0 then
	MOV.B	W3, W1
	MOV	W0, W3
;NMRAnetUtilities.mpas,139 :: 		end;
L__NMRAnetUtilities_CreateNMRABusMessageCANBuffer22:
;NMRAnetUtilities.mpas,140 :: 		Buffer^.DataCount := ByteCount;
; Offset start address is: 6 (W3)
; ByteCount start address is: 2 (W1)
	ADD	W11, #4, W0
	MOV.B	W1, [W0]
;NMRAnetUtilities.mpas,141 :: 		for i := 0 to ByteCount do
; i start address is: 10 (W5)
	CLR	W5
; DataBytes end address is: 4 (W2)
; ByteCount end address is: 2 (W1)
; i end address is: 10 (W5)
	MOV	W2, W4
	MOV.B	W1, W2
L__NMRAnetUtilities_CreateNMRABusMessageCANBuffer24:
; i start address is: 10 (W5)
; DataBytes start address is: 8 (W4)
; ByteCount start address is: 4 (W2)
; Offset start address is: 6 (W3)
; Offset end address is: 6 (W3)
; DataBytes start address is: 8 (W4)
; DataBytes end address is: 8 (W4)
	ZE	W2, W0
	CP	W5, W0
	BRA LE	L__NMRAnetUtilities_CreateNMRABusMessageCANBuffer269
	GOTO	L__NMRAnetUtilities_CreateNMRABusMessageCANBuffer28
L__NMRAnetUtilities_CreateNMRABusMessageCANBuffer269:
; Offset end address is: 6 (W3)
; DataBytes end address is: 8 (W4)
;NMRAnetUtilities.mpas,142 :: 		Buffer^.DataBytes[i+Offset] := DataBytes^[i];
; DataBytes start address is: 8 (W4)
; Offset start address is: 6 (W3)
	ADD	W11, #5, W1
	ADD	W5, W3, W0
	ADD	W1, W0, W1
	ADD	W4, W5, W0
	MOV.B	[W0], [W1]
	ZE	W2, W0
	CP	W5, W0
	BRA NZ	L__NMRAnetUtilities_CreateNMRABusMessageCANBuffer270
	GOTO	L__NMRAnetUtilities_CreateNMRABusMessageCANBuffer28
L__NMRAnetUtilities_CreateNMRABusMessageCANBuffer270:
; i start address is: 10 (W5)
	INC	W5
; i end address is: 10 (W5)
; ByteCount end address is: 4 (W2)
; Offset end address is: 6 (W3)
; DataBytes end address is: 8 (W4)
; i end address is: 10 (W5)
	GOTO	L__NMRAnetUtilities_CreateNMRABusMessageCANBuffer24
L__NMRAnetUtilities_CreateNMRABusMessageCANBuffer28:
;NMRAnetUtilities.mpas,143 :: 		end else
	GOTO	L__NMRAnetUtilities_CreateNMRABusMessageCANBuffer20
L__NMRAnetUtilities_CreateNMRABusMessageCANBuffer19:
;NMRAnetUtilities.mpas,144 :: 		Buffer^.DataCount := 0;
	ADD	W11, #4, W1
	CLR	W0
	MOV.B	W0, [W1]
L__NMRAnetUtilities_CreateNMRABusMessageCANBuffer20:
;NMRAnetUtilities.mpas,146 :: 		Buffer^.State := Buffer^.State or BS_EXTENDED;
	ADD	W11, #13, W1
	ZE	[W1], W0
	IOR	W0, #1, W0
	MOV.B	W0, [W1]
;NMRAnetUtilities.mpas,147 :: 		Buffer^.ID := $10000000 or MTI or NodeBuffer^.Info.AliasID;
	MOV	#0, W0
	MOV	#4096, W1
	IOR	W0, W12, W2
	IOR	W1, W13, W3
	ADD	W10, #2, W0
	ADD	W0, #16, W0
	MOV	[W0], W0
	CLR	W1
	IOR	W2, W0, W0
	IOR	W3, W1, W1
	MOV.D	W0, [W11]
;NMRAnetUtilities.mpas,148 :: 		end;
L_end_NMRAnetUtilities_CreateNMRABusMessageCANBuffer:
	ULNK
	RETURN
; end of _NMRAnetUtilities_CreateNMRABusMessageCANBuffer

_NMRAnetUtilities_CreateNMRABusMessageCANBufferWithDestInMTI:
	LNK	#0

;NMRAnetUtilities.mpas,159 :: 		begin
;NMRAnetUtilities.mpas,160 :: 		Buffer^.DataCount := ByteCount;
; DestinationAlias start address is: 12 (W6)
	MOV	[W14-8], W6
; ByteCount start address is: 2 (W1)
	MOV.B	[W14-10], W1
; DataBytes start address is: 4 (W2)
	MOV	[W14-12], W2
	ADD	W11, #4, W0
	MOV.B	W1, [W0]
;NMRAnetUtilities.mpas,161 :: 		if DataBytes <> nil then
	CP	W2, #0
	BRA NZ	L__NMRAnetUtilities_CreateNMRABusMessageCANBufferWithDestInMTI272
	GOTO	L__NMRAnetUtilities_CreateNMRABusMessageCANBufferWithDestInMTI31
L__NMRAnetUtilities_CreateNMRABusMessageCANBufferWithDestInMTI272:
;NMRAnetUtilities.mpas,163 :: 		Buffer^.DataCount := ByteCount;
	ADD	W11, #4, W0
	MOV.B	W1, [W0]
; ByteCount end address is: 2 (W1)
;NMRAnetUtilities.mpas,164 :: 		Buffer^.DataBytes := DataBytes^;
	ADD	W11, #5, W1
	MOV	W2, W0
; DataBytes end address is: 4 (W2)
	REPEAT	#7
	MOV.B	[W0++], [W1++]
;NMRAnetUtilities.mpas,165 :: 		end;
L__NMRAnetUtilities_CreateNMRABusMessageCANBufferWithDestInMTI31:
;NMRAnetUtilities.mpas,166 :: 		Buffer^.State := Buffer^.State or BS_EXTENDED;
	ADD	W11, #13, W1
	ZE	[W1], W0
	IOR	W0, #1, W0
	MOV.B	W0, [W1]
;NMRAnetUtilities.mpas,167 :: 		Buffer^.ID := $10000000 or MTI or NodeBuffer^.Info.AliasID;
	MOV	#0, W0
	MOV	#4096, W1
	IOR	W0, W12, W2
	IOR	W1, W13, W3
	ADD	W10, #2, W0
	ADD	W0, #16, W0
	MOV	[W0], W0
	CLR	W1
	IOR	W2, W0, W0
	IOR	W3, W1, W1
	MOV.D	W0, [W11]
;NMRAnetUtilities.mpas,168 :: 		if DestinationAlias <> 0 then
	CP	W6, #0
	BRA NZ	L__NMRAnetUtilities_CreateNMRABusMessageCANBufferWithDestInMTI273
	GOTO	L__NMRAnetUtilities_CreateNMRABusMessageCANBufferWithDestInMTI34
L__NMRAnetUtilities_CreateNMRABusMessageCANBufferWithDestInMTI273:
;NMRAnetUtilities.mpas,169 :: 		Buffer^.ID := Buffer^.ID or DWORD(DestinationAlias shl 12);
	MOV	W6, W4
	CLR	W5
; DestinationAlias end address is: 12 (W6)
	MOV	#12, W0
	MOV.D	W4, W2
L__NMRAnetUtilities_CreateNMRABusMessageCANBufferWithDestInMTI274:
	DEC	W0, W0
	BRA LT	L__NMRAnetUtilities_CreateNMRABusMessageCANBufferWithDestInMTI275
	SL	W2, W2
	RLC	W3, W3
	BRA	L__NMRAnetUtilities_CreateNMRABusMessageCANBufferWithDestInMTI274
L__NMRAnetUtilities_CreateNMRABusMessageCANBufferWithDestInMTI275:
	MOV.D	[W11], W0
	IOR	W0, W2, [W11++]
	IOR	W1, W3, [W11--]
L__NMRAnetUtilities_CreateNMRABusMessageCANBufferWithDestInMTI34:
;NMRAnetUtilities.mpas,170 :: 		end;
L_end_NMRAnetUtilities_CreateNMRABusMessageCANBufferWithDestInMTI:
	ULNK
	RETURN
; end of _NMRAnetUtilities_CreateNMRABusMessageCANBufferWithDestInMTI

_NMRAnetUtilities_ExtractDestinationAlias:

;NMRAnetUtilities.mpas,182 :: 		begin
;NMRAnetUtilities.mpas,183 :: 		Result := 0;
; Result start address is: 8 (W4)
	CLR	W4
;NMRAnetUtilities.mpas,184 :: 		if NMRAnetUtilities_IsAddressedMessage(CANBuffer) then
	CALL	_NMRAnetUtilities_IsAddressedMessage
	CP0	W0
	BRA NZ	L__NMRAnetUtilities_ExtractDestinationAlias277
	GOTO	L__NMRAnetUtilities_ExtractDestinationAlias237
L__NMRAnetUtilities_ExtractDestinationAlias277:
; Result end address is: 8 (W4)
;NMRAnetUtilities.mpas,186 :: 		Result := CANBuffer^.DataBytes[0] shl 8;
	ADD	W10, #5, W1
	MOV.B	[W1], W0
	ZE	W0, W0
	SL	W0, #8, W0
; Result start address is: 4 (W2)
	MOV	W0, W2
;NMRAnetUtilities.mpas,187 :: 		Result := Result or CANBuffer^.DataBytes[1];
	ADD	W1, #1, W0
	ZE	[W0], W0
; Result start address is: 2 (W1)
	IOR	W2, W0, W1
; Result end address is: 4 (W2)
;NMRAnetUtilities.mpas,188 :: 		Result := Result and $0FFF;
	MOV	#4095, W0
	AND	W1, W0, W1
; Result end address is: 2 (W1)
;NMRAnetUtilities.mpas,189 :: 		end
	GOTO	L__NMRAnetUtilities_ExtractDestinationAlias38
L__NMRAnetUtilities_ExtractDestinationAlias237:
;NMRAnetUtilities.mpas,184 :: 		if NMRAnetUtilities_IsAddressedMessage(CANBuffer) then
	MOV	W4, W1
;NMRAnetUtilities.mpas,189 :: 		end
L__NMRAnetUtilities_ExtractDestinationAlias38:
;NMRAnetUtilities.mpas,190 :: 		end;
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetUtilities_ExtractDestinationAlias:
	RETURN
; end of _NMRAnetUtilities_ExtractDestinationAlias

_NMRAnetUtilities_ExtractDestinationAliasFlags:

;NMRAnetUtilities.mpas,201 :: 		begin
;NMRAnetUtilities.mpas,202 :: 		Result := 0;
; Result start address is: 8 (W4)
	CLR	W4
;NMRAnetUtilities.mpas,203 :: 		if NMRAnetUtilities_IsAddressedMessage(Buffer) then
	CALL	_NMRAnetUtilities_IsAddressedMessage
	CP0	W0
	BRA NZ	L__NMRAnetUtilities_ExtractDestinationAliasFlags279
	GOTO	L__NMRAnetUtilities_ExtractDestinationAliasFlags238
L__NMRAnetUtilities_ExtractDestinationAliasFlags279:
; Result end address is: 8 (W4)
;NMRAnetUtilities.mpas,205 :: 		Result := Buffer^.DataBytes[0] shl 8;
	ADD	W10, #5, W1
	MOV.B	[W1], W0
	ZE	W0, W0
	SL	W0, #8, W0
; Result start address is: 4 (W2)
	MOV	W0, W2
;NMRAnetUtilities.mpas,206 :: 		Result := Result or Buffer^.DataBytes[1];
	ADD	W1, #1, W0
	ZE	[W0], W0
; Result start address is: 2 (W1)
	IOR	W2, W0, W1
; Result end address is: 4 (W2)
;NMRAnetUtilities.mpas,207 :: 		Result := Result and $F000;
	MOV	#61440, W0
	AND	W1, W0, W1
; Result end address is: 2 (W1)
;NMRAnetUtilities.mpas,208 :: 		end
	GOTO	L__NMRAnetUtilities_ExtractDestinationAliasFlags42
L__NMRAnetUtilities_ExtractDestinationAliasFlags238:
;NMRAnetUtilities.mpas,203 :: 		if NMRAnetUtilities_IsAddressedMessage(Buffer) then
	MOV	W4, W1
;NMRAnetUtilities.mpas,208 :: 		end
L__NMRAnetUtilities_ExtractDestinationAliasFlags42:
;NMRAnetUtilities.mpas,209 :: 		end;
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetUtilities_ExtractDestinationAliasFlags:
	RETURN
; end of _NMRAnetUtilities_ExtractDestinationAliasFlags

_NMRAnetUtilities_ExtractDestinationCodedInMTIAlias:
	LNK	#2

;NMRAnetUtilities.mpas,222 :: 		begin
;NMRAnetUtilities.mpas,223 :: 		Result := 0;
	CLR	W0
	MOV	W0, [W14+0]
;NMRAnetUtilities.mpas,224 :: 		if NMRAnetUtilities_IsDatagramMsg(CANBuffer) then
	CALL	_NMRAnetUtilities_IsDatagramMsg
	CP0	W0
	BRA NZ	L__NMRAnetUtilities_ExtractDestinationCodedInMTIAlias281
	GOTO	L__NMRAnetUtilities_ExtractDestinationCodedInMTIAlias46
L__NMRAnetUtilities_ExtractDestinationCodedInMTIAlias281:
;NMRAnetUtilities.mpas,225 :: 		Result := (CANBuffer^.ID and $00FFF000) shr 12
	MOV.D	[W10], W2
	MOV	#61440, W0
	MOV	#255, W1
	AND	W2, W0, W4
	AND	W3, W1, W5
	MOV	#12, W2
	MOV.D	W4, W0
L__NMRAnetUtilities_ExtractDestinationCodedInMTIAlias282:
	DEC	W2, W2
	BRA LT	L__NMRAnetUtilities_ExtractDestinationCodedInMTIAlias283
	ASR	W1, W1
	RRC	W0, W0
	BRA	L__NMRAnetUtilities_ExtractDestinationCodedInMTIAlias282
L__NMRAnetUtilities_ExtractDestinationCodedInMTIAlias283:
	MOV	W0, [W14+0]
L__NMRAnetUtilities_ExtractDestinationCodedInMTIAlias46:
;NMRAnetUtilities.mpas,226 :: 		end;
	MOV	[W14+0], W0
L_end_NMRAnetUtilities_ExtractDestinationCodedInMTIAlias:
	ULNK
	RETURN
; end of _NMRAnetUtilities_ExtractDestinationCodedInMTIAlias

_NMRAnetUtilities_ExtractSourceAlias:

;NMRAnetUtilities.mpas,237 :: 		begin
;NMRAnetUtilities.mpas,238 :: 		Result := CANBuffer^.ID and MASK_SOURCE_ALIAS
	MOV	[W10], W1
	MOV	#4095, W0
; Result start address is: 2 (W1)
	AND	W1, W0, W1
;NMRAnetUtilities.mpas,239 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetUtilities_ExtractSourceAlias:
	RETURN
; end of _NMRAnetUtilities_ExtractSourceAlias

_NMRAnetUtilities_CANBufferBytesToNodeID:

;NMRAnetUtilities.mpas,251 :: 		begin
;NMRAnetUtilities.mpas,252 :: 		NodeID[1] := DataBytes^[iStartByte+2];
	ADD	W11, #4, W2
	ZE	W12, W0
	INC2	W0
	ADD	W10, W0, W0
	ZE	[W0], W0
	CLR	W1
	MOV.D	W0, [W2]
;NMRAnetUtilities.mpas,253 :: 		NodeID[1] := NodeID[1] or DataBytes^[iStartByte+1] shl 8;
	ADD	W11, #4, W6
	ADD	W11, #4, W5
	ZE	W12, W0
	INC	W0
	ADD	W10, W0, W0
	ZE	[W0], W3
	CLR	W4
	MOV	#8, W2
	MOV	W3, W0
	MOV	W4, W1
L__NMRAnetUtilities_CANBufferBytesToNodeID286:
	DEC	W2, W2
	BRA LT	L__NMRAnetUtilities_CANBufferBytesToNodeID287
	SL	W0, W0
	RLC	W1, W1
	BRA	L__NMRAnetUtilities_CANBufferBytesToNodeID286
L__NMRAnetUtilities_CANBufferBytesToNodeID287:
	IOR	W0, [W5++], [W6++]
	IOR	W1, [W5--], [W6--]
;NMRAnetUtilities.mpas,254 :: 		NodeID[1] := NodeID[1] or DataBytes^[iStartByte] shl 16;
	ADD	W11, #4, W3
	ADD	W11, #4, W2
	ZE	W12, W0
	ADD	W10, W0, W0
	ZE	[W0], W0
	CLR	W1
	MOV	W0, W1
	CLR	W0
	IOR	W0, [W2++], [W3++]
	IOR	W1, [W2--], [W3--]
;NMRAnetUtilities.mpas,255 :: 		NodeID[0] := DataBytes^[iStartByte+5];
	ZE	W12, W0
	ADD	W0, #5, W0
	ADD	W10, W0, W0
	ZE	[W0], W0
	CLR	W1
	MOV.D	W0, [W11]
;NMRAnetUtilities.mpas,256 :: 		NodeID[0] := NodeID[0] or DataBytes^[iStartByte+4] shl 8;
	ZE	W12, W0
	ADD	W0, #4, W0
	ADD	W10, W0, W0
	ZE	[W0], W4
	CLR	W5
	MOV	#8, W0
	MOV.D	W4, W2
L__NMRAnetUtilities_CANBufferBytesToNodeID288:
	DEC	W0, W0
	BRA LT	L__NMRAnetUtilities_CANBufferBytesToNodeID289
	SL	W2, W2
	RLC	W3, W3
	BRA	L__NMRAnetUtilities_CANBufferBytesToNodeID288
L__NMRAnetUtilities_CANBufferBytesToNodeID289:
	MOV.D	[W11], W0
	IOR	W0, W2, [W11++]
	IOR	W1, W3, [W11--]
;NMRAnetUtilities.mpas,257 :: 		NodeID[0] := NodeID[0] or DataBytes^[iStartByte+3] shl 16;
	ZE	W12, W0
	ADD	W0, #3, W0
	ADD	W10, W0, W0
	ZE	[W0], W0
	CLR	W1
	MOV	W0, W3
	CLR	W2
	MOV.D	[W11], W0
	IOR	W0, W2, [W11++]
	IOR	W1, W3, [W11--]
;NMRAnetUtilities.mpas,258 :: 		end;
L_end_NMRAnetUtilities_CANBufferBytesToNodeID:
	RETURN
; end of _NMRAnetUtilities_CANBufferBytesToNodeID

_NMRAnetUtilities_EqualNodeID:

;NMRAnetUtilities.mpas,269 :: 		begin
;NMRAnetUtilities.mpas,270 :: 		Result := (NodeID[1] = NodeBuffer^.Info.ID[1]) and (NodeID[0] = NodeBuffer^.Info.ID[0])
	ADD	W11, #4, W4
	ADD	W10, #2, W5
	ADD	W5, #4, W0
	MOV.D	[W0], W2
	MOV.D	[W4], W0
	CP	W0, W2
	CPB	W1, W3
	CLR	W4
	BRA NZ	L__NMRAnetUtilities_EqualNodeID291
	COM	W4
L__NMRAnetUtilities_EqualNodeID291:
	MOV.D	[W5], W2
	MOV.D	[W11], W0
	CP	W0, W2
	CPB	W1, W3
	CLR	W0
	BRA NZ	L__NMRAnetUtilities_EqualNodeID292
	COM	W0
L__NMRAnetUtilities_EqualNodeID292:
; Result start address is: 2 (W1)
	AND	W4, W0, W1
;NMRAnetUtilities.mpas,271 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetUtilities_EqualNodeID:
	RETURN
; end of _NMRAnetUtilities_EqualNodeID

_NMRAnetUtilities_CompareNodeIDs:

;NMRAnetUtilities.mpas,282 :: 		begin
;NMRAnetUtilities.mpas,283 :: 		Result := (NodeID1[1] = NodeID2[1]) and (NodeID1[0] = NodeID2[0])
	ADD	W10, #4, W4
	ADD	W11, #4, W0
	MOV.D	[W0], W2
	MOV.D	[W4], W0
	CP	W0, W2
	CPB	W1, W3
	CLR	W4
	BRA NZ	L__NMRAnetUtilities_CompareNodeIDs294
	COM	W4
L__NMRAnetUtilities_CompareNodeIDs294:
	MOV.D	[W11], W2
	MOV.D	[W10], W0
	CP	W0, W2
	CPB	W1, W3
	CLR	W0
	BRA NZ	L__NMRAnetUtilities_CompareNodeIDs295
	COM	W0
L__NMRAnetUtilities_CompareNodeIDs295:
; Result start address is: 2 (W1)
	AND	W4, W0, W1
;NMRAnetUtilities.mpas,284 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetUtilities_CompareNodeIDs:
	RETURN
; end of _NMRAnetUtilities_CompareNodeIDs

_NMRAnetUtilities_EqualDestinationAlias:

;NMRAnetUtilities.mpas,295 :: 		begin
;NMRAnetUtilities.mpas,296 :: 		Result := NMRAnetUtilities_EqualAliasID(NodeBuffer, NMRAnetUtilities_ExtractDestinationAlias(CANBuffer))
	PUSH	W11
	PUSH	W10
	MOV	W11, W10
	CALL	_NMRAnetUtilities_ExtractDestinationAlias
	POP	W10
	MOV	W0, W11
	CALL	_NMRAnetUtilities_EqualAliasID
; Result start address is: 2 (W1)
	MOV	W0, W1
;NMRAnetUtilities.mpas,297 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetUtilities_EqualDestinationAlias:
	POP	W11
	RETURN
; end of _NMRAnetUtilities_EqualDestinationAlias

_NMRAnetUtilities_EqualAliasID:

;NMRAnetUtilities.mpas,308 :: 		begin
;NMRAnetUtilities.mpas,309 :: 		Result := AliasID = NodeBuffer^.Info.AliasID
	ADD	W10, #2, W0
	ADD	W0, #16, W0
	MOV	[W0], W0
; Result start address is: 2 (W1)
	CP	W11, W0
	CLR	W1
	BRA NZ	L__NMRAnetUtilities_EqualAliasID298
	COM	W1
L__NMRAnetUtilities_EqualAliasID298:
;NMRAnetUtilities.mpas,310 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetUtilities_EqualAliasID:
	RETURN
; end of _NMRAnetUtilities_EqualAliasID

_NMRAnetUtilities_CompareAliasIDs:

;NMRAnetUtilities.mpas,321 :: 		begin
;NMRAnetUtilities.mpas,322 :: 		Result := AliasID1 = AliasID2
; Result start address is: 2 (W1)
	CP	W10, W11
	CLR	W1
	BRA NZ	L__NMRAnetUtilities_CompareAliasIDs300
	COM	W1
L__NMRAnetUtilities_CompareAliasIDs300:
;NMRAnetUtilities.mpas,323 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetUtilities_CompareAliasIDs:
	RETURN
; end of _NMRAnetUtilities_CompareAliasIDs

_NMRAnetUtilities_LoadNodeID:
	LNK	#0

;NMRAnetUtilities.mpas,334 :: 		begin
;NMRAnetUtilities.mpas,335 :: 		NodeID[0] := Lower;
; Lower start address is: 0 (W0)
	MOV	[W14-10], W0
	MOV	[W14-8], W1
	MOV.D	W0, [W10]
; Lower end address is: 0 (W0)
;NMRAnetUtilities.mpas,336 :: 		NodeID[1] := Upper;
	ADD	W10, #4, W0
	MOV	W11, [W0++]
	MOV	W12, [W0--]
;NMRAnetUtilities.mpas,337 :: 		end;
L_end_NMRAnetUtilities_LoadNodeID:
	ULNK
	RETURN
; end of _NMRAnetUtilities_LoadNodeID

_NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed:

;NMRAnetUtilities.mpas,351 :: 		begin
;NMRAnetUtilities.mpas,352 :: 		temp1 := ((Seed[1] shl 9) or ((Seed[0] shr 15) and $000001FF)) and $00FFFFFF;   // x(i+1)(2^9 + 1)*x(i) + C  = 2^9 * x(i) + x(i) + C
	ADD	W10, #4, W1
	MOV	#9, W0
	MOV.D	[W1], W6
L__NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed303:
	DEC	W0, W0
	BRA LT	L__NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed304
	SL	W6, W6
	RLC	W7, W7
	BRA	L__NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed303
L__NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed304:
	MOV.D	[W10], W4
	MOV	#15, W0
	MOV.D	W4, W2
L__NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed305:
	DEC	W0, W0
	BRA LT	L__NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed306
	LSR	W3, W3
	RRC	W2, W2
	BRA	L__NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed305
L__NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed306:
	MOV	#511, W0
	MOV	#0, W1
	AND	W2, W0, W0
	AND	W3, W1, W1
	IOR	W6, W0, W2
	IOR	W7, W1, W3
	MOV	#65535, W0
	MOV	#255, W1
; temp1 start address is: 12 (W6)
	AND	W2, W0, W6
	AND	W3, W1, W7
;NMRAnetUtilities.mpas,353 :: 		temp2 := (Seed[0] shl 9) and $00FFFFFF;                                                                  // Calculate 2^9 * x
	MOV.D	[W10], W4
	MOV	#9, W0
	MOV.D	W4, W2
L__NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed307:
	DEC	W0, W0
	BRA LT	L__NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed308
	SL	W2, W2
	RLC	W3, W3
	BRA	L__NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed307
L__NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed308:
	MOV	#65535, W0
	MOV	#255, W1
	AND	W2, W0, W2
	AND	W3, W1, W3
;NMRAnetUtilities.mpas,355 :: 		Seed[0] := Seed[0] + temp2 + $7A4BA9;   // Now y = 2^9 * x so all we have left is x(i+1) = y + x + c
	MOV.D	[W10], W0
	ADD	W0, W2, W2
	ADDC	W1, W3, W3
	MOV	#19369, W0
	MOV	#122, W1
	ADD	W2, W0, [W10++]
	ADDC	W3, W1, [W10--]
;NMRAnetUtilities.mpas,356 :: 		Seed[1] := Seed[1] + temp1 + $1B0CA3;
	ADD	W10, #4, W4
	ADD	W10, #4, W0
	ADD	W6, [W0++], W2
	ADDC	W7, [W0--], W3
; temp1 end address is: 12 (W6)
	MOV	#3235, W0
	MOV	#27, W1
	ADD	W2, W0, [W4++]
	ADDC	W3, W1, [W4--]
;NMRAnetUtilities.mpas,358 :: 		Seed[1] := (Seed[1] and $00FFFFFF) or (Seed[0] and $FF000000) shr 24;   // Handle the carries of the lower 24 bits into the upper
	ADD	W10, #4, W8
	ADD	W10, #4, W0
	MOV.D	[W0], W2
	MOV	#65535, W0
	MOV	#255, W1
	AND	W2, W0, W6
	AND	W3, W1, W7
	MOV.D	[W10], W2
	MOV	#0, W0
	MOV	#65280, W1
	AND	W2, W0, W4
	AND	W3, W1, W5
	MOV	#24, W2
	MOV.D	W4, W0
L__NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed309:
	DEC	W2, W2
	BRA LT	L__NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed310
	LSR	W1, W1
	RRC	W0, W0
	BRA	L__NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed309
L__NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed310:
	IOR	W6, W0, [W8++]
	IOR	W7, W1, [W8--]
;NMRAnetUtilities.mpas,359 :: 		Seed[0] := Seed[0] and $00FFFFFF;
	MOV.D	[W10], W2
	MOV	#65535, W0
	MOV	#255, W1
	AND	W2, W0, [W10++]
	AND	W3, W1, [W10--]
;NMRAnetUtilities.mpas,360 :: 		end;
L_end_NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed:
	RETURN
; end of _NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed

NMRAnetUtilities_NMRAnetUtilities_GenerateID_Alias_From_Seed:

;NMRAnetUtilities.mpas,371 :: 		begin
;NMRAnetUtilities.mpas,372 :: 		Result := (Seed[0] xor Seed[1] xor (Seed[0] shr 12) xor (Seed[1] shr 12)) and $00000FFF;
	ADD	W10, #4, W1
	MOV	[W10], W0
	XOR	W0, [W1], W5
	MOV	[W10++], W3
	MOV	[W10--], W4
	MOV	#12, W2
	MOV	W3, W0
	MOV	W4, W1
L_NMRAnetUtilities_NMRAnetUtilities_GenerateID_Alias_From_Seed312:
	DEC	W2, W2
	BRA LT	L_NMRAnetUtilities_NMRAnetUtilities_GenerateID_Alias_From_Seed313
	LSR	W1, W1
	RRC	W0, W0
	BRA	L_NMRAnetUtilities_NMRAnetUtilities_GenerateID_Alias_From_Seed312
L_NMRAnetUtilities_NMRAnetUtilities_GenerateID_Alias_From_Seed313:
	XOR	W5, W0, W4
	ADD	W10, #4, W3
	MOV	#12, W2
	MOV.D	[W3], W0
L_NMRAnetUtilities_NMRAnetUtilities_GenerateID_Alias_From_Seed314:
	DEC	W2, W2
	BRA LT	L_NMRAnetUtilities_NMRAnetUtilities_GenerateID_Alias_From_Seed315
	LSR	W1, W1
	RRC	W0, W0
	BRA	L_NMRAnetUtilities_NMRAnetUtilities_GenerateID_Alias_From_Seed314
L_NMRAnetUtilities_NMRAnetUtilities_GenerateID_Alias_From_Seed315:
	XOR	W4, W0, W2
	MOV	#4095, W0
; Result start address is: 2 (W1)
	AND	W2, W0, W1
;NMRAnetUtilities.mpas,373 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetUtilities_GenerateID_Alias_From_Seed:
	RETURN
; end of NMRAnetUtilities_NMRAnetUtilities_GenerateID_Alias_From_Seed

_NMRAnetUtilities_CreateAliasID:

;NMRAnetUtilities.mpas,384 :: 		begin
;NMRAnetUtilities.mpas,385 :: 		if Regenerate then
	CP0	W11
	BRA NZ	L__NMRAnetUtilities_CreateAliasID317
	GOTO	L__NMRAnetUtilities_CreateAliasID60
L__NMRAnetUtilities_CreateAliasID317:
;NMRAnetUtilities.mpas,386 :: 		NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed(Seed);
	CALL	_NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed
L__NMRAnetUtilities_CreateAliasID60:
;NMRAnetUtilities.mpas,387 :: 		Result := NMRAnetUtilities_GenerateID_Alias_From_Seed(Seed);
	CALL	NMRAnetUtilities_NMRAnetUtilities_GenerateID_Alias_From_Seed
; Result start address is: 2 (W1)
	MOV	W0, W1
;NMRAnetUtilities.mpas,388 :: 		if Result = 0 then
	CP	W1, #0
	BRA Z	L__NMRAnetUtilities_CreateAliasID318
	GOTO	L__NMRAnetUtilities_CreateAliasID239
L__NMRAnetUtilities_CreateAliasID318:
; Result end address is: 2 (W1)
;NMRAnetUtilities.mpas,390 :: 		NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed(Seed);
	CALL	_NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed
;NMRAnetUtilities.mpas,391 :: 		Result := NMRAnetUtilities_GenerateID_Alias_From_Seed(Seed);
	CALL	NMRAnetUtilities_NMRAnetUtilities_GenerateID_Alias_From_Seed
; Result start address is: 2 (W1)
	MOV	W0, W1
; Result end address is: 2 (W1)
;NMRAnetUtilities.mpas,392 :: 		end
	GOTO	L__NMRAnetUtilities_CreateAliasID63
L__NMRAnetUtilities_CreateAliasID239:
;NMRAnetUtilities.mpas,388 :: 		if Result = 0 then
;NMRAnetUtilities.mpas,392 :: 		end
L__NMRAnetUtilities_CreateAliasID63:
;NMRAnetUtilities.mpas,393 :: 		end;
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetUtilities_CreateAliasID:
	RETURN
; end of _NMRAnetUtilities_CreateAliasID

_NMRAnetUtilities_RecreateAliasID:

;NMRAnetUtilities.mpas,404 :: 		begin
;NMRAnetUtilities.mpas,405 :: 		Result := NMRAnetUtilities_CreateAliasID(Seed, True)
	PUSH	W11
	MOV	#65535, W11
	CALL	_NMRAnetUtilities_CreateAliasID
; Result start address is: 2 (W1)
	MOV	W0, W1
;NMRAnetUtilities.mpas,406 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetUtilities_RecreateAliasID:
	POP	W11
	RETURN
; end of _NMRAnetUtilities_RecreateAliasID

_NMRAnetUtilities_IsAddressedMessage:

;NMRAnetUtilities.mpas,417 :: 		begin
;NMRAnetUtilities.mpas,418 :: 		Result := CANBuffer^.ID and MTI_ADDRESSED_MASK <> 0
	MOV.D	[W10], W2
	MOV	#32768, W0
	MOV	#0, W1
	AND	W2, W0, W0
	AND	W3, W1, W1
; Result start address is: 2 (W1)
	CP	W0, #0
	CPB	W1, #0
	CLR	W1
	BRA Z	L__NMRAnetUtilities_IsAddressedMessage321
	COM	W1
L__NMRAnetUtilities_IsAddressedMessage321:
;NMRAnetUtilities.mpas,419 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetUtilities_IsAddressedMessage:
	RETURN
; end of _NMRAnetUtilities_IsAddressedMessage

_NMRAnetUtilities_IsAddressedMessageToNode:

;NMRAnetUtilities.mpas,430 :: 		begin
;NMRAnetUtilities.mpas,431 :: 		if NMRAnetUtilities_IsAddressedMessage(CANBuffer) then
	PUSH	W11
	PUSH	W10
	MOV	W11, W10
	CALL	_NMRAnetUtilities_IsAddressedMessage
	POP	W10
	CP0	W0
	BRA NZ	L__NMRAnetUtilities_IsAddressedMessageToNode323
	GOTO	L__NMRAnetUtilities_IsAddressedMessageToNode69
L__NMRAnetUtilities_IsAddressedMessageToNode323:
;NMRAnetUtilities.mpas,432 :: 		Result := NMRAnetUtilities_EqualAliasID(NodeBuffer, NMRAnetUtilities_ExtractDestinationAlias(CANBuffer))
	PUSH	W10
	MOV	W11, W10
	CALL	_NMRAnetUtilities_ExtractDestinationAlias
	POP	W10
	MOV	W0, W11
	CALL	_NMRAnetUtilities_EqualAliasID
; Result start address is: 2 (W1)
	MOV	W0, W1
; Result end address is: 2 (W1)
	GOTO	L__NMRAnetUtilities_IsAddressedMessageToNode70
;NMRAnetUtilities.mpas,433 :: 		else
L__NMRAnetUtilities_IsAddressedMessageToNode69:
;NMRAnetUtilities.mpas,434 :: 		Result := False
; Result start address is: 2 (W1)
	CLR	W1
; Result end address is: 2 (W1)
L__NMRAnetUtilities_IsAddressedMessageToNode70:
;NMRAnetUtilities.mpas,435 :: 		end;
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetUtilities_IsAddressedMessageToNode:
	POP	W11
	RETURN
; end of _NMRAnetUtilities_IsAddressedMessageToNode

_NMRAnetUtilities_IsDatagramMsg:

;NMRAnetUtilities.mpas,446 :: 		begin
;NMRAnetUtilities.mpas,447 :: 		Result := ((CANBuffer^.ID and MTI_FRAME_TYPE_MASK) >= MTI_FRAME_TYPE_DATAGRAM_ONLY_FRAME) and ((CANBuffer^.ID and MTI_FRAME_TYPE_MASK) <= MTI_FRAME_TYPE_DATAGRAM_FRAME_END)
	MOV.D	[W10], W2
	MOV	#0, W0
	MOV	#3840, W1
	AND	W2, W0, W4
	AND	W3, W1, W5
	MOV	#0, W0
	MOV	#2560, W1
	CP	W4, W0
	CPB	W5, W1
	CLR	W2
	BRA LT	L__NMRAnetUtilities_IsDatagramMsg325
	COM	W2
L__NMRAnetUtilities_IsDatagramMsg325:
	MOV	#0, W0
	MOV	#3328, W1
	CP	W4, W0
	CPB	W5, W1
	CLR	W0
	BRA GT	L__NMRAnetUtilities_IsDatagramMsg326
	COM	W0
L__NMRAnetUtilities_IsDatagramMsg326:
; Result start address is: 2 (W1)
	AND	W2, W0, W1
;NMRAnetUtilities.mpas,448 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetUtilities_IsDatagramMsg:
	RETURN
; end of _NMRAnetUtilities_IsDatagramMsg

NMRAnetUtilities_NMRAnetUtilities_IsAddressedDatagramToNode:
	LNK	#2

;NMRAnetUtilities.mpas,459 :: 		begin
;NMRAnetUtilities.mpas,460 :: 		Result := NMRAnetUtilities_IsAddressedMessageToNode(NodeBuffer, CANBuffer) and NMRAnetUtilities_IsDatagramMsg(CANBuffer)
	PUSH	W10
	CALL	_NMRAnetUtilities_IsAddressedMessageToNode
	MOV	W0, [W14+0]
	MOV	W11, W10
	CALL	_NMRAnetUtilities_IsDatagramMsg
	MOV	[W14+0], W1
; Result start address is: 2 (W1)
	AND	W1, W0, W1
;NMRAnetUtilities.mpas,461 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetUtilities_IsAddressedDatagramToNode:
	POP	W10
	ULNK
	RETURN
; end of NMRAnetUtilities_NMRAnetUtilities_IsAddressedDatagramToNode

_NMRAnetUtilities_LoadCANData:
	LNK	#0

;NMRAnetUtilities.mpas,472 :: 		begin
;NMRAnetUtilities.mpas,473 :: 		DataBytes[0] := Byte0;
; Byte3 start address is: 2 (W1)
	MOV.B	[W14-8], W1
; Byte4 start address is: 4 (W2)
	MOV.B	[W14-10], W2
; Byte5 start address is: 6 (W3)
	MOV.B	[W14-12], W3
; Byte6 start address is: 8 (W4)
	MOV.B	[W14-14], W4
; Byte7 start address is: 10 (W5)
	MOV.B	[W14-16], W5
	MOV.B	W11, [W10]
;NMRAnetUtilities.mpas,474 :: 		DataBytes[1] := Byte1;
	ADD	W10, #1, W0
	MOV.B	W12, [W0]
;NMRAnetUtilities.mpas,475 :: 		DataBytes[2] := Byte2;
	ADD	W10, #2, W0
	MOV.B	W13, [W0]
;NMRAnetUtilities.mpas,476 :: 		DataBytes[3] := Byte3;
	ADD	W10, #3, W0
	MOV.B	W1, [W0]
; Byte3 end address is: 2 (W1)
;NMRAnetUtilities.mpas,477 :: 		DataBytes[4] := Byte4;
	ADD	W10, #4, W0
	MOV.B	W2, [W0]
; Byte4 end address is: 4 (W2)
;NMRAnetUtilities.mpas,478 :: 		DataBytes[5] := Byte5;
	ADD	W10, #5, W0
	MOV.B	W3, [W0]
; Byte5 end address is: 6 (W3)
;NMRAnetUtilities.mpas,479 :: 		DataBytes[6] := Byte6;
	ADD	W10, #6, W0
	MOV.B	W4, [W0]
; Byte6 end address is: 8 (W4)
;NMRAnetUtilities.mpas,480 :: 		DataBytes[7] := Byte7;
	ADD	W10, #7, W0
	MOV.B	W5, [W0]
; Byte7 end address is: 10 (W5)
;NMRAnetUtilities.mpas,481 :: 		end;
L_end_NMRAnetUtilities_LoadCANData:
	ULNK
	RETURN
; end of _NMRAnetUtilities_LoadCANData

_NMRAnetUtilities_LoadDestinationAlias:

;NMRAnetUtilities.mpas,492 :: 		begin
;NMRAnetUtilities.mpas,493 :: 		DataBytes^[0] := (DestinationAlias shr 8) and $000F;
	LSR	W10, #8, W0
	AND	W0, #15, W0
	MOV.B	W0, [W11]
;NMRAnetUtilities.mpas,494 :: 		DataBytes^[1] := DestinationAlias and $00FF;
	ADD	W11, #1, W1
	MOV	#255, W0
	AND	W10, W0, W0
	MOV.B	W0, [W1]
;NMRAnetUtilities.mpas,495 :: 		end;
L_end_NMRAnetUtilities_LoadDestinationAlias:
	RETURN
; end of _NMRAnetUtilities_LoadDestinationAlias

_NMRAnetUtilities_ZeroCANData:

;NMRAnetUtilities.mpas,508 :: 		begin
;NMRAnetUtilities.mpas,509 :: 		for i := 0 to CAN_DATA_LEN - 1 do
; i start address is: 4 (W2)
	CLR	W2
; i end address is: 4 (W2)
L__NMRAnetUtilities_ZeroCANData77:
;NMRAnetUtilities.mpas,510 :: 		DataBytes[i] := 0;
; i start address is: 4 (W2)
	ADD	W10, W2, W1
	CLR	W0
	MOV.B	W0, [W1]
	CP	W2, #7
	BRA NZ	L__NMRAnetUtilities_ZeroCANData331
	GOTO	L__NMRAnetUtilities_ZeroCANData80
L__NMRAnetUtilities_ZeroCANData331:
; i start address is: 4 (W2)
	INC	W2
; i end address is: 4 (W2)
; i end address is: 4 (W2)
	GOTO	L__NMRAnetUtilities_ZeroCANData77
L__NMRAnetUtilities_ZeroCANData80:
;NMRAnetUtilities.mpas,511 :: 		end;
L_end_NMRAnetUtilities_ZeroCANData:
	RETURN
; end of _NMRAnetUtilities_ZeroCANData

_NMRAnetUtilities_PackBytesLo:

;NMRAnetUtilities.mpas,522 :: 		begin
;NMRAnetUtilities.mpas,523 :: 		Result := CANBuffer^.DataBytes[3];
	ADD	W10, #5, W1
	ADD	W1, #3, W0
; Result start address is: 4 (W2)
	ZE	[W0], W2
	CLR	W3
;NMRAnetUtilities.mpas,524 :: 		Result := Result shr 8;
	MOV	#8, W0
; Result start address is: 0 (W0)
	MOV.D	W2, W0
L__NMRAnetUtilities_PackBytesLo333:
	DEC	W0, W0
	BRA LT	L__NMRAnetUtilities_PackBytesLo334
	LSR	W1, W1
	RRC	W0, W0
	BRA	L__NMRAnetUtilities_PackBytesLo333
L__NMRAnetUtilities_PackBytesLo334:
; Result end address is: 0 (W0)
; Result end address is: 4 (W2)
;NMRAnetUtilities.mpas,525 :: 		Result := CANBuffer^.DataBytes[2];
	ADD	W1, #2, W0
; Result start address is: 4 (W2)
	ZE	[W0], W2
	CLR	W3
;NMRAnetUtilities.mpas,526 :: 		Result := Result shr 8;
	MOV	#8, W0
; Result start address is: 0 (W0)
	MOV.D	W2, W0
L__NMRAnetUtilities_PackBytesLo335:
	DEC	W0, W0
	BRA LT	L__NMRAnetUtilities_PackBytesLo336
	LSR	W1, W1
	RRC	W0, W0
	BRA	L__NMRAnetUtilities_PackBytesLo335
L__NMRAnetUtilities_PackBytesLo336:
; Result end address is: 0 (W0)
; Result end address is: 4 (W2)
;NMRAnetUtilities.mpas,527 :: 		Result := CANBuffer^.DataBytes[1];
	ADD	W1, #1, W0
; Result start address is: 4 (W2)
	ZE	[W0], W2
	CLR	W3
;NMRAnetUtilities.mpas,528 :: 		Result := Result shr 8;
	MOV	#8, W0
; Result start address is: 0 (W0)
	MOV.D	W2, W0
L__NMRAnetUtilities_PackBytesLo337:
	DEC	W0, W0
	BRA LT	L__NMRAnetUtilities_PackBytesLo338
	LSR	W1, W1
	RRC	W0, W0
	BRA	L__NMRAnetUtilities_PackBytesLo337
L__NMRAnetUtilities_PackBytesLo338:
; Result end address is: 0 (W0)
; Result end address is: 4 (W2)
;NMRAnetUtilities.mpas,529 :: 		Result := CANBuffer^.DataBytes[0];
	MOV.B	[W1], W0
; Result start address is: 2 (W1)
	ZE	W0, W1
	CLR	W2
;NMRAnetUtilities.mpas,530 :: 		Result := Result shr 8;
	MOV	#8, W0
; Result start address is: 4 (W2)
	MOV	W2, W3
	MOV	W1, W2
; Result end address is: 2 (W1)
L__NMRAnetUtilities_PackBytesLo339:
	DEC	W0, W0
	BRA LT	L__NMRAnetUtilities_PackBytesLo340
	LSR	W3, W3
	RRC	W2, W2
	BRA	L__NMRAnetUtilities_PackBytesLo339
L__NMRAnetUtilities_PackBytesLo340:
;NMRAnetUtilities.mpas,531 :: 		end;
	MOV.D	W2, W0
; Result end address is: 4 (W2)
L_end_NMRAnetUtilities_PackBytesLo:
	RETURN
; end of _NMRAnetUtilities_PackBytesLo

_NMRAnetUtilities_PackBytesHi:

;NMRAnetUtilities.mpas,542 :: 		begin
;NMRAnetUtilities.mpas,543 :: 		Result := CANBuffer^.DataBytes[7];
	ADD	W10, #5, W1
	ADD	W1, #7, W0
; Result start address is: 4 (W2)
	ZE	[W0], W2
	CLR	W3
;NMRAnetUtilities.mpas,544 :: 		Result := Result shr 8;
	MOV	#8, W0
; Result start address is: 0 (W0)
	MOV.D	W2, W0
L__NMRAnetUtilities_PackBytesHi342:
	DEC	W0, W0
	BRA LT	L__NMRAnetUtilities_PackBytesHi343
	LSR	W1, W1
	RRC	W0, W0
	BRA	L__NMRAnetUtilities_PackBytesHi342
L__NMRAnetUtilities_PackBytesHi343:
; Result end address is: 0 (W0)
; Result end address is: 4 (W2)
;NMRAnetUtilities.mpas,545 :: 		Result := CANBuffer^.DataBytes[6];
	ADD	W1, #6, W0
; Result start address is: 4 (W2)
	ZE	[W0], W2
	CLR	W3
;NMRAnetUtilities.mpas,546 :: 		Result := Result shr 8;
	MOV	#8, W0
; Result start address is: 0 (W0)
	MOV.D	W2, W0
L__NMRAnetUtilities_PackBytesHi344:
	DEC	W0, W0
	BRA LT	L__NMRAnetUtilities_PackBytesHi345
	LSR	W1, W1
	RRC	W0, W0
	BRA	L__NMRAnetUtilities_PackBytesHi344
L__NMRAnetUtilities_PackBytesHi345:
; Result end address is: 0 (W0)
; Result end address is: 4 (W2)
;NMRAnetUtilities.mpas,547 :: 		Result := CANBuffer^.DataBytes[5];
	ADD	W1, #5, W0
; Result start address is: 4 (W2)
	ZE	[W0], W2
	CLR	W3
;NMRAnetUtilities.mpas,548 :: 		Result := Result shr 8;
	MOV	#8, W0
; Result start address is: 0 (W0)
	MOV.D	W2, W0
L__NMRAnetUtilities_PackBytesHi346:
	DEC	W0, W0
	BRA LT	L__NMRAnetUtilities_PackBytesHi347
	LSR	W1, W1
	RRC	W0, W0
	BRA	L__NMRAnetUtilities_PackBytesHi346
L__NMRAnetUtilities_PackBytesHi347:
; Result end address is: 0 (W0)
; Result end address is: 4 (W2)
;NMRAnetUtilities.mpas,549 :: 		Result := CANBuffer^.DataBytes[4];
	ADD	W1, #4, W0
; Result start address is: 2 (W1)
	ZE	[W0], W1
	CLR	W2
;NMRAnetUtilities.mpas,550 :: 		Result := Result shr 8;
	MOV	#8, W0
; Result start address is: 4 (W2)
	MOV	W2, W3
	MOV	W1, W2
; Result end address is: 2 (W1)
L__NMRAnetUtilities_PackBytesHi348:
	DEC	W0, W0
	BRA LT	L__NMRAnetUtilities_PackBytesHi349
	LSR	W3, W3
	RRC	W2, W2
	BRA	L__NMRAnetUtilities_PackBytesHi348
L__NMRAnetUtilities_PackBytesHi349:
;NMRAnetUtilities.mpas,551 :: 		end;
	MOV.D	W2, W0
; Result end address is: 4 (W2)
L_end_NMRAnetUtilities_PackBytesHi:
	RETURN
; end of _NMRAnetUtilities_PackBytesHi

_NMRAnetUtilities_EqualEventID:

;NMRAnetUtilities.mpas,564 :: 		begin
;NMRAnetUtilities.mpas,565 :: 		Result := True;
; Result start address is: 4 (W2)
	MOV	#65535, W2
;NMRAnetUtilities.mpas,566 :: 		i := 0;
; i start address is: 6 (W3)
	CLR	W3
; Result end address is: 4 (W2)
; i end address is: 6 (W3)
;NMRAnetUtilities.mpas,567 :: 		while (i < 8) and Result do
L__NMRAnetUtilities_EqualEventID85:
; i start address is: 6 (W3)
; Result start address is: 4 (W2)
	CP	W3, #8
	CLR	W0
	BRA GE	L__NMRAnetUtilities_EqualEventID351
	COM	W0
L__NMRAnetUtilities_EqualEventID351:
	AND	W0, W2, W0
	BRA NZ	L__NMRAnetUtilities_EqualEventID352
	GOTO	L__NMRAnetUtilities_EqualEventID240
L__NMRAnetUtilities_EqualEventID352:
;NMRAnetUtilities.mpas,569 :: 		if Event1^[i] <> Event2^[i] then
	ADD	W10, W3, W0
	ADD	W11, W3, W1
	MOV.B	[W0], W0
	CP.B	W0, [W1]
	BRA NZ	L__NMRAnetUtilities_EqualEventID353
	GOTO	L__NMRAnetUtilities_EqualEventID90
L__NMRAnetUtilities_EqualEventID353:
; Result end address is: 4 (W2)
; i end address is: 6 (W3)
;NMRAnetUtilities.mpas,571 :: 		Result := False;
; Result start address is: 2 (W1)
	CLR	W1
;NMRAnetUtilities.mpas,572 :: 		Break
; Result end address is: 2 (W1)
	GOTO	L__NMRAnetUtilities_EqualEventID86
;NMRAnetUtilities.mpas,573 :: 		end;
L__NMRAnetUtilities_EqualEventID90:
;NMRAnetUtilities.mpas,574 :: 		Inc(i);
; i start address is: 6 (W3)
; Result start address is: 4 (W2)
	INC	W3
;NMRAnetUtilities.mpas,575 :: 		end;
; Result end address is: 4 (W2)
; i end address is: 6 (W3)
	GOTO	L__NMRAnetUtilities_EqualEventID85
L__NMRAnetUtilities_EqualEventID240:
;NMRAnetUtilities.mpas,567 :: 		while (i < 8) and Result do
	MOV	W2, W1
;NMRAnetUtilities.mpas,575 :: 		end;
L__NMRAnetUtilities_EqualEventID86:
;NMRAnetUtilities.mpas,576 :: 		end;
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetUtilities_EqualEventID:
	RETURN
; end of _NMRAnetUtilities_EqualEventID

_NMRAnetUtilities_SupportsEventAsProducer:

;NMRAnetUtilities.mpas,588 :: 		begin
;NMRAnetUtilities.mpas,589 :: 		Result := False;
; Result start address is: 8 (W4)
	CLR	W4
;NMRAnetUtilities.mpas,591 :: 		EventIndex := 0;
	CLR	W0
	MOV	W0, [W11]
; Result end address is: 8 (W4)
;NMRAnetUtilities.mpas,592 :: 		while (EventIndex < MAX_SUPPORTED_EVENTS_PRODUCED) do
L__NMRAnetUtilities_SupportsEventAsProducer94:
; Result start address is: 8 (W4)
	MOV	[W11], W0
	CP	W0, #2
	BRA LT	L__NMRAnetUtilities_SupportsEventAsProducer355
	GOTO	L__NMRAnetUtilities_SupportsEventAsProducer95
L__NMRAnetUtilities_SupportsEventAsProducer355:
;NMRAnetUtilities.mpas,594 :: 		if NMRAnetUtilities_EqualEventID(@SUPPORTED_EVENTS_PRODUCED[EventIndex], DataBytes) then
	MOV	[W11], W0
	SL	W0, #3, W1
	MOV	#lo_addr(_SUPPORTED_EVENTS_PRODUCED), W0
	ADD	W0, W1, W0
	PUSH.D	W10
	MOV	W10, W11
	MOV	W0, W10
	CALL	_NMRAnetUtilities_EqualEventID
	POP.D	W10
	CP0	W0
	BRA NZ	L__NMRAnetUtilities_SupportsEventAsProducer356
	GOTO	L__NMRAnetUtilities_SupportsEventAsProducer99
L__NMRAnetUtilities_SupportsEventAsProducer356:
; Result end address is: 8 (W4)
;NMRAnetUtilities.mpas,596 :: 		Result := True;
; Result start address is: 2 (W1)
	MOV	#65535, W1
;NMRAnetUtilities.mpas,597 :: 		Exit
; Result end address is: 2 (W1)
	GOTO	L_end__NMRAnetUtilities_SupportsEventAsProducer
;NMRAnetUtilities.mpas,598 :: 		end;
L__NMRAnetUtilities_SupportsEventAsProducer99:
;NMRAnetUtilities.mpas,599 :: 		Inc(EventIndex)
; Result start address is: 8 (W4)
	MOV	[W11], W0
	INC	W0
	MOV	W0, [W11]
;NMRAnetUtilities.mpas,600 :: 		end;
	GOTO	L__NMRAnetUtilities_SupportsEventAsProducer94
L__NMRAnetUtilities_SupportsEventAsProducer95:
;NMRAnetUtilities.mpas,602 :: 		end;
	MOV	W4, W1
L_end__NMRAnetUtilities_SupportsEventAsProducer:
; Result end address is: 8 (W4)
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetUtilities_SupportsEventAsProducer:
	RETURN
; end of _NMRAnetUtilities_SupportsEventAsProducer

_NMRAnetUtilities_SupportsEventAsConsumer:

;NMRAnetUtilities.mpas,613 :: 		begin
;NMRAnetUtilities.mpas,614 :: 		Result := False;
; Result start address is: 8 (W4)
	CLR	W4
;NMRAnetUtilities.mpas,616 :: 		EventIndex := 0;
	CLR	W0
	MOV	W0, [W11]
; Result end address is: 8 (W4)
;NMRAnetUtilities.mpas,617 :: 		while (EventIndex < MAX_SUPPORTED_EVENTS_CONSUMED) do
L__NMRAnetUtilities_SupportsEventAsConsumer103:
; Result start address is: 8 (W4)
	MOV	[W11], W0
	CP	W0, #1
	BRA LT	L__NMRAnetUtilities_SupportsEventAsConsumer358
	GOTO	L__NMRAnetUtilities_SupportsEventAsConsumer104
L__NMRAnetUtilities_SupportsEventAsConsumer358:
;NMRAnetUtilities.mpas,619 :: 		if NMRAnetUtilities_EqualEventID(@SUPPORTED_EVENTS_CONSUMED[EventIndex], DataBytes) then
	MOV	[W11], W0
	SL	W0, #3, W1
	MOV	#lo_addr(_SUPPORTED_EVENTS_CONSUMED), W0
	ADD	W0, W1, W0
	PUSH.D	W10
	MOV	W10, W11
	MOV	W0, W10
	CALL	_NMRAnetUtilities_EqualEventID
	POP.D	W10
	CP0	W0
	BRA NZ	L__NMRAnetUtilities_SupportsEventAsConsumer359
	GOTO	L__NMRAnetUtilities_SupportsEventAsConsumer108
L__NMRAnetUtilities_SupportsEventAsConsumer359:
; Result end address is: 8 (W4)
;NMRAnetUtilities.mpas,621 :: 		Result := True;
; Result start address is: 2 (W1)
	MOV	#65535, W1
;NMRAnetUtilities.mpas,622 :: 		Exit
; Result end address is: 2 (W1)
	GOTO	L_end__NMRAnetUtilities_SupportsEventAsConsumer
;NMRAnetUtilities.mpas,623 :: 		end;
L__NMRAnetUtilities_SupportsEventAsConsumer108:
;NMRAnetUtilities.mpas,624 :: 		Inc(EventIndex)
; Result start address is: 8 (W4)
	MOV	[W11], W0
	INC	W0
	MOV	W0, [W11]
;NMRAnetUtilities.mpas,625 :: 		end;
	GOTO	L__NMRAnetUtilities_SupportsEventAsConsumer103
L__NMRAnetUtilities_SupportsEventAsConsumer104:
;NMRAnetUtilities.mpas,627 :: 		end;
	MOV	W4, W1
L_end__NMRAnetUtilities_SupportsEventAsConsumer:
; Result end address is: 8 (W4)
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetUtilities_SupportsEventAsConsumer:
	RETURN
; end of _NMRAnetUtilities_SupportsEventAsConsumer

_NMRAnetUtilities_SupportsVNodeEventAsProducer:

;NMRAnetUtilities.mpas,638 :: 		begin
;NMRAnetUtilities.mpas,639 :: 		Result := False;
; Result start address is: 8 (W4)
	CLR	W4
;NMRAnetUtilities.mpas,641 :: 		EventIndex := 0;
	CLR	W0
	MOV	W0, [W11]
; Result end address is: 8 (W4)
;NMRAnetUtilities.mpas,642 :: 		while (EventIndex < MAX_VNODE_SUPPORTED_EVENTS_PRODUCED) do
L__NMRAnetUtilities_SupportsVNodeEventAsProducer112:
; Result start address is: 8 (W4)
	MOV	[W11], W0
	CP	W0, #3
	BRA LT	L__NMRAnetUtilities_SupportsVNodeEventAsProducer361
	GOTO	L__NMRAnetUtilities_SupportsVNodeEventAsProducer113
L__NMRAnetUtilities_SupportsVNodeEventAsProducer361:
;NMRAnetUtilities.mpas,644 :: 		if NMRAnetUtilities_EqualEventID(@SUPPORTED_VNODE_EVENTS_PRODUCED[EventIndex], DataBytes) then
	MOV	[W11], W0
	SL	W0, #3, W1
	MOV	#lo_addr(_SUPPORTED_VNODE_EVENTS_PRODUCED), W0
	ADD	W0, W1, W0
	PUSH.D	W10
	MOV	W10, W11
	MOV	W0, W10
	CALL	_NMRAnetUtilities_EqualEventID
	POP.D	W10
	CP0	W0
	BRA NZ	L__NMRAnetUtilities_SupportsVNodeEventAsProducer362
	GOTO	L__NMRAnetUtilities_SupportsVNodeEventAsProducer117
L__NMRAnetUtilities_SupportsVNodeEventAsProducer362:
; Result end address is: 8 (W4)
;NMRAnetUtilities.mpas,646 :: 		Result := True;
; Result start address is: 2 (W1)
	MOV	#65535, W1
;NMRAnetUtilities.mpas,647 :: 		Exit
; Result end address is: 2 (W1)
	GOTO	L_end__NMRAnetUtilities_SupportsVNodeEventAsProducer
;NMRAnetUtilities.mpas,648 :: 		end;
L__NMRAnetUtilities_SupportsVNodeEventAsProducer117:
;NMRAnetUtilities.mpas,649 :: 		Inc(EventIndex)
; Result start address is: 8 (W4)
	MOV	[W11], W0
	INC	W0
	MOV	W0, [W11]
;NMRAnetUtilities.mpas,650 :: 		end;
	GOTO	L__NMRAnetUtilities_SupportsVNodeEventAsProducer112
L__NMRAnetUtilities_SupportsVNodeEventAsProducer113:
;NMRAnetUtilities.mpas,652 :: 		end;
	MOV	W4, W1
L_end__NMRAnetUtilities_SupportsVNodeEventAsProducer:
; Result end address is: 8 (W4)
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetUtilities_SupportsVNodeEventAsProducer:
	RETURN
; end of _NMRAnetUtilities_SupportsVNodeEventAsProducer

_NMRAnetUtilities_SupportsVNodeEventAsConsumer:

;NMRAnetUtilities.mpas,663 :: 		begin
;NMRAnetUtilities.mpas,664 :: 		Result := False;
; Result start address is: 8 (W4)
	CLR	W4
;NMRAnetUtilities.mpas,666 :: 		EventIndex := 0;
	CLR	W0
	MOV	W0, [W11]
; Result end address is: 8 (W4)
;NMRAnetUtilities.mpas,667 :: 		while (EventIndex < MAX_VNODE_SUPPORTED_EVENTS_CONSUMED) do
L__NMRAnetUtilities_SupportsVNodeEventAsConsumer121:
; Result start address is: 8 (W4)
	MOV	[W11], W0
	CP	W0, #1
	BRA LT	L__NMRAnetUtilities_SupportsVNodeEventAsConsumer364
	GOTO	L__NMRAnetUtilities_SupportsVNodeEventAsConsumer122
L__NMRAnetUtilities_SupportsVNodeEventAsConsumer364:
;NMRAnetUtilities.mpas,669 :: 		if NMRAnetUtilities_EqualEventID(@SUPPORTED_VNODE_EVENTS_CONSUMED[EventIndex], DataBytes) then
	MOV	[W11], W0
	SL	W0, #3, W1
	MOV	#lo_addr(_SUPPORTED_VNODE_EVENTS_CONSUMED), W0
	ADD	W0, W1, W0
	PUSH.D	W10
	MOV	W10, W11
	MOV	W0, W10
	CALL	_NMRAnetUtilities_EqualEventID
	POP.D	W10
	CP0	W0
	BRA NZ	L__NMRAnetUtilities_SupportsVNodeEventAsConsumer365
	GOTO	L__NMRAnetUtilities_SupportsVNodeEventAsConsumer126
L__NMRAnetUtilities_SupportsVNodeEventAsConsumer365:
; Result end address is: 8 (W4)
;NMRAnetUtilities.mpas,671 :: 		Result := True;
; Result start address is: 2 (W1)
	MOV	#65535, W1
;NMRAnetUtilities.mpas,672 :: 		Exit
; Result end address is: 2 (W1)
	GOTO	L_end__NMRAnetUtilities_SupportsVNodeEventAsConsumer
;NMRAnetUtilities.mpas,673 :: 		end;
L__NMRAnetUtilities_SupportsVNodeEventAsConsumer126:
;NMRAnetUtilities.mpas,674 :: 		Inc(EventIndex)
; Result start address is: 8 (W4)
	MOV	[W11], W0
	INC	W0
	MOV	W0, [W11]
;NMRAnetUtilities.mpas,675 :: 		end;
	GOTO	L__NMRAnetUtilities_SupportsVNodeEventAsConsumer121
L__NMRAnetUtilities_SupportsVNodeEventAsConsumer122:
;NMRAnetUtilities.mpas,677 :: 		end;
	MOV	W4, W1
L_end__NMRAnetUtilities_SupportsVNodeEventAsConsumer:
; Result end address is: 8 (W4)
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetUtilities_SupportsVNodeEventAsConsumer:
	RETURN
; end of _NMRAnetUtilities_SupportsVNodeEventAsConsumer

_NMRANetUtilities_LoadDatagramResultBytes:

;NMRAnetUtilities.mpas,689 :: 		begin
;NMRAnetUtilities.mpas,690 :: 		if CodeType <> nil then
	CP	W11, #0
	BRA NZ	L__NMRANetUtilities_LoadDatagramResultBytes367
	GOTO	L__NMRANetUtilities_LoadDatagramResultBytes130
L__NMRANetUtilities_LoadDatagramResultBytes367:
;NMRAnetUtilities.mpas,692 :: 		Datagram^.iByteCount := 2;
	ADD	W10, #10, W1
	MOV.B	#2, W0
	MOV.B	W0, [W1]
;NMRAnetUtilities.mpas,693 :: 		Datagram^.ErrorCode.SubType[0] := CodeType^[0];
	MOV	#83, W0
	ADD	W10, W0, W1
	MOV.B	[W11], W0
	MOV.B	W0, [W1]
;NMRAnetUtilities.mpas,694 :: 		Datagram^.ErrorCode.SubType[1] := CodeType^[1];
	MOV	#83, W0
	ADD	W10, W0, W0
	ADD	W0, #1, W1
	ADD	W11, #1, W0
	MOV.B	[W0], [W1]
;NMRAnetUtilities.mpas,695 :: 		end else
	GOTO	L__NMRANetUtilities_LoadDatagramResultBytes131
L__NMRANetUtilities_LoadDatagramResultBytes130:
;NMRAnetUtilities.mpas,696 :: 		Datagram^.iByteCount := 0
	ADD	W10, #10, W1
	CLR	W0
	MOV.B	W0, [W1]
L__NMRANetUtilities_LoadDatagramResultBytes131:
;NMRAnetUtilities.mpas,697 :: 		end;
L_end_NMRANetUtilities_LoadDatagramResultBytes:
	RETURN
; end of _NMRANetUtilities_LoadDatagramResultBytes

_NMRAnetUtilities_BaseBufferLink:

;NMRAnetUtilities.mpas,711 :: 		begin
;NMRAnetUtilities.mpas,712 :: 		if Node^.BaseBuffers = nil then
	ADD	W10, #26, W0
	MOV	[W0], W0
	CP	W0, #0
	BRA Z	L__NMRAnetUtilities_BaseBufferLink369
	GOTO	L__NMRAnetUtilities_BaseBufferLink134
L__NMRAnetUtilities_BaseBufferLink369:
;NMRAnetUtilities.mpas,713 :: 		Node^.BaseBuffers := Buffer
	ADD	W10, #26, W0
	MOV	W11, [W0]
	GOTO	L__NMRAnetUtilities_BaseBufferLink135
;NMRAnetUtilities.mpas,714 :: 		else begin                                  // Tack it to the end of the chain
L__NMRAnetUtilities_BaseBufferLink134:
;NMRAnetUtilities.mpas,715 :: 		Temp := Node^.BaseBuffers;
	ADD	W10, #26, W0
; Temp start address is: 2 (W1)
	MOV	[W0], W1
; Temp end address is: 2 (W1)
;NMRAnetUtilities.mpas,716 :: 		while Temp^.Next <> nil do
L__NMRAnetUtilities_BaseBufferLink137:
; Temp start address is: 2 (W1)
	ADD	W1, #4, W0
	MOV	[W0], W0
	CP	W0, #0
	BRA NZ	L__NMRAnetUtilities_BaseBufferLink370
	GOTO	L__NMRAnetUtilities_BaseBufferLink138
L__NMRAnetUtilities_BaseBufferLink370:
;NMRAnetUtilities.mpas,717 :: 		Temp := Temp^.Next;
	ADD	W1, #4, W0
	MOV	[W0], W1
	GOTO	L__NMRAnetUtilities_BaseBufferLink137
L__NMRAnetUtilities_BaseBufferLink138:
;NMRAnetUtilities.mpas,718 :: 		Temp^.Next := Buffer
	ADD	W1, #4, W0
; Temp end address is: 2 (W1)
	MOV	W11, [W0]
;NMRAnetUtilities.mpas,719 :: 		end
L__NMRAnetUtilities_BaseBufferLink135:
;NMRAnetUtilities.mpas,720 :: 		end;
L_end_NMRAnetUtilities_BaseBufferLink:
	RETURN
; end of _NMRAnetUtilities_BaseBufferLink

_NMRAnetUtilities_BaseBufferUnLink:

;NMRAnetUtilities.mpas,733 :: 		begin
;NMRAnetUtilities.mpas,734 :: 		if Node^.BaseBuffers <> nil then
	ADD	W10, #26, W0
	MOV	[W0], W0
	CP	W0, #0
	BRA NZ	L__NMRAnetUtilities_BaseBufferUnLink372
	GOTO	L__NMRAnetUtilities_BaseBufferUnLink143
L__NMRAnetUtilities_BaseBufferUnLink372:
;NMRAnetUtilities.mpas,736 :: 		if Node^.BaseBuffers = Buffer then           // Root Buffer match case is easy
	ADD	W10, #26, W0
	MOV	[W0], W0
	CP	W0, W11
	BRA Z	L__NMRAnetUtilities_BaseBufferUnLink373
	GOTO	L__NMRAnetUtilities_BaseBufferUnLink146
L__NMRAnetUtilities_BaseBufferUnLink373:
;NMRAnetUtilities.mpas,737 :: 		Node^.BaseBuffers := Node^.BaseBuffers^.Next
	ADD	W10, #26, W1
	MOV	[W1], W0
	ADD	W0, #4, W0
	MOV	[W0], [W1]
	GOTO	L__NMRAnetUtilities_BaseBufferUnLink147
;NMRAnetUtilities.mpas,738 :: 		else begin
L__NMRAnetUtilities_BaseBufferUnLink146:
;NMRAnetUtilities.mpas,739 :: 		Parent := Node^.BaseBuffers;                // Already know it is not the root buffer so setup for the first level down
	ADD	W10, #26, W0
; Parent start address is: 6 (W3)
	MOV	[W0], W3
;NMRAnetUtilities.mpas,740 :: 		Temp := Node^.BaseBuffers^.Next;
	MOV	[W0], W0
	ADD	W0, #4, W0
; Temp start address is: 4 (W2)
	MOV	[W0], W2
; Parent end address is: 6 (W3)
; Temp end address is: 4 (W2)
;NMRAnetUtilities.mpas,741 :: 		while (Temp <> nil) and (Temp <> Buffer) do
L__NMRAnetUtilities_BaseBufferUnLink149:
; Temp start address is: 4 (W2)
; Parent start address is: 6 (W3)
	CP	W2, #0
	CLR	W1
	BRA Z	L__NMRAnetUtilities_BaseBufferUnLink374
	COM	W1
L__NMRAnetUtilities_BaseBufferUnLink374:
	CP	W2, W11
	CLR	W0
	BRA Z	L__NMRAnetUtilities_BaseBufferUnLink375
	COM	W0
L__NMRAnetUtilities_BaseBufferUnLink375:
	AND	W1, W0, W0
	BRA NZ	L__NMRAnetUtilities_BaseBufferUnLink376
	GOTO	L__NMRAnetUtilities_BaseBufferUnLink150
L__NMRAnetUtilities_BaseBufferUnLink376:
;NMRAnetUtilities.mpas,743 :: 		Parent := Temp;
	MOV	W2, W3
;NMRAnetUtilities.mpas,744 :: 		Temp := Temp^.Next
	ADD	W2, #4, W0
	MOV	[W0], W2
;NMRAnetUtilities.mpas,745 :: 		end;
	GOTO	L__NMRAnetUtilities_BaseBufferUnLink149
L__NMRAnetUtilities_BaseBufferUnLink150:
;NMRAnetUtilities.mpas,746 :: 		if Temp <> nil then
	CP	W2, #0
	BRA NZ	L__NMRAnetUtilities_BaseBufferUnLink377
	GOTO	L__NMRAnetUtilities_BaseBufferUnLink154
L__NMRAnetUtilities_BaseBufferUnLink377:
;NMRAnetUtilities.mpas,747 :: 		Parent^.Next := Temp^.Next
	ADD	W3, #4, W1
; Parent end address is: 6 (W3)
	ADD	W2, #4, W0
; Temp end address is: 4 (W2)
	MOV	[W0], [W1]
L__NMRAnetUtilities_BaseBufferUnLink154:
;NMRAnetUtilities.mpas,748 :: 		end
L__NMRAnetUtilities_BaseBufferUnLink147:
;NMRAnetUtilities.mpas,749 :: 		end;
L__NMRAnetUtilities_BaseBufferUnLink143:
;NMRAnetUtilities.mpas,750 :: 		end;
L_end_NMRAnetUtilities_BaseBufferUnLink:
	RETURN
; end of _NMRAnetUtilities_BaseBufferUnLink

_NMRAnetUtilities_DatagramBufferLink:

;NMRAnetUtilities.mpas,763 :: 		begin
;NMRAnetUtilities.mpas,764 :: 		if Node^.DatagramBuffers = nil then
	ADD	W10, #28, W0
	MOV	[W0], W0
	CP	W0, #0
	BRA Z	L__NMRAnetUtilities_DatagramBufferLink379
	GOTO	L__NMRAnetUtilities_DatagramBufferLink158
L__NMRAnetUtilities_DatagramBufferLink379:
;NMRAnetUtilities.mpas,765 :: 		Node^.DatagramBuffers := Buffer
	ADD	W10, #28, W0
	MOV	W11, [W0]
	GOTO	L__NMRAnetUtilities_DatagramBufferLink159
;NMRAnetUtilities.mpas,766 :: 		else begin                                  // Tack it to the end of the chain
L__NMRAnetUtilities_DatagramBufferLink158:
;NMRAnetUtilities.mpas,767 :: 		Temp := Node^.DatagramBuffers;
	ADD	W10, #28, W0
; Temp start address is: 2 (W1)
	MOV	[W0], W1
; Temp end address is: 2 (W1)
;NMRAnetUtilities.mpas,768 :: 		while Temp^.Next <> nil do
L__NMRAnetUtilities_DatagramBufferLink161:
; Temp start address is: 2 (W1)
	ADD	W1, #4, W0
	MOV	[W0], W0
	CP	W0, #0
	BRA NZ	L__NMRAnetUtilities_DatagramBufferLink380
	GOTO	L__NMRAnetUtilities_DatagramBufferLink162
L__NMRAnetUtilities_DatagramBufferLink380:
;NMRAnetUtilities.mpas,769 :: 		Temp := Temp^.Next;
	ADD	W1, #4, W0
	MOV	[W0], W1
	GOTO	L__NMRAnetUtilities_DatagramBufferLink161
L__NMRAnetUtilities_DatagramBufferLink162:
;NMRAnetUtilities.mpas,770 :: 		Temp^.Next := Buffer
	ADD	W1, #4, W0
; Temp end address is: 2 (W1)
	MOV	W11, [W0]
;NMRAnetUtilities.mpas,771 :: 		end
L__NMRAnetUtilities_DatagramBufferLink159:
;NMRAnetUtilities.mpas,772 :: 		end;
L_end_NMRAnetUtilities_DatagramBufferLink:
	RETURN
; end of _NMRAnetUtilities_DatagramBufferLink

_NMRAnetUtilities_DatagramBufferUnLink:

;NMRAnetUtilities.mpas,785 :: 		begin
;NMRAnetUtilities.mpas,786 :: 		if Node^.DatagramBuffers <> nil then
	ADD	W10, #28, W0
	MOV	[W0], W0
	CP	W0, #0
	BRA NZ	L__NMRAnetUtilities_DatagramBufferUnLink382
	GOTO	L__NMRAnetUtilities_DatagramBufferUnLink167
L__NMRAnetUtilities_DatagramBufferUnLink382:
;NMRAnetUtilities.mpas,788 :: 		if Node^.DatagramBuffers = Buffer then           // Root Buffer match case is easy
	ADD	W10, #28, W0
	MOV	[W0], W0
	CP	W0, W11
	BRA Z	L__NMRAnetUtilities_DatagramBufferUnLink383
	GOTO	L__NMRAnetUtilities_DatagramBufferUnLink170
L__NMRAnetUtilities_DatagramBufferUnLink383:
;NMRAnetUtilities.mpas,789 :: 		Node^.DatagramBuffers := Node^.DatagramBuffers^.Next
	ADD	W10, #28, W1
	MOV	[W1], W0
	ADD	W0, #4, W0
	MOV	[W0], [W1]
	GOTO	L__NMRAnetUtilities_DatagramBufferUnLink171
;NMRAnetUtilities.mpas,790 :: 		else begin
L__NMRAnetUtilities_DatagramBufferUnLink170:
;NMRAnetUtilities.mpas,791 :: 		Parent := Node^.DatagramBuffers;                // Already know it is not the root buffer so setup for the first level down
	ADD	W10, #28, W0
; Parent start address is: 6 (W3)
	MOV	[W0], W3
;NMRAnetUtilities.mpas,792 :: 		Temp := Node^.DatagramBuffers^.Next;
	MOV	[W0], W0
	ADD	W0, #4, W0
; Temp start address is: 4 (W2)
	MOV	[W0], W2
; Parent end address is: 6 (W3)
; Temp end address is: 4 (W2)
;NMRAnetUtilities.mpas,793 :: 		while (Temp <> nil) and (Temp <> Buffer) do
L__NMRAnetUtilities_DatagramBufferUnLink173:
; Temp start address is: 4 (W2)
; Parent start address is: 6 (W3)
	CP	W2, #0
	CLR	W1
	BRA Z	L__NMRAnetUtilities_DatagramBufferUnLink384
	COM	W1
L__NMRAnetUtilities_DatagramBufferUnLink384:
	CP	W2, W11
	CLR	W0
	BRA Z	L__NMRAnetUtilities_DatagramBufferUnLink385
	COM	W0
L__NMRAnetUtilities_DatagramBufferUnLink385:
	AND	W1, W0, W0
	BRA NZ	L__NMRAnetUtilities_DatagramBufferUnLink386
	GOTO	L__NMRAnetUtilities_DatagramBufferUnLink174
L__NMRAnetUtilities_DatagramBufferUnLink386:
;NMRAnetUtilities.mpas,795 :: 		Parent := Temp;
	MOV	W2, W3
;NMRAnetUtilities.mpas,796 :: 		Temp := Temp^.Next
	ADD	W2, #4, W0
	MOV	[W0], W2
;NMRAnetUtilities.mpas,797 :: 		end;
	GOTO	L__NMRAnetUtilities_DatagramBufferUnLink173
L__NMRAnetUtilities_DatagramBufferUnLink174:
;NMRAnetUtilities.mpas,798 :: 		if Temp <> nil then
	CP	W2, #0
	BRA NZ	L__NMRAnetUtilities_DatagramBufferUnLink387
	GOTO	L__NMRAnetUtilities_DatagramBufferUnLink178
L__NMRAnetUtilities_DatagramBufferUnLink387:
;NMRAnetUtilities.mpas,799 :: 		Parent^.Next := Temp^.Next
	ADD	W3, #4, W1
; Parent end address is: 6 (W3)
	ADD	W2, #4, W0
; Temp end address is: 4 (W2)
	MOV	[W0], [W1]
L__NMRAnetUtilities_DatagramBufferUnLink178:
;NMRAnetUtilities.mpas,800 :: 		end
L__NMRAnetUtilities_DatagramBufferUnLink171:
;NMRAnetUtilities.mpas,801 :: 		end;
L__NMRAnetUtilities_DatagramBufferUnLink167:
;NMRAnetUtilities.mpas,802 :: 		end;
L_end_NMRAnetUtilities_DatagramBufferUnLink:
	RETURN
; end of _NMRAnetUtilities_DatagramBufferUnLink

_NMRAnetUtilities_FindInDatagramByState:
	LNK	#0

;NMRAnetUtilities.mpas,815 :: 		begin
;NMRAnetUtilities.mpas,816 :: 		Result := False;
; AnyBit start address is: 6 (W3)
	MOV	[W14-8], W3
; Result start address is: 4 (W2)
	CLR	W2
;NMRAnetUtilities.mpas,817 :: 		Temp := Node^.DatagramBuffers;
	ADD	W10, #28, W0
; Temp start address is: 8 (W4)
	MOV	[W0], W4
; Result end address is: 4 (W2)
; Temp end address is: 8 (W4)
;NMRAnetUtilities.mpas,818 :: 		while Temp <> nil do
L__NMRAnetUtilities_FindInDatagramByState182:
; Temp start address is: 8 (W4)
; Result start address is: 4 (W2)
; AnyBit start address is: 6 (W3)
; AnyBit end address is: 6 (W3)
	CP	W4, #0
	BRA NZ	L__NMRAnetUtilities_FindInDatagramByState389
	GOTO	L__NMRAnetUtilities_FindInDatagramByState241
L__NMRAnetUtilities_FindInDatagramByState389:
; AnyBit end address is: 6 (W3)
;NMRAnetUtilities.mpas,820 :: 		if Temp^.Alias = Alias then
; AnyBit start address is: 6 (W3)
	ADD	W4, #2, W0
	MOV	[W0], W0
	CP	W0, W11
	BRA Z	L__NMRAnetUtilities_FindInDatagramByState390
	GOTO	L__NMRAnetUtilities_FindInDatagramByState187
L__NMRAnetUtilities_FindInDatagramByState390:
;NMRAnetUtilities.mpas,822 :: 		if AnyBit then
	CP0	W3
	BRA NZ	L__NMRAnetUtilities_FindInDatagramByState391
	GOTO	L__NMRAnetUtilities_FindInDatagramByState190
L__NMRAnetUtilities_FindInDatagramByState391:
;NMRAnetUtilities.mpas,824 :: 		if Temp^.State and AState <> 0 then                                     // Test if any bit is set
	MOV.B	[W4], W0
	ZE	W0, W1
	ZE	W13, W0
	AND	W1, W0, W0
	CP	W0, #0
	BRA NZ	L__NMRAnetUtilities_FindInDatagramByState392
	GOTO	L__NMRAnetUtilities_FindInDatagramByState193
L__NMRAnetUtilities_FindInDatagramByState392:
; Result end address is: 4 (W2)
; AnyBit end address is: 6 (W3)
;NMRAnetUtilities.mpas,826 :: 		Buffer := Temp;
	MOV	W4, [W12]
; Temp end address is: 8 (W4)
;NMRAnetUtilities.mpas,827 :: 		Result := True;
; Result start address is: 2 (W1)
	MOV	#65535, W1
;NMRAnetUtilities.mpas,828 :: 		Break
; Result end address is: 2 (W1)
	GOTO	L__NMRAnetUtilities_FindInDatagramByState183
;NMRAnetUtilities.mpas,829 :: 		end;
L__NMRAnetUtilities_FindInDatagramByState193:
;NMRAnetUtilities.mpas,830 :: 		end else
; Temp start address is: 8 (W4)
; AnyBit start address is: 6 (W3)
; Result start address is: 4 (W2)
	GOTO	L__NMRAnetUtilities_FindInDatagramByState191
L__NMRAnetUtilities_FindInDatagramByState190:
;NMRAnetUtilities.mpas,832 :: 		if Temp^.State and AState = AState then                                 // Test for and exact match for the State bits
	MOV.B	[W4], W0
	ZE	W0, W1
	ZE	W13, W0
	AND	W1, W0, W1
	ZE	W13, W0
	CP	W1, W0
	BRA Z	L__NMRAnetUtilities_FindInDatagramByState393
	GOTO	L__NMRAnetUtilities_FindInDatagramByState196
L__NMRAnetUtilities_FindInDatagramByState393:
; Result end address is: 4 (W2)
; AnyBit end address is: 6 (W3)
;NMRAnetUtilities.mpas,834 :: 		Buffer := Temp;
	MOV	W4, [W12]
; Temp end address is: 8 (W4)
;NMRAnetUtilities.mpas,835 :: 		Result := True;
; Result start address is: 2 (W1)
	MOV	#65535, W1
;NMRAnetUtilities.mpas,836 :: 		Break
; Result end address is: 2 (W1)
	GOTO	L__NMRAnetUtilities_FindInDatagramByState183
;NMRAnetUtilities.mpas,837 :: 		end;
L__NMRAnetUtilities_FindInDatagramByState196:
;NMRAnetUtilities.mpas,838 :: 		end
; Temp start address is: 8 (W4)
; AnyBit start address is: 6 (W3)
; Result start address is: 4 (W2)
L__NMRAnetUtilities_FindInDatagramByState191:
;NMRAnetUtilities.mpas,839 :: 		end;
L__NMRAnetUtilities_FindInDatagramByState187:
;NMRAnetUtilities.mpas,840 :: 		Temp := Temp^.Next
	ADD	W4, #4, W0
	MOV	[W0], W4
;NMRAnetUtilities.mpas,841 :: 		end;
; Result end address is: 4 (W2)
; AnyBit end address is: 6 (W3)
; Temp end address is: 8 (W4)
	GOTO	L__NMRAnetUtilities_FindInDatagramByState182
L__NMRAnetUtilities_FindInDatagramByState241:
;NMRAnetUtilities.mpas,818 :: 		while Temp <> nil do
	MOV	W2, W1
;NMRAnetUtilities.mpas,841 :: 		end;
L__NMRAnetUtilities_FindInDatagramByState183:
;NMRAnetUtilities.mpas,842 :: 		end;
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetUtilities_FindInDatagramByState:
	ULNK
	RETURN
; end of _NMRAnetUtilities_FindInDatagramByState

_NMRAnetUtilities_FindInProcessDatagram:

;NMRAnetUtilities.mpas,854 :: 		begin
;NMRAnetUtilities.mpas,855 :: 		Result := NMRAnetUtilities_FindInDatagramByState(Node, Alias, Buffer, CBS_PROCESSING, False);
	PUSH	W13
	MOV.B	#2, W13
	CLR	W0
	PUSH	W0
	CALL	_NMRAnetUtilities_FindInDatagramByState
	SUB	#2, W15
; Result start address is: 2 (W1)
	MOV	W0, W1
;NMRAnetUtilities.mpas,856 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetUtilities_FindInProcessDatagram:
	POP	W13
	RETURN
; end of _NMRAnetUtilities_FindInProcessDatagram

_NMRAnetUtilities_FindCompletedDatagram:

;NMRAnetUtilities.mpas,867 :: 		begin
;NMRAnetUtilities.mpas,868 :: 		Result := NMRAnetUtilities_FindInDatagramByState(Node, Alias, Buffer, CBS_TRANSFER_COMPLETE, False);
	PUSH	W13
	MOV.B	#8, W13
	CLR	W0
	PUSH	W0
	CALL	_NMRAnetUtilities_FindInDatagramByState
	SUB	#2, W15
; Result start address is: 2 (W1)
	MOV	W0, W1
;NMRAnetUtilities.mpas,869 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetUtilities_FindCompletedDatagram:
	POP	W13
	RETURN
; end of _NMRAnetUtilities_FindCompletedDatagram

_NMRAnetUtilities_FindOutgoingDatagram:

;NMRAnetUtilities.mpas,884 :: 		begin
;NMRAnetUtilities.mpas,885 :: 		Result := False;
; Result start address is: 6 (W3)
	CLR	W3
;NMRAnetUtilities.mpas,886 :: 		AState := CBS_OUTGOING;
; AState start address is: 4 (W2)
	MOV.B	#4, W2
;NMRAnetUtilities.mpas,887 :: 		if ProcessingOnly then
	CP0	W12
	BRA NZ	L__NMRAnetUtilities_FindOutgoingDatagram397
	GOTO	L__NMRAnetUtilities_FindOutgoingDatagram242
L__NMRAnetUtilities_FindOutgoingDatagram397:
;NMRAnetUtilities.mpas,888 :: 		AState := AState or CBS_PROCESSING;
; AState start address is: 4 (W2)
	IOR.B	W2, #2, W2
; AState end address is: 4 (W2)
; AState end address is: 4 (W2)
	GOTO	L__NMRAnetUtilities_FindOutgoingDatagram202
L__NMRAnetUtilities_FindOutgoingDatagram242:
;NMRAnetUtilities.mpas,887 :: 		if ProcessingOnly then
;NMRAnetUtilities.mpas,888 :: 		AState := AState or CBS_PROCESSING;
L__NMRAnetUtilities_FindOutgoingDatagram202:
;NMRAnetUtilities.mpas,890 :: 		Temp := Node^.DatagramBuffers;
; AState start address is: 4 (W2)
	ADD	W10, #28, W0
; Temp start address is: 8 (W4)
	MOV	[W0], W4
; Result end address is: 6 (W3)
; Temp end address is: 8 (W4)
;NMRAnetUtilities.mpas,891 :: 		while Temp <> nil do
L__NMRAnetUtilities_FindOutgoingDatagram205:
; Temp start address is: 8 (W4)
; AState start address is: 4 (W2)
; AState end address is: 4 (W2)
; Result start address is: 6 (W3)
	CP	W4, #0
	BRA NZ	L__NMRAnetUtilities_FindOutgoingDatagram398
	GOTO	L__NMRAnetUtilities_FindOutgoingDatagram243
L__NMRAnetUtilities_FindOutgoingDatagram398:
; AState end address is: 4 (W2)
;NMRAnetUtilities.mpas,893 :: 		if Temp^.State and AState = AState then
; AState start address is: 4 (W2)
	MOV.B	[W4], W0
	ZE	W0, W1
	ZE	W2, W0
	AND	W1, W0, W1
	ZE	W2, W0
	CP	W1, W0
	BRA Z	L__NMRAnetUtilities_FindOutgoingDatagram399
	GOTO	L__NMRAnetUtilities_FindOutgoingDatagram210
L__NMRAnetUtilities_FindOutgoingDatagram399:
; AState end address is: 4 (W2)
; Result end address is: 6 (W3)
;NMRAnetUtilities.mpas,895 :: 		Buffer := Temp;
	MOV	W4, [W11]
; Temp end address is: 8 (W4)
;NMRAnetUtilities.mpas,896 :: 		Result := True;
; Result start address is: 2 (W1)
	MOV	#65535, W1
;NMRAnetUtilities.mpas,897 :: 		Break
; Result end address is: 2 (W1)
	GOTO	L__NMRAnetUtilities_FindOutgoingDatagram206
;NMRAnetUtilities.mpas,898 :: 		end;
L__NMRAnetUtilities_FindOutgoingDatagram210:
;NMRAnetUtilities.mpas,899 :: 		Temp := Temp^.Next
; Temp start address is: 8 (W4)
; Result start address is: 6 (W3)
; AState start address is: 4 (W2)
	ADD	W4, #4, W0
	MOV	[W0], W4
;NMRAnetUtilities.mpas,900 :: 		end;
; AState end address is: 4 (W2)
; Result end address is: 6 (W3)
; Temp end address is: 8 (W4)
	GOTO	L__NMRAnetUtilities_FindOutgoingDatagram205
L__NMRAnetUtilities_FindOutgoingDatagram243:
;NMRAnetUtilities.mpas,891 :: 		while Temp <> nil do
	MOV	W3, W1
;NMRAnetUtilities.mpas,900 :: 		end;
L__NMRAnetUtilities_FindOutgoingDatagram206:
;NMRAnetUtilities.mpas,901 :: 		end;
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetUtilities_FindOutgoingDatagram:
	RETURN
; end of _NMRAnetUtilities_FindOutgoingDatagram

_NMRAnetUtilities_NextBaseBuffer:

;NMRAnetUtilities.mpas,912 :: 		begin
;NMRAnetUtilities.mpas,913 :: 		Result := Node^.BaseBuffers;
	ADD	W10, #26, W0
; Result start address is: 2 (W1)
	MOV	[W0], W1
;NMRAnetUtilities.mpas,914 :: 		if Result <> nil then
	CP	W1, #0
	BRA NZ	L__NMRAnetUtilities_NextBaseBuffer401
	GOTO	L__NMRAnetUtilities_NextBaseBuffer245
L__NMRAnetUtilities_NextBaseBuffer401:
; Result end address is: 2 (W1)
;NMRAnetUtilities.mpas,916 :: 		while Result^.State and CBS_PROCESSING <> 0 do      // Skip over any In Process Buffers
L__NMRAnetUtilities_NextBaseBuffer217:
; Result start address is: 2 (W1)
	MOV	W1, W0
	MOV.B	[W0], W0
	ZE	W0, W0
	AND	W0, #2, W0
	CP	W0, #0
	BRA NZ	L__NMRAnetUtilities_NextBaseBuffer402
	GOTO	L__NMRAnetUtilities_NextBaseBuffer244
L__NMRAnetUtilities_NextBaseBuffer402:
;NMRAnetUtilities.mpas,918 :: 		Result := Result^.Next;
	MOV	W1, W0
; Result end address is: 2 (W1)
	ADD	W0, #4, W0
; Result start address is: 0 (W0)
	MOV	[W0], W0
;NMRAnetUtilities.mpas,919 :: 		if Result = nil then
	CP	W0, #0
	BRA Z	L__NMRAnetUtilities_NextBaseBuffer403
	GOTO	L__NMRAnetUtilities_NextBaseBuffer222
L__NMRAnetUtilities_NextBaseBuffer403:
;NMRAnetUtilities.mpas,920 :: 		Break
	MOV	W0, W1
	GOTO	L__NMRAnetUtilities_NextBaseBuffer218
L__NMRAnetUtilities_NextBaseBuffer222:
;NMRAnetUtilities.mpas,921 :: 		end
	MOV	W0, W1
; Result end address is: 0 (W0)
	GOTO	L__NMRAnetUtilities_NextBaseBuffer217
L__NMRAnetUtilities_NextBaseBuffer244:
;NMRAnetUtilities.mpas,916 :: 		while Result^.State and CBS_PROCESSING <> 0 do      // Skip over any In Process Buffers
;NMRAnetUtilities.mpas,921 :: 		end
L__NMRAnetUtilities_NextBaseBuffer218:
;NMRAnetUtilities.mpas,922 :: 		end
; Result start address is: 2 (W1)
; Result end address is: 2 (W1)
	GOTO	L__NMRAnetUtilities_NextBaseBuffer214
L__NMRAnetUtilities_NextBaseBuffer245:
;NMRAnetUtilities.mpas,914 :: 		if Result <> nil then
;NMRAnetUtilities.mpas,922 :: 		end
L__NMRAnetUtilities_NextBaseBuffer214:
;NMRAnetUtilities.mpas,923 :: 		end;
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetUtilities_NextBaseBuffer:
	RETURN
; end of _NMRAnetUtilities_NextBaseBuffer

_NMRAnetUtilities_NextDatagramBuffer:

;NMRAnetUtilities.mpas,934 :: 		begin
;NMRAnetUtilities.mpas,935 :: 		Result := Node^.DatagramBuffers;
	ADD	W10, #28, W0
; Result start address is: 2 (W1)
	MOV	[W0], W1
;NMRAnetUtilities.mpas,936 :: 		if Result <> nil then
	CP	W1, #0
	BRA NZ	L__NMRAnetUtilities_NextDatagramBuffer405
	GOTO	L__NMRAnetUtilities_NextDatagramBuffer247
L__NMRAnetUtilities_NextDatagramBuffer405:
; Result end address is: 2 (W1)
;NMRAnetUtilities.mpas,938 :: 		while Result^.State and CBS_TRANSFER_COMPLETE = 0 do    // Use only Complete Transfers
L__NMRAnetUtilities_NextDatagramBuffer229:
; Result start address is: 2 (W1)
	MOV	W1, W0
	MOV.B	[W0], W0
	ZE	W0, W0
	AND	W0, #8, W0
	CP	W0, #0
	BRA Z	L__NMRAnetUtilities_NextDatagramBuffer406
	GOTO	L__NMRAnetUtilities_NextDatagramBuffer246
L__NMRAnetUtilities_NextDatagramBuffer406:
;NMRAnetUtilities.mpas,940 :: 		Result := Result^.Next;
	MOV	W1, W0
; Result end address is: 2 (W1)
	ADD	W0, #4, W0
; Result start address is: 0 (W0)
	MOV	[W0], W0
;NMRAnetUtilities.mpas,941 :: 		if Result = nil then
	CP	W0, #0
	BRA Z	L__NMRAnetUtilities_NextDatagramBuffer407
	GOTO	L__NMRAnetUtilities_NextDatagramBuffer234
L__NMRAnetUtilities_NextDatagramBuffer407:
;NMRAnetUtilities.mpas,942 :: 		Break
	MOV	W0, W1
	GOTO	L__NMRAnetUtilities_NextDatagramBuffer230
L__NMRAnetUtilities_NextDatagramBuffer234:
;NMRAnetUtilities.mpas,943 :: 		end
	MOV	W0, W1
; Result end address is: 0 (W0)
	GOTO	L__NMRAnetUtilities_NextDatagramBuffer229
L__NMRAnetUtilities_NextDatagramBuffer246:
;NMRAnetUtilities.mpas,938 :: 		while Result^.State and CBS_TRANSFER_COMPLETE = 0 do    // Use only Complete Transfers
;NMRAnetUtilities.mpas,943 :: 		end
L__NMRAnetUtilities_NextDatagramBuffer230:
;NMRAnetUtilities.mpas,944 :: 		end
; Result start address is: 2 (W1)
; Result end address is: 2 (W1)
	GOTO	L__NMRAnetUtilities_NextDatagramBuffer226
L__NMRAnetUtilities_NextDatagramBuffer247:
;NMRAnetUtilities.mpas,936 :: 		if Result <> nil then
;NMRAnetUtilities.mpas,944 :: 		end
L__NMRAnetUtilities_NextDatagramBuffer226:
;NMRAnetUtilities.mpas,945 :: 		end;
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetUtilities_NextDatagramBuffer:
	RETURN
; end of _NMRAnetUtilities_NextDatagramBuffer
