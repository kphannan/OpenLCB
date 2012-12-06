
_NMRAnetNode_SortNodeList:
	LNK	#4

;NMRAnetNode.mpas,107 :: 		begin
;NMRAnetNode.mpas,108 :: 		for i := 1 to LocalNodes.AllocatedCount - 1 do
; i start address is: 10 (W5)
	MOV	#1, W5
; i end address is: 10 (W5)
L__NMRAnetNode_SortNodeList1:
; i start address is: 10 (W5)
	MOV	#3120, W0
	ADD	W10, W0, W0
	MOV	[W0], W0
	SUB	W0, #1, W4
	CP	W5, W4
	BRA LE	L__NMRAnetNode_SortNodeList220
	GOTO	L__NMRAnetNode_SortNodeList5
L__NMRAnetNode_SortNodeList220:
;NMRAnetNode.mpas,110 :: 		LocalData := LocalNodes.AllocatedList[i];
	MOV	#2990, W0
	ADD	W10, W0, W1
	SL	W5, #1, W0
	ADD	W1, W0, W0
	MOV	[W0], W0
	MOV	W0, [W14+2]
;NMRAnetNode.mpas,111 :: 		j := i;
	MOV	W5, [W14+0]
; i end address is: 10 (W5)
;NMRAnetNode.mpas,112 :: 		while (j > 0) and (LocalNodes.AllocatedList[j-1]^.Info.AliasID > LocalData^.Info.AliasID) do
L__NMRAnetNode_SortNodeList7:
; i start address is: 10 (W5)
	MOV	[W14+0], W0
	CP	W0, #0
	CLR	W3
	BRA LE	L__NMRAnetNode_SortNodeList221
	COM	W3
L__NMRAnetNode_SortNodeList221:
	MOV	#2990, W0
	ADD	W10, W0, W1
	MOV	[W14+0], W0
	DEC	W0
	SL	W0, #1, W0
	ADD	W1, W0, W0
	MOV	[W0], W0
	INC2	W0
	ADD	W0, #16, W2
	MOV	[W14+2], W0
	INC2	W0
	ADD	W0, #16, W0
	MOV	[W0], W1
	MOV	[W2], W0
	CP	W0, W1
	CLR	W0
	BRA LEU	L__NMRAnetNode_SortNodeList222
	COM	W0
L__NMRAnetNode_SortNodeList222:
	AND	W3, W0, W0
	BRA NZ	L__NMRAnetNode_SortNodeList223
	GOTO	L__NMRAnetNode_SortNodeList8
L__NMRAnetNode_SortNodeList223:
;NMRAnetNode.mpas,114 :: 		LocalNodes.AllocatedList[j] := LocalNodes.AllocatedList[j-1];
	MOV	#2990, W0
	ADD	W10, W0, W1
	MOV	[W14+0], W0
	SL	W0, #1, W0
	ADD	W1, W0, W2
	MOV	#2990, W0
	ADD	W10, W0, W1
	MOV	[W14+0], W0
	DEC	W0
	SL	W0, #1, W0
	ADD	W1, W0, W0
	MOV	[W0], [W2]
;NMRAnetNode.mpas,115 :: 		j := j - 1;
	MOV	[W14+0], W0
	DEC	W0
	MOV	W0, [W14+0]
;NMRAnetNode.mpas,116 :: 		if j = 0 then                                                             // mPascal does not support short circuit Boolean so the [j-1] when j = 0 is bad in the while conditional define
	CP	W0, #0
	BRA Z	L__NMRAnetNode_SortNodeList224
	GOTO	L__NMRAnetNode_SortNodeList12
L__NMRAnetNode_SortNodeList224:
;NMRAnetNode.mpas,117 :: 		Break;
	GOTO	L__NMRAnetNode_SortNodeList8
L__NMRAnetNode_SortNodeList12:
;NMRAnetNode.mpas,118 :: 		end;
	GOTO	L__NMRAnetNode_SortNodeList7
L__NMRAnetNode_SortNodeList8:
;NMRAnetNode.mpas,119 :: 		LocalNodes.AllocatedList[j] := LocalData;
	MOV	#2990, W0
	ADD	W10, W0, W1
	MOV	[W14+0], W0
	SL	W0, #1, W0
	ADD	W1, W0, W1
	MOV	[W14+2], W0
	MOV	W0, [W1]
;NMRAnetNode.mpas,121 :: 		end;
	CP	W5, W4
	BRA NZ	L__NMRAnetNode_SortNodeList225
	GOTO	L__NMRAnetNode_SortNodeList5
L__NMRAnetNode_SortNodeList225:
; i start address is: 10 (W5)
	INC	W5
; i end address is: 10 (W5)
; i end address is: 10 (W5)
	GOTO	L__NMRAnetNode_SortNodeList1
L__NMRAnetNode_SortNodeList5:
;NMRAnetNode.mpas,122 :: 		end;
L_end_NMRAnetNode_SortNodeList:
	ULNK
	RETURN
; end of _NMRAnetNode_SortNodeList

NMRAnetNode_BinarySearchAliasID:
	LNK	#4

;NMRAnetNode.mpas,158 :: 		begin
;NMRAnetNode.mpas,159 :: 		Min := 0;
; Min start address is: 8 (W4)
	CLR	W4
;NMRAnetNode.mpas,160 :: 		Max := LocalNodes.AllocatedCount - 1;
	MOV	#3120, W0
	ADD	W10, W0, W0
	MOV	[W0], W0
; Max start address is: 6 (W3)
	SUB	W0, #1, W3
; Min end address is: 8 (W4)
; Max end address is: 6 (W3)
;NMRAnetNode.mpas,161 :: 		while Min <= Max do
L_NMRAnetNode_BinarySearchAliasID16:
; Max start address is: 6 (W3)
; Min start address is: 8 (W4)
	CP	W4, W3
	BRA LE	L_NMRAnetNode_BinarySearchAliasID227
	GOTO	L_NMRAnetNode_BinarySearchAliasID17
L_NMRAnetNode_BinarySearchAliasID227:
;NMRAnetNode.mpas,163 :: 		Middle := (Min + Max) shr 1;
	ADD	W4, W3, W0
	ASR	W0, #1, W2
	MOV	W2, [W14+0]
;NMRAnetNode.mpas,164 :: 		MidAliasID := LocalNodes.AllocatedList[Middle]^.Info.AliasID;
	MOV	#2990, W0
	ADD	W10, W0, W1
	SL	W2, #1, W0
	ADD	W1, W0, W0
	MOV	[W0], W0
	INC2	W0
	ADD	W0, #16, W0
	MOV	[W0], W0
	MOV	W0, [W14+2]
;NMRAnetNode.mpas,165 :: 		if AliasID < MidAliasID then
	CP	W11, W0
	BRA LTU	L_NMRAnetNode_BinarySearchAliasID228
	GOTO	L_NMRAnetNode_BinarySearchAliasID21
L_NMRAnetNode_BinarySearchAliasID228:
; Max end address is: 6 (W3)
;NMRAnetNode.mpas,166 :: 		Max := Middle - 1
	MOV	[W14+0], W0
; Max start address is: 0 (W0)
	DEC	W0
	MOV	W0, W3
; Max end address is: 0 (W0)
; Min end address is: 8 (W4)
	GOTO	L_NMRAnetNode_BinarySearchAliasID22
;NMRAnetNode.mpas,167 :: 		else
L_NMRAnetNode_BinarySearchAliasID21:
;NMRAnetNode.mpas,168 :: 		if AliasID = MidAliasID then
; Max start address is: 6 (W3)
	ADD	W14, #2, W0
	CP	W11, [W0]
	BRA Z	L_NMRAnetNode_BinarySearchAliasID229
	GOTO	L_NMRAnetNode_BinarySearchAliasID24
L_NMRAnetNode_BinarySearchAliasID229:
; Max end address is: 6 (W3)
;NMRAnetNode.mpas,170 :: 		Result := Middle;
; Result start address is: 2 (W1)
	MOV	[W14+0], W1
;NMRAnetNode.mpas,171 :: 		Exit
; Result end address is: 2 (W1)
	GOTO	L_end_NMRAnetNode_BinarySearchAliasID
;NMRAnetNode.mpas,172 :: 		end else
L_NMRAnetNode_BinarySearchAliasID24:
;NMRAnetNode.mpas,173 :: 		Min := Middle + 1
; Max start address is: 6 (W3)
	MOV	[W14+0], W0
; Min start address is: 0 (W0)
	INC	W0
; Min end address is: 0 (W0)
; Max end address is: 6 (W3)
	MOV	W0, W4
L_NMRAnetNode_BinarySearchAliasID22:
;NMRAnetNode.mpas,174 :: 		end;
; Min start address is: 8 (W4)
; Max start address is: 6 (W3)
; Min end address is: 8 (W4)
; Max end address is: 6 (W3)
	GOTO	L_NMRAnetNode_BinarySearchAliasID16
L_NMRAnetNode_BinarySearchAliasID17:
;NMRAnetNode.mpas,175 :: 		Result := -1;
; Result start address is: 2 (W1)
	MOV	#65535, W1
; Result end address is: 2 (W1)
;NMRAnetNode.mpas,176 :: 		end;
L_end_NMRAnetNode_BinarySearchAliasID:
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_BinarySearchAliasID:
	ULNK
	RETURN
; end of NMRAnetNode_BinarySearchAliasID

_NMRAnetNode_FindByAlias:

;NMRAnetNode.mpas,190 :: 		begin
;NMRAnetNode.mpas,191 :: 		Result := PNMRAnetNode( nil);
	PUSH	W10
	PUSH	W11
; Result start address is: 10 (W5)
	MOV	#0, W5
;NMRAnetNode.mpas,202 :: 		Index := BinarySearchAliasID(Nodes, AliasID);
	MOV	W10, W11
	MOV	#lo_addr(_Nodes), W10
	CALL	NMRAnetNode_BinarySearchAliasID
; Index start address is: 6 (W3)
	MOV	W0, W3
;NMRAnetNode.mpas,203 :: 		if (Index > -1) and (Index < Nodes.AllocatedCount) then
	MOV	#65535, W1
	CP	W0, W1
	CLR	W2
	BRA LE	L__NMRAnetNode_FindByAlias231
	COM	W2
L__NMRAnetNode_FindByAlias231:
	MOV	#lo_addr(_Nodes+3120), W1
	CP	W0, [W1]
	CLR	W0
	BRA GE	L__NMRAnetNode_FindByAlias232
	COM	W0
L__NMRAnetNode_FindByAlias232:
	AND	W2, W0, W0
	CP0	W0
	BRA NZ	L__NMRAnetNode_FindByAlias233
	GOTO	L__NMRAnetNode_FindByAlias210
L__NMRAnetNode_FindByAlias233:
; Result end address is: 10 (W5)
;NMRAnetNode.mpas,204 :: 		Result := Nodes.AllocatedList[Index];
	SL	W3, #1, W1
; Index end address is: 6 (W3)
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W0
; Result start address is: 2 (W1)
	MOV	[W0], W1
; Result end address is: 2 (W1)
	GOTO	L__NMRAnetNode_FindByAlias28
L__NMRAnetNode_FindByAlias210:
;NMRAnetNode.mpas,203 :: 		if (Index > -1) and (Index < Nodes.AllocatedCount) then
	MOV	W5, W1
;NMRAnetNode.mpas,204 :: 		Result := Nodes.AllocatedList[Index];
L__NMRAnetNode_FindByAlias28:
;NMRAnetNode.mpas,205 :: 		end;
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetNode_FindByAlias:
	POP	W11
	POP	W10
	RETURN
; end of _NMRAnetNode_FindByAlias

_NMRAnetNode_FindByNodeID:

;NMRAnetNode.mpas,224 :: 		begin
;NMRAnetNode.mpas,225 :: 		Result := PNMRAnetNode( nil);
; Result start address is: 12 (W6)
	MOV	#0, W6
;NMRAnetNode.mpas,226 :: 		for i := 0 to Nodes.AllocatedCount - 1 do
; i start address is: 14 (W7)
	CLR	W7
; Result end address is: 12 (W6)
; i end address is: 14 (W7)
L__NMRAnetNode_FindByNodeID31:
; i start address is: 14 (W7)
; Result start address is: 12 (W6)
	MOV	_Nodes+3120, W0
	SUB	W0, #1, W5
	CP	W7, W5
	BRA LE	L__NMRAnetNode_FindByNodeID235
	GOTO	L__NMRAnetNode_FindByNodeID211
L__NMRAnetNode_FindByNodeID235:
;NMRAnetNode.mpas,228 :: 		if Nodes.AllocatedList[i]^.Info.ID[0] = NodeID[0] then
	SL	W7, #1, W1
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W0
	MOV	[W0], W0
	ADD	W0, #2, W4
	MOV.D	[W10], W2
	MOV.D	[W4], W0
	CP	W0, W2
	CPB	W1, W3
	BRA Z	L__NMRAnetNode_FindByNodeID236
	GOTO	L__NMRAnetNode_FindByNodeID37
L__NMRAnetNode_FindByNodeID236:
;NMRAnetNode.mpas,229 :: 		if Nodes.AllocatedList[i]^.Info.ID[1] = NodeID[1] then
	SL	W7, #1, W1
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W0
	MOV	[W0], W0
	INC2	W0
	ADD	W0, #4, W4
	ADD	W10, #4, W0
	MOV.D	[W0], W2
	MOV.D	[W4], W0
	CP	W0, W2
	CPB	W1, W3
	BRA Z	L__NMRAnetNode_FindByNodeID237
	GOTO	L__NMRAnetNode_FindByNodeID40
L__NMRAnetNode_FindByNodeID237:
; Result end address is: 12 (W6)
;NMRAnetNode.mpas,231 :: 		Result := Nodes.AllocatedList[i];
	SL	W7, #1, W1
; i end address is: 14 (W7)
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W0
; Result start address is: 0 (W0)
	MOV	[W0], W0
;NMRAnetNode.mpas,232 :: 		Break
	MOV	W0, W1
; Result end address is: 0 (W0)
	GOTO	L__NMRAnetNode_FindByNodeID35
;NMRAnetNode.mpas,233 :: 		end;
L__NMRAnetNode_FindByNodeID40:
; i start address is: 14 (W7)
; Result start address is: 12 (W6)
L__NMRAnetNode_FindByNodeID37:
;NMRAnetNode.mpas,234 :: 		end
	CP	W7, W5
	BRA NZ	L__NMRAnetNode_FindByNodeID238
	GOTO	L__NMRAnetNode_FindByNodeID212
L__NMRAnetNode_FindByNodeID238:
; i start address is: 14 (W7)
	INC	W7
; i end address is: 14 (W7)
; Result end address is: 12 (W6)
; i end address is: 14 (W7)
	GOTO	L__NMRAnetNode_FindByNodeID31
L__NMRAnetNode_FindByNodeID211:
;NMRAnetNode.mpas,226 :: 		for i := 0 to Nodes.AllocatedCount - 1 do
	MOV	W6, W1
;NMRAnetNode.mpas,234 :: 		end
	GOTO	L__NMRAnetNode_FindByNodeID35
L__NMRAnetNode_FindByNodeID212:
	MOV	W6, W1
L__NMRAnetNode_FindByNodeID35:
;NMRAnetNode.mpas,235 :: 		end;
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetNode_FindByNodeID:
	RETURN
; end of _NMRAnetNode_FindByNodeID

_NMRAnetNode_FindFirstVirtualNode:

;NMRAnetNode.mpas,254 :: 		begin
;NMRAnetNode.mpas,255 :: 		Result := nil;
; Result start address is: 4 (W2)
	CLR	W2
;NMRAnetNode.mpas,256 :: 		i := 0;
; i start address is: 6 (W3)
	CLR	W3
; Result end address is: 4 (W2)
; i end address is: 6 (W3)
;NMRAnetNode.mpas,257 :: 		while (i < Nodes.AllocatedCount) do
L__NMRAnetNode_FindFirstVirtualNode44:
; i start address is: 6 (W3)
; Result start address is: 4 (W2)
	MOV	#lo_addr(_Nodes+3120), W0
	CP	W3, [W0]
	BRA LT	L__NMRAnetNode_FindFirstVirtualNode240
	GOTO	L__NMRAnetNode_FindFirstVirtualNode213
L__NMRAnetNode_FindFirstVirtualNode240:
;NMRAnetNode.mpas,259 :: 		if Nodes.AllocatedList[i]^.State and NS_VIRTUAL = NS_VIRTUAL then
	SL	W3, #1, W1
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W0
	MOV	[W0], W0
	MOV.B	[W0], W0
	ZE	W0, W0
	AND	W0, #8, W0
	CP	W0, #8
	BRA Z	L__NMRAnetNode_FindFirstVirtualNode241
	GOTO	L__NMRAnetNode_FindFirstVirtualNode49
L__NMRAnetNode_FindFirstVirtualNode241:
; Result end address is: 4 (W2)
;NMRAnetNode.mpas,261 :: 		Result := Nodes.AllocatedList[i];
	SL	W3, #1, W1
; i end address is: 6 (W3)
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W0
; Result start address is: 2 (W1)
	MOV	[W0], W1
;NMRAnetNode.mpas,262 :: 		Break;
; Result end address is: 2 (W1)
	GOTO	L__NMRAnetNode_FindFirstVirtualNode45
;NMRAnetNode.mpas,263 :: 		end;
L__NMRAnetNode_FindFirstVirtualNode49:
;NMRAnetNode.mpas,264 :: 		Inc(i);
; i start address is: 6 (W3)
; Result start address is: 4 (W2)
	INC	W3
;NMRAnetNode.mpas,265 :: 		end;
; Result end address is: 4 (W2)
; i end address is: 6 (W3)
	GOTO	L__NMRAnetNode_FindFirstVirtualNode44
L__NMRAnetNode_FindFirstVirtualNode213:
;NMRAnetNode.mpas,257 :: 		while (i < Nodes.AllocatedCount) do
	MOV	W2, W1
;NMRAnetNode.mpas,265 :: 		end;
L__NMRAnetNode_FindFirstVirtualNode45:
;NMRAnetNode.mpas,266 :: 		end;
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetNode_FindFirstVirtualNode:
	RETURN
; end of _NMRAnetNode_FindFirstVirtualNode

_NMRAnetNode_FindLastVirtualNode:

;NMRAnetNode.mpas,285 :: 		begin
;NMRAnetNode.mpas,286 :: 		Result := nil;
; Result start address is: 4 (W2)
	CLR	W2
;NMRAnetNode.mpas,287 :: 		i := Nodes.AllocatedCount - 1;
	MOV	_Nodes+3120, W0
; i start address is: 6 (W3)
	SUB	W0, #1, W3
; Result end address is: 4 (W2)
; i end address is: 6 (W3)
;NMRAnetNode.mpas,288 :: 		while (i > -1) do
L__NMRAnetNode_FindLastVirtualNode53:
; i start address is: 6 (W3)
; Result start address is: 4 (W2)
	MOV	#65535, W0
	CP	W3, W0
	BRA GT	L__NMRAnetNode_FindLastVirtualNode243
	GOTO	L__NMRAnetNode_FindLastVirtualNode214
L__NMRAnetNode_FindLastVirtualNode243:
;NMRAnetNode.mpas,290 :: 		if Nodes.AllocatedList[i]^.State and NS_VIRTUAL = NS_VIRTUAL then
	SL	W3, #1, W1
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W0
	MOV	[W0], W0
	MOV.B	[W0], W0
	ZE	W0, W0
	AND	W0, #8, W0
	CP	W0, #8
	BRA Z	L__NMRAnetNode_FindLastVirtualNode244
	GOTO	L__NMRAnetNode_FindLastVirtualNode58
L__NMRAnetNode_FindLastVirtualNode244:
; Result end address is: 4 (W2)
;NMRAnetNode.mpas,292 :: 		Result := Nodes.AllocatedList[i];
	SL	W3, #1, W1
; i end address is: 6 (W3)
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W0
; Result start address is: 2 (W1)
	MOV	[W0], W1
;NMRAnetNode.mpas,293 :: 		Break;
; Result end address is: 2 (W1)
	GOTO	L__NMRAnetNode_FindLastVirtualNode54
;NMRAnetNode.mpas,294 :: 		end;
L__NMRAnetNode_FindLastVirtualNode58:
;NMRAnetNode.mpas,295 :: 		Dec(i);
; i start address is: 6 (W3)
; Result start address is: 4 (W2)
	DEC	W3
;NMRAnetNode.mpas,296 :: 		end;
; Result end address is: 4 (W2)
; i end address is: 6 (W3)
	GOTO	L__NMRAnetNode_FindLastVirtualNode53
L__NMRAnetNode_FindLastVirtualNode214:
;NMRAnetNode.mpas,288 :: 		while (i > -1) do
	MOV	W2, W1
;NMRAnetNode.mpas,296 :: 		end;
L__NMRAnetNode_FindLastVirtualNode54:
;NMRAnetNode.mpas,297 :: 		end;
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetNode_FindLastVirtualNode:
	RETURN
; end of _NMRAnetNode_FindLastVirtualNode

_NMRAnetNode_Initialize:

;NMRAnetNode.mpas,313 :: 		begin
;NMRAnetNode.mpas,314 :: 		Nodes.iActiveNode := 0;
	CLR	W0
	MOV	W0, _Nodes+3122
;NMRAnetNode.mpas,315 :: 		Nodes.AllocatedCount := 0;
	CLR	W0
	MOV	W0, _Nodes+3120
;NMRAnetNode.mpas,316 :: 		for i := 0 to MAX_NODE_COUNT - 1 do
; i start address is: 8 (W4)
	CLR	W4
; i end address is: 8 (W4)
L__NMRAnetNode_Initialize62:
;NMRAnetNode.mpas,318 :: 		NMRAnetStateMachine_InitializeNode(@Nodes.RawList[i], PhysicalNodeID_HI, PhysicalNodeID_LO + i);  // Physical Node + i MUST FIT IN THE LOWER 3 BYTES
; i start address is: 8 (W4)
	MOV	#46, W0
	MUL.UU	W0, W4, W2
	MOV	#lo_addr(_Nodes), W0
	ADD	W0, W2, W2
	MOV	W4, W0
	ASR	W0, #15, W1
	ADD	W12, W0, W0
	ADDC	W13, W1, W1
	PUSH	W4
	PUSH.D	W12
	PUSH.D	W10
	MOV	W11, W12
	MOV	W10, W11
	MOV	W2, W10
	PUSH.D	W0
	CALL	_NMRAnetStateMachine_InitializeNode
	SUB	#4, W15
	POP.D	W10
	POP.D	W12
	POP	W4
;NMRAnetNode.mpas,319 :: 		AppCallback_AssignConfigurationAddress(@Nodes.RawList[i], i);
	MOV	#46, W0
	MUL.UU	W0, W4, W2
	MOV	#lo_addr(_Nodes), W0
	ADD	W0, W2, W0
	PUSH	W4
	PUSH.D	W12
	PUSH.D	W10
	MOV	W4, W11
	MOV	W0, W10
	CALL	_AppCallback_AssignConfigurationAddress
	POP.D	W10
	POP.D	W12
	POP	W4
;NMRAnetNode.mpas,320 :: 		AppCallback_AssignRAMAddress(@Nodes.RawList[i], i);
	MOV	#46, W0
	MUL.UU	W0, W4, W2
	MOV	#lo_addr(_Nodes), W0
	ADD	W0, W2, W0
	PUSH	W4
	PUSH.D	W12
	PUSH.D	W10
	MOV	W4, W11
	MOV	W0, W10
	CALL	_AppCallback_AssignRAMAddress
	POP.D	W10
	POP.D	W12
	POP	W4
;NMRAnetNode.mpas,321 :: 		end;
	MOV	#64, W0
	CP	W4, W0
	BRA NZ	L__NMRAnetNode_Initialize246
	GOTO	L__NMRAnetNode_Initialize65
L__NMRAnetNode_Initialize246:
; i start address is: 8 (W4)
	INC	W4
; i end address is: 8 (W4)
; i end address is: 8 (W4)
	GOTO	L__NMRAnetNode_Initialize62
L__NMRAnetNode_Initialize65:
;NMRAnetNode.mpas,322 :: 		Node := NMRAnetNode_Allocate;                                                 // Allocate the Physical Node
	PUSH.D	W12
	PUSH.D	W10
	CALL	_NMRAnetNode_Allocate
	POP.D	W10
	POP.D	W12
;NMRAnetNode.mpas,323 :: 		Node^.State := Node^.State and not NS_VIRTUAL;
	MOV.B	[W0], W1
	ZE	W1, W2
	MOV	#247, W1
	AND	W2, W1, W1
	MOV.B	W1, [W0]
;NMRAnetNode.mpas,324 :: 		end;
L_end_NMRAnetNode_Initialize:
	RETURN
; end of _NMRAnetNode_Initialize

_NMRAnetNode_Allocate:
	LNK	#6

;NMRAnetNode.mpas,337 :: 		begin
;NMRAnetNode.mpas,338 :: 		Result := PNMRAnetNode( nil);
	PUSH	W10
	PUSH	W11
	PUSH	W12
	MOV	#0, W0
	MOV	W0, [W14+0]
;NMRAnetNode.mpas,339 :: 		if Nodes.AllocatedCount < MAX_NODE_COUNT then
	MOV	#65, W1
	MOV	#lo_addr(_Nodes+3120), W0
	CP	W1, [W0]
	BRA GT	L__NMRAnetNode_Allocate248
	GOTO	L__NMRAnetNode_Allocate68
L__NMRAnetNode_Allocate248:
;NMRAnetNode.mpas,341 :: 		Result := @Nodes.RawList[Nodes.AllocatedCount];
	MOV	#46, W1
	MOV	#lo_addr(_Nodes+3120), W0
	MUL.UU	W1, [W0], W2
	MOV	#lo_addr(_Nodes), W1
	ADD	W14, #0, W0
	ADD	W1, W2, [W0]
;NMRAnetNode.mpas,342 :: 		NMRAnetStateMachine_InitializeNode(Result, 0, 0);                           // The NodeID was already created in the initialization
	CLR	W11
	CLR	W12
	MOV	[W14+0], W10
	CLR	W0
	CLR	W1
	PUSH.D	W0
	CALL	_NMRAnetStateMachine_InitializeNode
	SUB	#4, W15
;NMRAnetNode.mpas,343 :: 		NMRAnetNode_SetStateFlag(Result, NS_ALLOCATED or NS_VIRTUAL);
	MOV.B	#9, W11
	MOV	[W14+0], W10
	CALL	_NMRAnetNode_SetStateFlag
;NMRAnetNode.mpas,344 :: 		Nodes.AllocatedList[Nodes.AllocatedCount] := Result;
	MOV	_Nodes+3120, W0
	SL	W0, #1, W1
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W1
	MOV	[W14+0], W0
	MOV	W0, [W1]
;NMRAnetNode.mpas,345 :: 		Result^.Info.AliasID := NMRAnetUtilities_CreateAliasID(Result^.Info.Seed, False); // Pregenerate it so it can be sorted
	MOV	[W14+0], W0
	INC2	W0
	ADD	W0, #16, W0
	MOV	W0, [W14+4]
	MOV	[W14+0], W0
	INC2	W0
	ADD	W0, #8, W0
	CLR	W11
	MOV	W0, W10
	CALL	_NMRAnetUtilities_CreateAliasID
	MOV	[W14+4], W1
	MOV	W0, [W1]
;NMRAnetNode.mpas,346 :: 		Inc(Nodes.AllocatedCount);
	MOV	_Nodes+3120, W0
	INC	W0
	MOV	W0, _Nodes+3120
;NMRAnetNode.mpas,347 :: 		NMRAnetNode_SortNodeList(Nodes);
	MOV	#lo_addr(_Nodes), W10
	CALL	_NMRAnetNode_SortNodeList
;NMRAnetNode.mpas,348 :: 		AppCallback_NodeAllocate(Result);
	MOV	[W14+0], W10
	CALL	_AppCallback_NodeAllocate
;NMRAnetNode.mpas,349 :: 		end;
L__NMRAnetNode_Allocate68:
;NMRAnetNode.mpas,350 :: 		end;
	MOV	[W14+0], W0
L_end_NMRAnetNode_Allocate:
	POP	W12
	POP	W11
	POP	W10
	ULNK
	RETURN
; end of _NMRAnetNode_Allocate

_NMRAnetNode_Release:
	LNK	#2

;NMRAnetNode.mpas,364 :: 		begin
;NMRAnetNode.mpas,365 :: 		if Node <> nil then
	CP	W10, #0
	BRA NZ	L__NMRAnetNode_Release250
	GOTO	L__NMRAnetNode_Release72
L__NMRAnetNode_Release250:
;NMRAnetNode.mpas,367 :: 		if Node^.State and NS_VIRTUAL = NS_VIRTUAL then                                     // Only release Virtual Nodes
	MOV.B	[W10], W0
	ZE	W0, W0
	AND	W0, #8, W0
	CP	W0, #8
	BRA Z	L__NMRAnetNode_Release251
	GOTO	L__NMRAnetNode_Release75
L__NMRAnetNode_Release251:
;NMRAnetNode.mpas,369 :: 		i := 0;
; i start address is: 4 (W2)
	CLR	W2
; i end address is: 4 (W2)
;NMRAnetNode.mpas,370 :: 		while i < Nodes.AllocatedCount do
L__NMRAnetNode_Release78:
; i start address is: 4 (W2)
	MOV	#lo_addr(_Nodes+3120), W0
	CP	W2, [W0]
	BRA LT	L__NMRAnetNode_Release252
	GOTO	L__NMRAnetNode_Release79
L__NMRAnetNode_Release252:
;NMRAnetNode.mpas,372 :: 		if Nodes.AllocatedList[i] = Node then
	SL	W2, #1, W1
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W0
	MOV	[W0], W0
	CP	W0, W10
	BRA Z	L__NMRAnetNode_Release253
	GOTO	L__NMRAnetNode_Release215
L__NMRAnetNode_Release253:
;NMRAnetNode.mpas,374 :: 		Nodes.AllocatedList[i] := PNMRAnetNode( nil);
	SL	W2, #1, W1
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W1
	MOV	#0, W0
	MOV	W0, [W1]
;NMRAnetNode.mpas,375 :: 		j := i;
	MOV	W2, [W14+0]
; i end address is: 4 (W2)
;NMRAnetNode.mpas,376 :: 		while j < Nodes.AllocatedCount - 1 do
L__NMRAnetNode_Release86:
	MOV	_Nodes+3120, W0
	SUB	W0, #1, W1
	ADD	W14, #0, W0
	CP	W1, [W0]
	BRA GT	L__NMRAnetNode_Release254
	GOTO	L__NMRAnetNode_Release87
L__NMRAnetNode_Release254:
;NMRAnetNode.mpas,378 :: 		Nodes.AllocatedList[j] := Nodes.AllocatedList[j + 1];
	MOV	[W14+0], W0
	SL	W0, #1, W1
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W2
	MOV	[W14+0], W0
	INC	W0
	SL	W0, #1, W1
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W0
	MOV	[W0], [W2]
;NMRAnetNode.mpas,379 :: 		Nodes.AllocatedList[j + 1] := PNMRAnetNode( nil);
	MOV	[W14+0], W0
	INC	W0
	SL	W0, #1, W1
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W1
	MOV	#0, W0
	MOV	W0, [W1]
;NMRAnetNode.mpas,380 :: 		Inc(j);
	MOV	#1, W1
	ADD	W14, #0, W0
	ADD	W1, [W0], [W0]
;NMRAnetNode.mpas,381 :: 		end;
	GOTO	L__NMRAnetNode_Release86
L__NMRAnetNode_Release87:
;NMRAnetNode.mpas,382 :: 		Dec(Nodes.AllocatedCount);
	MOV	_Nodes+3120, W0
	DEC	W0
	MOV	W0, _Nodes+3120
;NMRAnetNode.mpas,383 :: 		i := Nodes.AllocatedCount;                                                     // Done, break out
; i start address is: 0 (W0)
	MOV	_Nodes+3120, W0
; i end address is: 0 (W0)
;NMRAnetNode.mpas,384 :: 		end;
	GOTO	L__NMRAnetNode_Release83
L__NMRAnetNode_Release215:
;NMRAnetNode.mpas,372 :: 		if Nodes.AllocatedList[i] = Node then
	MOV	W2, W0
;NMRAnetNode.mpas,384 :: 		end;
L__NMRAnetNode_Release83:
;NMRAnetNode.mpas,385 :: 		Inc(i);
; i start address is: 0 (W0)
; i start address is: 4 (W2)
	ADD	W0, #1, W2
; i end address is: 0 (W0)
;NMRAnetNode.mpas,386 :: 		end;
; i end address is: 4 (W2)
	GOTO	L__NMRAnetNode_Release78
L__NMRAnetNode_Release79:
;NMRAnetNode.mpas,387 :: 		Node^.State := NS_EMPTY;                                                  // Do this last so item is not allocated in an interrupt half way through this
	CLR	W0
	MOV.B	W0, [W10]
;NMRAnetNode.mpas,388 :: 		end;
L__NMRAnetNode_Release75:
;NMRAnetNode.mpas,389 :: 		end;
L__NMRAnetNode_Release72:
;NMRAnetNode.mpas,390 :: 		end;
L_end_NMRAnetNode_Release:
	ULNK
	RETURN
; end of _NMRAnetNode_Release

_NMRAnetNode_SetMsgFlags:
	LNK	#2

;NMRAnetNode.mpas,406 :: 		begin
;NMRAnetNode.mpas,407 :: 		for i := 0 to Nodes.AllocatedCount - 1 do
; i start address is: 6 (W3)
	CLR	W3
; i end address is: 6 (W3)
L__NMRAnetNode_SetMsgFlags91:
; i start address is: 6 (W3)
	MOV	_Nodes+3120, W0
	SUB	W0, #1, W2
	CP	W3, W2
	BRA LE	L__NMRAnetNode_SetMsgFlags256
	GOTO	L__NMRAnetNode_SetMsgFlags95
L__NMRAnetNode_SetMsgFlags256:
;NMRAnetNode.mpas,409 :: 		Node := Nodes.AllocatedList[i];
	SL	W3, #1, W1
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W0
	MOV	[W0], W0
	MOV	W0, [W14+0]
;NMRAnetNode.mpas,410 :: 		if Node^.State and NS_PERMITTED = NS_PERMITTED then
	MOV.B	[W0], W0
	ZE	W0, W0
	AND	W0, #2, W0
	CP	W0, #2
	BRA Z	L__NMRAnetNode_SetMsgFlags257
	GOTO	L__NMRAnetNode_SetMsgFlags97
L__NMRAnetNode_SetMsgFlags257:
;NMRAnetNode.mpas,411 :: 		Node^.MsgFlags := Node^.MsgFlags or MsgFlags
	MOV	[W14+0], W0
	ADD	W0, #22, W1
	ZE	[W1], W0
	IOR	W0, W10, W0
	MOV.B	W0, [W1]
	GOTO	L__NMRAnetNode_SetMsgFlags98
;NMRAnetNode.mpas,412 :: 		else
L__NMRAnetNode_SetMsgFlags97:
;NMRAnetNode.mpas,413 :: 		Node^.MsgFlags := 0;
	MOV	[W14+0], W0
	ADD	W0, #22, W1
	CLR	W0
	MOV.B	W0, [W1]
L__NMRAnetNode_SetMsgFlags98:
;NMRAnetNode.mpas,414 :: 		end;
	CP	W3, W2
	BRA NZ	L__NMRAnetNode_SetMsgFlags258
	GOTO	L__NMRAnetNode_SetMsgFlags95
L__NMRAnetNode_SetMsgFlags258:
; i start address is: 6 (W3)
	INC	W3
; i end address is: 6 (W3)
; i end address is: 6 (W3)
	GOTO	L__NMRAnetNode_SetMsgFlags91
L__NMRAnetNode_SetMsgFlags95:
;NMRAnetNode.mpas,415 :: 		end;
L_end_NMRAnetNode_SetMsgFlags:
	ULNK
	RETURN
; end of _NMRAnetNode_SetMsgFlags

_NMRAnetNode_NextNode:

;NMRAnetNode.mpas,427 :: 		begin
;NMRAnetNode.mpas,428 :: 		LockCANInterrupt;
	CALL	_LockCANInterrupt
;NMRAnetNode.mpas,429 :: 		Result := PNMRAnetNode( nil);
; Result start address is: 2 (W1)
	MOV	#0, W1
;NMRAnetNode.mpas,430 :: 		if Nodes.AllocatedCount > 0 then
	MOV	_Nodes+3120, W0
	CP	W0, #0
	BRA GT	L__NMRAnetNode_NextNode260
	GOTO	L__NMRAnetNode_NextNode216
L__NMRAnetNode_NextNode260:
; Result end address is: 2 (W1)
;NMRAnetNode.mpas,432 :: 		if Nodes.iActiveNode > Nodes.AllocatedCount - 1 then
	MOV	_Nodes+3120, W0
	SUB	W0, #1, W1
	MOV	#lo_addr(_Nodes+3122), W0
	CP	W1, [W0]
	BRA LT	L__NMRAnetNode_NextNode261
	GOTO	L__NMRAnetNode_NextNode104
L__NMRAnetNode_NextNode261:
;NMRAnetNode.mpas,433 :: 		Nodes.iActiveNode := 0;
	CLR	W0
	MOV	W0, _Nodes+3122
L__NMRAnetNode_NextNode104:
;NMRAnetNode.mpas,434 :: 		Result := Nodes.AllocatedList[Nodes.iActiveNode];
	MOV	_Nodes+3122, W0
	SL	W0, #1, W1
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W0
; Result start address is: 2 (W1)
	MOV	[W0], W1
;NMRAnetNode.mpas,435 :: 		Inc(Nodes.iActiveNode);
	MOV	_Nodes+3122, W0
	INC	W0
	MOV	W0, _Nodes+3122
; Result end address is: 2 (W1)
;NMRAnetNode.mpas,436 :: 		end;
	GOTO	L__NMRAnetNode_NextNode101
L__NMRAnetNode_NextNode216:
;NMRAnetNode.mpas,430 :: 		if Nodes.AllocatedCount > 0 then
;NMRAnetNode.mpas,436 :: 		end;
L__NMRAnetNode_NextNode101:
;NMRAnetNode.mpas,437 :: 		UnLockCANInterrupt;
; Result start address is: 2 (W1)
	PUSH	W1
	CALL	_UnLockCANInterrupt
	POP	W1
;NMRAnetNode.mpas,438 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetNode_NextNode:
	RETURN
; end of _NMRAnetNode_NextNode

_NMRAnetNode_SetStateFlag:

;NMRAnetNode.mpas,450 :: 		begin
;NMRAnetNode.mpas,451 :: 		Node^.State := Node^.State or Flag;
	MOV.B	[W10], W0
	ZE	W0, W1
	ZE	W11, W0
	IOR	W1, W0, W0
	MOV.B	W0, [W10]
;NMRAnetNode.mpas,452 :: 		end;
L_end_NMRAnetNode_SetStateFlag:
	RETURN
; end of _NMRAnetNode_SetStateFlag

_NMRAnetNode_ClearStateFlag:

;NMRAnetNode.mpas,464 :: 		begin
;NMRAnetNode.mpas,465 :: 		Node^.State := Node^.State and not Flag;
	MOV.B	W11, W0
	COM.B	W0, W2
	MOV.B	[W10], W0
	ZE	W0, W1
	ZE	W2, W0
	AND	W1, W0, W0
	MOV.B	W0, [W10]
;NMRAnetNode.mpas,466 :: 		end;
L_end_NMRAnetNode_ClearStateFlag:
	RETURN
; end of _NMRAnetNode_ClearStateFlag

_NMRAnetNode_TestStateFlag:

;NMRAnetNode.mpas,478 :: 		begin
;NMRAnetNode.mpas,479 :: 		Result := Node^.State and Flag = Flag;
	MOV.B	[W10], W0
	ZE	W0, W1
	ZE	W11, W0
	AND	W1, W0, W1
	ZE	W11, W0
; Result start address is: 2 (W1)
	CP	W1, W0
	CLR	W1
	BRA NZ	L__NMRAnetNode_TestStateFlag265
	COM	W1
L__NMRAnetNode_TestStateFlag265:
;NMRAnetNode.mpas,480 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetNode_TestStateFlag:
	RETURN
; end of _NMRAnetNode_TestStateFlag

_NMRAnetNode_SetMsgFlag:

;NMRAnetNode.mpas,492 :: 		begin
;NMRAnetNode.mpas,493 :: 		Node^.MsgFlags := Node^.MsgFlags or Flag;
	ADD	W10, #22, W2
	ZE	[W2], W1
	ZE	W11, W0
	IOR	W1, W0, W0
	MOV.B	W0, [W2]
;NMRAnetNode.mpas,494 :: 		end;
L_end_NMRAnetNode_SetMsgFlag:
	RETURN
; end of _NMRAnetNode_SetMsgFlag

_NMRAnetNode_ClearMsgFlag:

;NMRAnetNode.mpas,506 :: 		begin
;NMRAnetNode.mpas,507 :: 		Node^.MsgFlags := Node^.MsgFlags and not Flag;
	ADD	W10, #22, W2
	MOV.B	W11, W0
	COM.B	W0
	ZE	[W2], W1
	ZE	W0, W0
	AND	W1, W0, W0
	MOV.B	W0, [W2]
;NMRAnetNode.mpas,508 :: 		end;
L_end_NMRAnetNode_ClearMsgFlag:
	RETURN
; end of _NMRAnetNode_ClearMsgFlag

_NMRAnetNode_ClearMsgFlags:

;NMRAnetNode.mpas,520 :: 		begin
;NMRAnetNode.mpas,521 :: 		Node^.MsgFlags := 0;
	ADD	W10, #22, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetNode.mpas,522 :: 		end;
L_end_NMRAnetNode_ClearMsgFlags:
	RETURN
; end of _NMRAnetNode_ClearMsgFlags

_NMRAnetNode_TestMsgFlags:

;NMRAnetNode.mpas,533 :: 		begin
;NMRAnetNode.mpas,534 :: 		Result := Node^.MsgFlags and Flag = Flag;
	ADD	W10, #22, W0
	ZE	[W0], W0
	AND	W0, W11, W0
; Result start address is: 6 (W3)
	CP	W0, W11
	CLR	W3
	BRA NZ	L__NMRAnetNode_TestMsgFlags270
	COM	W3
L__NMRAnetNode_TestMsgFlags270:
;NMRAnetNode.mpas,535 :: 		if DoClear then
	CP0	W12
	BRA NZ	L__NMRAnetNode_TestMsgFlags271
	GOTO	L__NMRAnetNode_TestMsgFlags114
L__NMRAnetNode_TestMsgFlags271:
;NMRAnetNode.mpas,536 :: 		NMRAnetNode_ClearMsgFlag(Node, Flag);
	CALL	_NMRAnetNode_ClearMsgFlag
L__NMRAnetNode_TestMsgFlags114:
;NMRAnetNode.mpas,537 :: 		end;
	MOV	W3, W0
; Result end address is: 6 (W3)
L_end_NMRAnetNode_TestMsgFlags:
	RETURN
; end of _NMRAnetNode_TestMsgFlags

_NMRAnetNode_SetProducerEventFlags:

;NMRAnetNode.mpas,553 :: 		begin
;NMRAnetNode.mpas,556 :: 		if NMRAnetNode_TestStateFlag(Node, NS_VIRTUAL) then
	PUSH	W12
	PUSH	W11
	MOV.B	#8, W11
	CALL	_NMRAnetNode_TestStateFlag
	POP	W11
	CP0	W0
	BRA NZ	L__NMRAnetNode_SetProducerEventFlags273
	GOTO	L__NMRAnetNode_SetProducerEventFlags118
L__NMRAnetNode_SetProducerEventFlags273:
;NMRAnetNode.mpas,558 :: 		for i := 0 to MAX_VNODE_SUPPORTED_EVENTS_PRODUCED - 1 do
; i start address is: 0 (W0)
	CLR	W0
; i end address is: 0 (W0)
	MOV	W0, W6
L__NMRAnetNode_SetProducerEventFlags121:
;NMRAnetNode.mpas,559 :: 		NMRAnetNode_SetProducerEventFlag(Node, i, State);
; i start address is: 12 (W6)
	PUSH	W11
	MOV.B	W11, W12
	MOV	W6, W11
	CALL	_NMRAnetNode_SetProducerEventFlag
	POP	W11
	CP	W6, #2
	BRA NZ	L__NMRAnetNode_SetProducerEventFlags274
	GOTO	L__NMRAnetNode_SetProducerEventFlags124
L__NMRAnetNode_SetProducerEventFlags274:
; i start address is: 12 (W6)
	INC	W6
; i end address is: 12 (W6)
; i end address is: 12 (W6)
	GOTO	L__NMRAnetNode_SetProducerEventFlags121
L__NMRAnetNode_SetProducerEventFlags124:
;NMRAnetNode.mpas,560 :: 		end else
	GOTO	L__NMRAnetNode_SetProducerEventFlags119
L__NMRAnetNode_SetProducerEventFlags118:
;NMRAnetNode.mpas,563 :: 		for i := 0 to MAX_SUPPORTED_EVENTS_PRODUCED - 1 do
; i start address is: 0 (W0)
	CLR	W0
; i end address is: 0 (W0)
	MOV	W0, W6
L__NMRAnetNode_SetProducerEventFlags126:
;NMRAnetNode.mpas,564 :: 		NMRAnetNode_SetProducerEventFlag(Node, i, State);
; i start address is: 12 (W6)
	PUSH	W11
	MOV.B	W11, W12
	MOV	W6, W11
	CALL	_NMRAnetNode_SetProducerEventFlag
	POP	W11
	CP	W6, #1
	BRA NZ	L__NMRAnetNode_SetProducerEventFlags275
	GOTO	L__NMRAnetNode_SetProducerEventFlags129
L__NMRAnetNode_SetProducerEventFlags275:
; i start address is: 12 (W6)
	INC	W6
; i end address is: 12 (W6)
; i end address is: 12 (W6)
	GOTO	L__NMRAnetNode_SetProducerEventFlags126
L__NMRAnetNode_SetProducerEventFlags129:
;NMRAnetNode.mpas,565 :: 		end
L__NMRAnetNode_SetProducerEventFlags119:
;NMRAnetNode.mpas,567 :: 		end;
L_end_NMRAnetNode_SetProducerEventFlags:
	POP	W12
	RETURN
; end of _NMRAnetNode_SetProducerEventFlags

_NMRAnetNode_SetProducerEventFlag:

;NMRAnetNode.mpas,583 :: 		begin
;NMRAnetNode.mpas,585 :: 		ByteOffset := EventIndex div 4;    // There are 4 Events supported in each Byte
	ASR	W11, #2, W0
; ByteOffset start address is: 8 (W4)
	MOV	W0, W4
;NMRAnetNode.mpas,586 :: 		EventOffset := EventIndex mod 4;   // There are
	MOV	#4, W2
	REPEAT	#17
	DIV.S	W11, W2
	MOV	W1, W0
; EventOffset start address is: 10 (W5)
	MOV	W0, W5
;NMRAnetNode.mpas,587 :: 		Mask := %00000011;
; Mask start address is: 4 (W2)
	MOV.B	#3, W2
;NMRAnetNode.mpas,588 :: 		Mask := Mask shl (EventOffset * 2);  // 2 bits per event
	SL	W0, #1, W1
	ZE	W2, W0
; Mask end address is: 4 (W2)
	SL	W0, W1, W0
;NMRAnetNode.mpas,589 :: 		Mask := not Mask;
	COM.B	W0, W3
;NMRAnetNode.mpas,590 :: 		Node^.EventsProducedFlags[ByteOffset] := Node^.EventsProducedFlags[ByteOffset] and Mask;  // Clear it
	ADD	W10, #23, W0
	ADD	W0, W4, W2
	ZE	[W2], W1
	ZE	W3, W0
	AND	W1, W0, W0
	MOV.B	W0, [W2]
;NMRAnetNode.mpas,591 :: 		Mask := State shl (EventOffset * 2);
	SL	W5, #1, W1
; EventOffset end address is: 10 (W5)
	ZE	W12, W0
	SL	W0, W1, W3
;NMRAnetNode.mpas,592 :: 		Node^.EventsProducedFlags[ByteOffset] := Node^.EventsProducedFlags[ByteOffset] or Mask;  // Set it to the State
	ADD	W10, #23, W0
	ADD	W0, W4, W2
; ByteOffset end address is: 8 (W4)
	ZE	[W2], W1
	ZE	W3, W0
	IOR	W1, W0, W0
	MOV.B	W0, [W2]
;NMRAnetNode.mpas,594 :: 		end;
L_end_NMRAnetNode_SetProducerEventFlag:
	RETURN
; end of _NMRAnetNode_SetProducerEventFlag

_NMRAnetNode_ClearProducerEventFlags:

;NMRAnetNode.mpas,609 :: 		begin
;NMRAnetNode.mpas,611 :: 		for i := 0 to MAX_EVENTS_PRODUCED_BIT_BYTES - 1 do                          // Shortcut to clear all
; i start address is: 4 (W2)
	CLR	W2
; i end address is: 4 (W2)
L__NMRAnetNode_ClearProducerEventFlags133:
;NMRAnetNode.mpas,612 :: 		Node^.EventsProducedFlags[i] := 0;
; i start address is: 4 (W2)
	ADD	W10, #23, W0
	ADD	W0, W2, W1
	CLR	W0
	MOV.B	W0, [W1]
	CP	W2, #0
	BRA NZ	L__NMRAnetNode_ClearProducerEventFlags278
	GOTO	L__NMRAnetNode_ClearProducerEventFlags136
L__NMRAnetNode_ClearProducerEventFlags278:
; i start address is: 4 (W2)
	INC	W2
; i end address is: 4 (W2)
; i end address is: 4 (W2)
	GOTO	L__NMRAnetNode_ClearProducerEventFlags133
L__NMRAnetNode_ClearProducerEventFlags136:
;NMRAnetNode.mpas,614 :: 		end;
L_end_NMRAnetNode_ClearProducerEventFlags:
	RETURN
; end of _NMRAnetNode_ClearProducerEventFlags

_NMRAnetNode_NextProducerEventFlag:
	LNK	#4

;NMRAnetNode.mpas,630 :: 		begin
;NMRAnetNode.mpas,631 :: 		Result := -1;
	PUSH	W12
; Result start address is: 4 (W2)
	MOV	#65535, W2
;NMRAnetNode.mpas,633 :: 		for i := 0 to MAX_EVENTS_PRODUCED_BIT_BYTES - 1 do
; i start address is: 2 (W1)
	CLR	W1
; Result end address is: 4 (W2)
; i end address is: 2 (W1)
L__NMRAnetNode_NextProducerEventFlag139:
;NMRAnetNode.mpas,635 :: 		if Node^.EventsProducedFlags[i] <> 0 then
; i start address is: 2 (W1)
; Result start address is: 4 (W2)
	ADD	W10, #23, W0
	ADD	W0, W1, W0
	MOV.B	[W0], W0
	CP.B	W0, #0
	BRA NZ	L__NMRAnetNode_NextProducerEventFlag280
	GOTO	L__NMRAnetNode_NextProducerEventFlag217
L__NMRAnetNode_NextProducerEventFlag280:
;NMRAnetNode.mpas,637 :: 		Temp := Node^.EventsProducedFlags[i];
	ADD	W10, #23, W0
	ADD	W0, W1, W0
	MOV.B	[W0], W0
	MOV.B	W0, [W14+2]
;NMRAnetNode.mpas,638 :: 		for j := 0 to 3 do                                                      // Find the first non zero state in the byte
	CLR	W0
	MOV	W0, [W14+0]
; i end address is: 2 (W1)
	MOV	W1, W3
L__NMRAnetNode_NextProducerEventFlag147:
;NMRAnetNode.mpas,640 :: 		State := Temp and $03;
; i start address is: 6 (W3)
; Result start address is: 4 (W2)
; Result end address is: 4 (W2)
; i start address is: 6 (W3)
; i end address is: 6 (W3)
	ADD	W14, #2, W0
	ZE	[W0], W0
	AND	W0, #3, W0
	MOV.B	W0, [W11]
;NMRAnetNode.mpas,641 :: 		if State <> 0 then
	MOV.B	[W11], W0
	CP.B	W0, #0
	BRA NZ	L__NMRAnetNode_NextProducerEventFlag281
	GOTO	L__NMRAnetNode_NextProducerEventFlag152
L__NMRAnetNode_NextProducerEventFlag281:
; Result end address is: 4 (W2)
; i end address is: 6 (W3)
;NMRAnetNode.mpas,643 :: 		Result := (i*4) + j;
; i start address is: 6 (W3)
	SL	W3, #2, W1
; i end address is: 6 (W3)
	ADD	W14, #0, W0
; Result start address is: 12 (W6)
	ADD	W1, [W0], W6
;NMRAnetNode.mpas,644 :: 		NMRAnetNode_SetProducerEventFlag(Node, Result, EVENT_STATE_CLEAR); // Clear the flag
	PUSH	W11
	CLR	W12
	MOV	W6, W11
	CALL	_NMRAnetNode_SetProducerEventFlag
	POP	W11
;NMRAnetNode.mpas,645 :: 		Exit;
	MOV	W6, W1
; Result end address is: 12 (W6)
	GOTO	L_end__NMRAnetNode_NextProducerEventFlag
;NMRAnetNode.mpas,646 :: 		end else
L__NMRAnetNode_NextProducerEventFlag152:
;NMRAnetNode.mpas,647 :: 		Temp := Temp shr 2;
; i start address is: 6 (W3)
; Result start address is: 4 (W2)
	ADD	W14, #2, W0
	ZE	[W0], W0
	LSR	W0, #2, W0
	MOV.B	W0, [W14+2]
;NMRAnetNode.mpas,648 :: 		end
	MOV	[W14+0], W0
	CP	W0, #3
	BRA NZ	L__NMRAnetNode_NextProducerEventFlag282
	GOTO	L__NMRAnetNode_NextProducerEventFlag150
L__NMRAnetNode_NextProducerEventFlag282:
	MOV	#1, W1
	ADD	W14, #0, W0
	ADD	W1, [W0], [W0]
	GOTO	L__NMRAnetNode_NextProducerEventFlag147
L__NMRAnetNode_NextProducerEventFlag150:
;NMRAnetNode.mpas,649 :: 		end
	MOV	W3, W1
; i end address is: 6 (W3)
	MOV	W2, W0
	GOTO	L__NMRAnetNode_NextProducerEventFlag144
; Result end address is: 4 (W2)
L__NMRAnetNode_NextProducerEventFlag217:
;NMRAnetNode.mpas,635 :: 		if Node^.EventsProducedFlags[i] <> 0 then
	MOV	W2, W0
;NMRAnetNode.mpas,649 :: 		end
L__NMRAnetNode_NextProducerEventFlag144:
;NMRAnetNode.mpas,650 :: 		end
; Result start address is: 0 (W0)
; i start address is: 2 (W1)
	CP	W1, #0
	BRA NZ	L__NMRAnetNode_NextProducerEventFlag283
	GOTO	L__NMRAnetNode_NextProducerEventFlag142
L__NMRAnetNode_NextProducerEventFlag283:
; i start address is: 2 (W1)
	INC	W1
; i end address is: 2 (W1)
	MOV	W0, W2
; i end address is: 2 (W1)
	GOTO	L__NMRAnetNode_NextProducerEventFlag139
L__NMRAnetNode_NextProducerEventFlag142:
;NMRAnetNode.mpas,652 :: 		end;
	MOV	W0, W1
L_end__NMRAnetNode_NextProducerEventFlag:
; Result end address is: 0 (W0)
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetNode_NextProducerEventFlag:
	POP	W12
	ULNK
	RETURN
; end of _NMRAnetNode_NextProducerEventFlag

_NMRAnetNode_IsAnyProducerEventSet:

;NMRAnetNode.mpas,667 :: 		begin
;NMRAnetNode.mpas,668 :: 		Result := False;
; Result start address is: 2 (W1)
	CLR	W1
;NMRAnetNode.mpas,670 :: 		for i := 0 to MAX_EVENTS_PRODUCED_BIT_BYTES - 1 do                          // Shortcut to see if any bits are set in any event
; i start address is: 0 (W0)
	CLR	W0
; i end address is: 0 (W0)
	MOV	W0, W2
L__NMRAnetNode_IsAnyProducerEventSet156:
;NMRAnetNode.mpas,672 :: 		if Node^.EventsProducedFlags[i] <> 0 then
; i start address is: 4 (W2)
; Result start address is: 2 (W1)
; Result end address is: 2 (W1)
	ADD	W10, #23, W0
	ADD	W0, W2, W0
	MOV.B	[W0], W0
	CP.B	W0, #0
	BRA NZ	L__NMRAnetNode_IsAnyProducerEventSet285
	GOTO	L__NMRAnetNode_IsAnyProducerEventSet161
L__NMRAnetNode_IsAnyProducerEventSet285:
; i end address is: 4 (W2)
; Result end address is: 2 (W1)
;NMRAnetNode.mpas,674 :: 		Result := True;
; Result start address is: 2 (W1)
	MOV	#65535, W1
;NMRAnetNode.mpas,675 :: 		Exit
	GOTO	L_end__NMRAnetNode_IsAnyProducerEventSet
;NMRAnetNode.mpas,676 :: 		end
L__NMRAnetNode_IsAnyProducerEventSet161:
;NMRAnetNode.mpas,677 :: 		end
; i start address is: 4 (W2)
	CP	W2, #0
	BRA NZ	L__NMRAnetNode_IsAnyProducerEventSet286
	GOTO	L__NMRAnetNode_IsAnyProducerEventSet159
L__NMRAnetNode_IsAnyProducerEventSet286:
; i start address is: 4 (W2)
	INC	W2
; i end address is: 4 (W2)
; i end address is: 4 (W2)
	GOTO	L__NMRAnetNode_IsAnyProducerEventSet156
L__NMRAnetNode_IsAnyProducerEventSet159:
;NMRAnetNode.mpas,679 :: 		end;
L_end__NMRAnetNode_IsAnyProducerEventSet:
; Result end address is: 2 (W1)
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetNode_IsAnyProducerEventSet:
	RETURN
; end of _NMRAnetNode_IsAnyProducerEventSet

_NMRAnetNode_SetConsumerEventFlags:

;NMRAnetNode.mpas,694 :: 		begin
;NMRAnetNode.mpas,697 :: 		if NMRAnetNode_TestStateFlag(Node, NS_VIRTUAL) then
	PUSH	W12
	PUSH	W11
	MOV.B	#8, W11
	CALL	_NMRAnetNode_TestStateFlag
	POP	W11
	CP0	W0
	BRA NZ	L__NMRAnetNode_SetConsumerEventFlags288
	GOTO	L__NMRAnetNode_SetConsumerEventFlags165
L__NMRAnetNode_SetConsumerEventFlags288:
;NMRAnetNode.mpas,699 :: 		for i := 0 to MAX_VNODE_SUPPORTED_EVENTS_CONSUMED - 1 do
; i start address is: 0 (W0)
	CLR	W0
; i end address is: 0 (W0)
	MOV	W0, W6
L__NMRAnetNode_SetConsumerEventFlags168:
;NMRAnetNode.mpas,700 :: 		NMRAnetNode_SetConsumerEventFlag(Node, i, State);
; i start address is: 12 (W6)
	PUSH	W11
	MOV.B	W11, W12
	MOV	W6, W11
	CALL	_NMRAnetNode_SetConsumerEventFlag
	POP	W11
	CP	W6, #0
	BRA NZ	L__NMRAnetNode_SetConsumerEventFlags289
	GOTO	L__NMRAnetNode_SetConsumerEventFlags171
L__NMRAnetNode_SetConsumerEventFlags289:
; i start address is: 12 (W6)
	INC	W6
; i end address is: 12 (W6)
; i end address is: 12 (W6)
	GOTO	L__NMRAnetNode_SetConsumerEventFlags168
L__NMRAnetNode_SetConsumerEventFlags171:
;NMRAnetNode.mpas,701 :: 		end else
	GOTO	L__NMRAnetNode_SetConsumerEventFlags166
L__NMRAnetNode_SetConsumerEventFlags165:
;NMRAnetNode.mpas,704 :: 		for i := 0 to MAX_SUPPORTED_EVENTS_CONSUMED - 1 do
; i start address is: 0 (W0)
	CLR	W0
; i end address is: 0 (W0)
	MOV	W0, W6
L__NMRAnetNode_SetConsumerEventFlags173:
;NMRAnetNode.mpas,705 :: 		NMRAnetNode_SetConsumerEventFlag(Node, i, State);
; i start address is: 12 (W6)
	PUSH	W11
	MOV.B	W11, W12
	MOV	W6, W11
	CALL	_NMRAnetNode_SetConsumerEventFlag
	POP	W11
	CP	W6, #0
	BRA NZ	L__NMRAnetNode_SetConsumerEventFlags290
	GOTO	L__NMRAnetNode_SetConsumerEventFlags176
L__NMRAnetNode_SetConsumerEventFlags290:
; i start address is: 12 (W6)
	INC	W6
; i end address is: 12 (W6)
; i end address is: 12 (W6)
	GOTO	L__NMRAnetNode_SetConsumerEventFlags173
L__NMRAnetNode_SetConsumerEventFlags176:
;NMRAnetNode.mpas,706 :: 		end
L__NMRAnetNode_SetConsumerEventFlags166:
;NMRAnetNode.mpas,708 :: 		end;
L_end_NMRAnetNode_SetConsumerEventFlags:
	POP	W12
	RETURN
; end of _NMRAnetNode_SetConsumerEventFlags

_NMRAnetNode_SetConsumerEventFlag:

;NMRAnetNode.mpas,724 :: 		begin
;NMRAnetNode.mpas,726 :: 		ByteOffset := EventIndex div 4;    // There are 4 Events supported in each Byte
	ASR	W11, #2, W0
; ByteOffset start address is: 8 (W4)
	MOV	W0, W4
;NMRAnetNode.mpas,727 :: 		EventOffset := EventIndex mod 4;   // There are
	MOV	#4, W2
	REPEAT	#17
	DIV.S	W11, W2
	MOV	W1, W0
; EventOffset start address is: 10 (W5)
	MOV	W0, W5
;NMRAnetNode.mpas,728 :: 		Mask := %00000011;
; Mask start address is: 4 (W2)
	MOV.B	#3, W2
;NMRAnetNode.mpas,729 :: 		Mask := Mask shl (EventOffset * 2);  // 2 bits per event
	SL	W0, #1, W1
	ZE	W2, W0
; Mask end address is: 4 (W2)
	SL	W0, W1, W0
;NMRAnetNode.mpas,730 :: 		Mask := not Mask;
	COM.B	W0, W3
;NMRAnetNode.mpas,731 :: 		Node^.EventsConsumedFlags[ByteOffset] := Node^.EventsConsumedFlags[ByteOffset] and Mask;
	ADD	W10, #24, W0
	ADD	W0, W4, W2
	ZE	[W2], W1
	ZE	W3, W0
	AND	W1, W0, W0
	MOV.B	W0, [W2]
;NMRAnetNode.mpas,732 :: 		Mask := State shl (EventOffset * 2);
	SL	W5, #1, W1
; EventOffset end address is: 10 (W5)
	ZE	W12, W0
	SL	W0, W1, W3
;NMRAnetNode.mpas,733 :: 		Node^.EventsConsumedFlags[ByteOffset] := Node^.EventsConsumedFlags[ByteOffset] or Mask;
	ADD	W10, #24, W0
	ADD	W0, W4, W2
; ByteOffset end address is: 8 (W4)
	ZE	[W2], W1
	ZE	W3, W0
	IOR	W1, W0, W0
	MOV.B	W0, [W2]
;NMRAnetNode.mpas,735 :: 		end;
L_end_NMRAnetNode_SetConsumerEventFlag:
	RETURN
; end of _NMRAnetNode_SetConsumerEventFlag

_NMRAnetNode_ClearConsumerEventFlags:

;NMRAnetNode.mpas,750 :: 		begin
;NMRAnetNode.mpas,752 :: 		for i := 0 to MAX_EVENTS_CONSUMED_BIT_BYTES - 1 do                          // Shortcut to clear them all
; i start address is: 4 (W2)
	CLR	W2
; i end address is: 4 (W2)
L__NMRAnetNode_ClearConsumerEventFlags180:
;NMRAnetNode.mpas,753 :: 		Node^.EventsConsumedFlags[i] := 0;
; i start address is: 4 (W2)
	ADD	W10, #24, W0
	ADD	W0, W2, W1
	CLR	W0
	MOV.B	W0, [W1]
	CP	W2, #0
	BRA NZ	L__NMRAnetNode_ClearConsumerEventFlags293
	GOTO	L__NMRAnetNode_ClearConsumerEventFlags183
L__NMRAnetNode_ClearConsumerEventFlags293:
; i start address is: 4 (W2)
	INC	W2
; i end address is: 4 (W2)
; i end address is: 4 (W2)
	GOTO	L__NMRAnetNode_ClearConsumerEventFlags180
L__NMRAnetNode_ClearConsumerEventFlags183:
;NMRAnetNode.mpas,755 :: 		end;
L_end_NMRAnetNode_ClearConsumerEventFlags:
	RETURN
; end of _NMRAnetNode_ClearConsumerEventFlags

_NMRAnetNode_NextConsumerEventFlag:
	LNK	#4

;NMRAnetNode.mpas,771 :: 		begin
;NMRAnetNode.mpas,772 :: 		Result := -1;
	PUSH	W12
; Result start address is: 4 (W2)
	MOV	#65535, W2
;NMRAnetNode.mpas,774 :: 		for i := 0 to MAX_EVENTS_CONSUMED_BIT_BYTES - 1 do
; i start address is: 2 (W1)
	CLR	W1
; Result end address is: 4 (W2)
; i end address is: 2 (W1)
L__NMRAnetNode_NextConsumerEventFlag186:
;NMRAnetNode.mpas,776 :: 		if Node^.EventsConsumedFlags[i] <> 0 then
; i start address is: 2 (W1)
; Result start address is: 4 (W2)
	ADD	W10, #24, W0
	ADD	W0, W1, W0
	MOV.B	[W0], W0
	CP.B	W0, #0
	BRA NZ	L__NMRAnetNode_NextConsumerEventFlag295
	GOTO	L__NMRAnetNode_NextConsumerEventFlag218
L__NMRAnetNode_NextConsumerEventFlag295:
;NMRAnetNode.mpas,778 :: 		Temp := Node^.EventsConsumedFlags[i];
	ADD	W10, #24, W0
	ADD	W0, W1, W0
	MOV.B	[W0], W0
	MOV.B	W0, [W14+2]
;NMRAnetNode.mpas,779 :: 		for j := 0 to 3 do                                                      // Find the first non zero state in the byte
	CLR	W0
	MOV	W0, [W14+0]
; i end address is: 2 (W1)
	MOV	W1, W3
L__NMRAnetNode_NextConsumerEventFlag194:
;NMRAnetNode.mpas,781 :: 		State := Temp and $03;
; i start address is: 6 (W3)
; Result start address is: 4 (W2)
; Result end address is: 4 (W2)
; i start address is: 6 (W3)
; i end address is: 6 (W3)
	ADD	W14, #2, W0
	ZE	[W0], W0
	AND	W0, #3, W0
	MOV.B	W0, [W11]
;NMRAnetNode.mpas,782 :: 		if State <> 0 then
	MOV.B	[W11], W0
	CP.B	W0, #0
	BRA NZ	L__NMRAnetNode_NextConsumerEventFlag296
	GOTO	L__NMRAnetNode_NextConsumerEventFlag199
L__NMRAnetNode_NextConsumerEventFlag296:
; Result end address is: 4 (W2)
; i end address is: 6 (W3)
;NMRAnetNode.mpas,784 :: 		Result := (i*4) + j;
; i start address is: 6 (W3)
	SL	W3, #2, W1
; i end address is: 6 (W3)
	ADD	W14, #0, W0
; Result start address is: 12 (W6)
	ADD	W1, [W0], W6
;NMRAnetNode.mpas,785 :: 		NMRAnetNode_SetConsumerEventFlag(Node, Result, EVENT_STATE_CLEAR); // Clear the flag
	PUSH	W11
	CLR	W12
	MOV	W6, W11
	CALL	_NMRAnetNode_SetConsumerEventFlag
	POP	W11
;NMRAnetNode.mpas,786 :: 		Exit;
	MOV	W6, W1
; Result end address is: 12 (W6)
	GOTO	L_end__NMRAnetNode_NextConsumerEventFlag
;NMRAnetNode.mpas,787 :: 		end else
L__NMRAnetNode_NextConsumerEventFlag199:
;NMRAnetNode.mpas,788 :: 		Temp := Temp shr 2;
; i start address is: 6 (W3)
; Result start address is: 4 (W2)
	ADD	W14, #2, W0
	ZE	[W0], W0
	LSR	W0, #2, W0
	MOV.B	W0, [W14+2]
;NMRAnetNode.mpas,789 :: 		end
	MOV	[W14+0], W0
	CP	W0, #3
	BRA NZ	L__NMRAnetNode_NextConsumerEventFlag297
	GOTO	L__NMRAnetNode_NextConsumerEventFlag197
L__NMRAnetNode_NextConsumerEventFlag297:
	MOV	#1, W1
	ADD	W14, #0, W0
	ADD	W1, [W0], [W0]
	GOTO	L__NMRAnetNode_NextConsumerEventFlag194
L__NMRAnetNode_NextConsumerEventFlag197:
;NMRAnetNode.mpas,790 :: 		end
	MOV	W3, W1
; i end address is: 6 (W3)
	MOV	W2, W0
	GOTO	L__NMRAnetNode_NextConsumerEventFlag191
; Result end address is: 4 (W2)
L__NMRAnetNode_NextConsumerEventFlag218:
;NMRAnetNode.mpas,776 :: 		if Node^.EventsConsumedFlags[i] <> 0 then
	MOV	W2, W0
;NMRAnetNode.mpas,790 :: 		end
L__NMRAnetNode_NextConsumerEventFlag191:
;NMRAnetNode.mpas,791 :: 		end
; Result start address is: 0 (W0)
; i start address is: 2 (W1)
	CP	W1, #0
	BRA NZ	L__NMRAnetNode_NextConsumerEventFlag298
	GOTO	L__NMRAnetNode_NextConsumerEventFlag189
L__NMRAnetNode_NextConsumerEventFlag298:
; i start address is: 2 (W1)
	INC	W1
; i end address is: 2 (W1)
	MOV	W0, W2
; i end address is: 2 (W1)
	GOTO	L__NMRAnetNode_NextConsumerEventFlag186
L__NMRAnetNode_NextConsumerEventFlag189:
;NMRAnetNode.mpas,793 :: 		end;
	MOV	W0, W1
L_end__NMRAnetNode_NextConsumerEventFlag:
; Result end address is: 0 (W0)
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetNode_NextConsumerEventFlag:
	POP	W12
	ULNK
	RETURN
; end of _NMRAnetNode_NextConsumerEventFlag

_NMRAnetNode_IsAnyConsumerEventSet:

;NMRAnetNode.mpas,808 :: 		begin
;NMRAnetNode.mpas,809 :: 		Result := False;
; Result start address is: 2 (W1)
	CLR	W1
;NMRAnetNode.mpas,811 :: 		for i := 0 to MAX_EVENTS_CONSUMED_BIT_BYTES - 1 do
; i start address is: 0 (W0)
	CLR	W0
; i end address is: 0 (W0)
	MOV	W0, W2
L__NMRAnetNode_IsAnyConsumerEventSet203:
;NMRAnetNode.mpas,813 :: 		if Node^.EventsConsumedFlags[i] <> 0 then
; i start address is: 4 (W2)
; Result start address is: 2 (W1)
; Result end address is: 2 (W1)
	ADD	W10, #24, W0
	ADD	W0, W2, W0
	MOV.B	[W0], W0
	CP.B	W0, #0
	BRA NZ	L__NMRAnetNode_IsAnyConsumerEventSet300
	GOTO	L__NMRAnetNode_IsAnyConsumerEventSet208
L__NMRAnetNode_IsAnyConsumerEventSet300:
; i end address is: 4 (W2)
; Result end address is: 2 (W1)
;NMRAnetNode.mpas,815 :: 		Result := True;
; Result start address is: 2 (W1)
	MOV	#65535, W1
;NMRAnetNode.mpas,816 :: 		Exit
	GOTO	L_end__NMRAnetNode_IsAnyConsumerEventSet
;NMRAnetNode.mpas,817 :: 		end
L__NMRAnetNode_IsAnyConsumerEventSet208:
;NMRAnetNode.mpas,818 :: 		end
; i start address is: 4 (W2)
	CP	W2, #0
	BRA NZ	L__NMRAnetNode_IsAnyConsumerEventSet301
	GOTO	L__NMRAnetNode_IsAnyConsumerEventSet206
L__NMRAnetNode_IsAnyConsumerEventSet301:
; i start address is: 4 (W2)
	INC	W2
; i end address is: 4 (W2)
; i end address is: 4 (W2)
	GOTO	L__NMRAnetNode_IsAnyConsumerEventSet203
L__NMRAnetNode_IsAnyConsumerEventSet206:
;NMRAnetNode.mpas,820 :: 		end;
L_end__NMRAnetNode_IsAnyConsumerEventSet:
; Result end address is: 2 (W1)
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetNode_IsAnyConsumerEventSet:
	RETURN
; end of _NMRAnetNode_IsAnyConsumerEventSet
