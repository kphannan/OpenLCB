
_NMRAnetCANReceive_Initialize:

;NMRAnetCANReceive.mpas,75 :: 		begin
;NMRAnetCANReceive.mpas,76 :: 		CANLayerReceiveHook := PPacketHandlerFunc( nil);
	MOV	#0, W0
	MOV	W0, NMRAnetCANReceive_CANLayerReceiveHook
;NMRAnetCANReceive.mpas,77 :: 		NMRAnetLayerReceiveHook := PPacketHandlerFunc( nil);
	MOV	#0, W0
	MOV	W0, NMRAnetCANReceive_NMRAnetLayerReceiveHook
;NMRAnetCANReceive.mpas,78 :: 		end;
L_end_NMRAnetCANReceive_Initialize:
	RETURN
; end of _NMRAnetCANReceive_Initialize

_NMRAnetCANReceive_SetCANLayerReceiveHook:

;NMRAnetCANReceive.mpas,87 :: 		begin
;NMRAnetCANReceive.mpas,88 :: 		CANLayerReceiveHook := HookFunc
	MOV	W10, NMRAnetCANReceive_CANLayerReceiveHook
;NMRAnetCANReceive.mpas,89 :: 		end;
L_end_NMRAnetCANReceive_SetCANLayerReceiveHook:
	RETURN
; end of _NMRAnetCANReceive_SetCANLayerReceiveHook

_NMRAnetCANReceive_SetNMRANetLayerReceiveHook:

;NMRAnetCANReceive.mpas,98 :: 		begin
;NMRAnetCANReceive.mpas,99 :: 		NMRAnetLayerReceiveHook := HookFunc
	MOV	W10, NMRAnetCANReceive_NMRAnetLayerReceiveHook
;NMRAnetCANReceive.mpas,100 :: 		end;
L_end_NMRAnetCANReceive_SetNMRANetLayerReceiveHook:
	RETURN
; end of _NMRAnetCANReceive_SetNMRANetLayerReceiveHook

_ReceivedOnFilter0:
	LNK	#18

;NMRAnetCANReceive.mpas,120 :: 		begin
;NMRAnetCANReceive.mpas,122 :: 		DoDefault := True;
	PUSH	W10
	PUSH	W11
	PUSH	W12
	PUSH	W13
	MOV	#65535, W0
	MOV	W0, [W14+0]
;NMRAnetCANReceive.mpas,123 :: 		if CANLayerReceiveHook <> nil then
	MOV	NMRAnetCANReceive_CANLayerReceiveHook, W0
	CP	W0, #0
	BRA NZ	L__ReceivedOnFilter0294
	GOTO	L__ReceivedOnFilter05
L__ReceivedOnFilter0294:
;NMRAnetCANReceive.mpas,124 :: 		CANLayerReceiveHook(CANBuffer, DoDefault);
	ADD	W14, #0, W0
	PUSH	W10
	MOV	W0, W11
	MOV	NMRAnetCANReceive_CANLayerReceiveHook, W0
	CALL	W0
	POP	W10
L__ReceivedOnFilter05:
;NMRAnetCANReceive.mpas,126 :: 		if DoDefault then
	ADD	W14, #0, W0
	CP0	[W0]
	BRA NZ	L__ReceivedOnFilter0295
	GOTO	L__ReceivedOnFilter08
L__ReceivedOnFilter0295:
;NMRAnetCANReceive.mpas,129 :: 		SourceAlias := NMRAnetUtilities_ExtractSourceAlias(CANBuffer);
	CALL	_NMRAnetUtilities_ExtractSourceAlias
; SourceAlias start address is: 16 (W8)
	MOV	W0, W8
;NMRAnetCANReceive.mpas,130 :: 		Node := NMRAnetNode_FindByAlias(SourceAlias);
	PUSH	W10
	MOV	W0, W10
	CALL	_NMRAnetNode_FindByAlias
	POP	W10
; Node start address is: 12 (W6)
	MOV	W0, W6
;NMRAnetCANReceive.mpas,131 :: 		if Node <> nil then                                                         // Check for a Duplicate Alias
	CP	W0, #0
	BRA NZ	L__ReceivedOnFilter0296
	GOTO	L__ReceivedOnFilter011
L__ReceivedOnFilter0296:
; SourceAlias end address is: 16 (W8)
;NMRAnetCANReceive.mpas,133 :: 		if (CANBuffer^.ID and MTI_CID_MASK = MTI_CID0) or (CANBuffer^.ID and MTI_CID_MASK = MTI_CID1) or (CANBuffer^.ID and MTI_CID_MASK = MTI_CID2) or (CANBuffer^.ID and MTI_CID_MASK = MTI_CID3) then
	MOV.D	[W10], W2
	MOV	#0, W0
	MOV	#1792, W1
	AND	W2, W0, W4
	AND	W3, W1, W5
	MOV	#0, W0
	MOV	#1792, W1
	CP	W4, W0
	CPB	W5, W1
	CLR	W2
	BRA NZ	L__ReceivedOnFilter0297
	COM	W2
L__ReceivedOnFilter0297:
	MOV	#0, W0
	MOV	#1536, W1
	CP	W4, W0
	CPB	W5, W1
	CLR	W0
	BRA NZ	L__ReceivedOnFilter0298
	COM	W0
L__ReceivedOnFilter0298:
	IOR	W2, W0, W2
	MOV	#0, W0
	MOV	#1280, W1
	CP	W4, W0
	CPB	W5, W1
	CLR	W0
	BRA NZ	L__ReceivedOnFilter0299
	COM	W0
L__ReceivedOnFilter0299:
	IOR	W2, W0, W2
	MOV	#0, W0
	MOV	#1024, W1
	CP	W4, W0
	CPB	W5, W1
	CLR	W0
	BRA NZ	L__ReceivedOnFilter0300
	COM	W0
L__ReceivedOnFilter0300:
	IOR	W2, W0, W0
	BRA NZ	L__ReceivedOnFilter0301
	GOTO	L__ReceivedOnFilter014
L__ReceivedOnFilter0301:
;NMRAnetCANReceive.mpas,134 :: 		Node^.MsgFlags := Node^.MsgFlags or MF_DUPLICATE_ALIAS_RID              // A "good" duplicate Alias
	ADD	W6, #22, W1
; Node end address is: 12 (W6)
	ZE	[W1], W0
	IOR	W0, #4, W0
	MOV.B	W0, [W1]
	GOTO	L__ReceivedOnFilter015
;NMRAnetCANReceive.mpas,135 :: 		else
L__ReceivedOnFilter014:
;NMRAnetCANReceive.mpas,136 :: 		Node^.MsgFlags := Node^.MsgFlags or MF_DUPLICATE_ALIAS;                 // A "bad" duplicate Alias
; Node start address is: 12 (W6)
	ADD	W6, #22, W1
; Node end address is: 12 (W6)
	ZE	[W1], W0
	IOR	W0, #2, W0
	MOV.B	W0, [W1]
L__ReceivedOnFilter015:
;NMRAnetCANReceive.mpas,137 :: 		end else
	GOTO	L__ReceivedOnFilter012
L__ReceivedOnFilter011:
;NMRAnetCANReceive.mpas,140 :: 		case CANBuffer^.ID and MTI_MASK of
; SourceAlias start address is: 16 (W8)
	MOV.D	[W10], W2
	MOV	#61440, W0
	MOV	#4095, W1
	AND	W2, W0, W2
	AND	W3, W1, W3
	MOV	W2, [W14+14]
	MOV	W3, [W14+16]
;NMRAnetCANReceive.mpas,141 :: 		MTI_AME  : begin                                                        // Alias Map Enquiry.....
	MOV	#8192, W0
	MOV	#112, W1
	CP	W2, W0
	CPB	W3, W1
	BRA Z	L__ReceivedOnFilter0302
	GOTO	L__ReceivedOnFilter019
L__ReceivedOnFilter0302:
; SourceAlias end address is: 16 (W8)
;NMRAnetCANReceive.mpas,142 :: 		if CANBuffer^.DataCount = 6 then                            // Does the data contain the correct number of bytes for a Node ID?
	ADD	W10, #4, W0
	MOV.B	[W0], W0
	CP.B	W0, #6
	BRA Z	L__ReceivedOnFilter0303
	GOTO	L__ReceivedOnFilter021
L__ReceivedOnFilter0303:
;NMRAnetCANReceive.mpas,144 :: 		NMRAnetUtilities_CANBufferBytesToNodeID(@CANBuffer^.DataBytes, NodeID, 0);
	ADD	W10, #5, W1
	ADD	W14, #2, W0
	CLR	W12
	MOV	W0, W11
	MOV	W1, W10
	CALL	_NMRAnetUtilities_CANBufferBytesToNodeID
;NMRAnetCANReceive.mpas,145 :: 		Node := NMRAnetNode_FindByNodeID(NodeID);
	ADD	W14, #2, W0
	MOV	W0, W10
	CALL	_NMRAnetNode_FindByNodeID
; Node start address is: 4 (W2)
	MOV	W0, W2
;NMRAnetCANReceive.mpas,146 :: 		if Node <> nil then
	CP	W0, #0
	BRA NZ	L__ReceivedOnFilter0304
	GOTO	L__ReceivedOnFilter024
L__ReceivedOnFilter0304:
;NMRAnetCANReceive.mpas,148 :: 		if NMRAnetNode_TestStateFlag(Node, NS_PERMITTED) then   // Only reply if node is in Permitted state
	MOV.B	#2, W11
	MOV	W2, W10
	CALL	_NMRAnetNode_TestStateFlag
	CP0	W0
	BRA NZ	L__ReceivedOnFilter0305
	GOTO	L__ReceivedOnFilter027
L__ReceivedOnFilter0305:
;NMRAnetCANReceive.mpas,149 :: 		NMRAnetNode_SetMsgFlag(Node, MF_ALIAS_MAP_ENQUIRY);
	MOV.B	#8, W11
	MOV	W2, W10
; Node end address is: 4 (W2)
	CALL	_NMRAnetNode_SetMsgFlag
L__ReceivedOnFilter027:
;NMRAnetCANReceive.mpas,150 :: 		end
L__ReceivedOnFilter024:
;NMRAnetCANReceive.mpas,151 :: 		end else
	GOTO	L__ReceivedOnFilter022
L__ReceivedOnFilter021:
;NMRAnetCANReceive.mpas,153 :: 		if CANBuffer^.DataCount = 0 then                            // Is the message for all Nodes?
	ADD	W10, #4, W0
	MOV.B	[W0], W0
	CP.B	W0, #0
	BRA Z	L__ReceivedOnFilter0306
	GOTO	L__ReceivedOnFilter030
L__ReceivedOnFilter0306:
;NMRAnetCANReceive.mpas,154 :: 		NMRAnetNode_SetMsgFlags(MF_ALIAS_MAP_ENQUIRY);
	MOV	#8, W10
	CALL	_NMRAnetNode_SetMsgFlags
L__ReceivedOnFilter030:
;NMRAnetCANReceive.mpas,155 :: 		end
L__ReceivedOnFilter022:
;NMRAnetCANReceive.mpas,156 :: 		end;
	GOTO	L__ReceivedOnFilter016
L__ReceivedOnFilter019:
;NMRAnetCANReceive.mpas,157 :: 		MTI_AMD  : begin                                                        // Alias Map Definition....
; SourceAlias start address is: 16 (W8)
	MOV	#4096, W2
	MOV	#112, W3
	MOV	[W14+14], W0
	MOV	[W14+16], W1
	CP	W0, W2
	CPB	W1, W3
	BRA Z	L__ReceivedOnFilter0307
	GOTO	L__ReceivedOnFilter034
L__ReceivedOnFilter0307:
;NMRAnetCANReceive.mpas,158 :: 		if CANBuffer^.DataCount = 6 then                            // Does the data contain the correct number of bytes for a Node ID?
	ADD	W10, #4, W0
	MOV.B	[W0], W0
	CP.B	W0, #6
	BRA Z	L__ReceivedOnFilter0308
	GOTO	L__ReceivedOnFilter036
L__ReceivedOnFilter0308:
;NMRAnetCANReceive.mpas,160 :: 		NMRAnetUtilities_CANBufferBytesToNodeID(@CANBuffer^.DataBytes, NodeID, 0);
	ADD	W10, #5, W1
	ADD	W14, #2, W0
	CLR	W12
	MOV	W0, W11
	MOV	W1, W10
	CALL	_NMRAnetUtilities_CANBufferBytesToNodeID
;NMRAnetCANReceive.mpas,161 :: 		Node := NMRAnetNode_FindByNodeID(NodeID);                 // *************** Need to characterize how much time this may take with a large number of nodes
	ADD	W14, #2, W0
	MOV	W0, W10
	CALL	_NMRAnetNode_FindByNodeID
;NMRAnetCANReceive.mpas,162 :: 		if Node <> nil then
	CP	W0, #0
	BRA NZ	L__ReceivedOnFilter0309
	GOTO	L__ReceivedOnFilter039
L__ReceivedOnFilter0309:
;NMRAnetCANReceive.mpas,163 :: 		NMRAnetNode_SetMsgFlags(MF_DUPLICATE_NODE_ID);
	MOV	#1, W10
	CALL	_NMRAnetNode_SetMsgFlags
L__ReceivedOnFilter039:
;NMRAnetCANReceive.mpas,164 :: 		end;
L__ReceivedOnFilter036:
;NMRAnetCANReceive.mpas,165 :: 		for i := 0 to Nodes.AllocatedCount - 1 do  // *************** Need to characterize how much time this may take with a large number of nodes
; i start address is: 10 (W5)
	CLR	W5
; SourceAlias end address is: 16 (W8)
; i end address is: 10 (W5)
	MOV	W8, W6
L__ReceivedOnFilter041:
; i start address is: 10 (W5)
; SourceAlias start address is: 12 (W6)
; SourceAlias start address is: 12 (W6)
; SourceAlias end address is: 12 (W6)
	MOV	_Nodes+3120, W0
	DEC	W0
	MOV	W0, [W14+12]
	CP	W5, W0
	BRA LE	L__ReceivedOnFilter0310
	GOTO	L__ReceivedOnFilter045
L__ReceivedOnFilter0310:
; SourceAlias end address is: 12 (W6)
;NMRAnetCANReceive.mpas,167 :: 		DatagramBuffer := nil;
; SourceAlias start address is: 12 (W6)
	CLR	W0
	MOV	W0, [W14+10]
;NMRAnetCANReceive.mpas,168 :: 		if NMRAnetUtilities_FindInDatagramByState(Nodes.AllocatedList[i], SourceAlias, DatagramBuffer, CBS_PROCESSING or CBS_TRANSFER_COMPLETE, True) then
	SL	W5, #1, W1
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W1
	ADD	W14, #10, W0
	PUSH	W10
	MOV.B	#10, W13
	MOV	W0, W12
	MOV	W6, W11
	MOV	[W1], W10
	MOV	#65535, W0
	PUSH	W0
	CALL	_NMRAnetUtilities_FindInDatagramByState
	SUB	#2, W15
	POP	W10
	CP0	W0
	BRA NZ	L__ReceivedOnFilter0311
	GOTO	L__ReceivedOnFilter047
L__ReceivedOnFilter0311:
;NMRAnetCANReceive.mpas,170 :: 		NMRAnetUtilities_DatagramBufferUnLink(Nodes.AllocatedList[i], DatagramBuffer); // Alias is no longer valid for this Datagram (it was reset) so we need to throw it away
	SL	W5, #1, W1
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W0
	PUSH	W10
	MOV	[W14+10], W11
	MOV	[W0], W10
	CALL	_NMRAnetUtilities_DatagramBufferUnLink
;NMRAnetCANReceive.mpas,171 :: 		NMRAnetBufferPools_ReleaseDatagramBuffer( DatagramBuffer);
	MOV	[W14+10], W10
	CALL	_NMRAnetBufferPools_ReleaseDatagramBuffer
	POP	W10
;NMRAnetCANReceive.mpas,173 :: 		end
L__ReceivedOnFilter047:
;NMRAnetCANReceive.mpas,174 :: 		end
	MOV	[W14+12], W0
	CP	W5, W0
	BRA NZ	L__ReceivedOnFilter0312
	GOTO	L__ReceivedOnFilter045
L__ReceivedOnFilter0312:
; i start address is: 10 (W5)
	INC	W5
; i end address is: 10 (W5)
; SourceAlias end address is: 12 (W6)
; i end address is: 10 (W5)
	GOTO	L__ReceivedOnFilter041
L__ReceivedOnFilter045:
;NMRAnetCANReceive.mpas,175 :: 		end;
	GOTO	L__ReceivedOnFilter016
L__ReceivedOnFilter034:
;NMRAnetCANReceive.mpas,176 :: 		MTI_AMR  : begin                                                        // Alias Map Reset.....
; SourceAlias start address is: 16 (W8)
	MOV	#12288, W2
	MOV	#112, W3
	MOV	[W14+14], W0
	MOV	[W14+16], W1
	CP	W0, W2
	CPB	W1, W3
	BRA Z	L__ReceivedOnFilter0313
	GOTO	L__ReceivedOnFilter051
L__ReceivedOnFilter0313:
;NMRAnetCANReceive.mpas,178 :: 		for i := 0 to Nodes.AllocatedCount - 1 do  // *************** Need to characterize how much time this may take with a large number of nodes
; i start address is: 12 (W6)
	CLR	W6
; SourceAlias end address is: 16 (W8)
; i end address is: 12 (W6)
	MOV	W8, W5
L__ReceivedOnFilter052:
; i start address is: 12 (W6)
; SourceAlias start address is: 10 (W5)
; SourceAlias start address is: 10 (W5)
; SourceAlias end address is: 10 (W5)
	MOV	_Nodes+3120, W0
	DEC	W0
	MOV	W0, [W14+12]
	CP	W6, W0
	BRA LE	L__ReceivedOnFilter0314
	GOTO	L__ReceivedOnFilter056
L__ReceivedOnFilter0314:
; SourceAlias end address is: 10 (W5)
;NMRAnetCANReceive.mpas,180 :: 		DatagramBuffer := nil;
; SourceAlias start address is: 10 (W5)
	CLR	W0
	MOV	W0, [W14+10]
;NMRAnetCANReceive.mpas,181 :: 		if NMRAnetUtilities_FindInDatagramByState(Nodes.AllocatedList[i], SourceAlias, DatagramBuffer, CBS_PROCESSING or CBS_TRANSFER_COMPLETE, True) then
	SL	W6, #1, W1
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W1
	ADD	W14, #10, W0
	PUSH	W10
	MOV.B	#10, W13
	MOV	W0, W12
	MOV	W5, W11
	MOV	[W1], W10
	MOV	#65535, W0
	PUSH	W0
	CALL	_NMRAnetUtilities_FindInDatagramByState
	SUB	#2, W15
	POP	W10
	CP0	W0
	BRA NZ	L__ReceivedOnFilter0315
	GOTO	L__ReceivedOnFilter058
L__ReceivedOnFilter0315:
;NMRAnetCANReceive.mpas,183 :: 		NMRAnetUtilities_DatagramBufferUnLink(Nodes.AllocatedList[i], DatagramBuffer); // Alias is no longer valid for this Datagram (it was reset) so we need to throw it away
	SL	W6, #1, W1
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W0
	PUSH	W10
	MOV	[W14+10], W11
	MOV	[W0], W10
	CALL	_NMRAnetUtilities_DatagramBufferUnLink
;NMRAnetCANReceive.mpas,184 :: 		NMRAnetBufferPools_ReleaseDatagramBuffer( DatagramBuffer);
	MOV	[W14+10], W10
	CALL	_NMRAnetBufferPools_ReleaseDatagramBuffer
	POP	W10
;NMRAnetCANReceive.mpas,186 :: 		end
L__ReceivedOnFilter058:
;NMRAnetCANReceive.mpas,187 :: 		end
	MOV	[W14+12], W0
	CP	W6, W0
	BRA NZ	L__ReceivedOnFilter0316
	GOTO	L__ReceivedOnFilter056
L__ReceivedOnFilter0316:
; i start address is: 12 (W6)
	INC	W6
; i end address is: 12 (W6)
; SourceAlias end address is: 10 (W5)
; i end address is: 12 (W6)
	GOTO	L__ReceivedOnFilter052
L__ReceivedOnFilter056:
;NMRAnetCANReceive.mpas,188 :: 		end;
	GOTO	L__ReceivedOnFilter016
L__ReceivedOnFilter051:
L__ReceivedOnFilter016:
;NMRAnetCANReceive.mpas,190 :: 		end
L__ReceivedOnFilter012:
;NMRAnetCANReceive.mpas,191 :: 		end;
L__ReceivedOnFilter08:
;NMRAnetCANReceive.mpas,192 :: 		end;
L_end_ReceivedOnFilter0:
	POP	W13
	POP	W12
	POP	W11
	POP	W10
	ULNK
	RETURN
; end of _ReceivedOnFilter0

_ReceivedOnFilter1:
	LNK	#38

;NMRAnetCANReceive.mpas,217 :: 		begin
;NMRAnetCANReceive.mpas,221 :: 		TMR4 := 0;
	PUSH	W10
	PUSH	W11
	PUSH	W12
	PUSH	W13
	CLR	TMR4
;NMRAnetCANReceive.mpas,223 :: 		DoDefault := True;
	MOV	#65535, W0
	MOV	W0, [W14+0]
;NMRAnetCANReceive.mpas,224 :: 		if NMRAnetLayerReceiveHook <> nil then
	MOV	NMRAnetCANReceive_NMRAnetLayerReceiveHook, W0
	CP	W0, #0
	BRA NZ	L__ReceivedOnFilter1318
	GOTO	L__ReceivedOnFilter162
L__ReceivedOnFilter1318:
;NMRAnetCANReceive.mpas,225 :: 		NMRAnetLayerReceiveHook(CANBuffer, DoDefault);                              // Allow the program to have a crack at the message handler
	ADD	W14, #0, W0
	PUSH	W10
	MOV	W0, W11
	MOV	NMRAnetCANReceive_NMRAnetLayerReceiveHook, W0
	CALL	W0
	POP	W10
L__ReceivedOnFilter162:
;NMRAnetCANReceive.mpas,227 :: 		if DoDefault then
	ADD	W14, #0, W0
	CP0	[W0]
	BRA NZ	L__ReceivedOnFilter1319
	GOTO	L__ReceivedOnFilter165
L__ReceivedOnFilter1319:
;NMRAnetCANReceive.mpas,229 :: 		BaseBuffer := nil;
	CLR	W0
	MOV	W0, [W14+10]
;NMRAnetCANReceive.mpas,230 :: 		DatagramBuffer := nil;
	CLR	W0
	MOV	W0, [W14+12]
;NMRAnetCANReceive.mpas,231 :: 		SourceAlias := NMRAnetUtilities_ExtractSourceAlias(CANBuffer);
	CALL	_NMRAnetUtilities_ExtractSourceAlias
; SourceAlias start address is: 10 (W5)
	MOV	W0, W5
;NMRAnetCANReceive.mpas,232 :: 		Node := NMRAnetNode_FindByAlias(SourceAlias);                               // The Source Alias that we receive should NEVER contain our alias
	PUSH	W5
	PUSH	W10
	MOV	W0, W10
	CALL	_NMRAnetNode_FindByAlias
	POP	W10
	POP	W5
; Node start address is: 8 (W4)
	MOV	W0, W4
;NMRAnetCANReceive.mpas,233 :: 		if Node <> nil then
	CP	W0, #0
	BRA NZ	L__ReceivedOnFilter1320
	GOTO	L__ReceivedOnFilter168
L__ReceivedOnFilter1320:
; Node end address is: 8 (W4)
; SourceAlias end address is: 10 (W5)
;NMRAnetCANReceive.mpas,234 :: 		NMRAnetNode_SetMsgFlags(MF_DUPLICATE_ALIAS)                               // A "bad" duplicate Alias
	MOV	#2, W10
	CALL	_NMRAnetNode_SetMsgFlags
	GOTO	L__ReceivedOnFilter169
;NMRAnetCANReceive.mpas,235 :: 		else begin
L__ReceivedOnFilter168:
;NMRAnetCANReceive.mpas,236 :: 		case CANBuffer^.ID and MTI_FRAME_TYPE_MASK of
; SourceAlias start address is: 10 (W5)
; Node start address is: 8 (W4)
	MOV.D	[W10], W2
	MOV	#0, W0
	MOV	#3840, W1
	AND	W2, W0, W2
	AND	W3, W1, W3
	MOV	W2, [W14+34]
	MOV	W3, [W14+36]
;NMRAnetCANReceive.mpas,237 :: 		MTI_FRAME_TYPE_GENERAL               : begin
	MOV	#0, W0
	MOV	#2304, W1
	CP	W2, W0
	CPB	W3, W1
	BRA Z	L__ReceivedOnFilter1321
	GOTO	L__ReceivedOnFilter173
L__ReceivedOnFilter1321:
;NMRAnetCANReceive.mpas,238 :: 		if NMRAnetUtilities_IsAddressedMessage(CANBuffer) then                      // Is this an Addressed Message?
	CALL	_NMRAnetUtilities_IsAddressedMessage
	CP0	W0
	BRA NZ	L__ReceivedOnFilter1322
	GOTO	L__ReceivedOnFilter175
L__ReceivedOnFilter1322:
; Node end address is: 8 (W4)
;NMRAnetCANReceive.mpas,240 :: 		Node := NMRAnetNode_FindByAlias(NMRAnetUtilities_ExtractDestinationAlias(CANBuffer));   // Extract the Node that the message is addressed to
	CALL	_NMRAnetUtilities_ExtractDestinationAlias
	PUSH	W5
	PUSH	W10
	MOV	W0, W10
	CALL	_NMRAnetNode_FindByAlias
	POP	W10
	POP	W5
; Node start address is: 12 (W6)
	MOV	W0, W6
;NMRAnetCANReceive.mpas,241 :: 		if Node <> nil then
	CP	W0, #0
	BRA NZ	L__ReceivedOnFilter1323
	GOTO	L__ReceivedOnFilter178
L__ReceivedOnFilter1323:
;NMRAnetCANReceive.mpas,243 :: 		if NMRAnetNode_TestStateFlag(Node, NS_PERMITTED) then
	PUSH	W10
	MOV.B	#2, W11
	MOV	W6, W10
	CALL	_NMRAnetNode_TestStateFlag
	POP	W10
	CP0	W0
	BRA NZ	L__ReceivedOnFilter1324
	GOTO	L__ReceivedOnFilter181
L__ReceivedOnFilter1324:
;NMRAnetCANReceive.mpas,245 :: 		BaseBufferAllocFailed := False;                // Optomistic
; BaseBufferAllocFailed start address is: 14 (W7)
	CLR	W7
;NMRAnetCANReceive.mpas,246 :: 		case CANBuffer^.ID and MTI_MASK of             // If we get here then the message is for our Node or VNode
	MOV.D	[W10], W2
	MOV	#61440, W0
	MOV	#4095, W1
	AND	W2, W0, W2
	AND	W3, W1, W3
	MOV	W2, [W14+26]
	MOV	W3, [W14+28]
;NMRAnetCANReceive.mpas,247 :: 		MTI_VERIFY_NODE_ID_NUMBER_DEST  :   begin
	MOV	#32768, W0
	MOV	#2376, W1
	CP	W2, W0
	CPB	W3, W1
	BRA Z	L__ReceivedOnFilter1325
	GOTO	L__ReceivedOnFilter186
L__ReceivedOnFilter1325:
;NMRAnetCANReceive.mpas,248 :: 		NMRAnetNode_SetMsgFlag(Node, MF_VERIFY_NODE_ID) // All messages addressed to node get replies even if the payload is wrong!
	PUSH	W10
	MOV.B	#16, W11
	MOV	W6, W10
	CALL	_NMRAnetNode_SetMsgFlag
	POP	W10
;NMRAnetCANReceive.mpas,249 :: 		end;
	MOV	W7, W0
	GOTO	L__ReceivedOnFilter183
L__ReceivedOnFilter186:
;NMRAnetCANReceive.mpas,250 :: 		MTI_OPTIONAL_INTERACTION_REJECTED : begin
	MOV	#32768, W0
	MOV	#2310, W1
	MOV	[W14+26], W2
	MOV	[W14+28], W3
	CP	W2, W0
	CPB	W3, W1
	BRA Z	L__ReceivedOnFilter1326
	GOTO	L__ReceivedOnFilter189
L__ReceivedOnFilter1326:
;NMRAnetCANReceive.mpas,251 :: 		end;
	MOV	W7, W0
	GOTO	L__ReceivedOnFilter183
L__ReceivedOnFilter189:
;NMRAnetCANReceive.mpas,252 :: 		MTI_PROTOCOL_SUPPORT_INQUIRY      : begin
	MOV	#32768, W0
	MOV	#2434, W1
	CP	W2, W0
	CPB	W3, W1
	BRA Z	L__ReceivedOnFilter1327
	GOTO	L__ReceivedOnFilter192
L__ReceivedOnFilter1327:
;NMRAnetCANReceive.mpas,255 :: 		if NMRAnetUtilities_ExtractDestinationAliasFlags(CANBuffer) and PIP_EXTENSION_START_BIT_MASK = 0 then
	CALL	_NMRAnetUtilities_ExtractDestinationAliasFlags
	MOV	#8192, W1
	AND	W0, W1, W0
	CP	W0, #0
	BRA Z	L__ReceivedOnFilter1328
	GOTO	L__ReceivedOnFilter1289
L__ReceivedOnFilter1328:
;NMRAnetCANReceive.mpas,257 :: 		if NMRAnetBufferPools_AllocateBaseBuffer(BaseBuffer) then
	ADD	W14, #10, W0
	PUSH	W10
	MOV	W0, W10
	CALL	_NMRAnetBufferPools_AllocateBaseBuffer
	POP	W10
	CP0	W0
	BRA NZ	L__ReceivedOnFilter1329
	GOTO	L__ReceivedOnFilter197
L__ReceivedOnFilter1329:
;NMRAnetCANReceive.mpas,259 :: 		NMRAnetUtilities_BaseBufferLink(Node, BaseBuffer);
	PUSH	W10
	MOV	[W14+10], W11
	MOV	W6, W10
	CALL	_NMRAnetUtilities_BaseBufferLink
	POP	W10
;NMRAnetCANReceive.mpas,260 :: 		BaseBuffer^.mCode := BMC_PROTOCOL_SUPPORT_QUERY;
	MOV	[W14+10], W0
	ADD	W0, #6, W1
	MOV.B	#1, W0
	MOV.B	W0, [W1]
;NMRAnetCANReceive.mpas,261 :: 		BaseBuffer^.Alias := SourceAlias
	MOV	[W14+10], W0
	INC2	W0
	MOV	W5, [W0]
;NMRAnetCANReceive.mpas,262 :: 		end else
	MOV	W7, W0
; BaseBufferAllocFailed end address is: 14 (W7)
	GOTO	L__ReceivedOnFilter198
L__ReceivedOnFilter197:
;NMRAnetCANReceive.mpas,263 :: 		BaseBufferAllocFailed := True
; BaseBufferAllocFailed start address is: 0 (W0)
	MOV	#65535, W0
; BaseBufferAllocFailed end address is: 0 (W0)
L__ReceivedOnFilter198:
;NMRAnetCANReceive.mpas,264 :: 		end
; BaseBufferAllocFailed start address is: 0 (W0)
; BaseBufferAllocFailed end address is: 0 (W0)
	GOTO	L__ReceivedOnFilter194
L__ReceivedOnFilter1289:
;NMRAnetCANReceive.mpas,255 :: 		if NMRAnetUtilities_ExtractDestinationAliasFlags(CANBuffer) and PIP_EXTENSION_START_BIT_MASK = 0 then
	MOV	W7, W0
;NMRAnetCANReceive.mpas,264 :: 		end
L__ReceivedOnFilter194:
;NMRAnetCANReceive.mpas,265 :: 		end;
; BaseBufferAllocFailed start address is: 0 (W0)
; BaseBufferAllocFailed end address is: 0 (W0)
	GOTO	L__ReceivedOnFilter183
L__ReceivedOnFilter192:
;NMRAnetCANReceive.mpas,267 :: 		MTI_EVENTS_IDENTIFY_DEST          : begin
; BaseBufferAllocFailed start address is: 14 (W7)
	MOV	#32768, W2
	MOV	#2454, W3
	MOV	[W14+26], W0
	MOV	[W14+28], W1
	CP	W0, W2
	CPB	W1, W3
	BRA Z	L__ReceivedOnFilter1330
	GOTO	L__ReceivedOnFilter1101
L__ReceivedOnFilter1330:
;NMRAnetCANReceive.mpas,268 :: 		if not AppCallback_EventsIdentifyByDest(Node) then
	PUSH.D	W6
	PUSH	W5
	PUSH	W10
	MOV	W6, W10
	CALL	_AppCallback_EventsIdentifyByDest
	POP	W10
	POP	W5
	POP.D	W6
	COM	W0
	BRA NZ	L__ReceivedOnFilter1331
	GOTO	L__ReceivedOnFilter1103
L__ReceivedOnFilter1331:
;NMRAnetCANReceive.mpas,270 :: 		NMRAnetNode_SetProducerEventFlags(Node, EVENT_STATE_UNKOWN);
	PUSH	W6
	PUSH	W5
	PUSH	W10
	MOV.B	#3, W11
	MOV	W6, W10
	CALL	_NMRAnetNode_SetProducerEventFlags
	POP	W10
	POP	W5
	POP	W6
;NMRAnetCANReceive.mpas,271 :: 		NMRAnetNode_SetConsumerEventFlags(Node, EVENT_STATE_UNKOWN);
	PUSH	W6
	PUSH	W5
	PUSH	W10
	MOV.B	#3, W11
	MOV	W6, W10
	CALL	_NMRAnetNode_SetConsumerEventFlags
	POP	W10
	POP	W5
	POP	W6
;NMRAnetCANReceive.mpas,272 :: 		end
L__ReceivedOnFilter1103:
;NMRAnetCANReceive.mpas,273 :: 		end;
	MOV	W7, W0
	GOTO	L__ReceivedOnFilter183
L__ReceivedOnFilter1101:
;NMRAnetCANReceive.mpas,275 :: 		MTI_DATAGRAM_OK_REPLY             : begin
	MOV	#32768, W2
	MOV	#2466, W3
	MOV	[W14+26], W0
	MOV	[W14+28], W1
	CP	W0, W2
	CPB	W1, W3
	BRA Z	L__ReceivedOnFilter1332
	GOTO	L__ReceivedOnFilter1107
L__ReceivedOnFilter1332:
;NMRAnetCANReceive.mpas,276 :: 		if NMRAnetUtilities_FindInDatagramByState(Node, SourceAlias, DatagramBuffer, CBS_OUTGOING or CBS_TRANSFER_COMPLETE, False) then
	ADD	W14, #12, W0
	PUSH	W10
	MOV.B	#12, W13
	MOV	W0, W12
	MOV	W5, W11
	MOV	W6, W10
	CLR	W0
	PUSH	W0
	CALL	_NMRAnetUtilities_FindInDatagramByState
	SUB	#2, W15
	POP	W10
	CP0	W0
	BRA NZ	L__ReceivedOnFilter1333
	GOTO	L__ReceivedOnFilter1109
L__ReceivedOnFilter1333:
;NMRAnetCANReceive.mpas,278 :: 		NMRAnetUtilities_DatagramBufferUnLink(Node, DatagramBuffer);
	PUSH	W10
	MOV	[W14+12], W11
	MOV	W6, W10
	CALL	_NMRAnetUtilities_DatagramBufferUnLink
;NMRAnetCANReceive.mpas,279 :: 		NMRAnetBufferPools_ReleaseDatagramBuffer(DatagramBuffer);
	MOV	[W14+12], W10
	CALL	_NMRAnetBufferPools_ReleaseDatagramBuffer
	POP	W10
;NMRAnetCANReceive.mpas,281 :: 		end
L__ReceivedOnFilter1109:
;NMRAnetCANReceive.mpas,282 :: 		end;
	MOV	W7, W0
	GOTO	L__ReceivedOnFilter183
L__ReceivedOnFilter1107:
;NMRAnetCANReceive.mpas,283 :: 		MTI_DATAGRAM_REJECTED_REPLY       : begin
	MOV	#32768, W2
	MOV	#2468, W3
	MOV	[W14+26], W0
	MOV	[W14+28], W1
	CP	W0, W2
	CPB	W1, W3
	BRA Z	L__ReceivedOnFilter1334
	GOTO	L__ReceivedOnFilter1113
L__ReceivedOnFilter1334:
;NMRAnetCANReceive.mpas,284 :: 		if NMRAnetUtilities_FindInDatagramByState(Node, SourceAlias, DatagramBuffer, CBS_OUTGOING or CBS_TRANSFER_COMPLETE, False) then
	ADD	W14, #12, W0
	PUSH	W10
	MOV.B	#12, W13
	MOV	W0, W12
	MOV	W5, W11
	MOV	W6, W10
	CLR	W0
	PUSH	W0
	CALL	_NMRAnetUtilities_FindInDatagramByState
	SUB	#2, W15
	POP	W10
	CP0	W0
	BRA NZ	L__ReceivedOnFilter1335
	GOTO	L__ReceivedOnFilter1115
L__ReceivedOnFilter1335:
;NMRAnetCANReceive.mpas,286 :: 		ErrorCode := (CANBuffer^.DataBytes[2] shl 8) or CANBuffer^.DataBytes[3];
	ADD	W10, #5, W2
	ADD	W2, #2, W0
	MOV.B	[W0], W0
	ZE	W0, W0
	SL	W0, #8, W1
	ADD	W2, #3, W0
	ZE	[W0], W0
	IOR	W1, W0, W1
;NMRAnetCANReceive.mpas,287 :: 		if ErrorCode and DATAGRAM_RESULT_REJECTED_RESEND_MASK <> 0 then
	MOV	#8192, W0
	AND	W1, W0, W0
	CP	W0, #0
	BRA NZ	L__ReceivedOnFilter1336
	GOTO	L__ReceivedOnFilter1118
L__ReceivedOnFilter1336:
;NMRAnetCANReceive.mpas,290 :: 		DatagramBuffer^.State := (DatagramBuffer^.State or CBS_OUTGOING or CBS_PROCESSING) and not CBS_TRANSFER_COMPLETE;  // Turn it into an outgoing Datagram
	MOV	[W14+12], W0
	MOV.B	[W0], W0
	ZE	W0, W0
	IOR	W0, #4, W0
	IOR	W0, #2, W1
	MOV	#247, W0
	AND	W1, W0, W1
	MOV	[W14+12], W0
	MOV.B	W1, [W0]
;NMRAnetCANReceive.mpas,291 :: 		DatagramBuffer^.Tag := 0;
	MOV	[W14+12], W0
	ADD	W0, #8, W1
	CLR	W0
	MOV	W0, [W1]
;NMRAnetCANReceive.mpas,292 :: 		end else   // Done
	GOTO	L__ReceivedOnFilter1119
L__ReceivedOnFilter1118:
;NMRAnetCANReceive.mpas,294 :: 		NMRAnetUtilities_DatagramBufferUnLink(Node, DatagramBuffer);
	PUSH	W10
	MOV	[W14+12], W11
	MOV	W6, W10
	CALL	_NMRAnetUtilities_DatagramBufferUnLink
;NMRAnetCANReceive.mpas,295 :: 		NMRAnetBufferPools_ReleaseDatagramBuffer(DatagramBuffer);
	MOV	[W14+12], W10
	CALL	_NMRAnetBufferPools_ReleaseDatagramBuffer
	POP	W10
;NMRAnetCANReceive.mpas,297 :: 		end
L__ReceivedOnFilter1119:
;NMRAnetCANReceive.mpas,298 :: 		end
L__ReceivedOnFilter1115:
;NMRAnetCANReceive.mpas,300 :: 		{$IFNDEF BOOTLOADER};
	MOV	W7, W0
	GOTO	L__ReceivedOnFilter183
L__ReceivedOnFilter1113:
;NMRAnetCANReceive.mpas,301 :: 		MTI_SIMPLE_NODE_INFO_REQUEST      : begin
	MOV	#32768, W2
	MOV	#2526, W3
	MOV	[W14+26], W0
	MOV	[W14+28], W1
	CP	W0, W2
	CPB	W1, W3
	BRA Z	L__ReceivedOnFilter1337
	GOTO	L__ReceivedOnFilter1122
L__ReceivedOnFilter1337:
;NMRAnetCANReceive.mpas,302 :: 		if NMRAnetBufferPools_AllocateBaseBuffer(BaseBuffer) then
	ADD	W14, #10, W0
	PUSH	W10
	MOV	W0, W10
	CALL	_NMRAnetBufferPools_AllocateBaseBuffer
	POP	W10
	CP0	W0
	BRA NZ	L__ReceivedOnFilter1338
	GOTO	L__ReceivedOnFilter1124
L__ReceivedOnFilter1338:
;NMRAnetCANReceive.mpas,304 :: 		NMRAnetUtilities_BaseBufferLink(Node, BaseBuffer);
	PUSH	W10
	MOV	[W14+10], W11
	MOV	W6, W10
	CALL	_NMRAnetUtilities_BaseBufferLink
	POP	W10
;NMRAnetCANReceive.mpas,305 :: 		BaseBuffer^.mCode := BMC_SIMPLE_NODE_INFO_REQEUST;
	MOV	[W14+10], W0
	ADD	W0, #6, W1
	MOV.B	#5, W0
	MOV.B	W0, [W1]
;NMRAnetCANReceive.mpas,306 :: 		BaseBuffer^.Alias := SourceAlias
	MOV	[W14+10], W0
	INC2	W0
	MOV	W5, [W0]
;NMRAnetCANReceive.mpas,307 :: 		end else
	MOV	W7, W0
; BaseBufferAllocFailed end address is: 14 (W7)
	GOTO	L__ReceivedOnFilter1125
L__ReceivedOnFilter1124:
;NMRAnetCANReceive.mpas,308 :: 		BaseBufferAllocFailed := True
; BaseBufferAllocFailed start address is: 0 (W0)
	MOV	#65535, W0
; BaseBufferAllocFailed end address is: 0 (W0)
L__ReceivedOnFilter1125:
;NMRAnetCANReceive.mpas,309 :: 		end;
; BaseBufferAllocFailed start address is: 0 (W0)
; BaseBufferAllocFailed end address is: 0 (W0)
	GOTO	L__ReceivedOnFilter183
L__ReceivedOnFilter1122:
;NMRAnetCANReceive.mpas,310 :: 		MTI_TRACTION_PROTOCOL             : begin
; BaseBufferAllocFailed start address is: 14 (W7)
	MOV	#32768, W2
	MOV	#2398, W3
	MOV	[W14+26], W0
	MOV	[W14+28], W1
	CP	W0, W2
	CPB	W1, W3
	BRA Z	L__ReceivedOnFilter1339
	GOTO	L__ReceivedOnFilter1128
L__ReceivedOnFilter1339:
;NMRAnetCANReceive.mpas,311 :: 		AppCallback_TractionControl(Node, CANBuffer);
	PUSH.D	W6
	PUSH	W5
	PUSH	W10
	MOV	W10, W11
	MOV	W6, W10
	CALL	_AppCallback_TractionControl
	POP	W10
	POP	W5
	POP.D	W6
;NMRAnetCANReceive.mpas,314 :: 		else begin
	MOV	W7, W0
	GOTO	L__ReceivedOnFilter183
L__ReceivedOnFilter1128:
;NMRAnetCANReceive.mpas,315 :: 		DataBytes[0] := $20;
	MOV.B	#32, W0
	MOV.B	W0, [W14+18]
;NMRAnetCANReceive.mpas,316 :: 		DataBytes[1] := $00;
	CLR	W0
	MOV.B	W0, [W14+19]
;NMRAnetCANReceive.mpas,317 :: 		MTI_Code := (CANBuffer^.ID shr 12) and $0FFF;
	MOV	[W10++], W3
	MOV	[W10--], W4
	MOV	#12, W0
	MOV	W3, W1
	MOV	W4, W2
L__ReceivedOnFilter1340:
	DEC	W0, W0
	BRA LT	L__ReceivedOnFilter1341
	LSR	W2, W2
	RRC	W1, W1
	BRA	L__ReceivedOnFilter1340
L__ReceivedOnFilter1341:
	MOV	#4095, W0
	AND	W1, W0, W3
;NMRAnetCANReceive.mpas,318 :: 		DataBytes[2] := (MTI_Code shr 8) and $00FF;
	LSR	W3, #8, W2
	MOV.B	#255, W1
	ADD	W14, #20, W0
	AND.B	W2, W1, [W0]
;NMRAnetCANReceive.mpas,319 :: 		DataBytes[3] := MTI_Code and $00FF;
	MOV.B	#255, W1
	ADD	W14, #21, W0
	AND.B	W3, W1, [W0]
;NMRAnetCANReceive.mpas,320 :: 		NMRAnetUtilities_CreateNMRABusMessageCANBuffer(Node, @CAN_Engine.TX_AddressedErrorBuffer, MTI_OPTIONAL_INTERACTION_REJECTED, SourceAlias, 4, @DataBytes);
	ADD	W14, #18, W0
	PUSH	W5
	PUSH	W10
	MOV	#32768, W12
	MOV	#2310, W13
	MOV	#lo_addr(_CAN_Engine+30), W11
	MOV	W6, W10
	PUSH	W0
	MOV	#4, W0
	PUSH	W0
	PUSH	W5
	CALL	_NMRAnetUtilities_CreateNMRABusMessageCANBuffer
	SUB	#6, W15
	POP	W10
	POP	W5
;NMRAnetCANReceive.mpas,321 :: 		CAN_Engine.TX_AddressedErrorBuffer.State := CAN_Engine.TX_AddressedErrorBuffer.State or BS_ALLOCATED;
	MOV	#lo_addr(_CAN_Engine+43), W0
	MOV.B	[W0], W1
	MOV	#lo_addr(_CAN_Engine+43), W0
	IOR.B	W1, #2, [W0]
;NMRAnetCANReceive.mpas,322 :: 		StartCANHighPriorityMessageEngine;
	PUSH.D	W6
; BaseBufferAllocFailed end address is: 14 (W7)
	PUSH	W5
	PUSH	W10
	CALL	_StartCANHighPriorityMessageEngine
	POP	W10
	POP	W5
	POP.D	W6
	MOV	W7, W0
;NMRAnetCANReceive.mpas,323 :: 		end;
L__ReceivedOnFilter183:
;NMRAnetCANReceive.mpas,327 :: 		if BaseBufferAllocFailed then
; BaseBufferAllocFailed start address is: 0 (W0)
	CP0	W0
	BRA NZ	L__ReceivedOnFilter1342
	GOTO	L__ReceivedOnFilter1130
L__ReceivedOnFilter1342:
; BaseBufferAllocFailed end address is: 0 (W0)
;NMRAnetCANReceive.mpas,329 :: 		DataBytes[0] := Hi( TERMINATE_DUE_TO_ERROR_TEMPORARY);
	MOV.B	#16, W0
	MOV.B	W0, [W14+18]
;NMRAnetCANReceive.mpas,330 :: 		DataBytes[1] := Lo( TERMINATE_DUE_TO_ERROR_TEMPORARY);
	CLR	W0
	MOV.B	W0, [W14+19]
;NMRAnetCANReceive.mpas,331 :: 		MTI_Code := (CANBuffer^.ID shr 12) and $0FFF;
	MOV	[W10++], W3
	MOV	[W10--], W4
	MOV	#12, W0
	MOV	W3, W1
	MOV	W4, W2
L__ReceivedOnFilter1343:
	DEC	W0, W0
	BRA LT	L__ReceivedOnFilter1344
	LSR	W2, W2
	RRC	W1, W1
	BRA	L__ReceivedOnFilter1343
L__ReceivedOnFilter1344:
	MOV	#4095, W0
	AND	W1, W0, W3
;NMRAnetCANReceive.mpas,332 :: 		DataBytes[2] := (MTI_Code shr 8) and $00FF;
	LSR	W3, #8, W2
	MOV.B	#255, W1
	ADD	W14, #20, W0
	AND.B	W2, W1, [W0]
;NMRAnetCANReceive.mpas,333 :: 		DataBytes[3] := MTI_Code and $00FF;
	MOV.B	#255, W1
	ADD	W14, #21, W0
	AND.B	W3, W1, [W0]
;NMRAnetCANReceive.mpas,334 :: 		NMRAnetUtilities_CreateNMRABusMessageCANBuffer(Node, @CAN_Engine.TX_AddressedErrorBuffer, MTI_OPTIONAL_INTERACTION_REJECTED, SourceAlias, 4, @DataBytes);
	ADD	W14, #18, W0
	MOV	#32768, W12
	MOV	#2310, W13
	MOV	#lo_addr(_CAN_Engine+30), W11
	MOV	W6, W10
; Node end address is: 12 (W6)
	PUSH	W0
	MOV	#4, W0
	PUSH	W0
	PUSH	W5
; SourceAlias end address is: 10 (W5)
	CALL	_NMRAnetUtilities_CreateNMRABusMessageCANBuffer
	SUB	#6, W15
;NMRAnetCANReceive.mpas,335 :: 		CAN_Engine.TX_AddressedErrorBuffer.State := CAN_Engine.TX_AddressedErrorBuffer.State or BS_ALLOCATED;
	MOV	#lo_addr(_CAN_Engine+43), W0
	MOV.B	[W0], W1
	MOV	#lo_addr(_CAN_Engine+43), W0
	IOR.B	W1, #2, [W0]
;NMRAnetCANReceive.mpas,336 :: 		StartCANHighPriorityMessageEngine;
	CALL	_StartCANHighPriorityMessageEngine
;NMRAnetCANReceive.mpas,337 :: 		end
L__ReceivedOnFilter1130:
;NMRAnetCANReceive.mpas,338 :: 		end
L__ReceivedOnFilter181:
;NMRAnetCANReceive.mpas,339 :: 		end
L__ReceivedOnFilter178:
;NMRAnetCANReceive.mpas,340 :: 		end else
	GOTO	L__ReceivedOnFilter176
L__ReceivedOnFilter175:
;NMRAnetCANReceive.mpas,342 :: 		case CANBuffer^.ID and MTI_MASK of
; Node start address is: 8 (W4)
	MOV.D	[W10], W2
	MOV	#61440, W0
	MOV	#4095, W1
	AND	W2, W0, W2
	AND	W3, W1, W3
	MOV	W2, [W14+30]
	MOV	W3, [W14+32]
;NMRAnetCANReceive.mpas,343 :: 		MTI_VERIFY_NODE_ID_NUMBER   : begin
	MOV	#0, W0
	MOV	#2377, W1
	CP	W2, W0
	CPB	W3, W1
	BRA Z	L__ReceivedOnFilter1345
	GOTO	L__ReceivedOnFilter1135
L__ReceivedOnFilter1345:
; Node end address is: 8 (W4)
;NMRAnetCANReceive.mpas,345 :: 		if (CANBuffer^.DataCount = 0) then                                      // THIS IS NOT CLEAR IN THE SPEC
	ADD	W10, #4, W0
	MOV.B	[W0], W0
	CP.B	W0, #0
	BRA Z	L__ReceivedOnFilter1346
	GOTO	L__ReceivedOnFilter1137
L__ReceivedOnFilter1346:
;NMRAnetCANReceive.mpas,346 :: 		NMRAnetNode_SetMsgFlags(MF_VERIFY_NODE_ID)
	MOV	#16, W10
	CALL	_NMRAnetNode_SetMsgFlags
	GOTO	L__ReceivedOnFilter1138
;NMRAnetCANReceive.mpas,347 :: 		else begin
L__ReceivedOnFilter1137:
;NMRAnetCANReceive.mpas,348 :: 		NMRAnetUtilities_CANBufferBytesToNodeID(@CANBuffer^.DataBytes, NodeID, 0);
	ADD	W10, #5, W1
	ADD	W14, #2, W0
	CLR	W12
	MOV	W0, W11
	MOV	W1, W10
	CALL	_NMRAnetUtilities_CANBufferBytesToNodeID
;NMRAnetCANReceive.mpas,349 :: 		Node := NMRAnetNode_FindByNodeID(NodeID);
	ADD	W14, #2, W0
	MOV	W0, W10
	CALL	_NMRAnetNode_FindByNodeID
; Node start address is: 2 (W1)
	MOV	W0, W1
;NMRAnetCANReceive.mpas,350 :: 		if Node <> nil then
	CP	W0, #0
	BRA NZ	L__ReceivedOnFilter1347
	GOTO	L__ReceivedOnFilter1140
L__ReceivedOnFilter1347:
;NMRAnetCANReceive.mpas,351 :: 		NMRAnetNode_SetMsgFlag(Node, MF_VERIFY_NODE_ID)
	MOV.B	#16, W11
	MOV	W1, W10
; Node end address is: 2 (W1)
	CALL	_NMRAnetNode_SetMsgFlag
L__ReceivedOnFilter1140:
;NMRAnetCANReceive.mpas,352 :: 		end
L__ReceivedOnFilter1138:
;NMRAnetCANReceive.mpas,353 :: 		end;
	GOTO	L__ReceivedOnFilter1132
L__ReceivedOnFilter1135:
;NMRAnetCANReceive.mpas,355 :: 		MTI_CONSUMER_IDENTIFY       : begin
; Node start address is: 8 (W4)
	MOV	#16384, W2
	MOV	#2447, W3
	MOV	[W14+30], W0
	MOV	[W14+32], W1
	CP	W0, W2
	CPB	W1, W3
	BRA Z	L__ReceivedOnFilter1348
	GOTO	L__ReceivedOnFilter1144
L__ReceivedOnFilter1348:
;NMRAnetCANReceive.mpas,356 :: 		if not AppCallback_ConsumerIdentify(Node, @CANBuffer^.DataBytes) then
	ADD	W10, #5, W0
	PUSH	W10
	MOV	W0, W11
	MOV	W4, W10
; Node end address is: 8 (W4)
	CALL	_AppCallback_ConsumerIdentify
	POP	W10
	COM	W0
	BRA NZ	L__ReceivedOnFilter1349
	GOTO	L__ReceivedOnFilter1146
L__ReceivedOnFilter1349:
;NMRAnetCANReceive.mpas,358 :: 		VNodeEventIndex := -1;
	MOV	#65535, W0
	MOV	W0, [W14+14]
;NMRAnetCANReceive.mpas,359 :: 		NodeEventIndex := -1;
	MOV	#65535, W0
	MOV	W0, [W14+16]
;NMRAnetCANReceive.mpas,360 :: 		VNodeEvent := NMRAnetUtilities_SupportsVNodeEventAsConsumer(@CANBuffer^.DataBytes, VNodeEventIndex);
	ADD	W10, #5, W1
	ADD	W14, #14, W0
	PUSH	W10
	MOV	W0, W11
	MOV	W1, W10
	CALL	_NMRAnetUtilities_SupportsVNodeEventAsConsumer
	POP	W10
; VNodeEvent start address is: 14 (W7)
	MOV	W0, W7
;NMRAnetCANReceive.mpas,361 :: 		NodeEvent := NMRAnetUtilities_SupportsEventAsConsumer(@CANBuffer^.DataBytes, NodeEventIndex);
	ADD	W10, #5, W1
	ADD	W14, #16, W0
	MOV	W0, W11
	MOV	W1, W10
	CALL	_NMRAnetUtilities_SupportsEventAsConsumer
; NodeEvent start address is: 12 (W6)
	MOV	W0, W6
;NMRAnetCANReceive.mpas,362 :: 		for i := 0 to Nodes.AllocatedCount - 1 do
; i start address is: 16 (W8)
	CLR	W8
; i end address is: 16 (W8)
L__ReceivedOnFilter1148:
; i start address is: 16 (W8)
; NodeEvent start address is: 12 (W6)
; NodeEvent end address is: 12 (W6)
; VNodeEvent start address is: 14 (W7)
; VNodeEvent end address is: 14 (W7)
	MOV	_Nodes+3120, W0
	DEC	W0
	MOV	W0, [W14+26]
	CP	W8, W0
	BRA LE	L__ReceivedOnFilter1350
	GOTO	L__ReceivedOnFilter1152
L__ReceivedOnFilter1350:
; NodeEvent end address is: 12 (W6)
; VNodeEvent end address is: 14 (W7)
;NMRAnetCANReceive.mpas,364 :: 		if NMRAnetNode_TestStateFlag(Nodes.AllocatedList[i], NS_VIRTUAL) then
; VNodeEvent start address is: 14 (W7)
; NodeEvent start address is: 12 (W6)
	SL	W8, #1, W1
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W0
	PUSH	W10
	MOV.B	#8, W11
	MOV	[W0], W10
	CALL	_NMRAnetNode_TestStateFlag
	POP	W10
	CP0	W0
	BRA NZ	L__ReceivedOnFilter1351
	GOTO	L__ReceivedOnFilter1154
L__ReceivedOnFilter1351:
;NMRAnetCANReceive.mpas,366 :: 		if VNodeEvent then
	CP0	W7
	BRA NZ	L__ReceivedOnFilter1352
	GOTO	L__ReceivedOnFilter1157
L__ReceivedOnFilter1352:
;NMRAnetCANReceive.mpas,367 :: 		NMRAnetNode_SetConsumerEventFlag(Nodes.AllocatedList[i], VNodeEventIndex, EVENT_STATE_UNKOWN);
	SL	W8, #1, W1
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W0
	PUSH	W10
	MOV.B	#3, W12
	MOV	[W14+14], W11
	MOV	[W0], W10
	CALL	_NMRAnetNode_SetConsumerEventFlag
	POP	W10
L__ReceivedOnFilter1157:
;NMRAnetCANReceive.mpas,368 :: 		end else
	GOTO	L__ReceivedOnFilter1155
L__ReceivedOnFilter1154:
;NMRAnetCANReceive.mpas,370 :: 		if NodeEvent then
	CP0	W6
	BRA NZ	L__ReceivedOnFilter1353
	GOTO	L__ReceivedOnFilter1160
L__ReceivedOnFilter1353:
;NMRAnetCANReceive.mpas,371 :: 		NMRAnetNode_SetConsumerEventFlag(Nodes.AllocatedList[0], NodeEventIndex, EVENT_STATE_UNKOWN);
	PUSH	W10
	MOV.B	#3, W12
	MOV	[W14+16], W11
	MOV	_Nodes+2990, W10
	CALL	_NMRAnetNode_SetConsumerEventFlag
	POP	W10
L__ReceivedOnFilter1160:
;NMRAnetCANReceive.mpas,372 :: 		end
L__ReceivedOnFilter1155:
;NMRAnetCANReceive.mpas,373 :: 		end;
	MOV	[W14+26], W0
	CP	W8, W0
	BRA NZ	L__ReceivedOnFilter1354
	GOTO	L__ReceivedOnFilter1152
L__ReceivedOnFilter1354:
; i start address is: 16 (W8)
	INC	W8
; i end address is: 16 (W8)
; NodeEvent end address is: 12 (W6)
; VNodeEvent end address is: 14 (W7)
; i end address is: 16 (W8)
	GOTO	L__ReceivedOnFilter1148
L__ReceivedOnFilter1152:
;NMRAnetCANReceive.mpas,374 :: 		end
L__ReceivedOnFilter1146:
;NMRAnetCANReceive.mpas,375 :: 		end;
	GOTO	L__ReceivedOnFilter1132
L__ReceivedOnFilter1144:
;NMRAnetCANReceive.mpas,376 :: 		MTI_CONSUMER_IDENTIFY_RANGE : begin
; Node start address is: 8 (W4)
	MOV	#16384, W0
	MOV	#2378, W1
	MOV	[W14+30], W2
	MOV	[W14+32], W3
	CP	W2, W0
	CPB	W3, W1
	BRA Z	L__ReceivedOnFilter1355
	GOTO	L__ReceivedOnFilter1164
L__ReceivedOnFilter1355:
; Node end address is: 8 (W4)
;NMRAnetCANReceive.mpas,378 :: 		end;
	GOTO	L__ReceivedOnFilter1132
L__ReceivedOnFilter1164:
;NMRAnetCANReceive.mpas,379 :: 		MTI_PRODUCER_IDENDIFY       : begin
; Node start address is: 8 (W4)
	MOV	#16384, W0
	MOV	#2449, W1
	CP	W2, W0
	CPB	W3, W1
	BRA Z	L__ReceivedOnFilter1356
	GOTO	L__ReceivedOnFilter1167
L__ReceivedOnFilter1356:
;NMRAnetCANReceive.mpas,380 :: 		if not AppCallback_ProducerIdentify(Node, @CANBuffer^.DataBytes) then
	ADD	W10, #5, W0
	PUSH	W10
	MOV	W0, W11
	MOV	W4, W10
; Node end address is: 8 (W4)
	CALL	_AppCallback_ProducerIdentify
	POP	W10
	COM	W0
	BRA NZ	L__ReceivedOnFilter1357
	GOTO	L__ReceivedOnFilter1169
L__ReceivedOnFilter1357:
;NMRAnetCANReceive.mpas,382 :: 		VNodeEventIndex := -1;
	MOV	#65535, W0
	MOV	W0, [W14+14]
;NMRAnetCANReceive.mpas,383 :: 		NodeEventIndex := -1;
	MOV	#65535, W0
	MOV	W0, [W14+16]
;NMRAnetCANReceive.mpas,384 :: 		VNodeEvent := NMRAnetUtilities_SupportsVNodeEventAsProducer(@CANBuffer^.DataBytes, VNodeEventIndex);
	ADD	W10, #5, W1
	ADD	W14, #14, W0
	PUSH	W10
	MOV	W0, W11
	MOV	W1, W10
	CALL	_NMRAnetUtilities_SupportsVNodeEventAsProducer
	POP	W10
; VNodeEvent start address is: 14 (W7)
	MOV	W0, W7
;NMRAnetCANReceive.mpas,385 :: 		NodeEvent := NMRAnetUtilities_SupportsEventAsProducer(@CANBuffer^.DataBytes, NodeEventIndex);
	ADD	W10, #5, W1
	ADD	W14, #16, W0
	MOV	W0, W11
	MOV	W1, W10
	CALL	_NMRAnetUtilities_SupportsEventAsProducer
; NodeEvent start address is: 12 (W6)
	MOV	W0, W6
;NMRAnetCANReceive.mpas,386 :: 		for i := 0 to Nodes.AllocatedCount - 1 do
; i start address is: 16 (W8)
	CLR	W8
; i end address is: 16 (W8)
L__ReceivedOnFilter1171:
; i start address is: 16 (W8)
; NodeEvent start address is: 12 (W6)
; NodeEvent end address is: 12 (W6)
; VNodeEvent start address is: 14 (W7)
; VNodeEvent end address is: 14 (W7)
	MOV	_Nodes+3120, W0
	DEC	W0
	MOV	W0, [W14+26]
	CP	W8, W0
	BRA LE	L__ReceivedOnFilter1358
	GOTO	L__ReceivedOnFilter1175
L__ReceivedOnFilter1358:
; NodeEvent end address is: 12 (W6)
; VNodeEvent end address is: 14 (W7)
;NMRAnetCANReceive.mpas,388 :: 		if NMRAnetNode_TestStateFlag(Nodes.AllocatedList[i], NS_VIRTUAL) then
; VNodeEvent start address is: 14 (W7)
; NodeEvent start address is: 12 (W6)
	SL	W8, #1, W1
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W0
	PUSH	W10
	MOV.B	#8, W11
	MOV	[W0], W10
	CALL	_NMRAnetNode_TestStateFlag
	POP	W10
	CP0	W0
	BRA NZ	L__ReceivedOnFilter1359
	GOTO	L__ReceivedOnFilter1177
L__ReceivedOnFilter1359:
;NMRAnetCANReceive.mpas,390 :: 		if VNodeEvent then
	CP0	W7
	BRA NZ	L__ReceivedOnFilter1360
	GOTO	L__ReceivedOnFilter1180
L__ReceivedOnFilter1360:
;NMRAnetCANReceive.mpas,391 :: 		NMRAnetNode_SetProducerEventFlag(Nodes.AllocatedList[i], VNodeEventIndex, EVENT_STATE_UNKOWN);
	SL	W8, #1, W1
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W0
	PUSH	W10
	MOV.B	#3, W12
	MOV	[W14+14], W11
	MOV	[W0], W10
	CALL	_NMRAnetNode_SetProducerEventFlag
	POP	W10
L__ReceivedOnFilter1180:
;NMRAnetCANReceive.mpas,392 :: 		end else
	GOTO	L__ReceivedOnFilter1178
L__ReceivedOnFilter1177:
;NMRAnetCANReceive.mpas,394 :: 		if NodeEvent then
	CP0	W6
	BRA NZ	L__ReceivedOnFilter1361
	GOTO	L__ReceivedOnFilter1183
L__ReceivedOnFilter1361:
;NMRAnetCANReceive.mpas,395 :: 		NMRAnetNode_SetProducerEventFlag(Nodes.AllocatedList[0], NodeEventIndex, EVENT_STATE_UNKOWN);
	PUSH	W10
	MOV.B	#3, W12
	MOV	[W14+16], W11
	MOV	_Nodes+2990, W10
	CALL	_NMRAnetNode_SetProducerEventFlag
	POP	W10
L__ReceivedOnFilter1183:
;NMRAnetCANReceive.mpas,396 :: 		end
L__ReceivedOnFilter1178:
;NMRAnetCANReceive.mpas,397 :: 		end;
	MOV	[W14+26], W0
	CP	W8, W0
	BRA NZ	L__ReceivedOnFilter1362
	GOTO	L__ReceivedOnFilter1175
L__ReceivedOnFilter1362:
; i start address is: 16 (W8)
	INC	W8
; i end address is: 16 (W8)
; NodeEvent end address is: 12 (W6)
; VNodeEvent end address is: 14 (W7)
; i end address is: 16 (W8)
	GOTO	L__ReceivedOnFilter1171
L__ReceivedOnFilter1175:
;NMRAnetCANReceive.mpas,398 :: 		end
L__ReceivedOnFilter1169:
;NMRAnetCANReceive.mpas,399 :: 		end;
	GOTO	L__ReceivedOnFilter1132
L__ReceivedOnFilter1167:
;NMRAnetCANReceive.mpas,400 :: 		MTI_PRODUCER_IDENTIFY_RANGE : begin
	MOV	#16384, W0
	MOV	#2386, W1
	MOV	[W14+30], W2
	MOV	[W14+32], W3
	CP	W2, W0
	CPB	W3, W1
	BRA Z	L__ReceivedOnFilter1363
	GOTO	L__ReceivedOnFilter1187
L__ReceivedOnFilter1363:
;NMRAnetCANReceive.mpas,402 :: 		end;
	GOTO	L__ReceivedOnFilter1132
L__ReceivedOnFilter1187:
;NMRAnetCANReceive.mpas,403 :: 		MTI_EVENT_LEARN             : begin
	MOV	#16384, W0
	MOV	#2393, W1
	CP	W2, W0
	CPB	W3, W1
	BRA Z	L__ReceivedOnFilter1364
	GOTO	L__ReceivedOnFilter1190
L__ReceivedOnFilter1364:
;NMRAnetCANReceive.mpas,405 :: 		end;
	GOTO	L__ReceivedOnFilter1132
L__ReceivedOnFilter1190:
;NMRAnetCANReceive.mpas,406 :: 		MTI_EVENTS_IDENTIFY         : begin
	MOV	#0, W0
	MOV	#2455, W1
	CP	W2, W0
	CPB	W3, W1
	BRA Z	L__ReceivedOnFilter1365
	GOTO	L__ReceivedOnFilter1193
L__ReceivedOnFilter1365:
;NMRAnetCANReceive.mpas,407 :: 		if not AppCallback_EventsIdentify then
	CALL	_AppCallback_EventsIdentify
	COM	W0
	BRA NZ	L__ReceivedOnFilter1366
	GOTO	L__ReceivedOnFilter1195
L__ReceivedOnFilter1366:
;NMRAnetCANReceive.mpas,409 :: 		for j := 0 to Nodes.AllocatedCount - 1 do
; j start address is: 14 (W7)
	CLR	W7
; j end address is: 14 (W7)
L__ReceivedOnFilter1197:
; j start address is: 14 (W7)
	MOV	_Nodes+3120, W0
	DEC	W0
	MOV	W0, [W14+26]
	CP	W7, W0
	BRA LE	L__ReceivedOnFilter1367
	GOTO	L__ReceivedOnFilter1201
L__ReceivedOnFilter1367:
;NMRAnetCANReceive.mpas,411 :: 		NMRAnetNode_SetProducerEventFlags(Nodes.AllocatedList[j], EVENT_STATE_UNKOWN);
	SL	W7, #1, W1
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W0
	PUSH	W10
	MOV.B	#3, W11
	MOV	[W0], W10
	CALL	_NMRAnetNode_SetProducerEventFlags
;NMRAnetCANReceive.mpas,412 :: 		NMRAnetNode_SetConsumerEventFlags(Nodes.AllocatedList[j], EVENT_STATE_UNKOWN);
	SL	W7, #1, W1
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W0
	MOV.B	#3, W11
	MOV	[W0], W10
	CALL	_NMRAnetNode_SetConsumerEventFlags
	POP	W10
;NMRAnetCANReceive.mpas,413 :: 		end;
	MOV	[W14+26], W0
	CP	W7, W0
	BRA NZ	L__ReceivedOnFilter1368
	GOTO	L__ReceivedOnFilter1201
L__ReceivedOnFilter1368:
; j start address is: 14 (W7)
	INC	W7
; j end address is: 14 (W7)
; j end address is: 14 (W7)
	GOTO	L__ReceivedOnFilter1197
L__ReceivedOnFilter1201:
;NMRAnetCANReceive.mpas,414 :: 		end
L__ReceivedOnFilter1195:
;NMRAnetCANReceive.mpas,415 :: 		end;
	GOTO	L__ReceivedOnFilter1132
L__ReceivedOnFilter1193:
L__ReceivedOnFilter1132:
;NMRAnetCANReceive.mpas,418 :: 		end;
L__ReceivedOnFilter176:
;NMRAnetCANReceive.mpas,419 :: 		end;
	GOTO	L__ReceivedOnFilter170
L__ReceivedOnFilter173:
;NMRAnetCANReceive.mpas,420 :: 		MTI_FRAME_TYPE_DATAGRAM_ONLY_FRAME : begin
; SourceAlias start address is: 10 (W5)
	MOV	#0, W2
	MOV	#2560, W3
	MOV	[W14+34], W0
	MOV	[W14+36], W1
	CP	W0, W2
	CPB	W1, W3
	BRA Z	L__ReceivedOnFilter1369
	GOTO	L__ReceivedOnFilter1204
L__ReceivedOnFilter1369:
;NMRAnetCANReceive.mpas,421 :: 		Node := NMRAnetNode_FindByAlias( NMRAnetUtilities_ExtractDestinationCodedInMTIAlias(CANBuffer));
	PUSH	W5
	CALL	_NMRAnetUtilities_ExtractDestinationCodedInMTIAlias
	PUSH	W10
	MOV	W0, W10
	CALL	_NMRAnetNode_FindByAlias
	POP	W10
	POP	W5
; Node start address is: 4 (W2)
	MOV	W0, W2
;NMRAnetCANReceive.mpas,422 :: 		if Node <> nil then
	CP	W0, #0
	BRA NZ	L__ReceivedOnFilter1370
	GOTO	L__ReceivedOnFilter1206
L__ReceivedOnFilter1370:
;NMRAnetCANReceive.mpas,424 :: 		if NMRAnetBufferPools_AllocateDatagramBuffer(DatagramBuffer, False) then
	ADD	W14, #12, W0
	PUSH	W2
	PUSH	W5
	PUSH	W10
	CLR	W11
	MOV	W0, W10
	CALL	_NMRAnetBufferPools_AllocateDatagramBuffer
	POP	W10
	POP	W5
	POP	W2
	CP0	W0
	BRA NZ	L__ReceivedOnFilter1371
	GOTO	L__ReceivedOnFilter1209
L__ReceivedOnFilter1371:
;NMRAnetCANReceive.mpas,426 :: 		NMRAnetUtilities_DatagramBufferLink(Node, DatagramBuffer);
	PUSH	W10
	MOV	[W14+12], W11
	MOV	W2, W10
	CALL	_NMRAnetUtilities_DatagramBufferLink
	POP	W10
;NMRAnetCANReceive.mpas,427 :: 		DatagramBuffer^.mCode := BMC_DATAGRAM;
	MOV	[W14+12], W0
	ADD	W0, #6, W1
	MOV.B	#7, W0
	MOV.B	W0, [W1]
;NMRAnetCANReceive.mpas,428 :: 		DatagramBuffer^.Alias := SourceAlias;
	MOV	[W14+12], W0
	INC2	W0
	MOV	W5, [W0]
;NMRAnetCANReceive.mpas,429 :: 		DatagramBuffer^.iByteCount := CANBuffer^.DataCount;
	MOV	[W14+12], W0
	ADD	W0, #10, W1
	ADD	W10, #4, W0
	MOV.B	[W0], [W1]
;NMRAnetCANReceive.mpas,430 :: 		for i := 0 to DatagramBuffer^.iByteCount - 1 do
; i start address is: 6 (W3)
	CLR	W3
; Node end address is: 4 (W2)
; i end address is: 6 (W3)
; SourceAlias end address is: 10 (W5)
	MOV	W2, W4
L__ReceivedOnFilter1211:
; i start address is: 6 (W3)
; Node start address is: 8 (W4)
; SourceAlias start address is: 10 (W5)
	MOV	[W14+12], W0
	ADD	W0, #10, W0
	ZE	[W0], W0
	SUB	W0, #1, W2
	CP	W3, W2
	BRA LE	L__ReceivedOnFilter1372
	GOTO	L__ReceivedOnFilter1215
L__ReceivedOnFilter1372:
;NMRAnetCANReceive.mpas,431 :: 		DatagramBuffer^.DataBytes[i] := CANBuffer^.DataBytes[i];
	MOV	[W14+12], W0
	ADD	W0, #11, W0
	ADD	W0, W3, W1
	ADD	W10, #5, W0
	ADD	W0, W3, W0
	MOV.B	[W0], [W1]
	CP	W3, W2
	BRA NZ	L__ReceivedOnFilter1373
	GOTO	L__ReceivedOnFilter1215
L__ReceivedOnFilter1373:
; i start address is: 6 (W3)
	INC	W3
; i end address is: 6 (W3)
; i end address is: 6 (W3)
	GOTO	L__ReceivedOnFilter1211
L__ReceivedOnFilter1215:
;NMRAnetCANReceive.mpas,432 :: 		DatagramBuffer^.State := DatagramBuffer^.State and not CBS_PROCESSING or CBS_TRANSFER_COMPLETE;
	MOV	[W14+12], W0
	MOV.B	[W0], W0
	ZE	W0, W1
	MOV	#253, W0
	AND	W1, W0, W0
	IOR	W0, #8, W1
	MOV	[W14+12], W0
	MOV.B	W1, [W0]
;NMRAnetCANReceive.mpas,433 :: 		case DatagramBuffer^.DataBytes[0] of
	MOV	[W14+12], W0
	ADD	W0, #11, W0
	MOV.B	[W0], W2
;NMRAnetCANReceive.mpas,437 :: 		DATAGRAM_TYPE_MEMORY_CONFIGURATION : DatagramBuffer^.mCode := BMC_DATAGRAM_MEMORY_CONFIG;
	MOV.B	#32, W0
	CP.B	W2, W0
	BRA Z	L__ReceivedOnFilter1374
	GOTO	L__ReceivedOnFilter1219
L__ReceivedOnFilter1374:
; Node end address is: 8 (W4)
; SourceAlias end address is: 10 (W5)
	MOV	[W14+12], W0
	ADD	W0, #6, W1
	MOV.B	#8, W0
	MOV.B	W0, [W1]
	GOTO	L__ReceivedOnFilter1216
L__ReceivedOnFilter1219:
;NMRAnetCANReceive.mpas,438 :: 		DATAGRAM_TYPE_TRAIN_CONTROL        : DatagramBuffer^.mCode := BMC_DATAGRAM_TRACTION_CONTROL
; SourceAlias start address is: 10 (W5)
; Node start address is: 8 (W4)
	MOV.B	#48, W0
	CP.B	W2, W0
	BRA Z	L__ReceivedOnFilter1375
	GOTO	L__ReceivedOnFilter1222
L__ReceivedOnFilter1375:
; Node end address is: 8 (W4)
; SourceAlias end address is: 10 (W5)
	MOV	[W14+12], W0
	ADD	W0, #6, W1
	MOV.B	#9, W0
	MOV.B	W0, [W1]
;NMRAnetCANReceive.mpas,439 :: 		else begin
	GOTO	L__ReceivedOnFilter1216
L__ReceivedOnFilter1222:
;NMRAnetCANReceive.mpas,440 :: 		NMRAnetUtilities_DatagramBufferUnLink(Node, DatagramBuffer);
; SourceAlias start address is: 10 (W5)
; Node start address is: 8 (W4)
	PUSH	W10
	MOV	[W14+12], W11
	MOV	W4, W10
	CALL	_NMRAnetUtilities_DatagramBufferUnLink
;NMRAnetCANReceive.mpas,441 :: 		NMRAnetBufferPools_ReleaseDatagramBuffer(DatagramBuffer);
	MOV	[W14+12], W10
	CALL	_NMRAnetBufferPools_ReleaseDatagramBuffer
;NMRAnetCANReceive.mpas,442 :: 		DataBytesPtr := PCAN_DataBytes( @DATAGRAM_RESULT_REJECTED_SOURCE_DATAGRAMS_NOT_ACCEPTED);
; DataBytesPtr start address is: 0 (W0)
	MOV	#lo_addr(_DATAGRAM_RESULT_REJECTED_SOURCE_DATAGRAMS_NOT_ACCEPTED), W0
;NMRAnetCANReceive.mpas,443 :: 		NMRAnetUtilities_CreateNMRABusMessageCANBuffer(Node, @CAN_Engine.TX_DatagramRejected, MTI_DATAGRAM_REJECTED_REPLY, SourceAlias, 2, DataBytesPtr);
	MOV	#32768, W12
	MOV	#2468, W13
	MOV	#lo_addr(_CAN_Engine+44), W11
	MOV	W4, W10
; Node end address is: 8 (W4)
	PUSH	W0
; DataBytesPtr end address is: 0 (W0)
	MOV	#2, W0
	PUSH	W0
	PUSH	W5
; SourceAlias end address is: 10 (W5)
	CALL	_NMRAnetUtilities_CreateNMRABusMessageCANBuffer
	SUB	#6, W15
;NMRAnetCANReceive.mpas,444 :: 		CAN_Engine.TX_DatagramRejected.State := CAN_Engine.TX_DatagramRejected.State or BS_ALLOCATED;
	MOV	#lo_addr(_CAN_Engine+57), W0
	MOV.B	[W0], W1
	MOV	#lo_addr(_CAN_Engine+57), W0
	IOR.B	W1, #2, [W0]
;NMRAnetCANReceive.mpas,445 :: 		StartCANHighPriorityMessageEngine
	CALL	_StartCANHighPriorityMessageEngine
	POP	W10
;NMRAnetCANReceive.mpas,446 :: 		end;
L__ReceivedOnFilter1216:
;NMRAnetCANReceive.mpas,448 :: 		end else
	GOTO	L__ReceivedOnFilter1210
L__ReceivedOnFilter1209:
;NMRAnetCANReceive.mpas,450 :: 		DataBytesPtr := PCAN_DataBytes( @DATAGRAM_RESULT_REJECTED_BUFFER_FULL);
; DataBytesPtr start address is: 0 (W0)
; SourceAlias start address is: 10 (W5)
; Node start address is: 4 (W2)
	MOV	#lo_addr(_DATAGRAM_RESULT_REJECTED_BUFFER_FULL), W0
;NMRAnetCANReceive.mpas,451 :: 		NMRAnetUtilities_CreateNMRABusMessageCANBuffer(Node, @CAN_Engine.TX_DatagramRejected, MTI_DATAGRAM_REJECTED_REPLY, SourceAlias, 2, DataBytesPtr);
	MOV	#32768, W12
	MOV	#2468, W13
	MOV	#lo_addr(_CAN_Engine+44), W11
	MOV	W2, W10
; Node end address is: 4 (W2)
	PUSH	W0
; DataBytesPtr end address is: 0 (W0)
	MOV	#2, W0
	PUSH	W0
	PUSH	W5
; SourceAlias end address is: 10 (W5)
	CALL	_NMRAnetUtilities_CreateNMRABusMessageCANBuffer
	SUB	#6, W15
;NMRAnetCANReceive.mpas,452 :: 		CAN_Engine.TX_DatagramRejected.State := CAN_Engine.TX_DatagramRejected.State or BS_ALLOCATED;
	MOV	#lo_addr(_CAN_Engine+57), W0
	MOV.B	[W0], W1
	MOV	#lo_addr(_CAN_Engine+57), W0
	IOR.B	W1, #2, [W0]
;NMRAnetCANReceive.mpas,453 :: 		StartCANHighPriorityMessageEngine;
	CALL	_StartCANHighPriorityMessageEngine
;NMRAnetCANReceive.mpas,454 :: 		end
L__ReceivedOnFilter1210:
;NMRAnetCANReceive.mpas,455 :: 		end
L__ReceivedOnFilter1206:
;NMRAnetCANReceive.mpas,456 :: 		end;
	GOTO	L__ReceivedOnFilter170
L__ReceivedOnFilter1204:
;NMRAnetCANReceive.mpas,457 :: 		MTI_FRAME_TYPE_DATAGRAM_FRAME_START : begin
; SourceAlias start address is: 10 (W5)
	MOV	#0, W2
	MOV	#2816, W3
	MOV	[W14+34], W0
	MOV	[W14+36], W1
	CP	W0, W2
	CPB	W1, W3
	BRA Z	L__ReceivedOnFilter1376
	GOTO	L__ReceivedOnFilter1225
L__ReceivedOnFilter1376:
;NMRAnetCANReceive.mpas,458 :: 		Node := NMRAnetNode_FindByAlias( NMRAnetUtilities_ExtractDestinationCodedInMTIAlias(CANBuffer));
	PUSH	W5
	CALL	_NMRAnetUtilities_ExtractDestinationCodedInMTIAlias
	PUSH	W10
	MOV	W0, W10
	CALL	_NMRAnetNode_FindByAlias
	POP	W10
	POP	W5
; Node start address is: 2 (W1)
	MOV	W0, W1
;NMRAnetCANReceive.mpas,459 :: 		if Node <> nil then
	CP	W0, #0
	BRA NZ	L__ReceivedOnFilter1377
	GOTO	L__ReceivedOnFilter1227
L__ReceivedOnFilter1377:
;NMRAnetCANReceive.mpas,461 :: 		if NMRAnetUtilities_FindInProcessDatagram(Node, SourceAlias, DatagramBuffer) then
	ADD	W14, #12, W0
	PUSH	W1
	PUSH	W10
	MOV	W0, W12
	MOV	W5, W11
	MOV	W1, W10
	CALL	_NMRAnetUtilities_FindInProcessDatagram
	POP	W10
	POP	W1
	CP0	W0
	BRA NZ	L__ReceivedOnFilter1378
	GOTO	L__ReceivedOnFilter1230
L__ReceivedOnFilter1378:
; Node end address is: 2 (W1)
; SourceAlias end address is: 10 (W5)
;NMRAnetCANReceive.mpas,464 :: 		end else
	GOTO	L__ReceivedOnFilter1231
L__ReceivedOnFilter1230:
;NMRAnetCANReceive.mpas,466 :: 		if NMRAnetBufferPools_AllocateDatagramBuffer(DatagramBuffer, False) then
; SourceAlias start address is: 10 (W5)
; Node start address is: 2 (W1)
	ADD	W14, #12, W0
	PUSH	W1
	PUSH	W5
	PUSH	W10
	CLR	W11
	MOV	W0, W10
	CALL	_NMRAnetBufferPools_AllocateDatagramBuffer
	POP	W10
	POP	W5
	POP	W1
	CP0	W0
	BRA NZ	L__ReceivedOnFilter1379
	GOTO	L__ReceivedOnFilter1233
L__ReceivedOnFilter1379:
;NMRAnetCANReceive.mpas,468 :: 		NMRAnetUtilities_DatagramBufferLink(Node, DatagramBuffer);
	PUSH	W10
	MOV	[W14+12], W11
	MOV	W1, W10
; Node end address is: 2 (W1)
	CALL	_NMRAnetUtilities_DatagramBufferLink
	POP	W10
;NMRAnetCANReceive.mpas,469 :: 		DatagramBuffer^.mCode := BMC_DATAGRAM;
	MOV	[W14+12], W0
	ADD	W0, #6, W1
	MOV.B	#7, W0
	MOV.B	W0, [W1]
;NMRAnetCANReceive.mpas,470 :: 		DatagramBuffer^.State := DatagramBuffer^.State or CBS_PROCESSING;          // Not ready for the main loop to work it yet.
	MOV	[W14+12], W0
	MOV.B	[W0], W0
	ZE	W0, W0
	IOR	W0, #2, W1
	MOV	[W14+12], W0
	MOV.B	W1, [W0]
;NMRAnetCANReceive.mpas,471 :: 		DatagramBuffer^.Alias := SourceAlias;
	MOV	[W14+12], W0
	INC2	W0
	MOV	W5, [W0]
; SourceAlias end address is: 10 (W5)
;NMRAnetCANReceive.mpas,472 :: 		DatagramBuffer^.iByteCount := CANBuffer^.DataCount;
	MOV	[W14+12], W0
	ADD	W0, #10, W1
	ADD	W10, #4, W0
	MOV.B	[W0], [W1]
;NMRAnetCANReceive.mpas,473 :: 		for i := 0 to DatagramBuffer^.iByteCount - 1 do
; i start address is: 6 (W3)
	CLR	W3
; i end address is: 6 (W3)
L__ReceivedOnFilter1235:
; i start address is: 6 (W3)
	MOV	[W14+12], W0
	ADD	W0, #10, W0
	ZE	[W0], W0
	SUB	W0, #1, W2
	CP	W3, W2
	BRA LE	L__ReceivedOnFilter1380
	GOTO	L__ReceivedOnFilter1239
L__ReceivedOnFilter1380:
;NMRAnetCANReceive.mpas,474 :: 		DatagramBuffer^.DataBytes[i] := CANBuffer^.DataBytes[i];
	MOV	[W14+12], W0
	ADD	W0, #11, W0
	ADD	W0, W3, W1
	ADD	W10, #5, W0
	ADD	W0, W3, W0
	MOV.B	[W0], [W1]
	CP	W3, W2
	BRA NZ	L__ReceivedOnFilter1381
	GOTO	L__ReceivedOnFilter1239
L__ReceivedOnFilter1381:
; i start address is: 6 (W3)
	INC	W3
; i end address is: 6 (W3)
; i end address is: 6 (W3)
	GOTO	L__ReceivedOnFilter1235
L__ReceivedOnFilter1239:
;NMRAnetCANReceive.mpas,475 :: 		end
L__ReceivedOnFilter1233:
;NMRAnetCANReceive.mpas,476 :: 		end
L__ReceivedOnFilter1231:
;NMRAnetCANReceive.mpas,477 :: 		end
L__ReceivedOnFilter1227:
;NMRAnetCANReceive.mpas,478 :: 		end;
	GOTO	L__ReceivedOnFilter170
L__ReceivedOnFilter1225:
;NMRAnetCANReceive.mpas,479 :: 		MTI_FRAME_TYPE_DATAGRAM_FRAME       : begin
; SourceAlias start address is: 10 (W5)
	MOV	#0, W2
	MOV	#3072, W3
	MOV	[W14+34], W0
	MOV	[W14+36], W1
	CP	W0, W2
	CPB	W1, W3
	BRA Z	L__ReceivedOnFilter1382
	GOTO	L__ReceivedOnFilter1242
L__ReceivedOnFilter1382:
;NMRAnetCANReceive.mpas,480 :: 		Node := NMRAnetNode_FindByAlias( NMRAnetUtilities_ExtractDestinationCodedInMTIAlias(CANBuffer));
	PUSH	W5
	CALL	_NMRAnetUtilities_ExtractDestinationCodedInMTIAlias
	PUSH	W10
	MOV	W0, W10
	CALL	_NMRAnetNode_FindByAlias
	POP	W10
	POP	W5
; Node start address is: 2 (W1)
	MOV	W0, W1
;NMRAnetCANReceive.mpas,481 :: 		if Node <> nil then
	CP	W0, #0
	BRA NZ	L__ReceivedOnFilter1383
	GOTO	L__ReceivedOnFilter1244
L__ReceivedOnFilter1383:
;NMRAnetCANReceive.mpas,483 :: 		if NMRAnetUtilities_FindInProcessDatagram(Node, SourceAlias, DatagramBuffer) then
	ADD	W14, #12, W0
	PUSH	W10
	MOV	W0, W12
	MOV	W5, W11
; SourceAlias end address is: 10 (W5)
	MOV	W1, W10
; Node end address is: 2 (W1)
	CALL	_NMRAnetUtilities_FindInProcessDatagram
	POP	W10
	CP0	W0
	BRA NZ	L__ReceivedOnFilter1384
	GOTO	L__ReceivedOnFilter1247
L__ReceivedOnFilter1384:
;NMRAnetCANReceive.mpas,485 :: 		Offset := DatagramBuffer^.iByteCount;
	MOV	[W14+12], W0
	ADD	W0, #10, W0
; Offset start address is: 6 (W3)
	MOV.B	[W0], W3
;NMRAnetCANReceive.mpas,486 :: 		for i := 0 to  CANBuffer^.DataCount - 1 do
; i start address is: 8 (W4)
	CLR	W4
; i end address is: 8 (W4)
; Offset end address is: 6 (W3)
L__ReceivedOnFilter1249:
; i start address is: 8 (W4)
; Offset start address is: 6 (W3)
	ADD	W10, #4, W0
	ZE	[W0], W0
	SUB	W0, #1, W2
	CP	W4, W2
	BRA LE	L__ReceivedOnFilter1385
	GOTO	L__ReceivedOnFilter1253
L__ReceivedOnFilter1385:
;NMRAnetCANReceive.mpas,487 :: 		DatagramBuffer^.DataBytes[Offset + i] := CANBuffer^.DataBytes[i];
	MOV	[W14+12], W0
	ADD	W0, #11, W1
	ZE	W3, W0
	ADD	W0, W4, W0
	ADD	W1, W0, W1
	ADD	W10, #5, W0
	ADD	W0, W4, W0
	MOV.B	[W0], [W1]
	CP	W4, W2
	BRA NZ	L__ReceivedOnFilter1386
	GOTO	L__ReceivedOnFilter1253
L__ReceivedOnFilter1386:
; i start address is: 8 (W4)
	INC	W4
; i end address is: 8 (W4)
; i end address is: 8 (W4)
	GOTO	L__ReceivedOnFilter1249
L__ReceivedOnFilter1253:
;NMRAnetCANReceive.mpas,488 :: 		DatagramBuffer^.iByteCount := CANBuffer^.DataCount + Offset;
	MOV	[W14+12], W0
	ADD	W0, #10, W2
	ADD	W10, #4, W0
	ZE	[W0], W1
	ZE	W3, W0
; Offset end address is: 6 (W3)
	ADD	W1, W0, W0
	MOV.B	W0, [W2]
;NMRAnetCANReceive.mpas,489 :: 		end
L__ReceivedOnFilter1247:
;NMRAnetCANReceive.mpas,490 :: 		end
L__ReceivedOnFilter1244:
;NMRAnetCANReceive.mpas,491 :: 		end;
	GOTO	L__ReceivedOnFilter170
L__ReceivedOnFilter1242:
;NMRAnetCANReceive.mpas,492 :: 		MTI_FRAME_TYPE_DATAGRAM_FRAME_END   : begin
; SourceAlias start address is: 10 (W5)
	MOV	#0, W2
	MOV	#3328, W3
	MOV	[W14+34], W0
	MOV	[W14+36], W1
	CP	W0, W2
	CPB	W1, W3
	BRA Z	L__ReceivedOnFilter1387
	GOTO	L__ReceivedOnFilter1256
L__ReceivedOnFilter1387:
;NMRAnetCANReceive.mpas,493 :: 		Node := NMRAnetNode_FindByAlias( NMRAnetUtilities_ExtractDestinationCodedInMTIAlias(CANBuffer));
	PUSH	W5
	CALL	_NMRAnetUtilities_ExtractDestinationCodedInMTIAlias
	PUSH	W10
	MOV	W0, W10
	CALL	_NMRAnetNode_FindByAlias
	POP	W10
	POP	W5
; Node start address is: 2 (W1)
	MOV	W0, W1
;NMRAnetCANReceive.mpas,494 :: 		if Node <> nil then
	CP	W0, #0
	BRA NZ	L__ReceivedOnFilter1388
	GOTO	L__ReceivedOnFilter1258
L__ReceivedOnFilter1388:
;NMRAnetCANReceive.mpas,496 :: 		DatagramBuffer := nil;
	CLR	W0
	MOV	W0, [W14+12]
;NMRAnetCANReceive.mpas,497 :: 		if NMRAnetUtilities_FindInProcessDatagram(Node, SourceAlias, DatagramBuffer) then
	ADD	W14, #12, W0
	PUSH	W1
	PUSH	W10
	MOV	W0, W12
	MOV	W5, W11
	MOV	W1, W10
	CALL	_NMRAnetUtilities_FindInProcessDatagram
	POP	W10
	POP	W1
	CP0	W0
	BRA NZ	L__ReceivedOnFilter1389
	GOTO	L__ReceivedOnFilter1261
L__ReceivedOnFilter1389:
;NMRAnetCANReceive.mpas,500 :: 		Offset := DatagramBuffer^.iByteCount;
	MOV	[W14+12], W0
	ADD	W0, #10, W0
; Offset start address is: 6 (W3)
	MOV.B	[W0], W3
;NMRAnetCANReceive.mpas,501 :: 		for i := 0 to CANBuffer^.DataCount - 1 do
; i start address is: 12 (W6)
	CLR	W6
; Node end address is: 2 (W1)
; Offset end address is: 6 (W3)
; SourceAlias end address is: 10 (W5)
; i end address is: 12 (W6)
	MOV	W1, W4
L__ReceivedOnFilter1263:
; i start address is: 12 (W6)
; Offset start address is: 6 (W3)
; Node start address is: 8 (W4)
; SourceAlias start address is: 10 (W5)
	ADD	W10, #4, W0
	ZE	[W0], W0
	SUB	W0, #1, W2
	CP	W6, W2
	BRA LE	L__ReceivedOnFilter1390
	GOTO	L__ReceivedOnFilter1267
L__ReceivedOnFilter1390:
;NMRAnetCANReceive.mpas,502 :: 		DatagramBuffer^.DataBytes[Offset + i] := CANBuffer^.DataBytes[i];
	MOV	[W14+12], W0
	ADD	W0, #11, W1
	ZE	W3, W0
	ADD	W0, W6, W0
	ADD	W1, W0, W1
	ADD	W10, #5, W0
	ADD	W0, W6, W0
	MOV.B	[W0], [W1]
	CP	W6, W2
	BRA NZ	L__ReceivedOnFilter1391
	GOTO	L__ReceivedOnFilter1267
L__ReceivedOnFilter1391:
; i start address is: 12 (W6)
	INC	W6
; i end address is: 12 (W6)
; i end address is: 12 (W6)
	GOTO	L__ReceivedOnFilter1263
L__ReceivedOnFilter1267:
;NMRAnetCANReceive.mpas,503 :: 		DatagramBuffer^.iByteCount := CANBuffer^.DataCount + Offset;
	MOV	[W14+12], W0
	ADD	W0, #10, W2
	ADD	W10, #4, W0
	ZE	[W0], W1
	ZE	W3, W0
; Offset end address is: 6 (W3)
	ADD	W1, W0, W0
	MOV.B	W0, [W2]
;NMRAnetCANReceive.mpas,504 :: 		DatagramBuffer^.State := DatagramBuffer^.State and not CBS_PROCESSING or CBS_TRANSFER_COMPLETE;  // Signed Sealed so Deliver.
	MOV	[W14+12], W0
	MOV.B	[W0], W0
	ZE	W0, W1
	MOV	#253, W0
	AND	W1, W0, W0
	IOR	W0, #8, W1
	MOV	[W14+12], W0
	MOV.B	W1, [W0]
;NMRAnetCANReceive.mpas,505 :: 		case DatagramBuffer^.DataBytes[0] of
	MOV	[W14+12], W0
	ADD	W0, #11, W0
	MOV.B	[W0], W2
;NMRAnetCANReceive.mpas,509 :: 		DATAGRAM_TYPE_MEMORY_CONFIGURATION : DatagramBuffer^.mCode := BMC_DATAGRAM_MEMORY_CONFIG;
	MOV.B	#32, W0
	CP.B	W2, W0
	BRA Z	L__ReceivedOnFilter1392
	GOTO	L__ReceivedOnFilter1271
L__ReceivedOnFilter1392:
; Node end address is: 8 (W4)
; SourceAlias end address is: 10 (W5)
	MOV	[W14+12], W0
	ADD	W0, #6, W1
	MOV.B	#8, W0
	MOV.B	W0, [W1]
	GOTO	L__ReceivedOnFilter1268
L__ReceivedOnFilter1271:
;NMRAnetCANReceive.mpas,510 :: 		DATAGRAM_TYPE_TRAIN_CONTROL        : DatagramBuffer^.mCode := BMC_DATAGRAM_TRACTION_CONTROL
; SourceAlias start address is: 10 (W5)
; Node start address is: 8 (W4)
	MOV.B	#48, W0
	CP.B	W2, W0
	BRA Z	L__ReceivedOnFilter1393
	GOTO	L__ReceivedOnFilter1274
L__ReceivedOnFilter1393:
; Node end address is: 8 (W4)
; SourceAlias end address is: 10 (W5)
	MOV	[W14+12], W0
	ADD	W0, #6, W1
	MOV.B	#9, W0
	MOV.B	W0, [W1]
;NMRAnetCANReceive.mpas,511 :: 		else begin
	GOTO	L__ReceivedOnFilter1268
L__ReceivedOnFilter1274:
;NMRAnetCANReceive.mpas,512 :: 		NMRAnetUtilities_DatagramBufferUnLink(Node, DatagramBuffer);
; SourceAlias start address is: 10 (W5)
; Node start address is: 8 (W4)
	PUSH	W10
	MOV	[W14+12], W11
	MOV	W4, W10
	CALL	_NMRAnetUtilities_DatagramBufferUnLink
;NMRAnetCANReceive.mpas,513 :: 		NMRAnetBufferPools_ReleaseDatagramBuffer(DatagramBuffer);
	MOV	[W14+12], W10
	CALL	_NMRAnetBufferPools_ReleaseDatagramBuffer
;NMRAnetCANReceive.mpas,514 :: 		DataBytesPtr := PCAN_DataBytes( @DATAGRAM_RESULT_REJECTED_SOURCE_DATAGRAMS_NOT_ACCEPTED);
; DataBytesPtr start address is: 0 (W0)
	MOV	#lo_addr(_DATAGRAM_RESULT_REJECTED_SOURCE_DATAGRAMS_NOT_ACCEPTED), W0
;NMRAnetCANReceive.mpas,515 :: 		NMRAnetUtilities_CreateNMRABusMessageCANBuffer(Node, @CAN_Engine.TX_DatagramRejected, MTI_DATAGRAM_REJECTED_REPLY, SourceAlias, 2, DataBytesPtr);
	MOV	#32768, W12
	MOV	#2468, W13
	MOV	#lo_addr(_CAN_Engine+44), W11
	MOV	W4, W10
; Node end address is: 8 (W4)
	PUSH	W0
; DataBytesPtr end address is: 0 (W0)
	MOV	#2, W0
	PUSH	W0
	PUSH	W5
; SourceAlias end address is: 10 (W5)
	CALL	_NMRAnetUtilities_CreateNMRABusMessageCANBuffer
	SUB	#6, W15
;NMRAnetCANReceive.mpas,516 :: 		CAN_Engine.TX_DatagramRejected.State := CAN_Engine.TX_DatagramRejected.State or BS_ALLOCATED;
	MOV	#lo_addr(_CAN_Engine+57), W0
	MOV.B	[W0], W1
	MOV	#lo_addr(_CAN_Engine+57), W0
	IOR.B	W1, #2, [W0]
;NMRAnetCANReceive.mpas,517 :: 		StartCANHighPriorityMessageEngine;
	CALL	_StartCANHighPriorityMessageEngine
	POP	W10
;NMRAnetCANReceive.mpas,518 :: 		end;
L__ReceivedOnFilter1268:
;NMRAnetCANReceive.mpas,520 :: 		end else
	GOTO	L__ReceivedOnFilter1262
L__ReceivedOnFilter1261:
;NMRAnetCANReceive.mpas,522 :: 		DataBytesPtr := PCAN_DataBytes( @DATAGRAM_RESULT_REJECTED_BUFFER_FULL);
; DataBytesPtr start address is: 0 (W0)
; SourceAlias start address is: 10 (W5)
; Node start address is: 2 (W1)
	MOV	#lo_addr(_DATAGRAM_RESULT_REJECTED_BUFFER_FULL), W0
;NMRAnetCANReceive.mpas,523 :: 		NMRAnetUtilities_CreateNMRABusMessageCANBuffer(Node, @CAN_Engine.TX_DatagramRejected, MTI_DATAGRAM_REJECTED_REPLY, SourceAlias, 2, DataBytesPtr);
	MOV	#32768, W12
	MOV	#2468, W13
	MOV	#lo_addr(_CAN_Engine+44), W11
	MOV	W1, W10
; Node end address is: 2 (W1)
	PUSH	W0
; DataBytesPtr end address is: 0 (W0)
	MOV	#2, W0
	PUSH	W0
	PUSH	W5
; SourceAlias end address is: 10 (W5)
	CALL	_NMRAnetUtilities_CreateNMRABusMessageCANBuffer
	SUB	#6, W15
;NMRAnetCANReceive.mpas,524 :: 		CAN_Engine.TX_DatagramRejected.State := CAN_Engine.TX_DatagramRejected.State or BS_ALLOCATED;
	MOV	#lo_addr(_CAN_Engine+57), W0
	MOV.B	[W0], W1
	MOV	#lo_addr(_CAN_Engine+57), W0
	IOR.B	W1, #2, [W0]
;NMRAnetCANReceive.mpas,525 :: 		StartCANHighPriorityMessageEngine;
	CALL	_StartCANHighPriorityMessageEngine
;NMRAnetCANReceive.mpas,526 :: 		end
L__ReceivedOnFilter1262:
;NMRAnetCANReceive.mpas,527 :: 		end
L__ReceivedOnFilter1258:
;NMRAnetCANReceive.mpas,528 :: 		end;
	GOTO	L__ReceivedOnFilter170
L__ReceivedOnFilter1256:
;NMRAnetCANReceive.mpas,529 :: 		MTI_FRAME_TYPE_STREAM_SEND          : begin
	MOV	#0, W2
	MOV	#3840, W3
	MOV	[W14+34], W0
	MOV	[W14+36], W1
	CP	W0, W2
	CPB	W1, W3
	BRA Z	L__ReceivedOnFilter1394
	GOTO	L__ReceivedOnFilter1277
L__ReceivedOnFilter1394:
;NMRAnetCANReceive.mpas,530 :: 		Node := NMRAnetNode_FindByAlias( NMRAnetUtilities_ExtractDestinationCodedInMTIAlias(CANBuffer));
	CALL	_NMRAnetUtilities_ExtractDestinationCodedInMTIAlias
	MOV	W0, W10
	CALL	_NMRAnetNode_FindByAlias
;NMRAnetCANReceive.mpas,531 :: 		if Node <> nil then
	CP	W0, #0
	BRA NZ	L__ReceivedOnFilter1395
	GOTO	L__ReceivedOnFilter1279
L__ReceivedOnFilter1395:
;NMRAnetCANReceive.mpas,533 :: 		end
L__ReceivedOnFilter1279:
;NMRAnetCANReceive.mpas,534 :: 		end;
	GOTO	L__ReceivedOnFilter170
L__ReceivedOnFilter1277:
L__ReceivedOnFilter170:
;NMRAnetCANReceive.mpas,536 :: 		end
L__ReceivedOnFilter169:
;NMRAnetCANReceive.mpas,537 :: 		end;
L__ReceivedOnFilter165:
;NMRAnetCANReceive.mpas,539 :: 		if TMR4 > MaxTime_RX then MaxTime_RX := TMR4;
	MOV	TMR4, W1
	MOV	#lo_addr(_MaxTime_RX), W0
	CP	W1, [W0]
	BRA GTU	L__ReceivedOnFilter1396
	GOTO	L__ReceivedOnFilter1282
L__ReceivedOnFilter1396:
	MOV	TMR4, WREG
	MOV	W0, _MaxTime_RX
L__ReceivedOnFilter1282:
;NMRAnetCANReceive.mpas,540 :: 		end;
L_end_ReceivedOnFilter1:
	POP	W13
	POP	W12
	POP	W11
	POP	W10
	ULNK
	RETURN
; end of _ReceivedOnFilter1

_ReceivedOnFilter2:

;NMRAnetCANReceive.mpas,543 :: 		begin
;NMRAnetCANReceive.mpas,545 :: 		end;
L_end_ReceivedOnFilter2:
	RETURN
; end of _ReceivedOnFilter2

_ReceivedOnFilter3:

;NMRAnetCANReceive.mpas,548 :: 		begin
;NMRAnetCANReceive.mpas,550 :: 		end;
L_end_ReceivedOnFilter3:
	RETURN
; end of _ReceivedOnFilter3

_ReceivedOnFilter4:

;NMRAnetCANReceive.mpas,553 :: 		begin
;NMRAnetCANReceive.mpas,555 :: 		end;
L_end_ReceivedOnFilter4:
	RETURN
; end of _ReceivedOnFilter4

_ReceivedOnFilter5:

;NMRAnetCANReceive.mpas,558 :: 		begin
;NMRAnetCANReceive.mpas,560 :: 		end;
L_end_ReceivedOnFilter5:
	RETURN
; end of _ReceivedOnFilter5

_ReceivedOnFilter6:

;NMRAnetCANReceive.mpas,563 :: 		begin
;NMRAnetCANReceive.mpas,565 :: 		end;
L_end_ReceivedOnFilter6:
	RETURN
; end of _ReceivedOnFilter6
