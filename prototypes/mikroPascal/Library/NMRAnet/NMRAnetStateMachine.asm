
_NMRAnetStateMachine_TrySendAbbreviatedCDI:
	LNK	#22

;NMRAnetStateMachine.mpas,89 :: 		begin
;NMRAnetStateMachine.mpas,90 :: 		Result := TransmitNMRABusLayerMsg(Node, @CANBuffer, MTI_SIMPLE_NODE_INFO_REQUEST, DestinationAliasID, 0, @DataBytes, False);
	PUSH	W11
	PUSH	W12
	PUSH	W13
	ADD	W14, #0, W0
	ADD	W14, #14, W1
	CLR	W0
	PUSH	W0
	PUSH	W1
	CLR	W0
	PUSH	W0
	PUSH	W11
	MOV	#32768, W12
	MOV	#2526, W13
	MOV	W0, W11
	CALL	_TransmitNMRABusLayerMsg
	SUB	#8, W15
; Result start address is: 2 (W1)
	MOV	W0, W1
;NMRAnetStateMachine.mpas,91 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetStateMachine_TrySendAbbreviatedCDI:
	POP	W13
	POP	W12
	POP	W11
	ULNK
	RETURN
; end of _NMRAnetStateMachine_TrySendAbbreviatedCDI

_NMRAnetStateMachine_TrySendIdentifyProducer:
	LNK	#14

;NMRAnetStateMachine.mpas,104 :: 		begin
;NMRAnetStateMachine.mpas,105 :: 		Result := TransmitNMRABusLayerMsg(Node, @CANBuffer, MTI_PRODUCER_IDENDIFY, 0, EVENT_ARRAY_LENGTH, PCAN_DataBytes( EventID), False)
	PUSH	W11
	PUSH	W12
	PUSH	W13
	ADD	W14, #0, W0
	CLR	W0
	PUSH	W0
	PUSH	W11
	MOV	#8, W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	MOV	#16384, W12
	MOV	#2449, W13
	MOV	W0, W11
	CALL	_TransmitNMRABusLayerMsg
	SUB	#8, W15
; Result start address is: 2 (W1)
	MOV	W0, W1
;NMRAnetStateMachine.mpas,106 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetStateMachine_TrySendIdentifyProducer:
	POP	W13
	POP	W12
	POP	W11
	ULNK
	RETURN
; end of _NMRAnetStateMachine_TrySendIdentifyProducer

_NMRAnetStateMachine_TrySendIdentifyConsumer:
	LNK	#14

;NMRAnetStateMachine.mpas,119 :: 		begin
;NMRAnetStateMachine.mpas,120 :: 		Result := TransmitNMRABusLayerMsg(Node, @CANBuffer, MTI_CONSUMER_IDENTIFY, 0, EVENT_ARRAY_LENGTH,  PCAN_DataBytes( EventID), False);
	PUSH	W11
	PUSH	W12
	PUSH	W13
	ADD	W14, #0, W0
	CLR	W0
	PUSH	W0
	PUSH	W11
	MOV	#8, W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	MOV	#16384, W12
	MOV	#2447, W13
	MOV	W0, W11
	CALL	_TransmitNMRABusLayerMsg
	SUB	#8, W15
; Result start address is: 2 (W1)
	MOV	W0, W1
;NMRAnetStateMachine.mpas,121 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetStateMachine_TrySendIdentifyConsumer:
	POP	W13
	POP	W12
	POP	W11
	ULNK
	RETURN
; end of _NMRAnetStateMachine_TrySendIdentifyConsumer

_NMRAnetStateMachine_TrySendIdentifyEvents:
	LNK	#14

;NMRAnetStateMachine.mpas,134 :: 		begin
;NMRAnetStateMachine.mpas,135 :: 		Result := TransmitNMRABusLayerMsg(Node, @CANBuffer, MTI_EVENTS_IDENTIFY, 0, 0, nil, False);
	PUSH	W11
	PUSH	W12
	PUSH	W13
	ADD	W14, #0, W0
	MOV	#0, W12
	MOV	#2455, W13
	MOV	W0, W11
	CLR	W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	CALL	_TransmitNMRABusLayerMsg
	SUB	#8, W15
; Result start address is: 2 (W1)
	MOV	W0, W1
;NMRAnetStateMachine.mpas,136 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetStateMachine_TrySendIdentifyEvents:
	POP	W13
	POP	W12
	POP	W11
	ULNK
	RETURN
; end of _NMRAnetStateMachine_TrySendIdentifyEvents

_NMRAnetStateMachine_TrySendAliasMapEnquiry:
	LNK	#14

;NMRAnetStateMachine.mpas,149 :: 		begin
;NMRAnetStateMachine.mpas,150 :: 		Result := TransmitCANLayerMsg(Node, @CANBuffer, MTI_AME);
	PUSH	W11
	PUSH	W12
	PUSH	W13
	ADD	W14, #0, W0
	MOV	#8192, W12
	MOV	#112, W13
	MOV	W0, W11
	CALL	_TransmitCANLayerMsg
; Result start address is: 2 (W1)
	MOV	W0, W1
;NMRAnetStateMachine.mpas,151 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetStateMachine_TrySendAliasMapEnquiry:
	POP	W13
	POP	W12
	POP	W11
	ULNK
	RETURN
; end of _NMRAnetStateMachine_TrySendAliasMapEnquiry

_NMRAnetStateMachine_TrySendAliasMapReset:
	LNK	#14

;NMRAnetStateMachine.mpas,164 :: 		begin
;NMRAnetStateMachine.mpas,165 :: 		Result := TransmitCANLayerMsg(Node, @CANBuffer, MTI_AMR);
	PUSH	W11
	PUSH	W12
	PUSH	W13
	ADD	W14, #0, W0
	MOV	#12288, W12
	MOV	#112, W13
	MOV	W0, W11
	CALL	_TransmitCANLayerMsg
; Result start address is: 2 (W1)
	MOV	W0, W1
;NMRAnetStateMachine.mpas,166 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetStateMachine_TrySendAliasMapReset:
	POP	W13
	POP	W12
	POP	W11
	ULNK
	RETURN
; end of _NMRAnetStateMachine_TrySendAliasMapReset

_NMRAnetStateMachine_TrySendVerifyNodeID:
	LNK	#22

;NMRAnetStateMachine.mpas,180 :: 		begin
;NMRAnetStateMachine.mpas,181 :: 		if DestinationAliasID <> 0 then
	PUSH	W11
	PUSH	W12
	PUSH	W13
	CP	W11, #0
	BRA NZ	L__NMRAnetStateMachine_TrySendVerifyNodeID583
	GOTO	L__NMRAnetStateMachine_TrySendVerifyNodeID8
L__NMRAnetStateMachine_TrySendVerifyNodeID583:
;NMRAnetStateMachine.mpas,182 :: 		Result := TransmitNMRABusLayerMsg(Node, @CANBuffer, MTI_VERIFY_NODE_ID_NUMBER_DEST, DestinationAliasID, 0, @DataBytes, False)
	ADD	W14, #8, W0
	ADD	W14, #0, W1
	CLR	W0
	PUSH	W0
	PUSH	W1
	CLR	W0
	PUSH	W0
	PUSH	W11
	MOV	#32768, W12
	MOV	#2376, W13
	MOV	W0, W11
	CALL	_TransmitNMRABusLayerMsg
	SUB	#8, W15
; Result start address is: 2 (W1)
	MOV	W0, W1
; Result end address is: 2 (W1)
	GOTO	L__NMRAnetStateMachine_TrySendVerifyNodeID9
;NMRAnetStateMachine.mpas,183 :: 		else
L__NMRAnetStateMachine_TrySendVerifyNodeID8:
;NMRAnetStateMachine.mpas,184 :: 		Result := TransmitNMRABusLayerMsg(Node, @CANBuffer, MTI_VERIFY_NODE_ID_NUMBER, 0, 0, nil, False);
	ADD	W14, #8, W0
	MOV	#0, W12
	MOV	#2377, W13
	MOV	W0, W11
	CLR	W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	CALL	_TransmitNMRABusLayerMsg
	SUB	#8, W15
; Result start address is: 2 (W1)
	MOV	W0, W1
; Result end address is: 2 (W1)
L__NMRAnetStateMachine_TrySendVerifyNodeID9:
;NMRAnetStateMachine.mpas,185 :: 		end;
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRAnetStateMachine_TrySendVerifyNodeID:
	POP	W13
	POP	W12
	POP	W11
	ULNK
	RETURN
; end of _NMRAnetStateMachine_TrySendVerifyNodeID

_NMRAnetStateMachine_TrySendDatagram:
	LNK	#10

;NMRAnetStateMachine.mpas,202 :: 		begin
;NMRAnetStateMachine.mpas,203 :: 		Result := False;
	PUSH	W13
	CLR	W0
	MOV	W0, [W14+0]
;NMRAnetStateMachine.mpas,204 :: 		if NMRABusBufferAvailable then
	CALL	_NMRABusBufferAvailable
	CP0	W0
	BRA NZ	L__NMRAnetStateMachine_TrySendDatagram585
	GOTO	L__NMRAnetStateMachine_TrySendDatagram12
L__NMRAnetStateMachine_TrySendDatagram585:
;NMRAnetStateMachine.mpas,210 :: 		if Datagram^.iByteCount <= 8 then                                           // Single Frame Datagram
	ADD	W12, #10, W0
	MOV.B	[W0], W0
	CP.B	W0, #8
	BRA LEU	L__NMRAnetStateMachine_TrySendDatagram586
	GOTO	L__NMRAnetStateMachine_TrySendDatagram15
L__NMRAnetStateMachine_TrySendDatagram586:
;NMRAnetStateMachine.mpas,213 :: 		for i := 0 to Datagram^.iByteCount - 1 do
; i start address is: 6 (W3)
	CLR	W3
; i end address is: 6 (W3)
L__NMRAnetStateMachine_TrySendDatagram17:
; i start address is: 6 (W3)
	ADD	W12, #10, W0
	ZE	[W0], W0
	SUB	W0, #1, W2
	CP	W3, W2
	BRA LE	L__NMRAnetStateMachine_TrySendDatagram587
	GOTO	L__NMRAnetStateMachine_TrySendDatagram21
L__NMRAnetStateMachine_TrySendDatagram587:
;NMRAnetStateMachine.mpas,214 :: 		DataBytes[i] := Datagram^.DataBytes[i];
	ADD	W14, #2, W0
	ADD	W0, W3, W1
	ADD	W12, #11, W0
	ADD	W0, W3, W0
	MOV.B	[W0], [W1]
	CP	W3, W2
	BRA NZ	L__NMRAnetStateMachine_TrySendDatagram588
	GOTO	L__NMRAnetStateMachine_TrySendDatagram21
L__NMRAnetStateMachine_TrySendDatagram588:
; i start address is: 0 (W0)
	ADD	W3, #1, W0
; i end address is: 6 (W3)
	MOV	W0, W3
; i end address is: 0 (W0)
	GOTO	L__NMRAnetStateMachine_TrySendDatagram17
L__NMRAnetStateMachine_TrySendDatagram21:
;NMRAnetStateMachine.mpas,215 :: 		TransmitNMRABusLayerMsg(Node, CANBuffer, MTI_FRAME_TYPE_DATAGRAM_ONLY_FRAME, Datagram^.Alias, Datagram^.iByteCount, @DataBytes, True);
	ADD	W12, #2, W3
	ADD	W12, #10, W0
	ADD	W14, #2, W2
	MOV.B	[W0], W1
	PUSH	W12
	PUSH	W11
	MOV	#0, W12
	MOV	#2560, W13
	MOV	#65535, W0
	PUSH	W0
	PUSH	W2
	ZE	W1, W0
	PUSH	W0
	PUSH	[W3]
	CALL	_TransmitNMRABusLayerMsg
	SUB	#8, W15
	POP	W11
	POP	W12
;NMRAnetStateMachine.mpas,216 :: 		Result := True;
	MOV	#65535, W0
	MOV	W0, [W14+0]
;NMRAnetStateMachine.mpas,217 :: 		end else
	GOTO	L__NMRAnetStateMachine_TrySendDatagram16
L__NMRAnetStateMachine_TrySendDatagram15:
;NMRAnetStateMachine.mpas,220 :: 		if Datagram^.Tag = 0 then
	ADD	W12, #8, W0
	MOV	[W0], W0
	CP	W0, #0
	BRA Z	L__NMRAnetStateMachine_TrySendDatagram589
	GOTO	L__NMRAnetStateMachine_TrySendDatagram23
L__NMRAnetStateMachine_TrySendDatagram589:
;NMRAnetStateMachine.mpas,222 :: 		MTI := MTI_FRAME_TYPE_DATAGRAM_FRAME_START  {$IFDEF TRACE_DATAGRAM_SEND};UART1_Write_Text('MTI_DATAGRAM_FRAME_START_SEND'+LF);{$ENDIF}
; MTI start address is: 8 (W4)
	MOV	#0, W4
	MOV	#2816, W5
;NMRAnetStateMachine.mpas,223 :: 		end
; MTI end address is: 8 (W4)
	GOTO	L__NMRAnetStateMachine_TrySendDatagram24
;NMRAnetStateMachine.mpas,224 :: 		else
L__NMRAnetStateMachine_TrySendDatagram23:
;NMRAnetStateMachine.mpas,225 :: 		if Datagram^.iByteCount - Datagram^.Tag > 8 then
	ADD	W12, #10, W0
	ADD	W12, #8, W1
	MOV.B	[W0], W0
	ZE	W0, W0
	SUB	W0, [W1], W0
	CP	W0, #8
	BRA GTU	L__NMRAnetStateMachine_TrySendDatagram590
	GOTO	L__NMRAnetStateMachine_TrySendDatagram26
L__NMRAnetStateMachine_TrySendDatagram590:
;NMRAnetStateMachine.mpas,227 :: 		MTI := MTI_FRAME_TYPE_DATAGRAM_FRAME  {$IFDEF TRACE_DATAGRAM_SEND};UART1_Write_Text('MTI_DATAGRAM_FRAME_SEND'+LF);{$ENDIF}
; MTI start address is: 8 (W4)
	MOV	#0, W4
	MOV	#3072, W5
;NMRAnetStateMachine.mpas,228 :: 		end
; MTI end address is: 8 (W4)
	GOTO	L__NMRAnetStateMachine_TrySendDatagram27
;NMRAnetStateMachine.mpas,229 :: 		else begin
L__NMRAnetStateMachine_TrySendDatagram26:
;NMRAnetStateMachine.mpas,230 :: 		MTI := MTI_FRAME_TYPE_DATAGRAM_FRAME_END; {$IFDEF TRACE_DATAGRAM_SEND}UART1_Write_Text('MTI_DATAGRAM_FRAME_END_SEND'+LF);{$ENDIF}
; MTI start address is: 8 (W4)
	MOV	#0, W4
	MOV	#3328, W5
;NMRAnetStateMachine.mpas,231 :: 		Result := True
	MOV	#65535, W0
	MOV	W0, [W14+0]
; MTI end address is: 8 (W4)
;NMRAnetStateMachine.mpas,232 :: 		end;
L__NMRAnetStateMachine_TrySendDatagram27:
; MTI start address is: 8 (W4)
; MTI end address is: 8 (W4)
L__NMRAnetStateMachine_TrySendDatagram24:
;NMRAnetStateMachine.mpas,234 :: 		i := 0;
; MTI start address is: 8 (W4)
; i start address is: 18 (W9)
	CLR	W9
; i end address is: 18 (W9)
; MTI end address is: 8 (W4)
;NMRAnetStateMachine.mpas,235 :: 		while (Datagram^.Tag < Datagram^.iByteCount) and (i < 8) do               // Copy 8 Data Byte, or as many that are left to the buffer
L__NMRAnetStateMachine_TrySendDatagram29:
; i start address is: 18 (W9)
; MTI start address is: 8 (W4)
	ADD	W12, #8, W1
	ADD	W12, #10, W0
	MOV.B	[W0], W0
	MOV	[W1], W1
	ZE	W0, W0
	CP	W1, W0
	CLR	W1
	BRA GEU	L__NMRAnetStateMachine_TrySendDatagram591
	COM	W1
L__NMRAnetStateMachine_TrySendDatagram591:
	CP	W9, #8
	CLR	W0
	BRA GE	L__NMRAnetStateMachine_TrySendDatagram592
	COM	W0
L__NMRAnetStateMachine_TrySendDatagram592:
	AND	W1, W0, W0
	BRA NZ	L__NMRAnetStateMachine_TrySendDatagram593
	GOTO	L__NMRAnetStateMachine_TrySendDatagram30
L__NMRAnetStateMachine_TrySendDatagram593:
;NMRAnetStateMachine.mpas,237 :: 		DataBytes[i] := Datagram^.DataBytes[Datagram^.Tag];
	ADD	W14, #2, W0
	ADD	W0, W9, W2
	ADD	W12, #11, W1
	ADD	W12, #8, W0
	ADD	W1, [W0], W0
	MOV.B	[W0], [W2]
;NMRAnetStateMachine.mpas,238 :: 		Inc(i);
; i start address is: 4 (W2)
	ADD	W9, #1, W2
; i end address is: 18 (W9)
;NMRAnetStateMachine.mpas,239 :: 		Inc(Datagram^.Tag);
	ADD	W12, #8, W1
	MOV	[W1], W0
	INC	W0
	MOV	W0, [W1]
;NMRAnetStateMachine.mpas,240 :: 		end;
; i end address is: 4 (W2)
	MOV	W2, W9
	GOTO	L__NMRAnetStateMachine_TrySendDatagram29
L__NMRAnetStateMachine_TrySendDatagram30:
;NMRAnetStateMachine.mpas,242 :: 		TransmitNMRABusLayerMsg(Node, CANBuffer, MTI, Datagram^.Alias, i, @DataBytes, True);
; i start address is: 18 (W9)
	ADD	W12, #2, W2
	ADD	W14, #2, W1
	PUSH	W12
; MTI end address is: 8 (W4)
	PUSH	W11
	MOV.D	W4, W12
	MOV	#65535, W0
	PUSH	W0
	PUSH	W1
	PUSH	W9
; i end address is: 18 (W9)
	PUSH	[W2]
	CALL	_TransmitNMRABusLayerMsg
	SUB	#8, W15
	POP	W11
	POP	W12
;NMRAnetStateMachine.mpas,244 :: 		end;
L__NMRAnetStateMachine_TrySendDatagram16:
;NMRAnetStateMachine.mpas,245 :: 		end
L__NMRAnetStateMachine_TrySendDatagram12:
;NMRAnetStateMachine.mpas,246 :: 		end;
	MOV	[W14+0], W0
L_end_NMRAnetStateMachine_TrySendDatagram:
	POP	W13
	ULNK
	RETURN
; end of _NMRAnetStateMachine_TrySendDatagram

NMRAnetStateMachine_ReleaseBuffers:

;NMRAnetStateMachine.mpas,260 :: 		begin
;NMRAnetStateMachine.mpas,261 :: 		while Node^.BaseBuffers <> nil do
	PUSH	W11
L_NMRAnetStateMachine_ReleaseBuffers35:
	ADD	W10, #26, W0
	MOV	[W0], W0
	CP	W0, #0
	BRA NZ	L_NMRAnetStateMachine_ReleaseBuffers595
	GOTO	L_NMRAnetStateMachine_ReleaseBuffers36
L_NMRAnetStateMachine_ReleaseBuffers595:
;NMRAnetStateMachine.mpas,263 :: 		BaseTemp := Node^.BaseBuffers;
	ADD	W10, #26, W0
; BaseTemp start address is: 8 (W4)
	MOV	[W0], W4
;NMRAnetStateMachine.mpas,264 :: 		NMRAnetUtilities_BaseBufferUnLink(Node, BaseTemp);
	MOV	[W0], W11
	CALL	_NMRAnetUtilities_BaseBufferUnLink
;NMRAnetStateMachine.mpas,265 :: 		NMRAnetBufferPools_ReleaseBaseBuffer(BaseTemp);
	PUSH	W10
; BaseTemp end address is: 8 (W4)
	MOV	W4, W10
	CALL	_NMRAnetBufferPools_ReleaseBaseBuffer
	POP	W10
;NMRAnetStateMachine.mpas,266 :: 		end;
	GOTO	L_NMRAnetStateMachine_ReleaseBuffers35
L_NMRAnetStateMachine_ReleaseBuffers36:
;NMRAnetStateMachine.mpas,267 :: 		while Node^.DatagramBuffers <> nil do
L_NMRAnetStateMachine_ReleaseBuffers40:
	ADD	W10, #28, W0
	MOV	[W0], W0
	CP	W0, #0
	BRA NZ	L_NMRAnetStateMachine_ReleaseBuffers596
	GOTO	L_NMRAnetStateMachine_ReleaseBuffers41
L_NMRAnetStateMachine_ReleaseBuffers596:
;NMRAnetStateMachine.mpas,269 :: 		DatagramTemp := Node^.DatagramBuffers;
	ADD	W10, #28, W0
; DatagramTemp start address is: 8 (W4)
	MOV	[W0], W4
;NMRAnetStateMachine.mpas,270 :: 		NMRAnetUtilities_DatagramBufferUnLink(Node, DatagramTemp);
	MOV	[W0], W11
	CALL	_NMRAnetUtilities_DatagramBufferUnLink
;NMRAnetStateMachine.mpas,271 :: 		NMRAnetBufferPools_ReleaseDatagramBuffer(DatagramTemp);
	PUSH	W10
; DatagramTemp end address is: 8 (W4)
	MOV	W4, W10
	CALL	_NMRAnetBufferPools_ReleaseDatagramBuffer
	POP	W10
;NMRAnetStateMachine.mpas,272 :: 		end;
	GOTO	L_NMRAnetStateMachine_ReleaseBuffers40
L_NMRAnetStateMachine_ReleaseBuffers41:
;NMRAnetStateMachine.mpas,273 :: 		end;
L_end_ReleaseBuffers:
	POP	W11
	RETURN
; end of NMRAnetStateMachine_ReleaseBuffers

_NMRAnetStateMachine_InitializeNode:
	LNK	#0

;NMRAnetStateMachine.mpas,286 :: 		begin
;NMRAnetStateMachine.mpas,288 :: 		Node^.BaseBuffers := nil;
; NodeID_LO start address is: 4 (W2)
	MOV	[W14-10], W2
	MOV	[W14-8], W3
	ADD	W10, #26, W1
	CLR	W0
	MOV	W0, [W1]
;NMRAnetStateMachine.mpas,289 :: 		Node^.DatagramBuffers := nil;
	ADD	W10, #28, W1
	CLR	W0
	MOV	W0, [W1]
;NMRAnetStateMachine.mpas,290 :: 		Node^.State := 0;
	CLR	W0
	MOV.B	W0, [W10]
;NMRAnetStateMachine.mpas,291 :: 		Node^.MsgFlags := 0;
	ADD	W10, #22, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,292 :: 		Node^.ParentAlias := 0;
	MOV	#34, W0
	ADD	W10, W0, W1
	CLR	W0
	MOV	W0, [W1]
;NMRAnetStateMachine.mpas,293 :: 		Node^.ChildAlias := 0;
	MOV	#36, W0
	ADD	W10, W0, W1
	CLR	W0
	MOV	W0, [W1]
;NMRAnetStateMachine.mpas,294 :: 		if (NodeID_HI <> 0) and (NodeID_LO <> 0) then
	CP	W11, #0
	CPB	W12, #0
	CLR	W1
	BRA Z	L__NMRAnetStateMachine_InitializeNode598
	COM	W1
L__NMRAnetStateMachine_InitializeNode598:
	CP	W2, #0
	CPB	W3, #0
	CLR	W0
	BRA Z	L__NMRAnetStateMachine_InitializeNode599
	COM	W0
L__NMRAnetStateMachine_InitializeNode599:
	AND	W1, W0, W0
	BRA NZ	L__NMRAnetStateMachine_InitializeNode600
	GOTO	L__NMRAnetStateMachine_InitializeNode46
L__NMRAnetStateMachine_InitializeNode600:
;NMRAnetStateMachine.mpas,297 :: 		Node^.Info.ID[0] := NodeID_LO;
	ADD	W10, #2, W0
	MOV.D	W2, [W0]
; NodeID_LO end address is: 4 (W2)
;NMRAnetStateMachine.mpas,298 :: 		Node^.Info.ID[1] := NodeID_HI;
	ADD	W10, #2, W0
	ADD	W0, #4, W0
	MOV	W11, [W0++]
	MOV	W12, [W0--]
;NMRAnetStateMachine.mpas,299 :: 		Node^.Info.Seed := Node^.Info.ID;
	ADD	W10, #2, W0
	ADD	W0, #8, W1
	REPEAT	#3
	MOV	[W0++], [W1++]
;NMRAnetStateMachine.mpas,300 :: 		Node^.Login.TimeCounter := 0;
	ADD	W10, #20, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,301 :: 		Node^.Login.iCID := 0;
	ADD	W10, #20, W0
	ADD	W0, #1, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,302 :: 		end;
L__NMRAnetStateMachine_InitializeNode46:
;NMRAnetStateMachine.mpas,303 :: 		for i := 0 to MAX_EVENTS_CONSUMED_BIT_BYTES - 1 do
; i start address is: 4 (W2)
	CLR	W2
; i end address is: 4 (W2)
L__NMRAnetStateMachine_InitializeNode49:
;NMRAnetStateMachine.mpas,304 :: 		Node^.EventsConsumedFlags[i] := 0;
; i start address is: 4 (W2)
	ADD	W10, #24, W0
	ADD	W0, W2, W1
	CLR	W0
	MOV.B	W0, [W1]
	CP	W2, #0
	BRA NZ	L__NMRAnetStateMachine_InitializeNode601
	GOTO	L__NMRAnetStateMachine_InitializeNode52
L__NMRAnetStateMachine_InitializeNode601:
; i start address is: 4 (W2)
	INC	W2
; i end address is: 4 (W2)
; i end address is: 4 (W2)
	GOTO	L__NMRAnetStateMachine_InitializeNode49
L__NMRAnetStateMachine_InitializeNode52:
;NMRAnetStateMachine.mpas,305 :: 		for i := 0 to MAX_EVENTS_PRODUCED_BIT_BYTES - 1 do
; i start address is: 4 (W2)
	CLR	W2
; i end address is: 4 (W2)
L__NMRAnetStateMachine_InitializeNode54:
;NMRAnetStateMachine.mpas,306 :: 		Node^.EventsProducedFlags[i] := 0;
; i start address is: 4 (W2)
	ADD	W10, #23, W0
	ADD	W0, W2, W1
	CLR	W0
	MOV.B	W0, [W1]
	CP	W2, #0
	BRA NZ	L__NMRAnetStateMachine_InitializeNode602
	GOTO	L__NMRAnetStateMachine_InitializeNode57
L__NMRAnetStateMachine_InitializeNode602:
; i start address is: 4 (W2)
	INC	W2
; i end address is: 4 (W2)
; i end address is: 4 (W2)
	GOTO	L__NMRAnetStateMachine_InitializeNode54
L__NMRAnetStateMachine_InitializeNode57:
;NMRAnetStateMachine.mpas,307 :: 		Node^.iStateMachine := STATE_NMRABUS_START;
	ADD	W10, #25, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,308 :: 		Node^.ParentAlias := nil;
	MOV	#34, W0
	ADD	W10, W0, W1
	CLR	W0
	MOV	W0, [W1]
;NMRAnetStateMachine.mpas,309 :: 		Node^.ChildAlias := nil;
	MOV	#36, W0
	ADD	W10, W0, W1
	CLR	W0
	MOV	W0, [W1]
;NMRAnetStateMachine.mpas,310 :: 		Node^.LeftSibling := nil;
	MOV	#38, W0
	ADD	W10, W0, W1
	CLR	W0
	MOV	W0, [W1]
;NMRAnetStateMachine.mpas,311 :: 		Node^.RightSibling := nil;
	MOV	#40, W0
	ADD	W10, W0, W1
	CLR	W0
	MOV	W0, [W1]
;NMRAnetStateMachine.mpas,312 :: 		Node^.Login.TimeCounter := 0;
	ADD	W10, #20, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,313 :: 		Node^.Login.iCID := 0;
	ADD	W10, #20, W0
	ADD	W0, #1, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,317 :: 		end;
L_end_NMRAnetStateMachine_InitializeNode:
	ULNK
	RETURN
; end of _NMRAnetStateMachine_InitializeNode

_NMRAnetStateMachine_Initialize:

;NMRAnetStateMachine.mpas,328 :: 		begin
;NMRAnetStateMachine.mpas,330 :: 		dsPIC33_CAN_Initialize;
	CALL	_dsPIC33_CAN_Initialize
;NMRAnetStateMachine.mpas,332 :: 		CANStorage_Initialize;
	CALL	_CANStorage_Initialize
;NMRAnetStateMachine.mpas,333 :: 		NMRAnetDatagrams_Initialize;
	CALL	_NMRAnetDatagrams_Initialize
;NMRAnetStateMachine.mpas,334 :: 		NMRAnetCANReceive_Initialize;
	CALL	_NMRAnetCANReceive_Initialize
;NMRAnetStateMachine.mpas,335 :: 		NMRAnetBufferPools_Initialize;
	CALL	_NMRAnetBufferPools_Initialize
;NMRAnetStateMachine.mpas,336 :: 		NMRAnetAppCallbacks_Initialize;
	PUSH.D	W12
	PUSH.D	W10
	CALL	_NMRAnetAppCallbacks_Initialize
	POP.D	W10
	POP.D	W12
;NMRAnetStateMachine.mpas,337 :: 		NMRAnetNode_Initialize(PhysicalNodeID_HI, PhysicalNodeID_Lo);
	CALL	_NMRAnetNode_Initialize
;NMRAnetStateMachine.mpas,338 :: 		end;
L_end_NMRAnetStateMachine_Initialize:
	RETURN
; end of _NMRAnetStateMachine_Initialize

_NMRAnetStateMachine_100ms_Timer:

;NMRAnetStateMachine.mpas,349 :: 		begin
;NMRAnetStateMachine.mpas,350 :: 		Inc(Node^.Login.TimeCounter);
	ADD	W10, #20, W1
	MOV.B	[W1], W0
	ZE	W0, W0
	INC	W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,351 :: 		end;
L_end_NMRAnetStateMachine_100ms_Timer:
	RETURN
; end of _NMRAnetStateMachine_100ms_Timer

_TransmitCANLayerMsg:

;NMRAnetStateMachine.mpas,362 :: 		begin
;NMRAnetStateMachine.mpas,363 :: 		Result := False;
; Result start address is: 2 (W1)
	CLR	W1
;NMRAnetStateMachine.mpas,364 :: 		if CAN_Engine.TX_CANBuffer.State and BS_ALLOCATED = 0 then
	MOV	#lo_addr(_CAN_Engine+15), W0
	ZE	[W0], W0
	AND	W0, #2, W0
	CP	W0, #0
	BRA Z	L__TransmitCANLayerMsg606
	GOTO	L__TransmitCANLayerMsg572
L__TransmitCANLayerMsg606:
; Result end address is: 2 (W1)
;NMRAnetStateMachine.mpas,366 :: 		NMRAnetUtilities_CreateCANControlFrameCANBuffer(Node, Buffer, VariableField);
	CALL	_NMRAnetUtilities_CreateCANControlFrameCANBuffer
;NMRAnetStateMachine.mpas,367 :: 		CAN_Engine.TX_CANBuffer := Buffer^;
	MOV	#lo_addr(_CAN_Engine+2), W1
	MOV	W11, W0
	REPEAT	#6
	MOV	[W0++], [W1++]
;NMRAnetStateMachine.mpas,368 :: 		CAN_Engine.TX_CANBuffer.State := CAN_Engine.TX_CANBuffer.State or BS_ALLOCATED;
	MOV	#lo_addr(_CAN_Engine+15), W0
	MOV.B	[W0], W1
	MOV	#lo_addr(_CAN_Engine+15), W0
	IOR.B	W1, #2, [W0]
;NMRAnetStateMachine.mpas,369 :: 		StartCANMessageEngine();
	CALL	_StartCANMessageEngine
;NMRAnetStateMachine.mpas,370 :: 		Result := True
; Result start address is: 2 (W1)
	MOV	#65535, W1
; Result end address is: 2 (W1)
;NMRAnetStateMachine.mpas,371 :: 		end;
	GOTO	L__TransmitCANLayerMsg62
L__TransmitCANLayerMsg572:
;NMRAnetStateMachine.mpas,364 :: 		if CAN_Engine.TX_CANBuffer.State and BS_ALLOCATED = 0 then
;NMRAnetStateMachine.mpas,371 :: 		end;
L__TransmitCANLayerMsg62:
;NMRAnetStateMachine.mpas,372 :: 		end;
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_TransmitCANLayerMsg:
	RETURN
; end of _TransmitCANLayerMsg

NMRAnetStateMachine_CANBusBufferAvailable:

;NMRAnetStateMachine.mpas,383 :: 		begin
;NMRAnetStateMachine.mpas,384 :: 		Result := CAN_Engine.TX_CANBuffer.State and BS_ALLOCATED = 0
	MOV	#lo_addr(_CAN_Engine+15), W0
	ZE	[W0], W0
	AND	W0, #2, W0
; Result start address is: 2 (W1)
	CP	W0, #0
	CLR	W1
	BRA NZ	L_NMRAnetStateMachine_CANBusBufferAvailable608
	COM	W1
L_NMRAnetStateMachine_CANBusBufferAvailable608:
;NMRAnetStateMachine.mpas,385 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_CANBusBufferAvailable:
	RETURN
; end of NMRAnetStateMachine_CANBusBufferAvailable

_TransmitNMRABusLayerMsg:
	LNK	#0

;NMRAnetStateMachine.mpas,396 :: 		begin
;NMRAnetStateMachine.mpas,400 :: 		Result := False;
; DestinationAlias start address is: 12 (W6)
	MOV	[W14-8], W6
; DataCount start address is: 14 (W7)
	MOV.B	[W14-10], W7
; DataBytes start address is: 16 (W8)
	MOV	[W14-12], W8
; AliasInHeader start address is: 2 (W1)
	MOV	[W14-14], W1
; Result start address is: 4 (W2)
	CLR	W2
;NMRAnetStateMachine.mpas,402 :: 		if CAN_Engine.TX_NMRAnetBuffer.State and BS_ALLOCATED = 0 then
	MOV	#lo_addr(_CAN_Engine+29), W0
	ZE	[W0], W0
	AND	W0, #2, W0
	CP	W0, #0
	BRA Z	L__TransmitNMRABusLayerMsg610
	GOTO	L__TransmitNMRABusLayerMsg571
L__TransmitNMRABusLayerMsg610:
; Result end address is: 4 (W2)
;NMRAnetStateMachine.mpas,404 :: 		if AliasInHeader then
	CP0	W1
	BRA NZ	L__TransmitNMRABusLayerMsg611
	GOTO	L__TransmitNMRABusLayerMsg70
L__TransmitNMRABusLayerMsg611:
; AliasInHeader end address is: 2 (W1)
;NMRAnetStateMachine.mpas,405 :: 		NMRAnetUtilities_CreateNMRABusMessageCANBufferWithDestInMTI(Node, Buffer, MTI, DestinationAlias, DataCount, DataBytes)
	PUSH	W8
; DataBytes end address is: 16 (W8)
	ZE	W7, W0
; DataCount end address is: 14 (W7)
	PUSH	W0
	PUSH	W6
; DestinationAlias end address is: 12 (W6)
	CALL	_NMRAnetUtilities_CreateNMRABusMessageCANBufferWithDestInMTI
	SUB	#6, W15
	GOTO	L__TransmitNMRABusLayerMsg71
;NMRAnetStateMachine.mpas,406 :: 		else
L__TransmitNMRABusLayerMsg70:
;NMRAnetStateMachine.mpas,407 :: 		NMRAnetUtilities_CreateNMRABusMessageCANBuffer(Node, Buffer, MTI, DestinationAlias, DataCount, DataBytes);
; DataBytes start address is: 16 (W8)
; DataCount start address is: 14 (W7)
; DestinationAlias start address is: 12 (W6)
	PUSH	W8
; DataBytes end address is: 16 (W8)
	ZE	W7, W0
; DataCount end address is: 14 (W7)
	PUSH	W0
	PUSH	W6
; DestinationAlias end address is: 12 (W6)
	CALL	_NMRAnetUtilities_CreateNMRABusMessageCANBuffer
	SUB	#6, W15
L__TransmitNMRABusLayerMsg71:
;NMRAnetStateMachine.mpas,409 :: 		CAN_Engine.TX_NMRAnetBuffer := Buffer^;
	MOV	#lo_addr(_CAN_Engine+16), W1
	MOV	W11, W0
	REPEAT	#6
	MOV	[W0++], [W1++]
;NMRAnetStateMachine.mpas,410 :: 		CAN_Engine.TX_NMRAnetBuffer.State := CAN_Engine.TX_NMRAnetBuffer.State or BS_ALLOCATED;
	MOV	#lo_addr(_CAN_Engine+29), W0
	MOV.B	[W0], W1
	MOV	#lo_addr(_CAN_Engine+29), W0
	IOR.B	W1, #2, [W0]
;NMRAnetStateMachine.mpas,411 :: 		StartCANMessageEngine();
	CALL	_StartCANMessageEngine
;NMRAnetStateMachine.mpas,412 :: 		Result := True
; Result start address is: 2 (W1)
	MOV	#65535, W1
; Result end address is: 2 (W1)
;NMRAnetStateMachine.mpas,413 :: 		end;
	GOTO	L__TransmitNMRABusLayerMsg67
L__TransmitNMRABusLayerMsg571:
;NMRAnetStateMachine.mpas,402 :: 		if CAN_Engine.TX_NMRAnetBuffer.State and BS_ALLOCATED = 0 then
	MOV	W2, W1
;NMRAnetStateMachine.mpas,413 :: 		end;
L__TransmitNMRABusLayerMsg67:
;NMRAnetStateMachine.mpas,414 :: 		end;
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_TransmitNMRABusLayerMsg:
	ULNK
	RETURN
; end of _TransmitNMRABusLayerMsg

_NMRABusBufferAvailable:

;NMRAnetStateMachine.mpas,425 :: 		begin
;NMRAnetStateMachine.mpas,426 :: 		Result := CAN_Engine.TX_NMRAnetBuffer.State and BS_ALLOCATED = 0
	MOV	#lo_addr(_CAN_Engine+29), W0
	ZE	[W0], W0
	AND	W0, #2, W0
; Result start address is: 2 (W1)
	CP	W0, #0
	CLR	W1
	BRA NZ	L__NMRABusBufferAvailable613
	COM	W1
L__NMRABusBufferAvailable613:
;NMRAnetStateMachine.mpas,427 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_NMRABusBufferAvailable:
	RETURN
; end of _NMRABusBufferAvailable

NMRAnetStateMachine_MaxAddressByAddressSpace:
	LNK	#4

;NMRAnetStateMachine.mpas,439 :: 		begin
;NMRAnetStateMachine.mpas,441 :: 		MSI_CDI       : begin
	MOV.B	#255, W0
	CP.B	W11, W0
	BRA Z	L_NMRAnetStateMachine_MaxAddressByAddressSpace615
	GOTO	L_NMRAnetStateMachine_MaxAddressByAddressSpace77
L_NMRAnetStateMachine_MaxAddressByAddressSpace615:
;NMRAnetStateMachine.mpas,443 :: 		if Node^.State and NS_VIRTUAL <> 0 then
	MOV.B	[W10], W0
	ZE	W0, W0
	AND	W0, #8, W0
	CP	W0, #0
	BRA NZ	L_NMRAnetStateMachine_MaxAddressByAddressSpace616
	GOTO	L_NMRAnetStateMachine_MaxAddressByAddressSpace79
L_NMRAnetStateMachine_MaxAddressByAddressSpace616:
;NMRAnetStateMachine.mpas,444 :: 		Result := MAX_CDI_ARRAY_VNODE
	MOV	#1640, W0
	MOV	#0, W1
	MOV	W0, [W14+0]
	MOV	W1, [W14+2]
	GOTO	L_NMRAnetStateMachine_MaxAddressByAddressSpace80
;NMRAnetStateMachine.mpas,445 :: 		else {$ENDIF}
L_NMRAnetStateMachine_MaxAddressByAddressSpace79:
;NMRAnetStateMachine.mpas,446 :: 		Result := MAX_CDI_ARRAY;
	MOV	#1643, W0
	MOV	#0, W1
	MOV	W0, [W14+0]
	MOV	W1, [W14+2]
L_NMRAnetStateMachine_MaxAddressByAddressSpace80:
;NMRAnetStateMachine.mpas,447 :: 		end;
	GOTO	L_NMRAnetStateMachine_MaxAddressByAddressSpace74
L_NMRAnetStateMachine_MaxAddressByAddressSpace77:
;NMRAnetStateMachine.mpas,448 :: 		MSI_ALL       : Result := ALL_MAP.HighMem;
	MOV.B	#254, W0
	CP.B	W11, W0
	BRA Z	L_NMRAnetStateMachine_MaxAddressByAddressSpace617
	GOTO	L_NMRAnetStateMachine_MaxAddressByAddressSpace83
L_NMRAnetStateMachine_MaxAddressByAddressSpace617:
	MOV	#32767, W0
	MOV	#0, W1
	MOV	W0, [W14+0]
	MOV	W1, [W14+2]
	GOTO	L_NMRAnetStateMachine_MaxAddressByAddressSpace74
L_NMRAnetStateMachine_MaxAddressByAddressSpace83:
;NMRAnetStateMachine.mpas,449 :: 		MSI_CONFIG    : Result := AppCallback_ConfigurationSize(Node);
	MOV.B	#253, W0
	CP.B	W11, W0
	BRA Z	L_NMRAnetStateMachine_MaxAddressByAddressSpace618
	GOTO	L_NMRAnetStateMachine_MaxAddressByAddressSpace86
L_NMRAnetStateMachine_MaxAddressByAddressSpace618:
	CALL	_AppCallback_ConfigurationSize
	CLR	W1
	MOV	W0, [W14+0]
	MOV	W1, [W14+2]
	GOTO	L_NMRAnetStateMachine_MaxAddressByAddressSpace74
L_NMRAnetStateMachine_MaxAddressByAddressSpace86:
;NMRAnetStateMachine.mpas,450 :: 		MSI_ACDI_MFG  : begin
	MOV.B	#252, W0
	CP.B	W11, W0
	BRA Z	L_NMRAnetStateMachine_MaxAddressByAddressSpace619
	GOTO	L_NMRAnetStateMachine_MaxAddressByAddressSpace89
L_NMRAnetStateMachine_MaxAddressByAddressSpace619:
;NMRAnetStateMachine.mpas,452 :: 		if Node^.State and NS_VIRTUAL <> 0 then
	MOV.B	[W10], W0
	ZE	W0, W0
	AND	W0, #8, W0
	CP	W0, #0
	BRA NZ	L_NMRAnetStateMachine_MaxAddressByAddressSpace620
	GOTO	L_NMRAnetStateMachine_MaxAddressByAddressSpace91
L_NMRAnetStateMachine_MaxAddressByAddressSpace620:
;NMRAnetStateMachine.mpas,453 :: 		Result := MAX_ACDI_MFG_ARRAY_VNODE + 1 // for the Version ID Byte
	MOV	#27, W0
	MOV	#0, W1
	MOV	W0, [W14+0]
	MOV	W1, [W14+2]
	GOTO	L_NMRAnetStateMachine_MaxAddressByAddressSpace92
;NMRAnetStateMachine.mpas,454 :: 		else {$ENDIF}
L_NMRAnetStateMachine_MaxAddressByAddressSpace91:
;NMRAnetStateMachine.mpas,455 :: 		Result := MAX_ACDI_MFG_ARRAY + 1 // for the Version ID Byte
	MOV	#27, W0
	MOV	#0, W1
	MOV	W0, [W14+0]
	MOV	W1, [W14+2]
L_NMRAnetStateMachine_MaxAddressByAddressSpace92:
;NMRAnetStateMachine.mpas,456 :: 		end;
	GOTO	L_NMRAnetStateMachine_MaxAddressByAddressSpace74
L_NMRAnetStateMachine_MaxAddressByAddressSpace89:
;NMRAnetStateMachine.mpas,457 :: 		MSI_ACDI_USER : Result := AppCallback_ConfigurationSize(Node) + 1  // for the Version ID Byte
	MOV.B	#251, W0
	CP.B	W11, W0
	BRA Z	L_NMRAnetStateMachine_MaxAddressByAddressSpace621
	GOTO	L_NMRAnetStateMachine_MaxAddressByAddressSpace95
L_NMRAnetStateMachine_MaxAddressByAddressSpace621:
	CALL	_AppCallback_ConfigurationSize
	MOV	W0, W1
	CLR	W2
	ADD	W14, #0, W0
	ADD	W1, #1, [W0++]
	ADDC	W2, #0, [W0--]
;NMRAnetStateMachine.mpas,458 :: 		else
	GOTO	L_NMRAnetStateMachine_MaxAddressByAddressSpace74
L_NMRAnetStateMachine_MaxAddressByAddressSpace95:
;NMRAnetStateMachine.mpas,459 :: 		Result := 0;
	CLR	W0
	CLR	W1
	MOV	W0, [W14+0]
	MOV	W1, [W14+2]
L_NMRAnetStateMachine_MaxAddressByAddressSpace74:
;NMRAnetStateMachine.mpas,461 :: 		end;
	MOV	[W14+0], W0
	MOV	[W14+2], W1
L_end_MaxAddressByAddressSpace:
	ULNK
	RETURN
; end of NMRAnetStateMachine_MaxAddressByAddressSpace

NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite:
	LNK	#0

;NMRAnetStateMachine.mpas,475 :: 		begin
;NMRAnetStateMachine.mpas,476 :: 		MemorySpaceCount := DataBytes^[6];
	PUSH	W11
; MemorySpaceCount start address is: 14 (W7)
	MOV	[W14-8], W7
	ADD	W11, #6, W0
	MOV.B	[W0], [W7]
;NMRAnetStateMachine.mpas,477 :: 		case DataBytes^[1] and $03 of      // Strip off bottom two bits
	ADD	W11, #1, W0
	ZE	[W0], W0
	AND	W0, #3, W1
;NMRAnetStateMachine.mpas,478 :: 		MCP_CDI            : MemorySpace := MSI_CDI;
	CP	W1, #3
	BRA Z	L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite623
	GOTO	L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite100
L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite623:
	MOV.B	#255, W0
	MOV.B	W0, [W12]
	GOTO	L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite97
L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite100:
;NMRAnetStateMachine.mpas,479 :: 		MCP_ALL            : MemorySpace := MSI_ALL;
	CP	W1, #2
	BRA Z	L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite624
	GOTO	L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite103
L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite624:
	MOV.B	#254, W0
	MOV.B	W0, [W12]
	GOTO	L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite97
L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite103:
;NMRAnetStateMachine.mpas,480 :: 		MCP_CONFIGURATION  : MemorySpace := MSI_CONFIG;
	CP	W1, #1
	BRA Z	L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite625
	GOTO	L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite106
L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite625:
	MOV.B	#253, W0
	MOV.B	W0, [W12]
	GOTO	L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite97
L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite106:
;NMRAnetStateMachine.mpas,481 :: 		MCP_NONE           : begin
	CP	W1, #0
	BRA Z	L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite626
	GOTO	L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite109
L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite626:
;NMRAnetStateMachine.mpas,482 :: 		MemorySpace := DataBytes^[6];
	ADD	W11, #6, W0
	MOV.B	[W0], [W12]
;NMRAnetStateMachine.mpas,483 :: 		MemorySpaceCount := DataBytes^[7]       // Should not be larger than 64 bytes
	ADD	W11, #7, W0
	MOV.B	[W0], [W7]
;NMRAnetStateMachine.mpas,484 :: 		end;
	GOTO	L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite97
L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite109:
L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite97:
;NMRAnetStateMachine.mpas,486 :: 		MemorySpaceAddress := DWord( DataBytes^[2] shl 24) or DWord( DataBytes^[3] shl 16) or DWord( DataBytes^[4] shl 8) or DWord( DataBytes^[5]);
	ADD	W11, #2, W0
	ZE	[W0], W4
	CLR	W5
	MOV	#24, W0
	MOV.D	W4, W2
L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite627:
	DEC	W0, W0
	BRA LT	L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite628
	SL	W2, W2
	RLC	W3, W3
	BRA	L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite627
L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite628:
	ADD	W11, #3, W0
	ZE	[W0], W0
	CLR	W1
	MOV	W0, W1
	CLR	W0
	IOR	W2, W0, W5
	IOR	W3, W1, W6
	ADD	W11, #4, W0
	ZE	[W0], W3
	CLR	W4
	MOV	#8, W2
	MOV	W3, W0
	MOV	W4, W1
L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite629:
	DEC	W2, W2
	BRA LT	L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite630
	SL	W0, W0
	RLC	W1, W1
	BRA	L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite629
L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite630:
	IOR	W5, W0, W2
	IOR	W6, W1, W3
	ADD	W11, #5, W0
	ZE	[W0], W0
	CLR	W1
	IOR	W2, W0, [W13++]
	IOR	W3, W1, [W13--]
;NMRAnetStateMachine.mpas,488 :: 		MemorySpaceMaxAddress := MaxAddressByAddressSpace(Node, MemorySpace);
	PUSH	W7
	PUSH	W13
	MOV.B	[W12], W11
	CALL	NMRAnetStateMachine_MaxAddressByAddressSpace
	POP	W13
	POP	W7
; MemorySpaceMaxAddress start address is: 8 (W4)
	MOV.D	W0, W4
;NMRAnetStateMachine.mpas,490 :: 		if MemorySpaceAddress >= MemorySpaceMaxAddress then
	MOV.D	[W13], W2
	CP	W2, W0
	CPB	W3, W1
	BRA GEU	L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite631
	GOTO	L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite111
L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite631:
; MemorySpaceMaxAddress end address is: 8 (W4)
;NMRAnetStateMachine.mpas,491 :: 		MemorySpaceCount := 0
	CLR	W0
	MOV.B	W0, [W7]
	GOTO	L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite112
;NMRAnetStateMachine.mpas,492 :: 		else begin
L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite111:
;NMRAnetStateMachine.mpas,493 :: 		if MemorySpaceAddress + MemorySpaceCount >= MemorySpaceMaxAddress then
; MemorySpaceMaxAddress start address is: 8 (W4)
	MOV.B	[W7], W0
	SE	W0, W2
	ASR	W2, #15, W3
	ADD	W2, [W13++], W0
	ADDC	W3, [W13--], W1
	CP	W0, W4
	CPB	W1, W5
	BRA GE	L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite632
	GOTO	L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite114
L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite632:
;NMRAnetStateMachine.mpas,494 :: 		MemorySpaceCount := MemorySpaceMaxAddress - MemorySpaceAddress
	SUB	W4, [W13], W0
; MemorySpaceMaxAddress end address is: 8 (W4)
	MOV.B	W0, [W7]
L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite114:
;NMRAnetStateMachine.mpas,495 :: 		end;
L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite112:
;NMRAnetStateMachine.mpas,496 :: 		if MemorySpaceCount > 64 then
	MOV.B	[W7], W1
	MOV.B	#64, W0
	CP.B	W1, W0
	BRA GT	L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite633
	GOTO	L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite117
L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite633:
;NMRAnetStateMachine.mpas,497 :: 		MemorySpaceCount := 64;       // By the spec the requestor can NOT send a request for more than 64 Bytes
	MOV.B	#64, W0
	MOV.B	W0, [W7]
; MemorySpaceCount end address is: 14 (W7)
L_NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite117:
;NMRAnetStateMachine.mpas,498 :: 		end;
L_end_DecodeMemoryConfigurationReadWrite:
	POP	W11
	ULNK
	RETURN
; end of NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite

NMRAnetStateMachine_ProcessNode:
	LNK	#68

;NMRAnetStateMachine.mpas,530 :: 		begin
;NMRAnetStateMachine.mpas,534 :: 		if Node^.MsgFlags <> 0 then
	PUSH	W11
	PUSH	W12
	PUSH	W13
	ADD	W10, #22, W0
	MOV.B	[W0], W0
	CP.B	W0, #0
	BRA NZ	L_NMRAnetStateMachine_ProcessNode635
	GOTO	L_NMRAnetStateMachine_ProcessNode121
L_NMRAnetStateMachine_ProcessNode635:
;NMRAnetStateMachine.mpas,536 :: 		if Node^.MsgFlags and CRITICAL_MSG_MASK <> 0 then                           // Subdivide to Critical vs. Noncritical messages
	ADD	W10, #22, W0
	ZE	[W0], W0
	AND	W0, #7, W0
	CP	W0, #0
	BRA NZ	L_NMRAnetStateMachine_ProcessNode636
	GOTO	L_NMRAnetStateMachine_ProcessNode124
L_NMRAnetStateMachine_ProcessNode636:
;NMRAnetStateMachine.mpas,539 :: 		if NMRAnetNode_TestMsgFlags(Node, MF_DUPLICATE_NODE_ID, True) then        // Jump Statemachine here
	PUSH	W11
	MOV	#65535, W12
	MOV	#1, W11
	CALL	_NMRAnetNode_TestMsgFlags
	POP	W11
	CP0	W0
	BRA NZ	L_NMRAnetStateMachine_ProcessNode637
	GOTO	L_NMRAnetStateMachine_ProcessNode127
L_NMRAnetStateMachine_ProcessNode637:
;NMRAnetStateMachine.mpas,540 :: 		Node^.iStateMachine := STATE_NMRABUS_DUPLICATE_FULL_ID
	ADD	W10, #25, W1
	MOV.B	#13, W0
	MOV.B	W0, [W1]
	GOTO	L_NMRAnetStateMachine_ProcessNode128
;NMRAnetStateMachine.mpas,541 :: 		else
L_NMRAnetStateMachine_ProcessNode127:
;NMRAnetStateMachine.mpas,542 :: 		if NMRAnetNode_TestMsgFlags(Node, MF_DUPLICATE_ALIAS, True) then          // Jump Statemachine here
	PUSH	W11
	MOV	#65535, W12
	MOV	#2, W11
	CALL	_NMRAnetNode_TestMsgFlags
	POP	W11
	CP0	W0
	BRA NZ	L_NMRAnetStateMachine_ProcessNode638
	GOTO	L_NMRAnetStateMachine_ProcessNode130
L_NMRAnetStateMachine_ProcessNode638:
;NMRAnetStateMachine.mpas,543 :: 		Node^.iStateMachine := STATE_NMRABUS_INHIBITED
	ADD	W10, #25, W1
	MOV.B	#12, W0
	MOV.B	W0, [W1]
	GOTO	L_NMRAnetStateMachine_ProcessNode131
;NMRAnetStateMachine.mpas,544 :: 		else
L_NMRAnetStateMachine_ProcessNode130:
;NMRAnetStateMachine.mpas,545 :: 		if NMRAnetNode_TestMsgFlags(Node, MF_DUPLICATE_ALIAS_RID, False) then     // Tell the node that is our Alias
	PUSH	W11
	CLR	W12
	MOV	#4, W11
	CALL	_NMRAnetNode_TestMsgFlags
	POP	W11
	CP0	W0
	BRA NZ	L_NMRAnetStateMachine_ProcessNode639
	GOTO	L_NMRAnetStateMachine_ProcessNode133
L_NMRAnetStateMachine_ProcessNode639:
;NMRAnetStateMachine.mpas,547 :: 		if CANBusBufferAvailable then
	CALL	NMRAnetStateMachine_CANBusBufferAvailable
	CP0	W0
	BRA NZ	L_NMRAnetStateMachine_ProcessNode640
	GOTO	L_NMRAnetStateMachine_ProcessNode136
L_NMRAnetStateMachine_ProcessNode640:
;NMRAnetStateMachine.mpas,549 :: 		TransmitCANLayerMsg(Node, CANBuffer, MTI_RID);
	MOV	#0, W12
	MOV	#112, W13
	CALL	_TransmitCANLayerMsg
;NMRAnetStateMachine.mpas,550 :: 		NMRAnetNode_ClearMsgFlag(Node, MF_DUPLICATE_ALIAS_RID);
	MOV.B	#4, W11
	CALL	_NMRAnetNode_ClearMsgFlag
;NMRAnetStateMachine.mpas,551 :: 		end
L_NMRAnetStateMachine_ProcessNode136:
;NMRAnetStateMachine.mpas,552 :: 		end;
L_NMRAnetStateMachine_ProcessNode133:
L_NMRAnetStateMachine_ProcessNode131:
L_NMRAnetStateMachine_ProcessNode128:
;NMRAnetStateMachine.mpas,553 :: 		Exit;                                                                     // Important things happened and were handled, exit
	GOTO	L_end_NMRAnetStateMachine_ProcessNode
;NMRAnetStateMachine.mpas,554 :: 		end;
L_NMRAnetStateMachine_ProcessNode124:
;NMRAnetStateMachine.mpas,557 :: 		if NMRAnetNode_TestMsgFlags(Node, MF_ALIAS_MAP_ENQUIRY, False) then
	PUSH	W11
	CLR	W12
	MOV	#8, W11
	CALL	_NMRAnetNode_TestMsgFlags
	POP	W11
	CP0	W0
	BRA NZ	L_NMRAnetStateMachine_ProcessNode641
	GOTO	L_NMRAnetStateMachine_ProcessNode139
L_NMRAnetStateMachine_ProcessNode641:
;NMRAnetStateMachine.mpas,559 :: 		if CANBusBufferAvailable then
	CALL	NMRAnetStateMachine_CANBusBufferAvailable
	CP0	W0
	BRA NZ	L_NMRAnetStateMachine_ProcessNode642
	GOTO	L_NMRAnetStateMachine_ProcessNode142
L_NMRAnetStateMachine_ProcessNode642:
;NMRAnetStateMachine.mpas,561 :: 		TransmitCANLayerMsg(Node, CANBuffer, MTI_AMD);
	MOV	#4096, W12
	MOV	#112, W13
	CALL	_TransmitCANLayerMsg
;NMRAnetStateMachine.mpas,562 :: 		NMRAnetNode_ClearMsgFlag(Node, MF_ALIAS_MAP_ENQUIRY);
	MOV.B	#8, W11
	CALL	_NMRAnetNode_ClearMsgFlag
;NMRAnetStateMachine.mpas,563 :: 		end
L_NMRAnetStateMachine_ProcessNode142:
;NMRAnetStateMachine.mpas,564 :: 		end else
	GOTO	L_NMRAnetStateMachine_ProcessNode140
L_NMRAnetStateMachine_ProcessNode139:
;NMRAnetStateMachine.mpas,565 :: 		if NMRAnetNode_TestMsgFlags(Node, MF_VERIFY_NODE_ID, False) then
	PUSH	W11
	CLR	W12
	MOV	#16, W11
	CALL	_NMRAnetNode_TestMsgFlags
	POP	W11
	CP0	W0
	BRA NZ	L_NMRAnetStateMachine_ProcessNode643
	GOTO	L_NMRAnetStateMachine_ProcessNode145
L_NMRAnetStateMachine_ProcessNode643:
;NMRAnetStateMachine.mpas,567 :: 		if NMRABusBufferAvailable then
	CALL	_NMRABusBufferAvailable
	CP0	W0
	BRA NZ	L_NMRAnetStateMachine_ProcessNode644
	GOTO	L_NMRAnetStateMachine_ProcessNode148
L_NMRAnetStateMachine_ProcessNode644:
;NMRAnetStateMachine.mpas,569 :: 		NMRAnetUtilities_LoadFrameCANBufferDataWith48BitNodeID(Node, CANBuffer);
	CALL	_NMRAnetUtilities_LoadFrameCANBufferDataWith48BitNodeID
;NMRAnetStateMachine.mpas,570 :: 		TransmitNMRABusLayerMsg(Node, CANBuffer, MTI_VERIFIED_NODE_ID_NUMBER, 0, 6, @CANBuffer^.DataBytes, False);
	ADD	W11, #5, W1
	MOV	#0, W12
	MOV	#2327, W13
	CLR	W0
	PUSH	W0
	PUSH	W1
	MOV	#6, W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	CALL	_TransmitNMRABusLayerMsg
	SUB	#8, W15
;NMRAnetStateMachine.mpas,571 :: 		NMRAnetNode_ClearMsgFlag(Node, MF_VERIFY_NODE_ID);
	MOV.B	#16, W11
	CALL	_NMRAnetNode_ClearMsgFlag
;NMRAnetStateMachine.mpas,572 :: 		end;
L_NMRAnetStateMachine_ProcessNode148:
;NMRAnetStateMachine.mpas,573 :: 		end;
L_NMRAnetStateMachine_ProcessNode145:
L_NMRAnetStateMachine_ProcessNode140:
;NMRAnetStateMachine.mpas,574 :: 		Exit;              // Don't interleave Buffer Replies...
	GOTO	L_end_NMRAnetStateMachine_ProcessNode
;NMRAnetStateMachine.mpas,575 :: 		end else
L_NMRAnetStateMachine_ProcessNode121:
;NMRAnetStateMachine.mpas,578 :: 		if AppCallback_StateMachine(Node, CANBuffer, @DataBytes) then
	ADD	W14, #0, W0
	PUSH.D	W10
	MOV	W0, W12
	CALL	_AppCallback_StateMachine
	POP.D	W10
	CP0	W0
	BRA NZ	L_NMRAnetStateMachine_ProcessNode645
	GOTO	L_NMRAnetStateMachine_ProcessNode151
L_NMRAnetStateMachine_ProcessNode645:
;NMRAnetStateMachine.mpas,580 :: 		Exit
	GOTO	L_end_NMRAnetStateMachine_ProcessNode
;NMRAnetStateMachine.mpas,581 :: 		end;
L_NMRAnetStateMachine_ProcessNode151:
;NMRAnetStateMachine.mpas,583 :: 		BaseBuffer := NMRAnetUtilities_NextBaseBuffer(Node);
	CALL	_NMRAnetUtilities_NextBaseBuffer
; BaseBuffer start address is: 8 (W4)
	MOV	W0, W4
;NMRAnetStateMachine.mpas,584 :: 		if BaseBuffer <> nil then
	CP	W0, #0
	BRA NZ	L_NMRAnetStateMachine_ProcessNode646
	GOTO	L_NMRAnetStateMachine_ProcessNode575
L_NMRAnetStateMachine_ProcessNode646:
;NMRAnetStateMachine.mpas,587 :: 		case BaseBuffer^.mCode of
	ADD	W4, #6, W0
	MOV.B	[W0], W0
	MOV.B	W0, [W14+60]
;NMRAnetStateMachine.mpas,588 :: 		BMC_PROTOCOL_SUPPORT_QUERY :        begin
	CP.B	W0, #1
	BRA Z	L_NMRAnetStateMachine_ProcessNode647
	GOTO	L_NMRAnetStateMachine_ProcessNode159
L_NMRAnetStateMachine_ProcessNode647:
;NMRAnetStateMachine.mpas,589 :: 		if NMRABusBufferAvailable then
	CALL	_NMRABusBufferAvailable
	CP0	W0
	BRA NZ	L_NMRAnetStateMachine_ProcessNode648
	GOTO	L_NMRAnetStateMachine_ProcessNode573
L_NMRAnetStateMachine_ProcessNode648:
;NMRAnetStateMachine.mpas,591 :: 		NMRAnetUtilities_ZeroCANData(DataBytes);
	ADD	W14, #0, W0
	PUSH	W10
	MOV	W0, W10
	CALL	_NMRAnetUtilities_ZeroCANData
	POP	W10
;NMRAnetStateMachine.mpas,593 :: 		if NMRAnetNode_TestStateFlag(NOde, NS_VIRTUAL) then
	PUSH	W11
	MOV.B	#8, W11
	CALL	_NMRAnetNode_TestStateFlag
	POP	W11
	CP0	W0
	BRA NZ	L_NMRAnetStateMachine_ProcessNode649
	GOTO	L_NMRAnetStateMachine_ProcessNode164
L_NMRAnetStateMachine_ProcessNode649:
;NMRAnetStateMachine.mpas,595 :: 		for i := 0 to LEN_PIV_PROTOCOL-1 do
	CLR	W0
	MOV	W0, [W14+8]
; BaseBuffer end address is: 8 (W4)
	MOV	W4, W6
L_NMRAnetStateMachine_ProcessNode167:
;NMRAnetStateMachine.mpas,596 :: 		for j := 0 to PIV_VNODE_SUPPORTED_PROTOCOL_COUNT - 1 do
; BaseBuffer start address is: 12 (W6)
	CLR	W0
	MOV	W0, [W14+10]
; BaseBuffer end address is: 12 (W6)
L_NMRAnetStateMachine_ProcessNode172:
;NMRAnetStateMachine.mpas,597 :: 		DataBytes[i] := DataBytes[i] or PIV_VNODE_SUPPORTED_PROTOCOLS[j][i];
; BaseBuffer start address is: 12 (W6)
	ADD	W14, #0, W1
	ADD	W14, #8, W0
	ADD	W1, [W0], W5
	MOV	W5, W4
	MOV	#6, W1
	ADD	W14, #10, W0
	MUL.UU	W1, [W0], W2
	MOV	#lo_addr(_PIV_VNODE_SUPPORTED_PROTOCOLS), W0
	ADD	W0, W2, W1
	ADD	W14, #8, W0
	ADD	W1, [W0], W1
	MOV	#___Lib_System_DefaultPage, W0
	MOV	WREG, 50
	MOV.B	[W1], W0
	ZE	[W4], W1
	ZE	W0, W0
	IOR	W1, W0, W0
	MOV.B	W0, [W5]
	MOV	[W14+10], W0
	CP	W0, #8
	BRA NZ	L_NMRAnetStateMachine_ProcessNode650
	GOTO	L_NMRAnetStateMachine_ProcessNode175
L_NMRAnetStateMachine_ProcessNode650:
	MOV	#1, W1
	ADD	W14, #10, W0
	ADD	W1, [W0], [W0]
	GOTO	L_NMRAnetStateMachine_ProcessNode172
L_NMRAnetStateMachine_ProcessNode175:
	MOV	[W14+8], W0
	CP	W0, #5
	BRA NZ	L_NMRAnetStateMachine_ProcessNode651
	GOTO	L_NMRAnetStateMachine_ProcessNode170
L_NMRAnetStateMachine_ProcessNode651:
	MOV	#1, W1
	ADD	W14, #8, W0
	ADD	W1, [W0], [W0]
	GOTO	L_NMRAnetStateMachine_ProcessNode167
L_NMRAnetStateMachine_ProcessNode170:
;NMRAnetStateMachine.mpas,598 :: 		end else
	MOV	W6, W9
; BaseBuffer end address is: 12 (W6)
	GOTO	L_NMRAnetStateMachine_ProcessNode165
L_NMRAnetStateMachine_ProcessNode164:
;NMRAnetStateMachine.mpas,601 :: 		for i := 0 to LEN_PIV_PROTOCOL-1 do
; BaseBuffer start address is: 8 (W4)
	CLR	W0
	MOV	W0, [W14+8]
; BaseBuffer end address is: 8 (W4)
	MOV	W4, W6
L_NMRAnetStateMachine_ProcessNode177:
;NMRAnetStateMachine.mpas,602 :: 		for j := 0 to PIV_SUPPORTED_PROTOCOL_COUNT - 1 do
; BaseBuffer start address is: 12 (W6)
	CLR	W0
	MOV	W0, [W14+10]
; BaseBuffer end address is: 12 (W6)
L_NMRAnetStateMachine_ProcessNode182:
;NMRAnetStateMachine.mpas,603 :: 		DataBytes[i] := DataBytes[i] or PIV_SUPPORTED_PROTOCOLS[j][i];
; BaseBuffer start address is: 12 (W6)
	ADD	W14, #0, W1
	ADD	W14, #8, W0
	ADD	W1, [W0], W5
	MOV	W5, W4
	MOV	#6, W1
	ADD	W14, #10, W0
	MUL.UU	W1, [W0], W2
	MOV	#lo_addr(_PIV_SUPPORTED_PROTOCOLS), W0
	ADD	W0, W2, W1
	ADD	W14, #8, W0
	ADD	W1, [W0], W1
	MOV	#___Lib_System_DefaultPage, W0
	MOV	WREG, 50
	MOV.B	[W1], W0
	ZE	[W4], W1
	ZE	W0, W0
	IOR	W1, W0, W0
	MOV.B	W0, [W5]
	MOV	[W14+10], W0
	CP	W0, #7
	BRA NZ	L_NMRAnetStateMachine_ProcessNode652
	GOTO	L_NMRAnetStateMachine_ProcessNode185
L_NMRAnetStateMachine_ProcessNode652:
	MOV	#1, W1
	ADD	W14, #10, W0
	ADD	W1, [W0], [W0]
	GOTO	L_NMRAnetStateMachine_ProcessNode182
L_NMRAnetStateMachine_ProcessNode185:
	MOV	[W14+8], W0
	CP	W0, #5
	BRA NZ	L_NMRAnetStateMachine_ProcessNode653
	GOTO	L_NMRAnetStateMachine_ProcessNode180
L_NMRAnetStateMachine_ProcessNode653:
	MOV	#1, W1
	ADD	W14, #8, W0
	ADD	W1, [W0], [W0]
	GOTO	L_NMRAnetStateMachine_ProcessNode177
L_NMRAnetStateMachine_ProcessNode180:
;NMRAnetStateMachine.mpas,604 :: 		end;
	MOV	W6, W9
L_NMRAnetStateMachine_ProcessNode165:
; BaseBuffer end address is: 12 (W6)
;NMRAnetStateMachine.mpas,605 :: 		TransmitNMRABusLayerMsg(Node, CANBuffer, MTI_PROTOCOL_SUPPORT_REPLY, BaseBuffer^.Alias, 6, @DataBytes, False);
; BaseBuffer start address is: 18 (W9)
	ADD	W9, #2, W2
	ADD	W14, #0, W1
	PUSH	W11
	MOV	#32768, W12
	MOV	#2406, W13
	CLR	W0
	PUSH	W0
	PUSH	W1
	MOV	#6, W0
	PUSH	W0
	PUSH	[W2]
	CALL	_TransmitNMRABusLayerMsg
	SUB	#8, W15
;NMRAnetStateMachine.mpas,606 :: 		NMRAnetUtilities_BaseBufferUnLink(Node, BaseBuffer);
	MOV	W9, W11
	CALL	_NMRAnetUtilities_BaseBufferUnLink
	POP	W11
;NMRAnetStateMachine.mpas,607 :: 		NMRAnetBufferPools_ReleaseBaseBuffer(BaseBuffer);
	PUSH	W10
	MOV	W9, W10
	CALL	_NMRAnetBufferPools_ReleaseBaseBuffer
; BaseBuffer end address is: 18 (W9)
	POP	W10
	MOV	W9, W0
;NMRAnetStateMachine.mpas,608 :: 		end;
	GOTO	L_NMRAnetStateMachine_ProcessNode161
L_NMRAnetStateMachine_ProcessNode573:
;NMRAnetStateMachine.mpas,589 :: 		if NMRABusBufferAvailable then
	MOV	W4, W0
;NMRAnetStateMachine.mpas,608 :: 		end;
L_NMRAnetStateMachine_ProcessNode161:
;NMRAnetStateMachine.mpas,609 :: 		end;
; BaseBuffer start address is: 0 (W0)
; BaseBuffer end address is: 0 (W0)
	GOTO	L_NMRAnetStateMachine_ProcessNode156
L_NMRAnetStateMachine_ProcessNode159:
;NMRAnetStateMachine.mpas,611 :: 		BMC_SIMPLE_NODE_INFO_REQEUST :      begin
; BaseBuffer start address is: 8 (W4)
	MOV.B	[W14+60], W0
	CP.B	W0, #5
	BRA Z	L_NMRAnetStateMachine_ProcessNode654
	GOTO	L_NMRAnetStateMachine_ProcessNode188
L_NMRAnetStateMachine_ProcessNode654:
;NMRAnetStateMachine.mpas,612 :: 		if NMRABusBufferAvailable then
	CALL	_NMRABusBufferAvailable
	CP0	W0
	BRA NZ	L_NMRAnetStateMachine_ProcessNode655
	GOTO	L_NMRAnetStateMachine_ProcessNode574
L_NMRAnetStateMachine_ProcessNode655:
;NMRAnetStateMachine.mpas,614 :: 		i := 0;
	CLR	W0
	MOV	W0, [W14+8]
; BaseBuffer end address is: 8 (W4)
;NMRAnetStateMachine.mpas,615 :: 		while (BaseBuffer^.StateMachine <> STATE_ACDI_DONE) and (i < 6) do   // All messages have the Destination Alias as the first 2 bytes so only 6 left to use
L_NMRAnetStateMachine_ProcessNode193:
; BaseBuffer start address is: 8 (W4)
	ADD	W4, #7, W0
	MOV.B	[W0], W0
	CP.B	W0, #7
	CLR	W1
	BRA Z	L_NMRAnetStateMachine_ProcessNode656
	COM	W1
L_NMRAnetStateMachine_ProcessNode656:
	MOV	[W14+8], W0
	CP	W0, #6
	CLR	W0
	BRA GE	L_NMRAnetStateMachine_ProcessNode657
	COM	W0
L_NMRAnetStateMachine_ProcessNode657:
	AND	W1, W0, W0
	BRA NZ	L_NMRAnetStateMachine_ProcessNode658
	GOTO	L_NMRAnetStateMachine_ProcessNode194
L_NMRAnetStateMachine_ProcessNode658:
;NMRAnetStateMachine.mpas,617 :: 		case BaseBuffer^.StateMachine of
	ADD	W4, #7, W0
	MOV.B	[W0], W3
	MOV.B	W3, [W14+60]
;NMRAnetStateMachine.mpas,618 :: 		STATE_ACDI_MFG_VERSION  : begin      {$IFDEF TRACE_SNIP}UART1_Write_Text('STATE_ACDI_MFG_VERSION'+LF); {$ENDIF}
	CP.B	W3, #0
	BRA Z	L_NMRAnetStateMachine_ProcessNode659
	GOTO	L_NMRAnetStateMachine_ProcessNode200
L_NMRAnetStateMachine_ProcessNode659:
;NMRAnetStateMachine.mpas,619 :: 		DataBytes[i] := ACDI_MFG_VERSION;
	ADD	W14, #0, W1
	ADD	W14, #8, W0
	ADD	W1, [W0], W1
	MOV.B	#1, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,620 :: 		Inc(i);
	MOV	[W14+8], W1
	ADD	W14, #8, W0
	ADD	W1, #1, [W0]
;NMRAnetStateMachine.mpas,621 :: 		BaseBuffer^.Tag := 0;
	ADD	W4, #8, W1
	CLR	W0
	MOV	W0, [W1]
;NMRAnetStateMachine.mpas,622 :: 		BaseBuffer^.StateMachine := STATE_ACDI_MFG_INFO;
	ADD	W4, #7, W1
	MOV.B	#1, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,623 :: 		end;
	GOTO	L_NMRAnetStateMachine_ProcessNode197
L_NMRAnetStateMachine_ProcessNode200:
;NMRAnetStateMachine.mpas,624 :: 		STATE_ACDI_MFG_INFO    :  begin  {$IFDEF TRACE_SNIP}UART1_Write_Text('STATE_ACDI_MFG_INFO'+LF); {$ENDIF}
	CP.B	W3, #1
	BRA Z	L_NMRAnetStateMachine_ProcessNode660
	GOTO	L_NMRAnetStateMachine_ProcessNode203
L_NMRAnetStateMachine_ProcessNode660:
;NMRAnetStateMachine.mpas,626 :: 		if Node^.State and NS_VIRTUAL <> 0 then
	MOV.B	[W10], W0
	ZE	W0, W0
	AND	W0, #8, W0
	CP	W0, #0
	BRA NZ	L_NMRAnetStateMachine_ProcessNode661
	GOTO	L_NMRAnetStateMachine_ProcessNode205
L_NMRAnetStateMachine_ProcessNode661:
;NMRAnetStateMachine.mpas,628 :: 		if BaseBuffer^.Tag < MAX_ACDI_MFG_ARRAY_VNODE then
	ADD	W4, #8, W0
	MOV	[W0], W0
	CP	W0, #26
	BRA LTU	L_NMRAnetStateMachine_ProcessNode662
	GOTO	L_NMRAnetStateMachine_ProcessNode208
L_NMRAnetStateMachine_ProcessNode662:
;NMRAnetStateMachine.mpas,630 :: 		DataBytes[i] := ACDI_MFG_STRINGS_VNODE[BaseBuffer^.Tag];
	ADD	W14, #0, W1
	ADD	W14, #8, W0
	ADD	W1, [W0], W2
	ADD	W4, #8, W1
	MOV	#lo_addr(_ACDI_MFG_STRINGS_VNODE), W0
	ADD	W0, [W1], W1
	MOV	#___Lib_System_DefaultPage, W0
	MOV	WREG, 50
	MOV.B	[W1], W0
	MOV.B	W0, [W2]
;NMRAnetStateMachine.mpas,631 :: 		Inc(BaseBuffer^.Tag);
	ADD	W4, #8, W1
	MOV	[W1], W0
	INC	W0
	MOV	W0, [W1]
;NMRAnetStateMachine.mpas,632 :: 		Inc(i);
	MOV	[W14+8], W1
	ADD	W14, #8, W0
	ADD	W1, #1, [W0]
;NMRAnetStateMachine.mpas,633 :: 		end else
	GOTO	L_NMRAnetStateMachine_ProcessNode209
L_NMRAnetStateMachine_ProcessNode208:
;NMRAnetStateMachine.mpas,634 :: 		BaseBuffer^.StateMachine := STATE_ACDI_USER_VERSION;
	ADD	W4, #7, W1
	MOV.B	#3, W0
	MOV.B	W0, [W1]
L_NMRAnetStateMachine_ProcessNode209:
;NMRAnetStateMachine.mpas,635 :: 		end else {$ENDIF}
	GOTO	L_NMRAnetStateMachine_ProcessNode206
L_NMRAnetStateMachine_ProcessNode205:
;NMRAnetStateMachine.mpas,637 :: 		if BaseBuffer^.Tag < MAX_ACDI_MFG_ARRAY then
	ADD	W4, #8, W0
	MOV	[W0], W0
	CP	W0, #26
	BRA LTU	L_NMRAnetStateMachine_ProcessNode663
	GOTO	L_NMRAnetStateMachine_ProcessNode211
L_NMRAnetStateMachine_ProcessNode663:
;NMRAnetStateMachine.mpas,639 :: 		DataBytes[i] := ACDI_MFG_STRINGS[BaseBuffer^.Tag];
	ADD	W14, #0, W1
	ADD	W14, #8, W0
	ADD	W1, [W0], W2
	ADD	W4, #8, W1
	MOV	#lo_addr(_ACDI_MFG_STRINGS), W0
	ADD	W0, [W1], W1
	MOV	#___Lib_System_DefaultPage, W0
	MOV	WREG, 50
	MOV.B	[W1], W0
	MOV.B	W0, [W2]
;NMRAnetStateMachine.mpas,640 :: 		Inc(BaseBuffer^.Tag);
	ADD	W4, #8, W1
	MOV	[W1], W0
	INC	W0
	MOV	W0, [W1]
;NMRAnetStateMachine.mpas,641 :: 		Inc(i);
	MOV	[W14+8], W1
	ADD	W14, #8, W0
	ADD	W1, #1, [W0]
;NMRAnetStateMachine.mpas,642 :: 		end else
	GOTO	L_NMRAnetStateMachine_ProcessNode212
L_NMRAnetStateMachine_ProcessNode211:
;NMRAnetStateMachine.mpas,643 :: 		BaseBuffer^.StateMachine := STATE_ACDI_USER_VERSION;
	ADD	W4, #7, W1
	MOV.B	#3, W0
	MOV.B	W0, [W1]
L_NMRAnetStateMachine_ProcessNode212:
;NMRAnetStateMachine.mpas,644 :: 		end;
L_NMRAnetStateMachine_ProcessNode206:
;NMRAnetStateMachine.mpas,645 :: 		end;
	GOTO	L_NMRAnetStateMachine_ProcessNode197
L_NMRAnetStateMachine_ProcessNode203:
;NMRAnetStateMachine.mpas,646 :: 		STATE_ACDI_USER_VERSION : begin    {$IFDEF TRACE_SNIP}UART1_Write_Text('STATE_ACDI_USER_VERSION'+LF); {$ENDIF}
	CP.B	W3, #3
	BRA Z	L_NMRAnetStateMachine_ProcessNode664
	GOTO	L_NMRAnetStateMachine_ProcessNode215
L_NMRAnetStateMachine_ProcessNode664:
;NMRAnetStateMachine.mpas,647 :: 		DataBytes[i] := ACDI_USER_VERSION;
	ADD	W14, #0, W1
	ADD	W14, #8, W0
	ADD	W1, [W0], W1
	MOV.B	#1, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,648 :: 		Inc(i);
	MOV	[W14+8], W1
	ADD	W14, #8, W0
	ADD	W1, #1, [W0]
;NMRAnetStateMachine.mpas,649 :: 		BaseBuffer^.StateMachine := STATE_ACDI_USER_NAME;
	ADD	W4, #7, W1
	MOV.B	#4, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,650 :: 		BaseBuffer^.Tag := 0;
	ADD	W4, #8, W1
	CLR	W0
	MOV	W0, [W1]
;NMRAnetStateMachine.mpas,651 :: 		end;
	GOTO	L_NMRAnetStateMachine_ProcessNode197
L_NMRAnetStateMachine_ProcessNode215:
;NMRAnetStateMachine.mpas,652 :: 		STATE_ACDI_USER_NAME    : begin      {$IFDEF TRACE_SNIP}UART1_Write_Text('STATE_ACDI_USER_NAME'+LF); {$ENDIF}
	CP.B	W3, #4
	BRA Z	L_NMRAnetStateMachine_ProcessNode665
	GOTO	L_NMRAnetStateMachine_ProcessNode218
L_NMRAnetStateMachine_ProcessNode665:
;NMRAnetStateMachine.mpas,653 :: 		if BaseBuffer^.Tag < MAX_USER_NAME then
	ADD	W4, #8, W0
	MOV	[W0], W0
	CP	W0, #20
	BRA LTU	L_NMRAnetStateMachine_ProcessNode666
	GOTO	L_NMRAnetStateMachine_ProcessNode220
L_NMRAnetStateMachine_ProcessNode666:
;NMRAnetStateMachine.mpas,656 :: 		AppCallback_ConfigurationRead(Node, @DataBytes[i], BaseBuffer^.Tag, 1);
	ADD	W14, #0, W1
	ADD	W14, #8, W0
	ADD	W1, [W0], W1
	ADD	W4, #8, W0
	PUSH	W4
	PUSH.D	W10
	MOV	[W0], W12
	CLR	W13
	MOV	W1, W11
	MOV	#1, W0
	PUSH	W0
	CALL	_AppCallback_ConfigurationRead
	SUB	#2, W15
	POP.D	W10
	POP	W4
;NMRAnetStateMachine.mpas,657 :: 		if DataBytes[i] = #0 then
	ADD	W14, #0, W1
	ADD	W14, #8, W0
	ADD	W1, [W0], W0
	MOV.B	[W0], W1
	MOV.B	#0, W0
	CP.B	W1, W0
	BRA Z	L_NMRAnetStateMachine_ProcessNode667
	GOTO	L_NMRAnetStateMachine_ProcessNode223
L_NMRAnetStateMachine_ProcessNode667:
;NMRAnetStateMachine.mpas,658 :: 		BaseBuffer^.StateMachine := STATE_ACDI_START_DESC
	ADD	W4, #7, W1
	MOV.B	#5, W0
	MOV.B	W0, [W1]
	GOTO	L_NMRAnetStateMachine_ProcessNode224
;NMRAnetStateMachine.mpas,659 :: 		else
L_NMRAnetStateMachine_ProcessNode223:
;NMRAnetStateMachine.mpas,660 :: 		if BaseBuffer^.Tag = MAX_USER_NAME - 1 then
	ADD	W4, #8, W0
	MOV	[W0], W0
	CP	W0, #19
	BRA Z	L_NMRAnetStateMachine_ProcessNode668
	GOTO	L_NMRAnetStateMachine_ProcessNode226
L_NMRAnetStateMachine_ProcessNode668:
;NMRAnetStateMachine.mpas,661 :: 		DataBytes[i] := #0;
	ADD	W14, #0, W1
	ADD	W14, #8, W0
	ADD	W1, [W0], W1
	MOV.B	#0, W0
	MOV.B	W0, [W1]
L_NMRAnetStateMachine_ProcessNode226:
L_NMRAnetStateMachine_ProcessNode224:
;NMRAnetStateMachine.mpas,662 :: 		Inc(i);
	MOV	[W14+8], W1
	ADD	W14, #8, W0
	ADD	W1, #1, [W0]
;NMRAnetStateMachine.mpas,663 :: 		Inc(BaseBuffer^.Tag);
	ADD	W4, #8, W1
	MOV	[W1], W0
	INC	W0
	MOV	W0, [W1]
;NMRAnetStateMachine.mpas,664 :: 		end else
	GOTO	L_NMRAnetStateMachine_ProcessNode221
L_NMRAnetStateMachine_ProcessNode220:
;NMRAnetStateMachine.mpas,665 :: 		BaseBuffer^.StateMachine := STATE_ACDI_START_DESC;
	ADD	W4, #7, W1
	MOV.B	#5, W0
	MOV.B	W0, [W1]
L_NMRAnetStateMachine_ProcessNode221:
;NMRAnetStateMachine.mpas,666 :: 		end;
	GOTO	L_NMRAnetStateMachine_ProcessNode197
L_NMRAnetStateMachine_ProcessNode218:
;NMRAnetStateMachine.mpas,667 :: 		STATE_ACDI_START_DESC   : begin      {$IFDEF TRACE_SNIP}UART1_Write_Text('STATE_ACDI_START_DESC'+LF); {$ENDIF}
	MOV.B	[W14+60], W2
	CP.B	W2, #5
	BRA Z	L_NMRAnetStateMachine_ProcessNode669
	GOTO	L_NMRAnetStateMachine_ProcessNode230
L_NMRAnetStateMachine_ProcessNode669:
;NMRAnetStateMachine.mpas,668 :: 		BaseBuffer^.Tag := MAX_USER_NAME;
	ADD	W4, #8, W1
	MOV	#20, W0
	MOV	W0, [W1]
;NMRAnetStateMachine.mpas,669 :: 		BaseBuffer^.StateMachine := STATE_ACDI_USER_DESC;
	ADD	W4, #7, W1
	MOV.B	#6, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,670 :: 		end;
	GOTO	L_NMRAnetStateMachine_ProcessNode197
L_NMRAnetStateMachine_ProcessNode230:
;NMRAnetStateMachine.mpas,671 :: 		STATE_ACDI_USER_DESC    : begin     {$IFDEF TRACE_SNIP}UART1_Write_Text('STATE_ACDI_USER_DESC'+LF); {$ENDIF}
	CP.B	W2, #6
	BRA Z	L_NMRAnetStateMachine_ProcessNode670
	GOTO	L_NMRAnetStateMachine_ProcessNode233
L_NMRAnetStateMachine_ProcessNode670:
;NMRAnetStateMachine.mpas,672 :: 		if BaseBuffer^.Tag < MAX_CONFIG_DATA then
	ADD	W4, #8, W0
	MOV	[W0], W1
	MOV	#60, W0
	CP	W1, W0
	BRA LTU	L_NMRAnetStateMachine_ProcessNode671
	GOTO	L_NMRAnetStateMachine_ProcessNode235
L_NMRAnetStateMachine_ProcessNode671:
;NMRAnetStateMachine.mpas,675 :: 		AppCallback_ConfigurationRead(Node, @DataBytes[i], BaseBuffer^.Tag, 1);
	ADD	W14, #0, W1
	ADD	W14, #8, W0
	ADD	W1, [W0], W1
	ADD	W4, #8, W0
	PUSH	W4
	PUSH.D	W10
	MOV	[W0], W12
	CLR	W13
	MOV	W1, W11
	MOV	#1, W0
	PUSH	W0
	CALL	_AppCallback_ConfigurationRead
	SUB	#2, W15
	POP.D	W10
	POP	W4
;NMRAnetStateMachine.mpas,676 :: 		if DataBytes[i] = #0 then
	ADD	W14, #0, W1
	ADD	W14, #8, W0
	ADD	W1, [W0], W0
	MOV.B	[W0], W1
	MOV.B	#0, W0
	CP.B	W1, W0
	BRA Z	L_NMRAnetStateMachine_ProcessNode672
	GOTO	L_NMRAnetStateMachine_ProcessNode238
L_NMRAnetStateMachine_ProcessNode672:
;NMRAnetStateMachine.mpas,677 :: 		BaseBuffer^.StateMachine := STATE_ACDI_DONE
	ADD	W4, #7, W1
	MOV.B	#7, W0
	MOV.B	W0, [W1]
	GOTO	L_NMRAnetStateMachine_ProcessNode239
;NMRAnetStateMachine.mpas,678 :: 		else
L_NMRAnetStateMachine_ProcessNode238:
;NMRAnetStateMachine.mpas,679 :: 		if BaseBuffer^.Tag = MAX_CONFIG_DATA - 1 then
	ADD	W4, #8, W0
	MOV	[W0], W1
	MOV	#59, W0
	CP	W1, W0
	BRA Z	L_NMRAnetStateMachine_ProcessNode673
	GOTO	L_NMRAnetStateMachine_ProcessNode241
L_NMRAnetStateMachine_ProcessNode673:
;NMRAnetStateMachine.mpas,680 :: 		DataBytes[i] := #0;
	ADD	W14, #0, W1
	ADD	W14, #8, W0
	ADD	W1, [W0], W1
	MOV.B	#0, W0
	MOV.B	W0, [W1]
L_NMRAnetStateMachine_ProcessNode241:
L_NMRAnetStateMachine_ProcessNode239:
;NMRAnetStateMachine.mpas,681 :: 		Inc(i);
	MOV	[W14+8], W1
	ADD	W14, #8, W0
	ADD	W1, #1, [W0]
;NMRAnetStateMachine.mpas,682 :: 		Inc(BaseBuffer^.Tag);
	ADD	W4, #8, W1
	MOV	[W1], W0
	INC	W0
	MOV	W0, [W1]
;NMRAnetStateMachine.mpas,683 :: 		end else
	GOTO	L_NMRAnetStateMachine_ProcessNode236
L_NMRAnetStateMachine_ProcessNode235:
;NMRAnetStateMachine.mpas,684 :: 		BaseBuffer^.StateMachine := STATE_ACDI_DONE;
	ADD	W4, #7, W1
	MOV.B	#7, W0
	MOV.B	W0, [W1]
L_NMRAnetStateMachine_ProcessNode236:
;NMRAnetStateMachine.mpas,685 :: 		end;
	GOTO	L_NMRAnetStateMachine_ProcessNode197
L_NMRAnetStateMachine_ProcessNode233:
;NMRAnetStateMachine.mpas,686 :: 		STATE_ACDI_DONE         : begin      {$IFDEF TRACE_SNIP}UART1_Write_Text('STATE_ACDI_DONE'+LF); {$ENDIF}
	MOV.B	[W14+60], W0
	CP.B	W0, #7
	BRA Z	L_NMRAnetStateMachine_ProcessNode674
	GOTO	L_NMRAnetStateMachine_ProcessNode245
L_NMRAnetStateMachine_ProcessNode674:
;NMRAnetStateMachine.mpas,690 :: 		end;
	GOTO	L_NMRAnetStateMachine_ProcessNode197
L_NMRAnetStateMachine_ProcessNode245:
L_NMRAnetStateMachine_ProcessNode197:
;NMRAnetStateMachine.mpas,692 :: 		end;
	GOTO	L_NMRAnetStateMachine_ProcessNode193
L_NMRAnetStateMachine_ProcessNode194:
;NMRAnetStateMachine.mpas,694 :: 		if i > 0 then
	MOV	[W14+8], W0
	CP	W0, #0
	BRA GT	L_NMRAnetStateMachine_ProcessNode675
	GOTO	L_NMRAnetStateMachine_ProcessNode247
L_NMRAnetStateMachine_ProcessNode675:
;NMRAnetStateMachine.mpas,695 :: 		TransmitNMRABusLayerMsg(Node, CANBuffer, MTI_SIMPLE_NODE_INFO_REPLY, BaseBuffer^.Alias, i, @DataBytes, False);
	ADD	W4, #2, W2
	ADD	W14, #0, W1
	PUSH	W4
	PUSH	W11
	MOV	#32768, W12
	MOV	#2464, W13
	CLR	W0
	PUSH	W0
	PUSH	W1
	ADD	W14, #8, W0
	PUSH	[W0]
	PUSH	[W2]
	CALL	_TransmitNMRABusLayerMsg
	SUB	#8, W15
	POP	W11
	POP	W4
L_NMRAnetStateMachine_ProcessNode247:
;NMRAnetStateMachine.mpas,697 :: 		if BaseBuffer^.StateMachine >= STATE_ACDI_DONE then
	ADD	W4, #7, W0
	MOV.B	[W0], W0
	CP.B	W0, #7
	BRA GEU	L_NMRAnetStateMachine_ProcessNode676
	GOTO	L_NMRAnetStateMachine_ProcessNode250
L_NMRAnetStateMachine_ProcessNode676:
;NMRAnetStateMachine.mpas,699 :: 		NMRAnetUtilities_BaseBufferUnLink(Node, BaseBuffer);
	PUSH	W11
	MOV	W4, W11
	CALL	_NMRAnetUtilities_BaseBufferUnLink
	POP	W11
;NMRAnetStateMachine.mpas,700 :: 		NMRAnetBufferPools_ReleaseBaseBuffer(BaseBuffer);
	PUSH	W10
	MOV	W4, W10
	CALL	_NMRAnetBufferPools_ReleaseBaseBuffer
	POP	W10
;NMRAnetStateMachine.mpas,701 :: 		end;
L_NMRAnetStateMachine_ProcessNode250:
;NMRAnetStateMachine.mpas,702 :: 		end
	MOV	W4, W0
	GOTO	L_NMRAnetStateMachine_ProcessNode190
; BaseBuffer end address is: 8 (W4)
L_NMRAnetStateMachine_ProcessNode574:
;NMRAnetStateMachine.mpas,612 :: 		if NMRABusBufferAvailable then
	MOV	W4, W0
;NMRAnetStateMachine.mpas,702 :: 		end
L_NMRAnetStateMachine_ProcessNode190:
;NMRAnetStateMachine.mpas,703 :: 		end;
; BaseBuffer start address is: 0 (W0)
; BaseBuffer end address is: 0 (W0)
	GOTO	L_NMRAnetStateMachine_ProcessNode156
L_NMRAnetStateMachine_ProcessNode188:
; BaseBuffer start address is: 8 (W4)
	MOV	W4, W0
L_NMRAnetStateMachine_ProcessNode156:
; BaseBuffer end address is: 8 (W4)
;NMRAnetStateMachine.mpas,707 :: 		end;
; BaseBuffer start address is: 0 (W0)
; BaseBuffer end address is: 0 (W0)
	GOTO	L_NMRAnetStateMachine_ProcessNode154
L_NMRAnetStateMachine_ProcessNode575:
;NMRAnetStateMachine.mpas,584 :: 		if BaseBuffer <> nil then
	MOV	W4, W0
;NMRAnetStateMachine.mpas,707 :: 		end;
L_NMRAnetStateMachine_ProcessNode154:
;NMRAnetStateMachine.mpas,709 :: 		if BaseBuffer <> nil then
; BaseBuffer start address is: 0 (W0)
	CP	W0, #0
	BRA NZ	L_NMRAnetStateMachine_ProcessNode677
	GOTO	L_NMRAnetStateMachine_ProcessNode253
L_NMRAnetStateMachine_ProcessNode677:
; BaseBuffer end address is: 0 (W0)
;NMRAnetStateMachine.mpas,710 :: 		Exit;              // Don't interleave Buffer Replies...
	GOTO	L_end_NMRAnetStateMachine_ProcessNode
L_NMRAnetStateMachine_ProcessNode253:
;NMRAnetStateMachine.mpas,713 :: 		if NMRABusBufferAvailable then
	CALL	_NMRABusBufferAvailable
	CP0	W0
	BRA NZ	L_NMRAnetStateMachine_ProcessNode678
	GOTO	L_NMRAnetStateMachine_ProcessNode256
L_NMRAnetStateMachine_ProcessNode678:
;NMRAnetStateMachine.mpas,715 :: 		if NMRAnetNode_IsAnyConsumerEventSet(Node) then
	CALL	_NMRAnetNode_IsAnyConsumerEventSet
	CP0	W0
	BRA NZ	L_NMRAnetStateMachine_ProcessNode679
	GOTO	L_NMRAnetStateMachine_ProcessNode259
L_NMRAnetStateMachine_ProcessNode679:
;NMRAnetStateMachine.mpas,717 :: 		EventIndex := NMRAnetNode_NextConsumerEventFlag(Node, State);
	ADD	W14, #14, W0
	PUSH	W11
	MOV	W0, W11
	CALL	_NMRAnetNode_NextConsumerEventFlag
	POP	W11
; EventIndex start address is: 4 (W2)
	MOV	W0, W2
;NMRAnetStateMachine.mpas,718 :: 		if EventIndex > -1 then
	MOV	#65535, W1
	CP	W0, W1
	BRA GT	L_NMRAnetStateMachine_ProcessNode680
	GOTO	L_NMRAnetStateMachine_ProcessNode262
L_NMRAnetStateMachine_ProcessNode680:
;NMRAnetStateMachine.mpas,721 :: 		if NMRAnetNode_TestStateFlag(Node, NS_VIRTUAL) then
	PUSH	W11
	MOV.B	#8, W11
	CALL	_NMRAnetNode_TestStateFlag
	POP	W11
	CP0	W0
	BRA NZ	L_NMRAnetStateMachine_ProcessNode681
	GOTO	L_NMRAnetStateMachine_ProcessNode265
L_NMRAnetStateMachine_ProcessNode681:
;NMRAnetStateMachine.mpas,722 :: 		DataBytesPtr := PCAN_DataBytes( @SUPPORTED_VNODE_EVENTS_CONSUMED[EventIndex])
	SL	W2, #3, W1
; EventIndex end address is: 4 (W2)
	MOV	#lo_addr(_SUPPORTED_VNODE_EVENTS_CONSUMED), W0
	ADD	W0, W1, W0
; DataBytesPtr start address is: 18 (W9)
	MOV	W0, W9
; DataBytesPtr end address is: 18 (W9)
	GOTO	L_NMRAnetStateMachine_ProcessNode266
;NMRAnetStateMachine.mpas,723 :: 		else
L_NMRAnetStateMachine_ProcessNode265:
;NMRAnetStateMachine.mpas,725 :: 		DataBytesPtr := PCAN_DataBytes( @SUPPORTED_EVENTS_CONSUMED[EventIndex]);
; EventIndex start address is: 4 (W2)
	SL	W2, #3, W1
; EventIndex end address is: 4 (W2)
	MOV	#lo_addr(_SUPPORTED_EVENTS_CONSUMED), W0
	ADD	W0, W1, W0
; DataBytesPtr start address is: 18 (W9)
	MOV	W0, W9
; DataBytesPtr end address is: 18 (W9)
L_NMRAnetStateMachine_ProcessNode266:
;NMRAnetStateMachine.mpas,727 :: 		EVENT_STATE_UNKOWN : TransmitNMRABusLayerMsg(Node, CANBuffer, MTI_CONSUMER_IDENTIFIED_UNKNOWN, 0, 8, DataBytesPtr, False);
; DataBytesPtr start address is: 18 (W9)
	MOV.B	[W14+14], W0
	CP.B	W0, #3
	BRA Z	L_NMRAnetStateMachine_ProcessNode682
	GOTO	L_NMRAnetStateMachine_ProcessNode270
L_NMRAnetStateMachine_ProcessNode682:
	PUSH	W11
	MOV	#28672, W12
	MOV	#2380, W13
	CLR	W0
	PUSH	W0
	PUSH	W9
; DataBytesPtr end address is: 18 (W9)
	MOV	#8, W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	CALL	_TransmitNMRABusLayerMsg
	SUB	#8, W15
	POP	W11
	GOTO	L_NMRAnetStateMachine_ProcessNode267
L_NMRAnetStateMachine_ProcessNode270:
;NMRAnetStateMachine.mpas,728 :: 		EVENT_STATE_VALID : TransmitNMRABusLayerMsg(Node, CANBuffer, MTI_CONSUMER_IDENTIFIED_SET, 0, 8, DataBytesPtr, False);
; DataBytesPtr start address is: 18 (W9)
	MOV.B	[W14+14], W0
	CP.B	W0, #1
	BRA Z	L_NMRAnetStateMachine_ProcessNode683
	GOTO	L_NMRAnetStateMachine_ProcessNode273
L_NMRAnetStateMachine_ProcessNode683:
	PUSH	W11
	MOV	#16384, W12
	MOV	#2380, W13
	CLR	W0
	PUSH	W0
	PUSH	W9
; DataBytesPtr end address is: 18 (W9)
	MOV	#8, W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	CALL	_TransmitNMRABusLayerMsg
	SUB	#8, W15
	POP	W11
	GOTO	L_NMRAnetStateMachine_ProcessNode267
L_NMRAnetStateMachine_ProcessNode273:
;NMRAnetStateMachine.mpas,729 :: 		EVENT_STATE_INVALID : TransmitNMRABusLayerMsg(Node, CANBuffer, MTI_CONSUMER_IDENTIFIED_CLEAR, 0, 8, DataBytesPtr, False);
; DataBytesPtr start address is: 18 (W9)
	MOV.B	[W14+14], W0
	CP.B	W0, #2
	BRA Z	L_NMRAnetStateMachine_ProcessNode684
	GOTO	L_NMRAnetStateMachine_ProcessNode276
L_NMRAnetStateMachine_ProcessNode684:
	PUSH	W11
	MOV	#20480, W12
	MOV	#2380, W13
	CLR	W0
	PUSH	W0
	PUSH	W9
; DataBytesPtr end address is: 18 (W9)
	MOV	#8, W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	CALL	_TransmitNMRABusLayerMsg
	SUB	#8, W15
	POP	W11
	GOTO	L_NMRAnetStateMachine_ProcessNode267
L_NMRAnetStateMachine_ProcessNode276:
L_NMRAnetStateMachine_ProcessNode267:
;NMRAnetStateMachine.mpas,731 :: 		end
L_NMRAnetStateMachine_ProcessNode262:
;NMRAnetStateMachine.mpas,732 :: 		end else
	GOTO	L_NMRAnetStateMachine_ProcessNode260
L_NMRAnetStateMachine_ProcessNode259:
;NMRAnetStateMachine.mpas,733 :: 		if NMRAnetNode_IsAnyProducerEventSet(Node) then
	CALL	_NMRAnetNode_IsAnyProducerEventSet
	CP0	W0
	BRA NZ	L_NMRAnetStateMachine_ProcessNode685
	GOTO	L_NMRAnetStateMachine_ProcessNode278
L_NMRAnetStateMachine_ProcessNode685:
;NMRAnetStateMachine.mpas,735 :: 		EventIndex := NMRAnetNode_NextProducerEventFlag(Node, State);
	ADD	W14, #14, W0
	PUSH	W11
	MOV	W0, W11
	CALL	_NMRAnetNode_NextProducerEventFlag
	POP	W11
; EventIndex start address is: 4 (W2)
	MOV	W0, W2
;NMRAnetStateMachine.mpas,736 :: 		if EventIndex > -1 then
	MOV	#65535, W1
	CP	W0, W1
	BRA GT	L_NMRAnetStateMachine_ProcessNode686
	GOTO	L_NMRAnetStateMachine_ProcessNode281
L_NMRAnetStateMachine_ProcessNode686:
;NMRAnetStateMachine.mpas,739 :: 		if NMRAnetNode_TestStateFlag(Node, NS_VIRTUAL) then
	PUSH	W11
	MOV.B	#8, W11
	CALL	_NMRAnetNode_TestStateFlag
	POP	W11
	CP0	W0
	BRA NZ	L_NMRAnetStateMachine_ProcessNode687
	GOTO	L_NMRAnetStateMachine_ProcessNode284
L_NMRAnetStateMachine_ProcessNode687:
;NMRAnetStateMachine.mpas,740 :: 		DataBytesPtr := PCAN_DataBytes( @SUPPORTED_VNODE_EVENTS_PRODUCED[EventIndex])
	SL	W2, #3, W1
; EventIndex end address is: 4 (W2)
	MOV	#lo_addr(_SUPPORTED_VNODE_EVENTS_PRODUCED), W0
	ADD	W0, W1, W0
; DataBytesPtr start address is: 18 (W9)
	MOV	W0, W9
; DataBytesPtr end address is: 18 (W9)
	GOTO	L_NMRAnetStateMachine_ProcessNode285
;NMRAnetStateMachine.mpas,741 :: 		else
L_NMRAnetStateMachine_ProcessNode284:
;NMRAnetStateMachine.mpas,743 :: 		DataBytesPtr := PCAN_DataBytes( @SUPPORTED_EVENTS_PRODUCED[EventIndex]);
; EventIndex start address is: 4 (W2)
	SL	W2, #3, W1
; EventIndex end address is: 4 (W2)
	MOV	#lo_addr(_SUPPORTED_EVENTS_PRODUCED), W0
	ADD	W0, W1, W0
; DataBytesPtr start address is: 18 (W9)
	MOV	W0, W9
; DataBytesPtr end address is: 18 (W9)
L_NMRAnetStateMachine_ProcessNode285:
;NMRAnetStateMachine.mpas,745 :: 		EVENT_STATE_UNKOWN : TransmitNMRABusLayerMsg(Node, CANBuffer, MTI_PRODUCER_IDENTIFIED_UNKNOWN, 0, 8, DataBytesPtr, False);
; DataBytesPtr start address is: 18 (W9)
	MOV.B	[W14+14], W0
	CP.B	W0, #3
	BRA Z	L_NMRAnetStateMachine_ProcessNode688
	GOTO	L_NMRAnetStateMachine_ProcessNode289
L_NMRAnetStateMachine_ProcessNode688:
	PUSH	W11
	MOV	#28672, W12
	MOV	#2388, W13
	CLR	W0
	PUSH	W0
	PUSH	W9
; DataBytesPtr end address is: 18 (W9)
	MOV	#8, W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	CALL	_TransmitNMRABusLayerMsg
	SUB	#8, W15
	POP	W11
	GOTO	L_NMRAnetStateMachine_ProcessNode286
L_NMRAnetStateMachine_ProcessNode289:
;NMRAnetStateMachine.mpas,746 :: 		EVENT_STATE_VALID : TransmitNMRABusLayerMsg(Node, CANBuffer, MTI_PRODUCER_IDENTIFIED_SET, 0, 8, DataBytesPtr, False);
; DataBytesPtr start address is: 18 (W9)
	MOV.B	[W14+14], W0
	CP.B	W0, #1
	BRA Z	L_NMRAnetStateMachine_ProcessNode689
	GOTO	L_NMRAnetStateMachine_ProcessNode292
L_NMRAnetStateMachine_ProcessNode689:
	PUSH	W11
	MOV	#16384, W12
	MOV	#2388, W13
	CLR	W0
	PUSH	W0
	PUSH	W9
; DataBytesPtr end address is: 18 (W9)
	MOV	#8, W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	CALL	_TransmitNMRABusLayerMsg
	SUB	#8, W15
	POP	W11
	GOTO	L_NMRAnetStateMachine_ProcessNode286
L_NMRAnetStateMachine_ProcessNode292:
;NMRAnetStateMachine.mpas,747 :: 		EVENT_STATE_INVALID : TransmitNMRABusLayerMsg(Node, CANBuffer, MTI_PRODUCER_IDENTIFIED_CLEAR, 0, 8, DataBytesPtr, False);
; DataBytesPtr start address is: 18 (W9)
	MOV.B	[W14+14], W0
	CP.B	W0, #2
	BRA Z	L_NMRAnetStateMachine_ProcessNode690
	GOTO	L_NMRAnetStateMachine_ProcessNode295
L_NMRAnetStateMachine_ProcessNode690:
	PUSH	W11
	MOV	#20480, W12
	MOV	#2388, W13
	CLR	W0
	PUSH	W0
	PUSH	W9
; DataBytesPtr end address is: 18 (W9)
	MOV	#8, W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	CALL	_TransmitNMRABusLayerMsg
	SUB	#8, W15
	POP	W11
	GOTO	L_NMRAnetStateMachine_ProcessNode286
L_NMRAnetStateMachine_ProcessNode295:
L_NMRAnetStateMachine_ProcessNode286:
;NMRAnetStateMachine.mpas,749 :: 		end
L_NMRAnetStateMachine_ProcessNode281:
;NMRAnetStateMachine.mpas,750 :: 		end;
L_NMRAnetStateMachine_ProcessNode278:
L_NMRAnetStateMachine_ProcessNode260:
;NMRAnetStateMachine.mpas,751 :: 		end;
L_NMRAnetStateMachine_ProcessNode256:
;NMRAnetStateMachine.mpas,754 :: 		DatagramBuffer := NMRAnetUtilities_NextDatagramBuffer(Node);                // Grab the next completed Datagram to work on
	CALL	_NMRAnetUtilities_NextDatagramBuffer
; DatagramBuffer start address is: 16 (W8)
	MOV	W0, W8
;NMRAnetStateMachine.mpas,755 :: 		if DatagramBuffer <> nil then
	CP	W0, #0
	BRA NZ	L_NMRAnetStateMachine_ProcessNode691
	GOTO	L_NMRAnetStateMachine_ProcessNode297
L_NMRAnetStateMachine_ProcessNode691:
;NMRAnetStateMachine.mpas,763 :: 		case DatagramBuffer^.mCode of
	ADD	W8, #6, W0
	MOV.B	[W0], W0
	MOV.B	W0, [W14+66]
;NMRAnetStateMachine.mpas,769 :: 		BMC_DATAGRAM_MEMORY_CONFIG         : begin
	CP.B	W0, #8
	BRA Z	L_NMRAnetStateMachine_ProcessNode692
	GOTO	L_NMRAnetStateMachine_ProcessNode302
L_NMRAnetStateMachine_ProcessNode692:
;NMRAnetStateMachine.mpas,770 :: 		case DatagramBuffer^.StateMachine of
	ADD	W8, #7, W0
	MOV.B	[W0], W0
	MOV.B	W0, [W14+64]
;NMRAnetStateMachine.mpas,771 :: 		STATE_MEM_CONFIG_SEND_REPLY           : begin  {$IFDEF TRACE_MEM_CONFIG_STATEMACHINE}  UART1_Write_Text('STATE_MEM_CONFIG_SEND_REPLY'+LF); {$ENDIF}
	CP.B	W0, #0
	BRA Z	L_NMRAnetStateMachine_ProcessNode693
	GOTO	L_NMRAnetStateMachine_ProcessNode306
L_NMRAnetStateMachine_ProcessNode693:
;NMRAnetStateMachine.mpas,772 :: 		if DatagramBuffer^.DataBytes[1] and $C0 = MCP_OPERATION then
	ADD	W8, #11, W0
	INC	W0
	ZE	[W0], W1
	MOV	#192, W0
	AND	W1, W0, W1
	MOV	#128, W0
	CP	W1, W0
	BRA Z	L_NMRAnetStateMachine_ProcessNode694
	GOTO	L_NMRAnetStateMachine_ProcessNode308
L_NMRAnetStateMachine_ProcessNode694:
;NMRAnetStateMachine.mpas,774 :: 		if NMRABusBufferAvailable then
	CALL	_NMRABusBufferAvailable
	CP0	W0
	BRA NZ	L_NMRAnetStateMachine_ProcessNode695
	GOTO	L_NMRAnetStateMachine_ProcessNode311
L_NMRAnetStateMachine_ProcessNode695:
;NMRAnetStateMachine.mpas,776 :: 		TransmitNMRABusLayerMsg(Node, CANBuffer, MTI_DATAGRAM_OK_REPLY, DatagramBuffer^.Alias, 0, @DataBytes, False);
	ADD	W8, #2, W2
	ADD	W14, #0, W1
	PUSH	W8
	PUSH	W11
	MOV	#32768, W12
	MOV	#2466, W13
	CLR	W0
	PUSH	W0
	PUSH	W1
	CLR	W0
	PUSH	W0
	PUSH	[W2]
	CALL	_TransmitNMRABusLayerMsg
	SUB	#8, W15
	POP	W11
	POP	W8
;NMRAnetStateMachine.mpas,777 :: 		DatagramBuffer^.Statemachine := STATE_MEM_CONFIG_OPERATION_DATAGRAM
	ADD	W8, #7, W1
; DatagramBuffer end address is: 16 (W8)
	MOV.B	#1, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,778 :: 		end
L_NMRAnetStateMachine_ProcessNode311:
;NMRAnetStateMachine.mpas,779 :: 		end else
	GOTO	L_NMRAnetStateMachine_ProcessNode309
L_NMRAnetStateMachine_ProcessNode308:
;NMRAnetStateMachine.mpas,780 :: 		if DatagramBuffer^.DataBytes[1] and $C0 = MCP_READ then
; DatagramBuffer start address is: 16 (W8)
	ADD	W8, #11, W0
	INC	W0
	ZE	[W0], W1
	MOV	#192, W0
	AND	W1, W0, W1
	MOV	#64, W0
	CP	W1, W0
	BRA Z	L_NMRAnetStateMachine_ProcessNode696
	GOTO	L_NMRAnetStateMachine_ProcessNode314
L_NMRAnetStateMachine_ProcessNode696:
;NMRAnetStateMachine.mpas,782 :: 		if NMRABusBufferAvailable then
	CALL	_NMRABusBufferAvailable
	CP0	W0
	BRA NZ	L_NMRAnetStateMachine_ProcessNode697
	GOTO	L_NMRAnetStateMachine_ProcessNode317
L_NMRAnetStateMachine_ProcessNode697:
;NMRAnetStateMachine.mpas,784 :: 		TransmitNMRABusLayerMsg(Node, CANBuffer, MTI_DATAGRAM_OK_REPLY, DatagramBuffer^.Alias, 0, @DataBytes, False);
	ADD	W8, #2, W2
	ADD	W14, #0, W1
	PUSH	W8
	PUSH	W11
	MOV	#32768, W12
	MOV	#2466, W13
	CLR	W0
	PUSH	W0
	PUSH	W1
	CLR	W0
	PUSH	W0
	PUSH	[W2]
	CALL	_TransmitNMRABusLayerMsg
	SUB	#8, W15
	POP	W11
	POP	W8
;NMRAnetStateMachine.mpas,785 :: 		DatagramBuffer^.Statemachine := STATE_MEM_CONFIG_READ_DATAGRAM
	ADD	W8, #7, W1
; DatagramBuffer end address is: 16 (W8)
	MOV.B	#2, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,786 :: 		end
L_NMRAnetStateMachine_ProcessNode317:
;NMRAnetStateMachine.mpas,787 :: 		end else
	GOTO	L_NMRAnetStateMachine_ProcessNode315
L_NMRAnetStateMachine_ProcessNode314:
;NMRAnetStateMachine.mpas,788 :: 		if DatagramBuffer^.DataBytes[1] and $C0 = MCP_WRITE then
; DatagramBuffer start address is: 16 (W8)
	ADD	W8, #11, W0
	INC	W0
	ZE	[W0], W1
	MOV	#192, W0
	AND	W1, W0, W0
	CP	W0, #0
	BRA Z	L_NMRAnetStateMachine_ProcessNode698
	GOTO	L_NMRAnetStateMachine_ProcessNode320
L_NMRAnetStateMachine_ProcessNode698:
;NMRAnetStateMachine.mpas,789 :: 		DatagramBuffer^.Statemachine := STATE_MEM_CONFIG_WRITE_DATAGRAM
	ADD	W8, #7, W1
; DatagramBuffer end address is: 16 (W8)
	MOV.B	#3, W0
	MOV.B	W0, [W1]
L_NMRAnetStateMachine_ProcessNode320:
L_NMRAnetStateMachine_ProcessNode315:
L_NMRAnetStateMachine_ProcessNode309:
;NMRAnetStateMachine.mpas,790 :: 		end;
	GOTO	L_NMRAnetStateMachine_ProcessNode303
L_NMRAnetStateMachine_ProcessNode306:
;NMRAnetStateMachine.mpas,791 :: 		STATE_MEM_CONFIG_OPERATION_DATAGRAM   : begin {$IFDEF TRACE_MEM_CONFIG_STATEMACHINE} UART1_Write_Text('STATE_MEM_OPERATION_DATAGRAM'+LF);    {$ENDIF}
; DatagramBuffer start address is: 16 (W8)
	MOV.B	[W14+64], W0
	CP.B	W0, #1
	BRA Z	L_NMRAnetStateMachine_ProcessNode699
	GOTO	L_NMRAnetStateMachine_ProcessNode324
L_NMRAnetStateMachine_ProcessNode699:
;NMRAnetStateMachine.mpas,793 :: 		case DatagramBuffer^.DataBytes[1] of      // Mask off the upper 2 bits
	ADD	W8, #11, W0
	INC	W0
	MOV.B	[W0], W2
	MOV.B	W2, [W14+60]
;NMRAnetStateMachine.mpas,794 :: 		MCP_OP_GET_CONFIG         : begin
	MOV.B	#128, W0
	CP.B	W2, W0
	BRA Z	L_NMRAnetStateMachine_ProcessNode700
	GOTO	L_NMRAnetStateMachine_ProcessNode328
L_NMRAnetStateMachine_ProcessNode700:
;NMRAnetStateMachine.mpas,795 :: 		DatagramBuffer^.DataBytes[0] := DATAGRAM_TYPE_MEMORY_CONFIGURATION;
	ADD	W8, #11, W1
	MOV.B	#32, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,796 :: 		DatagramBuffer^.DataBytes[1] := MCP_OP_GET_CONFIG_REPLY;
	ADD	W8, #11, W0
	ADD	W0, #1, W1
	MOV.B	#130, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,797 :: 		DatagramBuffer^.DataBytes[2] := Hi( MEMORY_CONFIG_OPTIONS.MemoryConfigOptions);
	ADD	W8, #11, W0
	ADD	W0, #2, W1
	MOV.B	#78, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,798 :: 		DatagramBuffer^.DataBytes[3] := Lo( MEMORY_CONFIG_OPTIONS.MemoryConfigOptions);
	ADD	W8, #11, W0
	ADD	W0, #3, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,799 :: 		DatagramBuffer^.DataBytes[4] := MEMORY_CONFIG_OPTIONS.MemoryConfigWriteLength;
	ADD	W8, #11, W0
	ADD	W0, #4, W1
	MOV.B	#242, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,800 :: 		DatagramBuffer^.DataBytes[5] := MEMORY_CONFIG_OPTIONS.MemoryConfigHighestSpace;
	ADD	W8, #11, W0
	ADD	W0, #5, W1
	MOV.B	#255, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,801 :: 		DatagramBuffer^.DataBytes[6] := MEMORY_CONFIG_OPTIONS.MemoryConfigLowestSpace;
	ADD	W8, #11, W0
	ADD	W0, #6, W1
	MOV.B	#251, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,802 :: 		DatagramBuffer^.iByteCount := 7;
	ADD	W8, #10, W1
	MOV.B	#7, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,803 :: 		end;
	GOTO	L_NMRAnetStateMachine_ProcessNode325
L_NMRAnetStateMachine_ProcessNode328:
;NMRAnetStateMachine.mpas,804 :: 		MCP_OP_GET_ADD_SPACE_INFO : begin
	MOV.B	#132, W0
	CP.B	W2, W0
	BRA Z	L_NMRAnetStateMachine_ProcessNode701
	GOTO	L_NMRAnetStateMachine_ProcessNode331
L_NMRAnetStateMachine_ProcessNode701:
;NMRAnetStateMachine.mpas,805 :: 		DatagramBuffer^.DataBytes[0] := DATAGRAM_TYPE_MEMORY_CONFIGURATION;
	ADD	W8, #11, W1
	MOV.B	#32, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,806 :: 		DatagramBuffer^.DataBytes[1] := MCP_OP_GET_ADD_SPACE_INFO_REPLY or MCP_OP_GET_ADD_SPACE_INFO_REPLY_PRESENT;  // assumption is we support ALL Address Spaces
	ADD	W8, #11, W0
	ADD	W0, #1, W1
	MOV.B	#135, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,807 :: 		DatagramBuffer^.DataBytes[2] := DatagramBuffer^.DataBytes[2];
	ADD	W8, #11, W0
	INC2	W0
;NMRAnetStateMachine.mpas,809 :: 		MemorySpaceMaxAddress := MaxAddressByAddressSpace(Node, DatagramBuffer^.DataBytes[2]);
	ADD	W8, #11, W0
	INC2	W0
	PUSH	W8
	PUSH.D	W10
	MOV.B	[W0], W11
	CALL	NMRAnetStateMachine_MaxAddressByAddressSpace
	POP.D	W10
	POP	W8
; MemorySpaceMaxAddress start address is: 10 (W5)
	MOV	W0, W5
	MOV	W1, W6
;NMRAnetStateMachine.mpas,810 :: 		DatagramBuffer^.DataBytes[3] := (DWord(MemorySpaceMaxAddress) shr 24) and $000000FF;
	ADD	W8, #11, W0
	ADD	W0, #3, W4
	MOV	#24, W0
	MOV	W5, W2
	MOV	W6, W3
L_NMRAnetStateMachine_ProcessNode702:
	DEC	W0, W0
	BRA LT	L_NMRAnetStateMachine_ProcessNode703
	LSR	W3, W3
	RRC	W2, W2
	BRA	L_NMRAnetStateMachine_ProcessNode702
L_NMRAnetStateMachine_ProcessNode703:
	MOV	#255, W0
	MOV	#0, W1
	AND	W2, W0, W0
	MOV.B	W0, [W4]
;NMRAnetStateMachine.mpas,811 :: 		DatagramBuffer^.DataBytes[4] := (DWord(MemorySpaceMaxAddress) shr 16) and $000000FF;
	ADD	W8, #11, W0
	ADD	W0, #4, W4
	MOV	W5, W2
	MOV	W6, W3
	MOV	W3, W2
	CLR	W3
	MOV	#255, W0
	MOV	#0, W1
	AND	W2, W0, W0
	MOV.B	W0, [W4]
;NMRAnetStateMachine.mpas,812 :: 		DatagramBuffer^.DataBytes[5] := (DWord(MemorySpaceMaxAddress) shr 8) and $000000FF;
	ADD	W8, #11, W0
	ADD	W0, #5, W4
	MOV	#8, W0
	MOV	W5, W2
	MOV	W6, W3
L_NMRAnetStateMachine_ProcessNode704:
	DEC	W0, W0
	BRA LT	L_NMRAnetStateMachine_ProcessNode705
	LSR	W3, W3
	RRC	W2, W2
	BRA	L_NMRAnetStateMachine_ProcessNode704
L_NMRAnetStateMachine_ProcessNode705:
	MOV	#255, W0
	MOV	#0, W1
	AND	W2, W0, W0
	MOV.B	W0, [W4]
;NMRAnetStateMachine.mpas,813 :: 		DatagramBuffer^.DataBytes[6] := DWord(MemorySpaceMaxAddress) and $000000FF;
	ADD	W8, #11, W0
	ADD	W0, #6, W2
	MOV	#255, W0
	MOV	#0, W1
	AND	W5, W0, W0
; MemorySpaceMaxAddress end address is: 10 (W5)
	MOV.B	W0, [W2]
;NMRAnetStateMachine.mpas,814 :: 		case DatagramBuffer^.DataBytes[2] of
	ADD	W8, #11, W0
	ADD	W0, #2, W2
;NMRAnetStateMachine.mpas,815 :: 		MSI_CONFIG, MSI_ACDI_USER : DatagramBuffer^.DataBytes[7] := $00          // Read/Write
	MOV.B	[W2], W1
	MOV.B	#253, W0
	CP.B	W1, W0
	BRA Z	L_NMRAnetStateMachine_ProcessNode706
	GOTO	L_NMRAnetStateMachine_ProcessNode335
L_NMRAnetStateMachine_ProcessNode706:
	GOTO	L_NMRAnetStateMachine_ProcessNode333
L_NMRAnetStateMachine_ProcessNode335:
	MOV.B	[W2], W1
	MOV.B	#251, W0
	CP.B	W1, W0
	BRA Z	L_NMRAnetStateMachine_ProcessNode707
	GOTO	L_NMRAnetStateMachine_ProcessNode336
L_NMRAnetStateMachine_ProcessNode707:
L_NMRAnetStateMachine_ProcessNode333:
	ADD	W8, #11, W0
	ADD	W0, #7, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,816 :: 		else
	GOTO	L_NMRAnetStateMachine_ProcessNode332
L_NMRAnetStateMachine_ProcessNode336:
;NMRAnetStateMachine.mpas,817 :: 		DatagramBuffer^.DataBytes[7] := $01;                                     // Read Only
	ADD	W8, #11, W0
	ADD	W0, #7, W1
	MOV.B	#1, W0
	MOV.B	W0, [W1]
L_NMRAnetStateMachine_ProcessNode332:
;NMRAnetStateMachine.mpas,819 :: 		DatagramBuffer^.iByteCount := 8;
	ADD	W8, #10, W1
	MOV.B	#8, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,820 :: 		end;
	GOTO	L_NMRAnetStateMachine_ProcessNode325
L_NMRAnetStateMachine_ProcessNode331:
;NMRAnetStateMachine.mpas,821 :: 		MCP_OP_LOCK               : begin
	MOV.B	#136, W0
	MOV.B	[W14+60], W2
	CP.B	W2, W0
	BRA Z	L_NMRAnetStateMachine_ProcessNode708
	GOTO	L_NMRAnetStateMachine_ProcessNode339
L_NMRAnetStateMachine_ProcessNode708:
;NMRAnetStateMachine.mpas,822 :: 		DatagramBuffer^.Statemachine := STATE_MEM_CONFIG_WAITFOR_REPLY
	ADD	W8, #7, W1
	MOV.B	#6, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,823 :: 		end;
	GOTO	L_NMRAnetStateMachine_ProcessNode325
L_NMRAnetStateMachine_ProcessNode339:
;NMRAnetStateMachine.mpas,824 :: 		MCP_OP_GET_UNIQUEID       : begin
	MOV.B	#140, W0
	CP.B	W2, W0
	BRA Z	L_NMRAnetStateMachine_ProcessNode709
	GOTO	L_NMRAnetStateMachine_ProcessNode342
L_NMRAnetStateMachine_ProcessNode709:
;NMRAnetStateMachine.mpas,825 :: 		DatagramBuffer^.Statemachine := STATE_MEM_CONFIG_WAITFOR_REPLY
	ADD	W8, #7, W1
	MOV.B	#6, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,826 :: 		end;
	GOTO	L_NMRAnetStateMachine_ProcessNode325
L_NMRAnetStateMachine_ProcessNode342:
;NMRAnetStateMachine.mpas,827 :: 		MCP_OP_FREEZE             : begin
	MOV.B	#160, W0
	CP.B	W2, W0
	BRA Z	L_NMRAnetStateMachine_ProcessNode710
	GOTO	L_NMRAnetStateMachine_ProcessNode345
L_NMRAnetStateMachine_ProcessNode710:
;NMRAnetStateMachine.mpas,828 :: 		DatagramBuffer^.Statemachine := STATE_MEM_CONFIG_WAITFOR_REPLY
	ADD	W8, #7, W1
	MOV.B	#6, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,829 :: 		end;
	GOTO	L_NMRAnetStateMachine_ProcessNode325
L_NMRAnetStateMachine_ProcessNode345:
;NMRAnetStateMachine.mpas,830 :: 		MCP_OP_INDICATE           : begin
	MOV.B	#164, W0
	CP.B	W2, W0
	BRA Z	L_NMRAnetStateMachine_ProcessNode711
	GOTO	L_NMRAnetStateMachine_ProcessNode348
L_NMRAnetStateMachine_ProcessNode711:
;NMRAnetStateMachine.mpas,831 :: 		DatagramBuffer^.Statemachine := STATE_MEM_CONFIG_WAITFOR_REPLY
	ADD	W8, #7, W1
	MOV.B	#6, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,832 :: 		end;
	GOTO	L_NMRAnetStateMachine_ProcessNode325
L_NMRAnetStateMachine_ProcessNode348:
;NMRAnetStateMachine.mpas,833 :: 		MCP_OP_RESETS             : begin
	MOV.B	#168, W0
	CP.B	W2, W0
	BRA Z	L_NMRAnetStateMachine_ProcessNode712
	GOTO	L_NMRAnetStateMachine_ProcessNode351
L_NMRAnetStateMachine_ProcessNode712:
;NMRAnetStateMachine.mpas,834 :: 		DatagramBuffer^.Statemachine := STATE_MEM_CONFIG_WAITFOR_REPLY
	ADD	W8, #7, W1
	MOV.B	#6, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,835 :: 		end;
	GOTO	L_NMRAnetStateMachine_ProcessNode325
L_NMRAnetStateMachine_ProcessNode351:
L_NMRAnetStateMachine_ProcessNode325:
;NMRAnetStateMachine.mpas,837 :: 		DatagramBuffer^.Statemachine := STATE_MEM_CONFIG_SEND_RESUSED_DATAGRAM
	ADD	W8, #7, W1
; DatagramBuffer end address is: 16 (W8)
	MOV.B	#5, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,838 :: 		end;
	GOTO	L_NMRAnetStateMachine_ProcessNode303
L_NMRAnetStateMachine_ProcessNode324:
;NMRAnetStateMachine.mpas,839 :: 		STATE_MEM_CONFIG_READ_DATAGRAM         : begin {$IFDEF TRACE_MEM_CONFIG_STATEMACHINE} UART1_Write_Text('STATE_MEM_CONFIG_READ_DATAGRAM'+LF);    {$ENDIF}
; DatagramBuffer start address is: 16 (W8)
	MOV.B	[W14+64], W0
	CP.B	W0, #2
	BRA Z	L_NMRAnetStateMachine_ProcessNode713
	GOTO	L_NMRAnetStateMachine_ProcessNode354
L_NMRAnetStateMachine_ProcessNode713:
;NMRAnetStateMachine.mpas,840 :: 		MemorySpace := 0;
	CLR	W0
	MOV.B	W0, [W14+12]
;NMRAnetStateMachine.mpas,841 :: 		MemorySpaceAddress := 0;
	CLR	W0
	CLR	W1
	MOV	W0, [W14+16]
	MOV	W1, [W14+18]
;NMRAnetStateMachine.mpas,842 :: 		MemorySpaceCount := 0;
	CLR	W0
	MOV.B	W0, [W14+13]
;NMRAnetStateMachine.mpas,843 :: 		DecodeMemoryConfigurationReadWrite(Node, @DatagramBuffer^.DataBytes, MemorySpace, MemorySpaceAddress, MemorySpaceCount);
	ADD	W8, #11, W2
	ADD	W14, #13, W1
	ADD	W14, #16, W0
	PUSH	W8
	PUSH.D	W10
	MOV	W0, W13
	ADD	W14, #12, W0
	MOV	W0, W12
	MOV	W2, W11
	PUSH	W1
	CALL	NMRAnetStateMachine_DecodeMemoryConfigurationReadWrite
	SUB	#2, W15
	POP.D	W10
	POP	W8
;NMRAnetStateMachine.mpas,846 :: 		DatagramBuffer^.DataBytes[1] := MCP_READ_DATAGRAM_REPLY or DatagramBuffer^.DataBytes[1];
	ADD	W8, #11, W0
	ADD	W0, #1, W2
	MOV	#80, W1
	ZE	[W2], W0
	IOR	W1, W0, W0
	MOV.B	W0, [W2]
;NMRAnetStateMachine.mpas,849 :: 		if DatagramBuffer^.DataBytes[1] and $03 = 0 then
	ADD	W8, #11, W0
	INC	W0
	ZE	[W0], W0
	AND	W0, #3, W0
	CP	W0, #0
	BRA Z	L_NMRAnetStateMachine_ProcessNode714
	GOTO	L_NMRAnetStateMachine_ProcessNode356
L_NMRAnetStateMachine_ProcessNode714:
;NMRAnetStateMachine.mpas,850 :: 		DatagramAddress := 7
; DatagramAddress start address is: 6 (W3)
	MOV.B	#7, W3
; DatagramAddress end address is: 6 (W3)
	GOTO	L_NMRAnetStateMachine_ProcessNode357
;NMRAnetStateMachine.mpas,851 :: 		else
L_NMRAnetStateMachine_ProcessNode356:
;NMRAnetStateMachine.mpas,852 :: 		DatagramAddress := 6;
; DatagramAddress start address is: 6 (W3)
	MOV.B	#6, W3
; DatagramAddress end address is: 6 (W3)
L_NMRAnetStateMachine_ProcessNode357:
;NMRAnetStateMachine.mpas,853 :: 		DatagramBuffer^.iByteCount := DatagramAddress;  // Start out with the 6 or 7 bytes for the header
; DatagramAddress start address is: 6 (W3)
	ADD	W8, #10, W0
	MOV.B	W3, [W0]
;NMRAnetStateMachine.mpas,856 :: 		MSI_CDI       : begin {$IFDEF SUPPORT_VIRTUAL_NODES}
	MOV.B	[W14+12], W1
	MOV.B	#255, W0
	CP.B	W1, W0
	BRA Z	L_NMRAnetStateMachine_ProcessNode715
	GOTO	L_NMRAnetStateMachine_ProcessNode361
L_NMRAnetStateMachine_ProcessNode715:
;NMRAnetStateMachine.mpas,857 :: 		if Node^.State and NS_VIRTUAL <> 0 then
	MOV.B	[W10], W0
	ZE	W0, W0
	AND	W0, #8, W0
	CP	W0, #0
	BRA NZ	L_NMRAnetStateMachine_ProcessNode716
	GOTO	L_NMRAnetStateMachine_ProcessNode363
L_NMRAnetStateMachine_ProcessNode716:
;NMRAnetStateMachine.mpas,859 :: 		for i := 0 to MemorySpaceCount - 1 do
	CLR	W0
	MOV	W0, [W14+8]
; DatagramAddress end address is: 6 (W3)
; DatagramBuffer end address is: 16 (W8)
	MOV	W8, W6
	MOV.B	W3, W5
L_NMRAnetStateMachine_ProcessNode365:
; DatagramAddress start address is: 10 (W5)
; DatagramAddress start address is: 10 (W5)
; DatagramAddress end address is: 10 (W5)
; DatagramBuffer start address is: 12 (W6)
	ADD	W14, #13, W0
	SE	[W0], W0
	SUB	W0, #1, W4
	ADD	W14, #8, W0
	CP	W4, [W0]
	BRA GE	L_NMRAnetStateMachine_ProcessNode717
	GOTO	L_NMRAnetStateMachine_ProcessNode369
L_NMRAnetStateMachine_ProcessNode717:
; DatagramAddress end address is: 10 (W5)
;NMRAnetStateMachine.mpas,860 :: 		DatagramBuffer^.DataBytes[i+DatagramAddress] := CDI_ARRAY_VNODE[i+MemorySpaceAddress]
; DatagramAddress start address is: 10 (W5)
	ADD	W6, #11, W2
	ZE	W5, W1
	ADD	W14, #8, W0
	ADD	W1, [W0], W0
	ADD	W2, W0, W3
	MOV	[W14+8], W2
	ADD	W14, #16, W0
	ADD	W2, [W0], W1
	MOV	#lo_addr(_CDI_ARRAY_VNODE), W0
	ADD	W0, W1, W1
	MOV	#___Lib_System_DefaultPage, W0
	MOV	WREG, 50
	MOV.B	[W1], W0
	MOV.B	W0, [W3]
	ADD	W14, #8, W0
	CP	W4, [W0]
	BRA NZ	L_NMRAnetStateMachine_ProcessNode718
	GOTO	L_NMRAnetStateMachine_ProcessNode369
L_NMRAnetStateMachine_ProcessNode718:
	MOV	[W14+8], W1
	ADD	W14, #8, W0
	ADD	W1, #1, [W0]
; DatagramAddress end address is: 10 (W5)
	GOTO	L_NMRAnetStateMachine_ProcessNode365
L_NMRAnetStateMachine_ProcessNode369:
;NMRAnetStateMachine.mpas,861 :: 		end else {$ENDIF}
	MOV	W6, W3
; DatagramBuffer end address is: 12 (W6)
	GOTO	L_NMRAnetStateMachine_ProcessNode364
L_NMRAnetStateMachine_ProcessNode363:
;NMRAnetStateMachine.mpas,863 :: 		for i := 0 to MemorySpaceCount - 1 do
; DatagramBuffer start address is: 16 (W8)
; DatagramAddress start address is: 6 (W3)
	CLR	W0
	MOV	W0, [W14+8]
; DatagramAddress end address is: 6 (W3)
; DatagramBuffer end address is: 16 (W8)
	MOV	W8, W6
	MOV.B	W3, W5
L_NMRAnetStateMachine_ProcessNode370:
; DatagramAddress start address is: 10 (W5)
; DatagramAddress start address is: 10 (W5)
; DatagramAddress end address is: 10 (W5)
; DatagramBuffer start address is: 12 (W6)
	ADD	W14, #13, W0
	SE	[W0], W0
	SUB	W0, #1, W4
	ADD	W14, #8, W0
	CP	W4, [W0]
	BRA GE	L_NMRAnetStateMachine_ProcessNode719
	GOTO	L_NMRAnetStateMachine_ProcessNode374
L_NMRAnetStateMachine_ProcessNode719:
; DatagramAddress end address is: 10 (W5)
;NMRAnetStateMachine.mpas,864 :: 		DatagramBuffer^.DataBytes[i+DatagramAddress] := CDI_ARRAY[i+MemorySpaceAddress]
; DatagramAddress start address is: 10 (W5)
	ADD	W6, #11, W2
	ZE	W5, W1
	ADD	W14, #8, W0
	ADD	W1, [W0], W0
	ADD	W2, W0, W3
	MOV	[W14+8], W2
	ADD	W14, #16, W0
	ADD	W2, [W0], W1
	MOV	#lo_addr(_CDI_ARRAY), W0
	ADD	W0, W1, W1
	MOV	#___Lib_System_DefaultPage, W0
	MOV	WREG, 50
	MOV.B	[W1], W0
	MOV.B	W0, [W3]
	ADD	W14, #8, W0
	CP	W4, [W0]
	BRA NZ	L_NMRAnetStateMachine_ProcessNode720
	GOTO	L_NMRAnetStateMachine_ProcessNode374
L_NMRAnetStateMachine_ProcessNode720:
	MOV	[W14+8], W1
	ADD	W14, #8, W0
	ADD	W1, #1, [W0]
; DatagramAddress end address is: 10 (W5)
	GOTO	L_NMRAnetStateMachine_ProcessNode370
L_NMRAnetStateMachine_ProcessNode374:
;NMRAnetStateMachine.mpas,865 :: 		end;
	MOV	W6, W3
L_NMRAnetStateMachine_ProcessNode364:
; DatagramBuffer end address is: 12 (W6)
;NMRAnetStateMachine.mpas,866 :: 		DatagramBuffer^.iByteCount := DatagramBuffer^.iByteCount + MemorySpaceCount;
; DatagramBuffer start address is: 6 (W3)
	ADD	W3, #10, W2
	ZE	[W2], W1
	ADD	W14, #13, W0
	SE	[W0], W0
	ADD	W1, W0, W0
	MOV.B	W0, [W2]
;NMRAnetStateMachine.mpas,867 :: 		end;
	MOV	W3, W0
; DatagramBuffer end address is: 6 (W3)
	GOTO	L_NMRAnetStateMachine_ProcessNode358
L_NMRAnetStateMachine_ProcessNode361:
;NMRAnetStateMachine.mpas,868 :: 		MSI_ALL       : begin
; DatagramBuffer start address is: 16 (W8)
; DatagramAddress start address is: 6 (W3)
	MOV.B	[W14+12], W1
	MOV.B	#254, W0
	CP.B	W1, W0
	BRA Z	L_NMRAnetStateMachine_ProcessNode721
	GOTO	L_NMRAnetStateMachine_ProcessNode377
L_NMRAnetStateMachine_ProcessNode721:
;NMRAnetStateMachine.mpas,869 :: 		ByteArray := PByteArray( ALL_MAP.LowMem);
; ByteArray start address is: 14 (W7)
	MOV	#0, W7
;NMRAnetStateMachine.mpas,870 :: 		for i := 0 to MemorySpaceCount - 1 do
	CLR	W0
	MOV	W0, [W14+8]
; DatagramAddress end address is: 6 (W3)
; DatagramBuffer end address is: 16 (W8)
	MOV.B	W3, W9
L_NMRAnetStateMachine_ProcessNode378:
; DatagramAddress start address is: 18 (W9)
; ByteArray start address is: 14 (W7)
; ByteArray end address is: 14 (W7)
; DatagramAddress start address is: 18 (W9)
; DatagramAddress end address is: 18 (W9)
; DatagramBuffer start address is: 16 (W8)
	ADD	W14, #13, W0
	SE	[W0], W0
	SUB	W0, #1, W6
	ADD	W14, #8, W0
	CP	W6, [W0]
	BRA GE	L_NMRAnetStateMachine_ProcessNode722
	GOTO	L_NMRAnetStateMachine_ProcessNode382
L_NMRAnetStateMachine_ProcessNode722:
; ByteArray end address is: 14 (W7)
; DatagramAddress end address is: 18 (W9)
;NMRAnetStateMachine.mpas,871 :: 		DatagramBuffer^.DataBytes[i+DatagramAddress] := ByteArray^[i+MemorySpaceAddress];
; DatagramAddress start address is: 18 (W9)
; ByteArray start address is: 14 (W7)
	ADD	W8, #11, W2
	ZE	W9, W1
	ADD	W14, #8, W0
	ADD	W1, [W0], W0
	ADD	W2, W0, W5
	MOV	[W14+8], W3
	ASR	W3, #15, W4
	ADD	W14, #16, W2
	ADD	W3, [W2], W0
	ADD	W7, W0, W0
	MOV.B	[W0], [W5]
	ADD	W14, #8, W0
	CP	W6, [W0]
	BRA NZ	L_NMRAnetStateMachine_ProcessNode723
	GOTO	L_NMRAnetStateMachine_ProcessNode382
L_NMRAnetStateMachine_ProcessNode723:
	MOV	[W14+8], W1
	ADD	W14, #8, W0
	ADD	W1, #1, [W0]
; ByteArray end address is: 14 (W7)
; DatagramAddress end address is: 18 (W9)
	GOTO	L_NMRAnetStateMachine_ProcessNode378
L_NMRAnetStateMachine_ProcessNode382:
;NMRAnetStateMachine.mpas,872 :: 		DatagramBuffer^.iByteCount := DatagramBuffer^.iByteCount + MemorySpaceCount;
	ADD	W8, #10, W2
	ZE	[W2], W1
	ADD	W14, #13, W0
	SE	[W0], W0
	ADD	W1, W0, W0
	MOV.B	W0, [W2]
;NMRAnetStateMachine.mpas,873 :: 		end;
	MOV	W8, W0
	GOTO	L_NMRAnetStateMachine_ProcessNode358
L_NMRAnetStateMachine_ProcessNode377:
;NMRAnetStateMachine.mpas,874 :: 		MSI_CONFIG    : begin
; DatagramAddress start address is: 6 (W3)
	MOV.B	[W14+12], W1
	MOV.B	#253, W0
	CP.B	W1, W0
	BRA Z	L_NMRAnetStateMachine_ProcessNode724
	GOTO	L_NMRAnetStateMachine_ProcessNode385
L_NMRAnetStateMachine_ProcessNode724:
;NMRAnetStateMachine.mpas,875 :: 		DatagramBuffer^.iByteCount := DatagramBuffer^.iByteCount + AppCallback_ConfigurationRead(Node, @DatagramBuffer^.DataBytes[DatagramAddress], MemorySpaceAddress, MemorySpaceCount);
	ADD	W8, #10, W0
	MOV	W0, [W14+62]
	MOV	W0, [W14+60]
	ADD	W8, #11, W1
	ZE	W3, W0
; DatagramAddress end address is: 6 (W3)
	ADD	W1, W0, W0
	PUSH	W8
	PUSH.D	W10
	MOV	[W14+16], W12
	MOV	[W14+18], W13
	MOV	W0, W11
	ADD	W14, #13, W0
	SE	[W0], W0
	PUSH	W0
	CALL	_AppCallback_ConfigurationRead
	SUB	#2, W15
	POP.D	W10
	POP	W8
	MOV	[W14+60], W1
	ZE	[W1], W1
	ZE	W0, W0
	ADD	W1, W0, W1
	MOV	[W14+62], W0
	MOV.B	W1, [W0]
;NMRAnetStateMachine.mpas,876 :: 		end;
	MOV	W8, W0
	GOTO	L_NMRAnetStateMachine_ProcessNode358
L_NMRAnetStateMachine_ProcessNode385:
;NMRAnetStateMachine.mpas,877 :: 		MSI_ACDI_MFG  : begin
; DatagramAddress start address is: 6 (W3)
	MOV.B	[W14+12], W1
	MOV.B	#252, W0
	CP.B	W1, W0
	BRA Z	L_NMRAnetStateMachine_ProcessNode725
	GOTO	L_NMRAnetStateMachine_ProcessNode388
L_NMRAnetStateMachine_ProcessNode725:
;NMRAnetStateMachine.mpas,878 :: 		if MemorySpaceAddress = 0 then
	MOV	[W14+16], W0
	MOV	[W14+18], W1
	CP	W0, #0
	CPB	W1, #0
	BRA Z	L_NMRAnetStateMachine_ProcessNode726
	GOTO	L_NMRAnetStateMachine_ProcessNode390
L_NMRAnetStateMachine_ProcessNode726:
;NMRAnetStateMachine.mpas,880 :: 		DatagramBuffer^.DataBytes[DatagramAddress] := ACDI_MFG_VERSION;
	ADD	W8, #11, W1
	ZE	W3, W0
	ADD	W1, W0, W1
	MOV.B	#1, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,881 :: 		Inc(DatagramAddress);
; DatagramAddress start address is: 4 (W2)
	ADD.B	W3, #1, W2
; DatagramAddress end address is: 6 (W3)
;NMRAnetStateMachine.mpas,882 :: 		Dec(MemorySpaceCount);
	MOV.B	#1, W1
	ADD	W14, #13, W0
	SUBR.B	W1, [W0], [W0]
;NMRAnetStateMachine.mpas,883 :: 		Inc(DatagramBuffer^.iByteCount);
	ADD	W8, #10, W1
	ZE	[W1], W0
	INC	W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,884 :: 		end else
	MOV.B	W2, W7
; DatagramAddress end address is: 4 (W2)
	GOTO	L_NMRAnetStateMachine_ProcessNode391
L_NMRAnetStateMachine_ProcessNode390:
;NMRAnetStateMachine.mpas,885 :: 		Dec(MemorySpaceAddress);        // Need to adjust the Address Pointer in subsequent calls to account for the virtual Version Byte
; DatagramAddress start address is: 6 (W3)
	MOV	#1, W1
	MOV	#0, W2
	ADD	W14, #16, W0
	SUBR	W1, [W0], [W0++]
	SUBBR	W2, [W0], [W0--]
	MOV.B	W3, W7
; DatagramAddress end address is: 6 (W3)
L_NMRAnetStateMachine_ProcessNode391:
;NMRAnetStateMachine.mpas,888 :: 		if Node^.State and NS_VIRTUAL <> 0 then
; DatagramAddress start address is: 14 (W7)
	MOV.B	[W10], W0
	ZE	W0, W0
	AND	W0, #8, W0
	CP	W0, #0
	BRA NZ	L_NMRAnetStateMachine_ProcessNode727
	GOTO	L_NMRAnetStateMachine_ProcessNode393
L_NMRAnetStateMachine_ProcessNode727:
;NMRAnetStateMachine.mpas,890 :: 		for i := 0 to MemorySpaceCount - 1 do
	CLR	W0
	MOV	W0, [W14+8]
; DatagramBuffer end address is: 16 (W8)
L_NMRAnetStateMachine_ProcessNode395:
; DatagramAddress start address is: 14 (W7)
; DatagramAddress end address is: 14 (W7)
; DatagramBuffer start address is: 16 (W8)
	ADD	W14, #13, W0
	SE	[W0], W0
	SUB	W0, #1, W6
	ADD	W14, #8, W0
	CP	W6, [W0]
	BRA GE	L_NMRAnetStateMachine_ProcessNode728
	GOTO	L_NMRAnetStateMachine_ProcessNode399
L_NMRAnetStateMachine_ProcessNode728:
; DatagramAddress end address is: 14 (W7)
;NMRAnetStateMachine.mpas,891 :: 		DatagramBuffer^.DataBytes[i+DatagramAddress] := ACDI_MFG_STRINGS_VNODE[i+MemorySpaceAddress]
; DatagramAddress start address is: 14 (W7)
	ADD	W8, #11, W2
	ZE	W7, W1
	ADD	W14, #8, W0
	ADD	W1, [W0], W0
	ADD	W2, W0, W5
	MOV	[W14+8], W3
	ASR	W3, #15, W4
	ADD	W14, #16, W0
	ADD	W3, [W0], W1
	MOV	#lo_addr(_ACDI_MFG_STRINGS_VNODE), W0
	ADD	W0, W1, W1
	MOV	#___Lib_System_DefaultPage, W0
	MOV	WREG, 50
	MOV.B	[W1], W0
	MOV.B	W0, [W5]
	ADD	W14, #8, W0
	CP	W6, [W0]
	BRA NZ	L_NMRAnetStateMachine_ProcessNode729
	GOTO	L_NMRAnetStateMachine_ProcessNode399
L_NMRAnetStateMachine_ProcessNode729:
	MOV	[W14+8], W1
	ADD	W14, #8, W0
	ADD	W1, #1, [W0]
; DatagramAddress end address is: 14 (W7)
	GOTO	L_NMRAnetStateMachine_ProcessNode395
L_NMRAnetStateMachine_ProcessNode399:
;NMRAnetStateMachine.mpas,892 :: 		end else {$ENDIF}
	MOV	W8, W3
	GOTO	L_NMRAnetStateMachine_ProcessNode394
L_NMRAnetStateMachine_ProcessNode393:
;NMRAnetStateMachine.mpas,894 :: 		for i := 0 to MemorySpaceCount - 1 do
; DatagramAddress start address is: 14 (W7)
	CLR	W0
	MOV	W0, [W14+8]
; DatagramBuffer end address is: 16 (W8)
L_NMRAnetStateMachine_ProcessNode400:
; DatagramAddress start address is: 14 (W7)
; DatagramAddress end address is: 14 (W7)
; DatagramBuffer start address is: 16 (W8)
	ADD	W14, #13, W0
	SE	[W0], W0
	SUB	W0, #1, W6
	ADD	W14, #8, W0
	CP	W6, [W0]
	BRA GE	L_NMRAnetStateMachine_ProcessNode730
	GOTO	L_NMRAnetStateMachine_ProcessNode404
L_NMRAnetStateMachine_ProcessNode730:
; DatagramAddress end address is: 14 (W7)
;NMRAnetStateMachine.mpas,895 :: 		DatagramBuffer^.DataBytes[i+DatagramAddress] := ACDI_MFG_STRINGS[i+MemorySpaceAddress]
; DatagramAddress start address is: 14 (W7)
	ADD	W8, #11, W2
	ZE	W7, W1
	ADD	W14, #8, W0
	ADD	W1, [W0], W0
	ADD	W2, W0, W5
	MOV	[W14+8], W3
	ASR	W3, #15, W4
	ADD	W14, #16, W0
	ADD	W3, [W0], W1
	MOV	#lo_addr(_ACDI_MFG_STRINGS), W0
	ADD	W0, W1, W1
	MOV	#___Lib_System_DefaultPage, W0
	MOV	WREG, 50
	MOV.B	[W1], W0
	MOV.B	W0, [W5]
	ADD	W14, #8, W0
	CP	W6, [W0]
	BRA NZ	L_NMRAnetStateMachine_ProcessNode731
	GOTO	L_NMRAnetStateMachine_ProcessNode404
L_NMRAnetStateMachine_ProcessNode731:
	MOV	[W14+8], W1
	ADD	W14, #8, W0
	ADD	W1, #1, [W0]
; DatagramAddress end address is: 14 (W7)
	GOTO	L_NMRAnetStateMachine_ProcessNode400
L_NMRAnetStateMachine_ProcessNode404:
;NMRAnetStateMachine.mpas,896 :: 		end;
	MOV	W8, W3
L_NMRAnetStateMachine_ProcessNode394:
; DatagramBuffer end address is: 16 (W8)
;NMRAnetStateMachine.mpas,897 :: 		DatagramBuffer^.iByteCount := DatagramBuffer^.iByteCount + MemorySpaceCount;
; DatagramBuffer start address is: 6 (W3)
	ADD	W3, #10, W2
	ZE	[W2], W1
	ADD	W14, #13, W0
	SE	[W0], W0
	ADD	W1, W0, W0
	MOV.B	W0, [W2]
;NMRAnetStateMachine.mpas,898 :: 		end;
	MOV	W3, W0
; DatagramBuffer end address is: 6 (W3)
	GOTO	L_NMRAnetStateMachine_ProcessNode358
L_NMRAnetStateMachine_ProcessNode388:
;NMRAnetStateMachine.mpas,899 :: 		MSI_ACDI_USER : begin
; DatagramBuffer start address is: 16 (W8)
; DatagramAddress start address is: 6 (W3)
	MOV.B	[W14+12], W1
	MOV.B	#251, W0
	CP.B	W1, W0
	BRA Z	L_NMRAnetStateMachine_ProcessNode732
	GOTO	L_NMRAnetStateMachine_ProcessNode407
L_NMRAnetStateMachine_ProcessNode732:
;NMRAnetStateMachine.mpas,900 :: 		if MemorySpaceAddress = 0 then
	MOV	[W14+16], W0
	MOV	[W14+18], W1
	CP	W0, #0
	CPB	W1, #0
	BRA Z	L_NMRAnetStateMachine_ProcessNode733
	GOTO	L_NMRAnetStateMachine_ProcessNode409
L_NMRAnetStateMachine_ProcessNode733:
;NMRAnetStateMachine.mpas,902 :: 		DatagramBuffer^.DataBytes[DatagramAddress] := ACDI_USER_VERSION;
	ADD	W8, #11, W1
	ZE	W3, W0
	ADD	W1, W0, W1
	MOV.B	#1, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,903 :: 		Inc(DatagramAddress);
; DatagramAddress start address is: 4 (W2)
	ADD.B	W3, #1, W2
; DatagramAddress end address is: 6 (W3)
;NMRAnetStateMachine.mpas,904 :: 		Dec(MemorySpaceCount);
	MOV.B	#1, W1
	ADD	W14, #13, W0
	SUBR.B	W1, [W0], [W0]
;NMRAnetStateMachine.mpas,905 :: 		Inc(DatagramBuffer^.iByteCount);
	ADD	W8, #10, W1
	ZE	[W1], W0
	INC	W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,906 :: 		end else
; DatagramAddress end address is: 4 (W2)
	GOTO	L_NMRAnetStateMachine_ProcessNode410
L_NMRAnetStateMachine_ProcessNode409:
;NMRAnetStateMachine.mpas,907 :: 		Dec(MemorySpaceAddress);        // Need to adjust the Address Pointer in subsequent calls to account for the virtual Version Byte
; DatagramAddress start address is: 6 (W3)
	MOV	#1, W1
	MOV	#0, W2
	ADD	W14, #16, W0
	SUBR	W1, [W0], [W0++]
	SUBBR	W2, [W0], [W0--]
	MOV.B	W3, W2
; DatagramAddress end address is: 6 (W3)
L_NMRAnetStateMachine_ProcessNode410:
;NMRAnetStateMachine.mpas,909 :: 		DatagramBuffer^.iByteCount := DatagramBuffer^.iByteCount + AppCallback_ConfigurationRead(Node, @DatagramBuffer^.DataBytes[DatagramAddress], MemorySpaceAddress, MemorySpaceCount);
; DatagramAddress start address is: 4 (W2)
	ADD	W8, #10, W0
	MOV	W0, [W14+62]
	MOV	W0, [W14+60]
	ADD	W8, #11, W1
	ZE	W2, W0
; DatagramAddress end address is: 4 (W2)
	ADD	W1, W0, W0
	PUSH	W8
	PUSH.D	W10
	MOV	[W14+16], W12
	MOV	[W14+18], W13
	MOV	W0, W11
	ADD	W14, #13, W0
	SE	[W0], W0
	PUSH	W0
	CALL	_AppCallback_ConfigurationRead
	SUB	#2, W15
	POP.D	W10
	POP	W8
	MOV	[W14+60], W1
	ZE	[W1], W1
	ZE	W0, W0
	ADD	W1, W0, W1
	MOV	[W14+62], W0
	MOV.B	W1, [W0]
;NMRAnetStateMachine.mpas,910 :: 		end;
	MOV	W8, W0
	GOTO	L_NMRAnetStateMachine_ProcessNode358
L_NMRAnetStateMachine_ProcessNode407:
	MOV	W8, W0
L_NMRAnetStateMachine_ProcessNode358:
; DatagramBuffer end address is: 16 (W8)
;NMRAnetStateMachine.mpas,912 :: 		DatagramBuffer^.Statemachine := STATE_MEM_CONFIG_SEND_RESUSED_DATAGRAM;
; DatagramBuffer start address is: 0 (W0)
	ADD	W0, #7, W1
; DatagramBuffer end address is: 0 (W0)
	MOV.B	#5, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,913 :: 		end;
	GOTO	L_NMRAnetStateMachine_ProcessNode303
L_NMRAnetStateMachine_ProcessNode354:
;NMRAnetStateMachine.mpas,914 :: 		STATE_MEM_CONFIG_WRITE_DATAGRAM :        begin   {$IFDEF TRACE_MEM_CONFIG_STATEMACHINE} UART1_Write_Text('STATE_MEM_CONFIG_WRITE_DATAGRAM'+LF);    {$ENDIF}
; DatagramBuffer start address is: 16 (W8)
	MOV.B	[W14+64], W0
	CP.B	W0, #3
	BRA Z	L_NMRAnetStateMachine_ProcessNode734
	GOTO	L_NMRAnetStateMachine_ProcessNode413
L_NMRAnetStateMachine_ProcessNode734:
;NMRAnetStateMachine.mpas,916 :: 		DatagramAddress := 6;
; DatagramAddress start address is: 18 (W9)
	MOV.B	#6, W9
;NMRAnetStateMachine.mpas,917 :: 		case DatagramBuffer^.DataBytes[1] and $03 of      // Strip off bottom two bits
	ADD	W8, #11, W0
	INC	W0
	ZE	[W0], W0
	AND	W0, #3, W1
;NMRAnetStateMachine.mpas,918 :: 		MCP_CDI            : MemorySpace := MSI_CDI;
	CP	W1, #3
	BRA Z	L_NMRAnetStateMachine_ProcessNode735
	GOTO	L_NMRAnetStateMachine_ProcessNode417
L_NMRAnetStateMachine_ProcessNode735:
	MOV.B	#255, W0
	MOV.B	W0, [W14+12]
	GOTO	L_NMRAnetStateMachine_ProcessNode414
L_NMRAnetStateMachine_ProcessNode417:
;NMRAnetStateMachine.mpas,919 :: 		MCP_ALL            : MemorySpace := MSI_ALL;
	CP	W1, #2
	BRA Z	L_NMRAnetStateMachine_ProcessNode736
	GOTO	L_NMRAnetStateMachine_ProcessNode420
L_NMRAnetStateMachine_ProcessNode736:
	MOV.B	#254, W0
	MOV.B	W0, [W14+12]
	GOTO	L_NMRAnetStateMachine_ProcessNode414
L_NMRAnetStateMachine_ProcessNode420:
;NMRAnetStateMachine.mpas,920 :: 		MCP_CONFIGURATION  : MemorySpace := MSI_CONFIG;
	CP	W1, #1
	BRA Z	L_NMRAnetStateMachine_ProcessNode737
	GOTO	L_NMRAnetStateMachine_ProcessNode423
L_NMRAnetStateMachine_ProcessNode737:
	MOV.B	#253, W0
	MOV.B	W0, [W14+12]
	GOTO	L_NMRAnetStateMachine_ProcessNode414
L_NMRAnetStateMachine_ProcessNode423:
;NMRAnetStateMachine.mpas,921 :: 		MCP_NONE           : begin
	CP	W1, #0
	BRA Z	L_NMRAnetStateMachine_ProcessNode738
	GOTO	L_NMRAnetStateMachine_ProcessNode426
L_NMRAnetStateMachine_ProcessNode738:
;NMRAnetStateMachine.mpas,922 :: 		DatagramAddress := 7;
	MOV.B	#7, W9
;NMRAnetStateMachine.mpas,923 :: 		MemorySpace := DatagramBuffer^.DataBytes[6];
	ADD	W8, #11, W0
	ADD	W0, #6, W0
	MOV.B	[W0], W0
	MOV.B	W0, [W14+12]
;NMRAnetStateMachine.mpas,924 :: 		end;
	GOTO	L_NMRAnetStateMachine_ProcessNode414
L_NMRAnetStateMachine_ProcessNode426:
L_NMRAnetStateMachine_ProcessNode414:
; DatagramAddress end address is: 18 (W9)
;NMRAnetStateMachine.mpas,926 :: 		MemorySpaceAddress := DWord( DatagramBuffer^.DataBytes[2] shl 24) or DWord( DatagramBuffer^.DataBytes[3] shl 16) or DWord( DatagramBuffer^.DataBytes[4] shl 8) or DWord( DatagramBuffer^.DataBytes[5]);
; DatagramAddress start address is: 18 (W9)
	ADD	W8, #11, W7
	ADD	W7, #2, W0
	ZE	[W0], W4
	CLR	W5
	MOV	#24, W0
	MOV.D	W4, W2
L_NMRAnetStateMachine_ProcessNode739:
	DEC	W0, W0
	BRA LT	L_NMRAnetStateMachine_ProcessNode740
	SL	W2, W2
	RLC	W3, W3
	BRA	L_NMRAnetStateMachine_ProcessNode739
L_NMRAnetStateMachine_ProcessNode740:
	ADD	W7, #3, W0
	ZE	[W0], W0
	CLR	W1
	MOV	W0, W1
	CLR	W0
	IOR	W2, W0, W5
	IOR	W3, W1, W6
	ADD	W7, #4, W0
	ZE	[W0], W3
	CLR	W4
	MOV	#8, W2
	MOV	W3, W0
	MOV	W4, W1
L_NMRAnetStateMachine_ProcessNode741:
	DEC	W2, W2
	BRA LT	L_NMRAnetStateMachine_ProcessNode742
	SL	W0, W0
	RLC	W1, W1
	BRA	L_NMRAnetStateMachine_ProcessNode741
L_NMRAnetStateMachine_ProcessNode742:
	IOR	W5, W0, W3
	IOR	W6, W1, W4
	ADD	W7, #5, W0
	ZE	[W0], W1
	CLR	W2
	ADD	W14, #16, W0
	IOR	W3, W1, [W0++]
	IOR	W4, W2, [W0--]
;NMRAnetStateMachine.mpas,927 :: 		MemorySpaceCount := DatagramBuffer^.iByteCount - DatagramAddress;
	ADD	W8, #10, W1
	ADD	W14, #13, W0
	SUBR.B	W9, [W1], [W0]
;NMRAnetStateMachine.mpas,929 :: 		MSI_CONFIG    : begin
	MOV.B	[W14+12], W1
	MOV.B	#253, W0
	CP.B	W1, W0
	BRA Z	L_NMRAnetStateMachine_ProcessNode743
	GOTO	L_NMRAnetStateMachine_ProcessNode430
L_NMRAnetStateMachine_ProcessNode743:
;NMRAnetStateMachine.mpas,930 :: 		AppCallback_ConfigurationWrite(Node, @DatagramBuffer^.DataBytes[DatagramAddress], MemorySpaceAddress, MemorySpaceCount);
	ADD	W8, #11, W1
	ZE	W9, W0
; DatagramAddress end address is: 18 (W9)
	ADD	W1, W0, W0
	PUSH	W8
	PUSH.D	W10
	MOV	[W14+16], W12
	MOV	[W14+18], W13
	MOV	W0, W11
	ADD	W14, #13, W0
	SE	[W0], W0
	PUSH	W0
	CALL	_AppCallback_ConfigurationWrite
	SUB	#2, W15
	POP.D	W10
	POP	W8
;NMRAnetStateMachine.mpas,931 :: 		end;
	GOTO	L_NMRAnetStateMachine_ProcessNode427
L_NMRAnetStateMachine_ProcessNode430:
;NMRAnetStateMachine.mpas,932 :: 		MSI_ACDI_USER : begin
	MOV.B	[W14+12], W1
	MOV.B	#251, W0
	CP.B	W1, W0
	BRA Z	L_NMRAnetStateMachine_ProcessNode744
	GOTO	L_NMRAnetStateMachine_ProcessNode433
L_NMRAnetStateMachine_ProcessNode744:
;NMRAnetStateMachine.mpas,933 :: 		UART1_Write_Text('Unimplemented Configuration Mem write'+LF)
	ADD	W14, #20, W1
	MOV.B	#85, W0
	MOV.B	W0, [W1++]
	MOV.B	#110, W0
	MOV.B	W0, [W1++]
	MOV.B	#105, W0
	MOV.B	W0, [W1++]
	MOV.B	#109, W0
	MOV.B	W0, [W1++]
	MOV.B	#112, W0
	MOV.B	W0, [W1++]
	MOV.B	#108, W0
	MOV.B	W0, [W1++]
	MOV.B	#101, W0
	MOV.B	W0, [W1++]
	MOV.B	#109, W0
	MOV.B	W0, [W1++]
	MOV.B	#101, W0
	MOV.B	W0, [W1++]
	MOV.B	#110, W0
	MOV.B	W0, [W1++]
	MOV.B	#116, W0
	MOV.B	W0, [W1++]
	MOV.B	#101, W0
	MOV.B	W0, [W1++]
	MOV.B	#100, W0
	MOV.B	W0, [W1++]
	MOV.B	#32, W0
	MOV.B	W0, [W1++]
	MOV.B	#67, W0
	MOV.B	W0, [W1++]
	MOV.B	#111, W0
	MOV.B	W0, [W1++]
	MOV.B	#110, W0
	MOV.B	W0, [W1++]
	MOV.B	#102, W0
	MOV.B	W0, [W1++]
	MOV.B	#105, W0
	MOV.B	W0, [W1++]
	MOV.B	#103, W0
	MOV.B	W0, [W1++]
	MOV.B	#117, W0
	MOV.B	W0, [W1++]
	MOV.B	#114, W0
	MOV.B	W0, [W1++]
	MOV.B	#97, W0
	MOV.B	W0, [W1++]
	MOV.B	#116, W0
	MOV.B	W0, [W1++]
	MOV.B	#105, W0
	MOV.B	W0, [W1++]
	MOV.B	#111, W0
	MOV.B	W0, [W1++]
	MOV.B	#110, W0
	MOV.B	W0, [W1++]
	MOV.B	#32, W0
	MOV.B	W0, [W1++]
	MOV.B	#77, W0
	MOV.B	W0, [W1++]
	MOV.B	#101, W0
	MOV.B	W0, [W1++]
	MOV.B	#109, W0
	MOV.B	W0, [W1++]
	MOV.B	#32, W0
	MOV.B	W0, [W1++]
	MOV.B	#119, W0
	MOV.B	W0, [W1++]
	MOV.B	#114, W0
	MOV.B	W0, [W1++]
	MOV.B	#105, W0
	MOV.B	W0, [W1++]
	MOV.B	#116, W0
	MOV.B	W0, [W1++]
	MOV.B	#101, W0
	MOV.B	W0, [W1++]
	MOV.B	#13, W0
	MOV.B	W0, [W1++]
	MOV.B	#10, W0
	MOV.B	W0, [W1++]
	MOV.B	#0, W0
	MOV.B	W0, [W1++]
	ADD	W14, #20, W0
	PUSH	W10
	MOV	W0, W10
	CALL	_UART1_Write_Text
	POP	W10
;NMRAnetStateMachine.mpas,934 :: 		end;
	GOTO	L_NMRAnetStateMachine_ProcessNode427
L_NMRAnetStateMachine_ProcessNode433:
L_NMRAnetStateMachine_ProcessNode427:
;NMRAnetStateMachine.mpas,936 :: 		DatagramBuffer^.Statemachine := STATE_MEM_CONFIG_REPLY_TO_WRITE_DATAGRAM;
	ADD	W8, #7, W1
; DatagramBuffer end address is: 16 (W8)
	MOV.B	#4, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,937 :: 		end;
	GOTO	L_NMRAnetStateMachine_ProcessNode303
L_NMRAnetStateMachine_ProcessNode413:
;NMRAnetStateMachine.mpas,938 :: 		STATE_MEM_CONFIG_REPLY_TO_WRITE_DATAGRAM : begin  {$IFDEF TRACE_MEM_CONFIG_STATEMACHINE} UART1_Write_Text('STATE_MEM_CONFIG_REPLY_TO_WRITE_DATAGRAM'+LF);    {$ENDIF}
; DatagramBuffer start address is: 16 (W8)
	MOV.B	[W14+64], W0
	CP.B	W0, #4
	BRA Z	L_NMRAnetStateMachine_ProcessNode745
	GOTO	L_NMRAnetStateMachine_ProcessNode436
L_NMRAnetStateMachine_ProcessNode745:
;NMRAnetStateMachine.mpas,939 :: 		if NMRABusBufferAvailable then
	CALL	_NMRABusBufferAvailable
	CP0	W0
	BRA NZ	L_NMRAnetStateMachine_ProcessNode746
	GOTO	L_NMRAnetStateMachine_ProcessNode438
L_NMRAnetStateMachine_ProcessNode746:
;NMRAnetStateMachine.mpas,941 :: 		TransmitNMRABusLayerMsg(Node, CANBuffer, MTI_DATAGRAM_OK_REPLY, DatagramBuffer^.Alias, 0, @DataBytes, False);
	ADD	W8, #2, W2
	ADD	W14, #0, W1
	PUSH	W8
	PUSH	W11
	MOV	#32768, W12
	MOV	#2466, W13
	CLR	W0
	PUSH	W0
	PUSH	W1
	CLR	W0
	PUSH	W0
	PUSH	[W2]
	CALL	_TransmitNMRABusLayerMsg
	SUB	#8, W15
	POP	W11
	POP	W8
;NMRAnetStateMachine.mpas,942 :: 		NMRAnetUtilities_DatagramBufferUnLink(Node, DatagramBuffer);
	PUSH	W11
	MOV	W8, W11
	CALL	_NMRAnetUtilities_DatagramBufferUnLink
	POP	W11
;NMRAnetStateMachine.mpas,943 :: 		NMRAnetBufferPools_ReleaseDatagramBuffer(DatagramBuffer)
	PUSH	W10
; DatagramBuffer end address is: 16 (W8)
	MOV	W8, W10
	CALL	_NMRAnetBufferPools_ReleaseDatagramBuffer
	POP	W10
;NMRAnetStateMachine.mpas,944 :: 		end
L_NMRAnetStateMachine_ProcessNode438:
;NMRAnetStateMachine.mpas,945 :: 		end;
	GOTO	L_NMRAnetStateMachine_ProcessNode303
L_NMRAnetStateMachine_ProcessNode436:
;NMRAnetStateMachine.mpas,946 :: 		STATE_MEM_CONFIG_SEND_RESUSED_DATAGRAM : begin  {$IFDEF TRACE_MEM_CONFIG_STATEMACHINE} UART1_Write_Text('STATE_MEM_CONFIG_SEND_RESUSED_DATAGRAM'+LF);    {$ENDIF}
; DatagramBuffer start address is: 16 (W8)
	MOV.B	[W14+64], W2
	CP.B	W2, #5
	BRA Z	L_NMRAnetStateMachine_ProcessNode747
	GOTO	L_NMRAnetStateMachine_ProcessNode442
L_NMRAnetStateMachine_ProcessNode747:
;NMRAnetStateMachine.mpas,948 :: 		DatagramBuffer^.State := (DatagramBuffer^.State or CBS_OUTGOING or CBS_PROCESSING) and not CBS_TRANSFER_COMPLETE;  // Turn it into an outgoing Datagram
	MOV.B	[W8], W0
	ZE	W0, W0
	IOR	W0, #4, W0
	IOR	W0, #2, W1
	MOV	#247, W0
	AND	W1, W0, W0
	MOV.B	W0, [W8]
;NMRAnetStateMachine.mpas,949 :: 		DatagramBuffer^.Tag := 0;                                                          // Reset for the Transmit side
	ADD	W8, #8, W1
	CLR	W0
	MOV	W0, [W1]
;NMRAnetStateMachine.mpas,950 :: 		DatagramBuffer^.iWatchdog := 0;
	MOV	#88, W0
	ADD	W8, W0, W1
	CLR	W0
	MOV	W0, [W1]
;NMRAnetStateMachine.mpas,951 :: 		DatagramBuffer^.SourceNodePtr := Generic16BitPointer( Node);                       // So the Transmit Engine knows what source Node and Alias to associate the Datagram to
	MOV	#86, W0
	ADD	W8, W0, W0
	MOV	W10, [W0]
;NMRAnetStateMachine.mpas,952 :: 		DatagramBuffer^.Statemachine := STATE_MEM_CONFIG_WAITFOR_REPLY;
	ADD	W8, #7, W1
; DatagramBuffer end address is: 16 (W8)
	MOV.B	#6, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,953 :: 		end;
	GOTO	L_NMRAnetStateMachine_ProcessNode303
L_NMRAnetStateMachine_ProcessNode442:
;NMRAnetStateMachine.mpas,954 :: 		STATE_MEM_CONFIG_WAITFOR_REPLY :         begin   {$IFDEF TRACE_MEM_CONFIG_STATEMACHINE} UART1_Write_Text('STATE_MEM_CONFIG_WAITFOR_REPLY'+LF);   {$ENDIF}
	CP.B	W2, #6
	BRA Z	L_NMRAnetStateMachine_ProcessNode748
	GOTO	L_NMRAnetStateMachine_ProcessNode445
L_NMRAnetStateMachine_ProcessNode748:
;NMRAnetStateMachine.mpas,963 :: 		end;
	GOTO	L_NMRAnetStateMachine_ProcessNode303
L_NMRAnetStateMachine_ProcessNode445:
L_NMRAnetStateMachine_ProcessNode303:
;NMRAnetStateMachine.mpas,965 :: 		end;
	GOTO	L_NMRAnetStateMachine_ProcessNode299
L_NMRAnetStateMachine_ProcessNode302:
;NMRAnetStateMachine.mpas,966 :: 		BMC_DATAGRAM_TRACTION_CONTROL      : begin
; DatagramBuffer start address is: 16 (W8)
	MOV.B	[W14+66], W0
	CP.B	W0, #9
	BRA Z	L_NMRAnetStateMachine_ProcessNode749
	GOTO	L_NMRAnetStateMachine_ProcessNode448
L_NMRAnetStateMachine_ProcessNode749:
;NMRAnetStateMachine.mpas,967 :: 		if DatagramTrainControlCallbackFunc <> PDatagramTaskCallbackFunc( nil) then
	MOV	_DatagramTrainControlCallbackFunc, W1
	MOV	#0, W0
	CP	W1, W0
	BRA NZ	L_NMRAnetStateMachine_ProcessNode750
	GOTO	L_NMRAnetStateMachine_ProcessNode450
L_NMRAnetStateMachine_ProcessNode750:
;NMRAnetStateMachine.mpas,968 :: 		DatagramTrainControlCallbackFunc(Node, DatagramBuffer);
	PUSH	W8
	PUSH.D	W10
	MOV	W8, W11
	MOV	_DatagramTrainControlCallbackFunc, W0
	CALL	W0
	POP.D	W10
	POP	W8
L_NMRAnetStateMachine_ProcessNode450:
;NMRAnetStateMachine.mpas,969 :: 		TransmitNMRABusLayerMsg(Node, CANBuffer, MTI_DATAGRAM_OK_REPLY, DatagramBuffer^.Alias, 0, @DataBytes, False);
	ADD	W8, #2, W2
	ADD	W14, #0, W1
	PUSH	W8
	PUSH	W11
	MOV	#32768, W12
	MOV	#2466, W13
	CLR	W0
	PUSH	W0
	PUSH	W1
	CLR	W0
	PUSH	W0
	PUSH	[W2]
	CALL	_TransmitNMRABusLayerMsg
	SUB	#8, W15
	POP	W11
	POP	W8
;NMRAnetStateMachine.mpas,970 :: 		NMRAnetUtilities_DatagramBufferUnLink(Node, DatagramBuffer);
	PUSH	W11
	MOV	W8, W11
	CALL	_NMRAnetUtilities_DatagramBufferUnLink
	POP	W11
;NMRAnetStateMachine.mpas,971 :: 		NMRAnetBufferPools_ReleaseDatagramBuffer(DatagramBuffer)
	PUSH	W10
; DatagramBuffer end address is: 16 (W8)
	MOV	W8, W10
	CALL	_NMRAnetBufferPools_ReleaseDatagramBuffer
	POP	W10
;NMRAnetStateMachine.mpas,972 :: 		end;
	GOTO	L_NMRAnetStateMachine_ProcessNode299
L_NMRAnetStateMachine_ProcessNode448:
L_NMRAnetStateMachine_ProcessNode299:
;NMRAnetStateMachine.mpas,974 :: 		end;
L_NMRAnetStateMachine_ProcessNode297:
;NMRAnetStateMachine.mpas,976 :: 		end;
L_end_NMRAnetStateMachine_ProcessNode:
L_end_ProcessNode:
	POP	W13
	POP	W12
	POP	W11
	ULNK
	RETURN
; end of NMRAnetStateMachine_ProcessNode

NMRAnetStateMachine_ProcessOutgoingNode:
	LNK	#2

;NMRAnetStateMachine.mpas,992 :: 		begin
;NMRAnetStateMachine.mpas,994 :: 		DatagramBuffer := nil;
	PUSH	W10
	PUSH	W12
	CLR	W0
	MOV	W0, [W14+0]
;NMRAnetStateMachine.mpas,995 :: 		if NMRAnetUtilities_FindOutgoingDatagram(Node, DatagramBuffer, True) then
	PUSH	W11
	MOV	#65535, W12
	ADD	W14, #0, W0
	MOV	W0, W11
	CALL	_NMRAnetUtilities_FindOutgoingDatagram
	POP	W11
	CP0	W0
	BRA NZ	L_NMRAnetStateMachine_ProcessOutgoingNode752
	GOTO	L_NMRAnetStateMachine_ProcessOutgoingNode454
L_NMRAnetStateMachine_ProcessOutgoingNode752:
;NMRAnetStateMachine.mpas,1000 :: 		if NMRAnetStateMachine_TrySendDatagram(PNMRAnetNode( DatagramBuffer^.SourceNodePtr), CANBuffer, DatagramBuffer) then
	MOV	#86, W1
	ADD	W14, #0, W0
	ADD	W1, [W0], W0
	MOV	[W14+0], W12
	MOV	[W0], W10
	CALL	_NMRAnetStateMachine_TrySendDatagram
	CP0	W0
	BRA NZ	L_NMRAnetStateMachine_ProcessOutgoingNode753
	GOTO	L_NMRAnetStateMachine_ProcessOutgoingNode457
L_NMRAnetStateMachine_ProcessOutgoingNode753:
;NMRAnetStateMachine.mpas,1003 :: 		DatagramBuffer^.State := (DatagramBuffer^.State and not CBS_PROCESSING) or CBS_TRANSFER_COMPLETE;    // Flag as complete so the caller and unlink and free
	MOV	[W14+0], W0
	MOV.B	[W0], W0
	ZE	W0, W1
	MOV	#253, W0
	AND	W1, W0, W0
	IOR	W0, #8, W1
	MOV	[W14+0], W0
	MOV.B	W1, [W0]
;NMRAnetStateMachine.mpas,1004 :: 		end;
L_NMRAnetStateMachine_ProcessOutgoingNode457:
;NMRAnetStateMachine.mpas,1005 :: 		end;
L_NMRAnetStateMachine_ProcessOutgoingNode454:
;NMRAnetStateMachine.mpas,1006 :: 		end;
L_end_ProcessOutgoingNode:
	POP	W12
	POP	W10
	ULNK
	RETURN
; end of NMRAnetStateMachine_ProcessOutgoingNode

NMRAnetStateMachine_ProcessAbandonBuffers:

;NMRAnetStateMachine.mpas,1011 :: 		begin
;NMRAnetStateMachine.mpas,1012 :: 		DatagramBuffer := Node^.DatagramBuffers;
	PUSH	W10
	PUSH	W11
	ADD	W10, #28, W0
	MOV	[W0], W0
; DatagramBuffer start address is: 8 (W4)
	MOV	W0, W4
;NMRAnetStateMachine.mpas,1013 :: 		if DatagramBuffer <> nil then
	CP	W0, #0
	BRA NZ	L_NMRAnetStateMachine_ProcessAbandonBuffers755
	GOTO	L_NMRAnetStateMachine_ProcessAbandonBuffers461
L_NMRAnetStateMachine_ProcessAbandonBuffers755:
;NMRAnetStateMachine.mpas,1015 :: 		if DatagramBuffer^.iWatchdog >= DATAGRAM_WATCHDOG_MAX then
	MOV	#88, W0
	ADD	W4, W0, W0
	MOV	[W0], W0
	CP	W0, #30
	BRA GEU	L_NMRAnetStateMachine_ProcessAbandonBuffers756
	GOTO	L_NMRAnetStateMachine_ProcessAbandonBuffers464
L_NMRAnetStateMachine_ProcessAbandonBuffers756:
;NMRAnetStateMachine.mpas,1017 :: 		NMRAnetUtilities_DatagramBufferUnLink(Node, DatagramBuffer);
	MOV	W4, W11
	CALL	_NMRAnetUtilities_DatagramBufferUnLink
;NMRAnetStateMachine.mpas,1018 :: 		NMRAnetBufferPools_ReleaseDatagramBuffer(DatagramBuffer);
	MOV	W4, W10
; DatagramBuffer end address is: 8 (W4)
	CALL	_NMRAnetBufferPools_ReleaseDatagramBuffer
;NMRAnetStateMachine.mpas,1019 :: 		end
L_NMRAnetStateMachine_ProcessAbandonBuffers464:
;NMRAnetStateMachine.mpas,1020 :: 		end;
L_NMRAnetStateMachine_ProcessAbandonBuffers461:
;NMRAnetStateMachine.mpas,1021 :: 		end;
L_end_ProcessAbandonBuffers:
	POP	W11
	POP	W10
	RETURN
; end of NMRAnetStateMachine_ProcessAbandonBuffers

_NMRAnetStateMachine_Process:
	LNK	#22

;NMRAnetStateMachine.mpas,1035 :: 		begin
;NMRAnetStateMachine.mpas,1039 :: 		LockCANInterrupt;
	PUSH	W11
	PUSH	W12
	PUSH	W13
	CALL	_LockCANInterrupt
;NMRAnetStateMachine.mpas,1040 :: 		case Node^.iStateMachine of
	ADD	W10, #25, W0
	MOV.B	[W0], W0
	MOV.B	W0, [W14+20]
;NMRAnetStateMachine.mpas,1041 :: 		STATE_NMRABUS_START :
	CP.B	W0, #0
	BRA Z	L__NMRAnetStateMachine_Process758
	GOTO	L__NMRAnetStateMachine_Process470
L__NMRAnetStateMachine_Process758:
;NMRAnetStateMachine.mpas,1043 :: 		CANStorage_FlushBuffers(Node^.Info.AliasID);
	ADD	W10, #2, W0
	ADD	W0, #16, W0
	PUSH	W10
	MOV	[W0], W10
	CALL	_CANStorage_FlushBuffers
	POP	W10
;NMRAnetStateMachine.mpas,1044 :: 		while CAN_Engine.State and CES_TRANSMITTING <> 0 do;                             // Wait for the last transmited CANBuffer
L__NMRAnetStateMachine_Process472:
	MOV	#lo_addr(_CAN_Engine), W0
	ZE	[W0], W0
	AND	W0, #1, W0
	CP	W0, #0
	BRA Z	L__NMRAnetStateMachine_Process759
	GOTO	L__NMRAnetStateMachine_Process472
L__NMRAnetStateMachine_Process759:
;NMRAnetStateMachine.mpas,1045 :: 		Node^.iStateMachine := STATE_NMRABUS_GENERATE_NODE_ALIAS;
	ADD	W10, #25, W1
	MOV.B	#1, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,1046 :: 		end;
	GOTO	L__NMRAnetStateMachine_Process467
L__NMRAnetStateMachine_Process470:
;NMRAnetStateMachine.mpas,1047 :: 		STATE_NMRABUS_GENERATE_NODE_ALIAS :
	MOV.B	[W14+20], W0
	CP.B	W0, #1
	BRA Z	L__NMRAnetStateMachine_Process760
	GOTO	L__NMRAnetStateMachine_Process478
L__NMRAnetStateMachine_Process760:
;NMRAnetStateMachine.mpas,1049 :: 		Node^.Info.AliasID := NMRAnetUtilities_CreateAliasID(Node^.Info.Seed, False);
	ADD	W10, #2, W1
	ADD	W1, #16, W0
	MOV	W0, [W14+18]
	ADD	W1, #8, W0
	PUSH	W10
	CLR	W11
	MOV	W0, W10
	CALL	_NMRAnetUtilities_CreateAliasID
	MOV	[W14+18], W1
	MOV	W0, [W1]
;NMRAnetStateMachine.mpas,1050 :: 		NMRAnetNode_SortNodeList(Nodes);
	MOV	#lo_addr(_Nodes), W10
	CALL	_NMRAnetNode_SortNodeList
	POP	W10
;NMRAnetStateMachine.mpas,1051 :: 		Node^.Login.iCID := 0;
	ADD	W10, #20, W0
	ADD	W0, #1, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,1052 :: 		Node^.iStateMachine := STATE_NMRABUS_TRANSMIT_CID;
	ADD	W10, #25, W1
	MOV.B	#3, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,1053 :: 		end;
	GOTO	L__NMRAnetStateMachine_Process467
L__NMRAnetStateMachine_Process478:
;NMRAnetStateMachine.mpas,1054 :: 		STATE_RANDOM_NUMBER_GENERATOR :
	MOV.B	[W14+20], W0
	CP.B	W0, #2
	BRA Z	L__NMRAnetStateMachine_Process761
	GOTO	L__NMRAnetStateMachine_Process481
L__NMRAnetStateMachine_Process761:
;NMRAnetStateMachine.mpas,1056 :: 		NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed(Node^.Info.Seed);
	ADD	W10, #2, W0
	ADD	W0, #8, W0
	PUSH	W10
	MOV	W0, W10
	CALL	_NMRAnetUtilities_PsudoRandomNumberGeneratorOnSeed
	POP	W10
;NMRAnetStateMachine.mpas,1057 :: 		Node^.iStateMachine := STATE_NMRABUS_GENERATE_NODE_ALIAS;
	ADD	W10, #25, W1
	MOV.B	#1, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,1058 :: 		end;
	GOTO	L__NMRAnetStateMachine_Process467
L__NMRAnetStateMachine_Process481:
;NMRAnetStateMachine.mpas,1059 :: 		STATE_NMRABUS_TRANSMIT_CID :
	MOV.B	[W14+20], W0
	CP.B	W0, #3
	BRA Z	L__NMRAnetStateMachine_Process762
	GOTO	L__NMRAnetStateMachine_Process484
L__NMRAnetStateMachine_Process762:
;NMRAnetStateMachine.mpas,1061 :: 		case Node^.Login.iCID of
	ADD	W10, #20, W0
	ADD	W0, #1, W2
;NMRAnetStateMachine.mpas,1062 :: 		0 : VariableField := MTI_CID0;                                         // Queue up
	MOV.B	[W2], W0
	CP.B	W0, #0
	BRA Z	L__NMRAnetStateMachine_Process763
	GOTO	L__NMRAnetStateMachine_Process488
L__NMRAnetStateMachine_Process763:
	MOV	#0, W0
	MOV	#1792, W1
	MOV	W0, [W14+14]
	MOV	W1, [W14+16]
	GOTO	L__NMRAnetStateMachine_Process485
L__NMRAnetStateMachine_Process488:
;NMRAnetStateMachine.mpas,1063 :: 		1 : VariableField := MTI_CID1;
	MOV.B	[W2], W0
	CP.B	W0, #1
	BRA Z	L__NMRAnetStateMachine_Process764
	GOTO	L__NMRAnetStateMachine_Process491
L__NMRAnetStateMachine_Process764:
	MOV	#0, W0
	MOV	#1536, W1
	MOV	W0, [W14+14]
	MOV	W1, [W14+16]
	GOTO	L__NMRAnetStateMachine_Process485
L__NMRAnetStateMachine_Process491:
;NMRAnetStateMachine.mpas,1064 :: 		2 : VariableField := MTI_CID2;
	MOV.B	[W2], W0
	CP.B	W0, #2
	BRA Z	L__NMRAnetStateMachine_Process765
	GOTO	L__NMRAnetStateMachine_Process494
L__NMRAnetStateMachine_Process765:
	MOV	#0, W0
	MOV	#1280, W1
	MOV	W0, [W14+14]
	MOV	W1, [W14+16]
	GOTO	L__NMRAnetStateMachine_Process485
L__NMRAnetStateMachine_Process494:
;NMRAnetStateMachine.mpas,1065 :: 		3 : VariableField := MTI_CID3;
	MOV.B	[W2], W0
	CP.B	W0, #3
	BRA Z	L__NMRAnetStateMachine_Process766
	GOTO	L__NMRAnetStateMachine_Process497
L__NMRAnetStateMachine_Process766:
	MOV	#0, W0
	MOV	#1024, W1
	MOV	W0, [W14+14]
	MOV	W1, [W14+16]
	GOTO	L__NMRAnetStateMachine_Process485
L__NMRAnetStateMachine_Process497:
L__NMRAnetStateMachine_Process485:
;NMRAnetStateMachine.mpas,1067 :: 		if CANBusBufferAvailable then
	CALL	NMRAnetStateMachine_CANBusBufferAvailable
	CP0	W0
	BRA NZ	L__NMRAnetStateMachine_Process767
	GOTO	L__NMRAnetStateMachine_Process499
L__NMRAnetStateMachine_Process767:
;NMRAnetStateMachine.mpas,1069 :: 		TransmitCANLayerMsg(Node, @CANBuffer, VariableField);
	ADD	W14, #0, W0
	MOV	[W14+14], W12
	MOV	[W14+16], W13
	MOV	W0, W11
	CALL	_TransmitCANLayerMsg
;NMRAnetStateMachine.mpas,1070 :: 		Node^.iStateMachine := STATE_NMRABUS_NEXT_CDI;
	ADD	W10, #25, W1
	MOV.B	#4, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,1071 :: 		end
L__NMRAnetStateMachine_Process499:
;NMRAnetStateMachine.mpas,1072 :: 		end;
	GOTO	L__NMRAnetStateMachine_Process467
L__NMRAnetStateMachine_Process484:
;NMRAnetStateMachine.mpas,1073 :: 		STATE_NMRABUS_NEXT_CDI :
	MOV.B	[W14+20], W2
	CP.B	W2, #4
	BRA Z	L__NMRAnetStateMachine_Process768
	GOTO	L__NMRAnetStateMachine_Process503
L__NMRAnetStateMachine_Process768:
;NMRAnetStateMachine.mpas,1075 :: 		if Node^.Login.iCID < 3 then
	ADD	W10, #20, W0
	INC	W0
	MOV.B	[W0], W0
	CP.B	W0, #3
	BRA LTU	L__NMRAnetStateMachine_Process769
	GOTO	L__NMRAnetStateMachine_Process505
L__NMRAnetStateMachine_Process769:
;NMRAnetStateMachine.mpas,1077 :: 		Inc(Node^.Login.iCID);
	ADD	W10, #20, W0
	ADD	W0, #1, W1
	ZE	[W1], W0
	INC	W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,1078 :: 		Node^.iStateMachine := STATE_NMRABUS_TRANSMIT_CID
	ADD	W10, #25, W1
	MOV.B	#3, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,1079 :: 		end else
	GOTO	L__NMRAnetStateMachine_Process506
L__NMRAnetStateMachine_Process505:
;NMRAnetStateMachine.mpas,1081 :: 		if CAN_Engine.State and CES_TRANSMITTING = 0 then                     // Wait until the transmission is complete before waiting the 200ms
	MOV	#lo_addr(_CAN_Engine), W0
	ZE	[W0], W0
	AND	W0, #1, W0
	CP	W0, #0
	BRA Z	L__NMRAnetStateMachine_Process770
	GOTO	L__NMRAnetStateMachine_Process508
L__NMRAnetStateMachine_Process770:
;NMRAnetStateMachine.mpas,1083 :: 		Node^.iStateMachine := STATE_NMRABUS_WAITSTATE;
	ADD	W10, #25, W1
	MOV.B	#5, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,1084 :: 		Node^.Login.TimeCounter := 0;
	ADD	W10, #20, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,1085 :: 		end
L__NMRAnetStateMachine_Process508:
;NMRAnetStateMachine.mpas,1086 :: 		end
L__NMRAnetStateMachine_Process506:
;NMRAnetStateMachine.mpas,1087 :: 		end;
	GOTO	L__NMRAnetStateMachine_Process467
L__NMRAnetStateMachine_Process503:
;NMRAnetStateMachine.mpas,1088 :: 		STATE_NMRABUS_WAITSTATE :
	CP.B	W2, #5
	BRA Z	L__NMRAnetStateMachine_Process771
	GOTO	L__NMRAnetStateMachine_Process512
L__NMRAnetStateMachine_Process771:
;NMRAnetStateMachine.mpas,1090 :: 		if Node^.Login.TimeCounter > MAX_BUS_LOGIN_TIMEOUT then
	ADD	W10, #20, W0
	MOV.B	[W0], W0
	CP.B	W0, #3
	BRA GTU	L__NMRAnetStateMachine_Process772
	GOTO	L__NMRAnetStateMachine_Process514
L__NMRAnetStateMachine_Process772:
;NMRAnetStateMachine.mpas,1091 :: 		Node^.iStateMachine := STATE_NMRABUS_SEND_LOGIN_RID;
	ADD	W10, #25, W1
	MOV.B	#6, W0
	MOV.B	W0, [W1]
L__NMRAnetStateMachine_Process514:
;NMRAnetStateMachine.mpas,1092 :: 		end;
	GOTO	L__NMRAnetStateMachine_Process467
L__NMRAnetStateMachine_Process512:
;NMRAnetStateMachine.mpas,1093 :: 		STATE_NMRABUS_SEND_LOGIN_RID :
	CP.B	W2, #6
	BRA Z	L__NMRAnetStateMachine_Process773
	GOTO	L__NMRAnetStateMachine_Process518
L__NMRAnetStateMachine_Process773:
;NMRAnetStateMachine.mpas,1095 :: 		if NMRAnetNode_TestMsgFlags(Node, MF_DUPLICATE_ALIAS, True) then
	MOV	#65535, W12
	MOV	#2, W11
	CALL	_NMRAnetNode_TestMsgFlags
	CP0	W0
	BRA NZ	L__NMRAnetStateMachine_Process774
	GOTO	L__NMRAnetStateMachine_Process520
L__NMRAnetStateMachine_Process774:
;NMRAnetStateMachine.mpas,1096 :: 		Node^.iStateMachine := STATE_RANDOM_NUMBER_GENERATOR
	ADD	W10, #25, W1
	MOV.B	#2, W0
	MOV.B	W0, [W1]
	GOTO	L__NMRAnetStateMachine_Process521
;NMRAnetStateMachine.mpas,1097 :: 		else begin
L__NMRAnetStateMachine_Process520:
;NMRAnetStateMachine.mpas,1098 :: 		if CANBusBufferAvailable then
	CALL	NMRAnetStateMachine_CANBusBufferAvailable
	CP0	W0
	BRA NZ	L__NMRAnetStateMachine_Process775
	GOTO	L__NMRAnetStateMachine_Process523
L__NMRAnetStateMachine_Process775:
;NMRAnetStateMachine.mpas,1100 :: 		TransmitCANLayerMsg(Node, @CANBuffer, MTI_RID);
	ADD	W14, #0, W0
	MOV	#0, W12
	MOV	#112, W13
	MOV	W0, W11
	CALL	_TransmitCANLayerMsg
;NMRAnetStateMachine.mpas,1101 :: 		Node^.iStateMachine := STATE_NMRABUS_SEND_LOGIN_AMD;
	ADD	W10, #25, W1
	MOV.B	#8, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,1102 :: 		end
L__NMRAnetStateMachine_Process523:
;NMRAnetStateMachine.mpas,1103 :: 		end
L__NMRAnetStateMachine_Process521:
;NMRAnetStateMachine.mpas,1104 :: 		end;
	GOTO	L__NMRAnetStateMachine_Process467
L__NMRAnetStateMachine_Process518:
;NMRAnetStateMachine.mpas,1105 :: 		STATE_NMRABUS_SEND_LOGIN_AMD :
	MOV.B	[W14+20], W0
	CP.B	W0, #8
	BRA Z	L__NMRAnetStateMachine_Process776
	GOTO	L__NMRAnetStateMachine_Process527
L__NMRAnetStateMachine_Process776:
;NMRAnetStateMachine.mpas,1107 :: 		if NMRAnetNode_TestMsgFlags(Node, MF_DUPLICATE_ALIAS, True) then
	MOV	#65535, W12
	MOV	#2, W11
	CALL	_NMRAnetNode_TestMsgFlags
	CP0	W0
	BRA NZ	L__NMRAnetStateMachine_Process777
	GOTO	L__NMRAnetStateMachine_Process529
L__NMRAnetStateMachine_Process777:
;NMRAnetStateMachine.mpas,1108 :: 		Node^.iStateMachine := STATE_RANDOM_NUMBER_GENERATOR
	ADD	W10, #25, W1
	MOV.B	#2, W0
	MOV.B	W0, [W1]
	GOTO	L__NMRAnetStateMachine_Process530
;NMRAnetStateMachine.mpas,1109 :: 		else begin
L__NMRAnetStateMachine_Process529:
;NMRAnetStateMachine.mpas,1110 :: 		if CANBusBufferAvailable then
	CALL	NMRAnetStateMachine_CANBusBufferAvailable
	CP0	W0
	BRA NZ	L__NMRAnetStateMachine_Process778
	GOTO	L__NMRAnetStateMachine_Process532
L__NMRAnetStateMachine_Process778:
;NMRAnetStateMachine.mpas,1112 :: 		TransmitCANLayerMsg(Node, @CANBuffer, MTI_AMD);
	ADD	W14, #0, W0
	MOV	#4096, W12
	MOV	#112, W13
	MOV	W0, W11
	CALL	_TransmitCANLayerMsg
;NMRAnetStateMachine.mpas,1113 :: 		NMRAnetNode_SetStateFlag(Node, NS_PERMITTED);
	MOV.B	#2, W11
	CALL	_NMRAnetNode_SetStateFlag
;NMRAnetStateMachine.mpas,1114 :: 		Node^.iStateMachine := STATE_NMRABUS_INITIALIZED
	ADD	W10, #25, W1
	MOV.B	#9, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,1115 :: 		end
L__NMRAnetStateMachine_Process532:
;NMRAnetStateMachine.mpas,1116 :: 		end
L__NMRAnetStateMachine_Process530:
;NMRAnetStateMachine.mpas,1117 :: 		end;
	GOTO	L__NMRAnetStateMachine_Process467
L__NMRAnetStateMachine_Process527:
;NMRAnetStateMachine.mpas,1118 :: 		STATE_NMRABUS_INITIALIZED :
	MOV.B	[W14+20], W0
	CP.B	W0, #9
	BRA Z	L__NMRAnetStateMachine_Process779
	GOTO	L__NMRAnetStateMachine_Process536
L__NMRAnetStateMachine_Process779:
;NMRAnetStateMachine.mpas,1120 :: 		if NMRAnetNode_TestMsgFlags(Node, MF_DUPLICATE_ALIAS, True) then
	MOV	#65535, W12
	MOV	#2, W11
	CALL	_NMRAnetNode_TestMsgFlags
	CP0	W0
	BRA NZ	L__NMRAnetStateMachine_Process780
	GOTO	L__NMRAnetStateMachine_Process538
L__NMRAnetStateMachine_Process780:
;NMRAnetStateMachine.mpas,1121 :: 		Node^.iStateMachine := STATE_RANDOM_NUMBER_GENERATOR
	ADD	W10, #25, W1
	MOV.B	#2, W0
	MOV.B	W0, [W1]
	GOTO	L__NMRAnetStateMachine_Process539
;NMRAnetStateMachine.mpas,1122 :: 		else begin
L__NMRAnetStateMachine_Process538:
;NMRAnetStateMachine.mpas,1123 :: 		if NMRABusBufferAvailable then
	CALL	_NMRABusBufferAvailable
	CP0	W0
	BRA NZ	L__NMRAnetStateMachine_Process781
	GOTO	L__NMRAnetStateMachine_Process541
L__NMRAnetStateMachine_Process781:
;NMRAnetStateMachine.mpas,1125 :: 		NMRAnetUtilities_LoadFrameCANBufferDataWith48BitNodeID(Node, @CANBuffer);
	ADD	W14, #0, W0
	MOV	W0, W11
	CALL	_NMRAnetUtilities_LoadFrameCANBufferDataWith48BitNodeID
;NMRAnetStateMachine.mpas,1126 :: 		TransmitNMRABusLayerMsg(Node, @CANBuffer, MTI_INITIALIZATION_COMPLETE, 0, 6, @CANBuffer.DataBytes, False);
	ADD	W14, #0, W0
	ADD	W14, #5, W1
	MOV	#0, W12
	MOV	#2320, W13
	MOV	W0, W11
	CLR	W0
	PUSH	W0
	PUSH	W1
	MOV	#6, W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	CALL	_TransmitNMRABusLayerMsg
	SUB	#8, W15
;NMRAnetStateMachine.mpas,1127 :: 		Node^.iStateMachine := STATE_NMRABUS_LOGIN_IDENTIFY_EVENTS;
	ADD	W10, #25, W1
	MOV.B	#10, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,1128 :: 		NMRAnetNode_SetStateFlag(Node, NS_INITIALIZED);
	MOV.B	#4, W11
	CALL	_NMRAnetNode_SetStateFlag
;NMRAnetStateMachine.mpas,1129 :: 		end
L__NMRAnetStateMachine_Process541:
;NMRAnetStateMachine.mpas,1130 :: 		end
L__NMRAnetStateMachine_Process539:
;NMRAnetStateMachine.mpas,1131 :: 		end;
	GOTO	L__NMRAnetStateMachine_Process467
L__NMRAnetStateMachine_Process536:
;NMRAnetStateMachine.mpas,1132 :: 		STATE_NMRABUS_LOGIN_IDENTIFY_EVENTS :
	MOV.B	[W14+20], W0
	CP.B	W0, #10
	BRA Z	L__NMRAnetStateMachine_Process782
	GOTO	L__NMRAnetStateMachine_Process545
L__NMRAnetStateMachine_Process782:
;NMRAnetStateMachine.mpas,1136 :: 		CANBuffer.State := CANBuffer.State or BS_EXTENDED;
	MOV.B	[W14+13], W1
	ADD	W14, #13, W0
	IOR.B	W1, #1, [W0]
;NMRAnetStateMachine.mpas,1137 :: 		CANBuffer.ID := $10000000 or MTI_EVENTS_IDENTIFY_DEST;                  // Make Alias ID $000 so we don't trigger a duplicate Alias ID loop!
	MOV	#32768, W0
	MOV	#6550, W1
	MOV	W0, [W14+0]
	MOV	W1, [W14+2]
;NMRAnetStateMachine.mpas,1138 :: 		CANBuffer.DataCount := 2;
	MOV.B	#2, W0
	MOV.B	W0, [W14+4]
;NMRAnetStateMachine.mpas,1139 :: 		CANBuffer.DataBytes[0] := Hi( Node^.Info.AliasID);                      // Addressed to this node
	ADD	W10, #2, W0
	ADD	W0, #16, W1
	ADD	W1, #1, W0
	MOV.B	[W0], W0
	MOV.B	W0, [W14+5]
;NMRAnetStateMachine.mpas,1140 :: 		CANBuffer.DataBytes[1] := Lo( Node^.Info.AliasID);
	MOV.B	[W1], W0
	MOV.B	W0, [W14+6]
;NMRAnetStateMachine.mpas,1141 :: 		ReceivedOnFilter1(@CANBuffer);
	ADD	W14, #0, W0
	PUSH	W10
	MOV	W0, W10
	CALL	_ReceivedOnFilter1
	POP	W10
;NMRAnetStateMachine.mpas,1143 :: 		Node^.iStateMachine := STATE_NMRABUS_PERMITTED;
	ADD	W10, #25, W1
	MOV.B	#11, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,1144 :: 		end;
	GOTO	L__NMRAnetStateMachine_Process467
L__NMRAnetStateMachine_Process545:
;NMRAnetStateMachine.mpas,1145 :: 		STATE_NMRABUS_PERMITTED :
	MOV.B	[W14+20], W0
	CP.B	W0, #11
	BRA Z	L__NMRAnetStateMachine_Process783
	GOTO	L__NMRAnetStateMachine_Process548
L__NMRAnetStateMachine_Process783:
;NMRAnetStateMachine.mpas,1148 :: 		ProcessNode(Node, @CANBuffer);                                             // Handle auto Actions to CAN/NMRAnet messages coming in
	ADD	W14, #0, W0
	PUSH	W10
	MOV	W0, W11
	CALL	NMRAnetStateMachine_ProcessNode
	POP	W10
;NMRAnetStateMachine.mpas,1149 :: 		ProcessOutgoingNode(Node, @CANBuffer);
	ADD	W14, #0, W0
	MOV	W0, W11
	CALL	NMRAnetStateMachine_ProcessOutgoingNode
;NMRAnetStateMachine.mpas,1151 :: 		ProcessAbandonBuffers(Node);
	CALL	NMRAnetStateMachine_ProcessAbandonBuffers
;NMRAnetStateMachine.mpas,1154 :: 		end;
	GOTO	L__NMRAnetStateMachine_Process467
L__NMRAnetStateMachine_Process548:
;NMRAnetStateMachine.mpas,1155 :: 		STATE_NMRABUS_INHIBITED :
	MOV.B	[W14+20], W0
	CP.B	W0, #12
	BRA Z	L__NMRAnetStateMachine_Process784
	GOTO	L__NMRAnetStateMachine_Process551
L__NMRAnetStateMachine_Process784:
;NMRAnetStateMachine.mpas,1157 :: 		CANStorage_FlushBuffers(Node^.Info.AliasID);
	ADD	W10, #2, W0
	ADD	W0, #16, W0
	PUSH	W10
	MOV	[W0], W10
	CALL	_CANStorage_FlushBuffers
	POP	W10
;NMRAnetStateMachine.mpas,1158 :: 		if CANBusBufferAvailable then
	CALL	NMRAnetStateMachine_CANBusBufferAvailable
	CP0	W0
	BRA NZ	L__NMRAnetStateMachine_Process785
	GOTO	L__NMRAnetStateMachine_Process553
L__NMRAnetStateMachine_Process785:
;NMRAnetStateMachine.mpas,1160 :: 		TransmitCANLayerMsg(Node, @CANBuffer, MTI_AMR);
	ADD	W14, #0, W0
	MOV	#12288, W12
	MOV	#112, W13
	MOV	W0, W11
	CALL	_TransmitCANLayerMsg
;NMRAnetStateMachine.mpas,1161 :: 		ReleaseBuffers(Node);
	CALL	NMRAnetStateMachine_ReleaseBuffers
;NMRAnetStateMachine.mpas,1162 :: 		NMRAnetNode_ClearStateFlag(Node, NS_PERMITTED);
	MOV.B	#2, W11
	CALL	_NMRAnetNode_ClearStateFlag
;NMRAnetStateMachine.mpas,1163 :: 		NMRAnetNode_ClearMsgFlags(Node);
	CALL	_NMRAnetNode_ClearMsgFlags
;NMRAnetStateMachine.mpas,1164 :: 		Node^.iStateMachine := STATE_RANDOM_NUMBER_GENERATOR;
	ADD	W10, #25, W1
	MOV.B	#2, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,1165 :: 		end
L__NMRAnetStateMachine_Process553:
;NMRAnetStateMachine.mpas,1166 :: 		end;
	GOTO	L__NMRAnetStateMachine_Process467
L__NMRAnetStateMachine_Process551:
;NMRAnetStateMachine.mpas,1167 :: 		STATE_NMRABUS_DUPLICATE_FULL_ID :
	MOV.B	[W14+20], W0
	CP.B	W0, #13
	BRA Z	L__NMRAnetStateMachine_Process786
	GOTO	L__NMRAnetStateMachine_Process557
L__NMRAnetStateMachine_Process786:
;NMRAnetStateMachine.mpas,1169 :: 		CANStorage_FlushBuffers(Node^.Info.AliasID);
	ADD	W10, #2, W0
	ADD	W0, #16, W0
	PUSH	W10
	MOV	[W0], W10
	CALL	_CANStorage_FlushBuffers
	POP	W10
;NMRAnetStateMachine.mpas,1170 :: 		if CANBusBufferAvailable then
	CALL	NMRAnetStateMachine_CANBusBufferAvailable
	CP0	W0
	BRA NZ	L__NMRAnetStateMachine_Process787
	GOTO	L__NMRAnetStateMachine_Process559
L__NMRAnetStateMachine_Process787:
;NMRAnetStateMachine.mpas,1172 :: 		TransmitCANLayerMsg(Node, @CANBuffer, MTI_AMR);
	ADD	W14, #0, W0
	MOV	#12288, W12
	MOV	#112, W13
	MOV	W0, W11
	CALL	_TransmitCANLayerMsg
;NMRAnetStateMachine.mpas,1173 :: 		ReleaseBuffers(Node);
	CALL	NMRAnetStateMachine_ReleaseBuffers
;NMRAnetStateMachine.mpas,1174 :: 		NMRAnetNode_ClearStateFlag(Node, NS_PERMITTED);
	MOV.B	#2, W11
	CALL	_NMRAnetNode_ClearStateFlag
;NMRAnetStateMachine.mpas,1175 :: 		NMRAnetNode_ClearMsgFlags(Node);
	CALL	_NMRAnetNode_ClearMsgFlags
;NMRAnetStateMachine.mpas,1176 :: 		Node^.iStateMachine := STATE_NMRABUS_TAKE_OFFLINE
	ADD	W10, #25, W1
	MOV.B	#14, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,1177 :: 		end
L__NMRAnetStateMachine_Process559:
;NMRAnetStateMachine.mpas,1178 :: 		end;
	GOTO	L__NMRAnetStateMachine_Process467
L__NMRAnetStateMachine_Process557:
;NMRAnetStateMachine.mpas,1179 :: 		STATE_NMRABUS_TAKE_OFFLINE :
	MOV.B	[W14+20], W0
	CP.B	W0, #14
	BRA Z	L__NMRAnetStateMachine_Process788
	GOTO	L__NMRAnetStateMachine_Process563
L__NMRAnetStateMachine_Process788:
;NMRAnetStateMachine.mpas,1181 :: 		if NMRABusBufferAvailable then
	CALL	_NMRABusBufferAvailable
	CP0	W0
	BRA NZ	L__NMRAnetStateMachine_Process789
	GOTO	L__NMRAnetStateMachine_Process565
L__NMRAnetStateMachine_Process789:
;NMRAnetStateMachine.mpas,1183 :: 		TransmitNMRABusLayerMsg(Node, @CANBuffer, MTI_PC_EVENT_REPORT, 0, 8, @EVENT_DUPLICATE_ID_DETECTED, False);
	ADD	W14, #0, W0
	MOV	#16384, W12
	MOV	#2395, W13
	MOV	W0, W11
	CLR	W0
	PUSH	W0
	MOV	#lo_addr(_EVENT_DUPLICATE_ID_DETECTED), W0
	PUSH	W0
	MOV	#8, W0
	PUSH	W0
	CLR	W0
	PUSH	W0
	CALL	_TransmitNMRABusLayerMsg
	SUB	#8, W15
;NMRAnetStateMachine.mpas,1184 :: 		Node^.iStateMachine := STATE_NMRABUS_OFFLINE
	ADD	W10, #25, W1
	MOV.B	#15, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,1185 :: 		end
L__NMRAnetStateMachine_Process565:
;NMRAnetStateMachine.mpas,1186 :: 		end;
	GOTO	L__NMRAnetStateMachine_Process467
L__NMRAnetStateMachine_Process563:
;NMRAnetStateMachine.mpas,1187 :: 		STATE_NMRABUS_OFFLINE :
	MOV.B	[W14+20], W0
	CP.B	W0, #15
	BRA Z	L__NMRAnetStateMachine_Process790
	GOTO	L__NMRAnetStateMachine_Process569
L__NMRAnetStateMachine_Process790:
;NMRAnetStateMachine.mpas,1191 :: 		else
	GOTO	L__NMRAnetStateMachine_Process467
L__NMRAnetStateMachine_Process569:
;NMRAnetStateMachine.mpas,1192 :: 		Node^.iStateMachine := STATE_NMRABUS_START;
	ADD	W10, #25, W1
	CLR	W0
	MOV.B	W0, [W1]
L__NMRAnetStateMachine_Process467:
;NMRAnetStateMachine.mpas,1194 :: 		UnLockCANInterrupt;
	CALL	_UnLockCANInterrupt
;NMRAnetStateMachine.mpas,1195 :: 		end;
L_end_NMRAnetStateMachine_Process:
	POP	W13
	POP	W12
	POP	W11
	ULNK
	RETURN
; end of _NMRAnetStateMachine_Process

_NMRAnetStateMachine_Disconnect:

;NMRAnetStateMachine.mpas,1206 :: 		begin
;NMRAnetStateMachine.mpas,1207 :: 		Node^.iStateMachine := STATE_NMRABUS_INHIBITED
	ADD	W10, #25, W1
	MOV.B	#12, W0
	MOV.B	W0, [W1]
;NMRAnetStateMachine.mpas,1208 :: 		end;
L_end_NMRAnetStateMachine_Disconnect:
	RETURN
; end of _NMRAnetStateMachine_Disconnect
