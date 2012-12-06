
_NMRAnetDatagrams_InitializeDatagramBuffer:

;NMRAnetDatagrams.mpas,34 :: 		begin
;NMRAnetDatagrams.mpas,35 :: 		if Buffer <> nil then
	CP	W10, #0
	BRA NZ	L__NMRAnetDatagrams_InitializeDatagramBuffer12
	GOTO	L__NMRAnetDatagrams_InitializeDatagramBuffer2
L__NMRAnetDatagrams_InitializeDatagramBuffer12:
;NMRAnetDatagrams.mpas,37 :: 		Buffer^.State := 0;
	CLR	W0
	MOV.B	W0, [W10]
;NMRAnetDatagrams.mpas,38 :: 		Buffer^.Alias := 0;
	ADD	W10, #2, W1
	CLR	W0
	MOV	W0, [W1]
;NMRAnetDatagrams.mpas,39 :: 		Buffer^.Next := 0;
	ADD	W10, #4, W1
	CLR	W0
	MOV	W0, [W1]
;NMRAnetDatagrams.mpas,40 :: 		Buffer^.mCode := 0;
	ADD	W10, #6, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetDatagrams.mpas,41 :: 		Buffer^.StateMachine := 0;
	ADD	W10, #7, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetDatagrams.mpas,42 :: 		Buffer^.Tag := 0;
	ADD	W10, #8, W1
	CLR	W0
	MOV	W0, [W1]
;NMRAnetDatagrams.mpas,43 :: 		Buffer^.SourceNodePtr := Generic16BitPointer( nil);
	MOV	#86, W0
	ADD	W10, W0, W1
	CLR	W0
	MOV	W0, [W1]
;NMRAnetDatagrams.mpas,45 :: 		Buffer^.iByteCount := 0;
	ADD	W10, #10, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetDatagrams.mpas,46 :: 		Buffer^.iWatchdog := 0;
	MOV	#88, W0
	ADD	W10, W0, W1
	CLR	W0
	MOV	W0, [W1]
;NMRAnetDatagrams.mpas,47 :: 		Buffer^.ErrorCode.Count := 1;                                               // Assume only the MTI
	MOV	#83, W0
	ADD	W10, W0, W0
	ADD	W0, #2, W1
	MOV.B	#1, W0
	MOV.B	W0, [W1]
;NMRAnetDatagrams.mpas,48 :: 		Buffer^.ErrorCode.SubType[0] := 0;
	MOV	#83, W0
	ADD	W10, W0, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetDatagrams.mpas,49 :: 		Buffer^.ErrorCode.SubType[1] := 0;
	MOV	#83, W0
	ADD	W10, W0, W0
	ADD	W0, #1, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetDatagrams.mpas,50 :: 		Buffer^.iRetransmit := 0;
	MOV	#90, W0
	ADD	W10, W0, W1
	CLR	W0
	MOV.B	W0, [W1]
;NMRAnetDatagrams.mpas,51 :: 		for j := 0 to LEN_DATAGRAM-1 do
; j start address is: 4 (W2)
	CLR	W2
; j end address is: 4 (W2)
L__NMRAnetDatagrams_InitializeDatagramBuffer5:
;NMRAnetDatagrams.mpas,52 :: 		Buffer^.DataBytes[j] := 0;
; j start address is: 4 (W2)
	ADD	W10, #11, W0
	ADD	W0, W2, W1
	CLR	W0
	MOV.B	W0, [W1]
	MOV	#71, W0
	CP	W2, W0
	BRA NZ	L__NMRAnetDatagrams_InitializeDatagramBuffer13
	GOTO	L__NMRAnetDatagrams_InitializeDatagramBuffer8
L__NMRAnetDatagrams_InitializeDatagramBuffer13:
; j start address is: 4 (W2)
	INC	W2
; j end address is: 4 (W2)
; j end address is: 4 (W2)
	GOTO	L__NMRAnetDatagrams_InitializeDatagramBuffer5
L__NMRAnetDatagrams_InitializeDatagramBuffer8:
;NMRAnetDatagrams.mpas,53 :: 		end
L__NMRAnetDatagrams_InitializeDatagramBuffer2:
;NMRAnetDatagrams.mpas,54 :: 		end;
L_end_NMRAnetDatagrams_InitializeDatagramBuffer:
	RETURN
; end of _NMRAnetDatagrams_InitializeDatagramBuffer

_NMRAnetDatagrams_Initialize:

;NMRAnetDatagrams.mpas,66 :: 		begin
;NMRAnetDatagrams.mpas,67 :: 		DatagramTrainControlCallbackFunc := PDatagramTaskCallbackFunc(nil);
	MOV	#0, W0
	MOV	W0, _DatagramTrainControlCallbackFunc
;NMRAnetDatagrams.mpas,68 :: 		end;
L_end_NMRAnetDatagrams_Initialize:
	RETURN
; end of _NMRAnetDatagrams_Initialize

_NMRAnetDatagrams_SetCallback:

;NMRAnetDatagrams.mpas,91 :: 		begin
;NMRAnetDatagrams.mpas,92 :: 		Hook := Func
	MOV	W11, [W10]
;NMRAnetDatagrams.mpas,93 :: 		end;
L_end_NMRAnetDatagrams_SetCallback:
	RETURN
; end of _NMRAnetDatagrams_SetCallback
