
_INTERRUPT_DCC_Timer:
	PUSH	DSWPAG
	PUSH	50
	PUSH	RCOUNT
	PUSH	W0
	MOV	#2, W0
	REPEAT	#12
	PUSH	[W0++]

;dsPIC33EP_NMRAnetCommandStation.mpas,62 :: 		begin
;dsPIC33EP_NMRAnetCommandStation.mpas,63 :: 		T1IF_bit := 0;                                                                // Clear the Flag
	PUSH	W10
	PUSH	W11
	BCLR	T1IF_bit, BitPos(T1IF_bit+0)
;dsPIC33EP_NMRAnetCommandStation.mpas,65 :: 		TMR5 := 0;
	CLR	TMR5
;dsPIC33EP_NMRAnetCommandStation.mpas,69 :: 		H_Bridge_A_Lo := 0;                                                           // Bridge Off
	BCLR	LATA1_bit, BitPos(LATA1_bit+0)
;dsPIC33EP_NMRAnetCommandStation.mpas,70 :: 		H_Bridge_A_Hi := 0;                                                           // Bridge Off
	BCLR	LATB0_bit, BitPos(LATB0_bit+0)
;dsPIC33EP_NMRAnetCommandStation.mpas,71 :: 		H_Bridge_B_Lo := 0;                                                           // Bridge Off
	BCLR	LATB1_bit, BitPos(LATB1_bit+0)
;dsPIC33EP_NMRAnetCommandStation.mpas,72 :: 		H_Bridge_B_Hi := 0;                                                           // Bridge Off
	BCLR	LATB4_bit, BitPos(LATB4_bit+0)
;dsPIC33EP_NMRAnetCommandStation.mpas,75 :: 		if Track.TX_Flags.TRANSMITTING_FLAG_DCC_PIN_BIT = 1 then
	MOV	#lo_addr(_Track+35), W0
	MOV.B	[W0], W0
	BTSS.B	W0, #2
	GOTO	L__INTERRUPT_DCC_Timer2
;dsPIC33EP_NMRAnetCommandStation.mpas,77 :: 		H_Bridge_A_Lo := 1;
	BSET	LATA1_bit, BitPos(LATA1_bit+0)
;dsPIC33EP_NMRAnetCommandStation.mpas,78 :: 		H_Bridge_B_Hi := 1;
	BSET	LATB4_bit, BitPos(LATB4_bit+0)
;dsPIC33EP_NMRAnetCommandStation.mpas,79 :: 		end else
	GOTO	L__INTERRUPT_DCC_Timer3
L__INTERRUPT_DCC_Timer2:
;dsPIC33EP_NMRAnetCommandStation.mpas,81 :: 		H_Bridge_A_Hi := 1;
	BSET	LATB0_bit, BitPos(LATB0_bit+0)
;dsPIC33EP_NMRAnetCommandStation.mpas,82 :: 		H_Bridge_B_Lo := 1;
	BSET	LATB1_bit, BitPos(LATB1_bit+0)
;dsPIC33EP_NMRAnetCommandStation.mpas,83 :: 		end;
L__INTERRUPT_DCC_Timer3:
;dsPIC33EP_NMRAnetCommandStation.mpas,86 :: 		NMRA_DCC_56us_TimeTick(@Track);                                                         // < 1us
	MOV	#lo_addr(_Track), W10
	CALL	_NMRA_DCC_56us_TimeTick
;dsPIC33EP_NMRAnetCommandStation.mpas,87 :: 		NMRA_DCC_TransmitterStateMachine(@Track);                                               // < 5us
	MOV	#lo_addr(_Track), W10
	CALL	_NMRA_DCC_TransmitterStateMachine
;dsPIC33EP_NMRAnetCommandStation.mpas,88 :: 		NMRA_DCC_LoadPacketIntoTransmitterStateMachine(@Track, PREAMBLE_BIT_COUNT_NORMAL);      // < 11us    Max
	MOV.B	#14, W11
	MOV	#lo_addr(_Track), W10
	CALL	_NMRA_DCC_LoadPacketIntoTransmitterStateMachine
;dsPIC33EP_NMRAnetCommandStation.mpas,90 :: 		if TMR5 > DCCTime then DCCTime := TMR5;
	MOV	TMR5, W1
	MOV	#lo_addr(_DCCTime), W0
	CP	W1, [W0]
	BRA GTU	L__INTERRUPT_DCC_Timer71
	GOTO	L__INTERRUPT_DCC_Timer5
L__INTERRUPT_DCC_Timer71:
	MOV	TMR5, WREG
	MOV	W0, _DCCTime
L__INTERRUPT_DCC_Timer5:
;dsPIC33EP_NMRAnetCommandStation.mpas,91 :: 		end;
L_end_INTERRUPT_DCC_Timer:
	POP	W11
	POP	W10
	MOV	#26, W0
	REPEAT	#12
	POP	[W0--]
	POP	W0
	POP	RCOUNT
	POP	50
	POP	DSWPAG
	RETFIE
; end of _INTERRUPT_DCC_Timer

_INTERRUPT_1ms_Timer:
	LNK	#2
	PUSH	DSWPAG
	PUSH	50
	PUSH	RCOUNT
	PUSH	W0
	MOV	#2, W0
	REPEAT	#12
	PUSH	[W0++]

;dsPIC33EP_NMRAnetCommandStation.mpas,97 :: 		begin
;dsPIC33EP_NMRAnetCommandStation.mpas,98 :: 		T2IF_bit := 0;                                                                // Clear the Flag
	PUSH	W10
	BCLR	T2IF_bit, BitPos(T2IF_bit+0)
;dsPIC33EP_NMRAnetCommandStation.mpas,99 :: 		for i := 0 to Nodes.AllocatedCount - 1 do
; i start address is: 4 (W2)
	CLR	W2
; i end address is: 4 (W2)
L__INTERRUPT_1ms_Timer8:
; i start address is: 4 (W2)
	MOV	_Nodes+3120, W0
	DEC	W0
	MOV	W0, [W14+0]
	CP	W2, W0
	BRA LE	L__INTERRUPT_1ms_Timer73
	GOTO	L__INTERRUPT_1ms_Timer12
L__INTERRUPT_1ms_Timer73:
;dsPIC33EP_NMRAnetCommandStation.mpas,100 :: 		NMRAnetStateMachine_100ms_Timer(Nodes.AllocatedList[i]);
	SL	W2, #1, W1
	MOV	#lo_addr(_Nodes+2990), W0
	ADD	W0, W1, W0
	MOV	[W0], W10
	CALL	_NMRAnetStateMachine_100ms_Timer
	MOV	[W14+0], W0
	CP	W2, W0
	BRA NZ	L__INTERRUPT_1ms_Timer74
	GOTO	L__INTERRUPT_1ms_Timer12
L__INTERRUPT_1ms_Timer74:
; i start address is: 4 (W2)
	INC	W2
; i end address is: 4 (W2)
; i end address is: 4 (W2)
	GOTO	L__INTERRUPT_1ms_Timer8
L__INTERRUPT_1ms_Timer12:
;dsPIC33EP_NMRAnetCommandStation.mpas,101 :: 		NMRAnetBufferPools_100ms_TimeTick;
	CALL	_NMRAnetBufferPools_100ms_TimeTick
;dsPIC33EP_NMRAnetCommandStation.mpas,102 :: 		end;
L_end_INTERRUPT_1ms_Timer:
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
; end of _INTERRUPT_1ms_Timer

_main:
	MOV	#2048, W15
	MOV	#6142, W0
	MOV	WREG, 32
	MOV	#1, W0
	MOV	WREG, 50
	MOV	#4, W0
	IOR	68
	LNK	#72

;dsPIC33EP_NMRAnetCommandStation.mpas,115 :: 		begin
;dsPIC33EP_NMRAnetCommandStation.mpas,116 :: 		_25AAxxxx_Initialize;
	PUSH	W10
	PUSH	W11
	PUSH	W12
	PUSH	W13
	CALL	__25AAxxxx_Initialize
;dsPIC33EP_NMRAnetCommandStation.mpas,117 :: 		NMRAnetStateMachine_Initialize(MUSTANGPEAK_ID_0_HI, MUSTANGPEAK_COMMANDSTATION_ID_0_LO);
	MOV	#1792, W12
	MOV	#1, W13
	MOV	#257, W10
	MOV	#5, W11
	CALL	_NMRAnetStateMachine_Initialize
;dsPIC33EP_NMRAnetCommandStation.mpas,118 :: 		MCU_Setup_Initialize;                                                         // Start the timers and perpherials last
	CALL	_MCU_Setup_Initialize
;dsPIC33EP_NMRAnetCommandStation.mpas,119 :: 		NMRA_DCC_Initialize;
	CALL	_NMRA_DCC_Initialize
;dsPIC33EP_NMRAnetCommandStation.mpas,121 :: 		H_Bridge_A_Lo := 0;                                                           // Bridge Off
	BCLR	LATA1_bit, BitPos(LATA1_bit+0)
;dsPIC33EP_NMRAnetCommandStation.mpas,122 :: 		H_Bridge_A_Hi := 0;                                                           // Bridge Off
	BCLR	LATB0_bit, BitPos(LATB0_bit+0)
;dsPIC33EP_NMRAnetCommandStation.mpas,123 :: 		H_Bridge_B_Lo := 0;                                                           // Bridge Off
	BCLR	LATB1_bit, BitPos(LATB1_bit+0)
;dsPIC33EP_NMRAnetCommandStation.mpas,124 :: 		H_Bridge_B_Hi := 0;                                                           // Bridge Off
	BCLR	LATB4_bit, BitPos(LATB4_bit+0)
;dsPIC33EP_NMRAnetCommandStation.mpas,125 :: 		H_Bridge_A_Lo_Direction := 0;                                                 // Output
	BCLR	TRISA1_bit, BitPos(TRISA1_bit+0)
;dsPIC33EP_NMRAnetCommandStation.mpas,126 :: 		H_Bridge_A_Hi_Direction := 0;                                                 // Output
	BCLR	TRISB0_bit, BitPos(TRISB0_bit+0)
;dsPIC33EP_NMRAnetCommandStation.mpas,127 :: 		H_Bridge_B_Lo_Direction := 0;                                                 // Output
	BCLR	TRISB1_bit, BitPos(TRISB1_bit+0)
;dsPIC33EP_NMRAnetCommandStation.mpas,128 :: 		H_Bridge_B_Hi_Direction := 0;                                                 // Output
	BCLR	TRISB4_bit, BitPos(TRISB4_bit+0)
;dsPIC33EP_NMRAnetCommandStation.mpas,131 :: 		TON_T1CON_bit := 1;                                                           // Start the DCC Timer
	BSET	TON_T1CON_bit, BitPos(TON_T1CON_bit+0)
;dsPIC33EP_NMRAnetCommandStation.mpas,132 :: 		NMRA_DCC_Packet_Init;                                                         // Send our 20 Idle Packets per the spec, note we are not on the OLCB bus yet so this will block until done.
	CALL	_NMRA_DCC_Packet_Init
;dsPIC33EP_NMRAnetCommandStation.mpas,135 :: 		UART1_Write_Text('Starting'+LF);
	ADD	W14, #0, W1
	MOV.B	#83, W0
	MOV.B	W0, [W1++]
	MOV.B	#116, W0
	MOV.B	W0, [W1++]
	MOV.B	#97, W0
	MOV.B	W0, [W1++]
	MOV.B	#114, W0
	MOV.B	W0, [W1++]
	MOV.B	#116, W0
	MOV.B	W0, [W1++]
	MOV.B	#105, W0
	MOV.B	W0, [W1++]
	MOV.B	#110, W0
	MOV.B	W0, [W1++]
	MOV.B	#103, W0
	MOV.B	W0, [W1++]
	MOV.B	#13, W0
	MOV.B	W0, [W1++]
	MOV.B	#10, W0
	MOV.B	W0, [W1++]
	MOV.B	#0, W0
	MOV.B	W0, [W1++]
	ADD	W14, #0, W0
	MOV	W0, W10
	CALL	_UART1_Write_Text
;dsPIC33EP_NMRAnetCommandStation.mpas,137 :: 		x := 0;
	CLR	W0
	MOV	W0, _x
;dsPIC33EP_NMRAnetCommandStation.mpas,138 :: 		y := 0;
	CLR	W0
	MOV	W0, _y
;dsPIC33EP_NMRAnetCommandStation.mpas,139 :: 		j := 0;
	CLR	W0
	MOV	W0, _j
;dsPIC33EP_NMRAnetCommandStation.mpas,140 :: 		i := 0;
	CLR	W0
	MOV	W0, _i
;dsPIC33EP_NMRAnetCommandStation.mpas,141 :: 		MaxTime_RX := 0;
	CLR	W0
	MOV	W0, _MaxTime_RX
;dsPIC33EP_NMRAnetCommandStation.mpas,142 :: 		LastRxTime := 0;
	CLR	W0
	MOV	W0, _LastRxTime
;dsPIC33EP_NMRAnetCommandStation.mpas,143 :: 		MaxTime_StateMachine := 0;
	CLR	W0
	MOV	W0, _MaxTime_StateMachine
;dsPIC33EP_NMRAnetCommandStation.mpas,144 :: 		LastStateMachineTime := 0;
	CLR	W0
	MOV	W0, _LastStateMachineTime
;dsPIC33EP_NMRAnetCommandStation.mpas,145 :: 		DCCTime := 0;
	CLR	W0
	MOV	W0, _DCCTime
;dsPIC33EP_NMRAnetCommandStation.mpas,146 :: 		LastDCCTime := 0;
	CLR	W0
	MOV	W0, _LastDCCTime
;dsPIC33EP_NMRAnetCommandStation.mpas,148 :: 		TRISA4_bit := 0;
	BCLR	TRISA4_bit, BitPos(TRISA4_bit+0)
;dsPIC33EP_NMRAnetCommandStation.mpas,149 :: 		LATA4_bit := 0;                                                   // Output
	BCLR	LATA4_bit, BitPos(LATA4_bit+0)
;dsPIC33EP_NMRAnetCommandStation.mpas,151 :: 		j := 0;
	CLR	W0
	MOV	W0, _j
;dsPIC33EP_NMRAnetCommandStation.mpas,153 :: 		TON_T3CON_bit := 1;       // Turn on Timer 3 to time loops
	BSET	TON_T3CON_bit, BitPos(TON_T3CON_bit+0)
;dsPIC33EP_NMRAnetCommandStation.mpas,154 :: 		TON_T4CON_bit := 1;       // Turn on Timer 3 to time loops
	BSET	TON_T4CON_bit, BitPos(TON_T4CON_bit+0)
;dsPIC33EP_NMRAnetCommandStation.mpas,155 :: 		TON_T5CON_bit := 1;
	BSET	TON_T5CON_bit, BitPos(TON_T5CON_bit+0)
;dsPIC33EP_NMRAnetCommandStation.mpas,157 :: 		while (TRUE) do
L__main15:
;dsPIC33EP_NMRAnetCommandStation.mpas,159 :: 		ActiveNode := NMRAnetNode_NextNode;
	CALL	_NMRAnetNode_NextNode
	MOV	W0, _ActiveNode
;dsPIC33EP_NMRAnetCommandStation.mpas,160 :: 		if ActiveNode <> PNMRAnetNode( nil) then
	MOV	#0, W1
	CP	W0, W1
	BRA NZ	L__main76
	GOTO	L__main20
L__main76:
;dsPIC33EP_NMRAnetCommandStation.mpas,163 :: 		TMR3 := 0;
	CLR	TMR3
;dsPIC33EP_NMRAnetCommandStation.mpas,164 :: 		NMRAnetStateMachine_Process(ActiveNode);
	MOV	_ActiveNode, W10
	CALL	_NMRAnetStateMachine_Process
;dsPIC33EP_NMRAnetCommandStation.mpas,165 :: 		if TMR3 > MaxTime_StateMachine then MaxTime_StateMachine := TMR3;
	MOV	TMR3, W1
	MOV	#lo_addr(_MaxTime_StateMachine), W0
	CP	W1, [W0]
	BRA GTU	L__main77
	GOTO	L__main23
L__main77:
	MOV	TMR3, WREG
	MOV	W0, _MaxTime_StateMachine
L__main23:
;dsPIC33EP_NMRAnetCommandStation.mpas,167 :: 		if MaxTime_StateMachine > LastStateMachineTime then
	MOV	_MaxTime_StateMachine, W1
	MOV	#lo_addr(_LastStateMachineTime), W0
	CP	W1, [W0]
	BRA GTU	L__main78
	GOTO	L__main26
L__main78:
;dsPIC33EP_NMRAnetCommandStation.mpas,169 :: 		LastStateMachineTime := MaxTime_StateMachine;
	MOV	_MaxTime_StateMachine, W0
	MOV	W0, _LastStateMachineTime
;dsPIC33EP_NMRAnetCommandStation.mpas,170 :: 		TickTime := real( LastStateMachineTime) * 16.6666666e-9;  // nano seconds
	MOV	_MaxTime_StateMachine, W0
	CLR	W1
	CALL	__Long2Float
	MOV	#10851, W2
	MOV	#12943, W3
	CALL	__Mul_FP
	MOV	W0, _TickTime
	MOV	W1, _TickTime+2
;dsPIC33EP_NMRAnetCommandStation.mpas,171 :: 		TickTime := TickTime*1e6;  // micro seconds (us)
	MOV	#9216, W2
	MOV	#18804, W3
	CALL	__Mul_FP
	MOV	W0, _TickTime
	MOV	W1, _TickTime+2
;dsPIC33EP_NMRAnetCommandStation.mpas,172 :: 		FloatToStr(TickTime, s1) ;
	MOV	#lo_addr(_s1), W12
	MOV.D	W0, W10
	CALL	_FloatToStr
;dsPIC33EP_NMRAnetCommandStation.mpas,173 :: 		UART1_Write_Text('StateMachineTime = ' + s1 + 'us'+LF);
	ADD	W14, #14, W0
	MOV.B	#83, W2
	MOV.B	W2, [W0++]
	MOV.B	#116, W2
	MOV.B	W2, [W0++]
	MOV.B	#97, W2
	MOV.B	W2, [W0++]
	MOV.B	#116, W2
	MOV.B	W2, [W0++]
	MOV.B	#101, W2
	MOV.B	W2, [W0++]
	MOV.B	#77, W2
	MOV.B	W2, [W0++]
	MOV.B	#97, W2
	MOV.B	W2, [W0++]
	MOV.B	#99, W2
	MOV.B	W2, [W0++]
	MOV.B	#104, W2
	MOV.B	W2, [W0++]
	MOV.B	#105, W2
	MOV.B	W2, [W0++]
	MOV.B	#110, W2
	MOV.B	W2, [W0++]
	MOV.B	#101, W2
	MOV.B	W2, [W0++]
	MOV.B	#84, W2
	MOV.B	W2, [W0++]
	MOV.B	#105, W2
	MOV.B	W2, [W0++]
	MOV.B	#109, W2
	MOV.B	W2, [W0++]
	MOV.B	#101, W2
	MOV.B	W2, [W0++]
	MOV.B	#32, W2
	MOV.B	W2, [W0++]
	MOV.B	#61, W2
	MOV.B	W2, [W0++]
	MOV.B	#32, W2
	MOV.B	W2, [W0++]
	MOV	#lo_addr(_s1), W1
	CALL	___CS2S
	MOV.B	#117, W2
	MOV.B	W2, [W0++]
	MOV.B	#115, W2
	MOV.B	W2, [W0++]
	MOV	#lo_addr(_LF), W1
	MOV	#___Lib_System_DefaultPage, W2
	MOV	W2, 50
	CALL	___CS2S
	MOV	W0, W1
	CLR	W0
	MOV.B	W0, [W1++]
	ADD	W14, #14, W0
	MOV	W0, W10
	CALL	_UART1_Write_Text
;dsPIC33EP_NMRAnetCommandStation.mpas,174 :: 		end;
L__main26:
;dsPIC33EP_NMRAnetCommandStation.mpas,175 :: 		if MaxTime_Rx > LastRxTime then
	MOV	_MaxTime_RX, W1
	MOV	#lo_addr(_LastRxTime), W0
	CP	W1, [W0]
	BRA GTU	L__main79
	GOTO	L__main29
L__main79:
;dsPIC33EP_NMRAnetCommandStation.mpas,177 :: 		LastRxTime := MaxTime_Rx;
	MOV	_MaxTime_RX, W0
	MOV	W0, _LastRxTime
;dsPIC33EP_NMRAnetCommandStation.mpas,178 :: 		TickTime := real( LastRxTime) * 16.6666666e-9;  // nano seconds
	MOV	_MaxTime_RX, W0
	CLR	W1
	CALL	__Long2Float
	MOV	#10851, W2
	MOV	#12943, W3
	CALL	__Mul_FP
	MOV	W0, _TickTime
	MOV	W1, _TickTime+2
;dsPIC33EP_NMRAnetCommandStation.mpas,179 :: 		TickTime := TickTime*1e6;  // micro seconds (us)
	MOV	#9216, W2
	MOV	#18804, W3
	CALL	__Mul_FP
	MOV	W0, _TickTime
	MOV	W1, _TickTime+2
;dsPIC33EP_NMRAnetCommandStation.mpas,180 :: 		FloatToStr(TickTime, s1) ;
	MOV	#lo_addr(_s1), W12
	MOV.D	W0, W10
	CALL	_FloatToStr
;dsPIC33EP_NMRAnetCommandStation.mpas,181 :: 		UART1_Write_Text('Rx Interrupt = ' + s1 + 'us'+LF);
	ADD	W14, #14, W0
	MOV.B	#82, W2
	MOV.B	W2, [W0++]
	MOV.B	#120, W2
	MOV.B	W2, [W0++]
	MOV.B	#32, W2
	MOV.B	W2, [W0++]
	MOV.B	#73, W2
	MOV.B	W2, [W0++]
	MOV.B	#110, W2
	MOV.B	W2, [W0++]
	MOV.B	#116, W2
	MOV.B	W2, [W0++]
	MOV.B	#101, W2
	MOV.B	W2, [W0++]
	MOV.B	#114, W2
	MOV.B	W2, [W0++]
	MOV.B	#114, W2
	MOV.B	W2, [W0++]
	MOV.B	#117, W2
	MOV.B	W2, [W0++]
	MOV.B	#112, W2
	MOV.B	W2, [W0++]
	MOV.B	#116, W2
	MOV.B	W2, [W0++]
	MOV.B	#32, W2
	MOV.B	W2, [W0++]
	MOV.B	#61, W2
	MOV.B	W2, [W0++]
	MOV.B	#32, W2
	MOV.B	W2, [W0++]
	MOV	#lo_addr(_s1), W1
	CALL	___CS2S
	MOV.B	#117, W2
	MOV.B	W2, [W0++]
	MOV.B	#115, W2
	MOV.B	W2, [W0++]
	MOV	#lo_addr(_LF), W1
	MOV	#___Lib_System_DefaultPage, W2
	MOV	W2, 50
	CALL	___CS2S
	MOV	W0, W1
	CLR	W0
	MOV.B	W0, [W1++]
	ADD	W14, #14, W0
	MOV	W0, W10
	CALL	_UART1_Write_Text
;dsPIC33EP_NMRAnetCommandStation.mpas,182 :: 		end;
L__main29:
;dsPIC33EP_NMRAnetCommandStation.mpas,183 :: 		if DCCTime > LastDCCTime then
	MOV	_DCCTime, W1
	MOV	#lo_addr(_LastDCCTime), W0
	CP	W1, [W0]
	BRA GTU	L__main80
	GOTO	L__main32
L__main80:
;dsPIC33EP_NMRAnetCommandStation.mpas,185 :: 		LastDCCTime:= DCCTime;
	MOV	_DCCTime, W0
	MOV	W0, _LastDCCTime
;dsPIC33EP_NMRAnetCommandStation.mpas,186 :: 		TickTime := real( LastDCCTime) * 16.6666666e-9;  // nano seconds
	MOV	_DCCTime, W0
	CLR	W1
	CALL	__Long2Float
	MOV	#10851, W2
	MOV	#12943, W3
	CALL	__Mul_FP
	MOV	W0, _TickTime
	MOV	W1, _TickTime+2
;dsPIC33EP_NMRAnetCommandStation.mpas,187 :: 		TickTime := TickTime*1e6;  // micro seconds (us)
	MOV	#9216, W2
	MOV	#18804, W3
	CALL	__Mul_FP
	MOV	W0, _TickTime
	MOV	W1, _TickTime+2
;dsPIC33EP_NMRAnetCommandStation.mpas,188 :: 		FloatToStr(TickTime, s1) ;
	MOV	#lo_addr(_s1), W12
	MOV.D	W0, W10
	CALL	_FloatToStr
;dsPIC33EP_NMRAnetCommandStation.mpas,189 :: 		UART1_Write_Text('DCC Interrupt = ' + s1 + 'us'+LF);
	ADD	W14, #14, W0
	MOV.B	#68, W2
	MOV.B	W2, [W0++]
	MOV.B	#67, W2
	MOV.B	W2, [W0++]
	MOV.B	#67, W2
	MOV.B	W2, [W0++]
	MOV.B	#32, W2
	MOV.B	W2, [W0++]
	MOV.B	#73, W2
	MOV.B	W2, [W0++]
	MOV.B	#110, W2
	MOV.B	W2, [W0++]
	MOV.B	#116, W2
	MOV.B	W2, [W0++]
	MOV.B	#101, W2
	MOV.B	W2, [W0++]
	MOV.B	#114, W2
	MOV.B	W2, [W0++]
	MOV.B	#114, W2
	MOV.B	W2, [W0++]
	MOV.B	#117, W2
	MOV.B	W2, [W0++]
	MOV.B	#112, W2
	MOV.B	W2, [W0++]
	MOV.B	#116, W2
	MOV.B	W2, [W0++]
	MOV.B	#32, W2
	MOV.B	W2, [W0++]
	MOV.B	#61, W2
	MOV.B	W2, [W0++]
	MOV.B	#32, W2
	MOV.B	W2, [W0++]
	MOV	#lo_addr(_s1), W1
	CALL	___CS2S
	MOV.B	#117, W2
	MOV.B	W2, [W0++]
	MOV.B	#115, W2
	MOV.B	W2, [W0++]
	MOV	#lo_addr(_LF), W1
	MOV	#___Lib_System_DefaultPage, W2
	MOV	W2, 50
	CALL	___CS2S
	MOV	W0, W1
	CLR	W0
	MOV.B	W0, [W1++]
	ADD	W14, #14, W0
	MOV	W0, W10
	CALL	_UART1_Write_Text
;dsPIC33EP_NMRAnetCommandStation.mpas,190 :: 		end;
L__main32:
;dsPIC33EP_NMRAnetCommandStation.mpas,192 :: 		if UART1_Data_Ready then
	CALL	_UART1_Data_Ready
	CP0	W0
	BRA NZ	L__main81
	GOTO	L__main35
L__main81:
;dsPIC33EP_NMRAnetCommandStation.mpas,194 :: 		case UART1_Read of
	CALL	_UART1_Read
	MOV	W0, [W14+70]
;dsPIC33EP_NMRAnetCommandStation.mpas,195 :: 		'P' : begin
	MOV	#80, W1
	CP	W0, W1
	BRA Z	L__main82
	GOTO	L__main40
L__main82:
;dsPIC33EP_NMRAnetCommandStation.mpas,197 :: 		end;
	GOTO	L__main37
L__main40:
;dsPIC33EP_NMRAnetCommandStation.mpas,198 :: 		'X' : begin
	MOV	#88, W1
	CP	W0, W1
	BRA Z	L__main83
	GOTO	L__main43
L__main83:
;dsPIC33EP_NMRAnetCommandStation.mpas,199 :: 		NMRAnetStateMachine_TrySendVerifyNodeID(ActiveNode, 0);
	CLR	W11
	MOV	_ActiveNode, W10
	CALL	_NMRAnetStateMachine_TrySendVerifyNodeID
;dsPIC33EP_NMRAnetCommandStation.mpas,200 :: 		end;
	GOTO	L__main37
L__main43:
;dsPIC33EP_NMRAnetCommandStation.mpas,201 :: 		'A', 'a' :
	MOV	#65, W0
	MOV	[W14+70], W1
	CP	W1, W0
	BRA Z	L__main84
	GOTO	L__main46
L__main84:
	GOTO	L__main44
L__main46:
	MOV	#97, W0
	CP	W1, W0
	BRA Z	L__main85
	GOTO	L__main47
L__main85:
L__main44:
;dsPIC33EP_NMRAnetCommandStation.mpas,203 :: 		NMRAnetNode_Allocate;
	CALL	_NMRAnetNode_Allocate
;dsPIC33EP_NMRAnetCommandStation.mpas,204 :: 		end;
	GOTO	L__main37
L__main47:
;dsPIC33EP_NMRAnetCommandStation.mpas,205 :: 		'D', 'd' :
	MOV	#68, W0
	MOV	[W14+70], W1
	CP	W1, W0
	BRA Z	L__main86
	GOTO	L__main50
L__main86:
	GOTO	L__main48
L__main50:
	MOV	#100, W0
	CP	W1, W0
	BRA Z	L__main87
	GOTO	L__main51
L__main87:
L__main48:
;dsPIC33EP_NMRAnetCommandStation.mpas,207 :: 		TempNode := NMRAnetNode_FindFirstVirtualNode;
	CALL	_NMRAnetNode_FindFirstVirtualNode
	MOV	W0, _TempNode
;dsPIC33EP_NMRAnetCommandStation.mpas,208 :: 		if TempNode <> nil then
	CP	W0, #0
	BRA NZ	L__main88
	GOTO	L__main53
L__main88:
;dsPIC33EP_NMRAnetCommandStation.mpas,210 :: 		ProxyData := GetProxyData( TempNode);
	MOV	_TempNode, W10
	CALL	_GetProxyData
	MOV	W0, _ProxyData
;dsPIC33EP_NMRAnetCommandStation.mpas,211 :: 		ProxyData^.State := PS_ALLOCATED;
	MOV.B	#1, W1
	MOV.B	W1, [W0]
;dsPIC33EP_NMRAnetCommandStation.mpas,212 :: 		end;
L__main53:
;dsPIC33EP_NMRAnetCommandStation.mpas,213 :: 		end;
	GOTO	L__main37
L__main51:
;dsPIC33EP_NMRAnetCommandStation.mpas,214 :: 		'F', 'f' :
	MOV	#70, W0
	MOV	[W14+70], W1
	CP	W1, W0
	BRA Z	L__main89
	GOTO	L__main57
L__main89:
	GOTO	L__main55
L__main57:
	MOV	#102, W0
	CP	W1, W0
	BRA Z	L__main90
	GOTO	L__main58
L__main90:
L__main55:
;dsPIC33EP_NMRAnetCommandStation.mpas,216 :: 		TempNode := NMRAnetNode_FindFirstVirtualNode;
	CALL	_NMRAnetNode_FindFirstVirtualNode
	MOV	W0, _TempNode
;dsPIC33EP_NMRAnetCommandStation.mpas,217 :: 		if TempNode <> nil then
	CP	W0, #0
	BRA NZ	L__main91
	GOTO	L__main60
L__main91:
;dsPIC33EP_NMRAnetCommandStation.mpas,220 :: 		while not NMRAnetStateMachine_TrySendAliasMapReset(TempNode) do;
L__main63:
	MOV	_TempNode, W10
	CALL	_NMRAnetStateMachine_TrySendAliasMapReset
	COM	W0
	BRA Z	L__main92
	GOTO	L__main63
L__main92:
;dsPIC33EP_NMRAnetCommandStation.mpas,221 :: 		NMRAnetNode_Release(TempNode);
	MOV	_TempNode, W10
	CALL	_NMRAnetNode_Release
;dsPIC33EP_NMRAnetCommandStation.mpas,222 :: 		end;
L__main60:
;dsPIC33EP_NMRAnetCommandStation.mpas,223 :: 		end;
	GOTO	L__main37
L__main58:
L__main37:
;dsPIC33EP_NMRAnetCommandStation.mpas,225 :: 		end;
L__main35:
;dsPIC33EP_NMRAnetCommandStation.mpas,226 :: 		end;
L__main20:
;dsPIC33EP_NMRAnetCommandStation.mpas,228 :: 		if C1IE_bit = 0 then
	BTSC	C1IE_bit, BitPos(C1IE_bit+0)
	GOTO	L__main68
;dsPIC33EP_NMRAnetCommandStation.mpas,229 :: 		UART1_Write_Text('CAN Disabled!');
	ADD	W14, #0, W1
	MOV.B	#67, W0
	MOV.B	W0, [W1++]
	MOV.B	#65, W0
	MOV.B	W0, [W1++]
	MOV.B	#78, W0
	MOV.B	W0, [W1++]
	MOV.B	#32, W0
	MOV.B	W0, [W1++]
	MOV.B	#68, W0
	MOV.B	W0, [W1++]
	MOV.B	#105, W0
	MOV.B	W0, [W1++]
	MOV.B	#115, W0
	MOV.B	W0, [W1++]
	MOV.B	#97, W0
	MOV.B	W0, [W1++]
	MOV.B	#98, W0
	MOV.B	W0, [W1++]
	MOV.B	#108, W0
	MOV.B	W0, [W1++]
	MOV.B	#101, W0
	MOV.B	W0, [W1++]
	MOV.B	#100, W0
	MOV.B	W0, [W1++]
	MOV.B	#33, W0
	MOV.B	W0, [W1++]
	CLR	W0
	MOV.B	W0, [W1++]
	ADD	W14, #0, W0
	MOV	W0, W10
	CALL	_UART1_Write_Text
L__main68:
;dsPIC33EP_NMRAnetCommandStation.mpas,231 :: 		end;
	GOTO	L__main15
;dsPIC33EP_NMRAnetCommandStation.mpas,232 :: 		end.
L_end_main:
	ULNK
L__main_end_loop:
	BRA	L__main_end_loop
; end of _main
