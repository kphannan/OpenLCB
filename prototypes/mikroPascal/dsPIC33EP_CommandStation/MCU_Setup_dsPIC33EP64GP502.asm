
_MCU_Setup_Initialize:

;MCU_Setup_dsPIC33EP64GP502.mpas,27 :: 		begin
;MCU_Setup_dsPIC33EP64GP502.mpas,28 :: 		ANSELA := 0;
	PUSH	W10
	PUSH	W11
	PUSH	W12
	PUSH	W13
	CLR	ANSELA
;MCU_Setup_dsPIC33EP64GP502.mpas,29 :: 		ANSELB := 0;
	CLR	ANSELB
;MCU_Setup_dsPIC33EP64GP502.mpas,32 :: 		OSCCON := OSCCON and $F8FF;                                                   // Clear COSC bits (set to FRC mode)
	MOV	#63743, W1
	MOV	#lo_addr(OSCCON), W0
	AND	W1, [W0], [W0]
;MCU_Setup_dsPIC33EP64GP502.mpas,33 :: 		OSCCON.0 := 1;                                                                // Tell it to change modes
	BSET	OSCCON, #0
;MCU_Setup_dsPIC33EP64GP502.mpas,34 :: 		while OSCCON.0 = 1 do;                                                        // wait for it to take effect
L__MCU_Setup_Initialize2:
	BTSC	OSCCON, #0
	GOTO	L__MCU_Setup_Initialize2
;MCU_Setup_dsPIC33EP64GP502.mpas,36 :: 		CLKDIV := CLKDIV and 0xFFE0;                                                  // PLLPRE<4:0> = 0  ->  N1 = 2    8MHz / 2 = 4MHz
	MOV	#65504, W1
	MOV	#lo_addr(CLKDIV), W0
	AND	W1, [W0], [W0]
;MCU_Setup_dsPIC33EP64GP502.mpas,38 :: 		PLLFBD :=   30;                                                               // PLLDIV<8:0> = 30 ->  M = 32    4MHz * 32 = 128MHz
	MOV	#30, W0
	MOV	WREG, PLLFBD
;MCU_Setup_dsPIC33EP64GP502.mpas,40 :: 		PLLPOST_1_bit := 0;
	BCLR	PLLPOST_1_bit, BitPos(PLLPOST_1_bit+0)
;MCU_Setup_dsPIC33EP64GP502.mpas,41 :: 		PLLPOST_0_bit := 0;                                                           // PLLPOST<1:0> = 0 ->  N2 = 2    128MHz / 2 = 64MHz
	BCLR	PLLPOST_0_bit, BitPos(PLLPOST_0_bit+0)
;MCU_Setup_dsPIC33EP64GP502.mpas,43 :: 		OSCCON := OSCCON or $0300;                                                    // Set COSC to 011 = XT with PLL
	MOV	#768, W1
	MOV	#lo_addr(OSCCON), W0
	IOR	W1, [W0], [W0]
;MCU_Setup_dsPIC33EP64GP502.mpas,44 :: 		OSCCON.0 := 1;                                                                // Tell it to change modes
	BSET	OSCCON, #0
;MCU_Setup_dsPIC33EP64GP502.mpas,45 :: 		while OSCCON.0 = 1 do;                                                        // wait for it to take effect
L__MCU_Setup_Initialize7:
	BTSC	OSCCON, #0
	GOTO	L__MCU_Setup_Initialize7
;MCU_Setup_dsPIC33EP64GP502.mpas,47 :: 		Delay_ms(10);
	MOV	#64000, W7
L__MCU_Setup_Initialize11:
	DEC	W7
	BRA NZ	L__MCU_Setup_Initialize11
	NOP
	NOP
;MCU_Setup_dsPIC33EP64GP502.mpas,49 :: 		dsPIC33_CAN_EnterConfigMode;                                                  // Place the module in Configuration Mode
	CALL	_dsPIC33_CAN_EnterConfigMode
;MCU_Setup_dsPIC33EP64GP502.mpas,51 :: 		dsPIC33_CAN_SetBaud(CAN_SWJ, CAN_BRP, CAN_PHASESEG_2, CAN_PHASESEG_1, CAN_PROP_SEG, True); // Setup the Baud Rate for 125kHz with a 64Mhz Clock
	MOV	#4, W13
	MOV	#5, W12
	MOV	#7, W11
	CLR	W10
	MOV	#65535, W0
	PUSH	W0
	MOV	#3, W0
	PUSH	W0
	CALL	_dsPIC33_CAN_SetBaud
	SUB	#4, W15
;MCU_Setup_dsPIC33EP64GP502.mpas,54 :: 		dsPIC33_CAN_SetMask(2, $08000000, True);                                      // Mask 2 looks only at bit 27 for the Filters
	MOV	#65535, W13
	MOV	#0, W11
	MOV	#2048, W12
	MOV.B	#2, W10
	CALL	_dsPIC33_CAN_SetMask
;MCU_Setup_dsPIC33EP64GP502.mpas,55 :: 		dsPIC33_CAN_SetFilter(0, $00000000, True);                                    // Look for a 0 in bit 27  (CAN Layer Messsage)
	MOV	#65535, W13
	CLR	W11
	CLR	W12
	CLR	W10
	CALL	_dsPIC33_CAN_SetFilter
;MCU_Setup_dsPIC33EP64GP502.mpas,56 :: 		dsPIC33_CAN_SetFilter(1, $08000000, True);                                    // Look for a 1 in bit 27  (NMRABus Layer Message)
	MOV	#65535, W13
	MOV	#0, W11
	MOV	#2048, W12
	MOV.B	#1, W10
	CALL	_dsPIC33_CAN_SetFilter
;MCU_Setup_dsPIC33EP64GP502.mpas,57 :: 		dsPIC33_CAN_AssociateFilterWithMask(0, 2);                                    // Link Filter 0 and Mask 2 which looks only at bit 27 = 0
	MOV	#2, W11
	CLR	W10
	CALL	_dsPIC33_CAN_AssociateFilterWithMask
;MCU_Setup_dsPIC33EP64GP502.mpas,58 :: 		dsPIC33_CAN_AssociateFilterWithMask(1, 2);                                    // Link Filter 1 and Mask 2 which looks only at bit 27 = 1
	MOV	#2, W11
	MOV	#1, W10
	CALL	_dsPIC33_CAN_AssociateFilterWithMask
;MCU_Setup_dsPIC33EP64GP502.mpas,59 :: 		dsPIC33_CAN_EnableDisableRXFilters($0003);                                    // Enable Filters 0 and 1
	MOV	#3, W10
	CALL	_dsPIC33_CAN_EnableDisableRXFilters
;MCU_Setup_dsPIC33EP64GP502.mpas,60 :: 		dsPIC33_CAN_RegisterBufferWithFilter(0, CAN_RX_0_BUFFER);                     // Filter 0 to be sent to Buffer
	MOV	#2, W11
	CLR	W10
	CALL	_dsPIC33_CAN_RegisterBufferWithFilter
;MCU_Setup_dsPIC33EP64GP502.mpas,61 :: 		dsPIC33_CAN_RegisterBufferWithFilter(1, CAN_RX_0_BUFFER);                     // Filter 1 to be sent to Buffer
	MOV	#2, W11
	MOV	#1, W10
	CALL	_dsPIC33_CAN_RegisterBufferWithFilter
;MCU_Setup_dsPIC33EP64GP502.mpas,63 :: 		dsPIC33_CAN_EnterNormalMode;                                                  // Place the module in Normal Mode
	CALL	_dsPIC33_CAN_EnterNormalMode
;MCU_Setup_dsPIC33EP64GP502.mpas,66 :: 		dsPIC33_CAN_SetBufferAsTransmitter(CAN_TX_0_BUFFER, True);                    // Setup Buffer 0 as a Transmit Buffer
	MOV	#65535, W11
	CLR	W10
	CALL	_dsPIC33_CAN_SetBufferAsTransmitter
;MCU_Setup_dsPIC33EP64GP502.mpas,69 :: 		dsPIC33_DMA_Enable(CAN_TX_0_DMA_CHANNEL, False);
	CLR	W11
	MOV	#1, W10
	CALL	_dsPIC33_DMA_Enable
;MCU_Setup_dsPIC33EP64GP502.mpas,70 :: 		dsPIC33_DMA_DataSize(CAN_TX_0_DMA_CHANNEL, DATASIZE_WORD);                    // DMA Data Size is a Word
	MOV	#1, W11
	MOV	#1, W10
	CALL	_dsPIC33_DMA_DataSize
;MCU_Setup_dsPIC33EP64GP502.mpas,71 :: 		dsPIC33_DMA_Direction(CAN_TX_0_DMA_CHANNEL, DIRECTION_RAM_TO_PERIPHERAL);     // Transmit move data from RAM to the Module
	CLR	W11
	MOV	#1, W10
	CALL	_dsPIC33_DMA_Direction
;MCU_Setup_dsPIC33EP64GP502.mpas,72 :: 		dsPIC33_DMA_AddressMode(CAN_TX_0_DMA_CHANNEL, ADDRESS_MODE_PERIPHERAL_INDIRECT); // Don't use the buggy Perpherial Addressing Mode
	MOV	#2, W11
	MOV	#1, W10
	CALL	_dsPIC33_DMA_AddressMode
;MCU_Setup_dsPIC33EP64GP502.mpas,73 :: 		dsPIC33_DMA_OperatingMode(CAN_TX_0_DMA_CHANNEL, OPERATING_MODE_CONTINIOUS);   // Continious Mode (as apposed to one shot)
	CLR	W11
	MOV	#1, W10
	CALL	_dsPIC33_DMA_OperatingMode
;MCU_Setup_dsPIC33EP64GP502.mpas,74 :: 		dsPIC33_DMA_TransferCount(CAN_TX_0_DMA_CHANNEL, 8);                           // 0...7
	MOV	#8, W11
	MOV	#1, W10
	CALL	_dsPIC33_DMA_TransferCount
;MCU_Setup_dsPIC33EP64GP502.mpas,75 :: 		dsPIC33_DMA_ManualDMATransfer(CAN_TX_0_DMA_CHANNEL, False);                   // Automatic DMA Transfers
	CLR	W11
	MOV	#1, W10
	CALL	_dsPIC33_DMA_ManualDMATransfer
;MCU_Setup_dsPIC33EP64GP502.mpas,76 :: 		dsPIC33_DMA_PeripheralAddress(CAN_TX_0_DMA_CHANNEL, @C1TXD);                  // Assign the DMA Channel to the Transmit Register of the CAN module
	MOV	#lo_addr(C1TXD), W11
	MOV	#1, W10
	CALL	_dsPIC33_DMA_PeripheralAddress
;MCU_Setup_dsPIC33EP64GP502.mpas,77 :: 		dsPIC33_DMA_InterruptSelect(CAN_TX_0_DMA_CHANNEL, IRQ_ECAN1_TX_DATA_READY);   // Assign the DMA Channel to the ECAN 1 TX to Trigger the Transfer
	MOV	#70, W11
	MOV	#1, W10
	CALL	_dsPIC33_DMA_InterruptSelect
;MCU_Setup_dsPIC33EP64GP502.mpas,78 :: 		dsPIC33_DMA_AddressOffsetA(CAN_TX_0_DMA_CHANNEL, Word( @TX_Main_RawBufferArray[CAN_TX_0_BUFFER]));  // Enable DMA Channel
	MOV	#lo_addr(_TX_Main_RawBufferArray), W0
	MOV	W0, W11
	MOV	#1, W10
	CALL	_dsPIC33_DMA_AddressOffsetA
;MCU_Setup_dsPIC33EP64GP502.mpas,79 :: 		dsPIC33_DMA_Enable(CAN_TX_0_DMA_CHANNEL, True);
	MOV	#65535, W11
	MOV	#1, W10
	CALL	_dsPIC33_DMA_Enable
;MCU_Setup_dsPIC33EP64GP502.mpas,83 :: 		dsPIC33_CAN_SetBufferAsTransmitter(CAN_TX_1_BUFFER, True);                    // Setup Buffer 0 as a Transmit Buffer
	MOV	#65535, W11
	MOV	#1, W10
	CALL	_dsPIC33_CAN_SetBufferAsTransmitter
;MCU_Setup_dsPIC33EP64GP502.mpas,86 :: 		dsPIC33_DMA_Enable(CAN_TX_1_DMA_CHANNEL, False);
	CLR	W11
	CLR	W10
	CALL	_dsPIC33_DMA_Enable
;MCU_Setup_dsPIC33EP64GP502.mpas,87 :: 		dsPIC33_DMA_DataSize(CAN_TX_1_DMA_CHANNEL, DATASIZE_WORD);                    // DMA Data Size is a Word
	MOV	#1, W11
	CLR	W10
	CALL	_dsPIC33_DMA_DataSize
;MCU_Setup_dsPIC33EP64GP502.mpas,88 :: 		dsPIC33_DMA_Direction(CAN_TX_1_DMA_CHANNEL, DIRECTION_RAM_TO_PERIPHERAL);     // Transmit move data from RAM to the Module
	CLR	W11
	CLR	W10
	CALL	_dsPIC33_DMA_Direction
;MCU_Setup_dsPIC33EP64GP502.mpas,89 :: 		dsPIC33_DMA_AddressMode(CAN_TX_1_DMA_CHANNEL, ADDRESS_MODE_PERIPHERAL_INDIRECT); // Don't use the buggy Perpherial Addressing Mode
	MOV	#2, W11
	CLR	W10
	CALL	_dsPIC33_DMA_AddressMode
;MCU_Setup_dsPIC33EP64GP502.mpas,90 :: 		dsPIC33_DMA_OperatingMode(CAN_TX_1_DMA_CHANNEL, OPERATING_MODE_CONTINIOUS);   // Continious Mode (as apposed to one shot)
	CLR	W11
	CLR	W10
	CALL	_dsPIC33_DMA_OperatingMode
;MCU_Setup_dsPIC33EP64GP502.mpas,91 :: 		dsPIC33_DMA_TransferCount(CAN_TX_1_DMA_CHANNEL, 8);                           // 0...7
	MOV	#8, W11
	CLR	W10
	CALL	_dsPIC33_DMA_TransferCount
;MCU_Setup_dsPIC33EP64GP502.mpas,92 :: 		dsPIC33_DMA_ManualDMATransfer(CAN_TX_1_DMA_CHANNEL, False);                   // Automatic DMA Transfers
	CLR	W11
	CLR	W10
	CALL	_dsPIC33_DMA_ManualDMATransfer
;MCU_Setup_dsPIC33EP64GP502.mpas,93 :: 		dsPIC33_DMA_PeripheralAddress(CAN_TX_1_DMA_CHANNEL, @C1TXD);                  // Assign the DMA Channel to the Transmit Register of the CAN module
	MOV	#lo_addr(C1TXD), W11
	CLR	W10
	CALL	_dsPIC33_DMA_PeripheralAddress
;MCU_Setup_dsPIC33EP64GP502.mpas,94 :: 		dsPIC33_DMA_InterruptSelect(CAN_TX_1_DMA_CHANNEL, IRQ_ECAN1_TX_DATA_READY);   // Assign the DMA Channel to the ECAN 1 TX to Trigger the Transfer
	MOV	#70, W11
	CLR	W10
	CALL	_dsPIC33_DMA_InterruptSelect
;MCU_Setup_dsPIC33EP64GP502.mpas,95 :: 		dsPIC33_DMA_AddressOffsetA(CAN_TX_1_DMA_CHANNEL, Word( @TX_Main_RawBufferArray[CAN_TX_1_BUFFER]));  // Enable DMA Channel
	MOV	#lo_addr(_TX_Main_RawBufferArray+16), W0
	MOV	W0, W11
	CLR	W10
	CALL	_dsPIC33_DMA_AddressOffsetA
;MCU_Setup_dsPIC33EP64GP502.mpas,96 :: 		dsPIC33_DMA_Enable(CAN_TX_1_DMA_CHANNEL, True);
	MOV	#65535, W11
	CLR	W10
	CALL	_dsPIC33_DMA_Enable
;MCU_Setup_dsPIC33EP64GP502.mpas,99 :: 		dsPIC33_CAN_SetBufferAsTransmitter(CAN_RX_0_BUFFER, False);                     // Setup Buffer 0 as a Receive Buffer
	CLR	W11
	MOV	#2, W10
	CALL	_dsPIC33_CAN_SetBufferAsTransmitter
;MCU_Setup_dsPIC33EP64GP502.mpas,102 :: 		dsPIC33_DMA_DataSize(CAN_RX_0_DMA_CHANNEL, DATASIZE_WORD);                                       // DMA Data Size is a Word
	MOV	#1, W11
	MOV	#2, W10
	CALL	_dsPIC33_DMA_DataSize
;MCU_Setup_dsPIC33EP64GP502.mpas,103 :: 		dsPIC33_DMA_Direction(CAN_RX_0_DMA_CHANNEL, DIRECTION_PERIPHERAL_TO_RAM);                        // Transmit move data from the Module to RAM
	MOV	#1, W11
	MOV	#2, W10
	CALL	_dsPIC33_DMA_Direction
;MCU_Setup_dsPIC33EP64GP502.mpas,104 :: 		dsPIC33_DMA_AddressMode(CAN_RX_0_DMA_CHANNEL, ADDRESS_MODE_REG_INDIRECT_POST_INCREMENT);         // Don't use the buggy Perpherial Addressing Mode
	CLR	W11
	MOV	#2, W10
	CALL	_dsPIC33_DMA_AddressMode
;MCU_Setup_dsPIC33EP64GP502.mpas,105 :: 		dsPIC33_DMA_OperatingMode(CAN_RX_0_DMA_CHANNEL, OPERATING_MODE_CONTINIOUS);                      // Continious Mode (as apposed to one shot)
	CLR	W11
	MOV	#2, W10
	CALL	_dsPIC33_DMA_OperatingMode
;MCU_Setup_dsPIC33EP64GP502.mpas,106 :: 		dsPIC33_DMA_TransferCount(CAN_RX_0_DMA_CHANNEL, 8);                                              // Transfers 8 Words (0 counts as 1)
	MOV	#8, W11
	MOV	#2, W10
	CALL	_dsPIC33_DMA_TransferCount
;MCU_Setup_dsPIC33EP64GP502.mpas,107 :: 		dsPIC33_DMA_ManualDMATransfer(CAN_RX_0_DMA_CHANNEL, False);                                      // Automatic DMA Transfers
	CLR	W11
	MOV	#2, W10
	CALL	_dsPIC33_DMA_ManualDMATransfer
;MCU_Setup_dsPIC33EP64GP502.mpas,108 :: 		dsPIC33_DMA_PeripheralAddress(CAN_RX_0_DMA_CHANNEL, @C1RXD);                                     // Assign the DMA Channel to the Receive Register of the CAN module
	MOV	#lo_addr(C1RXD), W11
	MOV	#2, W10
	CALL	_dsPIC33_DMA_PeripheralAddress
;MCU_Setup_dsPIC33EP64GP502.mpas,109 :: 		dsPIC33_DMA_InterruptSelect(CAN_RX_0_DMA_CHANNEL, IRQ_ECAN1_RX_DATA_READY);                      // Assign the DMA Channel to the ECAN 1 RX to Trigger the Transfer
	MOV	#34, W11
	MOV	#2, W10
	CALL	_dsPIC33_DMA_InterruptSelect
;MCU_Setup_dsPIC33EP64GP502.mpas,110 :: 		dsPIC33_DMA_AddressOffsetA(CAN_RX_0_DMA_CHANNEL, @RX_Main_RawBufferArray[0]);                    // Point the Receive Buffer Offset into the CAN Layer Buffer
	MOV	#lo_addr(_RX_Main_RawBufferArray), W11
	MOV	#2, W10
	CALL	_dsPIC33_DMA_AddressOffsetA
;MCU_Setup_dsPIC33EP64GP502.mpas,111 :: 		dsPIC33_DMA_Enable(CAN_RX_0_DMA_CHANNEL, True);                                                  // Enable DMA Channel 2
	MOV	#65535, W11
	MOV	#2, W10
	CALL	_dsPIC33_DMA_Enable
;MCU_Setup_dsPIC33EP64GP502.mpas,129 :: 		dsPIC33_CAN_InterruptFlagRXBufferOverflow(True);                              // Clear the flag
	MOV	#65535, W10
	CALL	_dsPIC33_CAN_InterruptFlagRXBufferOverflow
;MCU_Setup_dsPIC33EP64GP502.mpas,130 :: 		dsPIC33_CAN_InterruptFlagRXBuffer(True);                                      // RX Interrupt Flag Reset
	MOV	#65535, W10
	CALL	_dsPIC33_CAN_InterruptFlagRXBuffer
;MCU_Setup_dsPIC33EP64GP502.mpas,131 :: 		dsPIC33_CAN_InterruptFlagTXBuffer(True);                                      // TX Interrupt Flag Reset
	MOV	#65535, W10
	CALL	_dsPIC33_CAN_InterruptFlagTXBuffer
;MCU_Setup_dsPIC33EP64GP502.mpas,132 :: 		dsPIC33_CAN_RXBufferOverflowInterrupt(True);                                  // If we don't enable this and an interrupt occurs then it hangs the loop because you can't clear the Rx Interrupt until this is serviced
	MOV	#65535, W10
	CALL	_dsPIC33_CAN_RXBufferOverflowInterrupt
;MCU_Setup_dsPIC33EP64GP502.mpas,133 :: 		dsPIC33_CAN_TXBufferInterrupt(True);                                          // Enable the TX Done Event Interrupt
	MOV	#65535, W10
	CALL	_dsPIC33_CAN_TXBufferInterrupt
;MCU_Setup_dsPIC33EP64GP502.mpas,134 :: 		dsPIC33_CAN_RXBufferInterrupt(True);                                          // Enable the RX Done Event Interrupt
	MOV	#65535, W10
	CALL	_dsPIC33_CAN_RXBufferInterrupt
;MCU_Setup_dsPIC33EP64GP502.mpas,136 :: 		dsPIC33_CAN_GlobalInterruptCAN_EventPriority(6);                              // CAN Event Interrupt has a priority of 6 out of 7
	MOV	#6, W10
	CALL	_dsPIC33_CAN_GlobalInterruptCAN_EventPriority
;MCU_Setup_dsPIC33EP64GP502.mpas,137 :: 		dsPIC33_CAN_GlobalInterruptCAN_Event(True);                                   // Enable the CAN Event Interrupt
	MOV	#65535, W10
	CALL	_dsPIC33_CAN_GlobalInterruptCAN_Event
;MCU_Setup_dsPIC33EP64GP502.mpas,139 :: 		Unlock_IOLOCK;
	CALL	_Unlock_IOLOCK
;MCU_Setup_dsPIC33EP64GP502.mpas,141 :: 		PPS_Mapping_NoLock(44, _INPUT, _U1RX);                                        // Set RPI44 to the UART Receive
	MOV.B	#14, W12
	MOV.B	#1, W11
	MOV.B	#44, W10
	CALL	_PPS_Mapping_NoLock
;MCU_Setup_dsPIC33EP64GP502.mpas,142 :: 		PPS_Mapping_NoLock(42, _OUTPUT, _U1TX);                                       // Set RP42 to the UART Transmit
	MOV.B	#1, W12
	CLR	W11
	MOV.B	#42, W10
	CALL	_PPS_Mapping_NoLock
;MCU_Setup_dsPIC33EP64GP502.mpas,144 :: 		PPS_Mapping_NoLock(45, _INPUT, _C1RX);                                        // Set RPI45 to the CAN Receive
	MOV.B	#19, W12
	MOV.B	#1, W11
	MOV.B	#45, W10
	CALL	_PPS_Mapping_NoLock
;MCU_Setup_dsPIC33EP64GP502.mpas,145 :: 		PPS_Mapping_NoLock(43, _OUTPUT, _C1TX);                                       // Set RP43 to the CAN Transmit
	MOV.B	#14, W12
	CLR	W11
	MOV.B	#43, W10
	CALL	_PPS_Mapping_NoLock
;MCU_Setup_dsPIC33EP64GP502.mpas,146 :: 		Lock_IOLOCK;
	CALL	_Lock_IOLOCK
;MCU_Setup_dsPIC33EP64GP502.mpas,152 :: 		UART1_Init(230400);                       // Initialize UART module a
	MOV	#33792, W10
	MOV	#3, W11
	CALL	_UART1_Init
;MCU_Setup_dsPIC33EP64GP502.mpas,153 :: 		Delay_ms(100);                            // Wait for UART module to stabilize
	MOV	#10, W8
	MOV	#50181, W7
L__MCU_Setup_Initialize13:
	DEC	W7
	BRA NZ	L__MCU_Setup_Initialize13
	DEC	W8
	BRA NZ	L__MCU_Setup_Initialize13
	NOP
;MCU_Setup_dsPIC33EP64GP502.mpas,156 :: 		SPI1_Init();       // Initialize SPI1 module
	CALL	_SPI1_Init
;MCU_Setup_dsPIC33EP64GP502.mpas,157 :: 		SPIEN_bit := 0;    // Disable SPI
	BCLR	SPIEN_bit, BitPos(SPIEN_bit+0)
;MCU_Setup_dsPIC33EP64GP502.mpas,158 :: 		SPI1CON := SPI1CON and $FFE0;  // Clear the prescaler bits
	MOV	#65504, W1
	MOV	#lo_addr(SPI1CON), W0
	AND	W1, [W0], [W0]
;MCU_Setup_dsPIC33EP64GP502.mpas,159 :: 		SPI1CON := SPI1CON or $0003 or $0018;  // Setup for 5 Mhz (with the CAN plug in boards)     $10=5Mhz, $14=6.67Mhz, $18 = 10Mhz
	MOV	SPI1CON, WREG
	IOR	W0, #3, W1
	MOV	#lo_addr(SPI1CON), W0
	IOR	W1, #24, [W0]
;MCU_Setup_dsPIC33EP64GP502.mpas,160 :: 		SPIEN_bit := 1;    // Enable the SPI
	BSET	SPIEN_bit, BitPos(SPIEN_bit+0)
;MCU_Setup_dsPIC33EP64GP502.mpas,162 :: 		TCS_T2CON_bit := 0;       // internal cycle clock
	BCLR	TCS_T2CON_bit, BitPos(TCS_T2CON_bit+0)
;MCU_Setup_dsPIC33EP64GP502.mpas,163 :: 		T2IP_0_bit := 0;          // Timer 2 Interrupt Priority = 2   (1 means off)
	BCLR	T2IP_0_bit, BitPos(T2IP_0_bit+0)
;MCU_Setup_dsPIC33EP64GP502.mpas,164 :: 		T2IP_1_bit := 1;
	BSET	T2IP_1_bit, BitPos(T2IP_1_bit+0)
;MCU_Setup_dsPIC33EP64GP502.mpas,165 :: 		T2IP_2_bit := 0;
	BCLR	T2IP_2_bit, BitPos(T2IP_2_bit+0)
;MCU_Setup_dsPIC33EP64GP502.mpas,166 :: 		TCKPS_0_T2CON_bit := 1;   // 256 Prescaler
	BSET	TCKPS_0_T2CON_bit, BitPos(TCKPS_0_T2CON_bit+0)
;MCU_Setup_dsPIC33EP64GP502.mpas,167 :: 		TCKPS_1_T2CON_bit := 1;
	BSET	TCKPS_1_T2CON_bit, BitPos(TCKPS_1_T2CON_bit+0)
;MCU_Setup_dsPIC33EP64GP502.mpas,168 :: 		PR2 := 12500;             // Clock ticks every 31.25ns * 256 * 12500 = 100ms interrupts
	MOV	#12500, W0
	MOV	WREG, PR2
;MCU_Setup_dsPIC33EP64GP502.mpas,169 :: 		T2IF_bit := 0;            // Clear T2IF
	BCLR	T2IF_bit, BitPos(T2IF_bit+0)
;MCU_Setup_dsPIC33EP64GP502.mpas,170 :: 		T2IE_bit := 1;            // Enable the Interrupt
	BSET	T2IE_bit, BitPos(T2IE_bit+0)
;MCU_Setup_dsPIC33EP64GP502.mpas,171 :: 		TON_T2CON_bit := 1;       // Turn on
	BSET	TON_T2CON_bit, BitPos(TON_T2CON_bit+0)
;MCU_Setup_dsPIC33EP64GP502.mpas,174 :: 		TCS_T1CON_bit := 0;       // internal cycle clock
	BCLR	TCS_T1CON_bit, BitPos(TCS_T1CON_bit+0)
;MCU_Setup_dsPIC33EP64GP502.mpas,175 :: 		T1IP_0_bit := 1;          // Timer 1 Interrupt Priority = 7   (1 means off)
	BSET	T1IP_0_bit, BitPos(T1IP_0_bit+0)
;MCU_Setup_dsPIC33EP64GP502.mpas,176 :: 		T1IP_1_bit := 1;
	BSET	T1IP_1_bit, BitPos(T1IP_1_bit+0)
;MCU_Setup_dsPIC33EP64GP502.mpas,177 :: 		T1IP_2_bit := 1;
	BSET	T1IP_2_bit, BitPos(T1IP_2_bit+0)
;MCU_Setup_dsPIC33EP64GP502.mpas,178 :: 		PR1 := 1792;             // Clock ticks every 31.25ns * 1792 = 56us interrupts
	MOV	#1792, W0
	MOV	WREG, PR1
;MCU_Setup_dsPIC33EP64GP502.mpas,179 :: 		T1IF_bit := 0;           // Clear T1IF
	BCLR	T1IF_bit, BitPos(T1IF_bit+0)
;MCU_Setup_dsPIC33EP64GP502.mpas,180 :: 		T1IE_bit := 1;           // Enable the Interrupt
	BSET	T1IE_bit, BitPos(T1IE_bit+0)
;MCU_Setup_dsPIC33EP64GP502.mpas,183 :: 		end;
L_end_MCU_Setup_Initialize:
	POP	W13
	POP	W12
	POP	W11
	POP	W10
	RETURN
; end of _MCU_Setup_Initialize
