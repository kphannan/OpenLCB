
_25AAxxxx_LocalSPI_Read:

;_25AAxxxx.mpas,117 :: 		begin
;_25AAxxxx.mpas,121 :: 		Result := SPI1BUF;       // Force the SPIRBF flag to clear
; Result start address is: 0 (W0)
	MOV.B	SPI1BUF, WREG
; Result end address is: 0 (W0)
;_25AAxxxx.mpas,123 :: 		SPI1BUF := Dummy;   // This will set the SPITBF flag until the byte is written into the output latch to be sent.
	CLR	SPI1BUF
;_25AAxxxx.mpas,124 :: 		while SPIRBF_bit = 0 do;
L__25AAxxxx_LocalSPI_Read2:
	BTSS	SPIRBF_bit, BitPos(SPIRBF_bit+0)
	GOTO	L__25AAxxxx_LocalSPI_Read2
;_25AAxxxx.mpas,125 :: 		Result := SPI1BUF;
; Result start address is: 2 (W1)
	MOV	SPI1BUF, W1
;_25AAxxxx.mpas,129 :: 		end;
	MOV.B	W1, W0
; Result end address is: 2 (W1)
L_end_LocalSPI_Read:
	RETURN
; end of _25AAxxxx_LocalSPI_Read

_25AAxxxx_LocalSPI_Write:

;_25AAxxxx.mpas,132 :: 		begin
;_25AAxxxx.mpas,136 :: 		Result := SPI1BUF;       // Force the SPIRBF flag to clear
; Result start address is: 0 (W0)
	MOV.B	SPI1BUF, WREG
; Result end address is: 0 (W0)
;_25AAxxxx.mpas,137 :: 		nop;
	NOP
;_25AAxxxx.mpas,138 :: 		SPI1BUF := DataByte;   // This will set the SPITBF flag until the byte is written into the output latch to be sent.
	ZE	W10, W0
	MOV	WREG, SPI1BUF
;_25AAxxxx.mpas,139 :: 		while SPIRBF_bit = 0 do;
L__25AAxxxx_LocalSPI_Write8:
	BTSS	SPIRBF_bit, BitPos(SPIRBF_bit+0)
	GOTO	L__25AAxxxx_LocalSPI_Write8
;_25AAxxxx.mpas,140 :: 		Result := SPI1BUF;
; Result start address is: 2 (W1)
	MOV	SPI1BUF, W1
;_25AAxxxx.mpas,145 :: 		end;
	MOV.B	W1, W0
; Result end address is: 2 (W1)
L_end_LocalSPI_Write:
	RETURN
; end of _25AAxxxx_LocalSPI_Write

_25AAxxxx_SetBank_CS:

;_25AAxxxx.mpas,148 :: 		begin
;_25AAxxxx.mpas,150 :: 		if Bank = EEPROM_BANK_0 then CS_Bank_0 := CS_State {$ENDIF}
	CP	W10, #0
	BRA Z	L__25AAxxxx_SetBank_CS42
	GOTO	L__25AAxxxx_SetBank_CS14
L__25AAxxxx_SetBank_CS42:
	BTSS	W11, #0
	BCLR	CS_Bank_0, BitPos(CS_Bank_0+0)
	BTSC	W11, #0
	BSET	CS_Bank_0, BitPos(CS_Bank_0+0)
L__25AAxxxx_SetBank_CS14:
;_25AAxxxx.mpas,161 :: 		end;
L_end_SetBank_CS:
	RETURN
; end of _25AAxxxx_SetBank_CS

_25AAxxxx_WriteAddress:

;_25AAxxxx.mpas,164 :: 		begin
;_25AAxxxx.mpas,165 :: 		LocalSPI_Write(Address shr 16);
	MOV.D	W10, W0
	MOV	W1, W0
	CLR	W1
	PUSH.D	W10
	MOV.B	W0, W10
	CALL	_25AAxxxx_LocalSPI_Write
	POP.D	W10
;_25AAxxxx.mpas,166 :: 		LocalSPI_Write(Address shr 8);
	MOV	#8, W2
	MOV.D	W10, W0
L__25AAxxxx_WriteAddress44:
	DEC	W2, W2
	BRA LT	L__25AAxxxx_WriteAddress45
	LSR	W1, W1
	RRC	W0, W0
	BRA	L__25AAxxxx_WriteAddress44
L__25AAxxxx_WriteAddress45:
	PUSH.D	W10
	MOV.B	W0, W10
	CALL	_25AAxxxx_LocalSPI_Write
	POP.D	W10
;_25AAxxxx.mpas,167 :: 		LocalSPI_Write(Address);
	CALL	_25AAxxxx_LocalSPI_Write
;_25AAxxxx.mpas,168 :: 		end;
L_end_WriteAddress:
	RETURN
; end of _25AAxxxx_WriteAddress

__25AAxxxx_Read:
	LNK	#0

;_25AAxxxx.mpas,187 :: 		begin
;_25AAxxxx.mpas,188 :: 		SetBank_CS(Bank, 0);
	PUSH	W11
	PUSH	W12
; Buffer start address is: 6 (W3)
	MOV	[W14-8], W3
	PUSH	W11
	PUSH	W12
	CLR	W11
	CALL	_25AAxxxx_SetBank_CS
	POP	W12
	POP	W11
;_25AAxxxx.mpas,189 :: 		LocalSPI_Write($03);           // Read Instruction
	PUSH	W10
	MOV.B	#3, W10
	CALL	_25AAxxxx_LocalSPI_Write
;_25AAxxxx.mpas,190 :: 		WriteAddress(Address);
	MOV	W11, W10
	MOV	W12, W11
	CALL	_25AAxxxx_WriteAddress
	POP	W10
;_25AAxxxx.mpas,191 :: 		i := 0;
; i start address is: 0 (W0)
	CLR	W0
; Buffer end address is: 6 (W3)
; i end address is: 0 (W0)
	MOV	W3, W2
	MOV	W0, W1
;_25AAxxxx.mpas,192 :: 		while i < Count do
L___25AAxxxx_Read19:
; i start address is: 2 (W1)
; Buffer start address is: 4 (W2)
	CP	W1, W13
	BRA LT	L___25AAxxxx_Read47
	GOTO	L___25AAxxxx_Read20
L___25AAxxxx_Read47:
;_25AAxxxx.mpas,197 :: 		SPI1BUF := Dummy;          // This will set the SPITBF flag until the byte is written into the output latch to be sent.
	CLR	SPI1BUF
;_25AAxxxx.mpas,198 :: 		while SPIRBF_bit = 0 do;
	GOTO	L___25AAxxxx_Read24
L___25AAxxxx_Read38:
L___25AAxxxx_Read24:
; Buffer start address is: 4 (W2)
; Buffer end address is: 4 (W2)
; i start address is: 2 (W1)
; i end address is: 2 (W1)
	BTSS	SPIRBF_bit, BitPos(SPIRBF_bit+0)
	GOTO	L___25AAxxxx_Read38
; Buffer end address is: 4 (W2)
; i end address is: 2 (W1)
;_25AAxxxx.mpas,199 :: 		Buffer^ := SPI1BUF;
; i start address is: 2 (W1)
; Buffer start address is: 4 (W2)
	MOV.B	SPI1BUF, WREG
	MOV.B	W0, [W2]
;_25AAxxxx.mpas,203 :: 		Inc(Buffer);
; Buffer start address is: 0 (W0)
	ADD	W2, #1, W0
; Buffer end address is: 4 (W2)
;_25AAxxxx.mpas,204 :: 		Inc(i);
	INC	W1
;_25AAxxxx.mpas,205 :: 		end;
	MOV	W0, W2
; Buffer end address is: 0 (W0)
; i end address is: 2 (W1)
	GOTO	L___25AAxxxx_Read19
L___25AAxxxx_Read20:
;_25AAxxxx.mpas,206 :: 		SetBank_CS(Bank, 1);
	PUSH	W11
	PUSH	W12
	MOV	#1, W11
	CALL	_25AAxxxx_SetBank_CS
	POP	W12
	POP	W11
;_25AAxxxx.mpas,207 :: 		end;
L_end__25AAxxxx_Read:
	POP	W12
	POP	W11
	ULNK
	RETURN
; end of __25AAxxxx_Read

__25AAxxxx_Write:
	LNK	#0

;_25AAxxxx.mpas,225 :: 		begin
;_25AAxxxx.mpas,226 :: 		SetBank_CS(Bank, 0);
	PUSH	W11
	PUSH	W12
; Buffer start address is: 6 (W3)
	MOV	[W14-8], W3
	PUSH	W11
	PUSH	W12
	CLR	W11
	CALL	_25AAxxxx_SetBank_CS
	POP	W12
	POP	W11
;_25AAxxxx.mpas,227 :: 		LocalSPI_Write($06);           //  WREN (Write Enable) Instruction
	PUSH	W10
	MOV.B	#6, W10
	CALL	_25AAxxxx_LocalSPI_Write
	POP	W10
;_25AAxxxx.mpas,228 :: 		SetBank_CS(Bank, 1);
	PUSH	W11
	PUSH	W12
	MOV	#1, W11
	CALL	_25AAxxxx_SetBank_CS
;_25AAxxxx.mpas,229 :: 		SetBank_CS(Bank, 0);
	CLR	W11
	CALL	_25AAxxxx_SetBank_CS
	POP	W12
	POP	W11
;_25AAxxxx.mpas,230 :: 		LocalSPI_Write($02);           // Write Instruction
	PUSH	W10
	MOV.B	#2, W10
	CALL	_25AAxxxx_LocalSPI_Write
;_25AAxxxx.mpas,231 :: 		WriteAddress(Address);         // Can not check for Write Enabled in the Status here!
	MOV	W11, W10
	MOV	W12, W11
	CALL	_25AAxxxx_WriteAddress
	POP	W10
;_25AAxxxx.mpas,232 :: 		i := 0;
; i start address is: 4 (W2)
	CLR	W2
; Buffer end address is: 6 (W3)
; i end address is: 4 (W2)
;_25AAxxxx.mpas,233 :: 		while i < Count do
L___25AAxxxx_Write30:
; i start address is: 4 (W2)
; Buffer start address is: 6 (W3)
	CP	W2, W13
	BRA LT	L___25AAxxxx_Write49
	GOTO	L___25AAxxxx_Write31
L___25AAxxxx_Write49:
;_25AAxxxx.mpas,235 :: 		LocalSPI_Write(Buffer^);
	PUSH	W10
	MOV.B	[W3], W10
	CALL	_25AAxxxx_LocalSPI_Write
	POP	W10
;_25AAxxxx.mpas,236 :: 		Inc(Buffer);
; Buffer start address is: 0 (W0)
	ADD	W3, #1, W0
; Buffer end address is: 6 (W3)
;_25AAxxxx.mpas,237 :: 		Inc(i);
	INC	W2
;_25AAxxxx.mpas,238 :: 		end;
	MOV	W0, W3
; Buffer end address is: 0 (W0)
; i end address is: 4 (W2)
	GOTO	L___25AAxxxx_Write30
L___25AAxxxx_Write31:
;_25AAxxxx.mpas,239 :: 		SetBank_CS(Bank, 1);
	PUSH	W11
	PUSH	W12
	MOV	#1, W11
	CALL	_25AAxxxx_SetBank_CS
	POP	W12
	POP	W11
;_25AAxxxx.mpas,240 :: 		end;
L_end__25AAxxxx_Write:
	POP	W12
	POP	W11
	ULNK
	RETURN
; end of __25AAxxxx_Write

__25AAxxxx_Busy:

;_25AAxxxx.mpas,253 :: 		begin
;_25AAxxxx.mpas,254 :: 		SetBank_CS(Bank, 0);
	PUSH	W11
	CLR	W11
	CALL	_25AAxxxx_SetBank_CS
;_25AAxxxx.mpas,255 :: 		LocalSPI_Write($05);
	PUSH	W10
	MOV.B	#5, W10
	CALL	_25AAxxxx_LocalSPI_Write
	POP	W10
;_25AAxxxx.mpas,256 :: 		Result := LocalSPI_Read and $01 <> 0;
	CALL	_25AAxxxx_LocalSPI_Read
	ZE	W0, W0
	AND	W0, #1, W0
; Result start address is: 2 (W1)
	CP	W0, #0
	CLR	W1
	BRA Z	L___25AAxxxx_Busy51
	COM	W1
L___25AAxxxx_Busy51:
;_25AAxxxx.mpas,257 :: 		SetBank_CS(Bank, 1);
	MOV	#1, W11
	CALL	_25AAxxxx_SetBank_CS
;_25AAxxxx.mpas,258 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end__25AAxxxx_Busy:
	POP	W11
	RETURN
; end of __25AAxxxx_Busy

__25AAxxxx_WriteEnabled:

;_25AAxxxx.mpas,272 :: 		begin
;_25AAxxxx.mpas,273 :: 		SetBank_CS(Bank, 0);
	PUSH	W10
	PUSH	W11
	CLR	W11
	CALL	_25AAxxxx_SetBank_CS
;_25AAxxxx.mpas,274 :: 		LocalSPI_Write($05);
	MOV.B	#5, W10
	CALL	_25AAxxxx_LocalSPI_Write
;_25AAxxxx.mpas,275 :: 		Result := LocalSPI_Read and $02 <> 0
	CALL	_25AAxxxx_LocalSPI_Read
	ZE	W0, W0
	AND	W0, #2, W0
; Result start address is: 2 (W1)
	CP	W0, #0
	CLR	W1
	BRA Z	L___25AAxxxx_WriteEnabled53
	COM	W1
L___25AAxxxx_WriteEnabled53:
;_25AAxxxx.mpas,276 :: 		end;
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end__25AAxxxx_WriteEnabled:
	POP	W11
	POP	W10
	RETURN
; end of __25AAxxxx_WriteEnabled

__25AAxxxx_Erase:

;_25AAxxxx.mpas,279 :: 		begin
;_25AAxxxx.mpas,280 :: 		SetBank_CS(Bank, 0);
	PUSH	W11
	CLR	W11
	CALL	_25AAxxxx_SetBank_CS
;_25AAxxxx.mpas,281 :: 		LocalSPI_Write($06);           //  WREN (Write Enable) Instruction
	PUSH	W10
	MOV.B	#6, W10
	CALL	_25AAxxxx_LocalSPI_Write
	POP	W10
;_25AAxxxx.mpas,282 :: 		SetBank_CS(Bank, 1);
	MOV	#1, W11
	CALL	_25AAxxxx_SetBank_CS
;_25AAxxxx.mpas,283 :: 		SetBank_CS(Bank, 0);
	CLR	W11
	CALL	_25AAxxxx_SetBank_CS
;_25AAxxxx.mpas,284 :: 		LocalSPI_Write($C7);           // Chip Erase
	PUSH	W10
	MOV.B	#199, W10
	CALL	_25AAxxxx_LocalSPI_Write
	POP	W10
;_25AAxxxx.mpas,285 :: 		SetBank_CS(Bank, 1);
	MOV	#1, W11
	CALL	_25AAxxxx_SetBank_CS
;_25AAxxxx.mpas,286 :: 		end;
L_end__25AAxxxx_Erase:
	POP	W11
	RETURN
; end of __25AAxxxx_Erase

__25AAxxxx_Initialize:

;_25AAxxxx.mpas,289 :: 		begin
;_25AAxxxx.mpas,290 :: 		{$IFDEF EEPROM_ONE_BANK}CS_Bank_0 := 1; {$ENDIF}   // Output
	BSET	CS_Bank_0, BitPos(CS_Bank_0+0)
;_25AAxxxx.mpas,294 :: 		{$IFDEF EEPROM_ONE_BANK}CS_Bank_0_Direction := 0; {$ENDIF}   // Output
	BCLR	CS_Bank_0_Direction, BitPos(CS_Bank_0_Direction+0)
;_25AAxxxx.mpas,298 :: 		EE_PROM_Hold := 1; // No Hold
	BSET	EE_PROM_Hold, BitPos(EE_PROM_Hold+0)
;_25AAxxxx.mpas,299 :: 		EEPROM_Hold_Direction := 0 // Output
	BCLR	EEPROM_Hold_Direction, BitPos(EEPROM_Hold_Direction+0)
;_25AAxxxx.mpas,301 :: 		end;
L_end__25AAxxxx_Initialize:
	RETURN
; end of __25AAxxxx_Initialize
