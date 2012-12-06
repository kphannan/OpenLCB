
_Float16ToInt:

;Float16.mpas,53 :: 		begin
;Float16.mpas,54 :: 		Exponent := (Half and $7C00) shr 10;
	MOV	#31744, W0
	AND	W10, W0, W0
	LSR	W0, #10, W0
;Float16.mpas,55 :: 		LeftShiftCount := Exponent-15-10;                                              // 15 is offset in coding; 10 is to get a "1" value in LSB
	SUB	W0, #15, W0
	SUB	W0, #10, W2
; LeftShiftCount start address is: 6 (W3)
	MOV	W2, W3
;Float16.mpas,56 :: 		Fraction := ((Half and $03FF) or $0400);                                       // mask off sign, exponent; add implied 1 MSB.
	MOV	#1023, W0
	AND	W10, W0, W1
	MOV	#1024, W0
; Fraction start address is: 2 (W1)
	IOR	W1, W0, W1
;Float16.mpas,58 :: 		if (LeftShiftCount >= 0) then
	CP	W2, #0
	BRA GE	L__Float16ToInt107
	GOTO	L__Float16ToInt2
L__Float16ToInt107:
;Float16.mpas,59 :: 		Result := Fraction shl LeftShiftCount // final shift into place
	SL	W1, W3, W0
; LeftShiftCount end address is: 6 (W3)
; Fraction end address is: 2 (W1)
; Result start address is: 2 (W1)
	MOV	W0, W1
; Result end address is: 2 (W1)
	GOTO	L__Float16ToInt3
;Float16.mpas,60 :: 		else
L__Float16ToInt2:
;Float16.mpas,61 :: 		Result := Fraction shr (-LeftShiftCount); // final shift into place
; Fraction start address is: 2 (W1)
; LeftShiftCount start address is: 6 (W3)
	SUBR	W3, #0, W0
; LeftShiftCount end address is: 6 (W3)
	ASR	W1, W0, W0
; Fraction end address is: 2 (W1)
; Result start address is: 2 (W1)
	MOV	W0, W1
; Result end address is: 2 (W1)
L__Float16ToInt3:
;Float16.mpas,62 :: 		end;
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_Float16ToInt:
	RETURN
; end of _Float16ToInt

_HalfToFloat:
	LNK	#4

;Float16.mpas,68 :: 		begin
;Float16.mpas,70 :: 		Sign := Half shr 15;
	LSR	W10, #15, W0
; Sign start address is: 16 (W8)
	MOV	W0, W8
	CLR	W9
;Float16.mpas,71 :: 		Exp := (Half and $7C00) shr 10;
	MOV	#31744, W0
	AND	W10, W0, W0
	LSR	W0, #10, W0
	CLR	W1
	MOV	W0, [W14+0]
	MOV	W1, [W14+2]
;Float16.mpas,72 :: 		Mantissa := Half and 1023;
	MOV	#1023, W0
	AND	W10, W0, W0
; Mantissa start address is: 22 (W11)
	MOV	W0, W11
	CLR	W12
;Float16.mpas,74 :: 		if (Exp > 0) and (Exp < 31) then
	MOV	[W14+0], W0
	MOV	[W14+2], W1
	CP	W0, #0
	CPB	W1, #0
	CLR	W2
	BRA LE	L__HalfToFloat109
	COM	W2
L__HalfToFloat109:
	CP	W0, #31
	CPB	W1, #0
	CLR	W0
	BRA GE	L__HalfToFloat110
	COM	W0
L__HalfToFloat110:
	AND	W2, W0, W0
	BRA NZ	L__HalfToFloat111
	GOTO	L__HalfToFloat6
L__HalfToFloat111:
;Float16.mpas,77 :: 		Exp := Exp + (127 - 15);
	MOV	#112, W2
	MOV	#0, W3
	ADD	W14, #0, W1
	ADD	W14, #0, W0
	ADD	W2, [W1++], [W0++]
	ADDC	W3, [W1--], [W0--]
;Float16.mpas,78 :: 		Mantissa := Mantissa shl 13;
	MOV	#13, W0
	MOV	W11, W6
	MOV	W12, W7
L__HalfToFloat112:
	DEC	W0, W0
	BRA LT	L__HalfToFloat113
	SL	W6, W6
	RLC	W7, W7
	BRA	L__HalfToFloat112
L__HalfToFloat113:
; Mantissa end address is: 22 (W11)
;Float16.mpas,79 :: 		Dst := (Sign shl 31) or (DWord(Exp) shl 23) or Mantissa;
	MOV	#31, W0
	MOV.D	W8, W4
L__HalfToFloat114:
	DEC	W0, W0
	BRA LT	L__HalfToFloat115
	SL	W4, W4
	RLC	W5, W5
	BRA	L__HalfToFloat114
L__HalfToFloat115:
; Sign end address is: 16 (W8)
	MOV	#23, W3
	ADD	W14, #0, W2
	MOV.D	[W2], W0
L__HalfToFloat116:
	DEC	W3, W3
	BRA LT	L__HalfToFloat117
	SL	W0, W0
	RLC	W1, W1
	BRA	L__HalfToFloat116
L__HalfToFloat117:
	IOR	W4, W0, W0
	IOR	W5, W1, W1
; Dst start address is: 0 (W0)
	IOR	W0, W6, W0
	IOR	W1, W7, W1
;Float16.mpas,81 :: 		end
; Dst end address is: 0 (W0)
	GOTO	L__HalfToFloat7
;Float16.mpas,82 :: 		else if (Exp = 0) and (Mantissa = 0) then
L__HalfToFloat6:
; Sign start address is: 16 (W8)
; Mantissa start address is: 22 (W11)
	MOV	[W14+0], W0
	MOV	[W14+2], W1
	CP	W0, #0
	CPB	W1, #0
	CLR	W2
	BRA NZ	L__HalfToFloat118
	COM	W2
L__HalfToFloat118:
	CP	W11, #0
	CPB	W12, #0
	CLR	W0
	BRA NZ	L__HalfToFloat119
	COM	W0
L__HalfToFloat119:
	AND	W2, W0, W0
	CP0	W0
	BRA NZ	L__HalfToFloat120
	GOTO	L__HalfToFloat9
L__HalfToFloat120:
; Mantissa end address is: 22 (W11)
;Float16.mpas,85 :: 		Dst := Sign shl 31;
	MOV	#31, W0
; Dst start address is: 2 (W1)
	MOV	W8, W1
	MOV	W9, W2
L__HalfToFloat121:
	DEC	W0, W0
	BRA LT	L__HalfToFloat122
	SL	W1, W1
	RLC	W2, W2
	BRA	L__HalfToFloat121
L__HalfToFloat122:
; Sign end address is: 16 (W8)
;Float16.mpas,86 :: 		end
	MOV	W1, W0
	MOV	W2, W1
; Dst end address is: 2 (W1)
	GOTO	L__HalfToFloat10
;Float16.mpas,87 :: 		else if (Exp = 0) and (Mantissa <> 0) then
L__HalfToFloat9:
; Sign start address is: 16 (W8)
; Mantissa start address is: 22 (W11)
	MOV	[W14+0], W0
	MOV	[W14+2], W1
	CP	W0, #0
	CPB	W1, #0
	CLR	W2
	BRA NZ	L__HalfToFloat123
	COM	W2
L__HalfToFloat123:
	CP	W11, #0
	CPB	W12, #0
	CLR	W0
	BRA Z	L__HalfToFloat124
	COM	W0
L__HalfToFloat124:
	AND	W2, W0, W0
	CP0	W0
	BRA NZ	L__HalfToFloat125
	GOTO	L__HalfToFloat12
L__HalfToFloat125:
; Mantissa end address is: 22 (W11)
; Sign end address is: 16 (W8)
;Float16.mpas,90 :: 		while (Mantissa and $00000400) = 0 do
L__HalfToFloat15:
; Mantissa start address is: 22 (W11)
; Sign start address is: 16 (W8)
	MOV	#1024, W0
	MOV	#0, W1
	AND	W11, W0, W0
	AND	W12, W1, W1
	CP	W0, #0
	CPB	W1, #0
	BRA Z	L__HalfToFloat126
	GOTO	L__HalfToFloat16
L__HalfToFloat126:
;Float16.mpas,92 :: 		Mantissa := Mantissa shl 1;
; Mantissa start address is: 6 (W3)
	MOV	W11, W3
	MOV	W12, W4
	SL	W3, W3
	RLC	W4, W4
; Mantissa end address is: 22 (W11)
;Float16.mpas,93 :: 		Dec(Exp);
	MOV	[W14+0], W1
	MOV	[W14+2], W2
	ADD	W14, #0, W0
	SUB	W1, #1, [W0++]
	SUBB	W2, #0, [W0--]
;Float16.mpas,94 :: 		end;
; Mantissa end address is: 6 (W3)
	MOV	W3, W11
	MOV	W4, W12
	GOTO	L__HalfToFloat15
L__HalfToFloat16:
;Float16.mpas,95 :: 		Inc(Exp);
; Mantissa start address is: 22 (W11)
	MOV	[W14+0], W0
	MOV	[W14+2], W1
	ADD	W0, #1, W5
	ADDC	W1, #0, W6
;Float16.mpas,96 :: 		Mantissa := Mantissa and not $00000400;
	MOV	#64511, W0
	MOV	#0, W1
	AND	W11, W0, W3
	AND	W12, W1, W4
; Mantissa end address is: 22 (W11)
;Float16.mpas,98 :: 		Exp := Exp + (127 - 15);
	MOV	#112, W1
	MOV	#0, W2
	ADD	W14, #0, W0
	ADD	W5, W1, [W0++]
	ADDC	W6, W2, [W0--]
;Float16.mpas,99 :: 		Mantissa := Mantissa shl 13;
	MOV	#13, W0
	MOV	W3, W6
	MOV	W4, W7
L__HalfToFloat127:
	DEC	W0, W0
	BRA LT	L__HalfToFloat128
	SL	W6, W6
	RLC	W7, W7
	BRA	L__HalfToFloat127
L__HalfToFloat128:
;Float16.mpas,100 :: 		Dst := (Sign shl 31) or (DWord(Exp) shl 23) or Mantissa;
	MOV	#31, W0
	MOV.D	W8, W4
L__HalfToFloat129:
	DEC	W0, W0
	BRA LT	L__HalfToFloat130
	SL	W4, W4
	RLC	W5, W5
	BRA	L__HalfToFloat129
L__HalfToFloat130:
; Sign end address is: 16 (W8)
	MOV	#23, W3
	ADD	W14, #0, W2
	MOV.D	[W2], W0
L__HalfToFloat131:
	DEC	W3, W3
	BRA LT	L__HalfToFloat132
	SL	W0, W0
	RLC	W1, W1
	BRA	L__HalfToFloat131
L__HalfToFloat132:
	IOR	W4, W0, W0
	IOR	W5, W1, W1
; Dst start address is: 0 (W0)
	IOR	W0, W6, W0
	IOR	W1, W7, W1
;Float16.mpas,102 :: 		end
; Dst end address is: 0 (W0)
	GOTO	L__HalfToFloat13
;Float16.mpas,103 :: 		else if (Exp = 31) and (Mantissa = 0) then
L__HalfToFloat12:
; Sign start address is: 16 (W8)
; Mantissa start address is: 22 (W11)
	MOV	[W14+0], W0
	MOV	[W14+2], W1
	CP	W0, #31
	CPB	W1, #0
	CLR	W2
	BRA NZ	L__HalfToFloat133
	COM	W2
L__HalfToFloat133:
	CP	W11, #0
	CPB	W12, #0
	CLR	W0
	BRA NZ	L__HalfToFloat134
	COM	W0
L__HalfToFloat134:
	AND	W2, W0, W0
	CP0	W0
	BRA NZ	L__HalfToFloat135
	GOTO	L__HalfToFloat20
L__HalfToFloat135:
; Mantissa end address is: 22 (W11)
;Float16.mpas,106 :: 		Dst := (Sign shl 31) or $7F800000;
	MOV	#31, W0
	MOV.D	W8, W2
L__HalfToFloat136:
	DEC	W0, W0
	BRA LT	L__HalfToFloat137
	SL	W2, W2
	RLC	W3, W3
	BRA	L__HalfToFloat136
L__HalfToFloat137:
; Sign end address is: 16 (W8)
	MOV	#0, W0
	MOV	#32640, W1
; Dst start address is: 0 (W0)
	IOR	W2, W0, W0
	IOR	W3, W1, W1
;Float16.mpas,107 :: 		end
; Dst end address is: 0 (W0)
	GOTO	L__HalfToFloat21
;Float16.mpas,108 :: 		else //if (Exp = 31) and (Mantisa <> 0) then
L__HalfToFloat20:
;Float16.mpas,111 :: 		Dst := (Sign shl 31) or $7F800000 or (Mantissa shl 13);
; Sign start address is: 16 (W8)
; Mantissa start address is: 22 (W11)
	MOV	#31, W0
	MOV.D	W8, W2
L__HalfToFloat138:
	DEC	W0, W0
	BRA LT	L__HalfToFloat139
	SL	W2, W2
	RLC	W3, W3
	BRA	L__HalfToFloat138
L__HalfToFloat139:
; Sign end address is: 16 (W8)
	MOV	#0, W0
	MOV	#32640, W1
	IOR	W2, W0, W4
	IOR	W3, W1, W5
	MOV	#13, W2
	MOV	W11, W0
	MOV	W12, W1
L__HalfToFloat140:
	DEC	W2, W2
	BRA LT	L__HalfToFloat141
	SL	W0, W0
	RLC	W1, W1
	BRA	L__HalfToFloat140
L__HalfToFloat141:
; Mantissa end address is: 22 (W11)
; Dst start address is: 0 (W0)
	IOR	W4, W0, W0
	IOR	W5, W1, W1
; Dst end address is: 0 (W0)
;Float16.mpas,112 :: 		end;
L__HalfToFloat21:
; Dst start address is: 0 (W0)
; Dst end address is: 0 (W0)
L__HalfToFloat13:
; Dst start address is: 0 (W0)
; Dst end address is: 0 (W0)
L__HalfToFloat10:
; Dst start address is: 0 (W0)
; Dst end address is: 0 (W0)
L__HalfToFloat7:
;Float16.mpas,115 :: 		Result := PReal(@Dst)^;
; Dst start address is: 0 (W0)
; Result start address is: 4 (W2)
	MOV.D	W0, W2
; Dst end address is: 0 (W0)
;Float16.mpas,116 :: 		end;
	MOV.D	W2, W0
; Result end address is: 4 (W2)
L_end_HalfToFloat:
	ULNK
	RETURN
; end of _HalfToFloat

_FloatToHalf:
	LNK	#4

;Float16.mpas,122 :: 		begin
;Float16.mpas,123 :: 		Src := PDWord(@Float)^;
; Src start address is: 24 (W12)
	MOV.D	W10, W12
;Float16.mpas,125 :: 		Sign := Src shr 31;
	MOV	#31, W0
; Sign start address is: 12 (W6)
	MOV.D	W10, W6
L__FloatToHalf143:
	DEC	W0, W0
	BRA LT	L__FloatToHalf144
	LSR	W7, W7
	RRC	W6, W6
	BRA	L__FloatToHalf143
L__FloatToHalf144:
;Float16.mpas,126 :: 		Exp := LongInt((Src and $7F800000) shr 23) - 127 + 15;
	MOV	#0, W0
	MOV	#32640, W1
	AND	W10, W0, W4
	AND	W11, W1, W5
	MOV	#23, W0
	MOV.D	W4, W2
L__FloatToHalf145:
	DEC	W0, W0
	BRA LT	L__FloatToHalf146
	ASR	W3, W3
	RRC	W2, W2
	BRA	L__FloatToHalf145
L__FloatToHalf146:
	MOV	#127, W0
	MOV	#0, W1
	SUB	W2, W0, W0
	SUBB	W3, W1, W1
	ADD	W0, #15, W3
	ADDC	W1, #0, W4
; Exp start address is: 16 (W8)
	MOV	W3, W8
	MOV	W4, W9
;Float16.mpas,127 :: 		Mantissa := Src and $007FFFFF;
	MOV	#65535, W1
	MOV	#127, W2
	ADD	W14, #0, W0
	AND	W10, W1, [W0++]
	AND	W11, W2, [W0--]
;Float16.mpas,129 :: 		if (Exp > 0) and (Exp < 30) then
	CP	W3, #0
	CPB	W4, #0
	CLR	W1
	BRA LE	L__FloatToHalf147
	COM	W1
L__FloatToHalf147:
	CP	W3, #30
	CPB	W4, #0
	CLR	W0
	BRA GE	L__FloatToHalf148
	COM	W0
L__FloatToHalf148:
	AND	W1, W0, W0
	CP0	W0
	BRA NZ	L__FloatToHalf149
	GOTO	L__FloatToHalf24
L__FloatToHalf149:
; Src end address is: 24 (W12)
;Float16.mpas,132 :: 		Result := (Sign shl 15) or (Exp shl 10) or ((Mantissa + $00001000) shr 13);
	MOV	#15, W0
	SL	W6, W0, W2
; Sign end address is: 12 (W6)
	MOV	#10, W1
	SL	W8, W1, W0
; Exp end address is: 16 (W8)
	IOR	W2, W0, W5
	MOV	#4096, W1
	MOV	#0, W2
	ADD	W14, #0, W0
	ADD	W1, [W0++], W3
	ADDC	W2, [W0--], W4
	MOV	#13, W2
	MOV	W3, W0
	MOV	W4, W1
L__FloatToHalf150:
	DEC	W2, W2
	BRA LT	L__FloatToHalf151
	ASR	W1, W1
	RRC	W0, W0
	BRA	L__FloatToHalf150
L__FloatToHalf151:
; Result start address is: 2 (W1)
	IOR	W5, W0, W1
;Float16.mpas,133 :: 		end
; Result end address is: 2 (W1)
	GOTO	L__FloatToHalf25
;Float16.mpas,134 :: 		else if Src = 0 then
L__FloatToHalf24:
; Exp start address is: 16 (W8)
; Sign start address is: 12 (W6)
; Src start address is: 24 (W12)
	CP	W12, #0
	CPB	W13, #0
	BRA Z	L__FloatToHalf152
	GOTO	L__FloatToHalf27
L__FloatToHalf152:
; Src end address is: 24 (W12)
; Sign end address is: 12 (W6)
; Exp end address is: 16 (W8)
;Float16.mpas,137 :: 		Result := 0;
; Result start address is: 2 (W1)
	CLR	W1
;Float16.mpas,138 :: 		end
; Result end address is: 2 (W1)
	GOTO	L__FloatToHalf28
;Float16.mpas,139 :: 		else
L__FloatToHalf27:
;Float16.mpas,142 :: 		if Exp <= 0 then
; Exp start address is: 16 (W8)
; Sign start address is: 12 (W6)
	CP	W8, #0
	CPB	W9, #0
	BRA LE	L__FloatToHalf153
	GOTO	L__FloatToHalf30
L__FloatToHalf153:
;Float16.mpas,144 :: 		if Exp < -10 then
	MOV	#65526, W0
	MOV	#65535, W1
	CP	W8, W0
	CPB	W9, W1
	BRA LT	L__FloatToHalf154
	GOTO	L__FloatToHalf33
L__FloatToHalf154:
; Sign end address is: 12 (W6)
; Exp end address is: 16 (W8)
;Float16.mpas,147 :: 		Result := 0;
; Result start address is: 0 (W0)
	CLR	W0
;Float16.mpas,148 :: 		end
; Result end address is: 0 (W0)
	GOTO	L__FloatToHalf34
;Float16.mpas,149 :: 		else
L__FloatToHalf33:
;Float16.mpas,153 :: 		Mantissa := (Mantissa or $00800000) shr (1 - Exp);
; Exp start address is: 16 (W8)
; Sign start address is: 12 (W6)
	MOV	[W14+0], W2
	MOV	[W14+2], W3
	MOV	#0, W0
	MOV	#128, W1
	IOR	W2, W0, W4
	IOR	W3, W1, W5
	SUBR	W8, #1, W0
	SUBBR	W9, #0, W1
; Exp end address is: 16 (W8)
	MOV.D	W4, W2
L__FloatToHalf155:
	DEC	W0, W0
	BRA LT	L__FloatToHalf156
	ASR	W3, W3
	RRC	W2, W2
	BRA	L__FloatToHalf155
L__FloatToHalf156:
	MOV	W2, [W14+0]
	MOV	W3, [W14+2]
;Float16.mpas,155 :: 		if (Mantissa and $00001000) > 0 then
	MOV	#4096, W0
	MOV	#0, W1
	AND	W2, W0, W0
	AND	W3, W1, W1
	CP	W0, #0
	CPB	W1, #0
	BRA GT	L__FloatToHalf157
	GOTO	L__FloatToHalf36
L__FloatToHalf157:
;Float16.mpas,156 :: 		Mantissa := Mantissa + $00002000;
	MOV	#8192, W2
	MOV	#0, W3
	ADD	W14, #0, W1
	ADD	W14, #0, W0
	ADD	W2, [W1++], [W0++]
	ADDC	W3, [W1--], [W0--]
L__FloatToHalf36:
;Float16.mpas,158 :: 		Result := (Sign shl 15) or (Mantissa shr 13);
	MOV	#15, W0
	SL	W6, W0, W4
; Sign end address is: 12 (W6)
	MOV	#13, W3
	ADD	W14, #0, W2
	MOV.D	[W2], W0
L__FloatToHalf158:
	DEC	W3, W3
	BRA LT	L__FloatToHalf159
	ASR	W1, W1
	RRC	W0, W0
	BRA	L__FloatToHalf158
L__FloatToHalf159:
; Result start address is: 0 (W0)
	IOR	W4, W0, W0
; Result end address is: 0 (W0)
;Float16.mpas,159 :: 		end;
L__FloatToHalf34:
;Float16.mpas,160 :: 		end
; Result start address is: 0 (W0)
	MOV	W0, W1
; Result end address is: 0 (W0)
	GOTO	L__FloatToHalf31
;Float16.mpas,161 :: 		else if Exp = 255 - 127 + 15 then
L__FloatToHalf30:
; Exp start address is: 16 (W8)
; Sign start address is: 12 (W6)
	MOV	#143, W0
	MOV	#0, W1
	CP	W8, W0
	CPB	W9, W1
	BRA Z	L__FloatToHalf160
	GOTO	L__FloatToHalf39
L__FloatToHalf160:
; Exp end address is: 16 (W8)
;Float16.mpas,163 :: 		if Mantissa = 0 then
	MOV	[W14+0], W0
	MOV	[W14+2], W1
	CP	W0, #0
	CPB	W1, #0
	BRA Z	L__FloatToHalf161
	GOTO	L__FloatToHalf42
L__FloatToHalf161:
;Float16.mpas,166 :: 		Result := (Sign shl 15) or $7C00;
	MOV	#15, W0
	SL	W6, W0, W1
; Sign end address is: 12 (W6)
	MOV	#31744, W0
; Result start address is: 0 (W0)
	IOR	W1, W0, W0
;Float16.mpas,167 :: 		end
; Result end address is: 0 (W0)
	GOTO	L__FloatToHalf43
;Float16.mpas,168 :: 		else
L__FloatToHalf42:
;Float16.mpas,171 :: 		Result := (Sign shl 15) or $7C00 or (Mantissa shr 13);
; Sign start address is: 12 (W6)
	MOV	#15, W0
	SL	W6, W0, W1
; Sign end address is: 12 (W6)
	MOV	#31744, W0
	IOR	W1, W0, W4
	MOV	#13, W3
	ADD	W14, #0, W2
	MOV.D	[W2], W0
L__FloatToHalf162:
	DEC	W3, W3
	BRA LT	L__FloatToHalf163
	ASR	W1, W1
	RRC	W0, W0
	BRA	L__FloatToHalf162
L__FloatToHalf163:
; Result start address is: 0 (W0)
	IOR	W4, W0, W0
; Result end address is: 0 (W0)
;Float16.mpas,172 :: 		end;
L__FloatToHalf43:
;Float16.mpas,173 :: 		end
; Result start address is: 0 (W0)
; Result end address is: 0 (W0)
	GOTO	L__FloatToHalf40
;Float16.mpas,174 :: 		else
L__FloatToHalf39:
;Float16.mpas,179 :: 		if (Mantissa and $00001000) > 0 then
; Exp start address is: 16 (W8)
; Sign start address is: 12 (W6)
	MOV	#4096, W3
	MOV	#0, W4
	ADD	W14, #0, W2
	AND	W3, [W2++], W0
	AND	W4, [W2--], W1
	CP	W0, #0
	CPB	W1, #0
	BRA GT	L__FloatToHalf164
	GOTO	L__FloatToHalf103
L__FloatToHalf164:
;Float16.mpas,181 :: 		Mantissa := Mantissa + $00002000;
	MOV	#8192, W4
	MOV	#0, W5
	ADD	W14, #0, W0
	ADD	W4, [W0++], W2
	ADDC	W5, [W0--], W3
	MOV	W2, [W14+0]
	MOV	W3, [W14+2]
;Float16.mpas,182 :: 		if (Mantissa and $00800000) > 0 then
	MOV	#0, W0
	MOV	#128, W1
	AND	W2, W0, W0
	AND	W3, W1, W1
	CP	W0, #0
	CPB	W1, #0
	BRA GT	L__FloatToHalf165
	GOTO	L__FloatToHalf102
L__FloatToHalf165:
;Float16.mpas,184 :: 		Mantissa := 0;
	CLR	W0
	CLR	W1
	MOV	W0, [W14+0]
	MOV	W1, [W14+2]
;Float16.mpas,185 :: 		Exp := Exp + 1;
; Exp start address is: 0 (W0)
	ADD	W8, #1, W0
	ADDC	W9, #0, W1
; Exp end address is: 16 (W8)
; Exp end address is: 0 (W0)
;Float16.mpas,186 :: 		end;
	GOTO	L__FloatToHalf48
L__FloatToHalf102:
;Float16.mpas,182 :: 		if (Mantissa and $00800000) > 0 then
	MOV.D	W8, W0
;Float16.mpas,186 :: 		end;
L__FloatToHalf48:
;Float16.mpas,187 :: 		end;
; Exp start address is: 0 (W0)
	MOV	W0, W3
	MOV	W1, W4
; Exp end address is: 0 (W0)
	GOTO	L__FloatToHalf45
L__FloatToHalf103:
;Float16.mpas,179 :: 		if (Mantissa and $00001000) > 0 then
	MOV	W8, W3
	MOV	W9, W4
;Float16.mpas,187 :: 		end;
L__FloatToHalf45:
;Float16.mpas,189 :: 		if Exp > 30 then
; Exp start address is: 6 (W3)
	CP	W3, #30
	CPB	W4, #0
	BRA GT	L__FloatToHalf166
	GOTO	L__FloatToHalf51
L__FloatToHalf166:
; Exp end address is: 6 (W3)
;Float16.mpas,192 :: 		Result := (Sign shl 15) or $7C00;
	MOV	#15, W0
	SL	W6, W0, W1
; Sign end address is: 12 (W6)
	MOV	#31744, W0
; Result start address is: 0 (W0)
	IOR	W1, W0, W0
;Float16.mpas,193 :: 		end
; Result end address is: 0 (W0)
	GOTO	L__FloatToHalf52
;Float16.mpas,194 :: 		else
L__FloatToHalf51:
;Float16.mpas,196 :: 		Result := (Sign shl 15) or (Exp shl 10) or (Mantissa shr 13);
; Exp start address is: 6 (W3)
; Sign start address is: 12 (W6)
	MOV	#15, W0
	SL	W6, W0, W2
; Sign end address is: 12 (W6)
	MOV	#10, W1
	SL	W3, W1, W0
; Exp end address is: 6 (W3)
	IOR	W2, W0, W4
	MOV	#13, W3
	ADD	W14, #0, W2
	MOV.D	[W2], W0
L__FloatToHalf167:
	DEC	W3, W3
	BRA LT	L__FloatToHalf168
	ASR	W1, W1
	RRC	W0, W0
	BRA	L__FloatToHalf167
L__FloatToHalf168:
; Result start address is: 0 (W0)
	IOR	W4, W0, W0
; Result end address is: 0 (W0)
L__FloatToHalf52:
;Float16.mpas,197 :: 		end;
; Result start address is: 0 (W0)
; Result end address is: 0 (W0)
L__FloatToHalf40:
; Result start address is: 0 (W0)
	MOV	W0, W1
; Result end address is: 0 (W0)
L__FloatToHalf31:
;Float16.mpas,198 :: 		end;
; Result start address is: 2 (W1)
; Result end address is: 2 (W1)
L__FloatToHalf28:
; Result start address is: 2 (W1)
; Result end address is: 2 (W1)
L__FloatToHalf25:
;Float16.mpas,199 :: 		end;
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_FloatToHalf:
	ULNK
	RETURN
; end of _FloatToHalf

Float16_Float16ToFloat:
	LNK	#4

;Float16.mpas,205 :: 		begin
;Float16.mpas,207 :: 		Sign := Half shr 15;
	LSR	W10, #15, W0
; Sign start address is: 16 (W8)
	MOV	W0, W8
	CLR	W9
;Float16.mpas,208 :: 		Exp := (Half and $7C00) shr 10;
	MOV	#31744, W0
	AND	W10, W0, W0
	LSR	W0, #10, W0
	CLR	W1
	MOV	W0, [W14+0]
	MOV	W1, [W14+2]
;Float16.mpas,209 :: 		Mantissa := Half and 1023;
	MOV	#1023, W0
	AND	W10, W0, W0
; Mantissa start address is: 22 (W11)
	MOV	W0, W11
	CLR	W12
;Float16.mpas,211 :: 		if (Exp > 0) and (Exp < 31) then
	MOV	[W14+0], W0
	MOV	[W14+2], W1
	CP	W0, #0
	CPB	W1, #0
	CLR	W2
	BRA LE	L_Float16_Float16ToFloat170
	COM	W2
L_Float16_Float16ToFloat170:
	CP	W0, #31
	CPB	W1, #0
	CLR	W0
	BRA GE	L_Float16_Float16ToFloat171
	COM	W0
L_Float16_Float16ToFloat171:
	AND	W2, W0, W0
	BRA NZ	L_Float16_Float16ToFloat172
	GOTO	L_Float16_Float16ToFloat55
L_Float16_Float16ToFloat172:
;Float16.mpas,214 :: 		Exp := Exp + (127 - 15);
	MOV	#112, W2
	MOV	#0, W3
	ADD	W14, #0, W1
	ADD	W14, #0, W0
	ADD	W2, [W1++], [W0++]
	ADDC	W3, [W1--], [W0--]
;Float16.mpas,215 :: 		Mantissa := Mantissa shl 13;
	MOV	#13, W0
	MOV	W11, W6
	MOV	W12, W7
L_Float16_Float16ToFloat173:
	DEC	W0, W0
	BRA LT	L_Float16_Float16ToFloat174
	SL	W6, W6
	RLC	W7, W7
	BRA	L_Float16_Float16ToFloat173
L_Float16_Float16ToFloat174:
; Mantissa end address is: 22 (W11)
;Float16.mpas,216 :: 		Dst := (Sign shl 31) or (DWord(Exp) shl 23) or Mantissa;
	MOV	#31, W0
	MOV.D	W8, W4
L_Float16_Float16ToFloat175:
	DEC	W0, W0
	BRA LT	L_Float16_Float16ToFloat176
	SL	W4, W4
	RLC	W5, W5
	BRA	L_Float16_Float16ToFloat175
L_Float16_Float16ToFloat176:
; Sign end address is: 16 (W8)
	MOV	#23, W3
	ADD	W14, #0, W2
	MOV.D	[W2], W0
L_Float16_Float16ToFloat177:
	DEC	W3, W3
	BRA LT	L_Float16_Float16ToFloat178
	SL	W0, W0
	RLC	W1, W1
	BRA	L_Float16_Float16ToFloat177
L_Float16_Float16ToFloat178:
	IOR	W4, W0, W0
	IOR	W5, W1, W1
; Dst start address is: 0 (W0)
	IOR	W0, W6, W0
	IOR	W1, W7, W1
;Float16.mpas,218 :: 		end
; Dst end address is: 0 (W0)
	GOTO	L_Float16_Float16ToFloat56
;Float16.mpas,219 :: 		else if (Exp = 0) and (Mantissa = 0) then
L_Float16_Float16ToFloat55:
; Sign start address is: 16 (W8)
; Mantissa start address is: 22 (W11)
	MOV	[W14+0], W0
	MOV	[W14+2], W1
	CP	W0, #0
	CPB	W1, #0
	CLR	W2
	BRA NZ	L_Float16_Float16ToFloat179
	COM	W2
L_Float16_Float16ToFloat179:
	CP	W11, #0
	CPB	W12, #0
	CLR	W0
	BRA NZ	L_Float16_Float16ToFloat180
	COM	W0
L_Float16_Float16ToFloat180:
	AND	W2, W0, W0
	CP0	W0
	BRA NZ	L_Float16_Float16ToFloat181
	GOTO	L_Float16_Float16ToFloat58
L_Float16_Float16ToFloat181:
; Mantissa end address is: 22 (W11)
;Float16.mpas,222 :: 		Dst := Sign shl 31;
	MOV	#31, W0
; Dst start address is: 2 (W1)
	MOV	W8, W1
	MOV	W9, W2
L_Float16_Float16ToFloat182:
	DEC	W0, W0
	BRA LT	L_Float16_Float16ToFloat183
	SL	W1, W1
	RLC	W2, W2
	BRA	L_Float16_Float16ToFloat182
L_Float16_Float16ToFloat183:
; Sign end address is: 16 (W8)
;Float16.mpas,223 :: 		end
	MOV	W1, W0
	MOV	W2, W1
; Dst end address is: 2 (W1)
	GOTO	L_Float16_Float16ToFloat59
;Float16.mpas,224 :: 		else if (Exp = 0) and (Mantissa <> 0) then
L_Float16_Float16ToFloat58:
; Sign start address is: 16 (W8)
; Mantissa start address is: 22 (W11)
	MOV	[W14+0], W0
	MOV	[W14+2], W1
	CP	W0, #0
	CPB	W1, #0
	CLR	W2
	BRA NZ	L_Float16_Float16ToFloat184
	COM	W2
L_Float16_Float16ToFloat184:
	CP	W11, #0
	CPB	W12, #0
	CLR	W0
	BRA Z	L_Float16_Float16ToFloat185
	COM	W0
L_Float16_Float16ToFloat185:
	AND	W2, W0, W0
	CP0	W0
	BRA NZ	L_Float16_Float16ToFloat186
	GOTO	L_Float16_Float16ToFloat61
L_Float16_Float16ToFloat186:
; Mantissa end address is: 22 (W11)
; Sign end address is: 16 (W8)
;Float16.mpas,227 :: 		while (Mantissa and $00000400) = 0 do
L_Float16_Float16ToFloat64:
; Mantissa start address is: 22 (W11)
; Sign start address is: 16 (W8)
	MOV	#1024, W0
	MOV	#0, W1
	AND	W11, W0, W0
	AND	W12, W1, W1
	CP	W0, #0
	CPB	W1, #0
	BRA Z	L_Float16_Float16ToFloat187
	GOTO	L_Float16_Float16ToFloat65
L_Float16_Float16ToFloat187:
;Float16.mpas,229 :: 		Mantissa := Mantissa shl 1;
; Mantissa start address is: 6 (W3)
	MOV	W11, W3
	MOV	W12, W4
	SL	W3, W3
	RLC	W4, W4
; Mantissa end address is: 22 (W11)
;Float16.mpas,230 :: 		Dec(Exp);
	MOV	[W14+0], W1
	MOV	[W14+2], W2
	ADD	W14, #0, W0
	SUB	W1, #1, [W0++]
	SUBB	W2, #0, [W0--]
;Float16.mpas,231 :: 		end;
; Mantissa end address is: 6 (W3)
	MOV	W3, W11
	MOV	W4, W12
	GOTO	L_Float16_Float16ToFloat64
L_Float16_Float16ToFloat65:
;Float16.mpas,232 :: 		Inc(Exp);
; Mantissa start address is: 22 (W11)
	MOV	[W14+0], W0
	MOV	[W14+2], W1
	ADD	W0, #1, W5
	ADDC	W1, #0, W6
;Float16.mpas,233 :: 		Mantissa := Mantissa and not $00000400;
	MOV	#64511, W0
	MOV	#0, W1
	AND	W11, W0, W3
	AND	W12, W1, W4
; Mantissa end address is: 22 (W11)
;Float16.mpas,235 :: 		Exp := Exp + (127 - 15);
	MOV	#112, W1
	MOV	#0, W2
	ADD	W14, #0, W0
	ADD	W5, W1, [W0++]
	ADDC	W6, W2, [W0--]
;Float16.mpas,236 :: 		Mantissa := Mantissa shl 13;
	MOV	#13, W0
	MOV	W3, W6
	MOV	W4, W7
L_Float16_Float16ToFloat188:
	DEC	W0, W0
	BRA LT	L_Float16_Float16ToFloat189
	SL	W6, W6
	RLC	W7, W7
	BRA	L_Float16_Float16ToFloat188
L_Float16_Float16ToFloat189:
;Float16.mpas,237 :: 		Dst := (Sign shl 31) or (DWord(Exp) shl 23) or Mantissa;
	MOV	#31, W0
	MOV.D	W8, W4
L_Float16_Float16ToFloat190:
	DEC	W0, W0
	BRA LT	L_Float16_Float16ToFloat191
	SL	W4, W4
	RLC	W5, W5
	BRA	L_Float16_Float16ToFloat190
L_Float16_Float16ToFloat191:
; Sign end address is: 16 (W8)
	MOV	#23, W3
	ADD	W14, #0, W2
	MOV.D	[W2], W0
L_Float16_Float16ToFloat192:
	DEC	W3, W3
	BRA LT	L_Float16_Float16ToFloat193
	SL	W0, W0
	RLC	W1, W1
	BRA	L_Float16_Float16ToFloat192
L_Float16_Float16ToFloat193:
	IOR	W4, W0, W0
	IOR	W5, W1, W1
; Dst start address is: 0 (W0)
	IOR	W0, W6, W0
	IOR	W1, W7, W1
;Float16.mpas,239 :: 		end
; Dst end address is: 0 (W0)
	GOTO	L_Float16_Float16ToFloat62
;Float16.mpas,240 :: 		else if (Exp = 31) and (Mantissa = 0) then
L_Float16_Float16ToFloat61:
; Sign start address is: 16 (W8)
; Mantissa start address is: 22 (W11)
	MOV	[W14+0], W0
	MOV	[W14+2], W1
	CP	W0, #31
	CPB	W1, #0
	CLR	W2
	BRA NZ	L_Float16_Float16ToFloat194
	COM	W2
L_Float16_Float16ToFloat194:
	CP	W11, #0
	CPB	W12, #0
	CLR	W0
	BRA NZ	L_Float16_Float16ToFloat195
	COM	W0
L_Float16_Float16ToFloat195:
	AND	W2, W0, W0
	CP0	W0
	BRA NZ	L_Float16_Float16ToFloat196
	GOTO	L_Float16_Float16ToFloat69
L_Float16_Float16ToFloat196:
; Mantissa end address is: 22 (W11)
;Float16.mpas,243 :: 		Dst := (Sign shl 31) or $7F800000;
	MOV	#31, W0
	MOV.D	W8, W2
L_Float16_Float16ToFloat197:
	DEC	W0, W0
	BRA LT	L_Float16_Float16ToFloat198
	SL	W2, W2
	RLC	W3, W3
	BRA	L_Float16_Float16ToFloat197
L_Float16_Float16ToFloat198:
; Sign end address is: 16 (W8)
	MOV	#0, W0
	MOV	#32640, W1
; Dst start address is: 0 (W0)
	IOR	W2, W0, W0
	IOR	W3, W1, W1
;Float16.mpas,244 :: 		end
; Dst end address is: 0 (W0)
	GOTO	L_Float16_Float16ToFloat70
;Float16.mpas,245 :: 		else //if (Exp = 31) and (Mantisa <> 0) then
L_Float16_Float16ToFloat69:
;Float16.mpas,248 :: 		Dst := (Sign shl 31) or $7F800000 or (Mantissa shl 13);
; Sign start address is: 16 (W8)
; Mantissa start address is: 22 (W11)
	MOV	#31, W0
	MOV.D	W8, W2
L_Float16_Float16ToFloat199:
	DEC	W0, W0
	BRA LT	L_Float16_Float16ToFloat200
	SL	W2, W2
	RLC	W3, W3
	BRA	L_Float16_Float16ToFloat199
L_Float16_Float16ToFloat200:
; Sign end address is: 16 (W8)
	MOV	#0, W0
	MOV	#32640, W1
	IOR	W2, W0, W4
	IOR	W3, W1, W5
	MOV	#13, W2
	MOV	W11, W0
	MOV	W12, W1
L_Float16_Float16ToFloat201:
	DEC	W2, W2
	BRA LT	L_Float16_Float16ToFloat202
	SL	W0, W0
	RLC	W1, W1
	BRA	L_Float16_Float16ToFloat201
L_Float16_Float16ToFloat202:
; Mantissa end address is: 22 (W11)
; Dst start address is: 0 (W0)
	IOR	W4, W0, W0
	IOR	W5, W1, W1
; Dst end address is: 0 (W0)
;Float16.mpas,249 :: 		end;
L_Float16_Float16ToFloat70:
; Dst start address is: 0 (W0)
; Dst end address is: 0 (W0)
L_Float16_Float16ToFloat62:
; Dst start address is: 0 (W0)
; Dst end address is: 0 (W0)
L_Float16_Float16ToFloat59:
; Dst start address is: 0 (W0)
; Dst end address is: 0 (W0)
L_Float16_Float16ToFloat56:
;Float16.mpas,252 :: 		Result := PReal(@Dst)^;
; Dst start address is: 0 (W0)
; Result start address is: 4 (W2)
	MOV.D	W0, W2
; Dst end address is: 0 (W0)
;Float16.mpas,253 :: 		end;
	MOV.D	W2, W0
; Result end address is: 4 (W2)
L_end_Float16ToFloat:
	ULNK
	RETURN
; end of Float16_Float16ToFloat

Float16_FloatToFloat16:
	LNK	#4

;Float16.mpas,259 :: 		begin
;Float16.mpas,260 :: 		Src := PDWord(@Float)^;
; Src start address is: 24 (W12)
	MOV.D	W10, W12
;Float16.mpas,262 :: 		Sign := Src shr 31;
	MOV	#31, W0
; Sign start address is: 12 (W6)
	MOV.D	W10, W6
L_Float16_FloatToFloat16204:
	DEC	W0, W0
	BRA LT	L_Float16_FloatToFloat16205
	LSR	W7, W7
	RRC	W6, W6
	BRA	L_Float16_FloatToFloat16204
L_Float16_FloatToFloat16205:
;Float16.mpas,263 :: 		Exp := LongInt((Src and $7F800000) shr 23) - 127 + 15;
	MOV	#0, W0
	MOV	#32640, W1
	AND	W10, W0, W4
	AND	W11, W1, W5
	MOV	#23, W0
	MOV.D	W4, W2
L_Float16_FloatToFloat16206:
	DEC	W0, W0
	BRA LT	L_Float16_FloatToFloat16207
	ASR	W3, W3
	RRC	W2, W2
	BRA	L_Float16_FloatToFloat16206
L_Float16_FloatToFloat16207:
	MOV	#127, W0
	MOV	#0, W1
	SUB	W2, W0, W0
	SUBB	W3, W1, W1
	ADD	W0, #15, W3
	ADDC	W1, #0, W4
; Exp start address is: 16 (W8)
	MOV	W3, W8
	MOV	W4, W9
;Float16.mpas,264 :: 		Mantissa := Src and $007FFFFF;
	MOV	#65535, W1
	MOV	#127, W2
	ADD	W14, #0, W0
	AND	W10, W1, [W0++]
	AND	W11, W2, [W0--]
;Float16.mpas,266 :: 		if (Exp > 0) and (Exp < 30) then
	CP	W3, #0
	CPB	W4, #0
	CLR	W1
	BRA LE	L_Float16_FloatToFloat16208
	COM	W1
L_Float16_FloatToFloat16208:
	CP	W3, #30
	CPB	W4, #0
	CLR	W0
	BRA GE	L_Float16_FloatToFloat16209
	COM	W0
L_Float16_FloatToFloat16209:
	AND	W1, W0, W0
	CP0	W0
	BRA NZ	L_Float16_FloatToFloat16210
	GOTO	L_Float16_FloatToFloat1673
L_Float16_FloatToFloat16210:
; Src end address is: 24 (W12)
;Float16.mpas,269 :: 		Result := (Sign shl 15) or (Exp shl 10) or ((Mantissa + $00001000) shr 13);
	MOV	#15, W0
	SL	W6, W0, W2
; Sign end address is: 12 (W6)
	MOV	#10, W1
	SL	W8, W1, W0
; Exp end address is: 16 (W8)
	IOR	W2, W0, W5
	MOV	#4096, W1
	MOV	#0, W2
	ADD	W14, #0, W0
	ADD	W1, [W0++], W3
	ADDC	W2, [W0--], W4
	MOV	#13, W2
	MOV	W3, W0
	MOV	W4, W1
L_Float16_FloatToFloat16211:
	DEC	W2, W2
	BRA LT	L_Float16_FloatToFloat16212
	ASR	W1, W1
	RRC	W0, W0
	BRA	L_Float16_FloatToFloat16211
L_Float16_FloatToFloat16212:
; Result start address is: 2 (W1)
	IOR	W5, W0, W1
;Float16.mpas,270 :: 		end
; Result end address is: 2 (W1)
	GOTO	L_Float16_FloatToFloat1674
;Float16.mpas,271 :: 		else if Src = 0 then
L_Float16_FloatToFloat1673:
; Exp start address is: 16 (W8)
; Sign start address is: 12 (W6)
; Src start address is: 24 (W12)
	CP	W12, #0
	CPB	W13, #0
	BRA Z	L_Float16_FloatToFloat16213
	GOTO	L_Float16_FloatToFloat1676
L_Float16_FloatToFloat16213:
; Src end address is: 24 (W12)
; Sign end address is: 12 (W6)
; Exp end address is: 16 (W8)
;Float16.mpas,274 :: 		Result := 0;
; Result start address is: 2 (W1)
	CLR	W1
;Float16.mpas,275 :: 		end
; Result end address is: 2 (W1)
	GOTO	L_Float16_FloatToFloat1677
;Float16.mpas,276 :: 		else
L_Float16_FloatToFloat1676:
;Float16.mpas,279 :: 		if Exp <= 0 then
; Exp start address is: 16 (W8)
; Sign start address is: 12 (W6)
	CP	W8, #0
	CPB	W9, #0
	BRA LE	L_Float16_FloatToFloat16214
	GOTO	L_Float16_FloatToFloat1679
L_Float16_FloatToFloat16214:
;Float16.mpas,281 :: 		if Exp < -10 then
	MOV	#65526, W0
	MOV	#65535, W1
	CP	W8, W0
	CPB	W9, W1
	BRA LT	L_Float16_FloatToFloat16215
	GOTO	L_Float16_FloatToFloat1682
L_Float16_FloatToFloat16215:
; Sign end address is: 12 (W6)
; Exp end address is: 16 (W8)
;Float16.mpas,284 :: 		Result := 0;
; Result start address is: 0 (W0)
	CLR	W0
;Float16.mpas,285 :: 		end
; Result end address is: 0 (W0)
	GOTO	L_Float16_FloatToFloat1683
;Float16.mpas,286 :: 		else
L_Float16_FloatToFloat1682:
;Float16.mpas,290 :: 		Mantissa := (Mantissa or $00800000) shr (1 - Exp);
; Exp start address is: 16 (W8)
; Sign start address is: 12 (W6)
	MOV	[W14+0], W2
	MOV	[W14+2], W3
	MOV	#0, W0
	MOV	#128, W1
	IOR	W2, W0, W4
	IOR	W3, W1, W5
	SUBR	W8, #1, W0
	SUBBR	W9, #0, W1
; Exp end address is: 16 (W8)
	MOV.D	W4, W2
L_Float16_FloatToFloat16216:
	DEC	W0, W0
	BRA LT	L_Float16_FloatToFloat16217
	ASR	W3, W3
	RRC	W2, W2
	BRA	L_Float16_FloatToFloat16216
L_Float16_FloatToFloat16217:
	MOV	W2, [W14+0]
	MOV	W3, [W14+2]
;Float16.mpas,292 :: 		if (Mantissa and $00001000) > 0 then
	MOV	#4096, W0
	MOV	#0, W1
	AND	W2, W0, W0
	AND	W3, W1, W1
	CP	W0, #0
	CPB	W1, #0
	BRA GT	L_Float16_FloatToFloat16218
	GOTO	L_Float16_FloatToFloat1685
L_Float16_FloatToFloat16218:
;Float16.mpas,293 :: 		Mantissa := Mantissa + $00002000;
	MOV	#8192, W2
	MOV	#0, W3
	ADD	W14, #0, W1
	ADD	W14, #0, W0
	ADD	W2, [W1++], [W0++]
	ADDC	W3, [W1--], [W0--]
L_Float16_FloatToFloat1685:
;Float16.mpas,295 :: 		Result := (Sign shl 15) or (Mantissa shr 13);
	MOV	#15, W0
	SL	W6, W0, W4
; Sign end address is: 12 (W6)
	MOV	#13, W3
	ADD	W14, #0, W2
	MOV.D	[W2], W0
L_Float16_FloatToFloat16219:
	DEC	W3, W3
	BRA LT	L_Float16_FloatToFloat16220
	ASR	W1, W1
	RRC	W0, W0
	BRA	L_Float16_FloatToFloat16219
L_Float16_FloatToFloat16220:
; Result start address is: 0 (W0)
	IOR	W4, W0, W0
; Result end address is: 0 (W0)
;Float16.mpas,296 :: 		end;
L_Float16_FloatToFloat1683:
;Float16.mpas,297 :: 		end
; Result start address is: 0 (W0)
	MOV	W0, W1
; Result end address is: 0 (W0)
	GOTO	L_Float16_FloatToFloat1680
;Float16.mpas,298 :: 		else if Exp = 255 - 127 + 15 then
L_Float16_FloatToFloat1679:
; Exp start address is: 16 (W8)
; Sign start address is: 12 (W6)
	MOV	#143, W0
	MOV	#0, W1
	CP	W8, W0
	CPB	W9, W1
	BRA Z	L_Float16_FloatToFloat16221
	GOTO	L_Float16_FloatToFloat1688
L_Float16_FloatToFloat16221:
; Exp end address is: 16 (W8)
;Float16.mpas,300 :: 		if Mantissa = 0 then
	MOV	[W14+0], W0
	MOV	[W14+2], W1
	CP	W0, #0
	CPB	W1, #0
	BRA Z	L_Float16_FloatToFloat16222
	GOTO	L_Float16_FloatToFloat1691
L_Float16_FloatToFloat16222:
;Float16.mpas,303 :: 		Result := (Sign shl 15) or $7C00;
	MOV	#15, W0
	SL	W6, W0, W1
; Sign end address is: 12 (W6)
	MOV	#31744, W0
; Result start address is: 0 (W0)
	IOR	W1, W0, W0
;Float16.mpas,304 :: 		end
; Result end address is: 0 (W0)
	GOTO	L_Float16_FloatToFloat1692
;Float16.mpas,305 :: 		else
L_Float16_FloatToFloat1691:
;Float16.mpas,308 :: 		Result := (Sign shl 15) or $7C00 or (Mantissa shr 13);
; Sign start address is: 12 (W6)
	MOV	#15, W0
	SL	W6, W0, W1
; Sign end address is: 12 (W6)
	MOV	#31744, W0
	IOR	W1, W0, W4
	MOV	#13, W3
	ADD	W14, #0, W2
	MOV.D	[W2], W0
L_Float16_FloatToFloat16223:
	DEC	W3, W3
	BRA LT	L_Float16_FloatToFloat16224
	ASR	W1, W1
	RRC	W0, W0
	BRA	L_Float16_FloatToFloat16223
L_Float16_FloatToFloat16224:
; Result start address is: 0 (W0)
	IOR	W4, W0, W0
; Result end address is: 0 (W0)
;Float16.mpas,309 :: 		end;
L_Float16_FloatToFloat1692:
;Float16.mpas,310 :: 		end
; Result start address is: 0 (W0)
; Result end address is: 0 (W0)
	GOTO	L_Float16_FloatToFloat1689
;Float16.mpas,311 :: 		else
L_Float16_FloatToFloat1688:
;Float16.mpas,316 :: 		if (Mantissa and $00001000) > 0 then
; Exp start address is: 16 (W8)
; Sign start address is: 12 (W6)
	MOV	#4096, W3
	MOV	#0, W4
	ADD	W14, #0, W2
	AND	W3, [W2++], W0
	AND	W4, [W2--], W1
	CP	W0, #0
	CPB	W1, #0
	BRA GT	L_Float16_FloatToFloat16225
	GOTO	L_Float16_FloatToFloat16105
L_Float16_FloatToFloat16225:
;Float16.mpas,318 :: 		Mantissa := Mantissa + $00002000;
	MOV	#8192, W4
	MOV	#0, W5
	ADD	W14, #0, W0
	ADD	W4, [W0++], W2
	ADDC	W5, [W0--], W3
	MOV	W2, [W14+0]
	MOV	W3, [W14+2]
;Float16.mpas,319 :: 		if (Mantissa and $00800000) > 0 then
	MOV	#0, W0
	MOV	#128, W1
	AND	W2, W0, W0
	AND	W3, W1, W1
	CP	W0, #0
	CPB	W1, #0
	BRA GT	L_Float16_FloatToFloat16226
	GOTO	L_Float16_FloatToFloat16104
L_Float16_FloatToFloat16226:
;Float16.mpas,321 :: 		Mantissa := 0;
	CLR	W0
	CLR	W1
	MOV	W0, [W14+0]
	MOV	W1, [W14+2]
;Float16.mpas,322 :: 		Exp := Exp + 1;
; Exp start address is: 0 (W0)
	ADD	W8, #1, W0
	ADDC	W9, #0, W1
; Exp end address is: 16 (W8)
; Exp end address is: 0 (W0)
;Float16.mpas,323 :: 		end;
	GOTO	L_Float16_FloatToFloat1697
L_Float16_FloatToFloat16104:
;Float16.mpas,319 :: 		if (Mantissa and $00800000) > 0 then
	MOV.D	W8, W0
;Float16.mpas,323 :: 		end;
L_Float16_FloatToFloat1697:
;Float16.mpas,324 :: 		end;
; Exp start address is: 0 (W0)
	MOV	W0, W3
	MOV	W1, W4
; Exp end address is: 0 (W0)
	GOTO	L_Float16_FloatToFloat1694
L_Float16_FloatToFloat16105:
;Float16.mpas,316 :: 		if (Mantissa and $00001000) > 0 then
	MOV	W8, W3
	MOV	W9, W4
;Float16.mpas,324 :: 		end;
L_Float16_FloatToFloat1694:
;Float16.mpas,326 :: 		if Exp > 30 then
; Exp start address is: 6 (W3)
	CP	W3, #30
	CPB	W4, #0
	BRA GT	L_Float16_FloatToFloat16227
	GOTO	L_Float16_FloatToFloat16100
L_Float16_FloatToFloat16227:
; Exp end address is: 6 (W3)
;Float16.mpas,329 :: 		Result := (Sign shl 15) or $7C00;
	MOV	#15, W0
	SL	W6, W0, W1
; Sign end address is: 12 (W6)
	MOV	#31744, W0
; Result start address is: 0 (W0)
	IOR	W1, W0, W0
;Float16.mpas,330 :: 		end
; Result end address is: 0 (W0)
	GOTO	L_Float16_FloatToFloat16101
;Float16.mpas,331 :: 		else
L_Float16_FloatToFloat16100:
;Float16.mpas,333 :: 		Result := (Sign shl 15) or (Exp shl 10) or (Mantissa shr 13);
; Exp start address is: 6 (W3)
; Sign start address is: 12 (W6)
	MOV	#15, W0
	SL	W6, W0, W2
; Sign end address is: 12 (W6)
	MOV	#10, W1
	SL	W3, W1, W0
; Exp end address is: 6 (W3)
	IOR	W2, W0, W4
	MOV	#13, W3
	ADD	W14, #0, W2
	MOV.D	[W2], W0
L_Float16_FloatToFloat16228:
	DEC	W3, W3
	BRA LT	L_Float16_FloatToFloat16229
	ASR	W1, W1
	RRC	W0, W0
	BRA	L_Float16_FloatToFloat16228
L_Float16_FloatToFloat16229:
; Result start address is: 0 (W0)
	IOR	W4, W0, W0
; Result end address is: 0 (W0)
L_Float16_FloatToFloat16101:
;Float16.mpas,334 :: 		end;
; Result start address is: 0 (W0)
; Result end address is: 0 (W0)
L_Float16_FloatToFloat1689:
; Result start address is: 0 (W0)
	MOV	W0, W1
; Result end address is: 0 (W0)
L_Float16_FloatToFloat1680:
;Float16.mpas,335 :: 		end;
; Result start address is: 2 (W1)
; Result end address is: 2 (W1)
L_Float16_FloatToFloat1677:
; Result start address is: 2 (W1)
; Result end address is: 2 (W1)
L_Float16_FloatToFloat1674:
;Float16.mpas,336 :: 		end;
; Result start address is: 2 (W1)
	MOV	W1, W0
; Result end address is: 2 (W1)
L_end_FloatToFloat16:
	ULNK
	RETURN
; end of Float16_FloatToFloat16
