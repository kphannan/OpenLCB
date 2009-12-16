mcanled.hex: canled.c ../canlib/frametypes.c ../canlib/general.c ../canlib/ecan.c ../canlib/hash.c ../canldr/bootldr.hex
	MCC18 -fo=canled.o -Oa -Opa- -p18f2480 canled.c 
	mplink ../canlib/18f2480.lkr canled.o /i /a INHX32 /o canled.cof
	hexmerge canled.hex ../canldr/bootldr.hex mcanled.hex

clean:
	del *.o
	del *.cof
	del *.map
	del *.hex
	del *.lib
	del *.lst
	del *.cod
