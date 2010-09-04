all: mcancs.hex mcancs2.hex

mcancs.hex: cancs.c ../canlib/frametypes.c ../canlib/general.c ../canlib/entry.c ../canlib/ecan.c ../canldr/bootldr.hex
	MCC18 -fo=cancs.o -Oa -Opa- -p18f2480 cancs.c 
	mplink ../canlib/18f2480.lkr cancs.o /i /a INHX32 /o cancs.cof
	hexmerge cancs.hex ../canldr/bootldr.hex mcancs.hex

mcancs2.hex: cancs.c ../canlib/frametypes.c ../canlib/general.c ../canlib/entry.c ../canlib/ecan.c ../canldr/bootldr.hex
	MCC18 -fo=cancs2.o -Oa -Opa- -p18f2680 cancs.c 
	mplink ../canlib/18f2680.lkr cancs2.o /i /a INHX32 /o cancs2.cof
	hexmerge cancs2.hex ../canldr/bootldr.hex mcancs2.hex

clean:
	del *.o
	del *.cof
	del *.map
	del *.hex
	del *.lib
	del *.lst
	del *.cod
	del *.err

