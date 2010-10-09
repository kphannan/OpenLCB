all: mcanxcs.hex

mcanxcs.hex: canxcs.c ../canlib/frametypes.c ../canlib/general.c ../canlib/entry.c ../canlib/ecan.c ../canldr/bootldr.hex
	MCC18 -fo=canxcs.o -Oa -Opa- -p18f2480 canxcs.c 
	mplink ../canlib/18f2480.lkr canxcs.o /i /a INHX32 /o canxcs.cof
	hexmerge canxcs.hex ../canldr/bootldr.hex mcanxcs.hex

clean:
	del *.o
	del *.cof
	del *.map
	del *.hex
	del *.lib
	del *.lst
	del *.cod
	del *.err

