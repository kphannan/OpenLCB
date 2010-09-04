All: mcanrfid.hex

mcanrfid.hex: canrfid.c ../canlib/general.c ../canlib/frametypes.c ../canlib/entry.c ../canlib/ecan.c ../canldr/bootldr.hex
	MCC18 -fo=canrfid.o -Oa -Opa- -p18f2480 canrfid.c 
	mplink ../canlib/18f2480.lkr canrfid.o /a INHX32 /i /o canrfid.cof
	hexmerge canrfid.hex ../canldr/bootldr.hex mcanrfid.hex

clean:
	del *.o
	del *.cof
	del *.map
	del *.hex
	del *.lib
	del *.lst
	del *.cod
	del *.err
