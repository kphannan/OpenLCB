mcanace3.hex: canace3.c ../canlib/frametypes.c ../canlib/general.c ../canlib/entry.c ../canlib/ecan.c ../canldr/bootldr.hex
	MCC18 -fo=canace3.o -Oa -Opa- -p18f2480 canace3.c 
	mplink ../canlib/18f2480.lkr canace3.o /i /a INHX32 /o canace3.cof
	hexmerge canace3.hex ../canldr/bootldr.hex mcanace3.hex

clean:
	del *.o
	del *.cof
	del *.map
	del *.hex
	del *.lib
	del *.lst
	del *.cod
	del *.err
