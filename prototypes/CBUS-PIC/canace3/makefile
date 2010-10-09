all:	mcanace3.hex mcanace3i.hex

mcanace3.hex: canace3.c ../canlib/frametypes.c ../canlib/general.c ../canlib/entry.c ../canlib/ecan.c ../canldr/bootldr.hex
	MCC18 -fo=canace3.o -Oa -Opa- -p18f2480 canace3.c 
	mplink ../canlib/18f2480.lkr canace3.o /i /a INHX32 /o canace3.cof
	hexmerge canace3.hex ../canldr/bootldr.hex mcanace3.hex

interlock.c: layout.txt
	interlock.bat

mcanace3i.hex: canace3.c interlock.c ../canlib/frametypes.c ../canlib/general.c ../canlib/entry.c ../canlib/ecan.c ../canldr/bootldr.hex
	MCC18 -fo=canace3i.o -Oa -Opa- -p18f2480 -DINTERLOCK=1 canace3.c 
	mplink ../canlib/18f2480.lkr canace3i.o /i /a INHX32 /o canace3i.cof
	hexmerge canace3i.hex ../canldr/bootldr.hex mcanace3i.hex

clean:
	del *.o
	del *.cof
	del *.map
	del *.hex
	del *.lib
	del *.lst
	del *.cod
	del *.err
