all:	mcanace3.hex mcanace3i.hex

mcanace3.hex: canace3.c ../canlib/frametypes.c ../canlib/general.c ../canlib/entry.c ../canlib/ecan.c ../canldr/bootldr.hex
	MCC18 -fo=canace3.o -Oa -Opa- -p18f2480 canace3.c 
	mplink ../canlib/18f2480.lkr canace3.o /i /a INHX32 /o canace3.cof /m canace3.map
	hexmerge canace3.hex ../canldr/bootldr.hex mcanace3.hex "OpenLCB for CBUS-PIC CANACE3"

mcanace3i.hex: canace3.c ../canlib/frametypes.c ../canlib/general.c ../canlib/entry.c ../canlib/ecan.c ../canldr/bootldr.hex
	MCC18 -fo=canace3i.o -Oa -Opa- -p18f2480 -DINTERLOCK=1 canace3.c 
	mplink ../canlib/18f2480.lkr canace3i.o /i /a INHX32 /o canace3i.cof /m canace3i.map
	hexmerge canace3i.hex ../canldr/bootldr.hex mcanace3i.hex "OpenLCB for CBUS-PIC Interlocked CANACE3"

clean:
	del *.o
	del *.cof
	del *.map
	del *.hex
	del *.lib
	del *.lst
	del *.cod
	del *.err
