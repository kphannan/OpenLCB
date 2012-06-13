all: mcancs.hex mcancs2.hex

mcancs.hex: cancs.c ../canlib/frametypes.c ../canlib/general.c ../canlib/entry.c ../canlib/ecan.c ../canldr/bootldr.hex
	MCC18 -fo=cancs.o -Oa -Opa- -p18f2480 cancs.c 
	mplink ../canlib/18f2480.lkr cancs.o /i /a INHX32 /o cancs.cof /m cancs.map
	hexmerge cancs.hex ../canldr/bootldr.hex mcancs.hex "OpenLCB for CBUS-PIC Command Station"

clean:
	del *.o
	del *.cof
	del *.map
	del *.hex
	del *.lib
	del *.lst
	del *.cod
	del *.err

