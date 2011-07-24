all: mcanlcd.hex

mcanlcd.hex: canlcd.c ../canlib/general.c ../canlib/frametypes.c ../canlib/entry.c ../canlib/ecan.c ../canldr/bootldr.hex
	MCC18 -fo=canlcd.o -Oa -Opa- -p18f2480 canlcd.c 
	mplink ../canlib/18f2480.lkr canlcd.o /a INHX32 /i /o canlcd.cof /m canlcd.map
	hexmerge canlcd.hex ../canldr/bootldr.hex mcanlcd.hex "OpenLCB for CBUS-PIC CANLCD"

clean:
	del *.o
	del *.cof
	del *.map
	del *.hex
	del *.lib
	del *.lst
	del *.cod
	del *.err
