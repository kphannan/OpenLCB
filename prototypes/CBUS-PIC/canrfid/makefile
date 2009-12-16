All: mcanrfid.hex

mcanrfid.hex: canrfid.c ../canlib/general.c ../canlib/frametypes.c ../canlib/entry.c ../canlib/ecan.c ../canlib/serial.c ../canldr/bootldr.hex
	MCC18 -fo=canrfid.o -Oa -Opa- -p18f2480 -DRS232=1 canrfid.c 
	mplink ../canlib/18f2480.lkr canrfid.o /a INHX32 /i /o canrfid.cof
	hexmerge canrfid.hex ../canldr/rs232ldr.hex mcanrfid.hex

clean:
	del *.o
	del *.cof
	del *.map
	del *.hex
	del *.lib
	del *.lst
	del *.cod
	del *.err
