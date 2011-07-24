All: mcanrs.hex mcanusb.hex

mcanrs.hex: canrs.c ../canlib/general.c ../canlib/frametypes.c ../canlib/entry.c ../canlib/ecan.c ../canlib/serial.c ../canldr/rs232ldr.hex
	MCC18 -fo=canrs.o -Oa -Opa- -p18f2480 -DRS232=1 canrs.c 
	mplink ../canlib/18f2480.lkr canrs.o /a INHX32 /i /o canrs.cof /m canrs.map
	hexmerge canrs.hex ../canldr/rs232ldr.hex mcanrs.hex "OpenLCB for CBUS-PIC CANRS232"

mcanusb.hex: canrs.c ../canlib/general.c ../canlib/frametypes.c ../canlib/entry.c ../canlib/ecan.c ../canlib/serial.c ../canldr/usbldr.hex
	MCC18 -fo=canusb.o -Oa -Opa- -p18f2480 -DUSB=1 canrs.c 
	mplink ../canlib/18f2480.lkr canusb.o /a INHX32 /i /o canusb.cof /m canusb.map
	hexmerge canusb.hex ../canldr/usbldr.hex mcanusb.hex "OpenLCB for CBUS-PIC CANUCB"

clean:
	del *.o
	del *.cof
	del *.map
	del *.hex
	del *.lib
	del *.lst
	del *.cod
	del *.err



