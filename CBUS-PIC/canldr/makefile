All: bootldr.hex usbldr.hex rs232ldr.hex

bootldr.hex: bootldr.c ../canlib/general.c ../canlib/frametypes.c ../canlib/ecan.c ../canlib/eeprom.c LDR18f2480.lkr
	MCC18 -fo=bootldr.o -Oa -Opa- -DCAN=1 -p18f2480 bootldr.c 
	mplink LDR18f2480.lkr bootldr.o /i /o bootldr.cof

usbldr.hex: bootldr.c ../canlib/general.c ../canlib/frametypes.c ../canlib/serial.c ../canlib/eeprom.c LDR18f2480.lkr
	MCC18 -fo=usbldr.o -Oa -Opa- -DUSB=1 -p18f2480 bootldr.c 
	mplink LDR18f2480.lkr usbldr.o /i /o usbldr.cof /m usbldr.map

rs232ldr.hex: bootldr.c ../canlib/general.c ../canlib/frametypes.c ../canlib/serial.c ../canlib/eeprom.c LDR18f2480.lkr
	MCC18 -fo=rs232ldr.o -Oa -Opa- -DRS232=1 -p18f2480 bootldr.c 
	mplink LDR18f2480.lkr rs232ldr.o /i /o rs232ldr.cof

clean:
	del *.o
	del *.cof
	del *.map
	del *.hex
	del *.lib
	del *.lst
	del *.cod
	del *.err

