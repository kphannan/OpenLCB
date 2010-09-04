all: mcanacc8.hex mcanservo.hex mcanace8c.hex mcanacc4.hex

mcanacc8.hex: canacc.c ../canlib/frametypes.c ../canlib/general.c ../canlib/ecan.c ../canlib/eeprom.c ../canlib/hash.c ../canldr/bootldr.hex
	MCC18 -fo=canacc8.o -Oa -Opa- -p18f2480 -DACC8=1 canacc.c 
	mplink ../canlib/18f2480.lkr canacc8.o /i /a INHX32 /o canacc8.cof
	hexmerge canacc8.hex ../canldr/bootldr.hex mcanacc8.hex

mcanservo.hex: canacc.c ../canlib/frametypes.c ../canlib/general.c ../canlib/ecan.c ../canlib/eeprom.c ../canlib/hash.c ../canldr/bootldr.hex
	MCC18 -fo=canservo.o -Oa -Opa- -p18f2480 -DSERVO=1 canacc.c 
	mplink ../canlib/18f2480.lkr canservo.o /i /a INHX32 /o canservo.cof
	hexmerge canservo.hex ../canldr/bootldr.hex mcanservo.hex

mcanace8c.hex: canacc.c ../canlib/frametypes.c ../canlib/general.c ../canlib/ecan.c ../canlib/eeprom.c ../canlib/hash.c ../canldr/bootldr.hex
	MCC18 -fo=canace8c.o -Oa -Opa- -p18f2480 -DACE8C=1 canacc.c 
	mplink ../canlib/18f2480.lkr canace8c.o /i /a INHX32 /o canace8c.cof
	hexmerge canace8c.hex ../canldr/bootldr.hex mcanace8c.hex

mcanacc4.hex: canacc.c ../canlib/frametypes.c ../canlib/general.c ../canlib/ecan.c ../canlib/eeprom.c ../canlib/hash.c ../canldr/bootldr.hex
	MCC18 -fo=canacc4.o -Oa -Opa- -p18f2480 -DACC4=1 canacc.c 
	mplink ../canlib/18f2480.lkr canacc4.o /i /a INHX32 /o canacc4.cof
	hexmerge canacc4.hex ../canldr/bootldr.hex mcanacc4.hex

clean:
	del *.o
	del *.cof
	del *.map
	del *.hex
	del *.lib
	del *.lst
	del *.cod
	del *.err


