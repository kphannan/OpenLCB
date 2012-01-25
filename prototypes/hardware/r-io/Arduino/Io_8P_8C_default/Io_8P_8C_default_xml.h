#ifndef __IO_8P_8C_DEFAULT_XML_H__
#define __IO_8P_8C_DEFAULT_XML_H__


//see http://arduino.cc/en/Reference/PROGMEM

#include <avr/pgmspace.h>

//TODO why is this generating the warning: "only initialized variables can be placed into program memory area". This is initialized, right!?
char CDIXML[] PROGMEM = "<?xml><cdi><id><ma>Railstars Limited</ma><model>Io Developers Kit</mo><ha>1.0</ha><so>1.0</so></id><se name=\"Location\" origin=\"#00\" space=\"#00\"><char name=\"Location\" size=\"64\"></se><se name=\"Data\" origin=\"#00\" space=\"#01\"><na>Producers/Consumers</na><de>Producer/Consumer EventIDs.</de><gr replication=\"16\"><na>Producers</na><de>8 inputs with two states</de></gr><gr replication=\"16\"><na>Consumers</na><de>8 inputs with two states</de></gr></se></cdi>";

#endif
