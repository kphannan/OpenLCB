# Simple build for Arduino sketch, somewhat brute force.
# First line is local install location for Arduino tools

ROOT := /Applications/Arduino.app/Contents/Resources/Java/

INCLUDE_OPTIONS := -I${ROOT}hardware/cores/arduino -I${ROOT}hardware/libraries/CAN -I${ROOT}hardware/libraries/EEPROM -I${PWD}/../../libraries/OpenLCB

TMPDIR := $(shell mktemp -d /tmp/Arduino.XXXXXXXX)

PDE := $(foreach V,$(wildcard *.pde),$(V:.pde=))

all:
	echo ${PLACE}
	cp ${PDE}.pde ${TMPDIR}/${PDE}.cpp
	cd ${TMPDIR}; ${ROOT}hardware/tools/avr/bin/avr-g++ -c -g -Os -w -fno-exceptions -ffunction-sections -fdata-sections -std=gnu99 -mmcu=atmega168 -DF_CPU=16000000L ${INCLUDE_OPTIONS} *.cpp -o /dev/null
	rm -r ${TMPDIR}