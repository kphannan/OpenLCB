avrdude -c diecimila -P ft0 -p m168 -B 4800 -e -u -U efuse:w:0x00:m -U hfuse:w:0xdd:m -U lfuse:w:0xff:m
avrdude -c diecimila -P ft0 -p m168 -u -U flash:w:ATmegaBOOT_168_diecimila.hex -U efuse:w:0x00:m -U hfuse:w:0xdd:m -U lfuse:w:0xff:m -U lock:w:0x0f:m
