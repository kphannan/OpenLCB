#include <avr/pgmspace.h>

void setup() {                
  Serial.begin(115200);
  delay(10);
  
  Serial.println(pgm_read_byte(0x1FFF9),HEX);
  Serial.println(pgm_read_byte(0x1FFFA),HEX);
  Serial.println(pgm_read_byte(0x1FFFB),HEX);
  Serial.println(pgm_read_byte(0x1FFFC),HEX);
  Serial.println(pgm_read_byte(0x1FFFD),HEX);
  Serial.println(pgm_read_byte(0x1FFFE),HEX);
}

void loop() {
}
