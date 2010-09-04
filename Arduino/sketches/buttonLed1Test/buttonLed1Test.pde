// ButtonLED class
//
// Define a button: buttonLed b0(pin) or b0(pin,LOW) or b0(pin,HIGH)
// Periodically call b0.process(), it will return true if the button is down, else false
// b0.duration will return how long the button has been in its current state in ms
// b0.lastDuration will return how long it was in the last state, in msec
//
// b0.on(period) will turn on the LED, and flash at period in ms,
//     period=0 is off, period=1 is on

// Hardware is:
//  +5V---R1---LED>|----pin----R2----button---gnd
// where R1 is 0.5-1k, R2 is >0.5k.
// Values of 500 and 1k work well for cheap LEDs. 
// For high intensity LEDs, R1 should perhaps be 1k.  
#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>

class foo{};

#include "ButtonLed1.h"

buttonLed b0(14,LOW);
buttonLed b1(15,LOW);
buttonLed b2(16,LOW);
buttonLed b3(17,LOW);
buttonLed blue(18,LOW);
buttonLed gold(19,LOW);

void setup() {
  Serial.begin(19200);
  Serial.println("Test buttonLed");
 
  b0.on(0x33303303); 
  b1.on(0x0000FFFF); 
  b2.on(0x00000033); 
  b3.on(0xFF00FF00);
  blue.on(0x0000FFFF); delay(1000);
  gold.on(0x33303303);
}

boolean goldDown;

void loop() {
 b0.process();
 b1.process();
 b2.process();
 b3.process();
 blue.process();
 gold.process();

 if(b0.buttonDown) {
   b0.pattern=0xFFFF0005L;
   Serial.println("b0 down");
 }
 
 // report button pushes
 if(b1.buttonDown) Serial.println("b1 down");
 if(b2.buttonDown) Serial.println("b2 down");
 if(b3.buttonDown) Serial.println("b3 down");
 if(blue.buttonDown) Serial.println("blue down");

 // report how long gold was down
 if(gold.buttonDown) { goldDown=true;}  // only report once
 if((!gold.buttonDown) & goldDown) { Serial.print("gold up"); Serial.println(gold.lastDuration);goldDown=false;}
 // do something special on extended gold-down
 if(gold.buttonDown && gold.duration>1000) {
   Serial.println("gold x 1 sec");
   b0.pattern = 0L;
   b1.pattern = 0L;
   b2.pattern = 0L;
   b3.pattern = 0L;
   blue.pattern = 0L;
   gold.pattern = 0L;
 }
}
        
        

