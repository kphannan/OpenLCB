#include "WConstants.h"
#include "ButtonLed.h"


ButtonLed::ButtonLed(uint8_t p) : pin(p) {
  debounce=50;
  next = true;
}

void ButtonLed::on(long mask) {
  pattern = mask;
  ledState=HIGH;  // turns LED off
  if( (pattern&0x1) != 0) ledState=LOW;  // turns LED on
  next = true;
  pinMode(pin,OUTPUT);
  digitalWrite(pin,ledState); // initialize
}

void ButtonLed::process() {
  int s;
  sample+=1; 
  if(sample>10) {           // sample button every 10 calls

    // do input setup
    pinMode(pin, INPUT);
    digitalWrite(pin,HIGH); // activate pull up
    duration = millis()-lastTime; // do a little work to wait
    sample=0;               // reset cycle counter

    // take input
    s=digitalRead(pin);
    
    // set pin back to output
    pinMode(pin,OUTPUT);
    digitalWrite(pin,ledState); 

    // do state change and debounce logic
    if (waiting) {
        // waiting for debounce to complete
        if (duration>debounce) {
            // debounce complete
            waiting = false;
            state = s;
        }
    } else if (s!=state) {
        // not waiting for debounce and start of change
        lastDuration = duration;
        lastTime = millis();
        waiting = true;
    }
  }
  // drive output pattern
  if ( next && (millis()&0x3F) == 0) {
    // time for next bit
    if ((pattern & 0x1) !=0) {
       ledState = LOW;
       pattern = 0x80000000 | (pattern>>1);
    } else {
       ledState = HIGH;
       pattern = 0x7FFFFFFF & (pattern>>1);
    }
    digitalWrite(pin,ledState);
    next = false;
  } else {
    // wait for next bit
    if ( (millis()&0x3F) != 0) next = true;
  }
}

