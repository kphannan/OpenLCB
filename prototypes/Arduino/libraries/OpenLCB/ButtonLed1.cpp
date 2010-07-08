#include "WConstants.h"
#include "ButtonLed1.h"


ButtonLed::ButtonLed(byte p,int s) {      // define Button with pin and sense, 
  pin=p;                                  // Arduino pin the button/LED is attached
  sense=s;                                // sense: HIGH=active high, LOW=active low. 
  pinMode(pin,OUTPUT);                    // default to driving LED..
  digitalWrite(pin,sense);                // ..and turn it on
  lastButtonState=buttonDown=false;       // init vars
  duration=lastDuration=0;
  ledState=false;                         // start with button off
}

ButtonLed::ButtonLed(byte p) {            // default to sense=HIGH
  buttonLed(p,HIGH);
}

void ButtonLed::on(long mask) {
      pattern = p;
}

void ButtonLed::blink(uint8_t mask) {
  once |= mask;
  // wait for next time step to display
}

void ButtonLed::process() {
    if(bnext && (millis()&0x1f)==0) {             // check button state every 32 ms
      bnext=false;                                // only want to check once per 
      pinMode(pin, INPUT);                        // need to change the pin to input..
      digitalWrite(pin,HIGH);                     // .. and activate pull up
      newButtonDown=(sense==digitalRead(pin));    // is the button up or down
      if(newButtonDown!=lastButtonState) {        // if button changed then..
        lastButtonState=newButtonDown;            // ..remember button state
        return; 
        // now debounced
      } else {                                    // else button position is unchanged..
        if(buttonDown!=newButtonDown) {           // Debounced, but is it a new state?..
          buttonDown=newButtonDown;               // ..yes, so update
          lastDuration=duration + 32;             //    and remember the duration of the last state
          lastButtonTime = millis();              // ..so we can calc duration
          duration = 0;                           // ..start timing ne state
        } else {                                  // else same state continuing, so
          duration = millis() - lastButtonTime;   // .. calculate its duration
        } 
        pinMode(pin,OUTPUT);                      // return pin to output mode
        digitalWrite(pin,ledState);               // and make sure its showing its state
      }
    } else {
      // process LED
      if((millis()&0x1f) != 0) bnext = true;      // partial through this 64ms period, so can trigger next period
    }
    if ( next && (millis()&0x3F) == 0) {          // trigger every 64ms, but only once
      if ((pattern & 0x1) !=0) {                  // if low bit 1 then ..
         ledState = sense;                        // .. update LED and 
         pattern = 0x80000000 | (pattern>>1);     // ..mimic roll with 1 in
      } else {                                    // else low bit is 0, so ..
         ledState = !sense;                        // .. update LED and
         pattern = 0x7FFFFFFF & (pattern>>1);     // .. mimic a roll with a 0 in
      }
      digitalWrite(pin,ledState);
      next = false; 
    } else {
        if ( (millis()&0x3F) != 0) next = true;
    } 
    return;
  }
}

