// Indicator Class

// Indicator(pin) initializes an indicator on an Arduino pin
// set(period) sets the indicator flashing at the period in msec
//  set(0) is a special case, sets the indicator on permanently.  
// reset() resets the indocator to off (period==0)
// process() needs to be called often.    

#include <WProgram.h>
#include "button.h"

class Indicator {
 private:
  long _lastTime;
  long _period;
  int _pin;
  boolean _state;
 public:
  Indicator(int pin) {
    _pin = pin;
    pinMode(_pin, OUTPUT);
    digitalWrite(_pin, LOW);
  }
  void reset() {
    digitalWrite(_pin, LOW);
    _period=0;
    _state = false;
  }
  void set(long period) {
    _period = period;
    _lastTime = millis();
    digitalWrite(_pin, HIGH);  // turn it on
    _state=true;
  }
  void process() {
    long now;
    if(_period==0) return;
    now = millis();
    if((now-_lastTime)>=_period) {
      if(_state==true) { 
        digitalWrite(_pin, LOW); 
        _state=false; 
        _lastTime=now;
      }
      else {
        digitalWrite(_pin, HIGH);
        _state=true;
        _lastTime=now;
      }
    }
  }
};

// Test Indocator Class
//
// LED on pin 13
// Button on pin 14
//
// LED is turned on for two seconds, then
// watches the button, and flashes the LED at the period that 
// the button is held down.  

Indicator led(13);
PushButton b1(14);
void setup() {
//  Serial.begin(19200);
//  Serial.println("Test Indicator Class");
//  Serial.println("LED on");
  led.set(0);
  delay(2000);
  led.reset();
}

void loop() {
  long p;
  p=b1.value();
  if(p>10) {
    led.set(p);
  }
  led.process();
}
