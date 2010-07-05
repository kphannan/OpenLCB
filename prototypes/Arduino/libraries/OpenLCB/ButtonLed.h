// ButtonLED class
//
// Define a button: buttonLed b0(pin) or b0(pin,LOW) or b0(pin,HIGH)
// Periodically call b0.process(), it will return true if the button is down, else false
// b0.duration will return how long the button has been in its current state in ms
// b0.lastDuration will return how long it was in the last state, in msec
//
// b0.on(pattern) will drive the LED from the given pattern, LSB first
//     0 bits in pattern are off, 1 bits are on
//     each bit shows for 64 msec, aligned across all ButtonLed instances
//  full off:             0x0L
//  full on:             ~0x0L
//  slowest flash: 0x0000FFFFL
//  fastest flash: 0x55555555L
//  blink:         0x00000003L
//  double blink:  0x00000033L
//  wink:          0xFFFFFFFEL

// Hardware is:
//  +5V---R1---LED>|----pin----R2----button---gnd
// where R1 is 0.5-1k, R2 is >0.5k.
// Values of 500 and 1k work well for cheap LEDs. 
// For high intensity LEDs, R1 should perhaps be 1k.  

#include <stdint.h>

class ButtonLed {
  private:
    long lastTime;
    bool down;
    int sample;
    bool next;
  public:
    uint8_t pin;
    bool ledState;
    long debounce;
    int sense; // LOW=DOWN, HIGH=UP
    long duration;
    long lastDuration;
    long pattern;

    ButtonLed(uint8_t p) {
      pin=p;
      debounce=50;
      sense=HIGH;
      next = true;
    }
    ButtonLed(uint8_t p,int s) {
      pin=p;
      sense=s;
      debounce=50;
      next = true;
    }
    void on(long mask) {
      pattern = mask;
      ledState=HIGH;  // turns LED off
      if( (pattern&0x1) != 0) ledState=LOW;  // turns LED on
      next = true;
      pinMode(pin,OUTPUT);
      digitalWrite(pin,ledState); // initialize
    }
    bool process() {
      int s;
      sample+=1; 
      if(sample>10) {          // sample button every 100 processes
        pinMode(pin, INPUT);    // so input
        digitalWrite(pin,HIGH); // activate pull up
        sample=0;               // reset cycle counter
        duration=millis()-lastTime;
        if(down) {  // button was down
          if(duration>debounce) { // definitely down
             down=true;
          }
          s=digitalRead(pin);
          if(s!=sense) {       // button changed to up
            lastDuration=duration;
            duration=0;
            down=false;        // tag it
            lastTime=millis(); // start timing
          } 
        } else { // button was up
          if(duration>debounce) { // definitely up
             down=false;
          }
          s=digitalRead(pin);
          if(s==sense) {       // button changed to down
            lastDuration=duration;
            duration=0;
            down=true;         // remember
            lastTime=millis(); // start timing
          }
        }
        // and set back to output
        pinMode(pin,OUTPUT);
        digitalWrite(pin,ledState); 
      }
      if ( next && (millis()&0x3F) == 0) {
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
        if ( (millis()&0x3F) != 0) next = true;
      }
      return down;
    }
};

        


