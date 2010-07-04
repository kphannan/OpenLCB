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


class ButtonLed {
  private:
    long lastTime;
    long flashTime;
    boolean down;
    int sample;
  public:
    byte pin;
    boolean ledState;
    long debounce;
    int sense; // LOW=DOWN, HIGH=UP
    long duration;
    long lastDuration;
    long lperiod;

    ButtonLed(byte p) {
      pin=p;
      debounce=50;
      sense=HIGH;
    }
    ButtonLed(byte p,int s) {
      pin=p;
      sense=s;
      debounce=50;
    }
    void on(long per) {
      lperiod=per;
      ledState=LOW;  // turns LED on
      if(per==0) ledState=HIGH;  // turns LED off
      flashTime=millis();
      pinMode(pin,OUTPUT);
      digitalWrite(pin,ledState); // initialize
    }
    boolean process() {
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
      if(lperiod>1) {
        if((millis()-flashTime)>lperiod) {
          ledState = !ledState;
          flashTime=millis();
          digitalWrite(pin,ledState); 
        }
      }
      return down;
    }
};

        


