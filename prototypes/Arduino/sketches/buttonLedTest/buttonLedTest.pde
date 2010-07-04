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


class buttonLed {
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

    buttonLed(byte p) {
      pin=p;
      debounce=50;
      sense=HIGH;
    }
    buttonLed(byte p,int s) {
      pin=p;
      sense=s;
      debounce=50;
    }
    void on(long per) {
      lperiod=per;
      ledState=HIGH;
      if(per==0) ledState=LOW;
      flashTime=millis();
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
      }
      if(lperiod>1) {
        if((millis()-flashTime)>lperiod) {
          ledState = !ledState;
          flashTime=millis();
        }
        pinMode(pin,OUTPUT);
        digitalWrite(pin,ledState); 
      }
      return down;
    }
};

buttonLed b0(15,LOW);
static boolean ob = false;
static boolean rst = false;
void setup() {
  Serial.begin(19200);
  Serial.println("Test buttonLed");
  b0.on(1000);
}

void loop() {
  boolean b=b0.process();
  if(b){                                              // if button down
    if(b0.duration>3000) {
      Serial.println("Three");     // do something if its down for more than 3 sec
//      b0.on(50L);     // <--- doesn't work !?
    }
    else if(b0.duration>2000) Serial.println("Two");  // ..if 2 sec
    else if(b0.duration>1000) Serial.println("One");  // ..if 1 sec
  }
  if(b!=ob) {
    if(b) {
      Serial.print("down ");
      Serial.println(b0.lastDuration); // How long was the button up for?
    } else {
      b0.on(b0.lastDuration);
      Serial.print("up ");
      Serial.println(b0.lastDuration); // How long was the button donw?
    }
    ob=b;
  }
  if(!rst && !b && b0.lastDuration>3000) {
    Serial.println("Reset"); // reset if button held down more than 3 seconds. 
    b0.on(25);               // very rapid flash
    rst = true;
  }
//  delay(5);   // include to mimic other processing.
}
        
        

