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
    long    lastChangeTime;         // used for debounce
    long    lastFlashTime;          // how often to flash
    boolean lastButtonState;        // true=down, false=up (depends on sense)
    long    lastButtonTime;         // time present buttonstate started
    int     sampleCounter;          // periodic sampling of button
    boolean newButtonDown;          // instant state of button
  public:
    byte    pin;
    boolean ledState;
    boolean buttonDown;       // button state, down=true, up=false
    long    debouncePeriod;
    int     sense; // LOW=DOWN, HIGH=UP
    long    duration;
    long    lastDuration;
    long    flashPeriod;

    buttonLed(byte p,int s) {
      pin=p;
      sense=s;
      debouncePeriod=50;
      lastButtonState=buttonDown=false;
      duration=sampleCounter=flashPeriod=lastDuration=0;
      lastFlashTime=lastChangeTime=lastButtonTime=millis();
      ledState=false;
    }
    buttonLed(byte p) {
      buttonLed(p,HIGH);
    }
    void on(long per) {
      flashPeriod=per;
      ledState=sense;
      if(per==0) ledState=!ledState;
      lastFlashTime=millis();
    }
    
    // Process button state and led state
    boolean process() {
      sampleCounter+=1; 
      if(sampleCounter>=10) {                 // sample button every nth cycle
        sampleCounter=0;                      // reset cycle counter
        pinMode(pin, INPUT);                  // need to change the pin to input..
        digitalWrite(pin,HIGH);               // .. and activate pull up
        newButtonDown=(sense==digitalRead(pin)); // is the button up or down
        if(newButtonDown!=lastButtonState) {  // if button changed then..
          lastButtonState=newButtonDown;      // ..remember button state
          lastChangeTime=millis();            // ..and start timing
          return buttonDown; 
        } else {                              // else button position is unchanged..
                                              // ..is it debounced?
          duration=millis()-lastButtonTime;   // calc how long in this state
          if(duration<debouncePeriod) 
             return buttonDown;               // not debounced, try next time
                                              //
          if(buttonDown!=newButtonDown) {     // Deboundes, but is it a new state?..
            buttonDown=newButtonDown;         // ..yes, so update
            lastDuration=duration;            //    and remember the duration of the last state
            lastButtonTime=millis()+debouncePeriod; //   and the present state has been at 
                                              //         least debounceTime
          }
          pinMode(pin,OUTPUT);                 // put the 
          digitalWrite(pin,ledState);
        }
        if(flashPeriod>1) {                           // do flash calc
          if((millis()-lastFlashTime)>flashPeriod) {  // is it time? ..
            ledState = !ledState;                     // ..yes, so flip
            lastFlashTime=millis();                   //   and remember the time
            digitalWrite(pin,ledState);                 // update the pin
          }
        } 
      }
      return buttonDown;
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
    else if(b0.duration>2000) {
      Serial.println("Two");  // ..if 2 sec
      b0.on(50L);     // <--- doesn't work !?
    }
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
//  delay(1000);   // include to mimic other processing.
}
        
        

