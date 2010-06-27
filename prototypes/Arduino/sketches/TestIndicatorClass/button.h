class PushButton {
 private: 
   long _downTime;
   int _pin;
   boolean _down;
 public:
   PushButton(int pin){
      this->_pin = pin;
      this->_down = false;
      pinMode(this->_pin, INPUT);
      digitalWrite(this->_pin, HIGH);
//      this->_downTime = millis();
  }
  long value() {
      long diff;
      if(!this->_down) {				// if button was up
        if(digitalRead(this->_pin) == 0) {		// then if now down..
	  this->_downTime = millis();   // reset timer
	  this->_down = true;
//	  Serial.print("down");
	  return(0);
        }
      return(0);
      }
      if(digitalRead(this->_pin) == 1) {        // if was down, and now up
	diff = millis() - this->_downTime;// calc downtime
	this->_down = false;
//	Serial.print("diff");       
//	Serial.println(diff);
	return (diff);
      }
      return(0);
  }
};

