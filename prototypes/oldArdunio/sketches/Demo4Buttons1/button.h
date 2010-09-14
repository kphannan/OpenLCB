class PushButton {
 private: 
   long _downTime;
   int _pin;
   boolean _state;
 public:
   PushButton(int pin){
      this->_pin = pin;
      this->_state = false;
      pinMode(this->_pin, INPUT);
      digitalWrite(_pin, HIGH);
      this->_downTime = millis();
  }
  long value() {
      long diff;
      if(!this->_state) {				// if button was up
        if(digitalRead(this->_pin) == 0) {		// then if now down..
	  this->_downTime = millis();   // reset timer
	  this->_state = true;
//	  Serial.print("down");
	  return(0);
        }
      return(0);
      }
      if(digitalRead(_pin) == 1) {        // if was down, and now up
	diff = millis() - this->_downTime;// calc downtime
	this->_state = false;
//	Serial.print("diff");       
//	Serial.println(diff);
	return (diff);
      }
      return(0);
  }
};

