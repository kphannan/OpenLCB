//------------------------------------------------------------------------
//
// Charlieplex DPH 
//
// Copyright (c) 2009 David Harris,
//   from ideas by Alex Shepherd
//
// file:      CP.cpp
// author:    David Harris
// history:   2009.06.29 Initial Version
//
//------------------------------------------------------------------------
//
// purpose:   Provide a simplified interface to 
//             CharliePlexing from n pins to n(n-1) LEDs
//             EG: 2->2, 3->6, 4->12, 5->20, 6->30, 7->42, 8->56
//------------------------------------------------------------------------

#include "CP.h"

//#define debug
#ifdef debug
 #define P(x) Serial.print(" ");Serial.print(x,DEC)
 #define PT(x) Serial.print(" ");Serial.print(x)
 #define PL(x) Serial.println(x)
#else
 #define P(x) 
 #define PT(x)
 #define PL(x)
#endif


CP::CP(){}

void CP::init(uint8_t n_pins, uint8_t pins[]) {
  // set-up the pins used to drive leds
  num_pins = n_pins;
  num_leds_in_group = n_pins - 1;
  num_leds = n_pins * (n_pins-1);
  PL();PT("init:");P(num_pins);P(num_leds);
  for(int i=0; i<n_pins; i++) {
    PL();PT("pin:");P(i);P(pins[i]);
    drivePins[i] = pins[i];
    setDrivePin(pins[i],0);
  }
  group = pwm = pwm_base = 0;
}

void CP::setDrivePin( uint8_t Pin, uint8_t State ) {
  PT("setDrivePin:");P(Pin);P(State);
  if( State ) {
    digitalWrite( Pin,  HIGH );
    pinMode( Pin, OUTPUT );
  } else {
    pinMode( Pin, INPUT );
    digitalWrite( Pin,  LOW );
  }
}

void CP::getPwmValue(byte led, PWM_VALUE value) {
  PL();PT("get:");P(led);P(value);
  if(led < num_leds) value = pwmValues[led];
}

void CP::setPwmValue(byte led, PWM_VALUE value) {
  PL();PT("set:");P(led);P(value);
  if(led < num_leds) pwmValues[led] = value;
}

bool CP::process(void) {
  int i;
  PL();PT("process:"); P(group);P(pwm);P(pwm_base);
  bool endOfScan = false ;
  if(pwm==0) {
    pCommon = drivePins[group % num_pins] ;
    PL();PT("common=");P(pCommon);
    for(i=1;i<num_pins;i++) {
      PL();P(i);P(drivePins[(group+i) % num_pins]);
      pins[i] = drivePins[(group+i) % num_pins];
    }
    digitalWrite( pCommon, LOW );
    pinMode( pCommon, OUTPUT );
  }
  PL();PT("common=");P(pCommon);
  for(i=1;i<num_pins;i++) {
    PL();P(i);P(pwmValues[pwm_base+i]);
    setDrivePin( pins[i], (pwm > (MAX_PWM_VALUE - pwmValues[pwm_base+i-1])) ? HIGH : LOW );
  }
  if(++pwm >= MAX_PWM_VALUE) {
    PL();PT("End of cycle");
    for(i=1;i<num_pins;i++) setDrivePin( pins[i], LOW );
    pinMode( pCommon, INPUT );
    pwm = 0; group+=1; pwm_base+=num_leds_in_group;
    if(group >= num_pins) {
      pwm = group = pwm_base = 0;
      endOfScan = true;
    }
  }
  return endOfScan ;
}
  
