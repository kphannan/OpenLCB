//------------------------------------------------------------------------
// file: cpdph.h
// Copyright (c) 2009 D. P. Harris
// author:    Alex Shepherd
// history:   2009.06.29 Initial Version
//
//------------------------------------------------------------------------
//
// purpose:   Provide a simplified interface for CharliePlexing 12 LEDs from 4 pins
//
//------------------------------------------------------------------------

#ifndef CPDPH4_IS_IN
#define CPDPH4_IS_IN

#ifndef Wiring_h
#include <WProgram.h>
#endif

#define MAX_DRIVE_PINS 	8
#define MAX_LEDS        (MAX_DRIVE_PINS*(MAX_DRIVE_PINS-1))
#define MAX_PWM_VALUES  (MAX_LEDS)
#define MAX_PWM_VALUE	32

typedef uint8_t PWM_VALUE;

class CP {
  private:
    uint8_t num_pins, num_leds, num_leds_in_group;
    uint8_t drivePins[MAX_DRIVE_PINS];
    PWM_VALUE pwmValues[MAX_LEDS];
    uint8_t pwm, group, pwm_base, pCommon, pins[MAX_DRIVE_PINS];
    void setDrivePin( uint8_t Pin, uint8_t State );
  public:
    CP(void);
    void init(uint8_t n, uint8_t *pins);           // initialize number of pins and pin-numbers
    bool process(void);                            // process one cycle of CHarlieplexing
    void getPwmValue(byte led, PWM_VALUE value);   // get the brightness of a LED
    void setPwmValue(byte led, PWM_VALUE value);   // set the brightness of a LED 
    bool read(uint8_t posn);                   // read the value of a led-position
};

#endif
