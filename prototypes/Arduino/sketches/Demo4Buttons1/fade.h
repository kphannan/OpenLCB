// fade.h
//#ifdef FADE_H
//#define FADE_H

#define MAX_ASPECTS             10
#define MAX_PWM_STEPS           16
#define NUM_VALUES_PER_STEP      3
typedef struct {
  byte        lastState;
  bool        fading;                   // are we fading the last appearance?
  byte        fadep;               // current
  byte        fadeTicks;
  byte        stepinc;                 // current
  byte        stepi; 
  byte        targetPwm;
} Led_state_t ;
#define PWM_FADE_VALUE_DOWN 16
#define PWM_FADE_VALUE_UP 32


void populatePwm(byte led, byte stepi);  // transfer the step's values form the cv reggs.
bool calcFade(byte current, byte target);

extern void fade_init(uint8_t n, uint8_t *pins);
extern void setLedState(uint8_t led, uint8_t state);
extern bool fade_process(); 

//#endif
