//********************************************************
//  fade.cpp
//  d. p. harris
//  from ideas of A.Shepherd
//
//********************************************************

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

#include "cp.h"
#include "fade.h"
#include "fades.h"
#define MAX_LEDS 56
Led_state_t ledStates[MAX_LEDS];
CP   SigLeds ;
uint8_t num_leds;
void populateLedState(byte led, byte stepi)  // transfer the step's values form the cv reggs.
{
  uint16_t fadei = stepi * NUM_VALUES_PER_STEP;
  Led_state_t *ledState = ledStates + led;
  PT("Pop:");P(fadei);
  ledState->fadeTicks  = 0;
  ledState->fadep     = fades[fadei];
  ledState->targetPwm = fades[fadei+1];
  ledState->stepinc   = fades[fadei+2];
  PT("[");P(ledState->fadep);P(ledState->targetPwm);P(ledState->stepinc);PT("]");
}

/*
bool calcFade(byte current, byte target)
{
  PT("calc");P(target);P(current);
  bool stillFading = false;
  if( current > target) {            // fade down
    byte diff = current - target;
    if(diff >= PWM_FADE_VALUE_DOWN) {
      current -= diff/PWM_FADE_VALUE_DOWN;
      stillFading = true;
    } else current = target;
  } else if( current < target) {        // fade up
    byte diff = target - current;
    if(diff >= PWM_FADE_VALUE_UP) {
      current += diff/PWM_FADE_VALUE_UP;
      stillFading = true;
    } else                                // else we are finished
      current = target;
  }
  return stillFading ;
}
*/

bool doFade(uint8_t led, byte target) {
  byte current; 
  SigLeds.getPwmValue(led, current);
  PT("fade");P(led);P(target);P(current);
  bool stillFading = false;
  if( current > target) {            // fade down
    byte diff = current - target;
    if(diff >= PWM_FADE_VALUE_DOWN) {
      current -= diff/PWM_FADE_VALUE_DOWN;
      stillFading = true;
    } else current = target;
  } else if( current < target) {        // fade up
    byte diff = target - current;
    if(diff >= PWM_FADE_VALUE_UP) {
      current += diff/PWM_FADE_VALUE_UP;
      stillFading = true;
    } else                                // else we are finished
      current = target;
  }
  PT("->");P(current);
  SigLeds.setPwmValue(led, current);
  return stillFading ;
}

void setLedState(uint8_t led, uint8_t state) {
  PL();PT("Setstate:");P(led);P(state); 
  if((led >= num_leds ) || (state >= MAX_ASPECTS)) return;
  byte stepi = appearances[state];
  PT("index");P(stepi);
  if(stepi >= MAX_PWM_STEPS) return;
  Led_state_t *ledState = ledStates + led; // ledStates[led]
//  if(ledState->lastState == state) return; // Are we changing the state?, if not ignore it
  ledState->lastState = state;
  ledState->stepi = stepi;
  populateLedState(led, stepi);
  if(ledState->fadep) {
    ledState->fadeTicks = 0;
    ledState->fading = doFade(led, ledState->targetPwm);
  } else {
    SigLeds.setPwmValue(led, ledState->targetPwm);
  }
}

void fade_init(uint8_t n, uint8_t *pins) {
  PT( "Charlieplex setup");
  num_leds=n*(n-1);
  SigLeds.init(4, pins);
  for(uint8_t i = 0; i < num_leds; i++) {
    setLedState( i, 11); // Off
  }
  TIMSK0 |= (1<<OCIE0A);  // Enable Timer0 Compare Match A Interrupt
  TIFR0  |= (1<<OCF0A);   // Clear  Timer0 Compare Match A Flag 
}

volatile byte scanTick;
ISR(TIMER0_COMPA_vect) {
  scanTick++ ;
  OCR0A += 50 ;
}

bool fade_process() {
  if(SigLeds.process() && scanTick) {  /* is Charlieplax scan through the leds finished, and time for action */
    scanTick = 0;
//    PL();PT("fade:");
    for(byte led = 0; led < num_leds; led++) {
      Led_state_t *ledState = ledStates + led;       /* ledstates[led]; */
      PL();PL();PT("proc");P(led);
      if(ledState->fading) {                            // Are we still fading, then recalc the fade values 
        ledState->fading = doFade(led, ledState->targetPwm);
      }
      ledState->fadeTicks++;                             
      if(ledState->fadeTicks >= ledState->fadep) {  // Are we at the end of a fade period?
        if(ledState->stepinc) {                      // ,, no, is there another step, then make it the current step
          PT("next ");P(led);P(ledState->stepi);P(ledState->stepinc);
          ledState->stepi = ledState->stepi + ledState->stepinc;
          P(ledState->stepi);
          populateLedState(led, ledState->stepi);
          ledState->fading = doFade(led, ledState->targetPwm);
        }
      }
    }
    return 1;  // scan processed
  }
  return 0;
}

//*******************************End fade ***************************************
/*
void populateValueForStep(byte led, byte stepIndex)  // transfer the step's values form the cv reggs.
{
  uint16_t fadeIndex = stepIndex * NUM_VALUES_PER_STEP;
  Led_state_t *ledState = ledStates + led;
  PL();PT("Pop:");P(fadeIndex);
  ledState->fadeTicks  = 0;
  ledState->fadePeriod = fades[fadeIndex++];
  ledState->stepValue  = fades[fadeIndex++];
  ledState->nextStep   = fades[fadeIndex];
  P(ledState->fadePeriod);P(ledState->stepValue);P(ledState->nextStep);
}
bool calcFadeValue(byte current, byte final)
{
  bool stillFading = false;
  if( current > final) {            // fade down
    byte diff = current - final;
    if(diff >= PWM_FADE_VALUE_DOWN) {
      current -= diff/PWM_FADE_VALUE_DOWN;
      stillFading = true;
    } else current = final;
  } else if( current < final) {        // fade up
    byte diff = final - current;
    if(diff >= PWM_FADE_VALUE_UP) {
      current += diff/PWM_FADE_VALUE_UP;
      stillFading = true;
    } else                                // else we are finished
      current = final;
  }
  return stillFading ;
}
void setLedState(uint8_t led, uint8_t state) {
  PL();PT("Setstate:");P(led);P(state); 
  if((led >= num_leds ) || (state >= MAX_ASPECTS)) return;
  byte stepIndex = appearances[state];
  PL();PT("index");P(stepIndex);
  if(stepIndex >= MAX_PWM_STEPS) return;
  Led_state_t *ledState = ledStates + led; // ledStates[led]
  if(ledState->lastState == state) return; // Are we changing the state?, if not ignore it
  ledState->lastState = state;
  ledState->stepIndex = stepIndex;
  populateValueForStep(led, stepIndex);
  if(ledState->fadePeriod) {
    ledState->fadeTicks = 0;
    SigLeds.getPwmValue(led, ledState->currentValue);
    ledState->fading = calcFadeValue(ledState->currentValue, ledState->stepValue);
    SigLeds.setPwmValue(led, ledState->currentValue);
  } else {
    SigLeds.setPwmValue(led, ledState->stepValue);
  }
}

void fade_init(uint8_t n, uint8_t *pins) {
  PT( "Charlieplex setup");
  num_leds=n*(n-1);
  SigLeds.init(4, pins);
  for(uint8_t i = 0; i < num_leds; i++) {
    ledStates[i].lastState = 0;
    setLedState( i, 0); // Off
  }
  TIMSK0 |= (1<<OCIE0A);  // Enable Timer0 Compare Match A Interrupt
  TIFR0  |= (1<<OCF0A);   // Clear  Timer0 Compare Match A Flag 
}

volatile byte scanTick;
ISR(TIMER0_COMPA_vect) {
  scanTick++ ;
  OCR0A += 50 ;
}

void fade_process() {
  if(SigLeds.process() && scanTick) {  // is Charlieplax scan through the leds finished, and time for action 
    scanTick = 0;
//    PL();PT("fade:");
    for(byte led = 0; led < num_leds; led++) {
      Led_state_t *ledState = ledStates + led;       // ledstates[led]; 
//      P(ledIndex);
      if(ledState->fading) {                            // Are we still fading, then recalc the fade values 
        PT("fading ");P(ledState->currentValue);
        SigLeds.getPwmValue(led, ledState->currentValue);
        ledState->fading = calcFadeValue(ledState->currentValue, ledState->stepValue);
        SigLeds.setPwmValue(led, ledState->currentValue);
        P(ledState->currentValue);
      }
      ledState->fadeTicks++;                             
      if(ledState->fadeTicks >= ledState->fadePeriod) {  // Are we at the end of a fade period?
        if(ledState->nextStep) {                      // ,, no, is there another step, then make it the current step
          PL();PT("next ");P(ledState->stepIndex);P(ledState->nextStep);
          ledState->stepIndex = ledState->stepIndex + ledState->nextStep;
          P(ledState->stepIndex);
          populateValueForStep(led, ledState->stepIndex);
          SigLeds.getPwmValue(led, ledState->currentValue);
          ledState->fading = calcFadeValue(ledState->currentValue, ledState->stepValue);
          SigLeds.setPwmValue(led, ledState->currentValue);
        }
      }
    }
  }
}

//*******************************End fade ***************************************
*/
