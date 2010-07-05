#ifndef BG_h
#define BG_h

#include <ButtonLed.h>

/**
 * Class for Blue/Gold configuration.
 * Works with a PCE instance to do the actual operations.
 * Takes an array of buttons and a corresponding array of patterns
 * to blink for each event producer/consumer to be programmed
 */

class PCE;
class ButtonLed;

class BG {
  public:

  BG(PCE* pce, ButtonLed** buttons, long* patterns, int nEvents, ButtonLed* blue, ButtonLed* gold);
  
  void check();
  
  private:

  PCE* pce;
  ButtonLed** buttons;
  long* patterns;
  int nEvents;
  ButtonLed* blue;
  ButtonLed* gold;

  int index;
  bool lastBlue;
  bool lastGold;
  bool started;
  
};

#endif
