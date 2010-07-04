#ifndef BG_h
#define BG_h

#include <ButtonLed.h>

/**
 * Class for Blue/Gold configuration
 *
 */

class PCE;
class ButtonLed;

class BG {
  public:

  BG(PCE* pce, ButtonLed* cButtons, int nConsumers, ButtonLed* pButtons, int nProducers, ButtonLed* blue, ButtonLed* gold);
  
  void check();
  
  private:
  PCE* pce;
  int nConsumers;
  int nProducers;
  ButtonLed* blue;
  ButtonLed* gold;
  ButtonLed* cButtons;
  ButtonLed* pButtons;
  int index;
  bool lastBlue;
  bool lastGold;
  bool started;
  
};

#endif
