// makes this an Arduino file
#include "WConstants.h"
#include <string.h>

#include "BG.h"

#include "PCE.h"
#include "logging.h"

BG::BG(PCE* p, ButtonLed* bC, int nC, ButtonLed* bP, int nP, ButtonLed* bptr, ButtonLed* gptr) {
      pce = p;
      nConsumers = nC;
      nProducers = nP;
      lastBlue = true;
      cButtons = bC;
      pButtons = bP;
      blue = bptr;
      gold = gptr;
      started = false;
      index = -1;

      for (int i = 0; i<nConsumers; i++)
          cButtons[i].on(0);
      for (int i = 0; i<nProducers; i++)
          pButtons[i].on(0);
      blue->on(0); // turn off 
      lastBlue = blue->process();
      gold->on(0x63); // double blink until intialized
      lastGold = gold->process();
}
  
void BG::check() {
    if (!started) {
        started = true;
        gold->on(1); // turn off waiting to init flash, add heartbeat
    }
    bool temp;
    // check if blue pressed
    if (lastBlue != (temp = blue->process())) {
        lastBlue = temp;
        if (!temp) { // act on down
            // turn off current channel
            if (index>=0 && index<nConsumers) 
                cButtons[index].on(0L);
            if (index>=nConsumers && index<nConsumers+nProducers) 
                pButtons[index-nConsumers].on(0);
            // turn on next
            index++;
            if (index >= nConsumers+nProducers) {
                blue->on(0L);
                index = -1;
            } else if (index>=0 && index<nConsumers) {
                cButtons[index].on(0x01010101L);
                blue->on(~0L);
            } if (index>=nConsumers && index<nConsumers+nProducers) {
                pButtons[index-nConsumers].on(0xEFEFEFEFL);
                blue->on(~0L);
            }
        }
    }
    // check if gold pressed
    if (lastGold != (temp = gold->process())) {
        lastGold = temp;
        if (!temp) { // act on down
            // if gold lit, send message
            if (gold->pattern == ~0L) {
                if (index>=0 && index<nConsumers) {
                    cButtons[index].on(0x0); // off if lit
                    pce->sendTeachC(index);
                } if (index>=nConsumers && index<nConsumers+nProducers) {
                    pButtons[index-nConsumers].on(0x0); // off if lit
                    pce->sendTeachP(index-nConsumers);
                } // otherwise, nothing to do?
                gold->on(0);  // turn off
                blue->on(0);  // turn off
                index = -1;
            // if blue lit without gold, mark channel
            } else if (blue->pattern == ~0L) {
                if (index>=0 && index<nConsumers) {
                    cButtons[index].on(0x0); // off if lit
                    pce->markToLearnC(index, true);
                } if (index>=nConsumers && index<nConsumers+nProducers) {
                    pButtons[index-nConsumers].on(0x0); // off if lit
                    pce->markToLearnP(index-nConsumers, true);
                } // otherwise, nothing to do?
                blue->on(0);  // turn off
                index = -1;
            } else {
                // neither, light gold to start sequence
                gold->on(~0L);
            }
        }
    }
    // process buttons to flash LEDs
    for (int i = 0; i<nConsumers; i++)
        cButtons[i].process();
    for (int i = 0; i<nProducers; i++)
        pButtons[i].process();
    gold->process();
}
  
