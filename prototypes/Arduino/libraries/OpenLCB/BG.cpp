// makes this an Arduino file
#include "WConstants.h"
#include <string.h>

#include "BG.h"

#include "PCE.h"

#define UNREADY_BLINK 0xFF00FFL
#define READY_BLINK   0x1L

BG::BG(PCE* pc, ButtonLed** bC, long* pt, int n, ButtonLed* bptr, ButtonLed* gptr) {
      pce = pc;
      buttons = bC;
      patterns = pt;
      nEvents = n;
      blue = bptr;
      gold = gptr;

      lastBlue = true;
      started = false;
      index = -1;

      // all buttons off (might be redundant, as buttons can appear twice)
      for (int i = 0; i<nEvents; i++)
          buttons[i]->on(0);

      // initial blue/gold setting
      blue->on(0); // turn off 
      blue->process();
      lastBlue = blue->state;
      gold->on(UNREADY_BLINK); // unready blink until intialized
      gold->process();
      lastGold = gold->state;
}
  
void BG::check() {
    if (!started) {
        started = true;
        gold->on(READY_BLINK); // turn off waiting to init flash, start heartbeat ready blink
    }

    // check if blue pressed
    blue->process();
    if (lastBlue != blue->state) {
        lastBlue = blue->state;
        if (!lastBlue) { // act on button down
            // turn off current channel
            if (index>=0 && index<nEvents) 
                buttons[index]->on(0L);
            // turn on next
            index++;
            if (index >= nEvents) {  // off end, turn off blue
                blue->on(0L);
                index = -1;
            } else if (index>=0 && index<nEvents) {  // blink next light and turn on blue
                buttons[index]->on(patterns[index]);
                blue->on(~0L);
            }
        }
    }

    // check if gold pressed
    gold->process();
    if (lastGold != gold->state) {
        lastGold = gold->state;
        if (!lastGold) { // act on down
            // if gold lit, send message
            if (gold->pattern == ~0L) {
                if (index>=0 && index<nEvents) {
                    buttons[index]->on(0x0); // off if lit
                    pce->sendTeach(index);
                } // otherwise, nothing to do?
                gold->on(READY_BLINK);  // turn off (back to ready blink)
                blue->on(0);  // turn off
                index = -1;
            // if blue lit without gold, mark channel
            } else if (blue->pattern == ~0L) {
                if (index>=0 && index<nEvents) {
                    buttons[index]->on(0x0); // off if lit
                    pce->markToLearn(index, true);
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
    for (int i = 0; i<nEvents; i++) {
        buttons[i]->process();
    }
}
  
