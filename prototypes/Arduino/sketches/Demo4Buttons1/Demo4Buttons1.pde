// Demo4Button
// Copyright D. Harris
//
// Implements an example producer node with four buttons, and Blue and GOld.  
// 
// To nominate a producer: (blue)+, gold, *or* blue and push a producer-button.
// To teach: gold, (blue)+, gold, *or* gold and then a producer-button.

// Required hardware:
// Arduino with:
//   Gold wired to pin 18.
//   Blue wired to pin 19.
//   Four buttons wired to pins 14-17.
//   Optional indicator-LED wired to pin 13.  

#define OpenLcb
#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>

// The following lines are needed because the Arduino environment 
// won't search a library directory unless the library is included 
// from the top level file (this file)
#include <CAN.h>
#include <EEPROM.h>
class foo{};  // force Arduino environment to treat the rest of this file as C++

#define CEVENTS 0
#define PEVENTS 4

#ifdef OpenLcb
// OpenLcb definitions
#include <OpenLcbCanInterface.h>
#include "OpenLcbCanBuffer.h"
#include "NodeID.h"
#include "EventID.h"

// specific OpenLcb implementations
#include "LinkControl.h"
#include "PCE.h"

OpenLcbCanBuffer     rxBuffer;	// CAN receive buffer
OpenLcbCanBuffer     txBuffer;	// CAN send buffer
OpenLcbCanBuffer*    ptxCAN;

#define BOARD 2    
#define MAN0 5
#define MAN1 1
#define MAN2 1
#define MAN3 1
#define MAN4 1

NodeID nodeid(MAN0,MAN1,MAN2,MAN3,MAN4,BOARD);    // This node's ID
                                   // Temporary ID assigned to DPH

LinkControl link(&txBuffer, &nodeid);

// Events this node can produce, used by PCE
Event pEvents[] = {
    Event(MAN0,MAN1,MAN2,MAN3,MAN4,BOARD,0,11), // head 1, dark
    Event(MAN0,MAN1,MAN2,MAN3,MAN4,BOARD,0,11), // head 1, dark
    Event(MAN0,MAN1,MAN2,MAN3,MAN4,BOARD,0,11), // head 1, dark
    Event(MAN0,MAN1,MAN2,MAN3,MAN4,BOARD,0,11), // head 1, dark
};
#define pEventNum PEVENTS

// Events this node can consume, used by PCE
Event cEvents[] = {};
#define cEventNum CEVENTS

//static boolean nom[pEventNum+cEventNum];

void pceCallback(int index){
  // invoked when an event is consumed
}
PCE p(cEvents, cEventNum, pEvents, pEventNum, &txBuffer, &nodeid, pceCallback);
#else
#endif

// Debug
#define DEBUG
#ifdef DEBUG
 #define PH(x) Serial.print(" ");Serial.print(x,HEX)
 #define PL(x) Serial.println(x)
 #define P(x) Serial.print(x)
#else
 #define P(x)  
 #define PL(x)  
 #define PT(x)  
#endif

#ifdef OpenLcb
//=========================Events=======================================
uint8_t this_node[]    = {MAN0,MAN1,MAN2,MAN3,MAN4,BOARD};
Event event_base = Event(MAN0,MAN1,MAN2,MAN3,MAN4,BOARD,0,0);
static int _nextEvent=0;
uint8_t next_event(uint8_t i) {
  _nextEvent+=1;
  return _nextEvent;
}
bool compare_event(uint8_t event1[], uint8_t event2[]) {
  for(uint8_t i=0; i<8; i++) if(event1[i]!=event2[i]) return 0;
  return 1;
}
#endif
//==indicator led==
uint8_t indicator_rate;
uint8_t indicator_cnt;
#define indicator_pin 13
#define set_indicator(x) indicator_rate=x
#define reset_indicator() indicator_rate=0; digitalWrite(indicator_pin,LOW)
void toggle_indicator() {
  if(indicator_rate==0) digitalWrite(indicator_pin,LOW);
  else if(indicator_cnt==0) {
    indicator_cnt=indicator_rate;
    digitalWrite(13,!digitalRead(13));
  } else indicator_cnt-=1;
}

// Buttons
#include "button.h"
PushButton b0(14);
PushButton b1(15);
PushButton b2(16);
PushButton b3(17);

// blue-gold ================================================================
#define GOLD (digitalRead(19)==0)
#define debounce 50
PushButton gold(19);
PushButton blue(18);
static enum {mIdle, mChoose, mTeach, mTeachChoose} mode;
static int chosen;                   // tracks producer chosen with blue button
static boolean nom[PEVENTS+CEVENTS]; // tracks nominated producers and consumers
void blueGoldProcess() {
  toggle_indicator(); 
  long g=gold.value();
//  long b=blue.value();
  long b=0;
  if(g>5000) {                 // GOLD
                                     Serial.println(" reset");
  } else if(g>debounce) 
// Gold button actions
    switch (mode) 
    {      
    case mIdle:                // go to teach mode
      mode=mTeach;
      set_indicator(1);
      break;
    case mChoose:              // Nominate the chosen button
      mode=mIdle;
      reset_indicator();
      if(chosen<PEVENTS) nom[chosen]=true;
                                                 P(" nom b");PL(chosen);
      break;
    case mTeach:               // Cancel teach mode            
      mode=mIdle;
      reset_indicator();
      break;
    case mTeachChoose:         // 
      mode=mIdle;
      reset_indicator();
      if(nom[0])                                  PL(" teach(b0)");
      if(nom[1])                                  PL(" teach(b1)");
      if(nom[2])                                  PL(" teach(b2)");
      if(nom[3])                                  PL(" teach(b3)");
      break;
    }
  else if(b>5000) {                // Blue
    for(int i=0;i<PEVENTS;i++)   // 5 sec blur ==> reset nominations
      nom[i]=false;
  } else if(b>debounce) 
    switch (mode) {
    case mIdle:                    // co to choose mode
      mode=mChoose;
      chosen=0;
      set_indicator(5);
      break;
    case mTeach:                   // go to teachChoose mode
      mode=mTeachChoose;
      break;
    case mChoose:                  // pick a chosen
    case mTeachChoose:
      chosen+=1;
      if(chosen>PEVENTS) chosen=0;
      break;
    }
    else { // other buttons
      switch(mode) {
      case mIdle:        // just send the associated message
        if(b0.value()>debounce) { p.produce(0);      PL(" sendBO"); }
        if(b1.value()>debounce) { p.produce(1);      PL(" sendB1"); }
        if(b2.value()>debounce) { p.produce(2);      PL(" sendB2"); }
        if(b3.value()>debounce) { p.produce(3);      PL(" sendB3"); }
        break;
      case mChoose:      // can choose with the regular buttons
        if(b0.value()>debounce)      nom[0]=true;
        else if(b1.value()>debounce) nom[1]=true;
        else if(b2.value()>debounce) nom[2]=true;
        else if(b3.value()>debounce) nom[3]=true;;
        break;
      case mTeach:       // short-circuit the teaching
      case mTeachChoose:  
        if(b0.value()>debounce)                           PL("teach(b0)");
        else if(b1.value()>debounce)                      PL("teach(b1)");
        else if(b2.value()>debounce)                      PL("teach(b2)");
        else if(b3.value()>debounce)                      PL("teach(b3)");
        else break;
        mode=mIdle;
        reset_indicator();
        break;
    }
  }
}

//==================SETUP==================================
// init for serial communications
//#define         BAUD_RATE       9600
#define         BAUD_RATE       19200
//#define         BAUD_RATE       333333
static long oldTime;
void setup() {
  // set up serial comm
  Serial.begin(BAUD_RATE);
  // show we've started to run
  Serial.println("Demo4Buttons");
#ifdef OpenLcb
  // Initialize OpenLcb CAN connection
  OpenLcb_can_init();
  // Initialize OpenLcb CAN link controller
  link.reset();
#endif
  oldTime = millis();
  for(int i=0;i<(CEVENTS+PEVENTS);i++) nom[i]=false;
  PL("end of init");
}
//==============LOOP==============================================
void loop() {
#ifdef OpenLcb
  // check for input frames, acquire if present
  boolean rcvFramePresent = OpenLcb_can_get_frame(&rxBuffer);
  // process link control first
  link.check();
  if (rcvFramePresent) {
    // received a frame, ask if changes link state
    link.receivedFrame(&rxBuffer);
  }
  // if link is initialized, higher-level operations possible
  if (link.linkInitialized()) {
     // if frame present, pass to PC handler
     if (rcvFramePresent) {
        p.receivedFrame(&rxBuffer);
     }
     // periodic processing of any PCE state changes
     p.check();
  }
#endif
  if((millis()-oldTime)>20) {  // check every 20 ms. 
    oldTime=millis();
//    P(".");
    blueGoldProcess();
  }
}


// to test (messages in JMRI format)
//    send a CIM frame which should get a RIM: [1002d000]
//    then a RIM which should restart sequence: [17ffd000]

//    send a Verify Node frame of [1b5fd00F] 2 3 4 5 6 7
//    send a Request Consumers frame of [1b7fd00F] 1 2 3 4 5 6 7 8
//    send a Request Producers frame of [1b9fd00F] 8 7 6 5 4 3 2 1
//    send a Request Events frame of [1bbfd00F] 2 3 4 5 6 7

//    produce an event with [1b4fd00F] 8 7 6 5 4 3 2 1
