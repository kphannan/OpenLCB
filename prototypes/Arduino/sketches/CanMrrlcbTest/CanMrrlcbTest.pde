//===========================================================
// CanMrrlcbTest
//   Developing (eventual) classes for OpenLCB
// 
//   Bob Jacobsen 2009
//      based on examples by Alex Shepherd and David Harris
//===========================================================
#include "WProgram.h"
#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>

// The following lines are needed because the Arduino environment 
// won't search a library directory unless the library is included 
// from the top level file (this file)
#include <CAN.h>
#include <EEPROM.h>

// init for serial communications
#define         BAUD_RATE       115200
//#define         BAUD_RATE       333333

// demo I/O pins
#define CONSUMER_PIN 9
#define PRODUCER_PIN 14
int producer_pin_record;

// OpenLCB definitions
#include "OpenLcbCanInterface.h"
#include "OpenLcbCanBuffer.h"
#include "NodeID.h"
#include "EventID.h"

// specific OpenLCB implementations
#include "LinkControl.h"
#include "PCE.h"

OpenLcbCanBuffer     rxBuffer;	// CAN receive buffer
OpenLcbCanBuffer     txBuffer;	// CAN send buffer
OpenLcbCanBuffer*    ptxCAN;

NodeID nodeid(2,3,4,5,6,7);    // This node's ID

LinkControl link(&txBuffer, &nodeid);

// Events this node can produce and consume, used by PCE
Event events[] = {
    Event(1,2,3,4,5,6,7,8), 
    Event(17,18,19,20,21,22,23,24),
    Event(33,34,35,36,37,38,39,40), 
    Event(49,50,51,52,53,54,55,56)
};
int eventNum = 4;

void pceCallback(int index){
  // invoked when an event is consumed
  Serial.print("consume ");Serial.println(index);
  if (index == 0) {
    digitalWrite(CONSUMER_PIN, LOW);
  } else if (index == 1) {
    digitalWrite(CONSUMER_PIN, HIGH);
  } else {
      Serial.print("error index ");Serial.println(index);
  }
}
PCE p(events, eventNum, &txBuffer, &nodeid, pceCallback);

/**
 * This setup is just for testing
 */
void setup()
{
  // set up serial comm
  Serial.begin(BAUD_RATE);
  
  // show we've started to run
  Serial.println();Serial.println("\nStarting CanMrrlcbTest\n");

  // Initialize test I/O pins
  pinMode(CONSUMER_PIN,OUTPUT);
  digitalWrite(CONSUMER_PIN,HIGH);
  pinMode(PRODUCER_PIN,INPUT);
  digitalWrite(PRODUCER_PIN,HIGH);
  
  // Initialize OpenLCB CAN connection
  OpenLcb_can_init();
  
  // Initialize OpenLCB CAN link controller
  link.reset();
}

void loop() {
  // check for input frames, acquire if present
  bool rcvFramePresent = OpenLcb_can_get_frame(&rxBuffer);
  
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

     // Demo: handle possible production of events from pin
     if (producer_pin_record != digitalRead(PRODUCER_PIN)) {
         producer_pin_record = digitalRead(PRODUCER_PIN);
         if (producer_pin_record == LOW) {
             Serial.println("p0");
             p.produce(0);
         } else {
             Serial.println("p1");
             p.produce(1);
         }
     }
  }

}



