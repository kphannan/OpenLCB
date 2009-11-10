//===========================================================
// CanMrrlcbTest
//   Developing (eventual) classes for NmraNet S9.6
// 
//   Bob Jacobsen 2009
//      based on examples by Alex Shepherd and David Harris
//===========================================================
#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>

// The following line is needed because the Arduino environment 
// won't search a library directory unless the library is included 
// from the top level file (this file)
#include <CAN.h>

#include "WProgram.h"
void pceCallback(int index);
void setup();
void loop();
class foo{};  // force Arduino environment to treat the rest of this file as C++

// init for serial communications
#define         BAUD_RATE       115200
//#define         BAUD_RATE       333333

// demo I/O pins
#define CONSUMER_PIN 9
#define PRODUCER_PIN 14
int producer_pin_record;

// NMRAnet definitions
#include "NmraNetCanInterface.h"
#include "NmraNetCanBuffer.h"
#include "NodeID.h"
#include "EventID.h"

// specific NMRAnet implementations
#include "LinkControl.h"
#include "PCE.h"

NmraNetCanBuffer     rxBuffer;	// CAN receive buffer
NmraNetCanBuffer     txBuffer;	// CAN send buffer
NmraNetCanBuffer*    ptxCAN;

NodeID nodeid(2,3,4,5,6,7);    // This node's ID

LinkControl link(&txBuffer, &nodeid);

// Events this node can produce, used by PCE
Event pEvents[] = {
    Event(1,2,3,4,5,6,7,8), 
    Event(8,7,6,5,4,3,2,1)
};
int pEventNum = 2;

// Events this node can consume, used by PCE
Event cEvents[] = {
    Event(1,2,3,4,5,6,7,8), 
    Event(8,7,6,5,4,3,2,1)
};
int cEventNum = 2;

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
PCE p(cEvents, cEventNum, pEvents, pEventNum, &txBuffer, &nodeid, pceCallback);

/**
 * This setup is just for testing
 */
void setup()
{
  // set up serial comm
  Serial.begin(BAUD_RATE);
  
  // show we've started to run
  Serial.println();Serial.println("Starting CanMrrlcbTest");
  
  // Initialize test I/O pins
  pinMode(CONSUMER_PIN,OUTPUT);
  digitalWrite(CONSUMER_PIN,HIGH);
  pinMode(PRODUCER_PIN,INPUT);
  digitalWrite(PRODUCER_PIN,HIGH);
  
  // Initialize NmraNet CAN connection
  NMRAnet_can_init();
  
  // Initialize NmraNet CAN link controller
  link.reset();
}

void loop() {
  delay(10);
  // check for input frames, acquire if present
  boolean rcvFramePresent = NMRAnet_can_get_frame(&rxBuffer);
  
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


// to test (messages in JMRI format)
//    send a CIM frame which should get a RIM: [1002d000]
//    then a RIM which should restart sequence: [17ffd000]

//    send a Verify Node frame of [1b5fd00F] 2 3 4 5 6 7
//    send a Request Consumers frame of [1b7fd00F] 1 2 3 4 5 6 7 8
//    send a Request Producers frame of [1b9fd00F] 8 7 6 5 4 3 2 1
//    send a Request Events frame of [1bbfd00F] 2 3 4 5 6 7

//    produce an event with [1b4fd00F] 8 7 6 5 4 3 2 1

int main(void)
{
	init();

	setup();
    
	for (;;)
		loop();
        
	return 0;
}

