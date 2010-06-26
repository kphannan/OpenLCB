//===========================================================
// OlcbConfigureTest
//   Developing (eventual) classes for OpenLCB
// 
//   Bob Jacobsen 2010
//      based on examples by Alex Shepherd and David Harris
//===========================================================
#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include "logging.h"

// The following line is needed because the Arduino environment 
// won't search a library directory unless the library is included 
// from the top level file (this file)
#include <CAN.h>

class foo{};  // force Arduino environment to treat the rest of this file as C++

// init for serial communications
#define         BAUD_RATE       115200
//#define         BAUD_RATE       57600

// demo I/O pins
#define CONSUMER_PIN 9
#define PRODUCER_PIN 14
int startmem;
int producer_pin_record;

// OpenLCB definitions
#include "OpenLcbCanInterface.h"
#include "OpenLcbCanBuffer.h"
#include "NodeID.h"
#include "EventID.h"

// specific OpenLCB implementations
#include "LinkControl.h"
#include "Datagram.h"
#include "Stream.h"
#include "Configuration.h"
#include "PCE.h"

OpenLcbCanBuffer     rxBuffer;	// CAN receive buffer
OpenLcbCanBuffer     txBuffer;	// CAN send buffer
OpenLcbCanBuffer*    ptxCAN;

NodeID nodeid(2,3,4,5,6,7);    // This node's ID

LinkControl link(&txBuffer, &nodeid);

unsigned int datagramCallback(uint8_t *rbuf, unsigned int length, unsigned int from);
unsigned int rcvCallback(uint8_t *rbuf, unsigned int length);

Datagram dg(&txBuffer, datagramCallback, &link);
Stream str(&txBuffer, rcvCallback, &link);
Configuration cfg(&dg, &str,0,0,0);

unsigned int datagramCallback(uint8_t *rbuf, unsigned int length, unsigned int from){
  // invoked when a datagram arrives
  //logstr("consume datagram of length ");loghex(length); lognl();
  for (int i = 0; i<length; i++) printf("%x ", rbuf[i]);
  //printf("\n");
  // pass to consumers
  cfg.receivedDatagram(rbuf, length, from);
  
  return 0;  // return pre-ordained result
}

unsigned int resultcode;
unsigned int rcvCallback(uint8_t *rbuf, unsigned int length){
  // invoked when a stream frame arrives
  //printf("consume frame of length %d: ",length);
  for (int i = 0; i<length; i++) printf("%x ", rbuf[i]);
  //printf("\n");
  return resultcode;  // return pre-ordained result
}

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

int endmem;

/**
 * This setup is just for testing
 */
void setup()
{
  // set up serial comm
  Serial.begin(BAUD_RATE);
  
  // show we've started to run
  logstr("\nStarting OlcbConfigureTest\n");

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
        dg.receivedFrame(&rxBuffer);
        str.receivedFrame(&rxBuffer);
     }
     // periodic processing of any PCE state changes
     p.check();
     dg.check();
     str.check();
     cfg.check();

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
//    send a CIM frame which should get a RIM:  [110036ba]
//    then a RIM which should restart sequence: [17fff6ba]

// (these need to be redone)
//    send a Verify Node frame of [180Af00f] 2 3 4 5 6 7
//    send a Request Consumers frame of [1824F00F] 1 2 3 4 5 6 7 8
//    send a Request Producers frame of [1828F00F] 8 7 6 5 4 3 2 1
//    send a Request Events frame of [182BF00F] 2 3 4 5 6 7

//    produce an event matching 1  [182DF00F] 8 7 6 5 4 3 2 1


