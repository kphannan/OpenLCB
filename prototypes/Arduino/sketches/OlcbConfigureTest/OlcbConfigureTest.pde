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

// The following lines are needed because the Arduino environment 
// won't search a library directory unless the library is included 
// from the top level file (this file)
#include <EEPROM.h>
#include <CAN.h>

class foo{};  // force Arduino environment to treat the rest of this file as C++

// init for serial communications
#define         BAUD_RATE       115200
//#define         BAUD_RATE       57600

// demo I/O pins
#define CONSUMER_PIN 9
#define PRODUCER_PIN 14
int producer_pin_record;

// OpenLCB definitions
#include "OpenLcbCanInterface.h"
#include "OpenLcbCanBuffer.h"
#include "NodeID.h"
#include "EventID.h"
#include "Event.h"

// specific OpenLCB implementations
#include "LinkControl.h"
#include "Datagram.h"
#include "Stream.h"
#include "Configuration.h"
#include "NodeMemory.h"
#include "PCE.h"
#include "BG.h"

OpenLcbCanBuffer     rxBuffer;	// CAN receive buffer
OpenLcbCanBuffer     txBuffer;	// CAN send buffer
OpenLcbCanBuffer*    ptxCAN;

NodeID nodeid(2,3,4,5,6,7);    // This node's default ID

LinkControl link(&txBuffer, &nodeid);

unsigned int datagramCallback(uint8_t *rbuf, unsigned int length, unsigned int from);
unsigned int streamRcvCallback(uint8_t *rbuf, unsigned int length);

Datagram dg(&txBuffer, datagramCallback, &link);
Stream str(&txBuffer, streamRcvCallback, &link);

/**
 * Get and put routines that 
 * use a test memory space.
 */
uint8_t test_mem[200];
const uint8_t getRead(int address, int space) {
    return *(test_mem+address);
}
void getWrite(int address, int space, uint8_t val) {
    *(test_mem+address) = val;
}

Configuration cfg(&dg, &str, &getRead, &getWrite, (void (*)())0);

unsigned int datagramCallback(uint8_t *rbuf, unsigned int length, unsigned int from){
  // invoked when a datagram arrives
  //logstr("consume datagram of length ");loghex(length); lognl();
  //for (int i = 0; i<length; i++) printf("%x ", rbuf[i]);
  //printf("\n");
  // pass to consumers
  //cfg.receivedDatagram(rbuf, length, from);
  
  return 0;  // return pre-ordained result
}

unsigned int resultcode;
unsigned int streamRcvCallback(uint8_t *rbuf, unsigned int length){
  // invoked when a stream frame arrives
  //printf("consume frame of length %d: ",length);
  //for (int i = 0; i<length; i++) printf("%x ", rbuf[i]);
  //printf("\n");
  return resultcode;  // return pre-ordained result
}

// Events this node can produce or consume, used by PCE and loaded from EEPROM by NM
Event events[] = {
    Event(true, false), // produce? consume?
    Event(true, false),
    Event(true, false), 
    Event(true, false), 
    Event(true, false), 
    Event(true, false), 
    Event(true, false), 
    Event(true, false)
};
int eventNum = 8;


void pceCallback(int index){
  // invoked when an event is consumed
  //Serial.print("consume ");Serial.println(index);
  //if (index == 0) {
  //  digitalWrite(CONSUMER_PIN, LOW);
  //} else if (index == 1) {
  //  digitalWrite(CONSUMER_PIN, HIGH);
  //}
}

NodeMemory nm(0);  // allocate from start of EEPROM

PCE pce(events, eventNum, &txBuffer, &nodeid, pceCallback);
ButtonLed p14(14);
ButtonLed p15(15);
ButtonLed p16(16);
ButtonLed p17(17);
ButtonLed buttons[] = {p14,p14,p15,p15,p16,p16,p17,p17};
long patterns[] = {3L,~3L,3L,~3L,3L,~3L,3L,~3L};
ButtonLed blue(18);
ButtonLed gold(19);
BG bg(&pce, buttons, patterns, eventNum, &blue, &gold);

/**
 * This setup is just for testing
 */
void setup()
{
  // set up serial comm; may not be space for this!
  //Serial.begin(BAUD_RATE);logstr("\nOlcbConfigureTest\n");

  // Initialize test I/O pins
  pinMode(CONSUMER_PIN,OUTPUT);
  digitalWrite(CONSUMER_PIN,HIGH);
  pinMode(PRODUCER_PIN,INPUT);
  digitalWrite(PRODUCER_PIN,HIGH);
  
  // read OpenLCB from EEPROM
  //nm.forceInit(); // if need to go back to start
  nm.setup(&nodeid, events, eventNum);  
 
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
        pce.receivedFrame(&rxBuffer);
        dg.receivedFrame(&rxBuffer);
        str.receivedFrame(&rxBuffer);
     }
     // periodic processing of any state changes
     pce.check();
     dg.check();
     str.check();
     cfg.check();
     bg.check();
     
     // Demo: handle possible production of events from pin
     //if (producer_pin_record != digitalRead(PRODUCER_PIN)) {
     //    producer_pin_record = digitalRead(PRODUCER_PIN);
     //    if (producer_pin_record == LOW) {
     //        p.produce(0);
     //    } else {
     //        p.produce(1);
     //    }
     //}
  } else {
    // link not up, but continue to show indications on blue and gold
    blue.process();
    gold.process();
  }

}




