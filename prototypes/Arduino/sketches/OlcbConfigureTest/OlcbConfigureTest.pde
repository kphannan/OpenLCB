//==============================================================
// OlcbConfigureTest
//   A prototype of a 4-channel OpenLCB board with all features!
// 
//   Bob Jacobsen 2010
//      based on examples by Alex Shepherd and David Harris
//==============================================================
//#include "WProgram.h"
#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include "logging.h"

// The following lines are needed because the Arduino environment 
// won't search a library directory unless the library is included 
// from the top level file (this file)
#include <EEPROM.h>
#include <CAN.h>

class foo{};

// init for serial communications
//#define         BAUD_RATE       115200
#define         BAUD_RATE       57600
//#define         BAUD_RATE       19200

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
#include "ButtonLed.h"

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
//uint8_t test_mem[100];
const uint8_t getRead(int address, int space) {
    return 0; //*(test_mem+address);
}
void getWrite(int address, int space, uint8_t val) {
    //*(test_mem+address) = val;
}
void reset1() {}

Configuration cfg(&dg, &str, &getRead, &getWrite, &reset1);

unsigned int datagramCallback(uint8_t *rbuf, unsigned int length, unsigned int from){
  // invoked when a datagram arrives
  //logstr("consume datagram of length ");loghex(length); lognl();
  //for (int i = 0; i<length; i++) printf("%x ", rbuf[i]);
  //printf("\n");
  // pass to consumers
  // ToDo: turn on with configuration code
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
    Event(), Event(), Event(), Event(), 
    Event(), Event(), Event(), Event() 
};
int eventNum = 8;

// output drivers
ButtonLed p14(14);
ButtonLed p15(15);
ButtonLed p16(16);
ButtonLed p17(17);

#define ShortBlinkOn   0x01010101L
#define ShortBlinkOff  0xFEFEFEFEL

long patterns[] = {
  ShortBlinkOn,ShortBlinkOff,
  ShortBlinkOn,ShortBlinkOff,
  ShortBlinkOn,ShortBlinkOff,
  ShortBlinkOn,ShortBlinkOff
};
ButtonLed* buttons[] = {&p14,&p14,&p15,&p15,&p16,&p16,&p17,&p17};

ButtonLed blue(18);
ButtonLed gold(19);

void pceCallback(int index){
  //logstr("pce callback\n");
  // invoked when an event is consumed; drive pins as needed
  // from index
  //
  // sample code uses inverse of low bit of pattern to drive pin all on or all off
  //
  buttons[index]->on(patterns[index]&0x1 ? 0x0L : ~0x0L );
}

NodeMemory nm(0);  // allocate from start of EEPROM

PCE pce(events, eventNum, &txBuffer, &nodeid, pceCallback);

// Set up Blue/Gold configuration

BG bg(&pce, buttons, patterns, eventNum, &blue, &gold);

bool states[] = {false, false, false, false};  // true gives initial report due to implied change
void produceFromPins() {
  // called from loop(), this looks at pins and 
  // and decides which events to fire.
  // with pce.produce(i);
  for (int i = 0; i<4; i++) {
    if (states[i] != buttons[i*2]->state) {
      states[i] = buttons[i*2]->state;
      if (states[i]) {
        pce.produce(i*2);
      } else {
        pce.produce(i*2+1);
      }
    }
  }
}

/**
 * Setup does initial configuration
 */
void setup()
{
  // set up serial comm; may not be space for this!
  //delay(250);Serial.begin(BAUD_RATE);logstr("\nOlcbConfigureTest\n");
  
  // read OpenLCB from EEPROM
  //nm.forceInit(); // uncomment if need to go back to initial EEPROM state
  nm.setup(&nodeid, events, eventNum);  
  
  // set event types, now that IDs have been loaded from configuration
  for (int i=0; i<eventNum; i++) {
      pce.newEvent(i,true,false); // produce, consume
  }
  
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
     // if frame present, pass to handlers
     if (rcvFramePresent) {
        pce.receivedFrame(&rxBuffer);
        dg.receivedFrame(&rxBuffer);
        str.receivedFrame(&rxBuffer);
     }
     // periodic processing of any state changes
     pce.check();
     dg.check();
     str.check();
     // ToDo: Turn on config, but be cautious about memory overwrite
     //cfg.check();
     bg.check();
     produceFromPins();
  } else {
    // link not up, but continue to show indications on blue and gold
    blue.process();
    gold.process();
  }

}




