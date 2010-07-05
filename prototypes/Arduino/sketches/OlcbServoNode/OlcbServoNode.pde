//===========================================================
// OlcbServoNode
//   Very simple example of a OpenLCB node driving a single
//   servo to three positions.
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
#define SERVO_PIN 9

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
#include "NodeMemory.h"
#include "PCE.h"

#include "Servo.h"

OpenLcbCanBuffer     rxBuffer;	// CAN receive buffer
OpenLcbCanBuffer     txBuffer;	// CAN send buffer
OpenLcbCanBuffer*    ptxCAN;

NodeID nodeid(0,0,0,0,0,0);    // This node's default ID

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
  for (int i = 0; i<length; i++) printf("%x ", rbuf[i]);
  //printf("\n");
  // pass to consumers
  cfg.receivedDatagram(rbuf, length, from);
  
  return 0;  // return pre-ordained result
}

unsigned int resultcode;
unsigned int streamRcvCallback(uint8_t *rbuf, unsigned int length){
  // invoked when a stream frame arrives
  //printf("consume frame of length %d: ",length);
  for (int i = 0; i<length; i++) printf("%x ", rbuf[i]);
  //printf("\n");
  return resultcode;  // return pre-ordained result
}


// Events this node can consume, used by PCE
Event cEvents[] = {
    Event(), // down
    Event(), // center
    Event()  // up
};
int cEventNum = 3;

Servo servo;

void pceCallback(int index){
  // invoked when an event is consumed
  if (index == 0) {
    servo.write(0);
  } else if (index == 1) {
    servo.write(90);
  } else if (index == 2) {
    servo.write(180);
  }
}

NodeMemory nm(0);  // allocate from start of EEPROM

PCE p(cEvents, cEventNum, &txBuffer, &nodeid, pceCallback);

/**
 * This setup is just for testing
 */
void setup()
{
  // set up serial comm
  Serial.begin(BAUD_RATE);
  
  // show we've started to run
  logstr("\nOlcbServoNode\n");

  servo.attach(SERVO_PIN);
  
  // read OpenLCB from EEPROM
  //nm.forceInit(); // if need to go back to start
  nm.setup(&nodeid, cEvents, cEventNum);  
 
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
  }

}


