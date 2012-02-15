//==============================================================
// OlcbBasicNode23P_at90can
//   A prototype of a basic 32-channel OpenLCB board
//   
//   setup() at line 189 determines which are consumers and
//   which are producers
//
//   Bob Jacobsen 2010
//      based on examples by Alex Shepherd, David Harris and Tim Hatch
//==============================================================

// next line for stand-alone compile
#include <WProgram.h>

#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <avr/pgmspace.h>
#include "logging.h"

// The following lines are needed because the Arduino environment 
// won't search a library directory unless the library is included 
// from the top level file (this file)
#include <EEPROM.h>
#include <can.h>

class foo{};

//factory default reset pin
#define FACTORY_DEFAULT_PIN 20
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
#include "OlcbStream.h"
#include "Configuration.h"
#include "NodeMemory.h"
#include "PCE.h"
#include "BG.h"
#include "ButtonLed.h"

OpenLcbCanBuffer     rxBuffer;	// CAN receive buffer
OpenLcbCanBuffer     txBuffer;	// CAN send buffer
OpenLcbCanBuffer*    ptxCAN;

NodeID nodeid(2,1,54,1,0x12,3);    // This node's default ID

LinkControl link(&txBuffer, &nodeid);

unsigned int datagramCallback(uint8_t *rbuf, unsigned int length, unsigned int from);
unsigned int streamRcvCallback(uint8_t *rbuf, unsigned int length);

Datagram dg(&txBuffer, datagramCallback, &link);
OlcbStream str(&txBuffer, streamRcvCallback, &link);

/**
 * Get and put routines that 
 * use a test memory space.
 */
prog_char configDefInfo[] PROGMEM = "OlcbBasicNode"; // null terminated string

const uint8_t getRead(uint32_t address, int space) {
  if (space == 0xFF) {
    // Configuration definition information
    return pgm_read_byte(configDefInfo+address);
  } else if (space == 0xFE) {
    // All memory
    return *(((uint8_t*)&rxBuffer)+address);
  } else if (space == 0xFD) {
    // Configuration space
    return EEPROM.read(address);
  } else {
    // unknown space
    return 0; 
  }
}
void getWrite(uint32_t address, int space, uint8_t val) {
  if (space == 0xFE) {
    // All memory
    *(((uint8_t*)&rxBuffer)+address) = val;
  } else if (space == 0xFD) {
    // Configuration space
    EEPROM.write(address, val);
  } 
  // all other spaces not written
}

Configuration cfg(&dg, &str, &getRead, &getWrite, (void (*)())0);

unsigned int datagramCallback(uint8_t *rbuf, unsigned int length, unsigned int from){
  // invoked when a datagram arrives
  logstr("consume datagram of length ");loghex(length); lognl();
  for (int i = 0; i<length; i++) printf("%x ", rbuf[i]);
  printf("\n");
  // pass to consumers
  cfg.receivedDatagram(rbuf, length, from);
  
  return 0;  // return pre-ordained result
}

unsigned int resultcode;
unsigned int streamRcvCallback(uint8_t *rbuf, unsigned int length){
  // invoked when a stream frame arrives
  printf("consume frame of length %d: ",length);
  for (int i = 0; i<length; i++) printf("%x ", rbuf[i]);
  printf("\n");
  return resultcode;  // return pre-ordained result
}

// Events this node can produce or consume, used by PCE and loaded from EEPROM by NM
    Event events[] = {
    Event(), Event(), Event(), Event(), 
    Event(), Event(), Event(), Event(), 
    Event(), Event(), Event(), Event(),
    Event(), Event(), Event(), Event(),
    Event(), Event(), Event(), Event(),
    Event(), Event(), Event(), Event(),
    Event(), Event(), Event(), Event(), 
    Event(), Event(), Event(), Event(),
    Event(), Event(), Event(), Event(), 
    Event(), Event(), Event(), Event(),
    Event(), Event(), Event(), Event(), 
    Event(), Event(), Event(), Event(),
    Event(), Event(), Event(), Event(), 
    Event(), Event(), Event(), Event(),
    Event(), Event(), Event(), Event(),
    Event(), Event(), Event(), Event(),
    Event(), Event(), Event(), Event(),
    Event(), Event(), Event(), Event(), 
    Event(), Event(), Event(), Event(),
    Event(), Event(), Event(), Event()
    };
int eventNum = 80;

// output drivers

ButtonLed p22(22, HIGH);
ButtonLed p23(23, HIGH);
ButtonLed p24(24, HIGH);
ButtonLed p25(25, HIGH);
ButtonLed p26(26, HIGH);
ButtonLed p27(27, HIGH);
ButtonLed p28(28, HIGH);
ButtonLed p29(29, HIGH);//24
ButtonLed p30(30, LOW);//25
ButtonLed p31(31, LOW);
ButtonLed p32(32, LOW);
ButtonLed p33(33, LOW);
ButtonLed p34(34, LOW);
ButtonLed p35(35, LOW);
ButtonLed p36(36, LOW);
ButtonLed p37(37, LOW);//32
ButtonLed p38(38, LOW);
ButtonLed p39(39, LOW);
ButtonLed p40(40, LOW);
ButtonLed p41(41, LOW);
ButtonLed p44(44, LOW);
ButtonLed p45(45, LOW);
ButtonLed p46(46, LOW);
ButtonLed p47(47, LOW);
ButtonLed p2(2, HIGH);//1
ButtonLed p3(3, HIGH);
ButtonLed p4(4, HIGH);
ButtonLed p5(5, HIGH);
ButtonLed p6(6, HIGH);
ButtonLed p7(7, HIGH);
ButtonLed p8(8, HIGH);
ButtonLed p9(9, HIGH);//8
ButtonLed p10(10, HIGH);//9
ButtonLed p11(11, HIGH);
ButtonLed p12(12, HIGH);
ButtonLed p13(13, HIGH);
ButtonLed p14(14, HIGH);
ButtonLed p17(17, HIGH);
ButtonLed p18(18, HIGH);
ButtonLed p19(19, HIGH);//16

#define ShortBlinkOn   0x00010001L
#define ShortBlinkOff  0xFFFEFFFEL

long patterns[] = {
  ShortBlinkOff,ShortBlinkOn,//1
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,//8
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,//16
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,//24
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,//32
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn,
  ShortBlinkOff,ShortBlinkOn
  
  
};
//ButtonLed* buttons[] = {&p14,&p14,&p15,&p15,&p16,&p16,&p17,&p17,&p9,&p9,&p8,&p8,&p7,&p7,&p6,&p6};
ButtonLed* buttons[] = {&p2,&p2,&p3,&p3,&p4,&p4,&p5,&p5,&p6,&p6,&p7,&p7,&p8,&p8,&p9,&p9,&p10,&p10,&p11,&p11,&p12,&p12,&p13,&p13,&p14,&p14,&p17,&p17,&p18,&p18,&p19,&p19,&p22,&p22,&p23,&p23,&p24,&p24,&p25,&p25,&p26,&p26,&p27,&p27,&p28,&p28,&p29,&p29,&p30,&p30,&p31,&p31,&p32,&p32,&p33,&p33,&p34,&p34,&p35,&p35,&p36,&p36,&p37,&p37,&p38,&p38,&p39,&p39,&p40,&p40,&p41,&p41,&p44,&p44,&p45,&p45,&p46,&p46,&p47,&p47};


ButtonLed blue(42, LOW);
ButtonLed gold(43, LOW);

void pceCallback(int index){
  // invoked when an event is consumed; drive pins as needed
  // from index
  //
  // sample code uses inverse of low bit of pattern to drive pin all on or all off
  // (pattern is mostly one way, blinking the other, hence inverse)
  //
  buttons[index]->on(patterns[index]&0x1 ? 0x0L : ~0x0L );
}

NodeMemory nm(0);  // allocate from start of EEPROM
void store() { nm.store(&nodeid, events, eventNum); }

PCE pce(events, eventNum, &txBuffer, &nodeid, pceCallback, store, &link);

// Set up Blue/Gold configuration

BG bg(&pce, buttons, patterns, eventNum, &blue, &gold);

bool states[] = {false, false, false, false,false, false, false, false, false, false, false, false,
false, false, false, false,false, false, false, false, false, false, false, false,
false, false, false, false,false, false, false, false,false, false,false, false,false,false,false,false};
void produceFromPins() {
  // called from loop(), this looks at pins and 
  // and decides which events to fire.
  // with pce.produce(i);
  // The first event of each pair is sent on button down,
  // and second on button up.
  //for (int i = 0; i<eventNum/2; i++) {
    for (int i = 0; i<eventNum/2; i++) {
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
  delay(250);Serial.begin(BAUD_RATE);logstr("\nOpenLCB Basic Node 16 input producer 24 output consumer\n");
  
  // read OpenLCB from EEPROM
  //check for factory default reset jumper on pin ?
#if defined (FACTORY_DEFAULT_PIN)
   pinMode(FACTORY_DEFAULT_PIN, INPUT);
   if (digitalRead(FACTORY_DEFAULT_PIN) != 1) 
	nm.forceInitAll(); 
#endif
  nm.forceInitAll(); // uncomment if need to go back to initial EEPROM state
  nm.setup(&nodeid, events, eventNum);  
  
  // set event types, now that IDs have been loaded from configuration
  for (int i=0; i<eventNum/1.66605; i++) {
      pce.newEvent(i,false,true); // produce, consume
  }
  for (int i=eventNum/1.66605; i<eventNum; i++) {
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
    // blink blue to show that the frame was received
    blue.blink(0x1);
    // see if recieved frame changes link state
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
     cfg.check();
     bg.check();
     produceFromPins();
  } else {
    // link not up, but continue to show indications on blue and gold
    blue.process();
    gold.process();
  }

}

