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
#include <Arduino.h>

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
#include "PIP.h"
#include "SNII.h"
#include "BG.h"
#include "ButtonLed.h"

OpenLcbCanBuffer     rxBuffer;	// CAN receive buffer
OpenLcbCanBuffer     txBuffer;	// CAN send buffer
OpenLcbCanBuffer*    ptxCAN;

NodeID nodeid(2,0,54,2,18,1);    // This node's default ID

LinkControl link(&txBuffer, &nodeid);

unsigned int datagramCallback(uint8_t *rbuf, unsigned int length, unsigned int from);
unsigned int streamRcvCallback(uint8_t *rbuf, unsigned int length);

Datagram dg(&txBuffer, datagramCallback, &link);
OlcbStream str(&txBuffer, streamRcvCallback, &link);

/**
 * Get and put routines that 
 * use a test memory space.
 */
//prog_char configDefInfo[] PROGMEM = //"OlcbBasicNode"; // null terminated string
static PROGMEM prog_uchar configDefInfo[] = {

0xc1,  0x01,  0x3c,  0xe3,  0xc1,  0xcf,  0x83,  0x2d,  0x1b,  0x04,  0xdc,  0x98,  0x72,  0xe2,  0xcc,  0x92, 
0x3d,  0x3b,  0xbc,  0x88,  0x30,  0xa2,  0xc3,  0x80,  0x08,  0x01,  0x53,  0x76,  0x8c,  0xd9,  0x33,  0x61, 
0xc9,  0x8e,  0x79,  0x38,  0x50,  0xa5,  0x42,  0x0c,  0x2d,  0x0e,  0x44,  0xf8,  0xf1,  0x81,  0x05,  0x85, 
0x87,  0x71,  0x44,  0x04,  0x23,  0x48,  0x3c,  0x2c,  0x99,  0xc4,  0x03,  0x17,  0x96,  0xcc,  0x58,  0x32, 
0x66,  0x18,  0x14,  0x19,  0x11,  0x49,  0x91,  0xc1,  0x96,  0x21,  0x3b,  0xae,  0xcc,  0x18,  0x32,  0xe6, 
0xc2,  0x95,  0x53,  0x71,  0xe0,  0xa3,  0x42,  0x98,  0x04,  0x02,  0x2a,  0x4c,  0x19,  0xb3,  0x60,  0xc7, 
0x9e,  0x0d,  0x7b,  0xe6,  0x3c,  0xf1,  0xa0,  0x8f,  0xf3,  0x38,  0x2a,  0x01,  0x90,  0xe9,  0x58,  0x17, 
0x4c,  0xd9,  0xe0,  0x23,  0x4f,  0x86,  0x30,  0x21,  0xb4,  0x98,  0x31,  0xa1,  0x25,  0x49,  0x21,  0x31, 
0x23,  0x3a,  0x8e,  0x59,  0x30,  0xe4,  0xc4,  0x84,  0x7b,  0x60,  0x30,  0xa5,  0x36,  0x29,  0xf8,  0x26, 
0x02,  0x63,  0x7c,  0x44,  0x79,  0x94,  0xa6,  0x34,  0x67,  0xf6,  0xcc,  0xb8,  0xa4,  0xfc,  0x28,  0xa6, 
0x74,  0x94,  0xa7,  0x7c,  0x7b,  0xa0,  0xdf,  0xfc,  0xcd,  0xc7,  0xc3,  0x99,  0x29,  0x73,  0xb6,  0x27, 
0x88,  0x80,  0x3d,  0x27,  0x96,  0xcc,  0x5f,  0xe1,  0x25,  0xbe,  0x91,  0x33,  0x87,  0x43,  0x60,  0x1a, 
0x52,  0x57,  0xc2,  0x9c,  0x13,  0x7b,  0xae,  0x1c,  0xe2,  0x83,  0x19,  0xb3,  0x54,  0xe0,  0x12,  0xdf, 
0xfb,  0xc1,  0x81,  0xcd,  0xb7,  0x86,  0xc7,  0x0e,  0x20,  0x4a,  0xe6,  0xba,  0x60,  0xc7,  0xb0,  0x04, 
0xf0,  0x51,  0x48,  0x03,  0x26,  0x5c,  0x19,  0x33,  0x65,  0x82,  0x80,  0x28,  0xb7,  0xaa,  0xe4,  0xbc, 
0x11,  0x51,  0xbd,  0x4c,  0x09,  0xc0,  0x84,  0x29,  0x67,  0xc6,  0x4d,  0xc1,  0x81,  0x0b,  0xcb,  0xc5, 

0xa4,  0xc2,  0x82,  0x69,  0xa2,  0x90,  0x24,  0xc2,  0x19,  0x01,  0xb3,  0x3e,  0x40,  0xc0,  0x25,  0x02, 
0xf3,  0x94,  0x53,  0x36,  0xa6,  0x7e,  0x6a,  0xe6,  0x0d,  0xd3,  0x0a,  0x3a,  0x00,  0xf4,  0x6d,  0x1b, 
0xee,  0x71,  0xd5,  0x0c,  0x4b,  0x1c,  0x4b,  0xa1,  0x70,  0xb1,  0x61,  0x18,  0x1a,  0x8c,  0x99,  0xb0, 
0x0c,  0x4e,  0x00,
};
extern "C" {
const prog_char SNII_const_data[] PROGMEM = "\001OpenLCB\000OlcbBasicNode\0000.4";
}
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
  } else if (space == 0xFC) { // 
    // used by ADCDI for constant data
    return pgm_read_byte(SNII_const_data+address);
  } else if (space == 0xFB) { // 
    // used by ADCDI for variable data
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

uint8_t protocolIdent[6] = {0xD1,0,0,0,0,0};

Configuration cfg(&dg, &str, &getRead, &getWrite, (void (*)())0);

unsigned int datagramCallback(uint8_t *rbuf, unsigned int length, unsigned int from){
  // invoked when a datagram arrives
  logstr("consume datagram of length ");loghex(length); lognl();
  for (int i = 0; i<length; i++) printf("%x ", rbuf[i]);
  printf("\n");
  // pass to consumers
 return cfg.receivedDatagram(rbuf, length, from);  
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
    Event(), Event(), Event(), Event()
   };
int eventNum = 64;

// output drivers
ButtonLed p2(2, LOW);//1
ButtonLed p3(3, LOW);
ButtonLed p4(4, LOW);
ButtonLed p5(5, LOW);
ButtonLed p6(6, LOW);
ButtonLed p7(7, LOW);
ButtonLed p8(8, LOW);
ButtonLed p9(9, LOW);//8
ButtonLed p10(10, LOW);//9
ButtonLed p11(11, LOW);
ButtonLed p12(12, LOW);
ButtonLed p13(13, LOW);
ButtonLed p14(14, LOW);
ButtonLed p17(17, LOW);
ButtonLed p18(18, LOW);
ButtonLed p19(19, LOW);//16
ButtonLed p22(22, LOW);
ButtonLed p23(23, LOW);
ButtonLed p24(24, LOW);
ButtonLed p25(25, LOW);
ButtonLed p26(26, LOW);
ButtonLed p27(27, LOW);
ButtonLed p28(28, LOW);
ButtonLed p29(29, LOW);//24
ButtonLed p30(30, LOW);//25
ButtonLed p31(31, LOW);
ButtonLed p32(32, LOW);
ButtonLed p33(33, LOW);
ButtonLed p34(34, LOW);
ButtonLed p35(35, LOW);
ButtonLed p36(36, LOW);
ButtonLed p37(37, LOW);//32
//ButtonLed p38(38, LOW);
//ButtonLed p39(39, LOW);
//ButtonLed p40(40, LOW);

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
  ShortBlinkOff,ShortBlinkOn//32
  
  
};
//ButtonLed* buttons[] = {&p14,&p14,&p15,&p15,&p16,&p16,&p17,&p17,&p9,&p9,&p8,&p8,&p7,&p7,&p6,&p6};
ButtonLed* buttons[] = {&p2,&p2,&p3,&p3,&p4,&p4,&p5,&p5,&p6,&p6,&p7,&p7,&p8,&p8,&p9,&p9,&p10,&p10,&p11,&p11,&p12,&p12,&p13,&p13,&p14,&p14,&p17,&p17,&p18,&p18,&p19,&p19,&p22,&p22,&p23,&p23,&p24,&p24,&p25,&p25,&p26,&p26,&p27,&p27,&p28,&p28,&p29,&p29,&p30,&p30,&p31,&p31,&p32,&p32,&p33,&p33,&p34,&p34,&p35,&p35,&p36,&p36,&p37,&p37,};


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
false, false, false, false,false, false, false, false};
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
  delay(250);Serial.begin(BAUD_RATE);logstr("\nOlcbBasicNode 32 input producer\n");
  
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
  for (int i=0; i<eventNum; i++) {
      pce.newEvent(i,true,false); // produce, consume
  //}
  //for (int i=eventNum/2; i<eventNum; i++) {
      //pce.newEvent(i,false,true); // produce, consume
  }
 
 // Init protocol blocks
  PIP_setup(protocolIdent, &txBuffer, &link);
  SNII_setup((uint8_t)sizeof(SNII_const_data), &txBuffer, &link); 
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
  bool handled = false;
  if (rcvFramePresent) {
    // blink blue to show that the frame was received
    blue.blink(0x1);
    // see if recieved frame changes link state
    handled = link.receivedFrame(&rxBuffer);
  }

  // if link is initialized, higher-level operations possible
  if (link.linkInitialized()) {
     // if frame present, pass to handlers
      if (rcvFramePresent && rxBuffer.isMsgForHere(link.getAlias())) {
        handled |= pce.receivedFrame(&rxBuffer);
        handled |= dg.receivedFrame(&rxBuffer);
        handled |= str.receivedFrame(&rxBuffer);
        handled |= PIP_receivedFrame(&rxBuffer);
        handled |= SNII_receivedFrame(&rxBuffer);
        if (!handled && rxBuffer.isAddressedMessage()) link.rejectMessage(&rxBuffer);
     }
     // periodic processing of any state changes
     pce.check();
     dg.check();
     str.check();
     cfg.check();
     bg.check();
     PIP_check();
     SNII_check();
     produceFromPins();
  } else {
    // link not up, but continue to show indications on blue and gold
    blue.process();
    gold.process();
  }

}

