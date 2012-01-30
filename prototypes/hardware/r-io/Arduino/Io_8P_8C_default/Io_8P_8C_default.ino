#include <ButtonLED.h>

#include <EEPROM.h>

#include <OpenLCB.h>

#include <can.h>

#include "MyEventHandler.h"
#include "MyConfigHandler.h"
#include "MyBlueGoldHandler.h"

//==============================================================
// Io_8P_8C_default
// This is the default sketch for Railstars Io NMRAnet demonstration boards
// Sets up 8 inputs (and hence 16 producers) on the "input" header, and 8 outputs (and hence 16 consumers) on the "output" header
// As well as a blue-gold interface for programming both
//
// © 2012 D.E. Goodman
// License TBA
//==============================================================

#define DEBUG

#ifndef OLCBNID
#define OLCBNID 2,1,13,0,0,3 // This node's ID (DIY space)
#endif

OLCB_NodeID nodeid(OLCBNID); // Create the node structure with this node's NodeID

//Allocate memory for the event pool
OLCB_Event event_pool[32];

/* define the CAN link to the outside world */
OLCB_CAN_Link link;

/* declare the PC Event Report handler */
MyEventHandler pce;
MyConfigHandler cfg;
MyBlueGoldHandler bg;

/* define the Blue/Gold devices */
ButtonLed blue(BLUE, LOW); // button on pin 14
ButtonLed gold(GOLD, LOW); // button on pin 15


void loadNodeID(OLCB_NodeID *nid)
{
	//The NodeID is stored at the very end of EEPROM
	//on the AT90CAN128, the last EEPROM address is 0x0FFF
	//So:
	nid->val[0] = EEPROM.read(0x0FFA);
	nid->val[2] = EEPROM.read(0x0FFB);
	nid->val[3] = EEPROM.read(0x0FFC);
	nid->val[4] = EEPROM.read(0x0FFD);
	nid->val[5] = EEPROM.read(0x0FFE);
	nid->val[5] = EEPROM.read(0x0FFF);
}

// =============== Setup ===================

void setup()
{
    #ifdef DEBUG
    Serial.begin(115200);
    Serial.println("Io 8C 8P default");
  #endif
  //first, set up inputs and outputs, setting pull-up resistors on inputs
  for(int i = 0; i < 8; ++i) //outputs
  {
    pinMode(i, OUTPUT);
    digitalWrite(i, LOW);
  }
  for(int i = 8; i < 16; ++i)
  {
    pinMode(i, INPUT);
    digitalWrite(i, HIGH);
  }
  
  //now, load the NodeID from EEPROM
  loadNodeID(&nodeid);
  
    nodeid.print();
    link.initialize();
    pce.create(&link, &nodeid);
    pce.initialize(event_pool, 32); //set up a space for 32 events: 16 producers and 16 consumers TODO REMOVE THIS!?
    cfg.create(&link, &nodeid, &pce);
    bg.create(&link, &nodeid, &pce);
    link.addVNode(&pce);
    link.addVNode(&cfg);
    link.addVNode(&bg);
}

// ================ Loop ===================

void loop()
{
    // OpenLCB statndard processing:
    link.update();
    //Serial.println(millis());
}

// ---------------------------------------------------

