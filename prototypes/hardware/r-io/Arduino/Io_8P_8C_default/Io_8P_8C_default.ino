#include <EEPROM.h>

#include "ButtonLedDON.h"

#include <OpenLCB.h>

#include <can.h>

#include "MyEventHandler.h"
#include "MyConfigHandler.h"
#include "Io_8P_8C_default_xml.h"

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

/* define the Blue/Gold devices */
#ifndef BLUE
#define BLUE 13
#endif
#ifndef GOLD
#define GOLD 14
#endif
ButtonLed blue(BLUE, LOW); // button on pin 14
ButtonLed gold(GOLD, LOW); // button on pin 15


// =============== Setup ===================

void setup()
{
  //first, set up inputs and outputs, setting pull-up resistors on inputs
  for(int i = 0; i < 8; ++i) //outputs
    pinMode(i, OUTPUT);
  for(int i = 8; i < 16; ++i)
  {
    pinMode(i, INPUT);
    digitalWrite(i, HIGH);
  }
  
  #ifdef DEBUG
    Serial.begin(115200);
    Serial.println("Io 8C 8P default");
  #endif
    link.initialize();
    pce.create(&link, &nodeid);
    pce.initialize(event_pool, 32); //set up a space for 32 events: 16 producers and 16 consumers
    cfg.create(&link, &nodeid, &pce);
    link.addVNode(&pce);
    link.addVNode(&cfg);
}

// ================ Loop ===================

void loop()
{
    // OpenLCB statndard processing:
    link.update();
    // read the buttons (implements debouncing, etc.)
    blue.process();
    gold.process();
    //TODO blue-gold here!
}

// ---------------------------------------------------

