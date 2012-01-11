#include "ButtonLedDON.h"

#include <OpenLCB.h>

#include <can.h>

#include "MyEventHandler.h"

//==============================================================
// Io_8P_8C_default
// This is the default sketch for Railstars Io NMRAnet demonstration boards
// Sets up 8 producers on the "input" header, and 8 consumers on the "output" header
// As well as a blue-gold interface for programming both
// Provides capacity for up to 64 p-c slots
//
// © 2012 D.E. Goodman
// License TBA
//==============================================================

#define DEBUG

#define EVENT_POOL_SIZE 64
#define EEPROM_START 128 //why? I don't know.

#ifndef OLCBNID
#define OLCBNID 2,1,13,0,0,3 // This node's ID (DIY space)
#endif

OLCB_NodeID nodeid(OLCBNID); // Create the node structure with this node's NodeID

//Allocate memory for the event pool
OLCB_Event event_pool[EVENT_POOL_SIZE];

/* define the CAN link to the outside world */
OLCB_CAN_Link link;

/* declare the PC Event Report handler */
MyEventHandler pce;

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
  #ifdef DBEUG
    Serial.begin(115200);
    Serial.println("Io 8C 8P default");
  #endif
    link.initialize();
    pce.create(&link, &nodeid);
    pce.initialize();
    pce.loadEvents(&event_pool, &event_consumer_on_mask, &event_consumer_off_mask, &event_producer_on_mask, &event_producer_off_mask, EVENT_POOL_SIZE, EEPROM_START); //requires an offset from the base address to start reading.
    link.addVNode(&pce);
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

