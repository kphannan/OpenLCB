//===========================================================
// MrrlcbCanEtherNet
//   Developing (eventual) classes for OpenLCB
// 
//   Bob Jacobsen 2009
//      based on examples by Alex Shepherd and David Harris
//===========================================================
#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>

// The following lines are needed because the Arduino environment 
// won't search a library directory unless the library is included 
// from the top level file (this file)
#include <CAN.h>
#include <EEPROM.h>

class foo{};  // force Arduino environment to treat the rest of this file as C++

// init for serial communications
#define         BAUD_RATE       115200
//#define         BAUD_RATE       333333

// Addressing information for this node.
// Used for IP address and node ID
#define ADDR_BYTE_1 10
#define ADDR_BYTE_2  0
#define ADDR_BYTE_3  1
#define ADDR_BYTE_4 97

// openLCB definitions
#include <OpenLcbCanInterface.h>
#include <OpenLcbCanBuffer.h>
#include <NodeID.h>
#include <EventID.h>

// specific OpenLCB implementations
#include "LinkControl.h"

OpenLcbCanBuffer     rxBuffer;	// CAN receive buffer
OpenLcbCanBuffer     txBuffer;	// CAN send buffer
OpenLcbCanBuffer*    ptxCAN;

// openLCB nodeID for this node
NodeID nodeid(4,  // self-assigned NodeID based on IP address
    ADDR_BYTE_1, ADDR_BYTE_2, ADDR_BYTE_3, ADDR_BYTE_4,
    1);
    
LinkControl link(&txBuffer, &nodeid);

// Alternate Ethernet library for modified CAN-compatible shield
#include <Ethernet2.h>

// definition for Ethernet connection
byte mac[] = { 0xDE, 0xAD, ADDR_BYTE_1, ADDR_BYTE_2, ADDR_BYTE_3, ADDR_BYTE_4 };
byte ip[] = { ADDR_BYTE_1, ADDR_BYTE_2, ADDR_BYTE_3, ADDR_BYTE_4 };
byte gateway[] = { ADDR_BYTE_1, ADDR_BYTE_2, ADDR_BYTE_3, 1 };
byte subnet[] = { 255, 255, 255, 0 };

// telnet defaults to port 23
Server server(23);


//#include <NodeIDList.h>
//NodeIDList nodes(10);

/**
 * This setup is just for testing
 */
void setup()
{
  // set up serial comm
  Serial.begin(BAUD_RATE);
  
  // show we're starting to run
  delay(100);
  Serial.println();Serial.println("Starting CanMrrlcbTest");
  
  // Initialize OpenLCB CAN connection
  OpenLcb_can_init();
  
  // Initialize OpenLCB CAN link controller
  link.reset();
  
  // Ethernet, from WebClient demo
  Ethernet.begin(mac, ip);
  
  // start listening for clients
  server.begin();
  Serial.println("Server begun");
}

void loop() {
    
  // check for input CAN frames, acquire if present
  boolean rcvFramePresent = OpenLcb_can_get_frame(&rxBuffer);
  
  // process link control first
  link.check();
  if (rcvFramePresent) {
    // received a frame, ask if changes link state
    link.receivedFrame(&rxBuffer);
    // store any node aliases that are confirmed
    //nodes.receivedFrame(&rxBuffer);
    
    // ToDo: forward via server
    //server.write(toArray(rxBuffer), toArrayLen(rxBuffer));
  }

  // if link is initialized, higher-level operations possible
  if (link.linkInitialized()) {
    // other stuff here
  }

  // handle client connection
  // if client data available, process
  Client client = server.available();
  if (client) {
    // ToDo: client data into buffer until message frame complete
    // fillInputBuffer(client.read());
  }
  // see if have accumulated any messages to send
  if ( false /* Todo: inputBufferReady()*/ && false /* ToDo: isTxBufferFree() */) {
    // send CAN frame from buffered input
    //if(can_send_message(inputBufferFrame())) {
      // when successfully sent, drop from buffer
      // ToDo: inputBufferFrameSent(); 
    //}
    
  }
  
}

