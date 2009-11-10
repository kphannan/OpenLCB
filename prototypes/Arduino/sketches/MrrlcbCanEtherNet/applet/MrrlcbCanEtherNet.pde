//===========================================================
// MrrlcbCanEtherNet
//   Developing (eventual) classes for NmraNet S9.6
// 
//   Bob Jacobsen 2009
//      based on examples by Alex Shepherd and David Harris
//===========================================================
#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>

// The following line is needed because the Arduino environment 
// won't search a library directory unless the library is included 
// from the top level file (this file)
#include <CAN.h>

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

// NMRAnet definitions
#include <NmraNetCanInterface.h>
#include <NmraNetCanBuffer.h>
#include <NodeID.h>
#include <EventID.h>

// specific NMRAnet implementations
#include "LinkControl.h"

NmraNetCanBuffer     rxBuffer;	// CAN receive buffer
NmraNetCanBuffer     txBuffer;	// CAN send buffer
NmraNetCanBuffer*    ptxCAN;

// NMRAnet nodeID for this node
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


/** 
 * Class to watch NodeID aliases
 */
class NodeIdList {
  int length;
  int* aliasArray;
  
  NodeIdList(int n) : length(n)  {
    aliasArray = new int[n];
  }
  void add(int alias, NodeID* node) {}
  int getAlias(NodeID * node) { return -1; }
  NodeID* getNodeID(int alias) { return 0; }
};

/**
 * This setup is just for testing
 */
void setup()
{
  // set up serial comm
  Serial.begin(BAUD_RATE);
  
  // show we're starting to run
  delay(100);
  Serial.println();Serial.print("Starting CanMrrlcbTest 5 ");
  delay(100);Serial.print("4");delay(100);Serial.print("3");delay(100);Serial.print("2");delay(100);Serial.print("1");delay(100);Serial.print("0");
  delay(100);Serial.println("");
  
  // Initialize NmraNet CAN connection
  NMRAnet_can_init();
  
  // Initialize NmraNet CAN link controller
  link.reset();
  
  // Ethernet, from WebClient demo
  Ethernet.begin(mac, ip);
  
  // start listening for clients
  server.begin();
  Serial.println("Server begun");
}

void loop() {
    
  // check for input CAN frames, acquire if present
  boolean rcvFramePresent = NMRAnet_can_get_frame(&rxBuffer);
  
  // process link control first
  link.check();
  if (rcvFramePresent) {
    // received a frame, ask if changes link state
    link.receivedFrame(&rxBuffer);
    
    // forward via server
    server.write("Frame");
  }

  // if link is initialized, higher-level operations possible
  if (link.linkInitialized()) {
    // other stuff here
  }

  // handle client connection
  // if client data available, process
  Client client = server.available();
  if (client) {
    Serial.println("connected");
    server.write("Hello");
    // dump client data
    client.read();
  }
  
  // process server info; echo for now
  //client = server.available();
  //if (client) {
  //  server.write(client.read());
  //}

  //// Ethernet, from WebClient
  //if (client.available()) {
  //  char c = client.read();
  //  Serial.print(c);
  //}
  
  //if (!client.connected()) {
  //  Serial.println();
  //  Serial.println("disconnecting.");
  //  client.stop();
  //  for(;;)
  //    ;
  //}
}

