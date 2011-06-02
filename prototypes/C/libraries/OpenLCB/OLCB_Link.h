#ifndef __OLCB_LINK_H__
#define __OLCB_LINK_H__

#include <can.h>
#include <stdint.h>
#include "OLCB_Event.h"
#include "OLCB_Handler.h"
#include "OLCB_Datagram.h"
#include "OLCB_Stream.h"
#include "OLCB_NodeID.h"

class OLCB_Handler;

/* An abstract base class representing a physical link to the outside world */

class OLCB_Link
{
 public:
  OLCB_Link(OLCB_NodeID *id) : _nodeID(id), _handlers(0) {}
  
  virtual bool initialize(void) {return false;} //called once, returns true if init succeeded.
    
  virtual void update(void); //called repeatedly  
  void addHandler(OLCB_Handler *handler);
  
  virtual bool addVNode(OLCB_NodeID *NID) {return true;}
  
  virtual bool sendEvent(OLCB_Event *event) {return true;}
  
  virtual uint8_t sendDatagramFragment(OLCB_Datagram *datagram, uint8_t start) {return 0;}
  virtual bool ackDatagram(OLCB_NodeID *source, OLCB_NodeID *dest) {return true;}
  virtual bool nakDatagram(OLCB_NodeID *source, OLCB_NodeID *dest, int reason) {return true;}
  
  virtual bool sendStream(OLCB_Stream *stream) {return true;}
  //Not sure that this is how streams should work at all!
  
  
  OLCB_NodeID* getNodeID(void) {return _nodeID;}
  
 protected:
  OLCB_Handler* _handlers;
  OLCB_NodeID* _nodeID;
};

#endif //__OLCB_LINK_H__