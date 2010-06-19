// makes this an Arduino file
#include "WConstants.h"

#include "Datagram.h"

#include "NodeID.h"
#include "OpenLcbCanBuffer.h"

#include "logging.h"

Datagram::Datagram(OpenLcbCanBuffer* b, NodeID* n, void (*cb)(int i)) {
      buffer = b;
      nid = n;
      callback = cb;      
}

void Datagram::check() {
  // see in any replies are waiting to send
  //OpenLcb_can_queue_xmt_wait(buffer);  // wait until buffer queued, but OK due to earlier check
}

void Datagram::receivedFrame(OpenLcbCanBuffer* rcv) {
}
