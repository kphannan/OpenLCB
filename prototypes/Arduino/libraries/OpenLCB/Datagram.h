#ifndef Datagram_h
#define Datagram_h

/**
 * Class for handling Datagrams
 *
 * This combines Receiver and Transmitter for now;
 * perhaps they need to be refactored separately later.
 *<p>
 * Basic state machine handles transmission and reception.
 */

#define DATAGRAM_LENGTH 70
class NodeID;
class OpenLcbCanBuffer;

class Datagram {
  public:

  /**
   * Handle any routine processing that needs to be done.
   * Go through this in loop() to e.g. send pending messages
   */
  void check();
  
  /**
   * When a CAN frame is received, it should
   * be transferred to the PCER object via this method
   * so that it can handle the verification protocol.
   */
  void receivedFrame(OpenLcbCanBuffer* rcv);
  
  Datagram(OpenLcbCanBuffer* b, NodeID* nid, void (*callback)(int i));
  
  private:
  OpenLcbCanBuffer* buffer;
  NodeID* nid;
  void (*callback)(int i);   // void callback(int index) pointer

};

#endif
