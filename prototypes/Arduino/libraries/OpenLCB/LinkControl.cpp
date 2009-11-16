// makes this an Arduino file
#include "WConstants.h"

#include "LinkControl.h"

#include "OpenLcbCanBuffer.h"
#include "OpenLcbCanInterface.h"
#include "NodeID.h"

// state machine definitions
#define STATE_INITIAL 0
#define STATE_WAIT_CONFIRM 10
#define STATE_ALIAS_ASSIGNED 20
#define STATE_INITIALIZED 30

// time to wait between last CIM and RIM
#define CONFIRM_WAIT_TIME 500

// define some rudimentary logging
#define log(...) Serial.print(__VA_ARGS__)
#define logln(...) Serial.println(__VA_ARGS__)
#include "HardwareSerial.h"

LinkControl::LinkControl(OpenLcbCanBuffer* b, NodeID* n) {
  txBuffer = b;
  nid = n;
  // initialize sequence from node ID
  uint32_t t0 = nid->val[0];
  uint32_t t1 = nid->val[1];
  uint32_t t2 = nid->val[2];
  uint32_t t3 = nid->val[3];
  uint32_t t4 = nid->val[4];
  uint32_t t5 = nid->val[5];
  lfsr = t0 ^ t1 <<5 ^ t2 <<10 ^ t3 <<15 ^ t4 << 20 ^ t5 << 24;
  if (lfsr == 0)
     lfsr = (t0 << 23)+(t1 << 19)+(t2 << 15)+(t3 << 11)+(t4 << 7)+t5;
  if (lfsr == 0)
     lfsr = 0xAC01;
  // set up for next (first) alias
  reset();
}

void LinkControl::nextAlias() {
   // step the PRNG; see <http://en.wikipedia.org/wiki/Linear_feedback_shift_register> example 2
   lfsr = (lfsr >> 1) ^ (-(lfsr & 1u) & 0xd0000001u); 
}

void LinkControl::reset() {
  state = STATE_INITIAL;
  // take the 1st from the sequence
  nextAlias();
  log("new key ");log(lfsr, HEX);log(" alias ");logln(getAlias(), HEX);
}

boolean LinkControl::sendCIM(int i) {
  if (!OpenLcb_can_xmt_ready(txBuffer)) return false;  // couldn't send just now  if (!isTxBufferFree()) return false;  // couldn't send just now
  txBuffer->setCIM(i,nid->val[i],getAlias());
  OpenLcb_can_queue_xmt_wait(txBuffer);  // wait for queue, but earlier check says will succeed
  return true;
}

boolean LinkControl::sendRIM() {
  if (!OpenLcb_can_xmt_ready(txBuffer)) return false;  // couldn't send just now  if (!isTxBufferFree()) return false;  // couldn't send just now
  txBuffer->setRIM(getAlias());
  OpenLcb_can_queue_xmt_wait(txBuffer);  // wait for queue, but earlier check says will succeed
  return true;
}

boolean LinkControl::sendInitializationComplete() {
  if (!OpenLcb_can_xmt_ready(txBuffer)) return false;  // couldn't send just now  if (!isTxBufferFree()) return false;  // couldn't send just now
  txBuffer->setInitializationComplete(getAlias(), nid);
  OpenLcb_can_queue_xmt_wait(txBuffer);  // wait for queue, but earlier check says will succeed
  return true;
}

void LinkControl::check() {
  // find current state and act
  if (state == STATE_INITIALIZED) return;
  switch (state) {
  case STATE_INITIAL+0:
  case STATE_INITIAL+1:
  case STATE_INITIAL+2:
  case STATE_INITIAL+3:
  case STATE_INITIAL+4:
  case STATE_INITIAL+5:
    // send next CIM message if possible
    if (sendCIM(state-STATE_INITIAL)) 
      state++;
    return;
  case STATE_INITIAL+6:
    // last CIM, sent, wait for delay
    timer = millis();
    state = STATE_WAIT_CONFIRM; 
    log("alias assigned ");logln(getAlias(), HEX);

    return;
  case STATE_WAIT_CONFIRM:
    if ( (millis() > timer+CONFIRM_WAIT_TIME) && sendRIM()) {
      state = STATE_ALIAS_ASSIGNED;
    }
    return;
  case STATE_ALIAS_ASSIGNED:
    // send init
    if (sendInitializationComplete()) 
      state = STATE_INITIALIZED;
    return;
  default:
    return;
  }
}

boolean LinkControl::linkInitialized() {
  return state == STATE_INITIALIZED;
}

unsigned int LinkControl::getAlias() {
  return (lfsr>>16)&0xFFFF;
}

void LinkControl::receivedFrame(OpenLcbCanBuffer* rcv) {
   // check received message
   // see if this is a frame with our alias
   if (getAlias() == rcv->getSourceAlias()) {
     // somebody else trying to use this one, see to what extent
     if (rcv->isCIM()) {
       // somebody else trying to allocate, tell them
       while (!sendRIM()) {}  // insist on sending it now.
     } else if (rcv->isRIM()) {
       // RIM frame is an error, restart
       reset();
     } else {
       // some other frame; this is an error! Restart
       reset();
     }
   }
   // see if this is a Verify request to us; first check type
   if (rcv->isVerifyNID()) {
     // check address
     NodeID n;
     rcv->getNodeID(&n);
     if (n.equals(nid)) {
       // reply; should be threaded, but isn't
       txBuffer->setVerifiedNID(nid);
       OpenLcb_can_queue_xmt_wait(txBuffer);
     }
   }
}
