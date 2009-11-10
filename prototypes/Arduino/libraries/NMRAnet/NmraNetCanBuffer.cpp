// makes this an Arduino file
#include "WConstants.h"

// The following line is needed because the Arduino environment 
// won't search a library directory unless the library is included 
// from the top level file (this file)
#include "CAN.h"

#include "NmraNetCan.h"
#include "NmraNetCanInterface.h"
#include "NmraNetCanBuffer.h"
#include "NodeID.h"
#include "EventID.h"

  void NmraNetCanBuffer::init() {
    // set default header: extended frame w low priority
    flags.extended = 1;
    id = 0x1FFFFFFF;  // all bits in header default to 1
  }

  void NmraNetCanBuffer::setSourceAlias(unsigned int a) {
    id &= ~0x0000FFFFL;
    id = id | (a & 0xFFFFL);
  }
  
  unsigned int NmraNetCanBuffer::getSourceAlias() {
      return id&0xFFFF;
  }

  void NmraNetCanBuffer::setFrameTypeCAN() {
    id &= ~0x08000000L;     
  }
  
  boolean NmraNetCanBuffer::isFrameTypeCAN() {
    return (id&0x08000000L) == 0x00000000L;
  }

  void NmraNetCanBuffer::setFrameTypeNmraNet() {
    id |= 0x08000000L;     
  }
  
  boolean NmraNetCanBuffer::isFrameTypeNmraNet() {
    return (id&0x08000000L) == 0x08000000L;
  }

  void NmraNetCanBuffer::setVariableField(unsigned int f) {
    id &= ~0x07FF0000L;
    id |= (f & 0x7FFL)<<16;
  }

  unsigned int NmraNetCanBuffer::getVariableField() {
    return (id&0x07FF0000L) >> 16;
  }
  
  void NmraNetCanBuffer::setCIM(int i, unsigned int testval, unsigned int alias) {
    init();
    setFrameTypeCAN();
    setVariableField( ((i<<8)&0x700) | testval);
    setSourceAlias(alias);
    length=0;
  }

  boolean NmraNetCanBuffer::isCIM() {
    return isFrameTypeCAN() && (getVariableField()&0x700) <= 0x5FF;
  }

  void NmraNetCanBuffer::setRIM(unsigned int alias) {
    init();
    setFrameTypeCAN();
    setVariableField(0x7FF);
    setSourceAlias(alias);
    length=0;
  }

  boolean NmraNetCanBuffer::isRIM() {
      return isFrameTypeCAN() && getVariableField() == 0x7FF;
  }

  boolean NmraNetCanBuffer::isNmraNetMTI(unsigned int mti) {
      return isFrameTypeNmraNet() && getVariableField() == mti;
  }

  void NmraNetCanBuffer::setPCEventReport(EventID* eid) {
    init();
    setFrameTypeNmraNet();
    setVariableField(MTI_PCER);
    setSourceAlias(nodeAlias);
    length=8;
    loadFromEid(eid);
  }
  
  boolean NmraNetCanBuffer::isPCEventReport() {
      return isNmraNetMTI(MTI_PCER);
  }

  void NmraNetCanBuffer::setInitializationComplete(unsigned int alias, NodeID* nid) {
    nodeAlias = alias;
    init();
    setFrameTypeNmraNet();
    setVariableField(MTI_INITIALIZATION_COMPLETE);
    setSourceAlias(nodeAlias);
    length=6;
    data[0] = nid->val[0];
    data[1] = nid->val[1];
    data[2] = nid->val[2];
    data[3] = nid->val[3];
    data[4] = nid->val[4];
    data[5] = nid->val[5];
  }
  
  boolean NmraNetCanBuffer::isInitializationComplete() {
      return isNmraNetMTI(MTI_INITIALIZATION_COMPLETE);
  }
  
  void NmraNetCanBuffer::getEventID(EventID* evt) {
    evt->val[0] = data[0];
    evt->val[1] = data[1];
    evt->val[2] = data[2];
    evt->val[3] = data[3];
    evt->val[4] = data[4];
    evt->val[5] = data[5];
    evt->val[6] = data[6];
    evt->val[7] = data[7];
  }
  
  void NmraNetCanBuffer::getNodeID(NodeID* nid) {
    nid->val[0] = data[0];
    nid->val[1] = data[1];
    nid->val[2] = data[2];
    nid->val[3] = data[3];
    nid->val[4] = data[4];
    nid->val[5] = data[5];
  }
  
  boolean NmraNetCanBuffer::isVerifyNID() {
      return isNmraNetMTI(MTI_VERIFY_NID);
  }

  void NmraNetCanBuffer::setVerifiedNID(NodeID* nid) {
    init();
    setFrameTypeNmraNet();
    setVariableField(MTI_VERIFIED_NID);
    setSourceAlias(nodeAlias);
    length=6;
    data[0] = nid->val[0];
    data[1] = nid->val[1];
    data[2] = nid->val[2];
    data[3] = nid->val[3];
    data[4] = nid->val[4];
    data[5] = nid->val[5];
  }

  boolean NmraNetCanBuffer::isIdentifyConsumers() {
      return isNmraNetMTI(MTI_IDENTIFY_CONSUMERS);
  }

  void NmraNetCanBuffer::setConsumerIdentified(EventID* eid) {
    init();
    setFrameTypeNmraNet();
    setVariableField(MTI_CONSUMER_IDENTIFIED);
    setSourceAlias(nodeAlias);
    length=8;
    loadFromEid(eid);
  }

  void NmraNetCanBuffer::setConsumerIdentifyRange(EventID* eid, EventID* mask) {
    // does send a message, but not complete yet - RGJ 2009-06-14
    init();
    setFrameTypeNmraNet();
    setVariableField(MTI_CONSUMER_IDENTIFY_RANGE);
    setSourceAlias(nodeAlias);
    length=8;
    loadFromEid(eid);
  }

  boolean NmraNetCanBuffer::isIdentifyProducers() {
      return isNmraNetMTI(MTI_IDENTIFY_PRODUCERS);
  }

  void NmraNetCanBuffer::setProducerIdentified(EventID* eid) {
    init();
    setFrameTypeNmraNet();
    setVariableField(MTI_PRODUCER_IDENTIFIED);
    setSourceAlias(nodeAlias);
    length=8;
    loadFromEid(eid);
  }

  void NmraNetCanBuffer::setProducerIdentifyRange(EventID* eid, EventID* mask) {
    // does send a message, but not complete yet - RGJ 2009-06-14
    init();
    setFrameTypeNmraNet();
    setVariableField(MTI_PRODUCER_IDENTIFY_RANGE);
    setSourceAlias(nodeAlias);
    length=8;
    loadFromEid(eid);
  }

  boolean NmraNetCanBuffer::isIdentifyEvents() {
      return isNmraNetMTI(MTI_IDENTIFY_EVENTS);
  }

  void NmraNetCanBuffer::loadFromEid(EventID* eid) {
    data[0] = eid->val[0];
    data[1] = eid->val[1];
    data[2] = eid->val[2];
    data[3] = eid->val[3];
    data[4] = eid->val[4];
    data[5] = eid->val[5];
    data[6] = eid->val[6];
    data[7] = eid->val[7];
  }