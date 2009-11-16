// makes this an Arduino file
#include "WConstants.h"

// The following line is needed because the Arduino environment 
// won't search a library directory unless the library is included 
// from the top level file (this file)
#include "CAN.h"

#include "OpenLcbCan.h"
#include "OpenLcbCanInterface.h"
#include "OpenLcbCanBuffer.h"
#include "NodeID.h"
#include "EventID.h"

  void OpenLcbCanBuffer::init() {
    // set default header: extended frame w low priority
    flags.extended = 1;
    id = 0x1FFFFFFF;  // all bits in header default to 1
  }

  void OpenLcbCanBuffer::setSourceAlias(unsigned int a) {
    id &= ~0x0000FFFFL;
    id = id | (a & 0xFFFFL);
  }
  
  unsigned int OpenLcbCanBuffer::getSourceAlias() {
      return id&0xFFFF;
  }

  void OpenLcbCanBuffer::setFrameTypeCAN() {
    id &= ~0x08000000L;     
  }
  
  boolean OpenLcbCanBuffer::isFrameTypeCAN() {
    return (id&0x08000000L) == 0x00000000L;
  }

  void OpenLcbCanBuffer::setFrameTypeOpenLcb() {
    id |= 0x08000000L;     
  }
  
  boolean OpenLcbCanBuffer::isFrameTypeOpenLcb() {
    return (id&0x08000000L) == 0x08000000L;
  }

  void OpenLcbCanBuffer::setVariableField(unsigned int f) {
    id &= ~0x07FF0000L;
    id |= (f & 0x7FFL)<<16;
  }

  unsigned int OpenLcbCanBuffer::getVariableField() {
    return (id&0x07FF0000L) >> 16;
  }
  
  void OpenLcbCanBuffer::setCIM(int i, unsigned int testval, unsigned int alias) {
    init();
    setFrameTypeCAN();
    setVariableField( ((i<<8)&0x700) | testval);
    setSourceAlias(alias);
    length=0;
  }

  boolean OpenLcbCanBuffer::isCIM() {
    return isFrameTypeCAN() && (getVariableField()&0x700) <= 0x5FF;
  }

  void OpenLcbCanBuffer::setRIM(unsigned int alias) {
    init();
    setFrameTypeCAN();
    setVariableField(0x7FF);
    setSourceAlias(alias);
    length=0;
  }

  boolean OpenLcbCanBuffer::isRIM() {
      return isFrameTypeCAN() && getVariableField() == 0x7FF;
  }

  boolean OpenLcbCanBuffer::isOpenLcbMTI(unsigned int mti) {
      return isFrameTypeOpenLcb() && getVariableField() == mti;
  }

  void OpenLcbCanBuffer::setPCEventReport(EventID* eid) {
    init();
    setFrameTypeOpenLcb();
    setVariableField(MTI_PCER);
    setSourceAlias(nodeAlias);
    length=8;
    loadFromEid(eid);
  }
  
  boolean OpenLcbCanBuffer::isPCEventReport() {
      return isOpenLcbMTI(MTI_PCER);
  }

  void OpenLcbCanBuffer::setInitializationComplete(unsigned int alias, NodeID* nid) {
    nodeAlias = alias;
    init();
    setFrameTypeOpenLcb();
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
  
  boolean OpenLcbCanBuffer::isInitializationComplete() {
      return isOpenLcbMTI(MTI_INITIALIZATION_COMPLETE);
  }
  
  void OpenLcbCanBuffer::getEventID(EventID* evt) {
    evt->val[0] = data[0];
    evt->val[1] = data[1];
    evt->val[2] = data[2];
    evt->val[3] = data[3];
    evt->val[4] = data[4];
    evt->val[5] = data[5];
    evt->val[6] = data[6];
    evt->val[7] = data[7];
  }
  
  void OpenLcbCanBuffer::getNodeID(NodeID* nid) {
    nid->val[0] = data[0];
    nid->val[1] = data[1];
    nid->val[2] = data[2];
    nid->val[3] = data[3];
    nid->val[4] = data[4];
    nid->val[5] = data[5];
  }
  
  boolean OpenLcbCanBuffer::isVerifyNID() {
      return isOpenLcbMTI(MTI_VERIFY_NID);
  }

  void OpenLcbCanBuffer::setVerifiedNID(NodeID* nid) {
    init();
    setFrameTypeOpenLcb();
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

  boolean OpenLcbCanBuffer::isIdentifyConsumers() {
      return isOpenLcbMTI(MTI_IDENTIFY_CONSUMERS);
  }

  void OpenLcbCanBuffer::setConsumerIdentified(EventID* eid) {
    init();
    setFrameTypeOpenLcb();
    setVariableField(MTI_CONSUMER_IDENTIFIED);
    setSourceAlias(nodeAlias);
    length=8;
    loadFromEid(eid);
  }

  void OpenLcbCanBuffer::setConsumerIdentifyRange(EventID* eid, EventID* mask) {
    // does send a message, but not complete yet - RGJ 2009-06-14
    init();
    setFrameTypeOpenLcb();
    setVariableField(MTI_CONSUMER_IDENTIFY_RANGE);
    setSourceAlias(nodeAlias);
    length=8;
    loadFromEid(eid);
  }

  boolean OpenLcbCanBuffer::isIdentifyProducers() {
      return isOpenLcbMTI(MTI_IDENTIFY_PRODUCERS);
  }

  void OpenLcbCanBuffer::setProducerIdentified(EventID* eid) {
    init();
    setFrameTypeOpenLcb();
    setVariableField(MTI_PRODUCER_IDENTIFIED);
    setSourceAlias(nodeAlias);
    length=8;
    loadFromEid(eid);
  }

  void OpenLcbCanBuffer::setProducerIdentifyRange(EventID* eid, EventID* mask) {
    // does send a message, but not complete yet - RGJ 2009-06-14
    init();
    setFrameTypeOpenLcb();
    setVariableField(MTI_PRODUCER_IDENTIFY_RANGE);
    setSourceAlias(nodeAlias);
    length=8;
    loadFromEid(eid);
  }

  boolean OpenLcbCanBuffer::isIdentifyEvents() {
      return isOpenLcbMTI(MTI_IDENTIFY_EVENTS);
  }

  void OpenLcbCanBuffer::loadFromEid(EventID* eid) {
    data[0] = eid->val[0];
    data[1] = eid->val[1];
    data[2] = eid->val[2];
    data[3] = eid->val[3];
    data[4] = eid->val[4];
    data[5] = eid->val[5];
    data[6] = eid->val[6];
    data[7] = eid->val[7];
  }