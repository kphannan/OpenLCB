#ifndef OpenLcbCanBuffer_h
#define OpenLcbCanBuffer_h

class NodeID;
class EventID;
#include "OpenLcbCanInterface.h"

/**
 * Class to handle transforming OpenLCB (S9.6) frames to/from std CAN frames.
 * <p>
 * We're trying to localize the formating of frames to/from the node here,
 * so that only this class needs to change when/if the wire protocol changes.
 */
 class OpenLcbCanBuffer : public CanInterfaceBuffer {
  public: 
  
  // Initialize a buffer for transmission
  void init();
  
  void setFrameTypeCAN();
  boolean isFrameTypeCAN();
  
  void setFrameTypeOpenLcb();
  boolean isFrameTypeOpenLcb();
  
  void setVariableField(unsigned int f);
  unsigned int getVariableField();
  
  void setSourceAlias(unsigned int a);
  unsigned int getSourceAlias();
  
  void setCIM(int i, unsigned int testval, unsigned int alias);
  boolean isCIM();
  
  void setRIM(unsigned int alias);
  boolean isRIM();

  void setInitializationComplete(unsigned int alias, NodeID* nid);
  boolean isInitializationComplete();

  boolean isOpenLcbMTI(unsigned int mti);
  
  void setPCEventReport(EventID* eid);
  boolean isPCEventReport();
  
  void getEventID(EventID* evt);
  void getNodeID(NodeID* nid);
  
  boolean isVerifyNID();
  void setVerifiedNID(NodeID* nid);

  boolean isIdentifyConsumers();
  
  void setConsumerIdentified(EventID* eid);
  
  // Mask uses an EventID data structure; 1 bit means mask out when routing
  void setConsumerIdentifyRange(EventID* eid, EventID* mask);

  boolean isIdentifyProducers();

  void setProducerIdentified(EventID* eid);

  // Mask uses an EventID data structure; 1 bit means mask out when routing
  void setProducerIdentifyRange(EventID* eid, EventID* mask);

  boolean isIdentifyEvents();

  
  private: 
  unsigned int nodeAlias;   // Initialization complete sets, all later use

  // service routine to copy content (0-7) to a previously-allocated Eid
  void loadFromEid(EventID* eid);
};

#endif
