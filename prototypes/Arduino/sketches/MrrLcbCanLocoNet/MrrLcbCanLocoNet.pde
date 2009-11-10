#include <LocoNet.h>
#include <CAN.h>

#include <Event.h>
#include <EventID.h>
#include <LinkControl.h>
#include <NmraNetCan.h>
#include <NmraNetCanBuffer.h>
#include <NmraNetCanInterface.h>
#include <NodeID.h>
#include <PCE.h>

#define         BAUD_RATE       115200

NmraNetCanBuffer     rxBuffer;	// CAN receive buffer
NmraNetCanBuffer     txBuffer;	// CAN send buffer
NmraNetCanBuffer*    ptxCAN;

class foo{};  // force Arduino environment to treat the rest of this file as C++

NodeID nodeid(3,4,5,6,7,8);    // This node's ID
LinkControl link(&txBuffer, &nodeid);
EventID   eid(3,4,5,6,7,0,0,0);  // Make the first 5 bytes the same as the NodeId

lnMsg        *LnPacket;

void setup()
{
  // set up serial comm
  Serial.begin(BAUD_RATE);
  Serial.println();Serial.println("Starting MrrLcbCanLocoNet");
  
  // Init the LocoNet interface
  LocoNet.init();

  // Initialize NmraNet CAN connection
  NMRAnet_can_init();
  
  // Initialize NmraNet CAN link controller
  link.reset();
}

void loop() {
  // check for input frames, acquire if present
  boolean rcvFramePresent = NMRAnet_can_get_frame(&rxBuffer);
  
  // process link control first
  link.check();
  if (rcvFramePresent)
  {
    // received a frame, ask if changes link state
    link.receivedFrame(&rxBuffer);
  }

  LnPacket = LocoNet.receive() ;
  if(LnPacket)
  {
    // First print out the packet in HEX
    Serial.print("LN RX: ");
    uint8_t msgLen = getLnMsgSize(LnPacket); 
    for (uint8_t x = 0; x < msgLen; x++)
    {
      uint8_t val = LnPacket->data[x];
        // Print a leading 0 if less than 16 to make 2 HEX digits
      if(val < 16)
        Serial.print('0');
       
      Serial.print(val, HEX);
      Serial.print(' ');
    }
    Serial.println();
  }
  
  // if link is initialized, higher-level operations possible
  if (link.linkInitialized())
  {
    if(rcvFramePresent && rxBuffer.isIdentifyProducers())
    {
      eid.val[5] = 0;
      eid.val[6] = 0;
      eid.val[7] = 0;
      txBuffer.setProducerIdentifyRange(&eid,0);
      NMRAnet_can_queue_xmt_wait(&txBuffer);
    }
    
    if(LnPacket)
    {   
      switch(LnPacket->data[0])
      {
      case OPC_GPON:
      case OPC_GPOFF:
        eid.val[5] = LnPacket->data[0];
        eid.val[6] = 0;
        eid.val[7] = 0;
        
        txBuffer.setPCEventReport(&eid);
        NMRAnet_can_queue_xmt_wait(&txBuffer);
        break;
        
      case OPC_SW_REQ:
      case OPC_SW_REP:
      case OPC_INPUT_REP:
      case OPC_SW_STATE:
        eid.val[5] = LnPacket->data[0];
        eid.val[6] = LnPacket->data[1];
        eid.val[7] = LnPacket->data[2];

        txBuffer.setPCEventReport(&eid);
        NMRAnet_can_queue_xmt_wait(&txBuffer);
        break;
      }
    }
  }
}

