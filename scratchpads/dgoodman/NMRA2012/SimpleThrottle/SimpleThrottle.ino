#include <ButtonLED.h>

#include <EEPROM.h>

// Permits control of a single locomotive with address 03.

#define MAX_SPEED 100


#include <DCCPacket.h>
#include <DCCPacketQueue.h>
#include <DCCPacketScheduler.h>

#include <can.h>

#include <OpenLCB.h>

#include "OLCB_Train_Controller.h"

ButtonLed gold(GOLD);

float fmap(float x, float in_min, float in_max, float out_min, float out_max)
{
  return (x - in_min) * (out_max - out_min) / (in_max - in_min) + out_min;
}


class DCC_Throttle : public OLCB_Datagram_Handler, public OLCB_Train_Controller
{
  public:
  
    void create(OLCB_Link *link, OLCB_NodeID *nid)
    {
      OLCB_Datagram_Handler::create(link,nid);
      OLCB_Virtual_Node::create(link,nid);
      Train_Controller_create((OLCB_Datagram_Handler *)this);
    }

  bool handleMessage(OLCB_Buffer *buffer)
    {
      return OLCB_Datagram_Handler::handleMessage(buffer);
    }
    
  void update(void)
  {
    float a = analogRead(A0);
    if(a >= 315)
      a = fmap(a, 430, 315, -MAX_SPEED, 0);
    else
      a = fmap(a, 315, 0, 0, MAX_SPEED);
    Serial.println(a);
    
    //Serial.println(a, 4);
    Train_Controller_setSpeed_m_s(a); //divide by two to take a 0-1023 range number and make it 0-5 m/s range.
    if(isPermitted())
    {
      OLCB_Datagram_Handler::update();
      Train_Controller_update();
    }
  }

  void datagramResult(bool accepted, uint16_t errorcode)
  {
     Serial.print("The datagram was ");
     if(!accepted)
       Serial.print("not ");
     Serial.println("accepted.");
     if(!accepted)
     {
       Serial.print("   The reason: ");
       Serial.println(errorcode,HEX);
     }
  }
  
  void initialize(void)
  {
    Train_Controller_initialize();
  }
  
  uint16_t processDatagram(void)
  {
    if(isPermitted() && (_rxDatagramBuffer->destination == *OLCB_Virtual_Node::NID))
    {
      return Train_Controller_processDatagram(_rxDatagramBuffer);
    }
  }
  
  
};

void loadNodeID(OLCB_NodeID *nid)
{
	//The NodeID is stored at the very end of EEPROM
	//on the AT90CAN128, the last EEPROM address is 0x0FFF
	//So:
  for(uint16_t i = 0; i < 6; ++i)
    nid->val[i] = EEPROM.read(0x0FFA+i);
  if((nid->val[0] == 0xFF) &&  (nid->val[1] == 0xFF) && (nid->val[2] == 0xFF) && (nid->val[3] == 0xFF) && (nid->val[4] == 0xFF) && (nid->val[5] == 0xFF) )
        {
            //NO NODE ID!!
            while(1);
        }
}

OLCB_NodeID train_nid(6,1,0,0,0,3);
OLCB_NodeID nid;
DCC_Throttle myThrottle;
OLCB_CAN_Link link;

void setup()
{
  Serial.begin(115200);
  
  Serial.println("SimpleThrottle begin!");
  loadNodeID(&nid);
  link.initialize();
  myThrottle.initialize();
  myThrottle.create(&link, &nid);
  myThrottle.Train_Controller_attach(&train_nid);
  link.addVNode(&myThrottle);
  
  gold.on(0xFFFFFFFEL); // unready blink until intialized

  Serial.println("Initialization complete!");
}

void loop()
{
  //read analogl line for speed!
  link.update();
  gold.process();
}
