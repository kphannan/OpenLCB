#include <OLCB_AliasCache.h>
#include <OLCB_Buffer.h>
#include <OLCB_CAN_Buffer.h>
#include <OLCB_CAN_Link.h>
#include <OLCB_Datagram.h>
#include <OLCB_Datagram_Handler.h>
#include <OLCB_Event.h>
#include <OLCB_EventID.h>
#include <OLCB_Handler.h>
#include <OLCB_Link.h>
#include <OLCB_NodeID.h>
#include <OLCB_Stream.h>

#include <can.h>


OLCB_Datagram dg;


class Throttle: public OLCB_Datagram_Handler
{
 public:
  void init(void)
  {
      speed = 0;
      old_speed = 0;
      direction = 1; //forwards
      initialized = false;
  }
  
  void update(void)
  {
    if(!initialized)
    {
      dg.data[0] = 0x30;
      dg.data[1] = 0x01; //attach
      dg.length = 2;
//      initialized = true;
      sendDatagram(&dg);
    }
    else
    {
      unsigned int analog_value = analogRead(0);
      char raw_speed = (analog_value >> 4); //divide by two to take a 0-1023 range number and make it 0-127 range.
      
      if(speed != old_speed)
      {
        dg.data[0] = 0x30;
        dg.data[1] = 0x06; //set speed
        dg.data[2] = direction;
        dg.data[3] = speed;
        dg.length = 4;
        if(sendDatagram(&dg))
        {
          Serial.println("Datagram away! Sent to:");
        }
      }
    }
    OLCB_Datagram_Handler::update();
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
     if(!initialized && accepted)
     {
       initialized = true;
     }
  }
  boolean initialized;
 private:
  unsigned char speed, old_speed;
  unsigned char direction;
};

OLCB_NodeID nid(2,1,13,0,0,2);
OLCB_CAN_Link link(&nid);
Throttle datagram_handler;

OLCB_NodeID dest(6,1,0,0,0,3); //send to DCC address "03"

void setup() {
  Serial.begin(115200);
  
  delay(1000);
  Serial.println("Beginning initialization");  

  link.initialize();

  Serial.print("This is my alias (should not be 0): ");
  Serial.println(nid.alias);
  
  datagram_handler.setLink((OLCB_Link*)&link);
  
  dg.destination.copy(&dest);
    
  Serial.println("At end of init");
}

void loop() {  
  link.update();
}
