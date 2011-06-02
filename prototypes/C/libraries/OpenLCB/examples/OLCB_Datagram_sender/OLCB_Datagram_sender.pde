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

class simpleDatagramHandler: public OLCB_Datagram_Handler
{
 public:
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
};

OLCB_NodeID nid(2,1,13,0,0,2);
OLCB_CAN_Link link(&nid);
simpleDatagramHandler datagram_handler;

OLCB_Datagram dg;
OLCB_NodeID dest(6,1,0,0,0,1);

unsigned long timer, timer2;
unsigned char i = 1;

void setup() {
  Serial.begin(115200);
  
  delay(1000);
  Serial.println("Beginning initialization");  

  link.initialize();

  Serial.print("This is my alias (should not be 0): ");
  Serial.println(nid.alias);
  
  datagram_handler.setLink((OLCB_Link*)&link);
  
  dg.destination = &dest;
  dg.data[0] = 1;
  dg.data[1] = 2;
  dg.data[2] = 4;
  dg.length = 3;

  timer = 0;
  timer2 = millis();
  
  Serial.println("At end of init");
}

void loop() {
  timer = millis();
  if((timer - timer2) > 5000)
  {
    //send datagram
    if(datagram_handler.sendDatagram(&dg))
    {
      Serial.println("Datagram away!");
      dest.set(6,1,0,0,0,++i);
    }
//    else
//    {
//      Serial.println("Datagram sending failed :(");
//    }
    timer2 = timer;
  }
  
  
  link.update();
}
