#ifndef __MYEVENTHANDLER_H__
#define __MYEVENTHANDLER_H__

#include "ButtonLedDON.h"
#include "OLCB_Virtual_Node.h"
#include "OLCB_Event_Handler.h"

extern ButtonLed blue; // button on pin 14
extern ButtonLed gold; // button on pin 15

#ifndef EVENT_POOL_SIZE
#define EVENT_POOL_SIZE 64
#endif


/************************
Here is the class for handling PC Event Reports
************************/
class MyEventHandler: public OLCB_Virtual_Node, public OLCB_Event_Handler
{
  public:
  	MyEventHandler(void) : _inputs(0), _dirty(0)
  	{
		return;
  	}
    void initialize(OLCB_Event *events, uint8_t num); //assume offset is 0, uint8_t offset);
    
    void factoryReset(void);

    void create(OLCB_Link *link, OLCB_NodeID *nid);
    
    bool handleMessage(OLCB_Buffer *buffer);
        
    void update(void);

    bool consume(OLCB_Event *event);
    
    void readConfig(uint16_t address, uint8_t length, uint8_t *data);
    void writeConfig(uint16_t address, uint8_t length, uint8_t *data);
        
  protected:
    bool store(void);
    bool load(void);

  private:
  	uint8_t _inputs;
  	uint8_t _dirty;
};


#endif //__MYEVENTHANDLER_H__
