#ifndef __MYEVENTHANDLER_H__
#define __MYEVENTHANDLER_H__

#include "ButtonLED.h"

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
  	MyEventHandler(void) : _inhibit(false), _inputs(0x00), _dirty(0)
  	{
              for(uint8_t i = 0; i < 8; ++i)
              {
                _input_buttons[i].setPinSense(i+8, LOW);
              }
  	}
    void initialize(OLCB_Event *events, uint8_t num); //assume offset is 0, uint8_t offset);
    
    void factoryReset(void);

    void create(OLCB_Link *link, OLCB_NodeID *nid);
    
    bool handleMessage(OLCB_Buffer *buffer);
        
    void update(void);

    bool consume(OLCB_Event *event);
    
    void readConfig(uint16_t address, uint8_t length, uint8_t *data);
    void writeConfig(uint16_t address, uint8_t length, uint8_t *data);
    ButtonLed *getInputs(void) {return _input_buttons;}
    void inhibit(void) {_inhibit = true; Serial.println("INHIBITED!");}
    void disInhibit(void) {_inhibit = false; Serial.println("inhibit released");}
    
  protected:
    bool store(void);
    bool load(void);

  private:
  	bool _inhibit;
  	uint8_t _inputs;
  	uint8_t _dirty;
  	ButtonLed _input_buttons[8];
};


#endif //__MYEVENTHANDLER_H__
