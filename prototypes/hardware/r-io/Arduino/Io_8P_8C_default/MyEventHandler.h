#ifndef __MYEVENTHANDLER_H__
#define __MYEVENTHANDLER_H__

extern ButtonLed blue; // button on pin 14
extern ButtonLed gold; // button on pin 15


/************************
Here is the class for handling PC Event Reports
************************/
class MyEventHandler: public OLCB_Virtual_Node, public OLCB_Event_Handler
{
  public:
    void initialize(OLCB_Event *events, uint8_t num); //assume offset is 0, uint8_t offset);
    
    void factoryReset(void);

    void create(OLCB_Link *link, OLCB_NodeID *nid);
    
    bool handleMessage(OLCB_Buffer *buffer);
        
    void update(void);

    bool consume(OLCB_Event *event);
        
  private:
    void store(uint8_t index);

    /*************
	Here's how we're going to handle mapping events produced or consumed by each input/output:
	We have a pool of 64 EventIDs in memory = 64*8 bytes = 512bytes of EEPROM (we have that!)
	Each is paired with four additional bytes that are bitfields representing the inputs and outputs (in that order).
	*************/
	uint8_t _event_consumer_on_mask[EVENT_POOL_SIZE];
	uint8_t _event_consumer_off_mask[EVENT_POOL_SIZE];
	uint8_t _event_producer_on_mask[EVENT_POOL_SIZE];
	uint8_t _event_producer_off_mask[EVENT_POOL_SIZE];
	uint8_t _consumer_on_learn_field, _consumer_off_learn_field;
	uint8_t _producer_on_learn_field, _producer_off_learn_field;
	uint8_t _consumer_on_forget_field, _consumer_off_forget_field;
	uint8_t _producer_on_forget_field, _producer_off_forget_field;
};


#endif //__MYEVENTHANDLER_H__
