#include "MyEventHandler.h"
#include <EEPROM.h> 
//really, should probably be using avr/eeprom.h, but this is going to be more portable in the end, I think?


void MyEventHandler::create(OLCB_Link *link, OLCB_NodeID *nid)
{
  OLCB_Event_Handler::create(link,nid);
  OLCB_Virtual_Node::create(link,nid);
}

bool MyEventHandler::handleMessage(OLCB_Buffer *buffer)
{   
  return OLCB_Event_Handler::handleMessage(buffer);
}

bool MyEventHandler::store(void)
{
  //write the stored EventIDs into EEPROM
  //TODO just brute forcing it. Probably a better way!
  for(uint8_t i = 0; i < _numEvents; ++i)
  {
    for(uint8_t j = 0; j < 8; ++j)
    {
      EEPROM.write((i*8)+j+4, _events[i].val[j]);
    }
  }
  return true;
}

bool MyEventHandler::load(void)
{
  //write the stored EventIDs into EEPROM
  for(uint8_t i = 0; i < _numEvents; ++i)
  {
    for(uint8_t j = 0; j < 8; ++j)
    {
      _events[i].val[j] = EEPROM.read((i*8)+j+4);
    }
  }
  return true;
}

void MyEventHandler::initialize(OLCB_Event *events, uint8_t num)
{

  /****
   * Notes on EEPROM:
   * This class checks to see if the EEPROM has been programmed by checking two bytes, to see if they contain the string "Io". If not, we know that the EEPROM has not been formated, and the class proceeds by writing several things to EEPROM:
   * 	1) the string "Io"
   * 	2) the next available EventID, namely, NodeID.0.0, stored as two bytes
   * 	3) A set of 32 EventIDs
   * On the other hand, if the string "Io" is present, the EEPROM is read into memory
   ****/
  _events = events;
  _numEvents = num;

  //first, check first two bytes:
  if( (EEPROM.read(0) != 'I') || (EEPROM.read(1) != 'o') ) //not formatted!
    factoryReset();

  //now, read it back out into SRAM
  load();
}

void MyEventHandler::factoryReset(void)
{
  // WARNING: THIS FUNCTION RESETS THE EVENT POOL TO FACTORY SETTINGS!
  //first, check to see if the EEPROM has been formatted yet.
  uint8_t eid6=0, eid7=0, j;
  uint8_t i;
  bool formatted = false;
  if( (char(EEPROM.read(0)) == 'I') && (char(EEPROM.read(1)) == 'o') ) //it IS formatted!
  {
    formatted = true;
    //grab the actual next available EventID
    eid6 = EEPROM.read(2);
    eid7 = EEPROM.read(3);
  }

  //first, increment the next available ID by 16, and write it back
  if(eid7 == 240) //need to increment val[6] as well
    EEPROM.write(2, eid6+1); //might wrap around to 0; it's gonna happen, I guess
  else
    EEPROM.write(2, eid6);
  EEPROM.write(3, eid7+16);

  //now, increment through the eventIDs, and set them to the new values
  for(i = 0; i < _numEvents; ++i)
  {
    for(j = 0; j < 6; ++j)
    {
      EEPROM.write((i*8)+j+4, OLCB_Virtual_Node::NID->val[j]);
    }
    EEPROM.write((i*8)+6+4, eid6);
    EEPROM.write((i*8)+7+4, eid7+i);
  }
}

void MyEventHandler::update(void)
{
  if(isPermitted())
  {
    //first, are we going to set up any kind of learning? TODO

    // looks at the input pins and
    // and decide whether and which events to fire
    // with pce.produce(i);
    // inputs are pins 8..15
    uint8_t state, i;
    for(i = 0; i < 8; ++i)
    {
      state = digitalRead(i+8);
      if( state !=  ((_inputs & (1<<i))>>i) )
      {	//input has changed, fire event and update flag
        _inputs ^= (1<<i); //toggle flag
        produce((i<<1) | state);
      }	
    }
    OLCB_Event_Handler::update(); //called last to permit the new events to be sent out immediately.
    if(_dirty) //check to see if we need to dump memory to EEPROM
    {
      store();
      _dirty = 0;
    }
  }
}

bool MyEventHandler::consume(OLCB_Event *event)
{
  /* We've received an event; let's see if we need to consume it */
  int index = event->findIndexInArray(_events, _numEvents);
  if(index == -1)
    return false;
  //Outputs are pins 0..7
  //odd events are off, even events are on
  digitalWrite(index>>1, index&0x1);
  return true;
}



void MyEventHandler::readConfig(uint16_t address, uint8_t length, uint8_t *data)
{
  //This method gets called by configuration handlers. Basically, we are being asked for an EventID. We'll simply read from memory.
  //decode the address into a producer/consumer by dividing by 8
  //TODO THIS PRESUMES THAT THE REQUEST IS BEING MADE AT EVENTID BOUNDARIES! HAMFISTED!!!! WE ARE ALSO IGNORING LENGTH!
  uint8_t index = (address>>3);
  memcpy(data, _events[index].val, 8);
}

void MyEventHandler::writeConfig(uint16_t address, uint8_t length, uint8_t *data)
{
  //This method gets called by configuration handlers. We are being asked to write an EventID. We'll write it to memory, and do a lazy write to EEPROM later.
    _dirty = 1;

  //decode the address into a producer/consumer by dividing by 8
  //TODO THIS PRESUMES THAT THE REQUEST IS BEING MADE AT EVENTID BOUNDARIES! HAMFISTED!!!! WE ARE ALSO IGNORING LENGTH!
  uint8_t index = (address>>3);
  memcpy(_events[index].val, data, 8);
}

