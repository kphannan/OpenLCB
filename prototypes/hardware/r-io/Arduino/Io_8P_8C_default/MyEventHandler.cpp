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
  if(isPermitted());
    return OLCB_Event_Handler::handleMessage(buffer);

  return false;
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
  //read the stored EventIDs from EEPROM
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
  {
    //Serial.println("EEPROM not formatted!");
    factoryReset();
  }
  else
  {
      //Serial.println("EEPROM already formatted");
  }
  //now, read it back out into SRAM
  load(); //TODO add check for valid EEPROM.
  
  //and a little more setup...first the 16 producers
  for(uint8_t i = 0; i < 16; ++i)
    newEvent(i, true, false);
  //and the 16 consumers
  for(uint8_t i = 16; i < 32; ++i)
    newEvent(i, false, true);
  
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
    EEPROM.write(0, 'X');
  }

  //first, increment the next available ID by 16, and write it back
  if(eid7 == 240) //need to increment val[6] as well
  {
    EEPROM.write(2, eid6+1); //might wrap around to 0; it's gonna happen, I guess
  }
  else
  {
    EEPROM.write(2, eid6);
  }
  EEPROM.write(3, eid7+16);

  //now, increment through the producers, and write the new EventIDs
  for(i = 0; i < _numEvents/2; ++i)
  {
    for(j = 0; j < 6; ++j)
    {
      EEPROM.write((i*8)+j+4, OLCB_Virtual_Node::NID->val[j]);
    }
    EEPROM.write((i*8)+6+4, eid6);
    EEPROM.write((i*8)+7+4, eid7+i);
  }
  //now do it again, so that the consumers have the same EventIDs as the producers
  for(i = _numEvents/2; i < _numEvents; ++i)
  {
    for(j = 0; j < 6; ++j)
    {
      EEPROM.write((i*8)+j+4, OLCB_Virtual_Node::NID->val[j]);
    }
    EEPROM.write((i*8)+6+4, eid6);
    EEPROM.write((i*8)+7+4, eid7+(i-(_numEvents/2)));
  }
  //TODO CHECK TO SEE IF THESE NEED TO BE WRITTEn!!
  EEPROM.write(0, 'I');
  if(EEPROM.read(1) != 'o')
    EEPROM.write(1, 'o');
}

void MyEventHandler::update(void)
{
  if(!isPermitted())
    return;
    
  for(uint8_t i = 0; i < 8; ++i)
    _input_buttons[i].process();
    //first, are we going to set up any kind of learning? TODO

  if(!_inhibit)
  {
    // looks at the input pins and
    // and decide whether and which events to fire
    // with pce.produce(i);
    // inputs are pins 8..15
    uint8_t state, prev_state;
    for(uint8_t i = 0; i < 8; ++i)
    {
      state = _input_buttons[i].state; //digitalRead(i+8);
      prev_state = (_inputs & (1<<i))>>i;
      if(state != prev_state) //change in state!
      {	//input has changed, fire event and update flag
        //Serial.print("input state change to ");
        //Serial.print(state, DEC);
        //Serial.print(" on ");
        //Serial.println(i, DEC);
        //Serial.println(_inputs, BIN);
        _inputs ^= (1<<i); //toggle flag
        //now determine which producer to fire.
        produce((i<<1) | !(state&0x01)); //TODO NOT RIGHT!
      }
    }
  }
  OLCB_Event_Handler::update(); //called last to permit the new events to be sent out immediately.
  if(_dirty) //check to see if we need to dump memory to EEPROM
  {
    store();
    _dirty = 0;
  }
}

bool MyEventHandler::consume(uint16_t index)
{
  if(_inhibit)
    return true;
  /* We've received an event; let's see if we need to consume it */
  //Serial.print("consume() ");
  //Serial.println(index,DEC);
  //Outputs are pins 0..7
  //odd events are off, even events are on
  digitalWrite((index-16)>>1, !(index&0x1));
  return true;
}



uint8_t MyEventHandler::readConfig(uint16_t address, uint8_t length, uint8_t *data)
{
  //This method gets called by configuration handlers. Basically, we are being asked for an EventID. We'll simply read from memory.
  //decode the address into a producer/consumer by dividing by 8
  uint8_t index = (address>>3);
  uint16_t offset = address - (index<<3);
    if( (length+address) > (_numEvents*8) ) //too much! Would cause overflow
    //TODO caculate a shorter length to prevent overflow
    length = (_numEvents*8) - (address);
  //Serial.println("readConfig");
  //Serial.print("length: ");
  //Serial.println(length);
  //Serial.print("offset: ");
  //Serial.println(offset);
  //Serial.print("reading eventID from event pool index ");
  //Serial.println(index, DEC);
  //we can't do a straight memcpy, because EventIDs are actually NINE bytes long (the flags), and we want to skip every ninth byte.
  //so, we skip an address if it is even divisible by 8?
  uint8_t i, j, k;
  j = offset;
  k = index;
  for(i = 0; i < length; ++i, ++j)
  {
    if(j == 8)
    {
        j = 0;
        ++k;
    }
    *(data+i) = _events[k].val[j];
    //Serial.println(_events[k].val[j], HEX);
  }
  //Serial.println("===");
  //for(i = 0; i < length; ++i)
    //Serial.println(*(data+i), HEX);
  return length;
}

void MyEventHandler::writeConfig(uint16_t address, uint8_t length, uint8_t *data)
{
  //This method gets called by configuration handlers. We are being asked to write an EventID. We'll write it to memory, and do a lazy write to EEPROM later.
    _dirty = 1;

    //decode the address into a producer/consumer by dividing by 8
  uint8_t index = (address>>3);
  uint16_t offset = address - (index<<3);
    if( (length+address) > (_numEvents*8) ) //too much! Would cause overflow
    //TODO caculate a shorter length to prevent overflow
    length = (_numEvents*8) - (address);
  //Serial.println("writeConfig");
  //Serial.print("length: ");
  //Serial.println(length);
  //Serial.print("offset: ");
  //Serial.println(offset);
  //Serial.print("writing eventID from event pool index ");
  //Serial.println(index, DEC);
  //we can't do a straight memcpy, because EventIDs are actually NINE bytes long (the flags), and we want to skip every ninth byte.
  //so, we skip an address if it is even divisible by 8?
  uint8_t i, j, k;
  j = offset;
  k = index;
  for(i = 0; i < length; ++i, ++j)
  {
    if(j == 8)
    {
        j = 0;
        ++k;
    }
    _events[k].val[j] = *(data+i);
    //Serial.println(_events[k].val[j], HEX);
  }
  //Serial.println("===");
  //for(i = 0; i < length; ++i)
    //Serial.println(*(data+i), HEX);
}

