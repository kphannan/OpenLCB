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
	//we're going to handle LearnEvents differently!
	if(buffer->isLearnEvent())
    {
        OLCB_Event event;
    	OLCB_Event empty(0,0,0,0,0,0,0,0);
        buffer->getEventID(&event);
        //first, let's generate an index into the event pool:
        uint8_t index = event->findIndexInArray(_events, &event);
        if(index = -1 && (_consumer_on_learn_field | _consumer_off_learn_field | _producer_on_learn_field | _producer_off_learn_field) //not a duplicate, let's find an empty slot, but only if we're learning and not just forgetting. in which case this learn message is no use to us
        {
        	index = event->findIndexInArray(_events, &empty);
        	if(index = -1) //no room!
        	{
        		//TODO FLAG AN ERROR CONDITION SOMEHOW!!
        		return false;
        	}
        	else //copy the NID over
        	{
        		memcpy(_events[index].val, event->val, 8);
        	}
        }
		if(index = -1) //we can't unlearn this event
        {
        	return false;
        }
        
        //second, let's OR the event masks with the learning fields
        _event_consumer_on_mask[index] |= _consumer_on_learn_field;
        _event_consumer_off_mask[index] |= _consumer_off_learn_field;
        _event_producer_on_mask[index] |= _producer_on_learn_field;
        _event_producer_off_mask[index] |= _producer_off_learn_field;
        //and reset the learn fields
        _consumer_on_learn_field = _consumer_off_learn_field = _producer_on_learn_field = _producer_off_learn_field = 0;
        
        //second, let's XOR the event masks with the forgetting fields
		_event_consumer_on_mask[index] ^= _consumer_on_forget_field;
        _event_consumer_off_mask[index] ^= _consumer_off_forget_field;
        _event_producer_on_mask[index] ^= _producer_on_forget_field;
        _event_producer_off_mask[index] ^= _producer_off_forget_field;
        //and reset the forget fields
        _consumer_on_forget_field = _consumer_off_forget_field = _producer_on_forget_field = _producer_off_forget_field = 0;
        
        //finally, if we are unlearning, we might need to clear the slot:
        if(!_event_consumer_on_mask[index]
         & !_event_consumer_off_mask[index]
         & !_event_producer_on_mask[index]
         & !_event_producer_off_mask[index]) //no one cares about this eventID anymore. Delete it from the table.
        {
         	memcpy(_evets[index].val, empty.val, 8)
        }
        
        //now, write the changes into EEPROM!
        store(index);
		return true;
    }
    else
    {
		return OLCB_Event_Handler::handleMessage(buffer);
	}
}

void store(uint8_t index)
{
	//write the new fields at index into EEPROM
	//first, write the EventID
	// which requires we generate an index into EEPROM.
	// EventID's begin at offset 4
	for(uint8_t i = 0; i < 8; ++i)
		EEPROM.write(4+(8*index)+i, _events[index].val[i]);
	//second, write the various masks
	//_event_consumer_on_mask begins at 4+(_numEvents*8)
	EEPROM.write(4+(_numEvents*8)+index), _event_consumer_on_mask);
	EEPROM.write(4+(_numEvents*9)+index), _event_consumer_off_mask);
	EEPROM.write(4+(_numEvents*10)+index), _event_producer_on_mask);
	EEPROM.write(4+(_numEvents*11)+index), _event_producer_off_mask);
}

void MyEventHandler::initialize(OLCB_Event *events, uint8_t num)
{

/****
Notes on EEPROM:
This class checks to see if the EEPROM has been programmed by checking two bytes, to see if they contain the string "Io". If not, we know that the EEPROM has not been formated, and the class proceeds by writing several things to EEPROM:
	1) the string "Io"
	2) the next available EventID, namely, NodeID.0.0, stored as two bytes
	3) A set of 8 EventIDs starting at NodeID.0.0 to NodeID.0.7
	4) EVENT_POOL_SIZE-8 EventIDs of format 0.0.0.0.0.0.0.0 (placeholders indicating nothing has been programmed for those slots)
	5) A set of flags indicating events produced for inputs on and off, with one button mapped to the each of the first 8 programmed EventID, and the remaining EVENT_POOL_SIZE-8 initialized to not produced by any input.
	6) A set of flags indicating that no events are consumed.
On the other hand, if the string "Io" is present, the EEPROM is read into memory
****/

	//first, check first two bytes:
	if( (EEPROM.read(0) != 'I') || (EEPROM.read(1) != 'o') ) //not formatted!
		factoryReset();
	
	//now, read it back out into SRAM
	uint8_t i;
	uint16_t offset;
	//real data begins at 4;
	offset = 4;
	for(i = 0; i < _numEvents*8; ++i)
	{
		for(uint8_t j = 0; j < 8; ++j)
		{
			_events[i]->val[j] = EEPROM.read(i+offset);
		}
	}
	offset += i;
	for(i = 0; i < _numEvents; ++i)
	{
		_event_consumer_on_mask[i] = EEPROM.read(i+offset);
	}
	offset += i;
	for(i = 0; i < _numEvents; ++i)
	{
		_event_consumer_off_mask[i] = EEPROM.read(i+offset);
	}
	offset += i;
	for(i = 0; i < _numEvents; ++i)
	{
		_event_producer_on_mask[i] = EEPROM.read(i+offset);
	}
	offset += i;
	for(i = 0; i < _numEvents; ++i)
	{
		_event_producer_off_mask[i] = EEPROM.read(i+offset);
	}
}

void MyEventHandler::factoryReset(void)
{
	// WARNING: THIS FUNCTION RESETS THE EVENT POOL TO FACTORY SETTINGS!
	//first, check to see if the EEPROM has been formatted yet.
	uint8_t eid6=0, eid7=0, j;
	uint16_t offset, i;
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

	offset = 4; //start writing at location 4
	
	//now, increment through the first 16 eventIDs in the pool, and set them to the new values
	for(i = 0; i < 16; ++i)
	{
		for(j = 0; j < 6; ++j)
		{
			EEPROM.write(j+offset, NID->val[j]);
		}
		EEPROM.write(6+offset, eid6);
		EEPROM.write(7+offset, eid7+i);
		offset += 8;
	}
	//and for the remaining, zero them out
	uint16_t end = 4+_numEvents*8; //we begin writing eventIDs at location 4; we finish writing them at 4+numEvents*8 since they are 8 bytes long. TODO
	for(i = offset; i < end; ++i)
	{
		EEPROM.write(i, 0); //TODO check that offset! should we be calculating this number instead?
	}
	offset = end; //move the offset up
	
	//now, set the _event_consumer_on_mask and _event_consumer_off_mask to 0; we consume nothing!
	end = offset+(_numEvents*2);
	for(i = offset; i < end; ++i)
	{
		EEPROM.write(i, 0);
	}
	offset = end;

	//Finally the _event_producer_on_mask and _event_producer_off_mask
	// This is different, because the first 8 of each will have values according to this scheme:
	///  event_0 = output_0_on
	///  event_1 = output_0_off
	///  event_2 = output_1_on
	/// etc.
	//first, _event_producer_on_mark
	for(i = offset, j = 0; i < offset+16; i+=2, ++j)
	{
		EEPROM.write(i, (1<<j));
		EEPROM.write(i+1, 0);
	}
	offset += 16;
	for(i = offset; i < offset+(_numEvents-16); ++i)
	{
		EEPROM.write(i,0);
	}
	offset += (_numEvents-16);
	
	//now, _event_producer_off_mask
	for(i = offset, j = 0; i < offset+16; i+=2, ++j)
	{
		EEPROM.write(i, 0);
		EEPROM.write(i+1, (1<<j));
	}
	offset += 16;
	for(i = offset; i < offset+(_numEvents-16); ++i)
	{
		EEPROM.write(i,0);
	}
	offset += (_numEvents-16);
	//DONE! 
	//finally, if the memory wasn't formatted before, indicate that it is now
	if(!formatted)
	{
		EEPROM.write(0, byte('I'));
		EEPROM.write(1, byte('o'));
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
	for(uint8_t i = 0; i < 8; ++i)
	{
	  //TODO check input pin here.
	  //TODO if changes have occured, mark those events that need to be produced
	}
	OLCB_Event_Handler::update(); //called last to permit the new events to be sent out immediately.      
  }
}

bool MyEventHandler::consume(OLCB_Event *event)
{
  /* We've received an event; let's see if we need to consume it */
  int index = event->findIndexInArray(_events, EVENT_POOL_SIZE);
  if(index == -1)
	  return false;
  Serial.print ("Consuming ");
  //TODO
  for(uint8_t i = 0; i < 8; ++i)
  {
  	if(_event_consumer_on_mask[index] & (1<<i))
  	{
  		digitalWrite(i+OUTPUT_OFFSET, on);
  	}
  	if(_event_consumer_off_mask[index] & (1<<i))
  	{
  		digitalWrite(i+OUTPUT_OFFSET, off);
  	}
  }
  Serial.println(index, DEC);
  buttons[index]->on(index&0x1 ? 0x0L : ~0x0L ); // turn odd events off, and even events on
  return true;
}


/////learning!
/*****
Several things have to occur.
First, the user has to put one of the inputs or outputs into "learn mode"
Once this is done, the next available open slot is identified, and tagged
	normally, we'd find a slot that is 0.0.0.0.0.0.0.0, and flag it "learn"
	but it might be that the EventID is already in the table, but we can't know that in advance, now can we? Need a way to consoldate duplicate EventIDs
We then wait for a LearnEvent message, which will be handled properly by OLCB_Event_Handler.
If there are not enough available slots, what do we do?
How do we handle erasing a learned event?
*****/
/*
Second, whether an event is to be learned is a flag that is assigned to an EventID, as per the previous C code. This works fine when the position of the event in the list tells you which producer/consumer is being taught. But that method here is a problem. How do we know which row to teach the event to, not knowing the event in advance? We might think that we just pick the first empty row (row without an EventID assigned), but this can lead to duplicate EventID entries, defeating the purpose of this design.

So, we mark the column as being taught instead.

Forgetting is as simple: We mark the column as forgetting, rather than learning.
*/
