#include "MyBlueGoldHandler.h"

#define ShortBlinkOn   0x00010001L
#define ShortBlinkOff  0xFFFEFFFEL

#define PRESS_DELAY  75
#define LONG_DOUBLE  3000
#define VERY_LONG_DOUBLE  8000

void MyBlueGoldHandler::create(OLCB_Link *link, OLCB_NodeID *nid, MyEventHandler *eventHandler)
{
    OLCB_Virtual_Node::create(link,nid);
    _index = -1;
    _started = false;
    _event_handler = eventHandler;
  
    // initial blue/gold setting
    blue.on(0); // turn off 
    blue.process();
    _last_blue = blue.state;
    gold.on(UNREADY_BLINK); // unready blink until intialized
    gold.process();
    _last_gold = gold.state;
    //get the inputs to monitor
    _input_buttons = _event_handler->getInputs();
    _double_press = _last_double = 0;
}

void MyBlueGoldHandler::moveToIdle(bool reset)
{
	Serial.println("moving to IDLE state");
	blue.on(0x00000000);
	gold.on(READY_BLINK);
	if(reset)
	{
		for (uint8_t i = 0; i < 32; i++)
	    {
    		_event_handler->markToTeach(i, false);
    		_event_handler->markToLearn(i, false);
    	}
    }
for(uint8_t i = 0; i < 8; ++i)
{
		digitalWrite(i, LOW);
	}
		_index = -1;
	_event_handler->disInhibit();
	_state = BG_IDLE;
}
  
void MyBlueGoldHandler::update(void)
{
	if(!isPermitted())
	{
		return;
	}


    if (!_started) {
        Serial.println("Starting ready blink");
        _started = true;
        gold.on(READY_BLINK); // turn off waiting to init flash, start heartbeat ready blink
    }
    
    //determine which buttons have been pressed since last time.

    blue.process();
    gold.process();
    for(uint8_t i = 0; i < 8; ++i)
    	_input_buttons[i].process();

	// first, check and see if we've got a double-press
        _double_state = (blue.state && gold.state);
	if(_double_state)
	{
  		_last_blue = blue.state;
		_last_gold = gold.state;
		if(blue.duration > VERY_LONG_DOUBLE)
		{
			_double_state = 3;
		}
		else if(blue.duration > LONG_DOUBLE)
		{
			_double_state = 2;
		}
	}

	if(_last_double != _double_state) //check if state has changed, 
	{
		Serial.print("Double press! ");
		Serial.println(_double_state, DEC);
		_last_double = _double_state;
		_double_press = _double_state;
if(_double_state == 0); //reset the LEDs in case we are backing down from a flash reset
{
    blue.on(0x00);
    gold.on(READY_BLINK);
}
	}
	else
	{
		_double_press = 0;
	}

    // check if blue button pressed
    if (_last_blue != blue.state) //see if change
    {
    	if(blue.duration > PRESS_DELAY) //give it 75 ms before we count it
    	{
	        _last_blue = blue.state;
			if(blue.state)
			{
				Serial.println("Blue pressed!");
				_blue_pressed = true;
			}
		}
    }
    else //if no change in input, ignore it
    {
    	_blue_pressed = false;
    }
    
    // check if gold button pressed
    if (_last_gold != gold.state) //see if change
    {
    	if(gold.duration > PRESS_DELAY) //give it 75 ms before we count it
    	{
	        _last_gold = gold.state;
			if(gold.state)
			{
				Serial.println("Gold pressed!");
				_gold_pressed = true;
			}
		}
    }
    else //if no change in input, ignore it
    {
    	_gold_pressed = false;
    }
    
    //check if input buttons were pressed
    //TODO
    for(uint8_t i = 0; i < 8; ++i) //check each button!
    {
    	if( ((_last_input & (1<<i))>>i) != _input_buttons[i].state) //see if change
    	{
    		if(_input_buttons[i].state)
    		{
    			Serial.print("Input ");
    			Serial.print(i, DEC);
    			Serial.println(" pressed!");
    			_last_input |= (1<<i);
    			_input_pressed |= (1<<i);
    		}
    		else
    		{
    			_last_input &= ~(1<<i);
    		}
    	}
    	else //no change in input, ignore it
    	{
    		_input_pressed &= ~(1<<i);
    	}
    }
    

	//possibilities: Blue and Gold was pressed, only Blue was pressed or only Gold was pressed, and/or an input button was pressed.
	uint8_t channel, prev_channel;
	//blue has been pressed.
//	Serial.print("Current state is ");
//	Serial.println(_state, HEX);
	switch(_state)
	{
		case BG_IDLE:
			if(_double_press == 3) //8 seconds each
			{
				Serial.println("FACTORY RESET!!!!!");
                                factoryReset();
			}
			else if(_double_press == 2) //3 seconds, begin factory reset warning
			{
				Serial.println("Prepare for factory reset");
				gold.on(0xAAAAAAAA);
				blue.on(0x55555555);
			}
		    else if(_double_press == 1)
		    {
		    	Serial.println("Send Ident");
		    	sendIdent();
		    }
		    else if(_blue_pressed)
		    {
		    	//we were doing nothing before; a press of blue puts us in LEARN mode
		    	Serial.println("Moving to LEARN");
				_state = BG_LEARN;
				//inhibit the EventHandler to avoid confusions
				_event_handler->inhibit();
				blue.on(0xF0F0F0F0); //light the BLUE lamp solid for learning
				
		    }
		    else if(_gold_pressed)
		    {
		    	//we were doing nothing before; a press of gold puts us in TEACH mode
		    	Serial.println("Moving to TEACH");
				_state = BG_TEACH;
				//inhibit the EventHandler to avoid confusions
				_event_handler->inhibit(); //TODO is this right?
				gold.on(0xF0F0F0F0); //light the GOLD lamp solid for learning
				
		    }
		    break;
		case BG_LEARN:
			if(_double_press)
		    {
		    	Serial.println("LEARN canceled!");
		    	moveToIdle(true);
			}
			else if(_blue_pressed)
			{
				//we've entered learn state, now we're indexing over the outputs
				_index = ((_index+2)%17)-1;
				if(_index == -1) //cycled through, return to beginning.
				{
					Serial.println("Moving to IDLE");
					digitalWrite(7, LOW); //turn off last channel.
					blue.on(0x00000000);
					_state = BG_IDLE;
					_event_handler->disInhibit();
				}
				else
				{
					channel = _index >> 1;
					Serial.print("Selecting index ");
					Serial.print(_index, DEC);
					Serial.print(" on channel ");
					Serial.println(channel, DEC);
					// if _index is even, we are handling the producer for the output being off; blink the blue LED to indicate
					if(_index & 0x01)
					{
						blue.on(0x000A000A);
					}
					else
					{
						blue.on(0xFFFFFFFF);
					}
					digitalWrite(channel, HIGH);
					if(_index>1)
					{
						Serial.print("...and turning off channel ");
						Serial.println(channel-1,DEC);
						digitalWrite(channel-1, LOW); //turn off previous channel
					}
				}
			}
			else if(_gold_pressed)
			{
				//send off the LEARN messages!
				Serial.println("Sending LEARN messages!");
				moveToIdle(false);
			}
			else if(_input_pressed)
			{
				//figure out which input was pressed
				uint8_t in;
				for(in = 0; in < 8; ++in)
				{
					if((_input_pressed) & (1<<in))
						break;
				}
				//check to see if this is first, second, or third time.
				//First time, we flag "on" producer for that channel
				//Second time, we unflag "on", flag "off"
				//Third time, we unflag "off"
				//check "on"
				if(_event_handler->markedToLearn(in<<1)) //this is the third press
				{
					_event_handler->markToLearn(in<<1, false);
					blue.on(0xF0F0F0F0); //indicate that nothing is selected for learning
				}
				else if(_event_handler->markedToLearn(in<<0)) //this is the second press
				{
					_event_handler->markToLearn(in<<0, false);
					_event_handler->markToLearn(in<<1, true);
					blue.on(0x000A000A); //indicate that "off" is selected
				}
				else
				{
					_event_handler->markToLearn(in<<0, true);
					blue.on(0xFFFFFFFF); //indicate that "on" is selected
				}
			}
			break;
		case BG_TEACH: //we've entered teach state, now we're indexing over the outputs
			if(_double_press)
			{
		    	Serial.println("TEACH canceled!");
		    	moveToIdle(true);
            }
			else if(_blue_pressed)
			{
					_index = ((_index+2)%17)-1;
					if(_index == -1) //cycled through, return to beginning.
					{
						digitalWrite(7, LOW); //turn off last channel.
						blue.on(0xF0F0F0F0);
						//the difference is that we don't leave BG_TEACH state when we've wrapped around the outputs
					}
					else
					{
						channel = _index >> 1;
						// if _index is even, we are handling the producer for the output being off; blink the blue LED to indicate
						if(_index & 0x01)
						{
							blue.on(0xA000A000);
						}
						else
						{
							blue.on(0xFFFFFFFF);
						}
						digitalWrite(channel, HIGH);
						if(_index>1)
						{
							digitalWrite(channel-1, LOW); //turn off previous channel
						}
					}
        	}
        	else if(_gold_pressed)
        	{
                Serial.println("Sending Teach Messages");
                moveToIdle(true);
        	}
        	//else if input pressed
        	//break;
        }
}

bool MyBlueGoldHandler::handleMessage(OLCB_Buffer *buffer)
{
  if(isPermitted());
    return OLCB_Virtual_Node::handleMessage(buffer);

  return false;
}

/**
 * Send an event in response to the "ident" button pushes
 */
void MyBlueGoldHandler::sendIdent()
{
	_link->sendIdent();
}

/**
 * Fire factory reset
 * ToDo: better name!  Not really a true "factory reset"
 */
void MyBlueGoldHandler::factoryReset()
{
//	TODO REINSTATE AFTER TESTING BG INTERFACE!!!!!!_event_handler->factoryReset();
	delay(100); //just because. Don't know if we need it
	// reboot
    // cast a 0 to a function pointer, then dereference it. Ugly!
    (*  ((void (*)())0)  )();
}
