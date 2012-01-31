#include "MyBlueGoldHandler.h"

#define ShortBlinkOn   0x00010001L
#define ShortBlinkOff  0xFFFEFFFEL

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
}

void MyBlueGoldHandler::moveToIdle(bool reset)
{
	Serial.println("moving to IDLE state");
	blue.on(0x00000000);
	gold.on(READY_BLINK);
	if(reset)
	{
		for (int i = 0; i < 32; i++)
	    {
    		_event_handler->markToTeach(i, false);
    		_event_handler->markToLearn(i, false);
    	}
    }
	if(_index > -1)
    {
		digitalWrite(_index, LOW);
		_index = -1;
	}
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


    // check if blue button pressed
    if (_last_blue != blue.state) //see if change
    {
        _last_blue = blue.state;
        if(blue.state)
        {
        	Serial.println("Blue pressed!");
        	_blue_pressed = true;
        }
    }
    else //if no change in input, ignore it
    {
    	_blue_pressed = false;
    	if(blue.state) //if button is depressed
 	    {
    		if(blue.duration > 5000)
   		 		_blue_long_pressed = 2;
    		if(blue.duration > 3000)
    			_blue_long_pressed = 1;
    		else
    			_blue_long_pressed = 0;
 	   }
    }
    
    // check if gold button pressed
    if (_last_gold != gold.state) //see if change
    {
        _last_gold = gold.state;
        if(gold.state)
        {
        	Serial.println("Gold pressed!");
        	_gold_pressed = true;
        }
    }
    else //if no change in input, ignore it
    {
    	_gold_pressed = false;
    	if(gold.state) //if button is depressed
 	    {
    		if(gold.duration > 5000)
   		 		_gold_long_pressed = 2;
    		if(blue.duration > 3000)
    			_gold_long_pressed = 1;
    		else
    			_gold_long_pressed = 0;
 	   }
    }
    
    //check if input buttons were pressed
    //TODO
    

	//possibilities: Blue and Gold was pressed, only Blue was pressed or only Gold was pressed, and/or an input button was pressed.
	uint8_t channel, prev_channel;
	//blue has been pressed.
	Serial.print("Current state is ");
	Serial.println(_state, HEX);
	switch(_state)
	{
		case BG_IDLE:
			if(_blue_long_pressed == _gold_long_pressed == 2) //5 seconds each
			{
				Serial.println("FACTORY RESET!!!!!");
				//TODO factory reset
			}
		    if(_blue_pressed && _gold_pressed)
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
				blue.on(0xFFFFFFFF); //light the BLUE lamp solid for learning
				break;
		    }
		    else if(_gold_pressed)
		    {
		    	//we were doing nothing before; a press of gold puts us in TEACH mode
		    	Serial.println("Moving to TEACH");
				_state = BG_TEACH;
				//inhibit the EventHandler to avoid confusions
				_event_handler->inhibit(); //TODO is this right?
				gold.on(0xFFFFFFFF); //light the GOLD lamp solid for learning
				break;
		    }
		    //else if input pressed 		
		case BG_LEARN:
			if(_blue_long_pressed && _gold_long_pressed)
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
					break;
			}
			else if(_gold_pressed)
			{
				//send off the LEARN messages!
				Serial.println("Sending LEARN messages!");
				moveToIdle(false);
			}
			//else if input pressed
		case BG_TEACH: //we've entered teach state, now we're indexing over the outputs
			if(_blue_long_pressed & _gold_long_pressed)
			{
		    	Serial.println("TEACH canceled!");
		    	moveToIdle(true);
            }
			else if(_blue_pressed)
			{
					_index = ((_index+2)%17)-1;
					if(_index == -1) //cycled through, return to beginning.
					{
						digitalWrite(15, LOW); //turn off last channel.
						blue.on(0x00000000);
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
					break;
        	}
        	else if(_gold_pressed)
        	{
        		//send off the TEACH messages!
        	}
        	//else if input pressed
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
	_event_handler->factoryReset();
	delay(100); //just because. Don't know if we need it
	// reboot
    // cast a 0 to a function pointer, then dereference it. Ugly!
    (*  ((void (*)())0)  )();
}
