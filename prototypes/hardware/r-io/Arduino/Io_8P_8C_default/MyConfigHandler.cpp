#include "MyConfigHandler.h"

void MyConfigHandler::create(OLCB_Link *link, OLCB_NodeID *nid, MyEventHandler *eventHandler)
{
	_eventHandler = eventHandler;
	OLCB_Datagram_Handler::create(link,nid);
	OLCB_Virtual_Node::create(link,nid);
}

bool MyConfigHandler::handleMessage(OLCB_Buffer *buffer)
{
  if(isPermitted())
	return OLCB_Datagram_Handler::handleMessage(buffer);
  return false;
}

void MyConfigHandler::update(void)
{
	if(isPermitted())
	{
		OLCB_Datagram_Handler::update();
	}
}

void MyConfigHandler::datagramResult(bool accepted, uint16_t errorcode)
{
	//Serial.print("The datagram was ");
	if(!accepted)
		//Serial.print("not ");
	//Serial.println("accepted.");
	if(!accepted)
	{
		//Serial.print("   The reason: ");
		//Serial.println(errorcode,HEX);
	}
}

void MyConfigHandler::initialize(void)
{
	return;
}

uint32_t MyConfigHandler::getAddress(uint8_t* data)
{
    uint32_t val = 0;
    val |= ((uint32_t)data[2]<<24);
    val |= ((uint32_t)data[3]<<16);
    val |= ((uint32_t)data[4]<<8);
    val |= ((uint32_t)data[5]);
    return val;
}

// -1 means stream
uint8_t MyConfigHandler::decodeLength(uint8_t* data) {
    // ToDo:  Add stream
    return data[6];
}

uint8_t MyConfigHandler::decodeSpace(uint8_t* data) {
    int val;
    switch (data[1]&0x03) {
        case 0x03:
            val = 0xFF;
            break;
        case 0x01:
            val = 0xFE;
            break;
        case 0x02:
            val = 0xFD;
            break;
        case 0x00:
            val = data[6];
            break;
    }
    return val;
}

bool MyConfigHandler::processDatagram(void)
{
	//To have made it this far, we can be sure that _rxDatagramBuffer has a valid datagram loaded up, and that it is in fact addressed to us.

	if(isPermitted()) //only act on it if we are in Permitted state. Otherwise no point.
	{
            //Serial.println("got a datagram!");
		//check the first byte of the payload to see what kind of datagram we have
		switch(_rxDatagramBuffer->data[0])
		{
			case MAC_PROTOCOL_ID: //MAC protocol
                                //Serial.println("using MAC protocol");
				switch (_rxDatagramBuffer->data[1]&0xC0)
				{
			        case MAC_CMD_READ:
                                //Serial.println("read request");
            			return MACProcessRead();
			            break;
        			case MAC_CMD_WRITE:
        //Serial.println("write request");
            			return MACProcessWrite();
            			break;
        			case MAC_CMD_OPERATION:
        //Serial.println("cmd request");
            			return MACProcessCommand();
            		break;
    			}
    			return false;
		}
		return false;
	}
	return false;
}

//Memory Access Configuration Protocol
bool MyConfigHandler::MACProcessRead(void)
{
	OLCB_Datagram reply;
	
	//copy first six bytes. TODO why?
	memcpy(reply.data, _rxDatagramBuffer->data, 6);
	reply.data[0] = MAC_CMD_READ_REPLY | reply.data[0]&0x0F; //WTF?
	//TODO presume datagram?
	uint8_t length = decodeLength(_rxDatagramBuffer->data);
	uint32_t address = getAddress(_rxDatagramBuffer->data);
	uint8_t space = decodeSpace(_rxDatagramBuffer->data);
	//And, now do something useful.
	//first check the space?
	switch(space)
	{
		case 0xFF: //CDI request.
			//Serial.println("CDI request. ignoring");
			break;
		case 0xFE: //"All memory" access. Just give them what they want?
			//Serial.println("all memory request. ignoring");
			break;
		case 0xFD: //configuration space
                        //Serial.println("configuration space...");
			reply.length = _eventHandler->readConfig(address, length, &(reply.data[6])) + 6;
			sendDatagram(&reply);
			return true;
	}
//Serial.println("NAKing");
	return false; //send a NAK. Is this what we really want?
}

bool MyConfigHandler::MACProcessWrite(void)
{
	uint8_t length = decodeLength(_rxDatagramBuffer->data);
	uint32_t address = getAddress(_rxDatagramBuffer->data);
	uint8_t space = decodeSpace(_rxDatagramBuffer->data);
	//And, now do something useful.
	//first check the space?
	switch(space)
	{
		case 0xFF: //CDI request.
			//TODO
			break;
		case 0xFE: //"All memory" access. Just give them what they want?
			//TODO
			break;
		case 0xFD: //configuration space
			_eventHandler->writeConfig(address, length, &(_rxDatagramBuffer->data[6]));
			return true;
	}
	return false; //send a NAK. Is this what we really want?
}

bool MyConfigHandler::MACProcessCommand(void)
{       
    switch (_rxDatagramBuffer->data[1]&0xFC)
    {
        case MAC_CMD_GET_CONFIG:
        	break;
        case MAC_CMD_GET_ADD_SPACE_INFO:
        	break;
        case MAC_CMD_RESETS:
            // force restart (may not reply?)
            if ((_rxDatagramBuffer->data[1]&0x03) == 0x01)
            { // restart/reboot?
              //TODO tell other handlers that we need to write out anything that needs to be saved!!!
            	void (*restart)() = 0x00;
                (*restart)();
            }
            // TODO: Handle other cases
            break;
        case MAC_CMD_GET_CONFIG_REPLY :
        case MAC_CMD_GET_ADD_SPACE_INFO_REPLY:
        case MAC_CMD_LOCK:
        case MAC_CMD_LOCK_REPLY:
        case MAC_CMD_GET_UNIQUEID:
        case MAC_CMD_GET_UNIQUEID_REPLY:
        case MAC_CMD_FREEZE:
        case MAC_CMD_INDICATE:
        default:
            break;
    }
}
