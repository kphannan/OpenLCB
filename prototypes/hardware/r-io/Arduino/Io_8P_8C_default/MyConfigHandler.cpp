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
            Serial.println("got a datagram!");
            Serial.println(_rxDatagramBuffer->data[0], HEX);
            //check the first byte of the payload to see what kind of datagram we have
            switch(_rxDatagramBuffer->data[0])
            {
              case MAC_PROTOCOL_ID: //MAC protocol
                Serial.println("using MAC protocol");
                switch (_rxDatagramBuffer->data[1]&0xC0)
                {
                  case MAC_CMD_READ:
                    Serial.println("read request");
  		    return MACProcessRead();
                    break;
                  case MAC_CMD_WRITE:
                    Serial.println("write request");
                    return MACProcessWrite();
                    break;
                  case MAC_CMD_OPERATION:
                    Serial.println("cmd request");
                    return MACProcessCommand();
                    break;
		}
	    }
	}
        Serial.println("Not for us to handle, going to NAK");
	return false;
}

//Memory Access Configuration Protocol
bool MyConfigHandler::MACProcessRead(void)
{
	OLCB_Datagram reply;
        memcpy(&(reply.destination), &(_rxDatagramBuffer->source), sizeof(OLCB_NodeID));
	//copy first six bytes. TODO why?
	memcpy(reply.data, _rxDatagramBuffer->data, 6);
	reply.data[1] = MAC_CMD_READ_REPLY | reply.data[1]&0x0F; //WTF?
        Serial.print("Making reply with MTI ");
        Serial.println(reply.data[0], HEX);
        Serial.println(reply.data[1], HEX);
	//TODO presume datagram?
	uint8_t length = decodeLength(_rxDatagramBuffer->data);
	uint32_t address = getAddress(_rxDatagramBuffer->data);
	uint8_t space = decodeSpace(_rxDatagramBuffer->data);
	//And, now do something useful.
	//first check the space?
	switch(space)
	{
		case 0xFF: //CDI request.
			Serial.println("CDI request. ignoring");
			break;
		case 0xFE: //"All memory" access. Just give them what they want?
			Serial.println("all memory request. ignoring");
			break;
		case 0xFD: //configuration space
                        Serial.println("configuration space...");
			reply.length = _eventHandler->readConfig(address, length, &(reply.data[6])) + 6;
                        Serial.print("total length of reply = ");
                        Serial.println(reply.length, DEC);
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
    OLCB_Datagram reply;
    reply.data[0] = MAC_PROTOCOL_ID;
    memcpy(&(reply.destination), &(_rxDatagramBuffer->source), sizeof(OLCB_NodeID));
    switch (_rxDatagramBuffer->data[1]&0xFC)
    {
        case MAC_CMD_GET_CONFIG_OPTIONS:
                Serial.println("MAC_CMD_GET_CONFIG_OPTIONS");
                reply.length = 6;
                reply.data[1] = MAC_CMD_GET_CONFIG_OPTIONS_REPLY;
                reply.data[2] = (MAC_CONFIG_OPTIONS_1_BYTE_WRITE | MAC_CONFIG_OPTIONS_2_BYTE_WRITE | MAC_CONFIG_OPTIONS_4_BYTE_WRITE | MAC_CONFIG_OPTIONS_64_BYTE_WRITE) >> 8;
                reply.data[3] = (MAC_CONFIG_OPTIONS_1_BYTE_WRITE | MAC_CONFIG_OPTIONS_2_BYTE_WRITE | MAC_CONFIG_OPTIONS_4_BYTE_WRITE | MAC_CONFIG_OPTIONS_64_BYTE_WRITE) & 0xFF;
                reply.data[4] = 0xFF; //highest address space
                reply.data[5] = 0xFD; //lowest address space
                sendDatagram(&reply);
             	break;
        case MAC_CMD_GET_ADD_SPACE_INFO:
                Serial.println("MAC_CMD_GET_ADD_SPACE_INFO");
//                uint8_t space = _rxDatagramBuffer->data[0];
//                reply.length = TODO
//                reply.data[1] = MAC_CMD_GET_ADD_SPACE_INFO_REPLY;
//                if(space >= 0xFE)
//                  reply.data[2] = 
//                uint16_t write_opts = MAC_CONFIG_OPTIONS_1_BYTE_WRITE | MAC_CONFIG_OPTIONS_2_BYTE_WRITE | MAC_CONFIG_OPTIONS_4_BYTE_WRITE | MAC_CONFIG_OPTIONS_64_BYTE_WRITE;
//                reply.data[2] = write_opts >> 8;
//                reply.data[3] = write_opts & 0xFF;
//                reply.data[4] = 0xFF; //highest address space
//                reply.data[5] = 0xFD; //lowest address space
        	break;
        case MAC_CMD_RESETS:
            Serial.println("MAC_CMD_RESETS");
            // force restart (may not reply?)
            if ((_rxDatagramBuffer->data[1]&0x03) == 0x01)
            { // restart/reboot?
              //TODO tell other handlers that we need to write out anything that needs to be saved!!!
            	void (*restart)() = 0x00;
                (*restart)();
            }
            // TODO: Handle other cases
            break;
        case MAC_CMD_GET_CONFIG_OPTIONS_REPLY :
            Serial.println("MAC_CMD_GET_CONFIG_REPLY");
            break;
        case MAC_CMD_GET_ADD_SPACE_INFO_REPLY:
            Serial.println("MAC_CMD_GET_ADD_SPACE_INFO_REPLY");
            break;
        case MAC_CMD_LOCK:
            Serial.println("MAC_CMD_LOCK");
            break;
        case MAC_CMD_LOCK_REPLY:
            Serial.println("MAC_CMD_LOCK_REPLY");
            break;
        case MAC_CMD_GET_UNIQUEID:
            Serial.println("MAC_CMD_GET_UNIQUEID");
            break;
        case MAC_CMD_GET_UNIQUEID_REPLY:
            Serial.println("MAC_CMD_GET_UNIQUEID_REPLY");
            break;
        case MAC_CMD_FREEZE:
            Serial.println("MAC_CMD_FREEZE");
            break;
        case MAC_CMD_INDICATE:
            Serial.println("MAC_CMD_INDICATE");
            break;
        default:
            break;
    }
}
