#include "MyInfoHandler.h"
#include <OLCB_CAN_Buffer.h>

//NOTE This class is largely a hack. It works, but there's a lot going on here that should be handled by the lower-level libraries!

#define MTI_SNIP_REQUEST  0x52
#define MTI_SNIP_RESPONSE 0x53
#define MTI_PIP_REQUEST   0x2E
#define MTI_PIP_RESPONSE  0x2F

bool isSNIPRequest(OLCB_Buffer *buffer)
{
    if(! (buffer->isFrameTypeOpenLcb() && (buffer->getOpenLcbFormat() == MTI_FORMAT_ADDRESSED_NON_DATAGRAM)) )
      return false;
    return (buffer->data[0] == ((MTI_SNIP_REQUEST)&0xFF) );
}

bool isPIPRequest(OLCB_Buffer *buffer)
{
    if(! (buffer->isFrameTypeOpenLcb() && (buffer->getOpenLcbFormat() == MTI_FORMAT_ADDRESSED_NON_DATAGRAM)) )
      return false;
    return (buffer->data[0] == ((MTI_PIP_REQUEST)&0xFF) );
}

void MyInfoHandler::update(void)
{
	OLCB_Virtual_Node::update();
}


void MyInfoHandler::create(OLCB_Link *link, OLCB_NodeID *nid)
{
	OLCB_Virtual_Node::create(link,nid);
}

bool MyInfoHandler::handleMessage(OLCB_Buffer *buffer)
{
	//we care about the following kinds of messages:
	//Protocol Identification Protocol (PIP)
	//Simple Node Identification Protocol (SNIP)
	if(!isPermitted())
	{
		return false;
	}
	
	if(isSNIPRequest(buffer))
	{
		//is it for us?
		Serial.println("got SNIP aimed at...");
		OLCB_NodeID dest;
		buffer->getDestinationNID(&dest);
		Serial.println(dest.alias, DEC);
		Serial.println(NID->alias, DEC);
		if(dest == *NID)
		{
			Serial.println("Got SNIP request");
			OLCB_Buffer reply;
			OLCB_NodeID source_address;
			buffer->getSourceNID(&source_address);
			reply.init(NID);
			reply.setDestinationNID(&source_address);
			reply.setFrameTypeOpenLcb();
			reply.setOpenLcbFormat(MTI_FORMAT_ADDRESSED_NON_DATAGRAM);
			reply.data[0] = MTI_SNIP_RESPONSE;
			reply.length = 1;
			_link->sendMessage(&reply);
		}
	}
    else if(isPIPRequest(buffer))
    {
    	//is it for us?
		Serial.println("got PIP aimed at...");
		OLCB_NodeID dest;
		buffer->getDestinationNID(&dest);
		Serial.println(dest.alias, DEC);
		Serial.println(NID->alias, DEC);
		if(dest == *NID)
		{
			Serial.println("Got PIP request");
			OLCB_Buffer reply;
			OLCB_NodeID source_address;
			buffer->getSourceNID(&source_address);
			reply.init(NID);
			reply.setDestinationNID(&source_address);
			reply.setFrameTypeOpenLcb();
			reply.setOpenLcbFormat(MTI_FORMAT_ADDRESSED_NON_DATAGRAM);
			reply.data[0] = MTI_PIP_RESPONSE;
			reply.length = 7;
			reply.data[1] = 0x80 | 0x40 | 0x10 | 0x04 | 0x01;
			reply.data[2] = 0x00;
			reply.data[3] = 0x00;
			reply.data[4] = 0x00;
			reply.data[5] = 0x00;
			reply.data[6] = 0x00;
			_link->sendMessage(&reply);
		}
    }
		
	return false;
}
