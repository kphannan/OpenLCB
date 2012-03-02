#include "MyInfoHandler.h"
#include <OLCB_CAN_Buffer.h>

//NOTE This class is largely a hack. It works, but there's a lot going on here that should be handled by the lower-level libraries!

#define MTI_SNIP_REQUEST  0x52
#define MTI_SNIP_RESPONSE 0x53
#define MTI_PIP_REQUEST   0x2E
#define MTI_PIP_RESPONSE  0x2F

prog_char string_0[] PROGMEM = "Railstars Limited";   // "String 0" etc are strings to store - change to suit.
prog_char string_1[] PROGMEM = "Railstars Io";
prog_char string_2[] PROGMEM = "1.0";
PROGMEM const char *SNIP_table[] =	   // change "string_table" name to suit
{   
  string_0,
  string_1,
  string_2 };
  
#define MAX_STRING 2

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
    //handle pending snip request responses
    if(_info_index > -1)
    {
       Serial.println("Working on SNIP response!");
       //we have work to do.
       //figure out how many bytes to send. Either 7 bytes, or whatever remains
       _reply.length = (strlen(_buffer)+1)-_string_index + 1; //(the first +1 is to INCLUDE the null; the second, because the first byte is the MTI)
       if(_reply.length > 8)
         _reply.length = 8;
       Serial.print("len = ");
       Serial.println(_reply.length, DEC);
       Serial.print("start index= ");
       Serial.println(_string_index, DEC);
       memcpy(&(_reply.data[1]), &(_buffer[_string_index]), _reply.length-1);
       for(uint8_t i = 1; i < _reply.length; ++i)
         Serial.print((char)(_reply.data[i]));
       Serial.println();
       _link->sendMessage(&_reply);
       //now, set up for next run.
       Serial.println("setting up for next time");
       _string_index += (_reply.length-1);
       Serial.print("new string index = ");
       Serial.println(_string_index, DEC);
       Serial.print("str len = ");
       Serial.println(strlen(_buffer), DEC);
       Serial.print(_string_index > strlen(_buffer));
       if(_string_index > strlen(_buffer)) //next string!
       {
          Serial.println("new string!");
          _string_index = 0;
          _info_index++;
          Serial.print("new info index = ");
          Serial.println(_info_index, DEC);
          if(_info_index > MAX_STRING)
          {
            Serial.println("done!!");
            _info_index = -1; //done!
          }
          else
          {
            strcpy_P(_buffer, (char*)pgm_read_word(&(SNIP_table[_info_index])));
            Serial.println(_buffer);
          }
        }
        else
          Serial.println("truckin' on!");
    }
    OLCB_Virtual_Node::update();
}


void MyInfoHandler::create(OLCB_Link *link, OLCB_NodeID *nid)
{
    _info_index = -1;
    _string_index = 0;
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
		OLCB_NodeID dest;
		buffer->getDestinationNID(&dest);
		if(dest == *NID)
		{
			Serial.println("Got SNIP request");
                        _info_index = 0;
                        _string_index = 0;
                        strcpy_P(_buffer, (char*)pgm_read_word(&(SNIP_table[0])));
                        Serial.println(_buffer);
                        OLCB_NodeID source_address;
                        buffer->getSourceNID(&source_address);
                        _reply.init(NID);
                        _reply.setDestinationNID(&source_address);
                        _reply.setFrameTypeOpenLcb();
                        _reply.setOpenLcbFormat(MTI_FORMAT_ADDRESSED_NON_DATAGRAM);
                        _reply.data[0] = MTI_SNIP_RESPONSE;
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
