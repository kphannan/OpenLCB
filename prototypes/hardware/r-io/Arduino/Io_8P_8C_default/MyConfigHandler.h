#ifndef __MYCONFIGHANDLER_H__
#define __MYCONFIGHANDLER_H__

#include "MyEventHandler.h"
#include "Io_8P_8C_default_XML.h"
#include "OLCB_Virtual_Node.h"
#include "OLCB_Datagram_Handler.h"


//defines from other library
#define MAC_PROTOCOL_ID 0x20

// define the operation codes, full byte

//notice that these conflict with the WIKI http://sourceforge.net/apps/trac/openlcb/wiki/BasicSpecDoc
#define MAC_CMD_WRITE                       0x00
#define MAC_CMD_READ                        0x40
#define MAC_CMD_OPERATION                   0x80
#define MAC_CMD_READ_REPLY                  0x50
#define MAC_CMD_GET_CONFIG                  0x80
#define MAC_CMD_GET_CONFIG_REPLY            0x82
#define MAC_CMD_GET_ADD_SPACE_INFO          0x84
#define MAC_CMD_GET_ADD_SPACE_INFO_REPLY    0x86
#define MAC_CMD_LOCK                        0x88
#define MAC_CMD_LOCK_REPLY                  0x8A
#define MAC_CMD_GET_UNIQUEID                0x8C
#define MAC_CMD_GET_UNIQUEID_REPLY          0x8E

#define MAC_CMD_FREEZE                      0xA0
#define MAC_CMD_INDICATE                    0xA4
#define MAC_CMD_RESETS                      0xA8


/* Class to handle Memory Configuration protocol and CDI protocol */
class MyConfigHandler : public OLCB_Virtual_Node, public OLCB_Datagram_Handler
{
	void create(OLCB_Link *link, OLCB_NodeID *nid, MyEventHandler *eventHandler);
	bool handleMessage(OLCB_Buffer *buffer);
	void update(void);
	void datagramResult(bool accepted, uint16_t errorcode);
	void initialize(void);
	bool processDatagram(void);
	
	bool MACProcessRead(void);
	bool MACProcessWrite(void);
	bool MACProcessCommand(void);

 private:
 	MyEventHandler *_eventHandler; //for configuring PC-event memory space
        uint32_t getAddress(uint8_t* data);
        uint8_t decodeLength(uint8_t* data);
        uint8_t decodeSpace(uint8_t* data);
};


#endif
