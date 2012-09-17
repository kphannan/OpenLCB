// #ifdef DO_OLCB_DCC_TRAIN

#ifndef __OLCB_DCC_TRAIN_H__
#define __OLCB_DCC_TRAIN_H__

#include "OLCB_Virtual_Node.h"
#include "OLCB_Datagram_Handler.h"
#include "OLCB_Datagram.h"
#include "DCCPacketScheduler.h"

#define DATAGRAM_MOTIVE             	    0x30
#define DATAGRAM_MOTIVE_SETSPEED			0x01
#define DATAGRAM_MOTIVE_GETSPEED			0x03
#define DATAGRAM_MOTIVE_REPORTSPEED			0x02

#define DATAGRAM_MOTIVE_SETFX				0x11
#define DATAGRAM_MOTIVE_GETFX				0x13
#define DATAGRAM_MOTIVE_REPORTFX			0x12

#define DATAGRAM_MOTIVE_ATTACH      	    0x21
#define DATAGRAM_MOTIVE_ATTACHED			0x22
#define DATAGRAM_MOTIVE_ATTACH_DENIED		0x33
#define DATAGRAM_MOTIVE_RELEASE				0x24
#define DATAGRAM_MOTIVE_RELEASED			0x25

#define SPEED_STEPS_14	14
#define SPEED_STEPS_28	28
#define SPEED_STEPS_128	128


#define NUM_SIMULTANEOUS_CONTROLLERS 2

/***
Super states:
	inhibited/permitted
	
States in permitted:
	TODO
***/

class OLCB_DCC_Train
{
  public:
  	void DCC_Train_create(DCCPacketScheduler *controller);
	void DCC_Train_initialize();
	void DCC_Train_update(void);
	uint16_t DCC_Train_processDatagram(OLCB_Datagram *datagram);
//   	void DCC_Train_datagramResult(bool accepted, uint16_t errorcode);

	bool DCC_Train_isAttached(OLCB_NodeID *node) {return true;} //no security;
   	
  private:
	  uint8_t DCC_Train_DCCSpeedToNotch(uint8_t dccspeed);
	  uint8_t DCC_Train_metersPerSecondToDCCSpeed(float mps);

	  //TODO need to do these!
	  uint16_t handleAttachDatagram(OLCB_Datagram *datagram) {return DATAGRAM_REJECTED_DATAGRAM_TYPE_NOT_ACCEPTED;}
	  uint16_t handleReleaseDatagram(OLCB_Datagram *datagram) {return DATAGRAM_REJECTED_DATAGRAM_TYPE_NOT_ACCEPTED;}
	  uint16_t handleSetSpeedDatagram(OLCB_Datagram *datagram);
	  uint16_t handleGetSpeedDatagram(OLCB_Datagram *datagram) {return DATAGRAM_REJECTED_DATAGRAM_TYPE_NOT_ACCEPTED;}
	  uint16_t handleSetFXDatagram(OLCB_Datagram *datagram) {return DATAGRAM_REJECTED_DATAGRAM_TYPE_NOT_ACCEPTED;}
	  uint16_t handleGetFXDatagram(OLCB_Datagram *datagram) {return DATAGRAM_REJECTED_DATAGRAM_TYPE_NOT_ACCEPTED;}
  
  
  //helpers
    uint32_t DCC_Train_timer;
    DCCPacketScheduler *DCC_Controller;
//    OLCB_Datagram *DCC_Train_txDatagramBuffer;
//    OLCB_Datagram *DCC_Train_rxDatagramBuffer;
//    OLCB_Link *DCC_Train_link;

  //configuration
    uint8_t DCC_Train_speed_steps;
    uint8_t DCC_Train_dcc_address;
    uint8_t DCC_Train_speed_curve[128];

  //state infor
  	int8_t DCC_Train_speed; //in speedsteps; signed for direction
    uint32_t DCC_Train_FX; //bitfield of 32 FX
    OLCB_NodeID *DCC_Train_controllers[NUM_SIMULTANEOUS_CONTROLLERS];
};


#endif
// #endif //DO_OLCB_DCC_TRAIN
