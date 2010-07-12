//------------------------------------------------------------------------
//
// OpenDCC - NmraDcc.cpp 
//
// Copyright (c) 2008 Alex Shepherd
//
// This source file is subject of the GNU general public license 2,
// that is available at the world-wide-web at
// http://www.gnu.org/licenses/gpl.txt
// 
//------------------------------------------------------------------------
//
// file:      NmraDccTest.cpp
// author:    Alex Shepherd
// webpage:   http://opendcc.org/
// history:   2008-03-20 Initial Version
//
//------------------------------------------------------------------------
//
// purpose:   Provide a simplified interface to decode NMRA DCC packets
//
//------------------------------------------------------------------------

#ifndef Wiring_h
#include <WProgram.h>
#endif

#ifndef NMRADCC_IS_IN
#define NMRADCC_IS_IN

#define MAX_DCC_MESSAGE_LEN 6    // including XOR-Byte

typedef struct
{
	uint8_t	Size ;
	uint8_t	PreambleBits ;
	uint8_t Data[MAX_DCC_MESSAGE_LEN] ;
} DCC_MSG ;

//--------------------------------------------------------------------------
//  This section contains the NMRA Assigned DCC Manufacturer Id Codes that
//  are used in projects
//
//  This value is to be used for CV8 
//--------------------------------------------------------------------------

#define MAN_ID_JMRI             0x12
#define MAN_ID_DIY              0x0D
#define MAN_ID_SILICON_RAILWAY  0x21

//--------------------------------------------------------------------------
//  This section contains the Product/Version Id Codes for projects
//
//  This value is to be used for CV7 
//
//  NOTE: Each Product/Version Id Code needs to be UNIQUE for that particular
//  the DCC Manufacturer Id Code
//--------------------------------------------------------------------------

// Product/Version Id Codes allocated under: MAN_ID_JMRI
 
// Product/Version Id Codes allocated under: MAN_ID_DIY

#define DCCuinoDemo  0x02

#define FLAGS_MY_ADDRESS_ONLY  	      0x01
#define FLAGS_OUTPUT_ADDRESS_MODE     0x40  // CV 29/541 bit 6
#define FLAGS_DCC_ACCESSORY_DECODER  	0x80  // CV 29/541 bit 7

// Standard CV Addresses
#define CV_ACCESSORY_DECODER_ADDRESS_LSB       1
#define CV_ACCESSORY_DECODER_ADDRESS_MSB       9

#define CV_MULTIFUNCTION_PRIMARY_ADDRESS       1
#define CV_MULTIFUNCTION_EXTENDED_ADDRESS_MSB 17
#define CV_MULTIFUNCTION_EXTENDED_ADDRESS_LSB 18

#define CV_VERSION_ID                          7
#define CV_MANUFACTURER_ID                     8
#define CV_29_CONFIG                          29
#define CV_OPS_MODE_ADDRESS_LSB               33
#define MAXCV                                 E2END     // the upper limit of the CV value currently defined to max memory.

#define USE_DCC_CV_CACHE 

class NmraDcc
{
  private:
    DCC_MSG Msg ;
    
  public:
    NmraDcc();
    void init( uint8_t ManufacturerId, uint8_t VersionId, uint8_t Flags, uint8_t OpsModeAddressBaseCV );
    uint8_t process();
    uint8_t getCV( uint16_t CV );
    uint8_t setCV( uint16_t CV, uint8_t Value);
};

extern void notifyDccReset(uint8_t hardReset ) __attribute__ ((weak));
extern void notifyDccIdle(void) __attribute__ ((weak));

extern void notifyDccSpeed( uint16_t Addr, uint8_t Speed, uint8_t ForwardDir, uint8_t MaxSpeed ) __attribute__ ((weak));
extern void notifyDccFunc( uint16_t Addr, uint8_t FuncNum, uint8_t FuncState) __attribute__ ((weak));

extern void notifyDccAccState( uint16_t Addr, uint16_t BoardAddr, uint8_t OutputAddr, uint8_t State ) __attribute__ ((weak));

extern void notifyDccSigState( uint16_t Addr, uint8_t OutputIndex, uint8_t State) __attribute__ ((weak));

extern uint8_t notifyCVValid( uint16_t CV, uint8_t Writable ) __attribute__ ((weak));
extern uint8_t notifyCVRead( uint16_t CV) __attribute__ ((weak));
extern uint8_t notifyCVWrite( uint16_t CV, uint8_t Value) __attribute__ ((weak));
extern void    notifyCVChange( uint16_t CV, uint8_t Value) __attribute__ ((weak));

extern void    notifyCVAck(void) __attribute__ ((weak));

#endif
