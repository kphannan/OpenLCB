#ifndef OpenLcbCan_h
#define OpenLcbCan_h

/**
 * Basic definitions for OpenLCB on CAN
 */
 
#define MTI_INITIALIZATION_COMPLETE 0x33F

#define MTI_PCER                    0x34F

#define MTI_VERIFY_NID              0x35F
#define MTI_VERIFIED_NID            0x36F

#define MTI_IDENTIFY_CONSUMERS      0x37F
#define MTI_CONSUMER_IDENTIFIED     0x38F

#define MTI_IDENTIFY_PRODUCERS      0x39F
#define MTI_PRODUCER_IDENTIFIED     0x3AF

#define MTI_IDENTIFY_EVENTS         0x3BF

#define MTI_CONSUMER_IDENTIFY_RANGE 0x3EF
#define MTI_PRODUCER_IDENTIFY_RANGE 0x3FF

#endif
