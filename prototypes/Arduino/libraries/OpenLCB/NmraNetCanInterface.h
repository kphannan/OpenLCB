#ifndef NmraNetCanInterface_h
#define NmraNetCanInterface_h

/**
 * Provide generic CAN interface for NMRAnet code
 *
 * Heavily modelled on CAN.h, but intended to abstract
 * CAN access so that other hardware and software solutions
 * can be provided in the future.
 *
 */

class NmraNetCanBuffer;

/**
 * Specific definitions
 *
 * This section attaches the general interface (see below)
 * to the specific CAN.h implementation
 * 
 * This version is for the CAN.h library from 
 * Fabian Greif, Roboterclub Aachen e.V., adapted
 * and extended by Alex Shepherd.
 *
 * Because of the way the Arduino IDE creates the -I include
 * search list, you need to #include a file from each
 * needed library in the top level file.  In the case of
 * this implementation, this is the CAN.h file in the CAN library.
 */
#include <CAN.h>

/**
 * General interface
 *
 * This section defines the general interface used by
 * the NMRAnet sample code
 */

// Buffer structures
class CanInterfaceBuffer : public tCAN {};

// Initialize CAN system
void NMRAnet_can_init();

// Can a (the) CAN buffer be used?  
// Generally, indicates the buffer can be immediately
// queued for transmit, so it make sense to prepare it now
boolean NMRAnet_can_xmt_ready(NmraNetCanBuffer* b);

// Queue a CAN frame for sending, if possible
// Returns true if queued, false if not currently possible
boolean NMRAnet_can_queue_xmt_immediate(NmraNetCanBuffer* b);

// Queue a CAN frame for sending; waits until it can queue
void NMRAnet_can_queue_xmt_wait(NmraNetCanBuffer* b);

// Send a CAN frame, waiting until it has been sent
void NMRAnet_can_send_xmt(NmraNetCanBuffer* b);

// Check whether all frames have been sent,
// a proxy for the link having gone idle
boolean NMRAnet_can_xmt_idle();

// Make the oldest received CAN frame available,
// in the process removing it from the CAN subsystem.
// Return false (zero) if no frame available.
boolean NMRAnet_can_get_frame(NmraNetCanBuffer* b);


#endif
