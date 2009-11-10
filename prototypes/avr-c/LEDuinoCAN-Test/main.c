// coding: utf-8

#include <avr/io.h>
#include <avr/pgmspace.h>
#include <avr/interrupt.h>

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

#include <util/delay.h>

#include "can.h"

// -----------------------------------------------------------------------------
/** Set filters and masks.
 *
 * The filters are divided in two groups:
 *
 * Group 0: Filter 0 and 1 with corresponding mask 0.
 * Group 1: Filter 2, 3, 4 and 5 with corresponding mask 1.
 *
 * If a group mask is set to 0, the group will receive all messages.
 *
 * If you want to receive ONLY 11 bit identifiers, set your filters
 * and masks as follows:
 *
 *	prog_uint8_t can_filter[] = {
 *		// Group 0
 *		MCP2515_FILTER(0),				// Filter 0
 *		MCP2515_FILTER(0),				// Filter 1
 *		
 *		// Group 1
 *		MCP2515_FILTER(0),				// Filter 2
 *		MCP2515_FILTER(0),				// Filter 3
 *		MCP2515_FILTER(0),				// Filter 4
 *		MCP2515_FILTER(0),				// Filter 5
 *		
 *		MCP2515_FILTER(0),				// Mask 0 (for group 0)
 *		MCP2515_FILTER(0),				// Mask 1 (for group 1)
 *	};
 *
 *
 * If you want to receive ONLY 29 bit identifiers, set your filters
 * and masks as follows:
 *
 * \code
 *	prog_uint8_t can_filter[] = {
 *		// Group 0
 *		MCP2515_FILTER_EXTENDED(0),		// Filter 0
 *		MCP2515_FILTER_EXTENDED(0),		// Filter 1
 *		
 *		// Group 1
 *		MCP2515_FILTER_EXTENDED(0),		// Filter 2
 *		MCP2515_FILTER_EXTENDED(0),		// Filter 3
 *		MCP2515_FILTER_EXTENDED(0),		// Filter 4
 *		MCP2515_FILTER_EXTENDED(0),		// Filter 5
 *		
 *		MCP2515_FILTER_EXTENDED(0),		// Mask 0 (for group 0)
 *		MCP2515_FILTER_EXTENDED(0),		// Mask 1 (for group 1)
 *	};
 * \endcode
 *
 * If you want to receive both 11 and 29 bit identifiers, set your filters
 * and masks as follows:
 */
prog_uint8_t can_filter[] = 
{
	// Group 0
	MCP2515_FILTER(0),				// Filter 0
	MCP2515_FILTER(0),				// Filter 1
	
	// Group 1
	MCP2515_FILTER_EXTENDED(0),		// Filter 2
	MCP2515_FILTER_EXTENDED(0),		// Filter 3
	MCP2515_FILTER_EXTENDED(0),		// Filter 4
	MCP2515_FILTER_EXTENDED(0),		// Filter 5
	
	MCP2515_FILTER(0),				// Mask 0 (for group 0)
	MCP2515_FILTER_EXTENDED(0),		// Mask 1 (for group 1)
};
// You can receive 11 bit identifiers with either group 0 or 1.


tCAN 	can;	// CAN buffer

// -----------------------------------------------------------------------------
// Main loop for receiving and sending messages.

int main(void)
{
	// Initialize MCP2515
	can_init(BITRATE_125_KBPS);
	
	// Load filters and masks
	// can_static_filter(can_filter);
	
	// CAN test message:
	can.id = (uint32_t)0x123456;
	can.flags.rtr = 0;
	can.flags.extended = true;
	can.length = 8;
	can.data[0] = 00;
	can.data[1] = 00;
	can.data[2] = 00;
	can.data[3] = 00;
	can.data[4] = 00;
	can.data[5] = 00;
	can.data[6] = 00;
	can.data[7] = 00;
	
	DDRB |= _BV(0);

	// Swap messages
	while (1)
	{
		if( can_check_free_buffer())
		{
			can_send_message( &can);

			_delay_ms(500);	// Delay added so GridConnectLite adaptor keeps up

			// Modify CAN message each time
			if( ++can.data[7]==0)
				if( ++can.data[6]==0)
					if( ++can.data[5]==0)
						if( ++can.data[4]==0)
							if( ++can.data[3]==0)
								if( ++can.data[2]==0 )
									if( ++can.data[1]==0 )
										++can.data[0];
		}
	}
	
	return 0;
}
// -----------------------------------------------------------------------------

