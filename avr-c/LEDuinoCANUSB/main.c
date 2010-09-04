// coding: utf-8

#include <avr/io.h>
#include <avr/pgmspace.h>
#include <avr/interrupt.h>

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

#include <util/delay.h>

#include "can.h"
#include "uart.h"

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


tCAN 		rxCAN;	// CAN receive buffer
tCAN 		txCAN;	// CAN send buffer
tCAN		* ptxCAN;

char    	strBuf[10] ;	// String Buffer
					// 12345678901234567890123456789

#define 	RX_BUF_SIZE	32
char    	rxBuff[RX_BUF_SIZE];    // :lddddddddldddddddddddddddd:0
uint8_t		rxIndex;
uint16_t	rxChar;

// -----------------------------------------------------------------------------
void uart_put_hex(const uint8_t val);
uint8_t hex_to_byte(char *s);
uint8_t char_to_byte(char *s);

/* -----------------------------------------------------------------------------
Parser Test Cases
	:X1234N12345678;
	:XN;	(not valid but we parse to :X0N;
	:SN;	(not valid but we parse to :S0N;
	:SR0;	(not valid but we parse to :S0R0;
	:SR;	(not valid but we parse to :S0R0; 
	:S1R8;
	:S1N11;
	:S3FFN;
	:S7FFN;
	:S8FFN;	(Invalid)
	:X1FFFFFFFN1234567812345678;
	:X2FFFFFFFN1234567812345678; (Invalid)
-----------------------------------------------------------------------------*/

tCAN *parseCANStr(char *pBuf, tCAN *pCAN, uint8_t len)
{
	if( (pBuf[0] == ':') && (pBuf[len-1] == ';') && (len >= 4) && ( (pBuf[1] == 'X') || (pBuf[1] == 'S') ) )
	{
		memset(pCAN, 0, sizeof(tCAN));

		pCAN->flags.extended = pBuf[1] == 'X';

		char *pEnd;

		pCAN->id = strtoul(pBuf+2, &pEnd, 16);

			// If Standard Frame then check to see if Id is in the valid 11-bit range
		if((pCAN->flags.extended && (pCAN->id > 0x1FFFFFFF)) || (!pCAN->flags.extended && (pCAN->id > 0x07FF)))
			return NULL;
		 
		if(*pEnd == 'N')
		{
			pEnd++;
			pCAN->length = 0;
			while(isxdigit(*pEnd))
			{
				pCAN->data[pCAN->length] = hex_to_byte(pEnd);
				pCAN->length++;
				pEnd += 2;
			}
		}
		else if(*pEnd == 'R')
		{  
			pCAN->flags.rtr = 1;
			char tChar = *(pEnd+1);
			if(isdigit(tChar))
				pCAN->length = *(pEnd+1) - '0';
		}
		return pCAN;
	}
	return NULL;
} 


// -----------------------------------------------------------------------------
// Main loop for receiving and sending messages.

int main(void)
{
	// Initialise the UART
    uart_init( UART_BAUD_SELECT(250000UL,F_CPU) ); 
//    uart_init( UART_BAUD_SELECT(460800UL,F_CPU) ); 
//    uart_init( UART_BAUD_SELECT(230400UL,F_CPU) ); 
//    uart_init( UART_BAUD_SELECT(500000UL,F_CPU) ); 
    sei();
    uart_puts_P(":I LEDuino CAN-USB Adaptor Version: 1;\r\n");
    
	// Initialize MCP2515
	can_init(BITRATE_125_KBPS);
	
	// Load filters and masks
	can_static_filter(can_filter);

	ptxCAN = 0;
	
	DDRB |= _BV(0);

	// Swap messages
	while (1)
	{
		rxChar = uart_getc();
		switch(rxChar)
		{
		case ':':
			rxIndex = 0;
			rxBuff[rxIndex++] = rxChar & 0x00FF;
			break;

		case ';':
			if( rxIndex < RX_BUF_SIZE )
			{
				rxBuff[rxIndex++] = rxChar & 0x00FF;
				rxBuff[rxIndex] = '\0';	// Null Terminate the string

				ptxCAN = parseCANStr(rxBuff, &txCAN, rxIndex);
				if(ptxCAN && can_check_free_buffer())
				{
					if(can_send_message(ptxCAN))
						ptxCAN = NULL; 
				}
			}
			rxIndex = 0;
			break;

		case UART_FRAME_ERROR:
		case UART_OVERRUN_ERROR:
		case UART_BUFFER_OVERFLOW:
			rxIndex = 0;	// If we get an error then ditch the current rxBuff contents 
			break;

		case UART_NO_DATA:
			break;

		default:			// Everything else must be a 
			if( rxIndex < RX_BUF_SIZE )
				rxBuff[rxIndex++] = rxChar & 0x00FF;
			break;
		}

		if(can_get_message(&rxCAN))
		{
			uart_putc(':');
			uart_putc(rxCAN.flags.extended ? 'X' : 'S');

			uart_puts(strupr(ultoa(rxCAN.id, strBuf, 16)));

			if(rxCAN.flags.rtr)
			{
				uart_putc('R');
				uart_putc('0' + rxCAN.length);
			}
			else
			{
				uart_putc('N');
				for( uint8_t i = 0; i < rxCAN.length; i++)
				{
					uart_put_hex(rxCAN.data[i]);
				}
			}
			uart_puts_P(";\r\n");

			memset(&rxCAN, 0, sizeof(tCAN));
		}
		
	}
	
	return 0;
}

// -----------------------------------------------------------------------------
void uart_put_hex(const uint8_t val)
{
	uint8_t tmp = val >> 4;
	
	if (tmp > 9)
		tmp += 'A' - 10;
	else 
		tmp += '0';
	uart_putc(tmp);
	
	tmp = val & 0x0f;
	
	if (tmp > 9) 
		tmp += 'A' - 10;
	else 
		tmp += '0';
	uart_putc(tmp);
}
// -----------------------------------------------------------------------------
uint8_t char_to_byte(char *s)
{
	uint8_t t = *s;
	
	if (t >= 'a')
		t = t - 'a' + 10;
	else if (t >= 'A')
		t = t - 'A' + 10;
	else
		t = t - '0';
	
	return t;
}

// -----------------------------------------------------------------------------
uint8_t hex_to_byte(char *s)
{
	return (char_to_byte(s) << 4) | char_to_byte(s + 1);
}

