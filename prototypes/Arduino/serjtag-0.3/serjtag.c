/*
 * Copyright (c) 2007 Koji Suzuki
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/* circuit memo

 
   USB  -----  FT245R   -------- ATtiny2313V -- R -- JTAG
 
    |       3V3OUT  VCCIO         VCC
    |          |      |            |
    |          |      +------------+
    |          |                   |
    |          |   D1       D2     |
    |          | +-|>|-+ +-|>|-+   |
    |          | |     | |     |   |
    |          +-+-o o-+-+-o o-+---+
    V              ---     ---
   PWROUT(+5V)     SW1      SW2

       
  SW1 = ON , SW2 = ON  -> I/O base 3.3V (usable +- 25% ?)
  SW1 = OFF, SW2 = OFF -> I/O base 2.7V (usable +- 25% ?)
  SW1 = OFF, SW2 = OFF -> I/O base 2.1V (usable +- 25% ?)

                     ATtiny2313V
                     
                ~RESET --+   VCC
       D4          PD0   |   SCK     TGT_SCK   TGT_TCK --- R ---o
       D5          PD1   |   MISO    TGT_MOSI  TGT_TDI --- R ---o
       RXF#        PA1   |   MOSI    TGT_MISO  TGT_TDO --- R ---o
       TXE#        PA0   |   PB4     TGT_RST   TGT_TMS --+
       D6          PD2   |   PB3     D3                  |
       D7          PD3   |   PB2     D2                  o
       RD#         PD4   |   PB1     D1              SW3 o-- R -o
       WR          PD5   |   PB0     D0                  o
                   GND   |   PD6     PWREN#              |
                         |                               |
                         +-------------------------------+

  SW3 ... for self programming

 */


#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/pgmspace.h>
#include <avr/wdt.h>
#include <util/delay.h>

#include "serjtag.h"

#define JTAG_BUFSIZE	64

#define getc	par_getc
#define putc	par_putc

#define bit_set(port,bit)	{ port |= (1<<(bit));}
#define bit_clear(port,bit)	{ port &= ~(1<<(bit));}
#define min(a,b)		((a)<(b) ? (a):(b))


#define PAR_WR		PD5
#define PAR_WR_PORT	PORTD
#define PAR_WR_DDR	DDRD

#define PAR_RD		PD4
#define PAR_RD_PORT	PORTD
#define PAR_RD_DDR	DDRD

#define PAR_TXE		PA0
#define PAR_TXE_PIN	PINA

#define PAR_RXF		PA1
#define PAR_RXF_PIN	PINA

#define PAR_PWREN	PD6
#define PAR_PWREN_PIN	PIND

#define PAR_DATA_H_DDR  DDRD
#define PAR_DATA_L_DDR  DDRB

#define PAR_DATA_H_PORT  PORTD
#define PAR_DATA_L_PORT  PORTB

#define PAR_DATA_H_PIN  PIND
#define PAR_DATA_L_PIN  PINB

static void par_init() {
	bit_set(PAR_WR_DDR, PAR_WR);
	bit_set(PAR_RD_DDR, PAR_RD);
	bit_set(PAR_RD_PORT, PAR_RD);
}

#define swap(v)	__asm__ volatile ("swap %0" : "=r" (v) : "0" (v) )
#define nop()	__asm__ volatile ("nop" )

static inline uint8_t par_getcx() {
	uint8_t hi,lo;
	while (bit_is_set(PAR_RXF_PIN, PAR_RXF))
		;
	bit_clear(PAR_RD_PORT, PAR_RD);
	nop();
	hi = PAR_DATA_H_PIN;
	lo = PAR_DATA_L_PIN;
	bit_set(PAR_RD_PORT, PAR_RD);
	swap(hi);
	return (hi & 0xf0) | (lo & 0xf);
}

static uint8_t par_getc() {
	return par_getcx();
}

static void par_putc(uint8_t c) {
	while (bit_is_set(PAR_TXE_PIN, PAR_TXE))
		;
	bit_set(PAR_WR_PORT, PAR_WR);
	PAR_DATA_H_DDR |= 0xf;
	PAR_DATA_L_DDR |= 0xf;

	PAR_DATA_L_PORT |= c & 0xf;
	swap(c);
	PAR_DATA_H_PORT |= c & 0xf;

	nop();
	bit_clear(PAR_WR_PORT, PAR_WR);

	PAR_DATA_H_PORT &= 0xf0;
	PAR_DATA_L_PORT &= 0xf0;
	PAR_DATA_H_DDR &= 0xf0;
	PAR_DATA_L_DDR &= 0xf0;
}

static inline void par_putcx_start() {
	PAR_DATA_H_DDR |= 0xf;
	PAR_DATA_L_DDR |= 0xf;
}

static inline void par_putcx(uint8_t c) {
	while (bit_is_set(PAR_TXE_PIN, PAR_TXE))
		;
	bit_set(PAR_WR_PORT, PAR_WR);

	PAR_DATA_L_PORT &= 0xf0;
	PAR_DATA_L_PORT |= c & 0xf;

	swap(c);
	PAR_DATA_H_PORT &= 0xf0;
	PAR_DATA_H_PORT |= c & 0xf;

	nop();
	bit_clear(PAR_WR_PORT, PAR_WR);
}

static inline void par_putcx_stop() {
	PAR_DATA_H_PORT &= 0xf0;
	PAR_DATA_L_PORT &= 0xf0;
	PAR_DATA_H_DDR &= 0xf0;
	PAR_DATA_L_DDR &= 0xf0;
}

static uint8_t extmode = 0;
uint8_t ext_buf[JTAG_BUFSIZE+1];

static inline void init_spi() {
	TGT_DDR &=  ~((1<<TGT_MISO));
	TGT_PORT |=  ((1<<TGT_RESET));
	TGT_PORT &=  ~((1<<TGT_SCK) | (1<<TGT_MOSI) |(1<<TGT_MISO) );
	TGT_DDR |=    ((1<<TGT_SCK) | (1<<TGT_MOSI) |(1<<TGT_RESET) );
#ifdef USE_USI_SPI
	USICR = (1<<USIWM0); // enable USI as SPI Master
#endif
}

static inline void term_spi() {
#ifdef USE_USI_SPI
	USICR = 0; // disable USI
#endif
	TGT_DDR &=   ~((1<<TGT_SCK) | (1<<TGT_MOSI) | (1<<TGT_RESET) |(1<<TGT_MISO));
	TGT_PORT &=  ~((1<<TGT_SCK) | (1<<TGT_MOSI) | (1<<TGT_RESET) |(1<<TGT_MISO));
}

/* JTAG COMMAND format
 *
 *  +-----------+-----+-----+-----------+-----------+    +-----------+
 *  | COMMONAD  |FLAGS|BITS |   BYTES   |   DATA 1  |... |   DATA N  |
 *  +-----------+-----+-----+-----------+-----------+    +-----------+
 *       8         5     3       8
 *
 *  N : BYTES + (BITS != 0)?1:0 
 *  DATA: MSB first bit-stream
 *  DATA length : 8 * BYTES + BITS
 *
 */
#define TGT_TDI		TGT_MOSI
#define TGT_TDO		TGT_MISO
#define TGT_TCK		TGT_SCK
#define TGT_TMS		TGT_RESET

/* SET command parameter */
#define JTAG_SET_TDI	0x80
#define JTAG_SET_TMS	0x40
#define JTAG_SET_TCK	0x20
#define JTAG_SET_DELAY	0x0f

uint8_t jtag_delay = 1;
uint8_t jtag_flags;
#define JTAG_RECIEVE	0x80
#define JTAG_TMS_HIGH	0x40
#define JTAG_TDI_HIGH	0x20
#define JTAG_USE_DELAY	0x10
#define JTAG_BITS	0x07
#define JTAG_CONST_TMS	0x01 /* interval */

#define jtag_buf ext_buf
uint8_t jtag_bytes;
uint8_t jtag_bits;
uint8_t jtag_bitoff;

static inline void jtag_set(uint8_t flags) {
	if (flags & JTAG_SET_TDI) {
#ifdef USE_USI_SPI
		bit_set(USIDR, 7);
#else
		bit_set(TGT_PORT, TGT_TDI);
#endif
	} else {
#ifdef USE_USI_SPI
		bit_clear(USIDR, 7);
#else
		bit_clear(TGT_PORT, TGT_TDI);
#endif
	}
	if (flags & JTAG_SET_TMS) {
		bit_set(TGT_PORT, TGT_TMS);
	} else {
		bit_clear(TGT_PORT, TGT_TMS);
	}
	if (flags & JTAG_SET_TCK) {
		bit_set(TGT_PORT, TGT_TCK);
	} else {
		bit_clear(TGT_PORT, TGT_TCK);
	}
	jtag_delay = (uint8_t)(F_CPU/4000000) * (flags & JTAG_SET_DELAY) + 1;
	             /* 2 @8Mhz, 3@12Mhz */
}

static inline void jtag_prepare(uint8_t flags) {
	jtag_flags = flags;
	if (flags & JTAG_CONST_TMS) {
		if (flags & JTAG_TMS_HIGH) {
			bit_set(TGT_PORT, TGT_TMS);
		} else {
			bit_clear(TGT_PORT, TGT_TMS);
		}
	}
	jtag_bytes = 0;
	jtag_bits = 0;
	jtag_bitoff = 0x80;
}

static inline void jtag_send_8(uint8_t flags, uint8_t data, uint8_t bits) {
	uint8_t rcv = 0;
#ifdef USE_USI_SPI
	uint8_t v1,v2;
#ifdef USI_SLOWMODE
	/* F_CPU/6 bitclock(max) */
	uint8_t i;
	v1 = (1<<USIWM0)|(1<<USITC);
	v2 = (1<<USIWM0)|(1<<USITC)|(1<<USICLK);
	USIDR = data;
	for (i=0; i<8; i++) {
		USICR = v1;
		nop();
		nop();
		USICR = v2;
	}
	rcv = USIDR;
#else
	/* F_CPU/2 bitclock */
	v1 = (1<<USIWM0)|(1<<USITC);
	v2 = (1<<USIWM0)|(1<<USITC)|(1<<USICLK);
	USIDR = data;
	USICR = v1;
	USICR = v2;
	USICR = v1;
	USICR = v2;
	USICR = v1;
	USICR = v2;
	USICR = v1;
	USICR = v2;
	USICR = v1;
	USICR = v2;
	USICR = v1;
	USICR = v2;
	USICR = v1;
	USICR = v2;
	USICR = v1;
	USICR = v2;
	rcv = USIDR;
#endif
#else
	uint8_t i;
	for (i=0; i<8; i++) {
		rcv <<= 1;
		if (data & 0x80) {
			bit_set(TGT_PORT, TGT_TDI);
		} else {
			bit_clear(TGT_PORT, TGT_TDI);
		}
		data <<= 1;
		bit_set(TGT_PORT, TGT_TCK);
		if (bit_is_set(TGT_PIN, TGT_TDO)) {
			rcv |= 1;
		}
		nop();
		bit_clear(TGT_PORT, TGT_TCK);
	}
#endif
	if (flags & JTAG_RECIEVE) {
		jtag_buf[jtag_bytes++] = rcv;
	}
}

static void jtag_send_d(uint8_t flags, uint8_t data, uint8_t bits) {
	uint8_t i,delay;

	delay = jtag_delay;
	for (i=0; i<bits; i++) {
		if (data & 0x80) {
#ifdef USE_USI_SPI
			bit_set(USIDR, 7);
#else
			bit_set(TGT_PORT, TGT_TDI);
#endif
		} else {
#ifdef USE_USI_SPI
			bit_clear(USIDR, 7);
#else
			bit_clear(TGT_PORT, TGT_TDI);
#endif
		}
		data <<= 1;
		if (!(flags & JTAG_CONST_TMS)) {
			if (data & 0x80) {
				bit_set(TGT_PORT, TGT_TMS);
			} else {
				bit_clear(TGT_PORT, TGT_TMS);
			}
			data <<= 1;
		}
		_delay_loop_1(delay);
		bit_set(TGT_PORT, TGT_TCK);
		if (flags & JTAG_RECIEVE) {
			if (bit_is_set(TGT_PIN, TGT_TDO)) {
				jtag_buf[jtag_bytes] |= jtag_bitoff;
			} else {
				jtag_buf[jtag_bytes] &= ~jtag_bitoff;
			}
			jtag_bits++;
			jtag_bitoff >>= 1;
			if (jtag_bitoff == 0) {
				jtag_bitoff = 0x80;
				jtag_bytes++;
			}
		}
		_delay_loop_1(delay);
		bit_clear(TGT_PORT, TGT_TCK);
	}
}

int main(void)
{
    uint8_t ch;

#ifndef NO_CLKPR_SET
    /* set system_clock pre scaler 1/8 => 1/1 */
    CLKPR = (1<<CLKPCE);
    CLKPR = 0;
#endif

    par_init();

    sei();

    for(;;) {
#if XXX
	if (bit_is_set(PAR_PWREN_PIN, PAR_PWREN)) {
		// reset 
		wdt_enable(WDTO_15MS);
		for (;;)
		    ;
	}
#endif

	ch = getc();

	if (ch == 'S') { // get Software
		putc('S');
		putc('E');
		putc('R');
		putc('J');
		putc('T');
		putc('A');
		putc('G');
	} else if (ch == 'V' || ch == 'v') { // get Version 'V':sw 'v':hw
		extmode = 0;
		//term_spi();
		putc('1');
		putc('0');
	} else if ((ch == 0x1b) || (ch == '\n')) { // ESC,\n: nop
	} else if (extmode == 2) {
	    uint8_t bytes,bits,flags;
	    uint8_t i,r;
	   if (ch == 'r') { // Request Recieved Data
		getc(); getc(); // skip header
		flags = jtag_bits & JTAG_BITS;
		bytes = jtag_bytes;
		if (flags) bytes++;
		putc('R');
		putc(flags);
		putc(jtag_bytes);
		par_putcx_start();
		for (i=0 ; i< bytes; i++) {
			par_putcx(jtag_buf[i]);
		}
		par_putcx_stop();
		jtag_bits = 0;
		jtag_bytes = 0;
	   } else if (ch == 's') { //  Set Port
		getc(); getc(); // skip header
		jtag_set(getc());
	   } else if (ch == 'd') { // Put TDI Stream
		bits = getc();
		bytes = getc();
		flags = (bits & ~JTAG_BITS) | JTAG_CONST_TMS;
		bits &= JTAG_BITS;
		jtag_prepare(flags);
		if (flags & JTAG_USE_DELAY) {
			for (i=0; i<bytes; i++) {
				jtag_send_d(flags, getc(), 8);
			}
		} else {
			for (i=0; i<bytes; i++) {
				jtag_send_8(flags, par_getcx(), 8);
			}
		}
		if (bits) {
			jtag_send_d(flags, getc(), bits);
		}
	   } else if (ch == 'D') { // Put TDI+TMS Stream
		bits = getc();
		bytes = getc();
		flags = (bits & ~JTAG_BITS);
		bits &= JTAG_BITS;
		for (i=0; i<bytes; i++) {
			jtag_send_d(flags, getc(), 4);
		}
		bits >>= 1;
		if (bits) {
			jtag_send_d(flags, getc(), bits);
		}
	   } else if (ch == 'c') { // Get TDO Stream
		bits = getc();
		bytes = getc();
		flags = (bits & ~JTAG_BITS) | JTAG_RECIEVE;
		bits &= JTAG_BITS;
		jtag_prepare(flags);
		r = (flags & JTAG_TDI_HIGH)? 0xff: 0;
		if (flags & JTAG_USE_DELAY) {
			for (i=0; i<bytes; i++) {
				jtag_send_d(flags, r, 8);
			}
		} else {
			for (i=0; i<bytes; i++) {
				jtag_send_8(flags, r, 8);
			}
		}
		if (bits) {
			jtag_send_d(flags, r, bits);
		}
	   }
	} else if (extmode == 1) {
	    uint8_t bytes,i;
	    uint8_t dev;
	    int8_t r;
	    r = -1; // device is not existed
	    dev = getc();
	    bytes = getc();
	    if (ch == 's' ) { // Send
		for (i=0; i<bytes && i< EXT_BUFSIZE; i++) {
		    ext_buf[i] = getc();
		}
		for (; i<bytes; i++) {
			getc();
		}
		bytes = i;
		if (dev == DEV_SYS) {
#ifdef I2C_EXTENSION
		} else if (DEV_I2C <= dev && dev <= 0x7f) {
		    r = i2c_send(dev, bytes);
		}
#endif
		putc(r);
	    } else if (ch == 'r') { // Recv
		 if (bytes > EXT_BUFSIZE)
			 bytes = EXT_BUFSIZE;
		if (dev == DEV_SYS) {
		    ext_buf[0] = PORTA;
		    r = 1;
#ifdef I2C_EXTENSION
		} else if (DEV_I2C <= dev && dev <= 0x7f) {
		    r = i2c_recv(dev, bytes);
#endif
		}
		putc(r);
		for (i=0; i<r; i++) {
		    putc(ext_buf[i]);
	        }
	   } else {
		putc(r);
	   }
	} else if (ch == 'e') { // Enter Extent mode 1
		extmode = 1;
		//term_spi();
#ifdef I2C_EXTENSION
		init_i2c();
#endif
		putc('Y');
	} else if (ch == 'j') { // Enter Extent mode 2 (JTAG)
		extmode = 2;
		jtag_bytes = 0;
		jtag_bits = 0;
		init_spi();
		putc('Y');
	} else {
		putc('?');
	}
    }
    return 0;
}
