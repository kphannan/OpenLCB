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

#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/pgmspace.h>
#include <avr/wdt.h>

#include "serjtag.h"

#define poll    usbcdc_poll

#define bit_set(port,bit)	{ port |= (1<<(bit));}
#define bit_clear(port,bit)	{ port &= ~(1<<(bit));}
#define min(a,b)		((a)<(b) ? (a):(b))

#ifdef I2C_EXTENSION

#define TGT_SDA	TGT_MOSI
#define TGT_SCL TGT_SCK

#include <util/delay.h>

/*
 *                 400kHz    1Mhz
 * Min Hi  time    1.3u      0.4
 * Min Low time    0.6u      0.4
 *
 */

const static uint8_t i2c_delay_count1 = 2; //  0.5 us + @ 12MHz
const static uint8_t i2c_delay_count2 = 6; // 1.25 us + @ 12MHz

const static uint8_t i2c_wait_count = 100; // 125us @ 12MHz

static inline void SCL_HI() {
	bit_set(TGT_PORT,TGT_SCL);
	bit_clear(TGT_DDR,TGT_SCL);
}

static void SCL_HI_CHK(uint8_t i) {
	bit_set(TGT_PORT,TGT_SCL);
	bit_clear(TGT_DDR,TGT_SCL);
	for (i=0; i<i2c_wait_count; i++) {
		_delay_loop_1(i2c_delay_count2);
		if (bit_is_set(TGT_PIN,TGT_SCL)) break;
	}
}

static void SCL_LOW_DELAY() {
	bit_set(TGT_DDR,TGT_SCL);
	bit_clear(TGT_PORT,TGT_SCL);
	_delay_loop_1(i2c_delay_count1);
}

static void SDA_HI() {
	bit_set(TGT_PORT,TGT_SDA);
	bit_clear(TGT_DDR,TGT_SDA);
}

static void SDA_LOW() {
	bit_set(TGT_DDR,TGT_SDA);
	bit_clear(TGT_PORT,TGT_SDA);
}

static void SDA_LOW_DELAY() {
	bit_set(TGT_DDR,TGT_SDA);
	bit_clear(TGT_PORT,TGT_SDA);
	_delay_loop_1(i2c_delay_count1);
}

static uint8_t i2c_clock() {
	uint8_t r = 0;
	SCL_HI_CHK(i2c_wait_count);
	if (bit_is_set(TGT_PIN,TGT_SDA)) {
		r = 1;
	}
	SCL_LOW_DELAY();
	return r;
}

void init_i2c() {
	SCL_HI();
	SDA_HI();
}

#ifdef I2C_NOCHECK_START
static void i2c_start() {
	SDA_LOW_DELAY();
	SCL_LOW_DELAY();
}
#else
static int8_t i2c_start() {
	if (bit_is_clear(TGT_PIN,TGT_SDA) || bit_is_clear(TGT_PIN,TGT_SDA)) {
		return -3; // start conditon error (1)
	}
	SDA_LOW_DELAY();
	if (bit_is_set(TGT_PIN,TGT_SDA)) {
		SDA_HI();
		return -4; // start conditon error (2)
	}
	SCL_LOW_DELAY();
	if (bit_is_set(TGT_PIN,TGT_SCL)) {
		SCL_HI_CHK(i2c_wait_count);
		SDA_HI();
		return -5; // start conditon error (2)
	}
	return 0;
}
#endif

static void i2c_stop() {
	SDA_LOW_DELAY();
	SCL_HI_CHK(i2c_wait_count);
	SDA_HI();
}

static uint8_t i2c_out(uint8_t out) {
	uint8_t i;
	for (i=0; i<8; i++) {
		/* set DATA */
		if (out & 0x80) {
			SDA_HI();
		} else {
			SDA_LOW();
		}
		i2c_clock();
		
		out <<=1;
	}
	SDA_HI();
	return i2c_clock();
}

static uint8_t i2c_in(uint8_t ack) {
	uint8_t in = 0;
	uint8_t i;
	for (i=0; i<8; i++) {
		in <<= 1;
		in |= i2c_clock();
	}
	/* set ACK */
	if (ack) {
		SDA_LOW();
	}
	i2c_clock();
	SDA_HI();
	return in;
}

int8_t i2c_send(uint8_t dev, uint8_t size) {
	uint8_t r,i;
#ifdef I2C_NOCHECK_START
	i2c_start();
#else
	r = i2c_start();
	if (r) return r;
#endif
	r = i2c_out(dev <<1);
	if (r) {
		i = (uint8_t)(-2); // device not response
		goto out;
	}
	for (i=0; i<size; i++) {
		r = i2c_out(ext_buf[i]);
		if (r) goto out;
	}
out:
	i2c_stop();
	return (int8_t)i;
}

int8_t i2c_recv(uint8_t dev, uint8_t size) {
	uint8_t r,i,j;
#ifdef I2C_NOCHECK_START
	i2c_start();
#else
	r = i2c_start();
	if (r) return r;
#endif
	r = i2c_out((dev <<1) | 1);
	if (r) {
		i = (uint8_t)(-2); // device not response
		goto out;
	}
	i = 0;
	while (i<size) {
		j = i+1;
		ext_buf[i] = i2c_in(j != size);
		i = j;
	}
out:
	i2c_stop();
	return (int8_t)i;
}

#endif
