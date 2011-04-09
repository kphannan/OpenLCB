#define TGT_PORT	PORTB
#define TGT_DDR		DDRB
#define TGT_PIN		PINB

#define TGT_MISO	PB5
#define TGT_SCK		PB7
#define TGT_MOSI	PB6
#define TGT_RESET	PB4

#define I2C_EXTENSION
//#define I2C_NOCHECK_START
//#define NO_CLKPR_SET
#define USE_USI_SPI
#define USI_SLOWMODE

#define DEV_SYS         0
#define DEV_I2C         10

extern int8_t i2c_recv(uint8_t dev, uint8_t size);
extern int8_t i2c_send(uint8_t dev, uint8_t size);
extern void init_i2c();

#define EXT_BUFSIZE	16
extern uint8_t ext_buf[];
