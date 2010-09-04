//===========================================================
// LEDuinoRamp
//   Sends a series of CAN messages with id=101
//   The data[0] field has runs from 0..100, and repeats
//===========================================================
#include <ctype.h>
#include <CAN.h>
#include <stdarg.h>
#include <stdio.h>

tCAN 		rxCAN;	// CAN receive buffer
tCAN 		txCAN;	// CAN send buffer
tCAN		* ptxCAN;

char    	strBuf[10] ;	// String Buffer

#define 	RX_BUF_SIZE	32
#define         RX_CTS_PIN      9
#define         RX_BUF_LOW      32 
#define         RX_BUF_HIGH     96
#define         BAUD_RATE       115200
//#define         BAUD_RATE       333333


char    	rxBuff[RX_BUF_SIZE];    // :lddddddddldddddddddddddddd:0
uint8_t		rxIndex;
uint16_t	rxChar;

// -----------------------------------------------------------------------------
void printHexChar(const uint8_t val);
uint8_t hex_to_byte(char *s);
uint8_t char_to_byte(char *s);
tCAN *parseCANStr(char *pBuf, tCAN *pCAN, uint8_t len);


#define ENABLE_DEBUG_MESSAGES
extern "C" {
void  debugf(const char *__fmt,...)
{
#ifdef ENABLE_DEBUG_MESSAGES
  va_list ap;
  char _str[32]; // 32 chars max!  increase if required to avoid overflow
  
  va_start(ap, __fmt);
  vsnprintf(_str, 32,__fmt, ap);
  Serial.print(_str);
#endif
}
}


void setup()
{
  pinMode(RX_CTS_PIN,OUTPUT);
  digitalWrite(RX_CTS_PIN,LOW);

  Serial.begin(BAUD_RATE);
  Serial.println();
  Serial.println(":I LEDuino CAN-USB Adaptor Version: 1;");

  // Initialize MCP2515
  can_init(BITRATE_125_KBPS);
	
  // Dump out the CAN Controller Registers
  //  Serial.println(":I Before regdump ;");
  //can_regdump();
  //Serial.println(":I After regdump ;");
  txCAN.id=101;
  txCAN.length=1;
}
byte i=0;
void loop() {
  i=i+1;
  if(i>100) i=0;
  txCAN.data[0]=i;
  while(!can_check_free_buffer()) {}
  can_send_message(&txCAN);
  delay(100);  // delay 250 milliseconds
}



// -----------------------------------------------------------------------------
void printHexChar(const uint8_t val)
{
  uint8_t tmp = val >> 4;

  if (tmp > 9)
    tmp += 'A' - 10;
  else 
    tmp += '0';
  Serial.print(tmp);

  tmp = val & 0x0f;

  if (tmp > 9) 
    tmp += 'A' - 10;
  else 
    tmp += '0';
  Serial.print(tmp);
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
