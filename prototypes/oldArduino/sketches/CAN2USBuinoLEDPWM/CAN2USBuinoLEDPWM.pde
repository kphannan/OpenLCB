
//==========================================================
// LEDuinoPWM
//   Receives CAN messages with id=101
//   Drives a LED on pin LED_PIN with PWM dependant on the 
//      first data byte.  
//==========================================================

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
  Serial.println(":I LEDuino LED PWM;");

  // Initialize MCP2515
  can_init(BITRATE_125_KBPS);
	
  // Dump out the CAN Controller Registers
  //  Serial.println(":I Before regdump ;");
  //can_regdump();
  //Serial.println(":I After regdump ;");

  #define LED_PIN 9
  pinMode(LED_PIN, OUTPUT);   // sets the pin as output
  analogWrite(LED_PIN, 100);

}

int bright = 100;
void loop() {
  if(can_get_message(&rxCAN)) { 
//    Serial.print("rxCAN.data[0]=");
//    Serial.println(rxCAN.data[0], HEX); 
    if(rxCAN.id==101) {
      bright = rxCAN.data[0]<<1;  
    }
    memset(&rxCAN, 0, sizeof(tCAN));
  }
  analogWrite(LED_PIN, bright);  // analogRead values go from 0 to 1023, analogWrite values from 0 to 255
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
