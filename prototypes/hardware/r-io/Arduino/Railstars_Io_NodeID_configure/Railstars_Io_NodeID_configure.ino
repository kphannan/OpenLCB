#include <EEPROM.h>

/*
 * NodeID/EventID Initialize/Check
 *
 * Uploads new NodeID/EventIDs to EEPROM, or reads them out for verification.
 * Requires a matching script (forthcoming) on the PC side to write/read data.
 *
 */
 
 // TODO UNTESTED, NO CHECK BYTE COMPUTED, NO EVENTID READING
 
 uint8_t nid[6];
 
void setup()
{
  Serial.begin(115200);
  Serial.println("Railstars Io NodeID/EventID configuration program");
}

void loop()
{
  uint8_t count;
  char input;
  if (Serial.available() > 0)
  {
    uint8_t address;
    input = Serial.read();
    switch(input)
    {
      case 'S':  //set NodeID
        for(count = 0; count < 6; ++count)
        {
          nid[count] = Serial.read();
          //write it to EEPROM
          EEPROM.write(count, nid[count]);
        }
        //write check byte here TODO
        EEPROM.write(6, 0);
      case 'R':  //read NodeID; automatic when NodeID is set
        for(count = 0; count < 6; ++count)
        {
          nid[count] = EEPROM.read(count);
          Serial.write(nid[count]); //prints value as raw number
        }
        Serial.println();
          // TODO check check byte here
//          if(!EEPROM.read(6))
//            Serial.println("OK!");
//          else
//            Serial.println("ERROR!");
        }
        break;
      case 'E': //write eventID
        //three bytes to this command: slot to write, byte 6 of ID, byte 7 of ID (first 6 bytes are imputed from NodeID)
        //address counting starts at 0;
        address = Serial.read();
        EEPROM.write(address+7, Serial.read());
        EEPROM.write(address+8, Serial.read());
        break;
      default:
        Serial.println("Unsupported command");
    }
  }
}
