#ifndef __OLCB_EVENTID_H__
#define __OLCB_EVENTID_H__

#include <stdint.h>

class OLCB_EventID {
  public: 
  
  uint8_t val[8];

  OLCB_EventID() {
      val[0] = 0;
      val[1] = 0;
      val[2] = 0;
      val[3] = 0;
      val[4] = 0;
      val[5] = 0;
      val[6] = 0;
      val[7] = 0;
  }
  
  OLCB_EventID(uint8_t b0, uint8_t b1, uint8_t b2, uint8_t b3, uint8_t b4, uint8_t b5, uint8_t b6, uint8_t b7) {
      val[0] = b0;
      val[1] = b1;
      val[2] = b2;
      val[3] = b3;
      val[4] = b4;
      val[5] = b5;
      val[6] = b6;
      val[7] = b7;
  }
  
  bool operator==(const OLCB_EventID &other) const
  {
    return  (val[0]==other.val[0])&&(val[1]==other.val[1])
          &&(val[2]==other.val[2])&&(val[3]==other.val[3])
          &&(val[4]==other.val[4])&&(val[5]==other.val[5]);
  }
  
  bool operator!=(const OLCB_EventID &other) const
  {
    return !(*this == other);
  }
  
  /**
   * Check to see if this object is equal
   * to any in an array of OLCB_EventIDs
   */
  OLCB_EventID* findEidInArray(OLCB_EventID* array, int len) {
      for (int i = 0; i<len; i++) {
          if (*this == *(array+i)) return array+i;
      }
      return 0;
  }
  
  int findIndexInArray(OLCB_EventID* array, int len) {
      for (int i = 0; i<len; i++) {
          if (*this == *(array+i)) return i;
      }
      return -1;
  }
  
};

#endif
