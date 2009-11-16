#ifndef EventID_h
#define EventID_h

class EventID {
  public: 
  
  byte val[8];

  EventID() {
      val[0] = 0;
      val[1] = 0;
      val[2] = 0;
      val[3] = 0;
      val[4] = 0;
      val[5] = 0;
      val[6] = 0;
      val[7] = 0;
  }
  
  EventID(byte b0, byte b1, byte b2, byte b3, byte b4, byte b5, byte b6, byte b7) {
      val[0] = b0;
      val[1] = b1;
      val[2] = b2;
      val[3] = b3;
      val[4] = b4;
      val[5] = b5;
      val[6] = b6;
      val[7] = b7;
  }
  
  boolean equals(EventID* n) {
    return  (val[0]==n->val[0])&&(val[1]==n->val[1])
          &&(val[2]==n->val[2])&&(val[3]==n->val[3])
          &&(val[4]==n->val[4])&&(val[5]==n->val[5])
          &&(val[6]==n->val[6])&&(val[7]==n->val[7]);
  }
  
  /**
   * Check to see if this object is equal
   * to any in an array of EventIDs
   */
  EventID* findEidInArray(EventID* array, int len) {
      for (int i = 0; i<len; i++) {
          if (equals(array+i)) return array+i;
      }
      return 0;
  }
  
  int findIndexInArray(EventID* array, int len) {
      for (int i = 0; i<len; i++) {
          if (equals(array+i)) return i;
      }
      return -1;
  }
  
};

#endif
