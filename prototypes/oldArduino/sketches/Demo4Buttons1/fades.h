uint8_t fades[] = {
// period   pwm  next(relative)
      10,    31,    1,    // 0 rapid flash
      10,     0,   -1,    // 1
       0,     0,    0,    // 2 off
       0,    31,    0,    // 3 on
     100,    31,    1,    // 4 flash
     100,     0,   -1,    // 5 
      10,    31,    1,    // 6 strobe
     190,     0,   -1     // 7
};
//                        0  1  2  3  4  5  6  7  8  9
uint8_t appearances[] = { 0, 2, 3, 4, 6, 2, 2, 2, 2, 2 };

// 0 - rapid flash
// 1 - off
// 2 - on
// 3 - flash
// 4 - strobe
// 5 - off
// 6 - off
// 7 - off
// 8 - off
// 9 - off
