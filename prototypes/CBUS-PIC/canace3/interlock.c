// Locking
// Locked 1:	False 2r | 4r | 16r | 20r | 21r | 
// Locked 2:	False 1r | 4r | 16r | 20r | 21r | 
// Locked 3:	False 5r 4r & | 19r 4r & | 20r | 21r | 
// Locked 4:	False 3r | 1r | 2r | 20r | 21r | 5r | 19r | 
// Locked 5:	False 3r 4r & | 
// Locked 6:	False
// Locked 7:	False
// Locked 8:	False
// Locked 9:	False
// Locked 10:	False
// Locked 11:	False
// Locked 12:	False
// Locked 13:	False 15r | 18r | 19r | 
// Locked 14:	False 15n | 18r | 19r | 
// Locked 15:	False 14r | 13r | 18r | 19r | 
// Locked 16:	False 13r | 14r | 1r | 2r | 17r | 18r | 20r | 19r | 
// Locked 17:	False 16r | 21r | 
// Locked 18:	False 13r | 14r | 16r | 
// Locked 19:	False 16n | 3r 4r & | 13r | 14r | 21r | 
// Locked 20:	False 1r | 2r | 3r | 4r | 16r | 21r | 
// Locked 21:	False 17n | 1r | 2r | 3r | 4r | 19r | 20r | 
// Locked 22:	False
// Locked 23:	False
// Locked 24:	False

// Reverse polish logic for locking
// 0xFF = and, 0x7F = or, 0xFE = not, 0x00 marks the end of the equation
// 1-120 = lever normal, add 128 for Reversed.
BYTE * rom locking[25] = {
    "", // 0
    "\x82\x7F\x84\x7F\x90\x7F\x94\x7F\x95\x7F", // 1
    "\x81\x7F\x84\x7F\x90\x7F\x94\x7F\x95\x7F", // 2
    "\x85\x84\xFF\x7F\x93\x84\xFF\x7F\x94\x7F\x95\x7F", // 3
    "\x83\x7F\x81\x7F\x82\x7F\x94\x7F\x95\x7F\x85\x7F\x93\x7F", // 4
    "\x83\x84\xFF\x7F", // 5
    "", // 6
    "", // 7
    "", // 8
    "", // 9
    "", // 10
    "", // 11
    "", // 12
    "\x8F\x7F\x92\x7F\x93\x7F", // 13
    "\x0F\x7F\x92\x7F\x93\x7F", // 14
    "\x8E\x7F\x8D\x7F\x92\x7F\x93\x7F", // 15
    "\x8D\x7F\x8E\x7F\x81\x7F\x82\x7F\x91\x7F\x92\x7F\x94\x7F\x93\x7F", // 16
    "\x90\x7F\x95\x7F", // 17
    "\x8D\x7F\x8E\x7F\x90\x7F", // 18
    "\x10\x7F\x83\x84\xFF\x7F\x8D\x7F\x8E\x7F\x95\x7F", // 19
    "\x81\x7F\x82\x7F\x83\x7F\x84\x7F\x90\x7F\x95\x7F", // 20
    "\x11\x7F\x81\x7F\x82\x7F\x83\x7F\x84\x7F\x93\x7F\x94\x7F", // 21
    "", // 22
    "", // 23
    "", // 24
};
