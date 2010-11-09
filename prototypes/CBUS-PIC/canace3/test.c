// Locking
// Locked 1:	False 2n | 3r | 
// Locked 2:	False 1r | 
// Locked 3:	False 1r | 
// Locked 4:	False 5r 6r & | 
// Locked 5:	False 4r 6r & | 
// Locked 6:	False 4r | 
// Locked 7:	False
// Locked 8:	False 9n 10n & | 
// Locked 9:	False 8r | 
// Locked 10:	False 8r | 
// Locked 11:	False 12r 13r & 14n & 14n 34n & | 
// Locked 12:	False 11r 13r & 14n & | 
// Locked 13:	False 11r | 
// Locked 14:	False 11r | 
// Locked 15:	False 16n | 17n | 
// Locked 16:	False
// Locked 17:	False 15r | 
// Locked 18:	False 19r | 20r | 43n | 
// Locked 19:	False 18r | 
// Locked 20:	False 18r | 
// Locked 21:	False 22r 23r & 24n & | 42r 23r & 24n & | 
// Locked 22:	False 21r 23r & 24n & | 
// Locked 23:	False
// Locked 24:	False
// Locked 25:	False
// Locked 26:	False 25r | 
// Locked 27:	False 25r | 
// Locked 28:	False
// Locked 29:	False 28r | 
// Locked 30:	False 28r 31r & | 
// Locked 31:	False 28r | 
// Locked 32:	False 28r | 
// Locked 33:	False
// Locked 34:	False 11r | 
// Locked 35:	False
// Locked 36:	False 37n | 
// Locked 37:	False 36r | 
// Locked 38:	False 39r 40r & | 
// Locked 39:	False 38r 40n & | 
// Locked 40:	False 38r | 
// Locked 41:	False 28r 32n & | 
// Locked 42:	False 21r 23r & 24n & | 
// Locked 43:	False
// Locked 44:	False 45r 46r & 47r & | 
// Locked 45:	False 44r 46r & 47r & | 
// Locked 46:	False 44r | 44r | 45r 47r & | 
// Locked 47:	False 44r 46r & | 45r | 45r | 
// Locked 48:	False

// Reverse polish logic for locking
// 0xFF = and, 0x7F = or, 0xFE = not, 0x00 marks the end of the equation
// 1-120 = lever normal, add 128 for Reversed.
BYTE * rom locking[49] = {
    "", // 0
    "\x02\x7F\x83\x7F", // 1
    "\x81\x7F", // 2
    "\x81\x7F", // 3
    "\x85\x86\xFF\x7F", // 4
    "\x84\x86\xFF\x7F", // 5
    "\x84\x7F", // 6
    "", // 7
    "\x09\x0A\xFF\x7F", // 8
    "\x88\x7F", // 9
    "\x88\x7F", // 10
    "\x8C\x8D\xFF\x0E\xFF\x0E\x22\xFF\x7F", // 11
    "\x8B\x8D\xFF\x0E\xFF\x7F", // 12
    "\x8B\x7F", // 13
    "\x8B\x7F", // 14
    "\x10\x7F\x11\x7F", // 15
    "", // 16
    "\x8F\x7F", // 17
    "\x93\x7F\x94\x7F\x2B\x7F", // 18
    "\x92\x7F", // 19
    "\x92\x7F", // 20
    "\x96\x97\xFF\x18\xFF\x7F\xAA\x97\xFF\x18\xFF\x7F", // 21
    "\x95\x97\xFF\x18\xFF\x7F", // 22
    "", // 23
    "", // 24
    "", // 25
    "\x99\x7F", // 26
    "\x99\x7F", // 27
    "", // 28
    "\x9C\x7F", // 29
    "\x9C\x9F\xFF\x7F", // 30
    "\x9C\x7F", // 31
    "\x9C\x7F", // 32
    "", // 33
    "\x8B\x7F", // 34
    "", // 35
    "\x25\x7F", // 36
    "\xA4\x7F", // 37
    "\xA7\xA8\xFF\x7F", // 38
    "\xA6\x28\xFF\x7F", // 39
    "\xA6\x7F", // 40
    "\x9C\x20\xFF\x7F", // 41
    "\x95\x97\xFF\x18\xFF\x7F", // 42
    "", // 43
    "\xAD\xAE\xFF\xAF\xFF\x7F", // 44
    "\xAC\xAE\xFF\xAF\xFF\x7F", // 45
    "\xAC\x7F\xAC\x7F\xAD\xAF\xFF\x7F", // 46
    "\xAC\xAE\xFF\x7F\xAD\x7F\xAD\x7F", // 47
    "", // 48
};
