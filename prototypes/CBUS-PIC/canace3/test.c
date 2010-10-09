// Locking
// Locked 1:	 2N or 3R
// Locked 2:	 (1R and 2R)
// Locked 3:	 (1R and 3N)
// Locked 4:	 (5N or 6N or 7R) or 33N
// Locked 5:	 (4R and 5R)
// Locked 6:	 (4R and 5R and 6R)
// Locked 7:	 (4R and 5R and 7N)
// Locked 8:	 (9N and 10N)
// Locked 9:	 (8R and 9R)
// Locked 10:	 (8R and 10R)
// Locked 11:	 (12N or 13N or 14R) or (14R and 34N)
// Locked 12:	 (11R and 12R)
// Locked 13:	 (11R and 12R and 13R)
// Locked 14:	 (11R and 12R and 14N)
// Locked 15:	 16N or 17N
// Locked 16:	Never
// Locked 17:	 (15R and 17R)
// Locked 18:	 (19R and 18N) or (20R and 18N)
// Locked 19:	 (18R and 19N)
// Locked 20:	 (18R and 20N)
// Locked 21:	 (22R and 21N and 23R and 24N) or (42R and 21N and 23R and 24N)
// Locked 22:	 (21R and 22N and 23R and 24N)
// Locked 23:	Never
// Locked 24:	Never
// Locked 25:	Never
// Locked 26:	 25R
// Locked 27:	 25R
// Locked 28:	Never
// Locked 29:	 28R
// Locked 30:	 (28R and 31R and 32N)
// Locked 31:	Never
// Locked 32:	Never
// Locked 33:	 (4R and 33R)
// Locked 34:	 (11R and 34R)
// Locked 35:	Never
// Locked 36:	 37N
// Locked 37:	 (36R and 37R)
// Locked 38:	 (39N or 40N)
// Locked 39:	 (38R and 39R)
// Locked 40:	 (38R and 39R and 40R)
// Locked 41:	 (28R and 31R and 32N)
// Locked 42:	 (21R and 42N and 23R and 24N)

// Reverse polish logic for locking
// 0xFF = and, 0x7F = or, 0x00 marks the end of the equation
// 1-120 = lever normal, add 128 for Reversed.
BYTE * rom locking[43] = {
    "", // 0
    "\x02\x83\x7F", // 1
    "\x81\x82\xFF", // 2
    "\x81\x03\xFF", // 3
    "\x05\x06\x7F\x87\x7F\x21\x7F", // 4
    "\x84\x85\xFF", // 5
    "\x84\x85\xFF\x86\xFF", // 6
    "\x84\x85\xFF\x07\xFF", // 7
    "\x09\x0A\xFF", // 8
    "\x88\x89\xFF", // 9
    "\x88\x8A\xFF", // 10
    "\x0C\x0D\x7F\x8E\x7F\x8E\x22\xFF\x7F", // 11
    "\x8B\x8C\xFF", // 12
    "\x8B\x8C\xFF\x8D\xFF", // 13
    "\x8B\x8C\xFF\x0E\xFF", // 14
    "\x10\x11\x7F", // 15
    "", // 16
    "\x8F\x91\xFF", // 17
    "\x93\x12\xFF\x94\x12\xFF\x7F", // 18
    "\x92\x13\xFF", // 19
    "\x92\x14\xFF", // 20
    "\x96\x15\xFF\x97\xFF\x18\xFF\xAA\x15\xFF\x97\xFF\x18\xFF\x7F", // 21
    "\x95\x16\xFF\x97\xFF\x18\xFF", // 22
    "", // 23
    "", // 24
    "", // 25
    "\x99", // 26
    "\x99", // 27
    "", // 28
    "\x9C", // 29
    "\x9C\x9F\xFF\x20\xFF", // 30
    "", // 31
    "", // 32
    "\x84\xA1\xFF", // 33
    "\x8B\xA2\xFF", // 34
    "", // 35
    "\x25", // 36
    "\xA4\xA5\xFF", // 37
    "\x27\x28\x7F", // 38
    "\xA6\xA7\xFF", // 39
    "\xA6\xA7\xFF\xA8\xFF", // 40
    "\x9C\x9F\xFF\x20\xFF", // 41
    "\x95\x2A\xFF\x97\xFF\x18\xFF", // 42
};
