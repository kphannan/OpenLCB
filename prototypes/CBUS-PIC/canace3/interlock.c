// Locking
// Locked 1:	 (2R and 1N) or (4R and 1N) or (16R and 1N) or (20R and 1N) or (21R and 1N)
// Locked 2:	 (1R and 2N) or (4R and 2N) or (16R and 2N) or (20R and 2N) or (21R and 2N)
// Locked 3:	 (5R and 3N and 4R) or (19R and 3N) or (20R and 3N) or (21R and 3N)
// Locked 4:	 (1R and 4N) or (2R and 4N) or 3R or 5R or (20R and 4N) or (21R and 4N)
// Locked 5:	 (3R and 5N and 4R)
// Locked 6:	Never
// Locked 7:	Never
// Locked 8:	Never
// Locked 9:	Never
// Locked 10:	Never
// Locked 11:	Never
// Locked 12:	Never
// Locked 13:	 (14R and 13N) or (15R and 13N) or (18R and 13N) or (19R and 13N)
// Locked 14:	 (13R and 14N) or 15N or (18R and 14N) or (19R and 14N)
// Locked 15:	 (13R and 15N) or (14R and 15R) or 18R or 19R
// Locked 16:	 (1R and 16N) or (2R and 16N) or 13R or 14R or 17R or (18R and 16N) or (19R and 16R) or (20R and 16N) or (21R and 16N)
// Locked 17:	 (19R and 17R) or (20R and 17R) or (21R and 17R)
// Locked 18:	 (13R and 18N) or (14R and 18N) or (16R and 18N)
// Locked 19:	 (3R and 19N) or (13R and 19N) or (14R and 19N) or 16N or 17N
// Locked 20:	 (1R and 20N) or (2R and 20N) or (3R and 20N) or (4R and 20N) or (16R and 20N) or 17N or (21R and 20N)
// Locked 21:	 (1R and 21N) or (2R and 21N) or (3R and 21N) or (4R and 21N) or (16R and 21N) or (20R and 21N) or 17N
// Locked 22:	Never
// Locked 23:	Never
// Locked 24:	Never

// Reverse polish logic for locking
// 0xFF = and, 0x7F = or, 0x00 marks the end of the equation
// 1-120 = lever normal, add 128 for Reversed.
BYTE * rom locking[25] = {
    "", // 0
    "\x82\x01\xFF\x84\x01\xFF\x7F\x90\x01\xFF\x7F\x94\x01\xFF\x7F\x95\x01\xFF\x7F", // 1
    "\x81\x02\xFF\x84\x02\xFF\x7F\x90\x02\xFF\x7F\x94\x02\xFF\x7F\x95\x02\xFF\x7F", // 2
    "\x85\x03\xFF\x84\xFF\x93\x03\xFF\x7F\x94\x03\xFF\x7F\x95\x03\xFF\x7F", // 3
    "\x81\x04\xFF\x82\x04\xFF\x7F\x83\x7F\x85\x7F\x94\x04\xFF\x7F\x95\x04\xFF\x7F", // 4
    "\x83\x05\xFF\x84\xFF", // 5
    "", // 6
    "", // 7
    "", // 8
    "", // 9
    "", // 10
    "", // 11
    "", // 12
    "\x8E\x0D\xFF\x8F\x0D\xFF\x7F\x92\x0D\xFF\x7F\x93\x0D\xFF\x7F", // 13
    "\x8D\x0E\xFF\x0F\x7F\x92\x0E\xFF\x7F\x93\x0E\xFF\x7F", // 14
    "\x8D\x0F\xFF\x8E\x8F\xFF\x7F\x92\x7F\x93\x7F", // 15
    "\x81\x10\xFF\x82\x10\xFF\x7F\x8D\x7F\x8E\x7F\x91\x7F\x92\x10\xFF\x7F\x93\x90\xFF\x7F\x94\x10\xFF\x7F\x95\x10\xFF\x7F", // 16
    "\x93\x91\xFF\x94\x91\xFF\x7F\x95\x91\xFF\x7F", // 17
    "\x8D\x12\xFF\x8E\x12\xFF\x7F\x90\x12\xFF\x7F", // 18
    "\x83\x13\xFF\x8D\x13\xFF\x7F\x8E\x13\xFF\x7F\x10\x7F\x11\x7F", // 19
    "\x81\x14\xFF\x82\x14\xFF\x7F\x83\x14\xFF\x7F\x84\x14\xFF\x7F\x90\x14\xFF\x7F\x11\x7F\x95\x14\xFF\x7F", // 20
    "\x81\x15\xFF\x82\x15\xFF\x7F\x83\x15\xFF\x7F\x84\x15\xFF\x7F\x90\x15\xFF\x7F\x94\x15\xFF\x7F\x11\x7F", // 21
    "", // 22
    "", // 23
    "", // 24
};
