/*
    OpenLCB

    Copyright (C) 2009    Mike Johnson

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

//*********************************************************************************
//        Frame Types
//*********************************************************************************

enum FT
{
    FT_CIM0    = 0x0000,   // Top 12 bits of NID
    FT_CIM1    = 0x1000,   // 2nd top 12 bits of NID
    FT_CIM2    = 0x2000,   // 3rd top 12 bits of NID
    FT_CIM3    = 0x3000,   // lowest 12 bits of NID
    FT_MR      = 0x7FFE,   // Reset mapping, not sure this is actually useful ?
    FT_RIM     = 0x7FFF,   // RIM

// Broadcast type

// Misc
    FT_RESET   = 0x8000,   // System reset
    FT_VNSN    = 0x8001,   // Verify Node Serial Number 
    FT_INIT    = 0x8002,   // Normal Initialization Complete
    FT_BOOT    = 0x8003,   // Boot Loader Initialization Complete

// Accessory
    FT_ACOF    = 0x8010,   // Off
    FT_ACON    = 0x8011,   // On
    FT_ASOF    = 0x8012,   // Short Off
    FT_ASON    = 0x8013,   // Short On
    FT_RFID    = 0x8014,   // RFID tag

// Track commands
    FT_TOF     = 0x8020,   // Track Off, broadcast from CS
    FT_TON     = 0x8021,   // Track On or Normal operation, broadcast from CS
    FT_ESTOP   = 0x8022,   // Track Stopped (em. stop)
    FT_CSRESET = 0x8023,   // Command station Reset
    FT_RTOF    = 0x8024,   // Request Track Off, from CAB
    FT_RTON    = 0x8025,   // Request Track On or Normal operation, from CAB
    FT_RESTP   = 0x8026,   // Request Emergency Stop ALL

// CAB commands
    FT_RLOC    = 0x8030,   // Request loco info
    FT_STMOD   = 0x8031,   // Request speed step change
    FT_DSPD    = 0x8032,   // Set Engine Speed/Dir
    FT_DFUN    = 0x8033,   // Set engine functions
    FT_PLOC    = 0x8034,   // Engine report from CS
    FT_PLOCF   = 0x8035,   // Engine function report from CS
    FT_KLOC    = 0x8036,   // Release loco

// Consist commands
    FT_PCON    = 0x8037,   // Consist Engine
    FT_KCON    = 0x8038,   // Remove engine from consist

// DCC programming
    FT_RDCC3   = 0x8040,   // Request 3 byte DCC packet
    FT_RDCC4   = 0x8041,   // Request 4 byte DCC packet
    FT_RDCC5   = 0x8042,   // Request 5 byte DCC packet
    FT_RDCC6   = 0x8043,   // Request 6 byte DCC packet

    FT_DAA     = 0xE000,   // Destination Alias Addressed, message type in the data
    FT_STREAM  = 0xF000,   // Stream data
};

// Destination Addressed, 1st byte of data has message type
// 0123456789012
// :X1EdddsssN

enum DAA {
    DAA_DATA     = 0x00,      // up to 0F, 7 bytes of data sequence number in low 4 bits
    DAA_ACK      = 0x10,      // ack with status
// Loader
    DAA_UPGSTART = 0x11,      // enter loader
    DAA_UPGRESET = 0x12,      // start program
    DAA_UPGREAD  = 0x13,      // read 64 bytes
    DAA_UPGADDR  = 0x14,      // write 64 bytes
// Events
    DAA_EVERASEH = 0x15,      // erase events, High 4 bytes
    DAA_EVERASEL = 0x16,      // erase events, Low 4 bytes
    DAA_EVREADH  = 0x17,      // read events, High 4 bytes
    DAA_EVREADL  = 0x18,      // read events, Low 4 bytes
    DAA_EVWRITEH = 0x19,      // write event, High 4 bytes
    DAA_EVWRITEL = 0x1A,      // write event, Low 4 bytes
// Node variables
    DAA_NVRD     = 0x1B,      // read
    DAA_NVSET    = 0x1C,      // set
    DAA_NVANS    = 0x1D,      // reply to read
// Misc
    DAA_NSN      = 0x1E       // Node serial number
};

//*********************************************************************************


