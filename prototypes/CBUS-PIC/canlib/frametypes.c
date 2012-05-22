/*  OpenLCB frametypes.c

    9 Dec 2009

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
    FT_RID     = 0x0700,   // Reserved ID
    FT_AMD     = 0x0701,   // Alias Map Definition
    FT_AME     = 0x0702,   // Alias Mapping Enquiry
    FT_AMR     = 0x0703,   // Alias Map Reset
    FT_CIM3    = 0x4000,   // lowest 12 bits of NID
    FT_CIM2    = 0x5000,   // 3rd top 12 bits of NID
    FT_CIM1    = 0x6000,   // 2nd top 12 bits of NID
    FT_CIM0    = 0x7000,   // Top 12 bits of NID
    FT_XPRESSNET = 0x8517, // XpressNet raw message from a command station
    FT_VNSN    = 0x88A7,   // Verify Node Serial Number 
    FT_IDCONS  = 0x8A4F,   // Identify Consumers
    FT_IDPROD  = 0x8A8F,   // Identify Producers
    FT_IDEVNTS = 0x8AB7,   // Identify Events
    FT_EVENT   = 0x8ADF,   // EVENT
    FT_INIT    = 0x8087,   // Initialization Complete
    FT_NSN     = 0x88B7,   // Node Serial Number 
    FT_CONSIR  = 0x825F,   // Consumer Identify Range
    FT_CONSID  = 0x826B,   // Consumer Identified
    FT_PRODIR  = 0x829F,   // Producer Identify Range
    FT_PRODID  = 0x82AB,   // Producer Identified
    FT_DGS     = 0xA000,   // Datagram single packet
    FT_DGF     = 0xB000,   // Datagram first packet
    FT_DGM     = 0xC000,   // Datagram middle packets
    FT_DGL     = 0xD000,   // Datagram last packet
    FT_ADDR    = 0xE000,   // Addressed packet
    FT_STREAM  = 0xF000    // Stream data
};

// Protocol id, 1st byte of datagram
enum DA {
    DA_VNSN     = 0x0A,
    DA_OIR      = 0x0C,
    DA_TDE      = 0x0D,
    DA_IDEVNT   = 0x2B,
    DA_PSI      = 0x2E,
    DA_PSR      = 0x2F,
    DA_DG       = 0x40,
    DA_OK       = 0x4C,
    DA_ERR      = 0x4D,
    DA_SIQ      = 0x4E,
    DA_SIR      = 0x4F,
    DA_STR      = 0x69,
    DA_SDP      = 0x6A,
    DA_SRC      = 0x6B,
};

// Datagram protocol id, 1st byte of datagram
enum DG {
    DG_LOGMSG   = 0x01,
    DG_LOGREPLY = 0x02,
    DG_MEMORY   = 0x20,
    DG_REMOTE   = 0x21,
    DG_DISPLAY  = 0x28,
};

// Memory transfer datagram, protocol id = 0x20
enum DGM {
    DGM_WRITE  = 0x20,
    DGM_REPLY  = 0x30,
    DGM_READ   = 0x60,
    DGM_UPDCOMP= 0xA4,
    DGM_REBOOT = 0xA5,
    DGM_FACTORY= 0xA6,
    DGM_LOADER = 0xA7,
};

//*********************************************************************************


