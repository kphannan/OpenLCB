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

#pragma udata ovrly
#pragma romdata

//*********************************************************************************
//        EEPROM read and write
//*********************************************************************************

BYTE EEPROMRead(unsigned int adr)
{        
    EECON1bits.EEPGD = 0;    // Configure access to EEPROM
    EECON1bits.CFGS = 0;     // Configure access to EEPROM
#ifndef __18F2480
#ifndef __18F2580
#ifndef __18F4580
    EEADRH = HI(adr);
#endif
#endif
#endif
    EEADR = LO(adr);         // Load address
    EECON1bits.RD = 1;       // Do the read
    return EEDATA;           // Return the data
}

#define EEPROMWriting EECON1bits.WR

void EEPROMWrite(unsigned int adr, BYTE data)
{
    far overlay BYTE intsave;

    // while (EEPROMWriting) ;  // wait for write complete
    EECON1bits.EEPGD = 0;    // Configure access to EEPROM
    EECON1bits.CFGS = 0;     // Configure access to EEPROM
#ifndef __18F2480
#ifndef __18F2580
#ifndef __18F4580
    EEADRH = HI(adr);
#endif
#endif
#endif
    EEADR = LO(adr);         // Load address
    EEDATA = data;           // load data
    EECON1bits.WREN = 1;     // Enable writes
    intsave = INTCON;
    INTCONbits.GIEH = 0;     // disable all high priority interrupts          
    INTCONbits.GIEL = 0;     // disable all low priority interrupts
    EECON2 = 0x55;           // Required to start write cycle
    EECON2 = 0xAA;           // Required to start write cycle
    EECON1bits.WR = 1;       // Required to start write cycle
    INTCON = intsave;        // restore interrupts          
    EECON1bits.WREN = 0;     // Disable writes
    while (EEPROMWriting) ;  // wait for write complete
}

//*********************************************************************************

