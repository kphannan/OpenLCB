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
//    Entry and interrupts
//*********************************************************************************

void main(void);
void hpinterrupt(void);
void lpinterrupt(void);
void __entry (void);
void __hpint(void);

#pragma code __entry = STARTADDRESS

void __entry (void)
{
    _asm
        goto main		// stack pointer and memory cleared by loader
    _endasm
}

// Due to a bug involving 2 cycle instructions and the fast save registers
// with some versions of chip we have to implement this work around
#pragma code hpintvector = HIGHINTADDRESS
void hpintvector(void)
{
    _asm
        call __hpint,1
    _endasm
}

void __hpint(void)
{
    _asm
        pop
        goto hpinterrupt
    _endasm
}

#pragma code lowintvector = LOWINTADDRESS
void lowintvector(void)
{
    _asm
        goto lpinterrupt
    _endasm
}

#pragma code

//*********************************************************************************
