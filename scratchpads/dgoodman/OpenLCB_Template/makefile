#/*Copyright (C) 2011 by Sagar G V
#
#Permission is hereby granted, free of charge, to any person obtaining a copy
#of this software and associated documentation files (the "Software"), to deal
#in the Software without restriction, including without limitation the rights
#to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#copies of the Software, and to permit persons to whom the Software is
#furnished to do so, subject to the following conditions:
#
#The above copyright notice and this permission notice shall be included in
#all copies or substantial portions of the Software.
#
#THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
#THE SOFTWARE.*/


#  Project Name
PROJECT=main
#  List of the objects files to be compiled/assembled
OBJECTS = $(CSRCS:.c=.o)
OBJECTS += $(ASRCS:.S=.o)
OBJECTS += $(CPPSRCS:.cpp=.o)

LDOBJECTS = $(wildcard *.a)
LDOBJECTS += $(wildcard lib/*.a)
LDOBJECTS += $(wildcard lib/*/*.a)
LDOBJECTS += $(wildcard lib/*.o)
LDOBJECTS += $(wildcard lib/*/*.o)

LSCRIPT = makesection/LPC17xx.ld
LSCRIPT2 = makesection/LPC17xx_For_NXP_USB_Bootloader.ld

CSRCS = $(wildcard *.c)
CSRCS += $(wildcard src/*.c)
CSRCS += $(wildcard src/*/*.c)
CSRCS += $(wildcard Core/CM3/*.c)
CSRCS += $(wildcard Drivers/source/*.c)
CSRCS += $(wildcard OpenLCB/source/*.c)

ASRCS =  $(wildcard src/*.S)
ASRCS +=  $(wildcard src/*/*.S)

CPPSRCS = $(wildcard *.cpp)
CPPSRCS += $(wildcard src/*.cpp)
CPPSRCS += $(wildcard src/*/*.cpp)
CPPSRCS += $(wildcard OpenLCB/source/*.cpp)

INCDIRS = ./
INCDIRS += $(wildcard inc/*)
INCDIRS += $(wildcard Core/CM3/)
INCDIRS += $(wildcard Drivers/include/)
INCDIRS += $(wildcard OpenLCB/include/)

OPTIMIZATION = 2

#  Compiler Options
GCFLAGS = -O$(OPTIMIZATION) -gdwarf-2 -mcpu=cortex-m3 -mthumb -mthumb-interwork -mlong-calls -ffunction-sections -fdata-sections -fpromote-loop-indices -Wall -Wextra -Wimplicit -Wcast-align -Wpointer-arith -Wredundant-decls -Wshadow -Wcast-qual -Wcast-align -Wnested-externs --std=gnu99 

#GCFLAGS = -W -Wall -fno-common -O$(OPTIMIZATION) --std=gnu99 -fgnu89-inline -mcpu=cortex-m3 -mthumb -ffunction-sections -fdata-sections -g3
GCFLAGS += $(patsubst %,-I%,$(INCDIRS)) -I.
#GCFLAGS += -W -Wall -mcpu=cortex-m3 --std=gnu99 -fgnu89-inline -mthumb -O$(OPTIMIZATION)
#GCFLAGS += -Wcast-align -Wcast-qual -Wimplicit -Wpointer-arith -Wswitch
#GCFLAGS += -Wredundant-decls -Wreturn-type -Wshadow -Wunused
#GCFLAGS  += -W --std=gnu99 -fgnu89-inline
#GCFLAGS  += -ffunction-sections -fdata-sections
#GPFLAGS = -W -Wall -fno-common -mcpu=cortex-m3 -mthumb -O$(OPTIMIZATION)
GPFLAGS = -O$(OPTIMIZATION) -gdwarf-2 -mcpu=cortex-m3 -mthumb -mthumb-interwork -mlong-calls -ffunction-sections -fdata-sections -fpromote-loop-indices -Wall -Wextra -Wimplicit -Wcast-align -Wpointer-arith -Wredundant-decls -Wshadow -Wcast-qual -Wcast-align -fno-rtti -fno-exceptions

GPFLAGS += $(patsubst %,-I%,$(INCDIRS)) -I.
#LDFLAGS = -Map=$(PROJECT).map -T$(LSCRIPT) --gc-sections
LDFLAGS = -mcpu=cortex-m3 -mthumb -O$(OPTIMIZATION) -Wl,-Map=$(PROJECT).map,--cref,--gc-sections -lc -lm -lgcc -lstdc++  -T$(LSCRIPT)
LDFLAGS2 = -mcpu=cortex-m3 -mthumb -O$(OPTIMIZATION) -Wl,-Map=$(PROJECT).map,--cref,--gc-sections -lc -lm -lgcc -lstdc++  -T$(LSCRIPT2)
#LDFLAGS += --gc-sections
ASFLAGS = $(LISTING) -mcpu=cortex-m3 -mthumb -x assembler-with-cpp
ASFLAGS += $(patsubst %,-I%,$(INCDIRS)) -I.
#  Compiler/Assembler/Linker Paths
GCC = arm-none-eabi-gcc
GPP = arm-none-eabi-g++
AS = arm-none-eabi-gcc
LD = arm-none-eabi-g++
OBJCOPY = arm-none-eabi-objcopy
REMOVE = rm -f
SIZE = arm-none-eabi-size

#########################################################################

all:: $(PROJECT).hex $(PROJECT).bin $(PROJECT)_For_NXP_USB_Bootloader.bin

$(PROJECT).bin: $(PROJECT).elf
	$(OBJCOPY) -O binary -j .text -j .data $(PROJECT).elf $(PROJECT).bin

$(PROJECT)_For_NXP_USB_Bootloader.bin: $(PROJECT)_For_NXP_USB_Bootloader.elf
	$(OBJCOPY) -O binary -j .text -j .data $(PROJECT)_For_NXP_USB_Bootloader.elf $(PROJECT)_For_NXP_USB_Bootloader.bin

$(PROJECT).hex: $(PROJECT).elf
	$(OBJCOPY) -R .stack -O ihex $(PROJECT).elf $(PROJECT).hex
	
$(PROJECT).elf: $(LSCRIPT) $(OBJECTS)
	$(LD) $(LDFLAGS) $(LDOBJECTS) $(OBJECTS) -o $(PROJECT).elf
	$(SIZE) $(PROJECT).elf

$(PROJECT)_For_NXP_USB_Bootloader.elf: $(LSCRIPT2) $(OBJECTS)
	$(LD) $(LDFLAGS2) $(LDOBJECTS) $(OBJECTS) -o $(PROJECT)_For_NXP_USB_Bootloader.elf

stats: $(PROJECT).elf
	$(SIZE) $(PROJECT).elf

clean:
	$(REMOVE) $(OBJECTS)
	$(REMOVE) $(PROJECT).hex
	$(REMOVE) $(PROJECT).elf
	$(REMOVE) $(PROJECT)_For_NXP_USB_Bootloader.elf
	$(REMOVE) $(PROJECT).map
	$(REMOVE) $(PROJECT).bin
	$(REMOVE) $(PROJECT)_For_NXP_USB_Bootloader.bin
	$(REMOVE) $(OBJECTS)

#########################################################################
#  Default rules to compile .c and .cpp file to .o
#  and assemble .s files to .o

.c.o :
	$(GCC) $(GCFLAGS) -c $< -o $(<:.c=.o)

.cpp.o :
	$(GPP) $(GPFLAGS) -c $< -o $(<:.cpp=.o)

.S.o :
	$(AS) $(ASFLAGS) -c $< -o $(<:.S=.o)
#	$(AS) $(ASFLAGS) -o $(<:.S=.o) $< > $(<:.S=.lst)
#	$(AS) $(ASFLAGS) -o $(PROJECT)_crt.o $< > $(PROJECT)_crt.lst

#########################################################################
