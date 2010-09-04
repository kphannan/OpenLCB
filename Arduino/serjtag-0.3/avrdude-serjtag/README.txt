-------------------------------------
HOW-TO-INSTALL

(0) Install WinAVR

(1) replace WinAVR's avrdude.exe and avrdude.conf

    example: (MSYS)
       INSTALLDIR=/d/WinAVR/bin
       pushd $INSTALLDIR
       mv avrdude.exe avrdude-org.exe
       mv avrdude.conf avrdude-org.conf
       popd
       cd binary
       cp avrdude.exe avrdude.conf $INSTALLDIR

-------------------------------------
HOW-TO-USE

example: (Linux)
avrdude -pt44 -c serjtag -P /dev/ttyUSB0 -U flash:w:usb910j44.hex -B 4000000

example: (Windows)
avrdude -pt44 -c serjtag -P COM4 -U flash:w:usb910j44.hex -B 4000000

 -c <programmer>    
      serjtag : Serial JTAG protocol

      The SERJTAG protocol's main function is to send and recv SPI bit-stream.
      Therefore the AVRDUDE can support the full function.
      But this require the high bandwidth:
         32 bits send each 1 byte write
         32 bits send/recv each 1 byte read
      
 -B <bitclock> 
        4000000 MAX SPEED 
         250000 default --- for 1MHz (8MHz + 1/8 fuse bit conf) CPU CLOCK 

-------------------------------------
HOW-TO-BUILD: (for Windows (MinGW+MSYS))

(0) Install MinGW and MSYS 

(0-2) Download and Install ftdi VCP driver (CDM X.XX.X.zip) 
        cp FTD2XX.H /mingw/include/ftd2xx.h
        cp FTD2XX.lib /mingw/lib/ftd2xx.lib
        cp FTD2XX.dll /mingw/bin/ftd2xx.dll

(1) Download and extract avrdude-5.3.1.tar.gz
        Download from
            http://savannah.nongnu.org/projects/avrdude/
        tar -zxvf avrdude-5.3.1.tar.gz

(2) Patch all 
        cd avrdude-5.3.1
	patch -p1 < avrdude-5.3.1-usb910.patch
	patch -p1 < avrdude-5.3.1-avr910d.patch
	patch -p1 < avrdude-5.3.1-serjtag.patch
	patch -p1 < avrdude-5.3.1-ft245r.patch
	patch -p1 < avrdude-5.3.1-baud.patch

(3) Configure
	./configure

(4) Edit Makefile

    LIBS= -lhid -lserupapi -lftd2xx

(5) Make
       make

-------------------------------------
HOW-TO-BUILD: (for Linux RPM, no ft245r driver)

(1) Download avrdude-5.3.1
        Download from
            http://savannah.nongnu.org/projects/avrdude/

(2) cp avrdude-5.3.1.tar.gz avrdude-5.3.1-usb910.patch \
     avrdude-5.3.1-avr910d.patch avrdude-5.3.1-serjtag.patch \
     avrdude-5.3.1-baud.patch  avrdude-5.3.1-ft245r.patch \
         /usr/src/redhat/SOURCES

(3) cp avrdude.spec /usr/src/redhat/SPECS

(4) cd /usr/src/redhat/SPECS
      rpmbuild -bb avrdude.spec
      rpmbuild -bs avrdude.spec ( for Source RPM)







