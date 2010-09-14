Directory created by Alex Shepherd.

Well after hacking the Arduino IDE to add the magic -std=gnu99 option, 
I can now cleanly compile the libcan library as an Arduino library and 
I converted my CAN2USB adaptor code into an Arduino sketch and it seems to work!

I've committed all the code into SVN under:

http://nmranet.ajsystems.co.nz/svn/nmranet/proposals/v1/prototypes/Arduino/

I've been using the latest Arduino-0013 version - yeah I know David I 
changed my mind in the hope the -std=gnu99 might have been related 
to the WinAVR version, but alas no...

My <Arduino-install-dir> is: C:\Program Files\Arduino\arduino-0013

To get a working environment using Arduino 0013 you need to do the following:

1) Copy the lib\pde.jar file into your <Arduino-install-dir>\lib directory 
to replace the existing file. This added the -std=gnu99 CFLAGS option.

2) Copy the libraries\CAN folder into 
your <Arduino-install-dir>\hardware\libraries directory.

3) Copy the CAN2USBuino folder under sketches\CAN2USBuino into your 
Arduino sketches dir. This is the main sketch.

Then you should be able to launch the Arduino IDE, load the CAN2USBuino 
sketch, configure the serial port for the LEDuino USB Virtual Comm Port 
in the IDE, configure the board for Arduino Diecimila and then click the 
Upload icon to compile and upload the code, which hopefully works.

(This doesn't seem to be needed anymore with Arduino 16 - Bob J)

-----------------------

Note that the Arduino environment doesn't rebuild out-of-date *.o files
in the library directory. If the .o file exists, it's considered to be 
fine.  If you update source code in the libraries directory or subdirectories,
you should delete all the *.o files so that Arduino will rebuild it.

-----------------------
On Linux/Mac, you can use symbolic links to make directories (libraries
and sketches) available to the Arduino IDE, while still keeping the code
under the control of SVN.  Do something like (your pathnames _will_ differ):

# change to your Arduino home directory for sketches
cd ~/Arduino
# make a symbolic link to the SVN-controlled sketch directory
ln -s ~/svn/proposal/svn/proposals/v1/prototypes/Arduino/sketches/CanMrrlcbTest CanMrrlcbTest

# change to Arduino application library directory
cd /Applications/arduino-0013/hardware/libraries/
ln -s ~/svn/proposal/svn/proposals/v1/prototypes/Arduino/libraries/CAN CAN

