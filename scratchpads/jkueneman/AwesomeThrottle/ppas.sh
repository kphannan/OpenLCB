#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Assembling awesomethrottlemain
/usr/bin/as -o /Users/jimkueneman/Documents/Lazarus/OpenLCB/AwesomeThrottle/lib/i386-darwin/awesomethrottlemain.o /Users/jimkueneman/Documents/Lazarus/OpenLCB/AwesomeThrottle/lib/i386-darwin/awesomethrottlemain.s -arch i386
if [ $? != 0 ]; then DoExitAsm awesomethrottlemain; fi
rm /Users/jimkueneman/Documents/Lazarus/OpenLCB/AwesomeThrottle/lib/i386-darwin/awesomethrottlemain.s
echo Assembling awesomethrottle
/usr/bin/as -o /Users/jimkueneman/Documents/Lazarus/OpenLCB/AwesomeThrottle/lib/i386-darwin/AwesomeThrottle.o /Users/jimkueneman/Documents/Lazarus/OpenLCB/AwesomeThrottle/lib/i386-darwin/AwesomeThrottle.s -arch i386
if [ $? != 0 ]; then DoExitAsm awesomethrottle; fi
rm /Users/jimkueneman/Documents/Lazarus/OpenLCB/AwesomeThrottle/lib/i386-darwin/AwesomeThrottle.s
echo Linking AwesomeThrottle
OFS=$IFS
IFS="
"
/usr/bin/ld /Developer/SDKs/MacOSX10.5.sdk//usr/lib/crt1.o  -framework Carbon -framework OpenGL -dylib_file /System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib:/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib -macosx_version_min 10.5     -multiply_defined suppress -L. -o AwesomeThrottle `cat link.res`
if [ $? != 0 ]; then DoExitLink AwesomeThrottle; fi
IFS=$OFS
