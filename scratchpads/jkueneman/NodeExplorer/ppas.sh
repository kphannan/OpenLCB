#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Assembling nodevalidate
/usr/bin/as -o /Users/jimkueneman/Documents/Lazarus/Projects/OpenLCB_ATP/lib/i386-darwin/NodeValidate.o /Users/jimkueneman/Documents/Lazarus/Projects/OpenLCB_ATP/lib/i386-darwin/NodeValidate.s -arch i386
if [ $? != 0 ]; then DoExitAsm nodevalidate; fi
rm /Users/jimkueneman/Documents/Lazarus/Projects/OpenLCB_ATP/lib/i386-darwin/NodeValidate.s
echo Linking NodeValidate
OFS=$IFS
IFS="
"
/usr/bin/ld /Developer/SDKs/MacOSX10.5.sdk//usr/lib/crt1.o  -framework Carbon -framework OpenGL -dylib_file /System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib:/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib -macosx_version_min 10.5     -multiply_defined suppress -L. -o NodeValidate `cat link.res` -pagezero_size 0x10000
if [ $? != 0 ]; then DoExitLink NodeValidate; fi
IFS=$OFS
