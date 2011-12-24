#!/usr/bin/env python

COMPORT = '/dev/tty.usbserial'
#change to match the actual serial port being used. I just made this one up.
#better yet, create a symlink to the actual serial device with:
# sudo ln -s /dev/tty.usbserial-LOTSOFCHARSANDNUMBERS /dev/tty.usbserial

#require the PySerial library http://pypi.python.org/pypi/pyserial
import serial


#first, read in the next available ID
infile = file("next_nodeid.txt", 'r')
nextID = int(infile.readline().strip())
infile.close()

nid = [5,2,1,2,2,nextID]

if(nextID > 255): # error!
  print("Out of NodeIDs!")
  print(nid)
  exit()

#now, establish a serial connection
connection = serial.Serial(COMPORT, 115200)
new_nid = bytearray(7)
new_nid[0] = 'S'
for x in range(6):
  new_nid[x+1] = nid[x]
connection.write(new_nid)
#verify
verified_nid = connection.read(6)
connection.close()
for x in range(6):
  if(new_nid[x+1] != verified_nid[x]):
    #bail!
    println("Mismatch between sent NID and verified NID!")
    for x in range(6):
      print(int(new_nid[x+1]))
    for x in range(6):
      print(int(nid[x]))
    exit()

#now, if successful, add 1 to next_nodeid.txt
outfile = file("next_nodeid.txt", 'w')
outfile.write(str(nextID+1))
outfile.write("DO NOT MODIFY THIS FILE EXCEPT IF YOU ARE RUNNING allocate_nodeid.py IN A PRODUCTION ENVIRONMENT!")
outfile.close()
