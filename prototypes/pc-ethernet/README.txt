
I added my Ethernet software to the prototypes directory. It uses CAN 
mti's for the Ethernet mti's because the existing Ethernet mti cannot be
simply converted from the CAN mti's and vice versa.

Olcbsvr - the TCP hub and nodeid server. This allows up to 16 Ethernet 
connections and allocates a nodenumber to each connection, which the 
connection can ignore. No loop detection.

ComGateway - a serial connection to a CAN segment, since there is no 
loop detection opening 2 of them to the same segments is not a good 
idea. Connection to different CAN segments should be OK. It filters out 
InitComplete and VerifiedNodeID packets, but nothing else.

Config - the configuration program, reads the XML from space FF and uses
that to read and display the node configuration and also write back changes.

LenzSvr - A WiThrottle server to Xpressnet over OpenLCB. Allows an ipod 
touch to be used as a throttle.

Throttle - A simple speed control for Xpressnet.



The key part is the Ethernet server/hub. This uses Apple bonjour to
announce its server host name and port number.
It connect Ethernet nodes together, Com port Gateways, PC software
(config tool, throttles, control panels etc), or real Ethernet
gateways using TCP/IP.
This means that instead of only one PC program using a USB/CAN
connector, the Com port gateway and server/hub allows many programs to
connect.
Also PC software needs a node number, so the server allocates that as well.

The Com gateway, interfaces to the usb or rs232 can connector and the
Ethernet hub. It connect via bonjour to the server/hub. It handles
getting can aliases for the PC software. Today I had to change the
getting of aliases because I found that some message types had
changed.

The config program,  connects via Ethernet to the hub using bonjour.
It expects every node to contain an XML file, describing the
configuration data.
So far it only supports byte, char and int data. The XML structure is
as in the basic spec document.

Withrottle server and throttle are 2 PC programs, that talk to my
XpressNet command station adapter over OpenLCB.

Most of the PIC code has now been converted, but not fully tested.

