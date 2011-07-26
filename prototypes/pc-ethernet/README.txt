(From an email by Mike Johnson July 16, 2011)

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

So far I have the boot loader and CBUS USB adapter working. I'm slowly
working my way through the PIC code and converting it. So far I have
not uploaded the new PIC code.
