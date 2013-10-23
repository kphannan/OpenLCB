unit template_buffers;

// This file contains application layer variabile the user can change to customize
// features of OPStack including buffer memory management, etc.

interface

// Enter the number of buffers to allocate for CAN data, Datagrams, and Streams.  These
// buffers are created in RAM so the more you creat the less free RAM you will have
const
  USER_MAX_CAN_ARRAY_BUFFERS = 10;
  USER_MAX_DATAGRAM_ARRAY_BUFFERS = 4;
  USER_MAX_STREAM_ARRAY_BUFFERS = 2;

// Enter the size of a stream.  Note Ethernet is limited to about 1500, if using
// the MicroElectronica TCP libraries this MUST be a power of 2 (2, 4, ..., 256, 512, 1024, 2048...)
const
  USER_MAX_STREAM_BYTES = 1024;


implementation

end.

