// *****************************************************************************
//
// This file is the template for the application to define memory management by
// defining the number of buffers in the application.  The number of buffers will
// vary depending on the number of possible Nodes that will be allocated at the
// same time.
//
// *****************************************************************************

unit template_buffers;


{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

// Enter the number of buffers to allocate for CAN data, Datagrams, and Streams.  These
// buffers are created in RAM so the more you creat the less free RAM you will have
const
  USER_MAX_SIMPLE_ARRAY_BUFFERS = 20;
  USER_MAX_DATAGRAM_ARRAY_BUFFERS = 100;
  USER_MAX_STREAM_ARRAY_BUFFERS = 100;
  USER_MAX_MULTIFRAME_STRING_ARRAY_BUFFERS = 100;
  USER_MAX_MULTIFRAME_ARRAY_BUFFERS = 100;

// Enter the size of the buffer to hold Messages, this should be at LEAST equal to the number
// of Nodes, 2x is recommended
const
  USER_MAX_MESSAGE_ARRAY_BUFFERS = 20;

// Enter the size of a stream.  Note Ethernet is limited to about 1500, if using
// the MicroElectronica TCP libraries this MUST be a power of 2 (2, 4, ..., 256, 512, 1024, 2048...)
//const
  USER_MAX_STREAM_BYTES = 1032;                  // 1024 Databytes + 2 TNode ID structures (2*4) = 1032

// Enter the size of a buffer to hold a complete ACDI or SNIP string
const
  USER_MAX_MULTIFRAME_STRING_BYTES = 256;

{$IFDEF SUPPORT_TRACTION}
// Enter the size of a buffer to hold a complete Simple Train Node Information string
const
  USER_MAX_STNIP_BYTES = 256;
{$ENDIF}

{$IFDEF SUPPORT_TRACTION_PROXY}
const
  USER_MAX_TRAIN_PROXY_BUFFERS = 10;
{$ENDIF}

implementation

end.
