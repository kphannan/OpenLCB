unit opstacktypes;

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

type
  TEventID = array[0..7] of Byte;                            // Is the 48 Bit node ID + 16 Bits of unique Event ID = 64 Bits
  PEventID = ^TEventID;

  PByte = ^Byte;

const
  OPS_PROCESSING     = $01;                                                     // Start and stops the main StateMachine for running. Default is Off on startup Allows holding off OLCB processing until the node is read (Ethernet link is up, etc)

type
  TOPStack = record
    State: Word;                                                                // The State of the OPStack Library, see the OPS_xxx flags
  end;

implementation

end.

