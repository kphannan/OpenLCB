unit opstacktypes;

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

type
  TEventID = array[0..7] of Byte;                            // Is the 48 Bit node ID + 16 Bits of unique Event ID = 64 Bits
  PEventID = ^TEventID;

  PByte = ^Byte;

implementation

end.

