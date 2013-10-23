unit template_vnode;

// This file contains application layer variabile the user can change to customize
// features of OPStack including number of Nodes implemented by the library,
// Hardcoded Events for physical node, etc



{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  opstacktypes;

// Set the number of Events that are Consumed by this Node
{$IFDEF SUPPORT_AT_LEAST_ONE_CONSUMED_EVENT}
const
  USER_MAX_VNODE_SUPPORTED_EVENTS_CONSUMED = 1;
{$ELSE}
const
  USER_MAX_VNODE_SUPPORTED_EVENTS_CONSUMED = 0;
{$ENDIF}

// Define the UIDs of Events that are Consumed by this Node
{$IFDEF SUPPORT_AT_LEAST_ONE_CONSUMED_EVENT}
const
  SUPPORTED_VNODE_EVENTS_CONSUMED: array[0..USER_MAX_VNODE_SUPPORTED_EVENTS_CONSUMED-1] of TEventID = (
    ($01, $01, $00, $00, $00, $00, $FF, $FF)                                    // EVENT_EMERGENCY_STOP
  );
{$ENDIF}


// Set the number of Events that are Produced by this Node
{$IFDEF SUPPORT_AT_LEAST_ONE_PRODUCED_EVENT}
const
  MAX_VNODE_SUPPORTED_EVENTS_PRODUCED = 1;
{$ELSE}
const
  MAX_VNODE_SUPPORTED_EVENTS_PRODUCED = 0;
{$ENDIF}

// Define the UIDs of Events that are Produced by this Node
  {$IFDEF SUPPORT_AT_LEAST_ONE_PRODUCED_EVENT}
  const
    SUPPORTED_VNODE_EVENTS_PRODUCED: array[0..MAX_VNODE_SUPPORTED_EVENTS_PRODUCED-1] of TEventID = (
      ($01, $01, $00, $00, $00, $00, $FF, $FF)                                    // EVENT_EMERGENCY_STOP
    );
  {$ENDIF}

implementation

end.
