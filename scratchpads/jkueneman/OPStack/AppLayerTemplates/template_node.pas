unit template_node;

// This file contains application layer variabile the user can change to customize
// features of OPStack including number of Nodes implemented by the library,
// Hardcoded Events for physical node, etc

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  opstacktypes;


// Enter the number of nodes this build will emulate, must be at least 1, if more than
// one then SUPPORT_VIRTUAL_NODES must be enabled in the Options.inc file
const
  USER_MAX_NODE_COUNT = 10;

// Calculate the number of bytes needed for internal OPStack structures for Events
// Do this by selecting the Largest number of Events either Consumed or Produced in
// either the Node or Virtual Nodes.  When you have that number divide it by 4 and round
// UP to the nearest integer value
//
// For this Template Example we have 1 Consumed and 1 Produced event for both the
// physical node and the virtual nodes so the maximum number of events is 1 so:
//  1/4 = 0.25 which rounds up to 1
const
  USER_MAX_EVENTS_BYTES = 1;

// Calculate the number of bytes needed for internal OPStack structures for Events
// Do this by selecting the Largest number of Events either Consumed or Produced in
// either the Node or Virtual Nodes.  When you have that number divide it by 8 and round
// UP to the nearest integer value
//
// For this Template Example we have 1 Consumed and 1 Produced event for both the
// physical node and the virtual nodes so the maximum number of events is 1 so:
//  1/8 = 0.125 which rounds up to 1
const
  USER_MAX_PCER_BYTES   = 1;

// Set the number of Events that are Consumed by this Node
{$IFDEF SUPPORT_AT_LEAST_ONE_CONSUMED_EVENT}
const
  USER_MAX_SUPPORTED_EVENTS_CONSUMED = 1;
{$ELSE}
const
  USER_MAX_SUPPORTED_EVENTS_CONSUMED = 0;
{$ENDIF}

// Define the UIDs of Events that are Consumed by this Node
{$IFDEF SUPPORT_AT_LEAST_ONE_CONSUMED_EVENT}
const
  SUPPORTED_EVENTS_CONSUMED: array[0..USER_MAX_SUPPORTED_EVENTS_CONSUMED-1] of TEventID = (
    ($01, $01, $00, $00, $00, $00, $FF, $FF)                                    // EVENT_EMERGENCY_STOP
  );
{$ENDIF}


// Set the number of Events that are Produced by this Node
{$IFDEF SUPPORT_AT_LEAST_ONE_PRODUCED_EVENT}
const
  USER_MAX_SUPPORTED_EVENTS_PRODUCED = 1;
{$ELSE}
const
  USER_MAX_SUPPORTED_EVENTS_PRODUCED = 0;
{$ENDIF}

// Define the UIDs of Events that are Produced by this Node
  {$IFDEF SUPPORT_AT_LEAST_ONE_PRODUCED_EVENT}
  const
    SUPPORTED_EVENTS_PRODUCED: array[0..USER_MAX_SUPPORTED_EVENTS_PRODUCED-1] of TEventID = (
      ($01, $01, $00, $00, $00, $00, $FF, $FF)                                    // EVENT_EMERGENCY_STOP
    );
  {$ENDIF}

implementation

end.
