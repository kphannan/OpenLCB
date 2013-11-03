unit template_node;

// This file contains application layer variabile the user can change to customize
// features of OPStack including number of Nodes implemented by the library,
// Hardcoded Events for physical node, etc

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  nmranetdefines,
  opstacktypes;


// Enter the number of nodes this build will emulate, must be at least 1, if more than
// one then SUPPORT_VIRTUAL_NODES must be enabled in the Options.inc file
const
  USER_MAX_NODE_COUNT = 10;

// Calculate the number of bytes needed for internal OPStack structures for Events
// Do this by selecting the Largest number of Events either Consumed or Produced in
// either the Node or Virtual Nodes. This includes the dynamic Events.  When you have that
//number divide it by 4 and round UP to the nearest integer value
//
// For this Template Example we have 1 Consumed and 1 Produced event for both the
// physical node and the virtual nodes so the maximum number of events is 1 so:
//  1/4 = 0.25 which rounds up to 1
const
  USER_MAX_EVENTS_BYTES = 1;

// Calculate the number of bytes needed for internal OPStack structures for Events
// Do this by selecting the Largest number of Events either Consumed or Produced in
// either the Node or Virtual Nodes.  This includes the dynamic Events.  When you
// have that number divide it by 8 and round UP to the nearest integer value
//
// For this Template Example we have 1 Consumed and 1 Produced event for both the
// physical node and the virtual nodes so the maximum number of events is 1 so:
//  1/8 = 0.125 which rounds up to 1
const
  USER_MAX_PCER_BYTES   = 1;

// Set the number of Events that are Consumed by this Node, if none then remove the SUPPORT_AT_LEAST_ONE_CONSUMED_EVENT
//  conditional define from the Options.inc file
{$IFDEF SUPPORT_AT_LEAST_ONE_CONSUMED_EVENT}
const
  USER_MAX_SUPPORTED_EVENTS_CONSUMED = 1;
{$ELSE}
const
  USER_MAX_SUPPORTED_EVENTS_CONSUMED = 0;
{$ENDIF}

// Dynamic events are events whos IDs are not known at compile time. When enabled
// the library will callback on the AppCallback_DynamicEvent_Consumed with and index from
// 0 to USER_MAX_SUPPORTED_DYNAMIC_EVENTS_CONSUMED - 1.
const
  USER_MAX_SUPPORTED_DYNAMIC_EVENTS_CONSUMED = 0;


// Define the UIDs of Events that are Consumed by this Node, if none then remove the SUPPORT_AT_LEAST_ONE_CONSUMED_EVENT
//  conditional define from the Options.inc file
{$IFDEF SUPPORT_AT_LEAST_ONE_CONSUMED_EVENT}
const
  USER_SUPPORTED_EVENTS_CONSUMED: array[0..USER_MAX_SUPPORTED_EVENTS_CONSUMED-1] of TEventID = (
    ($01, $01, $00, $00, $00, $00, $FF, $FF)                                    // EVENT_EMERGENCY_STOP
  );
{$ENDIF}


// Set the number of Events that are Produced by this Node, if none then remove the SUPPORT_AT_LEAST_ONE_PRODUCED_EVENT
//  conditional define from the Options.inc file
{$IFDEF SUPPORT_AT_LEAST_ONE_PRODUCED_EVENT}
const
  USER_MAX_SUPPORTED_EVENTS_PRODUCED = 1;
{$ELSE}
const
  USER_MAX_SUPPORTED_EVENTS_PRODUCED = 0;
{$ENDIF}

// Dynamic events are events whos IDs are not known at compile time. When enabled
// the library will callback on the AppCallback_DynamicEvent_Produced with and index from
// 0 to USER_MAX_SUPPORTED_DYNAMIC_EVENTS_PRODUCED - 1.
const
  USER_MAX_SUPPORTED_DYNAMIC_EVENTS_PRODUCED = 0;

// Define the UIDs of Events that are Produced by this Node, if none then remove the SUPPORT_AT_LEAST_ONE_PRODUCED_EVENT
//  conditional define from the Options.inc file
  {$IFDEF SUPPORT_AT_LEAST_ONE_PRODUCED_EVENT}
  const
    USER_SUPPORTED_EVENTS_PRODUCED: array[0..USER_MAX_SUPPORTED_EVENTS_PRODUCED-1] of TEventID = (
      ($01, $01, $00, $00, $00, $00, $FF, $FF)                                    // EVENT_EMERGENCY_STOP
    );
  {$ENDIF}


const
  PIV_SUPPORTED_PROTOCOL_COUNT = 7;    // UPDATE THIS IF ADDING OR SUBTRACTING SUPPORTED PROTOCOLS
  PIV_SUPPORTED_PROTOCOLS: array[0..PIV_SUPPORTED_PROTOCOL_COUNT-1] of TPIVProtocolValueArray = (     // Look at the PIV_xxxx constants for more Protocols
    ($80, $00, $00, $00, $00, $00),                                             // Protocol
    ($40, $00, $00, $00, $00, $00),                                             // Datagram Protocol
    ($04, $00, $00, $00, $00, $00),                                             // Producer Consumer Protocol
    ($10, $00, $00, $00, $00, $00),                                             // Memory Configruation Protocol
    ($00, $08, $00, $00, $00, $00),                                             // CDI Protocol
    ($00, $10, $00, $00, $00, $00),                                             // SNIP Protocol
    ($00, $40, $00, $00, $00, $00)                                              // ACDI Protocol
    );

implementation

end.
