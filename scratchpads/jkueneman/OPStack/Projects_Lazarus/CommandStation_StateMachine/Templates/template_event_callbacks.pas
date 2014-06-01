// *****************************************************************************
//
// This file is the template for the application to define Dynamic Events.  If
// the application does not need dynamic events then this file can be left as is.
// Dynamic events are events that can change value or come and go based on
// the application state.  The Traction Protocol uses events of this type when
// Trains are allocated/deallocated
//
// You must define the number of Dynamic Events in the template_node/template_vnode
// files before these will be called
//
// *****************************************************************************

unit template_event_callbacks;


{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  opstacktypes,
  opstacknode,
  opstackdefines;

procedure AppCallback_InitializeEvents(Node: PNMRAnetNode; EventIndex: Integer; ConsumedProduced: Byte);
procedure AppCallback_InitializeDynamicEvents(Node: PNMRAnetNode; EventIndex: Integer; ConsumedProduced: Byte);

function AppCallback_DynamicConsumedEvent(Node: PNMRAnetNode; EventIndex: Integer; var EventID: TEventID): Boolean;
function AppCallback_DynamicProducedEvent(Node: PNMRAnetNode; EventIndex: Integer; var EventID: TEventID): Boolean;
function AppCallback_DynamicVNodeConsumedEvent(Node: PNMRAnetNode; EventIndex: Integer; var EventID: TEventID): Boolean;
function AppCallback_DynamicVNodeProducedEvent(Node: PNMRAnetNode; EventIndex: Integer; var EventID: TEventID): Boolean;

implementation

// *****************************************************************************
//  procedure AppCallback_InitializeEvents
//     Parameters: Node:  Node that is requesting a Dynamic Consumed Event definition
//                 EventIndex: Index of the Static Event, 0 indexed
//                 ConsumedProduced: EVENT_TYPE_xxxx flag
//     Returns:     None
//     Description: Allows event states to be initalized (set/clear) on node creation
//                  The Node^.State can be tested for NS_VIRTUAL to decide if it is
//                  a virtual node or not
// *****************************************************************************
procedure AppCallback_InitializeEvents(Node: PNMRAnetNode; EventIndex: Integer; ConsumedProduced: Byte);
begin
end;

// *****************************************************************************
//  procedure AppCallback_InitializeDynamicEvents
//     Parameters: Node:  Node that is requesting a Dynamic Consumed Event definition
//                 EventIndex: Index of the Dynamic Event, 0 indexed
//                 ConsumedProduced: EVENT_TYPE_xxxx flag
//     Returns:     None
//     Description: Allows event states to be initalized (set/clear) on node creation
//                  The Node^.State can be tested for NS_VIRTUAL to decide if it is
//                  a virtual node or not
// *****************************************************************************
procedure AppCallback_InitializeDynamicEvents(Node: PNMRAnetNode; EventIndex: Integer; ConsumedProduced: Byte);
begin
end;

// *****************************************************************************
//  procedure AppCallback_DynamicConsumedEvent
//     Parameters: Node:  Node that is requesting a Dynamic Consumed Event definition
//                 EventIndex: Index of the Dynamic Event, 0 indexed to USER_MAX_SUPPORTED_DYNAMIC_EVENTS_CONSUMED-1
//                 EventID : Buffer to hold the Event
//     Returns:     True if the Event exists and EventID is valid, else false if the Event does not exist that this time
//     Description: Allows dynamic Events to be created and destroyed
// *****************************************************************************
function AppCallback_DynamicConsumedEvent(Node: PNMRAnetNode; EventIndex: Integer; var EventID: TEventID): Boolean;
begin
  Result := False;
end;

// *****************************************************************************
//  procedure AppCallback_DynamicProducedEvent
//     Parameters: Node:  Node that is requesting a Dynamic Produced Event definition
//                 EventIndex: Index of the Dynamic Event, 0 indexed to USER_MAX_SUPPORTED_DYNAMIC_EVENTS_PRODCUED-1
//                 EventID : Buffer to hold the Event
//     Returns:     True if the Event exists and EventID is valid, else false if the Event does not exist that this time
//     Description: Allows dynamic Events to be created and destroyed
// *****************************************************************************
function AppCallback_DynamicProducedEvent(Node: PNMRAnetNode; EventIndex: Integer; var EventID: TEventID): Boolean;
begin
  Result := False;
end;

// *****************************************************************************
//  procedure AppCallback_DynamicVNodeConsumedEvent
//     Parameters: Node:  Virtaul Node that is requesting a Dynamic Consumed Event definition
//                 EventIndex: Index of the Dynamic Event, 0 indexed to USER_MAX_VNODE_SUPPORTED_DYNAMIC_EVENTS_CONSUMED-1
//                 EventID : Buffer to hold the Event
//     Returns:     True if the Event exists and EventID is valid, else false if the Event does not exist that this time
//     Description: Allows dynamic Events to be created and destroyed
// *****************************************************************************
function AppCallback_DynamicVNodeConsumedEvent(Node: PNMRAnetNode; EventIndex: Integer; var EventID: TEventID): Boolean;
begin
  Result := False
end;

// *****************************************************************************
//  procedure AppCallback_DynamicVNodeProducedEvent
//     Parameters: Node:  Virtaul Node that is requesting a Dynamic Prodcued Event definition
//                 EventIndex: Index of the Dynamic Event, 0 indexed to USER_MAX_VNODE_SUPPORTED_DYNAMIC_EVENTS_PRODCUED-1
//                 EventID : Buffer to hold the Event
//     Returns:     True if the Event exists and EventID is valid, else false if the Event does not exist that this time
//     Description: Allows dynamic Events to be created and destroyed
// *****************************************************************************
function AppCallback_DynamicVNodeProducedEvent(Node: PNMRAnetNode; EventIndex: Integer; var EventID: TEventID): Boolean;
begin
  Result := False;
end;

end.

