unit template_callbacks;


{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  opstacktypes,
  opstackdefines;

function AppCallback_DynamicConsumedEvent(Node: PNMRAnetNode; EventIndex: Integer; var EventID: TEventID): Boolean;
function AppCallback_DynamicProducedEvent(Node: PNMRAnetNode; EventIndex: Integer; var EventID: TEventID): Boolean;
function AppCallback_DynamicVNodeConsumedEvent(Node: PNMRAnetNode; EventIndex: Integer; var EventID: TEventID): Boolean;
function AppCallback_DynamicVNodeProducedEvent(Node: PNMRAnetNode; EventIndex: Integer; var EventID: TEventID): Boolean;

implementation

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
  Result := False
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
  Result := False
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
  Result := False
end;

end.

