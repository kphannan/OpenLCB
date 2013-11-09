unit template_vnode;

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

// Set the number of Events that are Consumed by this Node, if none then remove the SUPPORT_AT_LEAST_ONE_CONSUMED_EVENT
//  conditional define from the Options.inc file
{$IFDEF SUPPORT_AT_LEAST_ONE_VNODE_CONSUMED_EVENT}
const
  USER_MAX_VNODE_SUPPORTED_EVENTS_CONSUMED = 2;
{$ELSE}
const
  USER_MAX_VNODE_SUPPORTED_EVENTS_CONSUMED = 0;
{$ENDIF}

// Dynamic events are events whos IDs are not known at compile time. When enabled
// the library will callback on the AppCallback_DynamicEvent_Produced with and index from
// 0 to USER_MAX_SUPPORTED_DYNAMIC_EVENTS_CONSUMED - 1.
const
  USER_MAX_VNODE_SUPPORTED_DYNAMIC_EVENTS_CONSUMED = 0;

// Define the UIDs of Events that are Consumed by this Node, if none then remove the SUPPORT_AT_LEAST_ONE_CONSUMED_EVENT
//  conditional define from the Options.inc file
{$IFDEF SUPPORT_AT_LEAST_ONE_VNODE_CONSUMED_EVENT}
const
  USER_VNODE_SUPPORTED_EVENTS_CONSUMED: array[0..USER_MAX_VNODE_SUPPORTED_EVENTS_CONSUMED-1] of TEventID = (
    ($01, $01, $00, $00, $00, $00, $FF, $FF),                                    // EVENT_EMERGENCY_STOP
    ($05, $02, $01, $02, $02, $00, $00, $00)                                    // TEST
  );
{$ENDIF}


// Set the number of Events that are Produced by this Node, if none then remove the SUPPORT_AT_LEAST_ONE_PRODUCED_EVENT
//  conditional define from the Options.inc file
{$IFDEF SUPPORT_AT_LEAST_ONE_VNODE_PRODUCED_EVENT}
const
  USER_MAX_VNODE_SUPPORTED_EVENTS_PRODUCED = 2;
{$ELSE}
const
  USER_MAX_VNODE_SUPPORTED_EVENTS_PRODUCED = 0;
{$ENDIF}

// Dynamic events are events whos IDs are not known at compile time. When enabled
// the library will callback on the AppCallback_DynamicEvent_Produced with and index from
// 0 to USER_MAX_SUPPORTED_DYNAMIC_EVENTS_PRODUCED - 1.
const
  USER_MAX_VNODE_SUPPORTED_DYNAMIC_EVENTS_PRODUCED = 0;

// Define the UIDs of Events that are Produced by this Node
  {$IFDEF SUPPORT_AT_LEAST_ONE_VNODE_PRODUCED_EVENT}
  const
    USER_VNODE_SUPPORTED_EVENTS_PRODUCED: array[0..USER_MAX_VNODE_SUPPORTED_EVENTS_PRODUCED-1] of TEventID = (
      ($01, $01, $00, $00, $00, $00, $FF, $FF),                                    // EVENT_EMERGENCY_STOP
      ($05, $02, $01, $02, $02, $00, $00, $00)                                    // TEST
    );
  {$ENDIF}

// Set Protocols that the node(s) will support and return in the Protocol Identification Protcol
const
  USER_PIV_VNODE_SUPPORTED_PROTOCOL_COUNT = 8;    // UPDATE THIS IF ADDING OR SUBTRACTING SUPPORTED PROTOCOLS
  USER_PIV_VNODE_SUPPORTED_PROTOCOLS: array[0..USER_PIV_VNODE_SUPPORTED_PROTOCOL_COUNT-1] of TPIVProtocolValueArray = (     // Look at the PIV_xxxx constants for more Protocols
    ($80, $00, $00, $00, $00, $00),                                             // Protocol
    ($40, $00, $00, $00, $00, $00),                                             // Datagram Protocol
    ($04, $00, $00, $00, $00, $00),                                             // Producer Consumer Protocol
    ($10, $00, $00, $00, $00, $00),                                             // Memory Configruation Protocol
    ($00, $08, $00, $00, $00, $00),                                             // CDI Protocol
    ($00, $10, $00, $00, $00, $00),                                             // SNIP Protocol
    ($00, $40, $00, $00, $00, $00),                                             // ACDI Protocol
    ($00, $04, $00, $00, $00, $00)                                              // Traction Protocol
    );


  // Set options and configurations of the Configuration Memory Protocol.  Most depend on the
  //   capabilities of the device/EEPROM/Flash being used and what the application wants to support
const
  USER_VNODE_CONFIGMEM_OPTIONS = MCO_UNALIGNED_READS or MCO_ACDI_MFG_READS or MCO_ACDI_USER_READS or MCO_ACDI_USER_WRITES or MCO_UNALIGNED_WRITES;  // The commands that are avialable to the system, see the MCO_xxx constants
  USER_VNODE_CONFIGMEM_WRITE_LENGTH = MCWL_ONE_BYTE or MCWL_TWO_BYTE or MCWL_FOUR_BYTE or MCWL_64_BYTE or MCWL_ARBITRARY_BYTE;                      // The length of writes supported by the EEPROM, see the MCWL_xxx constants
  USER_VNODE_CONFIGMEM_HIGHEST_SPACE = MSI_CDI;                                                                                                     // Highest space, see MSI_xxx constants
  USER_VNODE_CONFIGMEM_LOWEST_SPACE = MSI_FDI;                                                                                                      // Lowest space, see MSI_xxx constants


const
  USER_MAX_VNODE_CDI_ARRAY = 0;
  USER_MAX_VNODE_ACDI_MFG_ARRAY = 0;
  USER_MAX_VNODE_USER_CONFIG_DATA = 0;

implementation

end.
