// *****************************************************************************
//
// This file is the template for the application to define the Node that is the
// physical node.  It can have completely different attributes than the virtual
// nodes.
//
// *****************************************************************************

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
  USER_MAX_SUPPORTED_EVENTS_CONSUMED = 2;
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
    ($01, $01, $00, $00, $00, $00, $FF, $FF),                                    // EVENT_EMERGENCY_STOP
    ($05, $02, $01, $02, $02, $00, $00, $00)                                    // TEST
  );
{$ENDIF}


// Set the number of Events that are Produced by this Node, if none then remove the SUPPORT_AT_LEAST_ONE_PRODUCED_EVENT
//  conditional define from the Options.inc file
{$IFDEF SUPPORT_AT_LEAST_ONE_PRODUCED_EVENT}
const
  USER_MAX_SUPPORTED_EVENTS_PRODUCED = 2;
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
      ($01, $01, $00, $00, $00, $00, $FF, $FF),                                    // EVENT_EMERGENCY_STOP
      ($05, $02, $01, $02, $02, $00, $00, $00)                                    // TEST
    );
  {$ENDIF}


// Set Protocols that the node(s) will support and return in the Protocol Identification Protcol
const
  USER_PIV_SUPPORTED_PROTOCOL_COUNT = 7;    // UPDATE THIS IF ADDING OR SUBTRACTING SUPPORTED PROTOCOLS
  USER_PIV_SUPPORTED_PROTOCOLS: array[0..USER_PIV_SUPPORTED_PROTOCOL_COUNT-1] of TPIVProtocolValueArray = (     // Look at the PIV_xxxx constants for more Protocols
    ($80, $00, $00, $00, $00, $00),                                             // Protocol
    ($40, $00, $00, $00, $00, $00),                                             // Datagram Protocol
    ($04, $00, $00, $00, $00, $00),                                             // Producer Consumer Protocol
    ($10, $00, $00, $00, $00, $00),                                             // Memory Configruation Protocol
    ($00, $08, $00, $00, $00, $00),                                             // CDI Protocol
    ($00, $10, $00, $00, $00, $00),                                             // SNIP Protocol
    ($00, $40, $00, $00, $00, $00)                                              // ACDI Protocol
    );


// Set options and configurations of the Configuration Memory Protocol.  Most depend on the
//   capabilities of the device/EEPROM/Flash being used and what the application wants to support
const
  USER_CONFIGMEM_OPTIONS = MCO_UNALIGNED_READS or MCO_ACDI_MFG_READS or MCO_ACDI_USER_READS or MCO_ACDI_USER_WRITES or MCO_UNALIGNED_WRITES;  // The commands that are avialable to the system, see the MCO_xxx constants
  USER_CONFIGMEM_WRITE_LENGTH = MCWL_ONE_BYTE or MCWL_TWO_BYTE or MCWL_FOUR_BYTE or MCWL_64_BYTE or MCWL_ARBITRARY_BYTE;                      // The length of writes supported by the EEPROM, see the MCWL_xxx constants
  USER_CONFIGMEM_HIGHEST_SPACE = MSI_CDI;                                                                                                     // Highest space, see MSI_xxx constants
  USER_CONFIGMEM_LOWEST_SPACE = MSI_FDI;                                                                                                      // Lowest space, see MSI_xxx constants

const
  USER_MAX_USER_CONFIG_DATA = 0;


 // **************************************************************************************************************************************************************
 // ACDI Manufacturer Memory ($FC) Space Implementation
 // **************************************************************************************************************************************************************
const
  USER_MAX_ACDI_MFG_ARRAY = 26;
  USER_ACDI_MFG_STRINGS: array[0..USER_MAX_ACDI_MFG_ARRAY - 1] of byte = (
      $4D,$75,$73,$74,$61,$6E,$67,$70,$65,$61,$6B,$00,  // Mustangpeak
      $43,$53,$31,$30,$30,$00,  // CS100
      $30,$2E,$31,$00,  // 0.1
      $30,$2E,$33,$00  // 0.3
    );

  // **************************************************************************************************************************************************************
  // CDI Memory ($FF) Space Implementation
  // **************************************************************************************************************************************************************
const
  USER_MAX_CDI_ARRAY = 12491 + 40;
  USER_CDI_ARRAY: array[0..USER_MAX_CDI_ARRAY-1] of byte = (
    $3C, $3F, $78, $6D, $6C, $20, $76, $65, $72, $73, $69, $6F, $6E, $3D, $22, $31, $2E, $30, $22, $20, $65, $6E, $63, $6F, $64, $69, $6E, $67, $3D, $22, $75, $74, $66, $2D, $38, $22, $3F, $3E,    // <?xml version="1.0" encoding="utf-8"?>
    $3C, $3F, $78, $6D, $6C, $2D, $73, $74, $79, $6C, $65, $73, $68, $65, $65, $74, $20, $74, $79, $70, $65, $3D, $22, $74, $65, $78, $74, $2F, $78, $73, $6C, $22, $20, $68, $72, $65, $66, $3D, $22, $68, $74, $74, $70, $3A, $2F, $2F, $6F, $70, $65, $6E, $6C, $63, $62, $2E, $6F, $72, $67, $2F, $74, $72, $75, $6E, $6B, $2F, $70, $72, $6F, $74, $6F, $74, $79, $70, $65, $73, $2F, $78, $6D, $6C, $2F, $78, $73, $6C, $74, $2F, $63, $64, $69, $2E, $78, $73, $6C, $22, $3F, $3E,    // <?xml-stylesheet type="text/xsl" href="http://openlcb.org/trunk/prototypes/xml/xslt/cdi.xsl"?>
    $3C, $63, $64, $69, $20, $78, $6D, $6C, $6E, $73, $3A, $78, $73, $69, $3D, $22, $68, $74, $74, $70, $3A, $2F, $2F, $77, $77, $77, $2E, $77, $33, $2E, $6F, $72, $67, $2F, $32, $30, $30, $31, $2F, $58, $4D, $4C, $53, $63, $68, $65, $6D, $61, $2D, $69, $6E, $73, $74, $61, $6E, $63, $65, $22, $20, $78, $73, $69, $3A, $6E, $6F, $4E, $61, $6D, $65, $73, $70, $61, $63, $65, $53, $63, $68, $65, $6D, $61, $4C, $6F, $63, $61, $74, $69, $6F, $6E, $3D, $22, $68, $74, $74, $70, $3A, $2F, $2F, $6F, $70, $65, $6E, $6C, $63, $62, $2E, $6F, $72, $67, $2F, $74, $72, $75, $6E, $6B, $2F, $73, $70, $65, $63, $73, $2F, $73, $63, $68, $65, $6D, $61, $2F, $63, $64, $69, $2E, $78, $73, $64, $22, $3E,    // <cdi xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="http://openlcb.org/trunk/specs/schema/cdi.xsd">
    $3C, $69, $64, $65, $6E, $74, $69, $66, $69, $63, $61, $74, $69, $6F, $6E, $3E,    // <identification>
    $3C, $6D, $61, $6E, $75, $66, $61, $63, $74, $75, $72, $65, $72, $3E, $4D, $75, $73, $74, $61, $6E, $67, $70, $65, $61, $6B, $3C, $2F, $6D, $61, $6E, $75, $66, $61, $63, $74, $75, $72, $65, $72, $3E,    // <manufacturer>Mustangpeak</manufacturer>
    $3C, $6D, $6F, $64, $65, $6C, $3E, $43, $53, $31, $30, $30, $3C, $2F, $6D, $6F, $64, $65, $6C, $3E,    // <model>CS100</model>
    $3C, $68, $61, $72, $64, $77, $61, $72, $65, $56, $65, $72, $73, $69, $6F, $6E, $3E, $31, $2E, $30, $3C, $2F, $68, $61, $72, $64, $77, $61, $72, $65, $56, $65, $72, $73, $69, $6F, $6E, $3E,    // <hardwareVersion>1.0</hardwareVersion>
    $3C, $73, $6F, $66, $74, $77, $61, $72, $65, $56, $65, $72, $73, $69, $6F, $6E, $3E, $30, $2E, $32, $3C, $2F, $73, $6F, $66, $74, $77, $61, $72, $65, $56, $65, $72, $73, $69, $6F, $6E, $3E,    // <softwareVersion>0.2</softwareVersion>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $44, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>Description</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $4D, $75, $73, $74, $61, $6E, $67, $70, $65, $61, $6B, $20, $43, $6F, $6D, $6D, $61, $6E, $64, $20, $53, $74, $61, $74, $69, $6F, $6E, $20, $4E, $6F, $64, $65, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Mustangpeak Command Station Node</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $53, $74, $61, $74, $75, $73, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>Status</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $50, $72, $6F, $74, $6F, $74, $79, $70, $65, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Prototype</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $64, $65, $6E, $74, $69, $66, $69, $63, $61, $74, $69, $6F, $6E, $3E,    // </identification>
    $3C, $61, $63, $64, $69, $20, $66, $69, $78, $65, $64, $3D, $22, $31, $22, $20, $76, $61, $72, $3D, $22, $31, $22, $20, $2F, $3E,    // <acdi fixed="1" var="1" />
    $3C, $73, $65, $67, $6D, $65, $6E, $74, $20, $6F, $72, $69, $67, $69, $6E, $3D, $22, $30, $22, $20, $73, $70, $61, $63, $65, $3D, $22, $32, $35, $33, $22, $3E,    // <segment origin="0" space="253">
    $3C, $6E, $61, $6D, $65, $3E, $4F, $70, $74, $69, $6F, $6E, $73, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Options</name>
    $3C, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E, $43, $6F, $6E, $66, $69, $67, $75, $72, $61, $74, $69, $6F, $6E, $20, $6F, $70, $74, $69, $6F, $6E, $73, $20, $66, $6F, $72, $20, $74, $68, $65, $20, $4C, $43, $42, $72, $69, $63, $6B, $73, $20, $43, $6F, $6D, $6D, $61, $6E, $64, $20, $53, $74, $61, $74, $69, $6F, $6E, $3C, $2F, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E,    // <description>Configuration options for the LCBricks Command Station</description>
    $3C, $67, $72, $6F, $75, $70, $20, $6F, $66, $66, $73, $65, $74, $3D, $22, $31, $22, $3E,    // <group offset="1">
    $3C, $6E, $61, $6D, $65, $3E, $55, $73, $65, $72, $20, $44, $61, $74, $61, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>User Data</name>
    $3C, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E, $41, $64, $64, $20, $79, $6F, $75, $72, $20, $6F, $77, $6E, $20, $75, $6E, $69, $71, $75, $65, $20, $6E, $6F, $64, $65, $20, $69, $6E, $66, $6F, $20, $68, $65, $72, $65, $3C, $2F, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E,    // <description>Add your own unique node info here</description>
    $3C, $73, $74, $72, $69, $6E, $67, $20, $73, $69, $7A, $65, $3D, $22, $36, $33, $22, $3E,    // <string size="63">
    $3C, $6E, $61, $6D, $65, $3E, $55, $73, $65, $72, $20, $4E, $61, $6D, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>User Name</name>
    $3C, $2F, $73, $74, $72, $69, $6E, $67, $3E,    // </string>
    $3C, $73, $74, $72, $69, $6E, $67, $20, $73, $69, $7A, $65, $3D, $22, $36, $34, $22, $3E,    // <string size="64">
    $3C, $6E, $61, $6D, $65, $3E, $55, $73, $65, $72, $20, $44, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>User Description</name>
    $3C, $2F, $73, $74, $72, $69, $6E, $67, $3E,    // </string>
    $3C, $2F, $67, $72, $6F, $75, $70, $3E,    // </group>
    $3C, $67, $72, $6F, $75, $70, $3E,    // <group>
    $3C, $6E, $61, $6D, $65, $3E, $43, $6F, $6D, $6D, $61, $6E, $64, $20, $53, $74, $61, $74, $69, $6F, $6E, $20, $4F, $70, $74, $69, $6F, $6E, $73, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Command Station Options</name>
    $3C, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E, $43, $6F, $6E, $66, $69, $67, $75, $72, $61, $74, $69, $6F, $6E, $20, $6F, $70, $74, $69, $6F, $6E, $73, $3C, $2F, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E,    // <description>Configuration options</description>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $4F, $75, $74, $70, $75, $74, $20, $4D, $6F, $64, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Output Mode</name>
    $3C, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E, $53, $65, $6C, $65, $63, $74, $20, $69, $66, $20, $74, $68, $69, $73, $20, $43, $6F, $6D, $6D, $61, $6E, $64, $20, $53, $74, $61, $74, $69, $6F, $6E, $20, $69, $73, $20, $61, $20, $4D, $61, $69, $6E, $20, $4C, $69, $6E, $65, $20, $4E, $6F, $64, $65, $20, $6F, $72, $20, $61, $20, $50, $72, $6F, $67, $72, $61, $6D, $6D, $69, $6E, $67, $20, $54, $72, $61, $63, $6B, $20, $4E, $6F, $64, $65, $3C, $2F, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E,    // <description>Select if this Command Station is a Main Line Node or a Programming Track Node</description>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $43, $6F, $6D, $6D, $61, $6E, $64, $20, $53, $74, $61, $74, $69, $6F, $6E, $20, $64, $72, $69, $76, $65, $73, $20, $61, $20, $4D, $61, $69, $6E, $20, $4C, $69, $6E, $65, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Command Station drives a Main Line</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $43, $6F, $6D, $6D, $61, $6E, $64, $20, $53, $74, $61, $74, $69, $6F, $6E, $20, $64, $72, $69, $76, $65, $73, $20, $61, $20, $50, $72, $6F, $67, $72, $61, $6D, $6D, $69, $6E, $67, $20, $54, $72, $61, $63, $6B, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Command Station drives a Programming Track</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $50, $72, $6F, $67, $72, $61, $6D, $6D, $69, $6E, $67, $20, $4D, $6F, $64, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Programming Mode</name>
    $3C, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E, $53, $65, $6C, $65, $63, $74, $73, $20, $74, $68, $65, $20, $50, $72, $6F, $67, $72, $61, $6D, $6D, $69, $6E, $67, $20, $4D, $6F, $64, $65, $20, $74, $68, $65, $20, $43, $6F, $6D, $6D, $61, $6E, $64, $20, $53, $74, $61, $74, $69, $6F, $6E, $20, $77, $69, $6C, $6C, $20, $75, $73, $65, $20, $74, $6F, $20, $77, $72, $69, $74, $65, $20, $43, $56, $27, $73, $20, $74, $6F, $20, $74, $68, $65, $20, $64, $65, $63, $6F, $64, $65, $72, $3C, $2F, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E,    // <description>Selects the Programming Mode the Command Station will use to write CV's to the decoder</description>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $4F, $70, $65, $72, $61, $74, $69, $6F, $6E, $20, $4D, $6F, $64, $65, $20, $28, $74, $6F, $20, $61, $20, $4D, $61, $69, $6E, $20, $4C, $69, $6E, $65, $29, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Operation Mode (to a Main Line)</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $41, $75, $74, $6F, $20, $44, $65, $74, $65, $63, $74, $20, $53, $65, $72, $76, $69, $63, $65, $20, $4D, $6F, $64, $65, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Auto Detect Service Mode</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $32, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>2</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $44, $69, $72, $65, $63, $74, $20, $42, $69, $74, $20, $4D, $6F, $64, $65, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Direct Bit Mode</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $33, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>3</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $44, $69, $72, $65, $63, $74, $20, $42, $79, $74, $65, $20, $4D, $6F, $64, $65, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Direct Byte Mode</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $34, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>4</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $50, $61, $67, $65, $64, $20, $4D, $6F, $64, $65, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Paged Mode</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $35, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>5</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $52, $65, $67, $69, $73, $74, $65, $72, $20, $4D, $6F, $64, $65, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Register Mode</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $44, $43, $43, $20, $42, $75, $73, $20, $4D, $6F, $64, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>DCC Bus Mode</name>
    $3C, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E, $53, $65, $6C, $65, $63, $74, $20, $69, $66, $20, $74, $68, $69, $73, $20, $43, $6F, $6D, $6D, $61, $6E, $64, $20, $53, $74, $61, $74, $69, $6F, $6E, $20, $69, $73, $20, $74, $68, $65, $20, $64, $72, $69, $76, $65, $72, $20, $6F, $72, $20, $72, $65, $63, $65, $69, $76, $65, $72, $20, $6F, $66, $20, $74, $68, $65, $20, $44, $43, $43, $20, $62, $75, $73, $20, $73, $69, $67, $6E, $61, $6C, $20, $28, $44, $61, $6D, $61, $67, $65, $20, $6D, $61, $79, $20, $6F, $63, $63, $75, $72, $20, $69, $66, $20, $6D, $6F, $72, $65, $20, $74, $68, $61, $6E, $20, $6F, $6E, $65, $20, $43, $6F, $6D, $6D, $61, $6E, $64, $20, $53, $74, $61, $74, $69, $6F, $6E, $20, $69, $73, $20, $73, $65, $6C, $65, $63, $74, $65, $64, $20, $74, $6F, $20, $62, $65, $20, $61, $20, $64, $72, $69, $76, $65, $72, $20, $6F, $6E, $20, $74, $68, $65, $20, $44, $43, $43, $20, $62, $75, $73, $29, $3C, $2F, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E,    // <description>Select if this Command Station is the driver or receiver of the DCC bus signal (Damage may occur if more than one Command Station is selected to be a driver on the DCC bus)</description>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $43, $6F, $6D, $6D, $61, $6E, $64, $20, $53, $74, $61, $74, $69, $6F, $6E, $20, $69, $73, $20, $61, $20, $44, $43, $43, $20, $42, $75, $73, $20, $52, $65, $63, $65, $69, $76, $65, $72, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Command Station is a DCC Bus Receiver</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $43, $6F, $6D, $6D, $61, $6E, $64, $20, $53, $74, $61, $74, $69, $6F, $6E, $20, $69, $73, $20, $61, $20, $44, $43, $43, $20, $42, $75, $73, $20, $54, $72, $61, $6E, $73, $6D, $69, $74, $74, $65, $72, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Command Station is a DCC Bus Transmitter</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $61, $69, $6C, $43, $6F, $6D, $20, $43, $75, $74, $6F, $75, $74, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>RailCom Cutout</name>
    $3C, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E, $45, $6E, $61, $62, $6C, $65, $2F, $44, $69, $73, $61, $62, $6C, $65, $20, $74, $68, $65, $20, $52, $61, $69, $6C, $43, $6F, $6D, $20, $43, $75, $74, $6F, $75, $74, $20, $69, $6E, $20, $74, $68, $65, $20, $44, $43, $43, $20, $4F, $75, $74, $70, $75, $74, $3C, $2F, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E,    // <description>Enable/Disable the RailCom Cutout in the DCC Output</description>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $52, $61, $69, $6C, $43, $6F, $6D, $20, $43, $75, $74, $6F, $75, $74, $20, $44, $69, $73, $61, $62, $6C, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>RailCom Cutout Disabled</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $52, $61, $69, $6C, $43, $6F, $6D, $20, $43, $75, $74, $6F, $75, $74, $20, $45, $6E, $61, $62, $6C, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>RailCom Cutout Enabled</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $32, $22, $3E,    // <int size="2">
    $3C, $6E, $61, $6D, $65, $3E, $4F, $76, $65, $72, $63, $75, $72, $72, $65, $6E, $74, $20, $54, $72, $69, $70, $20, $4C, $65, $76, $65, $6C, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Overcurrent Trip Level</name>
    $3C, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E, $4C, $65, $76, $65, $6C, $20, $6F, $66, $20, $63, $75, $72, $72, $65, $6E, $74, $20, $74, $6F, $20, $74, $72, $69, $70, $20, $6F, $76, $65, $72, $20, $63, $75, $72, $72, $65, $6E, $74, $20, $70, $72, $6F, $74, $65, $63, $74, $69, $6F, $6E, $20, $73, $68, $75, $74, $64, $6F, $77, $6E, $20, $28, $6D, $69, $6C, $6C, $69, $41, $6D, $70, $73, $29, $3C, $2F, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E,    // <description>Level of current to trip over current protection shutdown (milliAmps)</description>
    $3C, $6D, $69, $6E, $3E, $30, $3C, $2F, $6D, $69, $6E, $3E,    // <min>0</min>
    $3C, $6D, $61, $78, $3E, $35, $30, $30, $30, $3C, $2F, $6D, $61, $78, $3E,    // <max>5000</max>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $32, $22, $3E,    // <int size="2">
    $3C, $6E, $61, $6D, $65, $3E, $4F, $76, $65, $72, $63, $75, $72, $72, $65, $6E, $74, $20, $53, $68, $75, $74, $64, $6F, $77, $6E, $20, $54, $69, $6D, $65, $20, $28, $6D, $69, $6C, $6C, $69, $53, $65, $63, $6F, $6E, $64, $73, $29, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Overcurrent Shutdown Time (milliSeconds)</name>
    $3C, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E, $54, $69, $6D, $65, $20, $62, $65, $74, $77, $65, $65, $6E, $20, $66, $69, $72, $73, $74, $20, $64, $65, $74, $65, $63, $74, $65, $64, $20, $6F, $76, $65, $72, $63, $75, $72, $72, $65, $6E, $74, $20, $61, $6E, $64, $20, $73, $68, $75, $74, $64, $6F, $77, $6E, $20, $6F, $66, $20, $74, $68, $65, $20, $43, $6F, $6D, $6D, $61, $6E, $64, $20, $53, $74, $61, $74, $69, $6F, $6E, $3C, $2F, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E,    // <description>Time between first detected overcurrent and shutdown of the Command Station</description>
    $3C, $6D, $69, $6E, $3E, $30, $3C, $2F, $6D, $69, $6E, $3E,    // <min>0</min>
    $3C, $6D, $61, $78, $3E, $31, $30, $30, $30, $3C, $2F, $6D, $61, $78, $3E,    // <max>1000</max>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $32, $22, $3E,    // <int size="2">
    $3C, $6E, $61, $6D, $65, $3E, $41, $75, $74, $6F, $52, $65, $73, $74, $61, $72, $74, $20, $54, $69, $6D, $65, $20, $28, $6D, $69, $6C, $6C, $69, $53, $65, $63, $6F, $6E, $64, $73, $29, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>AutoRestart Time (milliSeconds)</name>
    $3C, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E, $54, $69, $6D, $65, $20, $62, $65, $74, $77, $65, $65, $6E, $20, $61, $6E, $20, $6F, $76, $65, $72, $63, $75, $72, $72, $65, $6E, $74, $20, $64, $65, $74, $65, $63, $74, $69, $6F, $6E, $20, $61, $6E, $64, $20, $61, $6E, $20, $61, $74, $74, $65, $6D, $70, $74, $20, $74, $6F, $20, $72, $65, $6E, $61, $62, $6C, $65, $20, $74, $68, $65, $20, $43, $6F, $6D, $6D, $61, $6E, $64, $20, $53, $74, $61, $74, $69, $6F, $6E, $3C, $2F, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E,    // <description>Time between an overcurrent detection and an attempt to renable the Command Station</description>
    $3C, $6D, $69, $6E, $3E, $31, $30, $30, $30, $3C, $2F, $6D, $69, $6E, $3E,    // <min>1000</min>
    $3C, $6D, $61, $78, $3E, $33, $30, $30, $30, $30, $3C, $2F, $6D, $61, $78, $3E,    // <max>30000</max>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $67, $72, $6F, $75, $70, $3E,    // <group>
    $3C, $6E, $61, $6D, $65, $3E, $53, $74, $61, $74, $65, $20, $52, $65, $70, $65, $61, $74, $20, $53, $65, $74, $75, $70, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>State Repeat Setup</name>
    $3C, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E, $57, $68, $65, $6E, $20, $74, $68, $65, $72, $65, $20, $69, $73, $20, $6E, $6F, $20, $6E, $65, $77, $20, $69, $6E, $66, $6F, $72, $6D, $61, $74, $69, $6F, $6E, $20, $73, $65, $6E, $64, $20, $63, $75, $72, $72, $65, $6E, $74, $20, $73, $74, $61, $74, $65, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $74, $6F, $20, $61, $63, $63, $6F, $75, $6E, $74, $20, $66, $6F, $72, $20, $64, $69, $72, $74, $79, $20, $74, $72, $61, $63, $6B, $2C, $20, $65, $74, $63, $3C, $2F, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E,    // <description>When there is no new information send current state packets to account for dirty track, etc</description>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $65, $70, $65, $61, $74, $20, $53, $70, $65, $65, $64, $2F, $44, $69, $72, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $77, $68, $65, $6E, $20, $69, $64, $6C, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Repeat Speed/Dir packets when idle</name>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $53, $70, $65, $65, $64, $2F, $44, $69, $72, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $6E, $6F, $74, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Speed/Dir Packets not repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $53, $70, $65, $65, $64, $2F, $44, $69, $72, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Speed/Dir Packets repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $65, $70, $65, $61, $74, $20, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $30, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $77, $68, $65, $6E, $20, $69, $64, $6C, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Repeat Function 0 packets when idle</name>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $30, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $6E, $6F, $74, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 0 Packets not repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $30, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 0 Packets repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $65, $70, $65, $61, $74, $20, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $77, $68, $65, $6E, $20, $69, $64, $6C, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Repeat Function 1 packets when idle</name>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $6E, $6F, $74, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 1 Packets not repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 1 Packets repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $65, $70, $65, $61, $74, $20, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $32, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $77, $68, $65, $6E, $20, $69, $64, $6C, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Repeat Function 2 packets when idle</name>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $32, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $6E, $6F, $74, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 2 Packets not repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $32, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 2 Packets repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $65, $70, $65, $61, $74, $20, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $33, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $77, $68, $65, $6E, $20, $69, $64, $6C, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Repeat Function 3 packets when idle</name>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $33, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $6E, $6F, $74, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 3 Packets not repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $33, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 3 Packets repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $65, $70, $65, $61, $74, $20, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $34, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $77, $68, $65, $6E, $20, $69, $64, $6C, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Repeat Function 4 packets when idle</name>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $34, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $6E, $6F, $74, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 4 Packets not repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $34, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 4 Packets repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $65, $70, $65, $61, $74, $20, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $35, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $77, $68, $65, $6E, $20, $69, $64, $6C, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Repeat Function 5 packets when idle</name>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $35, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $6E, $6F, $74, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 5 Packets not repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $35, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 5 Packets repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $65, $70, $65, $61, $74, $20, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $36, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $77, $68, $65, $6E, $20, $69, $64, $6C, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Repeat Function 6 packets when idle</name>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $36, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $6E, $6F, $74, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 6 Packets not repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $36, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 6 Packets repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $65, $70, $65, $61, $74, $20, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $37, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $77, $68, $65, $6E, $20, $69, $64, $6C, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Repeat Function 7 packets when idle</name>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $37, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $6E, $6F, $74, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 7 Packets not repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $37, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 7 Packets repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $65, $70, $65, $61, $74, $20, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $38, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $77, $68, $65, $6E, $20, $69, $64, $6C, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Repeat Function 8 packets when idle</name>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $38, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $6E, $6F, $74, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 8 Packets not repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $38, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 8 Packets repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $65, $70, $65, $61, $74, $20, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $39, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $77, $68, $65, $6E, $20, $69, $64, $6C, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Repeat Function 9 packets when idle</name>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $39, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $6E, $6F, $74, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 9 Packets not repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $39, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 9 Packets repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $65, $70, $65, $61, $74, $20, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $30, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $77, $68, $65, $6E, $20, $69, $64, $6C, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Repeat Function 10 packets when idle</name>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $30, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $6E, $6F, $74, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 10 Packets not repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $30, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 10 Packets repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $65, $70, $65, $61, $74, $20, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $31, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $77, $68, $65, $6E, $20, $69, $64, $6C, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Repeat Function 11 packets when idle</name>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $31, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $6E, $6F, $74, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 11 Packets not repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $31, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 11 Packets repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $65, $70, $65, $61, $74, $20, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $32, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $77, $68, $65, $6E, $20, $69, $64, $6C, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Repeat Function 12 packets when idle</name>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $32, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $6E, $6F, $74, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 12 Packets not repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $32, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 12 Packets repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $65, $70, $65, $61, $74, $20, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $33, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $77, $68, $65, $6E, $20, $69, $64, $6C, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Repeat Function 13 packets when idle</name>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $33, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $6E, $6F, $74, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 13 Packets not repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $33, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 13 Packets repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $65, $70, $65, $61, $74, $20, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $34, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $77, $68, $65, $6E, $20, $69, $64, $6C, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Repeat Function 14 packets when idle</name>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $34, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $6E, $6F, $74, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 14 Packets not repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $34, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 14 Packets repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $65, $70, $65, $61, $74, $20, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $35, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $77, $68, $65, $6E, $20, $69, $64, $6C, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Repeat Function 15 packets when idle</name>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $35, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $6E, $6F, $74, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 15 Packets not repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $35, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 15 Packets repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $65, $70, $65, $61, $74, $20, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $36, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $77, $68, $65, $6E, $20, $69, $64, $6C, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Repeat Function 16 packets when idle</name>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $36, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $6E, $6F, $74, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 16 Packets not repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $36, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 16 Packets repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $65, $70, $65, $61, $74, $20, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $37, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $77, $68, $65, $6E, $20, $69, $64, $6C, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Repeat Function 17 packets when idle</name>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $37, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $6E, $6F, $74, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 17 Packets not repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $37, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 17 Packets repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $65, $70, $65, $61, $74, $20, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $38, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $77, $68, $65, $6E, $20, $69, $64, $6C, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Repeat Function 18 packets when idle</name>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $38, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $6E, $6F, $74, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 18 Packets not repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $38, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 18 Packets repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $65, $70, $65, $61, $74, $20, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $39, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $77, $68, $65, $6E, $20, $69, $64, $6C, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Repeat Function 19 packets when idle</name>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $39, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $6E, $6F, $74, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 19 Packets not repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $31, $39, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 19 Packets repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $65, $70, $65, $61, $74, $20, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $32, $30, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $77, $68, $65, $6E, $20, $69, $64, $6C, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Repeat Function 20 packets when idle</name>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $32, $30, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $6E, $6F, $74, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 20 Packets not repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $32, $30, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 20 Packets repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $65, $70, $65, $61, $74, $20, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $32, $31, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $77, $68, $65, $6E, $20, $69, $64, $6C, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Repeat Function 21 packets when idle</name>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $32, $31, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $6E, $6F, $74, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 21 Packets not repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $32, $31, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 21 Packets repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $65, $70, $65, $61, $74, $20, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $32, $32, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $77, $68, $65, $6E, $20, $69, $64, $6C, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Repeat Function 22 packets when idle</name>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $32, $32, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $6E, $6F, $74, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 22 Packets not repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $32, $32, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 22 Packets repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $65, $70, $65, $61, $74, $20, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $32, $33, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $77, $68, $65, $6E, $20, $69, $64, $6C, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Repeat Function 23 packets when idle</name>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $32, $33, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $6E, $6F, $74, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 23 Packets not repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $32, $33, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 23 Packets repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $65, $70, $65, $61, $74, $20, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $32, $34, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $77, $68, $65, $6E, $20, $69, $64, $6C, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Repeat Function 24 packets when idle</name>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $32, $34, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $6E, $6F, $74, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 24 Packets not repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $32, $34, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 24 Packets repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $65, $70, $65, $61, $74, $20, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $32, $35, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $77, $68, $65, $6E, $20, $69, $64, $6C, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Repeat Function 25 packets when idle</name>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $32, $35, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $6E, $6F, $74, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 25 Packets not repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $32, $35, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 25 Packets repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $65, $70, $65, $61, $74, $20, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $32, $36, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $77, $68, $65, $6E, $20, $69, $64, $6C, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Repeat Function 26 packets when idle</name>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $32, $36, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $6E, $6F, $74, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 26 Packets not repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $32, $36, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 26 Packets repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $65, $70, $65, $61, $74, $20, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $32, $37, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $77, $68, $65, $6E, $20, $69, $64, $6C, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Repeat Function 27 packets when idle</name>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $32, $37, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $6E, $6F, $74, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 27 Packets not repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $32, $37, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 27 Packets repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $52, $65, $70, $65, $61, $74, $20, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $32, $38, $20, $70, $61, $63, $6B, $65, $74, $73, $20, $77, $68, $65, $6E, $20, $69, $64, $6C, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Repeat Function 28 packets when idle</name>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $32, $38, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $6E, $6F, $74, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 28 Packets not repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $46, $75, $6E, $63, $74, $69, $6F, $6E, $20, $32, $38, $20, $50, $61, $63, $6B, $65, $74, $73, $20, $72, $65, $70, $65, $61, $74, $65, $64, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Function 28 Packets repeated</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $2F, $67, $72, $6F, $75, $70, $3E,    // </group>
    $3C, $2F, $67, $72, $6F, $75, $70, $3E,    // </group>
    $3C, $2F, $73, $65, $67, $6D, $65, $6E, $74, $3E,    // </segment>
    $3C, $73, $65, $67, $6D, $65, $6E, $74, $20, $6F, $72, $69, $67, $69, $6E, $3D, $22, $32, $35, $36, $22, $20, $73, $70, $61, $63, $65, $3D, $22, $32, $35, $33, $22, $3E,    // <segment origin="256" space="253">
    $3C, $6E, $61, $6D, $65, $3E, $44, $65, $62, $75, $67, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Debug</name>
    $3C, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E, $44, $65, $62, $75, $67, $20, $6F, $70, $74, $69, $6F, $6E, $73, $20, $66, $6F, $72, $20, $64, $65, $76, $65, $6C, $6F, $70, $65, $72, $73, $2C, $20, $6E, $6F, $72, $6D, $61, $6C, $20, $75, $73, $65, $72, $73, $20, $73, $68, $6F, $75, $6C, $64, $20, $6E, $6F, $74, $20, $63, $68, $61, $6E, $67, $65, $20, $74, $68, $65, $73, $65, $20, $6F, $70, $74, $69, $6F, $6E, $73, $3C, $2F, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E,    // <description>Debug options for developers, normal users should not change these options</description>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $41, $75, $74, $6F, $20, $61, $6C, $6C, $6F, $63, $61, $74, $65, $20, $76, $69, $72, $74, $75, $61, $6C, $20, $74, $72, $61, $69, $6E, $20, $6E, $6F, $64, $65, $73, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Auto allocate virtual train nodes</name>
    $3C, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E, $41, $6C, $6C, $6F, $77, $73, $20, $6D, $61, $6E, $75, $61, $6C, $20, $61, $6C, $6C, $6F, $63, $61, $74, $69, $6F, $6E, $20, $6F, $66, $20, $54, $72, $61, $69, $6E, $20, $76, $4E, $6F, $64, $65, $20, $66, $6F, $72, $20, $73, $74, $72, $65, $73, $73, $69, $6E, $67, $20, $74, $68, $72, $6F, $74, $74, $6C, $65, $73, $20, $77, $69, $74, $68, $20, $6C, $61, $63, $6B, $20, $6F, $66, $20, $72, $65, $73, $6F, $75, $72, $63, $65, $73, $3C, $2F, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E,    // <description>Allows manual allocation of Train vNode for stressing throttles with lack of resources</description>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $30, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>0</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $41, $75, $74, $6F, $2D, $61, $6C, $6C, $6F, $63, $61, $74, $65, $20, $56, $69, $72, $74, $75, $61, $6C, $20, $4E, $6F, $64, $65, $73, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Auto-allocate Virtual Nodes</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $31, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>1</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $4D, $61, $6E, $75, $61, $6C, $6C, $79, $20, $41, $6C, $6C, $6F, $63, $61, $74, $65, $20, $4E, $6F, $64, $65, $73, $20, $28, $22, $61, $22, $20, $6F, $6E, $20, $74, $68, $65, $20, $55, $41, $52, $54, $29, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Manually Allocate Nodes ("a" on the UART)</value>
    $3C, $2F, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // </relation>
    $3C, $2F, $6D, $61, $70, $3E,    // </map>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $2F, $73, $65, $67, $6D, $65, $6E, $74, $3E,    // </segment>
    $3C, $73, $65, $67, $6D, $65, $6E, $74, $20, $6F, $72, $69, $67, $69, $6E, $3D, $22, $30, $22, $20, $73, $70, $61, $63, $65, $3D, $22, $32, $35, $32, $22, $3E,    // <segment origin="0" space="252">
    $3C, $6E, $61, $6D, $65, $3E, $4F, $70, $74, $69, $6F, $6E, $73, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Options</name>
    $3C, $67, $72, $6F, $75, $70, $3E,    // <group>
    $3C, $6E, $61, $6D, $65, $3E, $4D, $61, $6E, $75, $66, $61, $63, $74, $75, $72, $65, $72, $20, $49, $6E, $66, $6F, $72, $6D, $61, $74, $69, $6F, $6E, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Manufacturer Information</name>
    $3C, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E, $4D, $61, $6E, $75, $66, $61, $63, $74, $75, $72, $65, $72, $2D, $70, $72, $6F, $76, $69, $64, $65, $64, $20, $66, $69, $78, $65, $64, $20, $6E, $6F, $64, $65, $20, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3C, $2F, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E,    // <description>Manufacturer-provided fixed node description</description>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $56, $65, $72, $73, $69, $6F, $6E, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Version</name>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $73, $74, $72, $69, $6E, $67, $20, $73, $69, $7A, $65, $3D, $22, $31, $32, $22, $3E,    // <string size="12">
    $3C, $6E, $61, $6D, $65, $3E, $4D, $61, $6E, $75, $66, $61, $63, $74, $75, $72, $65, $72, $20, $4E, $61, $6D, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Manufacturer Name</name>
    $3C, $2F, $73, $74, $72, $69, $6E, $67, $3E,    // </string>
    $3C, $73, $74, $72, $69, $6E, $67, $20, $73, $69, $7A, $65, $3D, $22, $36, $22, $3E,    // <string size="6">
    $3C, $6E, $61, $6D, $65, $3E, $4D, $61, $6E, $75, $66, $61, $63, $74, $75, $72, $65, $72, $20, $49, $6E, $66, $6F, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Manufacturer Info</name>
    $3C, $2F, $73, $74, $72, $69, $6E, $67, $3E,    // </string>
    $3C, $73, $74, $72, $69, $6E, $67, $20, $73, $69, $7A, $65, $3D, $22, $34, $22, $3E,    // <string size="4">
    $3C, $6E, $61, $6D, $65, $3E, $48, $61, $72, $64, $77, $61, $72, $65, $20, $56, $65, $72, $73, $69, $6F, $6E, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Hardware Version</name>
    $3C, $2F, $73, $74, $72, $69, $6E, $67, $3E,    // </string>
    $3C, $73, $74, $72, $69, $6E, $67, $20, $73, $69, $7A, $65, $3D, $22, $34, $22, $3E,    // <string size="4">
    $3C, $6E, $61, $6D, $65, $3E, $53, $6F, $66, $74, $77, $61, $72, $65, $20, $56, $65, $72, $73, $69, $6F, $6E, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Software Version</name>
    $3C, $2F, $73, $74, $72, $69, $6E, $67, $3E,    // </string>
    $3C, $2F, $67, $72, $6F, $75, $70, $3E,    // </group>
    $3C, $2F, $73, $65, $67, $6D, $65, $6E, $74, $3E,    // </segment>
    $3C, $73, $65, $67, $6D, $65, $6E, $74, $20, $6F, $72, $69, $67, $69, $6E, $3D, $22, $30, $22, $20, $73, $70, $61, $63, $65, $3D, $22, $32, $35, $31, $22, $3E,    // <segment origin="0" space="251">
    $3C, $6E, $61, $6D, $65, $3E, $4F, $70, $74, $69, $6F, $6E, $73, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Options</name>
    $3C, $67, $72, $6F, $75, $70, $20, $6F, $66, $66, $73, $65, $74, $3D, $22, $30, $22, $3E,    // <group offset="0">
    $3C, $6E, $61, $6D, $65, $3E, $55, $73, $65, $72, $20, $44, $61, $74, $61, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>User Data</name>
    $3C, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E, $41, $64, $64, $20, $79, $6F, $75, $72, $20, $6F, $77, $6E, $20, $75, $6E, $69, $71, $75, $65, $20, $6E, $6F, $64, $65, $20, $69, $6E, $66, $6F, $20, $68, $65, $72, $65, $3C, $2F, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E,    // <description>Add your own unique node info here</description>
    $3C, $69, $6E, $74, $20, $73, $69, $7A, $65, $3D, $22, $31, $22, $3E,    // <int size="1">
    $3C, $6E, $61, $6D, $65, $3E, $56, $65, $72, $73, $69, $6F, $6E, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>Version</name>
    $3C, $2F, $69, $6E, $74, $3E,    // </int>
    $3C, $73, $74, $72, $69, $6E, $67, $20, $73, $69, $7A, $65, $3D, $22, $36, $33, $22, $3E,    // <string size="63">
    $3C, $6E, $61, $6D, $65, $3E, $55, $73, $65, $72, $20, $4E, $61, $6D, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>User Name</name>
    $3C, $2F, $73, $74, $72, $69, $6E, $67, $3E,    // </string>
    $3C, $73, $74, $72, $69, $6E, $67, $20, $73, $69, $7A, $65, $3D, $22, $36, $34, $22, $3E,    // <string size="64">
    $3C, $6E, $61, $6D, $65, $3E, $55, $73, $65, $72, $20, $44, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>User Description</name>
    $3C, $2F, $73, $74, $72, $69, $6E, $67, $3E,    // </string>
    $3C, $2F, $67, $72, $6F, $75, $70, $3E,    // </group>
    $3C, $2F, $73, $65, $67, $6D, $65, $6E, $74, $3E,    // </segment>
    $3C, $2F, $63, $64, $69, $3E, $00   // </cdi>
  );

implementation

end.
