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


 // **************************************************************************************************************************************************************
 // ACDI Manufacturer Memory ($FC) Space Implementation
 // **************************************************************************************************************************************************************
const
  USER_ACDI_MFG_VERSION = 1;

const
  USER_MAX_ACDI_MFG_ARRAY = 27;
  USER_ACDI_MFG_STRINGS: array[0..USER_MAX_ACDI_MFG_ARRAY - 1] of byte = (
      $01,                                              // Version ID
      $4D,$75,$73,$74,$61,$6E,$67,$70,$65,$61,$6B,$00,  // Mustangpeak
      $43,$53,$31,$30,$30,$00,  // CS100
      $30,$2E,$31,$00,  // 0.1
      $30,$2E,$33,$00  // 0.3
    );

    // **************************************************************************************************************************************************************
  // ACDI User Memory ($FB) Space Implementation
  //   Defines the Configuration Memory size and offsets into the configuration memory
  //   where the user definable string is found.  There are 2 user definable strings:
  //     User Name:
  //     User Description:
  //   The spec defines a version number which is 1 byte so a datagram can carry
  //   the version number + the Name Data (63 bytes).  The example
  //   uses an offset of 0 into the memory for the Name as well.  The Description
  //   will fill the next datagram (64 Bytes) and the offset is 63 since it will
  //   occupy the memory immediately following the Name in this example.  This
  //   all can be overridden in the template_configuration.mpas file
  // **************************************************************************************************************************************************************
const
  USER_ACDI_USER_VERSION = 1;

const
  USER_MAX_ACDI_USER_NAME_CONFIG_DATA_OFFSET = 0;
  USER_MAX_ACDI_USER_NAME_CONFIG_DATA = 63;      // This MUST MATCH the value defined in the CDI segment for $FB
  USER_MAX_ACDI_USER_DESC_CONFIG_DATA = 64;      // This MUST MATCH the value defined in the CDI segment for $FB


  // **************************************************************************************************************************************************************
  // CDI Memory ($FF) Space Implementation
  // **************************************************************************************************************************************************************
const
  USER_MAX_CDI_ARRAY = 1724 + 14 + 14;
  USER_CDI_ARRAY: array[0..USER_MAX_CDI_ARRAY-1] of byte = (
    $3C, $3F, $78, $6D, $6C, $20, $76, $65, $72, $73, $69, $6F, $6E, $3D, $22, $31, $2E, $30, $22, $20, $65, $6E, $63, $6F, $64, $69, $6E, $67, $3D, $22, $75, $74, $66, $2D, $38, $22, $3F, $3E,    // <?xml version="1.0" encoding="utf-8"?>
    $3C, $3F, $78, $6D, $6C, $2D, $73, $74, $79, $6C, $65, $73, $68, $65, $65, $74, $20, $74, $79, $70, $65, $3D, $27, $74, $65, $78, $74, $2F, $78, $73, $6C, $27, $20, $68, $72, $65, $66, $3D, $27, $68, $74, $74, $70, $3A, $2F, $2F, $6F, $70, $65, $6E, $6C, $63, $62, $2E, $6F, $72, $67, $2F, $74, $72, $75, $6E, $6B, $2F, $70, $72, $6F, $74, $6F, $74, $79, $70, $65, $73, $2F, $78, $6D, $6C, $2F, $78, $73, $6C, $74, $2F, $63, $64, $69, $2E, $78, $73, $6C, $27, $3F, $3E,    // <?xml-stylesheet type='text/xsl' href='http://openlcb.org/trunk/prototypes/xml/xslt/cdi.xsl'?>
    $3C, $63, $64, $69, $20, $78, $6D, $6C, $6E, $73, $3A, $78, $73, $69, $3D, $22, $68, $74, $74, $70, $3A, $2F, $2F, $77, $77, $77, $2E, $77, $33, $2E, $6F, $72, $67, $2F, $32, $30, $30, $31, $2F, $58, $4D, $4C, $53, $63, $68, $65, $6D, $61, $2D, $69, $6E, $73, $74, $61, $6E, $63, $65, $22, $20, $78, $73, $69, $3A, $6E, $6F, $4E, $61, $6D, $65, $73, $70, $61, $63, $65, $53, $63, $68, $65, $6D, $61, $4C, $6F, $63, $61, $74, $69, $6F, $6E, $3D, $22, $68, $74, $74, $70, $3A, $2F, $2F, $6F, $70, $65, $6E, $6C, $63, $62, $2E, $6F, $72, $67, $2F, $74, $72, $75, $6E, $6B, $2F, $70, $72, $6F, $74, $6F, $74, $79, $70, $65, $73, $2F, $78, $6D, $6C, $2F, $73, $63, $68, $65, $6D, $61, $2F, $63, $64, $69, $2E, $78, $73, $64, $22, $3E,    // <cdi xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="http://openlcb.org/trunk/prototypes/xml/schema/cdi.xsd">
    $3C, $69, $64, $65, $6E, $74, $69, $66, $69, $63, $61, $74, $69, $6F, $6E, $3E,    // <identification>
    $3C, $6D, $61, $6E, $75, $66, $61, $63, $74, $75, $72, $65, $72, $3E, $4D, $75, $73, $74, $61, $6E, $67, $70, $65, $61, $6B, $3C, $2F, $6D, $61, $6E, $75, $66, $61, $63, $74, $75, $72, $65, $72, $3E,    // <manufacturer>Mustangpeak</manufacturer>
    $3C, $6D, $6F, $64, $65, $6C, $3E, $4E, $41, $31, $30, $30, $3C, $2F, $6D, $6F, $64, $65, $6C, $3E,    // <model>NA100</model>
    $3C, $68, $61, $72, $64, $77, $61, $72, $65, $56, $65, $72, $73, $69, $6F, $6E, $3E, $31, $2E, $30, $3C, $2F, $68, $61, $72, $64, $77, $61, $72, $65, $56, $65, $72, $73, $69, $6F, $6E, $3E,    // <hardwareVersion>1.0</hardwareVersion>
    $3C, $73, $6F, $66, $74, $77, $61, $72, $65, $56, $65, $72, $73, $69, $6F, $6E, $3E, $30, $2E, $32, $3C, $2F, $73, $6F, $66, $74, $77, $61, $72, $65, $56, $65, $72, $73, $69, $6F, $6E, $3E,    // <softwareVersion>0.2</softwareVersion>
    $3C, $6D, $61, $70, $3E,    // <map>
    $3C, $72, $65, $6C, $61, $74, $69, $6F, $6E, $3E,    // <relation>
    $3C, $70, $72, $6F, $70, $65, $72, $74, $79, $3E, $44, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3C, $2F, $70, $72, $6F, $70, $65, $72, $74, $79, $3E,    // <property>Description</property>
    $3C, $76, $61, $6C, $75, $65, $3E, $4D, $75, $73, $74, $61, $6E, $67, $70, $65, $61, $6B, $20, $4E, $43, $45, $20, $41, $64, $61, $70, $74, $65, $72, $20, $4E, $6F, $64, $65, $3C, $2F, $76, $61, $6C, $75, $65, $3E,    // <value>Mustangpeak NCE Adapter Node</value>
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
    $3C, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E, $4F, $70, $74, $69, $6F, $6E, $73, $20, $66, $6F, $72, $20, $74, $68, $65, $20, $4E, $43, $45, $20, $74, $6F, $20, $4F, $70, $65, $6E, $4C, $43, $42, $20, $41, $64, $61, $70, $74, $65, $72, $3C, $2F, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E,    // <description>Options for the NCE to OpenLCB Adapter</description>
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
    $3C, $2F, $73, $65, $67, $6D, $65, $6E, $74, $3E,    // </segment>
    $3C, $73, $65, $67, $6D, $65, $6E, $74, $20, $6F, $72, $69, $67, $69, $6E, $3D, $22, $30, $22, $20, $73, $70, $61, $63, $65, $3D, $22, $32, $35, $32, $22, $3E,    // <segment origin="0" space="252">
    $3C, $6E, $61, $6D, $65, $3E, $4F, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>O</name> TEMPORARY TO PASS VALIDATION
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
    $3C, $6E, $61, $6D, $65, $3E, $4F, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>O</name> TEMPORARY TO VALIDATE
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

// **************************************************************************************************************************************************************
// Size of Configuration Memory block,
//  This may need to be a multiple of some 2^n number depending on what the
//  harware implementation of the Configuration Memory is.  This allows the library
//  to linearly line up the configruation memory block for multiple nodes, i.e.
//  defines the offsets to the start of the configuration memory for the next
//  virtual node
//  xxxxxxxxxxxxxxxxxxxx|yyyyyyyyy|yyyyyyyyy|yyyyyyyyy|yyyyyyyyy|
//  |   Physical Node   | VNode 1 | Vnode 2 | Vnode 3 | Vnode 4 |
//  | USER_NODE_SIZE    |     USER_VNODE_SIZE * 4               |
// **************************************************************************************************************************************************************
const
  USER_CONFIGURATION_MEMORY_SIZE = 256;

implementation

end.
