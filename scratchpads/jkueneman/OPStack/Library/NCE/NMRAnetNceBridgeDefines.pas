// ******************************************************************************
//
// * Copyright:
//     (c) Mustangpeak Software 2014.
//
//     The contents of this file are subject to the GNU GPL v3 licence/ you maynot use
//     this file except in compliance with the License. You may obtain a copy of the
//     License at http://www.gnu.org/licenses/gpl.html
//
// * Revision History:
//     2011-01-28:   Created
//     2012-10-07:   Version 1.0
//
// * Description:
//    Defines global constants for NCE Bus Bridge
//
// ******************************************************************************
unit NMRAnetNceBridgeDefines;

{$IFDEF FPC}
{$mode objfpc}{$H+}

interface
{$ENDIF}

uses
  opstackdefines,
  template_node;

const
  NCEBUS_MAX_DATA_BYTE = 12;                                                    // Max number of databytes in a NCE message
  
  // Olcb bus statemachine
  STATE_WAIT_FOR_USER_MESSAGE     = 0;

  STATE_RS485_READ_CAB_KEY_PRESS   = 0;  // State machine states for the RS485 receiver
  STATE_RS485_READ_CAB_SPEED       = 1;
  STATE_RS485_CONTINUE_INPUT       = 2;
  STATE_RS485_FULL                 = 3;

  ID_NO_DEVICE                         = $FE;
  ID_MIN_DEVICE                        = 2;
  {$IFDEF FPC}
  ID_MAX_DEVICE                        = 5;   // 63 Devices on NceBus bus allowed not including ID = 0 or 1
  {$ELSE}
  ID_MAX_DEVICE                        = 63;   // 63 Devices on NceBus bus allowed not including ID = 0 or 1
  {$ENDIF}
  NCEBUS_PING                          = %10000000;   // P10A AAAA
  MAX_MISSED_PINGS_TO_REMOVE_FROM_BUS  = 12;          // not supported as NCE cabs can't be on the bus alone they need pings to other cabs to work.....bit 4/7/2013
  {$IFDEF FPC}
  NCE_CAB_BUS_PADDING                  = 2;
  {$ELSE}
  NCE_CAB_BUS_PADDING                  = 8;          // How many fixed Cab objects to always have on the bus to Ping, NCE Cabs can't be on the bus alone and a NCE CS has 8 always available.
  {$ENDIF}
  REDISCOVERY_TIME         = 50;     // = Slow timer count (~100ms * REDISCOVERY_TIME = time to rescan for new Devices)
  
  CS_ALLOCATED                                  = $01;  // The Cab is allocated from the RAM buffer
  CS_MACRO_MESSAGE                              = $08;
  CS_LOCO_SELECT                                = $10;

  NCE_NO_SPEED_TO_REPORT    = %01111111;              // $7F
  NCE_NO_KEY_TO_REPORT      = %01111101;              // $7D
  NCE_SPEED_MASK            = %01111111;              // $7F
  
  NCE_CAB_SELECT_LOCO       = $48;   // 72
  NCE_CAB_ENTER             = $40;   // 64   
  NCE_CAB_DIR_TOGGLE        = $43;
  NCE_HORN_KEY_DOWN         = $49;
  NCE_CAB_ONE_STEP_FASTER   = $4A;
  NCE_CAB_ONE_STEP_SLOWER   = $4B;
  NCE_CAB_EMERGENCY_STOP    = $4C;
  NCE_CAB_BELL              = $4D;
  NCE_CAB_TOGGLE_F0_0       = $50;
  NCE_CAB_TOGGLE_F1_1       = $51;
  NCE_CAB_TOGGLE_F2_2       = $52;
  NCE_CAB_TOGGLE_F3_3       = $53;
  NCE_CAB_TOGGLE_F4_4       = $54;
  NCE_CAB_TOGGLE_F5_5       = $55;
  NCE_CAB_TOGGLE_F6_6       = $56;
  NCE_CAB_TOGGLE_F7_7       = $57;
  NCE_CAB_TOGGLE_F8_8       = $58;
  NCE_CAB_9                 = $59;
  NCE_HORN_KEY_UP           = $5F;
  NCE_CAB_FIVE_STEPS_FASTER = $5A;
  NCE_CAB_FIVE_STEPS_SLOWER = $5B;
  NCE_CAB_SELECT_MACRO      = $5C;
  NCE_CAB_DIR_FORWARD       = $6A;
  NCE_CAB_DIR_REVERSE       = $6B;

  
  NCE_CMD_CURSOR_ON         = $CF;  // 207
  NCE_CMD_CURSOR_OFF        = $CE;  // 206

type
  TIncomingBuffer = array[0..1] of Byte;
  TCabMessage = array[0..NCEBUS_MAX_DATA_BYTE] of Byte;
  
  TNceMessage = record
    DataBytes: TCabMessage;
    Count: Byte;                                      // Number of valid bytes in the array
    Full: Boolean;                                    // True if the message is ready to be process
  end;
  PNceMessage = ^TNceMessage;

  TNceCab = record
    State: Byte;                                     // See CS_xxxx constants
    ID: Byte;                                        // Cab Address (ID/Index)
    IncomingMsg: TNceMessage;                        // Structure to hold the info for an incoming message from the CAb
    iStateMachine: Byte;                             // SubStatemachine index
    WatchDog: Word;
  end;
  PNceCab = ^TNceCab;
  
  TAssignedCabArray = array[0..USER_MAX_NODE_COUNT-2] of PNMRAnetNode;          // Only Virtual Nodes make sense, skip the physical server node
  
  TNceBridge = record
    Discovering: Boolean;                        // The bridge is pinging all Cab IDs looking for newly added Cabs
    iDiscoveryCabID: Word;                       // The index of the Cab while discovering
    iStateMachine: Byte;                         // Main Statemachine index
    DiscoverTimer: Byte;                         // Counter to detect when to start a Discovery cycle
    IncomingBuffer: TIncomingBuffer;             //
    iIncomingCount: Byte;
    LastPortRead: Word;                          // Pin Change testing
    AssignedCabs: TAssignedCabArray;
    iAssignedCabCount: Integer;                  // Number of valid Cabs in the AssignedCabs array
    iActiveCab: Integer;
    ExistingCab: PNMRAnetNode;
  end;
  
  // Array of User Data for all Nodes ( there will be one extra since the Root Node is differnet and not a CAB
  TNceCabArray = array[0..USER_MAX_NODE_COUNT-1] of TNceCab;
  
procedure CabBridge_Initialize;

var
  NceCabArray: TNceCabArray;
  NceBridge: TNceBridge;

implementation

procedure CabBridge_Initialize;
begin
  NceBridge.Discovering := False;
  NceBridge.iDiscoveryCabID := 0;
  NceBridge.iStateMachine := 0;
  NceBridge.DiscoverTimer := 0;
  NceBridge.iIncomingCount := 0;
  NceBridge.LastPortRead := 0;
  NceBridge.iAssignedCabCount := 0;
  NceBridge.iActiveCab := 0;
  NceBridge.ExistingCab := nil;
end;

end.