unit template_userstatemachine;

{$IFDEF FPC}
{$mode objfpc}{$H+}

interface
{$ENDIF}

{$I Options.inc}

uses
  {$IFDEF FPC}
  Classes,
  SysUtils,
  FileUtil,
  ethernet_hub,
  olcb_transport_layer,
  template_hardware,
  olcb_defines,
 // LCLIntf,
 // LCLType,
  {$ENDIF}
  Float16,
  opstacktypes,
  opstackdefines,
  template_node,
  opstack_api,
  nmranetdefines,
  NMRAnetNceBridgeDefines,
  nmranetutilities;

procedure UserStateMachine_Initialize;
procedure AppCallback_UserStateMachine_Process(Node: PNMRAnetNode);
procedure AppCallback_NodeInitialize(Node: PNMRAnetNode);

// Called every 100ms typically from another thread so only use to update flags
procedure AppCallback_Timer_100ms;

// These message are called from the mainstatemachine loop.  They have been stored in
// internal storage buffers.  See the notes to understand the implications of this and how to use them correctly
procedure AppCallback_SimpleNodeInfoReply(Node: PNMRAnetNode; AMessage: POPStackMessage);
procedure AppCallBack_ProtocolSupportReply(Node: PNMRAnetNode; AMessage: POPStackMessage);  // This could be 2 replies per call.. read docs
procedure AppCallback_RemoteButtonReply(Node: PNMRAnetNode; var Source: TNodeInfo; DataBytes: PSimpleBuffer);
{$IFDEF SUPPORT_TRACTION}
procedure AppCallback_TractionProtocol(Node: PNMRAnetNode; AMessage: POPStackMessage);
procedure AppCallback_TractionProtocolReply(Node: PNMRAnetNode; AMessage: POPStackMessage);
procedure AppCallback_SimpleTrainNodeInfoReply(Node: PNMRAnetNode; AMessage: POPStackMessage);
{$ENDIF}
{$IFDEF SUPPORT_TRACTION_PROXY}
function AppCallback_TractionProxyProtocol(Node: PNMRAnetNode; AMessage: POPStackMessage; SourceHasLock: Boolean): Boolean;
procedure AppCallback_TractionProxyProtocolReply(Node: PNMRAnetNode; AMessage: POPStackMessage);
{$ENDIF}

// These messages are called directly from the hardware receive buffer.  See the notes to understand the
// implications of this and how to use them correctly
procedure AppCallback_InitializationComplete(var Source: TNodeInfo; NodeID: PNodeID);
procedure AppCallback_VerifiedNodeID(var Source: TNodeInfo; NodeID: PNodeID);
procedure AppCallback_ConsumerIdentified(var Source: TNodeInfo; MTI: Word; EventID: PEventID);
procedure AppCallback_ProducerIdentified(var Source: TNodeInfo; MTI: Word; EventID: PEventID);
procedure AppCallback_LearnEvent(var Source: TNodeInfo; EventID: PEventID);
procedure AppCallBack_PCEventReport(var Source: TNodeInfo; EventID: PEventID);

{$IFNDEF FPC}
  function OPStackNode_Allocate: PNMRAnetNode; external;
  procedure OPStackNode_MarkForRelease(Node: PNMRAnetNode); external;
  function OPStackNode_Find(AMessage: POPStackMessage; FindBy: Byte): PNMRAnetNode;   external;   // See FIND_BY_xxxx constants
  function OPStackNode_FindByTrainID(TrainID: Word): PNMRANetNode; external;
  function OPStackNode_FindByAlias(AliasID: Word): PNMRAnetNode; external;
  function OPStackNode_FindByID(var ID: TNodeID): PNMRAnetNode; external;
  procedure TractionProxyProtocolReply(Node: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage); external;
  procedure TractionProtocolReply(Node: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage); external;
{$ELSE}
type
  TPingCabEmulatorFunc = function(CabAddress: Word; Node: PNMRAnetNode): Boolean of object;

  procedure UART_RX_StateMachine(AChar: Byte);
{$ENDIF}

{$IFDEF FPC}
const
  SYNC_NONE                     = $0000;
  SYNC_NEW_THROTTLE             = $0001;

  SYNC_NODE_INFO                = $0010;
  SYNC_CONTROLLER               = $0020;
  SYNC_CLOSING                  = $0040;
  SYNC_CLOSED                   = $0080;

  SYNC_STATE_SPEED_DIR          = $0100;
  SYNC_STATE_FUNCTIONS          = $0200;
  SYNC_STATE_ADDRESS            = $0400;
  SYNC_SPEED_STEPS              = $0800;

type
  TSyncRec = record
    Node: PNMRAnetNode;
    State: Word;             // SYNC_xxxxx constant
    ObjPtr: TObject;         // Points to an object to link GUI items with the node
  end;
  PSyncRec = ^TSyncRec;

function ThrottleItem(iIndex: Integer): PSyncRec;

var
  OPStackCriticalSection: TRTLCriticalSection;
  ThrottleList: TList;
  PingEnumlatorFunc: TPingCabEmulatorFunc;
{$ELSE}

procedure CabBus_Timeout;
procedure UART_RX_StateMachine;
procedure PinChangeInterrupt;

var
  CabBus_RS485_Select              : sbit; sfr; external;
  CabBus_RS485_Select_Direction    : sbit; sfr; external;
{$ENDIF}

const
  FUNCTION_HORN = 1;
  FUNCTION_BELL = 2;

const
  CONFIG_OFFSET_SPEED_STEP     = 128;
  CONFIG_OFFSET_ADDRESS_TYPE   = 129;

const
  STATE_USER_START              = 0;
  STATE_FIND_PROXY              = 1;
  STATE_CREATE_REQUIRED_CABS    = 2;
  STATE_DISCOVERDEVICES         = 3;
  STATE_KNOWNDEVICES            = 4;

  STATE_INITIALIZE           = 0;
  STATE_SEND_SYNC            = 1;
  STATE_WAIT_SYNC_DELAY      = 2;
  STATE_SEND_PING            = 3;
  STATE_WAIT_FOR_RESPONSE    = 4;
  STATE_TIMEOUT              = 5;
  STATE_CAB_REPLIED          = 6;
  STATE_DISPATCH_MESSAGE     = 7;
  STATE_NEXT_CAB             = 8;

  STATE_CAB_IDLE             = 1;
  STATE_CAB_SELECT_LOCO      = 2;
  STATE_CAB_RUN_MACRO        = 3;
  STATE_CAB_RUN_CMD          = 4;
  STATE_CAB_CLEAR_MSG        = 5;


  STATE_CAB_SELECT_LOCO_SEND_PROXY_MANAGE_LOCK               = 1;
  STATE_CAB_SELECT_LOCO_SEND_PROXY_ALLOCATE                  = 2;
  STATE_CAB_SELECT_LOCO_SEND_PROXY_MANAGE_UNLOCK             = 3;
  STATE_CAB_SELECT_LOCO_SEND_TRACTION_MANAGE_LOCK            = 4;
  STATE_CAB_SELECT_LOCO_SEND_TRACTION_ASSIGN_CONTROLLER      = 5;
  STATE_CAB_SELECT_LOCO_SEND_TRACTION_QUERY_SPEED            = 6;
  STATE_CAB_SELECT_LOCO_SEND_TRACTION_QUERY_FUNCTIONS        = 7;
  STATE_CAB_SELECT_LOCO_SEND_TRACTION_MANAGE_UNLOCK          = 8;
  STATE_CAB_SELECT_LOCO_GENERIC_REPLY_WAIT                   = 20;
  STATE_CAB_SELECT_LOCO_GENERIC_TIMEOUT_PROXY_UNLOCK         = 21;
  STATE_CAB_SELECT_LOCO_GENERIC_TIMEOUT_TRACTION_UNLOCK      = 22;

implementation

{$IFDEF FPC}
  {$IFDEF SUPPORT_TRACTION and SUPPORT_TRACTION_PROXY}
  uses
    opstacknode,
    opstackcore_traction,
    opstackcore_traction_proxy;
  {$ELSE}
    {$IFDEF SUPPORT_TRACTION}
    uses
      opstackcore_traction;
    {$ELSE}
    uses
      opstackcore_traction_proxy;
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

var
  ProxyNode: TNodeInfo;
  GlobalTimer: Word;

{$IFDEF FPC}
procedure ZeroSyncRec(SyncRec: PSyncRec);
begin
  SyncRec^.Node := nil;
  SyncRec^.State := 0;
  SyncRec^.ObjPtr := nil;
end;

function ThrottleItem(iIndex: Integer): PSyncRec;
begin
  Result := nil;
  if iIndex < ThrottleList.Count then
    Result := PSyncRec( ThrottleList.Items[iIndex])
end;

function FindSync(Node: PNMRAnetNode): PSyncRec;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to ThrottleList.Count - 1 do
  begin
    if PSyncRec( ThrottleList.Items[i])^.Node = Node then
    begin
      Result := PSyncRec( ThrottleList.Items[i]);
      Break
    end;
  end;
end;

procedure SetSync(Node: PNMRAnetNode; SyncCode: Word);
var
  Train: PSyncRec;
begin
  Train := FindSync(Node);
  if Assigned(Train) then
    Train^.State := Train^.State or SyncCode;
end;
{$ENDIF}


{$IFNDEF FPC}

procedure PrintNodeInfo(NodeInfo: PNodeInfo);
begin
  WordToHex(NodeInfo^.AliasID, s1 + LF);
  UART1_Write_Text('Alias: 0x' + s1 + LF);
  LongWordToHex( (NodeInfo^.ID[1] shl 16) or NodeInfo^.ID[0], s1);
  UART1_Write_Text('Node ID: 0x' + s1 + LF);
end;

procedure PrintIncomingMsg(var IncomingMsg: TNceMessage);
var
  i: Integer;
begin
  UART1_Write_Text('Incoming Message' +LF);
  ByteToStr(IncomingMsg.Count, s1);
  UART1_Write_Text('Count: ' + s1 + LF);
  if IncomingMsg.Full then
    UART1_Write_Text('Full = True'+ LF)
  else
    UART1_Write_Text('Full = False'+ LF);
  UART1_Write_Text('Data = ');
  for i := 0 to IncomingMsg.Count - 1 do
  begin
    ByteToStr(IncomingMsg.DataBytes[i], s1);
    if i <  IncomingMsg.Count - 1 then
      UART1_Write_Text('0x' + s1 + ', ')
    else
      UART1_Write_Text('0x' + s1);
  end;
  UART1_Write_Text(LF+LF);
end;

// ****************************************************************************
// *****************************************************************************
procedure WriteUARTByte(DataByte: Byte; TxReg: ^Word; StatusReg: ^Word; Wait: Boolean);
begin
  TxReg^ := DataByte;
  StatusReg^.UTXEN := 1;          // Force the Register in to the TSR so the Idle check is not "too fast" to start
  if Wait then
  begin
    while StatusReg^.TRMT = 1 do;   // Wait for the UART to start transmitting
    while StatusReg^.TRMT = 0 do;   // Wait for the UART to finsh transmitting to make sure the ENceBus timing is met
  end
end;

// ****************************************************************************
// *****************************************************************************
procedure WriteByte(DataByte: Byte; Wait: Boolean);
begin
  CabBus_RS485_Select := 1;      // Select the 485 chip to transmit mode
  WriteUARTByte(DataByte, @U2TXREG, @U2STA, Wait);
  CabBus_RS485_Select := 0;      // Select the 485 chip to receive mode
end;

// ****************************************************************************
// *****************************************************************************
procedure FlushUartReceiver;
var
  Temp: Word;
begin
  while (URXDA_U2STA_bit = 1) do Temp := U2RXREG;             // Flush the RX Buffer
  U2RXIF_Bit := 0;                                            // Reset the hardware RX statemachine
end;

// ****************************************************************************
// *****************************************************************************
procedure EnableCabBusTimer(Time: Word);
begin
  TON_T4CON_bit := 0;      // Turn off
  T4IE_bit := 0;           // Disable the Interrupt
  T4IF_bit := 0;           // Clear T1IF
  T4IE_bit := 1;           // Enable the Interrupt
  PR4 := Time;             //
  TMR4 := 0;
  TON_T4CON_bit := 1;      // Turn on
end;

// ****************************************************************************
// *****************************************************************************
procedure DisableCabBusTimer;
begin
  TON_T4CON_bit := 0;      // Turn off
  T4IE_bit := 0;           // Disable the Interrupt
  TON_T4CON_bit := 0;      // Turn off
end;

// *****************************************************************************
// Called from Dynamically set Interrupt timer after 1800us
// *****************************************************************************
procedure CabBus_Timeout;
begin
  // The Ping timed out without a reply from the Cab move to the next state (depends of which wait state we are in)
  DisableCabBusTimer;
  case NceBridge.iStateMachine of
    STATE_WAIT_SYNC_DELAY   : NceBridge.iStateMachine := STATE_SEND_PING;
    STATE_WAIT_FOR_RESPONSE : NceBridge.iStateMachine := STATE_TIMEOUT;
  end;
end;

// *****************************************************************************
// *****************************************************************************
procedure PinChangeInterrupt;
begin
  CNIE_bit := 0;                                                                // Pin Change Interrupt disabled
  DisableCabBusTimer;
end;

{$ENDIF}

// *****************************************************************************
// Called from the UART RX Interrupt
// The Pin Change Interrupt stopped the Timeout Timer so we are free to handle
// the UART RX at our leasure
// *****************************************************************************
{$IFDEF FPC}
procedure UART_RX_StateMachine(AChar: Byte);
{$ELSE}
procedure UART_RX_StateMachine;
{$ENDIF}
begin
  {$IFNDEF FPC}
  while (URXDA_U2STA_bit = 1) do
  {$ENDIF}
  begin
    {$IFDEF FPC}
    NceBridge.IncomingBuffer[NceBridge.iIncomingCount] := AChar;
    {$ELSE}
    NceBridge.IncomingBuffer[NceBridge.iIncomingCount] := U2RXREG;
    {$ENDIF}
    Inc(NceBridge.iIncomingCount);
    if NceBridge.iIncomingCount = 2 then
      NceBridge.iStateMachine := STATE_CAB_REPLIED                   // Received both Bytes process them
  end;
end;

{$IFDEF FPC}
function NceKeyPressToString(KeyPress: Byte): ansistring;
{$ELSE}
function NceKeyPressToString(KeyPress: Byte): string[64];
{$ENDIF}
begin
  case KeyPress of
    NCE_CAB_SELECT_LOCO       : Result := 'Select Loco';
    NCE_CAB_ENTER             : Result := 'Enter';
    NCE_CAB_DIR_TOGGLE        : Result := 'Direction Toggle';
    NCE_HORN_KEY_DOWN         : Result := 'Horn Key Down';
    NCE_CAB_ONE_STEP_FASTER   : Result := 'One Step Faster';
    NCE_CAB_ONE_STEP_SLOWER   : Result := 'One Step Slower';
    NCE_CAB_EMERGENCY_STOP    : Result := 'Emergency Stop';
    NCE_CAB_BELL              : Result := 'Cab Bell';
    NCE_CAB_TOGGLE_F0_0       : Result := '0/Toggle F0';
    NCE_CAB_TOGGLE_F1_1       : Result := '1/Toggle F1';
    NCE_CAB_TOGGLE_F2_2       : Result := '2/Toggle F2';
    NCE_CAB_TOGGLE_F3_3       : Result := '3/Toggle F3';
    NCE_CAB_TOGGLE_F4_4       : Result := '4/Toggle F4';
    NCE_CAB_TOGGLE_F5_5       : Result := '5/Toggle F5';
    NCE_CAB_TOGGLE_F6_6       : Result := '6/Toggle F6';
    NCE_CAB_TOGGLE_F7_7       : Result := '7/Toggle F7';
    NCE_CAB_TOGGLE_F8_8       : Result := '8/Toggle F8';
    NCE_CAB_9                 : Result := '9';
    NCE_HORN_KEY_UP           : Result := 'Horn Key Up';
    NCE_CAB_FIVE_STEPS_FASTER : Result := '5 Speed Steps Faster';
    NCE_CAB_FIVE_STEPS_SLOWER : Result := '5 Speed Steps Slower';
    NCE_CAB_SELECT_MACRO      : Result := 'Macro';
    NCE_CAB_DIR_FORWARD       : Result := 'Forward Direction';
    NCE_CAB_DIR_REVERSE       : Result := 'Reverse Direction'
  else
    Result := 'Unknown Command';
  end;
end;

// ****************************************************************************
// *****************************************************************************
function ChangeSpeed(Speed: THalfFloat; DeltaInRealSpeed: {$IFDEF FPC}single{$ELSE}real{$ENDIF}): Word;
var
  IsReverse: Boolean;
  SpeedReal:{$IFDEF FPC}single{$ELSE}real{$ENDIF};
begin
  IsReverse := Speed and $8000 <> 0;
  SpeedReal := HalfToFloat( Speed and not $8000);

  SpeedReal := SpeedReal + (DeltaInRealSpeed);
  if SpeedReal > 100.0 then
    SpeedReal := 100.0
  else
  if SpeedReal < 0.0 then
    SpeedReal := 0.0;

  Result := FloatToHalf(SpeedReal);
  if IsReverse then
    Result := Result or $8000;
end;

function ToggleFunction(Node: PNMRAnetNode; var Functions: DWord; FunctionAddress: Byte): Boolean;
var
  Mask: DWord;
begin
  Result := False;
  Mask := $00000001;
  Mask := Mask shl FunctionAddress;
  if (Functions and Mask) <> 0 then
  begin
    if TrySendTractionFunctionSet(Node^.Info, Node^.TrainData.LinkedNode, FunctionAddress, 0) then
    begin
      Mask := not Mask;
      Functions := Functions and Mask;
      Result := True
    end;
  end else
  begin
    if TrySendTractionFunctionSet(Node^.Info, Node^.TrainData.LinkedNode, FunctionAddress, 1) then
    begin
      Functions := Functions or Mask;
      Result := True
    end;
  end;
end;

// ****************************************************************************
// *****************************************************************************
function CabMessageToTrainAddress(CabMessage: PNceMessage): Word;
var
  i: Integer;
  AddressStr: string[NCEBUS_MAX_DATA_BYTE];
begin
  Result := 0;
  AddressStr := '';
  for i := 0 to CabMessage^.Count - 1 do
    AddressStr := AddressStr + Char( CabMessage^.DataBytes[i] and $0F or $30);   // Make them ASCII
  {$IFDEF FPC}
  Result := StrToInt(AddressStr);
  {$ELSE}
  Result := StrToWord(AddressStr);
  {$ENDIF}
  if (AddressStr[0] = '0') or (Result > 127) then
    Result := Result or $C000;
end;

// ****************************************************************************
// *****************************************************************************
procedure ZeroizeNceCabData(CabData: PNceCab);
var
  i, j: Integer;
begin
  CabData^.State := 0;
  CabData^.IncomingMsg.Count := 0;
  CabData^.IncomingMsg.Full := False;
  CabData^.iStateMachine := 0;
  CabData^.ID := 0;
  for j := 0 to NCEBUS_MAX_DATA_BYTE - 1 do
    CabData^.IncomingMsg.DataBytes[j] := 0;
end;

// *****************************************************************************
// *****************************************************************************
function Cab_Allocate: PNceCab;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  while i < USER_MAX_NODE_COUNT do
  begin
    if NceCabArray[i].State and CS_ALLOCATED = 0 then
    begin
      Result := @NceCabArray[i];
      ZeroizeNceCabData(Result);
      NceCabArray[i].State := NceCabArray[i].State or CS_ALLOCATED;
      Exit;
    end;
    Inc(i)
  end
end;

// *****************************************************************************
// *****************************************************************************
procedure Cab_Free(CabData: PNceCab);
begin
if CabData <> nil then
  CabData^.State := CabData^.State and not CS_ALLOCATED
end;

// ****************************************************************************
// *****************************************************************************
function CreateCabNode(CabID: Word): PNMRAnetNode;
var
  CabData: PNceCab;
begin
  Result := nil;
  CabData := Cab_Allocate;
  if CabData <> nil then
  begin
    Result := OPStackNode_Allocate;
    if Result <> nil then
    begin
      Result^.UserData := CabData;
      CabData^.ID := CabID;
    end else
    begin
      Cab_Free(CabData)
    end
  end
end;

// *****************************************************************************
// *****************************************************************************
function CreateCab(CabID: Word): PNMRAnetNode;
var
  CabData: PNceCab;
begin
  Result := CreateCabNode(CabID);
  if Result <> nil then
  begin
    CabData := PNceCab( Result^.UserData);
    NceBridge.AssignedCabs[NceBridge.iAssignedCabCount] := Result;
    Inc(NceBridge.iAssignedCabCount);
    Exit;
  end;
end;

// *****************************************************************************
// *****************************************************************************
function HandleCabBusReply(CabNode: PNMRAnetNode): Boolean;
var
  CabData: PNceCab;
  i: Integer;
begin
  Result := False;

  CabData := PNceCab( CabNode^.UserData);

  // Ignore user input if the cab node is working a statemachine......
  if CabData^.IncomingMsg.Full then
    Exit;

  for i := 0 to NceBridge.iIncomingCount - 1 do
  begin
    case NceBridge.IncomingBuffer[i] of
      NCE_NO_KEY_TO_REPORT : begin end;
      NCE_NO_SPEED_TO_REPORT : begin end;
      NCE_CAB_SELECT_MACRO :
          begin
            {$IFNDEF FPC}WriteByte(NCE_CMD_CURSOR_ON, True);{$ENDIF}
            CabData^.State := CabData^.State or CS_MACRO_MESSAGE;
            CabData^.IncomingMsg.Count := 0;
          end;
      NCE_CAB_SELECT_LOCO :
          begin
            {$IFNDEF FPC}WriteByte(NCE_CMD_CURSOR_ON, True);{$ENDIF}
            CabData^.State := CabData^.State or CS_LOCO_SELECT;
            CabData^.IncomingMsg.Count := 0;
          end;
      NCE_CAB_ENTER :
          begin
            if CabData^.State and (CS_MACRO_MESSAGE or CS_LOCO_SELECT) <> 0 then
            begin
              {$IFNDEF FPC}WriteByte(NCE_CMD_CURSOR_OFF, True);{$ENDIF}
              CabData^.IncomingMsg.Full := True;
              Result := True;
            end;
          end
      else begin
          if CabData^.IncomingMsg.Count < NCEBUS_MAX_DATA_BYTE then
          begin
            CabData^.IncomingMsg.DataBytes[CabData^.IncomingMsg.Count] := NceBridge.IncomingBuffer[i];
            Inc(CabData^.IncomingMsg.Count);
            if CabData^.State and (CS_MACRO_MESSAGE or CS_LOCO_SELECT) = 0 then
            begin
              CabData^.IncomingMsg.Full := True;
              Result := True;
            end;
          end;
        end;
      end; {case}
  end; {if}
end;

// *****************************************************************************
// *****************************************************************************
function DispatchMessage(CabNode: PNMRAnetNode): Boolean;
var
 CabData: PNceCab;
 i: Integer;
 sss: array[0..128] of char;

begin
  Result := False;
  if CabNode <> nil then
  begin
    CabData := PNceCab( CabNode^.UserData);

    if CabData^.State and CS_LOCO_SELECT <> 0 then
    begin
      // Kick off the statemachine to do this....
      CabNode^.iUserStateMachine := STATE_CAB_SELECT_LOCO;
      CabData^.iStateMachine := STATE_INITIALIZE;

   //   UART1_Write_Text('Loco Select: ');
   //   PrintIncomingMsg(CabData^.IncomingMsg);
    end else
    if CabData^.State and CS_MACRO_MESSAGE <> 0 then
    begin
      // Kick off the statemachine to do this....
      CabNode^.iUserStateMachine := STATE_CAB_RUN_MACRO;
      CabData^.iStateMachine := STATE_INITIALIZE;

    //  UART1_Write_Text('Macro: ');
    //  PrintIncomingMsg(CabData^.IncomingMsg);
    end else
    begin
      CabNode^.iUserStateMachine := STATE_CAB_RUN_CMD;
      CabData^.iStateMachine := STATE_INITIALIZE;

  //    UART1_Write_Text('Command: ');
  //    PrintIncomingMsg(CabData^.IncomingMsg);
      for i := 0 to CabData^.IncomingMsg.Count - 1 do
      begin
        sss := NceKeyPressToString(CabData^.IncomingMsg.DataBytes[i]);
    //    UART1_Write_Text( sss + LF);
      end;

    end;
  end;

  Result := True;
  Exit;
end;

// *****************************************************************************
// *****************************************************************************
function FindCab(CabID: Word): PNMRAnetNode;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  while i < NceBridge.iAssignedCabCount do
  begin
    if PNceCab( NceBridge.AssignedCabs[i]^.UserData)^.ID = CabID then
    begin
      Result := NceBridge.AssignedCabs[i];
      Exit;
    end;
    Inc(i);
  end
end;

// *****************************************************************************
//  procedure UserStateMachine_Initialize
//     Parameters: : None
//     Returns     : None
//     Description : Called once when the library is starting.  Use to initalize
//                   variables, etc
// *****************************************************************************
procedure UserStateMachine_Initialize;
var
  i, j: Integer;
begin
  {$IFNDEF FPC}
  CabBus_RS485_Select := 0;                // Default in Receive
  CabBus_RS485_Select_Direction := 0;      // Output
  {$ENDIF}
  for i := 0 to USER_MAX_NODE_COUNT - 1 do
    ZeroizeNceCabData( @NceCabArray[i]);
  NceBridge.Discovering := False;
  NceBridge.iDiscoveryCabID := ID_MIN_DEVICE;
  NceBridge.iStateMachine := STATE_USER_START;
  NceBridge.DiscoverTimer := 0;
  NceBridge.iAssignedCabCount := 0;
  for i := 0 to USER_MAX_NODE_COUNT - 2 do   // no space for physical node
    NceBridge.AssignedCabs[i] := nil;
  NceBridge.iActiveCab := 0;
  ProxyNode.ID[0] := 0;
  ProxyNode.ID[1] := 0;
  ProxyNode.AliasID := 0;
  GlobalTimer := 0;
end;

// *****************************************************************************
//  procedure AppCallback_UserStateMachine_Process
//     Parameters: : None
//     Returns     : None
//     Description : Called as often as possible to run the user statemachine
// *****************************************************************************
procedure AppCallback_UserStateMachine_Process(Node: PNMRAnetNode);
var
  i: Integer;
  CabData: PNceCab;
  Address: Word;
  SpeedStep, AddressType: Byte;
  NewSpeed: THalfFloat;
begin
  {$IFDEF FPC}EnterCriticalsection(OPStackCriticalSection);{$ENDIF}
  if Node = GetPhysicalNode then
  begin
    case Node^.iUserStateMachine of
      STATE_USER_START :  // Create the minimum number of Pings to put on the NCE bus to make it happy
          begin
            if Node^.State and NS_PERMITTED <> 0 then
            begin
              GlobalTimer := 0;
              if TrySendIdentifyProducer(Node^.Info, @EVENT_IS_PROXY) then
                Node^.iUserStateMachine := STATE_FIND_PROXY;
            end;
            Exit;
          end;
      STATE_FIND_PROXY :   // Find the Proxy node (Command Station) on the network before progressing
          begin
            if (ProxyNode.AliasID > 0) or (ProxyNode.ID[0] > 0) or (ProxyNode.ID[1] > 0) then
              Node^.iUserStateMachine := STATE_CREATE_REQUIRED_CABS
            else begin
              if GlobalTimer > 10 then
                Node^.iUserStateMachine := STATE_USER_START                     // Try again
            end;
            Exit;
          end;
      STATE_CREATE_REQUIRED_CABS :
          begin
            for i := ID_MIN_DEVICE to ID_MIN_DEVICE + NCE_CAB_BUS_PADDING - 1 do
              CreateCab(i);                                                     // Build and assign cabs 2 - N so make the throttle hardware happy
            Node^.iUserStateMachine := STATE_DISCOVERDEVICES;
            Exit;
          end;
      STATE_DISCOVERDEVICES :
          begin
            case NceBridge.iStateMachine of   // I could use the Train Data machine for this if I wanted
              STATE_INITIALIZE :
                  begin
                    {$IFDEF DEBUG_DISCOVER_STATEMACHINE} UART1_Write_Text('STATE_DISCOVER_INITIALIZE'+LF); {$ENDIF}
                    NceBridge.iDiscoveryCabID := ID_MIN_DEVICE;
                    NceBridge.iStateMachine := STATE_SEND_SYNC;
                    Exit;
                  end;
              STATE_SEND_SYNC :
                  begin
                    {$IFDEF DEBUG_DISCOVER_STATEMACHINE} UART1_Write_Text('STATE_DISCOVER_SEND_SYNC..............'+LF); {$ENDIF}
                    {$IFDEF FPC}
                    NceBridge.iStateMachine := STATE_SEND_PING;           // No reason to wait if running on Lazarus
                    {$ELSE}
                    FlushUartReceiver;
                    WriteByte(NCEBUS_PING or 0, True);                                    // Special Ping with a CabID of 0 to Sync the stream
                    EnableCabBusTimer(28800);          // 31.25ns * 28800 = 900us      Needs some delay, the cabs are slow microprocessors
                    NceBridge.iStateMachine := STATE_WAIT_SYNC_DELAY;
                    {$ENDIF}
                    Exit;
                  end;
              STATE_WAIT_SYNC_DELAY :
                  begin  // Waiting for the Timer to Expire and jump us to the next state
                    {$IFDEF DEBUG_DISCOVER_STATEMACHINE} UART1_Write_Text('STATE_DISCOVER_WAIT_SYNC_DELAY'+LF); {$ENDIF}
                    Exit;
                  end;
              STATE_SEND_PING :
                  begin  // The node replied and
                    {$IFDEF DEBUG_DISCOVER_STATEMACHINE} IntToStr(NceBridge.iDiscoveryCabID, s1); UART1_Write_Text('STATE_DISCOVER_SEND_PING: ' + s1+LF); {$ENDIF}
                    NceBridge.ExistingCab := FindCab(NceBridge.iDiscoveryCabID);
                    {$IFDEF FPC}
                      NceBridge.iIncomingCount := 0;
                      if not PingEnumlatorFunc(NceBridge.iDiscoveryCabID, NceBridge.ExistingCab) then  // STATE_CAB_REPLIED will be set by the UART "interrupt" call
                        NceBridge.iStateMachine := STATE_NEXT_CAB;
                    {$ELSE}
                    FlushUartReceiver;
                    NceBridge.iIncomingCount := 0;
                    NceBridge.LastPortRead := PortB;
                    CNIE_bit := 1;                                                        // Pin Change Interrupt enable
                    WriteByte(NCEBUS_PING or NceBridge.iDiscoveryCabID, True);            // Ping the Cab ID
                    EnableCabBusTimer(57600);          // 31.25ns * 57600 = 1800us
                    NceBridge.iStateMachine := STATE_WAIT_FOR_RESPONSE;
                    {$ENDIF}
                    Exit;
                  end;
              STATE_WAIT_FOR_RESPONSE :
                  begin   // Waiting for the Timer to expire or we detect a reply....
                    // See CabBus_Timeout function for the Timeout or CabBus_UART_RX_StateMachine for a reply and what the next state is
                    {$IFDEF DEBUG_DISCOVER_STATEMACHINE} UART1_Write_Text('STATE_DISCOVER_WAIT_FOR_RESPONSE'+LF); {$ENDIF}
                    Exit;
                  end;
              STATE_TIMEOUT :
                  begin
                    {$IFDEF DEBUG_DISCOVER_STATEMACHINE} UART1_Write_Text('STATE_DISCOVER_TIMEOUT'+LF); {$ENDIF}
                    // Need to search the Allocated Cab List to find the NceBridge.iDiscoveryCabID matching Node to pull this
                    NceBridge.iStateMachine := STATE_NEXT_CAB;
                    Exit;
                  end;
              STATE_CAB_REPLIED :
                  begin
                    {$IFDEF DEBUG_DISCOVER_STATEMACHINE} UART1_Write_Text('STATE_DISCOVER_CAB_REPLIED'+LF); {$ENDIF}
                    if NceBridge.ExistingCab = nil then
                    begin
                      CreateCab(NceBridge.iDiscoveryCabID);
                      NceBridge.iStateMachine := STATE_NEXT_CAB
                    end else
                    begin
                      if HandleCabBusReply(NceBridge.ExistingCab) then
                        NceBridge.iStateMachine := STATE_DISPATCH_MESSAGE
                      else
                        NceBridge.iStateMachine := STATE_NEXT_CAB
                    end;
                    Exit;
                  end;
              STATE_DISPATCH_MESSAGE :
                  begin
                    {$IFDEF DEBUG_DISCOVER_STATEMACHINE} UART1_Write_Text('STATE_DISPATCH_MESSAGE'+LF); {$ENDIF}
                    if DispatchMessage(NceBridge.ExistingCab) then
                      NceBridge.iStateMachine := STATE_NEXT_CAB;
                    Exit;
                  end;
              STATE_NEXT_CAB :
                  begin
                    {$IFDEF DEBUG_DISCOVER_STATEMACHINE} UART1_Write_Text('STATE_DISCOVER_NEXT_CAB'+LF); {$ENDIF}
                    Inc(NceBridge.iDiscoveryCabID);
                    if NceBridge.iDiscoveryCabID > ID_MAX_DEVICE then
                    begin
                      NceBridge.Discovering := False;
                      NceBridge.DiscoverTimer := 0;
                      NceBridge.iStateMachine := STATE_INITIALIZE;                        // Get ready for the next Discovery Mode
                      Node^.iUserStateMachine := STATE_KNOWNDEVICES;                      // Go back to pinging only known Cabs
                    end else
                      NceBridge.iStateMachine := STATE_SEND_PING;
                    Exit;
                  end;
            end;
            Exit;
          end;
      STATE_KNOWNDEVICES    :
          begin
             case NceBridge.iStateMachine of
                STATE_INITIALIZE :
                    begin
                      if (NceBridge.iAssignedCabCount = 0) or (NceBridge.DiscoverTimer >= REDISCOVERY_TIME) then
                      begin
                        Node^.iUserStateMachine := STATE_DISCOVERDEVICES;
                        NceBridge.iStateMachine := STATE_INITIALIZE;
                      end else
                      begin
                       {$IFDEF DEBUG_KNOWNDEVICES_STATEMACHINE} UART1_Write_Text('STATE_INITIALIZE'+LF); {$ENDIF}
                       NceBridge.iStateMachine := STATE_SEND_SYNC;
                      end;
                      Exit;
                    end;
                STATE_SEND_SYNC :
                    begin
                      {$IFDEF FPC}
                      NceBridge.iStateMachine := STATE_SEND_PING;           // No reason to wait if running on Lazarus
                      {$ELSE}
                      LATB4_bit := 1;
                      {$IFDEF DEBUG_KNOWNDEVICES_STATEMACHINE} UART1_Write_Text('STATE_SEND_SYNC..............'+LF); {$ENDIF}
                      FlushUartReceiver;
                      WriteByte(NCEBUS_PING or 0, True);                                    // Special Ping with a CabID of 0 to Sync the stream
                      EnableCabBusTimer(28800);          // 31.25ns * 28800 = 900us      Needs some delay, the cabs are slow microprocessors
                      NceBridge.iStateMachine := STATE_WAIT_SYNC_DELAY;
                      LATB4_bit := 0;
                      {$ENDIF}
                      Exit;
                    end;
                STATE_WAIT_SYNC_DELAY :
                    begin  // Waiting for the Timer to Expire and jump us to the next state
                      {$IFDEF DEBUG_KNOWNDEVICES_STATEMACHINE} UART1_Write_Text('STATE_WAIT_SYNC_DELAY'+LF); {$ENDIF}
                      Exit;
                    end;
                STATE_SEND_PING :
                    begin  // The node replied and
                      {$IFDEF DEBUG_KNOWNDEVICES_STATEMACHINE} UART1_Write_Text('STATE_SEND_PING: ' + s1+LF); {$ENDIF}
                      {$IFDEF FPC}
                      NceBridge.iIncomingCount := 0;
                      if not PingEnumlatorFunc(PNceCab( NceBridge.AssignedCabs[NceBridge.iActiveCab]^.UserData)^.ID, NceBridge.AssignedCabs[NceBridge.iActiveCab]) then
                        NceBridge.iStateMachine := STATE_NEXT_CAB;              // STATE_CAB_REPLIED will be set by the UART "interrupt" call
                      {$ELSE}
                      FlushUartReceiver;
                      NceBridge.iIncomingCount := 0;
                      NceBridge.LastPortRead := PortB;
                      CNIE_bit := 1;                                            // Pin Change Interrupt enable
                      WriteByte(NCEBUS_PING or PNceCab( NceBridge.AssignedCabs[NceBridge.iActiveCab]^.UserData)^.ID, True);            // Ping the Cab ID
                      EnableCabBusTimer(57600);                                 // 31.25ns * 57600 = 1800us
                      NceBridge.iStateMachine := STATE_WAIT_FOR_RESPONSE;
                      {$ENDIF}
                      Exit;
                    end;
                STATE_WAIT_FOR_RESPONSE :
                    begin   // Waiting for the Timer to expire or we detect a reply....
                      // See CabBus_Timeout function for the Timeout or CabBus_UART_RX_StateMachine for a reply and what the next state is
                      {$IFDEF DEBUG_KNOWNDEVICES_STATEMACHINE} UART1_Write_Text('STATE_WAIT_FOR_RESPONSE'+LF); {$ENDIF}
                    end;
                STATE_TIMEOUT :
                    begin
                      {$IFDEF DEBUG_KNOWNDEVICES_STATEMACHINE} UART1_Write_Text('STATE_TIMEOUT'+LF); {$ENDIF}
                      // This should only happen if the throttle was unplugged
                      NceBridge.iStateMachine := STATE_NEXT_CAB;
                      Exit;
                    end;
                STATE_CAB_REPLIED :
                    begin
                      {$IFDEF DEBUG_KNOWNDEVICES_STATEMACHINE} UART1_Write_Text('STATE_CAB_REPLIED'+LF); {$ENDIF}
                      if HandleCabBusReply(NceBridge.AssignedCabs[NceBridge.iActiveCab]) then
                        NceBridge.iStateMachine := STATE_DISPATCH_MESSAGE
                      else
                        NceBridge.iStateMachine := STATE_NEXT_CAB;
                      Exit;
                    end;
                STATE_DISPATCH_MESSAGE :
                    begin
                      {$IFDEF DEBUG_DISCOVER_STATEMACHINE} UART1_Write_Text('STATE_DISPATCH_MESSAGE'+LF); {$ENDIF}
                      if DispatchMessage(NceBridge.AssignedCabs[NceBridge.iActiveCab]) then
                        NceBridge.iStateMachine := STATE_NEXT_CAB;
                      Exit;
                    end;
                STATE_NEXT_CAB :
                    begin
                      {$IFDEF DEBUG_KNOWNDEVICES_STATEMACHINE} UART1_Write_Text('STATE_NEXT_CAB'+LF); {$ENDIF}
                      Inc(NceBridge.iActiveCab);
                      if NceBridge.iActiveCab >= NceBridge.iAssignedCabCount then
                      begin
                        NceBridge.iStateMachine := STATE_INITIALIZE;
                        NceBridge.iActiveCab := 0;
                      end else
                        NceBridge.iStateMachine := STATE_SEND_PING;
                      Exit;
                    end;
              end;
            Exit
          end;
    end;
  end else
  begin
    // Cab Nodes, These states are entered by key presses and set by the Physical
    // node's interaction with the Cab Bus
    CabData := PNceCab( Node^.UserData);
    case Node^.iUserStateMachine of
      STATE_USER_START :
          begin
            if Node^.State and NS_PERMITTED <> 0 then
              Node^.iUserStateMachine := STATE_CAB_IDLE;
            Exit;
          end;
      STATE_CAB_IDLE :
          begin
            // Nothing going on....
            Exit;
          end;
      STATE_CAB_SELECT_LOCO :
          begin
            case CabData^.iStateMachine of
              STATE_INITIALIZE :
                  begin {$IFDEF DEBUG_STATE_CAB_SELECT_LOCO_STATEMACHINE} UART1_Write_Text('STATE_INITIALIZE'+LF); {$ENDIF}
                     CabData^.iStateMachine := STATE_CAB_SELECT_LOCO_SEND_PROXY_MANAGE_LOCK;
                     Exit;
                  end;
              STATE_CAB_SELECT_LOCO_SEND_PROXY_MANAGE_LOCK :
                  begin {$IFDEF DEBUG_STATE_CAB_SELECT_LOCO_STATEMACHINE} UART1_Write_Text('STATE_CAB_SELECT_LOCO_SEND_PROXY_MANAGE_LOCK'+LF); {$ENDIF}
                    if TrySendTractionProxyManage(Node^.Info, ProxyNode, True) then
                      CabData^.iStateMachine := STATE_CAB_SELECT_LOCO_GENERIC_REPLY_WAIT; // Wait for the Manage Reply Callback
                    CabData^.WatchDog := 0;
                    Exit;
                  end;
              STATE_CAB_SELECT_LOCO_SEND_PROXY_ALLOCATE :
                  begin {$IFDEF DEBUG_STATE_CAB_SELECT_LOCO_STATEMACHINE} UART1_Write_Text('STATE_CAB_SELECT_LOCO_SEND_PROXY_ALLOCATE'+LF); {$ENDIF}

                    Address := CabMessageToTrainAddress(@CabData^.IncomingMsg);

                    {$IFNDEF FPC}
                    WordToStr(Address, s1);
                    UART1_Write_Text('Address = ' + s1 + LF);
                    {$ENDIF}


                    SpeedStep := 28;

              {      AppCallback_ReadConfiguration(CONFIG_OFFSET_SPEED_STEP, 1, @SpeedStep);    // These offsets are into the Physical Nodes configuration address space
                    AppCallback_ReadConfiguration(CONFIG_OFFSET_ADDRESS_TYPE, 1, @AddressType);

                    if AddressType = 1 then
                      Address := Address or $C000;
                    case SpeedStep of
                      0 : SpeedStep := 14;
                      1 : SpeedStep := 28;
                      2 : SpeedStep := 128;
                    end;
                   }
                    if TrySendTractionProxyAllocate(Node^.Info, ProxyNode, TRACTION_PROXY_TECH_ID_DCC, Address, SpeedStep, 0) then
                      CabData^.iStateMachine := STATE_CAB_SELECT_LOCO_GENERIC_REPLY_WAIT;  // Wait for the Allocate Reply Callback
                    CabData^.WatchDog := 0;
                    Exit;
                  end;
              STATE_CAB_SELECT_LOCO_SEND_PROXY_MANAGE_UNLOCK :
                  begin {$IFDEF DEBUG_STATE_CAB_SELECT_LOCO_STATEMACHINE} UART1_Write_Text('STATE_CAB_SELECT_LOCO_SEND_PROXY_MANAGE_UNLOCK'+LF); {$ENDIF}
                    if TrySendTractionProxyManage(Node^.Info, ProxyNode, False) then
                      CabData^.iStateMachine := STATE_CAB_SELECT_LOCO_SEND_TRACTION_MANAGE_LOCK; // No Reply for Unlock
                    CabData^.WatchDog := 0;
                    Exit;
                  end;
              STATE_CAB_SELECT_LOCO_SEND_TRACTION_MANAGE_LOCK :
                  begin {$IFDEF DEBUG_STATE_CAB_SELECT_LOCO_STATEMACHINE} UART1_Write_Text('STATE_CAB_SELECT_LOCO_SEND_TRACTION_MANAGE_LOCK'+LF); {$ENDIF}
                    if TrySendTractionManage(Node^.Info, Node^.TrainData.LinkedNode, True) then
                      CabData^.iStateMachine := STATE_CAB_SELECT_LOCO_GENERIC_REPLY_WAIT;  // Wait for the Lock Reply Callback
                    CabData^.WatchDog := 0;
                    Exit;
                  end;
              STATE_CAB_SELECT_LOCO_SEND_TRACTION_ASSIGN_CONTROLLER :
                  begin {$IFDEF DEBUG_STATE_CAB_SELECT_LOCO_STATEMACHINE} UART1_Write_Text('STATE_CAB_SELECT_LOCO_SEND_TRACTION_ASSIGN_CONTROLLER'+LF); {$ENDIF}
                    if TrySendTractionControllerConfig(Node^.Info, Node^.TrainData.LinkedNode, Node^.Info, True) then
                      CabData^.iStateMachine := STATE_CAB_SELECT_LOCO_GENERIC_REPLY_WAIT;  // Wait for the Lock Reply Callback
                    CabData^.WatchDog := 0;
                    Exit;
                  end;
              STATE_CAB_SELECT_LOCO_SEND_TRACTION_QUERY_SPEED :
                  begin {$IFDEF DEBUG_STATE_CAB_SELECT_LOCO_STATEMACHINE} UART1_Write_Text('STATE_CAB_SELECT_LOCO_SEND_TRACTION_QUERY_SPEED'+LF); {$ENDIF}
                    if TrySendTractionQuerySpeed(Node^.Info, Node^.TrainData.LinkedNode) then
                      CabData^.iStateMachine := STATE_CAB_SELECT_LOCO_GENERIC_REPLY_WAIT;  // Wait for the Lock Reply Callback
                    CabData^.WatchDog := 0;
                    Exit;
                  end;
              STATE_CAB_SELECT_LOCO_SEND_TRACTION_QUERY_FUNCTIONS :
                  begin {$IFDEF DEBUG_STATE_CAB_SELECT_LOCO_STATEMACHINE} UART1_Write_Text('STATE_CAB_SELECT_LOCO_SEND_TRACTION_QUERY_FUNCTIONS'+LF); {$ENDIF}
                    if TrySendTractionQueryFunction(Node^.Info, Node^.TrainData.LinkedNode, 0) then
                      CabData^.iStateMachine := STATE_CAB_SELECT_LOCO_GENERIC_REPLY_WAIT;  // Wait for the Lock Reply Callback
                    CabData^.WatchDog := 0;
                    Exit;
                  end;
              STATE_CAB_SELECT_LOCO_SEND_TRACTION_MANAGE_UNLOCK :
                  begin {$IFDEF DEBUG_STATE_CAB_SELECT_LOCO_STATEMACHINE} UART1_Write_Text('STATE_CAB_SELECT_LOCO_SEND_TRACTION_MANAGE_UNLOCK'+LF); {$ENDIF}
                    if TrySendTractionManage(Node^.Info, Node^.TrainData.LinkedNode, False) then
                      Node^.iUserStateMachine := STATE_CAB_CLEAR_MSG;   // We are done....
                    CabData^.WatchDog := 0;
                    Exit;
                  end;
              STATE_CAB_SELECT_LOCO_GENERIC_REPLY_WAIT :
                  begin
                    // Waiting for the Reply to come into a callback
                    if CabData^.WatchDog > 200 then
                    begin
                      {$IFDEF DEBUG_STATE_CAB_SELECT_LOCO_STATEMACHINE} UART1_Write_Text('STATE_CAB_SELECT_LOCO_SEND_PROXY_MANAGE_REPLY_WAIT'+LF); {$ENDIF}
                      CabData^.iStateMachine := STATE_CAB_SELECT_LOCO_GENERIC_TIMEOUT_PROXY_UNLOCK;    // Force unlocks and exit
                    end;
                    Exit;
                  end;
              STATE_CAB_SELECT_LOCO_GENERIC_TIMEOUT_PROXY_UNLOCK :
                  begin {$IFDEF DEBUG_STATE_CAB_SELECT_LOCO_STATEMACHINE} UART1_Write_Text('STATE_CAB_SELECT_LOCO_GENERIC_TIMEOUT_PROXY_UNLOCK'+LF); {$ENDIF}
                    // Unsure if we are locked or not, just release just in case
                    if TrySendTractionProxyManage(Node^.Info, ProxyNode, False) then
                      CabData^.iStateMachine := STATE_CAB_SELECT_LOCO_SEND_TRACTION_MANAGE_UNLOCK; // No Reply for Unlock, just unlock the Traction Protcol and end
                    Exit;
                  end;
            end;
          end;
      STATE_CAB_RUN_MACRO :
          begin
            if NMRAnetUtilities_NullNodeIDInfo(Node^.TrainData.LinkedNode) then
            begin
              Node^.iUserStateMachine := STATE_CAB_CLEAR_MSG; // We are done....
              Exit;
            end else
            begin
              Node^.iUserStateMachine := STATE_CAB_CLEAR_MSG; // We are done....
              Exit;
            end;
          end;
      STATE_CAB_RUN_CMD :
          begin
            if NMRAnetUtilities_NullNodeIDInfo(Node^.TrainData.LinkedNode) then
            begin
              Node^.iUserStateMachine := STATE_CAB_CLEAR_MSG; // We are done....
              Exit;
            end else
            begin
              case CabData^.IncomingMsg.DataBytes[0] of
                NCE_CAB_DIR_TOGGLE                  :
                  begin
                    if Node^.TrainData.SpeedDir and $8000 <> 0 then
                      Node^.TrainData.SpeedDir := Node^.TrainData.SpeedDir and not $8000
                    else
                      Node^.TrainData.SpeedDir := Node^.TrainData.SpeedDir or $8000;
                    if TrySendTractionSpeedSet(Node^.Info, Node^.TrainData.LinkedNode, Node^.TrainData.SpeedDir) then
                      Node^.iUserStateMachine := STATE_CAB_CLEAR_MSG; // We are done....
                    Exit;
                  end;
                NCE_HORN_KEY_DOWN                   :
                  begin
                    if TrySendTractionFunctionSet(Node^.Info, Node^.TrainData.LinkedNode, FUNCTION_HORN, 1) then
                      Node^.iUserStateMachine := STATE_CAB_CLEAR_MSG; // We are done....
                    Exit;
                  end;
                NCE_CAB_ONE_STEP_FASTER             :
                  begin
                    NewSpeed := ChangeSpeed(Node^.TrainData.SpeedDir, +1);
                    if TrySendTractionSpeedSet(Node^.Info, Node^.TrainData.LinkedNode, NewSpeed) then
                    begin
                      Node^.TrainData.SpeedDir := NewSpeed;
                      Node^.iUserStateMachine := STATE_CAB_CLEAR_MSG; // We are done....
                    end;
                    Exit;
                  end;
                NCE_CAB_ONE_STEP_SLOWER             :
                  begin
                    NewSpeed := ChangeSpeed(Node^.TrainData.SpeedDir, -1);
                    if TrySendTractionSpeedSet(Node^.Info, Node^.TrainData.LinkedNode, NewSpeed) then
                    begin
                      Node^.TrainData.SpeedDir := NewSpeed;
                      Node^.iUserStateMachine := STATE_CAB_CLEAR_MSG; // We are done....
                    end;
                    Exit;
                  end;
                NCE_CAB_EMERGENCY_STOP              :
                  begin
                    if TrySendTractionEmergencyStop(Node^.Info, Node^.TrainData.LinkedNode) then
                      Node^.iUserStateMachine := STATE_CAB_CLEAR_MSG; // We are done....
                    Exit;
                  end;
                NCE_CAB_BELL                        :
                  begin
                    if ToggleFunction(Node, Node^.TrainData.Functions, FUNCTION_BELL) then
                      Node^.iUserStateMachine := STATE_CAB_CLEAR_MSG; // We are done....
                    Exit;
                  end;
                NCE_CAB_TOGGLE_F0_0                 :
                  begin
                    if ToggleFunction(Node, Node^.TrainData.Functions, 0) then
                      Node^.iUserStateMachine := STATE_CAB_CLEAR_MSG; // We are done....
                  end;
                NCE_CAB_TOGGLE_F1_1                 :
                  begin
                    if ToggleFunction(Node, Node^.TrainData.Functions, 1) then
                      Node^.iUserStateMachine := STATE_CAB_CLEAR_MSG; // We are done....
                    Exit;
                  end;
                NCE_CAB_TOGGLE_F2_2                 :
                  begin
                    if ToggleFunction(Node, Node^.TrainData.Functions, 2) then
                      Node^.iUserStateMachine := STATE_CAB_CLEAR_MSG; // We are done....
                    Exit;
                  end;
                NCE_CAB_TOGGLE_F3_3                 :
                  begin
                    if ToggleFunction(Node, Node^.TrainData.Functions, 3) then
                      Node^.iUserStateMachine := STATE_CAB_CLEAR_MSG; // We are done....
                    Exit;
                  end;
                NCE_CAB_TOGGLE_F4_4                 :
                  begin
                    if ToggleFunction(Node, Node^.TrainData.Functions, 4) then
                      Node^.iUserStateMachine := STATE_CAB_CLEAR_MSG; // We are done....
                    Exit;
                  end;
                NCE_CAB_TOGGLE_F5_5                 :
                  begin
                    if ToggleFunction(Node, Node^.TrainData.Functions, 5) then
                      Node^.iUserStateMachine := STATE_CAB_CLEAR_MSG; // We are done....
                    Exit;
                  end;
                NCE_CAB_TOGGLE_F6_6                 :
                  begin
                    if ToggleFunction(Node, Node^.TrainData.Functions, 6) then
                      Node^.iUserStateMachine := STATE_CAB_CLEAR_MSG; // We are done...
                    Exit;
                  end;
                NCE_CAB_TOGGLE_F7_7                 :
                  begin
                    if ToggleFunction(Node, Node^.TrainData.Functions, 7) then
                      Node^.iUserStateMachine := STATE_CAB_CLEAR_MSG; // We are done....
                    Exit;
                  end;
                NCE_CAB_TOGGLE_F8_8                 :
                  begin
                    if ToggleFunction(Node, Node^.TrainData.Functions, 8) then
                      Node^.iUserStateMachine := STATE_CAB_CLEAR_MSG; // We are done....
                    Exit;
                  end;
                NCE_CAB_9                           :
                  begin
                    if ToggleFunction(Node, Node^.TrainData.Functions, 9) then
                      Node^.iUserStateMachine := STATE_CAB_CLEAR_MSG; // We are done....
                    Exit;
                  end;
                NCE_HORN_KEY_UP                     :
                  begin
                    if TrySendTractionFunctionSet(Node^.Info, Node^.TrainData.LinkedNode, FUNCTION_HORN, 0) then
                      Node^.iUserStateMachine := STATE_CAB_CLEAR_MSG; // We are done....
                    Exit;
                  end;
                NCE_CAB_FIVE_STEPS_FASTER           :
                  begin
                    NewSpeed := ChangeSpeed(Node^.TrainData.SpeedDir, +5);
                    if TrySendTractionSpeedSet(Node^.Info, Node^.TrainData.LinkedNode, NewSpeed) then
                    begin
                      Node^.TrainData.SpeedDir := NewSpeed;
                      Node^.iUserStateMachine := STATE_CAB_CLEAR_MSG; // We are done....
                    end;
                    Exit;
                  end;
                NCE_CAB_FIVE_STEPS_SLOWER           :
                  begin
                    NewSpeed := ChangeSpeed(Node^.TrainData.SpeedDir, -5);
                    if TrySendTractionSpeedSet(Node^.Info, Node^.TrainData.LinkedNode, NewSpeed) then
                    begin
                      Node^.TrainData.SpeedDir := NewSpeed;
                      Node^.iUserStateMachine := STATE_CAB_CLEAR_MSG; // We are done....
                    end;
                    Exit;
                  end;
                NCE_CAB_DIR_FORWARD                 :
                  begin
                    if TrySendTractionDirectionSet(Node^.Info, Node^.TrainData.LinkedNode, Node^.TrainData.SpeedDir, True) then
                      Node^.iUserStateMachine := STATE_CAB_CLEAR_MSG; // We are done....
                    Exit;
                  end;
                NCE_CAB_DIR_REVERSE                 :
                  begin
                    if TrySendTractionDirectionSet(Node^.Info, Node^.TrainData.LinkedNode, Node^.TrainData.SpeedDir, False) then
                      Node^.iUserStateMachine := STATE_CAB_CLEAR_MSG; // We are done....
                    Exit;
                  end;
              end;
            end;
          end;
      STATE_CAB_CLEAR_MSG :
          begin {$IFDEF DEBUG_STATE_CAB_STATEMACHINE} UART1_Write_Text('STATE_CAB_CLEAR_MSG'+LF); {$ENDIF}
            CabData^.IncomingMsg.Full := False;
            CabData^.State := CabData^.State and not (CS_MACRO_MESSAGE or CS_LOCO_SELECT);
            CabData^.IncomingMsg.Count := 0;
            Node^.iUserStateMachine := STATE_CAB_IDLE; // We are done....
            CabData^.iStateMachine := STATE_INITIALIZE;  // Reset
          end;
    end;
  end;
  {$IFDEF FPC}LeaveCriticalsection(OPStackCriticalSection);{$ENDIF}
end;

// *****************************************************************************
//  procedure AppCallback_NodeInitialize
//     Parameters: : Node : Pointer to the node that needs to be initilized to its intial value
//     Returns     : None
//     Description : Typically called when a node is being intialized to be
//                   logged into the network.  It is possible the node can be
//                   discarded then reused so it may be called more than once for
//                   virtual nodes
// *****************************************************************************
procedure AppCallback_NodeInitialize(Node: PNMRAnetNode);
begin
  // Assign the user data record to the Node for future use
   Node^.UserData := @NceCabArray[Node^.iIndex];
   Node^.iUserStateMachine := STATE_USER_START;

   // Initialize the data, every time the node is reused!
   ZeroizeNceCabData( PNceCab (Node^.UserData))
end;

{$IFDEF SUPPORT_TRACTION}
// *****************************************************************************
//  procedure AppCallback_TractionControlReply
//     Parameters: : Source : Full Node ID (and Alias if on CAN) of the source node for the message
//                   Dest   : Full Node ID (and Alias if on CAN) of the dest node for the message
//                   DataBytes: pointer to the raw data bytes
//     Returns     : None
//     Description : Called when a Traction Protocol request comes in
// *****************************************************************************
procedure AppCallback_TractionProtocol(Node: PNMRAnetNode; AMessage: POPStackMessage);
var
 MultiFrameBuffer: PMultiFrameBuffer;
begin
  {$IFDEF FPC}EnterCriticalsection(OPStackCriticalSection);{$ENDIF}
  MultiFrameBuffer := PMultiFrameBuffer( PByte( AMessage^.Buffer));
  case MultiFrameBuffer^.DataArray[0] of
    TRACTION_CONTROLLER_CONFIG :
        begin
          case MultiFrameBuffer^.DataArray[1] of
            TRACTION_CONTROLLER_CONFIG_NOTIFY :
                begin

                end;
          end;
        end;
  end;
  {$IFDEF FPC}LeaveCriticalsection(OPStackCriticalSection);{$ENDIF}
end;

// *****************************************************************************
//  procedure AppCallback_TractionProtocolReply
//     Parameters: : Node           : Pointer to the node that the traction protocol has been called on
//                   ReplyMessage   : The Reply Message that needs to be allocated, populated and returned so it can be sent
//                   RequestingMessage    : Message that was sent to the node containing the requested information
//     Returns     : True if the RequestingMessage is handled and the ReplyMessage is ready to send
//                   False if the request has not been completed due to no available buffers or waiting on other information
//     Description : Called in response to a Traction Protcool request
// *****************************************************************************
procedure AppCallback_TractionProtocolReply(Node: PNMRAnetNode; AMessage: POPStackMessage);
var
 MultiFrameBuffer: PMultiFrameBuffer;
 CabData: PNceCab;
begin
  {$IFDEF FPC}EnterCriticalsection(OPStackCriticalSection);{$ENDIF}
  MultiFrameBuffer := PMultiFrameBuffer( PByte( AMessage^.Buffer));
  CabData := PNceCab( Node^.UserData);
  case MultiFrameBuffer^.DataArray[0] of
    TRACTION_QUERY_SPEED :
        begin
          Node^.TrainData.SpeedDir := (MultiFrameBuffer^.DataArray[1] shl 8) or MultiFrameBuffer^.DataArray[2];
          CabData^.iStateMachine := STATE_CAB_SELECT_LOCO_SEND_TRACTION_QUERY_FUNCTIONS
        end;
    TRACTION_QUERY_FUNCTION :
        begin
         // Node^.TrainData.Functions......
         CabData^.iStateMachine := STATE_CAB_SELECT_LOCO_SEND_TRACTION_MANAGE_UNLOCK
        end;
    TRACTION_CONTROLLER_CONFIG :
        begin
          case MultiFrameBuffer^.DataArray[1] of
            TRACTION_CONTROLLER_CONFIG_ASSIGN :
                begin
                  if MultiFrameBuffer^.DataArray[2] = TRACTION_CONTROLLER_ASSIGN_REPLY_OK then
                    CabData^.iStateMachine := STATE_CAB_SELECT_LOCO_SEND_TRACTION_QUERY_SPEED
                  else
                    CabData^.iStateMachine := STATE_CAB_SELECT_LOCO_GENERIC_TIMEOUT_PROXY_UNLOCK   // Can't reserve now go back to normal polling
                end;
          end;
        end;
    TRACTION_CONSIST :
        begin
          case MultiFrameBuffer^.DataArray[1] of
            TRACTION_CONSIST_ATTACH :
                begin
                end;
            TRACTION_CONSIST_DETACH :
                begin
                end;
            TRACTION_CONSIST_QUERY :
                begin
                end;
          end // case
        end;
    TRACTION_MANAGE :
        begin
          case MultiFrameBuffer^.DataArray[1] of
            TRACTION_MANAGE_RESERVE :
                begin
                  if MultiFrameBuffer^.DataArray[2] = TRACTION_MANAGE_RESERVE_REPLY_OK then
                    CabData^.iStateMachine := STATE_CAB_SELECT_LOCO_SEND_TRACTION_ASSIGN_CONTROLLER
                  else
                    CabData^.iStateMachine := STATE_CAB_SELECT_LOCO_GENERIC_TIMEOUT_PROXY_UNLOCK   // Can't reserve now go back to normal polling
                end;
          end
        end;
    end;
  {$IFDEF FPC}LeaveCriticalsection(OPStackCriticalSection);{$ENDIF}


  {
  EnterCriticalsection(OPStackCriticalSection);
  Link := FindLinkByNodeAlias(Node);
  if Link <> nil then
  begin
    MultiFrameBuffer := PMultiFrameBuffer( PByte( AMessage^.Buffer));
    case MultiFrameBuffer^.DataArray[0] of
      TRACTION_CONTROLLER_CONFIG :
          begin
            case MultiFrameBuffer^.DataArray[1] of
              TRACTION_CONTROLLER_CONFIG_ASSIGN :
                  begin
                    if MultiFrameBuffer^.DataArray[2] = TRACTION_CONTROLLER_ASSIGN_REPLY_OK then
                    begin
                      Link^.TrainAllocated := True;
                      Link^.SyncState := Link^.SyncState or SYNC_CONTROLLER;
                      Node^.iUserStateMachine := STATE_USER_5
                    end else
                      Node^.iUserStateMachine := STATE_USER_5 // Release the Train, error try again??????
                  end;
            end;
          end;
      TRACTION_CONSIST :
          begin
            case MultiFrameBuffer^.DataArray[1] of
              TRACTION_CONSIST_ATTACH :
                  begin
                  end;
              TRACTION_CONSIST_DETACH :
                  begin
                  end;
              TRACTION_CONSIST_QUERY :
                  begin
                  end
            end // case
          end;
      TRACTION_MANAGE :
          begin
            case MultiFrameBuffer^.DataArray[1] of
              TRACTION_MANAGE_RESERVE :
                  begin
                    if Link^.SyncState and SYNC_CLOSING <> 0 then       // Run the closing statemachine
                    begin
                      if MultiFrameBuffer^.DataArray[2] = TRACTION_MANAGE_RESERVE_REPLY_OK then
                        Node^.iUserStateMachine := STATE_USER_7
                      else
                        Node^.iUserStateMachine := STATE_USER_6  // Keep trying to lock it for now
                    end else
                    begin
                      if MultiFrameBuffer^.DataArray[2] = TRACTION_MANAGE_RESERVE_REPLY_OK then
                        Node^.iUserStateMachine := STATE_USER_4
                      else
                        Node^.iUserStateMachine := STATE_USER_3 // Keep trying to lock it for now
                    end
                  end
            end
          end
      end;
  end;
  LeaveCriticalsection(OPStackCriticalSection);   }
end;
{$ENDIF}

{$IFDEF SUPPORT_TRACTION_PROXY}
// *****************************************************************************
//  procedure AppCallback_TractionProtocol
//     Parameters: : Node           : Pointer to the node that the traction protocol has been called on
//                   ReplyMessage   : The Reply Message that needs to be allocated, populated and returned so it can be sent
//                   RequestingMessage    : Message that was sent to the node containing the requested information
//     Returns     : True if the RequestingMessage is handled and the ReplyMessage is ready to send
//                   False if the request has not been completed due to no available buffers or waiting on other information
//     Description : Called when a Traction Protocol message is received
// *****************************************************************************
function AppCallback_TractionProxyProtocol(Node: PNMRAnetNode; AMessage: POPStackMessage; SourceHasLock: Boolean): Boolean;
begin
  Result := False;
end;

// *****************************************************************************
//  procedure AppCallback_TractionProxyProtocolReply
//     Parameters: : Source : Full Node ID (and Alias if on CAN) of the source node for the message
//                   Dest   : Full Node ID (and Alias if on CAN) of the dest node for the message
//                   DataBytes: pointer to the raw data bytes
//     Returns     : None
//     Description : Called in response to a Traction Proxy request
// *****************************************************************************
procedure AppCallback_TractionProxyProtocolReply(Node: PNMRAnetNode; AMessage: POPStackMessage);
var
  MultiFrameBuffer: PMultiFrameBuffer;
  CabData: PNceCab;
  i: Integer;
begin
  {$IFDEF FPC}EnterCriticalsection(OPStackCriticalSection);{$ENDIF}
  MultiFrameBuffer := PMultiFrameBuffer( PByte(AMessage^.Buffer));
  CabData := PNceCab( Node^.UserData);
  case AMessage^.Buffer^.DataArray[0] of
    TRACTION_PROXY_MANAGE :
        begin
          if AMessage^.Buffer^.DataArray[1] = TRACTION_PROXY_MANAGE_RESERVE then
          begin
             if AMessage^.Buffer^.DataArray[2] = 0 then
               CabData^.iStateMachine := STATE_CAB_SELECT_LOCO_SEND_PROXY_ALLOCATE         // Move to next state after reserving
             else
               CabData^.iStateMachine := STATE_CAB_SELECT_LOCO_GENERIC_TIMEOUT_PROXY_UNLOCK   // Can't reserve now go back to normal polling
          end;
          Exit;
        end;
    TRACTION_PROXY_ALLOCATE :
        begin
          Node^.TrainData.LinkedNode.AliasID := (MultiFrameBuffer^.DataArray[11] shl 8) or (MultiFrameBuffer^.DataArray[12]);
          NMRAnetUtilities_Load48BitNodeIDWithSimpleData(Node^.TrainData.LinkedNode.ID, PSimpleDataArray( PByte( @MultiFrameBuffer^.DataArray[5]))^);
          CabData^.iStateMachine := STATE_CAB_SELECT_LOCO_SEND_PROXY_MANAGE_UNLOCK;    // Now need to unlock the Proxy
          Exit;
        end;
  end; // case
  {$IFDEF FPC}LeaveCriticalsection(OPStackCriticalSection);{$ENDIF}
end;
{$ENDIF}

// *****************************************************************************
//  procedure AppCallBack_ProtocolSupportReply
//     Parameters: : Source : Full Node ID (and Alias if on CAN) of the source node for the message
//                   Dest   : Full Node ID (and Alias if on CAN) of the dest node for the message
//                   DataBytes: pointer Raw data bytes, Byte 0 and 1 are the Alias
//     Returns     : None
//     Description : Called in response to a Protocol Support Request
// *****************************************************************************
procedure AppCallBack_ProtocolSupportReply(Node: PNMRAnetNode; AMessage: POPStackMessage);
begin

end;

// *****************************************************************************
//  procedure AppCallback_ConsumerIdentified
//     Parameters: : Source : Full Node ID (and Alias if on CAN) of the source node for the message
//                   Dest   : Full Node ID (and Alias if on CAN) of the dest node for the message
//                   MTI    : MTI of the message
//                   EventID: pointer to the Event ID for the message
//     Returns     : None
//     Description : This is called directly from the Hardware receive buffer.  Do
//                   not do anything here that stalls the call.  This is called
//                   Asyncronously from the Statemachine loop and the Statemachine loop
//                   is stalled until this returns.  Set a flag and move on is the
//                   best stratagy or store info in a buffer and process in the
//                   main statemachine.
// *****************************************************************************
procedure AppCallback_ConsumerIdentified(var Source: TNodeInfo; MTI: Word; EventID: PEventID);
begin

end;

// *****************************************************************************
//  procedure AppCallback_ProducerIdentified
//     Parameters: : Source : Full Node ID (and Alias if on CAN) of the source node for the message
//                   Dest   : Full Node ID (and Alias if on CAN) of the dest node for the message
//                   MTI    : MTI of the message
//                   EventID: pointer to the Event ID for the message
//     Returns     : None
//     Description : This is called directly from the Hardware receive buffer.  Do
//                   not do anything here that stalls the call.  This is called
//                   Asyncronously from the Statemachine loop and the Statemachine loop
//                   is stalled until this returns.  Set a flag and move on is the
//                   best stratagy or store info in a buffer and process in the
//                   main statemachine.
// *****************************************************************************
procedure AppCallback_ProducerIdentified(var Source: TNodeInfo; MTI: Word; EventID: PEventID);
begin
  if NMRAnetUtilities_EqualEventID(EventID, @EVENT_IS_PROXY) then
    ProxyNode := Source;
end;

// *****************************************************************************
//  procedure AppCallback_LearnEvent
//     Parameters: : Source : Full Node ID (and Alias if on CAN) of the source node for the message
//                   EventID: pointer to the Event ID for the message
//     Returns     : None
//     Description : This is called directly from the Hardware receive buffer.  Do
//                   not do anything here that stalls the call.  This is called
//                   Asyncronously from the Statemachine loop and the Statemachine loop
//                   is stalled until this returns.  Set a flag and move on is the
//                   best stratagy or store info in a buffer and process in the
//                   main statemachine.
// *****************************************************************************
procedure AppCallback_LearnEvent(var Source: TNodeInfo; EventID: PEventID);
begin

end;

// *****************************************************************************
//  procedure AppCallBack_PCEventReport
//     Parameters: : Source : Full Node ID (and Alias if on CAN) of the source node for the message
//                   EventID: pointer to the Event ID for the message
//     Returns     : None
//     Description : This is called directly from the Hardware receive buffer.  Do
//                   not do anything here that stalls the call.  This is called
//                   Asyncronously from the Statemachine loop and the Statemachine loop
//                   is stalled until this returns.  Set a flag and move on is the
//                   best stratagy or store info in a buffer and process in the
//                   main statemachine.
// *****************************************************************************
procedure AppCallBack_PCEventReport(var Source: TNodeInfo; EventID: PEventID);
begin

end;

// *****************************************************************************
//  procedure AppCallback_RemoteButtonReply
//     Parameters: : Source : Full Node ID (and Alias if on CAN) of the source node for the message
//                   Dest   : Full Node ID (and Alias if on CAN) of the dest node for the message
//                   DataBytes: pointer to the raw data bytes
//     Returns     : None
//     Description : Called in response to a Remote Button request
// *****************************************************************************
procedure AppCallback_RemoteButtonReply(Node: PNMRAnetNode; var Source: TNodeInfo; DataBytes: PSimpleBuffer);
begin

end;

{$IFDEF SUPPORT_TRACTION}
// *****************************************************************************
//  procedure AppCallback_SimpleTrainNodeInfoReply
//     Parameters: : Source : Full Node ID (and Alias if on CAN) of the source node for the message
//                   Dest   : Full Node ID (and Alias if on CAN) of the dest node for the message
//                   TrainNodeInfo: pointer to the null terminated strings
//     Returns     : None
//     Description : Called in response to a STNIP request
// *****************************************************************************
procedure AppCallback_SimpleTrainNodeInfoReply(Node: PNMRAnetNode; AMessage: POPStackMessage);
begin

end;
{$ENDIF}

// *****************************************************************************
//  procedure AppCallback_Timer_100ms
//     Parameters: : None
//     Returns     : None
//     Description : Typcally called from another thread or interrupt, only use
//                   to update asyncronous flags
// *****************************************************************************
procedure AppCallback_Timer_100ms;
var
  i: Integer;
  Cab: PNceCab;
begin
  Inc(GlobalTimer);

   // Count up to the time out then freeze.  The Timer Count will be reset after the
  // main loop is done rediscovering
  if NceBridge.DiscoverTimer < REDISCOVERY_TIME then
    Inc(NceBridge.DiscoverTimer);

  for i := 0 to NceBridge.iAssignedCabCount - 1 do
  begin
    Cab := PNceCab( NceBridge.AssignedCabs[i]^.UserData);
    Inc( Cab^.WatchDog);
  end;
end;

// *****************************************************************************
//  procedure AppCallback_SimpleNodeInfoReply
//     Parameters: : Source   : Full Node ID (and Alias if on CAN) of the source node for the message
//                   Dest     : Full Node ID (and Alias if on CAN) of the dest node for the message
//                   NodeInfo : pointer to the null terminated strings
//     Returns     : None
//     Description : Called in response to a SNIP Request
// *****************************************************************************
procedure AppCallback_SimpleNodeInfoReply(Node: PNMRAnetNode; AMessage: POPStackMessage);
begin

end;

// *****************************************************************************
//  procedure AppCallback_VerifiedNodeID
//     Parameters: : Source : Full Node ID (and Alias if on CAN) of the source node for the message
//                   EventID: pointer to the Event ID for the message
//     Returns     : None
//     Description : This is called directly from the Hardware receive buffer.  Do
//                   not do anything here that stalls the call.  This is called
//                   Asyncronously from the Statemachine loop and the Statemachine loop
//                   is stalled until this returns.  Set a flag and move on is the
//                   best stratagy or store info in a buffer and process in the
//                   main statemachine.
// *****************************************************************************
procedure AppCallback_VerifiedNodeID(var Source: TNodeInfo; NodeID: PNodeID);
begin

end;

// *****************************************************************************
//  procedure AppCallback_InitializationComplete
//     Parameters: : Source : Full Node ID (and Alias if on CAN) of the source node for the message
//                   EventID: pointer to the Event ID for the message
//     Returns     : None
//     Description : This is called directly from the Hardware receive buffer.  Do
//                   not do anything here that stalls the call.  This is called
//                   Asyncronously from the Statemachine loop and the Statemachine loop
//                   is stalled until this returns.  Set a flag and move on is the
//                   best stratagy or store info in a buffer and process in the
//                   main statemachine.
// *****************************************************************************
procedure AppCallback_InitializationComplete(var Source: TNodeInfo; NodeID: PNodeID);
begin

end;

{$IFDEF FPC}

var
  i: Integer;

initialization
  System.InitCriticalSection(OPStackCriticalSection);
  ThrottleList := TList.Create;

Finalization
  DoneCriticalsection( OPStackCriticalSection);
  for i := 0 to ThrottleList.Count - 1 do
  begin
    Dispose( ThrottleItem(i));
  end;
{$ENDIF}

end.