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
  template_configuration,
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
  template_vnode,
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
procedure AppCallback_RemoteButtonReply(Dest: PNMRAnetNode; var Source: TNodeInfo; DataBytes: PSimpleBuffer);
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

// Configuration Memory
procedure AppCallBack_ConfigMemReadReply(Node: PNMRAnetNode; AMessage: POPStackMessage; Success: Boolean);
procedure AppCallBack_ConfigMemStreamReadReply(Node: PNMRAnetNode; AMessage: POPStackMessage; Success: Boolean);
procedure AppCallBack_ConfigMemWriteReply(Node: PNMRAnetNode; AMessage: POPStackMessage; Success: Boolean);
procedure AppCallBack_ConfigMemStreamWriteReply(Node: PNMRAnetNode; AMessage: POPStackMessage; Success: Boolean);


var
  DatabaseFile: AnsiString;

implementation

{$IFDEF FPC}
  {$IFDEF SUPPORT_TRACTION and SUPPORT_TRACTION_PROXY}
  uses
    opstackcore_stnip,
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

const
// Physical Proxy Node User StateMachine
  STATE_SERVER_USER_START                 = 0;
  STATE_SERVER_FIND_PROXY                 = 1;
  STATE_SERVER_CREATE_DATABASE_NODES      = 2;
  STATE_SERVER_CREATE_DATABASE            = 3;
  STATE_SERVER_PROXY_IDLE                 = 4;

// Virtual Train Node User StateMachine
  STATE_TRAIN_USER_START           = 0;
  STATE_TRAIN_CREATE_UI            = 1;
  STATE_TRAIN_IDLE                 = 2;

var
  ProxyNode: TNodeInfo;
  GlobalTimer: Word;


procedure LoadTrainInfo(Node: PNMRANetNode; var EventTrainInfo: TNodeEventTrainInfo);
begin
  EventTrainInfo.Address := Node^.TrainData.Address;
  EventTrainInfo.Functions := Node^.TrainData.Functions;
  EventTrainInfo.SpeedSteps := Node^.TrainData.SpeedSteps;
  EventTrainInfo.Speed := Node^.TrainData.SpeedDir;
  EventTrainInfo.ControllerInfo := Node^.TrainData.Controller;
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
  i: Integer;
begin
  // Initialize the example data!
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
  EventTrainInfo: TNodeEventTrainInfo;
  ConfigOffset: DWord;
  Train: TTrainConfig;
begin
  if Node = GetPhysicalNode then
  begin
    // Proxy Node User StateMachine
    case Node^.iUserStateMachine of
      STATE_SERVER_USER_START :
          begin
            if Node^.State and NS_PERMITTED <> 0 then
            begin
              GlobalTimer := 0;
              if TrySendIdentifyProducer(Node^.Info, @EVENT_IS_PROXY) then
                Node^.iUserStateMachine := STATE_SERVER_FIND_PROXY;
            end;
            Exit;
          end;
      STATE_SERVER_FIND_PROXY :
          begin
            if (ProxyNode.AliasID > 0) or (ProxyNode.ID[0] > 0) or (ProxyNode.ID[1] > 0) then
              Node^.iUserStateMachine := STATE_SERVER_CREATE_DATABASE_NODES
            else begin
              if GlobalTimer > 10 then
                Node^.iUserStateMachine := STATE_SERVER_USER_START       // Try again
            end;
            Exit;
          end;
      STATE_SERVER_CREATE_DATABASE_NODES :
          begin
            for i := 0 to 5 do
              OPStackNode_Allocate;
            if not FileExistsUTF8(DatabaseFile) then
              Node^.iUserStateMachine := STATE_SERVER_CREATE_DATABASE
            else
              Node^.iUserStateMachine := STATE_SERVER_PROXY_IDLE
          end;
      STATE_SERVER_CREATE_DATABASE :
          begin
            for i := 0 to 5 do
            begin
              ConfigOffset := USER_CONFIGURATION_MEMORY_SIZE + (i * USER_VNODE_CONFIGURATION_MEMORY_SIZE);
              case i of
                0 : begin
                      Train.RoadName := 'Rio Grande Southern' + #0;
                      Train.TrainClass := 'K-27' + #0;
                      Train.RoadNumber := '455' + #0;
                      Train.Name :=  #0;
                      Train.Manufacturer := 'Blackstone Models' + #0;
                      Train.Owner := 'Jim Kueneman' + #0;
                      Train.TrainID := 455;
                      Train.SpeedSteps := 28;
                      Train.ShortLong := 1;
                      WriteTrainConfiguration(ConfigOffset, Train);
                    end;
                1 : begin
                      Train.RoadName := 'Rio Grande Southern' + #0;
                      Train.TrainClass := 'K-27' + #0;
                      Train.RoadNumber := '461' + #0;
                      Train.Name :=  #0;
                      Train.Manufacturer := 'Blackstone Models' + #0;
                      Train.Owner := 'Jim Kueneman' + #0;
                      Train.TrainID := 461;
                      Train.SpeedSteps := 28;
                      Train.ShortLong := 1;
                      WriteTrainConfiguration(ConfigOffset, Train);
                    end;
                  2 : begin
                      Train.RoadName := 'Rio Grande Southern' + #0;
                      Train.TrainClass := 'C-19' + #0;
                      Train.RoadNumber := '40' + #0;
                      Train.Name :=  'Sunrise Herald on Tender' + #0;
                      Train.Manufacturer := 'Blackstone Models' + #0;
                      Train.Owner := 'Jim Kueneman' + #0;
                      Train.TrainID := 40;
                      Train.SpeedSteps := 28;
                      Train.ShortLong := 1;
                      WriteTrainConfiguration(ConfigOffset, Train);
                    end;
                  3 : begin
                      Train.RoadName := 'Rio Grande Southern' + #0;
                      Train.TrainClass := 'C-19' + #0;
                      Train.RoadNumber := '40' + #0;
                      Train.Name :=  'Large Number on Tender ' + #0;
                      Train.Manufacturer := 'Blackstone Models' + #0;
                      Train.Owner := 'Jim Kueneman' + #0;
                      Train.TrainID := 40;
                      Train.SpeedSteps := 28;
                      Train.ShortLong := 1;
                      WriteTrainConfiguration(ConfigOffset, Train);
                    end;
                  4 : begin
                      Train.RoadName := 'Denver Rio Grande & Western' + #0;
                      Train.TrainClass := 'K-27' + #0;
                      Train.RoadNumber := '452' + #0;
                      Train.Name :=  #0;
                      Train.Manufacturer := 'Blackstone Models' + #0;
                      Train.Owner := 'Jim Kueneman' + #0;
                      Train.TrainID := 452;
                      Train.SpeedSteps := 28;
                      Train.ShortLong := 1;
                      WriteTrainConfiguration(ConfigOffset, Train);
                    end;
                  5 : begin
                      Train.RoadName := 'Rio Grande Southern' + #0;
                      Train.TrainClass := 'Railbus' + #0;
                      Train.RoadNumber := '5' + #0;
                      Train.Name :=  'Later Build with Tourist Windows' + #0;
                      Train.Manufacturer := 'Blackstone Models' + #0;
                      Train.Owner := 'Jim Kueneman' + #0;
                      Train.TrainID := 5;
                      Train.SpeedSteps := 28;
                      Train.ShortLong := 1;
                      WriteTrainConfiguration(ConfigOffset, Train);
                    end;
                end
            end;
            Node^.iUserStateMachine := STATE_SERVER_PROXY_IDLE
          end;
      STATE_SERVER_PROXY_IDLE :
          begin
            // Waiting for something to do
          end;
    end;
  end else
  begin
    case Node^.iUserStateMachine of
      STATE_TRAIN_USER_START :
            begin
              if Node^.State and NS_PERMITTED <> 0 then
              begin {$IFDEF DEBUG_TRAINOBJECT_STATEMACHINE} UART1_Write_Text('STATE_TRAIN_USER_START'+LF); {$ENDIF}
                Node^.iUserStateMachine := STATE_TRAIN_CREATE_UI
              end
            end;
      STATE_TRAIN_CREATE_UI :
            begin
              ConfigOffset := USER_CONFIGURATION_MEMORY_SIZE + ((Node^.iIndex - 1) * USER_VNODE_CONFIGURATION_MEMORY_SIZE);
              EventTrainInfo := TNodeEventTrainInfo.Create(Node^.Info, nil);
              EventTrainInfo.TrainConfigValid := True;
              ReadTrainConfiguration(ConfigOffset, EventTrainInfo.FTrainConfig);
              LoadNodeWithTrainConfig(Node, EventTrainInfo.FTrainConfig);
              LoadTrainInfo(Node, EventTrainInfo);
              NodeThread.AddEvent(EventTrainInfo);
              Node^.iUserStateMachine := STATE_TRAIN_IDLE
            end;
      STATE_TRAIN_IDLE :
          begin
            // Waiting for something to do
          end;
    end
  end
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
  Node^.iUserStateMachine := 0;
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
  EventTrainInfo: TNodeEventTrainInfo;
begin
  {$IFDEF DEBUG_TRACTION_PROTOCOL} UART1_Write_Text('AppCallback_TractionProtocol'+LF); {$ENDIF}

  // I don't want to read the configuration in everytime here so don't....
  EventTrainInfo := TNodeEventTrainInfo.Create(Node^.Info, nil);
  EventTrainInfo.TrainConfigValid := False;
  LoadTrainInfo(Node, EventTrainInfo);
  NodeThread.AddEvent(EventTrainInfo);

  case AMessage^.Buffer^.DataArray[0] of
      TRACTION_CONTROLLER_CONFIG :
          begin
            case AMessage^.Buffer^.DataArray[1] of
                TRACTION_CONTROLLER_CONFIG_ASSIGN : begin end;
                TRACTION_CONSIST_DETACH           : begin end;
            end
          end;
      TRACTION_SPEED_DIR : begin end;
      TRACTION_FUNCTION  : begin end;
      TRACTION_E_STOP    : begin end
  else begin
    end;
  end;

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
begin

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
begin

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
  begin
    ProxyNode := Source;
    NodeThread.AddEvent( TNodeEventProxyAssigned.Create(Source, nil));
  end;
  if NMRAnetUtilities_EqualEventID(EventID, @EVENT_IS_TRAIN) then
    NodeThread.AddEvent( TNodeEventIsTrain.Create(Source, nil));
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
//  procedure AppCallBack_ConfigMemReadReply
//     Parameters: : Node : Pointer to the node that needs to be initilized to its intial value
//     Returns     : None
//     Description : Typically called when a node is being intialized to be
//                   logged into the network.  It is possible the node can be
//                   discarded then reused so it may be called more than once for
//                   virtual nodes
// *****************************************************************************
procedure AppCallBack_ConfigMemReadReply(Node: PNMRAnetNode; AMessage: POPStackMessage; Success: Boolean);
begin

end;

// *****************************************************************************
//  procedure AppCallBack_ConfigMemStreamReadReply
//     Parameters: : Node : Pointer to the node that needs to be initilized to its intial value
//     Returns     : None
//     Description : Typically called when a node is being intialized to be
//                   logged into the network.  It is possible the node can be
//                   discarded then reused so it may be called more than once for
//                   virtual nodes
// *****************************************************************************
procedure AppCallBack_ConfigMemStreamReadReply(Node: PNMRAnetNode; AMessage: POPStackMessage; Success: Boolean);
begin

end;

// *****************************************************************************
//  procedure AppCallBack_ConfigMemWriteReply
//     Parameters: : Node : Pointer to the node that needs to be initilized to its intial value
//     Returns     : None
//     Description : Typically called when a node is being intialized to be
//                   logged into the network.  It is possible the node can be
//                   discarded then reused so it may be called more than once for
//                   virtual nodes
// *****************************************************************************
procedure AppCallBack_ConfigMemWriteReply(Node: PNMRAnetNode; AMessage: POPStackMessage; Success: Boolean);
begin

end;

// *****************************************************************************
//  procedure AppCallBack_ConfigMemStreamWriteReply
//     Parameters: : Node : Pointer to the node that needs to be initilized to its intial value
//     Returns     : None
//     Description : Typically called when a node is being intialized to be
//                   logged into the network.  It is possible the node can be
//                   discarded then reused so it may be called more than once for
//                   virtual nodes
// *****************************************************************************
procedure AppCallBack_ConfigMemStreamWriteReply(Node: PNMRAnetNode; AMessage: POPStackMessage; Success: Boolean);
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
procedure AppCallback_RemoteButtonReply(Dest: PNMRAnetNode; var Source: TNodeInfo; DataBytes: PSimpleBuffer);
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
begin
  Inc(GlobalTimer);
  for i := 0 to NodePool.AllocatedCount - 1 do
  begin
    if Assigned(NodePool.Pool[i].UserData) then
      TNodeTask( NodePool.Pool[i].UserData).Watchdog := TNodeTask( NodePool.Pool[i].UserData).Watchdog + 1;
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


end.


