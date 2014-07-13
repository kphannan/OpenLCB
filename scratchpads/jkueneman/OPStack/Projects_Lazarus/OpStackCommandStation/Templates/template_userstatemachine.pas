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
procedure AppCallback_RemoteButtonReply(Node: PNMRAnetNode; var Source: TNodeInfo; DataBytes: PSimpleBuffer);
{$IFDEF SUPPORT_TRACTION}
procedure AppCallback_TractionProtocol(Node: PNMRAnetNode; AMessage: POPStackMessage);
procedure AppCallback_TractionProtocolReply(Node: PNMRAnetNode; AMessage: POPStackMessage);
procedure AppCallback_SimpleTrainNodeInfoReply(Node: PNMRAnetNode; AMessage: POPStackMessage);
{$ENDIF}
{$IFDEF SUPPORT_TRACTION_PROXY}
procedure AppCallback_TractionProxyProtocol(Node: PNMRAnetNode; AMessage: POPStackMessage; SourceHasLock: Boolean);
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

procedure AppCallBack_ConfigMemReadReply(Node: PNMRAnetNode; AMessage: POPStackMessage; Success: Boolean);
procedure AppCallBack_ConfigMemStreamReadReply(Node: PNMRAnetNode; AMessage: POPStackMessage; Success: Boolean);
procedure AppCallBack_ConfigMemWriteReply(Node: PNMRAnetNode; AMessage: POPStackMessage; Success: Boolean);
procedure AppCallBack_ConfigMemStreamWriteReply(Node: PNMRAnetNode; AMessage: POPStackMessage; Success: Boolean);

{$IFNDEF FPC}
procedure SimpleTrainNodeInfoWriteConfig(ConfigOffset: DWord; var Info: TStnipBuffer); external;
{$ENDIF}

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
  STATE_PROXY_USER_START           = 0;
  STATE_PROXY_IDLE                 = 1;

// Virtual Train Node User StateMachine
  STATE_TRAIN_USER_START           = 0;
  STATE_TRAIN_ALLOCATE_PROXY_REPLY = 1;
  STATE_TRAIN_IDLE                 = 2;

var
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
begin

end;

// *****************************************************************************
//  procedure AppCallback_UserStateMachine_Process
//     Parameters: : None
//     Returns     : None
//     Description : Called as often as possible to run the user statemachine
// *****************************************************************************
procedure AppCallback_UserStateMachine_Process(Node: PNMRAnetNode);
var
  EventTrainInfo: TNodeEventTrainInfo;
  ConfigOffset: DWord;
  Task: TNodeTask;
begin
  if Node = GetPhysicalNode then
  begin
    // Proxy Node User StateMachine
    case Node^.iUserStateMachine of
      STATE_PROXY_USER_START :
            begin
              if Node^.State and NS_PERMITTED <> 0 then
              begin   {$IFDEF DEBUG_TRAINSERVER_STATEMACHINE} UART1_Write_Text('STATE_PROXY_USER_START'+LF); {$ENDIF}
                Node^.iUserStateMachine := STATE_PROXY_IDLE
              end;
            end;
      STATE_PROXY_IDLE :
          begin
            // Waiting for something to do
            if Assigned(Node^.UserData) then
            begin
              Task := TNodeTask( Node^.UserData);
              Node^.iUserStateMachine := Task.iStateMachine;                    // Start the new task...
            end;
          end;
    end;
  end else
  begin
    // Train Node User StateMachine
    case Node^.iUserStateMachine of
      STATE_TRAIN_USER_START :
            begin
              if Node^.State and NS_PERMITTED <> 0 then
              begin {$IFDEF DEBUG_TRAINOBJECT_STATEMACHINE} UART1_Write_Text('STATE_TRAIN_USER_START'+LF); {$ENDIF}
                Node^.iUserStateMachine := STATE_TRAIN_IDLE
              end
            end;
      STATE_TRAIN_ALLOCATE_PROXY_REPLY :
          begin {$IFDEF DEBUG_TRAINOBJECT_STATEMACHINE} UART1_Write_Text('STATE_TRAIN_ALLOCATE_PROXY_REPLY'+LF); {$ENDIF}
            if TrySendTractionProxyAllocateReply(GetPhysicalNode^.Info, Node^.TrainData.LinkedNode, TRACTION_PROXY_TECH_ID_DCC, Node^.Info, Node^.TrainData.Address) then
            begin
              ConfigOffset := USER_CONFIGURATION_MEMORY_SIZE + ((Node^.iIndex - 1) * USER_VNODE_CONFIGURATION_MEMORY_SIZE);
              EventTrainInfo := TNodeEventTrainInfo.Create(Node^.Info, nil);
              ReadTrainConfiguration(ConfigOffset, EventTrainInfo.FTrainConfig);
              LoadTrainInfo(Node, EventTrainInfo);
              NodeThread.AddEvent(EventTrainInfo);
              Node^.TrainData.State := Node^.TrainData.State and not TS_SEND_PROXY_ALLOCATE_REPLY;
              Node^.iUserStateMachine := STATE_TRAIN_IDLE;
            end;
          end;
      STATE_TRAIN_IDLE :
          begin
            // Waiting for something to do
            if Node^.TrainData.State and TS_SEND_PROXY_ALLOCATE_REPLY <> 0 then
              Node^.iUserStateMachine := STATE_TRAIN_ALLOCATE_PROXY_REPLY
            else begin
              if Assigned(Node^.UserData) then
              begin
                Task := TNodeTask( Node^.UserData);
                Node^.iUserStateMachine := Task.iStateMachine;                    // Start the new task...
              end;
            end
          end;
     end
  end;
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
  Node^.iUserStateMachine := STATE_PROXY_USER_START;
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

  EventTrainInfo := TNodeEventTrainInfo.Create(Node^.Info, nil);
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
procedure AppCallback_TractionProxyProtocol(Node: PNMRAnetNode; AMessage: POPStackMessage; SourceHasLock: Boolean);
var
  i: Integer;
  TrainID: Word;
  SpeedSteps: Byte;
  TrainNode: PNMRAnetNode;
  ConfigOffset: DWord;
  Train: TTrainConfig;
begin
  {$IFDEF DEBUG_TRACTION_PROXY_PROTOCOL} UART1_Write_Text('AppCallback_TractionProxyProtocol'+LF); {$ENDIF}
  // Only the main physical node is the proxy and can reply to these messages
  if Node = GetPhysicalNode then
  begin
    case AMessage^.Buffer^.DataArray[0] of
        TRACTION_PROXY_ALLOCATE :
          begin {$IFDEF DEBUG_TRACTION_PROXY_PROTOCOL} UART1_Write_Text('TRACTION_PROXY_ALLOCATE'+LF); {$ENDIF}
            // Extract Train ID
            TrainID := (AMessage^.Buffer^.DataArray[2] shl 8) or AMessage^.Buffer^.DataArray[3];
            // Extract Speed Steps
            if AMessage^.Buffer^.DataArray[1] and TRACTION_PROXY_TECH_ID_DCC <> 0 then
              SpeedSteps := AMessage^.Buffer^.DataArray[4]
            else
              SpeedSteps := 28;
            // Find or create the Train
            TrainNode := OPStackNode_FindByTrainID(TrainID);
            if TrainNode <> nil then
              TrainNode^.iUserStateMachine := STATE_TRAIN_ALLOCATE_PROXY_REPLY     // Node with this address exists, use it and set the state to send a Allocate Reply
            else begin
              TrainNode := OPStackNode_Allocate;
              TrainNode^.TrainData.State := TrainNode^.TrainData.State or TS_SEND_PROXY_ALLOCATE_REPLY;
           end;

           if TrainNode <> nil then
            begin
              // This node will now start and log in, then it will reply to the Allocate Message
              TrainNode^.TrainData.Address := TrainID;
              TrainNode^.TrainData.SpeedSteps := SpeedSteps;
              TrainNode^.TrainData.LinkedNode.ID[0] := AMessage^.Source.ID[0];      // Store the node that sent the Allocate message
              TrainNode^.TrainData.LinkedNode.ID[1] := AMessage^.Source.ID[1];
              TrainNode^.TrainData.LinkedNode.AliasID := AMessage^.Source.AliasID;

              ConfigOffset := USER_CONFIGURATION_MEMORY_SIZE + ((TrainNode^.iIndex - 1) * USER_VNODE_CONFIGURATION_MEMORY_SIZE);

              ZeroTrainConfiguration(Train);
              Train.RoadName := 'Address: ';
              Train.RoadNumber := IntToStr(TrainNode^.TrainData.Address and not $C000);
              Train.TrainClass := '';
              Train.Name := 'Transient DCC Node';
              Train.Manufacturer := 'Mustangpeak';
              Train.Owner := 'NMRA';
              Train.TrainID := TrainNode^.TrainData.Address;
              Train.SpeedStep := TrainNode^.TrainData.SpeedSteps;
              if TrainNode^.TrainData.Address and $C000 <> 0 then Train.ShortLong := 1 else Train.ShortLong := 0;

              WriteTrainConfiguration(ConfigOffset, Train);
            end;
          end;
    end
  end;
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
begin
  Inc(GlobalTimer);
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
