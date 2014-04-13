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
  {$ENDIF}
  opstacktypes,
  opstackdefines,
  template_node,
  opstack_api,
  olcb_defines,
  nmranetutilities;

procedure UserStateMachine_Initialize;
procedure AppCallback_UserStateMachine_Process;
procedure AppCallback_NodeInitialize(Node: PNMRAnetNode);

// Called every 100ms typically from another thread so only use to update flags
procedure AppCallback_Timer_100ms;

// These message are called from the mainstatemachine loop.  They have been stored in
// internal storage buffers.  See the notes to understand the implications of this and how to use them correctly
procedure AppCallback_SimpleNodeInfoReply(var Source: TNodeInfo; var Dest: TNodeInfo; NodeInfo: PAcdiSnipBuffer);
procedure AppCallBack_ProtocolSupportReply(var Source: TNodeInfo; var Dest: TNodeInfo; DataBytes: PSimpleBuffer);  // This could be 2 replies per call.. read docs
{$IFDEF SUPPORT_TRACTION}
function AppCallback_TractionProtocol(Node: PNMRAnetNode; var ReplyMessage, RequestingMessage: POPStackMessage): Boolean;
procedure AppCallback_SimpleTrainNodeInfoReply(var Source: TNodeInfo; var Dest: TNodeInfo; TrainNodeInfo: PAcdiSnipBuffer);
{$ENDIF}
{$IFDEF SUPPORT_TRACTION_PROXY}
function AppCallback_TractionProxyProtocol(Node: PNMRAnetNode; var ReplyMessage, RequestingMessage: POPStackMessage): Boolean;
{$ENDIF}

// These messages are called directly from the hardware receive buffer.  See the notes to understand the
// implications of this and how to use them correctly
procedure AppCallback_InitializationComplete(var Source: TNodeInfo; NodeID: PNodeID);
procedure AppCallback_VerifiedNodeID(var Source: TNodeInfo; NodeID: PNodeID);
procedure AppCallback_ConsumerIdentified(var Source: TNodeInfo; var Dest: TNodeInfo; MTI: Word; EventID: PEventID);
procedure AppCallback_ProducerIdentified(var Source: TNodeInfo; var Dest: TNodeInfo; MTI: Word; EventID: PEventID);
procedure AppCallback_LearnEvent(var Source: TNodeInfo; EventID: PEventID);
procedure AppCallBack_PCEventReport(var Source: TNodeInfo; EventID: PEventID);
procedure AppCallback_TractionControlReply(var Source: TNodeInfo; var Dest: TNodeInfo; DataBytes: PSimpleBuffer);  // Assumes we can make these all one frame long
procedure AppCallback_TractionProxyReply(var Source: TNodeInfo; var Dest: TNodeInfo; DataBytes: PSimpleBuffer);    // Assumes we can make these all one frame long
procedure AppCallback_RemoteButtonReply(var Source: TNodeInfo; var Dest: TNodeInfo; DataBytes: PSimpleBuffer);

{$IFNDEF FPC}
  procedure TractionProxyProtocolReply(Node: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage); external;
  procedure TractionProtocolReply(Node: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage); external;
{$ENDIF}

{$IFDEF FPC}
type
  TProcessState = (psClear, psReadyToProcess, psProcessing, psComplete);

  TAllocateDCCByProxy = record
    Process: TProcessState;
  end;

  TAllocateThrottleNode = record
    Process: TProcessState;
  end;

var
  Template_UserStateMachine_OnTaskDestroy: TOlcbTaskBeforeDestroy;
  OPStackCriticalSection: TRTLCriticalSection;
  AllocateThrottleNodeRec: TAllocateThrottleNode;
  AllocateDCCByProxyRec: TAllocateDCCByProxy;
{$ENDIF}

implementation

{$IFDEF FPC}
  {$IFDEF SUPPORT_TRACTION and SUPPORT_TRACTION_PROXY}
  uses
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
  STATE_USER_START = 0;
  STATE_USER_1     = 1;
  STATE_USER_2     = 2;
  STATE_USER_3     = 3;
  STATE_USER_4     = 4;
  STATE_USER_5     = 5;
  STATE_USER_6     = 6;
  STATE_USER_7     = 7;
  STATE_USER_8     = 8;
  STATE_USER_9     = 9;
  STATE_USER_10    = 10;

type
  // User Data for a single node
  TSampleUserNodeData = record
    UserData1 : Word;
    UserData2 : Byte;
  end;
  PSampleUserNodeData = ^TSampleUserNodeData;

  // Array of User Data for all Nodes
  TSampleUserDataArray = array[0..USER_MAX_NODE_COUNT-1] of TSampleUserNodeData;

var
  UserState: Word;
  UserDataArray: TSampleUserDataArray;

  ProxyNode: TNodeInfo;
  GlobalTimer: Word;


procedure ClearProxyNode;
begin
  ProxyNode.AliasID := 0;
  ProxyNode.ID[0] := 0;
  ProxyNode.ID[1] := 0;
end;

// *****************************************************************************
//  procedure ExtractUserData
//     Parameters: : Node : Pointer to the node that needs to be initilized to its intial value
//     Returns     : Pointer to the defined User Data type
//     Description : Nice helper function to type cast the user data generic pointer
//                   to a pointer to the actual data type
// *****************************************************************************
function ExtractUserData(Node: PNMRAnetNode): PSampleUserNodeData;
begin
  Result := PSampleUserNodeData( Node^.UserData)
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
  for i := 0 to USER_MAX_NODE_COUNT - 1 do
  begin
    UserDataArray[i].UserData1 := 0;
    UserDataArray[i].UserData2 := 0;
  end;
   // Initialize the example statemachine
  UserState := STATE_USER_START;
  ClearProxyNode;
end;

// *****************************************************************************
//  procedure AppCallback_UserStateMachine_Process
//     Parameters: : None
//     Returns     : None
//     Description : Called as often as possible to run the user statemachine
// *****************************************************************************
procedure AppCallback_UserStateMachine_Process;
var
  Node: PNMRAnetNode;
begin
  case UserState of
    STATE_USER_START :
        begin
          Node := GetPhysicalNode;
          if Node^.State and NS_INITIALIZED <> 0 then
          begin
            GlobalTimer := 0;
            if TrySendIdentifyProducer(Node^.Info, @EVENT_IS_PROXY) then
              UserState := STATE_USER_1;
          end;
        end;
    STATE_USER_1  :
        begin
          if (ProxyNode.AliasID > 0) or (ProxyNode.ID[0] > 0) or (ProxyNode.ID[1] > 0) then
            UserState := STATE_USER_2
          else begin
            if GlobalTimer > 10 then
              UserState := STATE_USER_START            // Try again
          end;
        end;
    STATE_USER_2  :
        begin

        end;
    STATE_USER_3  :
        begin

        end;
    STATE_USER_4  :
        begin

        end;
    STATE_USER_5  :
        begin

        end;
    STATE_USER_6  :
        begin

        end;
    STATE_USER_7  :
        begin

        end;
    STATE_USER_8  :
        begin

        end;
    STATE_USER_9   :
        begin

        end;
    STATE_USER_10  :
        begin

        end
  else begin
    end;
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
var
  NodeData: PSampleUserNodeData;
begin
  // Assign the user data record to the Node for future use
  Node^.UserData := @UserDataArray[Node^.iIndex];

  // Initialize the example data, evertime the node is reused!
  NodeData := ExtractUserData(Node);
  NodeData^.UserData1 := 0;
  NodeData^.UserData2 := 0;
end;

{$IFDEF SUPPORT_TRACTION}
// *****************************************************************************
//  procedure AppCallback_TractionProtocol
//     Parameters: : Node           : Pointer to the node that the traction protocol has been called on
//                   ReplyMessage   : The Reply Message that needs to be allocated, populated and returned so it can be sent
//                   RequestingMessage    : Message that was sent to the node containing the requested information
//     Returns     : True if the RequestingMessage is handled and the ReplyMessage is ready to send
//                   False if the request has not been completed due to no available buffers or waiting on other information
//     Description : This is called from the main statemachine so the Requesting Message is allocated from
//                   the internal buffer queue.  It is recommended that the message get handled quickly and released.
//                   The internal system can not process other incoming messages that require a reply until this message
//                   is cleared.  This means that if a reply can not be sent until another message is sent/received this
//                   will block that second message.  If that is required then return True with ReplyMessage = nil to
//                   release the Requesting message then send the reply to this message at a later time
// *****************************************************************************
function AppCallback_TractionProtocol(Node: PNMRAnetNode; var ReplyMessage, RequestingMessage: POPStackMessage): Boolean;
begin
  // Default reply handled automatically by OPStack
  TractionProtocolReply(Node, ReplyMessage, RequestingMessage);
  Result := ReplyMessage <> nil
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
//     Description : This is called from the main statemachine so the Requesting Message is allocated from
//                   the internal buffer queue.  It is recommended that the message get handled quickly and released.
//                   The internal system can not process other incoming messages that require a reply until this message
//                   is cleared.  This means that if a reply can not be sent until another message is sent/received this
//                   will block that second message.  If that is required then return True with ReplyMessage = nil to
//                   release the Requesting message then send the reply to this message at a later time
// *****************************************************************************
function AppCallback_TractionProxyProtocol(Node: PNMRAnetNode; var ReplyMessage, RequestingMessage: POPStackMessage): Boolean;
begin
  // Default reply handled automatically by OPStack
  TractionProxyProtocolReply(Node, ReplyMessage, RequestingMessage);
  Result := ReplyMessage <> nil
end;
{$ENDIF}

// *****************************************************************************
//  procedure AppCallBack_ProtocolSupportReply
//     Parameters: : Source : Full Node ID (and Alias if on CAN) of the source node for the message
//                   Dest   : Full Node ID (and Alias if on CAN) of the dest node for the message
//                   DataBytes: pointer Raw data bytes, Byte 0 and 1 are the Alias
//     Returns     : None
//     Description : This is called directly from the Hardware receive buffer.  Do
//                   not do anything here that stalls the call.  This is called
//                   Asyncronously from the Statemachine loop and the Statemachine loop
//                   is stalled until this returns.  Set a flag and move on is the
//                   best stratagy or store info in a buffer and process in the
//                   main statemachine.
// *****************************************************************************
procedure AppCallBack_ProtocolSupportReply(var Source: TNodeInfo; var Dest: TNodeInfo; DataBytes: PSimpleBuffer);
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
procedure AppCallback_ConsumerIdentified(var Source: TNodeInfo; var Dest: TNodeInfo; MTI: Word; EventID: PEventID);
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
procedure AppCallback_ProducerIdentified(var Source: TNodeInfo; var Dest: TNodeInfo; MTI: Word; EventID: PEventID);
{$IFDEF FPC}
var
  Task: TTaskSimpleNodeInformation;
{$ENDIF}
begin
  if GetPhysicalNode^.State and NS_INITIALIZED > 0 then
  begin
    // Staring looking for a Proxy to use
    if NMRAnetUtilities_EqualEventID(EventID, @EVENT_IS_PROXY) then
    begin
      ProxyNode := Source;
      {$IFDEF FPC}
      if Assigned(Template_UserStateMachine_OnTaskDestroy) then
      begin
        Task := TTaskSimpleNodeInformation.Create(GetPhysicalNode^.Info.AliasID, ProxyNode.AliasID, True);
        Task.OnBeforeDestroy := Template_UserStateMachine_OnTaskDestroy;
        EthernetHub.AddTask(Task);
        ComPortHub.AddTask(Task);
        Task.Free;
      end;
      {$ENDIF}
    end;
  end;
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
//  procedure AppCallback_TractionControlReply
//     Parameters: : Source : Full Node ID (and Alias if on CAN) of the source node for the message
//                   Dest   : Full Node ID (and Alias if on CAN) of the dest node for the message
//                   DataBytes: pointer to the raw data bytes
//     Returns     : None
//     Description : This is called directly from the Hardware receive buffer.  Do
//                   not do anything here that stalls the call.  This is called
//                   Asyncronously from the Statemachine loop and the Statemachine loop
//                   is stalled until this returns.  Set a flag and move on is the
//                   best stratagy or store info in a buffer and process in the
//                   main statemachine.
// *****************************************************************************
procedure AppCallback_TractionControlReply(var Source: TNodeInfo; var Dest: TNodeInfo; DataBytes: PSimpleBuffer);
begin

end;

// *****************************************************************************
//  procedure AppCallback_TractionProxyReply
//     Parameters: : Source : Full Node ID (and Alias if on CAN) of the source node for the message
//                   Dest   : Full Node ID (and Alias if on CAN) of the dest node for the message
//                   DataBytes: pointer to the raw data bytes
//     Returns     : None
//     Description : This is called directly from the Hardware receive buffer.  Do
//                   not do anything here that stalls the call.  This is called
//                   Asyncronously from the Statemachine loop and the Statemachine loop
//                   is stalled until this returns.  Set a flag and move on is the
//                   best stratagy or store info in a buffer and process in the
//                   main statemachine.
// *****************************************************************************
procedure AppCallback_TractionProxyReply(var Source: TNodeInfo; var Dest: TNodeInfo; DataBytes: PSimpleBuffer);
begin

end;

// *****************************************************************************
//  procedure AppCallback_RemoteButtonReply
//     Parameters: : Source : Full Node ID (and Alias if on CAN) of the source node for the message
//                   Dest   : Full Node ID (and Alias if on CAN) of the dest node for the message
//                   DataBytes: pointer to the raw data bytes
//     Returns     : None
//     Description : This is called directly from the Hardware receive buffer.  Do
//                   not do anything here that stalls the call.  This is called
//                   Asyncronously from the Statemachine loop and the Statemachine loop
//                   is stalled until this returns.  Set a flag and move on is the
//                   best stratagy or store info in a buffer and process in the
//                   main statemachine.
// *****************************************************************************
procedure AppCallback_RemoteButtonReply(var Source: TNodeInfo; var Dest: TNodeInfo; DataBytes: PSimpleBuffer);
begin

end;

{$IFDEF SUPPORT_TRACTION}
// *****************************************************************************
//  procedure AppCallback_SimpleTrainNodeInfoReply
//     Parameters: : Source : Full Node ID (and Alias if on CAN) of the source node for the message
//                   Dest   : Full Node ID (and Alias if on CAN) of the dest node for the message
//                   TrainNodeInfo: pointer to the null terminated strings
//     Returns     : None
//     Description : This is called directly from the Hardware receive buffer.  Do
//                   not do anything here that stalls the call.  This is called
//                   Asyncronously from the Statemachine loop and the Statemachine loop
//                   is stalled until this returns.  Set a flag and move on is the
//                   best stratagy or store info in a buffer and process in the
//                   main statemachine.
// *****************************************************************************
procedure AppCallback_SimpleTrainNodeInfoReply(var Source: TNodeInfo; var Dest: TNodeInfo; TrainNodeInfo: PAcdiSnipBuffer);
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
//     Description : This is called directly from the Hardware receive buffer.  Do
//                   not do anything here that stalls the call.  This is called
//                   Asyncronously from the Statemachine loop and the Statemachine loop
//                   is stalled until this returns.  Set a flag and move on is the
//                   best stratagy or store info in a buffer and process in the
//                   main statemachine.
// *****************************************************************************
procedure AppCallback_SimpleNodeInfoReply(var Source: TNodeInfo; var Dest: TNodeInfo; NodeInfo: PAcdiSnipBuffer);
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
initialization
  Template_UserStateMachine_OnTaskDestroy := nil;
  InitCriticalSection( OPStackCriticalSection);

  AllocateThrottleNodeRec.Process := psClear;
  AllocateDCCByProxyRec.Process := psClear;

Finalization
  DoneCriticalsection( OPStackCriticalSection);

{$ENDIF}

end.


