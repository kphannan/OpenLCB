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
  NMRAnetCabBridgeDefines,
  Float16,
  opstacktypes,
  opstackdefines,
  template_node,
  opstack_api,
  nmranetdefines,
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

procedure AppCallBack_ConfigMemReadReply(Node: PNMRAnetNode; AMessage: POPStackMessage; Success: Boolean);
procedure AppCallBack_ConfigMemStreamReadReply(Node: PNMRAnetNode; AMessage: POPStackMessage; Success: Boolean);
procedure AppCallBack_ConfigMemWriteReply(Node: PNMRAnetNode; AMessage: POPStackMessage; Success: Boolean);
procedure AppCallBack_ConfigMemStreamWriteReply(Node: PNMRAnetNode; AMessage: POPStackMessage; Success: Boolean);

const
   STATE_CAB_SEND_QUERY = 0;
   STATE_CAB_WAIT_QUERY = 1;
   STATE_CAB_DONE_QUERY = 2;

const
   STATE_THROTTLE_ROOT_USER_START          = 0;
   STATE_THROTTLE_ROOT_FIND_PROXY          = 1;
   STATE_THROTTLE_ROOT_IDLE                = 2;
   STATE_THROTTLE_ROOT_ALLOCATE_NEW        = 3;

   STATE_THROTTLE_USER_START                 = 0;
   STATE_THROTTLE_FIND_PROXY                 = 1;
   STATE_THROTTLE_IDLE                       = 2;
   STATE_THROTTLE_LOG_IN_AND_NOTIFY          = 3;
   STATE_THROTTLE_FREE                       = 4;
   STATE_THROTTLE_ALLOCATE_TRAIN_BY_ADDRESS  = 5;
   STATE_THROTTLE_RELEASE_CONTROLLER         = 6;
   STATE_THROTTLE_DIRECTION_FORWARD          = 7;
   STATE_THROTTLE_DIRECTION_REVERSE          = 8;
   STATE_THROTTLE_SPEED_CHANGE               = 9;
   STATE_THROTTLE_FUNCTION                   = 10;
   STATE_THROTTLE_E_STOP                     = 11;
   STATE_THROTTLE_QUERY_SPEED                = 12;
   STATE_THROTTLE_QUERY_FUNCTION             = 13;
   STATE_THROTTLE_FIND_TRAINS                = 14;
   STATE_THROTTLE_FIND_SIMPLE_TRAIN_INFO     = 15;
   STATE_THROTTLE_PROTOCOL_SUPPORT           = 16;
   STATE_THROTTLE_READ_FDI                   = 17;

   STATE_THROTTLE_FIND_SIMPLE_TRAIN_INFO_START = 0;
   STATE_THROTTLE_FIND_SIMPLE_TRAIN_INFO_WAIT  = 1;
   STATE_THROTTLE_FIND_SIMPLE_TRAIN_INFO_DONE  = 2;

   STATE_THROTTLE_PROTOCOL_SUPPORT_SEND        = 0;
   STATE_THROTTLE_PROTOCOL_SUPPORT_END         = 1;
   STATE_THROTTLE_PROTOCOL_SUPPORT_WAIT        = 2;

   STATE_THROTTLE_READ_FDI_SEND                = 0;
   STATE_THROTTLE_READ_FDI_END                 = 1;
   STATE_THROTTLE_READ_FDI_WAIT                = 2;

var
  ProxyNode: TNodeInfo;
  GlobalTimer: Word;


implementation

{$IFDEF FPC}
  {$IFDEF SUPPORT_TRACTION and SUPPORT_TRACTION_PROXY}
  uses
    NMRAnetCabBridge,
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
  Task: TNodeTask;
  NewNodeCreatedEvent: TNodeEventNodeCreated;
  NewNode: PNMRAnetNode;
  TrainAllocateByAddressTask: TNodeTaskAllocateTrainByAddress;
  TrainReleaseController: TNodeTaskReleaseController;
  NewFunctionValue, Address: Word;
begin
  if Node = GetPhysicalNode then
  begin
    case Node^.iUserStateMachine of
      STATE_THROTTLE_ROOT_USER_START :  // Create the minimum number of Pings to put on the NCE bus to make it happy
          begin
            if Node^.State and NS_PERMITTED <> 0 then
            begin
              GlobalTimer := 0;
              if TrySendIdentifyProducer(Node^.Info, @EVENT_IS_PROXY) then
                Node^.iUserStateMachine := STATE_THROTTLE_ROOT_FIND_PROXY;
            end;
            Exit;
          end;
      STATE_THROTTLE_ROOT_FIND_PROXY :   // Find the Proxy node (Command Station) on the network before progressing
          begin
            if (ProxyNode.AliasID > 0) or (ProxyNode.ID[0] > 0) or (ProxyNode.ID[1] > 0) then
              Node^.iUserStateMachine := STATE_THROTTLE_ROOT_IDLE
            else begin
              if GlobalTimer > 10 then
                Node^.iUserStateMachine := STATE_THROTTLE_ROOT_USER_START       // Try again
            end;
            Exit;
          end;
      STATE_THROTTLE_ROOT_IDLE :
          begin
            if Assigned(Node^.UserData) then
            begin
              Task := TNodeTask( Node^.UserData);
              Node^.iUserStateMachine := Task.iStateMachine;
            end;
            Exit;
          end;
      STATE_THROTTLE_ROOT_ALLOCATE_NEW :
          begin
            Task := TNodeTask( Node^.UserData);
            NewNode := OPStackNode_Allocate;
            LinkTaskToNode(NewNode, Task);                                      // Give the task to the new node
            UnLinkFirstTaskFromNode(Node, False);                               // Remove the task from the root node
            Task := TNodeTask( NewNode^.UserData);
            Task.iStateMachine := STATE_THROTTLE_LOG_IN_AND_NOTIFY   ;          // Set the task to the virtual nodes state to send the notification
            Node^.iUserStateMachine := STATE_THROTTLE_ROOT_IDLE;                // Look for more tasks
          end;
    end
  end else
  begin
    case Node^.iUserStateMachine of
      STATE_THROTTLE_USER_START :
          begin
            if Node^.State and NS_PERMITTED <> 0 then
            begin
              Node^.iUserStateMachine := STATE_THROTTLE_IDLE;
            end;
            Exit;
          end;
      STATE_THROTTLE_IDLE :
          begin
            if Assigned(Node^.UserData) then
            begin
              Task := TNodeTask( Node^.UserData);
              Node^.iUserStateMachine := Task.iStateMachine;                    // Start the new task...
            end;
            Exit;
          end;
        STATE_THROTTLE_LOG_IN_AND_NOTIFY :
          begin
            Task := TNodeTask( Node^.UserData);
            NodeThread.AddEvent( TNodeEventNodeCreated.Create(Node^.Info, Task.LinkedObj));
            UnLinkFirstTaskFromNode(Node, True);                               // Remove the task from the node and free it
            Node^.iUserStateMachine := STATE_THROTTLE_IDLE;                     // Look for more tasks
          end;
        STATE_THROTTLE_FREE :
          begin
            OPStackNode_MarkForRelease(Node);
            Node^.iUserStateMachine := STATE_THROTTLE_IDLE;                     // Look for more tasks
          end;
        STATE_THROTTLE_ALLOCATE_TRAIN_BY_ADDRESS :
          begin
              TrainAllocateByAddressTask := TNodeTaskAllocateTrainByAddress( Node^.UserData);
              case TrainAllocateByAddressTask.iSubStateMachine of
                STATE_SUB_BRIDGE_INITIALIZE :
                    begin
                       TrainAllocateByAddressTask.iSubStateMachine := STATE_CAB_SELECT_LOCO_SEND_PROXY_MANAGE_LOCK;
                       Exit;
                    end;
                STATE_CAB_SELECT_LOCO_SEND_PROXY_MANAGE_LOCK :
                    begin
                      if TrySendTractionProxyManage(Node^.Info, ProxyNode, True) then
                        TrainAllocateByAddressTask.iSubStateMachine := STATE_CAB_SELECT_LOCO_GENERIC_REPLY_WAIT; // Wait for the Manage Reply Callback
                      TrainAllocateByAddressTask.WatchDog := 0;
                      Exit;
                    end;
                STATE_CAB_SELECT_LOCO_SEND_PROXY_ALLOCATE :
                    begin
                      Address := TrainAllocateByAddressTask.Address;
                      if TrainAllocateByAddressTask.Long then
                        Address := Address or $C000;
                      if TrySendTractionProxyAllocate(Node^.Info, ProxyNode, TRACTION_PROXY_TECH_ID_DCC, Address, TrainAllocateByAddressTask.SpeedStep, 0) then
                        TrainAllocateByAddressTask.iSubStateMachine := STATE_CAB_SELECT_LOCO_GENERIC_REPLY_WAIT;  // Wait for the Allocate Reply Callback
                      TrainAllocateByAddressTask.WatchDog := 0;
                      Exit;
                    end;
                STATE_CAB_SELECT_LOCO_SEND_PROXY_MANAGE_UNLOCK :
                    begin
                      if TrySendTractionProxyManage(Node^.Info, ProxyNode, False) then
                        TrainAllocateByAddressTask.iSubStateMachine := STATE_CAB_SELECT_LOCO_SEND_TRACTION_MANAGE_LOCK; // No Reply for Unlock
                      TrainAllocateByAddressTask.WatchDog := 0;
                      Exit;
                    end;
                STATE_CAB_SELECT_LOCO_SEND_TRACTION_MANAGE_LOCK :
                    begin
                      if TrySendTractionManage(Node^.Info, Node^.TrainData.LinkedNode, True) then
                        TrainAllocateByAddressTask.iSubStateMachine := STATE_CAB_SELECT_LOCO_GENERIC_REPLY_WAIT;  // Wait for the Lock Reply Callback
                      TrainAllocateByAddressTask.WatchDog := 0;
                      Exit;
                    end;
                STATE_CAB_SELECT_LOCO_SEND_TRACTION_ASSIGN_CONTROLLER :
                    begin
                      if TrySendTractionControllerConfig(Node^.Info, Node^.TrainData.LinkedNode, Node^.Info, True) then
                        TrainAllocateByAddressTask.iSubStateMachine := STATE_CAB_SELECT_LOCO_GENERIC_REPLY_WAIT;  // Wait for the Lock Reply Callback
                      TrainAllocateByAddressTask.WatchDog := 0;
                      Exit;
                    end;
                STATE_CAB_SELECT_LOCO_SEND_TRACTION_QUERY_SPEED :
                    begin
                      if TrySendTractionQuerySpeed(Node^.Info, Node^.TrainData.LinkedNode) then
                        TrainAllocateByAddressTask.iSubStateMachine := STATE_CAB_SELECT_LOCO_GENERIC_REPLY_WAIT;  // Wait for the Lock Reply Callback
                      TrainAllocateByAddressTask.FunctionIndex := 0;
                      TrainAllocateByAddressTask.WatchDog := 0;
                      Exit;
                    end;
                STATE_CAB_SELECT_LOCO_SEND_TRACTION_QUERY_FUNCTIONS :
                    begin
                      if TrySendTractionQueryFunction(Node^.Info, Node^.TrainData.LinkedNode, TrainAllocateByAddressTask.FunctionIndex) then
                        TrainAllocateByAddressTask.iSubStateMachine := STATE_CAB_SELECT_LOCO_GENERIC_REPLY_WAIT;  // Wait for the Lock Reply Callback
                      TrainAllocateByAddressTask.WatchDog := 0;
                      Exit;
                    end;
                STATE_CAB_SELECT_LOCO_SEND_TRACTION_MANAGE_UNLOCK :
                    begin
                      if TrySendTractionManage(Node^.Info, Node^.TrainData.LinkedNode, False) then
                      begin
                        UnLinkFirstTaskFromNode(Node, True);
                        Node^.iUserStateMachine := STATE_THROTTLE_IDLE;                                // We are done....  Look for more tasks
                      end;
                      TrainAllocateByAddressTask.WatchDog := 0;
                      Exit;
                    end;
                STATE_CAB_SELECT_LOCO_GENERIC_REPLY_WAIT :
                    begin
                      if TrainAllocateByAddressTask.WatchDog > 50 then         // Waiting for the Reply to come into a callback
                      begin
                        TrainAllocateByAddressTask.iSubStateMachine := STATE_CAB_SELECT_LOCO_GENERIC_TIMEOUT_PROXY_UNLOCK;    // Force unlocks and exit
                      end;
                      Exit;
                    end;
                STATE_CAB_SELECT_LOCO_GENERIC_TIMEOUT_PROXY_UNLOCK :
                    begin
                      if TrySendTractionProxyManage(Node^.Info, ProxyNode, False) then   // Unsure if we are locked or not, just release just in case
                        TrainAllocateByAddressTask.iSubStateMachine := STATE_CAB_SELECT_LOCO_SEND_TRACTION_MANAGE_UNLOCK; // No Reply for Unlock, just unlock the Traction Protcol and end
                      Exit;
                    end;
              end;
          end;
        STATE_THROTTLE_RELEASE_CONTROLLER :
            begin
              TrainReleaseController := TNodeTaskReleaseController( Node^.UserData);
              case TrainReleaseController.iSubStateMachine of
                STATE_THROTTLE_RELEASE_CONTROLLER_INITIALIZE :
                    begin
                      TrainReleaseController.WatchDog := 0;
                      TrainReleaseController.iSubStateMachine := STATE_THROTTLE_RELEASE_CONTROLLER_SEND_TRACTION_LOCK;
                      Exit;
                    end;
                STATE_THROTTLE_RELEASE_CONTROLLER_SEND_TRACTION_LOCK :
                    begin
                      if TrySendTractionManage(Node^.Info, Node^.TrainData.LinkedNode, True) then
                        TrainReleaseController.iSubStateMachine := STATE_THROTTLE_RELEASE_CONTROLLER_WAIT;  // Wait for the Lock Reply Callback
                      TrainReleaseController.WatchDog := 0;
                      Exit;
                    end;
                STATE_THROTTLE_RELEASE_CONTROLLER_SEND_TRACTION_RELEASE :
                    begin
                      if TrySendTractionControllerConfig(Node^.Info, Node^.TrainData.LinkedNode, Node^.Info, False) then
                      begin
                        Node^.TrainData.LinkedNode.AliasID := 0;
                        Node^.TrainData.LinkedNode.ID := NULL_NODE_ID;
                        NodeThread.AddEvent(TNodeEventReleaseController.Create(Node^.Info, TrainReleaseController.LinkedObj));
                        TrainReleaseController.iSubStateMachine := STATE_THROTTLE_RELEASE_CONTROLLER_SEND_TRACTION_UNLOCK;  // There is no reply to this message
                      end;
                      TrainReleaseController.WatchDog := 0;
                      Exit;
                    end;
                STATE_THROTTLE_RELEASE_CONTROLLER_SEND_TRACTION_UNLOCK :
                    begin
                      if TrySendTractionManage(Node^.Info, Node^.TrainData.LinkedNode, False) then
                      begin
                        UnLinkFirstTaskFromNode(Node, True);
                        Node^.iUserStateMachine := STATE_THROTTLE_IDLE;                     // Look for more tasks
                      end;
                      Exit;
                    end;
                STATE_THROTTLE_RELEASE_CONTROLLER_WAIT :
                    begin
                      if TrainReleaseController.WatchDog > 20 then
                        TrainReleaseController.iSubStateMachine := STATE_THROTTLE_RELEASE_CONTROLLER_SEND_TRACTION_UNLOCK;   // Something is wrong, bail out
                      Exit;
                    end;
              end;
            end;
        STATE_THROTTLE_DIRECTION_FORWARD :
            begin
            end;
        STATE_THROTTLE_DIRECTION_REVERSE :
            begin
            end;
        STATE_THROTTLE_SPEED_CHANGE :
            begin
              if not NMRAnetUtilities_NullNodeIDInfo(Node^.TrainData.LinkedNode) then
              begin
                if TrySendTractionSpeedSet(Node^.Info, Node^.TrainData.LinkedNode, TNodeTaskSpeedDir( Node^.UserData).SpeedDir) then
                begin
                  UnLinkFirstTaskFromNode(Node, True);
                  Node^.iUserStateMachine := STATE_THROTTLE_IDLE; // We are done....
                end;
              end else
              begin
                UnLinkFirstTaskFromNode(Node, True);
                Node^.iUserStateMachine := STATE_THROTTLE_IDLE
              end
            end;
        STATE_THROTTLE_FUNCTION :
            begin
              if not NMRAnetUtilities_NullNodeIDInfo(Node^.TrainData.LinkedNode) then
              begin
                NewFunctionValue := 0;
                if ToggleFunction(Node, Node^.TrainData.Functions, TNodeTaskFunction( Node^.UserData).Address, NewFunctionValue) then
                begin
                  UnLinkFirstTaskFromNode(Node, True);
                  Node^.iUserStateMachine := STATE_THROTTLE_IDLE; // We are done....
                end;
              end else
              begin
                UnLinkFirstTaskFromNode(Node, True);
                Node^.iUserStateMachine := STATE_THROTTLE_IDLE
              end
            end;
        STATE_THROTTLE_E_STOP :
            begin
              if not NMRAnetUtilities_NullNodeIDInfo(Node^.TrainData.LinkedNode) then
              begin
                if TrySendTractionEmergencyStop(Node^.Info, Node^.TrainData.LinkedNode) then
                begin
                  Node^.iUserStateMachine := STATE_THROTTLE_IDLE; // We are done....
                  UnLinkFirstTaskFromNode(Node, True);
                end;
              end else
              begin
                UnLinkFirstTaskFromNode(Node, True);
                Node^.iUserStateMachine := STATE_THROTTLE_IDLE
              end
            end;
        STATE_THROTTLE_QUERY_SPEED :
            begin
              if not NMRAnetUtilities_NullNodeIDInfo(Node^.TrainData.LinkedNode) then
              begin
                case TNodeTask( Node^.UserData).iSubStateMachine of
                    STATE_CAB_SEND_QUERY :
                        begin
                          if not NMRAnetUtilities_NullNodeIDInfo(Node^.TrainData.LinkedNode) then
                          begin
                            if TrySendTractionQuerySpeed(Node^.Info, Node^.TrainData.LinkedNode) then
                              TNodeTask( Node^.UserData).iSubStateMachine := STATE_CAB_WAIT_QUERY;        // Wait for response
                          end else
                          begin
                            UnLinkFirstTaskFromNode(Node, True);
                            Node^.iUserStateMachine := STATE_THROTTLE_IDLE
                          end
                        end;
                    STATE_CAB_WAIT_QUERY :
                        begin
                          if TNodeTask( Node^.UserData).WatchDog > 20 then         // Waiting for the Reply to come into a callback
                          begin
                             UnLinkFirstTaskFromNode(Node, True);
                             Node^.iUserStateMachine := STATE_THROTTLE_IDLE;      // We are done
                          end;
                        end;
                    STATE_CAB_DONE_QUERY :
                        begin
                          UnLinkFirstTaskFromNode(Node, True);
                          Node^.iUserStateMachine := STATE_THROTTLE_IDLE;      // We are done
                        end
                  end
                end
            end;
        STATE_THROTTLE_QUERY_FUNCTION :
            begin
              if not NMRAnetUtilities_NullNodeIDInfo(Node^.TrainData.LinkedNode) then
              begin
                case TNodeTask( Node^.UserData).iSubStateMachine of
                    STATE_CAB_SEND_QUERY :
                        begin
                          if not NMRAnetUtilities_NullNodeIDInfo(Node^.TrainData.LinkedNode) then
                          begin
                            if TrySendTractionQueryFunction(Node^.Info, Node^.TrainData.LinkedNode, TNodeTaskFunctionQuery( Node^.UserData).Address) then
                              TNodeTask( Node^.UserData).iSubStateMachine := STATE_CAB_WAIT_QUERY;        // Wait for response
                          end else
                          begin
                            UnLinkFirstTaskFromNode(Node, True);
                            Node^.iUserStateMachine := STATE_THROTTLE_IDLE
                          end
                        end;
                    STATE_CAB_WAIT_QUERY :
                        begin
                          if TNodeTask( Node^.UserData).WatchDog > 20 then         // Waiting for the Reply to come into a callback
                          begin
                             UnLinkFirstTaskFromNode(Node, True);
                             Node^.iUserStateMachine := STATE_THROTTLE_IDLE;      // We are done
                          end;
                        end;
                    STATE_CAB_DONE_QUERY :
                        begin
                          UnLinkFirstTaskFromNode(Node, True);
                          Node^.iUserStateMachine := STATE_THROTTLE_IDLE;      // We are done
                        end
                  end
                end
              end;
        STATE_THROTTLE_FIND_TRAINS :
              begin
                if TrySendIdentifyProducer(Node^.Info, @EVENT_IS_TRAIN) then
                begin
                  UnLinkFirstTaskFromNode(Node, True);
                  Node^.iUserStateMachine := STATE_THROTTLE_IDLE;      // We are done
                end;
              end;
        STATE_THROTTLE_FIND_SIMPLE_TRAIN_INFO :
              begin
             //   if TrySendSupportInquiry(Node^.Info, TNodeTaskSimpleTrainNodeInfo( Node^.UserData).FDestNodeInfo) then
             //   begin
             //   end;
                case TNodeTask( Node^.UserData).iSubStateMachine of
                   STATE_THROTTLE_FIND_SIMPLE_TRAIN_INFO_START :
                     begin
                       if TrySendStnipRequest(Node^.Info, TNodeTaskSimpleTrainNodeInfo( Node^.UserData).FDestNodeInfo) then
                       begin
                         TNodeTask( Node^.UserData).Watchdog := 0;
                         TNodeTask( Node^.UserData).iSubStateMachine := STATE_THROTTLE_FIND_SIMPLE_TRAIN_INFO_WAIT;
                       end;
                     end;
                   STATE_THROTTLE_FIND_SIMPLE_TRAIN_INFO_WAIT :
                     begin
                       if  TNodeTask( Node^.UserData).Watchdog > 20 then
                         TNodeTask( Node^.UserData).iSubStateMachine := STATE_THROTTLE_FIND_SIMPLE_TRAIN_INFO_DONE
                     end;
                  STATE_THROTTLE_FIND_SIMPLE_TRAIN_INFO_DONE :
                     begin
                       Node^.iUserStateMachine := STATE_THROTTLE_IDLE;      // We are done
                       UnLinkFirstTaskFromNode(Node, True);
                     end;
                end;
              end;
          STATE_THROTTLE_PROTOCOL_SUPPORT :
             begin
               case TNodeTask( Node^.UserData).iSubStateMachine of
                 STATE_THROTTLE_PROTOCOL_SUPPORT_SEND :
                    begin
                      if TrySendSupportInquiry(Node^.Info, TNodeTaskSupportsProtocols( Node^.UserData).FDestNodeInfo) then
                        TNodeTask( Node^.UserData).iSubStateMachine := STATE_THROTTLE_PROTOCOL_SUPPORT_WAIT
                    end;
                 STATE_THROTTLE_PROTOCOL_SUPPORT_END :
                    begin
                      Node^.iUserStateMachine := STATE_THROTTLE_IDLE;      // We are done
                      UnLinkFirstTaskFromNode(Node, True);
                    end;
                 STATE_THROTTLE_PROTOCOL_SUPPORT_WAIT :
                    begin
                      if  TNodeTask( Node^.UserData).Watchdog > 20 then
                        TNodeTask( Node^.UserData).iSubStateMachine := STATE_THROTTLE_PROTOCOL_SUPPORT_END
                    end;
               end
             end;
          STATE_THROTTLE_READ_FDI :
             begin
               case TNodeTask( Node^.UserData).iSubStateMachine of
                 STATE_THROTTLE_READ_FDI_SEND :
                    begin
                      if TrySendConfigMemoryRead(Node, TNodeTaskReadConfigMemory( Node^.UserData).FDestNodeInfo, MSI_FDI, TNodeTaskReadConfigMemory( Node^.UserData).CurrentAddress, 64) then
                        TNodeTask( Node^.UserData).iSubStateMachine := STATE_THROTTLE_READ_FDI_WAIT;
                      TNodeTask( Node^.UserData).Watchdog := 0;
                    end;
                 STATE_THROTTLE_READ_FDI_END :
                    begin
                      UnLinkFirstTaskFromNode(Node, True);
                      Node^.iUserStateMachine := STATE_THROTTLE_IDLE;      // We are done
                    end;
                 STATE_THROTTLE_READ_FDI_WAIT :
                    begin
                      if  TNodeTask( Node^.UserData).Watchdog > 20 then
                        TNodeTask( Node^.UserData).iSubStateMachine := STATE_THROTTLE_READ_FDI_END
                    end;
               end
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
  if Node = GetPhysicalNode then
    Node^.iUserStateMachine := STATE_THROTTLE_ROOT_USER_START
  else
    Node^.iUserStateMachine := STATE_THROTTLE_USER_START;
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
begin

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
  TrainAllocateByAddressTask: TNodeTaskAllocateTrainByAddress;
  FunctionAddress: DWord;
  FunctionValue: Word;
  Mask: DWord;
begin
  MultiFrameBuffer := PMultiFrameBuffer( PByte( AMessage^.Buffer));

  case MultiFrameBuffer^.DataArray[0] of
    TRACTION_QUERY_SPEED :
        begin
          Node^.TrainData.SpeedDir := (MultiFrameBuffer^.DataArray[1] shl 8) or MultiFrameBuffer^.DataArray[2];   // Update our local copy

          if TNodeTask( Node^.UserData) is TNodeTaskAllocateTrainByAddress then
          begin
            TNodeTask( Node^.UserData).iSubStateMachine := STATE_CAB_SELECT_LOCO_SEND_TRACTION_QUERY_FUNCTIONS;
            NodeThread.AddEvent( TNodeEventSpeedDirQuery.Create( Node^.Info, TNodeTask( Node^.UserData).LinkedObj, (MultiFrameBuffer^.DataArray[1] shr 8) or (MultiFrameBuffer^.DataArray[2])));
          end else
          if TNodeTask( Node^.UserData) is TNodeTaskSpeedDirQuery then
          begin
            NodeThread.AddEvent( TNodeEventSpeedDirQuery.Create( Node^.Info, TNodeTask( Node^.UserData).LinkedObj, Node^.TrainData.SpeedDir));
            TNodeTask( Node^.UserData).iSubStateMachine := STATE_CAB_DONE_QUERY
          end
        end;
    TRACTION_QUERY_FUNCTION :
        begin
          FunctionAddress := (MultiFrameBuffer^.DataArray[1] shr 16) or (MultiFrameBuffer^.DataArray[2] shr 8) or MultiFrameBuffer^.DataArray[3];
          FunctionValue := (MultiFrameBuffer^.DataArray[4] shr 8) or MultiFrameBuffer^.DataArray[5];

          Mask := $00000001;
          Mask := Mask shl FunctionAddress;
          if FunctionValue = 0 then
            Node^.TrainData.Functions := Node^.TrainData.Functions and not Mask
          else
            Node^.TrainData.Functions := Node^.TrainData.Functions or Mask;

          if TNodeTask( Node^.UserData) is TNodeTaskAllocateTrainByAddress then
          begin
            TrainAllocateByAddressTask := TNodeTaskAllocateTrainByAddress( Node^.UserData);
            if TrainAllocateByAddressTask.FunctionIndex < 28 then
            begin
              TrainAllocateByAddressTask.FunctionIndex := TrainAllocateByAddressTask.FunctionIndex + 1;
              TNodeTask( Node^.UserData).iSubStateMachine := STATE_CAB_SELECT_LOCO_SEND_TRACTION_QUERY_FUNCTIONS;
              NodeThread.AddEvent( TNodeEventFunctionQuery.Create( Node^.Info, TNodeTask( Node^.UserData).LinkedObj, FunctionAddress, FunctionValue));
            end else
              TNodeTask( Node^.UserData).iSubStateMachine := STATE_CAB_SELECT_LOCO_SEND_TRACTION_MANAGE_UNLOCK
          end else
          if TNodeTask( Node^.UserData) is TNodeTaskFunctionQuery then
          begin
            NodeThread.AddEvent( TNodeEventFunctionQuery.Create( Node^.Info, TNodeTask( Node^.UserData).LinkedObj, FunctionAddress, FunctionValue));
            TNodeTask( Node^.UserData).iSubStateMachine := STATE_CAB_DONE_QUERY
          end
        end;
    TRACTION_CONTROLLER_CONFIG :
        begin
          case MultiFrameBuffer^.DataArray[1] of
            TRACTION_CONTROLLER_CONFIG_ASSIGN :
                begin
                  case Node^.iUserStateMachine of
                    STATE_THROTTLE_ALLOCATE_TRAIN_BY_ADDRESS :
                        begin
                          if MultiFrameBuffer^.DataArray[2] = TRACTION_CONTROLLER_ASSIGN_REPLY_OK then
                          begin
                            NodeThread.AddEvent( TNodeEventThrottleAssignedToTrain.Create(Node^.Info, TNodeTask( Node^.UserData).LinkedObj, Node^.TrainData.LinkedNode));  // Send this before Querying Functions and Speed
                            TNodeTask( Node^.UserData).iSubStateMachine := STATE_CAB_SELECT_LOCO_SEND_TRACTION_QUERY_SPEED
                          end else
                            TNodeTask( Node^.UserData).iSubStateMachine := STATE_CAB_SELECT_LOCO_GENERIC_TIMEOUT_PROXY_UNLOCK   // Can't reserve now go back to normal polling
                        end;
                  end;  // case
                end
          end; // case
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
                  case Node^.iUserStateMachine of
                    STATE_THROTTLE_ALLOCATE_TRAIN_BY_ADDRESS :
                        begin
                          if MultiFrameBuffer^.DataArray[2] = TRACTION_MANAGE_RESERVE_REPLY_OK then
                            TNodeTask( Node^.UserData).iSubStateMachine := STATE_CAB_SELECT_LOCO_SEND_TRACTION_ASSIGN_CONTROLLER
                          else
                            TNodeTask( Node^.UserData).iSubStateMachine := STATE_CAB_SELECT_LOCO_GENERIC_TIMEOUT_PROXY_UNLOCK   // Can't reserve now go back to normal polling
                        end;
                    STATE_THROTTLE_RELEASE_CONTROLLER :
                        begin
                          if MultiFrameBuffer^.DataArray[2] = TRACTION_MANAGE_RESERVE_REPLY_OK then
                            TNodeTask( Node^.UserData).iSubStateMachine := STATE_THROTTLE_RELEASE_CONTROLLER_SEND_TRACTION_RELEASE
                          else
                            TNodeTask( Node^.UserData).iSubStateMachine := STATE_THROTTLE_RELEASE_CONTROLLER_SEND_TRACTION_UNLOCK   // Can't reserve now go back to normal polling
                        end;
                  end; // case
                end;
          end
        end
  end;
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
begin
  case AMessage^.Buffer^.DataArray[0] of
    TRACTION_PROXY_MANAGE :
        begin
          if AMessage^.Buffer^.DataArray[1] = TRACTION_PROXY_MANAGE_RESERVE then
          begin
             if AMessage^.Buffer^.DataArray[2] = 0 then
               TNodeTask( Node^.UserData).iSubStateMachine := STATE_CAB_SELECT_LOCO_SEND_PROXY_ALLOCATE             // Move to next state after reserving
             else
               TNodeTask( Node^.UserData).iSubStateMachine := STATE_CAB_SELECT_LOCO_GENERIC_TIMEOUT_PROXY_UNLOCK   // Can't reserve now go back to normal polling
          end;
          Exit;
        end;
    TRACTION_PROXY_ALLOCATE :
        begin
          MultiFrameBuffer := PMultiFrameBuffer( PByte(AMessage^.Buffer));
          Node^.TrainData.LinkedNode.AliasID := (MultiFrameBuffer^.DataArray[11] shl 8) or (MultiFrameBuffer^.DataArray[12]);
          NMRAnetUtilities_Load48BitNodeIDWithSimpleData(Node^.TrainData.LinkedNode.ID, PSimpleDataArray( PByte( @MultiFrameBuffer^.DataArray[5]))^);
          TNodeTask( Node^.UserData).iSubStateMachine  := STATE_CAB_SELECT_LOCO_SEND_PROXY_MANAGE_UNLOCK;    // Now need to unlock the Proxy
          Exit;
        end;
  end; // case
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
var
  Protocols: TNodeEventSupportsProtocols;
begin
  if Node^.iUserStateMachine = STATE_THROTTLE_PROTOCOL_SUPPORT then
  begin
    Protocols := TNodeEventSupportsProtocols.Create(Node^.Info, TNodeTask( Node^.UserData).LinkedObj);
    if AMessage^.Buffer^.DataBufferSize > 0 then
    begin
      Protocols.SimpleProtocol := AMessage^.Buffer^.DataArray[0] and $80 <> 0;
      Protocols.Datagram := AMessage^.Buffer^.DataArray[0] and $40 <> 0;
      Protocols.Stream := AMessage^.Buffer^.DataArray[0] and $20 <> 0;
      Protocols.MemConfig := AMessage^.Buffer^.DataArray[0] and $10 <> 0;
      Protocols.Reservation := AMessage^.Buffer^.DataArray[0] and $08 <> 0;
      Protocols.EventExchange := AMessage^.Buffer^.DataArray[0] and $04 <> 0;
      Protocols.Identification := AMessage^.Buffer^.DataArray[0] and $02 <> 0;
      Protocols.Teaching := AMessage^.Buffer^.DataArray[0] and $01 <> 0;
    end;
    if AMessage^.Buffer^.DataBufferSize > 1 then
    begin
      Protocols.RemoteButton := AMessage^.Buffer^.DataArray[1] and $80 <> 0;
      Protocols.ACDI := AMessage^.Buffer^.DataArray[1] and $40 <> 0;
      Protocols.Display := AMessage^.Buffer^.DataArray[1] and $20 <> 0;
      Protocols.SNIP := AMessage^.Buffer^.DataArray[1] and $10 <> 0;
      Protocols.CDI := AMessage^.Buffer^.DataArray[1] and $08 <> 0;
      Protocols.Traction := AMessage^.Buffer^.DataArray[1] and $04 <> 0;
      Protocols.FDI := AMessage^.Buffer^.DataArray[1] and $02 <> 0;
      Protocols.TractionProxy := AMessage^.Buffer^.DataArray[1] and $01 <> 0;
    end;
    if AMessage^.Buffer^.DataBufferSize > 2 then
    begin
      Protocols.STNIP := AMessage^.Buffer^.DataArray[2] and $80 <> 0;
    end;
    NodeThread.AddEvent(Protocols);
    TNodeTask( Node^.UserData).iSubStateMachine := STATE_THROTTLE_PROTOCOL_SUPPORT_END;
  end;
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
var
  TaskReadConfigMemory: TNodeTaskReadConfigMemory;
  i, Offset: Integer;
begin
  if TNodeTask( Node^.UserData) is TNodeTaskReadConfigMemory then
  begin
    TNodeTask( Node^.UserData).iSubStateMachine := STATE_THROTTLE_READ_FDI_SEND;
    TaskReadConfigMemory := TNodeTask( Node^.UserData) as TNodeTaskReadConfigMemory;

    // Skip over the header
    if (AMessage^.Buffer^.DataArray[1] and $0F = 0) then
      Offset := 7
    else
      Offset := 6;

    TaskReadConfigMemory.Watchdog := 0;
    for i := Offset to AMessage^.Buffer^.DataBufferSize - 1 do
    begin
      TaskReadConfigMemory.FDI := TaskReadConfigMemory.FDI + Char(AMessage^.Buffer^.DataArray[i]);
      TaskReadConfigMemory.CurrentAddress := TaskReadConfigMemory.CurrentAddress + 1;
      if Char(AMessage^.Buffer^.DataArray[i]) = #0 then
      begin
        TNodeTask( Node^.UserData).iSubStateMachine := STATE_THROTTLE_READ_FDI_END;
        NodeThread.AddEvent( TNodeEventReadFDI.Create(Node^.Info, TaskReadConfigMemory.LinkedObj, TaskReadConfigMemory.FDI));
        Break;
      end;
    end;
  end;
  TaskReadConfigMemory.Watchdog := 0;
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
var
  Event: TNodeEventSimpleTrainNodeInfo;
begin
  Event := TNodeEventSimpleTrainNodeInfo.Create(Node^.Info, TNodeTask( Node^.UserData).LinkedObj);
  Event.Decode(AMessage);
  NodeThread.AddEvent(Event);
  TNodeTask( Node^.UserData).iSubStateMachine := STATE_THROTTLE_FIND_SIMPLE_TRAIN_INFO_DONE;       // Next Please
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

