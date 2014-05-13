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
  olcb_transport_layer,
  template_hardware,
  olcb_defines,
 // LCLIntf,
 // LCLType,
  {$ENDIF}
  Float16,
  opstacktypes,
  nmranetdefines,
  opstackdefines,
  template_node,
  opstack_api,
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

{$IFNDEF FPC}
  function OPStackNode_Allocate: PNMRAnetNode; external;
  procedure OPStackNode_MarkForRelease(Node: PNMRAnetNode); external;
  function OPStackNode_Find(AMessage: POPStackMessage; FindBy: Byte): PNMRAnetNode;   external;   // See FIND_BY_xxxx constants
  function OPStackNode_FindByAlias(AliasID: Word): PNMRAnetNode; external;
  function OPStackNode_FindByID(var ID: TNodeID): PNMRAnetNode; external;
  procedure TractionProxyProtocolReply(Node: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage); external;
  procedure TractionProtocolReply(Node: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage); external;
{$ENDIF}

{$IFDEF FPC}
const
  SYNC_NONE                     = $0000;
  SYNC_THROTTLE_OBJPTR          = $0001;

  SYNC_NODE_INFO                = $0010;
  SYNC_CONTROLLER               = $0020;
  SYNC_CLOSING                  = $0040;
  SYNC_CLOSED                   = $0080;

  SYNC_STATE_SPEED_DIR          = $0100;
  SYNC_STATE_FUNCTIONS          = $0200;
  SYNC_STATE_ADDRESS            = $0400;
  SYNC_SPEED_STEPS              = $0800;

type

  // Fields that the UI updates
  TThrottleRec = record
    ObjPtr: TObject;           // pointer to the TForm throttle
  end;

  // Fields that the OPStack Updates
  TNodeRec = record
    Info     : TNodeInfo;
    Index    : Integer;
  end;

  // Fields that either Update
  TThrottleState = record
    SpeedDir     : THalfFloat;
    Functions    : DWORD;
    Address      : Word;
    SpeedSteps   : Word;
  end;

  TLinkRec = record
    SyncState     : Word;    // SYNC_xxxxx contants to tell what has changed
    Throttle      : TThrottleRec;
    Node          : TNodeRec;
    ThrottleState  : TThrottleState;
    AllocatedNode : TNodeInfo;    // Train ID assigned to Throttle
    TrainAllocated: Boolean;
  end;
  PLinkRec = ^TLinkRec;

  TLinkArray = array[0..USER_MAX_NODE_COUNT-2] of TLinkRec;    // Don't need the physical node

  TSync = record
    NextLink: Integer;
    Link: TLinkArray;
    DatabaseChanged: Boolean;
  end;

var
  Template_UserStateMachine_OnTaskDestroy: TOlcbTaskBeforeDestroy;
  OPStackCriticalSection: TRTLCriticalSection;
  Sync: TSync;
{$ENDIF}

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
  UserDataArray: TSampleUserDataArray;

  ProxyNode: TNodeInfo;
  GlobalTimer: Word;


procedure ClearProxyNode;
begin
  ProxyNode.AliasID := 0;
  ProxyNode.ID[0] := 0;
  ProxyNode.ID[1] := 0;
end;

procedure ZeroLinkRec(LinkRec: PLinkRec);
begin
  LinkRec^.SyncState := SYNC_NONE;
  LinkRec^.Node.Index := 0;
  LinkRec^.Node.Info.AliasID := 0;
  LinkRec^.Node.Info.ID[0] := 0;
  LinkRec^.Node.Info.ID[1] := 0;
  LinkRec^.ThrottleState.Address := 0;
  LinkRec^.ThrottleState.Functions := 0;
  LinkRec^.ThrottleState.SpeedDir := 0;
  LinkRec^.ThrottleState.SpeedSteps := 28;
  LinkRec^.Throttle.ObjPtr := nil;
  LinkRec^.TrainAllocated := False;
end;

function FindLinkByNodeAlias(Node: PNMRAnetNode): PLinkRec;
var
  i: Integer;
begin
  // Find the Link for this Node
  Result := nil;
  for i := 0 to Sync.NextLink - 1 do
  begin
    if Sync.Link[i].Node.Info.AliasID = Node^.Info.AliasID then          // WARNING ONLY WORKS WITH CAN OR ETHERNET USING GRID CONNECT
    begin
      Result := @Sync.Link[i];
      Break;
    end;
  end;
end;

function FindLinkByPoolIndex(Index: Integer): PLinkRec;
var
  i: Integer;
begin
  // Find the Link for this Node
  Result := nil;
  for i := 0 to Sync.NextLink - 1 do
  begin
    if Sync.Link[i].Node.Index = Index then
    begin
      Result := @Sync.Link[i];
      Break;
    end;
  end;
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
  ClearProxyNode;
end;

// *****************************************************************************
//  procedure AppCallback_UserStateMachine_Process
//     Parameters: : None
//     Returns     : None
//     Description : Called as often as possible to run the user statemachine
// *****************************************************************************
procedure AppCallback_UserStateMachine_Process(Node: PNMRAnetNode);
var
  TempNode: PNMRAnetNode;
  i, j: Integer;
  Link: PLinkRec;
begin
  if Node = GetPhysicalNode then
  begin
    case Node^.iUserStateMachine of
        STATE_USER_START :
            begin
              // Find the Proxy node (Command Station) on the network before progressing
              if Node^.State and NS_INITIALIZED <> 0 then
               begin
                 GlobalTimer := 0;
                 if TrySendIdentifyProducer(Node^.Info, @EVENT_IS_PROXY) then
                   Node^.iUserStateMachine := STATE_USER_1;
               end;
            end;
        STATE_USER_1 :
            begin
              // Find the Proxy node (Command Station) on the network before progressing
              if (ProxyNode.AliasID > 0) or (ProxyNode.ID[0] > 0) or (ProxyNode.ID[1] > 0) then
                Node^.iUserStateMachine := STATE_USER_2
              else begin
                if GlobalTimer > 10 then
                  Node^.iUserStateMachine := STATE_USER_START            // Try again
              end;
            end;
        STATE_USER_2 :
            begin
              // Now the Physical node can be a Throttle Node Server
              EnterCriticalSection(OPStackCriticalSection);
              // Only do something if the number of links has changed
              if Sync.DatabaseChanged then
              begin
                for i := 0 to Sync.NextLink - 1 do     // Look only at active Link Objects
                begin
                  if Sync.Link[i].SyncState <> SYNC_NONE then
                  begin
                    if Sync.Link[i].SyncState and SYNC_THROTTLE_OBJPTR <> 0 then
                    begin
                      if Sync.Link[i].Node.Info.AliasID = 0 then
                      begin // Logging in
                        Sync.Link[i].SyncState := Sync.Link[i].SyncState and not SYNC_THROTTLE_OBJPTR;
                        TempNode := OPStackNode_Allocate;
                        Sync.Link[i].Node.Index := TempNode^.iIndex;
                        // Now need to wait for it to log in
                      end else
                      begin // Logging out
                        if Sync.Link[i].Node.Index > 0 then   // Don't deallocate the physical node if we messed up!
                        begin
                          Sync.Link[i].SyncState := Sync.Link[i].SyncState and not SYNC_THROTTLE_OBJPTR;
                          OPStackNode_MarkForRelease(@NodePool.Pool[Sync.Link[i].Node.Index]);
                          for j := i to (Sync.NextLink - 1) do
                            Sync.Link[j] := Sync.Link[j+1];
                          Dec(Sync.NextLink);
                          ZeroLinkRec(@Sync.Link[Sync.NextLink]);
                        end;
                      end
                    end;
                  end;
                end;
                Sync.DatabaseChanged := False;
              end;
              LeaveCriticalSection(OPStackCriticalSection);
            end
    end;
  end else
  begin
    // Throttle Node (Virtual Node)
    case Node^.iUserStateMachine of
        STATE_USER_START :
            begin
              EnterCriticalSection(OPStackCriticalSection);
              // Don't do anything until it is initialized
              if Node^.State and NS_PERMITTED <> 0 then
              begin
                Link := FindLinkByPoolIndex(Node^.iIndex);     // iIndex 0 is physcial node
                Link^.SyncState := SYNC_NODE_INFO;
                Link^.Node.Info := Node^.Info;
                Node^.iUserStateMachine := STATE_USER_1;
              end;
              LeaveCriticalSection(OPStackCriticalSection);
              Exit;
            end;
        STATE_USER_1 :    // Send message to Reserve the PROXY
            begin
              EnterCriticalSection(OPStackCriticalSection);
              Link := FindLinkByNodeAlias(Node);
              if Link <> nil then                     // May not be found if the node is being freed
              begin
                if Link^.SyncState <> SYNC_NONE then
                begin
                  if Link^.SyncState and (SYNC_STATE_ADDRESS or SYNC_SPEED_STEPS) = (SYNC_STATE_ADDRESS or SYNC_SPEED_STEPS) then
                  begin
                    // This means the throttle want to create a new train node
                    if TrySendTractionProxyManage(Node^.Info, ProxyNode, True) then
                    begin
                      Link^.SyncState := Link^.SyncState and not (SYNC_STATE_ADDRESS or SYNC_SPEED_STEPS);
                      Node^.iUserStateMachine := STATE_USER_10;                 // Wait for the Reserve callback
                    end;
                  end;
                end;
                LeaveCriticalSection(OPStackCriticalSection);
              end;
              Exit;
            end;
        STATE_USER_2 :  // Proxy is Reserved, allocate the Train Node
            begin
              EnterCriticalSection(OPStackCriticalSection);
              Link := FindLinkByNodeAlias(Node);
              if Link <> nil then
              begin
                if TrySendTractionProxyAllocate(Node^.Info, ProxyNode, TRACTION_PROXY_TECH_ID_DCC, Link^.ThrottleState.Address, Link^.ThrottleState.SpeedSteps, 0) then
                  Node^.iUserStateMachine := STATE_USER_10;  // Wait for the Allocate Callback
              end;
              LeaveCriticalSection(OPStackCriticalSection);
              Exit;
            end;
        STATE_USER_3  :     // Train has been created, Lock the Train
            begin
              EnterCriticalSection(OPStackCriticalSection);
              Link := FindLinkByNodeAlias(Node);
              if Link <> nil then
              begin
                if TrySendTractionManage(Node^.Info, Link^.AllocatedNode, True) then
                  Node^.iUserStateMachine := STATE_USER_10;  // Wait for the Throttle allocate callback
              end;
              LeaveCriticalSection(OPStackCriticalSection);
              Exit;
            end;
        STATE_USER_4  :   //  Assign the Throttle to the Train
            begin
              EnterCriticalSection(OPStackCriticalSection);
              Link := FindLinkByNodeAlias(Node);
              if Link <> nil then
              begin
                if TrySendTractionControllerConfig(Node^.Info, Link^.AllocatedNode, Node^.Info, True) then
                  Node^.iUserStateMachine := STATE_USER_10;  // Wait for the Throttle allocate callback
              end;
              LeaveCriticalSection(OPStackCriticalSection);
              Exit;
            end;
        STATE_USER_5  :   // Throttle assigned, unlock it
            begin
              EnterCriticalSection(OPStackCriticalSection);
              Link := FindLinkByNodeAlias(Node);
              if Link <> nil then
              begin
                if TrySendTractionManage(Node^.Info, Link^.AllocatedNode, False) then
                  Node^.iUserStateMachine := STATE_USER_6;  // No callback, just start looping
              end;
              LeaveCriticalSection(OPStackCriticalSection);
              Exit;
            end;
        STATE_USER_6  :
            begin
              // Running with Train
              EnterCriticalSection(OPStackCriticalSection);
              Link := FindLinkByNodeAlias(Node);
              if Link <> nil then
              begin
                if Link^.SyncState and SYNC_CLOSING <> 0 then
                begin
                  if TrySendTractionManage(Node^.Info, Link^.AllocatedNode, True) then
                    Node^.iUserStateMachine := STATE_USER_10;  // Wait for the Throttle allocate callback
                end;
              end;
              LeaveCriticalSection(OPStackCriticalSection);
              Exit;
            end;
        STATE_USER_7  :
            begin
              EnterCriticalSection(OPStackCriticalSection);
              Link := FindLinkByNodeAlias(Node);
              if Link <> nil then
              begin
                if TrySendTractionControllerConfig(Node^.Info, Link^.AllocatedNode, Node^.Info, False) then
                begin
                  Link^.AllocatedNode.AliasID := 0;
                  Link^.AllocatedNode.ID[0] := 0;
                  Link^.AllocatedNode.ID[1] := 0;
                  Link^.TrainAllocated := False;
                  Node^.iUserStateMachine := STATE_USER_8;  // There is no reply for deall
                end
              end;
              LeaveCriticalSection(OPStackCriticalSection);
              Exit;
            end;
        STATE_USER_8  :
            begin
              EnterCriticalSection(OPStackCriticalSection);
              Link := FindLinkByNodeAlias(Node);
              if Link <> nil then
              begin
                if TrySendTractionManage(Node^.Info, Link^.AllocatedNode, False) then
                begin
                  Link^.SyncState := Link^.SyncState and not SYNC_CLOSING or SYNC_CLOSED;
                  Node^.iUserStateMachine := STATE_USER_9;  // No callback, just start looping
                end
              end;
              LeaveCriticalSection(OPStackCriticalSection);
              Exit;
            end;
        STATE_USER_9   :
            begin
              Exit;
            end;
        STATE_USER_10  :
            begin
                // Wait for a callback to be called, this can be for many things
              Exit;
            end
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
var
  NodeData: PSampleUserNodeData;
begin
  // Assign the user data record to the Node for future use
  Node^.UserData := @UserDataArray[Node^.iIndex];
  Node^.iUserStateMachine := STATE_USER_START;

  // Initialize the example data, evertime the node is reused!
  NodeData := ExtractUserData(Node);
  NodeData^.UserData1 := 0;
  NodeData^.UserData2 := 0;
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
 Link: PLinkRec;
 MultiFrameBuffer: PMultiFrameBuffer;
begin
  EnterCriticalsection(OPStackCriticalSection);
  Link := FindLinkByNodeAlias(Node);
  if Link <> nil then
  begin
    MultiFrameBuffer := PMultiFrameBuffer( PByte( AMessage^.Buffer));
    case MultiFrameBuffer^.DataArray[0] of
      TRACTION_CONTROLLER_CONFIG :
          begin
            case MultiFrameBuffer^.DataArray[1] of
              TRACTION_CONTROLLER_CONFIG_NOTIFY :
                  begin
                    Link^.TrainAllocated := False;
                    Link^.SyncState := Link^.SyncState or SYNC_CONTROLLER;
                    Link^.AllocatedNode.AliasID := 0;
                    Link^.AllocatedNode.ID[0] := 0;
                    Link^.AllocatedNode.ID[1] := 0;
                    Node^.iUserStateMachine := STATE_USER_1;   // Reset statemachine to log in
                  end
            end;
          end;
    end;
  end;
  LeaveCriticalsection(OPStackCriticalSection);
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
 Link: PLinkRec;
 MultiFrameBuffer: PMultiFrameBuffer;
begin
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
  LeaveCriticalsection(OPStackCriticalSection);
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
var
  Link: PLinkRec;
  MultiFrameBuffer: PMultiFrameBuffer;
begin
  EnterCriticalsection(OPStackCriticalSection);
  Link := FindLinkByNodeAlias(Node);
  if Link <> nil then
  begin
    case AMessage^.Buffer^.DataArray[0] of
      TRACTION_PROXY_MANAGE :
          begin
            if AMessage^.Buffer^.DataArray[1] = TRACTION_PROXY_MANAGE_RESERVE then
            begin
               if AMessage^.Buffer^.DataArray[2] = 0 then
                 Node^.iUserStateMachine := STATE_USER_2         // Move to next state after reserving
               else
                 Node^.iUserStateMachine := STATE_USER_1         // Can't reserve now go back to normal polling
            end;
          end;
      TRACTION_PROXY_ALLOCATE :
          begin
            MultiFrameBuffer := PMultiFrameBuffer( PByte(AMessage^.Buffer));
            Link^.AllocatedNode.AliasID := (MultiFrameBuffer^.DataArray[11] shl 8) or (MultiFrameBuffer^.DataArray[12]);
            NMRAnetUtilities_Load48BitNodeIDWithSimpleData(Link^.AllocatedNode.ID, PSimpleDataArray( PByte( @MultiFrameBuffer^.DataArray[5]))^);
            Node^.iUserStateMachine := STATE_USER_3;    // Now need to allocate the Throttle to the Train Node
          end
    end; // case
  end;
  LeaveCriticalsection(OPStackCriticalSection);
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

{$IFDEF FPC}
var
  i: Integer;

initialization
  Template_UserStateMachine_OnTaskDestroy := nil;
  InitCriticalSection(OPStackCriticalSection);


  EnterCriticalSection(OPStackCriticalSection);
  Sync.NextLink := 0;
  Sync.DatabaseChanged := False;
  for i := 0 to USER_MAX_NODE_COUNT - 2 do
    ZeroLinkRec(@Sync.Link[i]);
  LeaveCriticalsection(OPStackCriticalSection);


Finalization
  DoneCriticalsection( OPStackCriticalSection);

{$ENDIF}

end.


