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
 // LCLIntf,
 // LCLType,
  {$ENDIF}
  Float16,
  opstacktypes,
  opstackdefines,
  template_node,
  opstack_api,
  olcb_defines,
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
procedure AppCallback_RemoteButtonReply(Node: PNMRAnetNode; AMessage: POPStackMessage);
{$IFDEF SUPPORT_TRACTION}
procedure AppCallback_TractionProtocol(Node: PNMRAnetNode; AMessage: POPStackMessage; SourceHasLock: Boolean);
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
  SYNC_OBJPTR                    = $0001;

  SYNC_NODE_INFO                = $0010;

  SYNC_STATE_SPEED_DIR          = $0100;
  SYNC_STATE_FUNCTIONS          = $0200;
  SYNC_STATE_ADDRESS            = $0400;
  SYNC_SPEED_STEPS              = $0800;

  SYNC_REPLY_NODE               = $1000;

type

  // Fields that the UI updates
  TTrainRec = record
    ObjPtr: TObject;
  end;

  // Fields that the OPStack Updates
  TNodeRec = record
    Info     : TNodeInfo;
    Index    : Integer;
  end;

  // Fields that either Update
  TTrainState = record
    SpeedDir     : THalfFloat;
    Functions    : DWORD;
    Address      : Word;
    SpeedSteps   : Word;
  end;

  TLinkRec = record
    SyncState   : Word;    // SYNC_xxxxx contants to tell what has changed
    Train       : TTrainRec;
    Node        : TNodeRec;
    TrainState  : TTrainState;
    ReplyNode   : TNodeInfo
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

  GlobalTimer: Word;

procedure ZeroLinkRec(LinkRec: PLinkRec);
begin
  LinkRec^.SyncState := SYNC_NONE;
  LinkRec^.Node.Index := 0;
  LinkRec^.Node.Info.AliasID := 0;
  LinkRec^.Node.Info.ID[0] := 0;
  LinkRec^.Node.Info.ID[1] := 0;
  LinkRec^.TrainState.Address := 0;
  LinkRec^.TrainState.Functions := 0;
  LinkRec^.TrainState.SpeedDir := 0;
  LinkRec^.TrainState.SpeedSteps := 28;
  LinkRec^.Train.ObjPtr := nil;
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
              // Now the Physical node can be a Throttle Node Server
              EnterCriticalSection(OPStackCriticalSection);
              // Only do something if the number of links has changed
              if Sync.DatabaseChanged then
              begin
                for i := 0 to Sync.NextLink - 1 do     // Look only at active Link Objects
                begin
                  if Sync.Link[i].SyncState and SYNC_REPLY_NODE <> 0 then
                  begin
                    if Sync.Link[i].Node.Info.AliasID = 0 then
                    begin // Logging in
                      Sync.Link[i].SyncState := Sync.Link[i].SyncState and not SYNC_REPLY_NODE;
                      TempNode := OPStackNode_Allocate;
                      Sync.Link[i].Node.Index := TempNode^.iIndex;
                      // Now need to wait for it to log in from with in the virtual nodes statemachine, we are done here
                    end else
                    begin // Logging out
                      if Sync.Link[i].Node.Index > 0 then   // Don't deallocate the physical node if we messed up!
                      begin
                        Sync.Link[i].SyncState := Sync.Link[i].SyncState and not SYNC_OBJPTR;
                        OPStackNode_MarkForRelease(@NodePool.Pool[Sync.Link[i].Node.Index]);
                        for j := i to (Sync.NextLink - 1) do
                          Sync.Link[j] := Sync.Link[j+1];
                        Dec(Sync.NextLink);
                        ZeroLinkRec(@Sync.Link[Sync.NextLink]);
                      end;
                    end
                  end;
                end;
                Sync.DatabaseChanged := False;
              end;
              LeaveCriticalSection(OPStackCriticalSection);
            end;
        STATE_USER_1 :
            begin

            end;
        STATE_USER_2 :
            begin

            end
    end;
  end else
  begin
    // Throttle Node (Virtaul Node)
    case Node^.iUserStateMachine of
        STATE_USER_START :
            begin
              EnterCriticalSection(OPStackCriticalSection);
              // Don't do anything until it is initialized
              if Node^.State and NS_PERMITTED <> 0 then
              begin
                Link := FindLinkByPoolIndex(Node^.iIndex);
                Link^.SyncState := SYNC_NODE_INFO;
                Link^.Node.Info := Node^.Info;
                Node^.iUserStateMachine := STATE_USER_1;
              end;
              LeaveCriticalSection(OPStackCriticalSection);
            end;
        STATE_USER_1 :
            begin
              EnterCriticalSection(OPStackCriticalSection);
              Link := FindLinkByNodeAlias(Node);
              if Link <> nil then                     // May not be found if the node is being freed
              begin
                if TrySendTractionProxyAllocateReply(GetPhysicalNode^.Info, Link^.ReplyNode, TRACTION_PROXY_TECH_ID_DCC, Node^.Info, Link^.TrainState.Address) then
                  Node^.iUserStateMachine := STATE_USER_2;
              end;
              LeaveCriticalSection(OPStackCriticalSection);
            end;
        STATE_USER_2 :
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
//  procedure AppCallback_TractionProtocol
//     Parameters: : Node           : Pointer to the node that the traction protocol has been called on
//                   ReplyMessage   : The Reply Message that needs to be allocated, populated and returned so it can be sent
//                   RequestingMessage    : Message that was sent to the node containing the requested information
//     Returns     : None
//                   the internal buffer queue.  It is recommended that the message get handled quickly and released.
//                   The internal system can not process other incoming messages that require a reply until this message
//                   is cleared.  This means that if a reply can not be sent until another message is sent/received this
//                   will block that second message.  If that is required then return True with ReplyMessage = nil to
//                   release the Requesting message then send the reply to this message at a later time
// *****************************************************************************
procedure AppCallback_TractionProtocol(Node: PNMRAnetNode; AMessage: POPStackMessage; SourceHasLock: Boolean);
begin

end;
{$ENDIF}

{$IFDEF SUPPORT_TRACTION_PROXY}
// *****************************************************************************
//  procedure AppCallback_TractionProxyProtocol
//     Parameters: : Node           : Pointer to the node that the traction protocol has been called on
//                   ReplyMessage   : The Reply Message that needs to be allocated, populated and returned so it can be sent
//                   RequestingMessage    : Message that was sent to the node containing the requested information
//     Returns     : True if the RequestingMessage is handled and the ReplyMessage is ready to send
//                   False if the request has not been completed due to no available buffers or waiting on other information
//     Description :
// *****************************************************************************
procedure AppCallback_TractionProxyProtocol(Node: PNMRAnetNode; AMessage: POPStackMessage; SourceHasLock: Boolean);
var
  Link: PLinkRec;
begin
  // Only the top node is the Proxy that replies to this
  if Node = GetPhysicalNode then
    if AMessage^.Buffer^.DataArray[0] = TRACTION_PROXY_ALLOCATE then
    begin
      Sync.Link[Sync.NextLink].SyncState := SYNC_REPLY_NODE;
      Sync.Link[Sync.NextLink].ReplyNode.AliasID := AMessage^.Source.AliasID;
      Sync.Link[Sync.NextLink].ReplyNode.ID[0] := AMessage^.Source.ID[0];
      Sync.Link[Sync.NextLink].ReplyNode.ID[1] := AMessage^.Source.ID[1];
      Sync.Link[Sync.NextLink].TrainState.Address := (AMessage^.Buffer^.DataArray[2] shl 8) or AMessage^.Buffer^.DataArray[3];
      Sync.Link[Sync.NextLink].TrainState.SpeedSteps := AMessage^.Buffer^.DataArray[4];
      Inc(Sync.NextLink);
      Sync.DatabaseChanged := True;
      // Run the reply statemachine on the physical node will create a new Train Node eventually
    end;
end;

// *****************************************************************************
//  procedure AppCallback_TractionProxyProtocolReply
//     Parameters: : Source : Full Node ID (and Alias if on CAN) of the source node for the message
//                   Dest   : Full Node ID (and Alias if on CAN) of the dest node for the message
//                   DataBytes: pointer Raw data bytes, Byte 0 and 1 are the Alias
//     Returns     : None
//     Description :
// *****************************************************************************
procedure AppCallback_TractionProxyProtocolReply(Node: PNMRAnetNode; AMessage: POPStackMessage);
begin

end;

// *****************************************************************************
//  procedure AppCallBack_ProtocolSupportReply
//     Parameters: : Source : Full Node ID (and Alias if on CAN) of the source node for the message
//                   Dest   : Full Node ID (and Alias if on CAN) of the dest node for the message
//                   DataBytes: pointer Raw data bytes, Byte 0 and 1 are the Alias
//     Returns     : None
//     Description :
// *****************************************************************************
procedure AppCallBack_ProtocolSupportReply(Node: PNMRAnetNode; AMessage: POPStackMessage);
begin

end;
{$ENDIF}

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
procedure AppCallback_TractionControlReply(var Source: TNodeInfo; Dest: PNMRAnetNode; DataBytes: PSimpleBuffer);
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
procedure AppCallback_RemoteButtonReply(Node: PNMRAnetNode; AMessage: POPStackMessage);
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
//     Description : This is called directly from the Hardware receive buffer.  Do
//                   not do anything here that stalls the call.  This is called
//                   Asyncronously from the Statemachine loop and the Statemachine loop
//                   is stalled until this returns.  Set a flag and move on is the
//                   best stratagy or store info in a buffer and process in the
//                   main statemachine.
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


