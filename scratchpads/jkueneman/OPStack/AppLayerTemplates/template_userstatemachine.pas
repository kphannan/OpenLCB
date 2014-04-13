unit template_userstatemachine;

{$mode objfpc}{$H+}

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  {$IFDEF FPC}
  Classes,
  SysUtils,
  FileUtil,
  {$ENDIF}
  opstacktypes,
  opstackdefines,
  template_node;

procedure UserStateMachine_Initialize;
procedure AppCallback_UserStateMachine_Process;
procedure AppCallback_NodeInitialize(Node: PNMRAnetNode);

// These message are called from the mainstatemachine loop.  They have been stored in
// internal storage buffers.  See the notes to understand the implications of this and how to use them correctly
{$IFDEF SUPPORT_TRACTION}
function AppCallback_TractionProtocol(Node: PNMRAnetNode; var ReplyMessage, RequestingMessage: POPStackMessage): Boolean;
{$ENDIF}
{$IFDEF SUPPORT_TRACTION_PROXY}
function AppCallback_TractionProxyProtocol(Node: PNMRAnetNode; var ReplyMessage, RequestingMessage: POPStackMessage): Boolean;
{$ENDIF}

// These messages are called directly from the hardware receive buffer.  See the notes to understand the
// implications of this and how to use them correctly
procedure AppCallback_ConsumerIdentified(var Source: TNodeInfo; var Dest: TNodeInfo; MTI: Word; EventID: PEventID);
procedure AppCallback_ProducerIdentified(var Source: TNodeInfo; var Dest: TNodeInfo; MTI: Word; EventID: PEventID);
procedure AppCallback_VerifiedNodeID(var Source: TNodeInfo; NodeID: PNodeID);
procedure AppCallback_InitializationComplete(var Source: TNodeInfo; NodeID: PNodeID);

{$IFNDEF FPC}
  procedure TractionProxyProtocolReply(Node: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage); external;
  procedure TractionProtocolReply(Node: PNMRAnetNode; var MessageToSend, NextMessage: POPStackMessage); external;
{$ENDIF}

implementation

{$IFDEF FPC}
uses
  opstackcore_traction,
  opstackcore_traction_proxy;
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
end;

// *****************************************************************************
//  procedure AppCallback_UserStateMachine_Process
//     Parameters: : None
//     Returns     : None
//     Description : Called as often as possible to run the user statemachine
// *****************************************************************************
procedure AppCallback_UserStateMachine_Process;
begin
  case UserState of
    STATE_USER_1  :
        begin

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
  i: Integer;
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


