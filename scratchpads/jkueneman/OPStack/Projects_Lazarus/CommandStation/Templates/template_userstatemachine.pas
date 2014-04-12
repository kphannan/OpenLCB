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

procedure AppCallback_ConsumerIdentified(DestinatinNode: PNMRAnetNode; MTI: Word; EventID: PEventID);
procedure AppCallback_ProducerIdentified(DestinatinNode: PNMRAnetNode; MTI: Word; EventID: PEventID);
procedure AppCallback_VerifiedNodeID(DestinatinNode: PNMRAnetNode; NodeID: PNodeID);
procedure AppCallback_InitializationComplete(DestinatinNode: PNMRAnetNode; NodeID: PNodeID);


implementation

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
begin

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
begin
  // Initialize the example statemachine, evertime the node is reused!
  UserState := STATE_USER_START;

  // Initialize the example data, evertime the node is reused!
  for i := 0 to USER_MAX_NODE_COUNT - 1 do
  begin
    UserDataArray[i].UserData1 := 0;
    UserDataArray[i].UserData2 := 0;
  end;

  // Assign the user data record to the Node for future use
  Node^.UserData := @UserDataArray[Node^.iIndex];
end;

procedure AppCallback_ConsumerIdentified(DestinatinNode: PNMRAnetNode; MTI: Word; EventID: PEventID);
begin

end;

procedure AppCallback_ProducerIdentified(DestinatinNode: PNMRAnetNode; MTI: Word; EventID: PEventID);
begin

end;

procedure AppCallback_VerifiedNodeID(DestinatinNode: PNMRAnetNode; NodeID: PNodeID);
begin

end;

procedure AppCallback_InitializationComplete(DestinatinNode: PNMRAnetNode; NodeID: PNodeID);
begin

end;

end.

