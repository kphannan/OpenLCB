unit opstacknode;

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  template_hardware,
  template_node,
  {$IFDEF SUPPORT_VIRTUAL_NODES}
  template_vnode,
  {$ENDIF}
  nmranetutilities,
  opstackdefines,
  template_userstatemachine;

{$I NodeID.inc}

const
  FIND_BY_SOURCE = 0;
  FIND_BY_DEST = 1;

procedure OPStackNode_Initialize;
function OPStackNode_Allocate: PNMRAnetNode;
procedure OPStackNode_MarkForRelease(Node: PNMRAnetNode);
function OPStackNode_Find(AMessage: POPStackMessage; FindBy: Byte): PNMRAnetNode;    // See FIND_BY_xxxx constants
function OPStackNode_FindByAlias(AliasID: Word): PNMRAnetNode;
function OPStackNode_FindByID(var ID: TNodeID): PNMRAnetNode;
{$IFDEF SUPPORT_VIRTUAL_NODES}
function OPStackNode_FindFirstVirtualNode: PNMRAnetNode;
function OPStackNode_FindLastVirtualNode: PNMRAnetNode;
{$ENDIF}
function OPStackNode_NextNode: PNMRAnetNode;
procedure OPStackNode_ZeroizeNode(Node: PNMRAnetNode);

procedure OPStackNode_SetFlag(Node: PNMRAnetNode; Flag: Byte);
procedure OPStackNode_SetFlags(Flags: Word);
procedure OPStackNode_ClearFlag(Node: PNMRAnetNode; Flag: Byte);
procedure OPStackNode_ClearFlags(Node: PNMRAnetNode);
function OPStackNode_TestFlags(Node: PNMRAnetNode; Flag: Word; DoClear: Boolean): Boolean;

procedure OPStackNode_SetState(Node: PNMRAnetNode; NodeState: Byte);
procedure OPStackNode_ClearState(Node: PNMRAnetNode; NodeState: Byte);
function OPStackNode_TestState(Node: PNMRAnetNode; NodeState: Byte): Boolean;

procedure OPStackNode_SetEventConsumedState(Node: PNMRAnetNode; EventState: Byte);                            // EVENT_STATE_CLEAR
procedure OPStackNode_SetEventProducedState(Node: PNMRAnetNode; EventState: Byte);                            //  EVENT_STATE_VALID
procedure OPStackNode_SetEventState(var Events: TNodeEventStateArray; EventIndex: Integer; EventState: Byte);  //  EVENT_STATE_INVALID
function OPStackNode_GetEventState(var Events: TNodeEventStateArray; EventIndex: Integer): Byte;               //  EVENT_STATE_UNKNOWN

procedure OPStackNode_SetEventFlags(var Events: TNodeEventArray);
procedure OPStackNode_SetEventFlag(EventIndex: Integer; Clear: Boolean; var Events: TNodeEventArray);
procedure OPStackNode_ClearEventFlags(var Events: TNodeEventArray);
function OPStackNode_NextEventFlag(var Events: TNodeEventArray): Integer;
function OPStackNode_IsAnyEventSet(var Events: TNodeEventArray): Boolean;

procedure OPStackNode_SetPCER_Flags(Node: PNMRAnetNode);
procedure OPStackNode_SetPCER_Flag(Node: PNMRAnetNode; EventIndex: Integer; Clear: Boolean);
procedure OPStackNode_ClearPCER_Flags(Node: PNMRAnetNode);
function OPStackNode_NextPCER_Flag(Node: PNMRAnetNode): Integer;
function OPStackNode_IsAnyPCER_Set(Node: PNMRAnetNode): Boolean;

procedure OPStackNode_IncomingMessageLink(Node: PNMRAnetNode; AMessage: POPStackMessage);
procedure OPStackNode_IncomingMessageUnLink(Node: PNMRAnetNode; AMessage: POPStackMessage);
function OPStackNode_NextIncomingMessage(Node: PNMRAnetNode): POPStackMessage;

procedure OPStackNode_StateMachineMessageLink(Node: PNMRAnetNode; AMessage: POPStackMessage);
procedure OPStackNode_StateMachineMessageUnLink(Node: PNMRAnetNode; AMessage: POPStackMessage);
function OPStackNode_NextStateMachineMessage(Node: PNMRAnetNode): POPStackMessage;


function OPStackNode_Equal(Message1, Message2: POPStackMessage): Boolean;

{$IFNDEF FPC}
procedure OPStackNode_PrintPool;
{$ENDIF}

var
  NodePool: TNodePool;

implementation

{$IFNDEF FPC}
procedure OPStackNode_PrintPool;
var
  i: Integer;
begin
  UART1_Write_Text('NodePool Info' + LF);
  WordToStr(USER_MAX_NODE_COUNT, s1);
  UART1_Write_Text('Max Nodes Available: ' + s1 + LF);
  IntToStr(NodePool.iActiveNode, s1);
  UART1_Write_Text('ActiveNode: ' + s1 + LF);
  IntToStr(NodePool.AllocatedCount, s1);
  UART1_Write_Text('AllocatedCount: ' + s1 + LF);
  
  for i := 0 to USER_MAX_NODE_COUNT - 1 do
  begin
    IntToStr(i, s1);
    UART1_Write_Text('Node: ' + s1 + LF);
    ByteToStr(NodePool.Pool[i].iIndex, s1);
    UART1_Write_Text('  iIndex: ' + s1 + LF);
    ByteToHex(NodePool.Pool[i].State, s1);
    UART1_Write_Text('  State: ' + s1 + LF);
    WordToHex(NodePool.Pool[i].Flags, s1);
    UART1_Write_Text('  Flags: ' + s1 + LF);
    WordToStr(NodePool.Pool[i].IncomingMessages, s1);
    UART1_Write_Text('  IncomingMessages: ' + s1 + LF);
    WordToStr(NodePool.Pool[i].StateMachineMessages, s1);
    UART1_Write_Text('  StateMachineMessages: ' + s1 + LF);
    ByteToStr(NodePool.Pool[i].iStateMachine, s1);
    UART1_Write_Text('  iStateMachine: ' + s1 + LF);
  end;
  
  for i := 0 to USER_MAX_NODE_COUNT - 1 do
  begin
    WordToHex(NodePool.AllocatedList[i], s1);
    UART1_Write_Text('AllocatedList: 0x' + s1 + LF);
  end;
end;
{$ENDIF}

// *****************************************************************************
//  procedure FindFreeNodeToAllocate;
//    Parameters:
//    Result:
//    Description:  The new Node will create its alias and register to the CAN bus
//                  automatically as its statemachine begins to run
// *****************************************************************************
function FindFreeNodeToAllocate: PNMRAnetNode;
var
  i: Integer;
  Node: PNMRAnetNode;
begin
  Result := nil;
  for i := 0 to USER_MAX_NODE_COUNT - 1 do        // 0 is the Physical Node
  begin
    Node := @NodePool.Pool[i];
    if Node^.State and NS_ALLOCATED = 0 then
    begin
      Result := Node;
      Exit
    end
  end;
end;

// *****************************************************************************
//  procedure OPStackNode_Initialize;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackNode_Initialize;
var
  i: Integer;
  Node: PNMRAnetNode;
begin
  NodePool.AllocatedCount := 0;                                                    // Number of Nodes Allocated
  NodePool.iActiveNode := 0;
  
  for i := 0 to USER_MAX_NODE_COUNT - 1 do
  begin
    Node := @NodePool.Pool[i];
    Node^.iIndex := i;
    OPStackNode_ZeroizeNode(Node);
    Node^.IncomingMessages := nil;
    Node^.StateMachineMessages := nil;
    Node^.Info.ID[0] := NodeID_LO + i;         // Picked up from the NodeID.inc file
    Node^.Info.ID[1] := NodeID_HI;
    Node^.Login.Seed := Node^.Info.ID;
    NodePool.AllocatedList[i] := nil;
  end;
end;

// *****************************************************************************
//  procedure OPStackNode_Allocate;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackNode_Allocate: PNMRAnetNode;
begin
  Result := PNMRAnetNode( nil);
  if NodePool.AllocatedCount < USER_MAX_NODE_COUNT then
  begin
    Result := FindFreeNodeToAllocate;
    if Result <> nil then
    begin
      OPStackNode_ZeroizeNode(Result);                                          // The NodeID was already created in the initialization but we need to zeroize it
      OPStackNode_SetState(Result, NS_ALLOCATED);                               // Mark as Allocated
      {$IFDEF SUPPORT_VIRTUAL_NODES}
      if Result^.iIndex > 0 then
        OPStackNode_SetState(Result, NS_VIRTUAL);                               // Mark as Virtual
      {$ENDIF}
      Result^.Info.AliasID := NMRAnetUtilities_CreateAliasID(Result^.Login.Seed, False); // Pregenerate it so it can be sorted
      NodePool.AllocatedList[NodePool.AllocatedCount] := Result;                // Add it to the end if the Allocated List
      Inc(NodePool.AllocatedCount);                                             // One more Allocated
    end;
  end;
end;

// *****************************************************************************
//  procedure OPStackNode_MarkForRelease;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackNode_MarkForRelease(Node: PNMRAnetNode);
begin
  if Node <> nil then
  begin
    {$IFDEF SUPPORT_VIRTUAL_NODES}
    if Node^.State and NS_VIRTUAL = NS_VIRTUAL then                             // Only release Virtual Nodes
      Node^.State := Node^.State or NS_RELEASING;                               // Tag it to send and AMR
    {$ENDIF}
  end
end;

// *****************************************************************************
//  procedure OPStackNode_FindByAlias;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackNode_FindByAlias(AliasID: Word): PNMRAnetNode;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to NodePool.AllocatedCount - 1 do
  begin
    if NodePool.AllocatedList[i]^.Info.AliasID = AliasID then
      if NodePool.AllocatedList[i]^.State and NS_RELEASING = 0 then
      begin
        Result := NodePool.AllocatedList[i];
        Exit
      end
  end
end;

// *****************************************************************************
//  procedure OPStackNode_FindByID;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackNode_FindByID(var ID: TNodeID): PNMRAnetNode;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to NodePool.AllocatedCount - 1 do
  begin
    if NodePool.AllocatedList[i]^.Info.ID[0] = ID[0] then
      if NodePool.AllocatedList[i]^.Info.ID[1] = ID[1] then
          if NodePool.AllocatedList[i]^.State and NS_RELEASING = 0 then
          begin
            Result := NodePool.AllocatedList[i];
            Exit
          end;
  end;
end;

{$IFDEF SUPPORT_VIRTUAL_NODES}
// *****************************************************************************
//  procedure OPStackNode_FindFirstVirtualNode;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackNode_FindFirstVirtualNode: PNMRAnetNode;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  while (i < NodePool.AllocatedCount) do
  begin
    if NodePool.AllocatedList[i]^.State and NS_VIRTUAL = NS_VIRTUAL then
    begin
      Result := NodePool.AllocatedList[i];
      Exit;
    end;
    Inc(i);
  end;
end;

// *****************************************************************************
//  procedure OPStackNode_FindLastVirtualNode;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackNode_FindLastVirtualNode: PNMRAnetNode;
var
  i: Integer;
begin
  Result := nil;
  i := NodePool.AllocatedCount - 1;
  while (i > -1) do
  begin
    if NodePool.AllocatedList[i]^.State and NS_VIRTUAL = NS_VIRTUAL then
    begin
      Result := NodePool.AllocatedList[i];
      Exit;
    end;
    Dec(i);
  end;
end;
{$ENDIF}

// *****************************************************************************
//  procedure OPStackNode_NextNode;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackNode_NextNode: PNMRAnetNode;
begin
  Result := PNMRAnetNode( nil);
  if NodePool.AllocatedCount > 0 then
  begin
    if NodePool.iActiveNode > NodePool.AllocatedCount - 1 then
      NodePool.iActiveNode := 0;
    Result := NodePool.AllocatedList[NodePool.iActiveNode];
    Inc(NodePool.iActiveNode);
  end;
end;

// *****************************************************************************
//  procedure OPStackNode_ZeroizeNode;
//    Parameters:
//    Result:
//    Description: Does not touch the NodeID or Seed, they are set at startup and
//                 stay the same throughout the operation of the node
// *****************************************************************************
procedure OPStackNode_ZeroizeNode(Node: PNMRAnetNode);
var
  j: Integer;
begin
  Node^.State := NS_EMPTY;
  Node^.Login.iCID := 0;
  Node^.Login.TimeCounter := 0;
  Node^.Info.AliasID := 0;
  Node^.iStateMachine := 0;
  for j := 0 to USER_MAX_EVENTS_BYTES - 1 do
    Node^.Events.ConsumedState[j] := $FF;          // Default is EVENT_STATE_UNKNOWN
  for j := 0 to USER_MAX_EVENTS_BYTES - 1 do       // Default is EVENT_STATE_UNKNOWN
    Node^.Events.ProducedState[j] := $FF;
  for j := 0 to USER_MAX_PCER_BYTES - 1 do
    Node^.Events.PCER[j] := 0;
  for j := 0 to USER_MAX_PCER_BYTES - 1 do
    Node^.Events.Consumed[j] := 0;
  for j := 0 to USER_MAX_PCER_BYTES - 1 do
    Node^.Events.Produced[j] := 0;
  Node^.IncomingMessages := nil;
  Node^.StateMachineMessages := nil;
  Node^.Flags := 0;
  Node^.UserData := nil;
  Node^.iUserStateMachine := 0;
  {$IFDEF SUPPORT_TRACTION}
  Node^.TrainData.State := 0;
  Node^.TrainData.Address := 0;
  Node^.TrainData.Functions := 0;
  Node^.TrainData.SpeedDir := 0;
  Node^.TrainData.SpeedSteps := DEFAULT_SPEED_STEPS;
  Node^.TrainData.Lock.AliasID := 0;
  Node^.TrainData.Lock.ID[0] := 0;
  Node^.TrainData.Lock.ID[1] := 0;
  {$ENDIF}
  {$IFDEF SUPPORT_TRACTION_PROXY}
  Node^.ProxyData.Lock.AliasID := 0;
  Node^.ProxyData.Lock.ID[0] := 0;
  Node^.ProxyData.Lock.ID[1] := 0;
  {$ENDIF}
  AppCallback_NodeInitialize(Node);                                             // Allow the App layer to initialize it
end;

// *****************************************************************************
//  procedure OPStackNode_SetFlag;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackNode_SetFlag(Node: PNMRAnetNode; Flag: Byte);
begin
  Node^.Flags := Node^.Flags or Flag;
end;

// *****************************************************************************
//  procedure OPStackNode_SetFlags;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackNode_SetFlags(Flags: Word);
var
  i: Integer;
  Node: PNMRAnetNode;
begin
  for i := 0 to NodePool.AllocatedCount - 1 do
  begin
    Node := NodePool.AllocatedList[i];
    if Node^.State and NS_PERMITTED = NS_PERMITTED then
      if Node^.State and NS_RELEASING = 0 then
        Node^.Flags := Node^.Flags or Flags
    else
      Node^.Flags := 0;
  end;
end;

// *****************************************************************************
//  procedure OPStackNode_ClearFlag;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackNode_ClearFlag(Node: PNMRAnetNode; Flag: Byte);
begin
  Node^.Flags := Node^.Flags and not Flag;
end;

// *****************************************************************************
//  procedure OPStackNode_ClearFlags;
//  Parameters:
//  Result:
//  Description:
// *****************************************************************************
procedure OPStackNode_ClearFlags(Node: PNMRAnetNode);
begin
  Node^.Flags := 0;
end;

// *****************************************************************************
//  function OPStackNode_TestFlags
//     Parameters:
//     Returns:
//     Description:
// *****************************************************************************
function OPStackNode_TestFlags(Node: PNMRAnetNode; Flag: Word; DoClear: Boolean): Boolean;
begin
  Result := Node^.Flags and Flag = Flag;
  if DoClear then
    OPStackNode_ClearFlag(Node, Flag);
end;

// *****************************************************************************
//  procedure OPStackNode_SetState
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackNode_SetState(Node: PNMRAnetNode; NodeState: Byte);
begin
  Node^.State := Node^.State or NodeState;
end;

// *****************************************************************************
//  procedure OPStackNode_ClearState
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackNode_ClearState(Node: PNMRAnetNode; NodeState: Byte);
begin
  Node^.State := Node^.State and not NodeState;
end;

// *****************************************************************************
//  procedure OPStackNode_TestState
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackNode_TestState(Node: PNMRAnetNode; NodeState: Byte): Boolean;
begin
  Result := Node^.State and NodeState = NodeState;
end;

// *****************************************************************************
//  procedure OPStackNode_SetEventConsumedState;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackNode_SetEventConsumedState(Node: PNMRAnetNode; EventState: Byte);
var
  i: Integer;
begin
  {$IFDEF SUPPORT_VIRTUAL_NODES}
  if OPStackNode_TestState(Node, NS_VIRTUAL) then
  begin
    for i := 0 to (USER_MAX_VNODE_SUPPORTED_EVENTS_CONSUMED + USER_MAX_VNODE_SUPPORTED_DYNAMIC_EVENTS_CONSUMED) - 1 do
      OPStackNode_SetEventState(Node^.Events.ConsumedState, i, EventState);
  end else
  {$ENDIF}
  begin
    for i := 0 to (USER_MAX_SUPPORTED_EVENTS_CONSUMED + USER_MAX_SUPPORTED_DYNAMIC_EVENTS_CONSUMED) - 1 do
      OPStackNode_SetEventState(Node^.Events.ConsumedState, i, EventState);
  end
end;

// *****************************************************************************
//  procedure OPStackNode_SetEventProducedState;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackNode_SetEventProducedState(Node: PNMRAnetNode; EventState: Byte);
var
  i: Integer;
begin
  {$IFDEF SUPPORT_VIRTUAL_NODES}
  if OPStackNode_TestState(Node, NS_VIRTUAL) then
  begin
    for i := 0 to (USER_MAX_VNODE_SUPPORTED_EVENTS_PRODUCED + USER_MAX_VNODE_SUPPORTED_DYNAMIC_EVENTS_CONSUMED) - 1 do
      OPStackNode_SetEventState(Node^.Events.ProducedState, i, EventState);
  end else
  {$ENDIF}
  begin
    for i := 0 to (USER_MAX_SUPPORTED_EVENTS_CONSUMED + USER_MAX_SUPPORTED_DYNAMIC_EVENTS_CONSUMED) - 1 do
      OPStackNode_SetEventState(Node^.Events.ProducedState, i, EventState);
  end
end;

// *****************************************************************************
//  procedure OPStackNode_SetEventState;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackNode_SetEventState(var Events: TNodeEventStateArray; EventIndex: Integer; EventState: Byte);
var
  ByteOffset, {EventOffset} NormalizedIndex: Integer;
  Mask: Byte;
begin
  ByteOffset := EventIndex div 4;    // There are 4 Events supported in each Byte
  NormalizedIndex := EventIndex - (ByteOffset*4);
  Mask := %00000011;
  Mask := Mask shl (NormalizedIndex * 2);  // 2 bits per event
  Mask := not Mask;
  Events[ByteOffset] := Events[ByteOffset] and Mask;
  Mask := EventState shl (NormalizedIndex * 2);
  Events[ByteOffset] := Events[ByteOffset] or Mask;
end;

// *****************************************************************************
//  function OPStackNode_GetEventState;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackNode_GetEventState(var Events: TNodeEventStateArray; EventIndex: Integer): Byte;
var
  ByteOffset, NormalizedIndex: Integer;
begin
  ByteOffset := EventIndex div 4;    // There are 4 Events supported in each Byte
  NormalizedIndex := EventIndex - (ByteOffset*4);
  Result := (Events[ByteOffset] shr (NormalizedIndex * 2)) and $03; // 2 bits per event
end;

procedure OPStackNode_SetEventFlags(var Events: TNodeEventArray);
var
  i: Integer;
begin
  for i := 0 to USER_MAX_PCER_BYTES - 1 do
    Events[i] := $FF
end;

procedure OPStackNode_SetEventFlag(EventIndex: Integer; Clear: Boolean; var Events: TNodeEventArray);
var
  ByteOffset: Integer;
  Mask: Byte;
begin
  ByteOffset := EventIndex div 8;    // There are 8 PCERs supported in each Byte
  Mask := $01;
  Mask := Mask shl (EventIndex mod 8);
  if Clear then
  begin
    Mask := not Mask;
    Events[ByteOffset] := Events[ByteOffset] and Mask;
  end else
    Events[ByteOffset] := Events[ByteOffset] or Mask;
end;

procedure OPStackNode_ClearEventFlags(var Events: TNodeEventArray);
var
  i: Integer;
begin
  for i := 0 to USER_MAX_PCER_BYTES - 1 do
    Events[i] := $00
end;

function OPStackNode_NextEventFlag(var Events: TNodeEventArray): Integer;
var
  i, j: Integer;
  Temp: Byte;
begin
  Result := -1;
  for i := 0 to USER_MAX_PCER_BYTES - 1 do
  begin
    if Events[i] <> 0 then
    begin
      Temp := Events[i];
      for j := 0 to 7 do                                                        // Find the first non zero state in the byte
      begin
        if Temp and $01 <> 0 then
        begin
          Result := (i*8) + j;
          OPStackNode_SetEventFlag(Result, True, Events);                 // Clear the flag
          Exit;
        end else
          Temp := Temp shr 1;
      end
    end
  end
end;

function OPStackNode_IsAnyEventSet(var Events: TNodeEventArray): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to USER_MAX_PCER_BYTES - 1 do
  begin
    if Events[i] <> 0 then
    begin
      Result := True;
      Break;
    end
  end
end;

// *****************************************************************************
//  procedure OPStackNode_SetPCER_Flags;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackNode_SetPCER_Flags(Node: PNMRAnetNode);
var
  i: Integer;
begin
  for i := 0 to USER_MAX_PCER_BYTES - 1 do
    Node^.Events.PCER[i] := $FF
end;

// *****************************************************************************
//  procedure OPStackNode_SetPCER_Flag;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackNode_SetPCER_Flag(Node: PNMRAnetNode; EventIndex: Integer; Clear: Boolean);
var
  ByteOffset: Integer;
  Mask: Byte;
begin
  ByteOffset := EventIndex div 8;    // There are 8 PCERs supported in each Byte
  Mask := $01;
  Mask := Mask shl (EventIndex mod 8);
  if Clear then
  begin
    Mask := not Mask;
    Node^.Events.PCER[ByteOffset] := Node^.Events.PCER[ByteOffset] and Mask;
  end else
    Node^.Events.PCER[ByteOffset] := Node^.Events.PCER[ByteOffset] or Mask;
end;

// *****************************************************************************
//  procedure OPStackNode_ClearPCER_Flags;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackNode_ClearPCER_Flags(Node: PNMRAnetNode);
var
  i: Integer;
begin
  for i := 0 to USER_MAX_PCER_BYTES - 1 do
    Node^.Events.PCER[i] := $00
end;

// *****************************************************************************
//  procedure OPStackNode_NextPCER_Flag;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackNode_NextPCER_Flag(Node: PNMRAnetNode): Integer;
var
  i, j: Integer;
  Temp: Byte;
begin
  Result := -1;
  for i := 0 to USER_MAX_PCER_BYTES - 1 do
  begin
    if Node^.Events.PCER[i] <> 0 then
    begin
      Temp := Node^.Events.PCER[i];
      for j := 0 to 7 do                                                      // Find the first non zero state in the byte
      begin
        if Temp and $01 <> 0 then
        begin
          Result := (i*8) + j;
          OPStackNode_SetPCER_Flag(Node, Result, True); // Clear the flag
          Exit;
        end else
          Temp := Temp shr 1;
      end
    end
  end
end;

// *****************************************************************************
//  procedure OPStackNode_IsAnyPCER_Set;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackNode_IsAnyPCER_Set(Node: PNMRAnetNode): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to USER_MAX_PCER_BYTES - 1 do
  begin
    if Node^.Events.PCER[i] <> 0 then
    begin
      Result := True;
      Break;
    end
  end
end;

// *****************************************************************************
//  procedure OPStackNode_IncomingMessageLink;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackNode_IncomingMessageLink(Node: PNMRAnetNode; AMessage: POPStackMessage);
var
  Temp: POPStackMessage;
begin
  if Node^.IncomingMessages = nil then
    Node^.IncomingMessages := AMessage
  else begin                                  // Tack it to the end of the chain
    Temp := Node^.IncomingMessages;
    while Temp^.NextIncoming <> nil do
      Temp := Temp^.NextIncoming;
    Temp^.NextIncoming := AMessage
  end
end;

// *****************************************************************************
//  procedure OPStackNode_IncomingMessageUnLink;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackNode_IncomingMessageUnLink(Node: PNMRAnetNode; AMessage: POPStackMessage);
var
  Temp, Parent: POPStackMessage;
begin
  if Node^.IncomingMessages <> nil then
  begin
    if Node^.IncomingMessages = AMessage then                                           // Root Buffer match case is easy
    begin
      Node^.IncomingMessages := Node^.IncomingMessages^.NextIncoming;
      AMessage^.NextIncoming := nil;
    end
    else begin
      Parent := Node^.IncomingMessages;                                                 // Already know it is not the root buffer so setup for the first level down
      Temp := Node^.IncomingMessages^.NextIncoming;
      while (Temp <> nil) and (Temp <> AMessage) do
      begin
        Parent := Temp;
        Temp := Temp^.NextIncoming;
        AMessage^.NextIncoming := nil;
      end;
      if Temp <> nil then
        Parent^.NextIncoming := Temp^.NextIncoming
    end
  end
end;

// *****************************************************************************
//  procedure OPStackNode_NextIncomingMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackNode_NextIncomingMessage(Node: PNMRAnetNode): POPStackMessage;
begin
  Result := Node^.IncomingMessages;
end;

// *****************************************************************************
//  procedure OPStackNode_StateMachineMessageLink;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackNode_StateMachineMessageLink(Node: PNMRAnetNode; AMessage: POPStackMessage);
var
  Temp: POPStackMessage;
begin
  if Node^.StateMachineMessages = nil then
    Node^.StateMachineMessages := AMessage
  else begin                                  // Tack it to the end of the chain
    Temp := Node^.StateMachineMessages;
    while Temp^.NextIncoming <> nil do
      Temp := Temp^.NextIncoming;
    Temp^.NextIncoming := AMessage
  end
end;

// *****************************************************************************
//  procedure OPStackNode_StateMachineMessageUnLink;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackNode_StateMachineMessageUnLink(Node: PNMRAnetNode; AMessage: POPStackMessage);
var
  Temp, Parent: POPStackMessage;
begin
  if Node^.StateMachineMessages <> nil then
  begin
    if Node^.StateMachineMessages = AMessage then                                           // Root Buffer match case is easy
      Node^.StateMachineMessages := Node^.StateMachineMessages^.NextIncoming
    else begin
      Parent := Node^.StateMachineMessages;                                                 // Already know it is not the root buffer so setup for the first level down
      Temp := Node^.StateMachineMessages^.NextIncoming;
      while (Temp <> nil) and (Temp <> AMessage) do
      begin
        Parent := Temp;
        Temp := Temp^.NextIncoming
      end;
      if Temp <> nil then
        Parent^.NextIncoming := Temp^.NextIncoming
    end
  end
end;

// *****************************************************************************
//  procedure OPStackNode_NextStateMachineMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackNode_NextStateMachineMessage(Node: PNMRAnetNode): POPStackMessage;
begin
  Result := Node^.StateMachineMessages;    // Basic for now
end;

// *****************************************************************************
//  procedure OPStackNode_Find;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackNode_Find(AMessage: POPStackMessage; FindBy: Byte): PNMRAnetNode;
var
  i: Integer;
  NodeID: TNodeID;
  AliasID: Word;
begin
  Result := nil;
  if FIndBy = FIND_BY_SOURCE then
  begin
    AliasID := AMessage^.Source.AliasID;
    NodeID := AMessage^.Source.ID;
  end else
  begin
    AliasID := AMessage^.Dest.AliasID;
    NodeID := AMessage^.Dest.ID;
  end;

  if AliasID <> 0 then
  begin
    for i := 0 to NodePool.AllocatedCount - 1 do
    begin
      if NodePool.AllocatedList[i]^.Info.AliasID = AliasID then
        if NodePool.AllocatedList[i]^.State and NS_RELEASING = 0 then
        begin
          Result := NodePool.AllocatedList[i];
          Exit
        end;
    end;
  end else
  begin
    for i := 0 to NodePool.AllocatedCount - 1 do
    begin
      if NodePool.AllocatedList[i]^.Info.ID[0] = NodeID[0] then
        if NodePool.AllocatedList[i]^.Info.ID[1] = NodeID[1] then
          if NodePool.AllocatedList[i]^.State and NS_RELEASING = 0 then
          begin
            Result := NodePool.AllocatedList[i];
            Exit
          end;
    end;
  end;
end;

// *****************************************************************************
//  procedure OPStackNode_Equal;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackNode_Equal(Message1, Message2: POPStackMessage): Boolean;
begin
  Result := False;
  if NMRAnetUtilities_EqualNodeIDInfo(Message1^.Dest, Message2^.Dest) then
    if NMRAnetUtilities_EqualNodeIDInfo(Message1^.Source, Message2^.Source) then
      Result := True
end;

end.
