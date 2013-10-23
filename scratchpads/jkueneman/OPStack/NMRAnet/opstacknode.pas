unit opstacknode;

interface

{$I Options.inc}

uses
  {$IFDEF FPC}
  template_node,
  template_vnode,
  {$ENDIF}

  {$IFDEF HARDWARE_TEMPLATE}hardware_template,{$ENDIF}
  {$IFDEF HARDWARE_DSPIC_CAN}hardware_dspic_CAN,{$ENDIF}
  {$IFDEF HARDWARE_ENC28J60}hardware_ENC28j60,{$ENDIF}
  nmranetutilities,
  opstackdefines;

{$I NodeID.inc}

const                                                                           // NodeState the node empty and ready to allocate
  NS_EMPTY                = $00;                                                // NodeState the node is not allocated
  NS_ALLOCATED            = $01;                                                // NodeState the node is allocated
  NS_PERMITTED            = $02;                                                // NodeState CAN Frame Layer is permitted (Node ID's resolved with bus)
  NS_INITIALIZED          = $04;                                                // NodeState Message Layer has sent its first Initialize Complete Message
  NS_VIRTUAL              = $08;                                                // NodeState If is a virtual node
  NS_RELEASING            = $10;                                                // Node is tagged to send and AMD and be removed from the bus (while this is set what happens??)

  // MsgFlags in order of precidence (= 0 highest precidence)
  MF_DUPLICATE_NODE_ID        = $01;                                            // MsgFlag, a Duplicate Node ID was detected, critical fault
  MF_DUPLICATE_ALIAS          = $02;                                            // MsgFlag, a Duplicate Alias was Detected, critical fault
  MF_DUPLICATE_ALIAS_RID      = $04;                                            // MsgFlag, a Duplicate Alias was Detected during a CID message, not a fault just need to respond to claim the Alias
  MF_ALIAS_MAP_ENQUIRY        = $08;                                            // MsgFlag, an AMD message need to be responded to
  MF_VERIFY_NODE_ID           = $10;                                            // MsgFlag, a Verify Node ID message needs to be responded to

type
  // Each Byte contains the state of up to 4 Events, as each event can have 3 state (2 bits)
  // The Index of the 2 bits block is mapped to the index into the defined array of Event ID values
  TNodeEventArray = array[0..USER_MAX_EVENTS_BYTES] of Byte;                    // Holds the current state (set, clear, unknown) of each Event.  Used when Consumer/Producer Indentifed messages need to be sent
  // Each Byte contains the state of up to 8 Events, as each event can have 1 state (1 bits)
  // The Index of the 1 bits block is mapped to the index into the defined array of Event ID values
  TNodePCERArray  = array[0..USER_MAX_PCER_BYTES] of Byte;                      // Holds a flag for if an Event requires a PCER message to be sent becuase that event has changed state

  TNodeInfo = record
    ID: TNodeID;                                                                // Unique 48 Bit ID for Node
    Seed: TNodeID;                                                              // Seed for Random Number Generator in case we have to reseed because of a duplicate ID
    AliasID: Word;                                                              // 12 Bit Alias ID
  end;

  TNMRAnetNodeLoginInfo = record
    TimeCounter: Byte;                                                          // Number of timer ticks into the time waiting for a RID response from another node for our RID broadcasts
    iCID: Byte;                                                                 // Which of the 4 CIDs we are broadcasting
  end;

  TNodeEvents = record
    Produced,
    Consumed  : TNodeEventArray;
    PCER      : TNodePCERArray;
  end;

type
  TNMRAnetNode = record
    iIndex: Byte;                                                               // Index in the main array
    State: Byte;                                                                // See the NS_xxxx flags; State of the Node
    Events: TNodeEvents;
    Info: TNodeInfo;                                                            // Information about a Node
    Login: TNMRAnetNodeLoginInfo;                                               // Login Information
    Flags: Byte;                                                                // Message Flags for messages passed to the Node through a simple set bit (no complex reply data needed like destination Alias), see the MF_xxxx flags
    MsgFlagsUserDefined: Byte;                                                  // Message Flags for user apps to define AND handle in App Callbacks
    iStateMachine: Byte;                                                        // Statemachine index for the main bus login
 //   BaseBuffers: PBaseBuffer;                                                   // Head of a possible linked list of dataless Messages Replies to service
 //   DatagramBuffers: PDatagramBuffer;                                           // Head of a possible linked list of Datagrams to service
 //   ConfigMemBuffers: PConfigMemBuffer;                                         // Head of a possible linked list of Configuration Memory Accesses to service
 //   DataBuffers: PDataBuffer;                                                   // Head of a possible linked list of Message Replies that need sent Data Bytes to be serviced
 //   StreamBuffers: PStreamBuffer;                                               // Head of a possible linked list of Streams to service
 //   ConfigurationAddress: Generic32BitPointer;                                  // Pointer into the EEProm Memory, user assigned in the NMRAnetAppCallbacks file
 //   ParentAlias,                                                                // Definition depends on what kind of node.  If a Throttle then Parent should never be set, If a Train then the will be the Owner Throttle
 //   ChildAlias,                                                                 // Definition depends on what kind of node.  If a Throttle then Child should the Train it is controlling, if a Train then should not be set
 //   LeftSibling,                                                                // Definition depends on what kind of node.  If Train then may be the next in a Consist Chain
 //   RightSibling: ^TNMRAnetNode;                                                // Definition depends on what kind of node.  If Train then may be the previous in a Consist Chain
 //   RAMAddress: Generic32BitPointer;                                            // Pointer to a DataStructure that is in Volatile RAM defined in the user defined NMRAnetAppCallbacks file, user assigned in the NMRAnetAppCallbacks file
  end;
  PNMRAnetNode = ^TNMRAnetNode;


  TNodePool = record
    Pool: array[0..USER_MAX_NODE_COUNT-1] of TNMRAnetNode;                      // Node [0] is ALWAYS the physical node
    AllocatedList: array[0..USER_MAX_NODE_COUNT-1] of PNMRAnetNode;             // Node List sorted by Alias
    AllocatedCount: Integer;                                                    // Number of Nodes Allocated
    iActiveNode: Word;                                                          // The node that is "active" which means it is the one that the main statemachine is giving a time slice to execute
  end;
  PNodePool = ^TNodePool;


procedure OPStackNode_Initialize;
function OPStackNode_Allocate: PNMRAnetNode;
procedure OPStackNode_MarkForRelease(Node: PNMRAnetNode);
function OPStackNode_FindByAlias(AliasID: Word): PNMRAnetNode;
function OPStackNode_FindByNodeID(var NodeID: TNodeID): PNMRAnetNode;
function OPStackNode_FindFirstVirtualNode: PNMRAnetNode;
function OPStackNode_FindLastVirtualNode: PNMRAnetNode;

function OPStackNode_NextNode: PNMRAnetNode;
procedure OPStackNode_ZeroizeNode(Node: PNMRAnetNode);

procedure OPStackNode_SetFlag(Node: PNMRAnetNode; Flag: Byte);
procedure OPStackNode_SetFlags(Flags: Word);
procedure OPStackNode_ClearFlag(Node: PNMRAnetNode; Flag: Byte);
procedure OPStackNode_ClearFlags(Node: PNMRAnetNode);
function OPStackNode_TestFlags(Node: PNMRAnetNode; Flag: Word; DoClear: Boolean): Boolean;

procedure OPStackNode_SetState(Node: PNMRAnetNode; State: Byte);
procedure OPStackNode_ClearState(Node: PNMRAnetNode; State: Byte);
function OPStackNode_TestState(Node: PNMRAnetNode; State: Byte): Boolean;

procedure OPStackNode_SetEventFlags(Node: PNMRAnetNode; var Events: TNodeEventArray; State: Byte);
procedure OPStackNode_SetEventFlag(var Events: TNodeEventArray; EventIndex: Integer; State: Byte);
procedure OPStackNode_ClearEventFlags(var Events: TNodeEventArray);
function OPStackNode_NextEventFlag(var Events: TNodeEventArray; var State: Byte): Integer;
function OPStackNode_IsAnyEventSet(var Events: TNodeEventArray): Boolean;

procedure OPStackNode_SetPCER_Flags(Node: PNMRAnetNode);
procedure OPStackNode_SetPCER_Flag(Node: PNMRAnetNode; EventIndex: Integer; Clear: Boolean);
procedure OPStackNode_ClearPCER_Flags(Node: PNMRAnetNode);
function OPStackNode_NextPCER_Flag(Node: PNMRAnetNode): Integer;
function OPStackNode_IsAnyPCER_Set(Node: PNMRAnetNode): Boolean;

var
  NodePool: TNodePool;

implementation

// *****************************************************************************
//  procedure ZeroizeNodeInfo;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure ZeroizeNodeInfo(var Info: TNodeInfo);
begin
  Info.ID[0] := 0;
  Info.ID[1] := 0;
  Info.Seed := Info.ID;
  Info.AliasID := 0;
end;

// *****************************************************************************
//  procedure ZeroizeLogin;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure ZeroizeLogin(var Login: TNMRAnetNodeLoginInfo);
begin
  Login.iCID := 0;
  Login.TimeCounter := 0;
end;

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
  i, j: Integer;
  Node: PNMRAnetNode;
begin
  for i := 0 to USER_MAX_NODE_COUNT - 1 do
  begin
    Node := @NodePool.Pool[i];
    Node^.iIndex := i;
    ZeroizeNodeInfo(Node^.Info);
    OPStackNode_ZeroizeNode(Node);
    Node^.Info.ID[0] := NodeID_LO + i;         // Picked up from the NodeID.inc file
    Node^.Info.ID[1] := NodeID_HI;
    Node^.Info.Seed := Node^.Info.ID;
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
      if Result^.iIndex > 0 then
        OPStackNode_SetState(Result, NS_VIRTUAL);                               // Mark as Virtual
      Result^.Info.AliasID := NMRAnetUtilities_CreateAliasID(Result^.Info.Seed, False); // Pregenerate it so it can be sorted
      NodePool.AllocatedList[NodePool.AllocatedCount] := Result;                // Add it to the end if the Allocated List
      Inc(NodePool.AllocatedCount);                                             // One more Allocated
  //    AppCallback_NodeAllocate(Result);                                         // Allow the App layer to initialize it
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
    if Node^.State and NS_VIRTUAL = NS_VIRTUAL then                             // Only release Virtual Nodes
      Node^.State := Node^.State or NS_RELEASING;                               // Tag it to send and AMR
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
  Result := PNMRAnetNode( nil);
  for i := 0 to NodePool.AllocatedCount- 1 do
  begin
    if NodePool.AllocatedList[i]^.Info.AliasID = AliasID then
    begin
      Result := NodePool.AllocatedList[i];
      Exit;
    end;
  end
end;

// *****************************************************************************
//  procedure OPStackNode_FindByNodeID;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackNode_FindByNodeID(var NodeID: TNodeID): PNMRAnetNode;
var
  i: Integer;
begin
  Result := PNMRAnetNode( nil);
  for i := 0 to NodePool.AllocatedCount - 1 do
  begin
    if NodePool.AllocatedList[i]^.Info.ID[0] = NodeID[0] then
      if NodePool.AllocatedList[i]^.Info.ID[1] = NodeID[1] then
      begin
        Result := NodePool.AllocatedList[i];
        Exit;
      end;
  end
end;

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

// *****************************************************************************
//  procedure OPStackNode_NextNode;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackNode_NextNode: PNMRAnetNode;
begin
  Hardware_DisableInterrupts;
  Result := PNMRAnetNode( nil);
  if NodePool.AllocatedCount > 0 then
  begin
    if NodePool.iActiveNode > NodePool.AllocatedCount - 1 then
      NodePool.iActiveNode := 0;
    Result := NodePool.AllocatedList[NodePool.iActiveNode];
    Inc(NodePool.iActiveNode);
  end;
  Hardware_EnableInterrupts
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
  ZeroizeLogin(Node^.Login);
  Node^.Info.AliasID := 0;
  for j := 0 to USER_MAX_EVENTS_BYTES - 1 do
    Node^.Events.Consumed[j] := 0;
  for j := 0 to USER_MAX_EVENTS_BYTES - 1 do
    Node^.Events.Produced[j] := 0;
  for j := 0 to USER_MAX_PCER_BYTES - 1 do
    Node^.Events.PCER[j] := 0;
  Node^.iStateMachine := 0;
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
procedure OPStackNode_SetState(Node: PNMRAnetNode; State: Byte);
begin
  Node^.State := Node^.State or State;
end;

// *****************************************************************************
//  procedure OPStackNode_ClearState
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackNode_ClearState(Node: PNMRAnetNode; State: Byte);
begin
  Node^.State := Node^.State and not State;
end;

// *****************************************************************************
//  procedure OPStackNode_TestState
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackNode_TestState(Node: PNMRAnetNode; State: Byte): Boolean;
begin
  Result := Node^.State and State = State;
end;

// *****************************************************************************
//  procedure OPStackNode_SetEventFlags;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackNode_SetEventFlags(Node: PNMRAnetNode; var Events: TNodeEventArray; State: Byte);
var
  i: Integer;
begin
  if OPStackNode_TestState(Node, NS_VIRTUAL) then
  begin
    for i := 0 to USER_MAX_VNODE_SUPPORTED_EVENTS_CONSUMED - 1 do
      OPStackNode_SetEventFlag(Events, i, State);
  end else
  begin
    for i := 0 to USER_MAX_SUPPORTED_EVENTS_CONSUMED - 1 do
      OPStackNode_SetEventFlag(Events, i, State);
  end
end;

// *****************************************************************************
//  procedure OPStackNode_SetEventFlag;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackNode_SetEventFlag(var Events: TNodeEventArray; EventIndex: Integer; State: Byte);
var
  ByteOffset, {EventOffset} NormalizedIndex: Integer;
  Mask: Byte;
begin
  ByteOffset := EventIndex div 4;    // There are 4 Events supported in each Byte
  NormalizedIndex := EventIndex - (ByteOffset*4);
 // EventOffset := EventIndex mod 4;   // There are
  Mask := %00000011;
  Mask := Mask shl (NormalizedIndex * 2);  // 2 bits per event
  Mask := not Mask;
  Events[ByteOffset] := Events[ByteOffset] and Mask;
  Mask := State shl (NormalizedIndex * 2);
  Events[ByteOffset] := Events[ByteOffset] or Mask;
end;

// *****************************************************************************
//  procedure OPStackNode_ClearEventFlags;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackNode_ClearEventFlags(var Events: TNodeEventArray);
var
    i: Integer;
begin
  for i := 0 to USER_MAX_EVENTS_BYTES - 1 do                                   // Shortcut to clear them all
    Events[i] := 0;
end;

// *****************************************************************************
//  procedure OPStackNode_NextEventFlag;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackNode_NextEventFlag(var Events: TNodeEventArray; var State: Byte): Integer;
var
  i, j: Integer;
  Temp: Byte;
begin
  Result := -1;
  for i := 0 to USER_MAX_EVENTS_BYTES - 1 do
  begin
    if Events[i] <> 0 then
    begin
      Temp := Events[i];
      for j := 0 to 3 do                                                      // Find the first non zero state in the byte
      begin
        State := Temp and $03;
        if State <> 0 then
        begin
          Result := (i*4) + j;
          OPStackNode_SetEventFlag(Events, Result, EVENT_STATE_CLEAR); // Clear the flag
          Exit;
        end else
          Temp := Temp shr 2;
      end
    end
  end

end;

// *****************************************************************************
//  procedure OPStackNode_IsAnyEventSet;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackNode_IsAnyEventSet(var Events: TNodeEventArray): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to USER_MAX_EVENTS_BYTES - 1 do
  begin
    if Events[i] <> 0 then
    begin
      Result := True;
      Exit
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
  ByteOffset, NormalizedIndex: Integer;
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


end.

