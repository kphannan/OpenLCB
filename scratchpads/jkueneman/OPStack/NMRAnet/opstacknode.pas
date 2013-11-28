unit opstacknode;

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  {$IFDEF HARDWARE_TEMPLATE}
  hardware_template,
  template_node,
  template_vnode,
  {$ENDIF}
  {$IFDEF HARDWARE_DSPIC_CAN}hardware_dspic_CAN,{$ENDIF}
  {$IFDEF HARDWARE_ENC28J60}hardware_ENC28j60,{$ENDIF}
  nmranetutilities,
  opstackdefines;

{$I NodeID.inc}

const
  FIND_BY_SOURCE = 0;
  FIND_BY_DEST = 1;

type
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
function OPStackNode_Find(AMessage: POPStackMessage; FindBy: Byte): PNMRAnetNode;    // See FIND_BY_xxxx constants
function OPStackNode_FindByAlias(AliasID: Word): PNMRAnetNode;
function OPStackNode_FindByID(var ID: TNodeID): PNMRAnetNode;
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

procedure OPStackNode_SetEventConsumedFlags(Node: PNMRAnetNode; State: Byte);
procedure OPStackNode_SetEventProducedFlags(Node: PNMRAnetNode; State: Byte);
procedure OPStackNode_SetEventFlag(var Events: TNodeEventArray; EventIndex: Integer; State: Byte);
procedure OPStackNode_ClearEventFlags(var Events: TNodeEventArray);
function OPStackNode_NextEventFlag(var Events: TNodeEventArray; var State: Byte): Integer;
function OPStackNode_IsAnyEventSet(var Events: TNodeEventArray): Boolean;

procedure OPStackNode_SetPCER_Flags(Node: PNMRAnetNode);
procedure OPStackNode_SetPCER_Flag(Node: PNMRAnetNode; EventIndex: Integer; Clear: Boolean);
procedure OPStackNode_ClearPCER_Flags(Node: PNMRAnetNode);
function OPStackNode_NextPCER_Flag(Node: PNMRAnetNode): Integer;
function OPStackNode_IsAnyPCER_Set(Node: PNMRAnetNode): Boolean;

procedure OPStackNode_MessageLink(Node: PNMRAnetNode; AMessage: POPStackMessage);
procedure OPStackNode_MessageUnLink(Node: PNMRAnetNode; AMessage: POPStackMessage);
function OPStackNode_NextMessage(Node: PNMRAnetNode): POPStackMessage;

{$IFDEF SUPPORT_STREAMS}
procedure OPStackNode_StreamLink(Node: PNMRAnetNode; AMessage: POPStackMessage);
procedure OPStackNode_StreamUnLink(Node: PNMRAnetNode; AMessage: POPStackMessage);
function OPStackNode_FindStream(Node: PNMRAnetNode; DestID, SourceID: Byte; var LinkNode: TNodeInfo): POPStackMessage;
function OPStackNode_NextStream(Node: PNMRAnetNode): POPStackMessage;
{$ENDIF}



function OPStackNode_Equal(Message1, Message2: POPStackMessage): Boolean;

var
  NodePool: TNodePool;

implementation

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
  for i := 0 to USER_MAX_NODE_COUNT - 1 do
  begin
    Node := @NodePool.Pool[i];
    Node^.iIndex := i;
    OPStackNode_ZeroizeNode(Node);
    Node^.IncomingMessages := nil;
    {$IFDEF SUPPORT_STREAMS}
    Node^.StreamMessages := nil;
    {$ENDIF}
    Node^.Info.ID[0] := NodeID_LO + i;         // Picked up from the NodeID.inc file
    Node^.Info.ID[1] := NodeID_HI;
    Node^.Login.Seed := Node^.Info.ID;
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
      Result^.Info.AliasID := NMRAnetUtilities_CreateAliasID(Result^.Login.Seed, False); // Pregenerate it so it can be sorted
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

  Node^.Login.iCID := 0;
  Node^.Login.TimeCounter := 0;
  Node^.Info.AliasID := 0;
  Node^.iStateMachine := 0;
  for j := 0 to USER_MAX_EVENTS_BYTES - 1 do
    Node^.Events.Consumed[j] := 0;
  for j := 0 to USER_MAX_EVENTS_BYTES - 1 do
    Node^.Events.Produced[j] := 0;
  for j := 0 to USER_MAX_PCER_BYTES - 1 do
    Node^.Events.PCER[j] := 0;
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
//  procedure OPStackNode_SetEventConsumedFlags;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackNode_SetEventConsumedFlags(Node: PNMRAnetNode; State: Byte);
var
  i: Integer;
begin
  if OPStackNode_TestState(Node, NS_VIRTUAL) then
  begin
    for i := 0 to (USER_MAX_VNODE_SUPPORTED_EVENTS_CONSUMED + USER_MAX_VNODE_SUPPORTED_DYNAMIC_EVENTS_CONSUMED) - 1 do
      OPStackNode_SetEventFlag(Node^.Events.Consumed, i, State);
  end else
  begin
    for i := 0 to (USER_MAX_SUPPORTED_EVENTS_CONSUMED + USER_MAX_SUPPORTED_DYNAMIC_EVENTS_CONSUMED) - 1 do
      OPStackNode_SetEventFlag(Node^.Events.Consumed, i, State);
  end
end;

// *****************************************************************************
//  procedure OPStackNode_SetEventProducedFlags;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackNode_SetEventProducedFlags(Node: PNMRAnetNode; State: Byte);
var
  i: Integer;
begin
  if OPStackNode_TestState(Node, NS_VIRTUAL) then
  begin
    for i := 0 to (USER_MAX_VNODE_SUPPORTED_EVENTS_PRODUCED + USER_MAX_VNODE_SUPPORTED_DYNAMIC_EVENTS_CONSUMED) - 1 do
      OPStackNode_SetEventFlag(Node^.Events.Produced, i, State);
  end else
  begin
    for i := 0 to (USER_MAX_SUPPORTED_EVENTS_CONSUMED + USER_MAX_SUPPORTED_DYNAMIC_EVENTS_CONSUMED) - 1 do
      OPStackNode_SetEventFlag(Node^.Events.Produced, i, State);
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

// *****************************************************************************
//  procedure OPStackNode_MessageLink;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackNode_MessageLink(Node: PNMRAnetNode; AMessage: POPStackMessage);
var
  Temp: POPStackMessage;
begin
  if Node^.IncomingMessages = nil then
    Node^.IncomingMessages := AMessage
  else begin                                  // Tack it to the end of the chain
    Temp := Node^.IncomingMessages;
    while Temp^.Next <> nil do
      Temp := Temp^.Next;
    Temp^.Next := AMessage
  end
end;

// *****************************************************************************
//  procedure OPStackNode_MessageUnLink;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackNode_MessageUnLink(Node: PNMRAnetNode; AMessage: POPStackMessage);
var
  Temp, Parent: POPStackMessage;
begin
  if Node^.IncomingMessages <> nil then
  begin
    if Node^.IncomingMessages = AMessage then                                           // Root Buffer match case is easy
      Node^.IncomingMessages := Node^.IncomingMessages^.Next
    else begin
      Parent := Node^.IncomingMessages;                                                 // Already know it is not the root buffer so setup for the first level down
      Temp := Node^.IncomingMessages^.Next;
      while (Temp <> nil) and (Temp <> AMessage) do
      begin
        Parent := Temp;
        Temp := Temp^.Next
      end;
      if Temp <> nil then
        Parent^.Next := Temp^.Next
    end
  end
end;

// *****************************************************************************
//  procedure OPStackNode_NextMessage;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackNode_NextMessage(Node: PNMRAnetNode): POPStackMessage;
begin
  Result := Node^.IncomingMessages;
end;

{$IFDEF SUPPORT_STREAMS}
// *****************************************************************************
//  procedure OPStackNode_StreamLink;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackNode_StreamLink(Node: PNMRAnetNode; AMessage: POPStackMessage);
var
  Temp: POPStackMessage;
begin
  if Node^.StreamMessages = nil then
    Node^.StreamMessages := AMessage
  else begin                                  // Tack it to the end of the chain
    Temp := Node^.StreamMessages;
    while Temp^.Next <> nil do
      Temp := Temp^.Next;
    Temp^.Next := AMessage
  end
end;

// *****************************************************************************
//  procedure OPStackNode_StreamUnLink;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
procedure OPStackNode_StreamUnLink(Node: PNMRAnetNode; AMessage: POPStackMessage);
var
  Temp, Parent: POPStackMessage;
begin
  if Node^.StreamMessages <> nil then
  begin
    if Node^.StreamMessages = AMessage then                                           // Root Buffer match case is easy
      Node^.StreamMessages := Node^.StreamMessages^.Next
    else begin
      Parent := Node^.StreamMessages;                                                 // Already know it is not the root buffer so setup for the first level down
      Temp := Node^.StreamMessages^.Next;
      while (Temp <> nil) and (Temp <> AMessage) do
      begin
        Parent := Temp;
        Temp := Temp^.Next
      end;
      if Temp <> nil then
        Parent^.Next := Temp^.Next
    end
  end
end;

// *****************************************************************************
//  procedure OPStackNode_FindStream;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackNode_FindStream(Node: PNMRAnetNode; DestID, SourceID: Byte; var LinkNode: TNodeInfo): POPStackMessage;
var
  Temp: POPStackMessage;
  StreamBuffer: PStreamBuffer;
begin
  Result := nil;
  if Node^.StreamMessages <> nil then
  begin
    Temp := Node^.StreamMessages;
    while Temp <> nil do
    begin
      StreamBuffer := PStreamBuffer( PByte( Temp^.Buffer));
      if LinkNode.AliasID <> 0 then
      begin
        if (LinkNode.AliasID = Temp^.Dest.AliasID) then
          if (StreamBuffer^.DestID = DestID) then
            if (StreamBuffer^.SourceID = SourceID) then
            begin
              Result := Temp;
              Exit;
            end;
       Temp := Temp^.Next;
      end else
      begin
        if (LinkNode.ID[0] = Temp^.Dest.ID[0]) then
          if (LinkNode.ID[1] = Temp^.Dest.ID[1]) then
            if (StreamBuffer^.DestID = DestID) then
              if (StreamBuffer^.SourceID = SourceID) then
              begin
                Result := Temp;
                Exit;
              end;
       Temp := Temp^.Next;
      end
    end
  end;
end;

// *****************************************************************************
//  procedure OPStackNode_NextStream;
//    Parameters:
//    Result:
//    Description:
// *****************************************************************************
function OPStackNode_NextStream(Node: PNMRAnetNode): POPStackMessage;
begin
  Result := Node^.StreamMessages;    // Basic for now
end;

{$ENDIF}

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
