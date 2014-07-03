unit template_hardware;

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  {$IFDEF FPC}
  Classes, SysUtils, blcksock, synsock, Forms, Dialogs, ethernet_hub, com_port_hub, olcb_transport_layer,
  {$ENDIF}
  gridconnect,
  opstackbuffers,
  opstackdefines,
  opstackcanstatemachinesbuffers;

// ***************************************************************************************************************************************************************************************************************************************
//  Lazarus Specific from here up
// ***************************************************************************************************************************************************************************************************************************************

procedure Hardware_Initialize;

// The OPStack calles these functions to control and/or send messages through the hardware layer
procedure Hardware_DisableInterrupts;
procedure Hardware_EnableInterrupts;

procedure OutgoingCriticalMessage(AMessage: POPStackMessage; FreeMessage: Boolean);                   // Called _back_ from within the IncomingMessageDispatch if we can't allocate buffers, unknown MTI's etc.  For CAN this is expected to be immediatly replied back to the sender as these are very high priority CAN headers
procedure OutgoingMessage(AMessage: POPStackMessage; FreeMessage: Boolean);                           // Expects that IsOutgoingBufferAvailable was called and returned True to ensure success in transmitting
procedure ProcessHardwareMessages;
function IsOutgoingBufferAvailable: Boolean;


procedure DispatchGridConnectStr(GridConnectStrPtr: PGridConnectString);

{$IFNDEF FPC}
// Callback to push received messages into the OPStack
procedure OPStackCANStatemachine_OPStackMessageToNMRAnetCanBuffer(AMessage: POPStackMessage; NMRAnetCanBuffer: PNMRAnetCanBuffer); external;
function OPStackCANStatemachine_NMRAnetCanBufferToOPStackBuffer(var NMRAnetCanBuffer: TNMRAnetCanBuffer; var OPStackMessage: POPStackMessage; var DestNode: PNMRAnetNode; var SourceNode: PNMRAnetNode): Boolean; external;
procedure OPStackCANStatemachine_ProcessMessages; external;
procedure IncomingMessageDispatch(AMessage: POPStackMessage; DestNode, SourceNode: PNMRAnetNode); external;
function OPStackNode_FindByAlias(AliasID: Word): PNMRAnetNode; external;
{$ENDIF}

{$IFDEF FPC}

procedure CreateHubs;
procedure DestroyHubs;

var
  EthernetHub: TEthernetHub;
  ComPortHub: TComPortHub;
  NodeThread: TNodeThread;
{$ENDIF}

implementation

{$IFDEF FPC}
uses
  opstackcore,
  opstackcanstatemachines;
{$ENDIF}

type
  THardware = record
    InterruptDisableCount: Integer;                                                // Allows stacked calls to Disable/Enable Interrupts (if used)
  end;

var
  Hardware: THardware;

  {$IFDEF FPC}
procedure CreateHubs;
begin
  NodeThread := TNodeThread.Create(True);
  NodeThread.Suspended := False;
  EthernetHub := TEthernetHub.Create(NodeThread);
  ComPortHub := TComPortHub.Create(NodeThread);
end;

procedure DestroyHubs;
begin
  EthernetHub.Enabled := False;
  EthernetHub.NodeThread := nil;
  FreeAndNil(EthernetHub);
  ComPortHub.NodeThread := nil;
  ComPortHub.RemoveComPort(nil);
  FreeAndNil(ComPortHub);
  NodeThread.EnableNode(False);
  NodeThread.Terminate;
  while not NodeThread.TerminateCompleted do
    Application.ProcessMessages;
  FreeAndNil(NodeThread);
end;
  {$ENDIF}

procedure Hardware_Initialize;
begin
  Hardware.InterruptDisableCount := 0;
  GridConnect_Initialize;                                                       // This is here because it only is needed if we are using a GridConnect interface
end;

// *****************************************************************************
//  procedure Hardware_DisableInterrupts
//    Parameters:
//    Result:
//    Description:   called when lists or similar are being maniuplated that
//                   could be in an interterminate state if an interrupt driven
//                   system is called in the middle of the manipulation. Use this
//                   function to disable asyncronous access to library variable
//                   during this call
// *****************************************************************************
procedure Hardware_DisableInterrupts;
begin
  // Disable Any interrupts here
  Inc(Hardware.InterruptDisableCount);
end;

// *****************************************************************************
//  procedure Hardware_EnableInterrupts
//    Parameters:
//    Result:
//    Description:   called when lists or similar are being maniuplated that
//                   could be in an interterminate state if an interrupt driven
//                   system is called in the middle of the manipulation. Use this
//                   function to re enable asyncronous access to library variable
//                   during this call
// *****************************************************************************
procedure Hardware_EnableInterrupts;
begin
  Dec(Hardware.InterruptDisableCount);
  if Hardware.InterruptDisableCount <= 0 then
  begin
    // Enable any Interrupts here
    Hardware.InterruptDisableCount := 0;
  end;
end;

procedure OutgoingCriticalMessage(AMessage: POPStackMessage; FreeMessage: Boolean);
begin
  OutgoingMessage(AMessage, FreeMessage)  // For Ethernet this is no different than a nomral Message
end;

// *****************************************************************************
//  procedure OutgoingMessage
//    Parameters:
//    Result:
//    Description:   called to send a message on the physical layer, must call
//                   IsOutgoingBufferAvailable before to ensure a buffer is
//                   available to use
// *****************************************************************************
procedure OutgoingMessage(AMessage: POPStackMessage; FreeMessage: Boolean);
var
  GridConnectStr: TGridConnectString;
  NMRAnetCanBuffer: TNMRAnetCanBuffer;
begin
  GridConnectStr[0] := #0;                                                       // Quiet the compiler
  case AMessage^.MessageType and MT_MASK of
    MT_SIMPLE :
        begin
          OPStackCANStatemachine_OPStackMessageToNMRAnetCanBuffer(AMessage, @NMRAnetCanBuffer);
          GridConnect_BufferToGridConnect(NMRAnetCanBuffer, GridConnectStr);
          if FreeMessage then
            OPStackBuffers_DeAllocateMessage(AMessage);
          {$IFDEF FPC}
             NodeThread.SendMessage(GridConnectStr + LF);
          {$ENDIF}
        end;
    MT_DATAGRAM : OPStackCANStatemachineBuffers_AddOutgoingDatagramMessage(AMessage);  // CAN can't handle a full Datagram Message so we need to parse it up into MT_SIMPLE frames
    MT_ACDISNIP : OPStackCANStatemachineBuffers_AddOutgoingAcdiSnipMessage(AMessage);  // CAN can't handle a full Datagram Message so we need to parse it up into MT_SIMPLE frames
    {$IFDEF SUPPORT_STREAMS}
    MT_STREAM   : OPStackCANStatemachineBuffers_AddOutgoingStreamMessage(AMessage);     // CAN can't handle a full Datagram Message so we need to parse it up into MT_SIMPLE frames
    {$ENDIF}
    MT_MULTIFRAME : OPStackCANStateMachineBuffer_AddOutgoingMultiFrameMessage(AMessage)
  else
     OPStackBuffers_DeAllocateMessage(AMessage);
  end;
end;

function IsOutgoingBufferAvailable: Boolean;
begin
  Result := True
end;

procedure ProcessHardwareMessages;
begin
  if IsOutgoingBufferAvailable then
    OPStackCANStatemachine_ProcessMessages;
end;

// *****************************************************************************
//  procedure DispatchGridConnectStr
//    Parameters:
//    Result:
//    Description:   called to setup a received message in GridConnect format into
//                   a format the OPStack can use.  NOTE: The buffer that is sent to
//                   IncomingMessageCallback is NOT from the allocated buffer Pool
//                   and will be gone when this function returns.  You MUST copy
//                   the contents of it if needed
// *****************************************************************************
procedure DispatchGridConnectStr(GridConnectStrPtr: PGridConnectString);
var
  CanBuffer: TNMRAnetCanBuffer;
  OPStackMessagePtr: POPStackMessage;
  SourceNode, DestNode: PNMRAnetNode;
begin
  SourceNode := nil;
  DestNode := nil;
  OpStackMessagePtr := nil;
  GridConnectStr_ToCanBuffer(GridConnectStrPtr, @CanBuffer);                     // Parse the string into a Grid Connect Data structure
  if OPStackCANStatemachine_NMRAnetCanBufferToOPStackBuffer(OPStackMessagePtr, DestNode, SourceNode, @CanBuffer) then // Convert the Grid Connect Data structure into an OPStack Message and dispatch it to the core case statement
    IncomingMessageDispatch(OPStackMessagePtr, DestNode, SourceNode);
end;


end.
