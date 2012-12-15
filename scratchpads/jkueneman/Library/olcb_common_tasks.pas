unit olcb_common_tasks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, olcb_defines, common_utilities, olcb_utilities, olcb_threaded_stack;

type

  { TQueryAllAddressSpacesInfoTask }

  TQueryAllAddressSpacesInfoTask = class(TOlcbTaskBase)
  private
    FCurrentAddress: Byte;
    FMaxAddress: Byte;
    FMinAddress: Byte;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word); override;
    procedure Process(MessageInfo: TOlcbMessage); override;
    property MinAddress: Byte read FMinAddress write FMinAddress;
    property MaxAddress: Byte read FMaxAddress write FMaxAddress;
    property CurrentAddress: Byte read FCurrentAddress write FCurrentAddress;
  end;



implementation

{ TQueryAllAddressSpacesInfoTask }

constructor TQueryAllAddressSpacesInfoTask.Create(ASourceAlias,
  ADestinationAlias: Word);
begin
  inherited Create(ASourceAlias, ADestinationAlias);
  FCurrentAddress := 0;
  FMaxAddress := 0;
  FMinAddress := 0;
  Sending := True;  // We send first
end;

procedure TQueryAllAddressSpacesInfoTask.Process(MessageInfo: TOlcbMessage);
var
  DatagramSend: TDatagramSend;
  DatagramReceive: TDatagramReceive;
  CANByteArray: TCANByteArray;
begin
  case iState of
    0: begin
         // Send the Query Options Memory Config
         DatagramSend := TDatagramSend.Create;
         DatagramSend.Initialize(nil, HEADER_MEMCONFIG_OPTIONS_REQUEST, 2, SourceAlias, DestinationAlias);
         ComPortThread.AddDatagramToSend(DatagramSend);
         iState := 1;
         Sending := False;
       end;
    1: begin
         if Assigned(MessageInfo) then
         begin
           if MessageInfo is TDatagramSend then                                 // Wait for the ACK from the send
           begin
             DatagramSend := TDatagramSend( MessageInfo);
             if DatagramSend.Empty and (DatagramSend.SourceAlias = SourceAlias) and (DatagramSend.DestinationAlias = DestinationAlias) then
             begin
               Sending := False;
               iState := 2;
             end
           end;
         end;
       end;
    2: begin
         // Wait for the Memory Config Address Space Reply, another space to get info > go to 2, else goto 4
         if Assigned(MessageInfo) then
         begin
           if MessageInfo is TDatagramReceive then
           begin
             DatagramReceive := TDatagramReceive(MessageInfo);
             if (DatagramReceive.RawDatagram[1] and $FE = MCP_OP_GET_CONFIG_REPLY) and (DatagramReceive.SourceAlias = SourceAlias) and (DatagramReceive.DestinationAlias = DestinationAlias) then
             begin
               MinAddress := DatagramReceive.RawDatagram[6];
               MaxAddress := DatagramReceive.RawDatagram[5];
               CurrentAddress := MaxAddress;
               Sending := True;
               iState := 3;
             end;
           end;
         end;
       end;
    3: begin
         DatagramSend := TDatagramSend.Create;
         CANByteArray := HEADER_MEMCONFIG_SPACE_INFO_UNKNOWN_REQUEST;
         CANByteArray[2] := CurrentAddress;                                     // Set the addressed
         DatagramSend.Initialize(nil, CANByteArray, 3, SourceAlias, DestinationAlias);
         ComPortThread.AddDatagramToSend(DatagramSend);
         Sending := False;  // Wait for the ACK
         iState := 4;
       end;
    4: begin
         if Assigned(MessageInfo) then
         begin
           if MessageInfo is TDatagramSend then                                 // Waiting for the ACK from the send
           begin
             DatagramSend := TDatagramSend( MessageInfo);
             if DatagramSend.Empty and (DatagramSend.SourceAlias = SourceAlias) and (DatagramSend.DestinationAlias = DestinationAlias) then
             begin
               Sending := False;
               iState := 5;
             end;
           end;
         end;
       end;
    5: begin
         // Wait for the Memory Config Address Space Reply, another space to get info > go to 2, else goto 4
         if Assigned(MessageInfo) then
         begin
           if MessageInfo is TDatagramReceive then
           begin
             DatagramReceive := TDatagramReceive(MessageInfo);
             if (DatagramReceive.RawDatagram[1] and $FE = MCP_OP_GET_ADD_SPACE_INFO_REPLY) and (DatagramReceive.SourceAlias = SourceAlias) and (DatagramReceive.DestinationAlias = DestinationAlias) then
             begin
               Dec(FCurrentAddress);
               if CurrentAddress < MinAddress then
                 iState :=6
               else
                 iState := 3;
               Sending := True;
             end;
           end;
         end;
       end;
    6: begin
       // Done
         FDone := True
       end;
  end;
end;

end.

