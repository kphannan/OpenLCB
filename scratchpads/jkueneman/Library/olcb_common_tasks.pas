unit olcb_common_tasks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, olcb_defines, olcb_utilities, olcb_threaded_stack,
  olcb_structure_helpers;


const
  ERROR_NO_MEMORY_CONFIG_PROTOCOL = $00000001;
  ERROR_NO_CDI_PROTOCOL           = $00000002;
  ERROR_NO_CDI_ADDRESS_SPACE      = $00000004;
  ERROR_ADDRESS_SPACE_NOT_PRESENT = $00000008;

type

  { TVerifyNodeIDGlobalTask }

  TVerifyNodeIDGlobalTask = class(TOlcbTaskBase)
  public
   procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TVerifyNodeIDTask }

  TVerifyNodeIDTask = class(TOlcbTaskBase)
  public
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TProtocolSupportTask }

  TProtocolSupportTask = class(TOlcbTaskBase)
  private
    FProtocols: QWord;
  public
    procedure Process(MessageInfo: TOlcbMessage); override;
    property Protocols: QWord read FProtocols;
  end;

  { TSimpleNodeInformationTask }

  TSimpleNodeInformationTask = class(TOlcbTaskBase)
  private
    FSnip: TOlcbSNIP;
    FStateMachineIndex: Integer;  // for inner SNIP statemachine
  protected
    property StateMachineIndex: Integer read FStateMachineIndex write FStateMachineIndex;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; StartAsSending: Boolean); override;
    destructor Destroy; override;
    procedure Process(MessageInfo: TOlcbMessage); override;
    property Snip: TOlcbSNIP read FSnip;
  end;

  { TConfigMemoryOptionsTask }

  TConfigMemoryOptionsTask = class(TOlcbTaskBase)
  private
    FConfigMemoryOptions: TOlcbMemOptions;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; StartAsSending: Boolean); override;
    destructor Destroy; override;
    procedure Process(MessageInfo: TOlcbMessage); override;
    property ConfigMemoryOptions: TOlcbMemOptions read FConfigMemoryOptions write FConfigMemoryOptions;
  end;

  { TConfigMemoryAddressSpaceInfoTask }

  TConfigMemoryAddressSpaceInfoTask = class(TOlcbTaskBase)
  private
    FAddressSpace: Byte;
    FConfigMemoryAddressSpace: TOlcbMemAddressSpace;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; StartAsSending: Boolean; AnAddressSpace: Byte); reintroduce; virtual;
    destructor Destroy; override;
    procedure Process(MessageInfo: TOlcbMessage); override;
    property ConfigMemoryAddressSpace: TOlcbMemAddressSpace read FConfigMemoryAddressSpace write FConfigMemoryAddressSpace;
    property AddressSpace: Byte read FAddressSpace;
  end;


  { TEnumAllConfigMemoryAddressSpaceInfoTask }

  TEnumAllConfigMemoryAddressSpaceInfoTask = class(TOlcbTaskBase)
  private
    FCurrentAddressSpace: Byte;
    FMaxAddressSpace: Byte;
    FMinAddressSpace: Byte;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; StartAsSending: Boolean); override;
    procedure Process(MessageInfo: TOlcbMessage); override;
    property MinAddressSpace: Byte read FMinAddressSpace write FMinAddressSpace;
    property MaxAddressSpace: Byte read FMaxAddressSpace write FMaxAddressSpace;
    property CurrentAddressSpace: Byte read FCurrentAddressSpace write FCurrentAddressSpace;
  end;

  { TReadAddressSpaceMemoryTask }

  TReadAddressSpaceMemoryTask = class(TOlcbTaskBase)
  private
    FAddressSpace: Byte;
    FCurrentAddress: DWord;
    FCurrentSendSize: Byte;
    FDataStream: TMemoryStream;
    FForceOptionalSpaceByte: Boolean;
    FMaxAddress: DWord;
    FMinAddress: DWord;
    function GetMaxPayloadSize: Byte;
  protected
    procedure CalculateCurrentSendSize;
    property CurrentAddress: DWord read FCurrentAddress write FCurrentAddress;
    property CurrentSendSize: Byte read FCurrentSendSize write FCurrentSendSize;
    property MaxPayloadSize: Byte read GetMaxPayloadSize;

  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; StartAsSending: Boolean; AnAddressSpace: Byte); reintroduce;
    destructor Destroy; override;
    procedure Process(MessageInfo: TOlcbMessage); override;

    property AddressSpace: Byte read FAddressSpace;
    property DataStream: TMemoryStream read FDataStream;
    property ForceOptionalSpaceByte: Boolean read FForceOptionalSpaceByte write FForceOptionalSpaceByte;
    property MinAddress: DWord read FMinAddress;
    property MaxAddress: DWord read FMaxAddress;
  end;



implementation

{ TSimpleNodeInformationTask }

constructor TSimpleNodeInformationTask.Create(ASourceAlias, ADestinationAlias: Word; StartAsSending: Boolean);
begin
  inherited Create(ASourceAlias, ADestinationAlias, StartAsSending);
  FSnip := TOlcbSNIP.Create;
end;

destructor TSimpleNodeInformationTask.Destroy;
begin
  FreeAndNil(FSnip);
  inherited Destroy;
end;

procedure TSimpleNodeInformationTask.Process(MessageInfo: TOlcbMessage);
const
  STATE_SNII_MFG_VERSION  = 0;
  STATE_SNII_MFG_NAME     = 1;
  STATE_SNII_MFG_MODEL    = 2;
  STATE_SNII_HARDWARE_VER = 3;
  STATE_SNII_SOFTWARE_VER = 4;
  STATE_SNII_USER_VERSION = 5;
  STATE_SNII_USER_NAME    = 6;
  STATE_SNII_USER_DESC    = 7;
var
  i: Integer;
  LocalMessageHelper: TOpenLCBMessageHelper;
begin
  case iState of
    0: begin
         SendSnipMessage;
         StateMachineIndex := 0;
         Sending := False;
         Inc(FiState);
       end;
    1: begin
         if IsSnipMessageReply(MessageInfo) then
         begin
           LocalMessageHelper := TOpenLCBMessageHelper( MessageInfo);  // Already know this is true
           i := 2;                                               // Strip off the destination Alias
           while i < LocalMessageHelper.DataCount do
           begin
             case StateMachineIndex of
               STATE_SNII_MFG_VERSION :
                 begin
                   Snip.SniiMfgVersion := LocalMessageHelper.Data[i];
                   Inc(i);
                   StateMachineIndex := STATE_SNII_MFG_NAME;
                 end;
               STATE_SNII_MFG_NAME     :
                 begin
                   if Chr( LocalMessageHelper.Data[i]) <> #0 then
                   begin
                     Snip.SniiMfgName := Snip.SniiMfgName + Chr( LocalMessageHelper.Data[i]);
                     Inc(i);
                   end else
                   begin
                     Inc(i);
                     StateMachineIndex := STATE_SNII_MFG_MODEL;
                   end;
                 end;
               STATE_SNII_MFG_MODEL     :
                 begin
                   if Chr( LocalMessageHelper.Data[i]) <> #0 then
                   begin
                     Snip.SniiMfgModel := Snip.SniiMfgModel + Chr( LocalMessageHelper.Data[i]);
                     Inc(i);
                   end else
                   begin
                     Inc(i);
                     StateMachineIndex := STATE_SNII_HARDWARE_VER;
                   end;
                 end;
               STATE_SNII_HARDWARE_VER  :
                 begin
                   if Chr( LocalMessageHelper.Data[i]) <> #0 then
                   begin
                     Snip.SniiHardwareVersion := Snip.SniiHardwareVersion + Chr( LocalMessageHelper.Data[i]);
                     Inc(i);
                   end else
                   begin
                     Inc(i);
                     StateMachineIndex := STATE_SNII_SOFTWARE_VER;
                   end;
                 end;
               STATE_SNII_SOFTWARE_VER  :
                 begin
                   if Chr( LocalMessageHelper.Data[i]) <> #0 then
                   begin
                     Snip.SniiSoftwareVersion := Snip.SniiSoftwareVersion + Chr( LocalMessageHelper.Data[i]);
                     Inc(i);
                   end else
                   begin
                     Inc(i);
                     StateMachineIndex := STATE_SNII_USER_VERSION;
                   end;
                 end;
               STATE_SNII_USER_VERSION  :
                 begin
                   Snip.SniiUserVersion := LocalMessageHelper.Data[i];
                   Inc(i);
                   StateMachineIndex := STATE_SNII_USER_NAME;
                 end;
               STATE_SNII_USER_NAME     :
                 begin
                   if Chr( LocalMessageHelper.Data[i]) <> #0 then
                   begin
                     Snip.SniiUserName := Snip.SniiUserName + Chr( LocalMessageHelper.Data[i]);
                     Inc(i);
                   end else
                   begin
                     Inc(i);
                     StateMachineIndex := STATE_SNII_USER_DESC;
                   end;
                 end;
               STATE_SNII_USER_DESC     :
                 begin
                   if Chr( LocalMessageHelper.Data[i]) <> #0 then
                   begin
                     Snip.SniiUserDescription := Snip.SniiUserDescription + Chr( LocalMessageHelper.Data[i]);
                     Inc(i);
                   end else
                   begin
                     Inc(i);
                     Sending := True;
                     Inc(FiState);
                   end;
                 end;
             end;
           end
         end;
       end;
    2: begin
         FDone := True;
       end;
  end;
end;

{ TProtocolSupportTask }

procedure TProtocolSupportTask.Process(MessageInfo: TOlcbMessage);
begin
  case iState of
    0: begin
         SendProtocolIdentificationProtocolMessage;
         FProtocols := 0;
         Sending := False;
         Inc(FiState);
       end;
    1: begin
         if IsProtocolIdentificationProcolReply(MessageInfo) then
         begin
           FProtocols := TOpenLCBMessageHelper( MessageInfo).ExtractDataBytesAsInt(2, 7);
           Sending := True;
           Inc(FiState);
         end;
       end;
    2: begin
         FDone := True;
       end;
  end;
end;

{ TConfigMemoryAddressSpaceInfoTask }

constructor TConfigMemoryAddressSpaceInfoTask.Create(ASourceAlias, ADestinationAlias: Word; StartAsSending: Boolean; AnAddressSpace: Byte);
begin
  inherited Create(ASourceAlias, ADestinationAlias, StartAsSending);
  FAddressSpace := AnAddressSpace;
  ConfigMemoryAddressSpace := TOlcbMemAddressSpace.Create;
end;

destructor TConfigMemoryAddressSpaceInfoTask.Destroy;
begin
  FreeAndNil(FConfigMemoryAddressSpace);
  inherited Destroy;
end;

procedure TConfigMemoryAddressSpaceInfoTask.Process(MessageInfo: TOlcbMessage);
var
  DatagramReceive: TDatagramReceive;
begin
  case iState of
    0: begin
         SendMemoryConfigurationSpaceInfo(AddressSpace);
         Sending := False;
         Inc(FiState);
       end;
    1: begin
         if IsDatagramAckFromDestination(MessageInfo) then
         begin
           Sending := False;
           Inc(FiState);
         end;
       end;
    2: begin
         if IsConfigMemorySpaceInfoReplyFromDestination(MessageInfo, AddressSpace, DatagramReceive) then
         begin
           ConfigMemoryAddressSpace.LoadByDatagram(DatagramReceive);
           Sending := True;
           Inc(FiState);
         end;
       end;
    3: begin
         FDone := True;
       end;
  end;
end;

{ TConfigMemoryOptionsTask }

constructor TConfigMemoryOptionsTask.Create(ASourceAlias, ADestinationAlias: Word; StartAsSending: Boolean);
begin
  inherited Create(ASourceAlias, ADestinationAlias, StartAsSending);
  FConfigMemoryOptions := TOlcbMemOptions.Create;
end;

destructor TConfigMemoryOptionsTask.Destroy;
begin
  FreeAndNil(FConfigMemoryOptions);
  inherited Destroy;
end;

procedure TConfigMemoryOptionsTask.Process(MessageInfo: TOlcbMessage);
var
  DatagramReceive: TDatagramReceive;
begin
  case iState of
    0: begin
         SendMemoryConfigurationOptions;
         Sending := False;
         Inc(FiState);
       end;
    1: begin
         if IsDatagramAckFromDestination(MessageInfo) then
         begin
           Sending := False;
           Inc(FiState);
         end;
       end;
    2: begin
         if IsConfigMemoryOptionsReplyFromDestination(MessageInfo, DatagramReceive) then
         begin
           ConfigMemoryOptions.LoadFromDatagram(DatagramReceive);
           Sending := True;
           Inc(FiState);
         end;
       end;
    3: begin
         FDone := True;
       end;
  end;
end;

{ TVerifyNodeIDTask }

procedure TVerifyNodeIDTask.Process(MessageInfo: TOlcbMessage);
begin
  case iState of
    0: begin
         SendVerifyNodeIDGlobalMessage;
         Inc(FiState);
       end;
    1: begin
         FDone := True;
       end;
  end;
end;

{ TVerifyNodeIDGlobalTask }

procedure TVerifyNodeIDGlobalTask.Process(MessageInfo: TOlcbMessage);
begin
  case iState of
    0: begin
         SendVerifyNodeIDToDestinationMessage;
         Inc(FiState);
       end;
    1: begin
         FDone := True;
       end;
  end;
end;

{ TReadAddressSpaceMemoryTask }

function TReadAddressSpaceMemoryTask.GetMaxPayloadSize: Byte;
begin
  if ForceOptionalSpaceByte then
    Result := MAX_DATAGRAM_LENGTH - 7
  else
    Result := MAX_DATAGRAM_LENGTH - 6;
end;

procedure TReadAddressSpaceMemoryTask.CalculateCurrentSendSize;
begin
  if MaxAddress - CurrentAddress > MaxPayloadSize then
    CurrentSendSize := MaxPayloadSize
  else
    CurrentSendSize := MaxAddress - CurrentAddress;
end;

constructor TReadAddressSpaceMemoryTask.Create(ASourceAlias, ADestinationAlias: Word; StartAsSending: Boolean; AnAddressSpace: Byte);
begin
  inherited Create(ASourceAlias, ADestinationAlias, StartAsSending);
  FAddressSpace := AnAddressSpace;
  ForceOptionalSpaceByte := False;
  FDataStream := TMemoryStream.Create;
end;

destructor TReadAddressSpaceMemoryTask.Destroy;
begin
  FreeAndNil(FDataStream);
  inherited Destroy;
end;

procedure TReadAddressSpaceMemoryTask.Process(MessageInfo: TOlcbMessage);
// Outline:
//    Read Protocols to see if the Memory Protocol is supported
//    Read Memory Protocol Options to read the Min/Max supported Spaces to see if $FF is supported
//    Read Memory Protocol Space Info to read size of the CDI Space
//    Read Memory Protocol at Space $FF until done
//

const
  STATE_DONE = 12;
var
  DatagramReceive: TDatagramReceive;
  PIP: TOlcbProtocolIdentification;
  Space: TOlcbMemAddressSpace;
  Options: TOlcbMemOptions;
  DatagramResultStart: Byte;
  i: Integer;
begin
  case iState of
    0: begin
         SendProtocolIdentificationProtocolMessage;
         Inc(FiState);
         Sending := False;
       end;
    1: begin
         if IsProtocolIdentificationProcolReply(MessageInfo) then
         begin
           PIP := TOlcbProtocolIdentification.Create;
           try
             PIP.LoadByMessage( TOpenLCBMessageHelper( MessageInfo));   // Already know that MessageInfo is a TOpenLCBMessageHelper by this point
             if PIP.MemoryConfigProtocol {and PIP.ConfigDescriptionInfoProtocol} then
             begin
               Sending := True;
               Inc(FiState);
             end else
             begin
               if not PIP.MemoryConfigProtocol then
                 ErrorCode := ErrorCode or ERROR_NO_MEMORY_CONFIG_PROTOCOL;
          {     if not PIP.ConfigDescriptionInfoProtocol then
                 ErrorCode := ErrorCode or ERROR_NO_CDI_PROTOCOL; }
               Sending := True;
               iState := STATE_DONE;
             end
           finally
             FreeAndNil(PIP)
           end
         end
       end;
    2: begin
         SendMemoryConfigurationOptions;
         Inc(FiState);
         Sending := False;
       end;
    3: begin
         if IsDatagramAckFromDestination(MessageInfo) then
         begin
           Sending := False;
           Inc(FiState);
         end;
       end;
    4: begin
         DatagramReceive := nil;
         if IsConfigMemoryOptionsReplyFromDestination(MessageInfo, DatagramReceive) then
         begin
           Options := TOlcbMemOptions.Create;
           try
             Options.LoadFromDatagram(DatagramReceive);
             if (AddressSpace <= Options.AddressSpaceHi) and (AddressSpace >= Options.AddressSpaceLo) then
             begin
               Sending := True;
               Inc(FiState);
             end else
             begin
               Sending := True;
               ErrorCode := ERROR_NO_CDI_ADDRESS_SPACE;
               iState := STATE_DONE;
             end;
           finally
             FreeAndNil(Options);
           end;
         end;
       end;
    5: begin
         SendMemoryConfigurationSpaceInfo(AddressSpace);
         Sending := False;
         Inc(FiState);
       end;
    6: begin
         if IsDatagramAckFromDestination(MessageInfo) then
         begin
           Sending := False;
           Inc(FiState);
         end;
       end;
    7: begin
         if IsConfigMemorySpaceInfoReplyFromDestination(MessageInfo, AddressSpace, DatagramReceive) then
         begin
           Space := TOlcbMemAddressSpace.Create;
           try
             Space.LoadByDatagram(DatagramReceive);
             if Space.IsPresent then
             begin
               FMinAddress := Space.AddressLo;
               FMaxAddress := Space.AddressHi;
               CurrentAddress := MinAddress;
               DataStream.Position := 0;
               Sending := True;
               Inc(FiState);
             end else
             begin
               ErrorCode := ErrorCode or ERROR_ADDRESS_SPACE_NOT_PRESENT;
               iState := STATE_DONE;
             end
           finally
             FreeAndNil(Space);
           end
         end;
       end;
    8: begin
         CalculateCurrentSendSize;
         Sending := True;
         Inc(FiState);
       end;
    9: begin
         SendMemoryConfigurationRead(AddressSpace, CurrentAddress, CurrentSendSize, ForceOptionalSpaceByte);
         Sending := False;
         Inc(FiState);
       end;
    10: begin
         if IsDatagramAckFromDestination(MessageInfo) then
         begin
           Sending := False;
           Inc(FiState);
         end;
       end;
    11: begin
          DatagramReceive := nil;
          if IsConfigMemoryReadReplyFromDestination(MessageInfo, DatagramReceive) then
          begin
            if ForceOptionalSpaceByte or (DatagramReceive.RawDatagram[1] and $03 = 0) then    // If using the {Space} byte need to skip over it
              DatagramResultStart := 7
            else
              DatagramResultStart := 6;
            for i := DatagramResultStart to DatagramReceive.CurrentPos - 1 do
              DataStream.WriteByte( DatagramReceive.RawDatagram[i]);
            CurrentAddress := CurrentAddress + (DatagramReceive.CurrentPos - DatagramResultStart);
            if CurrentAddress = MaxAddress then
            begin
              Sending := True;
              iState := STATE_DONE;
            end else
            begin
              Sending := True;
              iState := 8;
            end;
          end
        end;
    12: begin
       // Done
         FDone := True
       end;
  end;
end;

{ TEnumAllConfigMemoryAddressSpaceInfoTask }

constructor TEnumAllConfigMemoryAddressSpaceInfoTask.Create(ASourceAlias, ADestinationAlias: Word; StartAsSending: Boolean);
begin
  inherited Create(ASourceAlias, ADestinationAlias, StartAsSending);
  FCurrentAddressSpace := 0;
  FMaxAddressSpace := 0;
  FMinAddressSpace := 0;
end;

procedure TEnumAllConfigMemoryAddressSpaceInfoTask.Process(MessageInfo: TOlcbMessage);
var
  DatagramReceive: TDatagramReceive;
  Task: TConfigMemoryAddressSpaceInfoTask;
  i: Integer;
begin
  case iState of
    0: begin
         SendMemoryConfigurationOptions;
         iState := 1;
         Sending := False;
       end;
    1: begin
         if IsDatagramAckFromDestination(MessageInfo) then
         begin
           Sending := False;
           iState := 2;
         end;
       end;
    2: begin
         DatagramReceive := nil;
         if IsConfigMemoryOptionsReplyFromDestination(MessageInfo, DatagramReceive) then
         begin
           MinAddressSpace := DatagramReceive.RawDatagram[6];
           MaxAddressSpace := DatagramReceive.RawDatagram[5];
           CurrentAddressSpace := MaxAddressSpace;
           Sending := True;
           iState := 3;
         end;
       end;
    3: begin
         for i := MinAddressSpace to MaxAddressSpace do
         begin
           Task := TConfigMemoryAddressSpaceInfoTask.Create(SourceAlias, DestinationAlias, True, i);
           Task.OnBeforeDestroy := OnBeforeDestroy;
           ComPortThread.AddTask(Task);
         end;
         Sending := True;
         iState := 4;
       end;

    4: begin
       // Done
         FDone := True
       end;
  end;
end;

end.

