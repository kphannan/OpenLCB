unit olcb_common_tasks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, olcb_defines, olcb_utilities, olcb_threaded_stack,
  olcb_structure_helpers, math_float16;


const
  ERROR_NO_MEMORY_CONFIG_PROTOCOL = $00000001;
  ERROR_NO_CDI_PROTOCOL           = $00000002;
  ERROR_NO_CDI_ADDRESS_SPACE      = $00000004;
  ERROR_ADDRESS_SPACE_NOT_PRESENT = $00000008;
  ERROR_ADDRESS_SPACE_READ_ONLY   = $00000010;  // Trying to write to a read only space
  ERROR_ADDRESS_SPACE_WRITE_LARGER_THAN_SPACE = $00000020;
  SPEEDSTEP_DEFAULT = 28;

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
    FConfigMemAddressInfo: TOlcbMemConfig;
    FCurrentAddressSpace: Byte;
    FMaxAddressSpace: Byte;
    FMinAddressSpace: Byte;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; StartAsSending: Boolean); override;
    destructor Destroy; override;
    procedure Process(MessageInfo: TOlcbMessage); override;
    property MinAddressSpace: Byte read FMinAddressSpace write FMinAddressSpace;
    property MaxAddressSpace: Byte read FMaxAddressSpace write FMaxAddressSpace;
    property CurrentAddressSpace: Byte read FCurrentAddressSpace write FCurrentAddressSpace;
    property ConfigMemAddressInfo: TOlcbMemConfig read FConfigMemAddressInfo write FConfigMemAddressInfo;
  end;

  { TBaseAddressSpaceMemoryTask }

  TBaseAddressSpaceMemoryTask = class(TOlcbTaskBase)
  private
    FAddressSpace: Byte;
    FCurrentAddress: DWord;
    FCurrentSendSize: Byte;
    FDataStream: TMemoryStream;
    FForceOptionalSpaceByte: Boolean;
    FMaxAddress: DWord;
    FMinAddress: DWord;
    FWritingToAddress: Boolean;
    function GetMaxPayloadSize: Byte;
  protected
    property CurrentAddress: DWord read FCurrentAddress write FCurrentAddress;
    property CurrentSendSize: Byte read FCurrentSendSize write FCurrentSendSize;
    property MaxPayloadSize: Byte read GetMaxPayloadSize;
    property WritingToAddress: Boolean read FWritingToAddress write FWritingToAddress;
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

  { TReadAddressSpaceMemoryTask }

  TReadAddressSpaceMemoryTask = class(TBaseAddressSpaceMemoryTask)
  end;

  { TWriteAddressSpaceMemoryTask }

  TWriteAddressSpaceMemoryTask = class(TBaseAddressSpaceMemoryTask)
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; StartAsSending: Boolean; AnAddressSpace: Byte; AStream: TStream); reintroduce;
  end;

  { TWriteAddressSpaceMemoryRawTask }

  TWriteAddressSpaceMemoryRawTask = class(TOlcbTaskBase)
  private
    FAddressSpace: Byte;
    FForceOptionalSpaceByte: Boolean;
    FStream: TMemoryStream;
    FWriteAddress: DWord;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; StartAsSending: Boolean; AnAddressSpace: Byte; AWriteAddress: DWord; AStream: TStream); reintroduce;
    destructor Destroy; override;
    procedure Process(MessageInfo: TOlcbMessage); override;

    property AddressSpace: Byte read FAddressSpace;
    property ForceOptionalSpaceByte: Boolean read FForceOptionalSpaceByte write FForceOptionalSpaceByte;
    property Stream: TMemoryStream read FStream;
    property WriteAddress: DWord read FWriteAddress write FWriteAddress;
  end;

  { TReadAddressSpaceMemoryRawTask }

  TReadAddressSpaceMemoryRawTask = class(TOlcbTaskBase)
  private
    FAddressSpace: Byte;
    FCurrentOffset: DWord;
    FForceOptionalSpaceByte: Boolean;
    FIncludeTerminator: Boolean;
    FReadByteCount: DWord;
    FStream: TMemoryStream;
    FReadAddress: DWord;
    FTerminator: Char;
    FUsingTerminator: Boolean;
    function GetPayloadSize: Integer;
  protected
    property CurrentOffset: DWord read FCurrentOffset write FCurrentOffset;
    property PayloadSize: Integer read GetPayloadSize;
    property UsingTerminator: Boolean read FUsingTerminator write FUsingTerminator;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; StartAsSending: Boolean; AnAddressSpace: Byte; AReadAddress, AReadByteCount: DWord; UseTerminatorChar: Boolean); reintroduce;
    destructor Destroy; override;
    procedure Process(MessageInfo: TOlcbMessage); override;

    property AddressSpace: Byte read FAddressSpace;
    property ForceOptionalSpaceByte: Boolean read FForceOptionalSpaceByte write FForceOptionalSpaceByte;
    property IncludeTerminator: Boolean read FIncludeTerminator write FIncludeTerminator;  // Include the terminator in the Stream result
    property Stream: TMemoryStream read FStream;
    property ReadAddress: DWord read FReadAddress;
    property ReadByteCount: DWord read FReadByteCount;
    property Terminator: Char read FTerminator write FTerminator;
  end;

  { TIdentifyEventsTask }

  TIdentifyEventsTask = class(TOlcbTaskBase)
  public
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TIdentifyEventsAddressedTask }

  TIdentifyEventsAddressedTask = class(TOlcbTaskBase)
  public
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TIdentifyProducerTask }

  TIdentifyProducerTask = class(TOlcbTaskBase)
  private
    FEvent: TEventID;
  protected
    property Event: TEventID read FEvent write FEvent;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; StartAsSending: Boolean; AnEvent: TEventID); reintroduce;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TIdentifyConsumerTask }

  TIdentifyConsumerTask = class(TOlcbTaskBase)
  private
    FEvent: TEventID;
  protected
    property Event: TEventID read FEvent write FEvent;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; StartAsSending: Boolean; AnEvent: TEventID); reintroduce;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TCANLayerTask }
  TCANLayerTask = class(TOlcbTaskBase)
  public
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TEventTask }

  TEventTask = class(TOlcbTaskBase)
  public
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TVerifiedNodeIDTask }

  TVerifiedNodeIDTask = class(TOlcbTaskBase)
  public
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TTractionProtocolTask }

  TTractionProtocolTask = class(TOlcbTaskBase)
  public
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TInitializationCompleteTask }

  TInitializationCompleteTask = class(TOlcbTaskBase)
  public
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TTractionAllocateDccProxyTask }

  TTractionAllocateDccProxyTask = class(TOlcbTaskBase)
  private
    FAddress: Word;
    FIsShort: Boolean;
    FSpeedStep: Byte;
  protected
    property Address: Word read FAddress write FAddress;
    property IsShort: Boolean read FIsShort write FIsShort;
    property SpeedStep: Byte read FSpeedStep write FSpeedStep;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; StartAsSending: Boolean; AnAddress: Word; IsShortAddress: Boolean; ASpeedStep: Byte); reintroduce;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TTractionDeAllocateDccProxyTask }

  TTractionDeAllocateDccProxyTask = class(TOlcbTaskBase)
  public
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TTractionQueryDccAddressProxyTask }

  TTractionQueryDccAddressProxyTask = class(TOlcbTaskBase)
  private
    FAddress: Word;
    FIsShort: Boolean;
  protected
    property Address: Word read FAddress write FAddress;
    property IsShort: Boolean read FIsShort write FIsShort;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; StartAsSending: Boolean; AnAddress: Word; IsShortAddress: Boolean); reintroduce;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TTractionSpeedTask }

  TTractionSpeedTask = class(TOlcbTaskBase)
  private
    FEStop: Boolean;
    FSpeed: THalfFloat;
  protected
    property Speed: THalfFloat read FSpeed write FSpeed;  // Dir is wrapped up in the neg sign
    property EStop: Boolean read FEStop write FEStop;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; StartAsSending: Boolean; ASpeed: THalfFloat; IsEStop: Boolean); reintroduce;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

  { TTractionFunctionTask }

  TTractionFunctionTask = class(TOlcbTaskBase)
  private
    FAddress: DWord;
    FWord: Word;
  protected
    property Address: DWord read FAddress write FAddress;
    property Value: Word read FWord write FWord;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word; StartAsSending: Boolean; AnAddress: DWord; AValue: Word); reintroduce;
    procedure Process(MessageInfo: TOlcbMessage); override;
  end;

implementation

{ TReadAddressSpaceMemoryRawTask }

function TReadAddressSpaceMemoryRawTask.GetPayloadSize: Integer;
begin
  if ReadByteCount - Stream.Size < MAX_CONFIG_MEM_READWRITE_SIZE then
    Result := ReadByteCount - Stream.Size
  else
    Result := MAX_CONFIG_MEM_READWRITE_SIZE;
end;

constructor TReadAddressSpaceMemoryRawTask.Create(ASourceAlias, ADestinationAlias: Word; StartAsSending: Boolean; AnAddressSpace: Byte; AReadAddress, AReadByteCount: DWord; UseTerminatorChar: Boolean);
begin
  inherited Create(ASourceAlias, ADestinationAlias, StartAsSending);
  FStream := TMemoryStream.Create;
  FAddressSpace := AnAddressSpace;
  FForceOptionalSpaceByte := False;
  FReadAddress := AReadAddress;
  FReadByteCount := AReadByteCount;
  FCurrentOffset := 0;
  FUsingTerminator := UseTerminatorChar;
  FIncludeTerminator := True;
  FTerminator := #0;
end;

destructor TReadAddressSpaceMemoryRawTask.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

procedure TReadAddressSpaceMemoryRawTask.Process(MessageInfo: TOlcbMessage);
var
  DatagramReceive: TDatagramReceive;
  i: Integer;
  Finished: Boolean;
begin
  case iState of
    0: begin
        // Ask for a read from the node
         SendMemoryConfigurationRead(AddressSpace, CurrentOffset, PayloadSize, ForceOptionalSpaceByte);
         Sending := False;
         Inc(FiState);
       end;
    1: begin
         // Node received the datagram
         if IsDatagramAckFromDestination(MessageInfo) then
         begin
           Sending := False;
           Inc(FiState);
         end;
       end;
    2: begin
          // Node sending frame of data
          DatagramReceive := nil;
          if IsConfigMemoryReadReplyFromDestination(MessageInfo, DatagramReceive) then
          begin
            Finished := False;
            if ForceOptionalSpaceByte or (DatagramReceive.RawDatagram[1] and $03 = 0) then    // If using the {Space} byte need to skip over it
              i := 7
            else
              i := 6;
            while not Finished and (i < DatagramReceive.CurrentPos) do
            begin
              if UsingTerminator then
              begin
                if DatagramReceive.RawDatagram[i] = Ord( Terminator) then
                begin
                  if IncludeTerminator then
                    Stream.WriteByte( DatagramReceive.RawDatagram[i]);
                  Finished := True;
                end else
                  Stream.WriteByte( DatagramReceive.RawDatagram[i]);
              end else
                Stream.WriteByte( DatagramReceive.RawDatagram[i]);
              Inc(i);
            end;

            if Finished or (PayloadSize = 0) then
            begin
              Sending := True;
              iState := 3;
            end else
            begin
              CurrentOffset := CurrentOffset + MAX_CONFIG_MEM_READWRITE_SIZE;
              Sending := True;
              iState := 0;
            end;
          end
        end;
    3 : begin
       // Done
         FDone := True
       end;
  end;

end;

{ TWriteAddressSpaceMemoryRawTask }

constructor TWriteAddressSpaceMemoryRawTask.Create(ASourceAlias, ADestinationAlias: Word; StartAsSending: Boolean; AnAddressSpace: Byte; AWriteAddress: DWord; AStream: TStream);
begin
  inherited Create(ASourceAlias, ADestinationAlias, StartAsSending);
  FStream := TMemoryStream.Create;
  if Assigned(AStream) then
  begin
    AStream.Position := 0;
    Stream.CopyFrom(AStream, AStream.Size);
    Stream.Position := 0;
  end;
  FAddressSpace := AnAddressSpace;
  FForceOptionalSpaceByte := False;
  FWriteAddress := AWriteAddress;
end;

destructor TWriteAddressSpaceMemoryRawTask.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

procedure TWriteAddressSpaceMemoryRawTask.Process(MessageInfo: TOlcbMessage);
begin
  case iState of
    0 : begin
          SendMemoryConfigurationWrite(AddressSpace, WriteAddress, $FFFFFFFF, ForceOptionalSpaceByte, Stream);
          iState := 1
        end;
    1 : begin
       // Done
         FDone := True
       end;
  end;
end;

{ TWriteAddressSpaceMemoryTask }

constructor TWriteAddressSpaceMemoryTask.Create(ASourceAlias, ADestinationAlias: Word; StartAsSending: Boolean; AnAddressSpace: Byte; AStream: TStream);
begin
  inherited Create(ASourceAlias, ADestinationAlias, StartAsSending, AnAddressSpace);
  DataStream.CopyFrom(AStream, AStream.Size);
  FWritingToAddress := True;
end;

{ TIdentifyConsumerTask }

constructor TIdentifyConsumerTask.Create(ASourceAlias, ADestinationAlias: Word;
  StartAsSending: Boolean; AnEvent: TEventID);
begin
  inherited Create(ASourceAlias, ADestinationAlias, StartAsSending);
  FEvent := AnEvent;
end;

procedure TIdentifyConsumerTask.Process(MessageInfo: TOlcbMessage);
begin
  case iState of
    0: begin
         SendIdentifyConsumerMessage(Event);
         Inc(FiState);
       end;
    1: begin
         FDone := True;
       end;
  end;
end;

{ TIdentifyProducerTask }

constructor TIdentifyProducerTask.Create(ASourceAlias, ADestinationAlias: Word;
  StartAsSending: Boolean; AnEvent: TEventID);
begin
  inherited Create(ASourceAlias, ADestinationAlias, StartAsSending);
  FEvent := AnEvent;
end;

procedure TIdentifyProducerTask.Process(MessageInfo: TOlcbMessage);
begin
   case iState of
    0: begin
         SendIdentifyProducerMessage(Event);
         Inc(FiState);
       end;
    1: begin
         FDone := True;
       end;
  end;
end;

{ TTractionAllocateDccProxyTask }

constructor TTractionAllocateDccProxyTask.Create(ASourceAlias, ADestinationAlias: Word; StartAsSending: Boolean; AnAddress: Word; IsShortAddress: Boolean; ASpeedStep: Byte);
begin
  inherited Create(ASourceAlias, ADestinationAlias, StartAsSending);
  FSpeedStep := ASpeedStep;
  FAddress := AnAddress;
  FIsShort := IsShortAddress;
end;

procedure TTractionAllocateDccProxyTask.Process(MessageInfo: TOlcbMessage);
begin
  case iState of
    0: begin
         SendTractionAllocateDccProxyMessage(Address, IsShort, SpeedStep);
         Inc(FiState);
       end;
    1: begin
         FDone := True;
       end;
  end;
end;

{ TTractionDeAllocateDccProxyTask }

procedure TTractionDeAllocateDccProxyTask.Process(MessageInfo: TOlcbMessage);
begin
  case iState of
    0: begin
         SendTractionDeAllocateDccAddressProxyMessage;
         Inc(FiState);
       end;
    1: begin
         FDone := True;
       end;
  end;
end;

{ TTractionQueryDccAddressProxyTask }

constructor TTractionQueryDccAddressProxyTask.Create(ASourceAlias,
  ADestinationAlias: Word; StartAsSending: Boolean; AnAddress: Word;
  IsShortAddress: Boolean);
begin
  inherited Create(ASourceAlias, ADestinationAlias, StartAsSending);
  FAddress := AnAddress;
  FIsShort := IsShortAddress;
end;

procedure TTractionQueryDccAddressProxyTask.Process(MessageInfo: TOlcbMessage);
begin
  case iState of
    0: begin
         SendTractionQueryDccAddressProxyMessage(Address, IsShort);
         Inc(FiState);
       end;
    1: begin
         FDone := True;
       end;
  end;
end;

{ TTractionFunctionTask }

constructor TTractionFunctionTask.Create(ASourceAlias, ADestinationAlias: Word;
  StartAsSending: Boolean; AnAddress: DWord; AValue: Word);
begin
  inherited Create(ASourceAlias, ADestinationAlias, StartAsSending);
  Address := AnAddress;
  Value := AValue;
end;

procedure TTractionFunctionTask.Process(MessageInfo: TOlcbMessage);
begin
  case iState of
    0: begin
         SendTractionFunction(Address, Value);
         Inc(FiState);
       end;
    1: begin
         FDone := True;
       end;
  end;
end;

{ TTractionSpeedTask }

constructor TTractionSpeedTask.Create(ASourceAlias, ADestinationAlias: Word;
  StartAsSending: Boolean; ASpeed: THalfFloat; IsEStop: Boolean);
begin
  inherited Create(ASourceAlias, ADestinationAlias, StartAsSending);
  FSpeed := ASpeed;
  FEStop := IsEStop;
end;

procedure TTractionSpeedTask.Process(MessageInfo: TOlcbMessage);
begin
  case iState of
    0: begin
         if EStop then
           SendTractionEStopMessage
         else begin
           SendTractionSpeedMessage(Speed)
         end;
         Inc(FiState);
       end;
    1: begin
         FDone := True;
       end;
  end;
end;


{ TIdentifyEventsAddressedTask }

procedure TIdentifyEventsAddressedTask.Process(MessageInfo: TOlcbMessage);
begin
  case iState of
    0: begin
         SendIdentifyEventsAddressedMessage;
         Inc(FiState);
       end;
    1: begin
         FDone := True;
       end;
  end;
end;

{ TIdentifyEventsTask }

procedure TIdentifyEventsTask.Process(MessageInfo: TOlcbMessage);
begin
  case iState of
    0: begin
         SendIdentifyEventsMessage;
         Inc(FiState);
       end;
    1: begin
         FDone := True;
       end;
  end;
end;

{ TVerifiedNodeIDTask }

procedure TVerifiedNodeIDTask.Process(MessageInfo: TOlcbMessage);
begin
  FDone := True;  // Done before we start....
end;

{ TTractionProtocolTask }

procedure TTractionProtocolTask.Process(MessageInfo: TOlcbMessage);
begin
  FDone := True;  // Done before we start....
end;

{ TInitializationCompleteTask }

procedure TInitializationCompleteTask.Process(MessageInfo: TOlcbMessage);
begin
  FDone := True;  // Done before we start....
end;

{ TEventTask }

procedure TEventTask.Process(MessageInfo: TOlcbMessage);
begin
  FDone := True;  // Done before we start....
end;

{ TCANLayerTask }

procedure TCANLayerTask.Process(MessageInfo: TOlcbMessage);
begin
  FDone := True;  // Done before we start....
end;

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
         if IsProtocolIdentificationProcolReplyFromDestination(MessageInfo) then
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
         DatagramReceive := nil;
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
         DatagramReceive := nil;
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

{ TBaseAddressSpaceMemoryTask }

function TBaseAddressSpaceMemoryTask.GetMaxPayloadSize: Byte;
begin
  Result := MAX_CONFIG_MEM_READWRITE_SIZE;
end;

constructor TBaseAddressSpaceMemoryTask.Create(ASourceAlias, ADestinationAlias: Word; StartAsSending: Boolean; AnAddressSpace: Byte);
begin
  inherited Create(ASourceAlias, ADestinationAlias, StartAsSending);
  FAddressSpace := AnAddressSpace;
  ForceOptionalSpaceByte := False;
  FWritingToAddress := False;
  FDataStream := TMemoryStream.Create;
end;

destructor TBaseAddressSpaceMemoryTask.Destroy;
begin
  FreeAndNil(FDataStream);
  inherited Destroy;
end;

procedure TBaseAddressSpaceMemoryTask.Process(MessageInfo: TOlcbMessage);
// Outline:
//    Read Protocols to see if the Memory Protocol is supported
//    Read Memory Protocol Options to read the Min/Max supported Spaces to see if $FF is supported
//    Read Memory Protocol Space Info to read size of the CDI Space
//    Read Memory Protocol at Space $FF until done
//

const
  STATE_DONE = 100;
  STATE_READ_START = 8;
  STATE_WRITE_START = 20;
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
         // Ask for the protocols the node supports
         SendProtocolIdentificationProtocolMessage;
         Inc(FiState);
         Sending := False;
       end;
    1: begin
         // First see if the node even supports the Memory Configuration Protocol
         if IsProtocolIdentificationProcolReplyFromDestination(MessageInfo) then
         begin
           PIP := TOlcbProtocolIdentification.Create;
           try
             PIP.LoadByMessage( TOpenLCBMessageHelper( MessageInfo));   // Already know that MessageInfo is a TOpenLCBMessageHelper by this point
             if PIP.MemoryConfigProtocol then
             begin
               Sending := True;
               Inc(FiState);
             end else
             begin
               if not PIP.MemoryConfigProtocol then
                 ErrorCode := ErrorCode or ERROR_NO_MEMORY_CONFIG_PROTOCOL;
               Sending := True;
               iState := STATE_DONE;
             end
           finally
             FreeAndNil(PIP)
           end
         end
       end;
    2: begin
         // Ask for what Address Spaces the node supports
         SendMemoryConfigurationOptions;
         Inc(FiState);
         Sending := False;
       end;
    3: begin
         // Node received the request datagram
         if IsDatagramAckFromDestination(MessageInfo) then
         begin
           Sending := False;
           Inc(FiState);
         end;
       end;
    4: begin
         // Is the address space we are asking for supported?
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
        // Ask for details about the address space we are interested in
         SendMemoryConfigurationSpaceInfo(AddressSpace);
         Sending := False;
         Inc(FiState);
       end;
    6: begin
        // Node received the datagram
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
               if (WritingToAddress and Space.IsReadOnly) then
               begin
                 ErrorCode := ErrorCode or ERROR_ADDRESS_SPACE_NOT_PRESENT;
                 Sending := True;
                 iState := STATE_DONE;
               end else
               begin
                 FMinAddress := Space.AddressLo;
                 FMaxAddress := Space.AddressHi;
                 CurrentAddress := MinAddress;
                 DataStream.Position := 0;
                 Sending := True;
                 if WritingToAddress then
                   FiState := STATE_WRITE_START
                 else
                   FiState := STATE_READ_START;
               end
             end else
             begin
               ErrorCode := ErrorCode or ERROR_ADDRESS_SPACE_NOT_PRESENT;
               Sending := True;
               iState := STATE_DONE;
             end
           finally
             FreeAndNil(Space);
           end
         end;
       end;
    STATE_READ_START : begin
         // Calculate how many bytes to read in this frame (depends on if the address space is carried in the frame or if at the end of the mem space)
         if MaxAddress - CurrentAddress > MaxPayloadSize then
            CurrentSendSize := MaxPayloadSize
          else
            CurrentSendSize := MaxAddress - CurrentAddress;
         Sending := True;
         Inc(FiState);
       end;
    9: begin
        // Ask for a read from the node
         SendMemoryConfigurationRead(AddressSpace, CurrentAddress, CurrentSendSize, ForceOptionalSpaceByte);
         Sending := False;
         Inc(FiState);
       end;
    10: begin
         // Node received the datagram
         if IsDatagramAckFromDestination(MessageInfo) then
         begin
           Sending := False;
           Inc(FiState);
         end;
       end;
    11: begin
          // Node sending frame of data
          DatagramReceive := nil;
          if IsConfigMemoryReadReplyFromDestination(MessageInfo, DatagramReceive) then
          begin
            if ForceOptionalSpaceByte or (DatagramReceive.RawDatagram[1] and $03 = 0) then    // If using the {Space} byte need to skip over it
              DatagramResultStart := 7
            else
              DatagramResultStart := 6;
            for i := DatagramResultStart to DatagramReceive.CurrentPos - 1 do
              DataStream.WriteByte( DatagramReceive.RawDatagram[i]);
            CurrentAddress := CurrentAddress + DWord( (DatagramReceive.CurrentPos - DatagramResultStart));
            if CurrentAddress = MaxAddress then
            begin
              Sending := True;
              iState := STATE_DONE;
            end else
            begin
              Sending := True;
              iState := STATE_READ_START;
            end;
          end
        end;
    STATE_WRITE_START :
        begin
          if DataStream.Size > Space.AddressHi - Space.AddressLo then
            ErrorCode := ERROR_ADDRESS_SPACE_WRITE_LARGER_THAN_SPACE
          else
            SendMemoryConfigurationWrite(AddressSpace, CurrentAddress, Space.AddressHi - Space.AddressLo, ForceOptionalSpaceByte, DataStream);
          iState := STATE_DONE
        end;
    STATE_DONE : begin
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
  FConfigMemAddressInfo := TOlcbMemConfig.Create;
end;

destructor TEnumAllConfigMemoryAddressSpaceInfoTask.Destroy;
begin
  FreeAndNil(FConfigMemAddressInfo);
  inherited Destroy;
end;

procedure TEnumAllConfigMemoryAddressSpaceInfoTask.Process(MessageInfo: TOlcbMessage);
var
  DatagramReceive: TDatagramReceive;
  NewSpace: TOlcbMemAddressSpace;
begin
  case iState of
    0: begin
         SendMemoryConfigurationOptions;
         Inc(FiState);
         Sending := False;
       end;
    1: begin
         if IsDatagramAckFromDestination(MessageInfo) then
         begin
           Sending := False;
           Inc(FiState);
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
           Inc(FiState);
         end;
       end;
    3: begin
         SendMemoryConfigurationSpaceInfo(CurrentAddressSpace);
         Sending := False;
         Inc(FiState);
       end;
    4: begin
         if IsDatagramAckFromDestination(MessageInfo) then
         begin
           Sending := False;
           Inc(FiState);
         end;
       end;
    5: begin
         DatagramReceive := nil;
         if IsConfigMemorySpaceInfoReplyFromDestination(MessageInfo, CurrentAddressSpace, DatagramReceive) then
         begin
           NewSpace := ConfigMemAddressInfo.AddAddressSpace;
           NewSpace.LoadByDatagram(DatagramReceive);
           Dec(FCurrentAddressSpace);
           if CurrentAddressSpace < MinAddressSpace then
           begin
             Sending := True;
             iState := 6;
           end else
           begin
             Sending := True;
             iState := 3;
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

