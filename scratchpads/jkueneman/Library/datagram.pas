unit datagram;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, olcb_utilities, olcb_defines, serialport_thread;


const
  DATAGRAM_MAX_RETRYS = 5;

  HEADER_MEMCONFIG_OPTIONS_REQUEST: TCANByteArray = ($20, $80, $00, $00, $00, $00, $00, $00);

type

{ TDatagramReceive }

  TDatagramReceive = class
  private
    FDestinationAlias: Word;
    FLocalHelper: TOpenLCBMessageHelper;
    FRawDatagram: TDatagramArray;
    FFull: Boolean;
    FCurrentPos: Byte;
    FSourceAlias: Word;
  protected
    procedure Clear;
    procedure SendACK(ComPortThread: TComPortThread);
    property LocalHelper: TOpenLCBMessageHelper read FLocalHelper write FLocalHelper;   // Global object to work with OLCB messages
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word);
    destructor Destroy; override;
    function ExtractDataBytesAsInt(StartByteIndex, EndByteIndex: Integer): QWord;       // Helpers to pull information out of the datagram byte array
    function ExtractDataBytesAsString(StartIndex, Count: Integer): String;              // Helpers to pull information out of the datagram byte array
    procedure CopyToStream(Stream: TStream; StartIndex, Count: Integer);                // Helpers to move datagram bytes to a stream
    procedure Process(AHelper: TOpenLCBMessageHelper; ComPortThread: TComPortThread);   // Processes the message/
    property CurrentPos: Byte read FCurrentPos write FCurrentPos;                       // Running count of the number of bytes being received, once "Full" this is the number of byte in the datagram
    property DestinationAlias: Word read FDestinationAlias write FDestinationAlias;     // Node who is sending the Datagram to this object
    property RawDatagram: TDatagramArray read FRawDatagram write FRawDatagram;          // Raw datagram bytes received
    property Full: Boolean read FFull write FFull;                                      // Raw datagram bytes with a size of CurrentPos is valid
    property SourceAlias: Word read FSourceAlias write FSourceAlias;                    // Node who is looking for Datagrams assigned to it to come in
  end;


  { TDatagramReceiveManager }

  TDatagramReceiveManager = class
  private
    FDatagrams: TList;
    FSourceAlias: Word;
    function GetDatagram(Index: Integer): TDatagramReceive;
  protected
    function FindInProcessDatagramByAlias(DestinationAlias: Word): TDatagramReceive;
    property Datagrams: TList read FDatagrams write FDatagrams;
    property SourceAlias: Word read FSourceAlias write FSourceAlias;            // Alias of the receiver for the datagrams
  public
    constructor Create(ASourceAlias: Word);
    destructor Destroy; override;
    procedure Clear;
    function Process(AHelper: TOpenLCBMessageHelper; ComPortThread: TComPortThread): TDatagramReceive;
    property Datagram[Index: Integer]: TDatagramReceive read GetDatagram;       // Inprocess and completed Datagrams, the order they are received is preserved
  end;

  { TDatagramSend }

  TDatagramSend = class
  private
    FAbandonTime: Integer;
    FCurrentPos: Byte;
    FDataBytesSent: TCANByteArray;
    FDataBytesSentLen: Byte;
    FDestinationAlias: Word;
    FEmpty: Boolean;
    FErrorCode: Word;
    FLocalHelper: TOpenLCBMessageHelper;
    FMTI: DWord;
    FProtocolHeader: TCANByteArray;
    FProtocolHeaderLen: Byte;
    FRetryCount: Byte;
    FSourceAlias: Word;
    FStream: TMemoryStream;
    FWaitingForACK: Boolean;
  protected
    procedure StreamBytesToByteArray(Offset: Byte; var ByteArray: TCANByteArray; var Count: Byte);
    property AbandonTime: Integer read FAbandonTime write FAbandonTime;
    property CurrentPos: Byte read FCurrentPos write FCurrentPos;
    property LocalHelper: TOpenLCBMessageHelper read FLocalHelper write FLocalHelper;
    property ProtocolHeader: TCANByteArray read FProtocolHeader write FProtocolHeader;
    property ProtocolHeaderLen: Byte read FProtocolHeaderLen write FProtocolHeaderLen;
    property DataBytesSent: TCANByteArray read FDataBytesSent write FDataBytesSent;
    property DataBytesSentLen: Byte read FDataBytesSentLen write FDataBytesSentLen;
    property MTI: DWord read FMTI write FMTI;
    property RetryCount: Byte read FRetryCount write FRetryCount;
    property Stream: TMemoryStream read FStream write FStream;
    property WaitingForACK: Boolean read FWaitingForACK write FWaitingForACK;             // After a frame is sent need to wait
  public
    constructor Create;
    destructor Destroy; override;
    function Initialize(AStream: TStream; AProtocolHeader: TCANByteArray; AProtocolHeaderLen: Byte; ASourceAlias, ADestinationAlias: Word; ComPortThread: TComPortThread): Boolean;
    procedure ProcessSend(ComPortThread: TComPortThread);
    procedure ProcessReceive(AHelper: TOpenLCBMessageHelper; ComPortThread: TComPortThread);
    property DestinationAlias: Word read FDestinationAlias write FDestinationAlias;
    property Empty: Boolean read FEmpty write FEmpty;
    property ErrorCode: Word read FErrorCode write FErrorCode;    // One of the DATAGRAM_REJECTED_xxx constants, if $0000 then no error
    property SourceAlias: Word read FSourceAlias write FSourceAlias;
  end;

  { TDatagramSendManager }

  TDatagramSendManager = class
  private
    FAbandonDatagrams: TList;
    FDatagrams: TList;
    FSourceAlias: Word;
    FTimer: TTimer;
    function GetAbandanDatagram(Index: Integer): TDatagramSend;
    function GetAbandonDatagramCount: Integer;
    function GetDatagram(Index: Integer): TDatagramSend;
  protected
    function FindInProcessDatagram: TDatagramSend;
    procedure TimerTick(Sender: TObject);
    property AbandonDatagrams: TList read FAbandonDatagrams write FAbandonDatagrams;
    property Datagrams: TList read FDatagrams write FDatagrams;
    property SourceAlias: Word read FSourceAlias write FSourceAlias;            // Alias of the node that is SENDING the datagrams
    property Timer: TTimer read FTimer write FTimer;
  public
    constructor Create(ASourceAlias: Word);
    destructor Destroy; override;
    procedure Clear;
    procedure ClearAbandon;
    function CreateDatagram: TDatagramSend;
    function ProcessReceive(AHelper: TOpenLCBMessageHelper; AComPortThread: TComPortThread): TDatagramSend;
    procedure ProcessSend(AComPortThread: TComPortThread);
    property AbandonDatagramCount: Integer read GetAbandonDatagramCount;
    property AbandonDatagram[Index: Integer]: TDatagramSend read GetAbandanDatagram;
    property Datagram[Index: Integer]: TDatagramSend read GetDatagram;       // Inprocess and completed Datagrams, the order they are received is preserved
  end;


implementation

{ TDatagramSendManager }

function TDatagramSendManager.GetDatagram(Index: Integer): TDatagramSend;
begin
  if (Index > -1) and (Index < Datagrams.Count) then
    Result := TDatagramSend( Datagrams[Index])
  else
    Result := nil;
end;

function TDatagramSendManager.GetAbandanDatagram(Index: Integer): TDatagramSend;
begin
  if (Index > -1) and (Index < AbandonDatagrams.Count) then
    Result := TDatagramSend( AbandonDatagrams[Index])
  else
    Result := nil;
end;

function TDatagramSendManager.GetAbandonDatagramCount: Integer;
begin
  AbandonDatagrams.Count
end;

function TDatagramSendManager.FindInProcessDatagram: TDatagramSend;
var
  i: Integer;
begin
  i := 0;
  Result := nil;
  while not Assigned(Result) and (i < Datagrams.Count) do
  begin
    if not Datagram[i].Empty then
      Result := Datagram[i];
    Inc(i)
  end;
end;

procedure TDatagramSendManager.TimerTick(Sender: TObject);
var
  SendDatagram: TDatagramSend;
  i: Integer;
begin
  for i := Datagrams.Count - 1 downto 0 do     // May remove the item so need to go from the top down
  begin
    SendDatagram := Datagram[i];
    if not SendDatagram.Empty then
    begin
      if SendDatagram.AbandonTime > 4000 then
      begin
        Datagrams.Remove(SendDatagram);
        AbandonDatagrams.Add(SendDatagram);
      end;
      SendDatagram.AbandonTime := SendDatagram.AbandonTime + Timer.Interval;
    end;
  end;
end;

constructor TDatagramSendManager.Create(ASourceAlias: Word);
begin
  inherited Create;
  FSourceAlias := ASourceAlias;
  FDatagrams := TList.Create;
  FAbandonDatagrams := TList.Create;
  Timer := TTimer.Create(nil);
  Timer.Interval := 500;         // Every 500m seconds
  Timer.Enabled := False;
end;

destructor TDatagramSendManager.Destroy;
begin
  Clear;
  FreeAndNil(FDatagrams);
  ClearAbandon;
  FreeAndNil(FAbandonDatagrams);
  FreeAndNil(FTimer);
  inherited Destroy;
end;

procedure TDatagramSendManager.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to Datagrams.Count - 1 do
      TObject( Datagrams[i]).Free;
  finally
    Datagrams.Clear;
  end;
end;

procedure TDatagramSendManager.ClearAbandon;
var
  i: Integer;
begin
  try
    for i := 0 to AbandonDatagrams.Count - 1 do
      TObject( AbandonDatagrams[i]).Free;
  finally
    AbandonDatagrams.Clear;
  end;
end;

function TDatagramSendManager.CreateDatagram: TDatagramSend;
begin
  Result := TDatagramSend.Create;
  Datagrams.Add(Result);
end;

function TDatagramSendManager.ProcessReceive(AHelper: TOpenLCBMessageHelper; AComPortThread: TComPortThread): TDatagramSend;
var
  TestDatagram: TDatagramSend;
begin
  Result := nil;
  TestDatagram := FindInProcessDatagram;
  if Assigned(TestDatagram) then
  begin
    TestDatagram.ProcessReceive(AHelper, AComPortThread);
    if TestDatagram.Empty then
    begin
      Datagrams.Remove(TestDatagram);
      Result := TestDatagram;
    end;
  end;
end;

procedure TDatagramSendManager.ProcessSend(AComPortThread: TComPortThread);
var
  TestDatagram: TDatagramSend;
begin
  TestDatagram := FindInProcessDatagram;
  if Assigned(TestDatagram) then
    TestDatagram.ProcessSend(AComPortThread);   // Keep sending until it is time to wait for the node to respond with an ACK or NAK
 end;

{ TDatagramReceiveManager }

function TDatagramReceiveManager.GetDatagram(Index: Integer): TDatagramReceive;
begin
  if (Index > -1) and (Index < Datagrams.Count) then
    Result := TDatagramReceive( Datagrams[Index])
  else
    Result := nil;
end;

function TDatagramReceiveManager.FindInProcessDatagramByAlias(DestinationAlias: Word): TDatagramReceive;
var
  i: Integer;
begin
  i := 0;
  Result := nil;
  while not Assigned(Result) and (i < Datagrams.Count) do
  begin
    if (Datagram[i].DestinationAlias = DestinationAlias) and not Datagram[i].Full then
      Result := Datagram[i];
    Inc(i)
  end;
end;

constructor TDatagramReceiveManager.Create(ASourceAlias: Word);
begin
  inherited Create;
  FSourceAlias := ASourceAlias;
  FDatagrams := TList.Create;
end;

destructor TDatagramReceiveManager.Destroy;
begin
  Clear;
  FreeAndNil(FDatagrams);
  inherited Destroy;
end;

procedure TDatagramReceiveManager.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to Datagrams.Count - 1 do
      TObject( Datagrams[i]).Free;
  finally
    Datagrams.Clear;
  end;
end;

function TDatagramReceiveManager.Process(AHelper: TOpenLCBMessageHelper; ComPortThread: TComPortThread): TDatagramReceive;
var
  TestDatagram: TDatagramReceive;
begin
  TestDatagram := nil;
  if IsDatagramMTI(AHelper.MTI) then
  begin
    TestDatagram := FindInProcessDatagramByAlias(AHelper.SourceAliasID);
    if not Assigned(TestDatagram) then
    begin
      TestDatagram := TDatagramReceive.Create(SourceAlias, AHelper.SourceAliasID);  // Create a new receiving Datagram object for source alias of the message to us
      Datagrams.Add(TestDatagram);
    end;
    TestDatagram.Process(AHelper, ComPortThread);
    if TestDatagram.Full then
    begin
      Datagrams.Remove(TestDatagram);
      Result := TestDatagram
    end;
  end;
end;

{ TDatagramSend }

// *****************************************************************************
// Pulls the next 1..8 byte(s) from the Stream and puts them in the CAN Array to send, it always assumes the Stream Position of were to start
// in the Stream
//   Offset: [in] Where in the CAN array to use as the start of the transfer (may have some header bytes that need to be skipped)
//   ByteArray:  [out] CAN ByteArray that will be sent on the wire
//   Count    :  [out] The number of bytes that were transfered to the Byte Array
// *****************************************************************************
procedure TDatagramSend.StreamBytesToByteArray(Offset: Byte; var ByteArray: TCANByteArray; var Count: Byte);
begin
  Count := 0;
  // Tests for breaking out of loop:
  //   The Byte Array has 8 valid bytes (A)
  //   Finished moving all of the Stream Bytes (B)
  while (Offset < 8) {A} and (Stream.Position < Stream.Size) {B} do
  begin
    ByteArray[Offset] := Stream.ReadByte;
    Inc(Count);
    Inc(FCurrentPos);
    Inc(Offset);
  end;
end;

// *****************************************************************************
// Creates the datagram object
// *****************************************************************************
constructor TDatagramSend.Create;
begin
  inherited;
  FStream := TMemoryStream.Create;
  FLocalHelper := TOpenLCBMessageHelper.Create;
end;

// *****************************************************************************
// Destroys the datagram object
// *****************************************************************************
destructor TDatagramSend.Destroy;
begin
  FreeAndNil(FStream);
  FreeAndNil(FLocalHelper);
  inherited;
end;

// *****************************************************************************
// Loads and initalizes the object to start sending the datagram
// *****************************************************************************
function TDatagramSend.Initialize(AStream: TStream; AProtocolHeader: TCANByteArray; AProtocolHeaderLen: Byte; ASourceAlias, ADestinationAlias: Word; ComPortThread: TComPortThread): Boolean;
begin
  Result := False;
  {$B-}
  if not Assigned(AStream) or (AProtocolHeaderLen + AStream.Size <= DATAGRAM_LENGTH) then
  begin
    Stream.Position := 0;
    if Assigned(AStream) then
    begin
      AStream.Position := 0;
      Stream.CopyFrom(AStream, AStream.Size);
    end;
    FProtocolHeader := AProtocolHeader;
    FProtocolHeaderLen := AProtocolHeaderLen;
    FSourceAlias := ASourceAlias;
    FDestinationAlias := ADestinationAlias;
    FCurrentPos := 0;
    FEmpty := False;
    FWaitingForACK := False;
    FErrorCode := $0000;
    FRetryCount := 0;
    FAbandonTime := 0;
    ProcessSend(ComPortThread);  // get it kicked off
    Result := True;
  end;
end;

// *****************************************************************************
// Call repeatedly in the statemachine until Empty = True
//   ComPortThread: Thread to send the messages to
// *****************************************************************************
procedure TDatagramSend.ProcessSend(ComPortThread: TComPortThread);
var
  Count: Byte;
begin
  Count := 0;
  if not Empty and ComPortThread.Connected and not WaitingForACK then
  begin
    if CurrentPos = 0 then
    begin
      // Starting a new packet, need to decide if it fits in a single frame
      if Stream.Size + ProtocolHeaderLen <= 8 then
      begin
        MTI := MTI_FRAME_TYPE_DATAGRAM_ONLY_FRAME;
        WaitingForACK := True;
      end else
        MTI := MTI_FRAME_TYPE_DATAGRAM_FRAME_START;
      DataBytesSent := ProtocolHeader;
      StreamBytesToByteArray(ProtocolHeaderLen, FDataBytesSent, Count);
      DataBytesSentLen := Count + ProtocolHeaderLen;
      LocalHelper.Load(ol_OpenLCB, MTI, SourceAlias, DestinationAlias, DataBytesSentLen, DataBytesSent[0], DataBytesSent[1], DataBytesSent[2], DataBytesSent[3], DataBytesSent[4], DataBytesSent[5], DataBytesSent[6], DataBytesSent[7]);
      ComPortThread.Add(LocalHelper.Encode);
    end else
    begin
      if (Stream.Size - Stream.Position <= 8) then
      begin
        MTI := MTI_FRAME_TYPE_DATAGRAM_FRAME_END;
        WaitingForACK := True;
      end else
        MTI := MTI_FRAME_TYPE_DATAGRAM_FRAME;
      StreamBytesToByteArray(0, FDataBytesSent, Count);
      DataBytesSentLen := Count;
      LocalHelper.Load(ol_OpenLCB, MTI, SourceAlias, DestinationAlias, DataBytesSentLen, DataBytesSent[0], DataBytesSent[1], DataBytesSent[2], DataBytesSent[3], DataBytesSent[4], DataBytesSent[5], DataBytesSent[6], DataBytesSent[7]);
    end;
  end;
end;

procedure TDatagramSend.ProcessReceive(AHelper: TOpenLCBMessageHelper; ComPortThread: TComPortThread);
begin
  if not Empty and ComPortThread.Connected and WaitingForACK then   // See if we are waiting for the node to sent us and ACK
  begin
     // See if the message is from the node we sent the datagram to
    if (AHelper.SourceAliasID = DestinationAlias) and (AHelper.DestinationAliasID = SourceAlias) then
    begin
      case AHelper.MTI of
        MTI_DATAGRAM_OK_REPLY : Empty := True;
        MTI_DATAGRAM_REJECTED_REPLY :
          begin
             ErrorCode := AHelper.ExtractDataBytesAsInt(2, 3);
             if (ErrorCode and DATAGRAM_REJECTED_RESEND_MASK <> 0) and (RetryCount < DATAGRAM_MAX_RETRYS) then
             begin
               CurrentPos := 0;                                                 // Reset the statemachine to send it all over again
               if Assigned(Stream) then
                 Stream.Position := 0;
               ErrorCode := $0000;
               WaitingForACK := False;
               Inc(FRetryCount);                                                // Kick the resend off
               ProcessSend(ComPortThread);
             end else
               Empty := True                                                    // Error; don't resend and quit with ErrorCode set
          end;
      end;
    end;
  end
end;

{ TDatagramReceive }


// *****************************************************************************
// Clears the structures and fields of the object
// *****************************************************************************
procedure TDatagramReceive.Clear;
var
  i: Integer;
begin
  FCurrentPos := 0;
  FFull := False;
  for i := 0 to DATAGRAM_LENGTH - 1 do
    RawDatagram[i] := 0;
end;

procedure TDatagramReceive.SendACK(ComPortThread: TComPortThread);
begin
  LocalHelper.Load(ol_OpenLCB, MTI_DATAGRAM_OK_REPLY, SourceAlias, DestinationAlias, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  ComPortThread.Add(LocalHelper.Encode);
end;

constructor TDatagramReceive.Create(ASourceAlias, ADestinationAlias: Word);
begin
  inherited Create;
  FLocalHelper := TOpenLCBMessageHelper.Create;
  FDestinationAlias := ADestinationAlias;
  FSourceAlias := ASourceAlias;
end;

destructor TDatagramReceive.Destroy;
begin
  FreeAndNil(FLocalHelper);
  inherited;
end;

// *****************************************************************************
// Helper to concat up to 4 bytes in the Datagram Data Array into an integer/QWord
//  StartByteIndex: The first byte to use in the result (most significant 8 bits of integer/QWord)
//  EndByteIndex  : The last byte to use in the result (least significant 8 bits of integer/QWord)
// *****************************************************************************
function TDatagramReceive.ExtractDataBytesAsInt(StartByteIndex, EndByteIndex: Integer): QWord;
var
  i, Offset, Shift: Integer;
  ByteAsQ, ShiftedByte: QWord;
begin
  Result := 0;
  Offset := EndByteIndex - StartByteIndex;
  for i := StartByteIndex to EndByteIndex do
  begin
    Shift := Offset * 8;
    ByteAsQ := QWord( RawDatagram[i]);
    ShiftedByte := ByteAsQ shl Shift;
    Result := Result or ShiftedByte;
    Dec(Offset)
  end;
end;

// *****************************************************************************
// Helper to convert a section of the Datagram Array bytes to a string
//  StartIndex: The index of the byte in the Datagram Array as the first character of the string
//  Count     : The number of character in the string (bytes)
// *****************************************************************************
function TDatagramReceive.ExtractDataBytesAsString(StartIndex, Count: Integer): String;
var
  i, Last: Integer;
begin
  Result := '';
  if Count = -1 then
    Last := CurrentPos
  else
    Last := StartIndex + Count;
  if Last > CurrentPos then
    Last := CurrentPos;

  for i := StartIndex to Last - 1 do
    Result := Result + Chr( RawDatagram[i]);
end;

// *****************************************************************************
// Helper to copy the contents of the Datagram Array to a Stream
//   Stream:  The stream to copy the bytes to
//   StartIndex: The index of the byte in the Datagram Array to copy into the stream
//   Count     : The number of bytes to copy into the stream
// *****************************************************************************
procedure TDatagramReceive.CopyToStream(Stream: TStream; StartIndex, Count: Integer);
var
  i, Last: Integer;
begin
  if Count = -1 then
    Last := CurrentPos
  else
    Last := StartIndex + Count;

  if Last > CurrentPos then
    Last := CurrentPos;

  for i := StartIndex to Last - 1 do
    Stream.WriteByte(RawDatagram[i]);
end;

// *****************************************************************************
// Call as part of the statemachine as new OLCB messages are received.
//   AHelper:  The message that was received on the OLCB wire
//
//  NOTE:  It assumes that checks for the correct Source and Destination Alias's
//         have already been made.
//
// *****************************************************************************
procedure TDatagramReceive.Process(AHelper: TOpenLCBMessageHelper; ComPortThread: TComPortThread);
var
  i: Integer;
begin
  case AHelper.MTI of
     MTI_FRAME_TYPE_DATAGRAM_ONLY_FRAME :
       begin
         Clear;
         for i := 0 to AHelper.DataCount - 1 do
           RawDatagram[i] := AHelper.Data[i];
         CurrentPos := AHelper.DataCount;
         SendACK(ComPortThread);
         Full := True
       end;
     MTI_FRAME_TYPE_DATAGRAM_FRAME_START :
       begin
         Full := False;
         for i := 0 to AHelper.DataCount - 1 do
           RawDatagram[i] := AHelper.Data[i];
         CurrentPos := AHelper.DataCount;
       end;
     MTI_FRAME_TYPE_DATAGRAM_FRAME :
       begin
         for i := 0 to AHelper.DataCount - 1 do
           RawDatagram[CurrentPos+i] := AHelper.Data[i];
         CurrentPos := CurrentPos + AHelper.DataCount;
       end;
    MTI_FRAME_TYPE_DATAGRAM_FRAME_END :
      begin
        for i := 0 to AHelper.DataCount - 1 do
          RawDatagram[CurrentPos+i] := AHelper.Data[i];
        CurrentPos := CurrentPos + AHelper.DataCount;
        SendACK(ComPortThread);
        Full := True;
      end;
  end;
end;

end.

