unit datagram;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, olcb_utilities, olcb_defines, serialport_thread;


const
  DATAGRAM_MAX_RETRYS = 5;

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
    property DestinationAlias: Word read FDestinationAlias write FDestinationAlias;     // Node who is sending the Datagram to this object
    property LocalHelper: TOpenLCBMessageHelper read FLocalHelper write FLocalHelper;   // Global object to work with OLCB messages
    property SourceAlias: Word read FSourceAlias write FSourceAlias;                    // Node who is looking for Datagrams assigned to it to come in
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word);
    destructor Destroy; override;
    function ExtractDataBytesAsInt(StartByteIndex, EndByteIndex: Integer): QWord;       // Helpers to pull information out of the datagram byte array
    function ExtractDataBytesAsString(StartIndex, Count: Integer): String;              // Helpers to pull information out of the datagram byte array
    procedure CopyToStream(Stream: TStream; StartIndex, Count: Integer);                // Helpers to move datagram bytes to a stream
    procedure Process(AHelper: TOpenLCBMessageHelper; ComPortThread: TComPortThread);   // Processes the message/
    property CurrentPos: Byte read FCurrentPos write FCurrentPos;                       // Running count of the number of bytes being received, once "Full" this is the number of byte in the datagram
    property RawDatagram: TDatagramArray read FRawDatagram write FRawDatagram;          // Raw datagram bytes received
    property Full: Boolean read FFull write FFull;                                      // Raw datagram bytes with a size of CurrentPos is valid
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
    procedure Process(AHelper: TOpenLCBMessageHelper; ComPortThread: TComPortThread);
    property Datagram[Index: Integer]: TDatagramReceive read GetDatagram;       // Inprocess and completed Datagrams, the order they are received is preserved
  end;

  { TDatagramSend }

  TDatagramSend = class
  private
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
    property CurrentPos: Byte read FCurrentPos write FCurrentPos;
    property DestinationAlias: Word read FDestinationAlias write FDestinationAlias;
    property LocalHelper: TOpenLCBMessageHelper read FLocalHelper write FLocalHelper;
    property ProtocolHeader: TCANByteArray read FProtocolHeader write FProtocolHeader;
    property ProtocolHeaderLen: Byte read FProtocolHeaderLen write FProtocolHeaderLen;
    property DataBytesSent: TCANByteArray read FDataBytesSent write FDataBytesSent;
    property DataBytesSentLen: Byte read FDataBytesSentLen write FDataBytesSentLen;
    property MTI: DWord read FMTI write FMTI;
    property RetryCount: Byte read FRetryCount write FRetryCount;
    property SourceAlias: Word read FSourceAlias write FSourceAlias;
    property Stream: TMemoryStream read FStream write FStream;
    property WaitingForACK: Boolean read FWaitingForACK write FWaitingForACK;             // After a frame is sent need to wait
  public
    constructor Create;
    destructor Destroy; override;
    function Initialize(AStream: TStream; AProtocolHeader: TCANByteArray; AProtocolHeaderLen: Byte; ASourceAlias, ADestinationAlias: Word): Boolean;
    procedure Process(AHelper: TOpenLCBMessageHelper; ComPortThread: TComPortThread);
    property Empty: Boolean read FEmpty write FEmpty;
    property ErrorCode: Word read FErrorCode write FErrorCode;    // One of the DATAGRAM_REJECTED_xxx constants, if $0000 then no error
  end;


implementation

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

procedure TDatagramReceiveManager.Process(AHelper: TOpenLCBMessageHelper; ComPortThread: TComPortThread);
var
  TestDatagram: TDatagramReceive;
begin
  if IsDatagramMTI(AHelper.MTI) then
  begin
    TestDatagram := FindInProcessDatagramByAlias(AHelper.SourceAliasID);
    if not Assigned(TestDatagram) then
    begin
      TestDatagram := TDatagramReceive.Create(SourceAlias, AHelper.SourceAliasID);  // Create a new receiving Datagram object for source alias of the message to us
      Datagrams.Add(TestDatagram);
    end;
    TestDatagram.Process(AHelper, ComPortThread);
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
function TDatagramSend.Initialize(AStream: TStream; AProtocolHeader: TCANByteArray; AProtocolHeaderLen: Byte; ASourceAlias, ADestinationAlias: Word): Boolean;
begin
  Result := False;
  if AProtocolHeaderLen + AStream.Size <= DATAGRAM_LENGTH then
  begin
    Stream.Position := 0;
    AStream.Position := 0;
    Stream.CopyFrom(AStream, AStream.Size);
    FProtocolHeader := AProtocolHeader;
    FProtocolHeaderLen := AProtocolHeaderLen;
    FSourceAlias := ASourceAlias;
    FDestinationAlias := ADestinationAlias;
    FCurrentPos := 0;
    FEmpty := False;
    FWaitingForACK := False;
    FErrorCode := $0000;
    FRetryCount := 0;
    Result := True
  end;
end;

// *****************************************************************************
// Call repeatedly in the statemachine until Empty = True
//   AHelper: The next message that was received.  The object checks for successful ACK from the
//            destination before moving on
// *****************************************************************************
procedure TDatagramSend.Process(AHelper: TOpenLCBMessageHelper; ComPortThread: TComPortThread);
var
  Count: Byte;
begin
  Count := 0;
  if not Empty and ComPortThread.Connected then
  begin
    // See if we are waiting for the node to sent us and ACK
    if WaitingForACK then
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
                 CurrentPos := 0;                                               // Reset the statemachine to send it all over again
                 Stream.Position := 0;
                 ErrorCode := $0000;
                 WaitingForACK := False;
                 Inc(FRetryCount);
               end else
                 Empty := True                                                  // Error; don't resend and quit with ErrorCode set
            end;
        end;
      end
    end else
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
