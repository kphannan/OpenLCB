unit AwesomeThrottleMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Menus, ActnList, Buttons,
  serialport_thread, form_comportsettings, lcltype, Spin, form_logwindow,
  SynEditKeyCmds, form_about, olcb_utilities, olcb_defines, form_aliastree,
  form_memconfig, laz2_DOM, laz2_XMLRead, laz2_XMLWrite, math_float16, nmra_dcc;

const
  BUNDLENAME = 'AwesomeThrottle';

  FILENAME_SETTINGS_UNIX = '/Contents/Resources/Settings.txt';
  FILENAME_SETTINGS_WIN  = 'tests\Settings.txt';

(*
  {$IFDEF DARWIN}
var
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
{$ENDIF}
begin
  // Under OSX we get the path of the executable
{$IFDEF DARWIN}
  pathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle());
  pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
  CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
  CFRelease(pathRef);
  CFRelease(pathCFStr);
  FApplicationPath := pathStr;
{$ENDIF}
    // Under Windows we get the path of the executable
{$IFDEF Windows}
  FApplicationPath := ExtractFilePath(Application.ExeName);
{$ENDIF}
{$IFDEF Linux}
  FApplicationPath := PATH_UNIX_APPLICATION_PATH;    // Linux is typically hardcoded to a path
{$ENDIF}
*)

(*
  {$IFDEF Windows}
  MTIList.LoadFromFile(Settings.ApplicationPath + FILENAME_SETTINGS_WIN);
  {$ENDIF}
  {$IFDEF darwin}
  MTIList.LoadFromFile(Settings.ApplicationPath + FILENAME_SETTINGS_UNIX);
  {$ENDIF}
*)

type
   { TNode }
  TNode = class
  private
    FAliasID: Word;
    FMessageList: TStringList;
  public
    property AliasID: Word read FAliasID write FAliasID;
    property MessageList: TStringList read FMessageList write FMessageList;
    constructor Create;
    destructor Destroy; override;
  end;

  { TDatagram }

  TDatagram = class
  private
    FData: TDatagramArray;
    FFull: Boolean;
    FSendHeaderCount: Byte;
    FSendPacketCurrentIndex: Integer;
    FSendPacketMaxLen: Byte;
    FSendStream: TMemoryStream;
    FSendComplete: Boolean;
    FSendHeader: TByteArray;
    FSize: Byte;
    procedure SetSendPacketMaxLen(AValue: Byte);
  protected
    procedure SendStreamToByteArray(StartIndex: Byte; var ByteArray: TByteArray; var Count: Byte);
    procedure SendHeaderToByteArray(var ByteArray: TByteArray);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function ExtractDataBytesAsInt(StartByteIndex, EndByteIndex: Integer): QWord;
    function ExtractDataBytesAsString(StartIndex, Count: Integer): String;
    procedure CopyToStream(Stream: TStream; StartIndex, Count: Integer);
    procedure ProcessReceive(AHelper: TOpenLCBMessageHelper);
    procedure ProcessSend(AHelper: TOpenLCBMessageHelper; SourceAliasID, DestinationAliasID: Word);
    property Size: Byte read FSize write FSize;                                         // NOT SURE??
    property Data: TDatagramArray read FData write FData;                               // Raw datagram to send
    property Full: Boolean read FFull write FFull;                                      // Sending: Full is sending and not complete; Receiving Full is data is valid
    property SendStream: TMemoryStream read FSendStream write FSendStream;              // Source Stream to send
    property SendPacketCurrentIndex: Integer read FSendPacketCurrentIndex write FSendPacketCurrentIndex;  // Current index of the datagram being sent (0..62)
    property SendComplete: Boolean read FSendComplete write FSendComplete;              // The entire SendStream has been sent via datagrams
    property SendHeader: TByteArray read FSendHeader write FSendHeader;                 // Header for the Datagram type to send
    property SendHeaderCount: Byte read FSendHeaderCount write FSendHeaderCount;        // Number of bytes in the datagram header to send
    property SendPacketMaxLen: Byte read FSendPacketMaxLen write SetSendPacketMaxLen;   // The number of bytes to send per datagram (max = 64, min = 1)
  end;

  { TAddressSpaceReadWriteHelper }

  TAddressSpaceReadWriteHelper = class
  private
    FAddressHi: Integer;
    FAddressLo: DWord;
    FAddressSpace: Byte;
    FAddressCurrent: Integer;
    FCommand: Byte;
    FProtocolHeader: Byte;
    FBlockReadCount: Byte;
    FStream: TMemoryStream;
    FTerminate: Boolean;
    function GetSize: DWord;
  public
    constructor Create;
    destructor Destroy; override;
    function ReturnStreamAsString: AnsiString;
    property AddressCurrent: Integer read FAddressCurrent write FAddressCurrent;
    property AddressLo: DWord read FAddressLo write FAddressLo;
    property AddressHi: Integer read FAddressHi write FAddressHi;
    property Command: Byte read FCommand write FCommand;
    property AddressSpace: Byte read FAddressSpace write FAddressSpace;
    property ProtocolHeader: Byte read FProtocolHeader write FProtocolHeader;      // Number of bytes in the header for the Protocol
    property BlockReadCount: Byte read FBlockReadCount write FBlockReadCount;      // Number of bytes to read in one chunck
    property Size: DWord read GetSize;
    property Stream: TMemoryStream read FStream write FStream;
    property Terminate: Boolean read FTerminate write FTerminate;
  end;

  { TAddressSpace }

  TAddressSpace = class
  private
    FDescription: String;
    FFlags: Byte;
    FHiAddr: DWord;
    FLoAddr: DWord;
    FPresent: Boolean;
    FSpace: Byte;
    FSpaceReturned: Byte;    // Address Space retured by the Get Info message when call with the FSpace field, should be the same if node implemented correctly
    function GetIsReadOnly: Boolean;
  public
    constructor Create;
    property Space: Byte read FSpace write FSpace;
    property SpaceReturned: Byte read FSpaceReturned write FSpaceReturned;
    property LoAddr: DWord read FLoAddr write FLoAddr;
    property HiAddr: DWord read FHiAddr write FHiAddr;
    property Present: Boolean read FPresent write FPresent;
    property Flags: Byte read FFlags write FFlags;
    property IsReadOnly: Boolean read GetIsReadOnly;
    property Description: String read FDescription write FDescription;
  end;

  TAddressSpaceArray = array of TAddressSpace;

  TMessageCallback = procedure(AHelper: TOpenLCBMessageHelper) of object;

   { TForm1 }
  TForm1 = class(TForm)
    ActionF1: TAction;
    ActionF10: TAction;
    ActionF11: TAction;
    ActionF12: TAction;
    ActionF13: TAction;
    ActionF14: TAction;
    ActionF15: TAction;
    ActionF16: TAction;
    ActionF17: TAction;
    ActionF18: TAction;
    ActionF19: TAction;
    ActionF2: TAction;
    ActionF20: TAction;
    ActionF21: TAction;
    ActionF22: TAction;
    ActionF23: TAction;
    ActionF24: TAction;
    ActionF25: TAction;
    ActionF26: TAction;
    ActionF27: TAction;
    ActionF28: TAction;
    ActionF3: TAction;
    ActionF4: TAction;
    ActionF5: TAction;
    ActionF6: TAction;
    ActionF7: TAction;
    ActionF8: TAction;
    ActionF9: TAction;
    ActionF0: TAction;
    ActionThrottleEmergencyStop: TAction;
    ActionThrottleStop: TAction;
    ActionFindIsDccAddressProxies: TAction;
    ActionFindIsIdleProxies: TAction;
    ActionFindInUseProxies: TAction;
    ActionFindAllTrains: TAction;
    ActionMemConfigSendSNIP: TAction;
    ActionMemConfigWriteSpaceData: TAction;
    ActionMemConfigReadSpaceData: TAction;
    ActionMemConfigReadSpaces: TAction;
    ActionMemConfigTerminate: TAction;
    ActionMemConfigClearSysEdit: TAction;
    ActionMemConfigAddressSpaceList: TAction;
    ActionMemConfigShow: TAction;
    ActionVerifyNodeID: TAction;
    ActionMemConfigOptions: TAction;
    ActionAliasTreeShow: TAction;
    ActionAliasTreeRebuild: TAction;
    ActionAliasTreeClear: TAction;
    ActionFindAllocateTrain: TAction;
    ActionLogSelectAll: TAction;
    ActionLogPaste: TAction;
    ActionLogCopy: TAction;
    ActionLogCut: TAction;
    ActionLogClear: TAction;
    ActionLogWindow: TAction;
    ActionToolsShowOptionsWin: TAction;
    ActionToolsShowPreferencesMac: TAction;
    ActionToolsConnect: TAction;
    ActionHelpShowAbout: TAction;
    ActionFileExit: TAction;
    ActionToolsCOMPortSettings: TAction;
    ActionList: TActionList;
    BitBtnShowConfig: TBitBtn;
    BitBtnFindIsIdleProxies: TBitBtn;
    BitBtnFindIsDccAddress: TBitBtn;
    BitBtnFindAllTrains: TBitBtn;
    BitBtnFindInUseProxies: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtnSearch: TBitBtn;
    BitBtnShowLog: TBitBtn;
    BitBtnEnumTargetNodes: TBitBtn;
    BitBtnComPortSettings: TBitBtn;
    BitBtnConnect: TBitBtn;
    Button1: TButton;
    ButtonF0: TButton;
    ButtonF1: TButton;
    ButtonF10: TButton;
    ButtonF11: TButton;
    ButtonF12: TButton;
    ButtonF13: TButton;
    ButtonF2: TButton;
    ButtonF3: TButton;
    ButtonF4: TButton;
    ButtonF5: TButton ;
    ButtonF6: TButton;
    ButtonF7: TButton;
    ButtonF8: TButton;
    ButtonF9: TButton;
    ComboBoxNodeIDs: TComboBox;
    EditThrottleAlias: TEdit;
    EditThrottleNodeID: TEdit;
    GroupBox1: TGroupBox;
    GroupBoxSearch: TGroupBox;
    GroupBoxThrottle: TGroupBox;
    GroupBox4: TGroupBox;
    ImageListSmall: TImageList;
    Label1: TLabel;
    LabelThrottleAliasTitle: TLabel;
    LabelThrottleNodeIDTitle: TLabel;
    Label12: TLabel;
    LabelThrottleSpeed: TLabel;
    LabelThrottleIndicator: TLabel;
    LabelConsistAddress: TLabel;
    LabelTrainNameTitle: TLabel;
    LabelAliasIDTitle: TLabel;
    LabelTrainNodeIDTitle: TLabel;
    LabelTrainNumberTitle: TLabel;
    LabelTrainDCCAddressTitle: TLabel;
    LabelConsistAddressTitle: TLabel;
    LabelTrainDCCAddress: TLabel;
    LabelTrainNodeID: TLabel;
    LabelAliasID: TLabel;
    LabelTrainNumber: TLabel;
    LabelTrainName: TLabel;
    ListViewSearchList: TListView;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItemSeparator2: TMenuItem;
    MenuItemOptionsWin: TMenuItem;
    MenuItemSeparator0: TMenuItem;
    MenuItemConnect: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemTools: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItemFile: TMenuItem;
    RadioGroupDCCAddressFind: TRadioGroup;
    RadioGroupDccSpeedStep: TRadioGroup;
    RadioGroupDirection: TRadioGroup;
    RadioGroupDCCAddress: TRadioGroup;
    SpinEditDccAddress: TSpinEdit;
    SpinEditDccProxy: TSpinEdit;
    TabControlThottle: TTabControl;
    TimerReply: TTimer;
    TrackBarThrottle: TTrackBar;
    procedure ActionAliasTreeClearExecute(Sender: TObject);
    procedure ActionAliasTreeRebuildExecute(Sender: TObject);
    procedure ActionF0Execute(Sender: TObject);
    procedure ActionF10Execute(Sender: TObject);
    procedure ActionF11Execute(Sender: TObject);
    procedure ActionF12Execute(Sender: TObject);
    procedure ActionF13Execute(Sender: TObject);
    procedure ActionF1Execute(Sender: TObject);
    procedure ActionF2Execute(Sender: TObject);
    procedure ActionF3Execute(Sender: TObject);
    procedure ActionF4Execute(Sender: TObject);
    procedure ActionF5Execute(Sender: TObject);
    procedure ActionF6Execute(Sender: TObject);
    procedure ActionF7Execute(Sender: TObject);
    procedure ActionF8Execute(Sender: TObject);
    procedure ActionF9Execute(Sender: TObject);
    procedure ActionFindAllocateTrainExecute(Sender: TObject);
    procedure ActionToolsCOMPortSettingsExecute(Sender: TObject);
    procedure ActionToolsConnectExecute(Sender: TObject);
    procedure ActionFileExitExecute(Sender: TObject);
    procedure ActionFindAllTrainsExecute(Sender: TObject);
    procedure ActionFindInUseProxiesExecute(Sender: TObject);
    procedure ActionFindIsDccAddressProxiesExecute(Sender: TObject);
    procedure ActionFindIsIdleProxiesExecute(Sender: TObject);
    procedure ActionLogClearExecute(Sender: TObject);
    procedure ActionLogCopyExecute(Sender: TObject);
    procedure ActionLogCutExecute(Sender: TObject);
    procedure ActionLogPasteExecute(Sender: TObject);
    procedure ActionLogSelectAllExecute(Sender: TObject);
    procedure ActionLogWindowExecute(Sender: TObject);
    procedure ActionMemConfigClearSysEditExecute(Sender: TObject);
    procedure ActionMemConfigOptionsExecute(Sender: TObject);
    procedure ActionMemConfigReadSpaceDataExecute(Sender: TObject);
    procedure ActionMemConfigReadSpacesExecute(Sender: TObject);
    procedure ActionMemConfigSendSNIPExecute(Sender: TObject);
    procedure ActionMemConfigShowExecute(Sender: TObject);
    procedure ActionMemConfigTerminateExecute(Sender: TObject);
    procedure ActionMemConfigWriteSpaceDataExecute(Sender: TObject);
    procedure ActionHelpShowAboutExecute(Sender: TObject);
    procedure ActionAliasTreeShowExecute(Sender: TObject);
    procedure ActionThrottleEmergencyStopExecute(Sender: TObject);
    procedure ActionThrottleStopExecute(Sender: TObject);
    procedure ActionVerifyNodeIDExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ComboBoxNodeIDsChange(Sender: TObject);
    procedure ComboBoxNodeIDsDropDown(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LabelThrottleIndicatorClick(Sender: TObject);
    procedure ListViewSearchListChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure RadioGroupDccSpeedStepClick(Sender: TObject);
    procedure RadioGroupDirectionClick(Sender: TObject);
    procedure TabControlThottleChange(Sender: TObject);
    procedure TimerReplyTimer(Sender: TObject);
    procedure TrackBarThrottleChange(Sender: TObject);
  private
    FActiveSpace: TAddressSpace;
    FAddressEnumIndex: Byte;
    FAddressSpaces: TAddressSpaceArray;
    FAliasList: TStringList;
    FComPortThread: TComPortThread;
    FDatagram: TDatagram;
    FHelper: TOpenLCBMessageHelper;
    FAddressSpaceReadHelper: TAddressSpaceReadWriteHelper;
    FNodeList: TList;
    FReceivedCallback: TMessageCallback;
    FSendHelper: TOpenLCBMessageHelper;
    FShownOnce: Boolean;
    FSNIP_Stream: TMemoryStream;
    FAddressSpaceWriteHelper: TAddressSpaceReadWriteHelper;
    FTimerTickCount: Word;
    FTimerTickTimout: Word;
    function GetConnected: Boolean;
    function GetThrottleAliasID: Word;
    function GetTrainAliasID: Word;
    procedure SetReceivedCallback(AValue: TMessageCallback);
    { private declarations }
  protected
    {$IFDEF DARWIN}
      AppMenu     : TMenuItem;
      AppSep1Cmd  : TMenuItem;
      AppPrefCmd  : TMenuItem;
    {$ENDIF}
    AppAboutCmd : TMenuItem;
    procedure FreeNodes;
    function FindNode(AliasID: Word): TNode;
    procedure SyncReceiveMessage(MessageStr: String);
    procedure SyncSendMessage(MessageStr: String);
    procedure SyncLogWindowHide;
    procedure SyncMemConfigOptions(AHelper: TOpenLCBMessageHelper);
    procedure SyncMemConfigReadAddressSpaces(AHelper: TOpenLCBMessageHelper);
    procedure SyncMemConfigEnumAddressSpaces(AHelper: TOpenLCBMessageHelper);
    procedure SyncMemConfigAddressSpaceRead(AHelper: TOpenLCBMessageHelper);
    procedure SyncMemConfigAddressSpaceWrite(AHelper: TOpenLCBMessageHelper);
    procedure SyncMemConfigSendSNIP(AHelper: TOpenLCBMessageHelper);
    procedure SyncFindAllTrainProducers(AHelper: TOpenLCBMessageHelper);
    procedure SyncIsIdleProxyProducers(AHelper: TOpenLCBMessageHelper);
    procedure SyncIsInUseProxyProducers(AHelper: TOpenLCBMessageHelper);
    procedure SyncProxiedDccAddressProducers(AHelper: TOpenLCBMessageHelper);
    procedure SyncThrottleSpeedChange(AHelper: TOpenLCBMessageHelper);
    function DetailedMessageOutput(MessageString: String): String;
    procedure FunctionSend(AnAction: TAction; FunctionID: Word; Toggle: Boolean);

    procedure ListViewAddressSpaceSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure UpdateUI;
    property ActiveSpace: TAddressSpace read FActiveSpace write FActiveSpace;
    property AddressEnumIndex: Byte read FAddressEnumIndex write FAddressEnumIndex;
    property AddressSpaceReadHelper: TAddressSpaceReadWriteHelper read FAddressSpaceReadHelper write FAddressSpaceReadHelper;
    property AddressSpaceWriteHelper: TAddressSpaceReadWriteHelper read FAddressSpaceWriteHelper write FAddressSpaceWriteHelper;
    property Datagram: TDatagram read FDatagram write FDatagram;
    property ThrottleAliasID: Word read GetThrottleAliasID;
    property TrainAliasID: Word read GetTrainAliasID;
    property ReceivedCallback: TMessageCallback read FReceivedCallback write SetReceivedCallback;
    property SNIP_Stream: TMemoryStream read FSNIP_Stream write FSNIP_Stream;
    property TimerTickCount: Word read FTimerTickCount write FTimerTickCount;
    property TimerTickTimeout: Word read FTimerTickTimout write FTimerTickTimout;
  public
    { public declarations }
    property AliasList: TStringList read FAliasList write FAliasList;
    property ShownOnce: Boolean read FShownOnce write FShownOnce;
    property ComPortThread: TComPortThread read FComPortThread write FComPortThread;
    property Connected: Boolean read GetConnected;
    property Helper: TOpenLCBMessageHelper read FHelper write FHelper;
    property ReplyHelper: TOpenLCBMessageHelper read FSendHelper write FSendHelper;
    property NodeList: TList read FNodeList write FNodeList;
    property AddressSpaces: TAddressSpaceArray read FAddressSpaces write FAddressSpaces;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{$ifdef Windows}
uses Windows;
{$endif}
{$IFDEF Darwin}
//uses
//  MacOSAll;
{$ENDIF}

// xxxxxxx

{ TAddressSpace }

function TAddressSpace.GetIsReadOnly: Boolean;
begin
  Result := FFlags and $01 = $01
end;

constructor TAddressSpace.Create;
begin
  FDescription := '';
  FFlags := 0;
  FHiAddr := 0;
  FLoAddr := 0;
  FPresent := False;
  FSpace := 0;
  FSpaceReturned := 0;
end;

{ TAddressSpaceReadWriteHelper }

function TAddressSpaceReadWriteHelper.GetSize: DWord;
begin
  Result := (AddressHi - AddressLo) // + 1
end;

constructor TAddressSpaceReadWriteHelper.Create;
begin
  FStream := TMemoryStream.Create;
end;

destructor TAddressSpaceReadWriteHelper.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

function TAddressSpaceReadWriteHelper.ReturnStreamAsString: AnsiString;
var
  i: Integer;
begin
  Stream.Seek(0, soFromBeginning);
  SetLength(Result, Stream.Size);
  for i := 1 to Stream.Size do     // Strings are 1 indexed!!!!
    Stream.Read(Result[i], 1);
end;


{ TDatagram }

procedure TDatagram.SetSendPacketMaxLen(AValue: Byte);
begin
  if AValue > 64 then
    AValue := 64;
  if AValue < 1 then
    AValue := 1;
  FSendPacketMaxLen := AValue;
end;

procedure TDatagram.SendStreamToByteArray(StartIndex: Byte; var ByteArray: TByteArray; var Count: Byte);
begin
  Count := 0;
  while ((SendPacketCurrentIndex-SendHeaderCount) < SendPacketMaxLen) and (SendStream.Position < SendStream.Size) and (StartIndex < 8) do
  begin
    ByteArray[StartIndex] := SendStream.ReadByte;
    Inc(Count);
    Inc(FSendPacketCurrentIndex);
    Inc(StartIndex);
  end;
end;

procedure TDatagram.SendHeaderToByteArray(var ByteArray: TByteArray);
var
  i: Integer;
begin
  for i := 0 to SendHeaderCount - 1 do
    ByteArray[i] := SendHeader[i];
end;

constructor TDatagram.Create;
begin
  FSendStream := TMemoryStream.Create;
  SendPacketMaxLen := 64;
end;

destructor TDatagram.Destroy;
begin
  FreeAndNil(FSendStream);
  inherited Destroy;
end;

procedure TDatagram.Clear;
var
  i: Integer;
begin
  FSize := 0;
  FSendComplete := False;
  FFull := False;
  for i := 0 to DATAGRAM_LENGTH - 1 do
    Data[i] := 0;
  SendStream.Size := 0;
end;

function TDatagram.ExtractDataBytesAsInt(StartByteIndex, EndByteIndex: Integer): QWord;
var
  i, Offset, Shift: Integer;
  ByteAsQ, ShiftedByte: QWord;
begin
  Result := 0;
  Offset := EndByteIndex - StartByteIndex;
  for i := StartByteIndex to EndByteIndex do
  begin
    Shift := Offset * 8;
    ByteAsQ := QWord( Data[i]);
    ShiftedByte := ByteAsQ shl Shift;
    Result := Result or ShiftedByte;
    Dec(Offset)
  end;
end;

function TDatagram.ExtractDataBytesAsString(StartIndex, Count: Integer): String;
var
  i, Last: Integer;
begin
  Result := '';
  if Count = -1 then
    Last := Size
  else
    Last := StartIndex + Count;
  if Last > Size then
    Last := Size;

  for i := StartIndex to Last - 1 do
    Result := Result + Chr( Data[i]);
end;

procedure TDatagram.CopyToStream(Stream: TStream; StartIndex, Count: Integer);
var
  i, Last: Integer;
begin
  if Count = -1 then
    Last := Size
  else
    Last := StartIndex + Count;

  if Last > Size then
    Last := Size;

  for i := StartIndex to Last - 1 do
    Stream.WriteByte(Data[i]);
end;

procedure TDatagram.ProcessReceive(AHelper: TOpenLCBMessageHelper);
var
  i: Integer;
begin
  case AHelper.MTI of
     MTI_FRAME_TYPE_DATAGRAM_ONLY_FRAME :
       begin
         Clear;
         for i := 0 to AHelper.DataCount - 1 do
           Data[i] := AHelper.Data[i];
         Size := AHelper.DataCount;
         Full := True
       end;
     MTI_FRAME_TYPE_DATAGRAM_FRAME_START :
       begin
         Full := False;
         for i := 0 to AHelper.DataCount - 1 do
           Data[i] := AHelper.Data[i];
         Size := AHelper.DataCount;
       end;
     MTI_FRAME_TYPE_DATAGRAM_FRAME :
       begin
         for i := 0 to AHelper.DataCount - 1 do
           Data[Size+i] := AHelper.Data[i];
         Size := Size + AHelper.DataCount;
       end;
    MTI_FRAME_TYPE_DATAGRAM_FRAME_END :
      begin
        for i := 0 to AHelper.DataCount - 1 do
          Data[Size+i] := AHelper.Data[i];
        Size := Size + AHelper.DataCount;
        Full := True;
      end;
  end;
end;

procedure TDatagram.ProcessSend(AHelper: TOpenLCBMessageHelper; SourceAliasID, DestinationAliasID: Word);
var
  DataBytes: TByteArray;
  Count: Byte;
begin
  Count := 0;
  DataBytes[0] := 0;
  if Full then
  begin
    if SendPacketCurrentIndex = 0 then
    begin
      // Starting a new packet
      if (((SendStream.Size - SendStream.Position)+SendHeaderCount) <= 8)  or ((SendPacketCurrentIndex + 8) >= SendPacketMaxLen) then
      begin
        SendPacketCurrentIndex := SendHeaderCount;                              // first bytes are the header
        SendHeaderToByteArray(DataBytes);
        SendStreamToByteArray(SendHeaderCount, DataBytes, Count);
        AHelper.Load(ol_OpenLCB, MTI_FRAME_TYPE_DATAGRAM_ONLY_FRAME, SourceAliasID, DestinationAliasID, Count+SendHeaderCount, SendHeader[0], DataBytes[1], DataBytes[2], DataBytes[3], DataBytes[4], DataBytes[5], DataBytes[6], DataBytes[7]);
        Full := False;
        SendComplete := SendStream.Position = SendStream.Size;
      end else
      begin
        SendPacketCurrentIndex := SendHeaderCount;                              // first bytes are the header
        SendHeaderToByteArray(DataBytes);
        SendStreamToByteArray(SendHeaderCount, DataBytes, Count);
        AHelper.Load(ol_OpenLCB, MTI_FRAME_TYPE_DATAGRAM_FRAME_START, SourceAliasID, DestinationAliasID, Count+SendHeaderCount, DataBytes[0], DataBytes[1], DataBytes[2], DataBytes[3], DataBytes[4], DataBytes[5], DataBytes[6], DataBytes[7]);
      end;
    end else
    begin
      // Continuing a packet
      if ((SendStream.Size - SendStream.Position) <= 8) or ((SendPacketCurrentIndex + 8) >= SendPacketMaxLen) then
      begin
        SendStreamToByteArray(0, DataBytes, Count);
        AHelper.Load(ol_OpenLCB, MTI_FRAME_TYPE_DATAGRAM_FRAME_END, SourceAliasID, DestinationAliasID, Count, DataBytes[0], DataBytes[1], DataBytes[2], DataBytes[3], DataBytes[4], DataBytes[5], DataBytes[6], DataBytes[7]);
        Full := False;
        SendComplete := SendStream.Position = SendStream.Size;
      end else
      begin
        SendStreamToByteArray(0, DataBytes, Count);
        AHelper.Load(ol_OpenLCB, MTI_FRAME_TYPE_DATAGRAM_FRAME, SourceAliasID, DestinationAliasID, Count, DataBytes[0], DataBytes[1], DataBytes[2], DataBytes[3], DataBytes[4], DataBytes[5], DataBytes[6], DataBytes[7]);
      end;
    end;
  end;
end;

{ TNode }

constructor TNode.Create;
begin
  MessageList := TStringList.Create;
end;

destructor TNode.Destroy;
begin
  FreeAndNil(FMessageList);
  inherited Destroy;
end;

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
  if not ShownOnce then
  begin
    {$IFDEF DARWIN}
    AppMenu := TMenuItem.Create(Self);  {Application menu}
    AppMenu.Caption := #$EF#$A3#$BF;  {Unicode Apple logo char}
    MainMenu.Items.Insert(0, AppMenu);

    AppAboutCmd := TMenuItem.Create(Self);
    AppAboutCmd.Action := ActionHelpShowAbout;
    AppAboutCmd.Caption := 'About ' + BUNDLENAME;
    AppMenu.Add(AppAboutCmd);  {Add About as item in application menu}

    AppSep1Cmd := TMenuItem.Create(Self);
    AppSep1Cmd.Caption := '-';
    AppMenu.Add(AppSep1Cmd);

    ActionToolsShowPreferencesMac.ShortCut := ShortCut(VK_OEM_COMMA, [ssMeta]);
    AppPrefCmd := TMenuItem.Create(Self);
    AppPrefCmd.Action := ActionToolsShowPreferencesMac;
    AppMenu.Add(AppPrefCmd);
    ActionToolsShowOptionsWin.Visible := False;
   // MenuItemToolsSeparatorWin.Visible := False;
    {$ELSE}
    AppAboutCmd := TMenuItem.Create(Self);
    AppAboutCmd.Action := ActionHelpShowAbout;
    MenuItemHelp.Add(AppAboutCmd);
    {$ENDIF}

    ActionMemConfigTerminate.Enabled := False;
    FormMemConfig.ListViewAddressSpace.OnSelectItem := @ListViewAddressSpaceSelectItem;
    FormLog.HideCallback := @SyncLogWindowHide;
    FormComPort.ScanPorts;
    ComPortThread := nil;
    UpdateUI;
    ShownOnce := True
  end;
end;

procedure TForm1.LabelThrottleIndicatorClick(Sender: TObject);
begin

end;

procedure TForm1.ListViewSearchListChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  UpdateUI;
end;

procedure TForm1.RadioGroupDccSpeedStepClick(Sender: TObject);
begin
  case RadioGroupDccSpeedStep.ItemIndex of
    0: begin
         TrackBarThrottle.Max := DCC_SPEED_STEP_14;
         TrackBarThrottle.Frequency := 1;
       end;
    1: begin
         TrackBarThrottle.Max := DCC_SPEED_STEP_28;
         TrackBarThrottle.Frequency := 1;
       end;
    2: begin
         TrackBarThrottle.Max := DCC_SPEED_STEP_128;
         TrackBarThrottle.Frequency := 2;
       end;
  end;
  TrackBarThrottle.Position := 0
end;

procedure TForm1.RadioGroupDirectionClick(Sender: TObject);
begin
  TrackBarThrottleChange(TrackBarThrottle)
end;

procedure TForm1.TabControlThottleChange(Sender: TObject);
begin
  case TabControlThottle.TabIndex of
    0: begin   // OLCB Throttle
         LabelThrottleIndicator.Caption := 'm/s';
         RadioGroupDccSpeedStep.Visible := False;
         TrackBarThrottle.Max := 100;
         TrackBarThrottle.Frequency := 2;
       end;
    1: begin   // DCC Throttle
         LabelThrottleIndicator.Caption := 'Step';
         RadioGroupDccSpeedStep.Visible := True;
         RadioGroupDccSpeedStepClick(Self)
       end
  end;
end;

procedure TForm1.TimerReplyTimer(Sender: TObject);
begin
  if ReceivedCallback <> nil then
    Inc(FTimerTickCount);
  if TimerTickCount > TimerTickTimeout then
  begin
    TimerTickTimeout := 0;
    ReceivedCallback := nil;
  end;
end;

procedure TForm1.TrackBarThrottleChange(Sender: TObject);
var
  Speed: Word;
  Step, SpeedByte: Byte;
begin
  LabelThrottleSpeed.Caption := IntToStr(TrackBarThrottle.Position);
  case TabControlThottle.TabIndex of
    0: begin
         if RadioGroupDirection.ItemIndex = 0 then
           Speed := FloatToHalf( Single( TrackBarThrottle.Position))
         else
           Speed := FloatToHalf( -Single( TrackBarThrottle.Position));
         Helper.Load(ol_OpenLCB, MTI_TRACTION_PROTOCOL, ThrottleAliasID, StrToInt(ListViewSearchList.Selected.Caption), 5, $00, $00, TRACTION_OLCB or TRACTION_OP_SPEED_DIR, (Speed shr 8) and $00FF,  Speed and $00FF, $00, $00, $00);
         ComPortThread.Add(Helper.Encode);
         TimerTickCount := 0;
         TimerTickTimeout := 5;
         ReceivedCallback := @SyncThrottleSpeedChange;
       end;
    1: begin
         case RadioGroupDccSpeedStep.ItemIndex of
           0: Step := DCC_SPEED_STEP_14;
           1: Step := DCC_SPEED_STEP_28;
           2: Step := DCC_SPEED_STEP_128;
         end;
         SpeedByte := EncodeDCCSpeed(TrackBarThrottle.Position,  RadioGroupDirection.ItemIndex = 0, Step, False);
         Helper.Load(ol_OpenLCB, MTI_TRACTION_PROTOCOL, ThrottleAliasID, StrToInt(ListViewSearchList.Selected.Caption), 5, $00, $00, TRACTION_DCC or TRACTION_OP_SPEED_DIR, SpeedByte, Step, $00, $00, $00);
         ComPortThread.Add(Helper.Encode);
         TimerTickCount := 0;
         TimerTickTimeout := 5;
         ReceivedCallback := @SyncThrottleSpeedChange;
       end;
  end;
end;

procedure TForm1.ActionToolsConnectExecute(Sender: TObject);
begin
  if Connected then
  begin
    ComPortThread.Terminate;
    FComPortThread := nil;
    ActionToolsConnect.Caption:='Connect';
    ActionToolsConnect.ImageIndex := 4;
  end else
  begin
    FComPortThread := TComPortThread.Create(True);
    try
      ComPortThread.FreeOnTerminate := True;
      {$IFDEF MSWINDOWS}
      ComPortThread.Port := FormComPort.ComboBoxPorts.Text;
      {$ELSE}
        {$IFDEF DARWIN}
        ComPortThread.Port := 'dev/' + FormComPort.ComboBoxPorts.Text;
        {$ELSE}
        ComPortThread.Port := 'dev/' + FormComPort.ComboBoxPorts.Text;
        {$ENDIF}
      {$ENDIF}
      if FormComPort.ComboBoxBaud.ItemIndex = 0 then
        ComPortThread.BaudRate := StrToInt(FormComPort.EditCustomBaudRate.Text)
      else
        ComPortThread.BaudRate := StrToInt(FormComPort.ComboBoxBaud.Items[FormComPort.ComboBoxBaud.ItemIndex]);
      if FormLog.Visible then
      begin
        ComPortThread.EnableSendMessages := True;
      end;
  //    ComPortThread.EnableRawMessages := ActionRawMessageLogShow.Checked;
      ComPortThread.SyncReceiveMessageFunc := @SyncReceiveMessage;
      ComPortThread.SyncSendMessageFunc := @SyncSendMessage;
      ComPortThread.Suspended := False;
      Sleep(500);
      if ComPortThread.Connected then
      begin
        ActionToolsConnect.Caption:='Disconnect';
        ActionToolsConnect.ImageIndex := 3;
      end
      else begin
     //   ShowMessage(ComPortThread.LastErrorDesc);
        ComPortThread.Terminate;
        ComPortThread := nil;
      end;
    except
      if Assigned(ComPortThread) then
      begin
        ComPortThread.Terminate;
        ComPortThread := nil;
      end;
      ActionToolsConnect.Caption:='Connect';
      ActionToolsConnect.ImageIndex := 3;
    end;
  end;
  UpdateUI;
end;

procedure TForm1.ActionFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TForm1.ActionFindAllTrainsExecute(Sender: TObject);
begin
  if Assigned(FComPortThread) then
  begin
    ListViewSearchList.Items.BeginUpdate;
    ListViewSearchList.Items.Clear;
    ListViewSearchList.Items.EndUpdate;
    Helper.Load(ol_OpenLCB, MTI_PRODUCER_IDENDIFY, ThrottleAliasID, 0, 8, $01, $01, $00, $00, $00, $00, $03, $03);
    ComPortThread.Add(Helper.Encode);
    TimerTickCount := 0;
    TimerTickTimeout := 5;
    ReceivedCallback := @SyncFindAllTrainProducers;
  end;
end;

procedure TForm1.ActionFindInUseProxiesExecute(Sender: TObject);
begin
  if Assigned(FComPortThread) then
  begin
    ListViewSearchList.Items.BeginUpdate;
    ListViewSearchList.Items.Clear;
    ListViewSearchList.Items.EndUpdate;
    Helper.Load(ol_OpenLCB, MTI_PRODUCER_IDENDIFY, ThrottleAliasID, 0, 8, $01, $01, $00, $00, $00, $00, $03, $05);
    ComPortThread.Add(Helper.Encode);
    TimerTickCount := 0;
    TimerTickTimeout := 5;
    ReceivedCallback := @SyncIsInUseProxyProducers;
  end;
end;

procedure TForm1.ActionFindIsDccAddressProxiesExecute(Sender: TObject);
var
  DccAddress: Word;
begin
  if Assigned(FComPortThread) then
  begin
    ListViewSearchList.Items.BeginUpdate;
    ListViewSearchList.Items.Clear;
    ListViewSearchList.Items.EndUpdate;
    DccAddress := SpinEditDccProxy.Value;
    if RadioGroupDCCAddressFind.ItemIndex = 1 then
      DccAddress := DccAddress or $C000;
    Helper.Load(ol_OpenLCB, MTI_PRODUCER_IDENDIFY, ThrottleAliasID, 0, 8, $06, $01, $00, $00, Hi(DccAddress), Lo(DccAddress), $00, $01);
    ComPortThread.Add(Helper.Encode);
    TimerTickCount := 0;
    TimerTickTimeout := 5;
    ReceivedCallback := @SyncProxiedDccAddressProducers;
  end;
end;

procedure TForm1.ActionFindIsIdleProxiesExecute(Sender: TObject);
begin
  if Assigned(FComPortThread) then
  begin
    ListViewSearchList.Items.BeginUpdate;
    ListViewSearchList.Items.Clear;
    ListViewSearchList.Items.EndUpdate;
    Helper.Load(ol_OpenLCB, MTI_PRODUCER_IDENDIFY, ThrottleAliasID, 0, 8, $01, $01, $00, $00, $00, $00, $03, $04);
    ComPortThread.Add(Helper.Encode);
    TimerTickCount := 0;
    TimerTickTimeout := 5;
    ReceivedCallback := @SyncIsIdleProxyProducers;
  end;
end;

procedure TForm1.ActionLogClearExecute(Sender: TObject);
begin
  FormLog.SynMemo.ClearAll;
  FreeNodes;
end;

procedure TForm1.ActionLogCopyExecute(Sender: TObject);
begin
  FormLog.SynMemo.CommandProcessor(TSynEditorCommand(ecCopy), ' ', nil);
end;

procedure TForm1.ActionLogCutExecute(Sender: TObject);
begin
  FormLog.SynMemo.CommandProcessor(TSynEditorCommand(ecCut), ' ', nil);
end;

procedure TForm1.ActionLogPasteExecute(Sender: TObject);
begin
  FormLog.SynMemo.CommandProcessor(TSynEditorCommand(ecPaste), ' ', nil);
end;

procedure TForm1.ActionLogSelectAllExecute(Sender: TObject);
begin
  FormLog.SynMemo.SelectAll;
end;

procedure TForm1.ActionLogWindowExecute(Sender: TObject);
begin
  FormLog.Left := Left;
  FormLog.Top := Top + Height + 20;
  FormLog.Width := Width;
  FormLog.Show;
  if Assigned(ComPortThread) then
  begin
    ComPortThread.EnableSendMessages := True;
  end;
end;

procedure TForm1.ActionMemConfigClearSysEditExecute(Sender: TObject);
begin
  FormMemConfig.SynEditAddressSpaceData.BeginUpdate;
  FormMemConfig.SynEditAddressSpaceData.Lines.Clear;
  FormMemConfig.SynEditAddressSpaceData.EndUpdate;
end;

procedure TForm1.ActionMemConfigOptionsExecute(Sender: TObject);
begin
  if Assigned(FComPortThread) then
  begin
    Datagram.Clear;
    Helper.Load(ol_OpenLCB, MTI_FRAME_TYPE_DATAGRAM_ONLY_FRAME, ThrottleAliasID, TrainAliasID, 2, $20, MCP_OP_GET_CONFIG, 0, 0, 0, 0, 0, 0);
    ComPortThread.Add(Helper.Encode);
    TimerTickCount := 0;
    TimerTickTimeout := 50;
    ReceivedCallback := @SyncMemConfigOptions;
  end;
end;

procedure TForm1.ActionMemConfigReadSpaceDataExecute(Sender: TObject);
var
  ByteArray: TByteArray;
begin
  if Assigned(FComPortThread) and Assigned(ActiveSpace) then
  begin
    Datagram.Clear;
    AddressSpaceReadHelper.Stream.Clear;
    AddressSpaceReadHelper.Terminate := False;
    ActionMemConfigTerminate.Enabled := True;

    FormMemConfig.KHexEditor.Clear;
    FormMemConfig.SynEditAddressSpaceData.BeginUpdate;
    try
      // Clear the SynEdit
      FormMemConfig.SynEditAddressSpaceData.Lines.Clear;
      FormMemConfig.SynEditAddressSpaceData.Highlighter := nil;

      AddressSpaceReadHelper.AddressSpace := ActiveSpace.Space;
      AddressSpaceReadHelper.AddressCurrent := StrToInt( FormMemConfig.EditAddressSpaceDataReadLoAddress.Text);
      AddressSpaceReadHelper.AddressLo := StrToInt( FormMemConfig.EditAddressSpaceDataReadLoAddress.Text);
      AddressSpaceReadHelper.AddressHi := StrToInt( FormMemConfig.EditAddressSpaceDataReadHiAddress.Text);
      AddressSpaceReadHelper.BlockReadCount := StrToInt( FormMemConfig.EditAddressSpaceDataReadBlockSize.Text);

      case ActiveSpace.Space of
        MSI_CDI :
          begin
            FormMemConfig.SynEditAddressSpaceData.Highlighter := FormMemConfig.SynXMLSyn;
            if FormMemConfig.CheckBoxForceCommonAddressIntoSpace.Checked then
            begin
              AddressSpaceReadHelper.Command := MCP_READ;
              AddressSpaceReadHelper.ProtocolHeader := 7
            end else
            begin
              AddressSpaceReadHelper.Command := MCP_READ or MCP_CDI;
              AddressSpaceReadHelper.ProtocolHeader := 6
            end;
          end;
        MSI_ALL :
          begin
            if FormMemConfig.CheckBoxForceCommonAddressIntoSpace.Checked then
            begin
              AddressSpaceReadHelper.Command := MCP_READ;
              AddressSpaceReadHelper.ProtocolHeader := 7
            end else
            begin
              AddressSpaceReadHelper.Command := MCP_READ or MCP_ALL;
              AddressSpaceReadHelper.ProtocolHeader := 6
            end;
          end;
        MSI_CONFIG :
          begin
            if FormMemConfig.CheckBoxForceCommonAddressIntoSpace.Checked then
            begin
              AddressSpaceReadHelper.Command := MCP_READ;
              AddressSpaceReadHelper.ProtocolHeader := 7
            end else
            begin
              AddressSpaceReadHelper.Command := MCP_READ or MCP_CONFIGURATION;
              AddressSpaceReadHelper.ProtocolHeader := 6
            end;
          end;
        else    // MSI_ACDI_MFG and MSI_ACDI_USER are not special cases
          begin
            AddressSpaceReadHelper.Command := MCP_READ or MCP_NONE;
            AddressSpaceReadHelper.ProtocolHeader := 7
          end;
      end;
    finally
      FormMemConfig.SynEditAddressSpaceData.EndUpdate
    end;

    AddressSpaceReadHelper.Stream.Size := AddressSpaceReadHelper.Size;
    ByteArray[0] := 0;  // Stop compiler warning
    Helper.IntToByteArray(AddressSpaceReadHelper.AddressLo, ByteArray);
    if (AddressSpaceReadHelper.Command and $03 <> 0) then
      Helper.Load(ol_OpenLCB, MTI_FRAME_TYPE_DATAGRAM_ONLY_FRAME, ThrottleAliasID, TrainAliasID, 7, $20, AddressSpaceReadHelper.Command, ByteArray[3], ByteArray[2], ByteArray[1], ByteArray[0], AddressSpaceReadHelper.BlockReadCount, 0)
    else
      Helper.Load(ol_OpenLCB, MTI_FRAME_TYPE_DATAGRAM_ONLY_FRAME, ThrottleAliasID, TrainAliasID, 8, $20, AddressSpaceReadHelper.Command, ByteArray[3], ByteArray[2], ByteArray[1], ByteArray[0], AddressSpaceReadHelper.AddressSpace, AddressSpaceReadHelper.BlockReadCount);

    AddressSpaceReadHelper.AddressCurrent := AddressSpaceReadHelper.BlockReadCount;

    ComPortThread.Add(Helper.Encode);

    UpdateUI;

    TimerTickCount := 0;
    TimerTickTimeout := 50;
    ReceivedCallback := @SyncMemConfigAddressSpaceRead;
  end
end;

procedure TForm1.ActionMemConfigReadSpacesExecute(Sender: TObject);
begin
  if Assigned(FComPortThread) then
  begin
    Helper.Load(ol_OpenLCB, MTI_FRAME_TYPE_DATAGRAM_ONLY_FRAME, ThrottleAliasID, TrainAliasID, 2, $20, MCP_OP_GET_CONFIG, 0, 0, 0, 0, 0, 0);
    ComPortThread.Add(Helper.Encode);
    TimerTickCount := 0;
    TimerTickTimeout := 50;
    ReceivedCallback := @SyncMemConfigReadAddressSpaces;
  end;
end;

procedure TForm1.ActionMemConfigSendSNIPExecute(Sender: TObject);
begin
  if Assigned(FComPortThread) then
   begin
     SNIP_Stream.Clear;
     Helper.Load(ol_OpenLCB, MTI_SIMPLE_NODE_INFO_REQUEST, ThrottleAliasID, TrainAliasID, 2, 0, 0, 0, 0, 0, 0, 0, 0);
     ComPortThread.Add(Helper.Encode);
     TimerTickCount := 0;
     TimerTickTimeout := 50;
     ReceivedCallback := @SyncMemConfigSendSNIP;
   end;
end;

procedure TForm1.ActionMemConfigShowExecute(Sender: TObject);
begin
  FormMemConfig.Show
end;

procedure TForm1.ActionMemConfigTerminateExecute(Sender: TObject);
begin
  AddressSpaceReadHelper.Terminate := True;
  ActionMemConfigTerminate.Enabled := False;
end;

procedure TForm1.ActionMemConfigWriteSpaceDataExecute(Sender: TObject);
var
  ByteArray: TByteArray;
  BlockReadCount: Byte;
begin
  ByteArray[0] := 0;  // Stop compiler warning
  if  Assigned(FComPortThread) and Assigned(ActiveSpace) then
  begin
    Datagram.Clear;
    ActionMemConfigTerminate.Enabled := False;

    AddressSpaceWriteHelper.AddressSpace := ActiveSpace.Space;
    AddressSpaceWriteHelper.AddressCurrent := StrToInt( FormMemConfig.EditAddressSpaceDataReadLoAddress.Text);
    AddressSpaceWriteHelper.AddressLo := StrToInt( FormMemConfig.EditAddressSpaceDataReadLoAddress.Text);
    AddressSpaceWriteHelper.AddressHi := StrToInt( FormMemConfig.EditAddressSpaceDataReadHiAddress.Text);
    BlockReadCount := StrToInt( FormMemConfig.EditAddressSpaceDataReadBlockSize.Text);

    case ActiveSpace.Space of
      MSI_CDI :
        begin
          if FormMemConfig.CheckBoxForceCommonAddressIntoSpace.Checked then
            AddressSpaceWriteHelper.Command := MCP_WRITE
          else
            AddressSpaceWriteHelper.Command := MCP_WRITE or MCP_CDI
        end;
      MSI_ALL :
        begin
          if FormMemConfig.CheckBoxForceCommonAddressIntoSpace.Checked then
            AddressSpaceWriteHelper.Command := MCP_WRITE
          else
            AddressSpaceWriteHelper.Command := MCP_WRITE or MCP_ALL;
        end;
      MSI_CONFIG :
        begin
          if FormMemConfig.CheckBoxForceCommonAddressIntoSpace.Checked then
            AddressSpaceWriteHelper.Command := MCP_WRITE
          else
            AddressSpaceWriteHelper.Command := MCP_WRITE or MCP_CONFIGURATION;
        end;
      else    // MSI_ACDI_MFG and MSI_ACDI_USER are not special cases since they don't have the short cut MCP bits in the header
        AddressSpaceWriteHelper.Command := MCP_WRITE or MCP_NONE;
    end;
    FormMemConfig.KHexEditor.SaveToStream(Datagram.SendStream);                 // Copy what is in the editor to the Datagram Stream
    if Datagram.SendStream.Size > AddressSpaceWriteHelper.AddressHi - AddressSpaceWriteHelper.AddressLo then
      Datagram.SendStream.Size := AddressSpaceWriteHelper.AddressHi - AddressSpaceWriteHelper.AddressLo;
    Datagram.SendStream.Seek(0, soFromBeginning);                                          // Reset to start of stream

    Helper.IntToByteArray(AddressSpaceWriteHelper.AddressLo, ByteArray);                             // Break apart the 32 bit address into bytes
    Datagram.SendHeader[0] := $20;
    Datagram.SendHeader[1] := AddressSpaceWriteHelper.Command;
    Datagram.SendHeader[2] := ByteArray[3];
    Datagram.SendHeader[3] := ByteArray[2];
    Datagram.SendHeader[4] := ByteArray[1];
    Datagram.SendHeader[5] := ByteArray[0];
    Datagram.SendHeaderCount := 6;
    if AddressSpaceWriteHelper.Command and $03 = 0 then
    begin
      Datagram.SendHeader[6] := AddressSpaceWriteHelper.AddressSpace;
      Datagram.SendHeaderCount := 6;
    end;
    Datagram.SendPacketMaxLen := BlockReadCount;

    Datagram.Full := True;   // Ready to send next byte
    Datagram.SendPacketCurrentIndex := 0;                                     // Would like this to be automatic but not sure how to do it
    while Datagram.Full do                                                    // Wait until the Datagram is sent and not Full
    begin
      Datagram.ProcessSend(Helper, ThrottleAliasID, TrainAliasID);
      ComPortThread.Add(Helper.Encode);
    end;
  end;
  ReceivedCallback := @SyncMemConfigAddressSpaceWrite;
  TimerTickCount := 0;
  TimerTickTimeout := 50;
  UpdateUI;
end;

procedure TForm1.ActionFindAllocateTrainExecute(Sender: TObject);
var
  AddressHi, AddressLo: Byte;
begin
  if Assigned(FComPortThread) then
  begin
    AddressHi := (SpinEditDccAddress.Value shr 8) and $000000FF;
    AddressLo := SpinEditDccAddress.Value and $000000FF;
    if RadioGroupDCCAddress.ItemIndex = 1 then
      AddressHi := AddressHi or %11000000;             // Extended Address
    Helper.Load(ol_OpenLCB, MTI_TRACTION_PROTOCOL, ThrottleAliasID, StrToInt(ListViewSearchList.Selected.Caption), 6, $00, $00, TRACTION_DCC or TRACTION_OP_PROXY_MGMT, DCC_ALLOCATE_ADDRESS, AddressHi, AddressLo, 0, 0);
    ComPortThread.Add(Helper.Encode);
  end;
end;

procedure TForm1.ActionHelpShowAboutExecute(Sender: TObject);
begin
  FormAbout.ShowModal;
end;

procedure TForm1.ActionAliasTreeShowExecute(Sender: TObject);
begin
  FormAliasTree.Left := Left + Width + 4;
  FormAliasTree.Top := Top;
  FormAliasTree.Height := Height;
  FormAliasTree.Show;
end;

procedure TForm1.ActionThrottleEmergencyStopExecute(Sender: TObject);
begin
  TrackBarThrottle.Position := 0;
end;

procedure TForm1.ActionThrottleStopExecute(Sender: TObject);
begin
  TrackBarThrottle.Position := 0;
end;

procedure TForm1.ActionVerifyNodeIDExecute(Sender: TObject);
begin
  AliasList.Clear;
  ComboBoxNodeIDs.Items.Clear;
  if Assigned(FComPortThread) then
  begin
    Helper.Load(ol_OpenLCB, MTI_VERIFY_NODE_ID_NUMBER, ThrottleAliasID, 0, 0, 0, 0, 0, 0, 0, 0,0 ,0);
    ComPortThread.Add(Helper.Encode);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Half: Word;
begin
  FloatToHalf(25.0);
  FloatToHalf(0.0);
  FloatToHalf(-0.0);
  HalfToFloat($3C00);
  HalfToFloat($3C01);
  HalfToFloat($7BFF);
  HalfToFloat($0000);
  HalfToFloat($8000);
end;

procedure TForm1.ComboBoxNodeIDsChange(Sender: TObject);
begin
  UpdateUI
end;

procedure TForm1.ComboBoxNodeIDsDropDown(Sender: TObject);
var
  i: Integer;
begin
  ComboBoxNodeIDs.Items.Clear;
  if Assigned(FAliasList) then
  begin
    for i := 0 to AliasList.Count - 1 do
      ComboBoxNodeIDs.AddItem(AliasList[i], nil);
  end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := CanClose;  // stop compiler warning
  if Assigned(FComPortThread) then
  begin
    ComPortThread.SyncReceiveMessageFunc := nil;
    ComPortThread.SyncSendMessageFunc := nil;
    ComPortThread.Terminate;
    FComPortThread := nil;
  end;
  FormLog.HideCallback := nil;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  {$IFDEF DARWIN}
    AppMenu := nil;
    AppSep1Cmd := nil;
    AppPrefCmd := nil;
  {$ENDIF}
  AppAboutCmd := nil;
  FShownOnce := False;
  FComPortThread := nil;
  FHelper := TOpenLCBMessageHelper.Create;
  FNodeList := TList.Create;
  FAliasList := TStringList.Create;
  AliasList.Duplicates := dupIgnore;
  FReceivedCallback := nil;
  FDatagram := TDatagram.Create;
  FAddressSpaceReadHelper := TAddressSpaceReadWriteHelper.Create;
  FAddressSpaceWriteHelper := TAddressSpaceReadWriteHelper.Create;
  FSendHelper := TOpenLCBMessageHelper.Create;
  FSNIP_Stream := TMemoryStream.Create;
  FActiveSpace := nil;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FHelper);
  FreeNodes;
  FreeAndNil(FNodeList);
  FreeAndNil(FAliasList);
  FreeAndNil(FDatagram);
  FreeAndNil(FAddressSpaceReadHelper);
  FreeAndNil(FAddressSpaceWriteHelper);
  FreeAndNil(FSendHelper);
  FreeAndNil(FSNIP_Stream);
end;

procedure TForm1.ActionToolsCOMPortSettingsExecute(Sender: TObject);
begin
  FormComPort.Show;
end;

procedure TForm1.ActionAliasTreeClearExecute(Sender: TObject);
begin
  FormAliasTree.TreeView.Items.BeginUpdate;
  FormAliasTree.TreeView.Items.Clear;
  FormAliasTree.TreeView.Items.EndUpdate;
end;

procedure TForm1.ActionAliasTreeRebuildExecute(Sender: TObject);
var
  i, j: Integer;
  RootItem, Item, ChildItem: TTreeNode;
  Node: TNode;
begin
  ActionAliasTreeClear.Execute;
  FormAliasTree.TreeView.Items.BeginUpdate;
  try
    RootItem := FormAliasTree.TreeView.Items.Add(nil, 'Node Alias IDs');
    RootItem.ImageIndex := 32;
    RootItem.SelectedIndex := 32;
    for i := 0 to NodeList.Count - 1 do
    begin
      Node := TNode( NodeList[i]);
      Item := FormAliasTree.TreeView.Items.AddChild(RootItem, '0x' + IntToHex( Node.AliasID, 4));
      Item.ImageIndex := 30;
      Item.SelectedIndex := 30;
      for j := 0 to Node.MessageList.Count - 1 do
      begin
        ChildItem := FormAliasTree.TreeView.Items.AddChild(Item, Node.MessageList[j]);
        ChildItem.ImageIndex := 31;
        ChildItem.SelectedIndex := 31;
        Helper.Decompose(Node.MessageList[j]);
        ChildItem := FormAliasTree.TreeView.Items.AddChild(ChildItem, MTI_ToString(Helper.MTI));
        ChildItem.ImageIndex := 2;
        ChildItem.SelectedIndex := 2;
      end;
    end;
  finally
    FormAliasTree.TreeView.Items.EndUpdate;
  end;
end;

procedure TForm1.ActionF0Execute(Sender: TObject);
begin
  FunctionSend(ActionF0, 0, True);
end;

procedure TForm1.ActionF10Execute(Sender: TObject);
begin
  FunctionSend(ActionF10, 10, True);
end;

procedure TForm1.ActionF11Execute(Sender: TObject);
begin
  FunctionSend(ActionF11, 11, True);
end;

procedure TForm1.ActionF12Execute(Sender: TObject);
begin
  FunctionSend(ActionF12, 12, True);
end;

procedure TForm1.ActionF13Execute(Sender: TObject);
begin
  FunctionSend(ActionF13, 13, True);
end;

procedure TForm1.ActionF1Execute(Sender: TObject);
begin
  FunctionSend(ActionF1, 1, True);
end;

procedure TForm1.ActionF2Execute(Sender: TObject);
begin
  FunctionSend(ActionF2, 2, True);
end;

procedure TForm1.ActionF3Execute(Sender: TObject);
begin
  FunctionSend(ActionF3, 3, True);
end;

procedure TForm1.ActionF4Execute(Sender: TObject);
begin
  FunctionSend(ActionF4, 4, True);
end;

procedure TForm1.ActionF5Execute(Sender: TObject);
begin
  FunctionSend(ActionF5, 5, True);
end;

procedure TForm1.ActionF6Execute(Sender: TObject);
begin
  FunctionSend(ActionF6, 6, True);
end;

procedure TForm1.ActionF7Execute(Sender: TObject);
begin
  FunctionSend(ActionF7, 7, True);
end;

procedure TForm1.ActionF8Execute(Sender: TObject);
begin
  FunctionSend(ActionF8, 8, True);
end;

procedure TForm1.ActionF9Execute(Sender: TObject);
begin
  FunctionSend(ActionF9, 9, True);
end;

function TForm1.GetConnected: Boolean;
begin
  Result := Assigned(ComPortThread)
end;

function TForm1.GetThrottleAliasID: Word;
begin
  Result := StrToInt( EditThrottleAlias.Text);
end;

function TForm1.GetTrainAliasID: Word;
begin
  if ComboBoxNodeIDs.ItemIndex <> -1 then
    Result := StrToInt( ComboBoxNodeIDs.Items[ComboBoxNodeIDs.ItemIndex])
  else
    Result := 0;
end;

procedure TForm1.SetReceivedCallback(AValue: TMessageCallback);
begin
  if FReceivedCallback=AValue then Exit;
  FReceivedCallback:=AValue;
  UpdateUI
end;

procedure TForm1.FreeNodes;
var
  i: Integer;
begin
  try
    for i := 0 to NodeList.Count - 1 do
      TObject( NodeList[i]).Free;
  finally
    NodeList.Count := 0;
  end;
end;

function TForm1.FindNode(AliasID: Word): TNode;
var
  i: Integer;
begin
  i := 0;
  Result := nil;
  while (i < NodeList.Count) and not Assigned(Result) do
  begin
    if TNode( NodeList[i]).AliasID = AliasID then
      Result := TNode( NodeList[i]);
    Inc(i);
  end;
  if not Assigned(Result) then
  begin
    Result := TNode.Create;
    Result.AliasID := AliasID;
    NodeList.Add(Result);
  end;
end;

procedure TForm1.SyncReceiveMessage(MessageStr: String);
begin
  if FormLog.Visible then
  begin
    FormLog.SynMemo.Lines.BeginUpdate;
    FormLog.SynMemo.Text := FormLog.SynMemo.Text + DetailedMessageOutput( MessageStr);
    FormLog.SynMemo.CaretY := FormLog.SynMemo.LineHeight * FormLog.SynMemo.Lines.Count;
    FormLog.SynMemo.Lines.EndUpdate;
  end;

  if Length(MessageStr) > 1 then
  begin
    if (MessageStr[1] = ':') and (MessageStr[2] = 'X') then
    begin
      Helper.Decompose(MessageStr);
      FindNode(Helper.SourceAliasID).MessageList.Add(MessageStr);

      case Helper.MTI of
        MTI_VERIFIED_NODE_ID_NUMBER : AliasList.Add( '0x' + IntToHex(Helper.SourceAliasID, 4));
        MTI_FRAME_TYPE_DATAGRAM_ONLY_FRAME,
        MTI_FRAME_TYPE_DATAGRAM_FRAME_END :
          begin
            if Helper.DestinationAliasID = ThrottleAliasID then
            begin;
              ReplyHelper.Load(ol_OpenLCB, MTI_DATAGRAM_OK_REPLY, ThrottleAliasID, TrainAliasID, 2, 0, 0, 0, 0, 0, 0, 0, 0);
              ComPortThread.Add(ReplyHelper.Encode);
            end;
          end;
      end;

      if Assigned(FReceivedCallback) then
        ReceivedCallback(Helper);
    end
  end;
end;

procedure TForm1.SyncSendMessage(MessageStr: String);
begin
  if FormLog.Visible then
  begin
    MessageStr := MessageStr;
    FormLog.SynMemo.Lines.BeginUpdate;
    FormLog.SynMemo.Text := FormLog.SynMemo.Text + DetailedMessageOutput( MessageStr);
    FormLog.SynMemo.CaretY := FormLog.SynMemo.LineHeight * FormLog.SynMemo.Lines.Count;
    FormLog.SynMemo.Lines.EndUpdate;
  end;

 { if Length(MessageStr) > 1 then
  begin
    if (MessageStr[1] = ':') and (MessageStr[2] = 'X') then
    begin
      Helper.Decompose(MessageStr);
      FindNode(Helper.DestinationAliasID).MessageList.Add(MessageStr);
    end
  end;  }
end;

procedure TForm1.SyncLogWindowHide;
begin
  if Assigned(ComPortThread) then
  begin
    ComPortThread.EnableSendMessages := False;
  end;
  FormAliasTree.Hide
end;

procedure TForm1.SyncMemConfigOptions(AHelper: TOpenLCBMessageHelper);
var
  Cmd: Word;
begin
  TimerTickCount := 0;
  try
    if AHelper.DestinationAliasID = ThrottleAliasID then
    begin;
      if AHelper.SourceAliasID = TrainAliasID then
      begin;

        Datagram.ProcessReceive(AHelper);
        if Datagram.Full then
        begin;
          if (Datagram.Data[0] = $20) and (Datagram.Data[1] and MCP_OP_GET_CONFIG_REPLY = MCP_OP_GET_CONFIG_REPLY) then
          begin;
            Cmd := Datagram.ExtractDataBytesAsInt(2, 3);
            FormMemConfig.CheckGroupMask.Checked[0] := Cmd and MCO_WRITE_UNDER_MASK <> 0;
            FormMemConfig.CheckGroupMask.Checked[1] := Cmd and MCO_UNALIGNED_READS <> 0;
            FormMemConfig.CheckGroupMask.Checked[2] := Cmd and MCO_ACDI_MFG_READS <> 0;
            FormMemConfig.CheckGroupMask.Checked[3] := Cmd and MCO_ACDI_USER_READS <> 0;
            FormMemConfig.CheckGroupMask.Checked[4] := Cmd and MCO_ACDI_USER_WRITES <> 0;
            FormMemConfig.CheckGroupMask.Checked[5] := Cmd and MCO_RESERVED <> 0;

            Cmd := Datagram.Data[4];
            FormMemConfig.CheckGroupLength.Checked[0] := Cmd and MCWL_ONE_BYTE <> 0;
            FormMemConfig.CheckGroupLength.Checked[1] := Cmd and MCWL_TWO_BYTE <> 0;
            FormMemConfig.CheckGroupLength.Checked[2] := Cmd and MCWL_FOUR_BYTE <> 0;
            FormMemConfig.CheckGroupLength.Checked[3] := Cmd and MCWL_64_BYTE <> 0;
            FormMemConfig.CheckGroupLength.Checked[4] := Cmd and MCWL_ARBITRARY_BYTE <> 0;
            FormMemConfig.CheckGroupLength.Checked[5] := Cmd and MCWL_STREAM_WRITE_SUPPORTED <> 0;
            FormMemConfig.CheckGroupLength.Checked[5] := Cmd and MCWL_RESERVED <> 0;

            ReceivedCallback := nil;
          end;
        end;
      end;
    end;
  except
    ReceivedCallback := nil;
  end;
end;

procedure TForm1.SyncMemConfigReadAddressSpaces(AHelper: TOpenLCBMessageHelper);
var
  Item: TListItem;
  i, iSpace: Integer;
begin
  TimerTickCount := 0;
  try
    if AHelper.DestinationAliasID = ThrottleAliasID then
    begin;
      if AHelper.SourceAliasID = TrainAliasID then
      begin;
        Datagram.ProcessReceive(AHelper);
        if Datagram.Full then
        begin;
          if (Datagram.Data[0] = $20) and (Datagram.Data[1] = MCP_OP_GET_CONFIG_REPLY) then
          begin;
            // Clear the Space Array
            for i := 0 to Length(FAddressSpaces) - 1 do
              AddressSpaces[i].Free;
            SetLength(FAddressSpaces, 0);
            // Clear the List
            FormMemConfig.ListViewAddressSpace.Items.Clear;

            // Create new Space Array
            SetLength(FAddressSpaces, Datagram.Data[5] - Datagram.Data[6] + 1);
            for i := 0 to Length(FAddressSpaces) - 1 do
              AddressSpaces[i] := TAddressSpace.Create;
            iSpace := 0;

            for i := Datagram.Data[5] downto Datagram.Data[6] do
            begin
              // Setup the Space Object
              AddressSpaces[iSpace].Space := i;

              Item := FormMemConfig.ListViewAddressSpace.Items.Add;
              Item.Caption := '0x' + IntToHex(i, 2);
              case i of
                255 : Item.Caption := Item.Caption + ' [Configuration Definition Info (CDI)]';
                254 : Item.Caption := Item.Caption + ' [All]';
                253 : Item.Caption := Item.Caption + ' [Configuration]';
                252 : Item.Caption := Item.Caption + ' [ACDI Mfg]';
                251 : Item.Caption := Item.Caption + ' [ACDI User]';
                250 : Item.Caption := Item.Caption + ' [Function Defintion Info (FDI)]';
              else
                Item.Caption := Item.Caption + '[Unknown]';
              end;
              Item.ImageIndex := 0;
              Inc(iSpace);
            end;
            FormMemConfig.ListViewAddressSpace.AutoWidthLastColumn := not FormMemConfig.ListViewAddressSpace.AutoWidthLastColumn;
            FormMemConfig.ListViewAddressSpace.AutoWidthLastColumn := True;

            if Length(FAddressSpaces) > 0 then
            begin
              // Start the enumeration all the Spaces to gather the information
              Datagram.Clear;
              AddressEnumIndex := 0;
              Helper.Load(ol_OpenLCB, MTI_FRAME_TYPE_DATAGRAM_ONLY_FRAME, ThrottleAliasID, TrainAliasID, 3, $20, MCP_OP_GET_ADD_SPACE_INFO, AddressSpaces[AddressEnumIndex].Space, 0, 0, 0, 0, 0);
              ComPortThread.Add(Helper.Encode);
              TimerTickCount := 0;
              TimerTickTimeout := 5;
              ReceivedCallback := @SyncMemConfigEnumAddressSpaces;
            end else
            begin
              ReceivedCallback := nil;
            end;
          end;
        end;
      end;
    end;
  except
    ReceivedCallback := nil;
  end;
end;

procedure TForm1.SyncMemConfigEnumAddressSpaces(AHelper: TOpenLCBMessageHelper);
begin
  TimerTickCount := 0;
  try
    if AHelper.DestinationAliasID = ThrottleAliasID then
    begin;
      if AHelper.SourceAliasID = TrainAliasID then
      begin;
        Datagram.ProcessReceive(AHelper);
        if Datagram.Full then
        begin
          if (Datagram.Data[0] = $20) and (Datagram.Data[1] and MCP_OP_GET_ADD_SPACE_INFO_REPLY = MCP_OP_GET_ADD_SPACE_INFO_REPLY) then
          begin
            if (Datagram.Data[1] and MCP_OP_GET_ADD_SPACE_INFO_REPLY_PRESENT = MCP_OP_GET_ADD_SPACE_INFO_REPLY_PRESENT) then
            begin
              AddressSpaces[AddressEnumIndex].FPresent := True;
               AddressSpaces[AddressEnumIndex].SpaceReturned := Datagram.Data[2];
              AddressSpaces[AddressEnumIndex].HiAddr := Datagram.ExtractDataBytesAsInt(3, 6);
              AddressSpaces[AddressEnumIndex].Flags := Datagram.Data[7];
              if Datagram.Size >= 12 then
               AddressSpaces[AddressEnumIndex].LoAddr :=  Datagram.ExtractDataBytesAsInt(8, 11);
              if Datagram.Size > 12 then
                AddressSpaces[AddressEnumIndex].Description := PAnsiChar( Datagram.Data[12]);       // Is a POINTER 8 bit/16 bit/32 bit/64 bit?    how to handled that?????
            end else
            begin
              AddressSpaces[AddressEnumIndex].FPresent := False;
            end;

            Inc(FAddressEnumIndex);
            if AddressEnumIndex < Length(AddressSpaces) then
            begin
              // Get the next space info
              Datagram.Clear;
              Helper.Load(ol_OpenLCB, MTI_FRAME_TYPE_DATAGRAM_ONLY_FRAME, ThrottleAliasID, TrainAliasID, 3, $20, MCP_OP_GET_ADD_SPACE_INFO, AddressSpaces[AddressEnumIndex].Space, 0, 0, 0, 0, 0);
              ComPortThread.Add(Helper.Encode);
            end else
            begin
              // Complete
              if FormMemConfig.ListViewAddressSpace.Items.Count > 0 then
              begin
                FormMemConfig.ListViewAddressSpace.Selected := FormMemConfig.ListViewAddressSpace.Items[0];
                FormMemConfig.ListViewAddressSpace.ItemFocused := FormMemConfig.ListViewAddressSpace.Items[0];
                FormMemConfig.ListViewAddressSpace.Selected := FormMemConfig.ListViewAddressSpace.Items[0];
              end;
              ReceivedCallback := nil;
            end
          end
        end
      end;
    end
  except
    ReceivedCallback := nil;
  end
end;

procedure TForm1.SyncMemConfigAddressSpaceRead(AHelper: TOpenLCBMessageHelper);
var
  ByteArray: TByteArray;
  s: Ansistring;
  ADoc: TXMLDocument;
  i: Integer;
begin
  TimerTickCount := 0;
  try
    if AHelper.DestinationAliasID = ThrottleAliasID then
    begin;
      if AHelper.SourceAliasID = TrainAliasID then
      begin;
        Datagram.ProcessReceive(AHelper);
        if Datagram.Full then
        begin
          // If the node returned less than we asked for in the Block Read Count then it must be out of bytes and time to finish
          if AddressSpaceReadHelper.Terminate or (Datagram.Size - AddressSpaceReadHelper.ProtocolHeader < AddressSpaceReadHelper.BlockReadCount) then
          begin
            // Can't Terminate anymore
            ActionMemConfigTerminate.Enabled := False;

            Datagram.CopyToStream(AddressSpaceReadHelper.Stream, AddressSpaceReadHelper.ProtocolHeader, -1);

            if not FormMemConfig.CheckBoxUseHexViewer.Checked and (AddressSpaceReadHelper.AddressSpace = MSI_CDI) then
            begin
              FormMemConfig.SynEditAddressSpaceData.Visible := True;
              FormMemConfig.KHexEditor.Visible := False;

              FormMemConfig.SynEditAddressSpaceData.BeginUpdate;
              try
                FormMemConfig.SynEditAddressSpaceData.ClearAll;

                if FormMemConfig.SynEditAddressSpaceData.Highlighter = FormMemConfig.SynXMLSyn then
                begin
                  ADoc := nil;
                  s := Trim( AddressSpaceReadHelper.ReturnStreamAsString);
                  AddressSpaceReadHelper.Stream.Clear;
                  for i := 1 to Length(s) do
                    AddressSpaceReadHelper.Stream.WriteBuffer( Ord(s[i]), 1);
                  AddressSpaceReadHelper.Stream.Seek(0, soFromBeginning);
                  try
                    ReadXMLFile(ADoc, AddressSpaceReadHelper.Stream);
                    WriteXMLFile(ADoc, AddressSpaceReadHelper.Stream);
                  except
                    FormMemConfig.SynEditAddressSpaceData.Lines.Add('************************');
                    FormMemConfig.SynEditAddressSpaceData.Lines.Add('ERROR');
                    FormMemConfig.SynEditAddressSpaceData.Lines.Add('************************');
                    FormMemConfig.SynEditAddressSpaceData.Lines.Add('FAILED TO PARSE XML FILE');
                    FormMemConfig.SynEditAddressSpaceData.Lines.Add('************************');
                  end;
                  ADoc.Free;
                  FormMemConfig.SynEditAddressSpaceData.Text := FormMemConfig.SynEditAddressSpaceData.Text + AddressSpaceReadHelper.ReturnStreamAsString;
                end else
                begin
                  // Convert null characters to the word <null>
                  s := StringReplace(AddressSpaceReadHelper.ReturnStreamAsString, #0, '<null>', [rfReplaceAll, rfIgnoreCase]);
                  // Convert the Version number to the ASCII char for the number.  This should last many years to Version 8......
                  s := StringReplace(s, #1, '1', [rfReplaceAll, rfIgnoreCase]);
                  s := StringReplace(s, #2, '2', [rfReplaceAll, rfIgnoreCase]);
                  s := StringReplace(s, #3, '3', [rfReplaceAll, rfIgnoreCase]);
                  s := StringReplace(s, #4, '4', [rfReplaceAll, rfIgnoreCase]);
                  s := StringReplace(s, #5, '5', [rfReplaceAll, rfIgnoreCase]);
                  s := StringReplace(s, #6, '6', [rfReplaceAll, rfIgnoreCase]);
                  s := StringReplace(s, #7, '7', [rfReplaceAll, rfIgnoreCase]);
                  s := StringReplace(s, #8, '8', [rfReplaceAll, rfIgnoreCase]);
                  FormMemConfig.SynEditAddressSpaceData.Text := s;
                end;

              finally
                FormMemConfig.SynEditAddressSpaceData.EndUpdate
              end
            end else
            begin
              FormMemConfig.SynEditAddressSpaceData.Visible := False;
              FormMemConfig.KHexEditor.Visible := True;

              AddressSpaceReadHelper.Stream.Seek(0, soFromBeginning);
              FormMemConfig.KHexEditor.LoadFromStream(AddressSpaceReadHelper.Stream);
            end;
            ReceivedCallback := nil;
          end else
          begin
            // Not Done get some more

            Datagram.CopyToStream(AddressSpaceReadHelper.Stream, AddressSpaceReadHelper.ProtocolHeader, -1);

            ByteArray[0] := 0; // Stop compiler waring
            Helper.IntToByteArray(AddressSpaceReadHelper.AddressCurrent, ByteArray);

            if (AddressSpaceReadHelper.Command and $03 <> 0) then
              Helper.Load(ol_OpenLCB, MTI_FRAME_TYPE_DATAGRAM_ONLY_FRAME, ThrottleAliasID, TrainAliasID, 7, $20, AddressSpaceReadHelper.Command, ByteArray[3], ByteArray[2], ByteArray[1], ByteArray[0], AddressSpaceReadHelper.BlockReadCount, 0)
            else
              Helper.Load(ol_OpenLCB, MTI_FRAME_TYPE_DATAGRAM_ONLY_FRAME, ThrottleAliasID, TrainAliasID, 8, $20, AddressSpaceReadHelper.Command, ByteArray[3], ByteArray[2], ByteArray[1], ByteArray[0], AddressSpaceReadHelper.AddressSpace, AddressSpaceReadHelper.BlockReadCount);

            AddressSpaceReadHelper.AddressCurrent := AddressSpaceReadHelper.AddressCurrent + AddressSpaceReadHelper.BlockReadCount;

            ComPortThread.Add(Helper.Encode);

          end;
          Datagram.Clear;
        end;
      end;
    end;
  except
    ReceivedCallback := nil;
  end;
end;

procedure TForm1.SyncMemConfigAddressSpaceWrite(AHelper: TOpenLCBMessageHelper);
var
  ByteArray: TByteArray;
begin
  TimerTickCount := 0;
  ByteArray[0] := 0;
  try
    if AHelper.DestinationAliasID = ThrottleAliasID then
      if AHelper.SourceAliasID = TrainAliasID then
      begin
        case Helper.MTI of
          MTI_DATAGRAM_OK_REPLY :
            begin
              if not Datagram.SendComplete then
              begin
                // Update the Datagram Header to the new address for the next Datagram packet
                AddressSpaceWriteHelper.AddressCurrent := AddressSpaceWriteHelper.AddressLo + Datagram.SendStream.Position;
                Helper.IntToByteArray(AddressSpaceWriteHelper.AddressCurrent, ByteArray);
                Datagram.SendHeader[2] := ByteArray[3];
                Datagram.SendHeader[3] := ByteArray[2];
                Datagram.SendHeader[4] := ByteArray[1];
                Datagram.SendHeader[5] := ByteArray[0];
                Datagram.Full := True;   // Ready to send next datagram message
                Datagram.SendPacketCurrentIndex := 0;                                     // Would like this to be automatic but not sure how to do it
                while Datagram.Full do                                                    // Wait until the Datagram is sent and not Full
                begin
                  Datagram.ProcessSend(Helper, ThrottleAliasID, TrainAliasID);
                  ComPortThread.Add(Helper.Encode);
                end;
                TimerTickCount := 0;
                TimerTickTimeout := 5;
              end else
              begin
                ReceivedCallback := nil;
                TimerTickCount := 0;
              end;
            end;
          MTI_DATAGRAM_REJECTED_REPLY :
          begin
             ReceivedCallback := nil;
             TimerTickCount := 0;
          end;
        end;
      end
  except
    ReceivedCallback := nil;
    TimerTickCount := 0;
  end
end;

procedure TForm1.SyncMemConfigSendSNIP(AHelper: TOpenLCBMessageHelper);
var
  Buffer: Byte;
  NullCount, i: Integer;
begin
  TimerTickCount := 0;
  try
    if (AHelper.DestinationAliasID = ThrottleAliasID) and (AHelper.SourceAliasID = TrainAliasID) and (AHelper.MTI = MTI_SIMPLE_NODE_INFO_REPLY) then
    begin;
      FormMemConfig.SynEditAddressSpaceData.Visible := False;
      FormMemConfig.KHexEditor.Visible := True;
      SNIP_Stream.Seek(0, soFromEnd);
      SNIP_Stream.Write(AHelper.Data[2], AHelper.DataCount - 2);     // 2 bytes are the address
      SNIP_Stream.Seek(0, soFromBeginning);
      FormMemConfig.KHexEditor.LoadFromStream(SNIP_Stream);

      Buffer := 0;
      NullCount := 0;
      SNIP_Stream.Seek(0, soFromBeginning);
      for i := 0 to SNIP_Stream.Size - 1 do
      begin
        SNIP_Stream.Read(Buffer, 1);
        if Buffer = $00 then
          Inc(NullCount)
      end;
      if NullCount = 6 then
        ReceivedCallback := nil;
    end
  except
    ReceivedCallback := nil;
  end;
end;

procedure TForm1.SyncFindAllTrainProducers(AHelper: TOpenLCBMessageHelper);
var
  Item: TListItem;
begin
  TimerTickCount := 0;
 try
    // Waits for the TimerReply to Timeout
    if AHelper.MTI = MTI_PRODUCER_IDENTIFIED_SET then
      if EqualEvents(PEventID( @AHelper.Data), @EVENT_TRAIN) then
      begin
        Item := ListViewSearchList.Items.Add;
        Item.Caption := '0x' + IntToHex(AHelper.SourceAliasID, 4);
      end;
  except
    ReceivedCallback := nil;
  end;
end;

procedure TForm1.SyncIsIdleProxyProducers(AHelper: TOpenLCBMessageHelper);
var
  Item: TListItem;
begin
 TimerTickCount := 0;
 try
    // Waits for the TimerReply to Timeout
    if AHelper.MTI = MTI_PRODUCER_IDENTIFIED_SET then
      if EqualEvents(PEventID( @AHelper.Data), @EVENT_TRAIN_PROXY_IDLE) then
      begin
        Item := ListViewSearchList.Items.Add;
        Item.Caption := '0x' + IntToHex(AHelper.SourceAliasID, 4);
      end;
  except
    ReceivedCallback := nil;
  end;
end;

procedure TForm1.SyncIsInUseProxyProducers(AHelper: TOpenLCBMessageHelper);
var
  Item: TListItem;
begin
 TimerTickCount := 0;
 try
    // Waits for the TimerReply to Timeout
    if AHelper.MTI = MTI_PRODUCER_IDENTIFIED_SET then
      if EqualEvents(PEventID( @AHelper.Data), @EVENT_TRAIN_PROXY_INUSE) then
      begin
        Item := ListViewSearchList.Items.Add;
        Item.Caption := '0x' + IntToHex(AHelper.SourceAliasID, 4);
      end;
  except
    ReceivedCallback := nil;
  end;
end;

procedure TForm1.SyncProxiedDccAddressProducers(AHelper: TOpenLCBMessageHelper);
var
  Item: TListItem;
  Address: Word;
begin
  TimerTickCount := 0;
  try
     // Waits for the TimerReply to Timeout
     if AHelper.MTI = MTI_PRODUCER_IDENTIFIED_SET then
      if (AHelper.Data[0] = $06) and (AHelper.Data[1] = $01) then               // Is is a DCC event
      begin
        Address := ((AHelper.Data[6] shl 8) and $3FFF) or AHelper.Data[7];
        if (Address = SpinEditDccProxy.Value) then
        begin
          Item := ListViewSearchList.Items.Add;
          Item.Caption := '0x' + IntToHex(AHelper.SourceAliasID, 4);
        end;
      end;
   except
     ReceivedCallback := nil;
   end;
end;

procedure TForm1.SyncThrottleSpeedChange(AHelper: TOpenLCBMessageHelper);
begin
  TimerTickCount := 0;
  try
    ReceivedCallback := nil;
  except
    ReceivedCallback := nil;
  end;
end;

function TForm1.DetailedMessageOutput(MessageString: String): String;
var
  j, S_Len: Integer;
  f: single;
  Address: Word;
begin
  if Helper.Decompose(MessageString) then
  begin
    Result := MessageString;
    S_Len := Length(Result);
    for j := 0 to (28-S_Len) do
      Result := Result + ' ';

    if Helper.SourceAliasID = ThrottleAliasID then
      Result := Result + '  Send:   '
    else
      Result := Result + '  Recive: ';

    Result := Result + 'From = 0x' + IntToHex( Helper.SourceAliasID, 4);
    Result := Result + '   MTI: ' + MTI_ToString(Helper.MTI);

    if (Helper.MTI = MTI_PRODUCER_IDENDIFY) or (Helper.MTI = MTI_PRODUCER_IDENTIFIED_SET) or (Helper.MTI = MTI_PRODUCER_IDENTIFIED_CLEAR) or
      (Helper.MTI = MTI_PRODUCER_IDENTIFIED_UNKNOWN) or (Helper.MTI = MTI_CONSUMER_IDENTIFY) or (Helper.MTI = MTI_CONSUMER_IDENTIFIED_SET) or
      (Helper.MTI = MTI_CONSUMER_IDENTIFIED_CLEAR) or (Helper.MTI = MTI_CONSUMER_IDENTIFIED_UNKNOWN)
    then begin
      S_Len := Length(Result);
      for j := 94 downto S_Len do
        Result := Result + ' ';

        Result := Result + 'EventID: ' + EventIDToString(@Helper.Data);
    end;

    if Helper.MTI = MTI_TRACTION_PROTOCOL then
    begin
      S_Len := Length(Result);
      for j := 94 downto S_Len do
        Result := Result + ' ';

      case Helper.Data[2] and TRACTION_PROTOCOL_MASK of
        TRACTION_DCC:
          begin
            case Helper.Data[2] and TRACTION_OP_MASK of
              TRACTION_OP_SPEED_DIR :
                begin
                  Result := Result + 'DCC Speed/Dir Operation; Speed = 0x';
                  Result := Result + IntToHex( Helper.Data[3] and %00011111, 2) + '; Direction = ';
                  if Helper.Data[3] and %00100000 = %00100000 then
                    Result := Result + 'Fwd; Steps = '
                  else
                    Result := Result + 'Rev; Steps = ';
                  Result := Result + IntToStr(Helper.Data[4]) + '; Raw Speed Byte = 0x' + IntToHex(Helper.Data[3], 2)
                end;
              TRACTION_OP_FUNCTION :
                begin
                  Result := Result + 'DCC Function Operation; Function Number = ';
                  if Helper.Data[3] = $00 then
                    Result := Result + '28 Functions;'
                  else
                    Result := Result + '32k Functions;';
                  Result := Result + '  Address = ' + IntToStr( (Helper.Data[4] shl 8) or Helper.Data[5]);
                  if Helper.Data[6] = $00 then
                    Result := Result + '  Value = OFF'
                  else
                    Result := Result + '  Value = ON ';
                end;
              TRACTION_OP_PROXY_MGMT :
                begin
                  Result := Result + 'DCC Proxy Allocate Operation; ';

                  Address := ((Helper.Data[4] shl 8) or Helper.Data[5]) and $3FFF;  // Strip off the Extended bits if they are there
                  if Helper.Data[4] and $C0 = $C0 then
                    Result := Result + 'EVENT_TRAIN_DCC_ADDRESS : Extended Address;  Address = ' + IntToStr(Address) + ';  (0x' + IntToHex(Address, 4) + ')'
                  else
                    Result := Result + 'EVENT_TRAIN_DCC_ADDRESS : Short Address;     Address = ' + IntToStr(Address) + ';  (0x' + IntToHex(Address, 4) + ')'

                end
              else
                Result := Result + 'Unknown DCC Traction Operation';
            end;
          end;
        TRACTION_OLCB:
          begin
            case Helper.Data[2] and TRACTION_OP_MASK of
              TRACTION_OP_SPEED_DIR :
                begin
                  Result := Result + 'OLCB Speed/Dir Operation; Speed = ';

                  f := HalfToFloat( (Helper.Data[3] shl 8) or Helper.Data[4]);
                  if f= 0 then
                  begin
                    if DWord( f) and $80000000 = $80000000 then
                      Result := Result + '-0.0'
                    else
                      Result := Result + '+0.0'
                  end else
                    Result := Result + IntToStr( round(f));
                end;
              TRACTION_OP_FUNCTION :
                begin
                  Result := Result + 'OLCB Traction Operation; Speed = ';
                end
              else
                Result := Result + 'Unknown OLCB Traction Operation';
            end
          end;
        else
          Result := Result + 'Unknown Traction procotol';
      end;
    end
  end;
end;

procedure TForm1.FunctionSend(AnAction: TAction; FunctionID: Word; Toggle: Boolean);
var
  AddressHi, AddressLo: Byte;
begin
  if Assigned(FComPortThread) then
  begin
    if Toggle then
    begin
      if AnAction.Tag = 0 then
      begin
        AnAction.Tag := 1
      end
      else begin
        AnAction.Tag := 0
      end;
    end;
      case TabControlThottle.TabIndex of
    0: begin
       end;
    1: begin
         AddressHi := (FunctionID shr 8) and $00FF;
         AddressLo := FunctionID and $00FF;
         Helper.Load(ol_OpenLCB, MTI_TRACTION_PROTOCOL, ThrottleAliasID, StrToInt(ListViewSearchList.Selected.Caption), 7, $00, $00, TRACTION_DCC or TRACTION_OP_FUNCTION, DCC_FUNCTION_28, AddressHi, AddressLo, AnAction.Tag, 0);
         ComPortThread.Add(Helper.Encode);
       end;
    end;
  end;
end;

procedure TForm1.ListViewAddressSpaceSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if Selected and Assigned(Item) and Assigned(FComPortThread) then
  begin
    ActiveSpace := AddressSpaces[Item.Index];

    // Update the Address Space Info Tab
    FormMemConfig.EditAddressSpaceIDRequested.Text := '0x' + IntToHex(ActiveSpace.Space, 2) + ' [' + IntToStr(ActiveSpace.Space) + ']';
    FormMemConfig.EditAddressSpaceIDReturned.Text := '0x' + IntToHex(ActiveSpace.SpaceReturned, 2) + ' [' + IntToStr(ActiveSpace.SpaceReturned) + ']';
    FormMemConfig.EditAddressSpaceHiAddress.Text := '0x' + IntToHex(ActiveSpace.HiAddr, 8) + ' [' + IntToStr(ActiveSpace.HiAddr) + ' bytes]';
    FormMemConfig.EditAddressSpaceFlags.Text := '0x' + IntToHex(ActiveSpace.Flags, 1) + ' [' + IntToStr(ActiveSpace.Flags) + ']';
    FormMemConfig.CheckBoxAddressSpacePresent.Checked := ActiveSpace.Present;
    if ActiveSpace.IsReadOnly then
    begin
      FormMemConfig.CheckGroupAddressSpaceFlags.Checked[0] := True;
      FormMemConfig.CheckGroupAddressSpaceFlags.Checked[1] := False;
    end else
    begin
      FormMemConfig.CheckGroupAddressSpaceFlags.Checked[0] := False;
      FormMemConfig.CheckGroupAddressSpaceFlags.Checked[1] := True;
    end;
    if Datagram.Size >= 12 then
      FormMemConfig.EditAddressSpaceLoAddress.Text := '0x' + IntToHex( Datagram.ExtractDataBytesAsInt(8, 11), 8) + ' [' + IntToStr(Datagram.ExtractDataBytesAsInt(8, 11)) + ' bytes]'
    else
      FormMemConfig.EditAddressSpaceLoAddress.Text := '0x00000000' + ' [0 bytes] implied';
    if Datagram.Size > 12 then
      FormMemConfig.EditAddressSpaceDescription.Text := PChar( Datagram.Data[12]);    //  // Is a POINTER 8 bit/16 bit/32 bit/64 bit?    how to handled that?????             >>>>>>>>>>>>>>>>>>>

    // Update the Address Space Read Data Tab
    FormMemConfig.EditAddressSpaceDataReadLoAddress.Text := '0x' + IntToHex(ActiveSpace.LoAddr, 8);
    FormMemConfig.LabelLoAddress.Caption := 'Lo Address [' + IntToHex(ActiveSpace.LoAddr, 8) + ']';
    FormMemConfig.EditAddressSpaceDataReadHiAddress.Text := '0x' + IntToHex(ActiveSpace.HiAddr, 8);
    FormMemConfig.LabelHiAddress.Caption := 'Hi Address [' + IntToHex(ActiveSpace.HiAddr, 8) + ']';
    FormMemConfig.EditAddressSpaceDataReadBytesToRead.Text := '0x' + IntToHex(ActiveSpace.HiAddr - ActiveSpace.LoAddr, 8);

    FormMemConfig.KHexEditor.Visible := True;                                           // Make this default
  end else
  begin
 //   if FormMemConfig.ListViewAddressSpace.SelCount = 0 then
  //    ActiveSpace := nil;
  end;
  UpdateUI
end;


procedure TForm1.UpdateUI;
begin

  Label1.Caption := 'Active Space: 0x' + IntToHex(DWord( ActiveSpace), 8) ;

  if Assigned(ComPortThread) then
  begin
    ActionMemConfigOptions.Enabled := (ReceivedCallback = nil) and ComPortThread.Connected and (ComboBoxNodeIDs.ItemIndex > -1);
    ActionMemConfigSendSNIP.Enabled := (ReceivedCallback = nil) and ComPortThread.Connected;
    FormMemConfig.CheckBoxForceCommonAddressIntoSpace.Enabled := (ReceivedCallback = nil);
    ActionMemConfigReadSpaces.Enabled := (ReceivedCallback = nil) and ComPortThread.Connected and (ComboBoxNodeIDs.ItemIndex > -1);

    ActionFindAllocateTrain.Enabled := not ActionMemConfigTerminate.Enabled and ComPortThread.Connected;
    ActionVerifyNodeID.Enabled := (ReceivedCallback = nil) and ComPortThread.Connected;
    ActionAliasTreeClear.Enabled := (ReceivedCallback = nil);
    ComboBoxNodeIDs.Enabled := (ReceivedCallback = nil) and ComPortThread.Connected;
    EditThrottleAlias.Enabled := (ReceivedCallback = nil);
    EditThrottleNodeID.Enabled := (ReceivedCallback = nil);
    ActionToolsCOMPortSettings.Enabled := (ReceivedCallback = nil) and not ComPortThread.Connected;
    ActionToolsConnect.Enabled := (ReceivedCallback = nil);
    if ActiveSpace <> nil then
    begin
      ActionMemConfigReadSpaceData.Enabled := (ReceivedCallback = nil) and ComPortThread.Connected and (ComboBoxNodeIDs.ItemIndex > -1) and (ActiveSpace.Present); // Check for Preset, it overrides all other flags.
      ActionMemConfigWriteSpaceData.Enabled := (ReceivedCallback = nil) and ComPortThread.Connected and not (ActiveSpace.IsReadOnly) and (ActiveSpace.Present) and (ComboBoxNodeIDs.ItemIndex > -1)
    end else
    begin
      ActionMemConfigWriteSpaceData.Enabled := False;
      ActionMemConfigReadSpaceData.Enabled := False;
    end;
    ActionMemConfigSendSNIP.Enabled := (ReceivedCallback = nil) and ComPortThread.Connected and (ComboBoxNodeIDs.ItemIndex > -1);

    ActionFindAllocateTrain.Enabled := (ReceivedCallback = nil) and ComPortThread.Connected and (ListViewSearchList.SelCount > 0);
    ActionFindAllTrains.Enabled := (ReceivedCallback = nil) and ComPortThread.Connected;
    ActionFindInUseProxies.Enabled := (ReceivedCallback = nil) and ComPortThread.Connected;
    ActionFindIsIdleProxies.Enabled := (ReceivedCallback = nil) and ComPortThread.Connected;
    ActionFindIsDccAddressProxies.Enabled := (ReceivedCallback = nil) and ComPortThread.Connected;
    GroupBoxThrottle.Enabled := ComPortThread.Connected and (ListViewSearchList.SelCount > 0);
    ActionF0.Enabled := (ReceivedCallback = nil) and ComPortThread.Connected and (ListViewSearchList.SelCount > 0);
    ActionF1.Enabled := (ReceivedCallback = nil) and ComPortThread.Connected and (ListViewSearchList.SelCount > 0);
    ActionF2.Enabled := (ReceivedCallback = nil) and ComPortThread.Connected and (ListViewSearchList.SelCount > 0);
    ActionF3.Enabled := (ReceivedCallback = nil) and ComPortThread.Connected and (ListViewSearchList.SelCount > 0);
    ActionF4.Enabled := (ReceivedCallback = nil) and ComPortThread.Connected and (ListViewSearchList.SelCount > 0);
    ActionF5.Enabled := (ReceivedCallback = nil) and ComPortThread.Connected and (ListViewSearchList.SelCount > 0);
    ActionF6.Enabled := (ReceivedCallback = nil) and ComPortThread.Connected and (ListViewSearchList.SelCount > 0);
    ActionF7.Enabled := (ReceivedCallback = nil) and ComPortThread.Connected and (ListViewSearchList.SelCount > 0);
    ActionF8.Enabled := (ReceivedCallback = nil) and ComPortThread.Connected and (ListViewSearchList.SelCount > 0);
    ActionF9.Enabled := (ReceivedCallback = nil) and ComPortThread.Connected and (ListViewSearchList.SelCount > 0);
    ActionF10.Enabled := (ReceivedCallback = nil) and ComPortThread.Connected and (ListViewSearchList.SelCount > 0);
    ActionF11.Enabled := (ReceivedCallback = nil) and ComPortThread.Connected and (ListViewSearchList.SelCount > 0);
    ActionF12.Enabled := (ReceivedCallback = nil) and ComPortThread.Connected and (ListViewSearchList.SelCount > 0);
    ActionF13.Enabled := (ReceivedCallback = nil) and ComPortThread.Connected and (ListViewSearchList.SelCount > 0);
  end else
  begin
    ActionFindAllocateTrain.Enabled := False;
    ActionVerifyNodeID.Enabled := False;
    ComboBoxNodeIDs.Enabled := False;
    ActionMemConfigReadSpaceData.Enabled := False;
    ActionMemConfigOptions.Enabled := False;
    ActionMemConfigSendSNIP.Enabled := False;
    FormMemConfig.CheckBoxForceCommonAddressIntoSpace.Enabled := True;
    EditThrottleAlias.Enabled := True;
    EditThrottleNodeID.Enabled := True;
    ActionToolsCOMPortSettings.Enabled := True;
    ActionToolsConnect.Enabled := True;
    ActionMemConfigReadSpaces.Enabled := True;
    ActionMemConfigWriteSpaceData.Enabled := False;
    ActionMemConfigReadSpaceData.Enabled := False;
    ActionMemConfigSendSNIP.Enabled := False;
    ActionFindAllocateTrain.Enabled := False;
    ActionFindAllTrains.Enabled := False;
    ActionFindInUseProxies.Enabled := False;
    ActionFindIsIdleProxies.Enabled := False;
    ActionFindIsDccAddressProxies.Enabled := False;
    GroupBoxThrottle.Enabled := False;
    ActionF0.Enabled := False;
    ActionF1.Enabled := False;
    ActionF2.Enabled := False;
    ActionF3.Enabled := False;
    ActionF4.Enabled := False;
    ActionF5.Enabled := False;
    ActionF6.Enabled := False;
    ActionF7.Enabled := False;
    ActionF8.Enabled := False;
    ActionF9.Enabled := False;
    ActionF10.Enabled := False;
    ActionF11.Enabled := False;
    ActionF12.Enabled := False;
    ActionF13.Enabled := False;
  end;
end;

end.



