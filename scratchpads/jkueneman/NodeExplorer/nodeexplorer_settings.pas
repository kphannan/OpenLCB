unit nodeexplorer_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLRead, XMLWrite, FileUtil, Forms, Dialogs;


const
  XML_SETTINGS      = 'Settings';
  XML_SETTINGS_NODE = 'Node';
  XML_SETTINGS_ID   = 'ID';
  XML_SETTINGS_ALIAS = 'Alias';
  XML_SETTINGS_COM   = 'COM';
  XML_SETTINGS_PORT  = 'Port';
  XML_SETTINGS_BAUD  = 'Baud';


const
  FILENAME_STANDARD_TEST_FILE_UNIX = '/tests/testMatrixSample.xml';
  FILENAME_STANDARD_TEST_PATH_UNIX = '/tests';
  FILENAME_STANDARD_TEST_FILE_WIN = '\tests\testMatrixSample.xml';
  FILENAME_STANDARD_TEST_PATH_WIN = '\tests';
  FILENAME_OSX_RESOURCES_SUB_PATH = '/Contents/Resources';
  FILENAME_SETTINGS = 'Settings.xml';
  FILENAME_KNOWN_MTI_UNIX = '/Contents/Resources/tests/knownMTI.txt';
  FILENAME_KNOWN_MTI_WIN  = 'tests\knownMTI.txt';

  PATH_UNIX_APPLICATION_PATH = '/usr/share/nodeexplorer';

  DEFAULT_COM_READ_TIMEOUT = 30; // ms

type
{ TSettings }

  TSettings = class
  private
    FApplicationPath: String;
    FBaudRate: DWord;
    FComPort: String;
    FMultiNodeTest: Boolean;
    FPingPongStandardFrameTest: Boolean;
    FProxyNodeAlias: Word;
    FProxyNodeID: Int64;
    FSoftwareFlowControl: Boolean;
    FTargetNodeAlias: Word;
    FTargetNodeID: Int64;
    FTimeoutComRead: Integer;
    FTimeoutStartupRID: Integer;
    FUIRefreshRate: Integer;
    FXMLSettings: TXMLDocument;
    function GetTestMatrixPath: string;
    function GetTextMatrixFile: string;
  public
    property ProxyNodeAlias: Word read FProxyNodeAlias write FProxyNodeAlias;
    property ProxyNodeID: Int64 read FProxyNodeID write FProxyNodeID;
    property TargetNodeAlias: Word read FTargetNodeAlias write FTargetNodeAlias;
    property TargetNodeID: Int64 read FTargetNodeID write FTargetNodeID;
    property ComPort: String read FComPort write FComPort;
    property BaudRate: DWord read FBaudRate write FBaudRate;
    property XMLSettings: TXMLDocument read FXMLSettings write FXMLSettings;
    property ApplicationPath: String read FApplicationPath write FApplicationPath;
    property TestMatrixPath: string read GetTestMatrixPath;
    property TestMatrixFile: string read GetTextMatrixFile;
    property MultiNodeTest: Boolean read FMultiNodeTest write FMultiNodeTest;
    property TimeoutComRead: Integer read FTimeoutComRead write FTimeoutComRead;
    property TimeoutStartupRID: Integer read FTimeoutStartupRID write FTimeoutStartupRID;
    property PingPongStandardFrameTest: Boolean read FPingPongStandardFrameTest write FPingPongStandardFrameTest;
    property SoftwareFlowControl: Boolean read FSoftwareFlowControl write FSoftwareFlowControl;
    property UIRefreshRate: Integer read FUIRefreshRate write FUIRefreshRate;
    constructor Create;
    procedure ReadSettings;
    procedure WriteSettings;
  end;

var
  Settings: TSettings;

implementation

{$ifdef Windows}
uses Windows;
{$endif}
{$IFDEF Darwin}
uses
  MacOSAll;
{$ENDIF}

{ TSettings }

function TSettings.GetTestMatrixPath: string;
begin
  {$IFDEF Windows}
    Result := ApplicationPath + FILENAME_STANDARD_TEST_PATH_WIN;
  {$ENDIF}
  {$IFDEF darwin}
    Result := ApplicationPath + FILENAME_OSX_RESOURCES_SUB_PATH + FILENAME_STANDARD_TEST_PATH_UNIX;
  {$ENDIF}
  {$IFDEF Linux}
    Result := ApplicationPath + FILENAME_STANDARD_TEST_PATH_UNIX;
  {$ENDIF}
end;

function TSettings.GetTextMatrixFile: string;
begin
  {$IFDEF Windows}
    Result := ApplicationPath + FILENAME_STANDARD_TEST_FILE_WIN;
  {$ENDIF}
  {$IFDEF darwin}
    Result := ApplicationPath + FILENAME_OSX_RESOURCES_SUB_PATH + FILENAME_STANDARD_TEST_FILE_UNIX;
  {$ENDIF}
  {$IFDEF Linux}
    Result := ApplicationPath + FILENAME_STANDARD_TEST_FILE_UNIX;
  {$ENDIF}
end;

constructor TSettings.Create;
{$IFDEF DARWIN}
var
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
{$ENDIF}
begin
  inherited Create;
  FProxyNodeAlias := $0AAA;
  FProxyNodeID := $010203040506;
  FBaudRate := 333333;
  FComPort := 'COM4';
  FXMLSettings := nil;
  FMultiNodeTest := False;
  FTimeoutComRead := DEFAULT_COM_READ_TIMEOUT;
  FSoftwareFlowControl := False;
  FPingPongStandardFrameTest:= False;
  FTimeoutStartupRID := 2000; // 2 sec
  FUIRefreshRate := 40;

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
  FApplicationPath := PATH_UNIX_APPLICATION_PATH;
{$ENDIF}
end;

procedure TSettings.ReadSettings;
var
  NodeSettings, NodeIDs, NodeCOM, NodeID, NodeAlias, NodeCOMPort, NodeBaudRate: TDOMNode;
begin
  if FileExistsUTF8(GetAppConfigDir(True)+FILENAME_SETTINGS) then
  begin
    ReadXMLFile(FXMLSettings, UTF8ToSys(GetAppConfigDir(True)+FILENAME_SETTINGS));
    NodeSettings := XMLSettings.FindNode(XML_SETTINGS);
    if Assigned(NodeSettings) then
    begin
      NodeIDs := NodeSettings.FindNode(XML_SETTINGS_NODE);
      if Assigned(NodeIDs) then
      begin
        NodeID := NodeIDs.FindNode(XML_SETTINGS_ID);
        if Assigned(NodeID) then
          FProxyNodeID := StrToInt64(NodeID.FirstChild.NodeValue);
        NodeAlias := NodeIDs.FindNode(XML_SETTINGS_ALIAS);
        if Assigned(NodeAlias) then
          FProxyNodeAlias := StrToInt(NodeAlias.FirstChild.NodeValue);
      end;
      NodeCOM := NodeSettings.FindNode(XML_SETTINGS_COM);
      if Assigned(NodeCOM) then
      begin
        NodeCOMPort := NodeCOM.FindNode(XML_SETTINGS_PORT);
        if Assigned(NodeCOMPort) then
          FComPort := NodeCOMPort.FirstChild.NodeValue;
        NodeBaudRate := NodeCOM.FindNode(XML_SETTINGS_BAUD);
        if Assigned(NodeBaudRate) then
          FBaudRate := StrToInt(NodeBaudRate.FirstChild.NodeValue);
      end;
    end;
  end;
end;

procedure TSettings.WriteSettings;
begin
  WriteXMLFile(FXMLSettings, GetAppConfigDir(True)+FILENAME_SETTINGS);
end;

initialization
  Settings := TSettings.Create;

finalization
  FreeAndNil(Settings);

end.
