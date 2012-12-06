unit snii;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, olcb_utilities, olcb_defines, serialport_thread;

type

  { TSNII }

  TSNII = class
  private
    FDestinationAlias: Word;
    FFull: Boolean;
    FLocalHelper: TOpenLCBMessageHelper;
    FStateMachineIndex: Byte;
    FSniiHardwareVersion: string;
    FSniiMfgModel: string;
    FSniiMfgName: string;
    FSniiMfgVersion: Byte;
    FSniiSoftwareVersion: string;
    FSniiUserDesciption: string;
    FSniiUserDescription: string;
    FSniiUserName: string;
    FSniiUserVersion: Byte;
    FSourceAlias: Word;
  protected
    property LocalHelper: TOpenLCBMessageHelper read FLocalHelper write FLocalHelper;   // Global object to work with OLCB messages
    property StateMachineIndex: Byte read FStateMachineIndex write FStateMachineIndex;
  public
    constructor Create(ASourceAlias, ADestinationAlias: Word);
    destructor Destroy; override;
    function Process(AHelper: TOpenLCBMessageHelper): TSNII;

    property DestinationAlias: Word read FDestinationAlias write FDestinationAlias;
    property SourceAlias: Word read FSourceAlias write FSourceAlias;
    property Full: Boolean read FFull write FFull;
    property SniiMfgName: string read FSniiMfgName write FSniiMfgName;
    property SniiMfgModel: string read FSniiMfgModel write FSniiMfgModel;
    property SniiSoftwareVersion: string read FSniiSoftwareVersion write FSniiSoftwareVersion;
    property SniiHardwareVersion: string read FSniiHardwareVersion write FSniiHardwareVersion;
    property SniiUserName: string read FSniiUserName write FSniiUserName;
    property SniiUserDescription: string read FSniiUserDescription write FSniiUserDesciption;
    property SniiUserVersion: Byte read FSniiUserVersion write FSniiUserVersion;
    property SniiMfgVersion: Byte read FSniiMfgVersion write FSniiMfgVersion;
  end;

  { TSniiReceiveManager }

  TSniiReceiveManager = class
  private
    FSniis: TList;
    FSourceAlias: Word;
    function GetSnii(Index: Integer): TSNII;
  protected
    function FindInProcessSniiByAlias(DestinationAlias: Word): TSNII;
    property Sniis: TList read FSniis write FSniis;
    property SourceAlias: Word read FSourceAlias write FSourceAlias;            // Alias of the receiver for the datagrams
  public
    constructor Create(ASourceAlias: Word);
    destructor Destroy; override;
    procedure Clear;
    function Process(AHelper: TOpenLCBMessageHelper): TSNII;
    property Snii[Index: Integer]: TSNII read GetSnii;       // Inprocess and completed Datagrams, the order they are received is preserved
  end;

implementation

const
  STATE_MFG_VERSION  = 0;
  STATE_MFG_NAME     = 1;
  STATE_MFG_MODEL    = 2;
  STATE_HARDWARE_VER = 3;
  STATE_SOFTWARE_VER = 4;
  STATE_USER_VERSION = 5;
  STATE_USER_NAME    = 6;
  STATE_USER_DESC    = 7;
  STATE_DONE         = 8;

{ TSniiReceiveManager }

function TSniiReceiveManager.GetSnii(Index: Integer): TSNII;
begin
  if (Index > -1) and (Index < Sniis.Count) then
    Result := TSNII( Sniis[Index])
  else
    Result := nil
end;

function TSniiReceiveManager.FindInProcessSniiByAlias(DestinationAlias: Word): TSNII;
var
  i: Integer;
begin
  i := 0;
  Result := nil;
  while not Assigned(Result) and (i < Sniis.Count) do
  begin
    if (Snii[i].DestinationAlias = DestinationAlias) and not Snii[i].Full then
      Result := Snii[i];
    Inc(i)
  end;
end;

constructor TSniiReceiveManager.Create(ASourceAlias: Word);
begin
  inherited Create;
  FSourceAlias := ASourceAlias;
  FSniis := TList.Create;
end;

destructor TSniiReceiveManager.Destroy;
begin
  Clear;
  FreeAndNil(FSniis);
  inherited Destroy;
end;

procedure TSniiReceiveManager.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to Sniis.Count - 1 do
      TObject( Sniis[i]).Free;
  finally
    Sniis.Clear;
  end;
end;

function TSniiReceiveManager.Process(AHelper: TOpenLCBMessageHelper): TSNII;
var
  TestSnii: TSNII;
begin
  if AHelper.MTI = MTI_SIMPLE_NODE_INFO_REPLY then
  begin
    TestSnii := FindInProcessSniiByAlias(AHelper.SourceAliasID);
    if not Assigned(TestSnii) then
    begin
      TestSnii := TSNII.Create(SourceAlias, AHelper.SourceAliasID);  // Create a new receiving Datagram object for source alias of the message to us
      Sniis.Add(TestSnii);
    end;
    Result := TestSnii.Process(AHelper);
  end;
end;

{ TSNII }

constructor TSNII.Create(ASourceAlias, ADestinationAlias: Word);
begin
  inherited Create;
  FLocalHelper := TOpenLCBMessageHelper.Create;
  FDestinationAlias := ADestinationAlias;
  FSourceAlias := ASourceAlias;
  Full := False;
  SniiMfgName := '';
  SniiMfgModel := '';
  SniiSoftwareVersion := '';
  SniiHardwareVersion := '';
  SniiUserName := '';
  SniiUserDescription := '';
  SniiUserVersion := 0;
  SniiMfgVersion := 0;
  StateMachineIndex := 0;
end;

destructor TSNII.Destroy;
begin
  FreeAndNil(FLocalHelper);
  inherited;
end;

function TSNII.Process(AHelper: TOpenLCBMessageHelper): TSNII;
var
  i: Integer;
begin
  Result := nil;
  i := 2;                                      // Strip off the destination Alias
  while i < AHelper.DataCount do
  begin
    case StateMachineIndex of
      STATE_MFG_VERSION :
        begin
          SniiMfgVersion := AHelper.Data[i];
          Inc(i);
          StateMachineIndex := STATE_MFG_NAME;
        end;
      STATE_MFG_NAME     :
        begin
          if Chr( AHelper.Data[i]) <> #0 then
          begin
            SniiMfgName := SniiMfgName + Chr( AHelper.Data[i]);
            Inc(i);
          end else
          begin
            Inc(i);
            StateMachineIndex := STATE_MFG_MODEL;
          end;
        end;
      STATE_MFG_MODEL     :
        begin
          if Chr( AHelper.Data[i]) <> #0 then
          begin
            SniiMfgModel := SniiMfgModel + Chr( AHelper.Data[i]);
            Inc(i);
          end else
          begin
            Inc(i);
            StateMachineIndex := STATE_HARDWARE_VER;
          end;
        end;
      STATE_HARDWARE_VER  :
        begin
          if Chr( AHelper.Data[i]) <> #0 then
          begin
            SniiHardwareVersion := SniiHardwareVersion + Chr( AHelper.Data[i]);
            Inc(i);
          end else
          begin
            Inc(i);
            StateMachineIndex := STATE_SOFTWARE_VER;
          end;
        end;
      STATE_SOFTWARE_VER  :
        begin
          if Chr( AHelper.Data[i]) <> #0 then
          begin
            SniiSoftwareVersion := SniiSoftwareVersion + Chr( AHelper.Data[i]);
            Inc(i);
          end else
          begin
            Inc(i);
            StateMachineIndex := STATE_USER_VERSION;
          end;
        end;
      STATE_USER_VERSION  :
        begin
          SniiUserVersion := AHelper.Data[i];
          Inc(i);
          StateMachineIndex := STATE_USER_NAME;
        end;
      STATE_USER_NAME     :
        begin
          if Chr( AHelper.Data[i]) <> #0 then
          begin
            SniiUserName := SniiUserName + Chr( AHelper.Data[i]);
            Inc(i);
          end else
          begin
            Inc(i);
            StateMachineIndex := STATE_USER_DESC;
          end;
        end;
      STATE_USER_DESC     :
        begin
          if Chr( AHelper.Data[i]) <> #0 then
          begin
            SniiUserDescription := SniiUserDescription + Chr( AHelper.Data[i]);
            Inc(i);
          end else
          begin
            FFull := True;
            Result := Self;
            Inc(i);
            StateMachineIndex := STATE_DONE;
          end;
        end;
      STATE_DONE          :
        begin
          FFull := True;
          Result := Self;
          Inc(i)
        end;
    end;
  end;
end;

end.
