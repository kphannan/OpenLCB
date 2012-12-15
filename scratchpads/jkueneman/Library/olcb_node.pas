unit olcb_node;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, laz2_DOM, laz2_XMLRead, olcb_threaded_stack, olcb_mem_protocol;

type
  { TOpenLcbNode }

  TOpenLcbNode = class
  private
    FCDI: TXMLDocument;
    FConfigMem: TOlcbMemConfig;
    FNodeID: DWord;
    FNodeIDAlias: Word;
    FProtocolSupport: QWord;
    FSnii: TSnii;
    procedure SetCDI(AValue: TXMLDocument);
    procedure SetNodeID(AValue: DWord);
    procedure SetNodeIDAlias(AValue: Word);
    procedure SetProtocolSupport(AValue: QWord);
    procedure SetSnii(AValue: TSnii);
  public
    constructor Create;
    destructor Destroy; override;
    property CDI: TXMLDocument read FCDI write SetCDI;
    property ConfigMem: TOlcbMemConfig read FConfigMem write FConfigMem;
    property NodeID: DWord read FNodeID write SetNodeID;
    property NodeIDAlias: Word read FNodeIDAlias write SetNodeIDAlias;
    property ProtocolSupport: QWord read FProtocolSupport write SetProtocolSupport;
    property Snii: TSnii read FSnii write SetSnii;
  end;

implementation

{ TOpenLcbNode }

procedure TOpenLcbNode.SetCDI(AValue: TXMLDocument);
begin
  FreeAndNil(FCDI);
  FCDI:=AValue;
end;

procedure TOpenLcbNode.SetNodeID(AValue: DWord);
begin
  FNodeID:=AValue;
end;

procedure TOpenLcbNode.SetNodeIDAlias(AValue: Word);
begin
  FNodeIDAlias:=AValue;
end;

procedure TOpenLcbNode.SetProtocolSupport(AValue: QWord);
begin
  FProtocolSupport:=AValue;
end;

procedure TOpenLcbNode.SetSnii(AValue: TSnii);
begin
  FreeAndNil(FSnii);
  FSnii:=AValue;
end;

constructor TOpenLcbNode.Create;
begin
  FCDI := nil;
  FConfigMem := nil;
  FSnii := nil;
  NodeID := 0;
  NodeIDAlias := 0;
  ProtocolSupport := 0;
end;

destructor TOpenLcbNode.Destroy;
begin
  FreeAndNil(FSnii);
  FreeAndNil(FCDI);
  FreeAndNil(FConfigMem);
  inherited Destroy;
end;

end.

