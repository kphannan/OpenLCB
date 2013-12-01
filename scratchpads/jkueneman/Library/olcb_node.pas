unit olcb_node;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, laz2_DOM, laz2_XMLRead, olcb_transport_layer;

type
  { TOpenLcbNode }

  TOpenLcbNode = class
  private
    FCDI: TXMLDocument;
    FConfigMem: TOlcbStructureMemConfig;
    FNodeID: QWord;
    FNodeIDAlias: Word;
    FProtocolSupport: QWord;
    FSnii: TOlcbStructureSNIP;
    procedure SetCDI(AValue: TXMLDocument);
    procedure SetNodeID(AValue: QWord);
    procedure SetNodeIDAlias(AValue: Word);
    procedure SetProtocolSupport(AValue: QWord);
    procedure SetSnii(AValue: TOlcbStructureSNIP);
  public
    constructor Create;
    destructor Destroy; override;
    property CDI: TXMLDocument read FCDI write SetCDI;
    property ConfigMem: TOlcbStructureMemConfig read FConfigMem write FConfigMem;
    property NodeID: QWord read FNodeID write SetNodeID;
    property NodeIDAlias: Word read FNodeIDAlias write SetNodeIDAlias;
    property ProtocolSupport: QWord read FProtocolSupport write SetProtocolSupport;
    property Snii: TOlcbStructureSNIP read FSnii write SetSnii;
  end;

implementation

{ TOpenLcbNode }

procedure TOpenLcbNode.SetCDI(AValue: TXMLDocument);
begin
  FreeAndNil(FCDI);
  FCDI:=AValue;
end;

procedure TOpenLcbNode.SetNodeID(AValue: QWord);
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

procedure TOpenLcbNode.SetSnii(AValue: TOlcbStructureSNIP);
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

