unit olcb_node;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, laz2_DOM, laz2_XMLRead, laz2_XMLWrite, snii, olcb_mem_protocol;

type
  { TOpenLcbNode }

  TNodeValidAttrib = (
    nva_CDI,                   // CDI is Valid
    nva_NodeID,                // Node ID is Valid
    nva_NodeIDAlias,           // Node Alais is Valid
    nva_ProtocolSupport,       // Protocol Support is Valid
    nva_Snii                   // Snii/SNIP is Valid
  );
  TNodeValidAttribs = set of TNodeValidAttrib;

  TOpenLcbNode = class
  private
    FCDI: TXMLDocument;
    FNodeID: DWord;
    FNodeIDAlias: Word;
    FProtocolSupport: QWord;
    FSnii: TSnii;
    FValidAttribs: TNodeValidAttribs;
    procedure SetCDI(AValue: TXMLDocument);
    procedure SetNodeID(AValue: DWord);
    procedure SetNodeIDAlias(AValue: Word);
    procedure SetProtocolSupport(AValue: QWord);
    procedure SetSnii(AValue: TSnii);
  public
    constructor Create;
    destructor Destroy; override;
    property CDI: TXMLDocument read FCDI write SetCDI;
    property NodeID: DWord read FNodeID write SetNodeID;
    property NodeIDAlias: Word read FNodeIDAlias write SetNodeIDAlias;
    property ProtocolSupport: QWord read FProtocolSupport write SetProtocolSupport;
    property Snii: TSnii read FSnii write SetSnii;
    property ValidAttribs: TNodeValidAttribs read FValidAttribs;
  end;

implementation

{ TOpenLcbNode }

procedure TOpenLcbNode.SetCDI(AValue: TXMLDocument);
begin
  FreeAndNil(FCDI);
  FCDI:=AValue;
  Include(FValidAttribs, nva_CDI);
end;

procedure TOpenLcbNode.SetNodeID(AValue: DWord);
begin
  FNodeID:=AValue;
  Include(FValidAttribs, nva_NodeID);
end;

procedure TOpenLcbNode.SetNodeIDAlias(AValue: Word);
begin
  FNodeIDAlias:=AValue;
  Include(FValidAttribs, nva_NodeIDAlias);
end;

procedure TOpenLcbNode.SetProtocolSupport(AValue: QWord);
begin
  FProtocolSupport:=AValue;
  Include(FValidAttribs, nva_ProtocolSupport);
end;

procedure TOpenLcbNode.SetSnii(AValue: TSnii);
begin
  FreeAndNil(FSnii);
  FSnii:=AValue;
  Include(FValidAttribs, nva_Snii);
end;

constructor TOpenLcbNode.Create;
begin
  CDI := nil;
  NodeID := 0;
  NodeIDAlias := 0;
  ProtocolSupport := 0;
  Snii := nil;
  FValidAttribs := [];
end;

destructor TOpenLcbNode.Destroy;
begin
  FreeAndNil(FSnii);
  FreeAndNil(FCDI);
  inherited Destroy;
end;

end.

