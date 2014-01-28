unit nodeidresolutionprotocol;

// This is like ARP, it keeps a cache of Alias and full Node IDs mappings

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TNodeID = array[0..1] of DWord;

type

  { TNrp }

  TNrp = class
  private
    FAge: Word;                 // How old the mapping is
    FAliasID: Word;
    FNodeID: TNodeID;
  public
    property Age: Word read FAge write FAge;
    property AliasID: Word read FAliasID write FAliasID;
    property NodeID: TNodeID read FNodeID write FNodeID;
    constructor Create(AnAliasID: Word; ANodeID: TNodeID);
    destructor Destroy; override;
  end;

  { TNrpList }

  TNrpList = class(TList)
  private
    function Get(Index: Integer): TNrp;
    procedure Put(Index: Integer; AValue: TNrp);
  public
    property Items[Index: Integer]: TNrp read Get write Put; default;
    procedure Clear; override;
    destructor Destroy; override;
  end;

implementation

{ TNrpList }

function TNrpList.Get(Index: Integer): TNrp;
begin
  Result := TNrp( inherited Items[Index])
end;

procedure TNrpList.Put(Index: Integer; AValue: TNrp);
begin
  inherited Items[Index] := AValue
end;

procedure TNrpList.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to Count - 1 do
      Items[i].Free
  finally
    inherited Clear;
  end;
end;

destructor TNrpList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

{ TNrp }

constructor TNrp.Create(AnAliasID: Word; ANodeID: TNodeID);
begin
  inherited Create;
  FAliasID := AnAliasID;
  FNodeID := ANodeID;
  FAge := 0;
end;

destructor TNrp.Destroy;
begin
  inherited Destroy;
end;

end.

