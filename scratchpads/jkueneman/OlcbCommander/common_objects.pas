unit common_objects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TMpThreadedStringList }

  TMpThreadedStringList = class(TThreadList)
  protected

  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AString: ansistring);
  end;

implementation

{ TMpThreadedStringList }

constructor TMpThreadedStringList.Create;
var
  List: TList;
begin
  inherited Create;
  List := LockList;
  List.Add(TStringList.Create);
  UnLockList;
end;

destructor TMpThreadedStringList.Destroy;
var
  List: TList;
begin
  List := LockList;
  TObject( List[0]).Free;
  List.Clear;
  UnLockList;
  inherited Destroy;
end;

procedure TMpThreadedStringList.Add(AString: ansistring);
begin

end;

end.

