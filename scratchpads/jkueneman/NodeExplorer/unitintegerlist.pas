unit unitintegerlist;

{$mode objfpc}{$H+}


(*********************************************************)
(* Feel free to use it, but at your own risk!            *)
(* Ã€ utiliser librement, mais Ã  vos risques et pÃ©rils !  *)
(* CapJack.                                              *)
(*********************************************************)


(*********************************************************)
(***)                                                 (***)
(***)                  INTERFACE                      (***)
(***)                                                 (***)
(*********************************************************)


(*********************************************************)
(* TIntegerList (v1.31)                                  *)
(* ----------------------------------------------------- *)
(* Implements a powerful class encapsulating a list      *)
(* of "Integer", with additional features compared to    *)
(* conventional TList.                                   *)
(* Study the source code for the complete list           *)
(* of features.                                          *)
(* Compilable with any version of Delphi 32/64-bit,      *)
(* and also with Lazarus / Free Pascal.                  *)
(* ----------------------------------------------------- *)
(* ImplÃ©mente une puissante classe encapsulant une liste *)
(* d'Ã©lÃ©ments "Integer", avec des fonctionnalitÃ©s        *)
(* supplÃ©mentaires par rapport au classique TList.       *)
(* Ã‰tudiez le code source pour la liste complÃ¨te         *)
(* des fonctionnalitÃ©s.                                  *)
(* Compilable avec toute version de Delphi 32/64 bits,   *)
(* ainsi qu'avec Lazarus / Free Pascal.                  *)
(*********************************************************)


{$IFDEF VER80 }{$DEFINE VERYOLDVERSION}{$ENDIF D1    }
{$IFDEF VER90 }{$DEFINE VERYOLDVERSION}{$ENDIF D2    }
{$IFDEF VER93 }{$DEFINE VERYOLDVERSION}{$ENDIF BCB++1}
{$IFDEF VER100}{$DEFINE VERYOLDVERSION}{$ENDIF D3    }
{$IFDEF VER110}{$DEFINE VERYOLDVERSION}{$ENDIF BCB++3}
{$IFDEF VER120}{$DEFINE VERYOLDVERSION}{$ENDIF D4    }
{$IFDEF VER125}{$DEFINE VERYOLDVERSION}{$ENDIF BCB++4}
{$IFDEF VER130}{$DEFINE VERYOLDVERSION}{$ENDIF D5    }
{$IFDEF VER140}{$DEFINE OLDVERSION}{$ENDIF D6,CB++6,Kylix 1&2}
{$IFDEF VER150}{$DEFINE OLDVERSION}{$ENDIF D7, Kylix 3}
{$IFDEF VER160}{$DEFINE OLDVERSION}{$ENDIF D8 for .NET}
{$IFDEF VER170}{$DEFINE OLDVERSION}{$ENDIF D2005}
{$IFDEF VER180}{$DEFINE OLDVERSION}{$ENDIF D2006, D2007 Win32}
{$IFDEF VER180}{$DEFINE OLDVERSION}{$ENDIF D2007 Win32}
{$IFDEF VER185}{$DEFINE OLDVERSION}{$ENDIF D2007 Win32}
{$IFDEF VER190}{$DEFINE OLDVERSION}{$ENDIF D2007 .NET}
{$IFDEF VER200}{$DEFINE OLDVERSION}{$ENDIF D2009, CB++ 2009}
{$IFDEF VER210}{$DEFINE OLDVERSION}{$ENDIF Delphi 2010}
{$IFDEF VER220}{$DEFINE OLDVERSION}{$ENDIF Delphi XE}

{$IFDEF FPC} // Lazarus / Free Pascal
 uses SysUtils, Classes, RTLConsts;
 {$HINTS OFF} // Solve useless "local variable not initialized"
{$ELSE} // Delphi
 {$IFDEF VERYOLDVERSION} // Delphi 1 -> 5
  uses SysUtils, Classes, Consts;
 {$ELSE}
  {$IFDEF OLDVERSION} // Delphi 6 -> XE
   uses SysUtils, Classes, RTLConsts;
  {$ELSE} // Delphi XE2 Win 32/64 + MacOS
   uses System.SysUtils, System.Classes,
        System.RTLConsts;
  {$ENDIF OLDVERSION}
 {$ENDIF VERYOLDVERSION}
{$ENDIF FPC}

const
 MaxIntegerListSize = Maxint div SizeOf(Integer);

 SIntegerListVoidError='Invalid method call (empty list)!';
 SIntegerListSortError='Invalid method call (sorted list)!';

type
  PIntegerPtrList = ^TIntegerPtrList;

  TIntegerPtrList = array[0..MaxIntegerListSize - 1] of Integer;

  TIntegerListSortCompare = function (Item1, Item2: Integer): Integer;

  TIntegerDescriptor = function (Index:Integer;Item : Integer) : string;

  TIntegerSortOption  = (
      IntegerSortNone,
      IntegerSortUpWithDup,
      IntegerSortUpNoDup,
      IntegerSortDownWithDup,
      IntegerSortDownNoDup
     );

  TIntegerList = class(TObject)
  private
    FList       : PIntegerPtrList;
    FCount      : Integer;
    FCapacity   : Integer;
    FSortType   : TIntegerSortOption;
  protected
    function  Get(Index: Integer): Integer;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: Integer);
    procedure SetCapacity(AValue: Integer);
    procedure SetCount(AValue: Integer);
    procedure SetSortType(NewSortType: TIntegerSortOption);
    procedure EliminateDups;
    function  NormalFind(Value: Integer): Integer;
    function  FastFindUp(Value:Integer; var Position:Integer):Integer;
    function  FastFindDown(Value:Integer; var Position:Integer):Integer;
    procedure ForceInsert(Index: Integer; Item: Integer);
    function  NormalAdd(Item: Integer): Integer;

  public
    constructor Create;
    destructor Destroy; override;
    function  Add(Item: Integer): Integer;
    procedure Clear;
    procedure SaveToStream(const S:TStream);
    procedure LoadFromStream(const S:TStream; const KeepCurrentSortType:Boolean=false);
    procedure SaveToFile(FileName:string);
    procedure LoadFromFile(FileName:string; const KeepCurrentSortType:Boolean=false);
    procedure Delete(Index: Integer);
    function  ErrMsg(const Msg:string;Data:Integer):string;
    procedure Exchange(Index1, Index2: Integer);
    function  Expand: TIntegerList;
    function  First: Integer;
    function  IndexOf(Value: Integer): Integer;
    procedure Insert(Index: Integer; Item: Integer);
    function  Last: Integer;
    procedure Move(CurIndex, NewIndex: Integer);
    function  Remove(Item: Integer): Integer;
    procedure Pack(NilValue:Integer);
    procedure Sort(Compare: TIntegerListSortCompare);
    procedure SortUp;
    procedure SortDown;
    procedure ShowList(StringList:TStrings; Descriptor:TIntegerDescriptor=nil; ClearIt:Boolean=true);
    function  Minimum:Integer;
    function  Maximum:Integer;
    function  Range:Integer;
    function  Sum:Extended;
    function  SumSqr:Extended;
    function  Average:Extended;
    procedure CopyFrom(List:TIntegerList; const KeepCurrentSortType:Boolean=false);
    procedure CopyTo(List:TIntegerList; const KeepDestSortType:Boolean=false);

    procedure Push(Value:Integer);
    function  LifoPop(DefValue:Integer):Integer;
    function  FifoPop(DefValue:Integer):Integer;

    property  List: PIntegerPtrList read FList;
    property  Capacity: Integer read FCapacity write SetCapacity;
    property  Count: Integer read FCount write SetCount;
    property  Items[Index: Integer]: Integer read Get write Put; default;
    property  SortType:TIntegerSortOption read FSortType write SetSortType;
  end;


// Default descriptor - Descripteur par dÃ©faut (ShowList)
function DefDesc(Index: Integer;Item: Integer) : string;


implementation

{$IFNDEF FPC}
 {$IFDEF OLDVERSION}
  uses Consts;
 {$ENDIF}
{$ENDIF}

function DefDesc(Index: Integer;Item: Integer) : string;
 begin
  Result:=Format('Items[%d] = %d',[Index,Item]);
 end;

constructor TIntegerList.Create;
begin
  inherited Create;
  FSortType := IntegerSortNone;
end;

destructor TIntegerList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function  TIntegerList.NormalAdd(Item: Integer): Integer;
begin
  Result := FCount;
  if Result = FCapacity then Grow;
  FList^[Result] := Item;
  Inc(FCount);
end;

procedure TIntegerList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TIntegerList.SaveToStream(const S:TStream);
var T : Integer;
 begin
  T := Integer(FSortType);
  S.Write(FCount,SizeOf(FCount));
  S.Write(T,SizeOf(T));
  S.Write(FList^,FCount * SizeOf(Integer));
 end;


procedure TIntegerList.SaveToFile(FileName:string);
 var Stream:TFileStream;
 begin
  Stream:=nil;
  try
   Stream := TFileStream.Create(FileName, fmCreate
                                or fmShareExclusive);
   SaveToStream(Stream);
  finally
   if assigned(Stream) then Stream.Free;
  end;
 end;

{---------------------------------------------------------}

procedure TIntegerList.LoadFromStream(const S:TStream;
                  const KeepCurrentSortType:Boolean=false);
 var N, T    : Integer;
     Current : TIntegerSortOption;
     Saved   : TIntegerSortOption;
 begin
  S.Read(N,SizeOf(N));
  S.Read(T,SizeOf(T));
  Saved   := TIntegerSortOption(T);
  Current := FSortType;
  Clear;
  SetSortType(IntegerSortNone);
  SetCount(N);
  S.Read(FList^,N * SizeOf(Integer));
  if KeepCurrentSortType and (Current <> Saved)
     then SetSortType(Current)
     else FSortType := Saved;
 end;

{---------------------------------------------------------}

procedure TIntegerList.LoadFromFile(FileName:string;
                const KeepCurrentSortType:Boolean=false);
 var Stream:TFileStream;
 begin
  Stream:=nil;
  try
   Stream := TFileStream.Create(FileName,fmOpenRead
                                or fmShareDenyWrite);
   LoadFromStream(Stream,KeepCurrentSortType);
  finally
   if assigned(Stream) then Stream.Free;
  end;
 end;

{---------------------------------------------------------}

procedure TIntegerList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount)
   then raise EListError.Create(
                   ErrMsg(SListIndexError, Index));
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(Integer));
end;

{---------------------------------------------------------}

function  TIntegerList.ErrMsg(const Msg:string;
                                   Data:Integer):string;
begin
  Result := Format(Msg,[Data]);
end;

{---------------------------------------------------------}

procedure TIntegerList.Exchange
                                 (Index1, Index2: Integer);
var
  Item: Integer;
begin
  if FSortType <> IntegerSortNone
     then raise EListError.Create(
                     ErrMsg(SIntegerListSortError,0));
  if (Index1 < 0) or (Index1 >= FCount)
     then raise EListError.Create(
                     ErrMsg(SListIndexError, Index1));
  if (Index2 < 0) or (Index2 >= FCount)
     then raise EListError.Create(
                     ErrMsg(SListIndexError, Index2));
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end;

{---------------------------------------------------------}

function  TIntegerList.Expand: TIntegerList;
begin
  if FCount = FCapacity then Grow;
  Result := Self;
end;

{---------------------------------------------------------}

function  TIntegerList.First: Integer;
begin
  Result := Get(0);
end;

{---------------------------------------------------------}

function  TIntegerList.Get
                            (Index: Integer): Integer;
begin
  if (Index < 0) or (Index >= FCount)
     then raise EListError.Create(
                     ErrMsg(SListIndexError, Index));
  Result := FList^[Index];
end;

{---------------------------------------------------------}

procedure TIntegerList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64
     then Delta := FCapacity div 4
     else if FCapacity > 8
          then Delta := 16
          else Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

{---------------------------------------------------------}

procedure TIntegerList.ForceInsert(Index: Integer;
                                        Item: Integer);
begin
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(Integer));
  FList^[Index] := Item;
  Inc(FCount);
end;

{---------------------------------------------------------}

procedure TIntegerList.Insert(Index: Integer;
                                   Item: Integer);
begin
  if FSortType <> IntegerSortNone
     then raise EListError.Create(
                     ErrMsg(SIntegerListSortError,0));
  if (Index < 0) or (Index > FCount)
     then raise EListError.Create(
                     ErrMsg(SListIndexError, Index));
  ForceInsert(Index,Item);
end;

{---------------------------------------------------------}

function  TIntegerList.Last: Integer;
begin
  Result := Get(FCount - 1);
end;

{---------------------------------------------------------}

procedure TIntegerList.Move(CurIndex,
                                 NewIndex: Integer);
var
  Item: Integer;
begin
  if FSortType <> IntegerSortNone
     then raise EListError.Create(
                     ErrMsg(SIntegerListSortError,0));
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) or (NewIndex >= FCount)
       then raise EListError.Create(
                     ErrMsg(SListIndexError, NewIndex));
    Item := Get(CurIndex);
    Delete(CurIndex);
    Insert(NewIndex, Item);
  end;
end;

{---------------------------------------------------------}

procedure TIntegerList.Put(Index: Integer;
                                Item: Integer);
begin
  if (Index < 0) or (Index >= FCount)
     then raise EListError.Create(
                     ErrMsg(SListIndexError, Index));
  FList^[Index] := Item;
end;

{---------------------------------------------------------}

function  TIntegerList.Remove
                             (Item: Integer): Integer;
begin
  Result := IndexOf(Item);
  if Result <> -1 then Delete(Result);
end;

{---------------------------------------------------------}

procedure TIntegerList.Pack(NilValue:Integer);
var
  I: Integer;
begin
  for I := FCount - 1 downto 0
      do if Items[I] = NilValue
            then Delete(I);
end;

{---------------------------------------------------------}

procedure TIntegerList.SetCapacity(AValue: Integer);
begin
  if (AValue < FCount)
  or (AValue > MaxIntegerListSize)
     then raise EListError.Create(
                  ErrMsg(SListCapacityError, AValue));
  if AValue <> FCapacity
     then begin
           ReallocMem(FList,
                      AValue * SizeOf(Integer));
           FCapacity := AValue;
          end;
end;

{---------------------------------------------------------}

procedure TIntegerList.SetCount(AValue: Integer);
begin
  if (AValue < 0)
  or (AValue > MaxIntegerListSize)
     then raise EListError.Create(
                     ErrMsg(SListCountError, AValue));
  if AValue > FCapacity
     then SetCapacity(AValue);
  if AValue > FCount
     then FillChar((@FList^[FCount])^,
            (AValue - FCount) * SizeOf(Integer), 0);
  FCount := AValue;
end;

{---------------------------------------------------------}

procedure QuickSort(SortList: PIntegerPtrList;
                    L, R: Integer;
  SCompare: TIntegerListSortCompare);
var
  I, J: Integer;
  P, T: Integer;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while SCompare(SortList^[I], P) < 0 do Inc(I);
      while SCompare(SortList^[J], P) > 0 do Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(SortList, L, J, SCompare);
    L := I;
  until I >= R;
end;

{---------------------------------------------------------}

procedure TIntegerList.Sort
                   (Compare: TIntegerListSortCompare);
begin
  if (FList <> nil) and (Count > 0)
     then QuickSort(FList, 0, Count - 1, Compare);
end;

{---------------------------------------------------------}

procedure QuickSortUp(SortList: PIntegerPtrList;
                      L, R: Integer);
var
  I, J: Integer;
  P, T: Integer;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while SortList^[I] < P do Inc(I);
      while SortList^[J] > P do Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSortUp(SortList, L, J);
    L := I;
  until I >= R;
end;

procedure TIntegerList.SortUp;
begin
  if (FList <> nil) and (Count > 0) then
   begin
    QuickSortUp(FList, 0, Count - 1);
    FSortType := IntegerSortNone
   end;
end;

{---------------------------------------------------------}

procedure QuickSortDown(SortList: PIntegerPtrList;
                        L, R: Integer);
var
  I, J: Integer;
  P, T: Integer;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while SortList^[I] > P do Inc(I);
      while SortList^[J] < P do Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSortDown(SortList, L, J);
    L := I;
  until I >= R;
end;

procedure TIntegerList.SortDown;
begin
  if (FList <> nil) and (Count > 0) then
   begin
    QuickSortDown(FList, 0, Count - 1);
    FSortType := IntegerSortNone
   end;
end;

{---------------------------------------------------------}

procedure TIntegerList.ShowList(StringList:TStrings;
                  Descriptor:TIntegerDescriptor=nil;
                  ClearIt:boolean=true);
var I:integer;
begin
 if not assigned(StringList) then exit;
 with StringList do
  begin
   BeginUpdate;
   if ClearIt then Clear;
   if assigned(Descriptor)
      then for I := 0 to FCount -1
           do Add(Descriptor(I,FList^[I]))
      else for I := 0 to FCount -1
           do Add(DefDesc(I,FList^[I]));
   EndUpdate;
  end;
end;

{---------------------------------------------------------}

procedure TIntegerList.Push(Value:Integer);
 begin
  Add(Value);
 end;

{---------------------------------------------------------}

function  TIntegerList.LifoPop
                      (DefValue:Integer):Integer;
 begin
  if Count=0 then Result := DefValue
             else begin
                   Result := Last;
                   Delete(Count - 1);
                  end;
 end;

{---------------------------------------------------------}

function  TIntegerList.FifoPop
                      (DefValue:Integer):Integer;
 begin
  if Count=0 then Result := DefValue
             else begin
                   Result := First;
                   Delete(0);
                  end;
 end;

{---------------------------------------------------------}

function  TIntegerList.Minimum:Integer;
 var I:Integer;
 begin
  Result := 0;
  if FCount=0
     then raise EListError.Create(
                     ErrMsg(SIntegerListVoidError,0));
  case FSortType of

       IntegerSortNone:
         begin
           Result := FList^[0];
           for I:=1 to FCount-1 do
               if FList^[I]<Result
                  then Result := FList^[I];
         end;

       IntegerSortUpWithDup,
       IntegerSortUpNoDup:
         begin
           Result := FList^[0];
         end;

       IntegersortDownWithDup,
       IntegersortDownNoDup:
         begin
           Result := FList^[FCount - 1];
         end;

      end;
 end;

{---------------------------------------------------------}

function  TIntegerList.Maximum:Integer;
 var I:Integer;
 begin
  Result := 0;
  if FCount=0
     then raise EListError.Create(
                     ErrMsg(SIntegerListVoidError,0));
  case FSortType of

       IntegerSortNone:
         begin
           Result := FList^[0];
           for I:=1 to FCount-1
               do if FList^[I]>Result
                  then Result := FList^[I];
         end;

       IntegerSortUpWithDup,
       IntegerSortUpNoDup:
         Result := FList^[FCount - 1];

       IntegersortDownWithDup,
       IntegersortDownNoDup:
         Result := FList^[0];

      end;
 end;

{---------------------------------------------------------}

function  TIntegerList.Range:Integer;
 var I:Integer;Min,Max,Item:Integer;
 begin
  if FCount=0
     then raise EListError.Create(
                     ErrMsg(SIntegerListVoidError,0));
  if FSortType = IntegerSortNone
   then
     begin
      Min := FList^[0];
      Max:=Min;
      for I:=1 to FCount-1 do
          begin
           Item:=FList^[I];
           if Item > Max then Max := Item;
           if Item < Min then Min := Item;
          end;
      Result := Max - Min;
     end
   else Result := Maximum - Minimum;
 end;

{---------------------------------------------------------}

function  TIntegerList.Sum:Extended;
 var I:Integer;
 begin
  Result:=0;
  for I:=0 to FCount-1
      do Result := Result + FList^[I];
 end;

{---------------------------------------------------------}

function  TIntegerList.SumSqr:Extended;
 var I:Integer;Dummy:Extended;
 begin
  Result:=0;
  for I:=0 to FCount-1
      do begin
          Dummy := FList^[I];
          Result := Result + ( Dummy * Dummy );
         end;
 end;

{---------------------------------------------------------}

function  TIntegerList.Average:Extended;
 begin
  if FCount=0
     then raise EListError.Create(
                     ErrMsg(SIntegerListVoidError,0));
  Result := (Sum / FCount);
 end;

{---------------------------------------------------------}

procedure TIntegerList.CopyFrom(List:TIntegerList;
                   const KeepCurrentSortType:Boolean=false);
 var Current : TIntegerSortOption;
 begin
  Current := FSortType;
  Clear;
  SetSortType(IntegerSortNone);
  SetCount(List.Count);
  System.Move(List.List^, FList^,
              List.Count*SizeOf(Integer));
  if KeepCurrentSortType and (Current <> List.SortType)
     then SetSortType(Current)
     else FSortType := List.SortType;
 end;

{---------------------------------------------------------}

procedure TIntegerList.CopyTo(List:TIntegerList;
                    const KeepDestSortType:Boolean=false);
 begin
  List.CopyFrom(Self,KeepDestSortType);
 end;

{---------------------------------------------------------}

function  TIntegerList.NormalFind
                           (Value: Integer): Integer;
begin
  Result := 0;
  while (Result < FCount) and (FList^[Result] <> Value)
        do Inc(Result);
  if Result = FCount then Result := -1;
end;

{---------------------------------------------------------}

function  TIntegerList.FastFindUp(Value:Integer;
                           var Position:Integer):Integer;
 var A,B:Integer;
 begin
  if Count = 0
     then begin Position := 0; Result := -1; exit end;
  if Value = FList^[0]
     then begin Position := 0; Result :=  0; exit end;
  if Value < FList^[0]
     then begin Position := 0; Result := -1; exit end;
  A := 0;
  B := Count;
  repeat
   Position:=(A + B) div 2;
   if Value = FList^[Position]
      then begin Result := Position;exit end
      else if Value < FList^[Position]
           then B := Position
           else A := Position
  until B - A <= 1;
  Result := -1;
  if Value > FList^[Position] then inc(Position);
 end;

{---------------------------------------------------------}

function  TIntegerList.FastFindDown(Value:Integer;
                             var Position:Integer):Integer;
 var A,B:Integer;
 begin
  if Count = 0
     then begin Position := 0; Result := -1; exit end;
  if Value = FList^[0]
     then begin Position := 0; Result :=  0; exit end;
  if Value > FList^[0]
     then begin Position := 0; Result := -1; exit end;
  A := 0;
  B := Count;
  repeat
   Position:=(A + B) div 2;
   if Value = FList^[Position]
      then begin Result := Position;exit end
      else if Value > FList^[Position]
              then B := Position
              else A := Position
  until B - A <= 1;
  Result := -1;
  if Value < FList^[Position] then inc(Position);
 end;

{---------------------------------------------------------}

function  TIntegerList.IndexOf
                            (Value: Integer): Integer;
 var P:Integer;
 begin
  Result := -1;
  case FSortType of
       IntegerSortNone:
         Result := NormalFind(Value);

       IntegerSortUpWithDup,
       IntegerSortUpNoDup:
         Result := FastFindUp(Value,P);

       IntegersortDownWithDup,
       IntegersortDownNoDup:
         Result := FastFindDown(Value,P);
      end;
 end;

{---------------------------------------------------------}

function  TIntegerList.Add
                             (Item: Integer): Integer;
 var P:Integer;
 begin
  Result := -1;
  case FSortType of
       IntegerSortNone:
                         begin
                          Result := NormalAdd(Item);
                         end;
       IntegerSortUpWithDup:
                         begin
                          FastFindUp(Item,P);
                          ForceInsert(P,Item);
                          Result := P;
                         end;
       IntegerSortUpNoDup:
                         begin
                          if FastFindUp(Item,P) = -1
                             then begin
                                   ForceInsert(P,Item);
                                   Result:=P
                                  end;
                         end;
       IntegerSortDownWithDup:
                         begin
                          FastFindDown(Item,P);
                          ForceInsert(P,Item);
                          Result := P;
                         end;
       IntegerSortDownNoDup:
                         begin
                          if FastFindDown(Item,P) = -1
                             then begin
                                   ForceInsert(P,Item);
                                   Result:=P
                                  end;
                         end;
      end;
 end;

{---------------------------------------------------------}

procedure TIntegerList.EliminateDups;
 var I:Integer;
 begin
  I:=0;
  while I < Count - 1 do
    if FList^[I + 1] = FList^[I]
	 then Delete(I) else Inc(I);
 end;

{---------------------------------------------------------}

procedure TIntegerList.SetSortType
                     (NewSortType:TIntegerSortOption);
 begin
  if NewSortType = FSortType then exit;
  case NewSortType of

       IntegerSortNone:
         begin
         end;

       IntegerSortUpWithDup:
         begin
           if FSortType <> IntegerSortUpNoDup
              then SortUp;
         end;

       IntegerSortUpNoDup:
         begin
           if FSortType <> IntegerSortUpWithDup
              then SortUp;
           EliminateDups;
         end;

       IntegerSortDownWithDup:
         begin
           if FSortType <> IntegerSortDownNoDup
              then SortDown;
         end;

       IntegerSortDownNoDup:
         begin
           if FSortType <> IntegerSortDownWithDup
              then SortDown;
           EliminateDups;
         end;

      end;
  FSortType := NewSortType;
 end;

{---------------------------------------------------------}

END.

