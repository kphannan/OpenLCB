unit unit_cdi_parser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Spin, laz2_DOM, laz2_XMLRead, Types, olcb_common_tasks,
  olcb_threaded_stack;


type

  TOlcbConfigDataType = (cdt_String, cdt_Int, cdt_EventID, cdt_Bit);

  { TMapRelation }

  TMapRelation = class
  private
    FProp: string;
    FValue: string;
  public
    constructor Create( AValue, AProperty: string);
    property Value: string read FValue write FValue;
    property Prop: string read FProp write FProp;
  end;

  { TMap }
  TMap = class(TList)
  private
    FList: TList;
    function GetRelation(Index: Integer): TMapRelation;
    procedure SetRelation(Index: Integer; AValue: TMapRelation);
  protected
    property List: TList read FList write FList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Relation: TMapRelation);
    procedure AddRelation( AValue, AProperty: string);
    procedure ClearList;
    function FindMapByValue(AValue: string): TMapRelation;
    function FindMapByProperty(AProperty: string): TMapRelation;
    property Relations[Index: Integer]: TMapRelation read GetRelation write SetRelation;
  end;

  { TConfigInfo }

  TConfigInfo = class
  private
    FConfigMemAddress: DWord;
    FConfigMemSize: DWord;
    FDataType: TOlcbConfigDataType;
    FIsDirty: Boolean;
    FIsUnknownState: Boolean;
    FMapList: TMap;
    FTask: TOlcbTaskBase;
  public
    constructor Create(MemOffset, MemSize: DWord; ADataType: TOlcbConfigDataType);
    destructor Destroy; override;
    property ConfigMemAddress: DWord read FConfigMemAddress write FConfigMemAddress;
    property ConfigMemSize: DWord read FConfigMemSize write FConfigMemSize;
    property IsDirty: Boolean read FIsDirty write FIsDirty;
    property IsUnknownState: Boolean read FIsUnknownState write FIsUnknownState;
    property Task: TOlcbTaskBase read FTask write FTask;
    property DataType: TOlcbConfigDataType read FDataType write FDataType;
    property MapList: TMap read FMapList write FMapList;
  end;

  { TOlcbSpinEdit }

  TOlcbSpinEdit = class(TSpinEdit)
  private
    FConfigInfo: TConfigInfo;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property ConfigInfo: TConfigInfo read FConfigInfo write FConfigInfo;
  end;

  { TOlcbEdit }

  TOlcbEdit = class(TEdit)
  private
    FConfigInfo: TConfigInfo;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property ConfigInfo: TConfigInfo read FConfigInfo write FConfigInfo;
  end;

  { TOlcbComboBox }

  TOlcbComboBox = class(TComboBox)
  private
    FConfigInfo: TConfigInfo;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property ConfigInfo: TConfigInfo read FConfigInfo write FConfigInfo;
  end;

  { TCdiParser }

  TCdiParser = class
  protected
    function ExtractElementItem(Element: TDOMNode; Item: string; var ItemStr: string): Boolean;
    function ExtractElementAttribute(Element: TDOMNode; AttributeName: string; var AttributeStr: string): Boolean;
    function IsMemorySpace(Segment: TDOMNode; MemorySpace: Byte): Boolean;
    procedure UpdateMemOffsetJump(Element: TDOMNode; var MemOffset: DWord);
    function UpdateMemOffsetSize(Element: TDOMNode): DWord;
    function AddTab(PageControl: TPageControl; ACaption: string): TScrollBox;
    procedure AddLabel(ParentControl: TScrollBox; ACaption: string; var ControlOffset: Integer; ControlMargin, Indent: Integer; Bold: Boolean);
    procedure AddSpinEdit(ParentControl: TScrollBox; Element: TDOMNode; var ControlOffset: Integer; ControlMargin, Indent: Integer; MemOffset, MemSize: DWord; PrintMemOffset: Boolean);
    procedure AddEdit(ParentControl: TScrollBox; Element: TDOMNode; var ControlOffset: Integer; ControlMargin, Indent: Integer; MemOffset, MemSize: DWord; PrintMemOffset: Boolean; ElementType: string);
    procedure AddComboBoxList(ParentControl: TScrollBox; Element: TDOMNode; var ControlOffset: Integer; ControlMargin, Indent: Integer; MemOffset, MemSize: DWord; PrintMemOffset: Boolean);
    procedure ProcessElementForUI(ParentControl: TScrollBox; Element: TDOMNode; var MemOffset: DWord; var ControlOffset: Integer; Indent: Integer; SupressNameAndDescription: Boolean; PrintMemOffset: Boolean);
  public
    function Build_CDI_Interface(ParentControl: TPanel; CDI: TXMLDocument): TPageControl;
    procedure Clear_CDI_Interface(ParentControl: TPanel);
  end;

implementation

{ TMapRelation }

constructor TMapRelation.Create(AValue, AProperty: string);
begin
  inherited Create;
  Value := AValue;
  Prop := AProperty
end;

{ TMap }

function TMap.GetRelation(Index: Integer): TMapRelation;
begin
  Result := TMapRelation( List[Index])
end;


procedure TMap.SetRelation(Index: Integer; AValue: TMapRelation);
begin
  List[Index] := AValue
end;

constructor TMap.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TMap.Destroy;
begin
  ClearList;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TMap.Add(Relation: TMapRelation);
begin
  List.Add(Relation);
end;

procedure TMap.AddRelation(AValue, AProperty: string);
begin
  List.Add( TMapRelation.Create( AValue, AProperty));
end;

procedure TMap.ClearList;
var
  i: Integer;
begin
  try
    for i := 0 to List.Count - 1 do
      TObject( List[i]).Free;
  finally
    Clear;
  end;
end;

function TMap.FindMapByValue(AValue: string): TMapRelation;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  AValue := LowerCase( AValue);
  while not Assigned(Result) and (i < List.Count) do
  begin
    if LowerCase( Relations[i].Value) = AValue then
      Result := Relations[i];
    Inc(i)
  end;
end;

function TMap.FindMapByProperty(AProperty: string): TMapRelation;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  AProperty := LowerCase( AProperty);
  while not Assigned(Result) and (i < List.Count) do
  begin
    if LowerCase( Relations[i].Prop) = AProperty then
      Result := Relations[i];
    Inc(i)
  end;
end;

{ TOlcbComboBox }

constructor TOlcbComboBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FConfigInfo := nil;
end;

destructor TOlcbComboBox.Destroy;
begin
  FreeAndNil( FConfigInfo);
  inherited Destroy;
end;

{ TOlcbEdit }

constructor TOlcbEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FConfigInfo := nil;
end;

destructor TOlcbEdit.Destroy;
begin
  FreeAndNil( FConfigInfo);
  inherited Destroy;
end;

{ TOlcbSpinEdit }

constructor TOlcbSpinEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FConfigInfo := nil;
end;

destructor TOlcbSpinEdit.Destroy;
begin
  FreeAndNil( FConfigInfo);
  inherited Destroy;
end;

{ TCdiParser }

function TCdiParser.ExtractElementItem(Element: TDOMNode; Item: string; var ItemStr: string): Boolean;
var
  Node: TDOMNode;
begin
  Result := False;
  ItemStr := '';
  Node := Element.FindNode(Item);
  if Assigned(Node) then
  begin
    Result := True;
    ItemStr := Node.TextContent;
  end;
end;

function TCdiParser.ExtractElementAttribute(Element: TDOMNode; AttributeName: string; var AttributeStr: string): Boolean;
var
  Node: TDOMNode;
begin
  Result := False;
  AttributeStr := '';
  if Element.HasAttributes then
  begin
    Node := Element.Attributes.GetNamedItem(AttributeName);
    if Assigned(Node) then
    begin
      Result := True;
      AttributeStr := Node.TextContent;
    end;
  end;
end;

function TCdiParser.IsMemorySpace(Segment: TDOMNode; MemorySpace: Byte): Boolean;
var
  AttributeStr: string;
begin
  Result := False;
  AttributeStr := '';
  if Segment.HasAttributes then
  begin
    if ExtractElementAttribute(Segment, 'space', AttributeStr) then
      Result :=  AttributeStr = IntToStr(MemorySpace);
  end;
end;

procedure TCdiParser.UpdateMemOffsetJump(Element: TDOMNode; var MemOffset: DWord);
var
  i: Integer;
begin
  if Element.HasAttributes then
  begin
    for i := 0 to Element.Attributes.Length - 1 do
    begin
      if LowerCase( Element.Attributes.Item[i].NodeName) = 'origin' then
        MemOffset := StrToInt64( Element.Attributes.Item[i].TextContent)
      else
      if LowerCase( Element.Attributes.Item[i].NodeName) = 'offset' then
        MemOffset := MemOffset + StrToInt64( Element.Attributes.Item[i].TextContent);
    end;
  end;
end;

function TCdiParser.UpdateMemOffsetSize(Element: TDOMNode): DWord;
var
  OffsetModified: Boolean;
  TempStr: string;
begin
  OffsetModified := False;
  TempStr := '';
  if Element.HasAttributes then
  begin
    if Element.HasAttributes then
    begin
      if ExtractElementAttribute(Element, 'size', TempStr) then
      begin
        Result := StrToInt64( TempStr);
        OffsetModified := True;
      end
    end;
  end;
  if not OffsetModified then
  begin
    if LowerCase( Element.NodeName) = 'int' then
      Result := 1
    else
    if LowerCase( Element.NodeName) = 'eventid' then
      Result := 8
    else
  end;
end;

function TCdiParser.AddTab(PageControl: TPageControl; ACaption: string): TScrollBox;
var
  Tab: TTabSheet;
begin
  Tab := PageControl.AddTabSheet;
  Tab.Caption := ACaption;
  Result := TScrollBox.Create(Tab);
  Result.Align := alClient;
  Result.BorderSpacing.Around := 8;
  Result.VertScrollBar.Tracking := True;
  Result.HorzScrollBar.Tracking := True;
  Result.Parent := Tab;
end;

procedure TCdiParser.AddLabel(ParentControl: TScrollBox; ACaption: string;
  var ControlOffset: Integer; ControlMargin, Indent: Integer; Bold: Boolean);
var
  ALabel: TLabel;
begin
  ALabel := TLabel.Create(ParentControl);
  ALabel.Caption := ACaption;
  ALabel.Top := ControlOffset;
  ALabel.Left := Indent;
  if Bold then
    ALabel.Font.Style := [fsBold];
  ALabel.Parent := ParentControl;
  ControlOffset := ControlOffset + ALabel.Height + ControlMargin;
end;

procedure TCdiParser.AddSpinEdit(ParentControl: TScrollBox; Element: TDOMNode; var ControlOffset: Integer; ControlMargin, Indent: Integer; MemOffset, MemSize: DWord; PrintMemOffset: Boolean);
var
  ASpinEdit: TOlcbSpinEdit;
  TempStr: string;
begin
  TempStr := '';
  if PrintMemOffset then
  begin
    AddLabel(ParentControl, 'Offset: ' + IntToStr(MemOffset), ControlOffset, 2, Indent, False);
    AddLabel(ParentControl, 'Size: ' + IntToStr(MemSize), ControlOffset, 2, Indent, False);
  end;
  ASpinEdit := TOlcbSpinEdit.Create(ParentControl);
  if ExtractElementAttribute(Element, 'min', TempStr) then
    ASpinEdit.MinValue := StrToInt(TempStr);
  if ExtractElementAttribute(Element, 'max', TempStr) then
    ASpinEdit.MaxValue := StrToInt(TempStr);
  if ExtractElementAttribute(Element, 'default', TempStr) then
    ASpinEdit.Value := StrToInt(TempStr);
  if ExtractElementItem(Element, 'name', TempStr) then
    AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);
  if ExtractElementItem(Element, 'description', TempStr) then
    AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);
  Inc(Indent, 8);
  ASpinEdit.Top := ControlOffset;
  ASpinEdit.Left := Indent;
  ASpinEdit.Width := 120;
  ASpinEdit.MaxValue := MaxInt;
  ASpinEdit.Parent := ParentControl;
  ASpinEdit.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_Int);
  ControlOffset := ControlOffset + ASpinEdit.Height + ControlMargin;
end;

procedure TCdiParser.AddEdit(ParentControl: TScrollBox; Element: TDOMNode;
  var ControlOffset: Integer; ControlMargin, Indent: Integer; MemOffset,
  MemSize: DWord; PrintMemOffset: Boolean; ElementType: string);
var
  AnEdit: TOlcbEdit;
  TempStr: string;
  i: Integer;
  Size: TSize;
begin
  TempStr := '';
  if PrintMemOffset then
  begin
    AddLabel(ParentControl, 'Offset: ' + IntToStr(MemOffset), ControlOffset, 2, Indent, False);
    AddLabel(ParentControl, 'Size: ' + IntToStr(MemSize), ControlOffset, 2, Indent, False);
  end;
  AnEdit := TOlcbEdit.Create(ParentControl);
  if ExtractElementItem(Element, 'name', TempStr) then
    AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);
  if ExtractElementItem(Element, 'description', TempStr) then
    AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);
  AnEdit.Top := ControlOffset;
  AnEdit.Left := Indent;
  AnEdit.Parent := ParentControl;
  for i := 0 to MemSize - 1 do
    TempStr := TempStr + 'Y';
  Size := Application.MainForm.Canvas.TextExtent(TempStr);
  if ElementType = 'eventid' then
    AnEdit.Width := Round( Size.cx * 3.2)
  else
    AnEdit.Width := Round( Size.cx * 1.2);
  if AnEdit.Left + AnEdit.Width > ParentControl.Width then
    AnEdit.Width := ParentControl.Width - AnEdit.Left - 32;
  // Can't set anchors here if something that forces the client width wider is created this control is "pulled" wider
  if ElementType = 'eventid' then
    AnEdit.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_EventID)
  else
  if ElementType = 'int' then
    AnEdit.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_Int)
  else
  if ElementType = 'string' then
    AnEdit.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_String);

  ControlOffset := ControlOffset + AnEdit.Height + ControlMargin;
end;

procedure TCdiParser.AddComboBoxList(ParentControl: TScrollBox;
  Element: TDOMNode; var ControlOffset: Integer; ControlMargin,
  Indent: Integer; MemOffset, MemSize: DWord; PrintMemOffset: Boolean);
var
  AComboBoxList: TOlcbComboBox;
  TempStr, LongestStr, ValueStr, PropertyStr: string;
  MapNode, ChildNode: TDOMNode;
  DoIndent: Boolean;
  Size: TSize;
begin
  TempStr := '';
  if PrintMemOffset then
  begin
    AddLabel(ParentControl, 'Offset: ' + IntToStr(MemOffset), ControlOffset, 2, Indent, False);
    AddLabel(ParentControl, 'Size: ' + IntToStr(MemSize), ControlOffset, 2, Indent, False);
  end;
  MapNode := Element.FindNode('map');
  if MapNode <> nil then
  begin
    AComboBoxList := TOlcbComboBox.Create(ParentControl);
    if ExtractElementItem(Element, 'name', TempStr) then
      AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);
    if ExtractElementItem(Element, 'description', TempStr) then
      AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);
    Inc(Indent, 8);

    // The map can have a name and description too
    DoIndent := False;
    if ExtractElementItem(MapNode, 'name', TempStr) then
    begin
      AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);
      DoIndent := True;
    end;
    if ExtractElementItem(MapNode, 'description', TempStr) then
    begin
      AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);
      DoIndent := True
    end;
    if DoIndent then
      Inc(Indent, 8);

    AComboBoxList.AutoSize := True;
    AComboBoxList.Top := ControlOffset;
    AComboBoxList.Left := Indent;
    AComboBoxList.Style := csDropDownList;
    LongestStr := '';
    AComboBoxList.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_Int);
    ChildNode := MapNode.FirstChild;
    while ChildNode <> nil do
    begin
      if LowerCase( ChildNode.NodeName) = 'relation' then
      begin
        PropertyStr := '';
        ValueStr := '';
        if ExtractElementItem(ChildNode, 'value', ValueStr) then
        begin
          AComboBoxList.Items.Add(ValueStr);
          if Length(ValueStr) > Length(LongestStr) then
            LongestStr := ValueStr;
        end;
        PropertyStr := '';
        ExtractElementItem(ChildNode, 'property', PropertyStr);
        AComboBoxList.ConfigInfo.MapList.AddRelation(ValueStr, PropertyStr);
      end;
      ChildNode := ChildNode.NextSibling;
    end;
    AComboBoxList.ItemIndex := 0;

    Size := Application.MainForm.Canvas.TextExtent(LongestStr);
    AComboBoxList.Width := Round( Size.cx + 50);

    AComboBoxList.Parent := ParentControl;
    ControlOffset := ControlOffset + AComboBoxList.Height + ControlMargin;
  end;
end;

procedure TCdiParser.ProcessElementForUI(ParentControl: TScrollBox; Element: TDOMNode; var MemOffset: DWord; var ControlOffset: Integer; Indent: Integer; SupressNameAndDescription: Boolean; PrintMemOffset: Boolean);
var
  Group_Child, Map_Child: TDOMNode;
  TempStr: string;
  ReplicationCount, i: Integer;
  MemSize: DWord;
begin
 if Element <> nil then
 begin
   TempStr := '';
   // If it is a group then run into the group
   if LowerCase( Element.NodeName) = 'group' then
   begin
     Inc(Indent, 8);
     UpdateMemOffsetJump(Element, MemOffset);      // Group may override the Offset
     if ExtractElementItem(Element, 'name', TempStr) then
       AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, True);
     if ExtractElementItem(Element, 'description', TempStr) then
       AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);
     if ExtractElementAttribute(Element, 'replication', TempStr) then
       ReplicationCount := StrToInt(TempStr)
     else
       ReplicationCount := 1;
     ExtractElementItem(Element, 'repname', TempStr);
     for i := 1 to ReplicationCount do
     begin
       if TempStr <> '' then
         AddLabel(ParentControl, TempStr + ' ' + IntToStr(i), ControlOffset, 2, Indent + 8, False);
       Group_Child := Element.FirstChild;
       while Group_Child <> nil do
       begin
         ProcessElementForUI(ParentControl, Group_Child, MemOffset, ControlOffset, Indent + 16, True, PrintMemOffset);
         Group_Child := Group_Child.NextSibling;
       end;
     end;
   end else
   begin
     // It is not a group
     if (LowerCase(Element.NodeName) = 'name') or (LowerCase(Element.NodeName) = 'description') then
     begin
       if not SupressNameAndDescription then
         AddLabel(ParentControl, Element.TextContent, ControlOffset, 2, Indent, False);
     end else
     if LowerCase(Element.NodeName) = 'int' then
     begin
       UpdateMemOffsetJump(Element, MemOffset);
       MemSize := UpdateMemOffsetSize(Element);
       Map_Child := Element.FindNode('map');
       if Map_Child = nil then
         AddSpinEdit(ParentControl, Element, ControlOffset, 4, Indent + 4, MemOffset, MemSize, PrintMemOffset)
       else
         AddComboBoxList(ParentControl, Element, ControlOffset, 4, Indent + 4, MemOffset, MemSize, PrintMemOffset);
       Inc(MemOffset, MemSize);
     end else
     if LowerCase(Element.NodeName) = 'bit' then
     begin
       UpdateMemOffsetJump(Element, MemOffset);
       MemSize := UpdateMemOffsetSize(Element);
       Map_Child := Element.FindNode('map');       // Think a bit MUST have a map
       if Map_Child <> nil then
         AddComboBoxList(ParentControl, Element, ControlOffset, 4, Indent + 4, MemOffset, MemSize, PrintMemOffset);
       Inc(MemOffset, MemSize);
     end else
     if (LowerCase(Element.NodeName) = 'string') or (LowerCase(Element.NodeName) = 'eventid') then
     begin
       UpdateMemOffsetJump(Element, MemOffset);
       MemSize := UpdateMemOffsetSize(Element);
       Map_Child := Element.FindNode('map');
       if Map_Child = nil then
         AddEdit(ParentControl, Element, ControlOffset, 4, Indent + 4, MemOffset, MemSize, PrintMemOffset, Element.NodeName)
       else
         AddComboBoxList(ParentControl, Element, ControlOffset, 4, Indent + 4, MemOffset, MemSize, PrintMemOffset);
       Inc(MemOffset, MemSize);
     end else
   end;
 end;
end;

function TCdiParser.Build_CDI_Interface(ParentControl: TPanel; CDI: TXMLDocument): TPageControl;
const
  IDENTIFICATION_INDENT = 16;
var
  CDI_Root, Cdi_Child, Identification_Root, Identification_Child, Segment_Root, Segment_Child, Map_Child, Relation_Child: TDOMNode;
  ScrollBox: TScrollBox;
  MemOffset: DWord;
  ControlOffset: Integer;
  ItemStr: string;
 begin
  ItemStr := '';
  Clear_CDI_Interface(ParentControl);
    ParentControl.Caption := '';
  Result := TPageControl.Create(ParentControl);
  Result.Align := alClient;
  Result.Parent := ParentControl;
  ErrorCode := 0;
  CDI_Root := CDI.FindNode('cdi');
  if Assigned(CDI_Root) then
  begin
    Identification_Root := CDI_Root.FindNode('identification');
    if Assigned(Identification_Root) then
    begin
      ControlOffset := 0;
      ScrollBox := AddTab(Result, 'Identification');

      AddLabel(ScrollBox, ' ', ControlOffset, 4, 0, False);    // Space on the Top

      AddLabel(ScrollBox, 'Manufacturer: ', ControlOffset, 2, IDENTIFICATION_INDENT, True);
      Identification_Child := Identification_Root.FindNode('manufacturer');
      if Assigned(Identification_Child) then
        AddLabel(ScrollBox, ItemStr + Identification_Child.TextContent, ControlOffset, 2, IDENTIFICATION_INDENT + 4, False)
      else
        AddLabel(ScrollBox, ' ', ControlOffset, 2, 0, False);

      AddLabel(ScrollBox, 'Model: ', ControlOffset, 2, IDENTIFICATION_INDENT, True);
      Identification_Child := Identification_Root.FindNode('model');
      if Assigned(Identification_Child) then
        AddLabel(ScrollBox, ItemStr + Identification_Child.TextContent, ControlOffset, 2, IDENTIFICATION_INDENT + 4, False)
      else
        AddLabel(ScrollBox, ItemStr, ControlOffset, 2, 0, False);

      AddLabel(ScrollBox, 'Hardware Version: ', ControlOffset, 2, IDENTIFICATION_INDENT, True);
      Identification_Child := Identification_Root.FindNode('hardwareVersion');
      if Assigned(Identification_Child) then
        AddLabel(ScrollBox, ItemStr + Identification_Child.TextContent, ControlOffset, 2, IDENTIFICATION_INDENT + 4, False)
      else
        AddLabel(ScrollBox, ItemStr, ControlOffset, 2, 0, False);

      AddLabel(ScrollBox, 'Software Version: ', ControlOffset, 2, IDENTIFICATION_INDENT, True);
      Identification_Child := Identification_Root.FindNode('softwareVersion');
      if Assigned(Identification_Child) then
        AddLabel(ScrollBox, ItemStr + Identification_Child.TextContent, ControlOffset, 2, IDENTIFICATION_INDENT + 4, False)
      else
        AddLabel(ScrollBox, ItemStr, ControlOffset, 2, 0, False);

      Inc(ControlOffset, 8);
      Identification_Child := Identification_Root.FirstChild;
      while Assigned(Identification_Child) do
      begin
        if LowerCase( Identification_Child.NodeName) = 'map' then
        begin
          Map_Child := Identification_Child.FirstChild;
          while Assigned(Map_Child) do
          begin
            if LowerCase( Map_Child.NodeName) = 'relation' then
            begin
              Relation_Child := Map_Child.FirstChild;
              while Assigned(Relation_Child) do
              begin
                if (LowerCase( Relation_Child.NodeName) = 'value') then
                  AddLabel(ScrollBox, Relation_Child.TextContent, ControlOffset, 2, IDENTIFICATION_INDENT + 16, False)
                else
                if (LowerCase( Relation_Child.NodeName) = 'property') then
                   AddLabel(ScrollBox, Relation_Child.TextContent, ControlOffset, 2, IDENTIFICATION_INDENT + 8, False);
                Relation_Child := Relation_Child.NextSibling;
              end;
              Map_Child := Map_Child.NextSibling;
            end
          end;
        end;
        Identification_Child := Identification_Child.NextSibling;
      end;
      AddLabel(ScrollBox, ' ', ControlOffset, 4, 0, False);    // Space on the Bottom
    end;

    ControlOffset := 0;
    Cdi_Child := CDI_Root.FirstChild;
    while Assigned(Cdi_Child) do
    begin
      if LowerCase(Cdi_Child.NodeName) = 'segment' then
      begin
        Segment_Root := Cdi_Child;
        // First Find the Config Memory Segment
        if IsMemorySpace(Segment_Root, 253) then
        begin
          ControlOffset := 0;
          MemOffset := 0;
          AddLabel(ScrollBox, ' ', ControlOffset, 4, 0, False);    // Space on the Top
          // Add a new Tabsheet for this Segment using it Name Element as the tab title
          if ExtractElementItem(Segment_Root, 'name', ItemStr) then
            ScrollBox := AddTab(Result, ItemStr)
          else
            ScrollBox := AddTab(Result, '[Unknown]');
          Result.ActivePageIndex := Result.PageCount - 1;     // Select it to create the window so the size of the Scrollbox is correct
          Result.ActivePageIndex := 0;                             // Set it back to a simple tab so it builds faster
          // Add the description of this segment as the first line of the Tab Page
          if ExtractElementItem(Segment_Root, 'description', ItemStr) then
            AddLabel(ScrollBox, ItemStr, ControlOffset, 4, 4, False);
          // Time to build the UI for this segment
          UpdateMemOffsetJump(Segment_Root, MemOffset);      // Segment may override the Offset
          Segment_Child := Segment_Root.FirstChild;
          while Segment_Child <> nil do
          begin
            ProcessElementForUI(ScrollBox, Segment_Child, MemOffset, ControlOffset, 4, True, False);
            Segment_Child := Segment_Child.NextSibling;
          end;
          AddLabel(ScrollBox, ' ', ControlOffset, 4, 0, False);    // Space on the bottom
        end;
      end;
      Cdi_Child := Cdi_Child.NextSibling;
    end;
  end else
    ErrorCode := 1;   // No CDI Element
end;

procedure TCdiParser.Clear_CDI_Interface(ParentControl: TPanel);
var
  i: Integer;
begin
  for i := 0 to ParentControl.ControlCount - 1 do
    ParentControl.Controls[i].Free;
  ParentControl.Caption := 'Loading CDI File...';
end;


{ TConfigInfo }

constructor TConfigInfo.Create(MemOffset, MemSize: DWord;
  ADataType: TOlcbConfigDataType);
begin
  inherited Create;
  FIsDirty := False;
  FIsUnknownState := True;
  FConfigMemAddress := MemOffset;
  FConfigMemSize := MemSize;
  FTask := nil;
  FDataType := ADataType;
  MapList := TMap.Create;
end;

destructor TConfigInfo.Destroy;
begin
   FreeAndNil( FMapList);
  inherited Destroy;
end;

end.



