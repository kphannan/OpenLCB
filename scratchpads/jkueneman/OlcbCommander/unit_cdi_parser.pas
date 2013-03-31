unit unit_cdi_parser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Spin, laz2_DOM, laz2_XMLRead, Types, olcb_common_tasks,
  olcb_threaded_stack, Buttons;

const
  CV_BUTTON_WIDTH = 80;


type

  TOlcbConfigDataType = (cdt_String, cdt_Int, cdt_EventID, cdt_Bit);
  TOlcbConfigState = (ocs_Current, ocs_Unknown);

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
    FMapList: TMap;
    FOnChangeState: TNotifyEvent;
    FState: TOlcbConfigState;
    FTask: TOlcbTaskBase;
    procedure SetState(AValue: TOlcbConfigState);
  public
    constructor Create(MemOffset, MemSize: DWord; ADataType: TOlcbConfigDataType);
    destructor Destroy; override;
    property ConfigMemAddress: DWord read FConfigMemAddress write FConfigMemAddress;
    property ConfigMemSize: DWord read FConfigMemSize write FConfigMemSize;
    property Task: TOlcbTaskBase read FTask write FTask;
    property DataType: TOlcbConfigDataType read FDataType write FDataType;
    property MapList: TMap read FMapList write FMapList;
    property State: TOlcbConfigState read FState write SetState;
    property OnChangeState: TNotifyEvent read FOnChangeState write FOnChangeState;
  end;

  { TOlcbSpinEdit }

  TOlcbSpinEdit = class(TSpinEdit)
  private
    FCompareCVSpeedButton: TSpeedButton;
    FConfigInfo: TConfigInfo;
    FImageList16x16: TImageList;
    FReadCVSpeedButton: TSpeedButton;
    FStateImage: TImage;
    FWriteCVSpeedButton: TSpeedButton;
  protected
    procedure OnDrawImageState(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property ConfigInfo: TConfigInfo read FConfigInfo write FConfigInfo;
    property ReadCVSpeedButton: TSpeedButton read FReadCVSpeedButton write FReadCVSpeedButton;
    property WriteCVSpeedButton: TSpeedButton read FWriteCVSpeedButton write FWriteCVSpeedButton;
    property CompareCVSpeedButton: TSpeedButton read FCompareCVSpeedButton write FCompareCVSpeedButton;
    property ImageList16x16: TImageList read FImageList16x16 write FImageList16x16;
    property StateImage: TImage read FStateImage write FStateImage;
  end;

  { TOlcbEdit }

  TOlcbEdit = class(TEdit)
  private
    FCompareCVSpeedButton: TSpeedButton;
    FConfigInfo: TConfigInfo;
    FImageList16x16: TImageList;
    FReadCVSpeedButton: TSpeedButton;
    FStateImage: TImage;
    FWriteCVSpeedButton: TSpeedButton;
  protected
    procedure OnDrawImageState(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property ConfigInfo: TConfigInfo read FConfigInfo write FConfigInfo;
    property ReadCVSpeedButton: TSpeedButton read FReadCVSpeedButton write FReadCVSpeedButton;
    property WriteCVSpeedButton: TSpeedButton read FWriteCVSpeedButton write FWriteCVSpeedButton;
    property CompareCVSpeedButton: TSpeedButton read FCompareCVSpeedButton write FCompareCVSpeedButton;
    property ImageList16x16: TImageList read FImageList16x16 write FImageList16x16;
    property StateImage: TImage read FStateImage write FStateImage;
  end;

  { TOlcbComboBox }

  TOlcbComboBox = class(TComboBox)
  private
    FCompareCVSpeedButton: TSpeedButton;
    FConfigInfo: TConfigInfo;
    FImageList16x16: TImageList;
    FReadCVSpeedButton: TSpeedButton;
    FStateImage: TImage;
    FWriteCVSpeedButton: TSpeedButton;
  protected
    procedure OnDrawImageState(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property ConfigInfo: TConfigInfo read FConfigInfo write FConfigInfo;
    property ReadCVSpeedButton: TSpeedButton read FReadCVSpeedButton write FReadCVSpeedButton;
    property WriteCVSpeedButton: TSpeedButton read FWriteCVSpeedButton write FWriteCVSpeedButton;
    property CompareCVSpeedButton: TSpeedButton read FCompareCVSpeedButton write FCompareCVSpeedButton;
    property ImageList16x16: TImageList read FImageList16x16 write FImageList16x16;
    property StateImage: TImage read FStateImage write FStateImage;
  end;

  { TCdiParser }

  TCdiParser = class
  private
    FImageList16x16: TImageList;
    FOnSpeedButtonReadConfigClickCallback: TNotifyEvent;
    FOnSpeedButtonWriteConfigClickCallback: TNotifyEvent;
  protected
    procedure AddSpeedButtonGlyph(SpeedButton: TSpeedButton; ImageListIndex: Integer);
    function ExtractElementItem(Element: TDOMNode; Item: string; var ItemStr: string): Boolean;
    function ExtractElementAttribute(Element: TDOMNode; AttributeName: string; var AttributeStr: string): Boolean;
    function IsMemorySpace(Segment: TDOMNode; MemorySpace: Byte): Boolean;
    procedure UpdateMemOffsetJump(Element: TDOMNode; var MemOffset: DWord);
    function UpdateMemOffsetSize(Element: TDOMNode): DWord;
    function AddTab(PageControl: TPageControl; ACaption: string): TScrollBox;
    procedure AddLabel(ParentControl: TScrollBox; ACaption: string; var ControlOffset: Integer; ControlMargin, Indent: Integer; Bold: Boolean);
    procedure AddSpinEdit(ParentControl: TScrollBox; Element: TDOMNode; var ControlOffset: Integer; ControlMargin, Indent: Integer; MemOffset, MemSize: DWord; PrintMemOffset: Boolean; ElementType: string);
    procedure AddEdit(ParentControl: TScrollBox; Element: TDOMNode; var ControlOffset: Integer; ControlMargin, Indent: Integer; MemOffset, MemSize: DWord; PrintMemOffset: Boolean; ElementType: string);
    procedure AddComboBoxList(ParentControl: TScrollBox; Element: TDOMNode; var ControlOffset: Integer; ControlMargin, Indent: Integer; MemOffset, MemSize: DWord; PrintMemOffset: Boolean; ElementType: string);
    procedure ProcessElementForUI(ParentControl: TScrollBox; Element: TDOMNode; var MemOffset: DWord; var ControlOffset: Integer; Indent: Integer; SupressNameAndDescription: Boolean; PrintMemOffset: Boolean);
    procedure OnSpinEditChange(Sender: TObject);
    procedure OnEditChange(Sender: TObject);
    procedure OnComboBoxChange(Sender: TObject);
  public
    constructor Create;
    function Build_CDI_Interface(ParentControl: TPanel; CDI: TXMLDocument): TPageControl;
    procedure Clear_CDI_Interface(ParentControl: TPanel);
    property ImageList16x16: TImageList read FImageList16x16 write FImageList16x16;
    property OnSpeedButtonReadConfigClickCallback: TNotifyEvent read FOnSpeedButtonReadConfigClickCallback write FOnSpeedButtonReadConfigClickCallback;
    property OnSpeedButtonWriteConfigClickCallback: TNotifyEvent read FOnSpeedButtonWriteConfigClickCallback write FOnSpeedButtonWriteConfigClickCallback;
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

procedure TOlcbComboBox.OnDrawImageState(Sender: TObject);
var
  ImageIndex: Integer;
  Bitmap: TBitmap;
begin
  case ConfigInfo.State of
    ocs_Current :
      begin
        ImageIndex := 6;
        StateImage.Picture.Clear;
      end;
    ocs_Unknown :
      begin
        ImageIndex := 52;
        Bitmap := TBitmap.Create;
        Bitmap.Width := ImageList16x16.Width;
        Bitmap.Height := ImageList16x16.Height;
        ImageList16x16.GetBitmap(ImageIndex, Bitmap);
        StateImage.Picture.Graphic := Bitmap;
        Bitmap.Free;
      end;
  end;
end;

constructor TOlcbComboBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FConfigInfo := nil;
  ReadCVSpeedButton := TSpeedButton.Create(Self);
  WriteCVSpeedButton := TSpeedButton.Create(Self);
  CompareCVSpeedButton := TSpeedButton.Create(Self);
  StateImage := TImage.Create(Self);
  FImageList16x16 := nil;
end;

destructor TOlcbComboBox.Destroy;
begin
  FreeAndNil( FConfigInfo);
  inherited Destroy;
end;

{ TOlcbEdit }

procedure TOlcbEdit.OnDrawImageState(Sender: TObject);
var
  ImageIndex: Integer;
  Bitmap: TBitmap;
begin
  case ConfigInfo.State of
    ocs_Current :
      begin
        ImageIndex := 6;
        StateImage.Picture.Clear;
      end;
    ocs_Unknown :
      begin
        ImageIndex := 52;
        Bitmap := TBitmap.Create;
        Bitmap.Width := ImageList16x16.Width;
        Bitmap.Height := ImageList16x16.Height;
        ImageList16x16.GetBitmap(ImageIndex, Bitmap);
        StateImage.Picture.Graphic := Bitmap;
        Bitmap.Free;
      end;
  end;
end;

constructor TOlcbEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FConfigInfo := nil;
  ReadCVSpeedButton := TSpeedButton.Create(Self);
  WriteCVSpeedButton := TSpeedButton.Create(Self);
  CompareCVSpeedButton := TSpeedButton.Create(Self);
  StateImage := TImage.Create(Self);
  ImageList16x16 := nil;
end;

destructor TOlcbEdit.Destroy;
begin
  FreeAndNil( FConfigInfo);
  inherited Destroy;
end;

{ TOlcbSpinEdit }

procedure TOlcbSpinEdit.OnDrawImageState(Sender: TObject);
var
  ImageIndex: Integer;
  Bitmap: TBitmap;
begin
  case ConfigInfo.State of
    ocs_Current :
      begin
        ImageIndex := 6;
        StateImage.Picture.Clear;
      end;
    ocs_Unknown :
      begin
        ImageIndex := 52;
        Bitmap := TBitmap.Create;
        Bitmap.Width := ImageList16x16.Width;
        Bitmap.Height := ImageList16x16.Height;
        ImageList16x16.GetBitmap(ImageIndex, Bitmap);
        StateImage.Picture.Graphic := Bitmap;
        Bitmap.Free;
      end;
  end;
end;

constructor TOlcbSpinEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FConfigInfo := nil;
  ReadCVSpeedButton := TSpeedButton.Create(Self);
  WriteCVSpeedButton := TSpeedButton.Create(Self);
  CompareCVSpeedButton := TSpeedButton.Create(Self);
  StateImage := TImage.Create(Self);
  ImageList16x16 := nil;
end;

destructor TOlcbSpinEdit.Destroy;
begin
  FreeAndNil( FConfigInfo);
  inherited Destroy;
end;

{ TCdiParser }

procedure TCdiParser.AddSpeedButtonGlyph(SpeedButton: TSpeedButton; ImageListIndex: Integer);
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  Bitmap.Width := ImageList16x16.Width;
  Bitmap.Height := ImageList16x16.Height;
  ImageList16x16.GetBitmap(ImageListIndex, Bitmap);
  SpeedButton.Glyph.Assign(Bitmap);
  Bitmap.Free;
end;

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
        if Element.NodeName = 'bit' then
        begin
          Result := (Result div 8);
          if Result mod 8 <> 0 then
            Inc(Result);
        end;
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

procedure TCdiParser.AddSpinEdit(ParentControl: TScrollBox; Element: TDOMNode;
  var ControlOffset: Integer; ControlMargin, Indent: Integer; MemOffset,
  MemSize: DWord; PrintMemOffset: Boolean; ElementType: string);
var
  ASpinEdit: TOlcbSpinEdit;
  TempStr: string;
begin
  TempStr := '';

  // Debug Printing
  if PrintMemOffset then
  begin
    AddLabel(ParentControl, 'Offset: ' + IntToStr(MemOffset), ControlOffset, 2, Indent, False);
    AddLabel(ParentControl, 'Size: ' + IntToStr(MemSize), ControlOffset, 2, Indent, False);
  end;

  // Create the SpinEdit
  ASpinEdit := TOlcbSpinEdit.Create(ParentControl);
  ASpinEdit.Width := 120;
  ASpinEdit.MaxValue := MaxInt;

  // Extract special modifiers
  if ExtractElementItem(Element, 'min', TempStr) then
    ASpinEdit.MinValue := StrToInt(TempStr);
  if ExtractElementItem(Element, 'max', TempStr) then
    ASpinEdit.MaxValue := StrToInt(TempStr);
  if ExtractElementItem(Element, 'default', TempStr) then
    ASpinEdit.Value := StrToInt(TempStr);
  ASpinEdit.Text := '';

  // Look for descripive names and descriptions to print
  if ExtractElementItem(Element, 'name', TempStr) then
    AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);
  if ExtractElementItem(Element, 'description', TempStr) then
    AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);
  Inc(Indent, 8);

  // Create the ConfigInfo Struture
  if ElementType = 'int' then
    ASpinEdit.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_Int)
  else
  if ElementType = 'bit' then
  begin
    ASpinEdit.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_bit);
    ASpinEdit.MaxValue := 1;
    ASpinEdit.MinValue := 0;
  end;
  ASpinEdit.ConfigInfo.OnChangeState := @ASpinEdit.OnDrawImageState;

  // Create the Control Window
  ASpinEdit.StateImage.Left := Indent;
  ASpinEdit.StateImage.Top := ControlOffset;
  ASpinEdit.StateImage.Width := 20; // ImageList16x16.Width;
  ASpinEdit.StateImage.Height := 20; //ImageList16x16.Height;
  ASpinEdit.ImageList16x16 := ImageList16x16;
  ASpinEdit.OnDrawImageState(ASpinEdit.ConfigInfo);
  ASpinEdit.StateImage.Parent := ParentControl;

  ASpinEdit.Top := ControlOffset;
  ASpinEdit.Left := ASpinEdit.StateImage.Left + ASpinEdit.StateImage.Width + 8;
  ASpinEdit.OnChange := @OnSpinEditChange;
  ASpinEdit.Parent := ParentControl;

  ASpinEdit.ReadCVSpeedButton.Left := ASpinEdit.Left + ASpinEdit.Width + 4;
  ASpinEdit.ReadCVSpeedButton.Top := ASpinEdit.Top;
  ASpinEdit.ReadCVSpeedButton.Height := ASpinEdit.Height;
  ASpinEdit.ReadCVSpeedButton.Width := CV_BUTTON_WIDTH;
  ASpinEdit.ReadCVSpeedButton.Caption := 'Read';
  ASpinEdit.ReadCVSpeedButton.OnClick := OnSpeedButtonReadConfigClickCallback;
  AddSpeedButtonGlyph(ASpinEdit.ReadCVSpeedButton, 55);
  ASpinEdit.ReadCVSpeedButton.Parent := ParentControl;

  ASpinEdit.WriteCVSpeedButton.Left := ASpinEdit.ReadCVSpeedButton.Left + ASpinEdit.ReadCVSpeedButton.Width + 4;
  ASpinEdit.WriteCVSpeedButton.Top := ASpinEdit.Top;
  ASpinEdit.WriteCVSpeedButton.Height := ASpinEdit.Height;
  ASpinEdit.WriteCVSpeedButton.Width := CV_BUTTON_WIDTH;
  ASpinEdit.WriteCVSpeedButton.Caption := 'Write';
  ASpinEdit.WriteCVSpeedButton.OnClick := OnSpeedButtonWriteConfigClickCallback;
  AddSpeedButtonGlyph(ASpinEdit.WriteCVSpeedButton, 12);
  ASpinEdit.WriteCVSpeedButton.Parent := ParentControl;

{  ASpinEdit.CompareCVSpeedButton.Left := ASpinEdit.WriteCVSpeedButton.Left + ASpinEdit.WriteCVSpeedButton.Width + 4;
  ASpinEdit.CompareCVSpeedButton.Top := ASpinEdit.Top;
  ASpinEdit.CompareCVSpeedButton.Height := ASpinEdit.Height;
  ASpinEdit.CompareCVSpeedButton.Width := CV_BUTTON_WIDTH;
  ASpinEdit.CompareCVSpeedButton.Caption := 'Compare';
  ASpinEdit.CompareCVSpeedButton.OnClick := OnSpeedButtonReadConfigClickCallback;
  ASpinEdit.CompareCVSpeedButton.Parent := ParentControl;    }

  // Update the Control Offsets
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

  // Debug Printing
  if PrintMemOffset then
  begin
    AddLabel(ParentControl, 'Offset: ' + IntToStr(MemOffset), ControlOffset, 2, Indent, False);
    AddLabel(ParentControl, 'Size: ' + IntToStr(MemSize), ControlOffset, 2, Indent, False);
  end;

  // Create the Edit
  AnEdit := TOlcbEdit.Create(ParentControl);
  AnEdit.Left := Indent + ImageList16x16.Width + 8;    // Need valid to deal with setting the width

  // Look for descripive names and descriptions to print
  if ExtractElementItem(Element, 'name', TempStr) then
    AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);
  if ExtractElementItem(Element, 'description', TempStr) then
    AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);

  // Create the ConfigInfo Struture
  if ElementType = 'eventid' then
    AnEdit.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_EventID)
  else
  if ElementType = 'int' then
    AnEdit.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_Int)
  else
  if ElementType = 'string' then
    AnEdit.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_String)
  else
  if ElementType = 'bit' then
    AnEdit.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_bit);
  AnEdit.ConfigInfo.OnChangeState := @AnEdit.OnDrawImageState;

  // Calculate the Width of the control needed
  for i := 0 to MemSize - 1 do
    TempStr := TempStr + 'Y';
  Size := Application.MainForm.Canvas.TextExtent(TempStr);
  if ElementType = 'eventid' then
    AnEdit.Width := Round( Size.cx * 3.2)
  else
    AnEdit.Width := Round( Size.cx * 1.2);
  if AnEdit.Left + AnEdit.Width + (2*(CV_BUTTON_WIDTH+4)) + 32 > ParentControl.Parent.Width then      // The ScrollWindow can be wider than the view
    AnEdit.Width := ParentControl.Parent.Width - (AnEdit.Left + (2*(CV_BUTTON_WIDTH+4) + 32));

  // Create the Control Window
  AnEdit.StateImage.Left := Indent;
  AnEdit.StateImage.Top := ControlOffset;
  AnEdit.StateImage.Width := ImageList16x16.Width;
  AnEdit.StateImage.Height := ImageList16x16.Height;
  AnEdit.ImageList16x16 := ImageList16x16;
  AnEdit.OnDrawImageState(AnEdit.ConfigInfo);
 // AnEdit.Anchors := [akLeft, akRight, akTop];
  AnEdit.StateImage.Parent := ParentControl;

  AnEdit.Top := ControlOffset;
  AnEdit.OnChange := @OnEditChange;
  AnEdit.Parent := ParentControl;

  AnEdit.ReadCVSpeedButton.Left := AnEdit.Left + AnEdit.Width + 4;
  AnEdit.ReadCVSpeedButton.Top := AnEdit.Top;
  AnEdit.ReadCVSpeedButton.Height := AnEdit.Height;
  AnEdit.ReadCVSpeedButton.Width := CV_BUTTON_WIDTH;
  AnEdit.ReadCVSpeedButton.Caption := 'Read';
  AnEdit.ReadCVSpeedButton.OnClick := OnSpeedButtonReadConfigClickCallback;
  AddSpeedButtonGlyph(AnEdit.ReadCVSpeedButton, 55);
//  AnEdit.ReadCVSpeedButton.Anchors := [akRight, akTop];
  AnEdit.ReadCVSpeedButton.Parent := ParentControl;

  AnEdit.WriteCVSpeedButton.Left := AnEdit.ReadCVSpeedButton.Left + AnEdit.ReadCVSpeedButton.Width + 4;
  AnEdit.WriteCVSpeedButton.Top := AnEdit.Top;
  AnEdit.WriteCVSpeedButton.Height := AnEdit.Height;
  AnEdit.WriteCVSpeedButton.Width := CV_BUTTON_WIDTH;
  AnEdit.WriteCVSpeedButton.Caption := 'Write';
  AnEdit.WriteCVSpeedButton.OnClick := OnSpeedButtonWriteConfigClickCallback;
  AddSpeedButtonGlyph(AnEdit.WriteCVSpeedButton, 12);
 // AnEdit.WriteCVSpeedButton.Anchors := [akRight, akTop];
  AnEdit.WriteCVSpeedButton.Parent := ParentControl;

{  AnEdit.CompareCVSpeedButton.Left := AnEdit.WriteCVSpeedButton.Left + AnEdit.WriteCVSpeedButton.Width + 4;
  AnEdit.CompareCVSpeedButton.Top := AnEdit.Top;
  AnEdit.CompareCVSpeedButton.Height := AnEdit.Height;
  AnEdit.CompareCVSpeedButton.Width := CV_BUTTON_WIDTH;
  AnEdit.CompareCVSpeedButton.Caption := 'Compare';
  AnEdit.CompareCVSpeedButton.OnClick := OnSpeedButtonReadConfigClickCallback;
  AnEdit.CompareCVSpeedButton.Parent := ParentControl; }

  // Update the Control Offsets
  ControlOffset := ControlOffset + AnEdit.Height + ControlMargin;
end;

procedure TCdiParser.AddComboBoxList(ParentControl: TScrollBox;
  Element: TDOMNode; var ControlOffset: Integer; ControlMargin,
  Indent: Integer; MemOffset, MemSize: DWord; PrintMemOffset: Boolean;
  ElementType: string);
var
  AComboBoxList: TOlcbComboBox;
  TempStr, LongestStr, ValueStr, PropertyStr: string;
  MapNode, ChildNode: TDOMNode;
  DoIndent: Boolean;
  Size: TSize;
begin
  TempStr := '';

  // Debug Printing
  if PrintMemOffset then
  begin
    AddLabel(ParentControl, 'Offset: ' + IntToStr(MemOffset), ControlOffset, 2, Indent, False);
    AddLabel(ParentControl, 'Size: ' + IntToStr(MemSize), ControlOffset, 2, Indent, False);
  end;

  // Find the map for the element
  MapNode := Element.FindNode('map');

  // A ComboBox is only used for a map element
  if MapNode <> nil then
  begin
    // Create the ComboBox
    AComboBoxList := TOlcbComboBox.Create(ParentControl);
    AComboBoxList.AutoSize := True;
    AComboBoxList.Style := csDropDownList;

    // Look for descripive names and descriptions to print
    if ExtractElementItem(Element, 'name', TempStr) then
      AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);
    if ExtractElementItem(Element, 'description', TempStr) then
      AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);
    Inc(Indent, 8);

    // The map can have a name and description too, look for them and print
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

    // If there were map descriptions then indent the following deeper than the descriptions
    if DoIndent then
      Inc(Indent, 8);

    // Create the ConfigInfo Struture
    if ElementType = 'eventid' then
      AComboBoxList.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_EventID)
    else
    if ElementType = 'int' then
      AComboBoxList.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_Int)
    else
    if ElementType = 'string' then
      AComboBoxList.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_String)
    else
    if ElementType = 'bit' then
      AComboBoxList.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_bit);
    AComboBoxList.ConfigInfo.OnChangeState := @AComboBoxList.OnDrawImageState;

    // Run the children of the map looking for its relations
    LongestStr := '';
    ChildNode := MapNode.FirstChild;
    while ChildNode <> nil do
    begin
      if LowerCase( ChildNode.NodeName) = 'relation' then
      begin
        // Found a relation
        PropertyStr := '';
        ValueStr := '';
        // Look for the value
        if ExtractElementItem(ChildNode, 'value', ValueStr) then
        begin
          // Found the value add it to the Listbox
          AComboBoxList.Items.Add(ValueStr);
          // Track the longest string so the control width can be set later
          if Length(ValueStr) > Length(LongestStr) then
            LongestStr := ValueStr;
        end;
        PropertyStr := '';
        // Look for the property
        ExtractElementItem(ChildNode, 'property', PropertyStr);
        // Create a list of relations for later use in the ComboBox
        AComboBoxList.ConfigInfo.MapList.AddRelation(ValueStr, PropertyStr);
      end;
      ChildNode := ChildNode.NextSibling;
    end;

    // Deselect any relation so it is clear it is not a valid value yet (need to read the config memory to select the correct one)
    AComboBoxList.ItemIndex := -1;

    // Calculate the correct size to display all the text
    Size := Application.MainForm.Canvas.TextExtent(LongestStr);
    AComboBoxList.Width := Round( Size.cx + 50);

    // Create the Control Window
    AComboBoxList.StateImage.Left := Indent;
    AComboBoxList.StateImage.Top := ControlOffset;
    AComboBoxList.StateImage.Width := ImageList16x16.Width;
    AComboBoxList.StateImage.Height := ImageList16x16.Height;
    AComboBoxList.ImageList16x16 := ImageList16x16;
    AComboBoxList.OnDrawImageState(AComboBoxList.ConfigInfo);
    AComboBoxList.StateImage.Parent := ParentControl;

    AComboBoxList.Top := ControlOffset;
    AComboBoxList.Left := AComboBoxList.StateImage.Left + AComboBoxList.StateImage.Width + 8;
    AComboBoxList.OnChange := @OnComboBoxChange;
    AComboBoxList.Parent := ParentControl;

    AComboBoxList.ReadCVSpeedButton.Left := AComboBoxList.Left + AComboBoxList.Width + 4;
    AComboBoxList.ReadCVSpeedButton.Top := AComboBoxList.Top;
    AComboBoxList.ReadCVSpeedButton.Height := AComboBoxList.Height;
    AComboBoxList.ReadCVSpeedButton.Width := CV_BUTTON_WIDTH;
    AComboBoxList.ReadCVSpeedButton.Caption := 'Read';
    AComboBoxList.ReadCVSpeedButton.OnClick := OnSpeedButtonReadConfigClickCallback;
    AddSpeedButtonGlyph(AComboBoxList.ReadCVSpeedButton, 55);
    AComboBoxList.ReadCVSpeedButton.Parent := ParentControl;

    AComboBoxList.WriteCVSpeedButton.Left := AComboBoxList.ReadCVSpeedButton.Left + AComboBoxList.ReadCVSpeedButton.Width + 4;
    AComboBoxList.WriteCVSpeedButton.Top := AComboBoxList.Top;
    AComboBoxList.WriteCVSpeedButton.Height := AComboBoxList.Height;
    AComboBoxList.WriteCVSpeedButton.Width := CV_BUTTON_WIDTH;
    AComboBoxList.WriteCVSpeedButton.Caption := 'Write';
    AComboBoxList.WriteCVSpeedButton.OnClick := OnSpeedButtonWriteConfigClickCallback;
    AddSpeedButtonGlyph(AComboBoxList.WriteCVSpeedButton, 12);
    AComboBoxList.WriteCVSpeedButton.Parent := ParentControl;

 {   AComboBoxList.CompareCVSpeedButton.Left := AComboBoxList.WriteCVSpeedButton.Left + AComboBoxList.WriteCVSpeedButton.Width + 4;
    AComboBoxList.CompareCVSpeedButton.Top := AComboBoxList.Top;
    AComboBoxList.CompareCVSpeedButton.Height := AComboBoxList.Height;
    AComboBoxList.CompareCVSpeedButton.Width := CV_BUTTON_WIDTH;
    AComboBoxList.CompareCVSpeedButton.Caption := 'Compare';
    AComboBoxList.CompareCVSpeedButton.OnClick := OnSpeedButtonReadConfigClickCallback;
    AComboBoxList.CompareCVSpeedButton.Parent := ParentControl;       }

    // Update the Control Offsets
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

   // Test for a Group segment
   if LowerCase( Element.NodeName) = 'group' then
   begin
     // If it is a group then run into the group
     Inc(Indent, 8);

     // Group may override the Offset
     UpdateMemOffsetJump(Element, MemOffset);

     // Look for descripive names and descriptions to print
     if ExtractElementItem(Element, 'name', TempStr) then
       AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, True);
     if ExtractElementItem(Element, 'description', TempStr) then
       AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);

     // Look for replications
     if ExtractElementAttribute(Element, 'replication', TempStr) then
       ReplicationCount := StrToInt(TempStr)
     else
       ReplicationCount := 1;
     ExtractElementItem(Element, 'repname', TempStr);   // Is only one repeated name allowed?  Appears to be with the XML tool.

     // Run through the replicated group (if there was no replication then this is set to 1)
     for i := 1 to ReplicationCount do
     begin
       // TempStr contains the repeated name so print it with the iteration number
       if TempStr <> '' then
         AddLabel(ParentControl, TempStr + ' ' + IntToStr(i), ControlOffset, 2, Indent + 8, False);

       // Run through each of the children of the group calling this method to process them
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
       // It is a descriptive block so print it
       if not SupressNameAndDescription then
         AddLabel(ParentControl, Element.TextContent, ControlOffset, 2, Indent, False);
     end else
     if LowerCase(Element.NodeName) = 'int' then
     begin
       // It is an Integer which may have a memory modifier as well as a size
       UpdateMemOffsetJump(Element, MemOffset);
       MemSize := UpdateMemOffsetSize(Element);

       // If it has a map then create a ComboListBox to handle it else use a Spin Edit
       Map_Child := Element.FindNode('map');
       if Map_Child = nil then
         AddSpinEdit(ParentControl, Element, ControlOffset, 4, Indent + 4, MemOffset, MemSize, PrintMemOffset, Element.NodeName)
       else
         AddComboBoxList(ParentControl, Element, ControlOffset, 4, Indent + 4, MemOffset, MemSize, PrintMemOffset, Element.NodeName);

       // Update the Control Offset
       Inc(MemOffset, MemSize);
     end else
     if LowerCase(Element.NodeName) = 'bit' then
     begin
       // It is an Bit which may have a memory modifier as well as a size
       UpdateMemOffsetJump(Element, MemOffset);
       MemSize := UpdateMemOffsetSize(Element);

       // Think a bit MUST have a map, not sure what the alternative would look like
       Map_Child := Element.FindNode('map');
       if Map_Child = nil then
         AddSpinEdit(ParentControl, Element, ControlOffset, 4, Indent + 4, MemOffset, MemSize, PrintMemOffset, Element.NodeName)
       else
         AddComboBoxList(ParentControl, Element, ControlOffset, 4, Indent + 4, MemOffset, MemSize, PrintMemOffset, Element.NodeName);

       // Update the Control Offset
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
         AddComboBoxList(ParentControl, Element, ControlOffset, 4, Indent + 4, MemOffset, MemSize, PrintMemOffset, Element.NodeName);

       // Update the Control Offset
       Inc(MemOffset, MemSize);
     end else
   end;
 end;
end;

procedure TCdiParser.OnSpinEditChange(Sender: TObject);
begin
  (Sender as TOlcbSpinEdit).ConfigInfo.State := ocs_Unknown;
end;

procedure TCdiParser.OnEditChange(Sender: TObject);
begin
  (Sender as TOlcbEdit).ConfigInfo.State := ocs_Unknown;
end;

procedure TCdiParser.OnComboBoxChange(Sender: TObject);
begin
 (Sender as TOlcbComboBox).ConfigInfo.State := ocs_Unknown;
end;

constructor TCdiParser.Create;
begin
  inherited Create;
  ImageList16x16 := nil;
  OnSpeedButtonReadConfigClickCallback := nil;
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

    // Handle the Identification block
    Identification_Root := CDI_Root.FindNode('identification');
    if Assigned(Identification_Root) then
    begin
      ControlOffset := 0;

      // Add a tab to place the Identification information on
      ScrollBox := AddTab(Result, 'Identification');

      // Space on the Top
      AddLabel(ScrollBox, ' ', ControlOffset, 4, 0, False);

      // Handle the manufacturer
      AddLabel(ScrollBox, 'Manufacturer: ', ControlOffset, 2, IDENTIFICATION_INDENT, True);
      Identification_Child := Identification_Root.FindNode('manufacturer');
      if Assigned(Identification_Child) then
        AddLabel(ScrollBox, ItemStr + Identification_Child.TextContent, ControlOffset, 2, IDENTIFICATION_INDENT + 4, False)
      else
        AddLabel(ScrollBox, ' ', ControlOffset, 2, 0, False);

      // Handle the model number
      AddLabel(ScrollBox, 'Model: ', ControlOffset, 2, IDENTIFICATION_INDENT, True);
      Identification_Child := Identification_Root.FindNode('model');
      if Assigned(Identification_Child) then
        AddLabel(ScrollBox, ItemStr + Identification_Child.TextContent, ControlOffset, 2, IDENTIFICATION_INDENT + 4, False)
      else
        AddLabel(ScrollBox, ItemStr, ControlOffset, 2, 0, False);

      // Handle the Hardware Version
      AddLabel(ScrollBox, 'Hardware Version: ', ControlOffset, 2, IDENTIFICATION_INDENT, True);
      Identification_Child := Identification_Root.FindNode('hardwareVersion');
      if Assigned(Identification_Child) then
        AddLabel(ScrollBox, ItemStr + Identification_Child.TextContent, ControlOffset, 2, IDENTIFICATION_INDENT + 4, False)
      else
        AddLabel(ScrollBox, ItemStr, ControlOffset, 2, 0, False);

      // Handle the Software Version
      AddLabel(ScrollBox, 'Software Version: ', ControlOffset, 2, IDENTIFICATION_INDENT, True);
      Identification_Child := Identification_Root.FindNode('softwareVersion');
      if Assigned(Identification_Child) then
        AddLabel(ScrollBox, ItemStr + Identification_Child.TextContent, ControlOffset, 2, IDENTIFICATION_INDENT + 4, False)
      else
        AddLabel(ScrollBox, ItemStr, ControlOffset, 2, 0, False);

      // Handle any map blocks that contain descriptive information
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

    // Handled the Segment blocks
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
            ScrollBox := AddTab(Result, '[Unnamed]');

          // Select it to create the window so the size of the Scrollbox is correct
          // Set it back to a simple tab so it builds faster
          Result.ActivePageIndex := Result.PageCount - 1;
          Result.ActivePageIndex := 0;

          // Add the description of this segment as the first line of the Tab Page
          if ExtractElementItem(Segment_Root, 'description', ItemStr) then
            AddLabel(ScrollBox, ItemStr, ControlOffset, 4, 4, False);

          // Time to build the UI for this segment
          UpdateMemOffsetJump(Segment_Root, MemOffset);      // Segment may override the Offset

          // Run all children of the Segment
          Segment_Child := Segment_Root.FirstChild;
          while Segment_Child <> nil do
          begin
            ProcessElementForUI(ScrollBox, Segment_Child, MemOffset, ControlOffset, 4, True, False);
            Segment_Child := Segment_Child.NextSibling;
          end;

          // Space on the bottom
          AddLabel(ScrollBox, ' ', ControlOffset, 4, 0, False);
        end;
      end;
      Cdi_Child := Cdi_Child.NextSibling;
    end;

    // Allow the controls to be built so Change event are not fired the first time a tab is selected
    Result.ActivePageIndex := Result.PageCount - 1;
    Result.ActivePageIndex := 0;


    // If we anchor it earlier then any control that make the client widow wider "drags" the edits boxes to the right.
          for ControlOffset := 0 to ScrollBox.ControlCount - 1 do
          begin
            if ScrollBox.Controls[ControlOffset] is TOlcbEdit then
            begin
            //  (ScrollBox.Controls[ControlOffset] as TOlcbEdit).Visible := False;
            //  (ScrollBox.Controls[ControlOffset] as TOlcbEdit).Anchors := [akRight, akLeft, akTop];
           //   (ScrollBox.Controls[ControlOffset] as TOlcbEdit).Visible := True;
         //     (ScrollBox.Controls[ControlOffset] as TOlcbEdit).Width := 50;
            end;
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

procedure TConfigInfo.SetState(AValue: TOlcbConfigState);
begin
  if AValue <> FState then
  begin
    FState:=AValue;
    if Assigned(OnChangeState) then
      OnChangeState(Self);
  end;
end;

constructor TConfigInfo.Create(MemOffset, MemSize: DWord;
  ADataType: TOlcbConfigDataType);
begin
  inherited Create;
  FState := ocs_Unknown;
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



