unit unit_cdi_parser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Spin, laz2_DOM, laz2_XMLRead;


procedure Build_CDI_Interface(ParentControl: TPanel; CDI: TXMLDocument);
procedure Clear_CDI_Interface(ParentControl: TPanel);

implementation

//******************************************************************************
//  function ExtractElementItem
//  parameters - Element: The node that is the root of the element
//               Item:    The string that defines the subElement ("name", "description", ect)
//               AttibuteStr:     [OUT] The string of the Item
//  results    - Returns True if the Item existed
//
//******************************************************************************
function ExtractElementItem(Element: TDOMNode; Item: string; var ItemStr: string): Boolean;
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

//******************************************************************************
//  function ExtractElementAttribute
//  parameters - Element: The node to test for Attributes
//               AttributeName:   The string that defines the Attribute ("origin", "space", "size", "min", "max", etc)
//               AttibuteStr:     [OUT] The string of the Attribute
//  results    - Returns True if the Attribute existed
//
//******************************************************************************
function ExtractElementAttribute(Element: TDOMNode; AttributeName: string; var AttributeStr: string): Boolean;
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

//******************************************************************************
//  function IsMemorySpace
//  parameters - SegmentNode: The node that is the root of the segment element
//               MemorySpace: The OLCB defined byte for the memory space ($FF=CDI, $FE=All Mem, $FD=Config Mem, etc)
//  results    - True if the segement represents the passed Memory Space
//
//******************************************************************************
function IsMemorySpace(Segment: TDOMNode; MemorySpace: Byte): Boolean;
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

//******************************************************************************
//******************************************************************************
procedure UpdateMemOffsetPre(Element: TDOMNode; var MemOffset: DWord);
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

//******************************************************************************
//******************************************************************************
procedure UpdateMemOffsetPost(Element: TDOMNode; var MemOffset: DWord);
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
        MemOffset := MemOffset + StrToInt64( TempStr);
        OffsetModified := True;
      end
    end;
  end;
  if not OffsetModified then
  begin
    if LowerCase( Element.NodeName) = 'int' then
      MemOffset := MemOffset + 1
    else
    if LowerCase( Element.NodeName) = 'eventid' then
      MemOffset := MemOffset + 8
    else
  end;
end;


//******************************************************************************
//  function AddTab
//  parameters - PageControl: PageControl to add the sheet
//               ACaption:  The string to use as the Tab Caption
//  results    - New TabSheet
//
//******************************************************************************
function AddTab(PageControl: TPageControl; ACaption: string): TScrollBox;
var
  Tab: TTabSheet;
begin
  Tab := PageControl.AddTabSheet;
  Tab.Caption := ACaption;
  Result := TScrollBox.Create(Tab);
  Result.Align := alClient;
  Result.BorderSpacing.Around := 8;
  Result.Parent := Tab;
end;

//******************************************************************************
//  function AddLabel
//  parameters - ScrollBox: Where to write the label
//               ACaption:  The string to write to the TabSheet
//  results    - None
//
//******************************************************************************
procedure AddLabel(ParentControl: TScrollBox; ACaption: string; var ControlOffset: Integer; ControlMargin, Indent: Integer);
var
  ALabel: TLabel;
begin
  ALabel := TLabel.Create(ParentControl);
  ALabel.Caption := ACaption;
  ALabel.Top := ControlOffset;
  ALabel.Left := Indent;
  ALabel.Parent := ParentControl;
  ControlOffset := ControlOffset + ALabel.Height + ControlMargin;
end;

//******************************************************************************
// The Element is the "int" element
//******************************************************************************
procedure AddSpinEdit(ParentControl: TScrollBox; Element: TDOMNode; var ControlOffset: Integer; ControlMargin, Indent: Integer; MemOffset: DWord; PrintMemOffset: Boolean);
var
  ASpinEdit: TSpinEdit;
  TempStr: string;
begin
  TempStr := '';
  if PrintMemOffset then
    AddLabel(ParentControl, IntToStr(MemOffset), ControlOffset, 2, Indent);
  ASpinEdit := TSpinEdit.Create(ParentControl);
  if ExtractElementAttribute(Element, 'min', TempStr) then
    ASpinEdit.MinValue := StrToInt(TempStr);
  if ExtractElementAttribute(Element, 'max', TempStr) then
    ASpinEdit.MaxValue := StrToInt(TempStr);
  if ExtractElementAttribute(Element, 'default', TempStr) then
    ASpinEdit.Value := StrToInt(TempStr);
  if ExtractElementItem(Element, 'name', TempStr) then
    AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent);
  if ExtractElementItem(Element, 'description', TempStr) then
    AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent);
  Inc(Indent, 8);
  ASpinEdit.Top := ControlOffset;
  ASpinEdit.Left := Indent;
  ASpinEdit.Width := 120;
  ASpinEdit.Parent := ParentControl;
  ASpinEdit.Tag := MemOffset;
  ControlOffset := ControlOffset + ASpinEdit.Height + ControlMargin;
end;

//******************************************************************************
//  function AddEdit
//  parameters - ScrollBox: Where to write the label
//               ACaption:  The string to write to the Edit
//  results    - None
//
//******************************************************************************
procedure AddEdit(ParentControl: TScrollBox; Element: TDOMNode; var ControlOffset: Integer; ControlMargin, Indent: Integer; MemOffset: DWord; PrintMemOffset: Boolean);
var
  AnEdit: TEdit;
  TempStr: string;
begin
  TempStr := '';
  if PrintMemOffset then
    AddLabel(ParentControl, IntToStr(MemOffset), ControlOffset, 2, Indent);
  AnEdit := TEdit.Create(ParentControl);
  if ExtractElementItem(Element, 'name', TempStr) then
    AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent);
  if ExtractElementItem(Element, 'description', TempStr) then
    AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent);
  AnEdit.Top := ControlOffset;
  AnEdit.Left := Indent;
  AnEdit.Width := 250;
  AnEdit.Parent := ParentControl;
  AnEdit.Tag := MemOffset;
  ControlOffset := ControlOffset + AnEdit.Height + ControlMargin;
end;

//******************************************************************************
// The Element is the "int" element
//******************************************************************************
procedure AddComboBoxList(ParentControl: TScrollBox; Element: TDOMNode; var ControlOffset: Integer; ControlMargin, Indent: Integer; MemOffset: DWord; PrintMemOffset: Boolean);
var
  AComboBoxList: TComboBox;
  TempStr: string;
  MapNode, ChildNode: TDOMNode;
  DoIndent: Boolean;
begin
  TempStr := '';
  if PrintMemOffset then
    AddLabel(ParentControl, IntToStr(MemOffset), ControlOffset, 2, Indent);
  MapNode := Element.FindNode('map');
  if MapNode <> nil then
  begin
    AComboBoxList := TComboBox.Create(ParentControl);
    if ExtractElementItem(Element, 'name', TempStr) then
      AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent);
    if ExtractElementItem(Element, 'description', TempStr) then
      AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent);
    Inc(Indent, 8);

    // The map can have a name and description too
    DoIndent := False;
    if ExtractElementItem(MapNode, 'name', TempStr) then
    begin
      AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent);
      DoIndent := True;
    end;
    if ExtractElementItem(MapNode, 'description', TempStr) then
    begin
      AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent);
      DoIndent := True
    end;
    if DoIndent then
      Inc(Indent, 8);

    AComboBoxList.Top := ControlOffset;
    AComboBoxList.Left := Indent;
    AComboBoxList.Width := 250;
    AComboBoxList.Style := csDropDownList;
    ChildNode := MapNode.FirstChild;
    while ChildNode <> nil do
    begin
      if LowerCase( ChildNode.NodeName) = 'relation' then
      begin
        if ExtractElementItem(ChildNode, 'value', TempStr) then
          AComboBoxList.Items.Add(TempStr);
      end;
      ChildNode := ChildNode.NextSibling;
    end;
    AComboBoxList.ItemIndex := 0;
    AComboBoxList.Parent := ParentControl;
    AComboBoxList.Tag := MemOffset;
    ControlOffset := ControlOffset + AComboBoxList.Height + ControlMargin;
  end;
end;

//******************************************************************************
//******************************************************************************
procedure ProcessElementForUI(ParentControl: TScrollBox; Element: TDOMNode; var MemOffset: DWord; var ControlOffset: Integer; Indent: Integer; SupressNameAndDescription: Boolean; PrintMemOffset: Boolean);
var
  Group_Child, Map_Child: TDOMNode;
  TempStr: string;
  ReplicationCount, i: Integer;
begin
 if Element <> nil then
 begin
   TempStr := '';
   // If it is a group then run into the group
   if LowerCase( Element.NodeName) = 'group' then
   begin
     Inc(Indent, 8);
     UpdateMemOffsetPre(Element, MemOffset);      // Group may override the Offset
     if ExtractElementItem(Element, 'name', TempStr) then
       AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent);
     if ExtractElementItem(Element, 'description', TempStr) then
       AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent);
     if ExtractElementAttribute(Element, 'replication', TempStr) then
       ReplicationCount := StrToInt(TempStr)
     else
       ReplicationCount := 1;
     ExtractElementItem(Element, 'repname', TempStr);
     for i := 1 to ReplicationCount do
     begin
       if TempStr <> '' then
         AddLabel(ParentControl, TempStr + ' ' + IntToStr(i), ControlOffset, 2, Indent);
       Group_Child := Element.FirstChild;
       while Group_Child <> nil do
       begin
         ProcessElementForUI(ParentControl, Group_Child, MemOffset, ControlOffset, Indent, True, PrintMemOffset);
         Group_Child := Group_Child.NextSibling;
       end;
     end;
   end else
   begin
     // It is not a group
     if (LowerCase(Element.NodeName) = 'name') or (LowerCase(Element.NodeName) = 'description') then
     begin
       if not SupressNameAndDescription then
         AddLabel(ParentControl, Element.TextContent, ControlOffset, 2, Indent);
     end else
     if LowerCase(Element.NodeName) = 'int' then
     begin
       UpdateMemOffsetPre(Element, MemOffset);
       Map_Child := Element.FindNode('map');
       if Map_Child = nil then
         AddSpinEdit(ParentControl, Element, ControlOffset, 4, Indent + 4, MemOffset, PrintMemOffset)
       else
         AddComboBoxList(ParentControl, Element, ControlOffset, 4, Indent + 4, MemOffset, PrintMemOffset);
       UpdateMemOffsetPost(Element, MemOffset);
     end else
     if LowerCase(Element.NodeName) = 'bit' then
     begin
       UpdateMemOffsetPre(Element, MemOffset);
       Map_Child := Element.FindNode('map');       // Think a bit MUST have a map
       if Map_Child <> nil then
         AddComboBoxList(ParentControl, Element, ControlOffset, 4, Indent + 4, MemOffset, PrintMemOffset);
       UpdateMemOffsetPost(Element, MemOffset);
     end else
     if (LowerCase(Element.NodeName) = 'string') or (LowerCase(Element.NodeName) = 'eventid') then
     begin
       UpdateMemOffsetPre(Element, MemOffset);
       Map_Child := Element.FindNode('map');
       if Map_Child = nil then
         AddEdit(ParentControl, Element, ControlOffset, 4, Indent + 4, MemOffset, PrintMemOffset)
       else
         AddComboBoxList(ParentControl, Element, ControlOffset, 4, Indent + 4, MemOffset, PrintMemOffset);
       UpdateMemOffsetPost(Element, MemOffset);
     end else
   end;
 end;
end;


procedure Clear_CDI_Interface(ParentControl: TPanel);
var
  i: Integer;
begin
  for i := 0 to ParentControl.ControlCount - 1 do
    ParentControl.Controls[i].Free;
  ParentControl.Caption := 'Loading CDI File...';
end;

//******************************************************************************
//******************************************************************************
procedure Build_CDI_Interface(ParentControl: TPanel; CDI: TXMLDocument);
var
  PageControl: TPageControl;
  CDI_Root, Cdi_Child, Identification_Root, Identifiction_Child, Segment_Root, Segment_Child: TDOMNode;
  ScrollBox: TScrollBox;
  MemOffset: DWord;
  ControlOffset: Integer;
  ItemStr: string;
 begin
  ItemStr := '';
  Clear_CDI_Interface(ParentControl);
    ParentControl.Caption := '';
  PageControl := TPageControl.Create(ParentControl);
  PageControl.Align := alClient;
  PageControl.Parent := ParentControl;
  ErrorCode := 0;
  CDI_Root := CDI.FindNode('cdi');
  if Assigned(CDI_Root) then
  begin
    Identification_Root := CDI_Root.FindNode('identification');
    if Assigned(Identification_Root) then
    begin
      ControlOffset := 0;
      ScrollBox := AddTab(PageControl, 'Identification');
      Identifiction_Child := Identification_Root.FirstChild;
      while Assigned(Identifiction_Child) do
      begin
        AddLabel(ScrollBox, Identifiction_Child.NodeName + ': ' + Identifiction_Child.TextContent, ControlOffset, 2, 0);
        Identifiction_Child := Identifiction_Child.NextSibling;
      end;
    end;

    Cdi_Child := CDI_Root.FirstChild;
    while Assigned(Cdi_Child) do
    begin
      if LowerCase(Cdi_Child.NodeName) = 'segment' then
      begin
        Segment_Root := Cdi_Child;
        // First Find the Config Memory Segment
        if IsMemorySpace(Segment_Root, 253) then
        begin
          // Add a new Tabsheet for this Segment using it Name Element as the tab title
          if ExtractElementItem(Segment_Root, 'name', ItemStr) then
            ScrollBox := AddTab(PageControl, ItemStr)
          else
            ScrollBox := AddTab(PageControl, '[Unknown]');
          // Add the description of this segment as the first line of the Tab Page
          ControlOffset := 0;
          MemOffset := 0;
          if ExtractElementItem(Segment_Root, 'description', ItemStr) then
            AddLabel(ScrollBox, ItemStr, ControlOffset, 4, 0);
          // Time to build the UI for this segment
          UpdateMemOffsetPre(Segment_Root, MemOffset);      // Segment may override the Offset
          Segment_Child := Segment_Root.FirstChild;
          while Segment_Child <> nil do
          begin
            ProcessElementForUI(ScrollBox, Segment_Child, MemOffset, ControlOffset, 4, True, False);
            Segment_Child := Segment_Child.NextSibling;
          end;
        end;
      end;
      Cdi_Child := Cdi_Child.NextSibling;
    end;
  end else
    ErrorCode := 1;   // No CDI Element
end;

end.

