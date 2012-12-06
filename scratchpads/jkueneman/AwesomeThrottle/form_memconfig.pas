unit form_memconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterHTML,
  SynHighlighterXML, SynHighlighterDiff, SynPluginSyncroEdit, Forms, Controls,
  Graphics, Dialogs, ComCtrls, ExtCtrls, Buttons, StdCtrls, KHexEditor,
  lcltype;

type

  { TFormMemConfig }

  TFormMemConfig = class(TForm)
    BitBtnReadAddressSpaces: TBitBtn;
    BitBtnReadAddressSpacesReadData1: TBitBtn;
    BitBtnReadSNIP: TBitBtn;
    BitBtnReadAddressSpacesSynEditClear: TBitBtn;
    BitBtnReadAddressSpacesReadData: TBitBtn;
    BitBtnReadAddressSpacesSynEditStop: TBitBtn;
    BitBtnReadOptions: TBitBtn;
    CheckBoxUseHexViewer: TCheckBox;
    CheckBoxAddressSpacePresent: TCheckBox;
    CheckBoxForceCommonAddressIntoSpace: TCheckBox;
    CheckGroupAddressSpaceFlags: TCheckGroup;
    CheckGroupMask: TCheckGroup;
    CheckGroupLength: TCheckGroup;
    EditAddressSpaceDataReadBlockSize: TEdit;
    EditAddressSpaceDataReadBytesToRead: TEdit;
    EditAddressSpaceDataReadLoAddress: TEdit;
    EditAddressSpaceDataReadHiAddress: TEdit;
    EditAddressSpaceFlags: TEdit;
    EditAddressSpaceIDRequested: TEdit;
    EditAddressSpaceLoAddress: TEdit;
    EditAddressSpaceHiAddress: TEdit;
    EditAddressSpaceDescription: TEdit;
    EditAddressSpaceIDReturned: TEdit;
    GroupBoxAddressSpace: TGroupBox;
    KHexEditor: TKHexEditor;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    LabelLoAddress: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    LabelHiAddress: TLabel;
    ListViewAddressSpace: TListView;
    PageControl: TPageControl;
    SynEditAddressSpaceData: TSynEdit;
    SynXMLSyn: TSynXMLSyn;
    TabSheetMemData: TTabSheet;
    TabSheetAddressSpace: TTabSheet;
    TabSheetConfigOptions: TTabSheet;
    procedure EditAddressSpaceDataReadBlockSizeExit(Sender: TObject);
    procedure EditAddressSpaceDataReadBlockSizeKeyPress(Sender: TObject; var Key: char);
    procedure EditAddressSpaceDataReadBytesToReadExit(Sender: TObject);
    procedure EditAddressSpaceDataReadBytesToReadKeyPress(Sender: TObject; var Key: char);
    procedure EditAddressSpaceDataReadHiAddressExit(Sender: TObject);
    procedure EditAddressSpaceDataReadHiAddressKeyPress(Sender: TObject; var Key: char);
    procedure EditAddressSpaceDataReadLoAddressExit(Sender: TObject);
    procedure EditAddressSpaceDataReadLoAddressKeyPress(Sender: TObject; var Key: char);
    procedure PageControlChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function IsNumberChar(Key: char): Boolean;
    procedure Prettify;
  end;

var
  FormMemConfig: TFormMemConfig;

implementation

{$ifdef Windows}
uses Windows;
{$endif}
{$IFDEF Darwin}
//uses
//  MacOSAll;
{$ENDIF}

{$R *.lfm}

{ TFormMemConfig }


procedure TFormMemConfig.EditAddressSpaceDataReadLoAddressExit(Sender: TObject);
begin
  EditAddressSpaceDataReadBytesToRead.Text := '0x' + IntToHex( StrToInt(EditAddressSpaceDataReadHiAddress.Text) - StrToInt(EditAddressSpaceDataReadLoAddress.Text) , 8);
  Prettify;
end;

procedure TFormMemConfig.EditAddressSpaceDataReadLoAddressKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) then
  begin
    EditAddressSpaceDataReadLoAddressExit(Sender)
  end else
  if not IsNumberChar(Key) then
    Key := #0;
end;

procedure TFormMemConfig.PageControlChange(Sender: TObject);
begin

end;

function TFormMemConfig.IsNumberChar(Key: char): Boolean;
begin
  Result := ((Key >= '0') and (Key <= '9')) or (Key = 'x') or (Key = '$') or ((Key >= 'A') and (Key <= 'F')) or (Key = #8)
end;

procedure TFormMemConfig.Prettify;
begin
  EditAddressSpaceDataReadBytesToRead.Text := '0x' + IntToHex( StrToInt( EditAddressSpaceDataReadBytesToRead.Text), 8);
  EditAddressSpaceDataReadHiAddress.Text := '0x' + IntToHex( StrToInt( EditAddressSpaceDataReadHiAddress.Text), 8);
  EditAddressSpaceDataReadLoAddress.Text := '0x' + IntToHex( StrToInt( EditAddressSpaceDataReadLoAddress.Text), 8);
  EditAddressSpaceDataReadBlockSize.Text := '0x' + IntToHex( StrToInt( EditAddressSpaceDataReadBlockSize.Text), 2);
end;

procedure TFormMemConfig.EditAddressSpaceDataReadHiAddressExit(Sender: TObject);
begin
  EditAddressSpaceDataReadBytesToRead.Text := '0x' + IntToHex( StrToInt(EditAddressSpaceDataReadHiAddress.Text) - StrToInt(EditAddressSpaceDataReadLoAddress.Text) , 8);
  Prettify
end;

procedure TFormMemConfig.EditAddressSpaceDataReadHiAddressKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) then
  begin
    EditAddressSpaceDataReadHiAddressExit(Sender)
  end else
  if not IsNumberChar(Key) then
    Key := #0;
end;

procedure TFormMemConfig.EditAddressSpaceDataReadBytesToReadExit(Sender: TObject);
begin
  EditAddressSpaceDataReadHiAddress.Text := '0x' + IntToHex( StrToInt(EditAddressSpaceDataReadLoAddress.Text) + StrToInt(EditAddressSpaceDataReadBytesToRead.Text), 8);
  Prettify
end;

procedure TFormMemConfig.EditAddressSpaceDataReadBlockSizeKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) then
    Prettify
  else
  if not IsNumberChar(Key) then
    Key := #0;
end;

procedure TFormMemConfig.EditAddressSpaceDataReadBlockSizeExit(Sender: TObject);
begin
  Prettify
end;

procedure TFormMemConfig.EditAddressSpaceDataReadBytesToReadKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) then
  begin
    EditAddressSpaceDataReadBytesToReadExit(Sender)
  end else
  if not IsNumberChar(Key) then
    Key := #0;
end;


end.

