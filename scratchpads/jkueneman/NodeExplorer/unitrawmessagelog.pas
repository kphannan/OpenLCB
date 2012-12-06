unit unitrawmessagelog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynHighlighterXML, SynMemo, Forms, Controls,
  Graphics, Dialogs, StdCtrls, Menus, SynEditKeyCmds, LCLType;

type

  TActionCallback = procedure of object;

  { TFormRawMessageLog }

  TFormRawMessageLog = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FHidingCallback: TActionCallback;
  published
    ButtonClear: TButton;
    CheckBoxShowGutter: TCheckBox;
    MenuItem2: TMenuItem;
    MenuItemSynEditCopy: TMenuItem;
    MenuItemSynEditCut: TMenuItem;
    MenuItemSynEditPaste: TMenuItem;
    MenuItemSynEditSelectAll: TMenuItem;
    MenuItemSynEditSeparator1: TMenuItem;
    MenuItemSynEditSeparator2: TMenuItem;
    PopupMenuSynMemo: TPopupMenu;
    SynMemo: TSynMemo;
    SynXMLSyn: TSynXMLSyn;
    procedure FormHide(Sender: TObject);
    procedure SynMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    property HidingCallback: TActionCallback read FHidingCallback write FHidingCallback;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormRawMessageLog: TFormRawMessageLog;

implementation

{$R *.lfm}

{ TFormRawMessageLog }

procedure TFormRawMessageLog.SynMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // Windows/Linux/OSX already handled by SynEdit using the Windows Shortcuts
  {$IFDEF darwin}
  if (Shift = [ssMeta]) then
  begin
    case Key of
    VK_C: SynMemo.CommandProcessor(TSynEditorCommand(ecCopy), ' ', nil);
    VK_V: SynMemo.CommandProcessor(TSynEditorCommand(ecPaste), ' ', nil);
    VK_X: SynMemo.CommandProcessor(TSynEditorCommand(ecCut), ' ', nil);
    end;
  end;
  {$ENDIF}
end;

procedure TFormRawMessageLog.FormCreate(Sender: TObject);
begin
  FHidingCallback := nil;
end;

procedure TFormRawMessageLog.FormHide(Sender: TObject);
begin
  if Assigned(HidingCallback) then
    HidingCallback
end;

end.
