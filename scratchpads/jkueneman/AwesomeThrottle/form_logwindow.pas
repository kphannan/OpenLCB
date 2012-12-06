unit form_logwindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynMemo, Forms, Controls, Graphics, Dialogs,
  Buttons, Menus, LCLType, Spin, StdCtrls, SynEditKeyCmds, SynEdit,
  olcb_utilities, olcb_defines, SynEditMarkupHighAll;

type

  { TFormLog }

  TFormLog = class(TForm)
    BitBtn1: TBitBtn;
    BitBtnClear: TBitBtn;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    MenuItemSeparator1: TMenuItem;
    MenuItemSeparator0: TMenuItem;
    MenuItemPaste: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemSelectAll: TMenuItem;
    MenuItemCut: TMenuItem;
    PopupMenuSynMemo: TPopupMenu;
    SpinEditFontSize: TSpinEdit;
    SynMemo: TSynMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinEditFontSizeChange(Sender: TObject);
    procedure SynMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FHelper: TOpenLCBMessageHelper;
    FHideCallback: TFormHideCallback;
    { private declarations }
  protected
    property Helper: TOpenLCBMessageHelper read FHelper write FHelper;
  public
    { public declarations }
    property HideCallback: TFormHideCallback read FHideCallback write FHideCallback;
  end;

var
  FormLog: TFormLog;

implementation

{$R *.lfm}

{ TFormLog }

procedure TFormLog.SynMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TFormLog.FormHide(Sender: TObject);
begin
  if Assigned(HideCallback) then
    HideCallback;
end;

procedure TFormLog.FormShow(Sender: TObject);
begin
  SpinEditFontSize.Text := IntToStr( SynMemo.Font.Height);
end;

procedure TFormLog.SpinEditFontSizeChange(Sender: TObject);
begin
  SynMemo.Font.Height := StrToInt(SpinEditFontSize.Text);
end;

procedure TFormLog.FormCreate(Sender: TObject);
var
  Markup: TSynEditMarkupHighlightAllCaret;
begin
  Helper := TOpenLCBMessageHelper.Create;
  Markup := SynMemo.MarkupByClass[TSynEditMarkupHighlightAllCaret] as TSynEditMarkupHighlightAllCaret;
  Markup.MarkupInfo.FrameColor := clSkyBlue;
  Markup.MarkupInfo.Background := clSkyBlue;
  Markup.WaitTime := 500;
  Markup.Trim := True;
  Markup.FullWord := False;
  Markup.IgnoreKeywords := False;
end;


procedure TFormLog.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FHelper);
end;

end.
