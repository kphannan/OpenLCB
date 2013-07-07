unit form_ethernet_messagelog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynMemo, Forms, Controls, Graphics, Dialogs,
  Buttons, Menus, LCLType, StdCtrls, ActnList, SynEditKeyCmds, SynEdit,
  olcb_utilities, SynEditMarkupHighAll, common_utilities;

type

  { TFormEthernetMessageLog }

  TFormEthernetMessageLog = class(TForm)
    ActionLogPause: TAction;
    ActionListMessageLog: TActionList;
    ActionLogClear: TAction;
    ActionLogCopy: TAction;
    ActionLogCut: TAction;
    ActionLogPaste: TAction;
    ActionLogSelectAll: TAction;
    Label1: TLabel;
    MenuItemClear: TMenuItem;
    MenuItemSeparator1: TMenuItem;
    MenuItemSeparator0: TMenuItem;
    MenuItemPaste: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemSelectAll: TMenuItem;
    MenuItemCut: TMenuItem;
    PopupMenuSynMemo: TPopupMenu;
    SpeedButtonClear1: TSpeedButton;
    SpeedButtonClear: TSpeedButton;
    SynMemo: TSynMemo;
    procedure ActionLogClearExecute(Sender: TObject);
    procedure ActionLogCopyExecute(Sender: TObject);
    procedure ActionLogCutExecute(Sender: TObject);
    procedure ActionLogPasteExecute(Sender: TObject);
    procedure ActionLogPauseExecute(Sender: TObject);
    procedure ActionLogSelectAllExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure SynMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FHelper: TOpenLCBMessageHelper;
    FHideCallback: TFormHideCallback;
    FPaused: Boolean;
    { private declarations }
  protected
    property Helper: TOpenLCBMessageHelper read FHelper write FHelper;
  public
    { public declarations }
    property HideCallback: TFormHideCallback read FHideCallback write FHideCallback;
    property Paused: Boolean read FPaused write FPaused;
  end;

var
  FormEthernetMessageLog: TFormEthernetMessageLog;

implementation

{$R *.lfm}

{ TFormMessageLog }

procedure TFormEthernetMessageLog.SynMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TFormEthernetMessageLog.FormHide(Sender: TObject);
begin
  if Assigned(HideCallback) then
    HideCallback;
end;

procedure TFormEthernetMessageLog.FormCreate(Sender: TObject);
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
  FPaused := False;
end;

procedure TFormEthernetMessageLog.ActionLogClearExecute(Sender: TObject);
begin
  SynMemo.ClearAll;
end;

procedure TFormEthernetMessageLog.ActionLogCopyExecute(Sender: TObject);
begin
  SynMemo.CommandProcessor(TSynEditorCommand(ecCopy), ' ', nil);
end;

procedure TFormEthernetMessageLog.ActionLogCutExecute(Sender: TObject);
begin
  SynMemo.CommandProcessor(TSynEditorCommand(ecCut), ' ', nil);
end;

procedure TFormEthernetMessageLog.ActionLogPasteExecute(Sender: TObject);
begin
  SynMemo.CommandProcessor(TSynEditorCommand(ecPaste), ' ', nil);
end;

procedure TFormEthernetMessageLog.ActionLogPauseExecute(Sender: TObject);
begin
  Paused := not Paused;
end;

procedure TFormEthernetMessageLog.ActionLogSelectAllExecute(Sender: TObject);
begin
  SynMemo.SelectAll;
end;


procedure TFormEthernetMessageLog.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FHelper);
end;

end.

