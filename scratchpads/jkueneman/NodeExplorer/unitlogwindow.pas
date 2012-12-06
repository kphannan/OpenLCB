unit unitlogwindow;

// ******************************************************************************
//
// * Copyright:
//     (c) Mustangpeak Software 2012.
//
//     The contents of this file are subject to the GNU GPL v3 licence/ you maynot use
//     this file except in compliance with the License. You may obtain a copy of the
//     License at http://www.gnu.org/licenses/gpl.html
//
// * Revision History:
//     2012-08-05:   Created
//
// * Description:

//
// *****************************************************************************

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynMemo, SynHighlighterXML, Forms, Controls,
  Graphics, Dialogs, StdCtrls, SynEditMarkupSpecialLine, LCLType,
  Menus, SynEditKeyCmds;

type

  { TFormLog }

  TFormLog = class(TForm)
    ButtonClear: TButton;
    CheckBoxShowGutter: TCheckBox;
    MenuItemSynEditSeparator2: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItemSynEditSelectAll: TMenuItem;
    MenuItemSynEditCut: TMenuItem;
    MenuItemSynEditPaste: TMenuItem;
    MenuItemSynEditCopy: TMenuItem;
    MenuItemSynEditSeparator1: TMenuItem;
    PopupMenuSynMemo: TPopupMenu;
    SynMemo: TSynMemo;
    SynXMLSyn: TSynXMLSyn;
    procedure SynMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SynMemoSpecialLineColors(Sender: TObject; Line: integer; var Special: boolean; var FG, BG: TColor);
  private
    { private declarations }
  public
    { public declarations }
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

procedure TFormLog.SynMemoSpecialLineColors(Sender: TObject; Line: integer; var Special: boolean; var FG, BG: TColor);
begin
  if (Pos(':X', SynMemo.Lines[Line-1]) > 0) or (Pos(':S', SynMemo.Lines[Line-1]) > 0) then
  begin
    Special := True;
    FG := clGreen;
  //  BG := clLtGray;
  end;
end;

end.

