unit form_throttle_multiple_trains;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormMulitipleTrains }

  TFormMulitipleTrains = class(TForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    ButtonCreateNew: TButton;
    Label1: TLabel;
    Label2: TLabel;
    ListBoxTrains: TListBox;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure LoadAliasList(AliasList: TList);
  end;


implementation

{$R *.lfm}

{ TFormMulitipleTrains }

procedure TFormMulitipleTrains.FormCreate(Sender: TObject);
begin

end;

procedure TFormMulitipleTrains.LoadAliasList(AliasList: TList);
var
  i: Integer;
begin
  for i := 0 to AliasList.Count - 1 do
    ListBoxTrains.Items.Add( '0x' + IntToHex( QWord( AliasList[i]), 4));
  ListBoxTrains.ItemIndex := 0;
end;

procedure TFormMulitipleTrains.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  //CanClose := ListBoxTrains.ItemIndex > -1;
end;

procedure TFormMulitipleTrains.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

end.
