unit cabIDchooser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, ActnList, Spin, Buttons, math_float16,
  olcb_transport_layer, olcb_app_common_settings,
  olcb_utilities, olcb_defines,
  laz2_DOM, laz2_XMLRead, laz2_XMLWrite,
  form_train_config_editor, com_port_hub, ethernet_hub,
  template_userstatemachine, form_throttle;

type

  { TFormCabChooser }

  TFormCabChooser = class(TForm)
    ButtonCancel: TButton;
    ButtonOk: TButton;
    Label1: TLabel;
    ListViewCabID: TListView;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
  private
    FCabID: Word;
    FThrottles: TThrottleList;
    { private declarations }
  public
    { public declarations }
    property Throttles: TThrottleList read FThrottles write FThrottles;
    property CabID: Word read FCabID;
  end;

var
  FormCabChooser: TFormCabChooser;

implementation

{$R *.lfm}

uses
  NMRAnetNceBridgeDefines;

{ TFormCabChooser }

procedure TFormCabChooser.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  i: Integer;
  Item: TListItem;
  TooManyChecked: Boolean;
begin
  FCabID := 0;
  if ModalResult = mrOK then
  begin
    TooManyChecked := False;
    Item := nil;
    for i := 0 to ListViewCabID.Items.Count - 1 do
    begin
      if ListViewCabID.Items[i].Checked then
      begin
        if Item = nil then
          Item := ListViewCabID.Items[i]
        else begin
          TooManyChecked := True;;
          Item := nil;
          Break;
        end;
      end;
    end;
    if TooManyChecked then
    begin
      ShowMessage('Only select one Cab ID');
      CanClose := False;
    end else
    if Item = nil then
    begin
      ShowMessage('Select a Cab ID or Cancel');
      CanClose := False;
    end else
    begin
      FCabID := Word( Item.Data);
    end;
  end;
end;

procedure TFormCabChooser.FormShow(Sender: TObject);

  function FindCabID(CabID: Word): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 0 to Throttles.Count - 1 do
    begin
      if Throttles[i].CabID = CabID then
      begin
        Result := True;
        Break;
      end;
    end;
  end;

var
  i: Integer;
  Item: TListItem;
begin
  for i := ID_MIN_DEVICE to ID_MAX_DEVICE do
  begin
    if not FindCabID(i) then
    begin
      Item := ListViewCabID.Items.Add;
      Item.Caption := 'Cab ID ' + IntToStr(i);
      Item.Data := Pointer( i);
    end;
  end;
end;

end.

