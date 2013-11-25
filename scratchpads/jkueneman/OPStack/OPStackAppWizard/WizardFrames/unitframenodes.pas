unit unitframenodes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Spin, ComCtrls,
  unitframebase;

type

  { TFrameNodes }

  TFrameNodes = class(TFrameBase)
    CheckBoxNodeConsumers: TCheckBox;
    CheckBoxNodeDynamicProducer: TCheckBox;
    CheckBoxNodeDynamicConsumers: TCheckBox;
    CheckBoxNodeProducers: TCheckBox;
    GroupBoxNode: TGroupBox;
    ListViewConsumerEvents: TListView;
    ListViewProducerEvents: TListView;
    PageControlEvents: TPageControl;
    SpinEditNodeProducers: TSpinEdit;
    SpinEditNodeConsumers: TSpinEdit;
    SpinEditNodeDynamicProducers: TSpinEdit;
    SpinEditNodeDyamicConsumers: TSpinEdit;
    TabSheetConsumerEvents: TTabSheet;
    TabSheetProducerEvents: TTabSheet;
    procedure CheckBoxNodeConsumersChange(Sender: TObject);
    procedure CheckBoxNodeDynamicConsumersChange(Sender: TObject);
    procedure CheckBoxNodeDynamicProducerChange(Sender: TObject);
    procedure CheckBoxNodeProducersChange(Sender: TObject);
    procedure SpinEditNodeConsumersChange(Sender: TObject);
    procedure SpinEditNodeProducersChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure UpdateUI; override;
  end;

implementation

{$R *.lfm}

{ TFrameNodes }

procedure TFrameNodes.CheckBoxNodeProducersChange(Sender: TObject);
begin
  UpdateUI;
end;

procedure TFrameNodes.SpinEditNodeConsumersChange(Sender: TObject);
var
  ListItem: TListItem;
begin
  PageControlEvents.PageIndex := 1;
  if ListViewConsumerEvents.Items.Count < SpinEditNodeConsumers.Value then
  begin
    while ListViewConsumerEvents.Items.Count < SpinEditNodeConsumers.Value do
    begin
      ListItem := ListViewConsumerEvents.Items.Add;
      ListItem.Caption := '00 00 00 00 00 00 00 00';
    end;
  end else
  if ListViewConsumerEvents.Items.Count > SpinEditNodeConsumers.Value then
  begin
    while ListViewConsumerEvents.Items.Count > SpinEditNodeConsumers.Value do
    begin
      ListViewConsumerEvents.Items.Delete(ListViewConsumerEvents.Items.Count-1);
    end;
  end;
end;

procedure TFrameNodes.SpinEditNodeProducersChange(Sender: TObject);
var
  ListItem: TListItem;
begin
  PageControlEvents.PageIndex := 0;
  if ListViewProducerEvents.Items.Count < SpinEditNodeProducers.Value then
  begin
    while ListViewProducerEvents.Items.Count < SpinEditNodeProducers.Value do
    begin
      ListItem := ListViewProducerEvents.Items.Add;
      ListItem.Caption := '00 00 00 00 00 00 00 00';
    end;
  end else
  if ListViewProducerEvents.Items.Count > SpinEditNodeProducers.Value then
  begin
    while ListViewProducerEvents.Items.Count > SpinEditNodeProducers.Value do
    begin
      ListViewProducerEvents.Items.Delete(ListViewProducerEvents.Items.Count-1);
    end;
  end;

end;

procedure TFrameNodes.CheckBoxNodeConsumersChange(Sender: TObject);
begin
  UpdateUI;
end;

procedure TFrameNodes.CheckBoxNodeDynamicConsumersChange(Sender: TObject);
begin
  UpdateUI;
end;

procedure TFrameNodes.CheckBoxNodeDynamicProducerChange(Sender: TObject);
begin
  UpdateUI;
end;

procedure TFrameNodes.UpdateUI;
begin
  inherited UpdateUI;
  SpinEditNodeProducers.Enabled := CheckBoxNodeProducers.Checked;
  SpinEditNodeConsumers.Enabled := CheckBoxNodeConsumers.Checked;
  SpinEditNodeDynamicProducers.Enabled := CheckBoxNodeDynamicProducer.Checked;
  SpinEditNodeDyamicConsumers.Enabled := CheckBoxNodeDynamicConsumers.Checked;
end;

end.

