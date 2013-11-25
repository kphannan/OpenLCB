unit unitframevirtualnodes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Spin, ComCtrls,
  unitframebase;

type

  { TFrameVirtualNodes }

  TFrameVirtualNodes = class(TFrameBase)
    CheckBoxNodeConsumers: TCheckBox;
    CheckBoxNodeDynamicConsumers: TCheckBox;
    CheckBoxNodeDynamicProducer: TCheckBox;
    CheckBoxNodeProducers: TCheckBox;
    CheckBoxVirtualNodes: TCheckBox;
    GroupBoxVirtualNodes: TGroupBox;
    LabelNumberVirtualNodes: TLabel;
    ListViewConsumerEvents: TListView;
    ListViewProducerEvents: TListView;
    PageControlEvents: TPageControl;
    SpinEditNodeConsumers: TSpinEdit;
    SpinEditNodeDyamicConsumers: TSpinEdit;
    SpinEditNodeDynamicProducers: TSpinEdit;
    SpinEditNodeProducers: TSpinEdit;
    SpinEditVirtualNodes: TSpinEdit;
    TabSheetProducerEvents: TTabSheet;
    TabSheetConsumerEvents: TTabSheet;
    procedure CheckBoxNodeConsumersChange(Sender: TObject);
    procedure CheckBoxNodeDynamicConsumersChange(Sender: TObject);
    procedure CheckBoxNodeDynamicProducerChange(Sender: TObject);
    procedure CheckBoxNodeProducersChange(Sender: TObject);
    procedure CheckBoxVirtualNodesChange(Sender: TObject);
    procedure PageControlEventsChange(Sender: TObject);
    procedure SpinEditNodeConsumersChange(Sender: TObject);
    procedure SpinEditNodeProducersChange(Sender: TObject);
    procedure TabControlEventsChanging(Sender: TObject; var AllowChange: Boolean
      );
  private
    { private declarations }
  public
    { public declarations }
    procedure UpdateUI; override;
  end;

implementation

{$R *.lfm}

{ TFrameVirtualNodes }

procedure TFrameVirtualNodes.CheckBoxVirtualNodesChange(Sender: TObject);
begin
  UpdateUI
end;

procedure TFrameVirtualNodes.PageControlEventsChange(Sender: TObject);
begin

end;

procedure TFrameVirtualNodes.SpinEditNodeConsumersChange(Sender: TObject);
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

procedure TFrameVirtualNodes.SpinEditNodeProducersChange(Sender: TObject);
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

procedure TFrameVirtualNodes.TabControlEventsChanging(Sender: TObject; var AllowChange: Boolean);
begin

end;

procedure TFrameVirtualNodes.CheckBoxNodeDynamicConsumersChange(Sender: TObject);
begin
  UpdateUI
end;

procedure TFrameVirtualNodes.CheckBoxNodeDynamicProducerChange(Sender: TObject);
begin
  UpdateUI
end;

procedure TFrameVirtualNodes.CheckBoxNodeConsumersChange(Sender: TObject);
begin
  UpdateUI
end;

procedure TFrameVirtualNodes.CheckBoxNodeProducersChange(Sender: TObject);
begin
  UpdateUI
end;

procedure TFrameVirtualNodes.UpdateUI;
begin
  inherited UpdateUI;
  LabelNumberVirtualNodes.Enabled := CheckBoxVirtualNodes.Checked;
  SpinEditVirtualNodes.Enabled := CheckBoxVirtualNodes.Checked;
  SpinEditNodeConsumers.Enabled := CheckBoxVirtualNodes.Checked and CheckBoxNodeConsumers.Checked;
  SpinEditNodeDyamicConsumers.Enabled := CheckBoxVirtualNodes.Checked and CheckBoxNodeDynamicConsumers.Checked;;
  SpinEditNodeDynamicProducers.Enabled := CheckBoxVirtualNodes.Checked and CheckBoxNodeDynamicProducer.Checked;;
  SpinEditNodeProducers.Enabled := CheckBoxVirtualNodes.Checked and CheckBoxNodeProducers.Checked;;
  CheckBoxNodeProducers.Enabled := CheckBoxVirtualNodes.Checked;
  CheckBoxNodeConsumers.Enabled := CheckBoxVirtualNodes.Checked;
  CheckBoxNodeDynamicConsumers.Enabled := CheckBoxVirtualNodes.Checked;
  CheckBoxNodeDynamicProducer.Enabled := CheckBoxVirtualNodes.Checked; ;
end;

end.

