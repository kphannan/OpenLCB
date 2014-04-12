unit form_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Buttons, Spin, olcb_app_common_settings,
  synaser, common_utilities;

type

  { TFormSettings }

  TFormSettings = class(TForm)
    BitBtnFTDIDefaults: TBitBtn;
    BitBtnRescanPorts: TBitBtn;
    CheckBoxAutoLoadFDI: TCheckBox;
    CheckBoxAutoConnect: TCheckBox;
    CheckBoxAutoScanAtStart: TCheckBox;
    ComboBoxStopBits: TComboBox;
    ComboBoxParity: TComboBox;
    ComboBoxBaud: TComboBox;
    ComboBoxComPort: TComboBox;
    ComboBoxDataBits: TComboBox;
    ComboBoxFlowControl: TComboBox;
    EditEthernetLocalIP: TEdit;
    EditAliasID: TEdit;
    EditNodeID: TEdit;
    GroupBox1: TGroupBox;
    ImageListSettings: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    LabelComPort: TLabel;
    LabelBaud: TLabel;
    LabelDataBits: TLabel;
    LabelStopBits: TLabel;
    LabelParity: TLabel;
    LabelFlowControl: TLabel;
    ListViewSelectBar: TListView;
    PanelComPort: TPanel;
    PanelThrottle: TPanel;
    PanelGeneral: TPanel;
    PanelEthernet: TPanel;
    SpinEditEthernetLocalPort: TSpinEdit;
    SpinEditSendPacketDelay: TSpinEdit;
    procedure BitBtnFTDIDefaultsClick(Sender: TObject);
    procedure BitBtnRescanPortsClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListViewSelectBarSelectItem(Sender: TObject; Item: TListItem; Selected: boolean);
  private
    FSettingsFilePath: WideString;
    { private declarations }
    procedure ScanComPorts;
  protected
    procedure StoreSettings;
  public
    { public declarations }
    property SettingsFilePath: WideString read FSettingsFilePath write FSettingsFilePath;
  end;

var
  FormSettings: TFormSettings;

implementation

{$R *.lfm}

{ TFormSettings }

procedure TFormSettings.ListViewSelectBarSelectItem(Sender: TObject;
  Item: TListItem; Selected: boolean);
begin
  if Selected then
  begin
    if Item.Caption = 'Com Port' then
    begin
      PanelThrottle.Visible := False;
      PanelGeneral.Visible := False;
      PanelEthernet.Visible := False;
      PanelComPort.Visible := True;
    end
    else
    if Item.Caption = 'Throttle' then
    begin
      PanelGeneral.Visible := False;
      PanelComPort.Visible := False;
      PanelEthernet.Visible := False;
      PanelThrottle.Visible := True;
    end
    else
    if Item.Caption = 'General' then
    begin
      PanelThrottle.Visible := False;
      PanelComPort.Visible := False;
      PanelEthernet.Visible := False;
      PanelGeneral.Visible := True;
    end else
    if Item.Caption = 'Ethernet' then
    begin
      PanelThrottle.Visible := False;
      PanelGeneral.Visible := False;
      PanelComPort.Visible := False;
      PanelEthernet.Visible := True;
    end;
  end
  else
  begin
    if ListviewSelectBar.SelCount = 0 then
    begin
      PanelThrottle.Visible := False;
      PanelGeneral.Visible := False;
      PanelComPort.Visible := False;
    end;
  end;
end;

procedure TFormSettings.ScanComPorts;
begin
  ComboBoxComPort.Items.Delimiter := ';';
  ComboBoxComPort.Items.DelimitedText :=
    StringReplace(GetSerialPortNames, ',', ';', [rfReplaceAll, rfIgnoreCase]);
  if ComboBoxComPort.Items.Count > 0 then
    ComboBoxComPort.ItemIndex := 0;
end;

procedure TFormSettings.StoreSettings;
begin
  // ComPort
  GlobalSettings.ComPort.Port := ComboBoxComPort.Caption;
  GlobalSettings.ComPort.BaudRate := StrToInt( ComboBoxBaud.Caption);
  GlobalSettings.ComPort.DataBits := StrToInt( ComboBoxDataBits.Caption);
  GlobalSettings.ComPort.StopBits := StrToInt( ComboBoxStopBits.Caption);
  GlobalSettings.ComPort.Parity := TComPortParity( ComboBoxParity.ItemIndex);
  GlobalSettings.ComPort.FlowControl := TComPortFlowControl( ComboBoxFlowControl.ItemIndex);

  // General
  GlobalSettings.General.SendPacketDelay := SpinEditSendPacketDelay.Value;
  GlobalSettings.General.AliasID := ValidateHex(EditAliasID.Caption);
  GlobalSettings.General.NodeID := ValidateHex(EditNodeID.Caption);
  GlobalSettings.General.AutoScanNetworkAtBoot := CheckBoxAutoScanAtStart.Checked;
  GlobalSettings.ComPort.AutoConnectAtBoot := CheckBoxAutoConnect.Checked;

  // Throttle
  GlobalSettings.Throttle.AutoLoadFDI := CheckBoxAutoLoadFDI.Checked;

  // Ethernet
  GlobalSettings.Ethernet.LocalIP := EditEthernetLocalIP.Text;      // Should validate this
  GlobalSettings.Ethernet.ClientPort := SpinEditEthernetLocalPort.Value;

  GlobalSettings.SaveToFile(UTF8ToSys( SettingsFilePath));

end;

procedure TFormSettings.FormShow(Sender: TObject);
begin
  ScanComPorts;
  ListviewSelectBar.Items[0].Selected := True;
  ListviewSelectBar.Items[0].Focused := True;
  // ComPort
  ComboBoxComPort.ItemIndex := ComboBoxComPort.Items.IndexOf(GlobalSettings.ComPort.Port);
  ComboBoxBaud.ItemIndex := ComboBoxBaud.Items.IndexOf(IntToStr(GlobalSettings.ComPort.BaudRate));
  ComboBoxDataBits.ItemIndex := ComboBoxDataBits.Items.IndexOf(IntToStr( GlobalSettings.ComPort.DataBits));
  ComboBoxStopBits.ItemIndex := ComboBoxStopBits.Items.IndexOf(IntToStr( GlobalSettings.ComPort.StopBits));
  ComboBoxParity.ItemIndex := Integer( GlobalSettings.ComPort.Parity);
  ComboBoxFlowControl.ItemIndex := Integer( GlobalSettings.ComPort.FlowControl);
  CheckBoxAutoConnect.Checked := GlobalSettings.ComPort.AutoConnectAtBoot;

  // General
  SpinEditSendPacketDelay.Value := GlobalSettings.General.SendPacketDelay;
  EditAliasID.Caption := ValidateHex( GlobalSettings.General.AliasID);
  EditNodeID.Caption := ValidateHex(GlobalSettings.General.NodeID);
  CheckBoxAutoScanAtStart.Checked := GlobalSettings.General.AutoScanNetworkAtBoot;

  // Throttle
  CheckBoxAutoLoadFDI.Checked := GlobalSettings.Throttle.AutoLoadFDI;

  //Ethernet
  EditEthernetLocalIP.Text := GlobalSettings.Ethernet.LocalIP;
  SpinEditEthernetLocalPort.Value := GlobalSettings.Ethernet.ClientPort;
end;

procedure TFormSettings.FormHide(Sender: TObject);
begin
  StoreSettings
end;

procedure TFormSettings.BitBtnRescanPortsClick(Sender: TObject);
begin
  ScanComPorts;
end;

procedure TFormSettings.BitBtnFTDIDefaultsClick(Sender: TObject);
var
  i: integer;
begin
  i := ComboboxBaud.Items.IndexOf('333333');
  if i > -1 then
    ComboboxBaud.ItemIndex := i
  else
    ComboboxBaud.Caption := '333333';
  ComboBoxDataBits.ItemIndex := ComboBoxDataBits.Items.IndexOf('8');
  ComboBoxStopBits.ItemIndex := ComboBoxStopBits.Items.IndexOf('0');
  ComboBoxParity.ItemIndex := ComboBoxParity.Items.IndexOf('None');
  ComboBoxFlowControl.ItemIndex := ComboBoxFlowControl.Items.IndexOf('XON/XOFF');
end;

end.