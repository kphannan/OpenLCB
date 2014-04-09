unit unitmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynMemo, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, ethernet_hub, olcb_transport_layer,
  template_hardware,
  opstackcore;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonClear: TButton;
    StatusBar: TStatusBar;
    SynMemo: TSynMemo;
    TimerOpStackProcess: TTimer;
    TimerOpStackTimer: TTimer;
    ToggleBoxStartNode: TToggleBox;
    ToggleBoxConnect: TToggleBox;
    procedure ButtonClearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerOpStackProcessTimer(Sender: TObject);
    procedure TimerOpStackTimerTimer(Sender: TObject);
    procedure ToggleBoxConnectChange(Sender: TObject);
    procedure ToggleBoxStartNodeChange(Sender: TObject);
  private
    FClientCount: integer;
    FHubConnected: Boolean;
    FHubIP: string;
    FHubPort: Integer;
    FShownOnce: Boolean;
    { private declarations }
  protected
    procedure HubConnect(HostIP: string; HostPort: Integer);
    procedure HubDisconnect(HostIP: string; HostPort: Integer);
    procedure ClientConnect(SocketCount: Integer);
    procedure ClientDisconnect(SocketCount: Integer);
    procedure HubMessage(Sender: TObject; MessageStr: String);
    procedure HubDestroyTask(Sender: TTaskOlcbBase);
    property HubConnected: Boolean read FHubConnected write FHubConnected;
    property HubIP: string read FHubIP write FHubIP;
    property HubPort: Integer read FHubPort write FHubPort;
    property ClientCount: integer read FClientCount write FClientCount;
    property ShownOnce: Boolean read FShownOnce write FShownOnce;
  public
    { public declarations }

    procedure UpdateUI;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ButtonClearClick(Sender: TObject);
begin
  SynMemo.BeginUpdate();
  try
    SynMemo.Lines.Clear;
  finally
    SynMemo.EndUpdate;
  end;
end;

procedure TForm1.ClientConnect(SocketCount: Integer);
begin
  Inc(FClientCount);
  UpdateUI
end;

procedure TForm1.ClientDisconnect(SocketCount: Integer);
begin
  Dec(FClientCount);
  UpdateUI
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  EthernetHub.OnHubConnect := @HubConnect;
  EthernetHub.OnHubDisconnect := @HubDisconnect;
  EthernetHub.OnClientClientConnect := @ClientConnect;
  EthernetHub.OnClientDisconnect := @ClientDisconnect;
  EthernetHub.SyncReceiveMessageFunc := @HubMessage;
  EthernetHub.SyncSendMessageFunc:=@HubMessage;
  EthernetHub.OnBeforeDestroyTask := @HubDestroyTask;
  EthernetHub.EnableReceiveMessages := True;
  EthernetHub.EnableSendMessages := True;
  FHubConnected := False;
  FClientCount := 0;
  FHubIP := '';
  FHubPort := 0;
  FShownOnce := False;
  OPStackCore_Initialize;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  if not ShownOnce then
  begin
    UpdateUI;
    ShownOnce := True;
  end;
end;

procedure TForm1.HubConnect(HostIP: string; HostPort: Integer);
begin
  FHubConnected := True;
  FHubIP := HostIP;
  FHubPort := HostPort;
  UpdateUI
end;

procedure TForm1.HubDestroyTask(Sender: TTaskOlcbBase);
begin
  Exit;
end;

procedure TForm1.HubDisconnect(HostIP: string; HostPort: Integer);
begin
  FHubConnected := False;
  FClientCount := 0;
  UpdateUI
end;

procedure TForm1.HubMessage(Sender: TObject; MessageStr: String);
begin
  SynMemo.BeginUpdate();
  try
    SynMemo.Lines.Add(MessageStr);
  finally
    SynMemo.EndUpdate;
  end;
end;

procedure TForm1.TimerOpStackProcessTimer(Sender: TObject);
begin
  OPStackCore_Process;
end;

procedure TForm1.TimerOpStackTimerTimer(Sender: TObject);
begin
  OPStackCore_Timer;
end;

procedure TForm1.ToggleBoxConnectChange(Sender: TObject);
begin
  EthernetHub.Enabled := ToggleBoxConnect.Checked;
  ToggleBoxStartNode.Enabled := ToggleBoxConnect.Checked;
  if not ToggleBoxStartNode.Enabled then
    OPStackCore_Enable(False);
end;

procedure TForm1.ToggleBoxStartNodeChange(Sender: TObject);
begin
  OPStackCore_Enable(True);
end;

procedure TForm1.UpdateUI;
begin
  if HubConnected then
  begin
    Statusbar.Panels[0].Text := 'Connected: ' + HubIP + ':' + IntToStr(HubPort);
    Statusbar.Panels[1].Text := IntToStr(ClientCount) + ' Clients';
  end else
  begin
    Statusbar.Panels[0].Text := 'Disconnected:';
  end;
end;

end.

