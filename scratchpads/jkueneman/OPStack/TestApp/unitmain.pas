unit unitMain;

{$mode objfpc}{$H+}

interface

{$I Options.inc}

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Menus, opstackcore, opstacknode,
  {$IFDEF HARDWARE_TEMPLATE}hardware_template,{$ENDIF}
  {$IFDEF HARDWARE_DSPIC_CAN}hardware_dspic_CAN,{$ENDIF}
  {$IFDEF HARDWARE_ENC28J60}hardware_ENC28j60,{$ENDIF}
  opstackbuffers,
  opstacktypes,
  template_node;
type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonClear: TButton;
    ButtonRefreshBufferTracking: TButton;
    ButtonSendGlobalNotify: TButton;
    ButtonStartStack: TButton;
    ButtonAllocateNode: TButton;
    ButtonDeallocateNode: TButton;
    CheckBoxDisableLogging: TCheckBox;
    CheckBoxLogMessages: TCheckBox;
    CheckBoxAutoConnect: TCheckBox;
    MainMenu1: TMainMenu;
    MemoReceive: TMemo;
    MenuItemTrackBuffer: TMenuItem;
    RadioGroupEthernet: TRadioGroup;
    StatusBar: TStatusBar;
    TimerStatemachine: TTimer;
    TimerCore: TTimer;
    procedure ButtonClearClick(Sender: TObject);
    procedure ButtonRefreshBufferTrackingClick(Sender: TObject);
    procedure ButtonSendGlobalNotifyClick(Sender: TObject);
    procedure ButtonAllocateNodeClick(Sender: TObject);
    procedure ButtonDeallocateNodeClick(Sender: TObject);
    procedure ButtonStartStackClick(Sender: TObject);
    procedure CheckBoxDisableLoggingChange(Sender: TObject);
    procedure CheckBoxDisableTrackingChange(Sender: TObject);
    procedure CheckBoxLogMessagesChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RadioGroupEthernetClick(Sender: TObject);
    procedure TimerCoreTimer(Sender: TObject);
    procedure TimerStatemachineTimer(Sender: TObject);
  private
    FHalfConnected: Boolean;
  private
    FClient: TOPstackTestClient;
    FConnected: Boolean;
    FDisableLogging: Boolean;
    FListener: TOPStackTestListener;
    property HalfConnected: Boolean read FHalfConnected write FHalfConnected;
  public
    { public declarations }
    procedure ListenerCallback(ReceiveStr: ansistring);
    procedure ConnectedCallback(EthernetThreadType: TEthernetThreadType);
    procedure UpdateUI;
    property Client: TOPstackTestClient read FClient write FClient;
    property Connected: Boolean read FConnected;
    property Listener: TOPStackTestListener read FListener write FListener;
    property DisableLogging: Boolean read FDisableLogging write FDisableLogging;
  end;


var
  Form1: TForm1;

implementation

{$R *.lfm}


{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FHalfConnected := False;
  FConnected := False;
  OPStackCore_Initialize;
  Client := nil;
  Listener := nil;
  ClientThread := nil;
  ListenerThread := nil;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Listener := TOPStackTestListener.Create(False);
  ListenerThread := Listener;
  Listener.Callback := @ListenerCallback;
  Listener.RunningCallback := @ConnectedCallback;
  DisableLogging := False;
  UpdateUI;
end;

procedure TForm1.RadioGroupEthernetClick(Sender: TObject);
begin
  case RadioGroupEthernet.ItemIndex of
    0 : begin
          if Assigned(FClient) then
          begin
            ClientThread := nil;
            Client.Terminate;
            RTLeventSetEvent(Client.Event);
            while not Client.HasTerminated do
              ThreadSwitch;
            FreeAndNIl(FClient);
          end;

          Listener := TOPStackTestListener.Create(False);
          ListenerThread := Listener;
          Listener.Callback := @ListenerCallback;
          Listener.RunningCallback := @ConnectedCallback;
          UpdateUI;
        end;
    1 : begin
          ListenerThread := nil;
          if Assigned(Listener.ConnectionOutput) then
            Listener.ConnectionOutput.Callback := nil;
          if Assigned(Listener.ConnectionInput) then
            Listener.ConnectionInput.Callback := nil;
          Listener.Terminate;
          Listener.Abort;
          while not Listener.HasTerminated do;
            ThreadSwitch;
          FreeAndNil(FListener);

          Client := TOPstackTestClient.Create(True);
          Client.Callback := @ListenerCallback;
          Client.RunningCallback := @ConnectedCallback;
          Client.Start;
          ClientThread := Client;
          UpdateUI;
        end;
    end;
end;

procedure TForm1.TimerCoreTimer(Sender: TObject);
begin
  OPStackCore_Timer;
end;

procedure TForm1.TimerStatemachineTimer(Sender: TObject);
begin
  OPStackCore_Process;
end;

procedure TForm1.ListenerCallback(ReceiveStr: ansistring);
begin
  if not DisableLogging then
  begin
    MemoReceive.Lines.BeginUpdate;
    MemoReceive.Text := MemoReceive.Text + ReceiveStr;
    MemoReceive.SelStart := MemoReceive.GetTextLen;
    MemoReceive.SelLength := 0;
    MemoReceive.ScrollBy(0, MemoReceive.Lines.Count);
    MemoReceive.Lines.EndUpdate;
  end;
end;

procedure TForm1.ConnectedCallback(EthernetThreadType: TEthernetThreadType);
begin
  if HalfConnected then
  begin
    FConnected := True;
    if CheckBoxAutoConnect.Checked then
      ButtonStartStack.Click;
    UpdateUI;
  end
  else
    HalfConnected := True;
end;

procedure TForm1.UpdateUI;
begin
  ButtonAllocateNode.Enabled := NodePool.AllocatedCount < USER_MAX_NODE_COUNT;
  ButtonDeallocateNode.Enabled := NodePool.AllocatedCount > 1;
  Statusbar.Panels[1].Text := 'Allocated Nodes: ' + IntToStr(NodePool.AllocatedCount);
  if Connected then
    Statusbar.Panels[0].Text := 'Connected'
  else
    Statusbar.Panels[0].Text := 'Not Connected';
  ButtonStartStack.Enabled := Connected;
  Statusbar.Panels[2].Text := 'Message Buffers: ' + IntToStr(OPStackMessagePool.Count);
  Statusbar.Panels[3].Text := 'CAN Buffers: ' + IntToStr(SimpleBufferPool.Count);
  Statusbar.Panels[4].Text := 'Datagram Buffers: ' + IntToStr(DatagramBufferPool.Count);
  Statusbar.Panels[5].Text := 'Steam Buffers: ' + IntToStr(StreamBufferPool.Count);
end;

procedure TForm1.ButtonSendGlobalNotifyClick(Sender: TObject);
begin
  if Assigned(Listener) then
    Listener.Send(':X19490F37N;');
  UpdateUI
end;

procedure TForm1.ButtonClearClick(Sender: TObject);
begin
  MemoReceive.Lines.BeginUpdate;
  MemoReceive.Lines.Clear;
  MemoReceive.Lines.EndUpdate;
end;

procedure TForm1.ButtonRefreshBufferTrackingClick(Sender: TObject);
begin
  UpdateUI
end;

procedure TForm1.ButtonAllocateNodeClick(Sender: TObject);
begin
  OPStackNode_Allocate;
  UpdateUI
end;

procedure TForm1.ButtonDeallocateNodeClick(Sender: TObject);
begin
  OPStackNode_MarkForRelease(OPStackNode_FindLastVirtualNode);
  UpdateUI;
end;

procedure TForm1.ButtonStartStackClick(Sender: TObject);
begin
  OPStack.State := OPStack.State or OPS_PROCESSING;
end;

procedure TForm1.CheckBoxDisableLoggingChange(Sender: TObject);
begin
  DisableLogging := CheckBoxDisableLogging.Checked;
end;

procedure TForm1.CheckBoxDisableTrackingChange(Sender: TObject);
begin

end;

procedure TForm1.CheckBoxLogMessagesChange(Sender: TObject);
begin
  if CheckBoxLogMessages.Checked and Assigned(FListener) then
  begin
    if Assigned(Listener.ConnectionOutput) then
      Listener.ConnectionOutput.Callback := Listener.Callback;
    if Assigned(Listener.ConnectionInput.Callback) then
      Listener.ConnectionInput.Callback := Listener.Callback;
  end else
  begin
    if Assigned(Listener.ConnectionOutput) then
      Listener.ConnectionOutput.Callback := nil;
    if Assigned(Listener.ConnectionInput.Callback) then
      Listener.ConnectionInput.Callback := nil;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: Integer;
begin
  ClientThread := nil;
  ListenerThread := nil;

  if Assigned(FListener) then
  begin
    Listener.Callback := nil;
    Listener.RunningCallback := nil;
    Listener.Terminate;
    Listener.Abort;
    while not Listener.HasTerminated do;
      ThreadSwitch;
    FreeAndNil(FListener);
  end;

  if Assigned(FClient) then
  begin
    Client.Terminate;
    RTLeventSetEvent(Client.Event);
    while not Client.HasTerminated do
      ThreadSwitch;
    FreeAndNIl(FClient);
  end;
end;

end.

