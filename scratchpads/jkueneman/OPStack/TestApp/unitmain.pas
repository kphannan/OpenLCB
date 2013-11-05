unit unitMain;

{$mode objfpc}{$H+}

interface

{$I Options.inc}

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, opstackcore, opstacknode,
  {$IFDEF HARDWARE_TEMPLATE}hardware_template,{$ENDIF}
  {$IFDEF HARDWARE_DSPIC_CAN}hardware_dspic_CAN,{$ENDIF}
  {$IFDEF HARDWARE_ENC28J60}hardware_ENC28j60,{$ENDIF}
  opstackbuffers,
  opstacktypes,
  template_node,
  opstackdefines;
type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonSendGlobalNotify: TButton;
    ButtonStartStack: TButton;
    ButtonAllocateNode: TButton;
    ButtonDeallocateNode: TButton;
    CheckBoxLogMessages: TCheckBox;
    CheckBoxAutoConnect: TCheckBox;
    MemoReceive: TMemo;
    StatusBar: TStatusBar;
    TimerStatemachine: TTimer;
    TimerCore: TTimer;
    procedure ButtonSendGlobalNotifyClick(Sender: TObject);
    procedure ButtonAllocateNodeClick(Sender: TObject);
    procedure ButtonDeallocateNodeClick(Sender: TObject);
    procedure ButtonStartStackClick(Sender: TObject);
    procedure CheckBoxLogMessagesChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerCoreTimer(Sender: TObject);
    procedure TimerStatemachineTimer(Sender: TObject);
  private
    FHalfConnected: Boolean;
  private
    FConnected: Boolean;
    FListener: TOPStackTestListener;
    property HalfConnected: Boolean read FHalfConnected write FHalfConnected;
  public
    { public declarations }
    procedure ListenerCallback(ReceiveStr: ansistring);
    procedure ConnectedCallback(EthernetThreadType: TEthernetThreadType);
    procedure UpdateUI;
    property Connected: Boolean read FConnected;
    property Listener: TOPStackTestListener read FListener write FListener;
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
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Listener := TOPStackTestListener.Create(False);
  Listener.FreeOnTerminate := True;
  ListenerThread := Listener;
  Listener.Callback := @ListenerCallback;
  Listener.RunningCallback := @ConnectedCallback;
  UpdateUI;
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
  MemoReceive.Lines.BeginUpdate;
  MemoReceive.Text := MemoReceive.Text + ReceiveStr;
  MemoReceive.SelStart := MemoReceive.GetTextLen;
  MemoReceive.SelLength := 0;
  MemoReceive.ScrollBy(0, MemoReceive.Lines.Count);
  MemoReceive.Lines.EndUpdate;
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
  Statusbar.Panels[2].Text := 'Message Buffers: ' + IntToStr(SimpleMessagePool.Count);
  Statusbar.Panels[3].Text := 'CAN Buffers: ' + IntToStr(SimpleBufferPool.Count);
  Statusbar.Panels[4].Text := 'Datagram Buffers: ' + IntToStr(DatagramBufferPool.Count);
  Statusbar.Panels[5].Text := 'Steam Buffers: ' + IntToStr(StreamBufferPool.Count);
end;

procedure TForm1.ButtonSendGlobalNotifyClick(Sender: TObject);
begin
  Listener.Send(':X19490F37N;');
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

procedure TForm1.CheckBoxLogMessagesChange(Sender: TObject);
begin
  if CheckBoxLogMessages.Checked and Assigned(FListener) then
  begin
    Listener.ConnectionOutput.Callback := Listener.Callback;
    Listener.ConnectionInput.Callback := Listener.Callback;
  end else
  begin
    Listener.ConnectionOutput.Callback := nil;
    Listener.ConnectionInput.Callback := nil;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(FListener) then
  begin
    Listener.Callback := nil;
    Listener.RunningCallback := nil;
    Listener.Terminate;
  //  Listener.WaitFor;
  end;
end;

end.

