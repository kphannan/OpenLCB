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
  opstacktypes,
  template_node,
  opstackdefines;
type

  { TOlcbThread }

  TOlcbThread = class(TThread)
    procedure Execute; override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ButtonStartStack: TButton;
    ButtonAllocateNode: TButton;
    ButtonDeallocateNode: TButton;
    MemoReceive: TMemo;
    StatusBar: TStatusBar;
    TimerCore: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure ButtonAllocateNodeClick(Sender: TObject);
    procedure ButtonDeallocateNodeClick(Sender: TObject);
    procedure ButtonStartStackClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerCoreTimer(Sender: TObject);
  private
    FHalfConnected: Boolean;
  private
    FConnected: Boolean;
    FListener: TOPStackTestListener;
    FOlcbThread: TOlcbThread;
    property HalfConnected: Boolean read FHalfConnected write FHalfConnected;
  public
    { public declarations }
    procedure ListenerCallback(ReceiveStr: ansistring);
    procedure ConnectedCallback(EthernetThreadType: TEthernetThreadType);
    procedure UpdateUI;
    property Connected: Boolean read FConnected;
    property OlcbThread: TOlcbThread read FOlcbThread write FOlcbThread;
    property Listener: TOPStackTestListener read FListener write FListener;
  end;


var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TOlcbThread }

procedure TOlcbThread.Execute;
begin
  while not Terminated do
  begin
    OPStackCore_Process;
  end;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  OPStackCore_Initialize;
  OlcbThread := TOlcbThread.Create(False);
  OlcbThread.FreeOnTerminate := True;
  Listener := TOPStackTestListener.Create(False);
  Listener.FreeOnTerminate := True;
  FHalfConnected := False;
  FConnected := False;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Listener.Callback := @ListenerCallback;
  Listener.RunningCallback := @ConnectedCallback;
  UpdateUI;
end;

procedure TForm1.TimerCoreTimer(Sender: TObject);
begin
  OPStackCore_Timer;
end;

procedure TForm1.ListenerCallback(ReceiveStr: ansistring);
begin
  MemoReceive.Lines.BeginUpdate;
  MemoReceive.Text := MemoReceive.Text + ReceiveStr;
  MemoReceive.Lines.EndUpdate;
end;

procedure TForm1.ConnectedCallback(EthernetThreadType: TEthernetThreadType);
begin
  if HalfConnected then
  begin
    FConnected := True;
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
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  StringList: TStringList;
begin
  StringList := TStringList.Create;
  StringList.Add(':X19490F37N;');
  Listener.Send(StringList);
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

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  OlcbThread.Terminate;
  Listener.Callback := nil;
  Listener.Terminate;
end;

end.

