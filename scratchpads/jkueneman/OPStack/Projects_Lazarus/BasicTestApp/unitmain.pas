unit unitMain;

{$mode objfpc}{$H+}

interface

{$I Options.inc}

uses
  Classes, SysUtils, FileUtil, SynEdit, SynMemo, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Menus, opstackcore, opstacknode, SynEditKeyCmds,
  LCLType,
  template_hardware,
  opstackbuffers,
  olcb_utilities,
  opstacktypes,
  opstackdefines,
  olcb_transport_layer,
  ethernet_hub,
  template_node;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonClear: TButton;
    ButtonRefreshBufferCount: TButton;
    ButtonRefreshBufferTracking: TButton;
    ButtonSendGlobalNotify: TButton;
    ButtonStartStack: TButton;
    ButtonAllocateNode: TButton;
    ButtonDeallocateNode: TButton;
    CheckBoxDisableLogging: TCheckBox;
    CheckBoxLogMessages: TCheckBox;
    CheckBoxAutoConnect: TCheckBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label9: TLabel;
    LabelDatagramCount: TLabel;
    LabelDatagramCountMax: TLabel;
    LabelMessageCount: TLabel;
    LabelMessageCountMax: TLabel;
    LabelMultiFrameCount: TLabel;
    LabelMultiFrameCountMax: TLabel;
    LabelSimpleCount: TLabel;
    LabelSimpleCountMax: TLabel;
    LabelSnipCount: TLabel;
    LabelSnipCountMax: TLabel;
    MainMenu1: TMainMenu;
    MenuItemTrackBuffer: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    RadioGroupEthernet: TRadioGroup;
    Splitter1: TSplitter;
    StatusBar: TStatusBar;
    SynMemo: TSynMemo;
    TimerStatemachine: TTimer;
    TimerCore: TTimer;
    procedure ButtonClearClick(Sender: TObject);
    procedure ButtonRefreshBufferCountClick(Sender: TObject);
    procedure ButtonRefreshBufferTrackingClick(Sender: TObject);
    procedure ButtonSendGlobalNotifyClick(Sender: TObject);
    procedure ButtonAllocateNodeClick(Sender: TObject);
    procedure ButtonDeallocateNodeClick(Sender: TObject);
    procedure ButtonStartStackClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RadioGroupEthernetClick(Sender: TObject);
    procedure SynMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TimerCoreTimer(Sender: TObject);
    procedure TimerStatemachineTimer(Sender: TObject);
  private
    FComConnectionState: TConnectionState;
    FEthernetConnectionState: TConnectionState;
  public
    { public declarations }
    procedure EthernetReceiveLogging(Sender: TObject; MessageStr: String);
    procedure EthernetSendLogging(Sender: TObject; MessageStr: String);
    procedure EthernetConnectState(Sender: TObject; ConnectionState: TConnectionState);
    procedure EthernetError(Sender: TObject; MessageStr: string);
    procedure ComPortError(Sender: TObject; MessageStr: String);
    procedure ComPortConnectionState(Sender: TObject; NewConnectionState: TConnectionState);
    procedure ComPortReceiveLogging(Sender: TObject; MessageStr: String);
    procedure ComPortSendLogging(Sender: TObject; MessageStr: String);
    procedure UpdateMessageCountUI;

    procedure UpdateUI;
    property ComConnectionState: TConnectionState read FComConnectionState write FComConnectionState;
    property EthernetConnectionState: TConnectionState read FEthernetConnectionState write FEthernetConnectionState;
  end;


var
  Form1: TForm1;

implementation

{$R *.lfm}


{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  ComPortHub.OnReceiveMessage := @ComPortReceiveLogging;
  ComPortHub.OnSendMessage := @ComPortSendLogging;
  ComPortHub.OnErrorMessage := @ComPortError;
  ComPortHub.OnConnectionStateChange := @ComPortConnectionState;

  EthernetHub.OnReceiveMessage := @EthernetReceiveLogging;
  EthernetHub.OnSendMessage := @EthernetSendLogging;
  EthernetHub.OnErrorMessage := @EthernetError;
  EthernetHub.OnConnectionStateChange := @EthernetConnectState;

  FComConnectionState := csDisconnected;
  FEthernetConnectionState := csDisconnected;

  OPStackCore_Initialize;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  UpdateUI;
end;

procedure TForm1.RadioGroupEthernetClick(Sender: TObject);
begin
  case RadioGroupEthernet.ItemIndex of
    0 : begin
          EthernetHub.Enabled := True;
        end;
    1 : begin
          EthernetHub.Listener := True;
          EthernetHub.EnableSendMessages := not CheckBoxDisableLogging.Checked;
          EthernetHub.EnableReceiveMessages := not CheckBoxDisableLogging.Checked;
          EthernetHub.Enabled := True;
        end;
    2 : begin
          EthernetHub.Listener := False;
          EthernetHub.EnableSendMessages := not CheckBoxDisableLogging.Checked;
          EthernetHub.EnableReceiveMessages := not CheckBoxDisableLogging.Checked;
          EthernetHub.Enabled := True;
        end;
  end;
end;

procedure TForm1.SynMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TForm1.TimerCoreTimer(Sender: TObject);
begin
  OPStackCore_Timer;
end;

procedure TForm1.TimerStatemachineTimer(Sender: TObject);
begin
  OPStackCore_Process;
end;

procedure TForm1.UpdateMessageCountUI;
begin
  LabelSimpleCount.Caption := IntToSTr(SimpleBufferPool.Count);
  LabelSnipCount.Caption := IntToStr(AcdiSnipBufferPool.Count);
  LabelDatagramCount.Caption := IntToStr(DatagramBufferPool.Count);
  LabelMultiFrameCount.Caption := IntToStr(MultiFramePool.Count);
  LabelMessageCount.Caption := IntToStr(OPStackMessagePool.Count);

  LabelSimpleCountMax.Caption := IntToSTr(SimpleBufferPool.MaxCount);
  LabelSnipCountMax.Caption := IntToStr(AcdiSnipBufferPool.MaxCount);
  LabelDatagramCountMax.Caption := IntToStr(DatagramBufferPool.MaxCount);
  LabelMultiFrameCountMax.Caption := IntToStr(MultiFramePool.MaxCount);
  LabelMessageCountMax.Caption := IntToStr(OPStackMessagePool.MaxCount);
end;

procedure TForm1.UpdateUI;
begin
  case EthernetConnectionState of
    csDisconnected :
      begin
        Statusbar.Panels[0].Text := 'Ethernet: Disconnected';
      end;
    csConnecting :
      begin
       Statusbar.Panels[0].Text := 'Ethernet: Connecting';
      end;
    csDisconnecting :
      begin
        Statusbar.Panels[0].Text := 'Ethernet: Disconnecting';
      end;
    csConnected :
      begin
        if EthernetHub.Listener then
          Statusbar.Panels[0].Text := 'Connected - Listening on: ' //+ GlobalSettings.Ethernet.LocalIP + ':' + IntToStr(GlobalSettings.Ethernet.ListenPort)
        else
          Statusbar.Panels[0].Text := 'Connected - Client: ' //+ GlobalSettings.Ethernet.LocalIP + ':' + IntToStr(GlobalSettings.Ethernet.ClientPort)
      end;
   end;

  ButtonAllocateNode.Enabled := NodePool.AllocatedCount < USER_MAX_NODE_COUNT;
  ButtonDeallocateNode.Enabled := NodePool.AllocatedCount > 1;
  Statusbar.Panels[1].Text := 'Allocated Nodes: ' + IntToStr(NodePool.AllocatedCount);
  ButtonStartStack.Enabled := EthernetHub.Enabled;
  Statusbar.Panels[2].Text := 'Message Buffers: ' + IntToStr(OPStackMessagePool.Count);
  Statusbar.Panels[3].Text := 'CAN Buffers: ' + IntToStr(SimpleBufferPool.Count);
  Statusbar.Panels[4].Text := 'Datagram Buffers: ' + IntToStr(DatagramBufferPool.Count);
  UpdateMessageCountUI;
  {$IFDEF SUPPORT_STREAMS}
  Statusbar.Panels[5].Text := 'Steam Buffers: ' + IntToStr(StreamBufferPool.Count);
  {$ENDIF}
end;

procedure TForm1.ButtonSendGlobalNotifyClick(Sender: TObject);
begin
 // if Assigned(Listener) then
 //   Listener.Send(':X19490F37N;');
  UpdateUI
end;

procedure TForm1.ButtonClearClick(Sender: TObject);
begin
  SynMemo.Lines.BeginUpdate;
  SynMemo.Lines.Clear;
  SynMemo.Lines.EndUpdate;
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

procedure TForm1.ButtonRefreshBufferCountClick(Sender: TObject);
begin
  UpdateMessageCountUI
end;

procedure TForm1.ButtonStartStackClick(Sender: TObject);
begin
  OPStack.State := OPStack.State or OPS_PROCESSING;
end;

procedure TForm1.ComPortConnectionState(Sender: TObject; NewConnectionState: TConnectionState);
begin
  ComConnectionState := NewConnectionState;
  UpdateUI;
end;

procedure TForm1.ComPortError(Sender: TObject; MessageStr: String);
begin

end;

procedure TForm1.ComPortReceiveLogging(Sender: TObject; MessageStr: String);
begin
  PrintToSynMemo(MessageStr, SynMemo, False, CheckBoxDisableLogging.Checked, False);
end;

procedure TForm1.ComPortSendLogging(Sender: TObject; MessageStr: String);
begin
  PrintToSynMemo(MessageStr, SynMemo, False, CheckBoxDisableLogging.Checked, False);
end;

procedure TForm1.EthernetConnectState(Sender: TObject;
  ConnectionState: TConnectionState);
begin
  if (Sender is TEthernetListenDameonThread) or (not EthernetHub.Listener) then
    EthernetConnectionState := ConnectionState;
  UpdateUI
end;

procedure TForm1.EthernetError(Sender: TObject; MessageStr: string);
begin

end;

procedure TForm1.EthernetReceiveLogging(Sender: TObject; MessageStr: String);
begin
  PrintToSynMemo(MessageStr, SynMemo, False, CheckBoxDisableLogging.Checked, False);
end;

procedure TForm1.EthernetSendLogging(Sender: TObject; MessageStr: String);
begin
  PrintToSynMemo(MessageStr, SynMemo, False, CheckBoxDisableLogging.Checked, False);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ComPortHub.OnReceiveMessage := nil;
  ComPortHub.OnSendMessage := nil;
  ComPortHub.OnErrorMessage := nil;
  ComPortHub.OnConnectionStateChange := nil;
  ComPortHub.RemoveComPort(nil);

  EthernetHub.OnReceiveMessage := nil;
  EthernetHub.OnSendMessage := nil;
  EthernetHub.OnErrorMessage := nil;
  EthernetHub.OnConnectionStateChange := nil;
  EthernetHub.Enabled := False;
end;

end.

