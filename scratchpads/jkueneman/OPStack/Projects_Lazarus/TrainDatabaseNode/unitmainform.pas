unit unitmainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynMemo, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ActnList, Menus, ExtCtrls, StdCtrls,
  blcksock, synsock, SynEditKeyCmds, SynEditMarkupHighAll, LMessages,
  mustangpeakethernetconnection, mustangpeakcomconnection;

type

  { TForm1 }

  TForm1 = class(TForm)
    ActionSynMemoPaste: TAction;
    ActionSynMemoSelectAll: TAction;
    ActionSynMemoClear: TAction;
    ActionSynMemoCut: TAction;
    ActionSynMemoCopy: TAction;
    ActionComDisconnect: TAction;
    ActionComConnect: TAction;
    ActionEthernetDisconnect: TAction;
    ActionEthernetConnectServer: TAction;
    ActionEthernetConnectClient: TAction;
    ActionList: TActionList;
    ButtonSend: TButton;
    EditSend: TEdit;
    ImageListSmall: TImageList;
    MainMenu: TMainMenu;
    MenuItemSynMemoSpace1: TMenuItem;
    MenuItemSynMemoSpace0: TMenuItem;
    MenuItemSynMemoClearAll: TMenuItem;
    MenuItemSynMenoSelectAll: TMenuItem;
    MenuItemSynMemoPaste: TMenuItem;
    MenuItemSynMemoCopy: TMenuItem;
    MenuItemSynMemoCut: TMenuItem;
    MenuItemMainFileN: TMenuItem;
    MenuItemMainComDisconnect: TMenuItem;
    MenuItemMainComConnect: TMenuItem;
    MenuItemMainSeparatorConnection0: TMenuItem;
    MenuItemMainEthernetServer: TMenuItem;
    MenuItemMainEthernetConnectClient: TMenuItem;
    MenuItemMainEthernetDisconnect: TMenuItem;
    MenuItemConnections: TMenuItem;
    MenuItemMainFile: TMenuItem;
    MustangpeakComConnection: TMustangpeakComConnection;
    MustangpeakEthernetConnection: TMustangpeakEthernetConnection;
    Panel1: TPanel;
    PopupMenuSynMemo: TPopupMenu;
    Splitter1: TSplitter;
    StatusBar: TStatusBar;
    SynMemo: TSynMemo;
    SynMemoStatus: TSynMemo;
    TimerStateMachine: TTimer;
    procedure ActionSynMemoPasteExecute(Sender: TObject);
    procedure ActionEthernetConnectClientExecute(Sender: TObject);
    procedure ActionEthernetConnectServerExecute(Sender: TObject);
    procedure ActionEthernetDisconnectExecute(Sender: TObject);
    procedure ActionSynMemoClearExecute(Sender: TObject);
    procedure ActionSynMemoCopyExecute(Sender: TObject);
    procedure ActionSynMemoCutExecute(Sender: TObject);
    procedure ActionSynMemoSelectAllExecute(Sender: TObject);
    procedure ButtonSendClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MustangpeakEthernetConnectionEthernetReceive(Sender: TObject; Packet: string);
    procedure MustangpeakEthernetConnectionEthernetSocketEvent(Sender: TObject; var Event: TSocketEventBuffer);
    procedure TimerStateMachineTimer(Sender: TObject);
  private

  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }


procedure TForm1.ActionEthernetDisconnectExecute(Sender: TObject);
begin
  MustangpeakEthernetConnection.Connected := False;
  Caption:='Disconnected';
  ActionEthernetConnectServer.Enabled := True;
  ActionEthernetConnectClient.Enabled := True;
end;

procedure TForm1.ActionSynMemoClearExecute(Sender: TObject);
begin
  SynMemo.ClearAll;
end;

procedure TForm1.ActionSynMemoCopyExecute(Sender: TObject);
begin
  SynMemo.CommandProcessor(TSynEditorCommand(ecCopy), ' ', nil);
end;

procedure TForm1.ActionSynMemoCutExecute(Sender: TObject);
begin
  SynMemo.CommandProcessor(TSynEditorCommand(ecCut), ' ', nil);
end;

procedure TForm1.ActionSynMemoSelectAllExecute(Sender: TObject);
begin
  SynMemo.SelectAll;
end;

procedure TForm1.ButtonSendClick(Sender: TObject);
begin
  MustangpeakEthernetConnection.SendPacket(EditSend.Text);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  MustangpeakEthernetConnection.Connected := False;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ActionEthernetDisconnect.Execute;
end;

procedure TForm1.MustangpeakEthernetConnectionEthernetReceive(Sender: TObject;
  Packet: string);
begin
  SynMemo.Lines.Add(Packet);
end;

procedure TForm1.MustangpeakEthernetConnectionEthernetSocketEvent(
  Sender: TObject; var Event: TSocketEventBuffer);
var
  s: string;
  Ptr: ^Byte;
  i: Integer;
begin
  //SynMemoStatus.Lines.Add(Event^.ClassName);
  case Event.EventID of
    EVENT_AFTER_CONNECT : SynMemoStatus.Lines.Add('After Connect  [Local: ' + Event.Local + '  Remote: ' + Event.Remote + ']');
    EVENT_CREATE_SOCKET : SynMemoStatus.Lines.Add('Socket Created  [Local: ' + Event.Local + '  Remote: ' + Event.Remote + ']');
    EVENT_READ_FILTER   : SynMemoStatus.Lines.Add('Read Filter: Value = ' + Event.Value + '  [Local: ' + Event.Local + '  Remote: ' + Event.Remote + ']');
    EVENT_STATUS        : SynMemoStatus.Lines.Add('Conection Status: Reason = ' + ConnectionStatusReasonToStr(Event.Reason) + ' Value = ' + Event.Value + '  [Local: ' + Event.Local + '  Remote: ' + Event.Remote + ']');
    EVENT_HEARTBEAT     : SynMemoStatus.Lines.Add('Heartbeat  [Local: ' + Event.Local + '  Remote: ' + Event.Remote + ']');
    EVENT_MONITOR :
      begin
        s := 'Monitor: ';
        if Event.Writing then
          s := s + ' Writing, '
        else
          s := s + ' Reading, ';
        s := s + ' Length = ' + IntToStr(Event.Len) + ' ';
        Ptr := Event.Buffer;
        for i := 0 to Event.Len - 1 do
        begin
          s := s + '.' + IntToHex(Ptr^, 2);
          Inc(Ptr);
        end;
        s := s + '[';
        for i := 0 to Event.Len - 1 do
        begin
          s := s + '.' + Char(Ptr^);
          Inc(Ptr);
        end;
        s := s + ']  [Local: ' + Event.Local + '  Remote: ' + Event.Remote + ']';
        SynMemoStatus.Lines.Add(s);
    end;
  end;
end;

procedure TForm1.TimerStateMachineTimer(Sender: TObject);
begin
  StatusBar.Panels[0].Text := 'Thread Count: ' + IntToStr(ThreadObjectCount);
end;

procedure TForm1.ActionEthernetConnectServerExecute(Sender: TObject);
begin
  MustangpeakEthernetConnection.Connected := False;
  MustangpeakEthernetConnection.PortLocal := 12021;
  MustangpeakEthernetConnection.ConnectionType := mec_Server;
  MustangpeakEthernetConnection.Connected := True;
  Caption := 'I am a Server';
  ActionEthernetConnectClient.Enabled := False;
end;

procedure TForm1.ActionEthernetConnectClientExecute(Sender: TObject);
begin
  MustangpeakEthernetConnection.Connected := False;
  MustangpeakEthernetConnection.PortLocal := 12022;
  MustangpeakEthernetConnection.ConnectionType := mec_Client;
  MustangpeakEthernetConnection.Connected := True;
  Caption := 'I am a Client';
  ActionEthernetConnectServer.Enabled := False;
end;

procedure TForm1.ActionSynMemoPasteExecute(Sender: TObject);
begin
  SynMemo.CommandProcessor(TSynEditorCommand(ecPaste), ' ', nil);
end;

end.

