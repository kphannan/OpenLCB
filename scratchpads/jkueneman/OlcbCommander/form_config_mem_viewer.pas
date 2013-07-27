unit form_config_mem_viewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterXML, Forms, Controls,
  Graphics, Dialogs, ComCtrls, Grids, ExtCtrls, StdCtrls, Buttons, laz2_DOM,
  com_port_hub, olcb_app_common_settings, olcb_utilities,
  olcb_defines, KHexEditor, laz2_XMLRead;

type

  { TFormMemConfigViewer }

  TFormMemConfigViewer = class(TForm)
    ButtonClose: TButton;
    KHexEditor: TKHexEditor;
    PageControlMemConfig: TPageControl;
    Panel1: TPanel;
    SynEditCDI: TSynEdit;
    SynXMLSyn: TSynXMLSyn;
    TabSheetRawData: TTabSheet;
    procedure ButtonCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FAliasID: Word;
    FComPortThread: TComPortHub;
    FXmlDoc: TXMLDocument;
    procedure SetComPortHub(AValue: TComPortHub);
    { private declarations }
  protected

  public
    { public declarations }
    property AliasID: Word read FAliasID write FAliasID;
    property ComPortThread: TComPortHub read FComPortThread write SetComPortHub;
    property XmlDoc: TXMLDocument read FXmlDoc write FXmlDoc;
  end;

implementation

{$R *.lfm}

{ TFormMemConfigViewer }

procedure TFormMemConfigViewer.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TFormMemConfigViewer.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMemConfigViewer.FormCreate(Sender: TObject);
begin
  FComPortThread := nil;
  FAliasID := 0;
end;


procedure TFormMemConfigViewer.SetComPortHub(AValue: TComPortHub);
begin
  FComPortThread:=AValue;
end;


end.
