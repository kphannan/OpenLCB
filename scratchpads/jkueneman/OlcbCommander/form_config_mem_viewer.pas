unit form_config_mem_viewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterXML, Forms, Controls,
  Graphics, Dialogs, ComCtrls, laz2_DOM, laz2_XMLRead;

type

  { TFormMemConfigViewer }

  TFormMemConfigViewer = class(TForm)
    PageControlMemConfig: TPageControl;
    SynEditCDI: TSynEdit;
    SynXMLSyn: TSynXMLSyn;
    TabSheetConfigTool: TTabSheet;
    TabSheetRawData: TTabSheet;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    FXmlDoc: TXMLDocument;
    { private declarations }
  public
    { public declarations }
    property XmlDoc: TXMLDocument read FXmlDoc write FXmlDoc;
  end;

implementation

{$R *.lfm}

{ TFormMemConfigViewer }

procedure TFormMemConfigViewer.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

end.

