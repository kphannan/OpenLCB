program CDI_Converter;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitMainUnit in 'UnitMainUnit.pas' {Form1};

{.$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

