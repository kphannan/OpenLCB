program commandstation_project;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, unitmain, ethernet_hub, opstackcore, opstackcanstatemachines,
  HelperFunctions, template_configuration, template_hardware
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='CommandStationEmulator';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

