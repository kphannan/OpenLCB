program AwesomeThrottle;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, AwesomeThrottleMain, form_comportsettings, form_logwindow, form_about,
  form_aliastree, form_memconfig, khexeditorlaz,
math_float16, nmra_dcc;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFormComPort, FormComPort);
  Application.CreateForm(TFormLog, FormLog);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.CreateForm(TFormAliasTree, FormAliasTree);
  Application.CreateForm(TFormMemConfig, FormMemConfig);
  Application.Run;
end.
