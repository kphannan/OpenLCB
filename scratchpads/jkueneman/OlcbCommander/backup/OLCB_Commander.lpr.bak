program OLCB_Commander;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, form_main, olcb_app_common_settings, file_utilities, form_settings,
  form_about, form_messagelog, common_utilities, snii, olcb_node,
olcb_mem_protocol
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFormOLCB_Commander, FormOLCB_Commander);
  Application.CreateForm(TFormSettings, FormSettings);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.CreateForm(TFormMessageLog, FormMessageLog);
  Application.Run;
end.

