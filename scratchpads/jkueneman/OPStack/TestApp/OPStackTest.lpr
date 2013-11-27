program OPStackTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, unitMain, OPStackCore, nmranetdefines, opstackdefines, opstacknode,
  template_node, template_vnode, hardware_template, nmranetutilities,
  opstackbuffers, template_buffers, opstacktypes, gridconnect,
  template_event_callbacks, opstackcanstatemachines, template_configmem, 
unitbuffertrace, template_Configuration;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

