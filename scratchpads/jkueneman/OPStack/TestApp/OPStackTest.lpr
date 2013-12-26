program OPStackTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, unitMain, OPStackCore, nmranetdefines, opstackdefines, opstacknode,
  template_node, template_vnode, nmranetutilities, opstackbuffers,
  template_buffers, opstacktypes, gridconnect, template_event_callbacks,
  template_configmem, unitbuffertrace, template_Configuration, opstackcore_can,
  opstackcore_events, opstackcore_pip, opstackcore_basic, opstackcore_stream,
  opstackcore_snip, opstackcore_learn, opstackcore_datagram,
  opstackcore_configmem, hardware_template, opstackcanstatemachines,
  opstackcanstatemachinesbuffers, opstackcanstatemachinesdatagram,
  opstackcanstatemachinessnip, opstackcanstatemachinesstream;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

