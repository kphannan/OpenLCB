program OPStackCommandStationEmulator;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, unitMain, OPStackCore, nmranetdefines, opstackdefines, opstacknode,
  nmranetutilities, opstackbuffers, opstacktypes, gridconnect,
  template_event_callbacks, template_buffers, template_configmem,
  template_configuration, template_node, template_vnode, unitbuffertrace,
  opstackcore_can, opstackcore_events, opstackcore_pip, opstackcore_basic,
  opstackcore_stream, opstackcore_snip, opstackcore_learn, opstackcore_datagram,
  opstackcore_configmem, hardware_template, opstackcanstatemachines,
  opstackcanstatemachinesbuffers, opstackcanstatemachinesdatagram,
  opstackcanstatemachinesmultiframe, opstackcanstatemachinesstream,
  opstackcanstatemachinessnip, opstackcore_traction;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

