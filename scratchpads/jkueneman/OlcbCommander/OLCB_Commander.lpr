program OLCB_Commander;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, khexeditorlaz, form_main, olcb_app_common_settings, file_utilities,
  form_settings, form_about, form_ethernet_messagelog, common_utilities,
  olcb_node, com_port_hub, form_thread_debug, form_config_mem_viewer,
  common_objects, form_awesome_throttle, form_awesome_throttle_deallocate_error,
  form_awesome_throttle_duplicate_address, form_train_config_editor,
  unit_cdi_parser, olcb_defines, olcb_utilities, form_messagelog,
  olcb_transport_layer, ethernet_hub;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFormOLCB_Commander, FormOLCB_Commander);
  Application.CreateForm(TFormSettings, FormSettings);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.CreateForm(TFormMessageLog, FormMessageLog);
  Application.CreateForm(TFormEthernetMessageLog, FormEthernetMessageLog);
  Application.Run;
end.
