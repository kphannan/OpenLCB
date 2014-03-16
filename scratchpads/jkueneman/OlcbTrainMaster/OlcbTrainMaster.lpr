program OlcbTrainMaster;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, form_throttle_multiple_trains, form_train_config_editor,
  form_throttle, unit_cdi_parser, olcb_transport_layer, olcb_defines,
  math_float16, olcb_app_common_settings, olcb_utilities
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFormOlcbTrainMaster, FormOlcbTrainMaster);
  Application.Run;
end.

