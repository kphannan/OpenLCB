program OlcbTrainMaster;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, form_throttle_multiple_trains, form_train_config_editor,
  form_throttle, math_float16;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFormOlcbTrainMaster, FormOlcbTrainMaster);
  Application.Run;
end.

