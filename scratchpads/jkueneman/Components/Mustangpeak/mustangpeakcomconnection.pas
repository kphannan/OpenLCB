unit mustangpeakcomconnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type

  { TMustangpeakComConnection }

  TMustangpeakComConnection = class(TComponent)
  private
    FBaud: Integer;
    FComPort: string;
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
    property ComPort: string read FComPort write FComPort;
    property Baud: Integer  read FBaud write FBaud;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I mustangpeakcomconnection_icon.lrs}
  RegisterComponents('Mustangpeak',[TMustangpeakComConnection]);
end;

end.
