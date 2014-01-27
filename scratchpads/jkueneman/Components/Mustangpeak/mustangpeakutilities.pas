{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MustangpeakUtilities;

interface

uses
  mustangpeakcomconnection, mustangpeakethernetconnection, threadedstringlist, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('mustangpeakcomconnection', @mustangpeakcomconnection.Register);
  RegisterUnit('mustangpeakethernetconnection', 
    @mustangpeakethernetconnection.Register);
end;

initialization
  RegisterPackage('MustangpeakUtilities', @Register);
end.
