{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RackCtlsPkg;

interface

uses
  RackCtls, rrColors, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RackCtls', @RackCtls.Register);
end;

initialization
  RegisterPackage('RackCtlsPkg', @Register);
end.
