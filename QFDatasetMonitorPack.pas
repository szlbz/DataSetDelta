{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit QFDatasetMonitorPack;

{$warn 5023 off : no warning about unused units}
interface

uses
  QFDataSetDelta, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('QFDataSetDelta', @QFDataSetDelta.Register);
end;

initialization
  RegisterPackage('QFDatasetMonitorPack', @Register);
end.
