{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit XPathDBLaz; 

interface

uses
  xdbutils, xdbfiles, xdbprocess, xdbcentral, xdblog, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('XPathDBLaz', @Register); 
end.
