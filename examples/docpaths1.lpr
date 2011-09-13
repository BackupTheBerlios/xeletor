program docpaths1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, xdbfiles;

var
  Roots: TXDBRootDirectories;
begin
  Roots:=TXDBRootDirectories.Create('');
  try
    Roots.
  finally
    Roots.Free;
  end;
end.

