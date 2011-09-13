//#!/usr/bin/env instantfpc

{$mode objfpc}{$H+}

uses
  Classes, sysutils, FileProcs, xdbutils, xdbfiles, AVL_Tree;

var
  XMLFilename: String;
  Root: TXDBRootNode;
  Node: TXDBNode;
begin
  if Paramcount<>1 then begin
    writeln('Usage: ',ExtractFileName(ParamStr(0)), ' <xml file>');
    exit;
  end;
  XMLFilename:=CleanAndExpandFilename(ParamStr(1));
  Root:=nil;
  try
    ReadXDBTree(Root,XMLFilename,true);
    for Node in Root do
      writeln(Node.ClassName);
    Root.Free;
  except
    on E: Exception do
      writeln('Error when retrieving ',ParamStr(1),': ',E.Message);
  end;
end.

