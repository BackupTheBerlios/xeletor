//#!/usr/bin/env instantfpc

{$mode objfpc}{$H+}

uses
  Classes, sysutils, FileProcs, xdbutils, xdbfiles, AVL_Tree;

var
  XMLFilename: String;
  Root: TXDBRootNode;
  Node: TXDBNode;
  NodePath: String;
begin
  if Paramcount<>2 then begin
    writeln('Usage: ',ExtractFileName(ParamStr(0)), ' <xml file> <node path>');
    writeln;
    writeln('Node path: Example: A/B/*/C//D');
    writeln('The search starts below the root node.');
    exit;
  end;
  XMLFilename:=CleanAndExpandFilename(ParamStr(1));
  NodePath:=ParamStrUTF8(2);
  Root:=nil;
  try
    ReadXDBTree(Root,XMLFilename,true);
    Node:=Root.FindChildWithPath(NodePath);
    if Node=nil then
      writeln('Node not found: Path="',NodePath,'"')
    else
      writeln('Found: ',Node.GetPath);
    Root.Free;
  except
    on E: Exception do
      writeln('Error when retrieving ',ParamStr(1),': ',E.Message);
  end;
end.

