//#!/usr/bin/env instantfpc

{$mode objfpc}{$H+}

uses
  Classes, sysutils, FileProcs, laz2_DOM, laz2_XMLRead;
var
  Filename: String;
  doc: TXMLDocument;
begin
  if Paramcount<>1 then begin
    writeln('Usage: ',ExtractFileName(ParamStr(0)), ' xml-filename');
    exit;
  end;
  Filename:=TrimAndExpandFilename(ParamStr(1));
  ReadXMLFile(doc,Filename);
  writeln('Root: ',doc.DocumentElement.NodeName);
  doc.Free;
end.

