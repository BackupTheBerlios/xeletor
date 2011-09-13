//#!/usr/bin/env instantfpc

{$mode objfpc}{$H+}

uses
  Classes, sysutils, FileProcs, laz2_DOM, laz2_XMLRead, fphttpclient, xdbprocess;

var
  doc: TXMLDocument;
begin
  if Paramcount<>1 then begin
    writeln('Usage: ',ExtractFileName(ParamStr(0)), ' <URL of xml file>');
    exit;
  end;
  try
    DownloadXML(ParamStr(1),doc);
    writeln('Root: ',doc.DocumentElement.NodeName);
    doc.Free;
  except
    on E: Exception do
      writeln('Error when retrieving ',ParamStr(1),': ',E.Message);
  end;
end.

