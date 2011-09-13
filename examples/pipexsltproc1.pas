//#!/usr/bin/env instantfpc

{$note ToDo: pipe hangs}

{$mode objfpc}{$H+}

uses
  Classes, sysutils, FileProcs, xdbprocess;

var
  XSLFilename: String;
  XMLFilename: String;
  XML: TFileStream;
  HTML: TStringStream;
begin
  if Paramcount<>2 then begin
    writeln('Usage: ',ExtractFileName(ParamStr(0)), ' xslfilename xmlfilename');
    exit;
  end;
  XSLFilename:=CleanAndExpandFilename(ParamStr(1));
  XMLFilename:=CleanAndExpandFilename(ParamStr(2));
  XML:=nil;
  try
    XML:=TFileStream.Create(XMLFilename,fmOpenRead+fmShareDenyWrite);
    HTML:=TStringStream.Create('');
    try
      RunXSLTProcPipe(XSLFilename,XML,HTML);
      HTML.Position:=0;
      writeln(HTML.DataString);
    finally
      HTML.Free;
      XML.Free;
    end;
  except
    on E: Exception do
      writeln('Error when converting xml file ',XMLFilename,' via xsl file ',XSLFilename,': ',E.Message);
  end;
end.

