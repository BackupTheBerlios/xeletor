//#!/usr/bin/instantfpc

{$mode objfpc}{$H+}

uses Classes, SysUtils, FileProcs, xdbprocess;

var
  HTML: TStringList;
begin
  HTML:=RunXSLTProc('runxsltproc1.xsl','../../arachne/tei/BOOK-ZID853911/transcription.xml');
  if HTML=nil then begin
    writeln('RunXSLTProc failed');
    Halt;
  end;
  writeln(HTML.Text);
end.
