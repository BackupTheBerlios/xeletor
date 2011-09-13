unit xdbprocess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xdbutils, FileProcs,
  process, fphttpclient, laz2_DOM, laz2_XMLRead;

// execute external programs
function RunTool(const Filename: string; Params: TStringList;
  WorkingDirectory: string): TStringList;

// XSLTProc
type
  TXSLTProcFlag = (
    xslpfHTML,  // The input document is an HTML file
    xslpfNoDTDAttr,  // Do not apply default attributes from the document´s DTD
    xslpfNoMkdir, // Refuses to create directories
    xslpfNoNet, // Do not use the Internet to fetch DTDs, entities or documents
    xslpfNoValid, // Skip loading the document´s DTD
    xslpfNoWrite // Refuses to write to any file or resource
    );
  TXSLTProcFlags = set of TXSLTProcFlag;
const
  xslpfDefaultHTML = [xslpfHTML,xslpfNoMkdir,xslpfNoNet,xslpfNoWrite];

function GetDefaultXSLTProcPath: string;
function RunXSLTProc(XSLFilename, XMLFilename: string;
  WorkingDirectory: string  = '';
  Flags: TXSLTProcFlags = xslpfDefaultHTML): TStringList;
procedure RunXSLTProcPipe(XSLFilename: string;
  InputStream, OutputStream: TStream; WorkingDirectory: string = '';
  Flags: TXSLTProcFlags = xslpfDefaultHTML; Params: TStrings = nil);
procedure XSLTProcFlagsToList(const Flags: TXSLTProcFlags; Params: TStrings);
procedure XSLTProcNameValueToParams(NameValues, Params: TStrings);

// download
function DownloadText(const URL: string): TStrings;
procedure DownloadXML(const URL: string; out doc: TXMLDocument);

implementation

function RunTool(const Filename: string; Params: TStringList;
  WorkingDirectory: string): TStringList;
var
  buf: string;
  TheProcess: TProcess;
  OutputLine: String;
  OutLen: Integer;
  LineStart, i: Integer;
begin
  if not FileIsExecutable(Filename) then exit(nil);
  Result:=TStringList.Create;
  try
    TheProcess := TProcess.Create(nil);
    try
      TheProcess.Executable:=Filename;
      TheProcess.Parameters.Assign(Params);
      TheProcess.Options:= [poUsePipes, poStdErrToOutPut];
      TheProcess.ShowWindow := swoHide;
      TheProcess.CurrentDirectory:=UTF8ToSys(WorkingDirectory);
      TheProcess.Execute;
      OutputLine:='';
      SetLength(buf,4096);
      repeat
        if (TheProcess.Output<>nil) then begin
          OutLen:=TheProcess.Output.Read(Buf[1],length(Buf));
        end else
          OutLen:=0;
        LineStart:=1;
        i:=1;
        while i<=OutLen do begin
          if Buf[i] in [#10,#13] then begin
            OutputLine:=OutputLine+copy(Buf,LineStart,i-LineStart);
            Result.Add(OutputLine);
            OutputLine:='';
            if (i<OutLen) and (Buf[i+1] in [#10,#13]) and (Buf[i]<>Buf[i+1])
            then
              inc(i);
            LineStart:=i+1;
          end;
          inc(i);
        end;
        OutputLine:=copy(Buf,LineStart,OutLen-LineStart+1);
      until OutLen=0;
      TheProcess.WaitOnExit;
    finally
      TheProcess.Free;
    end;
  except
    FreeAndNil(Result);
  end;
end;

function GetDefaultXSLTProcPath: string;
begin
  Result:=FindDefaultExecutablePath('xsltproc');
end;

function RunXSLTProc(XSLFilename, XMLFilename: string;
  WorkingDirectory: string  = '';
  Flags: TXSLTProcFlags = xslpfDefaultHTML): TStringList;
var
  Params: TStringList;
begin
  Result:=nil;
  Params:=TStringList.Create;
  try
    XSLTProcFlagsToList(Flags,Params);
    Params.Add(XSLFilename);
    Params.Add(XMLFilename);
    Result:=RunTool(GetDefaultXSLTProcPath,Params,WorkingDirectory);
  finally
    Params.Free;
  end;
end;

procedure RunXSLTProcPipe(XSLFilename: string; InputStream, OutputStream: TStream;
  WorkingDirectory: string = '';
  Flags: TXSLTProcFlags = xslpfDefaultHTML; Params: TStrings = nil);
var
  XSLTProc: String;
  TheProcess: TProcess;
  Buffer: string;
  OutLen: Integer;
begin
  XSLTProc:=GetDefaultXSLTProcPath;
  if not FileIsExecutable(XSLTProc) then
    raise Exception.Create('can not execute xsltproc ('+XSLTProc+')');
  TheProcess:=TProcess.Create(nil);
  try
    TheProcess.Executable:=XSLTProc;
    XSLTProcFlagsToList(Flags,TheProcess.Parameters);
    if Params<>nil then
      TheProcess.Parameters.AddStrings(Params);
    TheProcess.Parameters.Append(XSLFilename);
    TheProcess.Parameters.Append('-'); // use stdin as input
    TheProcess.Options:= [poUsePipes];
    TheProcess.ShowWindow := swoHide;
    TheProcess.CurrentDirectory:=UTF8ToSys(WorkingDirectory);
    // feed the input
    debugln(['RunXSLTProcPipe Execute']);
    TheProcess.Execute;
    debugln(['RunXSLTProcPipe write Input ',InputStream.Size-InputStream.Position]);
    if InputStream<>nil then
      TheProcess.Input.CopyFrom(InputStream,InputStream.Size-InputStream.Position);
    debugln(['RunXSLTProcPipe read ...']);
    // read all output
    SetLength(Buffer,1);
    repeat
      if (TheProcess.Output<>nil) then
        OutLen:=TheProcess.Output.Read(Buffer[1],length(Buffer))
      else
        OutLen:=0;
      debugln(['RunXSLTProcPipe OutLen=',OutLen]);
      if OutLen=0 then break;
      OutputStream.Write(Buffer[1],OutLen);
    until false;
    TheProcess.WaitOnExit;
  finally
    TheProcess.Free;
  end;
end;

procedure XSLTProcFlagsToList(const Flags: TXSLTProcFlags; Params: TStrings);
begin
  if xslpfHTML in Flags then;
    Params.Add('--html');
  if xslpfNoDTDAttr in Flags then;
    Params.Add('--nodtdattr');
  if xslpfNoMkdir in Flags then;
    Params.Add('--nomkdir');
  if xslpfNoNet in Flags then;
    Params.Add('--nonet');
  if xslpfNoValid in Flags then;
    Params.Add('--novalid');
  if xslpfNoWrite in Flags then;
    Params.Add('--nowrite');
end;

procedure XSLTProcNameValueToParams(NameValues, Params: TStrings);
var
  i: Integer;
  Value: String;
  Name: String;
begin
  if NameValues=nil then exit;
  for i:=0 to NameValues.Count-1 do begin
    Name:=NameValues.Names[i];
    if Name='' then exit;
    Value:=NameValues.ValueFromIndex[i];
    Params.Add('--param');
    Params.Add(Name);
    Params.Add(Value);
  end;
end;

function DownloadText(const URL: string): TStrings;
var
  client: TFPHTTPClient;
  doc: TStringList;
begin
  Result:=nil;
  doc:=TStringList.Create;
  client:=TFPHTTPClient.Create(nil);
  try
    client.Get(URL,doc);
    Result:=doc;
    doc:=nil;
  finally
    doc.Free;
    client.Free;
  end;
end;

procedure DownloadXML(const URL: string; out doc: TXMLDocument);
var
  client: TFPHTTPClient;
  ms: TMemoryStream;
begin
  doc:=nil;
  client:=TFPHTTPClient.Create(nil);
  ms:=TMemoryStream.Create;
  try
    client.Get(URL,ms);
    ms.Position:=0;
    ReadXMLFile(doc,ms);
  finally
    client.Free;
    ms.Free;
  end;
end;

end.
