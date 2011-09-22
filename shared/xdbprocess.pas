unit xdbprocess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileProcs,
  process, fphttpclient, laz2_DOM, laz2_XMLRead,
  xdbutils, xdbfiles;

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
  xslpfDefaultCreateHTML = [xslpfNoMkdir,xslpfNoNet,xslpfNoWrite];

function GetDefaultXSLTProcPath: string;
function RunXSLTProc(XSLFilename, XMLFilename: string;
  WorkingDirectory: string  = '';
  Flags: TXSLTProcFlags = xslpfDefaultCreateHTML): TStringList;
procedure RunXSLTProcPipe(XSLFilename: string;
  XMLInputStream, OutputStream: TStream; WorkingDirectory: string = '';
  Flags: TXSLTProcFlags = xslpfDefaultCreateHTML; Params: TStrings = nil); overload;
procedure RunXSLTProcPipe(XSLFilename: string;
  XMLInput: TStrings; OutputStream: TStream; WorkingDirectory: string = '';
  Flags: TXSLTProcFlags = xslpfDefaultCreateHTML; Params: TStrings = nil); overload;
procedure XSLTProcFlagsToList(const Flags: TXSLTProcFlags; Params: TStrings);
procedure XSLTProcNameValueToParams(NameValues, Params: TStrings);

// download
function DownloadText(const URL: string): TStrings;
procedure DownloadXML(const URL: string; out doc: TXMLDocument);
procedure DownloadXDB(const URL: string; out Root: TXDBRootNode;
  CombineStrings: boolean = false);

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
  Flags: TXSLTProcFlags = xslpfDefaultCreateHTML): TStringList;
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

procedure RunXSLTProcPipe(XSLFilename: string; XMLInputStream, OutputStream: TStream;
  WorkingDirectory: string = '';
  Flags: TXSLTProcFlags = xslpfDefaultCreateHTML; Params: TStrings = nil);
var
  XSLTProc: String;
  TheProcess: TProcess;
  Buffer: string;
  OutLen: Integer;
  InputClosed: Boolean;
  Code: Integer;
  ErrMsg: String;
begin
  if XMLInputStream=nil then
    raise Exception.Create('RunXSLTProcPipe: missing XMLInputStream');
  if XMLInputStream.Position=XMLInputStream.Size then
    raise Exception.Create('RunXSLTProcPipe: missing XMLInputStream');
  if OutputStream=nil then
    raise Exception.Create('RunXSLTProcPipe: missing OutputStream');
  if XMLInputStream=nil then
    raise Exception.Create('RunXSLTProcPipe: missing XMLInputStream');
  XSLTProc:=GetDefaultXSLTProcPath;
  if not FileIsExecutable(XSLTProc) then
    raise Exception.Create('RunXSLTProcPipe: can not execute xsltproc ('+XSLTProc+')');
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
    // start process
    TheProcess.Execute;
    // read all output
    InputClosed:=false;
    SetLength(Buffer,4096);
    ErrMsg:='';
    while TheProcess.Output<>nil do begin
      // read error
      OutLen:=TheProcess.Stderr.NumBytesAvailable;
      if OutLen>length(Buffer) then
        OutLen:=length(Buffer);
      if OutLen>0 then begin
        OutLen:=TheProcess.Output.Read(Buffer[1],OutLen);
        if OutLen>0 then
          ErrMsg:=ErrMsg+copy(Buffer,1,OutLen);
        continue;
      end;
      // read output
      OutLen:=TheProcess.Output.NumBytesAvailable;
      if OutLen>length(Buffer) then
        OutLen:=length(Buffer);
      if OutLen=0 then begin
        // no output
        if not TheProcess.Running then break;
        if XMLInputStream.Size>XMLInputStream.Position then begin
          // feed input
          OutLen:=XMLInputStream.Read(Buffer[1],length(Buffer));
          if OutLen>0 then
            TheProcess.Input.Write(Buffer[1],OutLen);
        end else if not InputClosed then begin
          InputClosed:=true;
          TheProcess.CloseInput;
        end;
        Sleep(20); // no input, no output => wait a bit
      end else begin
        OutLen:=TheProcess.Output.Read(Buffer[1],OutLen);
        if OutLen>0 then
          OutputStream.Write(Buffer[1],OutLen);
      end;
    end;
    TheProcess.WaitOnExit;
    Code:=TheProcess.ExitStatus;
    if Code<>0 then
      raise Exception.Create('xsltproc failed with exit code '+IntToStr(Code));
  finally
    TheProcess.Free;
  end;
end;

procedure RunXSLTProcPipe(XSLFilename: string; XMLInput: TStrings;
  OutputStream: TStream; WorkingDirectory: string; Flags: TXSLTProcFlags;
  Params: TStrings);
var
  XMLInputStream: TStringStream;
begin
  if XMLInput=nil then
    raise Exception.Create('RunXSLTProcPipe: missing XMLInput');
  XMLInputStream:=TStringStream.Create(XMLInput.Text);
  try
    RunXSLTProcPipe(XSLFilename,XMLInputStream,OutputStream,WorkingDirectory,
                    Flags,Params);
  finally
    XMLInputStream.Free;
  end;
end;

procedure XSLTProcFlagsToList(const Flags: TXSLTProcFlags; Params: TStrings);
begin
  if xslpfHTML in Flags then
    Params.Add('--html');
  if xslpfNoDTDAttr in Flags then
    Params.Add('--nodtdattr');
  if xslpfNoMkdir in Flags then
    Params.Add('--nomkdir');
  if xslpfNoNet in Flags then
    Params.Add('--nonet');
  if xslpfNoValid in Flags then
    Params.Add('--novalid');
  if xslpfNoWrite in Flags then
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

procedure DownloadXDB(const URL: string; out Root: TXDBRootNode;
  CombineStrings: boolean);
var
  doc: TXMLDocument;
begin
  Root:=nil;
  doc:=nil;
  try
    DownloadXML(URL,doc);
    CreateXDBTree(doc,Root,CombineStrings);
  finally
    doc.Free;
  end;
end;

end.

