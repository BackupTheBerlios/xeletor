{ Daemon to search xml files via xpath expressions.

  Copyright (C) 2011  Mattias Gaertner  mattias@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
{
  ToDo:
    Update xml directory:
      - remove deleted files and directories
      - register directory change handler (Linux: inotify)
    Use SingleWriteMultipleRead
    real XPath
    Index:
      simple: node path + sort for attributes
    Multirequests
}
program xeletor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  Classes, SysUtils, CustApp, AVL_Tree, laz2_XMLRead, laz2_DOM, CodeToolManager,
  FileProcs, MTProcs, xdbhttpserver, xdbfphttpserver, xdbutils, xdbfiles,
  xdblog, xdbcentral;

const
  Version = '0.4';
type

  { TXPath }

  TXPath = class
  public
    XPath: string;
    ResultCount: integer;
    Results: TAVLTree;
  end;

  { TXeletorApplication }

  TXeletorApplication = class(TCustomApplication)
  private
    fCritSec: TRTLCriticalSection;
    FSingleThreaded: boolean;
    procedure ErrorRespond(ARequest: TFPHTTPConnectionRequest;
      AResponse: TFPHTTPConnectionResponse; Code: integer;
      Msg: string);
    function GetCaption: string;
    procedure SendGreeting(var AResponse: TFPHTTPConnectionResponse);
    procedure ServerAllowConnect(Sender: TObject; {%H-}ASocket: Longint;
      var Allow: Boolean);
    procedure ServerRequest(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure HandleRequestDoc(Path: string;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure HandleRequestListDocs(Path: string;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse;
      ExtendedFormat: boolean);
    procedure HandleRequestFindDocs(Path: string;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure HandleRequestFindNodes(Path: string;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    // Tests
    procedure TestIndex1;
    procedure TestFindDocs(const NodePath: string);
    procedure TestForAllGrandChildren(const NodePath: string);
  protected
    // parameters, configs
    fParams: TStringList;
    procedure ParamError(const Msg: string);
    function GetParams(Index: Integer): String; override;
    function GetParamCount: Integer; override; // returns maximum index
    procedure InsertOptionsFile(Filename: string; ParamPos: integer);
    procedure ReadConfig;
  protected
    Storage: TXDBCentral;
    Port: Integer;
    procedure DoRun; override;

    // scan files
    procedure GetRootDirectoriesFromParams;
    procedure ReadAllXMLFiles;
    function AddOrReplaceDoc(const DBDir: string; doc: TXDBDocument): TXDBDocument;
    function ReadXMLFile(const DBFilename: string; out doc: TXDBDocument): boolean;
    procedure ParallelReadXMLFile(Index: PtrInt; Data: Pointer;
                                  {%H-}Item: TMultiThreadProcItem);
    function CreateRootDirectory(aName, aPath: string): TXDBRootDirectory;

    // search
    procedure FindNodes(XPath: TXPath);
    procedure FindInAllDocs(aFile: TXDBFile; XPath: TXPath);
    procedure FindInDoc(Doc: TXDBDocument; XPath: TXPath);
    procedure FindInXChildren(XNode: TXDBNode; XPath: TXPath);
  public
    // log
    fLog: TXDBLog;
    procedure DoLog(EventType: TEventType; const Msg: String); override;
    procedure ClientLog(EventType: TEventType; Request: TFPHTTPConnectionRequest;
        Msg: array of const); overload;
    procedure ClientLog(EventType: TEventType; Connection : TFPHTTPConnection;
        Msg: array of const); overload;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp(WithHeader: boolean); virtual;
    procedure BeginReading;
    procedure EndReading;
    procedure BeginWriting;
    procedure EndWriting;

    property SingleThreaded: boolean read FSingleThreaded write FSingleThreaded;
  end;

{ TXeletorApplication }

procedure TXeletorApplication.DoRun;
var
  i: Integer;
  Server: TXDBHttpServer;
begin
  ReadConfig;

  fLog.LogFile:=GetOptionValue('l','logfile');
  Log(etInfo,'starting '+GetCaption+' ...');
  if fLog.Verbose then begin
    for i:=0 to ParamCount do
      Log(etInfo,'Parameter: '+GetParams(i));
  end;

  SingleThreaded:=HasOption('singlethreaded');
  if SingleThreaded then
    ProcThreadPool.MaxThreadCount:=1;
  if HasOption('p','port') then begin
    Port:=StrToIntDef(GetOptionValue('p','port'),-1);
    if (Port<1) or (Port>65535) then
      ParamError('invalid port '+GetOptionValue('p','port'));
  end;
  fLog.WatchDogIntervalInSec:=StrToIntDef(GetOptionValue('watchdoginterval'),
                                                    fLog.WatchDogIntervalInSec);

  if SingleThreaded then
    Log(etInfo,'singlethreaded');
  Log(etInfo,'WatchDogIntervalInSec='+IntToStr(fLog.WatchDogIntervalInSec));

  GetRootDirectoriesFromParams;

  // read all xml files
  ReadAllXMLFiles;

  // test
  //TestIndex1;
  //for i:=0 to 1000 do Sleep(1000);
  //TestFindDocs('//fileDesc');
  //TestForAllGrandChildren('//fileDesc');

  Log(etInfo,'starting on port '+IntToStr(Port));
  fLog.FlushStdOutLog;
  Server:=TXDBHttpServer.Create(nil);
  try
    Server.Port:=Port;
    Server.Threaded:=not SingleThreaded;
    Server.OnAllowConnect:=@ServerAllowConnect;
    Server.OnRequest:=@ServerRequest;
    Server.ActivateViaThread;
    repeat
      Sleep(50);
      fLog.FlushStdOutLog;
      fLog.WatchDog;
    until Terminated;
  finally
    Server.Free;
  end;

  // stop program loop
  Terminate;
end;

procedure TXeletorApplication.GetRootDirectoriesFromParams;
const
  DBDirOpt = '--dbdir=';
var
  i: Integer;
  p: String;
  DBDir: string;
  t: SizeInt;
  DBDirName: String;
begin
  // collect all root directories
  if not HasOption('d','dbdir') then begin
    writeln('ERROR: Missing option -d or --dbdir ');
    writeln('       Option -h for help.');
    Terminate;
    Halt;
  end;

  i:=1;
  while i<=ParamCount do begin
    p:=GetParams(i);
    DBDir:=#0;
    if p='-d' then begin
      inc(i);
      DBDir:=GetParams(i);
    end else if copy(p,1,length(DBDirOpt))=DBDirOpt then begin
      DBDir:=copy(p,length(DBDirOpt)+1,length(p));
    end;
    if DBDir<>#0 then begin
      t:=System.Pos('=',DBDir);
      if t<1 then
        ParamError('dbdir must start with a name: '+DBDir);
      DBDirName:=copy(DBDir,1,t-1);
      DBDir:=copy(DBDir,t+1,length(DBDir));
      if (DBDirName='') or (not IsValidIdent(DBDirName)) then
        ParamError('invalid dbdir name: '+DBDirName);
      DBDir:=TrimAndExpandDirectory(DBDir);
      if not DirPathExists(DBDir) then
        ParamError('dbdir not found: '+DBDir);
      CreateRootDirectory(DBDirName,DBDir);
    end;
    inc(i);
  end;
end;

procedure TXeletorApplication.ServerAllowConnect(Sender: TObject; ASocket: Longint;
  var Allow: Boolean);
begin
  Allow:=true;
end;

procedure TXeletorApplication.ErrorRespond(ARequest: TFPHTTPConnectionRequest;
  AResponse: TFPHTTPConnectionResponse; Code: integer; Msg: string);
var
  ss: TStringStream;
begin
  ClientLog(etError,ARequest,[Code,'-',Msg]);
  //AResponse.ContentType:='text/html';
  ss:=TStringStream.Create('<HTML><BODY>'
    +'<H1>Error: '+dbgs(Code)+'-'+Msg+'</H1>'
    +'<H3>'+GetCaption+'</H3></BODY></HTML>');
  try
    AResponse.ContentType:='text/html';
    AResponse.Code:=Code;
    AResponse.ContentLength:=ss.Size;
    AResponse.ContentStream:=ss;
    AResponse.SendContent;
    AResponse.ContentStream:=nil;
  finally
    ss.Free;
  end;
end;

function TXeletorApplication.GetCaption: string;
begin
  Result:='Xeletor version '+Version;
end;

procedure TXeletorApplication.SendGreeting(var AResponse: TFPHTTPConnectionResponse);
var
  ss: TStringStream;
begin
  ss:=TStringStream.Create('<HTML><BODY>'#13#10
    +'<H1>'+GetCaption+' is running.</H1>'#13#10
    +'<H3>Usage:</H3>'#13#10
    +'<ul>'#13#10

    +'  <li>doc:dbname/filepath<br>'#13#10
    +'Returns the xml document specified by database name and path.<br>'#13#10
    +'Example: doc:NameOfYourDB/dir/test.xml'#13#10
    +'  </li>'#13#10

    +'  <li>listdocs:DocPath'#13#10
    +'Returns an xml document with the list of files matching the DocPath.<br>'#13#10
    +'DocPath:<br>'#13#10
    +'<ul>'#13#10
    +'  <li>? = any character, but /</li>'#13#10
    +'  <li>* = any number of any character, but /</li>'#13#10
    +'  <li>/**/ = any number of any directory</li>'#13#10
    +'  <li>multiple // are treated as one /</li>'#13#10
    +'  <li>() = logical OR divided by pipe |</li>'#13#10
    +'  <li>\ = treat next UTF-8 character as normal character</li>'#13#10
    +'  <li>the first / can be omitted</li>'#13#10
    +'</ul>'#13#10
    +'Example: listdocs:**/*.xml  to list all xml files'#13#10
    +'  </li>'#13#10

    +'  <li>listdocsext:DocPath'#13#10
    +'As listdocs but with extra attributes like fileage.<br>'#13#10
    +'Example: listdocsext:db1/*.xml'#13#10
    +'  </li>'#13#10

    +'  <li>finddocs:doc(DocPath)XPath'#13#10
    +'Returns an xml document with all files matching the path.<br>'#13#10
    +'The XPath of the first matching node is returned as well.<br>'#13#10
    +'Optionally the path can be prepended with a doc(DocPath) specifying a'
    +' DocPath for the directories/files.<br>'#13#10
    +'Example: finddocs:doc(db1)//graphics'#13#10
    +'  </li>'#13#10

    +'  <li>findnodes:doc(DocPath)XPath'#13#10
    +'Returns an xml document with all nodes matching the path.<br>'#13#10
    +'Optionally the path can be prepended with a doc(DocPath) specifying a'
    +' DocPath for the directories/files.<br>'#13#10
    +'Example: finddocs:doc(db1)//graphics'#13#10
    +'  </li>'#13#10

    +'</ul>'#13#10
    +'</BODY></HTML>'#13#10);
  try
    AResponse.ContentType:='text/html';
    AResponse.ContentLength:=ss.Size;
    AResponse.ContentStream:=ss;
    AResponse.SendContent;
    AResponse.ContentStream:=nil;
  finally
    ss.Free;
  end;
end;

procedure TXeletorApplication.ServerRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
{ Called whenever a client created a connection
}
var
  URL : String;
  p: SizeInt;
  Scheme: String;
begin
  URL:=ARequest.Url;
  ClientLog(etInfo,ARequest,['URL: '+URL]);

  // check for test connection and send greeting and help
  if (URL='/') or (URL='') then begin
    SendGreeting(AResponse);
    exit;
  end;
  if URL[1]='/' then System.Delete(URL,1,1);

  // check scheme
  p:=Pos(':',URL);
  Scheme:=lowercase(copy(URL,1,p-1));
  URL:=copy(URL,p+1,length(URL));
  if Scheme='doc' then begin
    HandleRequestDoc(URL,ARequest,AResponse);
  end else if Scheme='listdocs' then begin
    HandleRequestListDocs(URL,ARequest,AResponse,false);
  end else if Scheme='listdocsext' then begin
    HandleRequestListDocs(URL,ARequest,AResponse,true);
  end else if Scheme='finddocs' then begin
    HandleRequestFindDocs(URL,ARequest,AResponse);
  end else if Scheme='findnodes' then begin
    HandleRequestFindNodes(URL,ARequest,AResponse);
  end else begin
    ErrorRespond(ARequest,AResponse,404,'invalid scheme "'+dbgstr(Scheme)+'"');
    exit;
  end;
end;

procedure TXeletorApplication.HandleRequestDoc(Path: string;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
{ Handle requests of the form
  DBName/Path
  or
  /DBName/Path
}
{ $DEFINE VerboseDocRequest}
var
  Doc: TXDBDocument;
  ms: TMemoryStream;
begin
  ms:=TMemoryStream.Create;
  try
    try
      BeginReading;
      try
        Doc:=Storage.FindDocument(Path,true,false);
        Doc.WriteToStream(ms);
        ms.Position:=0;
        {$IFDEF VerboseDocRequest}
        debugln(['TXeletorApplication.HandleRequestDoc ',dbgs(ms)]);
        {$ENDIF}
      finally
        EndReading;
      end;
    except
      on E: Exception do begin
        ErrorRespond(ARequest,AResponse,404,E.Message);
        exit;
      end;
    end;
    AResponse.ContentType:='text/xml';
    AResponse.ContentLength:=ms.Size;
    ClientLog(etInfo,ARequest,['Serving file: "',Path,'". MimeType: ',AResponse.ContentType,' Length=',AResponse.ContentLength]);
    AResponse.ContentStream:=ms;
    AResponse.SendContent;
    AResponse.ContentStream:=Nil;
  finally
    ms.Free;
  end;
end;

procedure TXeletorApplication.HandleRequestListDocs(Path: string;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse; ExtendedFormat: boolean);
{ Handle requests of the form
  DBName/Path
  or
  /DBName/Path
}
{ $DEFINE VerboseListDocsRequest}
var
  ms: TMemoryStream;

  procedure w(Line: string);
  begin
    Line:=Line+#13#10;
    ms.Write(Line[1],length(Line));
  end;

var
  Docs: TFPList;
  i: Integer;
  aFile: TXDBFile;
  s: String;
  Doc: TXDBDocument;
begin
  ms:=TMemoryStream.Create;
  Docs:=TFPList.Create;
  try
    try
      BeginReading;
      try
        Storage.Roots.ListFiles(Path,Docs,[xlfAddFiles]);
        w('<?xml version="1.0" encoding="UTF-8"?>');
        w('<listing path="'+StrToXMLValue(Path)+'">');
        for i:=0 to Docs.Count-1 do begin
          aFile:=TXDBFile(Docs[i]);
          s:='path="'+StrToXMLValue(aFile.GetFullFilename)+'"';
          if ExtendedFormat then begin
            if aFile is TXDBDocument then begin
              Doc:=TXDBDocument(aFile);
              s:=s+' fileage="'+FileAgeToXDBStr(Doc.FileAge)+'"';
              s:=s+' loadage="'+FileAgeToXDBStr(Doc.LoadAge)+'"';
              s:=s+' dbage="'+DateTimeToXDBStr(Doc.DBAge)+'"';
            end;
          end;
          w('<file '+s+'/>');
        end;
        w('</listing>');
        ms.Position:=0;
        {$IFDEF VerboseListDocsRequest}
        debugln(['TXeletorApplication.HandleRequestListDocs ',dbgs(ms)]);
        {$ENDIF}
      finally
        EndReading;
        FreeAndNil(Docs);
      end;
    except
      on E: Exception do begin
        ErrorRespond(ARequest,AResponse,404,E.Message);
        exit;
      end;
    end;
    AResponse.ContentType:='text/xml';
    AResponse.ContentLength:=ms.Size;
    ClientLog(etInfo,ARequest,['Serving listing: "',Path,'". MimeType: ',AResponse.ContentType,' Length=',AResponse.ContentLength]);
    AResponse.ContentStream:=ms;
    AResponse.SendContent;
    AResponse.ContentStream:=Nil;
  finally
    Docs.Free;
    ms.Free;
  end;
end;

procedure TXeletorApplication.HandleRequestFindDocs(Path: string;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  ms: TMemoryStream;

  procedure w(Line: string);
  begin
    Line:=Line+#13#10;
    ms.Write(Line[1],length(Line));
  end;

var
  Nodes: TFPList;
  i: Integer;
  Node: TXDBNode;
begin
  ms:=TMemoryStream.Create;
  Nodes:=TFPList.Create;
  try
    try
      BeginReading;
      try
        Storage.Roots.FindNodes(Path,Nodes,[xfnfFindFirst,xfnfContinueInNextFile]);
        w('<?xml version="1.0" encoding="UTF-8"?>');
        w('<nodes path="'+StrToXMLValue(Path)+'">');
        for i:=0 to Nodes.Count-1 do begin
          Node:=TXDBNode(Nodes[i]);
          w('<node file="'+Node.GetFullFilename+'" xpath="'+Node.GetPath+'"/>');
        end;
        w('</nodes>');
        ms.Position:=0;
      finally
        EndReading;
        FreeAndNil(Nodes);
      end;
    except
      on E: Exception do begin
        ErrorRespond(ARequest,AResponse,404,E.Message);
        exit;
      end;
    end;
    AResponse.ContentType:='text/xml';
    AResponse.ContentLength:=ms.Size;
    ClientLog(etInfo,ARequest,['Serving listing: "',Path,'". MimeType: ',AResponse.ContentType,' Length=',AResponse.ContentLength]);
    AResponse.ContentStream:=ms;
    AResponse.SendContent;
    AResponse.ContentStream:=Nil;
  finally
    Nodes.Free;
    ms.Free;
  end;
end;

procedure TXeletorApplication.HandleRequestFindNodes(Path: string;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  ms: TMemoryStream;

  procedure w(Line: string);
  begin
    Line:=Line+#13#10;
    ms.Write(Line[1],length(Line));
  end;

var
  Nodes: TFPList;
  i: Integer;
  Node: TXDBNode;
  CurRoot: TXDBNode;
  LastRoot: TXDBNode;
begin
  ms:=TMemoryStream.Create;
  Nodes:=TFPList.Create;
  try
    try
      BeginReading;
      try
        Storage.Roots.FindNodes(Path,Nodes);
        w('<?xml version="1.0" encoding="UTF-8"?>');
        w('<nodes path="'+StrToXMLValue(Path)+'">');
        LastRoot:=nil;
        for i:=0 to Nodes.Count-1 do begin
          Node:=TXDBNode(Nodes[i]);
          CurRoot:=Node.GetRoot;
          if CurRoot<>LastRoot then begin
            if LastRoot<>nil then
              w('  </file>');
            w('  <file docpath="'+CurRoot.GetFullFilename+'">');
            LastRoot:=CurRoot;
          end;
          w('    <node xpath="'+Node.GetPath+'">');
          Node.WriteToStream(ms,3);
          w('    </node>');
        end;
        if LastRoot<>nil then
          w('  </file>');
        w('</nodes>');
        ms.Position:=0;
      finally
        EndReading;
        FreeAndNil(Nodes);
      end;
    except
      on E: Exception do begin
        ErrorRespond(ARequest,AResponse,404,E.Message);
        exit;
      end;
    end;
    AResponse.ContentType:='text/xml';
    AResponse.ContentLength:=ms.Size;
    ClientLog(etInfo,ARequest,['Serving listing: "',Path,'". MimeType: ',AResponse.ContentType,' Length=',AResponse.ContentLength]);
    AResponse.ContentStream:=ms;
    AResponse.SendContent;
    AResponse.ContentStream:=Nil;
  finally
    Nodes.Free;
    ms.Free;
  end;
end;

procedure TXeletorApplication.TestIndex1;
var
  SearchItem: TXPathIndexItem;
  XIndex: TAVLTree;
  XPath: TXPath;
  i: Integer;
begin
  XPath:=TXPath.Create;
  BeginReading;
  SearchItem:=nil;
  try
    XPath.XPath:='graphic';
    XIndex:=TAVLTree.Create(@CompareXPIIStr);
    XPath.Results:=XIndex;
    FindNodes(XPath);
    debugln(['TXeletorApplication.DoRun ResultCount=', XPath.ResultCount, '=', XIndex.Count
      ]);
    SearchItem:=TXPathIndexItem.Create;
    SearchItem.Key:='BOOK-stoschgemmen-0662_159958';
    for i:=0 to 0 do begin
      if XIndex.Find(SearchItem)=nil then debugln(['TXeletorApplication.DoRun missing ',
        SearchItem.Key]);
    end;
  finally
    SearchItem.Free;
    XIndex.FreeAndClear;
    XIndex.Free;
    XPath.Free;
    EndReading;
  end;
end;

procedure TXeletorApplication.TestFindDocs(const NodePath: string);
var
  Nodes: TFPList;
  i: Integer;
  Node: TXDBNode;
begin
  debugln(['TXeletorApplication.TestFindDocs START ',NodePath]);
  Nodes:=TFPList.Create;
  try
    Storage.Roots.FindNodes(NodePath,Nodes);
    for i:=0 to Nodes.Count-1 do begin
      Node:=TXDBNode(Nodes[i]);
      debugln(['TXeletorApplication.TestFindDocs ',i,' file="'+Node.GetFullFilename+'" xpath="'+Node.GetPath+'"']);
    end;
  finally
    Nodes.Free;
  end;
  debugln(['TXeletorApplication.TestFindDocs END']);
end;

procedure TXeletorApplication.TestForAllGrandChildren(const NodePath: string);
var
  Node: TXDBNode;
  Child: TXDBNode;
begin
  debugln(['TXeletorApplication.TestForAllGrandChildren START NodePath=',NodePath]);
  Node:=Storage.Roots.FindFirstNode(NodePath,true);
  for Child in Node.EnumerateAllChildren do
    writeln(Child.GetPath);
  debugln(['TXeletorApplication.TestForAllGrandChildren END']);
end;

procedure TXeletorApplication.ParamError(const Msg: string);
begin
  writeln('Error: ',Msg);
  writeln;
  WriteHelp(false);
  Log(etError,'TFTPMirror.ParamError '+Msg);
  Halt;
end;

function TXeletorApplication.GetParams(Index: Integer): String;
begin
  Result:=fParams[Index];
end;

function TXeletorApplication.GetParamCount: Integer;
begin
  Result:=fParams.Count-1;
end;

procedure TXeletorApplication.InsertOptionsFile(Filename: string; ParamPos: integer);
var
  sl: TStringList;
  i: Integer;
  s: String;
begin
  if not FileExistsUTF8(Filename) then
    ParamError('Config file not found: '+Filename);

  sl:=TStringList.Create;
  try
    sl.LoadFromFile(Filename);
    for i:=0 to sl.Count-1 do begin
      s:=Trim(sl[i]);
      if (s='') or (s[1]='#') then continue;
      fParams.Insert(ParamPos,s);
      inc(ParamPos);
    end;
  finally
    sl.Free;
  end;
end;

procedure TXeletorApplication.ReadConfig;
const
  ShortOpts = 'hc:l:d:p:vqV';
  LongOpts = 'help config: log: dbdir: watchdoginterval: port: singlethreaded verbose quiet version';
  ConfigOpt = '--config=';
var
  LongOptions: TStrings;
  i: Integer;
  p: String;

  procedure CheckOpts;
  var
    Opts,NonOpts: TStrings;
    ErrorMsg: String;
    i: Integer;
  begin
    Opts:=TStringList.Create;
    NonOpts:=TStringList.Create;
    try
      ErrorMsg:=CheckOptions(ShortOpts,LongOptions,Opts,NonOpts);
      if ErrorMsg<>'' then begin
        ShowException(Exception.Create(ErrorMsg));
        Halt;
      end;
      for i:=0 to NonOpts.Count-1 do
        if NonOpts[i]<>'' then
          ParamError('invalid parameter "'+NonOpts[i]+'"');
    finally
      Opts.Free;
      NonOpts.Free;
    end;
    fLog.Verbose:=HasOption('V','verbose');
    fLog.Quiet:=HasOption('q','quiet');
  end;

begin
  LongOptions:=StringToList(LongOpts);
  try
    CheckOpts;

    // parse parameters
    if HasOption('h','help') then begin
      WriteHelp(true);
      Halt;
    end;

    // parse parameters
    if HasOption('v','version') then begin
      writeln(Version);
      Halt;
    end;

    i:=1;
    while i<=ParamCount do begin
      p:=GetParams(i);
      //debugln(['TXeletorApplication.ReadConfig ',i,'/',ParamCount,' ',p]);
      if p='-c' then begin
        inc(i);
        InsertOptionsFile(GetParams(i),i+1);
        CheckOpts;
      end else if copy(p,1,length(ConfigOpt))=ConfigOpt then begin
        p:=copy(p,length(ConfigOpt)+1,length(p));
        InsertOptionsFile(p,i+1);
        CheckOpts;
      end;
      inc(i);
    end;
  finally
    LongOptions.Free;
  end;
end;

procedure TXeletorApplication.ReadAllXMLFiles;

  procedure GatherFiles(const DiskDir, DBDir: string; Files: TStringList);
  var
    FileInfo: TSearchRec;
    aPath: String;
  begin
    aPath:=AppendPathDelim(DiskDir);
    if FindFirstUTF8(aPath+FileMask,faAnyFile,FileInfo)=0 then begin
      repeat
        if (FileInfo.Name='') or (FileInfo.Name[1]='.') then
          continue;
        if (faDirectory and FileInfo.Attr)>0 then begin
          GatherFiles(aPath+FileInfo.Name,DBDir+'/'+FileInfo.Name,Files);
        end else if CompareFileExt(FileInfo.Name,'xml',false)=0 then begin
          Files.Add(DBDir+'/'+FileInfo.Name);
        end;
      until FindNextUTF8(FileInfo)<>0;
    end;
    FindCloseUTF8(FileInfo);
  end;

var
  DBFiles: TStringList;
  RootDir: TXDBRootDirectory;
  DBFile: TXDBFile;
begin
  DBFiles:=TStringList.Create;
  try
    // search all root directories
    for DBFile in Storage.Roots do begin
      RootDir:=DBFile as TXDBRootDirectory;
      fLog.Log(etInfo,['scanning directory ',RootDir.LongFileName,' ...']);
      fLog.FlushStdOutLog;
      GatherFiles(RootDir.LongFileName,RootDir.Filename,DBFiles);
    end;

    fLog.Log(etInfo,['parsing ',DBFiles.Count,' files with ',ProcThreadPool.MaxThreadCount,' threads ...']);
    fLog.FlushStdOutLog;
    ProcThreadPool.DoParallel(@ParallelReadXMLFile,0,DBFiles.Count-1,DBFiles);
  finally
    DBFiles.Free;
  end;
end;

function TXeletorApplication.AddOrReplaceDoc(const DBDir: string; doc: TXDBDocument
  ): TXDBDocument;
var
  Dir: TXDBDirectory;
  aFile: TXDBFile;
begin
  Result:=nil;
  BeginWriting;
  try
    Dir:=Storage.FindDirectory(DBDir,true,true);
    aFile:=Dir.FindFile(doc.Filename);
    if aFile=nil then begin
      Dir.Add(doc);
      doc.DBAge:=Now;
      Result:=doc;
    end else begin
      if aFile is TXDBDirectory then begin
        raise Exception.Create('[TXeletorApplication.AddOrReplace] file is directory: '+aFile.GetFullFilename);
      end else if aFile is TXDBFile then begin
        raise Exception.Create('[TXeletorApplication.AddOrReplace] ToDo: replace file: '+aFile.GetFullFilename);
      end else begin
        raise Exception.Create('[TXeletorApplication.AddOrReplace] file '+aFile.GetFullFilename+' is '+aFile.ClassName);
      end;
    end;
  finally
    EndWriting;
  end;
end;

function TXeletorApplication.ReadXMLFile(const DBFilename: string; out doc: TXDBDocument
  ): boolean;
// DBFilename = DBName+DBPath
var
  ms: TMemoryStream;
  XMLDoc: TXMLDocument;
  Filename: String;
  p: SizeInt;
  NewDoc: TXDBDocument;
begin
  Result:=false;
  doc:=nil;
  try
    BeginReading;
    try
      Filename:=Storage.DBPathToFilename(DBFilename);
    finally
      EndReading;
    end;
  except
    on E: Exception do begin
      Log(etError,'[TXeletorApplication.ReadXMLFile] invalid DBFilename "'+DBFilename+'": '+E.Message);
      exit;
    end;
  end;
  //debugln(['TXeletorApplication.ReadXMLFile Filename=',Filename]);

  try
    XMLDoc:=nil;
    ms:=nil;
    NewDoc:=nil;
    try
      // load file into memory
      ms:=TMemoryStream.Create;
      try
        ms.LoadFromFile(Filename);
      except
        on E: Exception do begin
          fLog.Log(etError,['[TXeletorApplication.ReadXMLFile] ','unable to read file "'+Filename+'": '+E.Message]);
          exit;
        end;
      end;
      // parse xml
      ms.Position:=0;
      try
        laz2_XMLRead.ReadXMLFile(XMLDoc,ms);
      except
        on E: Exception do begin
          fLog.Log(etError,['[TXeletorApplication.ReadXMLFile] ','Error parsing file "'+Filename+'": '+E.Message]);
          exit;
        end;
      end;
      // convert xml doc to xdb doc
      p:=length(DBFilename);
      while (p>0) and (DBFilename[p]<>'/') do dec(p);
      NewDoc:=TXDBDocument.Create(copy(DBFilename,p+1,length(DBFilename)));
      NewDoc.XMLDoc:=XMLDoc;
      XMLDoc:=nil;
      NewDoc.CreateTreeFromXML;
      NewDoc.ClearXMLDoc;
      // add/replace to storage
      NewDoc.FileAge:=FileAge(Filename);
      NewDoc.LoadAge:=NewDoc.FileAge;
      doc:=AddOrReplaceDoc(copy(DBFilename,1,p-1),NewDoc);
      // success
      NewDoc:=nil;
      Result:=true;
    finally
      NewDoc.Free;
      ms.Free;
      XMLDoc.Free;
    end;
  except
    on E: Exception do begin
      Log(etError,'[TXeletorApplication.ReadXMLFile] DBFilename="'+DBFilename+'": '+E.Message);
      exit;
    end;
  end;
end;

procedure TXeletorApplication.ParallelReadXMLFile(Index: PtrInt; Data: Pointer;
  Item: TMultiThreadProcItem);
var
  DBFilename: String;
  doc: TXDBDocument;
begin
  DBFilename:=TStringList(Data)[Index];
  ReadXMLFile(DBFilename,doc);
end;

function TXeletorApplication.CreateRootDirectory(aName, aPath: string): TXDBRootDirectory;
var
  Dir: TXDBRootDirectory;
begin
  Result:=nil;
  Dir:=Storage.FindRootWithName(aName);
  if Dir<>nil then
    raise Exception.Create('There is already a directory with the name "'+aName+'": '+Dir.GetFullFilename);
  Result:=TXDBRootDirectory.Create(aName,ChompPathDelim(aPath));
  Storage.Roots.Add(Result);
end;

procedure TXeletorApplication.FindNodes(XPath: TXPath);
var
  aFile: TXDBFile;
begin
  for aFile in Storage.Roots do
    FindInAllDocs(aFile,XPath);
end;

procedure TXeletorApplication.FindInAllDocs(aFile: TXDBFile; XPath: TXPath);
var
  Dir: TXDBDirectory;
  Doc: TXDBDocument;
  SubFile: TXDBFile;
begin
  if aFile is TXDBDirectory then begin
    Dir:=TXDBDirectory(aFile);
    for SubFile in Dir do
      FindInAllDocs(SubFile,XPath);
  end else if aFile is TXDBDocument then begin
    Doc:=TXDBDocument(aFile);
    FindInDoc(Doc,XPath);
  end;
end;

procedure TXeletorApplication.FindInDoc(Doc: TXDBDocument; XPath: TXPath);
begin
  if Doc.Root=nil then exit;
  FindInXChildren(Doc.Root,XPath);
end;

procedure TXeletorApplication.FindInXChildren(XNode: TXDBNode; XPath: TXPath
  );
var
  ParentXNode: TXDBTreeNode;
  i: Integer;
  NodeWithAttr: TXDBNodeWithAttributes;
  NewIndexItem: TXPathIndexItem;
begin
  if XNode is TXDBNodeWithAttributes then begin
    NodeWithAttr:=TXDBNodeWithAttributes(XNode);
    if NodeWithAttr.Name=XPath.XPath then begin
      inc(XPath.ResultCount);
      if XPath.Results<>nil then begin
        NewIndexItem:=TXPathIndexItem.Create;
        NewIndexItem.Node:=NodeWithAttr;
        NewIndexItem.Key:=NodeWithAttr.GetAttribute('xml:id');
        XPath.Results.Add(NewIndexItem);
      end;
    end;
    if NodeWithAttr is TXDBTreeNode then begin
      ParentXNode:=TXDBTreeNode(NodeWithAttr);
      for i:=0 to ParentXNode.ChildCount-1 do begin
        FindInXChildren(ParentXNode.Children[i],XPath);
      end;
    end;
  end;
end;

procedure TXeletorApplication.DoLog(EventType: TEventType; const Msg: String);
begin
  fLog.Log(EventType, Msg);
end;

procedure TXeletorApplication.ClientLog(EventType: TEventType;
  Request: TFPHTTPConnectionRequest; Msg: array of const);
begin
  if Request=nil then
    Log(EventType,'no client '+DbgS(Msg))
  else
    ClientLog(EventType,Request.Connection,Msg);
end;

procedure TXeletorApplication.ClientLog(EventType: TEventType;
  Connection: TFPHTTPConnection; Msg: array of const);
var
  s: String;
begin
  s:='';
  if Connection=nil then
    s:='no connection'
  else if Connection.Socket=nil then
    s:='no socket'
  else begin
    s:=dbgs(Connection.Socket.RemoteAddress);
  end;
  Log(EventType,s+' '+DbgS(Msg));
end;

constructor TXeletorApplication.Create(TheOwner: TComponent);
var
  i: Integer;
begin
  fLog:=TXDBLog.Create;
  fParams:=TStringList.Create;
  for i:=0 to System.ParamCount do
    fParams.Add(System.ParamStr(i));
  inherited Create(TheOwner);
  InitCriticalSection(fCritSec);
  StopOnException:=True;
  Storage:=TXDBCentral.Create;
  Port:=5588;
end;

destructor TXeletorApplication.Destroy;
begin
  BeginWriting;
  try
    FreeAndNil(Storage);
  finally
    EndWriting;
  end;
  Log(etInfo,'Daemon stopped.');
  inherited Destroy;
  FreeAndNil(fLog);
  FreeAndNil(fParams);
  DoneCriticalsection(fCritSec);
end;

procedure TXeletorApplication.WriteHelp(WithHeader: boolean);
begin
  writeln(GetCaption);
  writeln;
  if WithHeader then begin
    writeln('Xeletor is a lightweight XML database.');
    writeln('It reads directories of xml files into memory and monitors them');
    writeln('to update automatically when files change on disk.');
    writeln('It provides a simple webserver for various types of queries.');
    writeln('To get some help about the supported queries download the default');
    writeln('webpage at http://localhost:',Port);
    writeln;
    writeln('Official homepage: http://developer.berlios.de/projects/xeletor/');
    writeln;
    writeln;
  end;
  writeln('Usage: ',ExeName,' -d db1=/path/to/your/xml/files');
  writeln;
  writeln('  -h');
  writeln('  --help          : write this help');
  writeln('  -c <configfile> : file with more options. One line per option.');
  writeln('                    Lines beginning with # are comments.');
  writeln('  -l <logfile>');
  writeln('  --logfile=<logfile>');
  writeln('  -d <name=dbdir>');
  writeln('  --dbdir=<name=dbdir> : directory of all your xml files, including sub directories');
  writeln('                         Can be given multiple times.');
  writeln('  --singlethreaded : run with least amount of threads');
  writeln('  -p <port>');
  writeln('  --port=<port>   : TCP port, default: ',Port);
  writeln('  --watchdoginterval=<in seconds> : write an alive message to the log');
  writeln('  -v');
  writeln('  --version       : write version and exit');
  writeln('  -V');
  writeln('  -verbose        : write what is going on');
  writeln('  -q');
  writeln('  -quiet          : write less information');
end;

procedure TXeletorApplication.BeginReading;
begin
  System.EnterCriticalsection(fCritSec);
end;

procedure TXeletorApplication.EndReading;
begin
  System.LeaveCriticalsection(fCritSec);
end;

procedure TXeletorApplication.BeginWriting;
begin
  System.EnterCriticalsection(fCritSec);
end;

procedure TXeletorApplication.EndWriting;
begin
  System.LeaveCriticalsection(fCritSec);
end;

var
  Application: TXeletorApplication;

{$R *.res}

begin
  Application:=TXeletorApplication.Create(nil);
  Application.Run;
  Application.Free;
end.

