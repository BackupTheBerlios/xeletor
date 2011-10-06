{ Copyright (C) 2011  Mattias Gaertner  mattias@freepascal.org

  Abstract:
    Storage for multiple root directories.
}
unit xdbcentral;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileProcs,
  {$IFDEF Linux}
  inotify,
  {$ENDIF}
  XFileWatch, xdbfiles, xdbutils;

type

  { TXDBCentral }

  TXDBCentral = class
  public
    Roots: TXDBRootDirectories;
    Watch: TXDBFileSystemWatch;
    constructor Create;
    destructor Destroy; override;
    function FindDocument(Path: string;
                ExceptionIfNotFound, CreateIfNotExists: boolean): TXDBDocument;
    function FindDirectory(Path: string;
                ExceptionIfNotFound, CreateIfNotExists: boolean): TXDBDirectory;
    function FindRootWithName(const DBName: string): TXDBRootDirectory;
    function FindRootWithDir(const Filename: string): TXDBRootDirectory;
    function DBPathToFilename(DBPath: string): string;
    function CreateRootDir(const ShortFilename, LongFilename: string): TXDBRootDirectory;
  end;

implementation

{ TXDBCentral }

constructor TXDBCentral.Create;
begin
  Roots:=TXDBRootDirectories.Create('');
  Watch:=TXDBFileSystemWatch.Create;
end;

destructor TXDBCentral.Destroy;
begin
  FreeAndNil(Roots);
  FreeAndNil(Watch);
  inherited Destroy;
end;

function TXDBCentral.FindDocument(Path: string; ExceptionIfNotFound,
  CreateIfNotExists: boolean): TXDBDocument;
var
  Dir: TXDBDirectory;
  aFile: TXDBFile;
  FileName: String;
  p: Integer;
begin
  Result:=nil;
  p:=length(Path);
  while (p>0) and (Path[p]<>'/') do dec(p);
  Dir:=FindDirectory(copy(Path,1,p-1),ExceptionIfNotFound,CreateIfNotExists);
  if Dir=nil then exit;
  Filename:=copy(Path,p+1,length(Path));
  aFile:=Dir.FindFile(Filename);
  if aFile<>nil then begin
    if aFile is TXDBDocument then begin
      Result:=TXDBDocument(aFile);
    end else begin
      if not ExceptionIfNotFound then exit;
      raise Exception.Create('file is directory: '+dbgstr(Path));
    end;
  end else if CreateIfNotExists then begin
    Result:=TXDBDocument.Create(FileName);
    Dir.Add(Result);
  end else begin
    if not ExceptionIfNotFound then exit;
    raise Exception.Create('file not found: '+dbgstr(Path));
  end;
end;

function TXDBCentral.FindDirectory(Path: string; ExceptionIfNotFound,
  CreateIfNotExists: boolean): TXDBDirectory;
var
  Dir: TXDBDirectory;
  FileName: String;
  aFile: TXDBFile;
begin
  Result:=nil;
  if (length(Path)>0) and (Path[1]='/') then
    Delete(Path,1,1);
  Dir:=Roots;
  while Path<>'' do begin
    FileName:=ExtractFirstURLPath(Path);
    aFile:=Dir.FindFile(FileName);
    if aFile<>nil then begin
      if aFile is TXDBDocument then begin
        if not ExceptionIfNotFound then exit;
        raise Exception.Create('[TXDBCentral.FindDirectory] Path is a document: "'+dbgstr(aFile.GetFullFilename)+'"');
      end else if aFile is TXDBDirectory then begin
        Dir:=TXDBDirectory(aFile);
      end else begin
        if not ExceptionIfNotFound then exit;
        raise Exception.Create('[TXDBCentral.FindDirectory] Path is not a directory: "'+dbgstr(aFile.GetFullFilename)+'"');
      end;
    end else if CreateIfNotExists and (Dir<>Roots) then begin
      aFile:=TXDBDirectory.Create(Filename);
      Dir.Add(aFile);
      Dir:=TXDBDirectory(aFile);
    end else begin
      if not ExceptionIfNotFound then exit;
      if Dir=Roots then
        raise Exception.Create('[TXDBCentral.FindDirectory] DB not found: "'+FileName+'"')
      else
        raise Exception.Create('[TXDBCentral.FindDirectory] Path not found: "'+dbgstr(Dir.GetFullFilename)+PathDelim+FileName+'"');
    end;
  end;
  Result:=Dir;
end;

function TXDBCentral.FindRootWithName(const DBName: string): TXDBRootDirectory;
begin
  Result:=TXDBRootDirectory(Roots.FindFile(DBName));
end;

function TXDBCentral.FindRootWithDir(const Filename: string
  ): TXDBRootDirectory;
begin
  Result:=Roots.FindLongFileName(Filename);
end;

function TXDBCentral.DBPathToFilename(DBPath: string): string;
var
  DBName: String;
  Dir: TXDBRootDirectory;
begin
  if (length(DBPath)>0) and (DBPath[1]='/') then
    Delete(DBPath,1,1);
  DBName:=ExtractFirstURLPath(DBPath);
  if DBName='' then
    Dir:=nil
  else
    Dir:=Roots.FindRoot(DBName);
  if Dir=nil then
    raise Exception.Create('DB not found: '+dbgstr(DBName));
  Result:=AppendPathDelim(Dir.LongFileName)+SetDirSeparators(DBPath);
end;

function TXDBCentral.CreateRootDir(const ShortFilename, LongFilename: string
  ): TXDBRootDirectory;
begin
  Result:=FindRootWithName(ShortFilename);
  if Result<>nil then
    raise Exception.Create('TXDBCentral.CreateRootDir '+ShortFilename+' already exists: '+Result.LongFileName);
  Result:=TXDBRootDirectory.Create(ShortFilename,LongFilename);
  Roots.Add(Result);
end;

end.

