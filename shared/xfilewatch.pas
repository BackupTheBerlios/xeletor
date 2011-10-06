{ Copyright (C) 2011  Mattias Gaertner  mattias@freepascal.org

  Abstract:
    File change notification.
}
unit XFileWatch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, inotify, LazFileUtils, XDBUtils, AVL_Tree;

type

  { TXDBDirectoryWatch }

  TXDBDirectoryWatch = class
  private
    FFilename: string;
  public
    constructor Create(const aFilename: string);
    destructor Destroy; override;
    property Filename: string read FFilename;
  end;

  { TXDBFileSystemWatch }

  TXDBFileSystemWatch = class
  private
    fDirectories: TAVLTree; // tree of TXDBDirectoryWatch sorted for filename
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function NormalizeFilename(const Filename: string): string;
    function WatchDirectory(Filename: string): TXDBDirectoryWatch;
    function GetDirectory(const Filename: string): TXDBDirectoryWatch;
    function DirectoryCount: integer;
  end;

function CompareDirectoryWatches(DirWatch1, DirWatch2: Pointer): integer;
function CompareStringWithDirectoryWatch(Filename, DirWatch: Pointer): integer;

implementation

function CompareDirectoryWatches(DirWatch1, DirWatch2: Pointer): integer;
var
  Dir1: TXDBDirectoryWatch absolute DirWatch1;
  Dir2: TXDBDirectoryWatch absolute DirWatch2;
begin
  Result:=CompareFilenames(Dir1.Filename,Dir2.Filename);
end;

function CompareStringWithDirectoryWatch(Filename, DirWatch: Pointer): integer;
var
  Dir: TXDBDirectoryWatch absolute DirWatch;
begin
  Result:=CompareFilenames(AnsiString(Filename),Dir.Filename);
end;

{ TXDBFileSystemWatch }

constructor TXDBFileSystemWatch.Create;
begin
  fDirectories:=TAVLTree.Create(@CompareDirectoryWatches);
end;

destructor TXDBFileSystemWatch.Destroy;
begin
  Clear;
  FreeAndNil(fDirectories);
  inherited Destroy;
end;

procedure TXDBFileSystemWatch.Clear;
begin
  fDirectories.FreeAndClear;
end;

function TXDBFileSystemWatch.NormalizeFilename(const Filename: string): string;
begin
  Result:=ChompPathDelim(TrimFilename(Filename));
end;

function TXDBFileSystemWatch.WatchDirectory(Filename: string
  ): TXDBDirectoryWatch;
begin
  Filename:=NormalizeFilename(Filename);
  Result:=GetDirectory(Filename);
  if Result<>nil then exit;
  Result:=TXDBDirectoryWatch.Create(Filename);
  fDirectories.Add(Result);
end;

function TXDBFileSystemWatch.GetDirectory(const Filename: string
  ): TXDBDirectoryWatch;
var
  Node: TAVLTreeNode;
begin
  Result:=nil;
  if Filename='' then exit;
  Node:=fDirectories.FindKey(PChar(Filename),@CompareStringWithDirectoryWatch);
  if Node=nil then exit;
  Result:=TXDBDirectoryWatch(Node.Data);
end;

function TXDBFileSystemWatch.DirectoryCount: integer;
begin
  Result:=fDirectories.Count;
end;

{ TXDBDirectoryWatch }

constructor TXDBDirectoryWatch.Create(const aFilename: string);
begin
  FFilename:=aFilename;
end;

destructor TXDBDirectoryWatch.Destroy;
begin
  inherited Destroy;
end;

end.

