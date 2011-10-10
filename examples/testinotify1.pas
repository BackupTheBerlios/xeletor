program testinotify1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, sysutils, LazFileUtils, Linux, syscall, XFileWatch;

var
  Watcher: TXDBFileSystemWatch;
  Dir: String;
begin
  Dir:=TrimAndExpandDirectory(ParamStr(1));
  if not DirectoryExistsUTF8(Dir) then
    raise Exception.Create('directory not found: '+Dir);
  Watcher:=TXDBFileSystemWatch.Create;
  try
    Watcher.WatchDirectory(Dir);

  finally
    Watcher.Free;
  end;
end.

