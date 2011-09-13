program testinotify1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, sysutils, Linux, syscall;

begin
  syscall_nr_inotify_init;
end.

