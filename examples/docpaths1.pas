program docpaths1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, sysutils, xdbfiles, xdbcentral, FileProcs;

var
  Storage: TXDBCentral;

procedure List(const DocPath: string);
var
  FileList: TFPList;
  i: Integer;
begin
  FileList:=TFPList.Create;
  try
    Storage.Roots.ListFiles(DocPath,FileList,xlfAll);
    debugln(['List DocPath="',DocPath,'" Results=',FileList.Count]);
    for i:=0 to FileList.Count-1 do begin
      debugln(['  ',i,': ',TXDBFile(FileList[i]).GetFullFilename]);
    end;
  finally
    FileList.Free;
  end;
end;

begin
  Storage:=TXDBCentral.Create;
  try
    Storage.CreateRootDir('Test1',GetCurrentDir);
    Storage.FindDocument('Test1/FirstDir/test1.xml',false,true);
    Storage.FindDocument('Test1/FirstDir/test2.xml',false,true);
    Storage.FindDocument('Test1/SecondDir/test2.xml',false,true);
    Storage.FindDocument('Test1/SecondDir/test3.xml',false,true);
    //List('Test1');
    //List('Test?');
    //List('Test?/*Dir');
    //List('Test?/*/test2.xml');
    //List('Test?/*Dir/*2.xml');
    //List('Test?/*Dir/test(1|2).xml');
    //List('**/test2.xml');
    List('/**/test2.xml');
  finally
    Storage.Free;
  end;
end.

