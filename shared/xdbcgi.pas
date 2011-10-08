unit xdbcgi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HTTPDefs, LazFileUtils;

function QueryParams: TStrings;
function GetQueryParam(const Name: string): string;

function GetRightFilePath(Filename: string; Index: integer = 1): string;

implementation

var
  FQueryParams: TStringList = nil;

procedure ParseQueryString;
var
  query: String;
  p: Integer;
  StartPos: Integer;
begin
  if FQueryParams<>nil then exit;
  FQueryParams:=TStringList.Create;
  query:=GetEnvironmentVariable('QUERY_STRING');
  if query='' then exit;
  p:=1;
  while p<=length(query) do begin
    while (p<=length(query)) and (query[p]='&') do inc(p);
    StartPos:=p;
    while (p<=length(query)) and (query[p]<>'&') do inc(p);
    if p>StartPos then
      FQueryParams.Add(HTTPDecode(copy(query,StartPos,p-StartPos)));
  end;
end;

function QueryParams: TStrings;
begin
  if FQueryParams=nil then
    ParseQueryString;
  Result:=FQueryParams;
end;

function GetQueryParam(const Name: string): string;
begin
  Result:=QueryParams.Values[Name];
end;

function GetRightFilePath(Filename: string; Index: integer = 1): string;
{ returns the Index-th directory at the end, the last part aka file name is Index 1
  for example: /docpath/file
    file is Index 1, docpath is Index 2
}
begin
  while Index>1 do begin
    Filename:=ChompPathDelim(ExtractFilePath(Filename));
    dec(Index);
  end;
  Result:=ExtractFileName(Filename);
end;

finalization
  FreeAndNil(FQueryParams);

end.

