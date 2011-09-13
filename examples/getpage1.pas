// #!/usr/bin/env instantfpc

{$mode objfpc}{$H+}

uses Classes, SysUtils, fphttpclient;

var
  I: Integer;
begin
  if (ParamCount<>2) then
    begin
    writeln('Usage : ',ExtractFileName(ParamStr(0)), ' URL filename');
    Halt(1);
    end;
  With TFPHTTPClient.Create(Nil) do
    try
      Get(ParamStr(1),ParamStr(2));
      Writeln('Response headers:');
      For I:=0 to ResponseHeaders.Count-1 do
        Writeln(ResponseHeaders[i]);
    finally
      Free;
    end;
end.
