{ Copyright (C) 2011  Mattias Gaertner  mattias@freepascal.org

  Abstract:
    Classes for storing xml files, directories, documents and nodes
}
unit xdbhttpserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileProcs, xdbfphttpserver, Sockets;

type
  TXDBHttpServer = class;

  { TXDBHttpServerThread }

  TXDBHttpServerThread = class(TThread)
  public
    Server: TXDBHttpServer;
    constructor Create(aServer: TXDBHttpServer);
    procedure Execute; override;
  end;

  { TXDBHttpServer }

  TXDBHttpServer = class(TFPCustomHttpServer)
  public
    Thread: TXDBHttpServerThread;
    procedure ActivateViaThread;
  published
    Property Active;
    Property Port;
    Property QueueSize;
    Property OnAllowConnect;
    property Threaded;
    Property OnRequest;
  end;

function dbgs(const a: TSockAddr): string; overload;

implementation

function dbgs(const a: TSockAddr): string;
begin
  Result:=dbgs(a.sin_addr.s_bytes[1])+'.'+
          dbgs(a.sin_addr.s_bytes[2])+'.'+
          dbgs(a.sin_addr.s_bytes[3])+'.'+
          dbgs(a.sin_addr.s_bytes[4])+'.'+
          ':'+dbgs(a.sin_port);
end;

procedure TXDBHttpServer.ActivateViaThread;
begin
  TXDBHttpServerThread.Create(Self);
end;

{ TXDBHttpServerThread }

constructor TXDBHttpServerThread.Create(aServer: TXDBHttpServer);
begin
  Server:=aServer;
  Server.Thread:=Self;
  inherited Create(false);
end;

procedure TXDBHttpServerThread.Execute;
begin
  Server.Active:=true;
end;

end.

