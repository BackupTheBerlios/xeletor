unit xdblog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileProcs;

type

  { TXDBLog }

  TXDBLog = class
  private
    FLogCritSect: TRTLCriticalSection;
    fStdOutLog: TStringList;
    fLastWatchDogMsg: TDateTime;
  public
    Verbose: boolean;
    Quiet: boolean;
    LogFile: string;
    WatchDogIntervalInSec: integer;// write an alive message to log
    constructor Create;
    destructor Destroy; override;
    procedure WatchDog;
    procedure FlushStdOutLog;
    procedure Log(EventType: TEventType; const Msg: String); overload;
    procedure Log(EventType: TEventType; Msg: array of const); overload;
  end;

implementation

{ TXDBLog }

constructor TXDBLog.Create;
begin
  fStdOutLog:=TStringList.Create;
  WatchDogIntervalInSec:=60;
  InitCriticalSection(FLogCritSect);
end;

destructor TXDBLog.Destroy;
begin
  inherited Destroy;
  DoneCriticalsection(FLogCritSect);
  FreeAndNil(fStdOutLog);
end;

procedure TXDBLog.WatchDog;
var
  n: TDateTime;
begin
  n:=Now;
  if Abs(fLastWatchDogMsg-n)*86400>=WatchDogIntervalInSec then begin
    fLastWatchDogMsg:=n;
    Log(etInfo,'WatchDog');
    FlushStdOutLog;
  end;
end;

procedure TXDBLog.FlushStdOutLog;
begin
  EnterCriticalsection(FLogCritSect);
  try
    dbgout(fStdOutLog.Text);
    fStdOutLog.Clear;
  finally
    LeaveCriticalsection(FLogCritSect);
  end;
end;

procedure TXDBLog.Log(EventType: TEventType; const Msg: String);
const
  EventNames: array[TEventType] of string = (
    'Custom','Info','Warning','Error','Debug'
    );
var
  fs: TFileStream;
  s: String;
begin
  s:=FormatDateTime('YYYYMMDD HH:NN:SS',Now)+' '+EventNames[EventType]+': '+Msg;
  EnterCriticalsection(FLogCritSect);
  try
    if (Verbose or (LogFile='')) and (fStdOutLog<>nil) then
      fStdOutLog.Add(s);
    s:=s+LineEnding;
    if LogFile='' then exit;
    try
      if FileExistsUTF8(LogFile) then
        fs:=TFileStream.Create(UTF8ToSys(LogFile),fmOpenWrite)
      else
        fs:=TFileStream.Create(UTF8ToSys(LogFile),fmCreate);
      try
        fs.Seek(0,fsFromEnd);
        fs.Write(s[1],length(s));
      finally
        fs.Free;
      end;
    except
      on E: Exception do begin
        fStdOutLog.Add('TFTPMirror.Log unable to log to file: '+E.Message);
      end;
    end;
  finally
    LeaveCriticalsection(FLogCritSect);
  end;
end;


procedure TXDBLog.Log(EventType: TEventType; Msg: array of const);
begin
  Log(EventType,dbgs(Msg));
end;

end.

