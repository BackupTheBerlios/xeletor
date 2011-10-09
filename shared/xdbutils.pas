{ Copyright (C) 2011  Mattias Gaertner  mattias@freepascal.org

  Abstract:
    Basic classes and functions.
}
unit XDBUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AVL_Tree, CodeToolsStructs, FileProcs;

type
  PFPList = ^TFPList;

  { TXDBAVLTreeNodeMemManager }

  TXDBAVLTreeNodeMemManager = class(TAVLTreeNodeMemManager)
  public
    procedure DisposeNode(ANode: TAVLTreeNode); override;
    function NewNode: TAVLTreeNode; override;
  end;

  { TXDBAVLTree }

  TXDBAVLTree = class(TAVLTree)
  protected
    fNodeManager: TAVLTreeNodeMemManager;
  public
    constructor Create(OnCompareMethod: TListSortCompare);
    destructor Destroy; override;
  end;

  { TXDBStringTree }

  TXDBStringTree = class(TStringTree)
  protected
    fNodeManager: TAVLTreeNodeMemManager;
  public
    constructor Create;
    destructor Destroy; override;
  end;

// string operations
function UTF8FindNearestCharStart(UTF8Str: PChar; Len: integer;
  BytePos: integer): integer;
function ExtractFirstURLPath(var Path: string): string;
function StringToList(const LongOpts: string): TStrings;

// xml strings
function GetXMLNameLength(Name: PChar): integer;
function GetXMLName(Name: PChar): string;
function CompareXMLNames(Name1, Name2: PChar): integer;
function CompareXMLNamesPtrs(Name1, Name2: Pointer): integer; inline;
function GetXMLAttriNameLength(Name: PChar): integer;
function GetXMLAttrName(Name: PChar): string;
function CompareXMLAttrNames(Name1, Name2: PChar): integer;
function CompareXMLAttrValue(Value1, Value2: PChar): integer;
function StrToXMLValue(const s: string): string;

// environment
function GetProgramSearchPath: string;
function FindDefaultExecutablePath(const Executable: string): string;

// date, time, age
function DateTimeToXDBStr(const ADateTime: TDateTime): string;
function FileAgeToXDBStr(aFileAge: longint): string;

var
  IsXMLNameStartChar, IsXMLNameChar: array[char] of boolean;

implementation

{ Find the start of the UTF8 character which contains BytePos,
  Len is length in byte, BytePos starts at 0 }
function UTF8FindNearestCharStart(UTF8Str: PChar; Len: integer;
  BytePos: integer): integer;
begin
  Result:=0;
  if (UTF8Str<>nil) and (Len>0) and (BytePos>=0) then begin
    Result:=BytePos;
    if Result>Len then Result:=Len-1;
    if (Result>0) and (ord(UTF8Str[Result]) and %11000000=%10000000) then begin
      dec(Result);
      if (Result>0) and (ord(UTF8Str[Result]) and %11000000=%10000000) then begin
        dec(Result);
        if (Result>0) and (ord(UTF8Str[Result]) and %11000000=%10000000) then begin
          dec(Result);
          // should be four byte character
          if (ord(UTF8Str[Result]) and %11111000<>%11110000) then begin
            // broken UTF8 character
            inc(Result,3);
          end else begin
            // is four byte character
          end;
        end else if (ord(UTF8Str[Result]) and %11110000<>%11100000) then begin
          // broken UTF8 character, should be three byte
          inc(Result,2);
        end else
        begin
          // is three byte character
        end;
      end else if (ord(UTF8Str[Result]) and %11100000<>%11000000) then begin
        // broken UTF8 character, should be two byte
        inc(Result);
      end else
      begin
        // is two byte character
      end;
    end;
  end;
end;

function ExtractFirstURLPath(var Path: string): string;
var
  p: SizeInt;
begin
  p:=Pos('/',Path);
  if p<1 then p:=length(Path)+1;
  Result:=copy(Path,1,p-1);
  Path:=copy(Path,p+1,length(Path));
end;

function StringToList(const LongOpts: string): TStrings;
const
  SepChars = ' '#10#13#9;
var
  L : TStringList;
  Len,I,J : Integer;
begin
  l:=TStringList.Create;
  I:=1;
  Len:=Length(LongOpts);
  while I<=Len do begin
    while Isdelimiter(SepChars,LongOpts,I) do
      Inc(I);
    J:=I;
    while (J<=Len) and Not IsDelimiter(SepChars,LongOpts,J) do
      Inc(J);
    if (I<=J) then
      L.Add(Copy(LongOpts,I,(J-I)));
    I:=J+1;
  end;
  Result:=l;
end;

function GetXMLNameLength(Name: PChar): integer;
begin
  Result:=0;
  if (Name=nil) or (not IsXMLNameStartChar[Name^]) then exit;
  inc(Name);
  inc(Result);
  while IsXMLNameChar[Name^] do begin
    inc(Name);
    inc(Result);
  end;
end;

function GetXMLName(Name: PChar): string;
begin
  SetLength(Result,GetXMLNameLength(Name));
  if Result='' then exit;
  Move(Name^,Result[1],length(Result));
end;

function CompareXMLNames(Name1, Name2: PChar): integer;
begin
  if (Name1<>nil) then begin
    if (Name2<>nil) then begin
      while (Name1^=Name2^) do begin
        if (IsXMLNameChar[Name1^]) then begin
          inc(Name1);
          inc(Name2);
        end else begin
          Result:=0; // for example  'aaA;' 'aAa;'
          exit;
        end;
      end;
      if (IsXMLNameChar[Name1^]) then begin
        if (IsXMLNameChar[Name2^]) then begin
          if Name1^>Name2^ then
            Result:=-1 // for example  'aab' 'aaa'
          else
            Result:=1; // for example  'aaa' 'aab'
        end else begin
          Result:=-1; // for example  'aaa' 'aa;'
        end;
      end else begin
        if (IsXMLNameChar[Name2^]) then
          Result:=1 // for example  'aa;' 'aaa'
        else
          Result:=0; // for example  'aa;' 'aa,'
      end;
    end else begin
      Result:=-1; // for example  'aaa' nil
    end;
  end else begin
    if (Name2<>nil) then begin
      Result:=1; // for example  nil 'bbb'
    end else begin
      Result:=0; // for example  nil nil
    end;
  end;
end;

function CompareXMLNamesPtrs(Name1, Name2: Pointer): integer;
begin
  Result:=CompareXMLNames(PChar(Name1),PChar(Name2));
end;

function GetXMLAttriNameLength(Name: PChar): integer;
begin
  Result:=0;
  if (Name=nil) or (not IsXMLNameStartChar[Name^]) then exit;
  inc(Name);
  inc(Result);
  while IsXMLNameChar[Name^] do begin
    inc(Name);
    inc(Result);
  end;
  if Name^<>':' then exit;
  inc(Result);
  inc(Name);
  while IsXMLNameChar[Name^] do begin
    inc(Name);
    inc(Result);
  end;
end;

function GetXMLAttrName(Name: PChar): string;
begin
  SetLength(Result,GetXMLAttriNameLength(Name));
  if Result='' then exit;
  Move(Name^,Result[1],length(Result));
end;

function CompareXMLAttrNames(Name1, Name2: PChar): integer;
var
  ColonFound: Boolean;
begin
  if (Name1<>nil) then begin
    if (Name2<>nil) then begin
      ColonFound:=false;
      while (Name1^=Name2^) do begin
        if (IsXMLNameChar[Name1^]) then begin
          inc(Name1);
          inc(Name2);
        end else if (Name1^=':') and not ColonFound then begin
          ColonFound:=true;
          inc(Name1);
          inc(Name2);
        end else begin
          Result:=0; // for example  'aaA;' 'aAa;'
          exit;
        end;
      end;
      if (IsXMLNameChar[Name1^]) then begin
        if (IsXMLNameChar[Name2^]) then begin
          if Name1^>Name2^ then
            Result:=-1 // for example  'aab' 'aaa'
          else
            Result:=1; // for example  'aaa' 'aab'
        end else begin
          Result:=-1; // for example  'aaa' 'aa;'
        end;
      end else begin
        if (IsXMLNameChar[Name2^]) then
          Result:=1 // for example  'aa;' 'aaa'
        else
          Result:=0; // for example  'aa;' 'aa,'
      end;
    end else begin
      Result:=-1; // for example  'aaa' nil
    end;
  end else begin
    if (Name2<>nil) then begin
      Result:=1; // for example  nil 'bbb'
    end else begin
      Result:=0; // for example  nil nil
    end;
  end;
end;

function CompareXMLAttrValue(Value1, Value2: PChar): integer;
begin
  if (Value1<>nil) then begin
    if (Value2<>nil) then begin
      while (Value1^=Value2^) do begin
        if (Value1^<>#0) then begin
          inc(Value1);
          inc(Value2);
        end else begin
          Result:=0; // for example  'aaA;' 'aAa;'
          exit;
        end;
      end;
      if Value1^>Value2^ then
        Result:=-1 // for example  'aab' 'aaa'
      else
        Result:=1; // for example  'aaa' 'aab'
    end else begin
      Result:=-1; // for example  'aaa' nil
    end;
  end else begin
    if (Value2<>nil) then begin
      Result:=1; // for example  nil 'bbb'
    end else begin
      Result:=0; // for example  nil nil
    end;
  end;
end;

function StrToXMLValue(const s: string): string;
var
  p: PChar;
  b: PtrUInt;
  h: String;
begin
  Result:=s;
  if Result='' then exit;
  p:=PChar(Result);
  repeat
    if (p^=#0) and (p-PChar(Result)=length(Result)) then
      exit
    else if p^ in [#0,'&','<','>','"',''''] then begin
      case p^ of
      '&': h:='&amp;';
      '<': h:='&lt;';
      '>': h:='&gt;';
      '"': h:='&quot;';
      '''': h:='&apos;';
      else h:='';
      end;
      b:=p-PChar(Result);
      Result:=copy(Result,1,b-1)+h+copy(Result,b+1,length(Result));
      p:=PChar(Result)+b+length(h);
    end else begin
      inc(p);
    end;
  until false;
end;

function GetProgramSearchPath: string;
begin
  Result := GetEnvironmentVariableUTF8('PATH');
end;

function FindDefaultExecutablePath(const Executable: string): string;
begin
  if FilenameIsAbsolute(Executable) then
    Result:=Executable
  else
    Result:=SearchFileInPath(Executable,'',GetProgramSearchPath,':',
                             ctsfcDefault);
end;

function DateTimeToXDBStr(const ADateTime: TDateTime): string;
var
  Year, Month, Day: word;
  Hour, Minute, Second, MilliSecond: word;
begin
  Year:=1900;
  Month:=1;
  Day:=1;
  Hour:=0;
  Minute:=0;
  Second:=0;
  MilliSecond:=0;
  DecodeDate(ADateTime,Year,Month,Day);
  DecodeTime(ADateTime,Hour,Minute,Second,MilliSecond);
  Result:=IntToStr(Year)+'-'+IntToStr(Month)+'-'+IntToStr(Day)
          +' '+IntToStr(Hour)+':'+IntToStr(Minute)+':'+IntToStr(Second);
end;

function FileAgeToXDBStr(aFileAge: longint): string;
begin
  Result:=DateTimeToXDBStr(FileDateToDateTime(aFileAge));
end;

{ TXDBStringTree }

constructor TXDBStringTree.Create;
begin
  inherited Create;
  fNodeManager:=TXDBAVLTreeNodeMemManager.Create;
  Tree.SetNodeManager(fNodeManager);
end;

destructor TXDBStringTree.Destroy;
begin
  inherited Destroy;
  FreeAndNil(fNodeManager);
end;

{ TXDBAVLTreeNodeMemManager }

procedure TXDBAVLTreeNodeMemManager.DisposeNode(ANode: TAVLTreeNode);
begin
  ANode.Free;
end;

function TXDBAVLTreeNodeMemManager.NewNode: TAVLTreeNode;
begin
  Result:=TAVLTreeNode.Create;
end;

{ TXDBAVLTree }

constructor TXDBAVLTree.Create(OnCompareMethod: TListSortCompare);
begin
  inherited Create(OnCompareMethod);
  fNodeManager:=TXDBAVLTreeNodeMemManager.Create;
  SetNodeManager(fNodeManager);
end;

destructor TXDBAVLTree.Destroy;
begin
  inherited Destroy;
  FreeAndNil(fNodeManager);
end;

procedure Init;
var
  c: Char;
begin
  for c:=low(char) to high(char) do begin
    IsXMLNameStartChar[c]:=c in ['a'..'z','A'..'Z','_',#192..#255];
    IsXMLNameChar[c]:=c in ['a'..'z','A'..'Z','_','0'..'9',#128..#255];
  end;
end;

initialization
  Init;

end.

