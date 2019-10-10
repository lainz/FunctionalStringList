unit ufunctionalstringlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TFunctionalStringList = class;

  TFunctionalStringListFilterMethod = function(const s: string): boolean of object;
  TFunctionalStringListReduceMethod = function(const s1: string;
    const s2: string): string of object;
  TFunctionalStringListMapMethod = function(const s: string): string of object;
  TFunctionalStringListForEachMethod = procedure(const currentValue: string;
    const index: integer; const arr: TFunctionalStringList) of object;
  TFunctionalStringListSomeMethod = function(const currentValue: string;
    const index: integer; const arr: TFunctionalStringList): boolean of object;

  { TFunctionalStringList }

  TFunctionalStringList = class(TStringList)
  public
    function Filter(fun: TFunctionalStringListFilterMethod): TFunctionalStringList;
    function Reduce(fun: TFunctionalStringListReduceMethod): string;
    function Map(fun: TFunctionalStringListMapMethod): TFunctionalStringList;
    procedure ForEach(fun: TFunctionalStringListForeachMethod);
    function Pop: string;
    function Push(s: string): integer;
    function Shift: string;
    function Unshift(s: string): integer;
    function Reverse: TFunctionalStringList;
    function ToString: string; override;
    function Join(separator: string): string;
    function Length: integer;
    function Concat(other: TFunctionalStringList): TFunctionalStringList;
    function Slice(fromIndex: integer): TFunctionalStringList;
    function Fill(Value: string; start: integer = 0;
      _end: integer = -1): TFunctionalStringList;
    function ValueOf: TFunctionalStringList;
    function Some(fun: TFunctionalStringListSomeMethod): boolean;
    function IndexOf(s: string; start: integer = 0): integer;
    function LastIndexOf(s: string; start: integer = -1): integer;
  end;

operator := (fsl: TFunctionalStringList): string;

implementation

operator := (fsl: TFunctionalStringList): string;
begin
  Result := fsl.ToString;
end;

{ TFunctionalStringList }

function TFunctionalStringList.Filter(fun: TFunctionalStringListFilterMethod):
TFunctionalStringList;
var
  i: integer;
begin
  Result := TFunctionalStringList.Create;
  for i := 0 to Self.Count - 1 do
  begin
    if fun(Self[i]) then
      Result.Add(Self[i]);
  end;
end;

function TFunctionalStringList.Reduce(fun: TFunctionalStringListReduceMethod): string;
var
  i: integer;
begin
  Result := Self[0];
  for i := 1 to Self.Count - 1 do
  begin
    Result := fun(Result, Self[i]);
  end;
end;

function TFunctionalStringList.Map(fun: TFunctionalStringListMapMethod):
TFunctionalStringList;
var
  i: integer;
begin
  Result := TFunctionalStringList.Create;
  for i := 0 to Self.Count - 1 do
  begin
    Result.Add(fun(Self[i]));
  end;
end;

procedure TFunctionalStringList.ForEach(fun: TFunctionalStringListForeachMethod);
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
  begin
    fun(Self[i], i, Self);
  end;
end;

function TFunctionalStringList.Pop: string;
begin
  Result := '';
  if (Self.Count > 0) then
  begin
    Result := Self[Self.Count - 1];
    Self.Delete(Self.Count - 1);
  end;
end;

function TFunctionalStringList.Push(s: string): integer;
begin
  Result := 0;
  Self.AddText(s);
  Result := Self.Count;
end;

function TFunctionalStringList.Shift: string;
begin
  Result := '';
  if (Self.Count > 0) then
  begin
    Result := Self[0];
    Self.Delete(0);
  end;
end;

function TFunctionalStringList.Unshift(s: string): integer;
begin
  Self.Insert(0, s);
  Result := Self.Count;
end;

function TFunctionalStringList.Reverse: TFunctionalStringList;
var
  i: integer;
  tmp: TFunctionalStringList;
begin
  tmp := TFunctionalStringList.Create;
  for i := Self.Count - 1 downto 0 do
    tmp.Add(Self[i]);
  Self.Clear;
  Self.Text := tmp.Text;
  tmp.Free;
  Result := Self;
end;

function TFunctionalStringList.ToString: string;
begin
  Result := Self.CommaText;
end;

function TFunctionalStringList.Join(separator: string): string;
var
  i: integer;
begin
  Result := Self[0];
  for i := 1 to Self.Count - 1 do
  begin
    Result := Result + separator + Self[i];
  end;
end;

function TFunctionalStringList.Length: integer;
begin
  Result := Self.Count;
end;

function TFunctionalStringList.Concat(other: TFunctionalStringList):
TFunctionalStringList;
begin
  Result := TFunctionalStringList.Create;
  Result.AddText(Self.Text);
  Result.AddText(other.Text);
end;

function TFunctionalStringList.Slice(fromIndex: integer): TFunctionalStringList;
var
  i: integer;
begin
  Result := TFunctionalStringList.Create;
  for i := fromIndex to Self.Count - 1 do
  begin
    Result.Add(Self[i]);
  end;
end;

function TFunctionalStringList.Fill(Value: string; start: integer;
  _end: integer): TFunctionalStringList;
var
  i: integer;
begin
  Result := Self;
  if _end = -1 then
    _end := Self.Count - 1;
  for i := start to _end do
    Self[i] := Value;
end;

function TFunctionalStringList.ValueOf: TFunctionalStringList;
begin
  Result := Self;
end;

function TFunctionalStringList.Some(fun: TFunctionalStringListSomeMethod): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Self.Count - 1 do
  begin
    if fun(Self[i], i, Self) then
    begin
      Result := True;
      break;
    end;
  end;
end;

function TFunctionalStringList.IndexOf(s: string; start: integer): integer;
var
  i: integer;
begin
  Result := -1;
  for i := start to Self.Count - 1 do
  begin
    if s = Self[i] then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TFunctionalStringList.LastIndexOf(s: string; start: integer): integer;
var
  i: integer;
begin
  Result := -1;
  if start = -1 then
    start := Self.Count - 1;
  for i := start downto 0 do
  begin
    if s = Self[i] then
    begin
      Result := i;
      Break;
    end;
  end;
end;

end.
