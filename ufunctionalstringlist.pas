unit ufunctionalstringlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TStringListFilterMethod = function(const s: string): boolean of object;
  TStringListReduceMethod = function(const s1: string;
    const s2: string): string of object;
  TStringListMapMethod = function(const s: string): string of object;
  TStringListForEachMethod = procedure(const currentValue: string;
    const index: integer; const arr: TStringList) of object;
  TStringListSomeMethod = function(const currentValue: string;
    const index: integer; const arr: TStringList): boolean of object;

  { TStringList }

  TFunctionalStringList = class helper for TStringList
  public
    function Filter(fun: TStringListFilterMethod): TStringList;
    function Reduce(fun: TStringListReduceMethod;
      startingValue: string = ''): string;
    function Map(fun: TStringListMapMethod): TStringList;
    procedure ForEach(fun: TStringListForeachMethod);
    function Pop: string;
    function Push(s: string): integer;
    function Shift: string;
    function Unshift(s: string): integer;
    function Reverse: TStringList;
    function Join(separator: string): string;
    function Concat(other: TStringList): TStringList;
    function Slice(fromIndex: integer): TStringList;
    function Slice(fromIndex, toIndex: integer): TStringList;
    function Fill(Value: string; start: integer = 0;
      _end: integer = -1): TStringList;
    function Some(fun: TStringListSomeMethod): boolean;
    function IndexOf(s: string; start: integer = 0): integer;
    function LastIndexOf(s: string; start: integer = -1): integer;
  end;

operator := (fsl: TStringList): string;

implementation

operator := (fsl: TStringList): string;
begin
  Result := fsl.CommaText;
end;

{ TStringList }

function TFunctionalStringList.Filter(fun: TStringListFilterMethod):
TStringList;
var
  i: integer;
begin
  Result := TStringList.Create;
  for i := 0 to Self.Count - 1 do
  begin
    if fun(Self[i]) then
      Result.Add(Self[i]);
  end;
end;

function TFunctionalStringList.Reduce(fun: TStringListReduceMethod;
  startingValue: string): string;
var
  i: integer;
begin
  Result := startingValue;
  for i := 0 to Self.Count - 1 do
  begin
    Result := fun(Result, Self[i]);
  end;
end;

function TFunctionalStringList.Map(fun: TStringListMapMethod):
TStringList;
var
  i: integer;
begin
  Result := TStringList.Create;
  for i := 0 to Self.Count - 1 do
  begin
    Result.Add(fun(Self[i]));
  end;
end;

procedure TFunctionalStringList.ForEach(fun: TStringListForeachMethod);
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
  Self.Add(s);
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

function TFunctionalStringList.Reverse: TStringList;
var
  i: integer;
begin
  Result := TStringList.Create;
  for i := Self.Count - 1 downto 0 do
    Result.Add(Self[i]);
end;

function TFunctionalStringList.Join(separator: string): string;
var
  i: integer;
begin
  Result := '';
  if (Self.Count > 0) then
  begin
    Result := Self[0];
    for i := 1 to Self.Count - 1 do
    begin
      Result := Result + separator + Self[i];
    end;
  end;
end;

function TFunctionalStringList.Concat(other: TStringList):
TStringList;
begin
  Result := TStringList.Create;
  Result.AddStrings(Self);
  Result.AddStrings(other);
end;

function TFunctionalStringList.Slice(fromIndex: integer): TStringList;
var
  i: integer;
begin
  Result := TStringList.Create;
  for i := fromIndex to Self.Count - 1 do
  begin
    Result.Add(Self[i]);
  end;
end;

function TFunctionalStringList.Slice(fromIndex, toIndex: integer
  ): TStringList;
var
  i: integer;
begin
  Result := TStringList.Create;
  for i := fromIndex to toIndex do
  begin
    Result.Add(Self[i]);
  end;
end;

function TFunctionalStringList.Fill(Value: string; start: integer;
  _end: integer): TStringList;
var
  i: integer;
begin
  Result := Self;
  if _end = -1 then
    _end := Self.Count - 1;
  for i := start to _end do
    Self[i] := Value;
end;

function TFunctionalStringList.Some(fun: TStringListSomeMethod): boolean;
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
