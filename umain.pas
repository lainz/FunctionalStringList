unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ufunctionalstringlist;

type

  { TForm1 }

  TForm1 = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    function Filter(const s: String): boolean;
    procedure ForEach(const currentValue: string; const index: integer;
      const arr: TStringList);
    function Map(const s: String): String;
    function Reduce(const s1: String; const s2: String): String;
    function Some(const currentValue: string; const index: integer;
      const arr: TStringList): boolean;
    procedure printseparator(Memo: TMemo);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  sl, sl2, slf, slempty: TStringList;
begin
  slempty := TStringList.Create;

  slf := TStringList.Create;
  slf.CommaText := '0,1,2,3,4';

  sl2 := TStringList.Create;
  sl2.CommaText := '5,6,7,8,9,0';

  // Concat
  sl := slf.Concat(sl2);
  Memo1.Lines.Add('Concat' + LineEnding + sl.ToString);
  printseparator(Memo1);
  sl2.Free;
  slf.Free;

  // Concat empty list
  sl2 := slempty.Concat(slempty);
  Memo2.Lines.Add('Concat empty lists' + LineEnding + slempty.ToString);
  printseparator(Memo2);
  sl2.Free;

  // IndexOf
  Memo1.Lines.Add('IndexOf' + LineEnding + IntToStr(sl.IndexOf('0')));
  printseparator(Memo1);
  Memo2.Lines.Add('IndexOf in empty list' + LineEnding + IntToStr(slempty.IndexOf('0')));
  printseparator(Memo2);

  // LastIndexOf
  Memo1.Lines.Add('LastIndexOf' + LineEnding + IntToStr(sl.LastIndexOf('0')));
  printseparator(Memo1);
  Memo2.Lines.Add('LastIndexOf in empty list' + LineEnding + IntToStr(slempty.LastIndexOf('0')));
  printseparator(Memo2);

  // Some
  Memo1.Lines.Add('Some' + LineEnding + BoolToStr(sl.Some(@Some), 'True', 'False'));
  printseparator(Memo1);
  Memo2.Lines.Add('Some in empty list' + LineEnding + BoolToStr(slempty.Some(@Some), 'True', 'False'));
  printseparator(Memo2);

  // ToString
  Memo1.Lines.Add(sl);
  printseparator(Memo1);
  Memo2.Lines.Add(slempty);
  printseparator(Memo2);

  // Filter
  slf := sl.Filter(@Filter);
  Memo1.Lines.Add('Filter' + LineEnding + slf.ToString);
  printseparator(Memo1);
  slf.Free;

  slf := slempty.Filter(@Filter);
  Memo2.Lines.Add('Filter in empty list' + LineEnding + slf.CommaText);
  printseparator(Memo2);
  slf.Free;

  // Reduce
  Memo1.Lines.Add('Reduce' + LineEnding + sl.Reduce(@Reduce, '0'));
  printseparator(Memo1);
  Memo2.Lines.Add('Reduce empty list' + LineEnding + slempty.Reduce(@Reduce, '0'));
  printseparator(Memo2);

  // Map
  slf := sl.Map(@Map);
  Memo1.Lines.Add('Map' + LineEnding + slf.Text);
  printseparator(Memo1);
  slf.Free;

  slf := slempty.Map(@Map);
  Memo2.Lines.Add('Map empty list' + LineEnding + slf.Text);
  printseparator(Memo2);
  slf.Free;

  // Pop
  Memo1.Lines.Add('Pop' + LineEnding + sl.Pop);
  printseparator(Memo1);
  Memo1.Lines.Add(sl);
  printseparator(Memo1);

  Memo2.Lines.Add('Pop empty list' + LineEnding + slempty.Pop);
  printseparator(Memo2);
  Memo2.Lines.Add(slempty);
  printseparator(Memo2);

  // Push
  Memo1.Lines.Add('Push' + LineEnding + IntToStr(sl.Push('9')));
  printseparator(Memo1);
  Memo1.Lines.Add(sl);
  printseparator(Memo1);

  Memo2.Lines.Add('Push empty list' + LineEnding + IntToStr(slempty.Push('9')));
  printseparator(Memo2);
  Memo2.Lines.Add(slempty);
  printseparator(Memo2);
  slempty.Clear;

  // Reverse
  slf := sl.Reverse;
  Memo1.Lines.Add('Reverse' + LineEnding + slf.CommaText);
  printseparator(Memo1);
  slf.Free;

  slf := slempty.Reverse;
  Memo2.Lines.Add('Reverse empty list' + LineEnding + slf.CommaText);
  printseparator(Memo2);
  slf.Free;

  // Shift
  Memo1.Lines.Add('Shift' + LineEnding + sl.Shift);
  printseparator(Memo1);
  Memo1.Lines.Add(sl);
  printseparator(Memo1);

  Memo2.Lines.Add('Shift empty list' + LineEnding + slempty.Shift);
  printseparator(Memo2);
  Memo2.Lines.Add(slempty);
  printseparator(Memo2);

  // Unshift
  Memo1.Lines.Add('Unshift' + LineEnding + IntToStr(sl.Unshift('10')));
  printseparator(Memo1);
  Memo1.Lines.Add(sl);
  printseparator(Memo1);

  Memo2.Lines.Add('Unshift empty list' + LineEnding + IntToStr(slempty.Unshift('10')));
  printseparator(Memo2);
  Memo2.Lines.Add(slempty);
  printseparator(Memo2);
  slempty.Clear;

  // Join
  Memo1.Lines.Add('Join' + LineEnding + sl.Join(' * '));
  printseparator(Memo1);
  Memo2.Lines.Add('Join empty list' + LineEnding + slempty.Join(' * '));
  printseparator(Memo2);

  // Slice
  slf := sl.Slice(5);
  Memo1.Lines.Add('Slice' + LineEnding + slf.CommaText);
  printseparator(Memo1);
  slf.Free;

  slf := slempty.Slice(5);
  Memo2.Lines.Add('Slice empty list' + LineEnding + slf.CommaText);
  printseparator(Memo2);
  slf.Free;

  // Fill
  Memo1.Lines.Add('Fill' + LineEnding + sl.Fill('00').CommaText);
  printseparator(Memo1);
  Memo2.Lines.Add('Fill empty list' + LineEnding + slempty.Fill('00').CommaText);
  printseparator(Memo2);

  // ForEach
  sl.ForEach(@ForEach);
  Memo1.Lines.Add('ForEach' + LineEnding + sl.Text);
  printseparator(Memo1);

  slempty.ForEach(@ForEach);
  Memo2.Lines.Add('ForEach empty list' + LineEnding + slempty.Text);
  printseparator(Memo2);

  sl.Free;
  slempty.Free;
end;

function TForm1.Filter(const s: String): boolean;
begin
  Result := StrToInt(s) mod 2 = 0;
end;

procedure TForm1.ForEach(const currentValue: string; const index: integer;
  const arr: TStringList);
begin
  arr[index] := '<p>' + currentValue + '</p>';
end;

function TForm1.Map(const s: String): String;
begin
  Result := s + '</br>';
end;

function TForm1.Reduce(const s1: String; const s2: String): String;
begin
  Result := IntToStr(StrToInt(s1) + StrToInt(s2));
end;

function TForm1.Some(const currentValue: string; const index: integer;
  const arr: TStringList): boolean;
begin
  Result := StrToInt(currentValue) > 15;
end;

procedure TForm1.printseparator(Memo: TMemo);
begin
  Memo.Lines.Add('---------------');
end;

end.

