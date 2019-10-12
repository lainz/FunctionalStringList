unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ufunctionalstringlist;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    function Filter(const s: String): boolean;
    procedure ForEach(const currentValue: string; const index: integer;
      const arr: TFunctionalStringList);
    function Map(const s: String): String;
    function Reduce(const s1: String; const s2: String): String;
    function Some(const currentValue: string; const index: integer;
      const arr: TFunctionalStringList): boolean;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  sl, sl2, slf: TFunctionalStringList;
begin
  slf := TFunctionalStringList.Create;
  slf.CommaText := '0,1,2,3,4';

  sl2 := TFunctionalStringList.Create;
  sl2.CommaText := '5,6,7,8,9,0';

  // Concat
  sl := slf.Concat(sl2);
  sl2.Free;
  slf.Free;

  // IndexOf
  ShowMessage('IndexOf' + LineEnding + IntToStr(sl.IndexOf('0')));

  // LastIndexOf
  ShowMessage('LastIndexOf' + LineEnding + IntToStr(sl.LastIndexOf('0')));

  // Some
  ShowMessage('Some' + LineEnding + BoolToStr(sl.Some(@Some), 'True', 'False'));

  // ToString
  ShowMessage(sl);

  // Filter
  slf := sl.Filter(@Filter);
  ShowMessage('Filter' + LineEnding + slf.ToString);
  slf.Free;

  // Reduce
  ShowMessage('Reduce' + LineEnding + sl.Reduce(@Reduce, '0'));

  // Map
  slf := sl.Map(@Map);
  ShowMessage('Map' + LineEnding + slf.Text);
  slf.Free;

  // Pop
  ShowMessage('Pop' + LineEnding + sl.Pop);
  ShowMessage(sl);

  // Push
  ShowMessage('Push' + LineEnding + IntToStr(sl.Push('9' + LineEnding + '10')));
  ShowMessage(sl);

  // Reverse
  slf := sl.Reverse;
  ShowMessage('Reverse' + LineEnding + slf.ToString);
  slf.Free;

  // Shift
  ShowMessage('Shift' + LineEnding + sl.Shift);
  ShowMessage(sl);

  // Unshift
  ShowMessage('Unshift' + LineEnding + IntToStr(sl.Unshift('10')));
  ShowMessage(sl);

  // Join
  ShowMessage('Join' + LineEnding + sl.Join(' * '));

  // Slice
  slf := sl.Slice(5);
  ShowMessage('Slice' + LineEnding + slf.ToString);
  slf.Free;

  // Fill
  ShowMessage('Fill' + LineEnding + sl.Fill('00').ToString);

  // ForEach
  sl.ForEach(@ForEach);
  ShowMessage('ForEach' + LineEnding + sl.Text);

  sl.Free;

  Application.Terminate;
end;

function TForm1.Filter(const s: String): boolean;
begin
  Result := StrToInt(s) mod 2 = 0;
end;

procedure TForm1.ForEach(const currentValue: string; const index: integer;
  const arr: TFunctionalStringList);
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
  const arr: TFunctionalStringList): boolean;
begin
  Result := StrToInt(currentValue) > 15;
end;

end.

