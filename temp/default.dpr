program default;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  System.Classes;

var
  f: TFileStream;
  a: array [0 .. 10] of Byte;
  c: WideChar;
  i: integer;

begin
  try
    { TODO -oUser -cConsole メイン : ここにコードを記述してください }
    f := TFileStream.Create('default.fo', fmOpenRead);
    f.Position := 2;
    f.ReadBuffer((@c)^, SizeOf(WideChar));
    Writeln(c);
    Readln;
    f.Free;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
