program buffertest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes;

var
  s: TMemoryStream;
  str1, str2: string;
  i: integer;
begin
  try
    { TODO -oUser -cConsole メイン : ここにコードを記述してください }
    s:=TMemoryStream.Create;
    str1:='jiro';
    i:=Length(str1);
    s.WriteBuffer(i,SizeOf(integer));
    s.Write(@str1,i*SizeOf(WideChar));
    s.Position:=SizeOf(integer);
    s.Read((@str2)^,i*SizeOf(WideChar));
    Writeln(str2);
    Readln;
    s.Free;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
