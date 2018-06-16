program buffertest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes;

var
  f: TFileStream;
  s, ss: string[20];
  a, b: array [0..10] of WideChar;
  i: Byte;
begin
  try
    { TODO -oUser -cConsole メイン : ここにコードを記述してください }
    f:=TFileStream.Create('test.str',fmCreate or fmOpenReadWrite);
    a:='kainushi';
    i:=Length(a);
    f.WriteBuffer(i,1);
    f.Write(a,i*SizeOf(WideChar));
    a[0]:='q';
    f.Position:=0;
    f.ReadBuffer(i,1);
    f.Read(b,i*SizeOf(WideChar));
    Writeln(b);
    s:='hello world';
    i:=Length(s)+1;
    f.Position:=0;
    f.WriteBuffer(i,1);
    f.WriteBuffer((@s)^,i);
    s[1]:='q';
    f.Position:=1;
    f.ReadBuffer((@ss)^,i);
    Writeln(ss);
    Readln;
    f.Free;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
