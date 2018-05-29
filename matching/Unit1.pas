unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ExtDlgs,
  Vcl.ComCtrls, System.Types, Jpeg;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Image2: TImage;
    OpenPictureDialog1: TOpenPictureDialog;
    Edit1: TEdit;
    UpDown1: TUpDown;
    procedure Button1Click(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private êÈåæ }
    fsize: Integer;
    bmp: TBitmap;
    procedure DrawRect(canvas: TCanvas; rect: TRect);
  public
    { Public êÈåæ }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute = true then
  begin
    Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
    bmp.Assign(Image1.Picture.Graphic);
    Image1.Picture.Assign(bmp);
  end;
end;

procedure TForm1.DrawRect(canvas: TCanvas; rect: TRect);
var
  s: array [0 .. 3] of TPoint;
begin
  s[0] := rect.TopLeft;
  s[1] := Point(rect.Left, rect.Bottom);
  s[2] := rect.BottomRight;
  s[3] := Point(rect.Right, rect.Top);
  canvas.Polyline(s);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  bmp := TBitmap.Create;
  if Image1.Picture.Bitmap.Empty = false then
    bmp.Assign(Image1.Picture.Bitmap);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  bmp.Free;
end;

procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
const
  MAX_CAN = 100;
var
  i, j, k, l, nx, ny, s2, numCandidate: Integer;
  x1, y1, min: Integer;
  xMatch, yMatch: Integer;
  col: TRGBTriple;
  rr, thTemp: Integer;
  g: array of TRGBTriple;
  xCandidate, yCandidate, rrCandidate: array [0 .. MAX_CAN - 1] of Integer;
  f: array of array of Integer;
  ff, temp: array of array of Integer;
label
  finish;
begin
  thTemp := UpDown1.Position;
  fsize := Image2.Height;
  s2 := fsize div 2;
  if Image1.Picture.Bitmap.Empty = true then
    Exit;
  nx := Image1.Picture.Bitmap.Width;
  ny := Image1.Picture.Bitmap.Height;
  if (X < s2) or (X > nx - s2) or (Y < s2) or (Y > ny - s2) then
    Exit;
  Image1.Picture.Assign(bmp);
  Image2.Canvas.CopyRect(rect(0, 0, fsize-1, fsize-1), Image1.canvas,
    rect(X - s2, Y - s2, X + s2 - 1, Y + s2 - 1));
  SetLength(f, nx, ny);
  SetLength(ff, fsize, fsize);
  SetLength(temp, fsize, fsize);
  for j := ny - 1 downto 0 do
  begin
    Pointer(g) := Image1.Picture.Bitmap.ScanLine[j];
    for i := 0 to nx - 1 do
      f[i, j] := g[i].rgbtRed;
  end;
  for j := fsize - 1 downto 0 do
  begin
    Pointer(g) := Image2.Picture.Bitmap.ScanLine[j];
    for i := 0 to fsize - 1 do
      temp[i, j] := g[i].rgbtRed;
  end;
  numCandidate := 0;
  for j := s2 to ny - s2 - 1 do
    for i := s2 to nx - s2 - 1 do
    begin
      rr := 0;
      for l := 0 to fsize - 1 do
        for k := 0 to fsize - 1 do
        begin
          ff[k, l] := f[i + k - s2, j + l - s2];
          inc(rr, ff[k, l] - temp[k, l]);
        end;
      rr := rr div (fsize * fsize);
      if rr < Abs(thTemp) then
      begin
        xCandidate[numCandidate] := i;
        yCandidate[numCandidate] := j;
        rrCandidate[numCandidate] := rr;
        inc(numCandidate);
        if numCandidate > MAX_CAN then
          goto finish;
      end;
    end;
finish:
  min := rrCandidate[0];
  xMatch := xCandidate[0];
  yMatch := yCandidate[0];
  Image1.canvas.Pen.Color := clBlack;
  for i := 0 to numCandidate - 1 do
  begin
    if min > Abs(rrCandidate[i]) then
    begin
      min := rrCandidate[i];
      xMatch := xCandidate[i];
      yMatch := yCandidate[i];
    end;
    x1 := xCandidate[i] - s2;
    y1 := yCandidate[i] - s2;
    DrawRect(Image1.canvas, rect(x1, y1, x1 + fsize, y1 + fsize));
  end;
  Image1.canvas.Pen.Color := clRed;
  DrawRect(Image1.canvas, rect(xMatch - s2, yMatch - s2, xMatch + s2,
    yMatch + s2));
  Caption := 'rrCandidate = ' + IntToStr(min);
  Finalize(f);
  Finalize(ff);
  Finalize(temp);
end;

end.
