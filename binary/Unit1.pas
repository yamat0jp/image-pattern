unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TRawImg = array of array of integer;

  TForm1 = class(TForm)
    Image1: TImage;
    Button1: TButton;
    Image2: TImage;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private êÈåæ }
    function thinning4(nx, ny: integer; img: TRawImg): integer;
    function connect(a: array of integer): integer;
    procedure getBinaryImage(bmp: TBitmap; arr: TRawImg);
    procedure getBorder4(nx, ny: integer; f, border: TRawImg);
    procedure chase4(i, j, code: integer; f, border: TRawImg);
  public
    { Public êÈåæ }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  i, j, nx, ny, kaisu: integer;
  f: TRawImg;
  g: array of TRGBTriple;
  color: TRGBTriple;
  bmp: TBitmap;
begin
  nx := Image1.Picture.Bitmap.Width;
  ny := Image1.Picture.Bitmap.Height;
  SetLength(f, nx, ny);
  bmp := TBitmap.Create;
  try
    bmp.Assign(Image1.Picture.Bitmap);
    getBinaryImage(bmp, f);
    kaisu := thinning4(nx, ny, f);
    color.rgbtBlue := 255;
    color.rgbtGreen := 255;
    color.rgbtRed := 255;
    for j := ny - 1 downto 0 do
    begin
      Pointer(g) := bmp.ScanLine[j];
      for i := 0 to nx - 1 do
        if f[i, j] = 1 then
          g[i] := color;
    end;
    Image2.Picture.Assign(bmp);
  finally
    bmp.Free;
  end;
  Finalize(f);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  bmp1, bmp2: TBitmap;
  f, border: TRawImg;
  g: array of TRGBTriple;
  c: TRGBTriple;
  nx, ny: integer;
  i: integer;
  j: integer;
begin
  bmp1 := TBitmap.Create;
  bmp2 := TBitmap.Create;
  try
    bmp1.Assign(Image1.Picture.Bitmap);
    nx := bmp1.Width;
    ny := bmp1.Height;
    bmp2.Width := nx;
    bmp2.Height := ny;
    bmp2.Canvas.FloodFill(0, 0, clWhite, fsSurface);
    SetLength(f, nx, ny);
    SetLength(border, nx, ny);
    getBinaryImage(bmp1, f);
    for j := 0 to ny - 1 do
      for i := 0 to nx - 1 do
        border[i, j] := 0;
    getBorder4(nx, ny, f, border);
    for j := ny - 1 downto 0 do
    begin
      Pointer(g) := bmp2.ScanLine[j];
      for i := 0 to nx - 1 do
        if border[i, j] = 1 then
        begin
          c.rgbtBlue := 0;
          c.rgbtGreen := 0;
          c.rgbtRed := 0;
          g[i] := c;
        end;
    end;
    Image2.Picture.Assign(bmp2);
  finally
    bmp1.Free;
    bmp2.Free;
  end;
  Finalize(f);
  Finalize(border);
end;

procedure TForm1.chase4(i, j, code: integer; f, border: TRawImg);
var
  i1, i2, j1, j2, ist, jst: integer;
begin
  i1 := i;
  j1 := j;
  ist := i;
  jst := j;
  i2 := 0;
  j2 := 0;
  while (i2 <> ist) or (j2 <> jst) do
  begin
    case code of
      0:
        begin
          i2 := i1;
          j2 := j1 + 1;
          if f[i2, j2] = 1 then
            code := 6
          else
            code := 2;
        end;
      2:
        begin
          i2 := i1 + 1;
          j2 := j1;
          if f[i2, j2] = 1 then
            code := 0
          else
            code := 4;
          break;
        end;
      4:
        begin
          i2 := i1;
          j2 := j1 - 1;
          if f[i2, j2] = 1 then
            code := 2
          else
            code := 6;
          break;
        end;
      6:
        begin
          i2 := i1 - 1;
          j2 := j1;
          if f[i2, j2] = 1 then
            code := 4
          else
            code := 0;
          break;
        end;
    end;
    if f[i2, j2] = 1 then
    begin
      border[i2, j2] := 1;
      i1 := i2;
      j1 := j2;
    end;
  end;
end;

function TForm1.connect(a: array of integer): integer;
var
  i: integer;
begin
  a[High(a)] := a[Low(a)];
  result := 0;
  for i := 1 to 7 do
    if (a[i] = 1) and (a[i - 1] = 0) then
      inc(result);
end;

procedure TForm1.getBinaryImage(bmp: TBitmap; arr: TRawImg);
var
  i, j, wid, hei: integer;
  g: array of TRGBTriple;
  color: TRGBTriple;
begin
  wid := bmp.Width;
  hei := bmp.Height;
  for j := hei - 1 downto 0 do
  begin
    Pointer(g) := bmp.ScanLine[j];
    for i := 0 to wid - 1 do
    begin
      color := g[i];
      if (color.rgbtBlue > 200) and (color.rgbtGreen > 200) and
        (color.rgbtRed > 200) then
        arr[i, j] := 0
      else
        arr[i, j] := 1;
    end;
  end;
end;

procedure TForm1.getBorder4(nx, ny: integer; f, border: TRawImg);
var
  i, j, code: integer;
begin
  for j := 0 to ny - 1 do
    for i := 0 to nx - 1 do
      if (f[i, j] = 1) and (border[i, j] = 0) then
        if f[i - 1, j] = 0 then
        begin
          code := 0;
          chase4(i, j, code, f, border);
        end
        else if f[i + 1, j] = 0 then
        begin
          code := 4;
          chase4(i, j, code, f, border);
        end;
end;

function TForm1.thinning4(nx, ny: integer; img: TRawImg): integer;
var
  i, j, k, c: integer;
  hanten, kaisu, sum: integer;
  aa, bb: array [0 .. 7] of integer;
  gg: TRawImg;
  x: Boolean;
begin
  SetLength(gg, nx, ny);
  for j := 0 to ny - 1 do
    for i := 0 to nx - 1 do
      gg[i, j] := img[i, j];
  hanten := 1;
  kaisu := 0;
  while hanten > 0 do
  begin
    hanten := 0;
    inc(kaisu);
    for j := 1 to ny - 2 do
      for i := 1 to nx - 2 do
      begin
        if img[i, j] = 0 then
          continue;
        aa[0] := img[i + 1, j];
        aa[1] := img[i + 1, j - 1];
        aa[2] := img[i, j - 1];
        aa[3] := img[i - 1, j - 1];
        aa[4] := img[i - 1, j];
        aa[5] := img[i - 1, j + 1];
        aa[6] := img[i, j + 1];
        aa[7] := img[i + 1, j + 1];
        bb[0] := gg[i + 1, j];
        bb[1] := gg[i + 1, j - 1];
        bb[2] := gg[i, j - 1];
        bb[3] := gg[i - 1, j - 1];
        bb[4] := gg[i - 1, j];
        bb[5] := gg[i - 1, j + 1];
        bb[6] := gg[i, j + 1];
        bb[7] := gg[i + 1, j + 1];
        sum := 0;
        for k := 0 to 7 do
          inc(sum, aa[k]);
        if sum = 0 then
          gg[i, j] := 0;
        if (sum >= 3) and (sum <= 5) then
          if (connect(aa) = 1) and (connect(bb) = 1) then
          begin
            for k := 0 to 5 do
            begin
              x := true;
              if bb[k] = 0 then
              begin
                c := aa[k];
                aa[k] := 0;
                if connect(aa) <> 1 then
                begin
                  aa[k] := c;
                  x := false;
                  break;
                end;
                aa[k] := c;
              end;
            end;
            if x = true then
            begin
              gg[i, j] := 0;
              inc(hanten);
            end;
          end;
      end;
    for j := 0 to ny - 1 do
      for i := 0 to nx - 1 do
        img[i, j] := gg[i, j];
  end;
  Finalize(gg);
  result := kaisu;
end;

end.
