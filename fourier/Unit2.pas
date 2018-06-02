unit Unit2;

interface

uses
  FMX.Graphics, FMX.Types, System.UITypes, System.Types;

type
  TBinary = array of array of integer;

  TRGBData = record
    R, G, B, A: Byte;
  end;

  TPreProcess = class
  const
    MAX_RECT = 50;
  private
    function labelborder8(nx, ny, X, Y, code, cnt: integer;
      f, id: TBinary): Boolean;
  public
    ar: array [0 .. MAX_RECT - 1] of TRect;
    minWidth, minHeight: integer;
    procedure BinaryGray(bmp: TBitmap; th: integer; f: TBinary;
      flagBinaryDisp: Boolean);
    function DetectArea(bmp: TBitmap; f: TBinary): integer;
    procedure sortingPos(numrect: integer);
  end;

implementation

procedure TPreProcess.BinaryGray(bmp: TBitmap; th: integer; f: TBinary;
  flagBinaryDisp: Boolean);
var
  i, k, nx, ny: integer;
  AData: TBitmapData;
  acc: array of TAlphaColor;
  color: ^TRGBData;
begin
  nx := bmp.Width;
  ny := bmp.Height;
  bmp.Map(TMapAccess.ReadWrite, AData);
  try
    Pointer(acc) := AData.Data;
    for i := 0 to nx * ny - 1 do
    begin
      color := @acc[i];
      if color^.R < th then
        k := 1
      else
        k := 0;
      if flagBinaryDisp = true then
        if k = 1 then
        begin
          color^.R := 0;
          color^.G := 0;
          color^.B := 0;
        end
        else
        begin
          color^.R := 255;
          color^.G := 255;
          color^.B := 255;
        end;
      f[i mod nx, i div nx] := k;
    end;
  finally
    bmp.Unmap(AData);
  end;
end;

function TPreProcess.DetectArea(bmp: TBitmap; f: TBinary): integer;
var
  i: integer;
  j: integer;
  nx, ny, m, n: integer;
  cnt, code: integer;
  id: TBinary;
  procedure increment;
  begin
    inc(i);
    if i >= nx - 10 then
    begin
      dec(i, nx - 20);
      inc(j);
    end;
  end;

begin
  nx := bmp.Width;
  ny := bmp.Height;
  SetLength(id, nx, ny);
  for i := 0 to nx - 1 do
    for j := 0 to ny - 1 do
      id[i, j] := 0;
  i := 10;
  j := 10;
  result := 0;
  while j < ny - 10 do
  begin
    if (f[i, j] = 1) and (id[i, j] = 0) then
    begin
      cnt := 0;
      for m := i - 1 to i + 1 do
        for n := j - 1 to j + 1 do
          if f[i, j] = 1 then
            inc(cnt);
      if cnt <= 2 then
      begin
        f[i, j] := 0;
        increment;
        continue;
      end;
      if f[i - 1, j] = 0 then
      begin
        if result >= MAX_RECT - 1 then
          break;
        ar[result].TopLeft := Point(i - 1, j - 1);
        ar[result].Width := 3;
        ar[result].Height := 3;
        code := 7;
        if labelborder8(nx, ny, i, j, code, result, f, id) = true then
          inc(result);
      end
      else if f[i + 1, j] = 0 then
      begin
        code := 3;
        labelborder8(nx, ny, i, j, code, result, f, id);
      end;
    end;
    increment;
  end;
  with bmp.Canvas do
  begin
    Stroke.color := TAlphaColors.Blue;
    StrokeThickness := 3;
    BeginScene;
    for i := 0 to MAX_RECT - 1 do
      bmp.Canvas.DrawRect(RectF(ar[i].Left, ar[i].Top, ar[i].Right,
        ar[i].Bottom), 0, 0, [], 1.0);
    EndScene;
  end;
  Finalize(id);
end;

function TPreProcess.labelborder8(nx, ny, X, Y, code, cnt: integer;
  f, id: TBinary): Boolean;
const
  edge = 10;
var
  i1, i2, j1, j2: integer;
begin
  i1 := X;
  j1 := Y;
  i2 := 0;
  j2 := 0;
  while (i2 <> X) or (j2 <> Y) do
  begin
    case code of
      0:
        begin
          i2 := i1;
          j2 := j1 + 1;
          if f[i2, j2] = 1 then
            code := 7
          else
            code := 1;
        end;
      1:
        begin
          i2 := i1 + 1;
          j2 := j1 + 1;
          if f[i2, j2] = 1 then
            code := 0
          else
            code := 2;
        end;
      2:
        begin
          i2 := i1 + 1;
          j2 := j1;
          if f[i2, j2] = 1 then
            code := 1
          else
            code := 3;
        end;
      3:
        begin
          i2 := i1 + 1;
          j2 := j1 - 1;
          if f[i2, j2] = 1 then
            code := 2
          else
            code := 4;
        end;
      4:
        begin
          i2 := i1;
          j2 := j1 - 1;
          if f[i2, j2] = 1 then
            code := 3
          else
            code := 5;
        end;
      5:
        begin
          i2 := i1 - 1;
          j2 := j1 - 1;
          if f[i2, j2] = 1 then
            code := 4
          else
            code := 6;
        end;
      6:
        begin
          i2 := i1 - 1;
          j2 := j1;
          if f[i2, j2] = 1 then
            code := 5
          else
            code := 7;
        end;
      7:
        begin
          i2 := i1 - 1;
          j2 := j1 + 1;
          if f[i2, j2] = 1 then
            code := 6
          else
            code := 0;
        end;
    end;
    if (i2 < edge) or (i2 > nx - edge) or (j2 < edge) or (j2 > ny - edge) then
    begin
      result := false;
      Exit;
    end;
    if f[i2, j2] = 1 then
    begin
      id[i2, j2] := 1;
      if i2 < ar[cnt].Left - 1 then
        ar[cnt].Left := i2 - 1
      else if i2 > ar[cnt].Right + 1 then
        ar[cnt].Right := i2 + 1;
      if j2 < ar[cnt].Top - 1 then
        ar[cnt].Top := j2 - 1
      else if j2 > ar[cnt].Bottom + 1 then
        ar[cnt].Bottom := j2 + 1;
      i1 := i2;
      j1 := j2;
    end
    else
    begin
      i2 := i1;
      j2 := j1;
    end;
  end;
  result := not((ar[cnt].Width < minWidth) or (ar[cnt].Height < minHeight));
end;

procedure TPreProcess.sortingPos(numrect: integer);
const
  eps = 10;
var
  i: integer;
  j: integer;
  center: TPoint;
  ar0: TRect;
begin
  for i := 0 to numrect do
  begin
    ar0 := ar[i];
    center := ar[i].CenterPoint;
    for j := i to numrect do
      if (center.Y > ar[j].CenterPoint.Y - eps) or
        ((Abs(center.Y - ar[j].CenterPoint.Y) < eps) and
        (center.X > ar[j].CenterPoint.X)) then
      begin
        ar[i] := ar[j];
        ar[j] := ar0;
        ar0 := ar[i];
        center := ar0.CenterPoint;
      end;
  end;
end;

end.
