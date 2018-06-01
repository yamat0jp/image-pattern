unit Unit2;

interface

uses
  FMX.Graphics, FMX.Types, System.UITypes;

type
  TBinary = array of array of integer;

  TRGBData = record
    A, R, G, B: Byte;
  end;

procedure BinaryGray(bmp: TBitmap; th: integer; f: TBinary;
  flagBinaryDisp: Boolean);

implementation

procedure BinaryGray(bmp: TBitmap; th: integer; f: TBinary;
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

end.
