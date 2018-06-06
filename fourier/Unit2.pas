unit Unit2;

interface

uses
  FMX.Graphics, FMX.Types, System.UITypes, System.Types;

type
  TBinary = array of array of integer;

  TRGBData = record
    R, G, B, A: Byte;
  end;

  TModel = class
  const
    MAX_REPRESENTATIVE = 20;
  private
    FReal1: array [0 .. MAX_REPRESENTATIVE - 1] of Double;
    FReal2: array [0 .. MAX_REPRESENTATIVE - 1] of Double;
    FImag1: array [0 .. MAX_REPRESENTATIVE - 1] of Double;
    FImag2: array [0 .. MAX_REPRESENTATIVE - 1] of Double;
    FName: string;
    function GetcoParam(X: integer; const Index: integer): Double;
    procedure SetcoParam(X: integer; const Index: integer; const Value: Double);
  public
    numDescriptor: integer;
    property coReal1[X: integer]: Double index 0 read GetcoParam
      write SetcoParam;
    property coReal2[X: integer]: Double index 1 read GetcoParam
      write SetcoParam;
    property coImag1[X: integer]: Double index 2 read GetcoParam
      write SetcoParam;
    property coImag2[X: integer]: Double index 3 read GetcoParam
      write SetcoParam;
    property name: string read FName write FName;
  end;

  TBoundary = class
  const
    MAX_POINT = 1000;
  public
    X, Y: array [0 .. MAX_POINT - 1] of Double;
    numP: integer;
    Count: integer;
    Area: integer;
  end;

  TFourier = class
  const
    MAX_RECT = 50;
    MAX_ENTRY = 100;
  private
    FModels: array [0 .. MAX_ENTRY] of TModel;
    FBoundary: array [0 .. MAX_ENTRY] of TBoundary;
    FnumEntry: integer;
    farr: TBinary;
    numRect: integer;
    function Getmodel(X: integer): TModel;
    function Getboundary(X: integer): TBoundary;
    function GetnumDescriptor: integer;
    procedure SetnumDescriptor(const Value: integer);
    procedure Clear;
    function labelborder8(nx, ny, X, Y, code, cnt: integer;
      id: TBinary): Boolean;
    procedure SetnumEntry(const Value: integer);
  public
    color: TAlphaColor;
    ar: array [0 .. MAX_RECT - 1] of TRect;
    minWidth, minHeight: integer;
    rIndex: integer;
    constructor Create;
    destructor Destroy; override;
    property model[X: integer]: TModel read Getmodel;
    property boundary[X: integer]: TBoundary read Getboundary;
    property numEntry: integer read FnumEntry write SetnumEntry;
    property numDescriptor: integer read GetnumDescriptor
      write SetnumDescriptor;
    procedure BinaryGray(bmp: TBitmap; th: integer; flagBinaryDisp: Boolean);
    procedure DetectArea(bmp: TBitmap);
    procedure sortingPos;
    function Correlation(A, B: array of Double; cnt: integer): Double;
    procedure sortingSmall(A: array of Double; id: array of integer;
      n: integer);
    procedure sortingBig(A: array of Double; id: array of integer; n: integer);
  end;

implementation

procedure TFourier.BinaryGray(bmp: TBitmap; th: integer;
  flagBinaryDisp: Boolean);
var
  i, k, nx, ny: integer;
  AData: TBitmapData;
  acc: array of TAlphaColor;
  color: ^TRGBData;
begin
  nx := bmp.Width;
  ny := bmp.Height;
  Initialize(farr);
  SetLength(farr, nx, ny);
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
      farr[i mod nx, i div nx] := k;
    end;
  finally
    bmp.Unmap(AData);
  end;
end;

function TFourier.Correlation(A, B: array of Double; cnt: integer): Double;
var
  sigA, sigB, sig: Double;
  i: integer;
begin
  sigA := 0;
  sigB := 0;
  sig := 0;
  for i := 0 to cnt - 1 do
  begin
    sigA := sigA + A[i] * A[i];
    sigB := sigB + B[i] * B[i];
    sig := sig + A[i] * B[i];
  end;
  result := sig / (Sqrt(sigA) * Sqrt(sigB));
end;

constructor TFourier.Create;
begin
  inherited;
  SetnumEntry(10);
end;

procedure TFourier.DetectArea(bmp: TBitmap);
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
  numRect := 0;
  boundary[0].Count := 0;
  while j < ny - 10 do
  begin
    if (farr[i, j] = 1) and (id[i, j] = 0) then
    begin
      cnt := 0;
      for m := i - 1 to i + 1 do
        for n := j - 1 to j + 1 do
          if farr[i, j] = 1 then
            inc(cnt);
      if cnt <= 2 then
      begin
        farr[i, j] := 0;
        increment;
        continue;
      end;
      if farr[i - 1, j] = 0 then
      begin
        if numRect >= MAX_RECT - 1 then
          break;
        ar[numRect].TopLeft := Point(i - 1, j - 1);
        ar[numRect].Width := 3;
        ar[numRect].Height := 3;
        code := 7;
        if labelborder8(nx, ny, i, j, code, numRect, id) = true then
        begin
          boundary[numRect].Area:=numRect;
          inc(numRect);
        end;
        if numRect < numEntry then
          boundary[numRect].Count := 0
        else
          break;
      end
      else if farr[i + 1, j] = 0 then
      begin
        code := 3;
        labelborder8(nx, ny, i, j, code, numRect, id);
      end;
    end;
    increment;
  end;
  with bmp.Canvas do
  begin
    Stroke.color := color;
    StrokeThickness := 3;
    BeginScene;
    for i := 0 to MAX_RECT - 1 do
      bmp.Canvas.DrawRect(RectF(ar[i].Left - 3, ar[i].Top - 3, ar[i].Right + 3,
        ar[i].Bottom + 3), 0, 0, [], 1.0);
    EndScene;
  end;
  Finalize(id);
end;

function TFourier.labelborder8(nx, ny, X, Y, code, cnt: integer;
  id: TBinary): Boolean;
const
  edge = 10;
var
  i1, i2, j1, j2, ii: integer;
begin
  i1 := X;
  j1 := Y;
  i2 := 0;
  j2 := 0;
  ii := 0;
  while (i2 <> X) or (j2 <> Y) do
  begin
    case code of
      0:
        begin
          i2 := i1;
          j2 := j1 + 1;
          if farr[i2, j2] = 1 then
            code := 7
          else
            code := 1;
        end;
      1:
        begin
          i2 := i1 + 1;
          j2 := j1 + 1;
          if farr[i2, j2] = 1 then
            code := 0
          else
            code := 2;
        end;
      2:
        begin
          i2 := i1 + 1;
          j2 := j1;
          if farr[i2, j2] = 1 then
            code := 1
          else
            code := 3;
        end;
      3:
        begin
          i2 := i1 + 1;
          j2 := j1 - 1;
          if farr[i2, j2] = 1 then
            code := 2
          else
            code := 4;
        end;
      4:
        begin
          i2 := i1;
          j2 := j1 - 1;
          if farr[i2, j2] = 1 then
            code := 3
          else
            code := 5;
        end;
      5:
        begin
          i2 := i1 - 1;
          j2 := j1 - 1;
          if farr[i2, j2] = 1 then
            code := 4
          else
            code := 6;
        end;
      6:
        begin
          i2 := i1 - 1;
          j2 := j1;
          if farr[i2, j2] = 1 then
            code := 5
          else
            code := 7;
        end;
      7:
        begin
          i2 := i1 - 1;
          j2 := j1 + 1;
          if farr[i2, j2] = 1 then
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
    if farr[i2, j2] = 1 then
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
      if ii < TBoundary.MAX_POINT then
      begin
        boundary[cnt].X[ii] := i1;
        boundary[cnt].Y[ii] := j1;
        inc(boundary[cnt].Count);
      end
      else
      begin
        result:=false;
        Exit;
      end;
      inc(ii);
    end
    else
    begin
      i2 := i1;
      j2 := j1;
    end;
  end;
  result := not((ar[cnt].Width < minWidth) or (ar[cnt].Height < minHeight));
end;

procedure TFourier.sortingBig(A: array of Double; id: array of integer;
  n: integer);
var
  k, kk, i: integer;
  min: Double;
begin
  for k := 0 to n - 1 do
  begin
    min := A[k];
    i := id[k];
    for kk := k + 1 to n - 1 do
      if min > A[k] then
      begin
        A[k] := A[kk];
        A[kk] := min;
        id[k] := id[kk];
        id[kk] := i;
      end;
  end;
end;

procedure TFourier.sortingPos;
const
  eps = 10;
var
  i: integer;
  j: integer;
  center: TPoint;
  ar0: TRect;
begin
  for i := 0 to numRect do
  begin
    ar0 := ar[i];
    center := ar[i].CenterPoint;
    for j := i to numRect do
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

procedure TFourier.sortingSmall(A: array of Double; id: array of integer;
  n: integer);
var
  k, kk, i: integer;
  max: Double;
begin
  for k := 0 to n - 1 do
  begin
    max := A[k];
    i := id[k];
    for kk := k to n - 1 do
      if max < A[k] then
      begin
        A[k] := A[kk];
        A[kk] := max;
        id[k] := id[kk];
        id[kk] := i;
      end;
  end;
end;

procedure TFourier.Clear;
var
  i: integer;
begin
  for i := 0 to FnumEntry - 1 do
  begin
    FModels[i].Free;
    FBoundary[i].Free;
  end;
  Finalize(farr);
end;

destructor TFourier.Destroy;
begin
  Clear;
  inherited;
end;

function TFourier.Getboundary(X: integer): TBoundary;
begin
  result := FBoundary[X];
end;

function TFourier.Getmodel(X: integer): TModel;
begin
  result := FModels[X];
end;

function TFourier.GetnumDescriptor: integer;
begin
  result := FModels[0].numDescriptor;
end;

procedure TFourier.SetnumDescriptor(const Value: integer);
var
  i: integer;
begin
  for i := 0 to FnumEntry - 1 do
    FModels[i].numDescriptor := Value;
end;

procedure TFourier.SetnumEntry(const Value: integer);
var
  i: integer;
begin
  FnumEntry := Value;
  Clear;
  for i := 0 to Value - 1 do
  begin
    FModels[i] := TModel.Create;
    FBoundary[i] := TBoundary.Create;
  end;
end;

{ TModel }

function TModel.GetcoParam(X: integer; const Index: integer): Double;
begin
  result := 0;
  case Index of
    0:
      result := FReal1[X];
    1:
      result := FReal2[X];
    2:
      result := FImag1[X];
    3:
      result := FImag2[X];
  end;
end;

procedure TModel.SetcoParam(X: integer; const Index: integer;
  const Value: Double);
begin
  case Index of
    0:
      FReal1[X] := Value;
    1:
      FReal2[X] := Value;
    2:
      FImag1[X] := Value;
    3:
      FImag2[X] := Value;
  end;
end;

end.
