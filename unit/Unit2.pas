unit Unit2;

interface

uses
  FMX.Graphics, FMX.Types, System.UITypes, System.Types, System.Math,
  System.SysUtils, System.Classes;

type
  TRGBData = record
    R, G, B, A: Byte;
  end;

  TMAX_PARAM = record
  const
    MAX_RECT = 50;
    MAX_ENTRY = 100;
    MAX_KIND = 50;
    MAX_REPRESENTATIVE = 50;
    MAX_INPUT = MAX_REPRESENTATIVE;
    MAX_HIDDEN = 60;
    MAX_OUTPUT = 100;
    MAX_BATCH = 50;
    MAX_POINT = 1000;
  end;

  TBoundary = class
  public
    X, Y: array [0 .. TMAX_PARAM.MAX_POINT - 1] of Single;
    Count: integer;
  end;

  TModel = class
  private
    FReal1: array [0 .. TMAX_PARAM.MAX_REPRESENTATIVE - 1] of Single;
    FReal2: array [0 .. TMAX_PARAM.MAX_REPRESENTATIVE - 1] of Single;
    FImag1: array [0 .. TMAX_PARAM.MAX_REPRESENTATIVE - 1] of Single;
    FImag2: array [0 .. TMAX_PARAM.MAX_REPRESENTATIVE - 1] of Single;
    function GetcoParam(X: integer; const Index: integer): Single;
    procedure SetcoParam(X: integer; const Index: integer; const Value: Single);
  public
    numDescriptor: integer;
    name: string[20];
    procedure Clear;
    property coReal1[X: integer]: Single index 0 read GetcoParam
      write SetcoParam;
    property coReal2[X: integer]: Single index 1 read GetcoParam
      write SetcoParam;
    property coImag1[X: integer]: Single index 2 read GetcoParam
      write SetcoParam;
    property coImag2[X: integer]: Single index 3 read GetcoParam
      write SetcoParam;
  end;

  TNueralNet = class
  protected
    numInput, numOutput: integer;
    XX: array [0 .. TMAX_PARAM.MAX_INPUT - 1, 0 .. TMAX_PARAM.MAX_ENTRY - 1]
      of Single;
    u, v: array [0 .. TMAX_PARAM.MAX_INPUT - 1, 0 .. TMAX_PARAM.MAX_OUTPUT - 1]
      of Single;
    xu: array [0 .. 2 * TMAX_PARAM.MAX_REPRESENTATIVE - 1] of Single;
    zu: array [0 .. TMAX_PARAM.MAX_KIND - 1] of Single;
    teachLabels: array [0 .. TMAX_PARAM.MAX_OUTPUT - 1] of string;
    data: array of TModel;
    batch: integer;
    procedure convert(test: TModel); overload;
    procedure convert(tests: array of TModel); overload;
  public
    numHidden: integer;
    eta: Single;
    cadidate: TStrings;
    constructor Create;
    procedure recogNN(item: TModel);
    procedure learnBP3(tests: array of TModel; numRepeat: integer);
  end;

  TFourier = class
  type
    TBinary = array of array of integer;
  protected
    FBoundary: array [0 .. TMAX_PARAM.MAX_ENTRY - 1] of TBoundary;
    farr: TBinary;
    FModels: array [0 .. TMAX_PARAM.MAX_ENTRY - 1] of TModel;
    function Getmodel(X: integer): TModel;
    function Getboundary(X: integer): TBoundary;
    function labelborder8(nx, ny, X, Y, code, cnt: integer;
      id: TBinary): Boolean;
    procedure calfourierC(model: TModel; boundary: TBoundary; cnt: integer);
  public
    numEntry: integer;
    color: TAlphaColor;
    ar: array [0 .. TMAX_PARAM.MAX_RECT - 1] of TRect;
    minWidth, minHeight: integer;
    numRect: integer;
    bnd: TBoundary;
    nn: TNueralNet;
    rIndex: integer;
    numDescriptor: integer;
    constructor Create;
    destructor Destroy; override;
    property model[X: integer]: TModel read Getmodel;
    property boundary[X: integer]: TBoundary read Getboundary;
    procedure BinaryGray(bmp: TBitmap; th: integer; flagBinaryDisp: Boolean);
    procedure DetectArea(bmp: TBitmap);
    procedure sortingPos;
    function Correlation(A, B: array of Single; cnt: integer): Single;
    procedure sortingSmall(A: array of Single; id: array of integer;
      n: integer);
    procedure sortingBig(A: array of Single; id: array of integer; n: integer);
    procedure recognition;
    function select(X, Y: Single): integer;
    procedure preProcess;
    procedure numbers;
    procedure letters;
    procedure nrecg;
    procedure saveModels(filename: string);
    procedure loadModels(filename: string);
    procedure clearModels;
    procedure learn(numRepeat: integer);
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
    Pointer(acc) := AData.data;
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

function TFourier.Correlation(A, B: array of Single; cnt: integer): Single;
var
  i: integer;
begin
  result := 0;
  for i := 0 to cnt - 1 do
    result := result + A[i] * B[i];
  result := result / (Norm(A) * Norm(B) + 0.01);
end;

constructor TFourier.Create;
var
  i: integer;
begin
  inherited;
  numEntry := 0;
  numDescriptor := 10;
  bnd := TBoundary.Create;
  nn := TNueralNet.Create;
  minWidth := 2;
  minHeight := 5;
  color := TAlphaColors.Red;
  for i := 0 to TMAX_PARAM.MAX_ENTRY - 1 do
  begin
    FModels[i] := TModel.Create;
    FBoundary[i] := TBoundary.Create;
  end;
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
        if numRect >= TMAX_PARAM.MAX_RECT - 1 then
          break;
        ar[numRect].TopLeft := Point(i - 1, j - 1);
        ar[numRect].Width := 3;
        ar[numRect].Height := 3;
        code := 7;
        if labelborder8(nx, ny, i, j, code, numRect, id) = true then
          inc(numRect);
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
    for i := 0 to numRect - 1 do
      bmp.Canvas.DrawRect(RectF(ar[i].Left - 3, ar[i].Top - 3, ar[i].Right + 3,
        ar[i].Bottom + 3), 0, 0, [], 1.0);
    EndScene;
  end;
  Finalize(id);
  numEntry := numRect;
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
  boundary[cnt].Count := 0;
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
      if ii < TMAX_PARAM.MAX_POINT then
      begin
        boundary[cnt].X[ii] := i1 - ar[cnt].Left + 5;
        boundary[cnt].Y[ii] := j1 - ar[cnt].Top + 5;
        inc(boundary[cnt].Count);
      end
      else
      begin
        result := false;
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

procedure TFourier.learn(numRepeat: integer);
begin
  nn.learnBP3(FModels, numRepeat);
end;

procedure TFourier.letters;
var
  i: integer;
begin
  nn.data := @FModels;
  nn.batch := numRect;
  nn.numOutput := 26;
  for i := 0 to nn.numOutput - 1 do
    nn.teachLabels[i] := Chr(Ord('a') + i);
end;

procedure TFourier.loadModels(filename: string);
var
  f: TFileStream;
  i: integer;
  u: Byte;
begin
  if FileExists(filename) = false then
    Exit;
  clearModels;
  f := TFileStream.Create(filename, fmOpenRead);
  try
    f.ReadBuffer(numEntry, 1);
    for i := 0 to numEntry - 1 do
    begin
      f.ReadBuffer(u, 1);
      f.Read((@FModels[i].name)^, u * SizeOf(WideChar));
      f.ReadBuffer(numDescriptor, 1);
      f.Read(FModels[i].FReal1, SizeOf(FModels[i].FReal1));
      f.Read(FModels[i].FImag1, SizeOf(FModels[i].FImag1));
      f.Read(FModels[i].FReal2, SizeOf(FModels[i].FReal2));
      f.Read(FModels[i].FImag2, SizeOf(FModels[i].FImag2));
      f.Read(FBoundary[i].X, SizeOf(FBoundary[i].X));
      f.Read(FBoundary[i].Y, SizeOf(FBoundary[i].Y));
      f.ReadBuffer(FBoundary[i].Count, SizeOf(LongInt));
    end;
  finally
    f.Free;
  end;
end;

procedure TFourier.nrecg;
var
  i: integer;
begin
  for i := 0 to numRect - 1 do
    calfourierC(model[i], boundary[i], numDescriptor);
end;

procedure TFourier.numbers;
var
  i: integer;
begin
  nn.data := @FModels;
  nn.batch := numEntry;
  numDescriptor := 8;
  nn.numOutput := 10;
  for i := 0 to nn.numOutput - 1 do
    nn.teachLabels[i] := i.ToString;
end;

procedure TFourier.preProcess;
var
  i: integer;
begin
  for i := 0 to numEntry - 1 do
    calfourierC(model[i], boundary[i], numDescriptor);
end;

procedure TFourier.recognition;
var
  test: TModel;
  s: TBoundary;
  wr, wi: array [0 .. TMAX_PARAM.MAX_POINT] of Single;
  i, j, n: integer;
  ss, cc: Single;
begin
  s := boundary[rIndex];
  test := model[rIndex];
  calfourierC(test, s, numDescriptor);
  bnd.Count := s.Count;
  bnd.X[0] := s.X[0];
  bnd.Y[0] := s.Y[0];
  n := s.Count;
  for i := 0 to s.Count - 1 do
  begin
    wr[i] := 0;
    wi[i] := 0;
    for j := 0 to numDescriptor - 1 do
    begin
      cc := cos(2 * pi * i * j / n);
      ss := sin(2 * pi * i * j / n);
      wr[i] := wr[i] + test.coReal1[j] * cc - test.coImag1[j] * ss +
        test.coReal2[j] * cc + test.coImag2[j] * ss;
      wi[i] := wi[i] + test.coReal1[j] * ss + test.coImag1[j] * cc -
        test.coReal2[j] * ss + test.coImag2[j] * cc;
    end;
  end;
  for i := 1 to bnd.Count - 1 do
  begin
    bnd.X[i] := bnd.X[i - 1] + wr[i - 1];
    bnd.Y[i] := bnd.Y[i - 1] + wi[i - 1];
  end;
end;

procedure TFourier.saveModels(filename: string);
var
  f: TFileStream;
  i: integer;
  u: Byte;
begin
  f := TFileStream.Create(filename, fmOpenWrite or fmCreate);
  try
    f.WriteBuffer(numEntry, 1);
    for i := 0 to numEntry - 1 do
    begin
      u := Length(FModels[i].name);
      f.WriteBuffer(u, 1);
      f.Write((@FModels[i].name)^, u * SizeOf(WideChar));
      f.Write(numDescriptor, 1);
      f.Write(FModels[i].FReal1, SizeOf(FModels[i].FReal1));
      f.Write(FModels[i].FImag1, SizeOf(FModels[i].FImag1));
      f.Write(FModels[i].FReal2, SizeOf(FModels[i].FReal2));
      f.Write(FModels[i].FImag2, SizeOf(FModels[i].FImag2));
      f.Write(FBoundary[i].X, SizeOf(FBoundary[i].X));
      f.Write(FBoundary[i].Y, SizeOf(FBoundary[i].Y));
      f.WriteBuffer(FBoundary[i].Count, SizeOf(LongInt));
    end;
  finally
    f.Free;
  end;
end;

function TFourier.select(X, Y: Single): integer;
var
  i: integer;
  R: TRect;
begin
  result := 0;
  for i := 0 to numEntry - 1 do
  begin
    R := Rect(ar[i].Left, ar[i].Top, ar[i].Right, ar[i].Bottom);
    if (X > R.Left) and (X < R.Right) and (Y > R.Top) and (Y < R.Bottom) then
    begin
      rIndex := i;
      result := i;
      break;
    end;
  end;
end;

procedure TFourier.sortingBig(A: array of Single; id: array of integer;
  n: integer);
var
  k, kk, i: integer;
  min: Single;
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
  eps = 30;
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
      if (center.Y > ar[j].CenterPoint.Y + eps) or
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

procedure TFourier.sortingSmall(A: array of Single; id: array of integer;
  n: integer);
var
  k, kk, i: integer;
  max: Single;
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

procedure TFourier.calfourierC(model: TModel; boundary: TBoundary;
  cnt: integer);
var
  i, j, n: integer;
  fr, fi, ss, cc: Single;
begin
  model.numDescriptor := cnt;
  n := boundary.Count;
  for i := 0 to cnt - 1 do
  begin
    model.coReal1[i] := 0;
    model.coImag1[i] := 0;
    model.coReal2[i] := 0;
    model.coImag2[i] := 0;
    for j := 0 to boundary.Count - 1 do
    begin
      fr := boundary.X[j + 1] - boundary.X[j];
      fi := boundary.Y[j + 1] - boundary.Y[j];
      cc := cos(2 * pi * i * j / n);
      ss := sin(2 * pi * i * j / n);
      model.coReal1[i] := model.coReal1[i] + fr * cc + fi * ss;
      model.coImag1[i] := model.coImag1[i] - fr * ss + fi * cc;
      model.coReal2[i] := model.coReal2[i] + fr * cc - fi * ss;
      model.coImag2[i] := model.coImag2[i] + fr * ss + fi * cc;
    end;
    model.coReal1[i] := model.coReal1[i] / n;
    model.coImag1[i] := model.coImag1[i] / n;
    model.coReal2[i] := model.coReal2[i] / n;
    model.coImag2[i] := model.coImag2[i] / n;
  end;
end;

procedure TFourier.clearModels;
var
  i: integer;
begin
  for i := 0 to TMAX_PARAM.MAX_ENTRY - 1 do
    FModels[i].Clear;
  numEntry := 0;
end;

destructor TFourier.Destroy;
var
  i: integer;
begin
  for i := 0 to TMAX_PARAM.MAX_ENTRY - 1 do
  begin
    FModels[i].Free;
    FBoundary[i].Free;
  end;
  Finalize(farr);
  bnd.Free;
  nn.Free;
  inherited;
end;

function TFourier.Getboundary(X: integer): TBoundary;
begin
  result := FBoundary[X];
end;

function TFourier.Getmodel(X: integer): TModel;
begin
  if (X >= 0) and (X < numEntry) then
    result := FModels[X]
  else
    result := nil;
end;

{ TModel }

procedure TModel.Clear;
var
  i: integer;
begin
  for i := 0 to TMAX_PARAM.MAX_REPRESENTATIVE - 1 do
  begin
    FReal1[i] := 0;
    FImag1[i] := 0;
    FReal2[i] := 0;
    FImag2[i] := 0;
  end;
  name := '';
  numDescriptor := 0;
end;

function TModel.GetcoParam(X: integer; const Index: integer): Single;
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
  const Value: Single);
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

{ TNueralNet }

procedure TNueralNet.convert(test: TModel);
var
  i: integer;
begin
  for i := 1 to test.numDescriptor do
  begin
    xu[1 + 2 * (i - 1)] := test.coReal1[i] * test.coReal1[i] + test.coImag1[i] *
      test.coImag1[i];
    xu[2 + 2 * (i - 1)] := test.coReal2[i] * test.coReal2[i] + test.coImag2[i] *
      test.coImag2[i];
  end;
end;

procedure TNueralNet.convert(tests: array of TModel);
var
  i: integer;
  j: integer;
begin
  numInput := 2 * (tests[0].numDescriptor + 1);
  for i := 0 to batch - 1 do
    for j := 1 to tests[i].numDescriptor do
    begin
      XX[1 + 2 * (j - 1), i] := tests[i].coReal1[j] * tests[i].coReal1[j] +
        tests[i].coImag1[j] * tests[i].coImag1[i];
      XX[2 + 2 * (j - 1), i] := tests[i].coReal2[j] * tests[i].coReal2[j] +
        tests[i].coImag2[j] * tests[i].coImag2[j];
    end;
end;

constructor TNueralNet.Create;
begin
  inherited;
  eta := 0.3;
  numHidden := 8;
end;

procedure TNueralNet.learnBP3(tests: array of TModel; numRepeat: integer);
var
  i: integer;
  yy: array [0 .. TMAX_PARAM.MAX_INPUT, 0 .. TMAX_PARAM.MAX_ENTRY] of Single;
  zz: array [0 .. TMAX_PARAM.MAX_OUTPUT, 0 .. TMAX_PARAM.MAX_BATCH] of Single;
  delta: array [0 .. TMAX_PARAM.MAX_OUTPUT, 0 .. TMAX_PARAM.MAX_ENTRY]
    of Single;
  sigma: array [0 .. TMAX_PARAM.MAX_ENTRY] of Single;
  teacher: array [0 .. TMAX_PARAM.MAX_OUTPUT, 0 .. TMAX_PARAM.MAX_BATCH]
    of Single;
  j: integer;
  cnt: integer;
  k: integer;
  sumX, sumY, ss: Single;
begin
  convert(tests);
  for i := 0 to batch - 1 do
    for j := 0 to numOutput - 1 do
      if teachLabels[j] = data[i].name then
        teacher[j, i] := 1.0
      else
        teacher[j, i] := 0;
  for i := 0 to batch-1 do
  begin
    xx[0,i]:=1;
    yy[0,i]:=1;
    for j := 1 to numHidden-1 do
      yy[j,i]:=0;
  end;
  for j := 0 to numHidden - 1 do
  begin
    Randomize;
    for i := 0 to numInput - 1 do
      u[i, j] := 2 * (Random - 0.5);
  end;
  for j := 0 to numOutput - 1 do
  begin
    Randomize;
    for i := 0 to numHidden - 1 do
      v[i, j] := 2 * (Random - 0.5);
  end;
  cnt := 0;
  while cnt <= numRepeat do
  begin
    for i := 1 to numHidden - 1 do
      for j := 0 to batch - 1 do
      begin
        sumX := 0;
        for k := 0 to numInput - 1 do
          sumX := sumX + u[k, i] * XX[k, j];
        yy[i, j] := 1 / (1 + EXP(-sumX));
      end;
    for i := 0 to numOutput - 1 do
    begin
      for j := 0 to batch - 1 do
      begin
        sumY := 0;
        for k := 0 to numHidden - 1 do
          sumY := sumY + v[k, i] * yy[k, j];
        zz[i, j] := 1 / (1 + EXP(-sumY));
        delta[i, j] := (teacher[i, j] - zz[i, j]) * zz[i, j] * (1 - zz[i, j]);
      end;
      for j := 0 to numHidden - 1 do
        for k := 0 to batch - 1 do
          v[j, i] := v[j, i] + eta * delta[i, k] * yy[j, k];
    end;
    for i := 1 to numHidden - 1 do
    begin
      for k := 0 to batch - 1 do
      begin
        ss := 0;
        for j := 0 to numOutput - 1 do
          ss := ss + delta[j, k] * v[i, j];
        sigma[k] := ss * yy[i, k] * (1 - yy[i, k]);
      end;
      for j := 0 to numInput - 1 do
        for k := 0 to batch - 1 do
          u[j, i] := u[j, i] + eta * sigma[k] * XX[j, k];
    end;
    inc(cnt);
  end;
end;

procedure TNueralNet.recogNN(item: TModel);
var
  xsum, ysum: Single;
  yu: array [0 .. TMAX_PARAM.MAX_HIDDEN] of Single;
  max: Single;
  i: integer;
  j: integer;
begin
  convert(item);
  for i := 0 to numHidden - 1 do
  begin
    xsum := u[0, i];
    for j := 0 to numInput - 1 do
      xsum := xsum + u[j, i] * xu[j];
    yu[i] := 1 / (1 + EXP(-xsum));
  end;
  for i := 0 to numOutput - 1 do
  begin
    ysum := v[0, i];
    for j := 0 to numHidden - 1 do
      ysum := ysum + v[j, i] * yu[j];
    zu[i] := 1 / (1 + EXP(-ysum));
  end;
  if Assigned(cadidate) = true then
  begin
    cadidate.Clear;
    for i := 1 to 3 do
      cadidate.AddObject('', nil);
    max := -1;
    for i := 0 to numOutput - 1 do
      if max < zu[i] then
      begin
        max := zu[i];
        cadidate[0] := teachLabels[i];
        cadidate.Objects[0] := Pointer(i);
      end;
    max := -1;
    for i := 0 to numOutput - 1 do
      if (max < zu[i]) and (i <> integer(cadidate.Objects[0])) then
      begin
        max := zu[i];
        cadidate[1] := teachLabels[i];
        cadidate.Objects[1] := Pointer(i);
      end;
    max := -1;
    for i := 0 to numOutput - 1 do
      if (max < zu[i]) and (i <> integer(cadidate.Objects[0])) and
        (i <> integer(cadidate.Objects[1])) then
      begin
        max := zu[i];
        cadidate[2] := teachLabels[i];
      end;
  end;
end;

end.
