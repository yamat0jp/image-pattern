unit Unit2;

interface

uses
  FMX.Graphics, FMX.Types, System.UITypes, System.Types, System.Math,
  System.SysUtils;

type
  TRGBData = record
    R, G, B, A: Byte;
  end;

  TMAX_PARAM = record
  const
    MAX_RECT = 50;
    MAX_ENTRY = 100;
    MAX_REPRESENTATIVE = 50;
    MAX_INPUT = MAX_REPRESENTATIVE;
    MAX_HIDDEN = 60;
    MAX_OUTPUT = MAX_ENTRY;
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
    name: string;
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
  private
    numInput, numOutput: integer;
    u, v: array [0 .. TMAX_PARAM.MAX_INPUT, 0 .. TMAX_PARAM.MAX_OUTPUT]
      of Single;
    xu: array [0 .. 2 * TMAX_PARAM.MAX_REPRESENTATIVE] of Single;
    zu: array [0 .. 9] of Single;
    teachLabels: array [0 .. TMAX_PARAM.MAX_OUTPUT] of string;
    procedure recogNN;
    procedure teachLabel(const name: array of string);
  public
    numHidden, numEntry: integer;
    eta: Single;
    xx, yy: array of array of Single;
    models: array of TModel;
    cadidateNo: array [0 .. 9] of integer;
    constructor Create;
    procedure learnBP3(batch, numRepeat: integer);
    procedure recognition(test: integer);
  end;

  TFourier = class
  type
    TBinary = array of array of integer;
  private
    FModels: array [0 .. TMAX_PARAM.MAX_ENTRY] of TModel;
    FBoundary: array [0 .. TMAX_PARAM.MAX_ENTRY] of TBoundary;
    FnumEntry: integer;
    farr: TBinary;
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
    ar: array [0 .. TMAX_PARAM.MAX_RECT - 1] of TRect;
    minWidth, minHeight: integer;
    numRect: integer;
    bnd: TBoundary;
    nn: TNueralNet;
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
    function Correlation(A, B: array of Single; cnt: integer): Single;
    procedure sortingSmall(A: array of Single; id: array of integer;
      n: integer);
    procedure sortingBig(A: array of Single; id: array of integer; n: integer);
    procedure recognition;
    function select(X,Y: Single): integer;
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
begin
  inherited;
  SetnumEntry(1);
  bnd := TBoundary.Create;
  nn := TNueralNet.Create;
  minWidth:=2;
  minHeight:=5;
  color:=TAlphaColors.Red;
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
          inc(numRect)
        else
          SetnumEntry(numEntry - 1);
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
  SetnumEntry(cnt + 1);
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

procedure TFourier.recognition;
var
  fr, fi, cc, ss: Single;
  test: TModel;
  s: TBoundary;
  wr, wi: array [0 .. TMAX_PARAM.MAX_POINT] of Single;
  i, j, n: integer;
begin
  s := boundary[rIndex];
  test := model[rIndex];
  n := s.Count;
  for i := 0 to numDescriptor - 1 do
  begin
    test.coReal1[i] := 0;
    test.coImag1[i] := 0;
    test.coReal2[i] := 0;
    test.coImag2[i] := 0;
    for j := 0 to s.Count - 1 do
    begin
      fr := s.X[j + 1] - s.X[j];
      fi := s.Y[j + 1] - s.Y[j];
      cc := cos(2 * pi * i * j / n);
      ss := sin(2 * pi * i * j / n);
      test.coReal1[i] := test.coReal1[i] + fr * cc + fi * ss;
      test.coImag1[i] := test.coImag1[i] - fr * ss + fi * cc;
      test.coReal2[i] := test.coReal2[i] + fr * cc - fi * ss;
      test.coImag2[i] := test.coImag2[i] + fr * ss + fi * cc;
    end;
    test.coReal1[i] := test.coReal1[i] / n;
    test.coImag1[i] := test.coImag1[i] / n;
    test.coReal2[i] := test.coReal2[i] / n;
    test.coImag2[i] := test.coImag2[i] / n;
  end;
  bnd.Count := s.Count;
  bnd.X[0] := s.X[0];
  bnd.Y[0] := s.Y[0];
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

function TFourier.select(X, Y: Single): integer;
var
  i: integer;
  r: TRect;
begin
  for i := 0 to numEntry - 1 do
  begin
    r := Rect(ar[i].Left, ar[i].Top, ar[i].Right, ar[i].Bottom);
    if (X > r.Left) and (X < r.Right) and (Y > r.Top) and (Y < r.Bottom) then
    begin
      rIndex := i;
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
  if Value > FnumEntry then
    for i := FnumEntry to Value - 1 do
    begin
      FModels[i] := TModel.Create;
      FBoundary[i] := TBoundary.Create;
    end
  else if Value < FnumEntry then
    for i := Value to FnumEntry - 1 do
    begin
      FModels[i].Free;
      FBoundary[i].Free;
    end;
  FnumEntry := Value;
end;

{ TModel }

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

constructor TNueralNet.Create;
begin
  inherited;
  eta := 0.3;
  numHidden := 10;
  teachLabel(['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']);
end;

procedure TNueralNet.learnBP3(batch, numRepeat: integer);
var
  i: integer;
  yy: array [0 .. TMAX_PARAM.MAX_INPUT, 0 .. TMAX_PARAM.MAX_ENTRY] of Single;
  zz: array [0 .. TMAX_PARAM.MAX_OUTPUT, 0 .. TMAX_PARAM.MAX_ENTRY] of Single;
  delta: array [0 .. TMAX_PARAM.MAX_OUTPUT, 0 .. TMAX_PARAM.MAX_ENTRY]
    of Single;
  sigma: array [0 .. TMAX_PARAM.MAX_ENTRY] of Single;
  teacher: array [0 .. TMAX_PARAM.MAX_OUTPUT, 0 .. TMAX_PARAM.MAX_ENTRY]
    of Single;
  j: integer;
  cnt: integer;
  k: integer;
  sumX, sumY, ss: Single;
begin
  numInput := models[0].numDescriptor;
  numOutput := numEntry;
  for i := 0 to batch - 1 do
    for j := 0 to numOutput - 1 do
      if teachLabels[i] = models[i].name then
        teacher[j, i] := 1.0
      else
        teacher[j, i] := 0;
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
  for i := 0 to batch - 1 do
  begin
    xx[0, i] := 1;
    yy[0, i] := 1;
    for j := 1 to numHidden - 1 do
      yy[j, i] := 0;
  end;
  cnt := 0;
  while cnt <= numRepeat do
  begin
    for i := 1 to numHidden - 1 do
      for j := 0 to numEntry - 1 do
      begin
        sumX := 0;
        for k := 0 to numInput - 1 do
          sumX := sumX + u[k, i] * xx[k, j];
        yy[i, j] := 1 / (1 + EXP(-sumX));
      end;
    for i := 0 to numOutput - 1 do
    begin
      for j := 0 to batch - 1 do
      begin
        sumY := 0;
        for k := 1 to numHidden - 1 do
          sumY := sumY + v[k, i] * yy[k, j];
        zz[i, j] := 1 / (1 + EXP(-sumY));
        delta[i, j] := (teacher[i, j] - zz[i, j]) * zz[i, j] * (1 - zz[i, j]);
      end;
      for j := 0 to numHidden - 1 do
        for k := 0 to batch - 1 do
          v[j, i] := eta * delta[i, k] * yy[j, k];
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
          u[j, i] := eta * sigma[k] * xx[j, k];
    end;
    inc(cnt);
  end;
end;

procedure TNueralNet.recognition(test: integer);
var
  s: TModel;
  i: integer;
begin
  s := models[test];
  for i := 0 to s.numDescriptor - 1 do
  begin
    xu[1 + 2 * (i - 1)] := Abs(s.coReal1[i] * s.coReal1[i] + s.coImag1[i] *
      s.coImag1[i]);
    xu[2 + 2 * (i - 1)] := Abs(s.coReal2[i] * s.coReal2[i] + s.coImag1[i] *
      s.coImag2[i]);
  end;
  recogNN;
end;

procedure TNueralNet.recogNN;
var
  xsum, ysum: Single;
  yu: array [0 .. TMAX_PARAM.MAX_HIDDEN] of Single;
  max: Single;
  i: integer;
  j: integer;
begin
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
  max := -1;
  for i := 0 to numOutput - 1 do
    if max < zu[i] then
    begin
      max := zu[i];
      cadidateNo[0] := i;
    end;
  for i := 0 to numOutput - 1 do
    if (max < zu[i]) and (zu[i] <> cadidateNo[0]) then
    begin
      max := zu[i];
      cadidateNo[1] := i;
    end;
  for i := 0 to numOutput - 1 do
    if (max < zu[i]) and (zu[i] <> cadidateNo[0]) and (zu[i] <> cadidateNo[1])
    then
    begin
      max := zu[i];
      cadidateNo[2] := i;
    end;
end;

procedure TNueralNet.teachLabel(const name: array of string);
var
  i: integer;
begin
  for i := 0 to High(name) do
    teachLabels[i] := name[i];
end;

end.