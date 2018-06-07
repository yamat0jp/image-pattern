unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Ani, FMX.Layouts,
  FMX.Gestures, FMX.Graphics,
  FMX.TabControl, FMX.StdCtrls, System.Actions, FMX.ActnList, FMX.StdActns,
  FMX.MediaLibrary.Actions, FMX.Objects, FMX.Controls.Presentation, FMX.Edit,
  FMX.Media, Unit2, Math, FMX.ListBox;

type
  TForm1 = class(TForm)
    StyleBook1: TStyleBook;
    ToolbarHolder: TLayout;
    ToolbarPopup: TPopup;
    ToolbarPopupAnimation: TFloatAnimation;
    ToolBar1: TToolBar;
    ToolbarApplyButton: TButton;
    ToolbarCloseButton: TButton;
    ToolbarAddButton: TButton;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Label3: TLabel;
    Edit3: TEdit;
    Image1: TImage;
    Label4: TLabel;
    Edit4: TEdit;
    Label5: TLabel;
    Button3: TButton;
    Label6: TLabel;
    Edit5: TEdit;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    CameraComponent1: TCameraComponent;
    Panel1: TPanel;
    SpinEditButton1: TSpinEditButton;
    SpinEditButton2: TSpinEditButton;
    SpinEditButton3: TSpinEditButton;
    GroupBox1: TGroupBox;
    Button1: TButton;
    Button2: TButton;
    Button4: TButton;
    Label7: TLabel;
    Image2: TImage;
    Image3: TImage;
    ListBox1: TListBox;
    Image4: TImage;
    Button5: TButton;
    TabItem4: TTabItem;
    Button6: TButton;
    Button7: TButton;
    ListBox2: TListBox;
    procedure ToolbarCloseButtonClick(Sender: TObject);
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure FormDestroy(Sender: TObject);
    procedure CameraComponent1SampleBufferReady(Sender: TObject;
      const ATime: Int64);
    procedure Image4MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    FGestureOrigin: TPointF;
    FGestureInProgress: Boolean;
    bmp: TBitmap;
    buf, back: TBitmap;
    cap: Boolean;
    Fourier, recg: TFourier;
    thBinary: integer;
    { private éŒ¾ }
    procedure ShowToolbar(AShow: Boolean);
    procedure detectImage;
    procedure recognition;
    function SingleSortS(item1, item2: TFmxObject): integer;
    function SingleSortL(item1, item2: TFmxObject): integer;
  public
    { public éŒ¾ }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if Key = vkEscape then
    ShowToolbar(not ToolbarPopup.IsOpen);
end;

procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  r, rr: TRectF;
  i: integer;
  cnt: integer;
  j: integer;
  a, b: array of Single;
begin
  Image2.Canvas.BeginScene;
  for i := 0 to Fourier.MAX_RECT - 1 do
  begin
    r := RectF(Fourier.ar[i].Left, Fourier.ar[i].Top, Fourier.ar[i].Right,
      Fourier.ar[i].Bottom);
    if (X > r.Left) and (X < r.Right) and (Y > r.Top) and (Y < r.Bottom) then
    begin
      if r.Width < r.Height then
      begin
        rr.Height := r.Height;
        rr.Width := r.Width * rr.Height / r.Height;
      end
      else
      begin
        rr.Width := r.Width;
        rr.Height := r.Height * rr.Width / r.Width;
      end;
      // rr.Left := (Image2.Width - rr.Width) / 2;
      // rr.Top := (Image2.Height - rr.Height) / 2;
      Fourier.rIndex := i;
      Image2.Canvas.FillRect(Image2.BoundsRect, 0, 0, [], 1.0);
      Image2.Canvas.DrawBitmap(Image1.Bitmap, r, rr, 1.0);
      break;
    end;
  end;
  Image2.Canvas.EndScene;
  Edit4.SetFocus;
end;

procedure TForm1.Image4MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  recognition;
end;

procedure TForm1.recognition;
var
  dist: Single;
  i: integer;
  id: array of integer;
  a, b: array of Single;
  estima: array of Single;
  X, Y, wr, wi: array [0 .. TBoundary.MAX_POINT] of Single;
  n, cnt: integer;
  test: TModel;
  j: integer;
  fr, fi, cc, ss: Single;
  bnd: TBoundary;
begin
  SetLength(a, 4 * Fourier.numDescriptor);
  SetLength(b, 4 * Fourier.numDescriptor);
  SetLength(id, Fourier.numEntry);
  SetLength(estima, Fourier.numEntry);
  test := TModel.Create;
  try
    bnd := Fourier.boundary[Fourier.rIndex];
    n := bnd.Count;
    for i := 0 to Fourier.numDescriptor - 1 do
    begin
      test.coReal1[i] := 0;
      test.coImag1[i] := 0;
      test.coReal2[i] := 0;
      test.coImag2[i] := 0;
      for j := 0 to n - 1 do
      begin
        fr := bnd.X[j + 1] - bnd.X[j];
        fi := bnd.Y[j + 1] - bnd.Y[j];
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
    X[0] := bnd.X[0];
    Y[0] := bnd.Y[0];
    for i := 0 to n - 1 do
    begin
      wr[i] := 0;
      wi[i] := 0;
      for j := 1 to Fourier.numDescriptor do
      begin
        cc := cos(2 * pi * i * j / n);
        ss := sin(2 * pi * i * j / n);
        wr[i] := wr[i] + test.coReal1[j] * cc + test.coImag1[j] * ss +
          test.coReal2[j] * cc + test.coImag2[j] * ss;
        wi[i] := wi[i] + test.coReal1[j] * ss + test.coImag1[j] * cc -
          test.coReal2[j] * ss + test.coImag2[j] * cc;
      end;
    end;
    Image3.Canvas.BeginScene;
    Image3.Canvas.FillRect(Image3.BoundsRect, 0, 0, [], 1);
    Image3.Canvas.DrawRect(Image3.BoundsRect, 0, 0, [], 1);
    for i := 1 to n - 1 do
    begin
      X[i] := X[i - 1] + wr[i];
      Y[i] := Y[i - 1] + wi[i];
      Image3.Canvas.DrawLine(PointF(X[i], Y[i]), PointF(X[i - 1], Y[i - 1]), 1);
    end;
    Image3.Canvas.EndScene;
    for i := 0 to Fourier.numEntry - 1 do
      id[i] := i;
    cnt := 0;
    for i := 1 to Fourier.numDescriptor do
    begin
      a[cnt] := test.coReal1[i];
      a[Fourier.numDescriptor + cnt] := test.coImag1[i];
      a[2 * Fourier.numDescriptor + cnt] := test.coReal2[i];
      a[3 * Fourier.numDescriptor + cnt] := test.coImag2[i];
      inc(cnt);
    end;
    for n := 0 to Fourier.numEntry - 1 do
    begin
      cnt := 0;
      for i := 1 to Fourier.numDescriptor do
      begin
        b[cnt] := Fourier.model[n].coImag1[i];
        b[2 * Fourier.numDescriptor + cnt] := Fourier.model[n].coImag1[i];
        b[3 * Fourier.numDescriptor + cnt] := Fourier.model[n].coReal2[i];
        b[4 * Fourier.numDescriptor + cnt] := Fourier.model[n].coImag2[i];
        inc(cnt);
      end;
      if RadioButton1.IsChecked = true then
      begin
        dist := 0;
        for i := 0 to 4 * Fourier.numDescriptor - 1 do
          dist := dist + (a[i] - b[i]) * (a[i] - b[i]);
        estima[n] := Sqrt(dist);
      end
      else
        estima[n] := Fourier.Correlation(a, b, 4 * Fourier.numDescriptor);
    end;
    if RadioButton1.IsChecked = true then
      Fourier.sortingSmall(estima, id, Fourier.numEntry)
    else
      Fourier.sortingBig(estima, id, Fourier.numEntry);
    ListBox1.Items.Clear;
    i := 0;
    while (i < 5) and (i < Fourier.numEntry) do
    begin
      j := ListBox1.Items.Add('(' + Fourier.model[i].name + ')' +
        estima[i].ToString);
      ListBox1.ListItems[j].TagFloat := estima[i];
      inc(i);
    end;
    if RadioButton1.IsChecked = true then
      ListBox1.Sort(SingleSortS)
    else
      ListBox1.Sort(SingleSortL);
  finally
    Finalize(a);
    Finalize(b);
    Finalize(estima);
    test.Free;
  end;
end;

procedure TForm1.ToolbarCloseButtonClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  CameraComponent1.Active := true;
  cap := true;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  CameraComponent1.Active := false;
  detectImage;
  TabControl1.TabIndex := 0;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Fourier.model[Fourier.rIndex].name := Edit4.Text;
  Edit4.Text := '';
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  i, n, m: integer;
  j: integer;
  k: integer;
  fr, fi, ss, cc: Single;
begin
  Fourier.numDescriptor := Edit5.Text.ToInteger;
  for i := 0 to Fourier.numEntry - 1 do
  begin
    n := Fourier.boundary[i].Count;
    for j := 0 to Fourier.numDescriptor - 1 do
    begin
      with Fourier.model[i] do
      begin
        coReal1[j] := 0;
        coReal2[j] := 0;
        coImag1[j] := 0;
        coImag2[j] := 0;
      end;
      with Fourier.boundary[i] do
      begin
        m := Count div n;
        X[n * m] := X[0];
        Y[n * m] := Y[0];
      end;
      for k := 0 to n - 1 do
      begin
        fr := Fourier.boundary[i].X[(k + 1) * m] - Fourier.boundary[i].X[k * m];
        fi := Fourier.boundary[i].Y[(k + 1) * m] - Fourier.boundary[i].Y[k * m];
        cc := cos(2 * pi * j * k / n);
        ss := sin(2 * pi * j * k / n);
        with Fourier.model[i] do
        begin
          coReal1[j] := coReal1[j] + fr * cc + fi * ss;
          coReal2[j] := coReal2[j] - fr * ss + fi * cc;
          coImag1[j] := coImag1[j] + fr * cc - fi * ss;
          coImag2[j] := coImag2[j] + fr * ss + fi * cc;
        end;
      end;
      with Fourier.model[i] do
      begin
        coReal1[j] := coReal1[j] / n;
        coReal2[j] := coReal2[j] / n;
        coImag1[j] := coImag1[j] / n;
        coImag2[j] := coImag2[j] / n;
      end;
    end;
  end;
  thBinary := Edit3.Text.ToInteger;
  recg.minWidth := Edit1.Text.ToInteger;
  recg.minHeight := Edit2.Text.ToInteger;
  Image4.Bitmap.Assign(back);
  recg.BinaryGray(Image4.Bitmap, thBinary, true);
  recg.DetectArea(Image4.Bitmap);
  TabControl1.TabIndex := 2;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  i: integer;
begin
  ListBox2.Items.Clear;
  for i := 0 to Fourier.numEntry - 1 do
    ListBox2.Items.Add(Fourier.model[i].name + ' / ' + i.ToString);
end;

procedure TForm1.CameraComponent1SampleBufferReady(Sender: TObject;
  const ATime: Int64);
begin
  CameraComponent1.SampleBufferToBitmap(Image1.Bitmap, true);
end;

procedure TForm1.detectImage;
begin
  if cap = true then
  begin
    bmp.Assign(Image1.Bitmap);
    buf.Assign(bmp);
  end
  else
    bmp.Assign(buf);
  cap := false;
  buf.Assign(bmp);
  thBinary := Edit3.Text.ToInteger;
  Fourier.minWidth := Edit1.Text.ToInteger;
  Fourier.minHeight := Edit2.Text.ToInteger;
  Fourier.BinaryGray(bmp, thBinary, true);
  Fourier.DetectArea(bmp);
  Image1.Bitmap.Assign(bmp);
end;

function TForm1.SingleSortL(item1, item2: TFmxObject): integer;
var
  s: Single;
begin
  s := TListBoxItem(item1).TagFloat - TListBoxItem(item2).TagFloat;
  if s < 0 then
    result := 1
  else if s > 0 then
    result := -1
  else
    result := 0;
end;

function TForm1.SingleSortS(item1, item2: TFmxObject): integer;
var
  s: Single;
begin
  s := TListBoxItem(item1).TagFloat - TListBoxItem(item2).TagFloat;
  if s > 0 then
    result := 1
  else if s < 0 then
    result := -1
  else
    result := 0;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  bmp := TBitmap.Create;
  buf := TBitmap.Create;
  back := TBitmap.Create;
  cap := not Image1.Bitmap.IsEmpty;
  Fourier := TFourier.Create;
  Fourier.color := TAlphaColors.Blue;
  recg := TFourier.Create;
  recg.color := TAlphaColors.Red;
  back.Assign(Image4.Bitmap);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  bmp.Free;
  buf.Free;
  back.Free;
  Fourier.Free;
  recg.Free;
end;

procedure TForm1.FormGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
var
  DX, DY: Single;
begin
  if EventInfo.GestureID = igiPan then
  begin
    if (TInteractiveGestureFlag.gfBegin in EventInfo.Flags) and
      ((Sender = ToolbarPopup) or (EventInfo.Location.Y > (ClientHeight - 70)))
    then
    begin
      FGestureOrigin := EventInfo.Location;
      FGestureInProgress := true;
    end;

    if FGestureInProgress and (TInteractiveGestureFlag.gfEnd in EventInfo.Flags)
    then
    begin
      FGestureInProgress := false;
      DX := EventInfo.Location.X - FGestureOrigin.X;
      DY := EventInfo.Location.Y - FGestureOrigin.Y;
      if (Abs(DY) > Abs(DX)) then
        ShowToolbar(DY < 0);
    end;
  end
end;

procedure TForm1.ShowToolbar(AShow: Boolean);
begin
  ToolbarPopup.Width := ClientWidth;
  ToolbarPopup.PlacementRectangle.Rect :=
    TRectF.Create(0, ClientHeight - ToolbarPopup.Height, ClientWidth - 1,
    ClientHeight - 1);
  ToolbarPopupAnimation.StartValue := ToolbarPopup.Height;
  ToolbarPopupAnimation.StopValue := 0;

  ToolbarPopup.IsOpen := AShow;
end;

end.
