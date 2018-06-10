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
begin
  recg.select(X,Y);
  if Sender = Image1 then
    Edit4.SetFocus
  else
    recognition;
end;

procedure TForm1.recognition;
var
  dist: Single;
  i: integer;
  a, b: array of Single;
  estima: array of Single;
  n, cnt: integer;
  j: integer;
begin
  recg.recognition;
  SetLength(a, 4 * recg.numDescriptor);
  SetLength(b, 4 * recg.numDescriptor);
  SetLength(estima, recg.numEntry);
  Image3.Canvas.BeginScene;
  Image3.Canvas.FillRect(Image3.BoundsRect, 0, 0, [], 1);
  Image3.Canvas.DrawRect(Image3.BoundsRect, 0, 0, [], 1);
  with recg.bnd do
    for i := 0 to Count - 1 do
      Image3.Canvas.DrawLine(PointF(X[i - 1], Y[i - 1]), PointF(X[i], Y[i]), 1);
  Image3.Canvas.EndScene;
  cnt := 0;
  for i := 0 to recg.numDescriptor - 1 do
    with recg.model[recg.rIndex] do
    begin
      a[cnt] := coReal1[i];
      a[recg.numDescriptor + cnt] := coImag1[i];
      a[2 * recg.numDescriptor + cnt] := coReal2[i];
      a[3 * recg.numDescriptor + cnt] := coImag2[i];
      inc(cnt);
    end;
  for n := 0 to Fourier.numEntry - 1 do
  begin
    cnt := 0;
    for i := 0 to Fourier.numDescriptor - 1 do
    begin
      b[cnt] := Fourier.model[n].coImag1[i];
      b[recg.numDescriptor + cnt] := Fourier.model[n].coImag1[i];
      b[2 * recg.numDescriptor + cnt] := Fourier.model[n].coReal2[i];
      b[3 * recg.numDescriptor + cnt] := Fourier.model[n].coImag2[i];
      inc(cnt);
    end;
    if RadioButton1.IsChecked = true then
    begin
      dist := 0;
      for i := 0 to 4 * recg.numDescriptor - 1 do
        dist := dist + (a[i] - b[i]) * (a[i] - b[i]);
      estima[n] := Sqrt(dist);
    end
    else
      estima[n] := recg.Correlation(a, b, 4 * recg.numDescriptor);
  end;
  ListBox1.Items.Clear;
  for i := 0 to recg.numEntry - 1 do
  begin
    j := ListBox1.Items.Add('(' + Fourier.model[i].name + ')' +
      estima[i].ToString);
    ListBox1.ListItems[j].TagFloat := estima[i];
  end;
  if RadioButton1.IsChecked = true then
    ListBox1.Sort(SingleSortS)
  else
    ListBox1.Sort(SingleSortL);
  for i := ListBox1.Items.Count - 1 downto 5 do
    ListBox1.Items.Delete(i);
  Finalize(a);
  Finalize(b);
  Finalize(estima);
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
begin
  Fourier.numDescriptor := Edit5.Text.ToInteger;
  if Fourier.numDescriptor > 50 then
  begin
    Fourier.numDescriptor := 50;
    Edit5.Text := '50';
  end;
  Fourier.preProcess;
  thBinary := Edit3.Text.ToInteger;
  recg.minWidth := Edit1.Text.ToInteger;
  recg.minHeight := Edit2.Text.ToInteger;
  Image4.Bitmap.Assign(back);
  recg.BinaryGray(Image4.Bitmap, thBinary, true);
  recg.DetectArea(Image4.Bitmap);
  recg.numDescriptor := Fourier.numDescriptor;
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
