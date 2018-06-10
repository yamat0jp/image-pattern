unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Ani, FMX.Layouts,
  FMX.Gestures,
  FMX.StdCtrls, FMX.Media, FMX.Objects, FMX.TabControl, FMX.Graphics, Unit2,
  FMX.TextLayout;

type
  TForm1 = class(TForm)
    ToolbarHolder: TLayout;
    ToolbarPopup: TPopup;
    ToolbarPopupAnimation: TFloatAnimation;
    ToolBar1: TToolBar;
    ToolbarApplyButton: TButton;
    ToolbarCloseButton: TButton;
    ToolbarAddButton: TButton;
    TabControl1: TTabControl;
    Panel1: TPanel;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    Image1: TImage;
    CameraComponent1: TCameraComponent;
    Image2: TImage;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    SpeedButton1: TSpeedButton;
    OpenDialog1: TOpenDialog;
    Button3: TButton;
    procedure ToolbarCloseButtonClick(Sender: TObject);
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure CameraComponent1SampleBufferReady(Sender: TObject;
      const ATime: Int64);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Image1Paint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
  private
    FGestureOrigin: TPointF;
    FGestureInProgress: Boolean;
    obj: TFourier;
    buf1, buf2: TBitmap;
    { private éŒ¾ }
    procedure ShowToolbar(AShow: Boolean);
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

procedure TForm1.Image1Paint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  i, j: Integer;
  s: TTextLayout;
begin
  j := 0;
  s := TTextLayoutManager.DefaultTextLayout.Create;
  try
    for i := 0 to obj.numRect - 1 do
    begin
      if j > 9 then
        dec(j, 10);
      s.BeginUpdate;
      s.Font.Size := 20;
      s.Color := TAlphaColors.Blue;
      s.Text := j.ToString;
      s.TopLeft := PointF(obj.ar[i].Left, obj.ar[i].Top);
      s.EndUpdate;
      s.RenderLayout(Canvas);
      inc(j);
    end;
  finally
    s.Free;
  end;
end;

procedure TForm1.ToolbarCloseButtonClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Image1.Bitmap.Assign(buf1);
  obj.minWidth := 2;
  obj.minHeight := 5;
  obj.Color := TAlphaColors.Red;
  obj.BinaryGray(Image1.Bitmap, 77, true);
  obj.DetectArea(Image1.Bitmap);
  obj.sortingPos;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if OpenDialog1.Execute = true then
  begin
    buf1.LoadFromFile(OpenDialog1.FileName);
    Image1.Bitmap.Assign(buf1);
  end;
end;

procedure TForm1.CameraComponent1SampleBufferReady(Sender: TObject;
  const ATime: Int64);
var
  bmp: TBitmap;
begin
  if TabControl1.TabIndex = 0 then
    bmp := Image2.Bitmap
  else
    bmp := Image1.Bitmap;
  CameraComponent1.SampleBufferToBitmap(bmp, true);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  obj := TFourier.Create;
  buf1 := TBitmap.Create;
  buf2 := TBitmap.Create;
  buf1.Assign(Image1.Bitmap);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  obj.Free;
  buf1.Free;
  buf2.Free;
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
      FGestureInProgress := False;
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

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  CameraComponent1.Active := SpeedButton1.IsPressed;
  if SpeedButton1.IsPressed = False then
    if TabControl1.TabIndex = 0 then
      buf2.Assign(Image2.Bitmap)
    else
      buf1.Assign(Image1.Bitmap);
end;

end.
