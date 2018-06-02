unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Ani, FMX.Layouts,
  FMX.Gestures, FMX.Graphics,
  FMX.TabControl, FMX.StdCtrls, System.Actions, FMX.ActnList, FMX.StdActns,
  FMX.MediaLibrary.Actions, FMX.Objects, FMX.Controls.Presentation, FMX.Edit,
  FMX.Media, Unit2;

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
    Button5: TButton;
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
    procedure Button5Click(Sender: TObject);
    procedure CameraComponent1SampleBufferReady(Sender: TObject;
      const ATime: Int64);
  private
    FGestureOrigin: TPointF;
    FGestureInProgress: Boolean;
    bmp: TBitmap;
    buf: TBitmap;
    cap: Boolean;
    obj: TPreProcess;
    farr: TBinary;
    numRect: integer;
    { private éŒ¾ }
    procedure ShowToolbar(AShow: Boolean);
    procedure detectImage;
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
  r: TRectF;
  i: integer;
begin
  case TabControl1.TabIndex of
    0:
    begin
      Image2.Canvas.BeginScene;
      for i := 0 to obj.MAX_RECT - 1 do
      begin
        r := RectF(obj.ar[i].Left, obj.ar[i].Top, obj.ar[i].right,
          obj.ar[i].bottom);
        if (X > r.Left) and (X < r.right) and (Y > r.Top) and (Y < r.bottom)
        then
          Image2.Canvas.DrawBitmap(Image1.Bitmap, r, Image2.BoundsRect, 1.0);
      end;
      Image2.Canvas.EndScene;
    end;
    2:
      begin
        if RadioButton1.IsChecked = true then
        else;
      end;
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
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  TabControl1.TabIndex := 2;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  r1, r2: TRectF;
begin
  r1:= RectF(0,0,Image2.Width,Image2.Height);
  r2.TopLeft:=PointF(300,300);
  Image1.Canvas.DrawBitmap(Image1.Bitmap,r1,r2,1);
end;

procedure TForm1.CameraComponent1SampleBufferReady(Sender: TObject;
  const ATime: Int64);
begin
  CameraComponent1.SampleBufferToBitmap(Image1.Bitmap, true);
end;

procedure TForm1.detectImage;
var
  thBinary: integer;
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
  Initialize(farr);
  SetLength(farr, bmp.Width, bmp.Height);
  thBinary := Edit3.Text.ToInteger;
  obj.minWidth := Edit1.Text.ToInteger;
  obj.minHeight := Edit2.Text.ToInteger;
  obj.BinaryGray(bmp, thBinary, farr, true);
  numRect := obj.DetectArea(bmp, farr);
  obj.sortingPos(numRect);
  Image1.Bitmap.Assign(bmp);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  bmp := TBitmap.Create;
  buf := TBitmap.Create;
  cap := not Image1.Bitmap.IsEmpty;
  obj := TPreProcess.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  bmp.Free;
  buf.Free;
  obj.Free;
  Finalize(farr);
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
