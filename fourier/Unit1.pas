unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Ani, FMX.Layouts,
  FMX.Gestures, FMX.Graphics,
  FMX.TabControl, FMX.StdCtrls, System.Actions, FMX.ActnList, FMX.StdActns,
  FMX.MediaLibrary.Actions, FMX.Objects, FMX.Controls.Presentation, FMX.Edit,
  FMX.Media;

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
    EditButton1: TEditButton;
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
    procedure CameraComponent1SampleBufferReady(Sender: TObject;
      const ATime: Int64);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Button5Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FGestureOrigin: TPointF;
    FGestureInProgress: Boolean;
    bmp: TBitmap;
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

uses Unit2;

var
  farr: TBinary;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if Key = vkEscape then
    ShowToolbar(not ToolbarPopup.IsOpen);
end;

procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  s: TBitmap;
begin
  case TabControl1.TabIndex of
    0:
      begin
        s := TBitmap.Create;
        try
          Image2.Bitmap.Assign(s);
        finally
          s.Free;
        end;
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
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  CameraComponent1.Active := false;
  bmp.Assign(Image1.Bitmap);
  Initialize(farr);
  SetLength(farr,bmp.Width,bmp.Height);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  i: integer;
  s: string;
begin
  i := 0;
  while FileExists(Edit1.Text + s) = true do
  begin
    inc(i);
    s := i.ToString + '.bmp';
  end;
  Image2.Bitmap.SaveToFile(Edit1.Text + '.bmp');
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  TabControl1.TabIndex := 2;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  BinaryGray(bmp, 77, farr, true);
  Image1.Bitmap.Assign(bmp);
end;

procedure TForm1.CameraComponent1SampleBufferReady(Sender: TObject;
  const ATime: Int64);
begin
  CameraComponent1.SampleBufferToBitmap(Image1.Bitmap, true);
end;

procedure TForm1.detectImage;
begin
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  bmp := TBitmap.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  bmp.Free;
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
