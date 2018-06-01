unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtDlgs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Imaging.jpeg, System.UITypes;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Button1: TButton;
    Image2: TImage;
    OpenPictureDialog1: TOpenPictureDialog;
    Button2: TButton;
    PaintBox1: TPaintBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private éŒ¾ }
    bmp: TBitmap;
  public
    { Public éŒ¾ }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute = true then
    Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
end;

procedure TForm1.Button2Click(Sender: TObject);
const
  GMAX = 255;
var
  nx, ny, i, j, k, small, big: integer;
  kk: Byte;
  ss, mean: integer;
  hist, buffer: array [0 .. GMAX] of integer;
  nmove, cnt: array [0 .. GMAX, 0 .. GMAX] of integer;
  col: TRGBTriple;
  p: array of TRGBTriple;
begin
  bmp.Assign(Image1.Picture.Graphic);
  bmp.PixelFormat := pf24bit;
  nx := bmp.Width - 1;
  ny := bmp.Height - 1;
  mean := (nx * ny) div (GMAX + 1);
  for i := 0 to High(hist) do
    hist[i] := 0;
  for j := 0 to ny do
  begin
    Pointer(p) := bmp.ScanLine[j];
    for i := 0 to nx do
    begin
      col := p[i];
      k := (col.rgbtBlue + col.rgbtGreen + col.rgbtRed) div 3;
      inc(hist[k]);
    end;
  end;
  buffer := hist;
  for i := 0 to GMAX do
    for j := 0 to GMAX do
    begin
      nmove[i, j] := 0;
      nmove[i, i] := hist[i];
    end;
  for i := 0 to GMAX do
    if hist[i] >= mean then
    begin
      ss := nmove[0, i];
      for small := 0 to i do
      begin
        if ss < mean then
          inc(ss, nmove[small + 1, i])
        else
        begin
          nmove[small, i + 1] := ss - mean;
          nmove[small, i] := nmove[small, i] - nmove[small, i + 1];
          dec(hist[i], nmove[small, i + 1]);
          inc(hist[i + 1], nmove[small, i + 1]);
          if i <> small then
          begin
            for k := small + 1 to i do
            begin
              nmove[k, i + 1] := nmove[k, i];
              inc(hist[i + 1], nmove[k, i]);
              dec(hist[i], nmove[k, i]);
              nmove[k, i] := 0;
            end;
          end;
          break;
        end;
      end;
    end
    else
    begin
      ss := hist[i];
      for big := i + 1 to GMAX do
      begin
        inc(ss, hist[big]);
        if ss <= mean then
        begin
          nmove[big, i] := hist[big];
          inc(hist[i], hist[big]);
          nmove[big, big] := 0;
          hist[big] := 0;
        end
        else
        begin
          nmove[big, i] := mean - hist[big];
          inc(hist[i], nmove[big, i]);
          dec(nmove[big, big], nmove[big, i]);
          dec(hist[big], nmove[big, i]);
          break;
        end;
      end;
    end;
  for i := 0 to GMAX do
    for j := 0 to GMAX do
      cnt[i, j] := 0;
  for j := 0 to ny do
  begin
    Pointer(p) := bmp.ScanLine[j];
    for i := 0 to nx do
    begin
      col := p[i];
      kk := (col.rgbtRed + col.rgbtGreen + col.rgbtRed) div 3;
      for k := 0 to GMAX do
        if cnt[kk, k] < nmove[kk, k] then
        begin
          col.rgbtBlue := k;
          col.rgbtGreen := k;
          col.rgbtRed := k;
          p[i] := col;
          inc(cnt[kk, k]);
          break;
        end;
    end;
  end;
  Image2.Picture.Bitmap.Assign(bmp);
  PaintBox1.Canvas.Rectangle(0, 0, PaintBox1.Width, PaintBox1.Height);
  for i := 0 to GMAX do
    with PaintBox1 do
    begin
      Canvas.MoveTo(i, Height);
      if RadioButton1.Checked = true then
        Canvas.LineTo(i, buffer[i])
      else
        Canvas.LineTo(i, Height - hist[i]);
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  bmp := TBitmap.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  bmp.Free;
end;

end.
