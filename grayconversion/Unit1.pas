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
type
  TTripleArray = array [WORD] of TRGBTriple;
const
  GMAX = 255;
var
  nx, ny, i, j, k, k1: integer;
  ss, mean: integer;
  hist: array [0 .. GMAX] of integer;
  nmove, cnt: array [0 .. GMAX, 0 .. GMAX] of integer;
  col: TRGBTriple;
  p: ^TTripleArray;
begin
  bmp.Assign(Image1.Picture.Graphic);
  nx := bmp.Width - 1;
  ny := bmp.Height - 1;
  mean := nx * ny div (GMAX + 1);
  for i := 0 to High(hist) do
    hist[i] := 0;
  for j := 0 to ny do
  begin
    p := bmp.ScanLine[j];
    for i := 0 to nx do
    begin
      col := p[i];
      inc(hist[col.rgbtRed]);
    end;
  end;
  for i := 0 to GMAX do
    for j := 0 to GMAX do
    begin
      nmove[i, j] := 0;
      nmove[i, i] := hist[i];
    end;
  for i := 0 to GMAX do
    if hist[i] > mean then
    begin
      ss := nmove[0, i];
      for j := i to GMAX do
      begin
        if ss < mean then
          inc(ss, nmove[j + 1, i])
        else
        begin
          nmove[j, i + 1] := ss - mean;
          nmove[j, i] := nmove[j, i] - nmove[j, i + 1];
          dec(hist[j], nmove[j, i + 1]);
          inc(hist[i + 1], nmove[j, i + 1]);
          if i <> j then
          begin
            for k := j + 1 to i do
            begin
              nmove[j, k + 1] := nmove[j, k];
              inc(hist[k + 1], nmove[j, k]);
              dec(hist[k], nmove[j, k]);
              nmove[j, k] := 0;
            end;
          end;
          break;
        end;
      end;
    end
    else
    begin
      ss := hist[i];
      for j := i + 1 to GMAX do
      begin
        inc(ss, hist[j]);
        if ss <= mean then
        begin
          nmove[j, i] := hist[j];
          inc(hist[i], hist[j]);
          nmove[j, j] := 0;
          hist[j] := 0;
        end
        else
        begin
          nmove[j, i] := hist[j] - mean;
          inc(hist[i], nmove[j, i]);
          dec(nmove[j, j], nmove[j, i]);
          hist[j] := mean;
          break;
        end;
      end;
    end;
  for i := 0 to GMAX do
    for j := 0 to GMAX do
      cnt[i, j] := 0;
  for j := 0 to ny do
  begin
    p := bmp.ScanLine[j];
    for i := 0 to nx do
    begin
      col := p[i];
      col.rgbtBlue := col.rgbtRed;
      col.rgbtGreen := col.rgbtRed;
      for k := 0 to GMAX do
      begin
        if cnt[col.rgbtRed, k] < nmove[col.rgbtRed, k] then
          break;
        if k <= GMAX then
        begin
          p[i] := col;
          inc(cnt[col.rgbtRed, k]);
        end;
      end;
    end;
  end;
  Image2.Picture.Bitmap.Assign(bmp);
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
