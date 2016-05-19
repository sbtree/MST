unit FormGeneralTester;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DataBuffer, StdCtrls, ExtCtrls;

type
  TfrmGeneralTester = class(TForm)
    btnTest: TButton;
    imgTest: TImage;
    procedure btnTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmGeneralTester: TfrmGeneralTester;

implementation

{$R *.dfm}

procedure TfrmGeneralTester.btnTestClick(Sender: TObject);
var chbuffer: TCharBuffer;  t: cardinal; n:integer;
begin
  imgTest.Picture.LoadFromFile('N:\SW_INBE\DIS2_230_SW3.2\Kundenlogos\Logo-Lenze.bmp');
  ShowMessage(format('imgTest.ClientRect=[%d, %d, %d, %d]',
            [imgTest.ClientRect.Left, imgTest.ClientRect.Top, imgTest.ClientRect.Right, imgTest.ClientRect.Bottom]));
  //imgTest.Picture.Bitmap.Canvas.StretchDraw (imgTest.ClientRect, imgTest.Picture.Bitmap);

  chbuffer := TCharBuffer.Create();
  chbuffer.Resize(10);
  t:=GetTickCount;
  n:=chbuffer.WriteHex('19084091483758913478faf214f');
  //chbuffer.Resize(14);
  t:=GetTickCount - t;
  ShowMessage(format('n=%d, t=%d, CountUsed=%d, CountFree=%d', [n,t,chbuffer.CountUsed(), chbuffer.CountFree()]));
  ShowMessage(format('1.ReadHex=%s',[chbuffer.ReadHex()]));
  ShowMessage(format('2.ReadHex=%s',[chbuffer.ReadHex()]));
  ShowMessage(format('n=%d, t=%d, CountUsed=%d, CountFree=%d', [n,t,chbuffer.CountUsed(), chbuffer.CountFree()]));
  ShowMessage(format('HistoryHex=%s', [chbuffer.HistoryHex()]));
  t:=GetTickCount;
  n:=chbuffer.WriteStr('987654321');
  t:=GetTickCount - t;
  ShowMessage(format('n=%d, t=%d, CountUsed=%d, CountFree=%d', [n,t,chbuffer.CountUsed(), chbuffer.CountFree()]));
  ShowMessage(format('1.ReadStr=%s',[chbuffer.ReadStr()]));
  ShowMessage(format('2.ReadStr=%s',[chbuffer.ReadStr()]));
  ShowMessage(format('n=%d, t=%d, CountUsed=%d, CountFree=%d', [n,t,chbuffer.CountUsed(), chbuffer.CountFree()]));
  ShowMessage(format('HistoryStr=%s', [chbuffer.HistoryStr()]));


  for n := $30 to $39 do chbuffer.WriteChar(char(n));
  ShowMessage(format('ReadStr=%s',[chbuffer.ReadStr()]));
  ShowMessage(format('HistoryStr=%s',[chbuffer.HistoryStr()]));
  ShowMessage(format('HistoryHex=%s',[chbuffer.HistoryHex()]));

  FreeAndNil(chbuffer);
end;

procedure TfrmGeneralTester.FormCreate(Sender: TObject);
begin
  imgTest.Picture.LoadFromFile('N:\SW_INBE\DIS2_230_SW3.2\Kundenlogos\Logo_BuR.bmp');
  //imgTest.Picture.LoadFromFile('N:\SW_INBE\DIS2_230_SW3.2\Kundenlogos\Logo-Lenze.bmp');
  //imgTest.Picture.Bitmap.Canvas.StretchDraw (imgTest.ClientRect, imgTest.Picture.Bitmap);
end;

end.
