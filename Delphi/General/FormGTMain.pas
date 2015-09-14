unit FormGTMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DataBuffer, StdCtrls;

type
  TForm2 = class(TForm)
    btnTest: TButton;
    procedure btnTestClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.btnTestClick(Sender: TObject);
var chbuffer: TCharBuffer;  t: cardinal; n:integer;
begin
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

end.
