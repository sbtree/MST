unit GTMain;

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
  t:=GetTickCount;
  n:=chbuffer.WriteStr('19084091483758913478faf214f',true);
  //chbuffer.Resize(14);
  t:=GetTickCount - t;
  ShowMessage(format('n=%d, t=%d, CountUsed=%d', [n,t,chbuffer.CountUsed()]));
  ShowMessage(chbuffer.ReadStr(true));
  t:=GetTickCount;
  n:=chbuffer.WriteStr('987654321');
  t:=GetTickCount - t;
  ShowMessage(format('n=%d, t=%d, CountUsed=%d', [n,t,chbuffer.CountUsed()]));
  ShowMessage(chbuffer.ReadStr());
  FreeAndNil(chbuffer);
end;

end.
