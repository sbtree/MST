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
var chbuffer: TCharBuffer;  t: cardinal; i,n:integer;
begin
  chbuffer := TCharBuffer.Create(1024);
  t:=GetTickCount;
  n:=chbuffer.WriteStr('19084091483758913478faf214f',true);
  t:=GetTickCount - t;
  ShowMessage(format('n=%d, t=%d', [n,t]));
  ShowMessage(chbuffer.ReadStr(true));
  FreeAndNil(chbuffer);
end;

end.
