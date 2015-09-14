unit FormTTMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    btnTest: TButton;
    procedure btnTestClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
uses RegExpr;

procedure TForm1.btnTestClick(Sender: TObject);
var s_pattern : string; t_regexp: TRegExpr;
begin
  t_regexp := TRegExpr.Create;
  t_regexp.Expression := '^BAUDRATE:(\d{1,})$'; //'^BAUDRATE:([110|300|600|1200|2400|4800|9600|14400|19200|38400|56000|57600|115200|128000|256000])$';

  if t_regexp.Exec('BAUDRATE:9600') then ShowMessage('t_regexp.Match[1]='+t_regexp.Match[1])
  else ShowMessage('not matched');
  
end;

end.
