unit FormCommTester;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ConnBase;

type
  TfrmCommTester = class(TForm)
    btnRS232: TButton;
    procedure btnRS232Click(Sender: TObject);
  private
    { Private-Deklarationen }
    t_conn: TConnBase;
  public
    { Public-Deklarationen }
  end;

var
  frmCommTester: TfrmCommTester;

implementation

{$R *.dfm}
uses RS232;

procedure TfrmCommTester.btnRS232Click(Sender: TObject);
var s_conf: string; s_send, s_recv: string;
begin
  t_conn := TConnRS232.Create(self);
  //t_rs232.Config('Port:1|baudrate:115200');
  s_conf := 'Port:5|baudrate:9600';
  if t_conn.Config(s_conf) then ShowMessage('RS232 is configured' + ' [' + s_conf + ']')
  else ShowMessage('RS232 is NOT configured' + ' [' + s_conf + ']');
  
  if t_conn.Connect then begin
    s_send := 'FORM:ELEM?' + Char(#13);
    t_conn.Timeout := 3000;
    if t_conn.SendStr(s_send) then begin
      t_conn.RecvStr(s_recv);
      ShowMessage('RS232 receives string:' + ' [' + s_recv + ']');
    end;
  end else ShowMessage('RS232 is NOT connected' + ' [' + s_conf + ']');

  FreeAndNil(t_conn);
{   }
end;

end.
