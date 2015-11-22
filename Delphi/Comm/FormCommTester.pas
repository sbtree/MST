unit FormCommTester;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmCommTester = class(TForm)
    btnRS232: TButton;
    procedure btnRS232Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmCommTester: TfrmCommTester;

implementation

{$R *.dfm}

procedure TfrmCommTester.btnRS232Click(Sender: TObject);
//var t_rs232: TConnRS232; s_conf: string;
begin
{  t_rs232 := TConnRS232.Create(self);
  //t_rs232.Config('Port:1|baudrate:115200');
  s_conf := 'Port:8|baudrate:115200|Port:2|Port:3';
  if t_rs232.Config(s_conf) then ShowMessage('RS232 is configured' + ' [' + s_conf + ']')
  else ShowMessage('RS232 is NOT configured' + ' [' + s_conf + ']');
  
  if t_rs232.Connect then ShowMessage('RS232 is connected' + ' [' + s_conf + ']')
  else ShowMessage('RS232 is NOT connected' + ' [' + s_conf + ']');
  FreeAndNil(t_rs232);
  }
end;

end.
