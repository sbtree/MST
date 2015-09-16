unit FormCommTester;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, RS232, StdCtrls;

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
var t_rs232: TConnRS232;
begin
  t_rs232 := TConnRS232.Create(self);
  t_rs232.Config('Port:1|baudrate:115200');
  t_rs232.Config('Port:1|baudrate:115200|Port:2|Port:3');
  FreeAndNil(t_rs232);
end;

end.
