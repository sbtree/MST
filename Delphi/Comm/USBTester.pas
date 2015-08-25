unit USBTester;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UsbConnect, StdCtrls;

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
var t_usb: TUsbConnect;
begin
  t_usb := TUsbConnect.Create(self);
  if t_usb.Init then
  begin

  end;
end;

end.
