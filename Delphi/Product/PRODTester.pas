unit PRODTester;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm3 = class(TForm)
    btnTest: TButton;
    procedure btnTestClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.btnTestClick(Sender: TObject);
var pch: PChar; str: string; i: integer;
begin
  str := 'ABCDE';
  pch := PChar(str);
  for i := 0 to length(str) -1 do begin
    pch := 'A';
    inc(pch);
  end;
  pch := PChar(str);
  ShowMessage(pch);
  ShowMessage(str);

  str[2]:= 'A';
  str[3]:= 'A';
  str[4]:= 'A';
  str[5]:= 'A';
  ShowMessage(pch);
  ShowMessage(str);
end;

end.
