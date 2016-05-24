unit vclmain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, qlog, StdCtrls;

type
  TForm2 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
var
  I: Integer;
begin
  SetDefaultLogFile(ExtractFilePath(Application.ExeName) + 'test.log',
    1024 * 1024, False, true);
  for I := 0 to 999 do
    PostLog(llHint, 'This is log,SeqNo: %d', [I + 1]);
end;

end.
