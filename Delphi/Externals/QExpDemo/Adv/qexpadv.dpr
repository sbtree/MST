program qexpadv;

uses
  Forms,
  main in 'main.pas' {Form1},
  qexp_vcl in 'qexp_vcl.pas',
  QExp in 'QExp.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
