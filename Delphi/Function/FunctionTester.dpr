program FunctionTester;

uses
  Forms,
  FTMain in 'FTMain.pas' {Form4},
  FunctionBase in 'FunctionBase.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
