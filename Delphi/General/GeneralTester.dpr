program GeneralTester;

uses
  Forms,
  GTMain in 'GTMain.pas' {Form2},
  DataBuffer in 'DataBuffer.pas',
  GenUtils in 'GenUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
