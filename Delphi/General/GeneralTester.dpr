program GeneralTester;

uses
  Forms,
  FormGTMain in 'FormGTMain.pas' {Form2},
  DataBuffer in 'DataBuffer.pas',
  GenUtils in 'GenUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
