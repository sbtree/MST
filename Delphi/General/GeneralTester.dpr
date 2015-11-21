program GeneralTester;

uses
  Forms,
  DataBuffer in 'DataBuffer.pas',
  GenUtils in 'GenUtils.pas',
  FormGeneralTester in 'FormGeneralTester.pas' {frmGeneralTester},
  TextMessage in 'TextMessage.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmGeneralTester, frmGeneralTester);
  Application.Run;
end.
