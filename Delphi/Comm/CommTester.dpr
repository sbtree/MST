program CommTester;

uses
  Forms,
  FormCommTester in 'FormCommTester.pas' {frmCommTester},
  DataBuffer in '..\General\DataBuffer.pas',
  RS232 in 'RS232.pas',
  GenUtils in '..\General\GenUtils.pas',
  TextMessage in '..\General\TextMessage.pas',
  ConnBase in 'ConnBase.pas',
  CAN in 'CAN.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmCommTester, frmCommTester);
  Application.Run;
end.
