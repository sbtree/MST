program CommTester;

uses
  Forms,
  FormCommTester in 'FormCommTester.pas' {frmCommTester},
  ConnBase in 'ConnBase.pas',
  DataBuffer in '..\General\DataBuffer.pas',
  RS232 in 'RS232.pas',
  ProtocolBase in 'ProtocolBase.pas',
  GenUtils in '..\General\GenUtils.pas',
  ConnectorBase in 'ConnectorBase.pas',
  TextMessage in '..\General\TextMessage.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmCommTester, frmCommTester);
  Application.Run;
end.
