program CommTester;

uses
  Forms,
  FormCommTester in 'FormCommTester.pas' {FormCTMain},
  ConnBase in 'ConnBase.pas',
  DataBuffer in '..\General\DataBuffer.pas',
  RS232 in 'RS232.pas',
  ProtocolBase in 'ProtocolBase.pas',
  GenUtils in '..\General\GenUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormCTMain, FormCTMain);
  Application.Run;
end.
