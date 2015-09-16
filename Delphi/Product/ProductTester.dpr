program ProductTester;

uses
  Forms,
  FormProdTester in 'FormProdTester.pas' {frmProdTester},
  MtxDIS2 in 'MtxDIS2.pas',
  MtxProduct in 'MtxProduct.pas',
  MtxARS2000 in 'MtxARS2000.pas',
  ConnBase in '..\Comm\ConnBase.pas',
  RS232 in '..\Comm\RS232.pas',
  ProtocolBase in '..\Comm\ProtocolBase.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmProdTester, frmProdTester);
  Application.Run;
end.
