program ProductTester;

uses
  Forms,
  FormProdTester in 'FormProdTester.pas' {frmProdTester},
  ProductConfig in 'ProductConfig.pas',
  StringPairs in '..\General\StringPairs.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmProdTester, frmProdTester);
  Application.Run;
end.
