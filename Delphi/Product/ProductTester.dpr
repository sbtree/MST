program ProductTester;

uses
  Forms,
  FormProdTester in 'FormProdTester.pas' {frmProdTester},
  DataBuffer in '..\General\DataBuffer.pas',
  GenUtils in '..\General\GenUtils.pas',
  ProductConfig in 'ProductConfig.pas',
  PairStrings in '..\General\PairStrings.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmProdTester, frmProdTester);
  Application.Run;
end.
