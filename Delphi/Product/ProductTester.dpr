program ProductTester;

uses
  Forms,
  FormProdTester in 'FormProdTester.pas' {frmProdTester},
  DataBuffer in '..\General\DataBuffer.pas',
  GenUtils in '..\General\GenUtils.pas',
  ProductConfig in 'ProductConfig.pas',
  StringPairs in '..\General\StringPairs.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmProdTester, frmProdTester);
  Application.Run;
end.
