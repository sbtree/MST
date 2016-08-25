program ProductTester;

uses
  Forms,
  FormProdTester in 'FormProdTester.pas' {frmProdTester},
  ProductConfig in 'ProductConfig.pas',
  StringPairs in '..\General\StringPairs.pas',
  MtxDownloader in 'MtxDownloader.pas',
  ConnBase in '..\Comm\ConnBase.pas',
  RS232 in '..\Comm\RS232.pas',
  TextMessage in '..\General\TextMessage.pas',
  GenUtils in '..\General\GenUtils.pas',
  NewProgressbar in '..\General\NewProgressbar.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmProdTester, frmProdTester);
  Application.Run;
end.
