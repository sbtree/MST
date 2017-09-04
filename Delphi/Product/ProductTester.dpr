program ProductTester;

uses
  Forms,
  FormProdTester in 'FormProdTester.pas' {frmProdTester},
  StringPairs in '..\General\StringPairs.pas',
  MtxDownloader in 'MtxDownloader.pas',
  TextMessage in '..\General\TextMessage.pas',
  GenUtils in '..\General\GenUtils.pas',
  ProductBase in 'ProductBase.pas',
  DeviceBase in '..\Device\DeviceBase.pas',
  ProductConfig in '..\Config\ProductConfig.pas',
  ConfigBase in '..\Config\ConfigBase.pas',
  DeviceConfig in '..\Config\DeviceConfig.pas',
  UnitsSystem in 'UnitsSystem.pas',
  CommBase in '..\Comm\CommBase.pas',
  RS232 in '..\Comm\RS232.pas',
  DataBuffer in '..\General\DataBuffer.pas',
  NewProgressbar in '..\General\NewProgressbar.pas',
  TestConfig in '..\Config\TestConfig.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmProdTester, frmProdTester);
  Application.Run;
end.
