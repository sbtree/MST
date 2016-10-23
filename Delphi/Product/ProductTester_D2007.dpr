program ProductTester_D2007;

uses
  Forms,
  FormProdTester in 'FormProdTester.pas' {frmProdTester},
  StringPairs in '..\General\StringPairs.pas',
  MtxDownloader in 'MtxDownloader.pas',
  ConnBase in '..\Comm\ConnBase.pas',
  RS232 in '..\Comm\RS232.pas',
  TextMessage in '..\General\TextMessage.pas',
  GenUtils in '..\General\GenUtils.pas',
  NewProgressbar in '..\General\NewProgressbar.pas',
  ProductBase in 'ProductBase.pas',
  DeviceBase in '..\Device\DeviceBase.pas',
  ProductConfig in '..\Config\ProductConfig.pas',
  ConfigBase in '..\Config\ConfigBase.pas',
  DeviceConfig in '..\Config\DeviceConfig.pas',
  UnitsSystem in 'UnitsSystem.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmProdTester, frmProdTester);
  Application.Run;
end.
