program ProductTester;

uses
  Forms,
  FormProdTester in 'FormProdTester.pas' {frmProdTester},
  MtxDIS2 in 'MtxDIS2.pas',
  MtxProduct in 'MtxProduct.pas',
  MtxARS2000 in 'MtxARS2000.pas',
  ConnBase in '..\Comm\ConnBase.pas',
  RS232 in '..\Comm\RS232.pas',
  ProtocolBase in '..\Comm\ProtocolBase.pas',
  DeviceBase in '..\Device\DeviceBase.pas',
  DataBuffer in '..\General\DataBuffer.pas',
  GenUtils in '..\General\GenUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmProdTester, frmProdTester);
  Application.Run;
end.
