program CommTester;

uses
  Forms,
  FormCommTester in 'FormCommTester.pas' {frmCommTester},
  DataBuffer in '..\General\DataBuffer.pas',
  RS232 in 'RS232.pas',
  GenUtils in '..\General\GenUtils.pas',
  TextMessage in '..\General\TextMessage.pas',
  ConnBase in 'ConnBase.pas',
  PCAN in 'PCAN.pas',
  MtxUSB in 'MtxUSB.pas',
  USBIOCOMLib_TLB in '..\Externals\USBIO\USBIOCOMLib_TLB.pas',
  Usbspec in '..\Externals\USBIO\Usbspec.pas',
  TekUSB in 'TekUSB.pas',
  DllLoader in '..\General\DllLoader.pas',
  Serial3 in '..\Externals\Serial3.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmCommTester, frmCommTester);
  Application.Run;
end.
