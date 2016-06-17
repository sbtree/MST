program CommTester;

uses
  Forms,
  FormCommTester in 'FormCommTester.pas' {frmCommTester},
  DataBuffer in '..\General\DataBuffer.pas',
  RS232 in 'RS232.pas',
  GenUtils in '..\General\GenUtils.pas',
  TextMessage in '..\General\TextMessage.pas',
  ConnBase in 'ConnBase.pas',
  CAN in 'CAN.pas',
  USB in 'USB.pas',
  usbio in '..\Externals\USBIO\usbio.pas',
  Usbio_i in '..\Externals\USBIO\Usbio_i.pas',
  usbio_i_delphi in '..\Externals\USBIO\usbio_i_delphi.pas',
  usbiobuf in '..\Externals\USBIO\usbiobuf.pas',
  USBIOCOMLib_TLB in '..\Externals\USBIO\USBIOCOMLib_TLB.pas',
  usbiopipe in '..\Externals\USBIO\usbiopipe.pas',
  Usbspec in '..\Externals\USBIO\Usbspec.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmCommTester, frmCommTester);
  Application.Run;
end.
