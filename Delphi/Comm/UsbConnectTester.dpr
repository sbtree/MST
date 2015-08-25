program UsbConnectTester;

uses
  Forms,
  USBTester in 'USBTester.pas' {Form2},
  UsbConnect in 'UsbConnect.pas',
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
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
