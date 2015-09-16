program DeviceTester;

uses
  Forms,
  FlashRunner in 'FlashRunner.pas',
  DeviceBase in 'DeviceBase.pas',
  Multimeter in 'Multimeter.pas',
  Oscilloscope in 'Oscilloscope.pas',
  GenUtils in '..\General\GenUtils.pas',
  FormDeviceTester in 'FormDeviceTester.pas' {frmDeviceTester},
  ConnBase in '..\Comm\ConnBase.pas',
  ProtocolBase in '..\Comm\ProtocolBase.pas',
  DataBuffer in '..\General\DataBuffer.pas',
  RS232 in '..\Comm\RS232.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmDeviceTester, frmDeviceTester);
  Application.Run;
end.
