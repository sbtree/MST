program DeviceManager;

uses
  Forms,
  DeviceBase in 'DeviceBase.pas',
  Multimeter in 'Multimeter.pas',
  Oscilloscope in 'Oscilloscope.pas',
  GenUtils in '..\General\GenUtils.pas',
  FormDeviceManager in 'FormDeviceManager.pas' {frmDeviceManager},
  ConnBase in '..\Comm\ConnBase.pas',
  ProtocolBase in '..\Comm\ProtocolBase.pas',
  DataBuffer in '..\General\DataBuffer.pas',
  RS232 in '..\Comm\RS232.pas',
  PowerSupply in 'PowerSupply.pas',
  Thermometer in 'Thermometer.pas',
  DeviceConfig in 'DeviceConfig.pas',
  CAN in '..\Comm\CAN.pas',
  MtxUSB in '..\Comm\MtxUSB.pas',
  USBIOCOMLib_TLB in '..\Externals\USBIO\USBIOCOMLib_TLB.pas',
  Usbspec in '..\Externals\USBIO\Usbspec.pas',
  TekUSB in '..\Comm\TekUSB.pas',
  TextMessage in '..\General\TextMessage.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmDeviceManager, frmDeviceManager);
  Application.Run;
end.
