program DeviceTester;

uses
  Forms,
  FormDeviceTester in 'FormDeviceTester.pas' {FormDTMain},
  FlashRunner in 'FlashRunner.pas',
  Multimeter in 'Multimeter.pas',
  Oscilloscope in 'Oscilloscope.pas',
  DataBuffer in '..\General\DataBuffer.pas',
  DeviceBase in 'DeviceBase.pas',
  ConnBase in '..\Comm\ConnBase.pas',
  ProtocolBase in '..\Comm\ProtocolBase.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormDTMain, FormDTMain);
  Application.Run;
end.
