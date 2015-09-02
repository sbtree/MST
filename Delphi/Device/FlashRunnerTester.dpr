program FlashRunnerTester;

uses
  Forms,
  FRMain in 'FRMain.pas' {FRTester},
  FlashRunner in 'FlashRunner.pas',
  DeviceBase in 'DeviceBase.pas',
  DataBuffer in '..\General\DataBuffer.pas',
  Multimeter in 'Multimeter.pas',
  Oscilloscope in 'Oscilloscope.pas',
  GenUtils in '..\General\GenUtils.pas',
  InterpreterBase in 'InterpreterBase.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFRTester, FRTester);
  Application.Run;
end.
