program FlashRunnerTester;

uses
  Forms,
  FRMain in 'FRMain.pas' {FRTester},
  FlashRunner in 'FlashRunner.pas',
  DeviceBase in 'DeviceBase.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFRTester, FRTester);
  Application.Run;
end.
