program DeviceTests;
{

  Delphi DUnit-Testprojekt
  -------------------------
  Dieses Projekt enth�lt das DUnit-Test-Framework und die GUI/Konsolen-Test-Runner.
  Zum Verwenden des Konsolen-Test-Runners f�gen Sie den konditinalen Definitionen  
  in den Projektoptionen "CONSOLE_TESTRUNNER" hinzu. Ansonsten wird standardm��ig 
  der GUI-Test-Runner verwendet.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestFlashRunner in 'TestFlashRunner.pas',
  FlashRunner in 'FlashRunner.pas',
  ConnBase in '..\Comm\ConnBase.pas',
  ProtocolBase in '..\Comm\ProtocolBase.pas',
  RS232 in '..\Comm\RS232.pas',
  DataBuffer in '..\General\DataBuffer.pas',
  GenUtils in '..\General\GenUtils.pas',
  DeviceBase in 'DeviceBase.pas',
  TextMessage in '..\General\TextMessage.pas',
  DeviceConfig in '..\Config\DeviceConfig.pas',
  ConfigBase in '..\Config\ConfigBase.pas',
  StringPairs in '..\General\StringPairs.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
end.

