program ServoTestRunner;

uses
  Vcl.Forms,
  RunnerMain in '..\GUI\RunnerMain.pas' {FrmTestRunner},
  StepData in '..\Core\Data\StepData.pas',
  StepGroup in '..\Core\Data\StepGroup.pas',
  ScriptReader in '..\Core\Script\ScriptReader.pas',
  StepChecker in '..\Core\Script\StepChecker.pas',
  TestDebugger in '..\Core\Runner\TestDebugger.pas',
  TestRunner in '..\Core\Runner\TestRunner.pas',
  DataBuffer in '..\Core\General\DataBuffer.pas',
  DllLoader in '..\Core\General\DllLoader.pas',
  GenType in '..\Core\General\GenType.pas',
  GenUtils in '..\Core\General\GenUtils.pas',
  StringPairs in '..\Core\General\StringPairs.pas',
  TextMessage in '..\Core\General\TextMessage.pas',
  FuncCaller in '..\Core\Function\FuncCaller.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmTestRunner, FrmTestRunner);
  Application.Run;
end.
