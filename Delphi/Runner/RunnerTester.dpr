program RunnerTester;

uses
  Forms,
  FormRunnerTester in 'FormRunnerTester.pas' {Form2},
  TestRunner in 'TestRunner.pas',
  GenType in '..\General\GenType.pas',
  TextMessage in '..\General\TextMessage.pas',
  FuncCaller in '..\Function\FuncCaller.pas',
  FuncBase in '..\Function\FuncBase.pas',
  ScriptReader in '..\Script\ScriptReader.pas',
  StepChecker in '..\Script\StepChecker.pas',
  StepDescriptor in '..\CoreData\StepDescriptor.pas',
  StringPairs in '..\General\StringPairs.pas',
  TestDebugger in 'TestDebugger.pas',
  PseudoActor in 'PseudoActor.pas',
  LoopControl in 'LoopControl.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
