program ScriptTester;

uses
  Forms,
  FormScriptTester in 'FormScriptTester.pas' {frmScriptTester},
  ScriptReader in 'ScriptReader.pas',
  TestStep in 'TestStep.pas',
  ScriptRunner in 'ScriptRunner.pas',
  TextMessage in '..\General\TextMessage.pas',
  StepResult in 'StepResult.pas',
  FunctionCaller in '..\Function\FunctionCaller.pas',
  GenType in '..\General\GenType.pas',
  FunctionBase in '..\Function\FunctionBase.pas',
  StepChecker in 'StepChecker.pas',
  StepContainer in 'StepContainer.pas',
  FuncMat in '..\Function\FuncMat.pas',
  TestCase in 'TestCase.pas',
  TestSequence in 'TestSequence.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmScriptTester, frmScriptTester);
  Application.Run;
end.
