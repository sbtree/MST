program ScriptTester;

uses
  Forms,
  FormScriptTester in 'FormScriptTester.pas' {frmScriptTester},
  ScriptReader in 'ScriptReader.pas',
  TestStep in 'TestStep.pas',
  TextMessage in '..\General\TextMessage.pas',
  StepResult in 'StepResult.pas',
  FunctionCaller in '..\Function\FunctionCaller.pas',
  GenType in '..\General\GenType.pas',
  FuncBase in '..\Function\FuncBase.pas',
  StepChecker in 'StepChecker.pas',
  StepContainer in 'StepContainer.pas',
  FuncMat in '..\Function\FuncMat.pas',
  CaseChecker in 'CaseChecker.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmScriptTester, frmScriptTester);
  Application.Run;
end.
