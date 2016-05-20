program ScriptTester;

uses
  Forms,
  GenType in '..\General\GenType.pas',
  TextMessage in '..\General\TextMessage.pas',
  FuncBase in '..\Function\FuncBase.pas',
  FuncMat in '..\Function\FuncMat.pas',
  FormScriptTester in 'FormScriptTester.pas' {frmScriptTester},
  ScriptReader in 'ScriptReader.pas',
  StepDescriptor in 'StepDescriptor.pas',
  StepResult in 'StepResult.pas',
  StepChecker in 'StepChecker.pas',
  StepContainer in 'StepContainer.pas',
  PairStrings in '..\General\PairStrings.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmScriptTester, frmScriptTester);
  Application.Run;
end.
