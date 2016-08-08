program ScriptTester;

uses
  Forms,
  GenType in '..\General\GenType.pas',
  TextMessage in '..\General\TextMessage.pas',
  FuncBase in '..\Function\FuncBase.pas',
  FuncMat in '..\Function\FuncMat.pas',
  FormScriptTester in 'FormScriptTester.pas' {frmScriptTester},
  ScriptReader in 'ScriptReader.pas',
  StepDescriptor in '..\CoreData\StepDescriptor.pas',
  StepChecker in 'StepChecker.pas',
  StringPairs in '..\General\StringPairs.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmScriptTester, frmScriptTester);
  Application.Run;
end.
