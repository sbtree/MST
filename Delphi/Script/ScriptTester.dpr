program ScriptTester;

uses
  Forms,
  GenType in '..\General\GenType.pas',
  TextMessage in '..\General\TextMessage.pas',
  FormScriptTester in 'FormScriptTester.pas' {frmScriptTester},
  ScriptReader in 'ScriptReader.pas',
  StepGroup in '..\CoreData\StepGroup.pas',
  StepChecker in 'StepChecker.pas',
  StringPairs in '..\General\StringPairs.pas',
  StepData in '..\CoreData\StepData.pas',
  GenUtils in '..\General\GenUtils.pas',
  FuncCaller in '..\Function\FuncCaller.pas',
  FuncBase in '..\Function\FuncBase.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmScriptTester, frmScriptTester);
  Application.Run;
end.
