program ScriptTester;

uses
  Forms,
  FormScriptTester in 'FormScriptTester.pas' {frmScriptTester},
  ScriptReader in 'ScriptReader.pas',
  ScriptTerm in 'ScriptTerm.pas',
  ScriptRunner in 'ScriptRunner.pas',
  TextMessage in '..\General\TextMessage.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmScriptTester, frmScriptTester);
  Application.Run;
end.
