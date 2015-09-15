program ScriptTester;

uses
  Forms,
  FormScriptTester in 'FormScriptTester.pas' {frmScriptTester};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmScriptTester, frmScriptTester);
  Application.Run;
end.
