program ScriptTester;

uses
  Forms,
  GenType in '..\General\GenType.pas',
  TextMessage in '..\General\TextMessage.pas',
  FuncBase in '..\Function\FuncBase.pas',
  FuncMat in '..\Function\FuncMat.pas',
  FormScriptTester in 'FormScriptTester.pas' {frmScriptTester},
  ScriptReader in 'ScriptReader.pas',
  StepGroup in '..\CoreData\StepGroup.pas',
  StepChecker in 'StepChecker.pas',
  StringPairs in '..\General\StringPairs.pas',
  StepData in '..\CoreData\StepData.pas',
  DeviceBase in '..\Device\DeviceBase.pas',
  ConnBase in '..\Comm\ConnBase.pas',
  ConfigBase in '..\Config\ConfigBase.pas',
  GenUtils in '..\General\GenUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmScriptTester, frmScriptTester);
  Application.Run;
end.
