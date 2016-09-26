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
  StepGroup in '..\CoreData\StepGroup.pas',
  StringPairs in '..\General\StringPairs.pas',
  TestDebugger in 'TestDebugger.pas',
  PseudoActor in 'PseudoActor.pas',
  StepData in '..\CoreData\StepData.pas',
  ResultValidator in 'ResultValidator.pas',
  FuncSys in '..\Function\FuncSys.pas',
  FuncAux in '..\Function\FuncAux.pas',
  OObjects in '..\Externals\ExprParser\OObjects.pas',
  ParseClass in '..\Externals\ExprParser\ParseClass.pas',
  ParseExpr in '..\Externals\ExprParser\ParseExpr.pas',
  GenUtils in '..\General\GenUtils.pas',
  DeviceBase in '..\Device\DeviceBase.pas',
  ConnBase in '..\Comm\ConnBase.pas',
  Serial3 in '..\Externals\Serial3.pas',
  ConfigBase in '..\Config\ConfigBase.pas',
  Multimeter in '..\Device\Multimeter.pas',
  RelayControl in '..\Device\RelayControl.pas',
  RS232 in '..\Comm\RS232.pas',
  FuncDmm in '..\Function\FuncDmm.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
