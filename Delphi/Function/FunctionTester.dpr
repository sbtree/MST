program FunctionTester;

uses
  Forms,
  FormFunctionTester in 'FormFunctionTester.pas' {FormFTMain},
  FuncCaller in 'FuncCaller.pas',
  TextMessage in '..\General\TextMessage.pas',
  FuncBase in 'FuncBase.pas',
  GenType in '..\General\GenType.pas',
  FuncDmm in 'FuncDmm.pas',
  FuncDso in 'FuncDso.pas',
  FuncSwt in 'FuncSwt.pas',
  FuncMxp in 'FuncMxp.pas',
  FuncTmm in 'FuncTmm.pas',
  FuncSys in 'FuncSys.pas',
  FuncAux in 'FuncAux.pas',
  RS232 in '..\Comm\RS232.pas',
  MtxUSB in '..\Comm\MtxUSB.pas',
  PCAN in '..\Comm\PCAN.pas',
  CommBase in '..\Comm\CommBase.pas',
  USBIOCOMLib_TLB in '..\Externals\USBIO\USBIOCOMLib_TLB.pas',
  Usbspec in '..\Externals\USBIO\Usbspec.pas',
  StepGroup in '..\CoreData\StepGroup.pas',
  StepData in '..\CoreData\StepData.pas',
  ScriptReader in '..\Script\ScriptReader.pas',
  StepChecker in '..\Script\StepChecker.pas',
  StringPairs in '..\General\StringPairs.pas',
  OObjects in '..\Externals\ExprParser\OObjects.pas',
  ParseClass in '..\Externals\ExprParser\ParseClass.pas',
  ParseExpr in '..\Externals\ExprParser\ParseExpr.pas',
  GenUtils in '..\General\GenUtils.pas',
  DeviceBase in '..\Device\DeviceBase.pas',
  Multimeter in '..\Device\Multimeter.pas',
  ToolBase in '..\Tools\ToolBase.pas',
  ConfigBase in '..\Config\ConfigBase.pas',
  RelayControl in '..\Device\RelayControl.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormFTMain, FormFTMain);
  Application.Run;
end.
