program ServoTester;

uses
  Forms,
  TesterMain in '..\GUI\TesterMain.pas' {MainForm},
  TesterChild in '..\GUI\TesterChild.pas' {MDIChild},
  TesterAbout in '..\GUI\TesterAbout.pas' {AboutBox},
  FuncAux in '..\Core\Function\FuncAux.pas',
  FuncBase in '..\Core\Function\FuncBase.pas',
  FuncCaller in '..\Core\Function\FuncCaller.pas',
  FuncDmm in '..\Core\Function\FuncDmm.pas',
  FuncDso in '..\Core\Function\FuncDso.pas',
  FuncMat in '..\Core\Function\FuncMat.pas',
  FuncMxp in '..\Core\Function\FuncMxp.pas',
  FuncSwt in '..\Core\Function\FuncSwt.pas',
  FuncSys in '..\Core\Function\FuncSys.pas',
  FuncTmm in '..\Core\Function\FuncTmm.pas',
  DataBuffer in '..\Core\General\DataBuffer.pas',
  DllLoader in '..\Core\General\DllLoader.pas',
  GenType in '..\Core\General\GenType.pas',
  GenUtils in '..\Core\General\GenUtils.pas',
  StringPairs in '..\Core\General\StringPairs.pas',
  TextMessage in '..\Core\General\TextMessage.pas',
  ConnBase in '..\Core\Comm\ConnBase.pas',
  MtxUSB in '..\Core\Comm\MtxUSB.pas',
  PCAN in '..\Core\Comm\PCAN.pas',
  RS232 in '..\Core\Comm\RS232.pas',
  TekUSB in '..\Core\Comm\TekUSB.pas',
  Serial3 in '..\External\Serial3.pas',
  USBIOCOMLib_TLB in '..\External\USBIO\USBIOCOMLib_TLB.pas',
  Usbspec in '..\External\USBIO\Usbspec.pas',
  StepData in '..\Core\Data\StepData.pas',
  StepGroup in '..\Core\Data\StepGroup.pas',
  ConfigBase in '..\Core\Config\ConfigBase.pas',
  DeviceConfig in '..\Core\Config\DeviceConfig.pas',
  ProductConfig in '..\Core\Config\ProductConfig.pas',
  ProductBase in '..\Core\Product\ProductBase.pas',
  TestDebugger in '..\Core\Runner\TestDebugger.pas',
  TestRunner in '..\Core\Runner\TestRunner.pas',
  ScriptReader in '..\Core\Script\ScriptReader.pas',
  StepChecker in '..\Core\Script\StepChecker.pas',
  FlashTool in '..\Core\Tool\FlashTool.pas',
  ToolBase in '..\Core\Tool\ToolBase.pas',
  FormExpr in '..\External\ExprParser\FormExpr.pas',
  OObjects in '..\External\ExprParser\OObjects.pas',
  ParseClass in '..\External\ExprParser\ParseClass.pas',
  ParseExpr in '..\External\ExprParser\ParseExpr.pas',
  DeviceBase in '..\Core\Device\DeviceBase.pas',
  DeviceManager in '..\Core\Device\DeviceManager.pas',
  Multimeter in '..\Core\Device\Multimeter.pas',
  Oscilloscope in '..\Core\Device\Oscilloscope.pas',
  PowerSupply in '..\Core\Device\PowerSupply.pas',
  RelayControl in '..\Core\Device\RelayControl.pas',
  Thermometer in '..\Core\Device\Thermometer.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.
