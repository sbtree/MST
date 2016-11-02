program ServoTestRunner;

uses
  Vcl.Forms,
  RunnerMain in '..\GUI\RunnerMain.pas' {FrmTestRunner},
  StepData in '..\Core\Data\StepData.pas',
  StepGroup in '..\Core\Data\StepGroup.pas',
  ScriptReader in '..\Core\Script\ScriptReader.pas',
  StepChecker in '..\Core\Script\StepChecker.pas',
  TestDebugger in '..\Core\Runner\TestDebugger.pas',
  TestRunner in '..\Core\Runner\TestRunner.pas',
  DataBuffer in '..\Core\General\DataBuffer.pas',
  DllLoader in '..\Core\General\DllLoader.pas',
  GenType in '..\Core\General\GenType.pas',
  GenUtils in '..\Core\General\GenUtils.pas',
  StringPairs in '..\Core\General\StringPairs.pas',
  TextMessage in '..\Core\General\TextMessage.pas',
  FuncCaller in '..\Core\Function\FuncCaller.pas',
  FuncBase in '..\Core\Function\FuncBase.pas',
  FuncAux in '..\Core\Function\FuncAux.pas',
  FuncDmm in '..\Core\Function\FuncDmm.pas',
  FuncDso in '..\Core\Function\FuncDso.pas',
  FuncMat in '..\Core\Function\FuncMat.pas',
  FuncMxp in '..\Core\Function\FuncMxp.pas',
  FuncSwt in '..\Core\Function\FuncSwt.pas',
  FuncSys in '..\Core\Function\FuncSys.pas',
  FuncTmm in '..\Core\Function\FuncTmm.pas',
  FormExpr in '..\External\ExprParser\FormExpr.pas',
  OObjects in '..\External\ExprParser\OObjects.pas',
  ParseClass in '..\External\ExprParser\ParseClass.pas',
  ParseExpr in '..\External\ExprParser\ParseExpr.pas',
  ConnBase in '..\Core\Comm\ConnBase.pas',
  MtxUSB in '..\Core\Comm\MtxUSB.pas',
  PCAN in '..\Core\Comm\PCAN.pas',
  RS232 in '..\Core\Comm\RS232.pas',
  TekUSB in '..\Core\Comm\TekUSB.pas',
  ConfigBase in '..\Core\Config\ConfigBase.pas',
  DeviceConfig in '..\Core\Config\DeviceConfig.pas',
  ProductConfig in '..\Core\Config\ProductConfig.pas',
  DeviceBase in '..\Core\Device\DeviceBase.pas',
  DeviceManager in '..\Core\Device\DeviceManager.pas',
  Multimeter in '..\Core\Device\Multimeter.pas',
  Oscilloscope in '..\Core\Device\Oscilloscope.pas',
  PowerSupply in '..\Core\Device\PowerSupply.pas',
  RelayControl in '..\Core\Device\RelayControl.pas',
  Thermometer in '..\Core\Device\Thermometer.pas',
  Serial3 in '..\External\Serial3.pas',
  USBIOCOMLib_TLB in '..\External\USBIO\USBIOCOMLib_TLB.pas',
  Usbspec in '..\External\USBIO\Usbspec.pas',
  ProductBase in '..\Core\Product\ProductBase.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmTestRunner, FrmTestRunner);
  Application.Run;
end.
