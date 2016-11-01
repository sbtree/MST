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
  FuncFR in '..\Core\Function\FuncFR.pas',
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
  MtxARS2000 in '..\Core\Product\MtxARS2000.pas',
  MtxDIS2 in '..\Core\Product\MtxDIS2.pas',
  MtxDownloader in '..\Core\Product\MtxDownloader.pas',
  MtxProduct in '..\Core\Product\MtxProduct.pas',
  ProductBase in '..\Core\Product\ProductBase.pas',
  TestDebugger in '..\Core\Runner\TestDebugger.pas',
  TestRunner in '..\Core\Runner\TestRunner.pas',
  ScriptReader in '..\Core\Script\ScriptReader.pas',
  StepChecker in '..\Core\Script\StepChecker.pas',
  ConsoleIO in '..\Core\Tool\ConsoleIO.pas',
  FlashTool in '..\Core\Tool\FlashTool.pas',
  ToolBase in '..\Core\Tool\ToolBase.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.
