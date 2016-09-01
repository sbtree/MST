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
  FlashUpdater in 'FlashUpdater.pas',
  RS232 in '..\Comm\RS232.pas',
  MtxUSB in '..\Comm\MtxUSB.pas',
  CAN in '..\Comm\CAN.pas',
  ConnBase in '..\Comm\ConnBase.pas',
  USBIOCOMLib_TLB in '..\Externals\USBIO\USBIOCOMLib_TLB.pas',
  Usbspec in '..\Externals\USBIO\Usbspec.pas',
  StepGroup in '..\CoreData\StepGroup.pas',
  StepData in '..\CoreData\StepData.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormFTMain, FormFTMain);
  Application.Run;
end.
