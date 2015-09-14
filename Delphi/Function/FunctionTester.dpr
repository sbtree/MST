program FunctionTester;

uses
  Forms,
  FormFunctionTester in 'FormFunctionTester.pas' {FormFTMain},
  FunctionBase in 'FunctionBase.pas',
  DeviceBase in '..\Device\DeviceBase.pas',
  FlashRunner in '..\Device\FlashRunner.pas',
  FuncFlashRunner in 'FuncFlashRunner.pas',
  DataBuffer in '..\General\DataBuffer.pas',
  ConnBase in '..\Comm\ConnBase.pas',
  ProtocolBase in '..\Comm\ProtocolBase.pas',
  GenUtils in '..\General\GenUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormFTMain, FormFTMain);
  Application.Run;
end.
