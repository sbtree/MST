program FunctionTester;

uses
  Forms,
  FormFunctionTester in 'FormFunctionTester.pas' {FormFTMain},
  FunctionCaller in 'FunctionCaller.pas',
  TextMessage in '..\General\TextMessage.pas',
  FunctionBase in 'FunctionBase.pas',
  FuncFlashRunner in 'FuncFlashRunner.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormFTMain, FormFTMain);
  Application.Run;
end.
