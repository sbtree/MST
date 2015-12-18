program FunctionTester;

uses
  Forms,
  FormFunctionTester in 'FormFunctionTester.pas' {FormFTMain},
  FunctionCaller in 'FunctionCaller.pas',
  TextMessage in '..\General\TextMessage.pas',
  FunctionBase in 'FunctionBase.pas',
  GenType in '..\General\GenType.pas',
  FuncMat in 'FuncMat.pas',
  FuncDmm in 'FuncDmm.pas',
  FuncDso in 'FuncDso.pas',
  FuncSwt in 'FuncSwt.pas',
  FuncMxp in 'FuncMxp.pas',
  FuncTmm in 'FuncTmm.pas',
  FuncSys in 'FuncSys.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormFTMain, FormFTMain);
  Application.Run;
end.
