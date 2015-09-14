program ProductTester;

uses
  Forms,
  FormProdTester in 'FormProdTester.pas' {FormPTMain},
  MtxDIS2 in 'MtxDIS2.pas',
  MtxProduct in 'MtxProduct.pas',
  MtxARS2000 in 'MtxARS2000.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormPTMain, FormPTMain);
  Application.Run;
end.
