program ProductTester;

uses
  Forms,
  PRODTester in 'PRODTester.pas' {Form3},
  MtxDIS2 in 'MtxDIS2.pas',
  MtxProduct in 'MtxProduct.pas',
  MtxARS2000 in 'MtxARS2000.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
