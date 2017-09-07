program ProductDict;

uses
  Vcl.Forms,
  ProdDict in 'ProdDict.pas' {frmProdDict},
  ConfigBase in '..\Config\ConfigBase.pas',
  TestConfig in '..\Config\TestConfig.pas',
  StringPairs in '..\General\StringPairs.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmProdDict, frmProdDict);
  Application.Run;
end.
