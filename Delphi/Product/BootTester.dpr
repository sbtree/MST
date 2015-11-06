program BootTester;

uses
  Forms,
  BootTesterMain in 'BootTesterMain.pas' {FrmBootTester},
  Serial3 in '..\Externals\Serial3.pas',
  MtxDownloader in 'MtxDownloader.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmBootTester, FrmBootTester);
  Application.Run;
end.
