program BootTester;

uses
  Forms,
  BootTesterMain in 'BootTesterMain.pas' {FrmBootTester},
  Serial3 in '..\Externals\Serial3.pas',
  MtxDownloader in 'MtxDownloader.pas',
  GenUtils in '..\General\GenUtils.pas',
  NewProgressbar in '..\General\NewProgressbar.pas',
  ConnBase in '..\Comm\ConnBase.pas',
  RS232 in '..\Comm\RS232.pas',
  TextMessage in '..\General\TextMessage.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmBootTester, FrmBootTester);
  Application.Run;
end.
