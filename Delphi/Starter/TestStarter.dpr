program TestStarter;

uses
  Forms,
  StarterMain in 'StarterMain.pas' {FrmStarterMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmStarterMain, FrmStarterMain);
  Application.Run;
end.
