program TestStarter;

uses
  Forms,
  StarterMain in 'StarterMain.pas' {FrmStarterMain},
  StartMode in 'StartMode.pas' {frmStart};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmStarterMain, FrmStarterMain);
  Application.CreateForm(TfrmStart, frmStart);
  Application.Run;
end.
