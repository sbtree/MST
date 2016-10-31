program ServoTestRunner;

uses
  Vcl.Forms,
  RunnerMain in '..\GUI\RunnerMain.pas' {FrmTestRunner};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmTestRunner, FrmTestRunner);
  Application.Run;
end.
