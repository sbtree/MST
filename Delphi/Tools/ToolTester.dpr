program ToolTester;

uses
  Forms,
<<<<<<< HEAD:Delphi/Device/MultimeterTester.dpr
  MMTester in 'MMTester.pas' {Form1},
  Multimeter in 'Multimeter.pas',
  DeviceBase in 'DeviceBase.pas';
=======
  FormTTMain in 'FormTTMain.pas' {Form1},
  ToolBase in 'ToolBase.pas';
>>>>>>> origin/master:Delphi/Tools/ToolTester.dpr

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
