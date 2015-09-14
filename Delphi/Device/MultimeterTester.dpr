program MultimeterTester;

uses
  Forms,
  MMTester in 'MMTester.pas' {Form1},
  Multimeter in 'Multimeter.pas',
  DeviceBase in 'DeviceBase.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
