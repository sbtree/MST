program ToolTester;

uses
  Forms,
  FromToolTester in 'FromToolTester.pas' {frmToolTester},
  ToolBase in 'ToolBase.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmToolTester, frmToolTester);
  Application.Run;
end.
