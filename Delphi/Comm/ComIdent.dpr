program ComIdent;

uses
  Vcl.Forms,
  ComIdentMain in 'ComIdentMain.pas' {Form3},
  Serial3 in '..\Externals\Serial3.pas',
  RS232 in 'RS232.pas',
  DataBuffer in '..\General\DataBuffer.pas',
  TextMessage in '..\General\TextMessage.pas',
  GenUtils in '..\General\GenUtils.pas',
  ConnBase in 'ConnBase.pas',
  ComIdentHelp in 'ComIdentHelp.pas' {LoopbackDlg},
  RS232Dlg in 'RS232Dlg.pas' {SerialDialog};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TLoopbackDlg, LoopbackDlg);
  Application.CreateForm(TSerialDialog, SerialDialog);
  Application.Run;
end.
