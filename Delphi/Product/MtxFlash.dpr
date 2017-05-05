program MtxFlash;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  FileInfoDetails in '..\General\FileInfoDetails.pas';

Function GetFileVersion: String;
var t_fileinfo: TFileInfoDetails;
Begin
  Result := 'NoVersionInfo';
  t_fileinfo := TFileInfoDetails.Create;
  if t_fileinfo.GetFileInfoDetails(ParamStr(0)) then
    result := t_fileinfo.MetronixVersion;
  t_fileinfo.Free;
End;

begin
  try
    Writeln('Metronix Software - Servo Flash Tool ', GetFileVersion());
    if (ParamCount <> 1) then begin
      Writeln('  Usage: ', ExtractFileName(ParamStr(0)), ' config_file.');
      ExitCode := 1;
    end else
      ExitCode := 0;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
