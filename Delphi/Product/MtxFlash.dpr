program MtxFlash;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  FileInfoDetails in '..\General\FileInfoDetails.pas';

var s_appver: string;

Function GetFileVersion: String;
//Var i, W: LongWord; P: Pointer; FI: PVSFixedFileInfo;
var t_fileinfo: TFileInfoDetails;
Begin
  Result := 'NoVersionInfo';
  t_fileinfo := TFileInfoDetails.Create;
  if t_fileinfo.GetFileInfoDetails(ParamStr(0)) then
    result := t_fileinfo.MetronixVersion;
  t_fileinfo.Free;

{
  i := GetFileVersionInfoSize(PChar(ParamStr(0)), W);
  If i = 0 Then Exit;
  GetMem(P, i);
  Try
    If not GetFileVersionInfo(PChar(ParamStr(0)), W, i, P)
      or not VerQueryValue(P, '\', Pointer(FI), W) Then Exit;
    Result := IntToStr(FI^.dwFileVersionMS shr 16)
      + '.' + IntToStr(FI^.dwFileVersionMS and $FFFF)
      + '.' + IntToStr(FI^.dwFileVersionLS shr 16)
      + '.' + IntToStr(FI^.dwFileVersionLS and $FFFF);
    If FI^.dwFileFlags and VS_FF_DEBUG <> 0 Then Result := Result + ' debug';
    If FI^.dwFileFlags and VS_FF_PRERELEASE <> 0 Then Result := Result + ' beta';
    If FI^.dwFileFlags and VS_FF_PRIVATEBUILD <> 0 Then Result := Result + ' private';
    If FI^.dwFileFlags and VS_FF_SPECIALBUILD <> 0 Then Result := Result + ' special';
  Finally
    FreeMem(P);
  End;}
End;

begin
  try
    s_appver := GetFileVersion;
    Writeln('Metronix Software - Servo Flash Tool ', s_appver);
    if (ParamCount <> 1) then begin
      Writeln('  Usage: ', ExtractFileName(ParamStr(0)), ' config_file.');

    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
