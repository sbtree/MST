// =============================================================================
// Module name  : $RCSfile: FuncDmm.pas,v $
// description  : The classes of script functions which are relevant to some
//                software tools (SWT), are implemented in this unit, e.g. to
//                call some extern program, to execute batch file, and so an
// Compiler     : Delphi 2007
// Author       : 2015-12-18 /bsu/
// History      :
//==============================================================================
unit FuncSwt;

interface
uses Classes, Windows, FunctionBase, TextMessage;

type
  ExecConsoleCmd = class(TFunctionBase)
  protected
    s_cmdline: string;
  protected
    procedure CaptureStdOut();
    procedure OutputInfo(outputs: TStrings; S: string);
  public
    function LoadParameter(const par: string): boolean; override;
    function Execute(): boolean; override;
  end;

implementation
uses SysUtils, StdCtrls, Forms;

procedure ExecConsoleCmd.CaptureStdOut();
var
  hReadPipe, hWritePipe: THandle;
  si: STARTUPINFO;
  lsa: SECURITY_ATTRIBUTES;
  pi: PROCESS_INFORMATION;
  cchReadBuffer: DWORD;
  ph: PChar;
  fname: PChar;
  t_outputs: TStringList;
  i: integer;
begin
  fname := allocmem(1024);
  ph := AllocMem(1024);
  lsa.nLength := sizeof(SECURITY_ATTRIBUTES);
  lsa.lpSecurityDescriptor := nil;
  lsa.bInheritHandle := True;
  if CreatePipe(hReadPipe, hWritePipe, @lsa, 0) = false then
    Exit;
  fillchar(si, sizeof(STARTUPINFO), 0);
  si.cb := sizeof(STARTUPINFO);
  si.dwFlags := (STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW);
  si.wShowWindow := SW_HIDE;
  si.hStdOutput := hWritePipe;
  si.hStdError := hWritePipe;
  StrPCopy(fname, s_cmdline);
  if CreateProcess(nil, fname, nil, nil, true, 0, nil, nil, si, pi) = False then
  begin
    FreeMem(ph);
    FreeMem(fname);
    Exit;
  end;
  t_outputs := TStringList.Create();
  CloseHandle(hWritePipe);
  while (true) do
  begin
    if not PeekNamedPipe(hReadPipe, ph, 1, @cchReadBuffer, nil, nil) then break;
    if cchReadBuffer <> 0 then
    begin
      if ReadFile(hReadPipe, ph^, 512, cchReadBuffer, nil) = false then break;
      ph[cchReadbuffer] := chr(0);
    end
    else if (WaitForSingleObject(pi.hProcess, 0) = WAIT_OBJECT_0) then break;
    Application.ProcessMessages;
    Sleep(200);
  end;
  ph[cchReadBuffer] := chr(0);
  OutputInfo(t_outputs, ph);
  if assigned(t_messenger) then begin
    for i := 0 to t_outputs.Count - 1 do t_messenger.AddMessage(t_outputs[i]);
  end;
  FreeAndNil(t_outputs);
  CloseHandle(hReadPipe);
  CloseHandle(pi.hThread);
  CloseHandle(pi.hProcess);
  FreeMem(ph);
  FreeMem(fname);
end;

procedure ExecConsoleCmd.OutputInfo(outputs: TStrings; S: string);
var
  i, p: Integer; s_line: string;
begin
  outputs.Clear;
  p := 0;
  for i := 0 to Length(S) - 1 do begin
    //\r
    if (S[i] = #10) then begin
      s_line := trim(Copy(s, p, i - p));
      if (s_line <> '') then outputs.Add(s_line);
      p := i + 1;
    end;
  end;
  s_line := trim(Copy(s, p, Length(S) - p));
  if (s_line <> '') then outputs.Add(s_line);
end;

function ExecConsoleCmd.LoadParameter(const par: string): boolean;
begin
  s_cmdline := trim(par);
  result := FileExists(par);
end;

function ExecConsoleCmd.Execute(): boolean;
begin
  result := true;
  CaptureStdOut();
end;

initialization
  Classes.RegisterClass(ExecConsoleCmd);

finalization
  Classes.UnregisterClass(ExecConsoleCmd);
end.
