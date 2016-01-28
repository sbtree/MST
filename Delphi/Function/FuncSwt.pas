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

implementation
uses SysUtils;
procedure CmdExecAndView(FileName: string; memo: TMemo);
  {procedure _AddInfo(mmInfo:TMemo; S: string; var line: string);
  var
    i, p: Integer;
  begin
    if mmInfo.Lines.Count > 800 then
      mmInfo.Lines.Clear;
    //\r
    for i := 0 to Length(S) - 1 do
      if S[i] = #13 then S[i] := ' ';
    line := line + S;
    // \n
    p := Pos(#10, line);
    if p > 0 then
    begin
      // \n
      mmInfo.Lines.Add(Copy(line, 1, p - 1));
      line := Copy(line, p + 1, Length(line) - p);
    end;
  end;}
var
  hReadPipe, hWritePipe: THandle;
  si: STARTUPINFO;
  lsa: SECURITY_ATTRIBUTES;
  pi: PROCESS_INFORMATION;
  cchReadBuffer: DWORD;
  ph: PChar;
  fname: PChar;
  line: string;
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
  StrPCopy(fname, FileName);
  if CreateProcess(nil, fname, nil, nil, true, 0, nil, nil, si, pi) = False then
  begin
    FreeMem(ph);
    FreeMem(fname);
    Exit;
  end;
  CloseHandle(hWritePipe);
  while (true) do
  begin
    if not PeekNamedPipe(hReadPipe, ph, 1, @cchReadBuffer, nil, nil) then break;
    if cchReadBuffer <> 0 then
    begin
      if ReadFile(hReadPipe, ph^, 512, cchReadBuffer, nil) = false then break;
      ph[cchReadbuffer] := chr(0);
      _AddInfo(memo, ph, line);
    end
    else if (WaitForSingleObject(pi.hProcess, 0) = WAIT_OBJECT_0) then break;
    Application.ProcessMessages;
    Sleep(200);
  end;
  ph[cchReadBuffer] := chr(0);
  _AddInfo(memo, ph, line);
  CloseHandle(hReadPipe);
  CloseHandle(pi.hThread);
  CloseHandle(pi.hProcess);
  FreeMem(ph);
  FreeMem(fname);
end;

initialization
  //Classes.RegisterClass(YourSubclass);

finalization
  //Classes.UnregisterClass(YourSubclass);
end.
