unit ConsoleIO;

interface
uses StdCtrls;

  procedure RunDosInMemo(const DosApp: string; AMemo: TMemo);
implementation
uses Windows, SysUtils;

procedure RunDosInMemo(const DosApp: string; AMemo: TMemo);
const
  {??ReadBuffer???}
  ReadBuffer = 2400;
var
  Security: TSecurityAttributes;
  ReadPipe, WritePipe: THandle;
  start: TStartUpInfo;
  ProcessInfo: TProcessInformation;
  Buffer: PChar;
  BytesRead: DWord;
  Buf: string;
  curerror: DWORD;
begin
  with Security do
  begin
    nlength := SizeOf(TSecurityAttributes);
    binherithandle := true;
    lpsecuritydescriptor := nil;
  end;
  {????????????console?????}
  if Createpipe(ReadPipe, WritePipe, @Security, 0) then
  begin
    Buffer := AllocMem(ReadBuffer + 1);
    FillChar(Start, Sizeof(Start), #0);
    {??console???????}
    with start do
    begin
      cb := SizeOf(start);
      start.lpReserved := nil;
      lpDesktop := nil;
      lpTitle := nil;
      dwX := 0;
      dwY := 0;
      dwXSize := 0;
      dwYSize := 0;
      dwXCountChars := 0;
      dwYCountChars := 0;
      dwFillAttribute := 0;
      cbReserved2 := 0;
      lpReserved2 := nil;
      hStdOutput := WritePipe; //???????????WritePipe?
      hStdInput := ReadPipe; //???????????ReadPipe?
      hStdError := WritePipe;//?????????????WritePipe?
      dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
      wShowWindow := SW_HIDE;//?????hide
    end;

    try
      {??????????console??}
      if CreateProcess(nil, PChar(DosApp), @Security, @Security, true,
        NORMAL_PRIORITY_CLASS, nil, nil, start, ProcessInfo) then
      begin
       {????????}
        WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
        {????...?????????????????????????}
        CloseHandle(WritePipe);
        Buf := '';
        {??console?????}
        repeat
          BytesRead := 0;
          ReadFile(ReadPipe, Buffer[0], ReadBuffer, BytesRead, nil);
          Buffer[BytesRead] := #0;
          OemToAnsi(Buffer, Buffer);
          Buf := Buf + string(Buffer);
        until (BytesRead < ReadBuffer);

        //SendDebug(Buf);
       {????????????Memo?????}
        while pos(#10, Buf) > 0 do
        begin
          AMemo.Lines.Add(Copy(Buf, 1, pos(#10, Buf) - 1));
          Delete(Buf, 1, pos(#10, Buf));
        end;
      end else begin
        curerror := GetLastError();
        AMemo.Lines.Add(format('Last Error: %0.8x', [curerror]));
      end;
    finally
      FreeMem(Buffer);
      CloseHandle(ProcessInfo.hProcess);
      CloseHandle(ProcessInfo.hThread);
      CloseHandle(ReadPipe);
    end;
  end;
end;
end.
