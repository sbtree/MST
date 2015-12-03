unit MtxDownloader;

interface
uses Serial3, Classes, Controls;
type
  EBootState = (
                BS_UNKNOWN,
                BS_MTLBL_ONLY,  //only Motorola S-Record Loader exists on the board (virginal board)
                BS_MTLBL_UPD,   //Motorola S-Record Loader + Metronix BL_Updater
                BS_MTLBL_APP,   //Motorola S-Record Loader + Metronix FW
                BS_MTXBL_ONLY,  //only Metronix Boot Loader exists on the board
                BS_MTXBL_UPD,   //Metronix Boot Loader + Metronix BL_Updater
                BS_MTXBL_APP,   //Metronix Boot Loader + Metronix FW
                BS_XBL_UPD      //one Boot Loader (of s-record loader and metronix boot loader) is just updated through BL-Updater
                );
  EDownloadProtocol = (
                DP_UNDEFINED,//undefined download protocol 
                DP_MOTOROLA, //download protocol of Motorola S-Record loader
                DP_METRONIX  //download protocol of metronix boot loader
                );

  TDownloader = class
  protected
    e_dlprotocol: EDownloadProtocol; //to save protocol
    s_blmessage, s_fwmessage: string; //to save switch-on message of boot loader and firmware
    s_lastmsg: string; //to save information in the last action
    s_lastfile: string; //to save file path (an s-record file), which is given in function Download last time;
    t_srecords: TStringList; //to save s-records, which is loaded last time;
    t_progress: TControl; //TProgressBar; //to illustrate the progress of the download
    t_messager: TStrings; //to output messages
    b_dlcancel: boolean;  //to cancel downloading manually
  protected
    procedure UpdateStartMessage(); virtual; abstract;
    function  ResetDevice(const cmd: string; const tend: cardinal; const bmsg: boolean = true): boolean; virtual; abstract;
    function  EnterService(const cmd: string): boolean; virtual; abstract;
    procedure InitProgressBar(); virtual;
    procedure UpdateProgressBar(const val: integer); virtual;

  public
    constructor Create();
    destructor Destroy; override;

    property Cancel : boolean read b_dlcancel write b_dlcancel;
    property Messager: TStrings write t_messager;
    property ProgressBar: TControl write t_progress;
    function TestApp(): boolean; virtual; abstract;
    function GetBootState(const cmd: string): EBootState; virtual; abstract;
    function Download(const cmd, fname: string): boolean; virtual; abstract;
  end;

  TComDownloader = class(TDownloader)
  protected
    t_ser: TSerial;
  protected
    function  SendStr(const str: string; const bprint: boolean = true): boolean;
    function  RecvStr(var str: string; const bwait: boolean = true): integer;
    function  RecvStrTimeout(var str: string; const tend: cardinal): integer;
    function  RecvStrInterval(var str: string; const tend: cardinal; const interv: cardinal = 3000): integer;
    function  RecvStrExpected(var str: string; const exstr: string; tend: cardinal; const bcase: boolean = false): integer;
    function  WaitForReading(const tend: cardinal): boolean;
    procedure ResetSerial();

    procedure UpdateStartMessage(); override;
    function  TryBootMessageMTL(var msg: string): integer;
    function  ResetDevice(const cmd: string; const tend: cardinal; const bmsg: boolean = true): boolean; override;
    function  EnterService(const cmd: string): boolean; override;
  public
    property ComObj: TSerial read t_ser write t_ser;
  public
    function TestApp(): boolean; override;
    function GetBootState(const cmd: string): EBootState; override;
    function Download(const cmd, fname: string): boolean; override;
  end;

  var t_comdownloader: TComDownloader;
implementation
uses Windows, SysUtils, GenUtils, Forms, TypInfo, ComCtrls, NewProgressBar;

const
  CSTR_BOOTQUE: string = 'BOOT?';
  CSTR_SERVICE: string = 'service';
  CSTR_APPLICATION: string = 'APPLICATION';
  CSTR_ERROR: string = 'ERROR!';
  CSTR_UNKNOWNCMD: string = 'UNKNOWN COMMAND';
  CSTR_CHECKSUM: string = 'CHECKSUM';
  CSTR_DONE: string = 'DONE.';
  CSTR_WAITING: string = 'WAITING...';
  CSTR_MOTOROLA: string = 'MOTOROLA INC. S-RECORD LOADER';
  CSTR_METRONIX: string = 'BOOTLOADER (C) METRONIX';
  CSTR_BLUPDATER: string = 'BOOTLOADER UPDATER';
  CSTR_BOOTCODE: string = 'BOOTCODE';
  CSTR_B115200: string = 'B115200';
  CSTR_POWER_ONOFF: string = 'Please manually reset the unit...';

  CCHR_RETURN: Char = Char(VK_RETURN);
  CINT_B115200: integer = 115200;
  CINT_B9600:   integer = 9600;
  CINT_TRIALS_MAX: integer = 5;

  C_REBOOT_TIME: cardinal = 10000;
  C_ANSWER_WAIT: cardinal = 1000;
  C_RECV_INTERVAL: cardinal = 100;

procedure TDownloader.InitProgressBar();
begin
  if assigned(t_progress) then begin
    if (t_progress is TProgressBar) then begin
      (t_progress as TProgressBar).Min := 0;
      (t_progress as TProgressBar).Max := t_srecords.Count;
      (t_progress as TProgressBar).Position := 0;
    end else if (t_progress is tCustomProgressBar) then begin
      (t_progress as tCustomProgressBar).MinValue := 0;
      (t_progress as tCustomProgressBar).MaxValue := t_srecords.Count;
      (t_progress as tCustomProgressBar).Progress := 0;
    end;
  end;
end;
procedure TDownloader.UpdateProgressBar(const val: integer);
begin
  if assigned(t_progress) then begin
    if (t_progress is TProgressBar) then (t_progress as TProgressBar).Position := val
    else if (t_progress is tCustomProgressBar) then (t_progress as tCustomProgressBar).Progress := val;
  end;
end;

constructor TDownloader.Create;
begin
	inherited Create;
  e_dlprotocol := DP_UNDEFINED;
  s_blmessage := '';
  s_fwmessage := '';
  s_lastmsg := '';
  s_lastfile := '';
  t_srecords := TStringList.Create();
  b_dlcancel := false;
end;

destructor TDownloader.Destroy;
begin
  t_srecords.Clear();
  FreeAndNil(t_srecords);
	inherited Destroy;
end;

function  TComDownloader.SendStr(const str: string; const bprint: boolean = true): boolean;
var c_time: cardinal; s_recv: string;
begin
  t_ser.ReadString(s_recv); //clear reading buffer of the serial interface
  c_time := GetTickCount() + C_ANSWER_WAIT;
  t_ser.WriteString(str);
  while ((t_ser.TxWaiting > 0) and (GetTickCount() < c_time)) do Delay(C_DELAY_ONCE);
  result := (t_ser.TxWaiting <= 0);
  if assigned(t_messager) then t_messager.Add(format('[%s]:>%s', [DateTimeToStr(Now()), str]));
end;

function  TComDownloader.RecvStr(var str: string; const bwait: boolean): integer;
var c_time: cardinal;
begin
  str := '';
  c_time := GetTickCount() + C_ANSWER_WAIT;
  if bwait then WaitForReading(c_time);
  result := t_ser.ReadString(str);
  if assigned(t_messager) then t_messager.Add(format('[%s]:<%s', [DateTimeToStr(Now()), str]));
end;

function  TComDownloader.RecvStrTimeout(var str: string; const tend: cardinal): integer;
var s_recv: string; i_len: integer;
begin
  result := 0; str := '';
  repeat
    s_recv := ''; WaitForReading(tend);
    i_len := t_ser.ReadString(s_recv);
    if (i_len > 0) then begin
      str := str + s_recv;
      result := result + i_len;
    end;
  until (tend <= GetTickCount());
  if assigned(t_messager) then t_messager.Add(format('[%s]:<%s', [DateTimeToStr(Now()), str]));
end;

function  TComDownloader.RecvStrInterval(var str: string; const tend: cardinal; const interv: cardinal): integer;
var b_break, b_timeout: boolean; s_recv: string; i_len: integer; c_time: cardinal;
begin
  result := 0; str := '';
  repeat
    s_recv := '';
    c_time := GetTickCount() + interv;
    if (tend < c_time) then c_time := tend;
    b_break := (not WaitForReading(c_time));
    i_len := t_ser.ReadString(s_recv);
    if ( i_len> 0) then begin
      str := str + s_recv;
      result := result + i_len;
    end;
    b_timeout := (tend <= GetTickCount());
  until (b_timeout or b_break);
  if assigned(t_messager) then t_messager.Add(format('[%s]:<%s', [DateTimeToStr(Now()), str]));
end;

function  TComDownloader.RecvStrExpected(var str: string; const exstr: string; tend: cardinal; const bcase: boolean = false): integer;
const C_SHORT_INTERVAL: cardinal = 50;
var s_input, s_temp: string; b_found: boolean; i_len: integer; b_timeout: boolean;
begin
  result := 0; str := '';  b_found := false;
  if bcase then s_input := exstr
  else s_input := UpperCase(exstr);
  repeat
    s_temp := ''; i_len := RecvStrInterval(s_temp, tend, C_SHORT_INTERVAL);
    if (i_len > 0) then begin
      result := result + i_len;
      if bcase then str := str + s_temp
      else  str := str + UpperCase(s_temp);
      b_found := (Pos(s_input, str) > 0);
    end;
    b_timeout := (tend <= GetTickCount());
  until (b_timeout or b_found);
end;

function  TComDownloader.WaitForReading(const tend: cardinal): boolean;
var s_lastmsg: string; c_tend, c_count: cardinal;
begin
  c_tend := GetTickCount(); c_count := c_tend;
  if assigned(t_messager) then s_lastmsg := t_messager[t_messager.Count - 1];
  while ((t_ser.RxWaiting <= 0) and (c_tend <= tend)) do begin
    Application.ProcessMessages();
    c_tend := GetTickCount();
    if assigned(t_messager) then begin
      if (c_tend - c_count) > 500  then begin //update the message per 0.5 second to avoid flashing
        t_messager[t_messager.Count - 1] := format('%s %ds', [s_lastmsg, Round((tend - c_tend) / 1000)]);
        c_count := c_tend;
      end;
    end;
  end;
  result := (t_ser.RxWaiting > 0);
end;

function  TComDownloader.TryBootMessageMTL(var msg: string): integer;
const CSTR_TEST_CMD: string= 'abcdefg';
var i_baud: integer; c_endtime: cardinal;
begin
  i_baud := t_ser.Baudrate;
  t_ser.Baudrate := CINT_B115200;
  SendStr(CSTR_TEST_CMD + CCHR_RETURN);
  c_endtime := GetTickCount() + C_ANSWER_WAIT;
  result := RecvStrInterval(msg, c_endtime, C_RECV_INTERVAL);
  t_ser.Baudrate := i_baud;
end;

procedure TComDownloader.ResetSerial();
begin
  t_ser.Baudrate := CINT_B9600;
  t_ser.FlowMode := fcNone;
end;

procedure TComDownloader.UpdateStartMessage();
const C_BL_FW_INTERVAL: cardinal = 6000; C_FW_OUTPUT_INTERVAL: cardinal = 1500;
var c_time: cardinal;
begin
  s_blmessage := ''; s_fwmessage := '';
  c_time := GetTickCount() + C_REBOOT_TIME;
  RecvStrInterval(s_blmessage, c_time, C_RECV_INTERVAL);
  if (not IsValidAscii(s_blmessage)) then TryBootMessageMTL(s_blmessage);
  c_time := GetTickCount() + C_BL_FW_INTERVAL;
  if WaitForReading(c_time) then RecvStrInterval(s_fwmessage, c_time, C_FW_OUTPUT_INTERVAL);
end;

function  TComDownloader.ResetDevice(const cmd: string; const tend: cardinal; const bmsg: boolean): boolean;
const C_MANUAL_RESET: cardinal = 30000;
var c_time, c_tcurr, c_tlast: cardinal;  b_timeout: boolean; i_relay: integer;
    s_recv, s_cmd, s_last: string;
begin
  //clear buffers of the serial interface
  t_ser.ReadString(s_recv);
  ResetSerial();
  s_cmd := trim(cmd);
  if ((s_cmd = '-1') or (s_cmd = '')) then begin //manually reset
    c_tcurr := GetTickCount(); c_tlast := c_tcurr;
    c_time := c_tcurr + C_MANUAL_RESET; //set timeout
    //wait for the anwser from the unit
    if assigned(t_messager) then begin
      s_last := format('[%s]:%s', [DateTimeToStr(Now()), CSTR_POWER_ONOFF]);
      t_messager.Add(format('%s %ds', [s_last, Round(C_MANUAL_RESET/1000)]));
    end;
    repeat
      Delay(C_DELAY_ONCE);
      c_tcurr := GetTickCount();
      if assigned(t_messager) then begin
        if (c_tcurr - c_tlast) > 500 then begin //upgrade the text per 0.5s
          t_messager[t_messager.Count - 1] := format('%s %ds', [s_last, Round((c_time - c_tcurr)/1000)]);
          c_tlast := c_tcurr;
        end;
      end;
      b_timeout := (c_tcurr >= c_time);
    until (b_timeout or (t_ser.RxWaiting > 0));
  end else begin
    if assigned(t_messager) then t_messager.Add(format('[%s]: the unit is resetting...', [DateTimeToStr(Now())]));
    if TryStrToInt(s_cmd, i_relay) then //DMM.Control_Relais(s_cmd, 500) //reset by relay (automatically hard reset)
    else SendStr(s_cmd + Char(VK_RETURN)); //reset through command (soft rest)
    // wait for that the first char arrives in C_RESET_TIMEOUT milliseconds after resetting
    WaitForReading(tend);
  end;
  result := (t_ser.RxWaiting > 0);
  if (not result) then
    if assigned(t_messager) then t_messager.Add(format('[%s]: failed to reset the unit.', [DateTimeToStr(Now())]));
    
  //receive the message
  if (bmsg and result) then UpdateStartMessage();
end;

function TComDownloader.EnterService(const cmd: string): boolean;
var s_recv, s_temp: string; c_endtime: cardinal; i_trials: integer;
begin
  result := false;
  c_endtime := GetTickCount() + C_REBOOT_TIME;
  s_recv := ''; i_trials := 0;
  if ResetDevice(cmd, c_endtime, false) then begin
    c_endtime := GetTickCount() + C_REBOOT_TIME;
    while (GetTickCount() < c_endtime) do begin
      s_temp := ''; RecvStrInterval(s_temp, c_endtime, C_RECV_INTERVAL);
      s_recv := s_recv + s_temp;
      if (not IsValidAscii(s_recv)) then begin //s-record loader, baudrate=115200
        t_ser.Baudrate := CINT_B115200;
        repeat
          SendStr(CSTR_BOOTQUE + CCHR_RETURN);
          WaitForReading(c_endtime);
          s_recv := ''; RecvStrInterval(s_recv, c_endtime, C_RECV_INTERVAL);
          s_temp := UpperCase(trim(s_recv));
          result := (Pos(CSTR_MOTOROLA, s_temp) > 0);
          if result then begin
            t_ser.FlowMode := fcXON_XOF;
            e_dlprotocol := DP_MOTOROLA;
          end;
          Inc(i_trials);
        until (result or (i_trials > CINT_TRIALS_MAX));
        break;
      end else begin
        s_temp := UpperCase(s_recv);
        if (Pos(CSTR_WAITING, s_temp) > 0) then begin
          SendStr(CSTR_SERVICE + CCHR_RETURN);
          Delay(C_DELAY_ONCE); //wait till the service mode is reached
          i_trials := 0;
          repeat
            SendStr(CSTR_BOOTQUE + CCHR_RETURN);
            result := (RecvStrExpected(s_recv, CSTR_SERVICE, GetTickCount() + C_ANSWER_WAIT) >= 0);
            if result then begin
              SendStr(CSTR_B115200 + CCHR_RETURN);
              if WaitForReading(GetTickCount() + C_ANSWER_WAIT) then begin
                if (t_ser.ReadString(s_recv) > 0) then t_ser.Baudrate := CINT_B115200;
              end;
            end else Delay(C_DELAY_ONCE);
            Inc(i_trials);
          until (result or (i_trials > CINT_TRIALS_MAX));
          e_dlprotocol := DP_METRONIX;
          break;
        end;
      end;
    end;
  end;
end;

function TComDownloader.TestApp(): boolean;
var c_time: cardinal; s_recv, s_temp: string; b_repeat: boolean; i_trials: integer;
begin
  result := false;
  if assigned(t_ser) then begin
    if (not t_ser.Active) then t_ser.Active := true;
    if t_ser.Active  then begin
      i_trials := 0;
      repeat
        c_time := GetTickCount() + C_ANSWER_WAIT;
        RecvStr(s_temp, false); //clear buffer
        SendStr(CSTR_BOOTQUE + CCHR_RETURN);
        WaitForReading(c_time);
        s_recv := ''; RecvStrInterval(s_recv, c_time, C_RECV_INTERVAL);
        s_temp := UpperCase(trim(s_recv));
        b_repeat := false;
        if (Pos(CSTR_APPLICATION, s_temp) > 0) then result := true
        else if ((Pos(CSTR_ERROR, s_temp) > 0) or (Pos(CSTR_UNKNOWNCMD, s_temp) > 0)) then b_repeat := true;
        Inc(i_trials);
      until ((not b_repeat) or (i_trials > CINT_TRIALS_MAX));
    end;
  end;
end;

function  TComDownloader.GetBootState(const cmd: string): EBootState;
const
  C_MTLBL   = $00010000;
  C_MTXBL   = $00020000;
  C_MTXUPD  = $00000001;
  C_MTXAPP  = $00000002;
var s_recv, s_temp, s_inblmsg, s_infwmsg: string; c_time: cardinal; b_repeat: boolean; lw_blfw: longword; t_lines: TStringList;
begin
  result := BS_UNKNOWN; lw_blfw := 0;
  c_time := GetTickCount() + C_REBOOT_TIME;
  if assigned(t_ser) then begin
    if (not t_ser.Active) then t_ser.Active := true;
    if t_ser.Active  then begin
      if ResetDevice(cmd, c_time) then begin
        s_inblmsg := UpperCase(trim(s_blmessage));
        s_infwmsg := UpperCase(trim(s_fwmessage));
        if (Pos(CSTR_MOTOROLA, s_inblmsg) > 0) then lw_blfw := (lw_blfw or C_MTLBL);
        if ((Pos(CSTR_WAITING, s_inblmsg) > 0) or (Pos(CSTR_BOOTCODE, s_inblmsg) > 0) or (Pos(CSTR_METRONIX, s_inblmsg) > 0))  then lw_blfw := (lw_blfw or C_MTXBL);

        if (s_fwmessage <> '') then begin
          if (Pos(CSTR_BLUPDATER, s_infwmsg) > 0)  then lw_blfw := (lw_blfw or C_MTXUPD);
          t_lines := TStringList.Create;
          ExtractStrings([Char(10)], [Char(13)], PChar(s_infwmsg), t_lines);
          if ((Pos(CSTR_DONE, t_lines[t_lines.Count - 1]) > 0))  then lw_blfw := C_MTXUPD
          else begin
            repeat
              c_time := GetTickCount() + C_ANSWER_WAIT;
              RecvStr(s_temp, false); //clear buffer
              SendStr(CSTR_BOOTQUE + CCHR_RETURN);
              WaitForReading(c_time);
              s_recv := ''; RecvStrInterval(s_recv, c_time, C_RECV_INTERVAL);
              s_temp := UpperCase(trim(s_recv));
              b_repeat := false;
              if (Pos(CSTR_APPLICATION, s_temp) > 0) then lw_blfw := (lw_blfw or C_MTXAPP)
              else if (Pos(UpperCase(CSTR_SERVICE), s_temp) > 0) then lw_blfw := (lw_blfw or C_MTXBL)
              else if ((Pos(CSTR_ERROR, s_temp) > 0) or (Pos(CSTR_UNKNOWNCMD, s_temp) > 0)) then b_repeat := true;
            until ((not b_repeat) or (GetTickCount() >= c_time));
          end;
          t_lines.Clear;
          FreeAndNil(t_lines);
        end;
      end;
    end;
  end;
  case lw_blfw of
    $00010000: result := BS_MTLBL_ONLY;
    $00010001: result := BS_MTLBL_UPD;
    $00010002: result := BS_MTLBL_APP;
    $00020000: result := BS_MTXBL_ONLY;
    $00020001: result := BS_MTXBL_UPD;
    $00000001: result := BS_XBL_UPD;
    $00020002: result := BS_MTXBL_APP;
  end;
  if assigned(t_messager) then t_messager.Add(format('[%s]: boot state is %s', [DateTimeToStr(Now()), GetEnumName(TypeInfo(EBootState), Ord(result))]));
end;

function TComDownloader.Download(const cmd, fname: string): boolean;
const C_DOWNLOAD_INTERVAL: cardinal = 6000;
var s_line, s_file, s_recv: string; i, i_trial: integer; b_break, b_ok: boolean;
begin
  result := false; b_dlcancel := false;
  if assigned(t_ser) then begin
    if (not t_ser.Active) then t_ser.Active := true;
    if t_ser.Active then begin
      s_file := trim(fname);
      if ((s_file <> s_lastfile) and FileExists(s_file)) then begin
        if assigned(t_messager) then t_messager.Add(format('[%s]: loading file ''%s''', [DateTimeToStr(Now()), s_file]));
        s_lastfile := s_file;
        t_srecords.Clear;
        t_srecords.LoadFromFile(s_lastfile);
      end;

      if (t_srecords.Count > 0) then begin
        InitProgressBar();
        if EnterService(cmd) then begin
          b_break := false;
          if assigned(t_messager) then t_messager.Add(format('[%s]: download started', [DateTimeToStr(Now())]));

          if (e_dlprotocol = DP_METRONIX) then begin
            for i := 0 to t_srecords.Count - 1 do begin
              i_trial := 0; b_ok := false;
              s_line := t_srecords[i] + Char(13);
              repeat
                t_ser.WriteString(s_line);
                s_recv := '';
                if WaitForReading(GetTickCount() + C_DOWNLOAD_INTERVAL) then begin
                  t_ser.ReadString(s_recv);
                  if SameText(s_recv, '*') then b_ok := true
                  else if SameText(s_recv, '#') then begin //repeat sending the same line
                    Inc(i_trial);
                    b_break := (i_trial > CINT_TRIALS_MAX);
                  end else b_break := true; //'@' or other char
                end else b_break := true;  //received no data
                Application.ProcessMessages();
              until (b_break or b_ok);
              if (b_break or b_dlcancel) then break;
              UpdateProgressBar(i + 1);
            end;
          end else if (e_dlprotocol = DP_MOTOROLA) then begin
            for i := 0 to t_srecords.Count - 1 do begin
              t_ser.WriteString(t_srecords[i] + CCHR_RETURN);
              while t_ser.TxWaiting > 0 do Delay(C_DELAY_ONCE);
              Application.ProcessMessages();
              UpdateProgressBar(i + 1);
              if b_dlcancel then break;
            end;
          end else b_break := true;
          result := ((not b_break) and (not b_dlcancel));
          if assigned(t_messager) then t_messager.Add(format('[%s]: download is over with %d s-records', [DateTimeToStr(Now()), t_srecords.Count]));
        end;
      end;
    end else ;
  end;
end;

initialization
  t_comdownloader := TComDownloader.Create();
end.
