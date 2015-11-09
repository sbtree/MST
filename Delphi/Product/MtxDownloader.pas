unit MtxDownloader;

interface
uses Serial3, Classes;
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
  protected
    procedure LoadFile(const sfile: string); virtual; abstract;
    procedure GetStartMessage(const tend: cardinal); virtual; abstract;
    function  ResetDevice(const cmd: string; const tend: cardinal): boolean; virtual; abstract;
    function  GetBootState(): EBootState; virtual; abstract;
    function  EnterService(const cmd: string): boolean; virtual; abstract;
  public
    property BootState: EBootState read GetBootState;
  public
    constructor Create();
    destructor Destroy; override;
    
    function Download(const fname: string): boolean; virtual; abstract;
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

    procedure GetStartMessage(const tend: cardinal); override;
    function  ResetDevice(const cmd: string; const tend: cardinal): boolean; override;
    function  GetBootState(): EBootState; override;
    function  EnterService(const cmd: string): boolean; override;
  public
    function Download(const fname: string): boolean; override;
  end;

implementation
uses Windows, SysUtils, GenUtils, Forms;

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

  CINT_B115200: integer = 115200;

  C_REBOOT_TIME: cardinal = 10000;
  C_ANSWER_WAIT: cardinal = 1000;
  C_RECV_INTERVAL: cardinal = 100;

constructor TDownloader.Create;
begin
	inherited Create;
  e_dlprotocol := DP_UNDEFINED;
  t_srecords := TStringList.Create();
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
end;

function  TComDownloader.RecvStr(var str: string; const bwait: boolean): integer;
var c_time: cardinal;
begin
  str := '';
  c_time := GetTickCount() + C_ANSWER_WAIT;
  if bwait then WaitForReading(c_time);
  result := t_ser.ReadString(str);
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
end;

function  TComDownloader.RecvStrExpected(var str: string; const exstr: string; tend: cardinal; const bcase: boolean = false): integer;
const C_SHORT_INTERVAL: cardinal = 50;
var s_input, s_temp: string; b_found: boolean; i_len: integer; b_timeout: boolean;
begin
  result := 0; str := '';  b_found := false;
  if bcase then s_input := exstr
  else s_input := UpperCase(exstr);
  repeat
    s_temp := ''; result := result + RecvStrInterval(s_temp, tend, C_SHORT_INTERVAL);
    if (i_len > 0) then begin
      if bcase then str := str + s_temp
      else  str := str + UpperCase(s_temp);
      b_found := (Pos(s_input, str) > 0);
    end;
    b_timeout := (tend <= GetTickCount());
  until (b_timeout or b_found);
end;

function  TComDownloader.WaitForReading(const tend: cardinal): boolean;
var b_timeout: boolean;
begin
  b_timeout := false;
  while ((t_ser.RxWaiting <= 0) and (not b_timeout)) do begin
    Application.ProcessMessages();
    b_timeout := (GetTickCount() >= tend);
  end;
  result := (t_ser.RxWaiting > 0);
end;

procedure TComDownloader.GetStartMessage(const tend: cardinal);
begin

end;

function  TComDownloader.ResetDevice(const cmd: string; const tend: cardinal): boolean;
const C_MANUAL_RESET: cardinal = 30000;
var c_time: cardinal;  b_timeout: boolean; s_recv: string;
begin
  //clear buffers of the serial interface
  t_ser.ReadString(s_recv);
  if ((cmd = '-1') or (cmd = '')) then begin //manually reset
    c_time := GetTickCount() + C_MANUAL_RESET; //set timeout
    //wait for the anwser from the unit
    repeat
      Delay(C_DELAY_ONCE);
      b_timeout := (GetTickCount() >= c_time);
    until (b_timeout or (t_ser.RxWaiting > 0));
  end else begin
    //todo: reset by relay (automatically hard reset)
    //todo: reset through command (soft rest)
    // wait for that the first char arrives in C_RESET_TIMEOUT milliseconds after resetting
    WaitForReading(tend);
  end;
  result := (t_ser.RxWaiting > 0);
end;

function  TComDownloader.GetBootState(): EBootState;
begin

end;

function TComDownloader.EnterService(const cmd: string): boolean;
const CINT_MAX_TRIALS: integer = 5; CCHR_RETURN: Char = Char(VK_RETURN);
var s_recv, s_temp: string; c_endtime, c_start, c_end: cardinal; t_exstrs: TStringList; i_trials: integer;
begin
  result := false;
  c_start := GetTickCount();
  c_endtime := c_start + C_REBOOT_TIME;
  s_recv := ''; i_trials := 0;
  if ResetDevice(cmd, c_endtime) then begin
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
        until (result or (i_trials > CINT_MAX_TRIALS));
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
          until (result or (i_trials > CINT_MAX_TRIALS));
          e_dlprotocol := DP_METRONIX;
          break;
        end;
      end;
    end;
  end;
  c_end := GetTickCount();
end;

end.
