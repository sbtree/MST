unit MtxDownloader;

interface
uses Serial3, Classes, Controls, RS232, ConnBase, TextMessage;
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
                DP_UNDEFINED, //undefined download protocol
                DP_MOTOROLA,  //download protocol of Motorola S-Record loader
                DP_MTXDIS2,   //download protocol of metronix boot loader, z.B. DIS2
                DP_MTXARS   //download protocol of metronix boot loader, z.B. ARS2000
                );

  EDownloadChannel = (
                DC_RS232,    //download directly over rs232 connection
                DC_USB,      //download directly over usb connection
                DC_CAN,      //download directly over can-bus connection
                DC_FOJ,      //flash over jtag, a software tool (adapter: parallel port -> jtag)
                DC_FDT,      //flash development toolkit, a software tool of Renesas for flashing (rs232 in boot mode)
                DC_FCW       //Freescale CodeWarrior flash, a software tool for flashing (adapter: usb->jtag)
                );

  TMtxDownloader = class(TInterfacedObject, ITextMessengerImpl)
  protected
    e_dlprotocol: EDownloadProtocol;//to save protocol
    s_lastmsg:    string;           //to save information in the last action
    s_lastfile:   string;           //to save file path (an s-record file), which is given in function Download last time;
    t_srecords:   TStringList;      //to save s-records, which is loaded last time;
    i_curline:    integer;          //to save the current index of t_srecords which is downloaded, internal variable
    t_progress:   TControl;         //TProgressBar; //to illustrate the progress of the download
    b_dlcancel:   boolean;          //to cancel downloading manually
    r_baudfactor: single;           //factor of baudrate, useful if the oscilator frequence of the target device and its boot loader don't match
    t_msgrimpl:   TTextMessengerImpl; //for transfering messages
  protected
    procedure UpdateStartMessage(const smsg: string); virtual; abstract;
    function  ResetDevice(const cmd: string; const tend: cardinal; const bmsg: boolean = true): boolean; virtual; abstract;
    function  EnterService(const cmd: string): boolean; virtual; abstract;
    procedure InitProgressBar(); virtual;
    procedure UpdateProgressBar(const val: integer); virtual;
    function GetMessenger(): TTextMessenger;
    procedure SetMessenger(const msgr: TTextMessenger); virtual;
  public
    constructor Create();
    destructor Destroy; override;

    property MessengerService: TTextMessengerImpl read t_msgrimpl implements ITextMessengerImpl;
    property BaudrateFactor: single read r_baudfactor write r_baudfactor;
    property Cancel : boolean read b_dlcancel write b_dlcancel;
    //property Messager: TStrings write t_messager;
    property Messenger: TTextMessenger read GetMessenger write SetMessenger;
    property ProgressBar: TControl write t_progress;
    function TestApp(): boolean; virtual; abstract;
    function GetBootState(const cmd: string): EBootState; virtual; abstract;
    function Download(const cmd, fname: string): boolean; virtual; abstract;
  end;

  TMtxComDownloader = class(TMtxDownloader)
  protected
    //t_ser: TSerial;
    t_conn: TSerialAdapter;
    s_blmessage: string; //save switch-on message of boot loader
    s_fwmessage: string; //save switch-on message of firmware
  protected
    function GetSerialObj(): TSerial;
    procedure SetSerialObj(tser: TSerial);
    procedure SetMessenger(const msgr: TTextMessenger); override;
    procedure SwitchBaudrate(const ibaud: Integer; const bxonoff: Boolean = false);
    //function  SendStr(const str: string; const bprint: boolean = true): boolean;
    //function  RecvStr(var str: string; const bwait: boolean = true): integer;
    //function  RecvStrInterval(var str: string; const tend: cardinal; const interv: cardinal = 3000): integer;
    //function  RecvStrExpected(var str: string; const exstr: string; tend: cardinal; const bcase: boolean = false): integer;
    //function  WaitForReading(const tend: cardinal): boolean;
    function  StartWithMTL(): boolean;
    function  StartWithMTX1(): boolean;
    function  StartWithMTX2(): boolean;

    procedure UpdateStartMessage(const smsg: string); override;
    function  TryBootMessageMTL(var msg: string): integer;
    function  ResetDevice(const cmd: string; const timeout: cardinal; const bmsg: boolean = true): boolean; override;
    function  EnterService(const cmd: string): boolean; override;
    function  StartDownload(): boolean; virtual;
  public
    constructor Create();
    destructor Destroy(); override;

    property SerialObj: TSerial read GetSerialObj write SetSerialObj;

    function TestApp(): boolean; override;
    function GetBootState(const cmd: string): EBootState; override;
    function Download(const cmd, fname: string): boolean; override;
  end;

//  var t_comdownloader: TMtxComDownloader;
implementation
uses Windows, SysUtils, StrUtils, GenUtils, Forms, TypInfo, ComCtrls, NewProgressBar;

const
  CSTR_BOOTQUE: string = 'BOOT?';
  CSTR_TYPQUE: string = 'TYP?';
  CSTR_TYPANS: string = ':TYP:';
  CSTR_SERVICE: string = 'service';
  CSTR_APPLICATION: string = 'APPLICATION';
  CSTR_ERROR: string = 'ERROR!';
  CSTR_ERR: string = 'ERR!';
  CSTR_UNKNOWNCMD: string = 'UNKNOWN COMMAND';
  CSTR_CHECKSUM: string = 'CHECKSUM';
  CSTR_DONE: string = 'DONE.';
  CSTR_ARS2000: string = 'ARS 2000';
  CSTR_PERCENT: string = '%';
  CSTR_WAITING: string = 'WAITING...';
  CSTR_SERVICE_MENU: string = 'SERVICE-MENUE';
  CSTR_EPR: string = 'EPR';
  CSTR_OK: string = 'OK!';
  CSTR_PROMPT: string = '>';
  CSTR_START_ADDR: string = 'Start-Address';
  CSTR_START_APP: string = 'STARTING APPLICATION...';
  CSTR_MOTOROLA: string = 'MOTOROLA INC. S-RECORD LOADER';
  CSTR_METRONIX: string = 'BOOTLOADER (C) METRONIX';
  CSTR_BLUPDATER: string = 'BOOTLOADER UPDATER';
  CSTR_BOOTCODE: string = 'BOOTCODE';
  CSTR_B115200: string = 'B115200';
  CSTR_POWER_ONOFF: string = 'please manually reset the device...';

  CCHR_RETURN: Char = Char(VK_RETURN);
  CINT_B115200: integer = 115200;
  CINT_B9600:   integer = 9600;
  CINT_TRIALS_MAX: integer = 5;

  C_REBOOT_TIME: cardinal = 10000;
  C_ANSWER_WAIT: cardinal = 1000;
  C_RECV_INTERVAL: cardinal = 50;
  C_DOWNLOAD_INTERVAL: cardinal = 6000;

procedure TMtxDownloader.InitProgressBar();
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
procedure TMtxDownloader.UpdateProgressBar(const val: integer);
begin
  if assigned(t_progress) then begin
    if (t_progress is TProgressBar) then (t_progress as TProgressBar).Position := val
    else if (t_progress is tCustomProgressBar) then (t_progress as tCustomProgressBar).Progress := val;
  end;
end;

function TMtxDownloader.GetMessenger(): TTextMessenger;
begin
  result := t_msgrimpl.Messenger;
end;

procedure TMtxDownloader.SetMessenger(const msgr: TTextMessenger);
begin
  t_msgrimpl.Messenger := msgr;
end;

constructor TMtxDownloader.Create;
begin
	inherited Create;
  e_dlprotocol := DP_UNDEFINED;
  s_lastmsg := '';
  s_lastfile := '';
  t_srecords := TStringList.Create();
  b_dlcancel := false;
  //i_cbaud := CINT_B115200;
  r_baudfactor := 1.0;
  t_msgrimpl := TTextMessengerImpl.Create(ClassName());
end;

destructor TMtxDownloader.Destroy;
begin
  t_srecords.Clear();
  FreeAndNil(t_srecords);
	inherited Destroy;
end;

function TMtxComDownloader.GetSerialObj(): TSerial;
begin
  result := t_conn.SerialObj;
end;

procedure TMtxComDownloader.SetSerialObj(tser: TSerial);
begin
  t_conn.SerialObj := tser;
end;

procedure TMtxComDownloader.SetMessenger(const msgr: TTextMessenger);
begin
  inherited SetMessenger(msgr);
  ITextMessengerImpl(t_conn).Messenger := msgr;
end;

procedure TMtxComDownloader.SwitchBaudrate(const ibaud: Integer; const bxonoff: Boolean);
var i_baud: integer;
begin
  if assigned(t_conn.SerialObj) then begin
    i_baud := Round(ibaud * r_baudfactor);
    t_conn.SerialObj.Baudrate := ibaud;
    if bxonoff then t_conn.SerialObj.FlowMode := fcXON_XOF
    else t_conn.SerialObj.FlowMode := fcNone;
  end;
end;

//function  TMtxComDownloader.SendStr(const str: string; const bprint: boolean = true): boolean;
//var c_time: cardinal; s_recv: string;
//begin
//  RecvStr(s_recv, false); //clear reading buffer of the serial interface
//  c_time := GetTickCount() + C_ANSWER_WAIT;
//  t_ser.WriteString(str);
//  while ((t_ser.TxWaiting > 0) and (GetTickCount() < c_time)) do TGenUtils.Delay(C_DELAY_ONCE);
//  result := (t_ser.TxWaiting <= 0);
//  if assigned(t_messager) then t_messager.Add(format('[%s]:>%s', [DateTimeToStr(Now()), str]));
//end;

//function  TMtxComDownloader.RecvStr(var str: string; const bwait: boolean): integer;
//var c_time: cardinal;
//begin
//  str := '';
//  if bwait then begin
//    c_time := GetTickCount() + C_ANSWER_WAIT;
//    WaitForReading(c_time);
//  end;
//  result := t_ser.ReadString(str);
//  if (assigned(t_messager) and (str <> '')) then t_messager.Add(format('[%s]:<%s', [DateTimeToStr(Now()), str]));
//end;

//function  TMtxComDownloader.RecvStrInterval(var str: string; const tend: cardinal; const interv: cardinal): integer;
//var b_break, b_timeout: boolean; s_recv: string; i_len: integer; c_time: cardinal;
//begin
//  result := 0; str := '';
//  repeat
//    s_recv := '';
//    c_time := GetTickCount() + interv;
//    if (tend < c_time) then c_time := tend;
//    b_break := (not WaitForReading(c_time));
//    i_len := t_ser.ReadString(s_recv);
//    if ( i_len> 0) then begin
//      str := str + s_recv;
//      result := result + i_len;
//    end;
//    b_timeout := (tend <= GetTickCount());
//  until (b_timeout or b_break);
//  if (assigned(t_messager) and (str <> '')) then t_messager.Add(format('[%s]:<%s', [DateTimeToStr(Now()), str]));
//end;

//function  TMtxComDownloader.RecvStrExpected(var str: string; const exstr: string; tend: cardinal; const bcase: boolean = false): integer;
//const C_SHORT_INTERVAL: cardinal = 50;
//var s_input, s_temp: string; b_found: boolean; i_len: integer; b_timeout: boolean;
//begin
//  result := 0; str := '';  b_found := false;
//  if bcase then s_input := exstr
//  else s_input := UpperCase(exstr);
//  repeat
//    s_temp := ''; i_len := RecvStrInterval(s_temp, tend, C_SHORT_INTERVAL);
//    if (i_len > 0) then begin
//      result := result + i_len;
//      if bcase then str := str + s_temp
//      else  str := str + UpperCase(s_temp);
//      b_found := (Pos(s_input, str) > 0);
//    end;
//    b_timeout := (tend <= GetTickCount());
//  until (b_timeout or b_found);
//end;

//function  TMtxComDownloader.WaitForReading(const tend: cardinal): boolean;
//var s_lastmsg: string; c_tend, c_count: cardinal;
//begin
//  c_tend := GetTickCount(); c_count := c_tend;
//  if assigned(t_messager) then s_lastmsg := t_messager[t_messager.Count - 1];
//  while ((t_ser.RxWaiting <= 0) and (c_tend <= tend)) do begin
//    Application.ProcessMessages();
//    c_tend := GetTickCount();
//    if assigned(t_messager) then begin
//      if (c_tend - c_count) > 500  then begin //update the message per 0.5 second to avoid flashing
//        t_messager[t_messager.Count - 1] := format('%s %ds', [s_lastmsg, Round((tend - c_tend) / 1000)]);
//        c_count := c_tend;
//      end;
//    end;
//  end;
//  result := (t_ser.RxWaiting > 0);
//end;

function  TMtxComDownloader.StartWithMTL(): boolean;
var i: integer;
begin
  for i := 0 to t_srecords.Count - 1 do begin
    t_conn.SendStr(t_srecords[i] + CCHR_RETURN);
    Application.ProcessMessages();
    UpdateProgressBar(i + 1);
    if b_dlcancel then break;
  end;
  i_curline := i;          
  result := (i_curline = t_srecords.Count);
end;

function  TMtxComDownloader.StartWithMTX1(): boolean;
var i, i_trial: integer; b_ok, b_break: boolean; s_line, s_recv: string;
begin
  t_conn.Timeout := C_DOWNLOAD_INTERVAL;
  for i := 0 to t_srecords.Count - 1 do begin
    i_trial := 0; b_break := false;
    s_line := t_srecords[i] + CCHR_RETURN;
    if ((length(s_line) > 4) and SameText('S', s_line[1])) then begin //length of s-record at least
      repeat
        s_recv := ''; b_ok := false; b_break := false;
        if t_conn.SendStr(s_line) then begin
          if (t_conn.RecvStr(s_recv) > 0) then  s_recv := trim(s_recv);
          if (Pos('*', s_recv) > 0) then b_ok := true //'*' or '>' ok
          else if (Pos('#', s_recv) > 0) then begin //'#' repeat sending the same line
            Inc(i_trial);
            b_break := (i_trial > CINT_TRIALS_MAX);
          end else if ((length(s_recv) > 0) and (Pos('>', s_recv) = length(s_recv))) then b_ok := true
          else if ContainsText(s_recv, CSTR_START_ADDR) then b_ok := true
          else begin //'@' or other char
            b_break := true;
            //if assigned(t_messager) then t_messager.Add(format('[%s]:<%s', [DateTimeToStr(Now()), s_recv]))
          end;
        end else b_ok := true; //b_break := true;  //received no data as ok
        Application.ProcessMessages();
      until (b_ok or b_break);
    end;
    UpdateProgressBar(i + 1);
    if (b_break or b_dlcancel) then break;
  end;          
  i_curline := i;          
  result := (i_curline = t_srecords.Count);
end;

function  TMtxComDownloader.StartWithMTX2(): boolean;
var s_recv: string;
begin
  t_conn.SendStr(CSTR_EPR + CCHR_RETURN);
  result := t_conn.RecvStrExpected(s_recv, CSTR_OK + #$A + #$D + CSTR_PROMPT{CSTR_SERVICE}, GetTickCount() + C_ANSWER_WAIT);
  if result then result := StartWithMTX1();
end;

function  TMtxComDownloader.TryBootMessageMTL(var msg: string): integer;
const CSTR_TEST_CMD: string= 'abcdefg';
      C_WAITING_INTERVAL: cardinal = 3000;
      C_RESTART_INTERVAL: cardinal = 2000;
var i_baud: integer; c_endtime: cardinal;
begin
  i_baud := t_conn.SerialObj.Baudrate;
  SwitchBaudrate(CINT_B115200, true);
  t_conn.SendStr(CSTR_TEST_CMD + CCHR_RETURN);
  c_endtime := GetTickCount() + C_WAITING_INTERVAL; //C_ANSWER_WAIT;
  result := t_conn.RecvStrInterval(msg, c_endtime, C_RESTART_INTERVAL);
  SwitchBaudrate(i_baud, false);
end;

procedure TMtxComDownloader.UpdateStartMessage(const smsg: string);
const C_BL_FW_INTERVAL: cardinal = 6000; C_FW_OUTPUT_INTERVAL: cardinal = 1500;
var c_time: cardinal; s_temp: string; i_pos: integer;
begin
  s_blmessage := ''; s_fwmessage := '';
  if (length(smsg) <= 1) then s_blmessage := ''//only one char is handled as noise
  else begin
    if (not TGenUtils.IsAsciiValid(smsg)) then
      TryBootMessageMTL(s_blmessage)
    else begin
      if ContainsText(smsg, CSTR_UNKNOWNCMD) then
        t_msgrimpl.AddMessage('The device received an unknown command.', ML_ERROR)
      else begin
      end;
      s_temp := UpperCase(smsg);
      i_pos := Pos(CSTR_START_APP, s_temp);
      if (i_pos > 0) then begin
        s_blmessage := LeftStr(smsg, i_pos - 1);
        s_fwmessage := MidStr(smsg,i_pos, length(smsg));
      end;
    end;
  end;
end;

function  TMtxComDownloader.ResetDevice(const cmd: string; const timeout: cardinal; const bmsg: boolean): boolean;
const C_MANUAL_RESET: cardinal = 30000;
var c_time, c_timeout: cardinal; i_relay: integer;
    s_recv, s_cmd, s_last: string;
begin
  //clear buffers of the serial interface
  t_conn.RecvStr(s_recv, false);
  SwitchBaudrate(CINT_B9600, false);
  s_cmd := trim(cmd);
  if ((s_cmd = '-1') or (s_cmd = '')) then begin //manually reset
    c_timeout := C_MANUAL_RESET;
    t_msgrimpl.AddMessage(CSTR_POWER_ONOFF);
  end else begin
    c_timeout := timeout;
    if TryStrToInt(s_cmd, i_relay) then
      //DMM.Control_Relais(s_cmd, 500) //reset by relay (automatically hard reset)
    else
      t_conn.SendStr(s_cmd + Char(VK_RETURN)); //reset through command (soft rest)

    t_msgrimpl.AddMessage('The device is resetting...');
  end;
  c_time := GetTickCount() + c_timeout; //set timeout

  // wait for that the first char arrives in the time of timeout after resetting
  repeat
    Application.ProcessMessages();
    result := (t_conn.SerialObj.RxWaiting > 0);
  until (result or (GetTickCount() >= c_time));

  if not result then t_msgrimpl.AddMessage('Failed to reset the device.', ML_ERROR)
  else if bmsg then begin
    t_conn.RecvStrInterval(s_recv, c_time);
    if bmsg then UpdateStartMessage(s_recv);
  end;
end;

function TMtxComDownloader.EnterService(const cmd: string): boolean;
var s_recv, s_temp: string; c_endtime: cardinal; i_trials: integer;
begin
  result := false;
  c_endtime := GetTickCount() + C_REBOOT_TIME;
  s_recv := ''; i_trials := 0; e_dlprotocol := DP_UNDEFINED;
  if ResetDevice(cmd, c_endtime, false) then begin
    c_endtime := GetTickCount() + C_REBOOT_TIME;
    while (GetTickCount() < c_endtime) do begin
      s_temp := ''; t_conn.RecvStrInterval(s_temp, c_endtime, C_RECV_INTERVAL);
      s_recv := s_recv + s_temp;

      if (ContainsText(s_recv, CSTR_WAITING) or ContainsText(s_recv, CSTR_SERVICE_MENU)) then begin
        t_conn.SendStr(CSTR_SERVICE + CCHR_RETURN);
        if t_conn.RecvStrExpected(s_temp, CSTR_PROMPT, c_endtime) then begin //wait till the service mode is reached
          s_recv := s_recv + s_temp;
          i_trials := 1;
          repeat
            t_conn.SendStr(CSTR_BOOTQUE + CCHR_RETURN);
            result := t_conn.RecvStrExpected(s_temp, CSTR_SERVICE, GetTickCount() + C_ANSWER_WAIT);
            if result then begin
              s_recv := s_recv + s_temp;
              t_conn.SendStr(CSTR_B115200 + CCHR_RETURN);
              t_conn.Timeout := C_ANSWER_WAIT;
              if (t_conn.RecvStr(s_temp, true) > 0) then SwitchBaudrate(CINT_B115200);
            end else TGenUtils.Delay(C_DELAY_ONCE);
            Inc(i_trials);
          until (result or (i_trials > CINT_TRIALS_MAX));
        end;
        if ContainsText(s_recv, CSTR_SERVICE_MENU) then e_dlprotocol := DP_MTXARS
        else e_dlprotocol := DP_MTXDIS2;
        break;
      end else if ContainsText(s_recv, CSTR_UNKNOWNCMD) then begin
        t_msgrimpl.AddMessage(format('''%s'' is not supported.', [cmd]), ML_ERROR);
        break;
      end else if (not TGenUtils.IsAsciiValid(s_recv)) then begin //messy code, assume that the Motorola S-Record Loader exists on the device
        if (length(s_recv) = 1) then begin //start char of ARS iS Variant 
          s_recv := '';
          continue;
        end else begin
          SwitchBaudrate(CINT_B115200, true);
          repeat
            t_conn.SendStr(CSTR_BOOTQUE + CCHR_RETURN);
            t_conn.RecvStrInterval(s_temp, c_endtime, C_RECV_INTERVAL);
            result := ContainsText(s_temp, CSTR_MOTOROLA);  //ensure, that the Motorola S-Record Loader is found on the device
            if result then  e_dlprotocol := DP_MOTOROLA
            else SwitchBaudrate(CINT_B9600);;
            Inc(i_trials);
          until (result or (i_trials > CINT_TRIALS_MAX));
          break;
        end;
      end;
    end;
  end;
end;

function  TMtxComDownloader.StartDownload(): boolean;
var e_msglevel: EMessageLevel;
begin
  i_curline := 0;
  t_msgrimpl.AddMessage(format('Download started with protocol %s', [GetEnumName(TypeInfo(EDownloadProtocol), Ord(e_dlprotocol))]));
  //ITextMessengerImpl(t_conn).SetWaitingMode(true, 'Downloading...');
  e_msglevel := t_msgrimpl.Messenger.MessageThreshold;
  t_msgrimpl.Messenger.MessageThreshold := ML_ERROR;
  case e_dlprotocol of
  DP_MTXARS: result := StartWithMTX2();
  DP_MTXDIS2: result := StartWithMTX1();
  DP_MOTOROLA : result := StartWithMTL();
  else result := false;
  end;
  //ITextMessengerImpl(t_conn).SetWaitingMode(false);
  t_msgrimpl.Messenger.MessageThreshold := e_msglevel;

  if (e_dlprotocol = DP_UNDEFINED) then
    t_msgrimpl.AddMessage(format('Download protocol (%s) is not defined', [GetEnumName(TypeInfo(EDownloadProtocol), Ord(e_dlprotocol))]))
  else
    t_msgrimpl.AddMessage(format('Download is finished with %d/%d lines', [i_curline, t_srecords.Count]));
end;

constructor TMtxComDownloader.Create();
begin
  inherited Create();
  t_conn := TSerialAdapter.Create(nil);
  s_blmessage := '';
  s_fwmessage := '';
end;

destructor TMtxComDownloader.Destroy();
begin
  t_conn.Free();
  inherited Destroy();
end;

function TMtxComDownloader.TestApp(): boolean;
var c_time: cardinal; s_recv: string; b_repeat: boolean; i_trials: integer;
begin
  result := false;
  if assigned(t_conn) then begin
    if (not t_conn.Connected) then t_conn.Connect();
    if t_conn.Connected  then begin
      i_trials := 0;
      repeat
        c_time := GetTickCount() + C_ANSWER_WAIT;
        t_conn.RecvStr(s_recv, false); //clear buffer
        t_conn.SendStr(CSTR_BOOTQUE + CCHR_RETURN); //test query with 'BOOT?'
        s_recv := ''; t_conn.RecvStrInterval(s_recv, c_time, C_RECV_INTERVAL);
        b_repeat := false;
        if ContainsText(s_recv, CSTR_APPLICATION) then result := true
        else if (ContainsText(s_recv, CSTR_ERROR) or ContainsText(s_recv, CSTR_ERR) or ContainsText(s_recv, CSTR_UNKNOWNCMD)) then begin
          c_time := GetTickCount() + C_ANSWER_WAIT;
          t_conn.SendStr(CSTR_TYPQUE + CCHR_RETURN); //test query with 'TYP?'
          s_recv := ''; t_conn.RecvStrInterval(s_recv, c_time, C_RECV_INTERVAL);
          result := ContainsText(s_recv, CSTR_TYPANS);
          if (not result) then b_repeat := true;
        end;
        Inc(i_trials);
      until ((not b_repeat) or (i_trials > CINT_TRIALS_MAX));
    end;
  end;
end;

function  TMtxComDownloader.GetBootState(const cmd: string): EBootState;
const
  C_MTLBL   = $00010000;
  C_MTXBL   = $00020000;
  C_MTXUPD  = $00000001;
  C_MTXAPP  = $00000002;
var s_recv: string; c_time: cardinal; b_repeat: boolean; lw_blfw: longword; t_lines: TStringList;
begin
  result := BS_UNKNOWN; lw_blfw := 0;
  c_time := GetTickCount() + C_REBOOT_TIME;
  if assigned(t_conn) then begin
    if (not t_conn.Connected) then t_conn.Connect();
    if t_conn.Connected  then begin
      if ResetDevice(cmd, c_time, true) then begin
        if ContainsText(s_blmessage, CSTR_MOTOROLA) then lw_blfw := (lw_blfw or C_MTLBL);
        if (ContainsText(s_blmessage, CSTR_WAITING) or ContainsText(s_blmessage, CSTR_BOOTCODE) or ContainsText(s_blmessage, CSTR_METRONIX))  then lw_blfw := (lw_blfw or C_MTXBL);

        if (s_fwmessage <> '') then begin
          if ContainsText(s_fwmessage, CSTR_BLUPDATER) then lw_blfw := (lw_blfw or C_MTXUPD);
          t_lines := TStringList.Create;
          ExtractStrings([Char(10)], [Char(13)], PChar(s_fwmessage), t_lines);
          if ContainsText(t_lines[t_lines.Count - 1], CSTR_DONE) then lw_blfw := C_MTXUPD
          else if (ContainsText(s_fwmessage, CSTR_ARS2000) or ContainsText(s_fwmessage, CSTR_PERCENT + CCHR_RETURN)) then lw_blfw := (lw_blfw or C_MTXAPP)
          else begin
            repeat
              c_time := GetTickCount() + C_ANSWER_WAIT;
              s_recv := ''; t_conn.RecvStr(s_recv, false); //clear buffer
              t_conn.SendStr(CSTR_BOOTQUE + CCHR_RETURN); //test query with 'BOOT?'
              s_recv := ''; t_conn.RecvStrInterval(s_recv, c_time, C_RECV_INTERVAL);
              b_repeat := false;
              if ContainsText(s_recv,CSTR_APPLICATION) then lw_blfw := (lw_blfw or C_MTXAPP)
              else if ContainsText(s_recv, CSTR_SERVICE) then lw_blfw := (lw_blfw or C_MTXBL)
              else if (ContainsText(s_recv, CSTR_ERROR) or ContainsText(s_recv, CSTR_ERR) or ContainsText(s_recv, CSTR_UNKNOWNCMD)) then begin
                c_time := GetTickCount() + C_ANSWER_WAIT;
                t_conn.SendStr(CSTR_TYPQUE + CCHR_RETURN); //test query with 'TYP?'
                s_recv := ''; t_conn.RecvStrInterval(s_recv, c_time, C_RECV_INTERVAL);
                if (not ContainsText(s_recv, CSTR_TYPANS)) then b_repeat := true
                else lw_blfw := (lw_blfw or C_MTXAPP);
              end;
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
  t_msgrimpl.AddMessage(format('The boot state is %s', [GetEnumName(TypeInfo(EBootState), Ord(result))]));
end;

function TMtxComDownloader.Download(const cmd, fname: string): boolean;
var s_file: string;
begin
  result := false; b_dlcancel := false;
  if assigned(t_conn) then begin
    if (not t_conn.Connected) then t_conn.Connect();
    if t_conn.Connected then begin
      s_file := trim(fname);
      if ((s_file <> s_lastfile) and FileExists(s_file)) then begin
        t_msgrimpl.AddMessage(format('Loading file ''%s''', [s_file]));
        s_lastfile := s_file;
        t_srecords.Clear;
        t_srecords.LoadFromFile(s_lastfile);
      end;

      if (t_srecords.Count > 0) then begin
        InitProgressBar();
        if EnterService(cmd) then result := StartDownload();
      end;
    end;
  end;
end;
//
//initialization
//  t_comdownloader := TMtxComDownloader.Create();
//
//finalization
//  t_comdownloader.Free();

end.
