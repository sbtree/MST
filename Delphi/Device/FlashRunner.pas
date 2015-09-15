// =============================================================================
// Module name  : $RCSfile: FlashRunner.pas,v $
// description  : This unit implements all methodes and properties of class
//                TFlashRunner for device FlashRunner
// Compiler     : Delphi 2007
// Author       : 2015-08-14 /bsu/
// History      :
//==============================================================================
unit FlashRunner;

interface
uses Serial3, Classes, SysUtils, StrUtils, Windows, Forms, DeviceBase, IniFiles, ConnBase, RS232;

type
// =============================================================================
// Class        : TFlashRunner
// Description  : Definition of class TFlashRunner for FlashRunner deriving
//                from TDeviceBase
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
  TFlashRunner=class(TDeviceBase)
  protected
    //t_ser: TSerial;
  protected
    function CheckAnswer(const ans: string): boolean;
    function ConfigConnByStr(const conf: string): boolean;
    function Sync(): boolean; override;
    function IntToDMDataStr(const num: integer): string;

  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    function GetLastError(var msg: string): Integer; override;
    function ConfigDevice(const ini: TMemIniFile): Boolean; override;
    function FreeDevice(): Boolean; override;
    function Connect(): Boolean; override;
    function Disconnect: boolean; override;
    function SendStr(const data: string; const ans: boolean = true): integer; override;
    function RecvStr(var data: string): integer; override;

    function SetDynamicMem(const addr: word; const num: integer): boolean;
    function RunScript(const script: string; const msecs: integer = -1): boolean;
  end;

var t_flashrunner: TFlashRunner; //a global variable of the FlashRunner instance

implementation
const
  C_ERR_PORT      = $8001; //
  C_ERR_BAUDRATE  = $8002; //
  C_ERR_WRONG_ANS = $8003; //
  C_ERR_WRONG_STA = $8004; //
  C_ERR_TIMEOUT   = $8005; //

// =============================================================================
// Class        : --
// Function     : Delay
//                delay for a moment in milli second
// Parameter    : msec, integer of milli seconds
// Return       : --
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
procedure Delay(const msec: cardinal);
var i_timeout: cardinal;
begin
  i_timeout := GetTickCount() + msec;
  repeat Application.ProcessMessages();
  until (GetTickCount() >= i_timeout);
end;

// =============================================================================
// Class        : --
// Function     : IndexOfStr
//                return index of a string in an array
// Parameter    : aArray, in which the string is searched
//                S, string to search
// Return       : index of string in the array. -1 is returnd, if not found
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function IndexOfStr(const aArray: array of string; const S: string): integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(aArray) to High(aArray) do
    if SameText(S, aArray[I]) then begin
      Result := I;
      Break;
    end;
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : ConfigConnByStr
//                get settings of connection from a given string and config it
// Parameter    : conf, a string of rs232 settings, e.g.
//                'Port:8|Baudrate:115200'
// Return       : true, if the setting is available.
//                false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      : 2015-09-03 /bsu/ return true, only if PORT and BAUDRATE are both configured
// =============================================================================
function TFlashRunner.ConfigConnByStr(const conf: string): boolean;
const CSTR_KEYS: array[0..1] of string = ('PORT','BAUDRATE');
var
  s_in, s_key, s_val: string;
  t_keyvalues, t_keyval: TStringList;
  i, i_value, i_idx: integer;
  ar_flags : array[LOW(CSTR_KEYS)..HIGH(CSTR_KEYS)] of boolean;
begin
  s_in := trim(conf);
  result := false;
  t_keyvalues := TStringList.Create;
  t_keyval := TStringList.Create;
  for i := LOW(ar_flags) to HIGH(ar_flags) do ar_flags[i] := false;

  if (ExtractStrings(['|'], [' ', Char(9)], PChar(s_in), t_keyvalues) > 0) then begin
    i := 0;
    repeat
      t_keyval.Clear;
      if (ExtractStrings([':'], [' '], PChar(t_keyvalues[i]), t_keyval) = 2) then begin
        s_key := UpperCase(t_keyval[0]);
        s_val := UpperCase(t_keyval[1]);
        i_idx := IndexOfStr( CSTR_KEYS, s_key);
        i_lasterr := C_ERR_PORT + i_idx;
        case i_idx of
        0: begin//'PORT'
          if TryStrToInt(s_val, i_value) then begin
            if ((i_value >= 1) and (i_value <= 256)) then begin
              t_ser.Port := i_value;
              ar_flags[i_idx] := true;
              i_lasterr := C_ERR_NOERROR;
              s_lastmsg := '';
            end else s_lastmsg := format('serial port out of range [1..256]: %d', [i_value]);
          end else s_lastmsg := format('invalid serial port: %s', [s_val]);
        end;
        1: begin//'BAUDRATE'
          if TryStrToInt(s_val, i_value) then begin
            t_ser.Baudrate := i_value;
            ar_flags[i_idx] := true;
            i_lasterr := C_ERR_NOERROR;
            s_lastmsg := '';
          end else s_lastmsg := format('invalid baudrate of serial port: %s', [s_val]);
        end;
        end;
      end;
      inc(i);
    until ((i>=t_keyvalues.Count) or (not (result)));
  end;
  t_keyval.Clear;
  FreeAndNil(t_keyval);
  t_keyvalues.Clear;
  FreeAndNil(t_keyvalues);
  result := true;
  for i := LOW(ar_flags) to HIGH(ar_flags) do  begin
    if not ar_flags[i] then begin
      i_lasterr := C_ERR_PORT + i;
      s_lastmsg := format('[%s] of serial port is not given.', [CSTR_KEYS[i]]);
      result := false;
      break;
    end;
  end;
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : IntToDMDataStr
//                convert an integer into a string with format of data
//                for FlashRunner-command DMSET in word-addressing,
//                e.g. 1050820084($3EA23DF4)->'$3E $00 $A2 $00 $3D $00 $F4 $00'
// Parameter    : num, integer to be converted
// Return       : string in format of DMSET-data
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TFlashRunner.IntToDMDataStr(const num: integer): string;
var str_data: string;
begin
  str_data := RightStr(IntToHex(num, 8), 8);
  Insert(' $00 $', str_data, 7);
  Insert(' $00 $', str_data, 5);
  Insert(' $00 $', str_data, 3);
  result := '$' + str_data + ' $00';
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : Create
//                constructor of the class, which initializes the instance with
//                default values
// Parameter    : owner, which instanced this class
// Return       : --
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
constructor TFlashRunner.Create(owner: TComponent);
begin
	inherited Create(owner);
  t_ser := TSerial.Create(self);
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : Destroy
//                destructor of the class, which finilizes the instance
// Parameter    : owner, which instanced this class
// Return       : --
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
destructor TFlashRunner.Destroy;
begin
  FreeDevice();
  FreeAndNil(t_ser);
	inherited Destroy;
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : Connect
//                connects to the device (FlashRunner)
// Parameter    : --
// Return       : true, if the device is accesible in the time of timeout
//                false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TFlashRunner.Connect: boolean;
var ds_set: set of EDeviceState;
begin
  if e_state = DS_CONFIGURED then begin
    if not t_ser.Active then t_ser.Active := true;
    result := t_ser.Active;
    PostEvent(DE_CONNECT, result);
  end;
  ds_set := [DS_CONNECTED, DS_READY, DS_WAITING, DS_COMERROR];
  result := (e_state in ds_set);
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : Sync
//                coordinate with the device
// Parameter    : --
// Return       : true, if the device is accesible in the time of timeout
//                false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TFlashRunner.Sync(): boolean;
const C_STR_PING: string = 'SPING'; C_STR_PONG: string = 'PONG>'; C_TIMEOUT: cardinal = 3000;
var s_ans: string; ds_set : set of EDeviceState; i_time: cardinal; ch: char;
begin
  result := false;
  ds_set := [DS_CONNECTED, DS_COMERROR];
  if (e_state in ds_set) then
  begin
    i_time := GetTickCount() + C_TIMEOUT;

    //clear buffer of t_ser
    Delay(C_DELAY_MSEC);
    while ((t_ser.RxWaiting > 0) and ( GetTickCount() < i_time)) do t_ser.ReadChar(ch);

    //send string and wait til write-buffer is completely sent
    t_ser.WriteString(C_STR_PING + Char(13));
    repeat if (t_ser.TxWaiting > 0) then Delay(C_DELAY_MSEC);
    until ((t_ser.TxWaiting <= 0) or (GetTickCount() >= i_time));

    //receive string til read-buffer is empty or timeout
    repeat begin
      ch := chr(0);
      if (t_ser.ReadChar(ch) = 1) then s_ans := s_ans + ch
      else Delay(C_DELAY_MSEC);
    end; until ((t_ser.RxWaiting <= 0) or (GetTickCount() >= i_time));

    //verify the received string
    s_ans := trim(s_ans);
    result := SameText(C_STR_PONG,s_ans);
    PostEvent(DE_SYNC, result);
  end;
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : SendStr
//                send string to device
// Parameter    : str, a string to send
// Return       : integer, which counts char, which are sent
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TFlashRunner.SendStr(const data: string; const ans: boolean): integer;
var ch: char; timeout: cardinal;
begin
  result := 0;
  if TryToReady() then begin
    timeout := GetTickCount() + i_timeout;
    //clear read-buffer of t_ser
    repeat if (t_ser.ReadChar(ch) <> 1) then Delay(C_DELAY_MSEC);
    until ((t_ser.RxWaiting <= 0) or (GetTickCount() >= timeout));

    t_ser.WriteString(data);
    //wait til write-buffer is completely sent
    repeat if (t_ser.TxWaiting > 0) then Delay(C_DELAY_MSEC);
    until ((t_ser.TxWaiting <= 0) or (GetTickCount() >= timeout));
    result := (length(data) - t_ser.TxWaiting);

    PostEvent(DE_SEND, (result > 0));
    if (not ans) then PostEvent(DE_RECV, true);
  end;
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : RecvStr
//                receiv string from device
// Parameter    : str, a string for receiving
// Return       : integer, which counts char, which are received
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TFlashRunner.RecvStr(var data: string): integer;
var ch: char; timeout: cardinal;
begin
  result := 0;
  if e_state = DS_WAITING then begin
    timeout := GetTickCount() + i_timeout;
    //wait til any data arrives
    repeat if (t_ser.RxWaiting <= 0) then Delay(C_DELAY_MSEC);
    until ((t_ser.RxWaiting > 0) or (GetTickCount() >= timeout));

    //prepare string for receiving
    data := '';
    //receive char and save it into the string
    repeat
    begin
      ch := chr(0);
      if (t_ser.ReadChar(ch) = 1) then data := data + ch
      else Delay(C_DELAY_MSEC);
    end;
    until ((t_ser.RxWaiting <= 0) or (GetTickCount() >= timeout));
    result := length(data);
    CheckAnswer(data);
    PostEvent(DE_RECV, (i_lasterr=0));
  end;
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : CheckAnswer
//                check, if an error exists in the answer string from device
// Parameter    : ans, the received string
// Return       : integer, error number
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TFlashRunner.CheckAnswer(const ans: string): boolean;
const C_VALID_CHARS: set of char = ['>', '!'];
var s_in: string; i_len: integer;
begin
  s_in := trim(ans);
  i_len := length(s_in);
  s_lastmsg := format('answer string: %s', [IfThen(i_len>0,ans,'[empty]')]);
  if (i_len > 0) then begin
    if (s_in[i_len] = '>') then  i_lasterr := C_ERR_NOERROR //sucessful answer
    else if (s_in[i_len] = '!') then begin //failure answer
      delete(s_in, i_len, 1);
      TryStrToInt(s_in, i_lasterr);
    end  else i_lasterr := C_ERR_UNKNOWN;
  end else  i_lasterr := C_ERR_WRONG_ANS;
  result := (i_lasterr = C_ERR_NOERROR);
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : GetLastError
//                override function
// Parameter    : msg, output string
// Return       : integer, last error number and set msg with the last message
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TFlashRunner.GetLastError(var msg: string): Integer;
begin
  result := i_lasterr;
  case i_lasterr of
  0: msg := 'No error exist.';
  else msg := 'This error is not specified.';
  end;
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : ConfigDevice
//                config FlashRunner using settings from INI like:
//                [FlashRunner]
//                DESCRIPTION=A universal programmer for writing flash
//                PRODUCER=SMH Technologies
//                TYPE=FR01LAN
//                CONN_RS232=PORT:8|Baudrate:115200
//                TIMEOUT=30000
// Parameter    : ini, a instance of TMemIniFile, in which section [FlashRunner]
//                     is saved
// Return       : true, if the device is configured successfully
//                false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TFlashRunner.ConfigDevice(const ini: TMemIniFile): boolean;
const
  C_STR_FR   : string = 'FlashRunner';
//  C_STR_DESC : string = 'DESCRIPTION';
//  C_STR_PROD : string = 'PRODUCER';
//  C_STR_TYPE : string = 'TYPE';
  C_STR_TIMEOUT : string = 'TIMEOUT';
var
  s_inivalue: string;
begin
  result := false;
  if (DeviceState = DS_NONE) then
  begin
    //settings from ini file
    if (ini.SectionExists(C_STR_FR)) then
    begin
      SetTimeout(ini.ReadInteger(C_STR_FR, C_STR_TIMEOUT, C_TIMEOUT_MSEC));
      s_inivalue := trim(ini.ReadString(C_STR_FR, CSTR_CONN_KEYS[CT_RS232], ''));
      if not assigned(t_conn) then t_conn := TConnRS232.Create(self);
      result := t_conn.Config(s_inivalue);
    end;
    PostEvent(DE_CONFIG, result);;
  end;
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : Disconnect
//                disconnect from the device
// Parameter    : --
// Return       : true, if the device is disconnected successfully
//                false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TFlashRunner.Disconnect(): boolean;
begin
  t_ser.Active := false;
  result := (t_ser.Active = false);
  PostEvent(DE_DISCONNECT, result);
end;


// =============================================================================
// Class        : TFlashRunner
// Function     : FreeDevice
//                release device, undo what is done in ConfigDevice
// Parameter    : --
// Return       : true, if the device is released successfully
//                false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TFlashRunner.FreeDevice(): boolean;
begin
  result := Disconnect();
  PostEvent(DE_FREE, result);;
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : SetDynamicMem
//                builds a string for setting dynamic memory of flash runner and
//                sends it to flash runner, then receives the answer and checks it.
//                The dynamic memory has 512 bytes and is addressed from 0 on.
// Parameter    : addr, address of dynamic memory in flash runner
//                num, a integer which is being set in the dynamic memory
// Return       : true, if the device is released successfully
//                false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TFlashRunner.SetDynamicMem(const addr: word; const num: integer): boolean;
var s_send, s_answer: string;
begin
  result := false;
  s_send := format('DMSET $%.4x 8 %s', [addr, IntToDMDataStr(num)]) + Char(13);
  if SendStr(s_send) = length(s_send) then begin
    if RecvStr(s_answer) > 0 then result := CheckAnswer(s_answer);
  end;
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : SetDynamicMem
//                builds a string for executing a script form SD card of flash
//                runner, then receives the answer and checks it
// Parameter    : script, address of dynamic memory in flash runner
//                num, a integer which is being set in the dynamic memory
// Return       : true, if the device is released successfully
//                false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TFlashRunner.RunScript(const script: string; const msecs: integer): boolean;
var s_send, s_answer: string; i_tmp: cardinal;
begin
  result := false;
  i_tmp := i_timeout;
  SetTimeout(msecs);
  s_send := format('RUN %s', [script]) + Char(13);
  if SendStr(s_send) = length(s_send) then begin
    if RecvStr(s_answer) > 0 then result := CheckAnswer(s_answer);
  end;
  SetTimeout(i_tmp);
end;

end.
